const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;
const DEBUG_TRACE_EXECUTION = @import("common.zig").DEBUG_TRACE_EXECUTION;
const disassembleInstruction = @import("debug.zig").disassembleInstruction;

const OpCode = Chunk.OpCode;
const print = std.debug.print;

const STACK_MAX = 256;

chunk: *Chunk,
ip: [*]u8,
stack: [STACK_MAX]Value,
stackTop: [*]Value,

const Self = @This();

pub const InterpretResult = enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
};

pub fn init(chunk: *Chunk) Self {
    return Self{
        .chunk = chunk,
        .ip = chunk.code.items.ptr,
        .stack = undefined,
        .stackTop = undefined,
    };
}

pub fn deinit(_: Self) void {}

fn resetStack(self: *Self) void {
    self.stackTop = @ptrCast(&self.stack);
}

pub fn push(self: *Self, value: Value) void {
    self.stackTop[0] = value;
    self.stackTop += 1;
}

pub fn pop(self: *Self) Value {
    self.stackTop -= 1;
    return self.stackTop[0];
}

pub fn interpret(self: *Self) InterpretResult {
    self.resetStack();

    while (true) {
        if (DEBUG_TRACE_EXECUTION) {
            print("          ", .{});
            var slot: [*]Value = @ptrCast(&self.stack);
            while (slot != self.stackTop) {
                print("[ ", .{});
                slot[0].print();
                print(" ]", .{});
                slot += 1;
            }
            print("\n", .{});

            _ = disassembleInstruction(self.chunk, @intFromPtr(self.ip) - @intFromPtr(self.chunk.code.items.ptr));
        }

        const instruction: OpCode = @enumFromInt(self.readByte());
        switch (instruction) {
            OpCode.OP_CONSTANT => {
                const constant = self.readConstant();
                self.push(constant);
            },
            OpCode.OP_ADD => {
                const b = self.pop();
                const a = self.pop();
                self.push(.{ .f64 = a.f64 + b.f64 });
            },
            OpCode.OP_SUBTRACT => {
                const b = self.pop();
                const a = self.pop();
                self.push(.{ .f64 = a.f64 - b.f64 });
            },
            OpCode.OP_MULTIPLY => {
                const b = self.pop();
                const a = self.pop();
                self.push(.{ .f64 = a.f64 * b.f64 });
            },
            OpCode.OP_DIVIDE => {
                const b = self.pop();
                const a = self.pop();
                self.push(.{ .f64 = a.f64 / b.f64 });
            },
            OpCode.OP_NEGATE => {
                self.push(.{ .f64 = -(self.pop().f64) });
            },
            OpCode.OP_RETURN => {
                self.pop().print();
                print("\n", .{});
                return InterpretResult.INTERPRET_OK;
            },
            _ => {
                print("Unknown opcode {d:4}\n", .{instruction});
                return InterpretResult.INTERPRET_RUNTIME_ERROR;
            },
        }
    }
}

inline fn readByte(self: *Self) u8 {
    const byte: u8 = self.ip[0];
    self.ip += 1;
    return byte;
}

fn readConstant(self: *Self) Value {
    return self.chunk.constants.values.items[self.readByte()];
}

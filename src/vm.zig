const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;
const DEBUG_TRACE_EXECUTION = @import("common.zig").DEBUG_TRACE_EXECUTION;
const disassembleInstruction = @import("debug.zig").disassembleInstruction;

const OpCode = Chunk.OpCode;
const print = std.debug.print;

chunk: *Chunk,
ip: [*]u8,

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
    };
}

pub fn deinit(_: Self) void {}

pub fn interpret(self: *Self) InterpretResult {
    while (true) {
        if (DEBUG_TRACE_EXECUTION) {
            _ = disassembleInstruction(self.chunk, @intFromPtr(self.ip) - @intFromPtr(self.chunk.code.items.ptr));
        }

        const instruction: OpCode = @enumFromInt(self.readByte());
        switch (instruction) {
            OpCode.OP_CONSTANT => {
                const constant: Value = self.readConstant();
                constant.print();
                print("\n", .{});
            },
            OpCode.OP_RETURN => {
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

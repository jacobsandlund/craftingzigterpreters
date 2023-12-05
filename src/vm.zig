const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;
const DEBUG_TRACE_EXECUTION = @import("common.zig").DEBUG_TRACE_EXECUTION;
const disassembleInstruction = @import("debug.zig").disassembleInstruction;
const compile = @import("compiler.zig").compile;

const OpCode = Chunk.OpCode;

const STACK_MAX = 256;

allocator: std.mem.Allocator,
ip: [*]u8,
chunk: *Chunk,
stack: [STACK_MAX]Value,
stackTop: [*]Value,

const Self = @This();

pub const InterpretError = error{
    CompileError,
    RuntimeError,
} || std.fs.File.WriteError;

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{
        .allocator = allocator,
        .chunk = undefined,
        .ip = undefined,
        .stack = undefined,
        .stackTop = undefined,
    };
}

pub fn deinit(_: Self) void {}

fn resetStack(self: *Self) void {
    self.stackTop = @ptrCast(&self.stack);
}

fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) !void {
    const stderr = std.io.getStdErr();
    const errWriter = stderr.writer();

    try errWriter.print(fmt, args);

    const instruction = @intFromPtr(self.ip) - @intFromPtr(self.chunk.code.items.ptr) - 1;
    const line = self.chunk.lines.items[instruction];
    try errWriter.print("[line {d}] in script\n", .{line});
    self.resetStack();
}

fn push(self: *Self, value: Value) void {
    self.stackTop[0] = value;
    self.stackTop += 1;
}

fn pop(self: *Self) Value {
    self.stackTop -= 1;
    return self.stackTop[0];
}

fn peek(self: Self, distance: usize) Value {
    return (self.stackTop - 1 - distance)[0];
}

fn isFalsey(value: Value) bool {
    return value == Value.nil or (value == Value.boolean and !value.boolean);
}

pub fn interpret(self: *Self, source: []const u8) InterpretError!void {
    const stderr = std.io.getStdErr();
    const errWriter = stderr.writer();

    var chunk = Chunk.init(self.allocator);
    defer chunk.deinit();

    const success = compile(source, &chunk) catch |err| {
        try errWriter.print("Got error when trying to compile: {}\n", .{err});

        return InterpretError.CompileError;
    };

    if (!success) {
        return InterpretError.CompileError;
    }

    self.chunk = &chunk;
    self.ip = chunk.code.items.ptr;

    return self.run();
}

pub fn run(self: *Self) InterpretError!void {
    const stdout = std.io.getStdOut();
    const stderr = std.io.getStdErr();
    const writer = stdout.writer();
    const errWriter = stderr.writer();
    var chunk = self.chunk;

    self.resetStack();

    while (true) {
        if (DEBUG_TRACE_EXECUTION) {
            try errWriter.print("          ", .{});
            var slot: [*]Value = @ptrCast(&self.stack);
            while (slot != self.stackTop) {
                try errWriter.print("[ ", .{});
                try slot[0].print(errWriter);
                try errWriter.print(" ]", .{});
                slot += 1;
            }
            try errWriter.print("\n", .{});

            _ = try disassembleInstruction(errWriter, chunk, @intFromPtr(self.ip) - @intFromPtr(chunk.code.items.ptr));
        }

        const instruction: OpCode = @enumFromInt(self.readByte());
        switch (instruction) {
            OpCode.OP_CONSTANT => {
                const constant = self.readConstant(chunk);
                self.push(constant);
            },
            OpCode.OP_NIL => {
                self.push(Value.nil);
            },
            OpCode.OP_TRUE => {
                self.push(Value{ .boolean = true });
            },
            OpCode.OP_FALSE => {
                self.push(Value{ .boolean = false });
            },
            OpCode.OP_EQUAL => {
                const b = self.pop();
                const a = self.pop();
                self.push(.{ .boolean = Value.equal(a, b) });
            },
            OpCode.OP_GREATER => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .boolean = a > b });
            },
            OpCode.OP_LESS => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .boolean = a < b });
            },
            OpCode.OP_ADD => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .number = a + b });
            },
            OpCode.OP_SUBTRACT => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .number = a - b });
            },
            OpCode.OP_MULTIPLY => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .number = a * b });
            },
            OpCode.OP_DIVIDE => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .number = a / b });
            },
            OpCode.OP_NOT => {
                self.push(.{ .boolean = isFalsey(self.pop()) });
            },
            OpCode.OP_NEGATE => {
                if (self.peek(0) != Value.number) {
                    try self.runtimeError("Operand must be a number.", .{});
                    return InterpretError.RuntimeError;
                }
                self.push(.{ .number = -(self.pop().number) });
            },
            OpCode.OP_RETURN => {
                try self.pop().print(writer);
                try writer.print("\n", .{});
                return;
            },
            _ => {
                try errWriter.print("Unknown opcode {d:4}\n", .{instruction});
                return InterpretError.RuntimeError;
            },
        }
    }
}

inline fn readByte(self: *Self) u8 {
    const byte: u8 = self.ip[0];
    self.ip += 1;
    return byte;
}

fn readConstant(self: *Self, chunk: *Chunk) Value {
    return chunk.constants.values.items[self.readByte()];
}

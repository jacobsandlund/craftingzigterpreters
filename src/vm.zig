const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;
const DEBUG_TRACE_EXECUTION = @import("common.zig").DEBUG_TRACE_EXECUTION;
const disassembleInstruction = @import("debug.zig").disassembleInstruction;
const compile = @import("compiler.zig").compile;
const object = @import("object.zig");
const GcAllocator = @import("GcAllocator.zig");
const Table = @import("table.zig");

const OpCode = Chunk.OpCode;
const Obj = object.Obj;
const ObjString = object.ObjString;
const ObjFunction = object.ObjFunction;

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * (std.math.maxInt(u8) + 1);

allocator: GcAllocator,
frames: [FRAMES_MAX]CallFrame,
frameCount: usize,
stack: [STACK_MAX]Value,
stackTop: [*]Value,
globals: Table,

const CallFrame = struct {
    function: *ObjFunction,
    ip: [*]u8,
    slots: [*]Value,
};

const Self = @This();

pub const InterpretError = error{
    CompileError,
    RuntimeError,
} || std.fs.File.WriteError || std.mem.Allocator.Error;

pub fn init(allocator: std.mem.Allocator) Self {
    var gcAllocator = GcAllocator.init(allocator);
    return Self{
        .allocator = gcAllocator,
        .frames = undefined,
        .frameCount = 0,
        .stack = undefined,
        .stackTop = undefined,
        .globals = Table.init(gcAllocator.allocator()),
    };
}

pub fn deinit(self: *Self) void {
    self.globals.deinit();
    self.allocator.deinit();
}

fn resetStack(self: *Self) void {
    self.stackTop = @ptrCast(&self.stack);
    self.frameCount = 0;
}

fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) !void {
    const stderr = std.io.getStdErr();
    const errWriter = stderr.writer();

    try errWriter.print(fmt, args);

    const frame = &self.frames[self.frameCount - 1];
    const instruction = @intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.code.items.ptr) - 1;
    const line = frame.function.chunk.lines.items[instruction];
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

    const function = compile(&self.allocator, source) catch |err| {
        try errWriter.print("Got error when trying to compile: {}\n", .{err});

        return error.CompileError;
    };

    if (function) |f| {
        self.resetStack();
        self.push(Value{ .obj = &f.obj });
        var frame = &self.frames[self.frameCount];
        self.frameCount += 1;
        frame.function = f;
        frame.ip = f.chunk.code.items.ptr;
        frame.slots = @ptrCast(&self.stack);

        return self.run();
    } else {
        return error.CompileError;
    }
}

pub fn run(self: *Self) InterpretError!void {
    const stdout = std.io.getStdOut();
    const stderr = std.io.getStdErr();
    const writer = stdout.writer();
    const errWriter = stderr.writer();
    var frame = &self.frames[self.frameCount - 1];

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

            _ = try disassembleInstruction(errWriter, &frame.function.chunk, @intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.code.items.ptr));
        }

        const instruction: OpCode = @enumFromInt(readByte(frame));
        switch (instruction) {
            .OP_CONSTANT => {
                const constant = readConstant(frame);
                self.push(constant);
            },
            .OP_NIL => self.push(Value.nil),
            .OP_TRUE => self.push(Value{ .boolean = true }),
            .OP_FALSE => self.push(Value{ .boolean = false }),
            .OP_POP => _ = self.pop(),
            .OP_GET_LOCAL => {
                const slot = readByte(frame);
                self.push(frame.slots[slot]);
            },
            .OP_SET_LOCAL => {
                const slot = readByte(frame);
                frame.slots[slot] = self.peek(0);
            },
            .OP_GET_GLOBAL => {
                const name: *ObjString = readString(frame);
                const value = self.globals.get(name);
                if (value) |v| {
                    self.push(v);
                } else {
                    try self.runtimeError("Undefined variable '{s}'.", .{name.string});
                    return InterpretError.RuntimeError;
                }
            },
            .OP_DEFINE_GLOBAL => {
                const name = readString(frame);
                _ = try self.globals.set(name, self.peek(0));
                _ = self.pop();
            },
            .OP_SET_GLOBAL => {
                const name = readString(frame);
                if (try self.globals.set(name, self.peek(0))) {
                    _ = self.globals.delete(name);
                    try self.runtimeError("Undefined variable '{s}'.", .{name.string});
                    return InterpretError.RuntimeError;
                }
            },
            .OP_EQUAL => {
                const b = self.pop();
                const a = self.pop();
                self.push(.{ .boolean = Value.equal(a, b) });
            },
            .OP_GREATER => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .boolean = a > b });
            },
            .OP_LESS => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .boolean = a < b });
            },
            .OP_ADD => {
                if (self.peek(0).isObjType(Obj.Type.OBJ_STRING) and self.peek(1).isObjType(Obj.Type.OBJ_STRING)) {
                    try self.concatenate();
                } else if (self.peek(0) == Value.number and self.peek(1) == Value.number) {
                    const b = self.pop().number;
                    const a = self.pop().number;
                    self.push(.{ .number = a + b });
                } else {
                    try self.runtimeError("Operands must be two numbers or two strings.", .{});
                    return InterpretError.RuntimeError;
                }
            },
            .OP_SUBTRACT => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .number = a - b });
            },
            .OP_MULTIPLY => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .number = a * b });
            },
            .OP_DIVIDE => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return InterpretError.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .number = a / b });
            },
            .OP_NOT => {
                self.push(.{ .boolean = isFalsey(self.pop()) });
            },
            .OP_NEGATE => {
                if (self.peek(0) != Value.number) {
                    try self.runtimeError("Operand must be a number.", .{});
                    return InterpretError.RuntimeError;
                }
                self.push(.{ .number = -(self.pop().number) });
            },
            .OP_PRINT => {
                try self.pop().print(writer);
                try writer.print("\n", .{});
            },
            .OP_JUMP => {
                const offset = readShort(frame);
                frame.ip += offset;
            },
            .OP_JUMP_IF_FALSE => {
                const offset = readShort(frame);
                if (isFalsey(self.peek(0))) frame.ip += offset;
            },
            .OP_LOOP => {
                const offset = readShort(frame);
                frame.ip -= offset;
            },
            .OP_RETURN => {
                // Exit interpreter.
                return;
            },
            _ => {
                try errWriter.print("Unknown opcode {d:4}\n", .{instruction});
                return InterpretError.RuntimeError;
            },
        }
    }
}

inline fn readByte(frame: *CallFrame) u8 {
    const byte: u8 = frame.ip[0];
    frame.ip += 1;
    return byte;
}

inline fn readShort(frame: *CallFrame) u16 {
    const short: u16 = @as(u16, frame.ip[0]) << 8 | frame.ip[1];
    frame.ip += 2;
    return short;
}

fn readConstant(frame: *CallFrame) Value {
    return frame.function.chunk.constants.values.items[readByte(frame)];
}

fn readString(frame: *CallFrame) *ObjString {
    return readConstant(frame).obj.string();
}

fn concatenate(self: *Self) !void {
    const bString = self.pop().obj.string().string;
    const aString = self.pop().obj.string().string;

    var resultSlice = try self.allocator.allocator().alloc(u8, aString.len + bString.len);
    @memcpy(resultSlice[0..aString.len], aString);
    @memcpy(resultSlice[aString.len..], bString);

    const result = try ObjString.takeString(&self.allocator, resultSlice);
    self.push(Value{ .obj = &result.obj });
}

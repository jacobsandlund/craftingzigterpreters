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
const ObjFunction = object.ObjFunction;
const ObjNative = object.ObjNative;
const ObjString = object.ObjString;
const NativeFn = object.NativeFn;

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

pub fn init(allocator: std.mem.Allocator) !Self {
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

// TODO: what's the Zig way to have this happen in init?
pub fn setup(self: *Self) !void {
    self.resetStack();
    try self.defineNative("clock", clockNative);
    try self.defineNative("countArgs", countArgsNative);
}

fn reset(self: *Self) void {
    self.resetStack();
    startNanoTimestamp = std.time.nanoTimestamp();
}

pub fn deinit(self: *Self) void {
    self.globals.deinit();
    self.allocator.deinit();
}

const nanosecondsPerSecond = 1000_000_000.0;
var startNanoTimestamp: i128 = undefined;

fn clockNative(_: []Value) Value {
    const now = std.time.nanoTimestamp();
    const delta = now - startNanoTimestamp;
    return .{ .number = @as(f64, @floatFromInt(delta)) / nanosecondsPerSecond };
}

fn countArgsNative(args: []Value) Value {
    return .{ .number = @floatFromInt(args.len) };
}

fn resetStack(self: *Self) void {
    self.stackTop = @ptrCast(&self.stack);
    self.frameCount = 0;
}

fn runtimeError(self: *Self, comptime fmt: []const u8, args: anytype) !void {
    const stderr = std.io.getStdErr();
    const errWriter = stderr.writer();

    try errWriter.print(fmt, args);
    try errWriter.print("\n", .{});

    var i = @as(isize, @intCast(self.frameCount)) - 1;
    while (i >= 0) : (i -= 1) {
        const frame = &self.frames[@intCast(i)];
        const function = frame.function;
        const instruction = @intFromPtr(frame.ip) - @intFromPtr(frame.function.chunk.code.items.ptr) - 1;
        try errWriter.print("[line {d}] in ", .{function.chunk.lines.items[instruction]});
        if (function.name) |name| {
            try errWriter.print("{s}()\n", .{name.string});
        } else {
            try errWriter.print("script\n", .{});
        }
    }

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

fn defineNative(self: *Self, name: []const u8, function: *const NativeFn) !void {
    self.push(Value{ .obj = &(try ObjString.copyString(&self.allocator, name)).obj });
    self.push(Value{ .obj = &(try ObjNative.create(&self.allocator, function)).obj });
    _ = try self.globals.set(self.stack[0].obj.string(), self.stack[1]);
    _ = self.pop();
    _ = self.pop();
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
        self.reset();
        self.push(Value{ .obj = &f.obj });
        _ = try self.call(f, 0);

        return self.run();
    } else {
        return error.CompileError;
    }
}

fn run(self: *Self) InterpretError!void {
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
            .OP_CALL => {
                const argCount = readByte(frame);
                if (!try self.callValue(self.peek(argCount), argCount)) {
                    return InterpretError.RuntimeError;
                }
                frame = &self.frames[self.frameCount - 1];
            },
            .OP_RETURN => {
                const result = self.pop();
                self.frameCount -= 1;
                if (self.frameCount == 0) {
                    _ = self.pop();
                    return;
                }

                self.stackTop = frame.slots;
                self.push(result);
                frame = &self.frames[self.frameCount - 1];
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

fn callValue(self: *Self, callee: Value, argCount: usize) !bool {
    if (callee == Value.obj) {
        switch (callee.obj.type) {
            .OBJ_FUNCTION => return self.call(callee.obj.function(), argCount),
            .OBJ_NATIVE => {
                const native = callee.obj.native().function;
                const result = native((self.stackTop - argCount)[0..argCount]);
                self.stackTop -= argCount + 1;
                self.push(result);
                return true;
            },
            else => {}, // pass
        }
    }
    try self.runtimeError("Can only call functions and classes.", .{});
    return false;
}

fn call(self: *Self, function: *ObjFunction, argCount: usize) !bool {
    if (argCount != function.arity) {
        try self.runtimeError("Expected {d} arguments but got {d}", .{ function.arity, argCount });
        return false;
    }

    if (self.frameCount == FRAMES_MAX) {
        try self.runtimeError("Stack overflow.", .{});
        return false;
    }

    var frame: *CallFrame = &self.frames[self.frameCount];
    self.frameCount += 1;
    frame.function = function;
    frame.ip = function.chunk.code.items.ptr;
    frame.slots = self.stackTop - argCount - 1;
    return true;
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

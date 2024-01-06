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
const ObjBoundMethod = object.ObjBoundMethod;
const ObjClass = object.ObjClass;
const ObjClosure = object.ObjClosure;
const ObjFunction = object.ObjFunction;
const ObjInstance = object.ObjInstance;
const ObjNative = object.ObjNative;
const ObjString = object.ObjString;
const ObjUpvalue = object.ObjUpvalue;
const NativeFn = object.NativeFn;

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * (std.math.maxInt(u8) + 1);

allocator: GcAllocator,
frames: [FRAMES_MAX]CallFrame,
frameCount: usize,
openUpvalues: ?*ObjUpvalue,
stack: [STACK_MAX]Value,
stackTop: [*]Value,
globals: Table,
initString: *ObjString,

const CallFrame = struct {
    closure: *ObjClosure,
    ip: [*]u8,
    slots: [*]Value,
};

const Self = @This();

pub const InterpretError = error{
    CompileError,
    RuntimeError,
} || std.fs.File.WriteError || std.mem.Allocator.Error;

pub fn init(allocator: std.mem.Allocator) !Self {
    return .{
        .allocator = GcAllocator.init(allocator),
        .frames = undefined,
        .frameCount = 0,
        .openUpvalues = null,
        .stack = undefined,
        .stackTop = undefined,
        .globals = undefined,
        .initString = undefined,
    };
}

// TODO: what's the Zig way to have this happen in init?
pub fn setup(self: *Self) !void {
    self.allocator.setup(self);
    self.globals = Table.init(self.allocator.allocator());
    self.initString = try ObjString.copyString(&self.allocator, "init");
    self.allocator.ready = true;
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
        const function = frame.closure.function;
        const instruction = @intFromPtr(frame.ip) - @intFromPtr(frame.closure.function.chunk.code.items.ptr) - 1;
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
        const closure = try ObjClosure.create(&self.allocator, f);
        _ = self.pop();
        self.push(Value{ .obj = &closure.obj });
        _ = try self.call(closure, 0);

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

            _ = try disassembleInstruction(errWriter, &frame.closure.function.chunk, @intFromPtr(frame.ip) - @intFromPtr(frame.closure.function.chunk.code.items.ptr));
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
                    return error.RuntimeError;
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
                    return error.RuntimeError;
                }
            },
            .OP_GET_UPVALUE => {
                const slot = readByte(frame);
                self.push(frame.closure.upvalues[slot].?.location.*);
            },
            .OP_SET_UPVALUE => {
                const slot = readByte(frame);
                frame.closure.upvalues[slot].?.location.* = self.peek(0);
            },
            .OP_GET_PROPERTY => {
                if (!self.peek(0).isObjType(.OBJ_INSTANCE)) {
                    try self.runtimeError("Only instances have properties.", .{});
                    return error.RuntimeError;
                }

                const instance = self.peek(0).obj.instance();
                const name = readString(frame);

                if (instance.fields.get(name)) |value| {
                    _ = self.pop(); // Instance.
                    self.push(value);
                } else if (!try self.bindMethod(instance.class, name)) {
                    return error.RuntimeError;
                }
            },
            .OP_SET_PROPERTY => {
                if (!self.peek(1).isObjType(.OBJ_INSTANCE)) {
                    try self.runtimeError("Only instances have properties.", .{});
                    return error.RuntimeError;
                }

                const instance = self.peek(1).obj.instance();
                try self.peek(0).print(errWriter);
                _ = try instance.fields.set(readString(frame), self.peek(0));
                const value = self.pop();
                _ = self.pop();
                self.push(value);
            },
            .OP_EQUAL => {
                const b = self.pop();
                const a = self.pop();
                self.push(.{ .boolean = Value.equal(a, b) });
            },
            .OP_GREATER => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return error.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .boolean = a > b });
            },
            .OP_LESS => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return error.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .boolean = a < b });
            },
            .OP_ADD => {
                if (self.peek(0).isObjType(.OBJ_STRING) and self.peek(1).isObjType(Obj.Type.OBJ_STRING)) {
                    try self.concatenate();
                } else if (self.peek(0) == Value.number and self.peek(1) == Value.number) {
                    const b = self.pop().number;
                    const a = self.pop().number;
                    self.push(.{ .number = a + b });
                } else {
                    try self.runtimeError("Operands must be two numbers or two strings.", .{});
                    return error.RuntimeError;
                }
            },
            .OP_SUBTRACT => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return error.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .number = a - b });
            },
            .OP_MULTIPLY => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return error.RuntimeError;
                }
                const b = self.pop().number;
                const a = self.pop().number;
                self.push(.{ .number = a * b });
            },
            .OP_DIVIDE => {
                if (self.peek(0) != Value.number or self.peek(1) != Value.number) {
                    try self.runtimeError("Operands must be numbers.", .{});
                    return error.RuntimeError;
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
                    return error.RuntimeError;
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
                    return error.RuntimeError;
                }
                frame = &self.frames[self.frameCount - 1];
            },
            .OP_INVOKE => {
                const method = readString(frame);
                const argCount = readByte(frame);
                if (!try self.invoke(method, argCount)) {
                    return error.RuntimeError;
                }
                frame = &self.frames[self.frameCount - 1];
            },
            .OP_CLOSURE => {
                const function = readConstant(frame).obj.function();
                const closure = try ObjClosure.create(&self.allocator, function);
                self.push(Value{ .obj = &closure.obj });
                for (closure.upvalues) |*upvalue| {
                    const isLocal = readByte(frame) == 1;
                    const index = readByte(frame);
                    if (isLocal) {
                        upvalue.* = try self.captureUpvalue(@ptrCast(frame.slots + index));
                    } else {
                        upvalue.* = frame.closure.upvalues[index].?;
                    }
                }
            },
            .OP_CLOSE_UPVALUE => {
                self.closeUpvalues(@ptrCast(self.stackTop - 1));
                _ = self.pop();
            },
            .OP_RETURN => {
                const result = self.pop();
                self.closeUpvalues(@ptrCast(frame.slots));
                self.frameCount -= 1;
                if (self.frameCount == 0) {
                    _ = self.pop();
                    return;
                }

                self.stackTop = frame.slots;
                self.push(result);
                frame = &self.frames[self.frameCount - 1];
            },
            .OP_CLASS => {
                const class = try ObjClass.create(&self.allocator, readString(frame));
                self.push(Value{ .obj = &class.obj });
            },
            .OP_METHOD => {
                try self.defineMethod(readString(frame));
            },
            _ => {
                try errWriter.print("Unknown opcode {d:4}\n", .{instruction});
                return error.RuntimeError;
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
    return frame.closure.function.chunk.constants.values.items[readByte(frame)];
}

fn readString(frame: *CallFrame) *ObjString {
    return readConstant(frame).obj.string();
}

fn callValue(self: *Self, callee: Value, argCount: usize) !bool {
    if (callee == Value.obj) {
        switch (callee.obj.type) {
            .OBJ_BOUND_METHOD => {
                const bound = callee.obj.boundMethod();
                (self.stackTop - argCount - 1)[0] = bound.receiver;
                return self.call(bound.method, argCount);
            },
            .OBJ_CLASS => {
                const class = callee.obj.class();
                const instance = try ObjInstance.create(&self.allocator, class);
                (self.stackTop - argCount - 1)[0] = Value{ .obj = &instance.obj };
                if (class.methods.get(self.initString)) |initializer| {
                    return self.call(initializer.obj.closure(), argCount);
                } else if (argCount != 0) {
                    try self.runtimeError("Expected 0 arguments but got {d}.", .{argCount});
                    return false;
                }
                return true;
            },
            .OBJ_CLOSURE => return self.call(callee.obj.closure(), argCount),
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

fn call(self: *Self, closure: *ObjClosure, argCount: usize) !bool {
    if (argCount != closure.function.arity) {
        try self.runtimeError("Expected {d} arguments but got {d}", .{ closure.function.arity, argCount });
        return false;
    }

    if (self.frameCount == FRAMES_MAX) {
        try self.runtimeError("Stack overflow.", .{});
        return false;
    }

    var frame: *CallFrame = &self.frames[self.frameCount];
    self.frameCount += 1;
    frame.closure = closure;
    frame.ip = closure.function.chunk.code.items.ptr;
    frame.slots = self.stackTop - argCount - 1;
    return true;
}

fn invoke(self: *Self, name: *ObjString, argCount: usize) !bool {
    const receiver = self.peek(argCount);

    if (!receiver.isObjType(.OBJ_INSTANCE)) {
        try self.runtimeError("Only instances have methods.", .{});
        return false;
    }

    const instance = receiver.obj.instance();

    if (instance.fields.get(name)) |value| {
        (self.stackTop - argCount - 1)[0] = value;
        return self.callValue(value, argCount);
    }

    return self.invokeFromClass(instance.class, name, argCount);
}

fn invokeFromClass(self: *Self, class: *ObjClass, name: *ObjString, argCount: usize) !bool {
    if (class.methods.get(name)) |method| {
        return self.call(method.obj.closure(), argCount);
    } else {
        try self.runtimeError("Undefined property '{s}'.", .{name.string});
        return false;
    }
}

fn captureUpvalue(self: *Self, local: *Value) !*ObjUpvalue {
    var prevUpvalue: ?*ObjUpvalue = null;
    var upvalue: ?*ObjUpvalue = self.openUpvalues;
    while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
        prevUpvalue = upvalue;
        upvalue = upvalue.?.next;
    }

    if (upvalue != null and upvalue.?.location == local) {
        return upvalue.?;
    }

    const createdUpvalue = try ObjUpvalue.create(&self.allocator, local);
    createdUpvalue.next = prevUpvalue;

    if (prevUpvalue) |prev| {
        prev.next = createdUpvalue;
    } else {
        self.openUpvalues = createdUpvalue;
    }

    return createdUpvalue;
}

fn closeUpvalues(self: *Self, last: *Value) void {
    while (self.openUpvalues != null and @intFromPtr(self.openUpvalues.?.location) >= @intFromPtr(last)) {
        const upvalue = self.openUpvalues.?;
        upvalue.closed = upvalue.location.*;
        upvalue.location = &upvalue.closed;
        self.openUpvalues = upvalue.next;
    }
}

fn concatenate(self: *Self) !void {
    const bString = self.peek(0).obj.string().string;
    const aString = self.peek(1).obj.string().string;

    var resultSlice = try self.allocator.allocator().alloc(u8, aString.len + bString.len);
    @memcpy(resultSlice[0..aString.len], aString);
    @memcpy(resultSlice[aString.len..], bString);

    const result = try ObjString.takeString(&self.allocator, resultSlice);
    _ = self.pop();
    _ = self.pop();
    self.push(Value{ .obj = &result.obj });
}

fn defineMethod(self: *Self, name: *ObjString) !void {
    const method = self.peek(0);
    const class = self.peek(1).obj.class();
    _ = try class.methods.set(name, method);
    _ = self.pop();
}

fn bindMethod(self: *Self, class: *ObjClass, name: *ObjString) !bool {
    if (class.methods.get(name)) |method| {
        const bound = try ObjBoundMethod.create(&self.allocator, self.peek(0), method.obj.closure());
        _ = self.pop();
        self.push(Value{ .obj = &bound.obj });
        return true;
    } else {
        try self.runtimeError("Undefined property '{s}'.", .{name.string});
        return false;
    }
}

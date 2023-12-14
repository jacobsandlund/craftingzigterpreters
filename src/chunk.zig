const std = @import("std");
const Value = @import("value.zig").Value;
const ValueArray = @import("value.zig").ValueArray;

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_DEFINE_GLOBAL,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_RETURN,
    _,
};

const Self = @This();

code: ArrayList(u8),
lines: ArrayList(u32),
constants: ValueArray,

pub fn init(allocator: Allocator) Self {
    return Self{
        .code = ArrayList(u8).init(allocator),
        .lines = ArrayList(u32).init(allocator),
        .constants = ValueArray.init(allocator),
    };
}

pub fn deinit(self: Self) void {
    self.code.deinit();
    self.lines.deinit();
    self.constants.deinit();
}

pub fn writeOpCode(self: *Self, opCode: OpCode, line: u32) Allocator.Error!void {
    try self.writeByte(@intFromEnum(opCode), line);
}

pub fn writeByte(self: *Self, byte: u8, line: u32) Allocator.Error!void {
    try self.code.append(byte);
    try self.lines.append(line);
}

pub fn addConstant(self: *Self, value: Value) Allocator.Error!usize {
    try self.constants.write(value);
    return self.constants.values.items.len - 1;
}

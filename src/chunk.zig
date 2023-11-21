const std = @import("std");
const Value = @import("value.zig").Value;
const ValueArray = @import("value.zig").ValueArray;

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const OpCode = enum(u8) {
    OP_CONSTANT,
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

pub fn addConstant(self: *Self, value: Value) Allocator.Error!u8 {
    try self.constants.write(value);
    const len = self.constants.values.items.len;
    if (len <= 256) {
        return @truncate(len - 1);
    } else {
        @panic("Too many constants!");
    }
}

const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const OpCode = enum(u8) {
    OP_RETURN,
};

const Self = @This();

code: ArrayList(u8),

pub fn init(allocator: Allocator) Self {
    return Self{
        .code = ArrayList(u8).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.code.deinit();
}

pub fn writeOpCode(self: *Self, opCode: OpCode) Allocator.Error!void {
    try self.code.append(@intFromEnum(opCode));
}

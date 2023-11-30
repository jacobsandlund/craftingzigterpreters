const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Value = union {
    f64: f64,

    const Self = @This();

    pub fn print(self: Self, writer: std.io.Writer) !void {
        try writer.print("{e}", .{self.f64});
    }
};

pub const ValueArray = struct {
    values: ArrayList(Value),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .values = ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: Self) void {
        self.values.deinit();
    }

    pub fn write(self: *Self, value: Value) Allocator.Error!void {
        try self.values.append(value);
    }
};

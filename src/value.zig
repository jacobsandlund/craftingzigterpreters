const std = @import("std");
const Obj = @import("object.zig").Obj;
const GcAllocator = @import("GcAllocator.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Value = union(Type) {
    number: f64,
    boolean: bool,
    obj: *Obj,
    nil,

    const Self = @This();

    pub const Type = enum {
        number,
        boolean,
        obj,
        nil,
    };

    pub fn print(self: Self, writer: std.fs.File.Writer) !void {
        switch (self) {
            .number => |number| try writer.print("{e}", .{number}),
            .boolean => |boolean| try writer.print("{s}", .{if (boolean) "true" else "false"}),
            .nil => try writer.print("nil", .{}),
            .obj => |obj| try obj.printObject(writer),
        }
    }

    pub fn equal(a: Self, b: Self) bool {
        if (@as(Type, a) != @as(Type, b)) {
            return false;
        }

        switch (a) {
            .boolean => return a.boolean == b.boolean,
            .nil => return true,
            .number => return a.number == b.number,
            .obj => return a.obj == b.obj,
        }

        return true;
    }

    pub fn isObjType(self: Self, objType: Obj.Type) bool {
        return self == Self.obj and self.obj.type == objType;
    }
};

pub const ValueArray = struct {
    values: ArrayList(Value),
    allocator: *GcAllocator,

    const Self = @This();

    pub fn init(allocator: *GcAllocator) Self {
        return .{
            .values = ArrayList(Value).init(allocator.allocator()),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: Self) void {
        self.values.deinit();
    }

    pub fn write(self: *Self, value: Value) Allocator.Error!void {
        self.allocator.temporaryValue = value;
        try self.values.append(value);
        self.allocator.temporaryValue = Value.nil;
    }
};

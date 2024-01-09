const std = @import("std");
const Obj = @import("object.zig").Obj;
const GcAllocator = @import("GcAllocator.zig");
const NAN_BOXING = @import("common.zig").NAN_BOXING;

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const Value = if (NAN_BOXING)
    u64
else
    union(ValueType) {
        boolean: bool,
        nil,
        number: f64,
        obj: *Obj,
    };

const SIGN_BIT: u64 = 0x8000000000000000;
const QNAN: u64 = 0x7ffc000000000000;

pub const ValueType = enum {
    boolean,
    nil,
    number,
    obj,
};

const ValueTag = enum(u2) {
    TagNil = 0b01,
    TagFalse = 0b10,
    TagTrue = 0b11,
};

const falseValue: u64 = QNAN | @intFromEnum(ValueTag.TagFalse);
const trueValue: u64 = QNAN | @intFromEnum(ValueTag.TagTrue);

pub const nilValue: Value = if (NAN_BOXING)
    QNAN | @intFromEnum(ValueTag.TagNil)
else
    Value.nil;

pub inline fn booleanValue(boolean: bool) Value {
    return if (NAN_BOXING)
        if (boolean)
            trueValue
        else
            falseValue
    else
        Value{ .boolean = boolean };
}

pub inline fn numberValue(number: f64) Value {
    return if (NAN_BOXING)
        @bitCast(number)
    else
        Value{ .number = number };
}

pub inline fn objValue(obj: *Obj) Value {
    return if (NAN_BOXING)
        SIGN_BIT | QNAN | @intFromPtr(obj)
    else
        Value{ .obj = obj };
}

pub inline fn isNil(value: Value) bool {
    return if (NAN_BOXING)
        value == nilValue
    else
        value == Value.nil;
}

pub inline fn isBoolean(value: Value) bool {
    return if (NAN_BOXING)
        value | 1 == trueValue
    else
        value == Value.boolean;
}

pub inline fn isNumber(value: Value) bool {
    return if (NAN_BOXING)
        value & QNAN != QNAN
    else
        value == Value.number;
}

pub inline fn isObj(value: Value) bool {
    return if (NAN_BOXING)
        value & (QNAN | SIGN_BIT) == (QNAN | SIGN_BIT)
    else
        value == Value.obj;
}

pub inline fn asBoolean(value: Value) bool {
    return if (NAN_BOXING)
        value == trueValue
    else
        value.boolean;
}

pub inline fn asNumber(value: Value) f64 {
    return if (NAN_BOXING) @bitCast(value) else value.number;
}

pub inline fn asObj(value: Value) *Obj {
    return if (NAN_BOXING)
        @ptrFromInt(value & ~(SIGN_BIT | QNAN))
    else
        value.obj;
}

pub fn print(value: Value, writer: std.fs.File.Writer) !void {
    if (NAN_BOXING) {
        if (isBoolean(value)) {
            try writer.print("{s}", .{if (asBoolean(value)) "true" else "false"});
        } else if (isNil(value)) {
            try writer.print("nil", .{});
        } else if (isNumber(value)) {
            try writer.print("{e}", .{asNumber(value)});
        } else if (isObj(value)) {
            try asObj(value).printObject(writer);
        }
    } else {
        switch (value) {
            .number => |number| try writer.print("{e}", .{number}),
            .boolean => |boolean| try writer.print("{s}", .{if (boolean) "true" else "false"}),
            .nil => try writer.print("nil", .{}),
            .obj => |obj| try obj.printObject(writer),
        }
    }
}

pub fn equal(a: Value, b: Value) bool {
    if (NAN_BOXING) {
        return a == b;
    } else {
        if (@as(ValueType, a) != @as(ValueType, b)) {
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
}

pub fn isObjType(value: Value, objType: Obj.Type) bool {
    return isObj(value) and asObj(value).type == objType;
}

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
        self.allocator.temporaryValue = nilValue;
    }
};

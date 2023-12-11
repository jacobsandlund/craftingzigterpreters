const Value = @import("value.zig");
const ObjString = @import("object.zig").ObjString;

count: usize,
capacity: usize,
entries: [*]Entry,

const Self = @This();

const Entry = struct {
    key: *ObjString,
    value: Value,
};

fn init() Self {
    return .{
        .count = 0,
        .capacity = 0,
        .entries = undefined,
    };
}

fn deinit(self: *Self) void {
    _ = self;
}

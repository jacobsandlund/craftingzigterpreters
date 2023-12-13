const std = @import("std");
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;
const Table = @import("table.zig");
const Value = @import("value.zig").Value;

const Allocator = std.mem.Allocator;

backingAllocator: Allocator,
objects: ?*Obj,
strings: Table,

const Self = @This();

pub fn init(backingAllocator: std.mem.Allocator) Self {
    return Self{
        .backingAllocator = backingAllocator,
        .objects = null,
        .strings = Table.init(backingAllocator),
    };
}

pub fn allocator(self: *Self) Allocator {
    return self.backingAllocator;
}

pub fn deinit(self: *Self) void {
    var object: ?*Obj = self.objects;
    while (object) |obj| {
        const next: ?*Obj = obj.next;
        obj.destroyWithAllocator(self.backingAllocator);
        object = next;
    }

    self.strings.deinit();
}

fn trackObject(self: *Self, obj: *Obj) void {
    obj.next = self.objects;
    self.objects = obj;
}

pub fn createString(self: *Self) !*ObjString {
    const string = try self.backingAllocator.create(ObjString);
    self.trackObject(&string.obj);
    return string;
}

pub fn findString(self: *Self, slice: []const u8, hash: u32) ?*ObjString {
    return self.strings.findString(slice, hash);
}

pub fn storeString(self: *Self, string: *ObjString) !void {
    _ = try self.strings.set(string, Value.nil);
}

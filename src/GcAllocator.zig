const std = @import("std");
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;

const Allocator = std.mem.Allocator;

backingAllocator: Allocator,
objects: ?*Obj,

const Self = @This();

pub fn init(backingAllocator: std.mem.Allocator) Self {
    return Self{
        .backingAllocator = backingAllocator,
        .objects = null,
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
}

fn trackObject(self: *Self, obj: *Obj) void {
    obj.next = self.objects;
    self.objects = obj;
}

pub fn createString(self: *Self, capacity: usize) !*ObjString {
    const string = try self.backingAllocator.create(ObjString);
    string.* = try ObjString.initCapacity(self.backingAllocator, capacity);
    self.trackObject(&string.obj);
    return string;
}

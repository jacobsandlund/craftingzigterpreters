const std = @import("std");
const object = @import("object.zig");
const Table = @import("table.zig");
const Value = @import("value.zig").Value;

const Allocator = std.mem.Allocator;
const Obj = object.Obj;
const ObjClosure = object.ObjClosure;
const ObjFunction = object.ObjFunction;
const ObjNative = object.ObjNative;
const ObjString = object.ObjString;
const ObjUpvalue = object.ObjUpvalue;

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
    var obj: ?*Obj = self.objects;
    while (obj) |o| {
        const next: ?*Obj = o.next;
        o.destroyWithAllocator(self.backingAllocator);
        obj = next;
    }

    self.strings.deinit();
}

fn trackObject(self: *Self, obj: *Obj) void {
    obj.next = self.objects;
    self.objects = obj;
}

pub fn createClosure(self: *Self) !*ObjClosure {
    const closure = try self.backingAllocator.create(ObjClosure);
    self.trackObject(&closure.obj);
    return closure;
}

pub fn createFunction(self: *Self) !*ObjFunction {
    const function = try self.backingAllocator.create(ObjFunction);
    self.trackObject(&function.obj);
    return function;
}

pub fn createNative(self: *Self) !*ObjNative {
    const native = try self.backingAllocator.create(ObjNative);
    self.trackObject(&native.obj);
    return native;
}

pub fn createString(self: *Self) !*ObjString {
    const string = try self.backingAllocator.create(ObjString);
    self.trackObject(&string.obj);
    return string;
}

pub fn createUpvalue(self: *Self) !*ObjUpvalue {
    const upvalue = try self.backingAllocator.create(ObjUpvalue);
    self.trackObject(&upvalue.obj);
    return upvalue;
}

pub fn findString(self: *Self, slice: []const u8, hash: u32) ?*ObjString {
    return self.strings.findString(slice, hash);
}

pub fn storeString(self: *Self, string: *ObjString) !void {
    _ = try self.strings.set(string, Value.nil);
}

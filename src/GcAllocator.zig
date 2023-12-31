const std = @import("std");
const object = @import("object.zig");
const Table = @import("table.zig");
const Value = @import("value.zig").Value;
const ValueArray = @import("value.zig").ValueArray;
const common = @import("common.zig");
const Vm = @import("vm.zig");
const compiler = @import("compiler.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Obj = object.Obj;
const ObjClosure = object.ObjClosure;
const ObjFunction = object.ObjFunction;
const ObjNative = object.ObjNative;
const ObjString = object.ObjString;
const ObjUpvalue = object.ObjUpvalue;
const DEBUG_STRESS_GC = common.DEBUG_STRESS_GC;
const DEBUG_LOG_GC = common.DEBUG_LOG_GC;

backingAllocator: Allocator,
objects: ?*Obj,
strings: Table,
vm: *Vm,
grayStack: ArrayList(*Obj),

const Self = @This();

pub fn init(backingAllocator: std.mem.Allocator) Self {
    return Self{
        .backingAllocator = backingAllocator,
        .objects = null,
        .strings = Table.init(backingAllocator),
        .vm = undefined,
        .grayStack = ArrayList(*Obj).init(backingAllocator),
    };
}

fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));
    if (DEBUG_STRESS_GC) {
        self.collectGarbage();
    }
    return self.backingAllocator.rawAlloc(len, ptr_align, ret_addr);
}

fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
    const self: *Self = @ptrCast(@alignCast(ctx));
    if (new_len > buf.len) {
        if (DEBUG_STRESS_GC) {
            self.collectGarbage();
        }
    }
    return self.backingAllocator.rawResize(buf, buf_align, new_len, ret_addr);
}

fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    return self.backingAllocator.rawFree(buf, buf_align, ret_addr);
}

fn create(self: *Self, comptime T: type) !*T {
    if (DEBUG_STRESS_GC) {
        self.collectGarbage();
    }

    const ptr = try self.backingAllocator.create(T);

    if (DEBUG_LOG_GC) {
        std.debug.print("{*} allocate {d} bytes\n", .{ ptr, @sizeOf(T) });
    }

    return ptr;
}

pub fn destroy(self: *Self, ptr: anytype) void {
    if (DEBUG_LOG_GC) {
        std.debug.print("{*} free\n", .{ptr});
    }

    self.backingAllocator.destroy(ptr);
}

pub fn allocator(self: *Self) Allocator {
    return .{
        .ptr = self,
        .vtable = &.{
            .alloc = alloc,
            .resize = resize,
            .free = free,
        },
    };
}

pub fn deinit(self: *Self) void {
    var obj: ?*Obj = self.objects;
    while (obj) |o| {
        const next: ?*Obj = o.next;
        o.destroyWithAllocator(self);
        obj = next;
    }

    self.strings.deinit();
    self.grayStack.deinit();
}

fn collectGarbage(self: *Self) void {
    if (DEBUG_LOG_GC) {
        std.debug.print("-- gc begin\n", .{});
    }

    self.markRoots();
    self.traceReferences();
    tableRemoveWhite(&self.strings);
    self.sweep();

    if (DEBUG_LOG_GC) {
        std.debug.print("-- gc end\n", .{});
    }
}

fn markRoots(self: *Self) void {
    var slot: [*]Value = @ptrCast(&self.vm.stack);
    while (slot != self.vm.stackTop) : (slot += 1) {
        self.markValue(slot[0]);
    }

    for (0..self.vm.frameCount) |i| {
        self.markObject(&self.vm.frames[i].closure.obj);
    }

    var upvalue = self.vm.openUpvalues;
    while (upvalue) |u| {
        self.markObject(&u.obj);
        upvalue = u.next;
    }

    self.markTable(&self.vm.globals);
    self.markCompilerRoots();
}

fn markCompilerRoots(self: *Self) void {
    var compiler_ = compiler.current;
    while (compiler_) |c| {
        self.markObject(&c.function.obj);
        compiler_ = c.enclosing;
    }
}

fn markValue(self: *Self, value: Value) void {
    if (value == Value.obj) self.markObject(value.obj);
}

fn markObject(self: *Self, obj: *Obj) void {
    if (obj.isMarked) return;

    if (DEBUG_LOG_GC) {
        std.debug.print("{*} mark ", .{obj});
        obj.printObject(std.io.getStdErr().writer()) catch |err| {
            std.debug.print("Got error: {}\n", .{err});
        };
        std.debug.print("\n", .{});
    }
    obj.isMarked = true;

    self.grayStack.append(obj) catch |err| {
        std.debug.panic("Could not append to grayStack: {}\n", .{err});
    };
}

fn markArray(self: *Self, array: ValueArray) void {
    for (array.values.items) |value| self.markValue(value);
    self.markValue(array.appendingValue);
}

fn markTable(self: *Self, table: *Table) void {
    for (table.entries) |*entry| {
        if (entry.key) |key| {
            self.markObject(&key.obj);
        }

        self.markValue(entry.value);
    }
}

fn traceReferences(self: *Self) void {
    while (self.grayStack.items.len > 0) {
        const obj = self.grayStack.pop();
        self.blackenObject(obj);
    }
}

fn blackenObject(self: *Self, obj: *Obj) void {
    if (DEBUG_LOG_GC) {
        std.debug.print("{*} blacken ", .{obj});
        obj.printObject(std.io.getStdErr().writer()) catch |err| {
            std.debug.print("Got error: {}\n", .{err});
        };
        std.debug.print("\n", .{});
    }

    switch (obj.type) {
        Obj.Type.OBJ_CLOSURE => {
            const closure = obj.closure();
            self.markObject(&closure.function.obj);
            for (closure.upvalues) |upvalue| {
                if (upvalue) |u| self.markObject(&u.obj);
            }
        },
        Obj.Type.OBJ_FUNCTION => {
            const function = obj.function();
            if (function.name) |name| self.markObject(&name.obj);
            self.markArray(function.chunk.constants);
        },
        Obj.Type.OBJ_UPVALUE => {
            self.markValue(obj.upvalue().closed);
        },
        Obj.Type.OBJ_NATIVE, Obj.Type.OBJ_STRING => {},
    }
}

fn tableRemoveWhite(table: *Table) void {
    for (table.entries) |entry| {
        if (entry.key) |key| {
            if (!key.obj.isMarked) {
                if (DEBUG_LOG_GC) {
                    std.debug.print("{*} delete from table ", .{key});
                    key.obj.printObject(std.io.getStdErr().writer()) catch |err| {
                        std.debug.print("Got error: {}\n", .{err});
                    };
                    std.debug.print("\n", .{});
                }
                _ = table.delete(key);
            }
        }
    }
}

fn sweep(self: *Self) void {
    var previous: ?*Obj = null;
    var obj: ?*Obj = self.objects;
    while (obj) |o| {
        if (o.isMarked) {
            o.isMarked = false;
            previous = o;
            obj = o.next;
        } else {
            const unreached = o;
            obj = o.next;
            if (previous) |prev| {
                prev.next = obj;
            } else {
                self.objects = obj;
            }

            unreached.destroyWithAllocator(self);
        }
    }
}

fn trackObject(self: *Self, obj: *Obj, objType: Obj.Type) void {
    obj.type = objType;
    obj.isMarked = false;
    obj.next = self.objects;

    self.objects = obj;
}

pub fn createClosure(self: *Self) !*ObjClosure {
    const closure = try self.create(ObjClosure);
    self.trackObject(&closure.obj, Obj.Type.OBJ_CLOSURE);
    return closure;
}

pub fn createFunction(self: *Self) !*ObjFunction {
    const function = try self.create(ObjFunction);
    self.trackObject(&function.obj, Obj.Type.OBJ_FUNCTION);
    return function;
}

pub fn createNative(self: *Self) !*ObjNative {
    const native = try self.create(ObjNative);
    self.trackObject(&native.obj, Obj.Type.OBJ_NATIVE);
    return native;
}

pub fn createString(self: *Self) !*ObjString {
    const string = try self.create(ObjString);
    self.trackObject(&string.obj, Obj.Type.OBJ_STRING);
    return string;
}

pub fn createUpvalue(self: *Self) !*ObjUpvalue {
    const upvalue = try self.create(ObjUpvalue);
    self.trackObject(&upvalue.obj, Obj.Type.OBJ_UPVALUE);
    return upvalue;
}

pub fn findString(self: *Self, slice: []const u8, hash: u32) ?*ObjString {
    return self.strings.findString(slice, hash);
}

pub fn storeString(self: *Self, string: *ObjString) !void {
    _ = try self.strings.set(string, Value.nil);
}

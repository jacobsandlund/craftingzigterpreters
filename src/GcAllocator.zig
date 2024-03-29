const std = @import("std");
const object = @import("object.zig");
const Table = @import("table.zig");
const value = @import("value.zig");
const common = @import("common.zig");
const Vm = @import("vm.zig");
const compiler = @import("compiler.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Obj = object.Obj;
const ObjBoundMethod = object.ObjBoundMethod;
const ObjClass = object.ObjClass;
const ObjClosure = object.ObjClosure;
const ObjFunction = object.ObjFunction;
const ObjInstance = object.ObjInstance;
const ObjNative = object.ObjNative;
const ObjString = object.ObjString;
const ObjUpvalue = object.ObjUpvalue;
const Value = value.Value;
const ValueArray = value.ValueArray;
const DEBUG_STRESS_GC = common.DEBUG_STRESS_GC;
const DEBUG_LOG_GC = common.DEBUG_LOG_GC;

backingAllocator: Allocator,
objects: ?*Obj,
strings: Table,
vm: *Vm,
grayStack: ArrayList(*Obj),
bytesAllocated: usize,
nextGC: usize,
temporaryValue: Value,
ready: bool,

const Self = @This();

const GC_HEAP_GROW_FACTOR = 2;

pub fn init(backingAllocator: std.mem.Allocator) Self {
    return Self{
        .backingAllocator = backingAllocator,
        .objects = null,
        .strings = undefined,
        .vm = undefined,
        .grayStack = ArrayList(*Obj).init(backingAllocator),
        .bytesAllocated = 0,
        .nextGC = 1024 * 1024,
        .temporaryValue = value.nilValue,
        .ready = false,
    };
}

pub fn setup(self: *Self, vm: *Vm) void {
    self.vm = vm;
    self.strings = Table.init(self.allocator());
}

fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
    const self: *Self = @ptrCast(@alignCast(ctx));
    self.bytesAllocated += len;
    if (DEBUG_STRESS_GC) {
        self.collectGarbage();
    }
    if (self.bytesAllocated > self.nextGC) {
        self.collectGarbage();
    }
    return self.backingAllocator.rawAlloc(len, ptr_align, ret_addr);
}

fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
    const self: *Self = @ptrCast(@alignCast(ctx));
    self.bytesAllocated += new_len - buf.len;
    if (DEBUG_STRESS_GC and new_len > buf.len) {
        self.collectGarbage();
    }
    if (self.bytesAllocated > self.nextGC) {
        self.collectGarbage();
    }
    return self.backingAllocator.rawResize(buf, buf_align, new_len, ret_addr);
}

fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
    const self: *Self = @ptrCast(@alignCast(ctx));
    self.bytesAllocated -= buf.len;
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
    if (!self.ready) {
        return;
    }

    const before = self.bytesAllocated;
    if (DEBUG_LOG_GC) {
        std.debug.print("-- gc begin\n", .{});
    }

    self.markRoots();
    self.traceReferences();
    tableRemoveWhite(&self.strings);
    self.sweep();

    self.nextGC = self.bytesAllocated * GC_HEAP_GROW_FACTOR;

    if (DEBUG_LOG_GC) {
        std.debug.print("-- gc end\n", .{});
        std.debug.print("   collected {d} bytes (from {d} to {d}) next at {d}\n", .{ before - self.bytesAllocated, before, self.bytesAllocated, self.nextGC });
    }
}

fn markRoots(self: *Self) void {
    self.markValue(self.temporaryValue);

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
    self.markObject(&self.vm.initString.obj);
    self.markCompilerRoots();
}

fn markCompilerRoots(self: *Self) void {
    var compiler_ = compiler.current;
    while (compiler_) |c| {
        self.markObject(&c.function.obj);
        compiler_ = c.enclosing;
    }
}

fn markValue(self: *Self, val: Value) void {
    if (value.isObj(val)) self.markObject(value.asObj(val));
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
    for (array.values.items) |val| self.markValue(val);
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
        .OBJ_BOUND_METHOD => {
            const bound = obj.boundMethod();
            self.markValue(bound.receiver);
            self.markObject(&bound.method.obj);
        },
        .OBJ_CLASS => {
            const class = obj.class();
            self.markObject(&class.name.obj);
            self.markTable(&class.methods);
        },
        .OBJ_CLOSURE => {
            const closure = obj.closure();
            self.markObject(&closure.function.obj);
            for (closure.upvalues) |upvalue| {
                if (upvalue) |u| self.markObject(&u.obj);
            }
        },
        .OBJ_FUNCTION => {
            const function = obj.function();
            if (function.name) |name| self.markObject(&name.obj);
            self.markArray(function.chunk.constants);
        },
        .OBJ_INSTANCE => {
            const instance = obj.instance();
            self.markObject(&instance.class.obj);
            self.markTable(&instance.fields);
        },
        .OBJ_UPVALUE => {
            self.markValue(obj.upvalue().closed);
        },
        .OBJ_NATIVE, .OBJ_STRING => {},
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

pub fn createBoundMethod(self: *Self) !*ObjBoundMethod {
    const bound = try self.create(ObjBoundMethod);
    self.trackObject(&bound.obj, .OBJ_BOUND_METHOD);
    return bound;
}

pub fn createClass(self: *Self) !*ObjClass {
    const class = try self.create(ObjClass);
    self.trackObject(&class.obj, .OBJ_CLASS);
    return class;
}

pub fn createClosure(self: *Self) !*ObjClosure {
    const closure = try self.create(ObjClosure);
    self.trackObject(&closure.obj, .OBJ_CLOSURE);
    return closure;
}

pub fn createFunction(self: *Self) !*ObjFunction {
    const function = try self.create(ObjFunction);
    self.trackObject(&function.obj, .OBJ_FUNCTION);
    return function;
}

pub fn createInstance(self: *Self) !*ObjInstance {
    const instance = try self.create(ObjInstance);
    self.trackObject(&instance.obj, .OBJ_INSTANCE);
    return instance;
}

pub fn createNative(self: *Self) !*ObjNative {
    const native = try self.create(ObjNative);
    self.trackObject(&native.obj, .OBJ_NATIVE);
    return native;
}

pub fn createString(self: *Self) !*ObjString {
    const string = try self.create(ObjString);
    self.trackObject(&string.obj, .OBJ_STRING);
    return string;
}

pub fn createUpvalue(self: *Self) !*ObjUpvalue {
    const upvalue = try self.create(ObjUpvalue);
    self.trackObject(&upvalue.obj, .OBJ_UPVALUE);
    return upvalue;
}

pub fn findString(self: *Self, slice: []const u8, hash: u32) ?*ObjString {
    return self.strings.findString(slice, hash);
}

pub fn storeString(self: *Self, string: *ObjString) !void {
    self.temporaryValue = value.objValue(&string.obj);
    _ = try self.strings.set(string, value.nilValue);
    self.temporaryValue = value.nilValue;
}

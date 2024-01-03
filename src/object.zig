const std = @import("std");
const GcAllocator = @import("GcAllocator.zig");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;
const Table = @import("table.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const Obj = struct {
    type: Type,
    isMarked: bool,
    next: ?*Obj = null,

    pub const Type = enum {
        OBJ_BOUND_METHOD,
        OBJ_CLASS,
        OBJ_CLOSURE,
        OBJ_FUNCTION,
        OBJ_INSTANCE,
        OBJ_NATIVE,
        OBJ_STRING,
        OBJ_UPVALUE,
    };

    pub fn boundMethod(self: *Obj) *ObjBoundMethod {
        return @fieldParentPtr(ObjBoundMethod, "obj", self);
    }

    pub fn class(self: *Obj) *ObjClass {
        return @fieldParentPtr(ObjClass, "obj", self);
    }

    pub fn closure(self: *Obj) *ObjClosure {
        return @fieldParentPtr(ObjClosure, "obj", self);
    }

    pub fn function(self: *Obj) *ObjFunction {
        return @fieldParentPtr(ObjFunction, "obj", self);
    }

    pub fn instance(self: *Obj) *ObjInstance {
        return @fieldParentPtr(ObjInstance, "obj", self);
    }

    pub fn native(self: *Obj) *ObjNative {
        return @fieldParentPtr(ObjNative, "obj", self);
    }

    pub fn string(self: *Obj) *ObjString {
        return @fieldParentPtr(ObjString, "obj", self);
    }

    pub fn upvalue(self: *Obj) *ObjUpvalue {
        return @fieldParentPtr(ObjUpvalue, "obj", self);
    }

    pub fn printObject(self: *Obj, writer: std.fs.File.Writer) !void {
        switch (self.type) {
            .OBJ_BOUND_METHOD => try self.boundMethod().print(writer),
            .OBJ_CLASS => try self.class().print(writer),
            .OBJ_CLOSURE => try self.closure().print(writer),
            .OBJ_FUNCTION => try self.function().print(writer),
            .OBJ_INSTANCE => try self.instance().print(writer),
            .OBJ_NATIVE => try self.native().print(writer),
            .OBJ_STRING => try self.string().print(writer),
            .OBJ_UPVALUE => try self.upvalue().print(writer),
        }
    }

    pub fn destroyWithAllocator(self: *Obj, allocator: *GcAllocator) void {
        switch (self.type) {
            .OBJ_BOUND_METHOD => {
                const b = self.boundMethod();
                allocator.destroy(b);
            },
            .OBJ_CLASS => {
                const c = self.class();
                c.deinit();
                allocator.destroy(c);
            },
            .OBJ_CLOSURE => {
                const c = self.closure();
                c.deinitWithAllocator(allocator.allocator());
                allocator.destroy(c);
            },
            .OBJ_FUNCTION => {
                const f = self.function();
                f.deinit();
                allocator.destroy(f);
            },
            .OBJ_INSTANCE => {
                const i = self.instance();
                i.deinit();
                allocator.destroy(i);
            },
            .OBJ_NATIVE => {
                const n = self.native();
                allocator.destroy(n);
            },
            .OBJ_STRING => {
                const s = self.string();
                s.deinitWithAllocator(allocator.allocator());
                allocator.destroy(s);
            },
            .OBJ_UPVALUE => {
                const u = self.upvalue();
                allocator.destroy(u);
            },
        }
    }
};

pub const ObjBoundMethod = struct {
    obj: Obj,
    receiver: Value,
    method: *ObjClosure,

    pub fn create(allocator: *GcAllocator, receiver: Value, method: *ObjClosure) !*ObjBoundMethod {
        const bound = try allocator.createBoundMethod();
        bound.receiver = receiver;
        bound.method = method;
        return bound;
    }

    pub fn print(self: ObjBoundMethod, writer: std.fs.File.Writer) !void {
        try self.method.print(writer);
    }
};

pub const ObjClass = struct {
    obj: Obj,
    name: *ObjString,
    methods: Table,

    pub fn create(allocator: *GcAllocator, name: *ObjString) !*ObjClass {
        const class = try allocator.createClass();
        class.name = name;
        class.methods = Table.init(allocator.allocator());
        return class;
    }

    pub fn print(self: ObjClass, writer: std.fs.File.Writer) !void {
        try writer.print("{s}", .{self.name.string});
    }

    pub fn deinit(self: *ObjClass) void {
        self.methods.deinit();
    }
};

pub const ObjClosure = struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,

    pub fn create(allocator: *GcAllocator, function: *ObjFunction) !*ObjClosure {
        const upvalues = try allocator.allocator().alloc(?*ObjUpvalue, function.upvalueCount);
        for (0..function.upvalueCount) |i| {
            upvalues[i] = null;
        }

        const closure = try allocator.createClosure();
        closure.function = function;
        closure.upvalues = upvalues;
        return closure;
    }

    pub fn print(self: ObjClosure, writer: std.fs.File.Writer) !void {
        try self.function.print(writer);
    }

    pub fn deinitWithAllocator(self: *ObjClosure, allocator: Allocator) void {
        allocator.free(self.upvalues);
    }
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: usize,
    upvalueCount: usize,
    chunk: Chunk,
    name: ?*ObjString,

    pub fn create(allocator: *GcAllocator) !*ObjFunction {
        const function = try allocator.createFunction();
        function.arity = 0;
        function.upvalueCount = 0;
        function.name = null;
        function.chunk = Chunk.init(allocator);
        return function;
    }

    pub fn deinit(self: *ObjFunction) void {
        self.chunk.deinit();
    }

    pub fn print(self: ObjFunction, writer: std.fs.File.Writer) !void {
        if (self.name) |name| {
            try writer.print("<fn {s}>", .{name.string});
        } else {
            try writer.print("<script>", .{});
        }
    }
};

pub const ObjInstance = struct {
    obj: Obj,
    class: *ObjClass,
    fields: Table,

    pub fn create(allocator: *GcAllocator, class: *ObjClass) !*ObjInstance {
        const instance = try allocator.createInstance();
        instance.class = class;
        instance.fields = Table.init(allocator.allocator());
        return instance;
    }

    pub fn print(self: *ObjInstance, writer: std.fs.File.Writer) !void {
        try writer.print("{s} instance", .{self.class.name.string});
    }

    pub fn deinit(self: *ObjInstance) void {
        self.fields.deinit();
    }
};

pub const NativeFn = fn (args: []Value) Value;

pub const ObjNative = struct {
    obj: Obj,
    function: *const NativeFn,

    pub fn create(allocator: *GcAllocator, function: *const NativeFn) !*ObjNative {
        const native = try allocator.createNative();
        native.function = function;
        return native;
    }

    pub fn print(_: ObjNative, writer: std.fs.File.Writer) !void {
        try writer.print("<native fn>", .{});
    }
};

pub const ObjString = struct {
    obj: Obj,
    string: []const u8,
    hash: u32,

    pub fn copyString(allocator: *GcAllocator, slice: []const u8) !*ObjString {
        const hash = hashString(slice);
        const interned: ?*ObjString = allocator.findString(slice, hash);
        if (interned) |i| return i;

        return create(allocator, try allocator.allocator().dupe(u8, slice), hash);
    }

    pub fn takeString(allocator: *GcAllocator, slice: []const u8) !*ObjString {
        const hash = hashString(slice);
        const interned: ?*ObjString = allocator.findString(slice, hash);
        if (interned) |i| {
            allocator.allocator().free(slice);
            return i;
        }

        return create(allocator, slice, hash);
    }

    fn create(allocator: *GcAllocator, slice: []const u8, hash: u32) !*ObjString {
        const string = try allocator.createString();
        string.string = slice;
        string.hash = hash;
        try allocator.storeString(string);
        return string;
    }

    pub fn deinitWithAllocator(self: *ObjString, allocator: Allocator) void {
        allocator.free(self.string);
    }

    pub fn print(self: ObjString, writer: std.fs.File.Writer) !void {
        try writer.print("{s}", .{self.string});
    }
};

pub const ObjUpvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,

    pub fn create(allocator: *GcAllocator, slot: *Value) !*ObjUpvalue {
        const upvalue = try allocator.createUpvalue();
        upvalue.location = slot;
        upvalue.closed = Value.nil;
        upvalue.next = null;
        return upvalue;
    }

    pub fn print(_: ObjUpvalue, writer: std.fs.File.Writer) !void {
        try writer.print("upvalue", .{});
    }
};

fn hashString(slice: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (slice) |char| {
        hash ^= char;
        hash *%= 16777619;
    }
    return hash;
}

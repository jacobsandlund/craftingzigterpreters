const std = @import("std");
const GcAllocator = @import("GcAllocator.zig");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const Obj = struct {
    type: Type,
    next: ?*Obj = null,

    pub const Type = enum {
        OBJ_CLOSURE,
        OBJ_FUNCTION,
        OBJ_NATIVE,
        OBJ_STRING,
        OBJ_UPVALUE,
    };

    pub fn closure(self: *Obj) *ObjClosure {
        return @fieldParentPtr(ObjClosure, "obj", self);
    }

    pub fn function(self: *Obj) *ObjFunction {
        return @fieldParentPtr(ObjFunction, "obj", self);
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
            .OBJ_CLOSURE => try self.closure().print(writer),
            .OBJ_FUNCTION => try self.function().print(writer),
            .OBJ_NATIVE => try self.native().print(writer),
            .OBJ_STRING => try self.string().print(writer),
            .OBJ_UPVALUE => try self.upvalue().print(writer),
        }
    }

    pub fn destroyWithAllocator(self: *Obj, allocator: Allocator) void {
        switch (self.type) {
            .OBJ_CLOSURE => {
                const c = self.closure();
                c.deinitWithAllocator(allocator);
                allocator.destroy(c);
            },
            .OBJ_FUNCTION => {
                const f = self.function();
                f.deinitWithAllocator(allocator);
                allocator.destroy(f);
            },
            .OBJ_NATIVE => {
                const n = self.native();
                allocator.destroy(n);
            },
            .OBJ_STRING => {
                const s = self.string();
                s.deinitWithAllocator(allocator);
                allocator.destroy(s);
            },
            .OBJ_UPVALUE => {
                const u = self.upvalue();
                allocator.destroy(u);
            },
        }
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
        closure.obj.type = Obj.Type.OBJ_CLOSURE;
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
        function.obj.type = Obj.Type.OBJ_FUNCTION;
        function.arity = 0;
        function.upvalueCount = 0;
        function.name = null;
        function.chunk = Chunk.init(allocator.allocator());
        return function;
    }

    pub fn deinitWithAllocator(self: *ObjFunction, _: Allocator) void {
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

pub const NativeFn = fn (args: []Value) Value;

pub const ObjNative = struct {
    obj: Obj,
    function: *const NativeFn,

    pub fn create(allocator: *GcAllocator, function: *const NativeFn) !*ObjNative {
        const native = try allocator.createNative();
        native.obj.type = Obj.Type.OBJ_NATIVE;
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
        string.obj.type = Obj.Type.OBJ_STRING;
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

    pub fn create(allocator: *GcAllocator, slot: *Value) !*ObjUpvalue {
        const upvalue = try allocator.createUpvalue();
        upvalue.obj.type = Obj.Type.OBJ_UPVALUE;
        upvalue.location = slot;
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

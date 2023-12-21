const std = @import("std");
const GcAllocator = @import("GcAllocator.zig");
const Chunk = @import("chunk.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const Obj = struct {
    type: Type,
    next: ?*Obj = null,

    pub const Type = enum {
        OBJ_STRING,
        OBJ_FUNCTION,
    };

    pub fn string(self: *Obj) *ObjString {
        return @fieldParentPtr(ObjString, "obj", self);
    }

    pub fn function(self: *Obj) *ObjFunction {
        return @fieldParentPtr(ObjFunction, "obj", self);
    }

    pub fn printObject(self: *Obj, writer: std.fs.File.Writer) !void {
        switch (self.type) {
            .OBJ_STRING => try self.string().print(writer),
            .OBJ_FUNCTION => try self.function().print(writer),
        }
    }

    pub fn destroyWithAllocator(self: *Obj, allocator: Allocator) void {
        switch (self.type) {
            .OBJ_STRING => {
                const s = self.string();
                s.deinitWithAllocator(allocator);
                allocator.destroy(s);
            },
            .OBJ_FUNCTION => {
                const f = self.function();
                f.deinitWithAllocator(allocator);
                allocator.destroy(f);
            },
        }
    }
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: isize,
    chunk: Chunk,
    name: ?*ObjString,

    pub fn create(allocator: *GcAllocator) !*ObjFunction {
        const function = try allocator.createFunction();
        function.obj.type = Obj.Type.OBJ_FUNCTION;
        function.arity = 0;
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

fn hashString(slice: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (slice) |char| {
        hash ^= char;
        hash *%= 16777619;
    }
    return hash;
}

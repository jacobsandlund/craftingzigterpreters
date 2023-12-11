const std = @import("std");
const GcAllocator = @import("GcAllocator.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const Obj = struct {
    type: Type,
    next: ?*Obj = null,

    pub const Type = enum {
        OBJ_STRING,
    };

    pub fn string(self: *Obj) *ObjString {
        return @fieldParentPtr(ObjString, "obj", self);
    }

    pub fn printObject(self: *Obj, writer: std.fs.File.Writer) !void {
        switch (self.type) {
            .OBJ_STRING => try self.string().print(writer),
        }
    }

    pub fn equal(a: *Obj, b: *Obj) bool {
        // TODO: don't assume string
        const aString = a.string().string;
        const bString = b.string().string;
        return std.mem.eql(u8, aString, bString);
    }

    pub fn destroyWithAllocator(self: *Obj, allocator: Allocator) void {
        switch (self.type) {
            .OBJ_STRING => {
                const s = self.string();
                s.deinitWithAllocator(allocator);
                allocator.destroy(s);
            },
        }
    }
};

pub const ObjString = struct {
    obj: Obj,
    string: []const u8,

    pub fn copyString(allocator: *GcAllocator, slice: []const u8) !*ObjString {
        return createString(allocator, try allocator.allocator().dupe(u8, slice));
    }

    pub fn takeString(allocator: *GcAllocator, slice: []const u8) !*ObjString {
        return createString(allocator, slice);
    }

    fn createString(allocator: *GcAllocator, slice: []const u8) !*ObjString {
        const string = try allocator.createString();
        string.string = slice;
        string.obj.type = Obj.Type.OBJ_STRING;
        return string;
    }

    pub fn deinitWithAllocator(self: *ObjString, allocator: Allocator) void {
        allocator.free(self.string);
    }

    pub fn print(self: ObjString, writer: std.fs.File.Writer) !void {
        try writer.print("{s}", .{self.string});
    }
};

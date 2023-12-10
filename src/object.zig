const std = @import("std");

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
        return std.mem.eql(u8, aString.items, bString.items);
    }

    pub fn destroyWithAllocator(self: *Obj, allocator: Allocator) void {
        switch (self.type) {
            .OBJ_STRING => {
                const s = self.string();
                s.deinit();
                allocator.destroy(s);
            },
        }
    }
};

pub const ObjString = struct {
    obj: Obj,
    string: ArrayList(u8),

    pub fn initCapacity(allocator: Allocator, num: usize) !ObjString {
        return ObjString{
            .obj = Obj{
                .type = Obj.Type.OBJ_STRING,
            },
            .string = try ArrayList(u8).initCapacity(allocator, num),
        };
    }

    pub fn deinit(self: *ObjString) void {
        self.string.deinit();
    }

    pub fn print(self: ObjString, writer: std.fs.File.Writer) !void {
        try writer.print("{s}", .{self.string.items});
    }

    pub fn appendSliceAssumeCapacity(self: *ObjString, items: []const u8) void {
        self.string.appendSliceAssumeCapacity(items);
    }
};

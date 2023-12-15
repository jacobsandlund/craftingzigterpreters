const std = @import("std");
const Value = @import("value.zig").Value;
const ObjString = @import("object.zig").ObjString;

const Allocator = std.mem.Allocator;

allocator: Allocator,
count: usize,
entries: []Entry,

const Self = @This();

const TABLE_MAX_LOAD: f64 = 0.75;

const Entry = struct {
    key: ?*ObjString,
    value: Value,
};

const emptyEntries = &[0]Entry{};

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .count = 0,
        .entries = emptyEntries,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.entries);
}

inline fn maxCount(capacity: usize) usize {
    return @intFromFloat(@as(f64, @floatFromInt(capacity)) * TABLE_MAX_LOAD);
}

pub fn set(self: *Self, key: *ObjString, value: Value) !bool {
    if (self.count + 1 > maxCount(self.entries.len)) {
        try self.adjustCapacity();
    }

    var entry: *Entry = findEntry(self.entries, key);
    const isNewKey = entry.key == null;
    if (isNewKey and entry.value == Value.nil) {
        self.count += 1;
    }

    entry.key = key;
    entry.value = value;
    return isNewKey;
}

pub fn addAll(self: *Self, from: *Self) void {
    for (from.entries) |entry| {
        if (entry.key) |key| {
            self.set(key, entry.value);
        }
    }
}

pub fn get(self: *Self, key: *ObjString) ?Value {
    if (self.count == 0) return null;

    const entry = findEntry(self.entries, key);
    if (entry.key == null) return null;

    return entry.value;
}

pub fn findString(self: *Self, slice: []const u8, hash: u32) ?*ObjString {
    if (self.count == 0) return null;

    var index: u32 = @truncate(hash % self.entries.len);
    while (true) : (index = @truncate((index + 1) % self.entries.len)) {
        const entry: *Entry = &self.entries[index];
        if (entry.key) |key| {
            if (key.hash == hash and std.mem.eql(u8, key.string, slice)) {
                // We found it.
                return key;
            }
        } else {
            // Stop if we find an empty non-tombstone entry.
            if (entry.value == Value.nil) return null;
        }
    }
}

pub fn delete(self: *Self, key: *ObjString) bool {
    if (self.count == 0) return false;

    const entry = findEntry(self.entries, key);
    if (entry.key == null) return false;

    entry.key = null;
    entry.value = Value{ .boolean = true };
    return true;
}

fn adjustCapacity(self: *Self) !void {
    const capacity = if (self.entries.len < 8) 8 else self.entries.len * 2;
    var entries = try self.allocator.alloc(Entry, capacity);
    for (entries) |*entry| {
        entry.key = null;
        entry.value = Value.nil;
    }

    self.count = 0;
    for (self.entries) |entry| {
        if (entry.key) |key| {
            var dest = findEntry(entries, key);
            dest.key = key;
            dest.value = entry.value;
            self.count += 1;
        }
    }

    self.allocator.free(self.entries);
    self.entries = entries;
}

fn findEntry(entries: []Entry, key: *ObjString) *Entry {
    var index: u32 = @truncate(key.hash % entries.len);
    var tombstone: ?*Entry = null;

    while (true) : (index = @truncate((index + 1) % entries.len)) {
        const entry = &entries[index];
        if (entry.key) |entryKey| {
            if (entryKey == key) {
                // We found the key.
                return entry;
            }
        } else {
            if (entry.value == Value.nil) {
                // Empty entry.
                return if (tombstone) |t| t else entry;
            } else {
                // We found a tombstone.
                if (tombstone == null) tombstone = entry;
            }
        }
    }
}

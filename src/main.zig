const std = @import("std");
const Chunk = @import("chunk.zig");

const panic = std.debug.panic;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) {
            panic("Leak detected!", .{});
        }
    }

    const allocator = gpa.allocator();
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    chunk.writeOpCode(Chunk.OpCode.OP_RETURN) catch |err| {
        panic("Failed to write op code: {}", .{err});
    };

    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
}

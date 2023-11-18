const std = @import("std");
const Chunk = @import("chunk.zig");
const debug = @import("debug.zig");

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

    try chunk.writeOpCode(Chunk.OpCode.OP_RETURN);
    debug.dissassembleChunk(chunk, "test chunk");
}

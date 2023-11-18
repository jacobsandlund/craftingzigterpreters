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

    const constant = try chunk.addConstant(.{ .f64 = 1.2 });
    try chunk.writeOpCode(Chunk.OpCode.OP_CONSTANT, 123);
    try chunk.writeByte(constant, 123);

    try chunk.writeOpCode(Chunk.OpCode.OP_RETURN, 123);

    debug.dissassembleChunk(chunk, "test chunk");
}

const std = @import("std");
const Chunk = @import("chunk.zig");

const OpCode = Chunk.OpCode;

const print = std.debug.print;

pub fn dissassembleChunk(chunk: Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;

    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

fn disassembleInstruction(chunk: Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});

    const instruction: OpCode = @enumFromInt(chunk.code.items[offset]);
    switch (instruction) {
        OpCode.OP_RETURN => {
            return simpleInstruction("OP_RETURN", offset);
        },
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}

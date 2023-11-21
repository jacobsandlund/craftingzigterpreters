const std = @import("std");
const Chunk = @import("chunk.zig");

const OpCode = Chunk.OpCode;
const print = std.debug.print;

pub fn dissassembleChunk(chunk: *Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;

    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        print("   | ", .{});
    } else {
        print("{d:4} ", .{chunk.lines.items[offset]});
    }

    const instruction: OpCode = @enumFromInt(chunk.code.items[offset]);
    switch (instruction) {
        OpCode.OP_CONSTANT => {
            return constantInstruction("OP_CONSTANT", chunk, offset);
        },
        OpCode.OP_RETURN => {
            return simpleInstruction("OP_RETURN", offset);
        },
        _ => {
            print("Unknown opcode {d:4}\n", .{instruction});
            return offset + 1;
        },
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    print("{s:<16} {d:4} '", .{ name, constant });
    chunk.constants.values.items[constant].print();
    print("'\n", .{});
    return offset + 2;
}

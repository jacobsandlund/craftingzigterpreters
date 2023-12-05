const std = @import("std");
const Chunk = @import("chunk.zig");

const Writer = std.fs.File.Writer;
const OpCode = Chunk.OpCode;

pub fn dissassembleChunk(chunk: *Chunk, name: []const u8) !void {
    const stderr = std.io.getStdErr();
    const writer = stderr.writer();
    try writer.print("== {s} ==\n", .{name});

    var offset: usize = 0;

    while (offset < chunk.code.items.len) {
        offset = try disassembleInstruction(writer, chunk, offset);
    }
}

pub fn disassembleInstruction(writer: Writer, chunk: *Chunk, offset: usize) !usize {
    try writer.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        try writer.print("   | ", .{});
    } else {
        try writer.print("{d:4} ", .{chunk.lines.items[offset]});
    }

    const instruction: OpCode = @enumFromInt(chunk.code.items[offset]);
    switch (instruction) {
        OpCode.OP_CONSTANT => {
            return try constantInstruction(writer, "OP_CONSTANT", chunk, offset);
        },
        OpCode.OP_NIL => {
            return try simpleInstruction(writer, "OP_NIL", offset);
        },
        OpCode.OP_TRUE => {
            return try simpleInstruction(writer, "OP_TRUE", offset);
        },
        OpCode.OP_FALSE => {
            return try simpleInstruction(writer, "OP_FALSE", offset);
        },
        OpCode.OP_EQUAL => {
            return try simpleInstruction(writer, "OP_EQUAL", offset);
        },
        OpCode.OP_GREATER => {
            return try simpleInstruction(writer, "OP_GREATER", offset);
        },
        OpCode.OP_LESS => {
            return try simpleInstruction(writer, "OP_LESS", offset);
        },
        OpCode.OP_ADD => {
            return try simpleInstruction(writer, "OP_ADD", offset);
        },
        OpCode.OP_SUBTRACT => {
            return try simpleInstruction(writer, "OP_SUBTRACT", offset);
        },
        OpCode.OP_MULTIPLY => {
            return try simpleInstruction(writer, "OP_MULTIPLY", offset);
        },
        OpCode.OP_DIVIDE => {
            return try simpleInstruction(writer, "OP_DIVIDE", offset);
        },
        OpCode.OP_NOT => {
            return try simpleInstruction(writer, "OP_NOT", offset);
        },
        OpCode.OP_NEGATE => {
            return try simpleInstruction(writer, "OP_NEGATE", offset);
        },
        OpCode.OP_RETURN => {
            return try simpleInstruction(writer, "OP_RETURN", offset);
        },
        _ => {
            try writer.print("Unknown opcode {d:4}\n", .{instruction});
            return offset + 1;
        },
    }
}

fn simpleInstruction(writer: Writer, name: []const u8, offset: usize) !usize {
    try writer.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(writer: Writer, name: []const u8, chunk: *Chunk, offset: usize) !usize {
    const constant = chunk.code.items[offset + 1];
    try writer.print("{s:<16} {d:4} '", .{ name, constant });
    try chunk.constants.values.items[constant].print(writer);
    try writer.print("'\n", .{});
    return offset + 2;
}

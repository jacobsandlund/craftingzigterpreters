const std = @import("std");
const Chunk = @import("chunk.zig");

const Writer = std.fs.File.Writer;
const OpCode = Chunk.OpCode;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) !void {
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
        .OP_CONSTANT => {
            return try constantInstruction(writer, "OP_CONSTANT", chunk, offset);
        },
        .OP_NIL => {
            return try simpleInstruction(writer, "OP_NIL", offset);
        },
        .OP_TRUE => {
            return try simpleInstruction(writer, "OP_TRUE", offset);
        },
        .OP_FALSE => {
            return try simpleInstruction(writer, "OP_FALSE", offset);
        },
        .OP_POP => {
            return try simpleInstruction(writer, "OP_POP", offset);
        },
        .OP_GET_LOCAL => {
            return try byteInstruction(writer, "OP_GET_LOCAL", chunk, offset);
        },
        .OP_SET_LOCAL => {
            return try byteInstruction(writer, "OP_SET_LOCAL", chunk, offset);
        },
        .OP_GET_GLOBAL => {
            return try constantInstruction(writer, "OP_GET_GLOBAL", chunk, offset);
        },
        .OP_DEFINE_GLOBAL => {
            return try constantInstruction(writer, "OP_DEFINE_GLOBAL", chunk, offset);
        },
        .OP_SET_GLOBAL => {
            return try constantInstruction(writer, "OP_SET_GLOBAL", chunk, offset);
        },
        .OP_GET_UPVALUE => {
            return try byteInstruction(writer, "OP_GET_UPVALUE", chunk, offset);
        },
        .OP_SET_UPVALUE => {
            return try byteInstruction(writer, "OP_SET_UPVALUE", chunk, offset);
        },
        .OP_GET_PROPERTY => {
            return try byteInstruction(writer, "OP_GET_PROPERTY", chunk, offset);
        },
        .OP_SET_PROPERTY => {
            return try byteInstruction(writer, "OP_SET_PROPERTY", chunk, offset);
        },
        .OP_EQUAL => {
            return try simpleInstruction(writer, "OP_EQUAL", offset);
        },
        .OP_GREATER => {
            return try simpleInstruction(writer, "OP_GREATER", offset);
        },
        .OP_LESS => {
            return try simpleInstruction(writer, "OP_LESS", offset);
        },
        .OP_ADD => {
            return try simpleInstruction(writer, "OP_ADD", offset);
        },
        .OP_SUBTRACT => {
            return try simpleInstruction(writer, "OP_SUBTRACT", offset);
        },
        .OP_MULTIPLY => {
            return try simpleInstruction(writer, "OP_MULTIPLY", offset);
        },
        .OP_DIVIDE => {
            return try simpleInstruction(writer, "OP_DIVIDE", offset);
        },
        .OP_NOT => {
            return try simpleInstruction(writer, "OP_NOT", offset);
        },
        .OP_NEGATE => {
            return try simpleInstruction(writer, "OP_NEGATE", offset);
        },
        .OP_PRINT => {
            return try simpleInstruction(writer, "OP_PRINT", offset);
        },
        .OP_JUMP => {
            return try jumpInstruction(writer, "OP_JUMP", 1, chunk, offset);
        },
        .OP_JUMP_IF_FALSE => {
            return try jumpInstruction(writer, "OP_JUMP", 1, chunk, offset);
        },
        .OP_LOOP => {
            return try jumpInstruction(writer, "OP_LOOP", -1, chunk, offset);
        },
        .OP_CALL => {
            return try byteInstruction(writer, "OP_CALL", chunk, offset);
        },
        .OP_CLOSURE => {
            var i = offset + 1;
            const constant = chunk.code.items[i];
            i += 1;
            try writer.print("{s:<16} {d:4} ", .{ "OP_CLOSURE", constant });
            const function = chunk.constants.values.items[constant].obj.function();
            try function.print(writer);
            try writer.print("\n", .{});

            for (0..function.upvalueCount) |_| {
                const isLocal = chunk.code.items[i] == 1;
                const index = chunk.code.items[i + 1];
                try writer.print("{d:4}      |                     {s} {d}\n", .{ i, if (isLocal) "local" else "upvalue", index });
                i += 2;
            }

            return i;
        },
        .OP_CLOSE_UPVALUE => {
            return try simpleInstruction(writer, "OP_CLOSE_UPVALUE", offset);
        },
        .OP_RETURN => {
            return try simpleInstruction(writer, "OP_RETURN", offset);
        },
        .OP_CLASS => {
            return constantInstruction(writer, "OP_CLASS", chunk, offset);
        },
        .OP_METHOD => {
            return constantInstruction(writer, "OP_METHOD", chunk, offset);
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

fn byteInstruction(writer: Writer, name: []const u8, chunk: *Chunk, offset: usize) !usize {
    const slot = chunk.code.items[offset + 1];
    try writer.print("{s:<16} {d:4}\n", .{ name, slot });
    return offset + 2;
}

fn constantInstruction(writer: Writer, name: []const u8, chunk: *Chunk, offset: usize) !usize {
    const constant = chunk.code.items[offset + 1];
    try writer.print("{s:<16} {d:4} '", .{ name, constant });
    try chunk.constants.values.items[constant].print(writer);
    try writer.print("'\n", .{});
    return offset + 2;
}

fn jumpInstruction(writer: Writer, name: []const u8, sign: isize, chunk: *Chunk, offset: usize) !usize {
    const jump = (@as(u16, chunk.code.items[offset + 1]) << 8) | (chunk.code.items[offset + 2]);
    try writer.print("{s:<16} {d:4} -> {d}\n", .{ name, offset, @as(isize, @intCast(offset)) + 3 + sign * @as(isize, @intCast(jump)) });
    return offset + 3;
}

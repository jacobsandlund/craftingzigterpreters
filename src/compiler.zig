const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;
const DEBUG_PRINT_CODE = @import("common.zig").DEBUG_PRINT_CODE;
const dissassembleChunk = @import("debug.zig").dissassembleChunk;
const ObjString = @import("object.zig").ObjString;
const GcAllocator = @import("GcAllocator.zig");

const Scanner = @import("scanner.zig");

const Token = Scanner.Token;
const print = std.debug.print;
const OpCode = Chunk.OpCode;
const panic = std.debug.panic;

const Parser = struct {
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,
};

const Precedence = enum {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR, // or
    PREC_AND, // and
    PREC_EQUALITY, // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM, // + -
    PREC_FACTOR, // * /
    PREC_UNARY, // ! -
    PREC_CALL, // . ()
    PREC_PRIMARY,
};

const ParseError = std.mem.Allocator.Error;

const ParseFn = *const fn () ParseError!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

var parser: Parser = undefined;
var scanner: Scanner = undefined;
var compilingChunk: *Chunk = undefined;
var allocator: *GcAllocator = undefined;

const rules = blk: {
    var r: std.EnumArray(Token.Type, ParseRule) = undefined;

    r.set(Token.Type.TOKEN_LEFT_PAREN, .{ .prefix = grouping, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_RIGHT_PAREN, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_LEFT_BRACE, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_RIGHT_BRACE, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_COMMA, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_DOT, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_MINUS, .{ .prefix = unary, .infix = binary, .precedence = Precedence.PREC_TERM });
    r.set(Token.Type.TOKEN_PLUS, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_TERM });
    r.set(Token.Type.TOKEN_SEMICOLON, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_SLASH, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_FACTOR });
    r.set(Token.Type.TOKEN_STAR, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_FACTOR });
    r.set(Token.Type.TOKEN_BANG, .{ .prefix = unary, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_BANG_EQUAL, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_EQUALITY });
    r.set(Token.Type.TOKEN_EQUAL, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_EQUAL_EQUAL, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_EQUALITY });
    r.set(Token.Type.TOKEN_GREATER, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_COMPARISON });
    r.set(Token.Type.TOKEN_GREATER_EQUAL, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_COMPARISON });
    r.set(Token.Type.TOKEN_LESS, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_COMPARISON });
    r.set(Token.Type.TOKEN_LESS_EQUAL, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_COMPARISON });
    r.set(Token.Type.TOKEN_IDENTIFIER, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_STRING, .{ .prefix = string, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_NUMBER, .{ .prefix = number, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_AND, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_CLASS, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_ELSE, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_FALSE, .{ .prefix = literal, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_FOR, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_FUN, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_IF, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_NIL, .{ .prefix = literal, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_OR, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_PRINT, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_RETURN, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_SUPER, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_THIS, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_TRUE, .{ .prefix = literal, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_VAR, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_WHILE, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_ERROR, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(Token.Type.TOKEN_EOF, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });

    break :blk r;
};

pub fn compile(allocator_: *GcAllocator, source: []const u8, chunk: *Chunk) !bool {
    scanner = Scanner.init(source);
    compilingChunk = chunk;
    allocator = allocator_;

    parser.hadError = false;
    parser.panicMode = false;

    advance();
    try expression();
    consume(Token.Type.TOKEN_EOF, "Expect end of expression.");
    try endCompiler();
    return !parser.hadError;
}

fn errorAtCurrent(message: []const u8) void {
    errorAt(&parser.current, message);
}

fn error_(message: []const u8) void {
    errorAt(&parser.previous, message);
}

fn errorAt(token: *Token, message: []const u8) void {
    if (parser.panicMode) return;
    parser.panicMode = true;

    const stderr = std.io.getStdErr();
    const writer = stderr.writer();

    writer.print("[line {d}] Error", .{token.line}) catch panic("Error writing error", .{});

    if (token.type == Token.Type.TOKEN_EOF) {
        writer.print(" at end", .{}) catch panic("Error writing error", .{});
    } else if (token.type == Token.Type.TOKEN_ERROR) {
        // Nothing.
    } else {
        writer.print(" at '{s}'", .{token.slice}) catch panic("Error writing error", .{});
    }

    writer.print(": {s}\n", .{message}) catch panic("Error writing error", .{});
    parser.hadError = true;
}

fn advance() void {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanner.scanToken();
        if (parser.current.type != Token.Type.TOKEN_ERROR) {
            break;
        }

        errorAtCurrent(parser.current.slice);
    }
}

fn consume(tokenType: Token.Type, message: []const u8) void {
    if (parser.current.type == tokenType) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

fn currentChunk() *Chunk {
    return compilingChunk;
}

fn emitByte(byte: u8) ParseError!void {
    try currentChunk().writeByte(byte, parser.previous.line);
}

fn emitBytes(byte1: u8, byte2: u8) ParseError!void {
    try emitByte(byte1);
    try emitByte(byte2);
}

fn emitOpCode(opCode: OpCode) ParseError!void {
    try currentChunk().writeOpCode(opCode, parser.previous.line);
}

fn emitOpCodeWithByte(opCode: OpCode, byte: u8) ParseError!void {
    try emitOpCode(opCode);
    try emitByte(byte);
}

fn emitReturn() ParseError!void {
    try emitOpCode(OpCode.OP_RETURN);
}

fn emitConstant(value: Value) ParseError!void {
    try emitOpCodeWithByte(OpCode.OP_CONSTANT, makeConstant(value));
}

fn makeConstant(value: Value) u8 {
    const constant = currentChunk().addConstant(value) catch {
        error_("Error writing constant: Out of memory");
        return 0;
    };
    if (constant > std.math.maxInt(u8)) {
        error_("Too many constants in one chunk.");
        return 0;
    }

    return @truncate(constant);
}

fn endCompiler() !void {
    try emitReturn();
    if (DEBUG_PRINT_CODE) {
        if (!parser.hadError) {
            _ = try dissassembleChunk(currentChunk(), "code");
        }
    }
}

fn parsePrecedence(precedence: Precedence) ParseError!void {
    advance();
    const prefixRule = rules.get(parser.previous.type).prefix;
    if (prefixRule) |parseFn| {
        try parseFn();
    } else {
        error_("Expect expression.");
        return;
    }

    while (@intFromEnum(precedence) < @intFromEnum(rules.get(parser.current.type).precedence)) {
        advance();
        const infixRule = rules.get(parser.previous.type).infix.?;
        try infixRule();
    }
}

fn expression() ParseError!void {
    try parsePrecedence(Precedence.PREC_ASSIGNMENT);
}

fn grouping() ParseError!void {
    try expression();
    consume(Token.Type.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

fn unary() ParseError!void {
    const operatorType = parser.previous.type;

    // Compile the operand.
    try parsePrecedence(Precedence.PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        Token.Type.TOKEN_BANG => {
            try emitOpCode(OpCode.OP_NOT);
        },
        Token.Type.TOKEN_MINUS => {
            try emitOpCode(OpCode.OP_NEGATE);
        },
        else => {
            unreachable;
        },
    }
}

fn binary() ParseError!void {
    const operatorType = parser.previous.type;
    const rule: ParseRule = rules.get(operatorType);
    try parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

    switch (operatorType) {
        Token.Type.TOKEN_BANG_EQUAL => {
            try emitBytes(@intFromEnum(OpCode.OP_ADD), @intFromEnum(OpCode.OP_NOT));
        },
        Token.Type.TOKEN_EQUAL_EQUAL => {
            try emitOpCode(OpCode.OP_EQUAL);
        },
        Token.Type.TOKEN_GREATER => {
            try emitOpCode(OpCode.OP_GREATER);
        },
        Token.Type.TOKEN_GREATER_EQUAL => {
            try emitBytes(@intFromEnum(OpCode.OP_LESS), @intFromEnum(OpCode.OP_NOT));
        },
        Token.Type.TOKEN_LESS => {
            try emitOpCode(OpCode.OP_LESS);
        },
        Token.Type.TOKEN_LESS_EQUAL => {
            try emitBytes(@intFromEnum(OpCode.OP_GREATER), @intFromEnum(OpCode.OP_NOT));
        },
        Token.Type.TOKEN_PLUS => {
            try emitOpCode(OpCode.OP_ADD);
        },
        Token.Type.TOKEN_MINUS => {
            try emitOpCode(OpCode.OP_SUBTRACT);
        },
        Token.Type.TOKEN_STAR => {
            try emitOpCode(OpCode.OP_MULTIPLY);
        },
        Token.Type.TOKEN_SLASH => {
            try emitOpCode(OpCode.OP_DIVIDE);
        },
        else => {
            unreachable;
        },
    }
}

fn literal() ParseError!void {
    switch (parser.previous.type) {
        Token.Type.TOKEN_FALSE => {
            try emitOpCode(OpCode.OP_FALSE);
        },
        Token.Type.TOKEN_NIL => {
            try emitOpCode(OpCode.OP_NIL);
        },
        Token.Type.TOKEN_TRUE => {
            try emitOpCode(OpCode.OP_TRUE);
        },
        else => {
            unreachable;
        },
    }
}

fn number() ParseError!void {
    const value: f64 = std.fmt.parseFloat(f64, parser.previous.slice) catch {
        error_("Could not parse float.");
        return;
    };
    try emitConstant(Value{ .number = value });
}

fn string() ParseError!void {
    const stringObj = try ObjString.copyString(allocator, parser.previous.slice[1 .. parser.previous.slice.len - 1]);
    try emitConstant(Value{ .obj = &stringObj.obj });
}

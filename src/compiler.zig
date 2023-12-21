const std = @import("std");
const Chunk = @import("chunk.zig");
const Value = @import("value.zig").Value;
const DEBUG_PRINT_CODE = @import("common.zig").DEBUG_PRINT_CODE;
const dissassembleChunk = @import("debug.zig").dissassembleChunk;
const ObjString = @import("object.zig").ObjString;
const ObjFunction = @import("object.zig").ObjFunction;
const GcAllocator = @import("GcAllocator.zig");

const Scanner = @import("scanner.zig");

const Token = Scanner.Token;
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

const ParseFn = *const fn (canAssign: bool) ParseError!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

const Local = struct {
    name: Token,
    depth: isize,
};

const FunctionType = enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
};

const Compiler = struct {
    function: *ObjFunction,
    type: FunctionType,

    locals: [std.math.maxInt(u8) + 1]Local,
    localCount: usize = 0,
    scopeDepth: isize = 0,

    const Self = @This();

    fn init(allocator_: *GcAllocator, functionType: FunctionType) !Self {
        var compiler: Self = .{
            .function = try ObjFunction.create(allocator_),
            .type = functionType,
            .locals = undefined,
        };
        var local = &compiler.locals[0];
        compiler.localCount += 1;
        local.depth = 0;
        local.name.slice = "";
        return compiler;
    }

    fn resolveLocal(self: *Self, name: *const Token) isize {
        var i: isize = @as(isize, @intCast(current.localCount)) - 1;
        while (i >= 0) : (i -= 1) {
            const local = &self.locals[@intCast(i)];
            if (identifiersEqual(name, &local.name)) {
                if (local.depth == -1) {
                    error_("Can't read local variable in its own initializer.");
                }
                return i;
            }
        }

        return -1;
    }
};

var parser: Parser = undefined;
var current: *Compiler = undefined;
var scanner: Scanner = undefined;
var allocator: *GcAllocator = undefined;

fn currentChunk() *Chunk {
    return &current.function.chunk;
}

const rules = blk: {
    var r: std.EnumArray(Token.Type, ParseRule) = undefined;

    r.set(.TOKEN_LEFT_PAREN, .{ .prefix = grouping, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_RIGHT_PAREN, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_LEFT_BRACE, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_RIGHT_BRACE, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_COMMA, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_DOT, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_MINUS, .{ .prefix = unary, .infix = binary, .precedence = Precedence.PREC_TERM });
    r.set(.TOKEN_PLUS, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_TERM });
    r.set(.TOKEN_SEMICOLON, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_SLASH, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_FACTOR });
    r.set(.TOKEN_STAR, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_FACTOR });
    r.set(.TOKEN_BANG, .{ .prefix = unary, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_BANG_EQUAL, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_EQUALITY });
    r.set(.TOKEN_EQUAL, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_EQUAL_EQUAL, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_EQUALITY });
    r.set(.TOKEN_GREATER, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_COMPARISON });
    r.set(.TOKEN_GREATER_EQUAL, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_COMPARISON });
    r.set(.TOKEN_LESS, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_COMPARISON });
    r.set(.TOKEN_LESS_EQUAL, .{ .prefix = null, .infix = binary, .precedence = Precedence.PREC_COMPARISON });
    r.set(.TOKEN_IDENTIFIER, .{ .prefix = variable, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_STRING, .{ .prefix = string, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_NUMBER, .{ .prefix = number, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_AND, .{ .prefix = null, .infix = and_, .precedence = Precedence.PREC_AND });
    r.set(.TOKEN_CLASS, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_ELSE, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_FALSE, .{ .prefix = literal, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_FOR, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_FUN, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_IF, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_NIL, .{ .prefix = literal, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_OR, .{ .prefix = null, .infix = or_, .precedence = Precedence.PREC_OR });
    r.set(.TOKEN_PRINT, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_RETURN, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_SUPER, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_THIS, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_TRUE, .{ .prefix = literal, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_VAR, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_WHILE, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_ERROR, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });
    r.set(.TOKEN_EOF, .{ .prefix = null, .infix = null, .precedence = Precedence.PREC_NONE });

    break :blk r;
};

pub fn compile(allocator_: *GcAllocator, source: []const u8) !?*ObjFunction {
    scanner = Scanner.init(source);
    allocator = allocator_;
    var compiler = try Compiler.init(allocator, .TYPE_SCRIPT);
    current = &compiler;

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(.TOKEN_EOF)) {
        try declaration();
    }

    const function = try endCompiler();
    return if (parser.hadError) null else function;
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

    if (token.type == .TOKEN_EOF) {
        writer.print(" at end", .{}) catch panic("Error writing error", .{});
    } else if (token.type == .TOKEN_ERROR) {
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
        if (parser.current.type != .TOKEN_ERROR) {
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

fn check(tokenType: Token.Type) bool {
    return parser.current.type == tokenType;
}

fn match(tokenType: Token.Type) bool {
    if (!check(tokenType)) return false;

    advance();
    return true;
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
    try emitOpCode(.OP_RETURN);
}

fn emitConstant(value: Value) ParseError!void {
    try emitOpCodeWithByte(.OP_CONSTANT, makeConstant(value));
}

fn emitJump(opCode: OpCode) ParseError!usize {
    try emitOpCode(opCode);
    try emitByte(0xff);
    try emitByte(0xff);
    return currentChunk().code.items.len - 2;
}

fn emitLoop(loopStart: usize) ParseError!void {
    try emitOpCode(.OP_LOOP);

    const offset = currentChunk().code.items.len - loopStart + 2;
    if (offset > std.math.maxInt(u16)) {
        error_("Loop body too large.");
    }

    try emitByte(@intCast(offset >> 8));
    try emitByte(@truncate(offset));
}

fn patchJump(offset: usize) void {
    const jump = currentChunk().code.items.len - offset - 2;

    if (jump > std.math.maxInt(u16)) {
        error_("Too much code to jump over.");
    }

    currentChunk().code.items[offset] = @intCast(jump >> 8);
    currentChunk().code.items[offset + 1] = @truncate(jump);
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

    return @intCast(constant);
}

fn endCompiler() !*ObjFunction {
    try emitReturn();
    const function = current.function;

    if (DEBUG_PRINT_CODE) {
        if (!parser.hadError) {
            _ = try dissassembleChunk(currentChunk(), if (function.name) |name| name.string else "<script>");
        }
    }

    return function;
}

fn beginScope() void {
    current.scopeDepth += 1;
}

fn endScope() ParseError!void {
    current.scopeDepth -= 1;

    while (current.localCount > 0 and current.locals[current.localCount - 1].depth > current.scopeDepth) {
        try emitOpCode(.OP_POP);
        current.localCount -= 1;
    }
}

fn parsePrecedence(precedence: Precedence) ParseError!void {
    advance();

    const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.PREC_ASSIGNMENT);
    const prefixRule = rules.get(parser.previous.type).prefix;
    if (prefixRule) |parseFn| {
        try parseFn(canAssign);
    } else {
        error_("Expect expression.");
        return;
    }

    while (@intFromEnum(precedence) < @intFromEnum(rules.get(parser.current.type).precedence)) {
        advance();
        const infixRule = rules.get(parser.previous.type).infix.?;
        try infixRule(canAssign);
    }

    if (canAssign and match(.TOKEN_EQUAL)) {
        error_("Invalid assignment target.");
    }
}

fn expression() ParseError!void {
    try parsePrecedence(Precedence.PREC_ASSIGNMENT);
}

fn declaration() ParseError!void {
    if (match(.TOKEN_VAR)) {
        try varDeclaration();
    } else {
        try statement();
    }

    if (parser.panicMode) synchronize();
}

fn varDeclaration() ParseError!void {
    const global: u8 = try parseVariable("Expect variable name.");

    if (match(.TOKEN_EQUAL)) {
        try expression();
    } else {
        try emitOpCode(.OP_NIL);
    }
    consume(.TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    try defineVariable(global);
}

fn statement() ParseError!void {
    if (match(.TOKEN_PRINT)) {
        try printStatement();
    } else if (match(.TOKEN_FOR)) {
        try forStatment();
    } else if (match(.TOKEN_IF)) {
        try ifStatement();
    } else if (match(.TOKEN_WHILE)) {
        try whileStatement();
    } else if (match(.TOKEN_LEFT_BRACE)) {
        beginScope();
        try block();
        try endScope();
    } else {
        try expressionStatement();
    }
}

fn synchronize() void {
    parser.panicMode = false;

    while (parser.current.type != .TOKEN_EOF) {
        if (parser.previous.type == .TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            .TOKEN_CLASS, .TOKEN_FUN, .TOKEN_VAR, .TOKEN_FOR, .TOKEN_IF, .TOKEN_WHILE, .TOKEN_PRINT, .TOKEN_RETURN => return,
            else => {
                // Do nothing.
            },
        }
    }
}

fn parseVariable(errorMessage: []const u8) !u8 {
    consume(.TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current.scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

fn identifierConstant(name: *const Token) !u8 {
    const stringObj = try ObjString.copyString(allocator, name.slice);
    return makeConstant(Value{ .obj = &stringObj.obj });
}

fn declareVariable() void {
    if (current.scopeDepth == 0) return;

    const name = &parser.previous;
    var i: isize = @as(isize, @intCast(current.localCount)) - 1;
    while (i >= 0) : (i -= 1) {
        const local = &current.locals[@intCast(i)];
        if (local.depth != -1 and local.depth < current.scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local.name)) {
            error_("Already a variable with this name in this scope.");
        }
    }

    addLocal(name.*);
}

fn identifiersEqual(a: *const Token, b: *const Token) bool {
    return std.mem.eql(u8, a.slice, b.slice);
}

fn addLocal(name: Token) void {
    if (current.localCount == current.locals.len) {
        error_("Too many local variables in function.");
        return;
    }

    var local: *Local = &current.locals[current.localCount];
    current.localCount += 1;
    local.name = name;
    local.depth = -1;
}

fn defineVariable(global: u8) !void {
    if (current.scopeDepth > 0) {
        markInitialized();
        return;
    }

    try emitOpCodeWithByte(.OP_DEFINE_GLOBAL, global);
}

fn markInitialized() void {
    current.locals[current.localCount - 1].depth = current.scopeDepth;
}

fn printStatement() ParseError!void {
    try expression();
    consume(.TOKEN_SEMICOLON, "Expect ';' after value.");
    try emitOpCode(.OP_PRINT);
}

fn forStatment() ParseError!void {
    beginScope();
    consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(.TOKEN_SEMICOLON)) {
        // Empty initializer.
    } else if (match(.TOKEN_VAR)) {
        try varDeclaration();
    } else {
        try expressionStatement();
    }

    var loopStart = currentChunk().code.items.len;
    var exitJump: isize = -1;
    if (!match(.TOKEN_SEMICOLON)) {
        try expression();
        consume(.TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false.
        exitJump = @intCast(try emitJump(.OP_JUMP_IF_FALSE));
        try emitOpCode(.OP_POP);
    }

    if (!match(.TOKEN_RIGHT_PAREN)) {
        const bodyJump = try emitJump(.OP_JUMP);
        const incrementStart = currentChunk().code.items.len;
        try expression();
        try emitOpCode(.OP_POP);
        consume(.TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        try emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    try statement();
    try emitLoop(loopStart);

    if (exitJump != -1) {
        patchJump(@intCast(exitJump));
        try emitOpCode(.OP_POP);
    }

    try endScope();
}

fn ifStatement() ParseError!void {
    consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    try expression();
    consume(.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    const thenJump = try emitJump(.OP_JUMP_IF_FALSE);
    try emitOpCode(.OP_POP);
    try statement();

    const elseJump = try emitJump(.OP_JUMP);

    patchJump(thenJump);
    try emitOpCode(.OP_POP);

    if (match(.TOKEN_ELSE)) try statement();
    patchJump(elseJump);
}

fn whileStatement() ParseError!void {
    const loopStart = currentChunk().code.items.len;
    consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    try expression();
    consume(.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    const exitJump = try emitJump(.OP_JUMP_IF_FALSE);
    try emitOpCode(.OP_POP);
    try statement();
    try emitLoop(loopStart);

    patchJump(exitJump);
    try emitOpCode(.OP_POP);
}

fn block() ParseError!void {
    while (!check(.TOKEN_RIGHT_BRACE) and !check(.TOKEN_EOF)) {
        try declaration();
    }

    consume(.TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

fn expressionStatement() ParseError!void {
    try expression();
    consume(.TOKEN_SEMICOLON, "Expect ';' after expression.");
    try emitOpCode(.OP_POP);
}

fn grouping(_: bool) ParseError!void {
    try expression();
    consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

fn unary(_: bool) ParseError!void {
    const operatorType = parser.previous.type;

    // Compile the operand.
    try parsePrecedence(Precedence.PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        .TOKEN_BANG => {
            try emitOpCode(.OP_NOT);
        },
        .TOKEN_MINUS => {
            try emitOpCode(.OP_NEGATE);
        },
        else => {
            unreachable;
        },
    }
}

fn binary(_: bool) ParseError!void {
    const operatorType = parser.previous.type;
    const rule: ParseRule = rules.get(operatorType);
    try parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

    switch (operatorType) {
        .TOKEN_BANG_EQUAL => {
            try emitBytes(@intFromEnum(OpCode.OP_ADD), @intFromEnum(OpCode.OP_NOT));
        },
        .TOKEN_EQUAL_EQUAL => {
            try emitOpCode(.OP_EQUAL);
        },
        .TOKEN_GREATER => {
            try emitOpCode(.OP_GREATER);
        },
        .TOKEN_GREATER_EQUAL => {
            try emitBytes(@intFromEnum(OpCode.OP_LESS), @intFromEnum(OpCode.OP_NOT));
        },
        .TOKEN_LESS => {
            try emitOpCode(.OP_LESS);
        },
        .TOKEN_LESS_EQUAL => {
            try emitBytes(@intFromEnum(OpCode.OP_GREATER), @intFromEnum(OpCode.OP_NOT));
        },
        .TOKEN_PLUS => {
            try emitOpCode(.OP_ADD);
        },
        .TOKEN_MINUS => {
            try emitOpCode(.OP_SUBTRACT);
        },
        .TOKEN_STAR => {
            try emitOpCode(.OP_MULTIPLY);
        },
        .TOKEN_SLASH => {
            try emitOpCode(.OP_DIVIDE);
        },
        else => unreachable,
    }
}

fn and_(_: bool) ParseError!void {
    const endJump = try emitJump(.OP_JUMP_IF_FALSE);

    try emitOpCode(.OP_POP);
    try parsePrecedence(.PREC_AND);

    patchJump(endJump);
}

fn or_(_: bool) ParseError!void {
    const elseJump = try emitJump(.OP_JUMP_IF_FALSE);
    const endJump = try emitJump(.OP_JUMP);

    patchJump(elseJump);
    try emitOpCode(.OP_POP);

    try parsePrecedence(.PREC_OR);
    patchJump(endJump);
}

fn literal(_: bool) ParseError!void {
    switch (parser.previous.type) {
        .TOKEN_FALSE => {
            try emitOpCode(.OP_FALSE);
        },
        .TOKEN_NIL => {
            try emitOpCode(.OP_NIL);
        },
        .TOKEN_TRUE => {
            try emitOpCode(.OP_TRUE);
        },
        else => {
            unreachable;
        },
    }
}

fn number(_: bool) ParseError!void {
    const value: f64 = std.fmt.parseFloat(f64, parser.previous.slice) catch {
        error_("Could not parse float.");
        return;
    };
    try emitConstant(Value{ .number = value });
}

fn string(_: bool) ParseError!void {
    const stringObj = try ObjString.copyString(allocator, parser.previous.slice[1 .. parser.previous.slice.len - 1]);
    try emitConstant(Value{ .obj = &stringObj.obj });
}

fn variable(canAssign: bool) ParseError!void {
    try namedVariable(parser.previous, canAssign);
}

fn namedVariable(name: Token, canAssign: bool) ParseError!void {
    var getOp: OpCode = undefined;
    var setOp: OpCode = undefined;
    var arg: isize = current.resolveLocal(&name);
    if (arg != -1) {
        getOp = .OP_GET_LOCAL;
        setOp = .OP_SET_LOCAL;
    } else {
        arg = try identifierConstant(&name);
        getOp = .OP_GET_GLOBAL;
        setOp = .OP_SET_GLOBAL;
    }

    if (canAssign and match(.TOKEN_EQUAL)) {
        try expression();
        try emitOpCodeWithByte(setOp, @intCast(arg));
    } else {
        try emitOpCodeWithByte(getOp, @intCast(arg));
    }
}

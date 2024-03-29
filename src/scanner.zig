const std = @import("std");

const Self = @This();

start: [*]u8,
current: [*]u8,
sourceEnd: [*]u8,
line: u32,

pub const Token = struct {
    type: Type,
    slice: []const u8,
    line: u32,

    pub const Type = enum {
        // Single-character tokens.
        TOKEN_LEFT_PAREN,
        TOKEN_RIGHT_PAREN,
        TOKEN_LEFT_BRACE,
        TOKEN_RIGHT_BRACE,
        TOKEN_COMMA,
        TOKEN_DOT,
        TOKEN_MINUS,
        TOKEN_PLUS,
        TOKEN_SEMICOLON,
        TOKEN_SLASH,
        TOKEN_STAR,

        // One or two character tokens.
        TOKEN_BANG,
        TOKEN_BANG_EQUAL,
        TOKEN_EQUAL,
        TOKEN_EQUAL_EQUAL,
        TOKEN_GREATER,
        TOKEN_GREATER_EQUAL,
        TOKEN_LESS,
        TOKEN_LESS_EQUAL,

        // Literals.
        TOKEN_IDENTIFIER,
        TOKEN_STRING,
        TOKEN_NUMBER,

        // Keywords.
        TOKEN_AND,
        TOKEN_CLASS,
        TOKEN_ELSE,
        TOKEN_FALSE,
        TOKEN_FOR,
        TOKEN_FUN,
        TOKEN_IF,
        TOKEN_NIL,
        TOKEN_OR,
        TOKEN_PRINT,
        TOKEN_RETURN,
        TOKEN_SUPER,
        TOKEN_THIS,
        TOKEN_TRUE,
        TOKEN_VAR,
        TOKEN_WHILE,

        TOKEN_ERROR,
        TOKEN_EOF,
    };
};

pub fn init(source: []const u8) Self {
    const start: [*]u8 = @constCast(@ptrCast(source));

    return Self{
        .start = start,
        .current = start,
        .sourceEnd = start + source.len,
        .line = 1,
    };
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or
        (c >= 'A' and c <= 'Z') or
        c == '_';
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

pub fn scanToken(self: *Self) Token {
    self.skipWhitespace();
    self.start = self.current;

    if (self.isAtEnd()) return self.makeToken(.TOKEN_EOF);

    const c: u8 = self.advance();

    if (isAlpha(c)) return self.identifier();
    if (isDigit(c)) return self.number();

    switch (c) {
        '(' => return self.makeToken(.TOKEN_LEFT_PAREN),
        ')' => return self.makeToken(.TOKEN_RIGHT_PAREN),
        '{' => return self.makeToken(.TOKEN_LEFT_BRACE),
        '}' => return self.makeToken(.TOKEN_RIGHT_BRACE),
        ';' => return self.makeToken(.TOKEN_SEMICOLON),
        ',' => return self.makeToken(.TOKEN_COMMA),
        '.' => return self.makeToken(.TOKEN_DOT),
        '-' => return self.makeToken(.TOKEN_MINUS),
        '+' => return self.makeToken(.TOKEN_PLUS),
        '/' => return self.makeToken(.TOKEN_SLASH),
        '*' => return self.makeToken(.TOKEN_STAR),
        '!' => return self.makeToken(if (self.match('=')) .TOKEN_BANG_EQUAL else .TOKEN_BANG),
        '=' => return self.makeToken(if (self.match('=')) .TOKEN_EQUAL_EQUAL else .TOKEN_EQUAL),
        '<' => return self.makeToken(if (self.match('=')) .TOKEN_LESS_EQUAL else .TOKEN_LESS),
        '>' => return self.makeToken(if (self.match('=')) .TOKEN_GREATER_EQUAL else .TOKEN_GREATER),
        '"' => return self.string(),
        else => return self.errorToken("Unexpected character."),
    }
}

fn isAtEnd(self: Self) bool {
    return self.current == self.sourceEnd;
}

fn advance(self: *Self) u8 {
    self.current += 1;
    return (self.current - 1)[0];
}

fn peek(self: Self) u8 {
    return self.current[0];
}

fn peekNext(self: Self) u8 {
    if (self.isAtEnd()) return 0;
    return self.current[1];
}

fn match(self: *Self, expected: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.current[0] != expected) return false;
    self.current += 1;
    return true;
}

fn makeToken(self: Self, tokenType: Token.Type) Token {
    return Token{
        .type = tokenType,
        .slice = self.start[0..(@intFromPtr(self.current) - @intFromPtr(self.start))],
        .line = self.line,
    };
}

fn errorToken(self: Self, message: []const u8) Token {
    return Token{
        .type = .TOKEN_ERROR,
        .slice = message,
        .line = self.line,
    };
}

fn skipWhitespace(self: *Self) void {
    while (!self.isAtEnd()) {
        const c: u8 = self.peek();
        switch (c) {
            ' ', '\r', '\t' => _ = self.advance(),
            '\n' => {
                self.line += 1;
                _ = self.advance();
            },
            '/' => {
                if (self.peekNext() == '/') {
                    // A comment goes until the end of the line.
                    while (!self.isAtEnd() and self.peek() != '\n') _ = self.advance();
                } else {
                    return;
                }
            },
            else => return,
        }
    }
}

fn identifier(self: *Self) Token {
    while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) _ = self.advance();
    return self.makeToken(self.identifierType());
}

fn identifierType(self: Self) Token.Type {
    switch (self.start[0]) {
        'a' => return self.checkKeyword(1, "nd", .TOKEN_AND),
        'c' => return self.checkKeyword(1, "lass", .TOKEN_CLASS),
        'e' => return self.checkKeyword(1, "lse", .TOKEN_ELSE),
        'f' => {
            if (@intFromPtr(self.current) - @intFromPtr(self.start) > 1) {
                switch (self.start[1]) {
                    'a' => return self.checkKeyword(2, "lse", .TOKEN_FALSE),
                    'o' => return self.checkKeyword(2, "r", .TOKEN_FOR),
                    'u' => return self.checkKeyword(2, "n", .TOKEN_FUN),
                    else => return .TOKEN_IDENTIFIER,
                }
            } else return .TOKEN_IDENTIFIER;
        },
        'i' => return self.checkKeyword(1, "f", .TOKEN_IF),
        'n' => return self.checkKeyword(1, "il", .TOKEN_NIL),
        'o' => return self.checkKeyword(1, "r", .TOKEN_OR),
        'p' => return self.checkKeyword(1, "rint", .TOKEN_PRINT),
        'r' => return self.checkKeyword(1, "eturn", .TOKEN_RETURN),
        's' => return self.checkKeyword(1, "uper", .TOKEN_SUPER),
        't' => {
            if (@intFromPtr(self.current) - @intFromPtr(self.start) > 1) {
                switch (self.start[1]) {
                    'h' => return self.checkKeyword(2, "is", .TOKEN_THIS),
                    'r' => return self.checkKeyword(2, "ue", .TOKEN_TRUE),
                    else => return .TOKEN_IDENTIFIER,
                }
            } else return .TOKEN_IDENTIFIER;
        },
        'v' => return self.checkKeyword(1, "ar", .TOKEN_VAR),
        'w' => return self.checkKeyword(1, "hile", .TOKEN_WHILE),
        else => return .TOKEN_IDENTIFIER,
    }
}

fn checkKeyword(self: Self, start: usize, rest: []const u8, tokenType: Token.Type) Token.Type {
    if (@intFromPtr(self.current) - @intFromPtr(self.start) == start + rest.len and std.mem.eql(u8, self.start[start .. start + rest.len], rest)) {
        return tokenType;
    }

    return .TOKEN_IDENTIFIER;
}

fn number(self: *Self) Token {
    while (!self.isAtEnd() and isDigit(self.peek())) _ = self.advance();

    // Look for a fractional part.
    if (!self.isAtEnd() and self.peek() == '.' and isDigit(self.peekNext())) {
        // Consume the ".".
        _ = self.advance();

        while (!self.isAtEnd() and isDigit(self.peek())) _ = self.advance();
    }

    return self.makeToken(.TOKEN_NUMBER);
}

fn string(self: *Self) Token {
    while (!self.isAtEnd() and self.peek() != '"') {
        if (self.peek() == '\n') {
            self.line += 1;
        }
        _ = self.advance();
    }

    if (self.isAtEnd()) return self.errorToken("Unterminated string.");

    // The closing quote
    _ = self.advance();
    return self.makeToken(.TOKEN_STRING);
}

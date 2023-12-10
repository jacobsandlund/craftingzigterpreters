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

    if (self.isAtEnd()) return self.makeToken(Token.Type.TOKEN_EOF);

    const c: u8 = self.advance();

    if (isAlpha(c)) return self.identifier();
    if (isDigit(c)) return self.number();

    switch (c) {
        '(' => {
            return self.makeToken(Token.Type.TOKEN_LEFT_PAREN);
        },
        ')' => {
            return self.makeToken(Token.Type.TOKEN_RIGHT_PAREN);
        },
        '{' => {
            return self.makeToken(Token.Type.TOKEN_LEFT_BRACE);
        },
        '}' => {
            return self.makeToken(Token.Type.TOKEN_LEFT_BRACE);
        },
        ';' => {
            return self.makeToken(Token.Type.TOKEN_SEMICOLON);
        },
        ',' => {
            return self.makeToken(Token.Type.TOKEN_COMMA);
        },
        '.' => {
            return self.makeToken(Token.Type.TOKEN_DOT);
        },
        '-' => {
            return self.makeToken(Token.Type.TOKEN_MINUS);
        },
        '+' => {
            return self.makeToken(Token.Type.TOKEN_PLUS);
        },
        '/' => {
            return self.makeToken(Token.Type.TOKEN_SLASH);
        },
        '*' => {
            return self.makeToken(Token.Type.TOKEN_STAR);
        },
        '!' => {
            return self.makeToken(if (self.match('=')) Token.Type.TOKEN_BANG_EQUAL else Token.Type.TOKEN_BANG);
        },
        '=' => {
            return self.makeToken(if (self.match('=')) Token.Type.TOKEN_EQUAL_EQUAL else Token.Type.TOKEN_EQUAL);
        },
        '<' => {
            return self.makeToken(if (self.match('=')) Token.Type.TOKEN_LESS_EQUAL else Token.Type.TOKEN_LESS);
        },
        '>' => {
            return self.makeToken(if (self.match('=')) Token.Type.TOKEN_GREATER_EQUAL else Token.Type.TOKEN_GREATER);
        },
        '"' => {
            return self.string();
        },
        else => {
            return self.errorToken("Unexpected character.");
        },
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
        .type = Token.Type.TOKEN_ERROR,
        .slice = message,
        .line = self.line,
    };
}

fn skipWhitespace(self: *Self) void {
    while (!self.isAtEnd()) {
        const c: u8 = self.peek();
        switch (c) {
            ' ', '\r', '\t' => {
                _ = self.advance();
            },
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
            else => {
                return;
            },
        }
    }
}

fn identifier(self: *Self) Token {
    while (!self.isAtEnd() and (isAlpha(self.peek()) or isDigit(self.peek()))) _ = self.advance();
    return self.makeToken(self.identifierType());
}

fn identifierType(self: Self) Token.Type {
    switch (self.start[0]) {
        'a' => {
            return self.checkKeyword(1, "nd", Token.Type.TOKEN_AND);
        },
        'c' => {
            return self.checkKeyword(1, "lass", Token.Type.TOKEN_CLASS);
        },
        'e' => {
            return self.checkKeyword(1, "lse", Token.Type.TOKEN_ELSE);
        },
        'f' => {
            if (@intFromPtr(self.current) - @intFromPtr(self.start) > 1) {
                switch (self.start[1]) {
                    'a' => {
                        return self.checkKeyword(2, "lse", Token.Type.TOKEN_FALSE);
                    },
                    'o' => {
                        return self.checkKeyword(2, "r", Token.Type.TOKEN_FOR);
                    },
                    'u' => {
                        return self.checkKeyword(2, "1", Token.Type.TOKEN_FUN);
                    },
                    else => {
                        return Token.Type.TOKEN_IDENTIFIER;
                    },
                }
            } else {
                return Token.Type.TOKEN_IDENTIFIER;
            }
        },
        'i' => {
            return self.checkKeyword(1, "f", Token.Type.TOKEN_IF);
        },
        'n' => {
            return self.checkKeyword(1, "il", Token.Type.TOKEN_NIL);
        },
        'o' => {
            return self.checkKeyword(1, "r", Token.Type.TOKEN_OR);
        },
        'p' => {
            return self.checkKeyword(1, "rint", Token.Type.TOKEN_PRINT);
        },
        'r' => {
            return self.checkKeyword(1, "eturn", Token.Type.TOKEN_RETURN);
        },
        's' => {
            return self.checkKeyword(1, "uper", Token.Type.TOKEN_SUPER);
        },
        't' => {
            if (@intFromPtr(self.current) - @intFromPtr(self.start) > 1) {
                switch (self.start[1]) {
                    'h' => {
                        return self.checkKeyword(2, "is", Token.Type.TOKEN_THIS);
                    },
                    'r' => {
                        return self.checkKeyword(2, "ue", Token.Type.TOKEN_TRUE);
                    },
                    else => {
                        return Token.Type.TOKEN_IDENTIFIER;
                    },
                }
            } else {
                return Token.Type.TOKEN_IDENTIFIER;
            }
        },
        'v' => {
            return self.checkKeyword(1, "ar", Token.Type.TOKEN_VAR);
        },
        'w' => {
            return self.checkKeyword(1, "hile", Token.Type.TOKEN_WHILE);
        },
        else => {
            return Token.Type.TOKEN_IDENTIFIER;
        },
    }
}

fn checkKeyword(self: Self, start: usize, rest: []const u8, tokenType: Token.Type) Token.Type {
    if (@intFromPtr(self.current) - @intFromPtr(self.start) == start + rest.len and std.mem.eql(u8, self.start[start .. start + rest.len], rest)) {
        return tokenType;
    }

    return Token.Type.TOKEN_IDENTIFIER;
}

fn number(self: *Self) Token {
    while (!self.isAtEnd() and isDigit(self.peek())) _ = self.advance();

    // Look for a fractional part.
    if (!self.isAtEnd() and self.peek() == '.' and isDigit(self.peekNext())) {
        // Consume the ".".
        _ = self.advance();

        while (!self.isAtEnd() and isDigit(self.peek())) _ = self.advance();
    }

    return self.makeToken(Token.Type.TOKEN_NUMBER);
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
    return self.makeToken(Token.Type.TOKEN_STRING);
}

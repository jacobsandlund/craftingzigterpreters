const std = @import("std");

const Scanner = @import("scanner.zig");

const Token = Scanner.Token;
const print = std.debug.print;

pub fn compile(source: []const u8) void {
    var scanner = Scanner.init(source);
    var line: isize = -1;

    while (true) {
        const token: Token = scanner.scanToken();
        if (token.line != line) {
            print("{d:4} ", .{token.line});
            line = token.line;
        } else {
            print("   | ", .{});
        }

        print("{} '{s}'\n", .{ token.type, token.slice });

        if (token.type == Token.Type.TOKEN_EOF) {
            break;
        }
    }
}

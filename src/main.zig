const std = @import("std");
const debug = @import("debug.zig");
const Vm = @import("vm.zig");

const panic = std.debug.panic;
const fs = std.fs;

pub fn main() !void {
    const stderr = std.io.getStdErr();
    const writer = stderr.writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) {
            panic("Leak detected!", .{});
        }
    }

    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    // Skip past exe name
    _ = args.skip();

    var hasPath = false;
    var path: []const u8 = undefined;
    if (args.next()) |next| {
        path = try fs.cwd().realpathAlloc(allocator, next);
        hasPath = true;
    }

    if (args.skip()) {
        try writer.print("Usage: craftingzigterpreters [path]\n", .{});
        std.process.exit(64);
    }

    var vm = try Vm.init(allocator);
    try vm.setup();
    defer vm.deinit();

    if (hasPath) {
        try runFile(&vm, allocator, path);
        allocator.free(path);
    } else {
        try repl(&vm);
    }
}

fn repl(vm: *Vm) !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();
    const stderr = std.io.getStdErr();
    const reader = stdin.reader();
    const writer = stdout.writer();
    const errWriter = stderr.writer();

    var line: [1024]u8 = undefined;
    while (true) {
        try writer.print("> ", .{});

        if (try nextLine(reader.any(), &line)) |input| {
            vm.interpret(input) catch |err| {
                try errWriter.print("Got InterpretError: {!}\n", .{err});
            };
        } else {
            return;
        }
    }
}

fn nextLine(reader: std.io.AnyReader, buffer: []u8) !?[]const u8 {
    var fbs = std.io.fixedBufferStream(buffer);
    reader.streamUntilDelimiter(
        fbs.writer(),
        '\n',
        buffer.len,
    ) catch |err| switch (err) {
        error.EndOfStream => if (fbs.getWritten().len == 0) {
            return null;
        },

        else => |e| return e,
    };

    var line = fbs.getWritten();

    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        return std.mem.trimRight(u8, line, "\r");
    } else {
        return line;
    }
}

fn runFile(vm: *Vm, allocator: std.mem.Allocator, path: []const u8) !void {
    const stderr = std.io.getStdErr();
    const errWriter = stderr.writer();

    const source = try readFile(allocator, path);
    defer allocator.free(source);

    vm.interpret(source) catch |err| {
        switch (err) {
            error.CompileError => {
                try errWriter.print("Got CompileError!\n", .{});
                std.process.exit(65);
            },
            error.RuntimeError => {
                try errWriter.print("Got RuntimeError!\n", .{});
                std.process.exit(70);
            },
            else => {
                try errWriter.print("Got WriteError: {!}\n", .{err});
                std.process.exit(75);
            },
        }
    };
}

const MaxFileBytes = (1 << 10) << 10; // 1 Mb

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const file = try fs.openFileAbsolute(path, .{});
    defer file.close();

    return file.readToEndAlloc(allocator, MaxFileBytes);
}

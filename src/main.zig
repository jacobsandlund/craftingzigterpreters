const std = @import("std");
const Chunk = @import("chunk.zig");
const debug = @import("debug.zig");
const Vm = @import("vm.zig");

const panic = std.debug.panic;
const OpCode = Chunk.OpCode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) {
            panic("Leak detected!", .{});
        }
    }

    const allocator = gpa.allocator();

    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    var constant = try chunk.addConstant(.{ .f64 = 1.2 });
    try chunk.writeOpCode(OpCode.OP_CONSTANT, 123);
    try chunk.writeByte(constant, 123);

    constant = try chunk.addConstant(.{ .f64 = 3.4 });
    try chunk.writeOpCode(OpCode.OP_CONSTANT, 123);
    try chunk.writeByte(constant, 123);

    try chunk.writeOpCode(OpCode.OP_ADD, 123);

    constant = try chunk.addConstant(.{ .f64 = 5.6 });
    try chunk.writeOpCode(OpCode.OP_CONSTANT, 123);
    try chunk.writeByte(constant, 123);

    try chunk.writeOpCode(OpCode.OP_DIVIDE, 123);
    try chunk.writeOpCode(OpCode.OP_NEGATE, 123);

    try chunk.writeOpCode(OpCode.OP_RETURN, 123);

    debug.dissassembleChunk(&chunk, "test chunk");

    var vm = Vm.init(&chunk);
    defer vm.deinit();

    const interpret_result = vm.interpret();
    if (interpret_result == Vm.InterpretResult.INTERPRET_COMPILE_ERROR) {
        panic("Compile error!", .{});
    }
    if (interpret_result == Vm.InterpretResult.INTERPRET_RUNTIME_ERROR) {
        panic("Runtime error!", .{});
    }
}

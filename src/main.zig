const std = @import("std");
const pvm = @import("pvm");
const zls = @import("zls");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = pvm.VM.init(allocator);
    defer vm.deinit();

    std.debug.print("=== Arithmetic Program ===\n", .{});
    try vm.execute(&pvm.arithmetic_program);

    // Reset VM for next program
    vm.stack.clearRetainingCapacity();
    vm.memory = std.mem.zeroes([256]i32);

    std.debug.print("\n=== Memory Program ===\n", .{});
    try vm.execute(&pvm.memory_program);

    // Reset VM for next program
    vm.stack.clearRetainingCapacity();
    vm.memory = std.mem.zeroes([256]i32);

    std.debug.print("\n=== Countdown Program ===\n", .{});
    try vm.execute(&pvm.countdown_program);
}

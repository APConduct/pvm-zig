const std = @import("std");
const pvm = @import("pvm");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var vm = pvm.VM.init(allocator);
    defer vm.deinit();

    // Helper function to reset VM
    const resetVM = struct {
        fn reset(v: *pvm.VM) void {
            v.stack.clearRetainingCapacity();
            v.call_stack.clearRetainingCapacity();
            v.memory = std.mem.zeroes([4096]i32);
            v.pc = 0;
            v.sp = 0;
            v.fp = 0;
            v.running = true;
        }
    }.reset;

    std.debug.print("=== Arithmetic Program ===\n", .{});
    try vm.execute(&pvm.arithmetic_program);

    resetVM(&vm);
    std.debug.print("\n=== Memory Program ===\n", .{});
    try vm.execute(&pvm.memory_program);

    resetVM(&vm);
    std.debug.print("\n=== Countdown Program ===\n", .{});
    try vm.execute(&pvm.countdown_program);

    resetVM(&vm);
    std.debug.print("\n=== Function Call Program ===\n", .{});
    try vm.execute(&pvm.function_program);

    resetVM(&vm);
    std.debug.print("\n=== Bitwise Operations Program ===\n", .{});
    try vm.execute(&pvm.bitwise_program);

    resetVM(&vm);
    std.debug.print("\n=== Compile-time Assembled Program ===\n", .{});
    try vm.execute(pvm.compiled_program);

    resetVM(&vm);
    std.debug.print("\n=== Stack Manipulation Demo ===\n", .{});
    const stack_demo = pvm.assemble(.{
        .{ .PUSH, 10 },
        .{ .PUSH, 20 },
        .{ .PUSH, 30 },
        .{.DUP}, // Stack: [10, 20, 30, 30]
        .{.PRINT}, // Print 30
        .{.SWAP}, // Stack: [10, 20, 30, 30] -> [10, 20, 30, 30]
        .{.PRINT}, // Print 30
        .{.POP}, // Stack: [10, 20, 30]
        .{.PRINT}, // Print 30
        .{.HALT},
    });
    try vm.execute(stack_demo);

    resetVM(&vm);
    std.debug.print("\n=== Enhanced Arithmetic Demo ===\n", .{});
    const enhanced_math = pvm.assemble(.{
        .{ .PUSH, 17 },
        .{ .PUSH, 5 },
        .{.MOD}, // 17 % 5 = 2
        .{.PRINT},
        .{ .PUSH, -10 },
        .{.NEG}, // -(-10) = 10
        .{.PRINT},
        .{ .PUSH, 15 },
        .{ .PUSH, 10 },
        .{.GE}, // 15 >= 10 = 1 (true)
        .{.PRINT},
        .{.HALT},
    });
    try vm.execute(enhanced_math);

    resetVM(&vm);
    std.debug.print("\n=== Factorial Program (Recursive Functions) ===\n", .{});
    try vm.execute(&pvm.factorial_program);

    resetVM(&vm);
    std.debug.print("\n=== Advanced Compile-time Assembly Demo ===\n", .{});
    // Simplified example to test basic function calls with compile-time assembly
    const advanced_compiled = pvm.assemble(.{
        // Main program: simple function call test
        .{ .PUSH, 10 },
        .{ .PUSH, 20 },
        .{ .CALL, 5 }, // Call simple add function
        .{.PRINT}, // Print result
        .{.HALT},

        // Simple add function
        .{.ADD}, // Add the two numbers on stack
        .{.RET}, // Return result
    });
    try vm.execute(advanced_compiled);

    resetVM(&vm);
    std.debug.print("\n=== Simple Loop Demo (Compile-time Assembly) ===\n", .{});
    // Simple loop that counts from 1 to 3
    const simple_loop = pvm.assemble(.{
        .{ .PUSH, 1 }, // counter = 1
        .{.PRINT}, // print 1
        .{ .PUSH, 1 }, // increment
        .{.ADD}, // counter = 2
        .{.PRINT}, // print 2
        .{ .PUSH, 1 }, // increment
        .{.ADD}, // counter = 3
        .{.PRINT}, // print 3
        .{.HALT},
    });
    try vm.execute(simple_loop);
}

const std = @import("std");
const print = std.debug.print;
const Vec = std.array_list.Managed;
const Allocator = std.mem.Allocator;

const OpCode = enum(u8) { PUSH, POP, ADD, SUB, MUL, DIV, LOAD, STORE, JMP, JZ, JNZ, EQ, LT, GT, PRINT, HALT };

const Instruction = struct {
    opcode: OpCode,
    operand: ?i32 = null,
};

pub const VMError = error{
    StackUnderflow,
    StackOverflow,
    InvalidMemoryAddress,
    DivisionByZero,
    InvalidInstruction,
    OutOfMemory,
};

pub const VM = struct {
    stack: Vec(i32),
    memory: [256]i32,
    pc: usize,
    sp: usize,
    running: bool,
    allocator: Allocator,

    const STACK_SIZE = 256;

    pub fn init(allocator: Allocator) VM {
        return VM{
            .stack = Vec(i32).init(allocator),
            .memory = std.mem.zeroes([256]i32),
            .pc = 0,
            .sp = 0,
            .running = true,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
    }

    // Stack operations
    fn push(self: *VM, value: i32) VMError!void {
        if (self.stack.items.len >= STACK_SIZE) {
            return VMError.StackOverflow;
        }
        try self.stack.append(value);
        self.sp = self.stack.items.len;
    }

    fn pop(self: *VM) VMError!i32 {
        if (self.stack.items.len == 0) {
            return VMError.StackUnderflow;
        }
        const value = self.stack.pop();
        self.sp = self.stack.items.len;
        return value.?;
    }

    fn peek(self: *VM) VMError!i32 {
        if (self.stack.items.len == 0) {
            return VMError.StackUnderflow;
        }
        return self.stack.items[self.stack.items.len - 1];
    }

    // Execute a single instruction
    fn executeInstruction(self: *VM, instruction: Instruction) VMError!void {
        switch (instruction.opcode) {
            .PUSH => {
                const value = instruction.operand orelse return VMError.InvalidInstruction;
                try self.push(value);
            },

            .POP => {
                _ = try self.pop();
            },

            .ADD => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(a + b);
            },

            .SUB => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(a - b);
            },

            .MUL => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(a * b);
            },

            .DIV => {
                const b = try self.pop();
                if (b == 0) return VMError.DivisionByZero;
                const a = try self.pop();
                try self.push(@divTrunc(a, b));
            },

            .LOAD => {
                const addr = try self.pop();
                if (addr < 0 or addr >= self.memory.len) {
                    return VMError.InvalidMemoryAddress;
                }
                const value = self.memory[@intCast(addr)];
                try self.push(value);
            },

            .STORE => {
                const addr = try self.pop();
                const value = try self.pop();
                if (addr < 0 or addr >= self.memory.len) {
                    return VMError.InvalidMemoryAddress;
                }
                self.memory[@intCast(addr)] = value;
            },

            .JMP => {
                const addr = instruction.operand orelse return VMError.InvalidInstruction;
                if (addr < 0) return VMError.InvalidInstruction;
                self.pc = @intCast(addr);
                return; // Don't increment PC
            },

            .JZ => {
                const value = try self.pop();
                if (value == 0) {
                    const addr = instruction.operand orelse return VMError.InvalidInstruction;
                    if (addr < 0) return VMError.InvalidInstruction;
                    self.pc = @intCast(addr);
                    return; // Don't increment PC
                }
            },

            .JNZ => {
                const value = try self.pop();
                if (value != 0) {
                    const addr = instruction.operand orelse return VMError.InvalidInstruction;
                    if (addr < 0) return VMError.InvalidInstruction;
                    self.pc = @intCast(addr);
                    return; // Don't increment PC
                }
            },

            .EQ => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a == b) 1 else 0);
            },

            .LT => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a < b) 1 else 0);
            },

            .GT => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a > b) 1 else 0);
            },

            .PRINT => {
                const value = try self.peek();
                print("{}\n", .{value});
            },

            .HALT => {
                self.running = false;
            },
        }

        self.pc += 1;
    }

    // Execute a program
    pub fn execute(self: *VM, program: []const Instruction) VMError!void {
        self.pc = 0;
        self.running = true;

        while (self.running and self.pc < program.len) {
            try self.executeInstruction(program[self.pc]);
        }
    }

    // Debug helper
    pub fn printState(self: *VM) void {
        print("PC: {}, SP: {}, Stack: [", .{ self.pc, self.sp });
        for (self.stack.items, 0..) |value, i| {
            if (i > 0) print(", ");
            print("{}", .{value});
        }
        print("]\n");
    }
};

// Example programs

// Program 1: Simple arithmetic (5 + 3) * 2 = 16
pub const arithmetic_program = [_]Instruction{
    .{ .opcode = .PUSH, .operand = 5 },
    .{ .opcode = .PUSH, .operand = 3 },
    .{ .opcode = .ADD },
    .{ .opcode = .PUSH, .operand = 2 },
    .{ .opcode = .MUL },
    .{ .opcode = .PRINT },
    .{ .opcode = .HALT },
};

// Program 2: Memory operations
pub const memory_program = [_]Instruction{
    .{ .opcode = .PUSH, .operand = 42 }, // Value to store
    .{ .opcode = .PUSH, .operand = 10 }, // Memory address
    .{ .opcode = .STORE }, // Store 42 at address 10
    .{ .opcode = .PUSH, .operand = 10 }, // Load address 10
    .{ .opcode = .LOAD }, // Load value from address 10
    .{ .opcode = .PRINT }, // Should print 42
    .{ .opcode = .HALT },
};

// Program 3: Conditional jump (count down from 5)
pub const countdown_program = [_]Instruction{
    .{ .opcode = .PUSH, .operand = 5 }, // 0: Push initial value
    .{ .opcode = .PUSH, .operand = 0 }, // 1: Push memory address 0
    .{ .opcode = .STORE }, // 2: Store counter at memory[0]
    .{ .opcode = .PUSH, .operand = 0 }, // 3: Push address 0 (loop start)
    .{ .opcode = .LOAD }, // 4: Load counter from memory[0]
    .{ .opcode = .PRINT }, // 5: Print current value
    .{ .opcode = .PUSH, .operand = 0 }, // 6: Push address 0
    .{ .opcode = .LOAD }, // 7: Load counter again
    .{ .opcode = .PUSH, .operand = 1 }, // 8: Push 1
    .{ .opcode = .SUB }, // 9: Subtract 1 from counter
    .{ .opcode = .PUSH, .operand = 0 }, // 10: Push address 0
    .{ .opcode = .STORE }, // 11: Store new counter value
    .{ .opcode = .PUSH, .operand = 0 }, // 12: Push address 0
    .{ .opcode = .LOAD }, // 13: Load counter for comparison
    .{ .opcode = .JZ, .operand = 16 }, // 14: If counter is 0, jump to halt
    .{ .opcode = .JMP, .operand = 3 }, // 15: Jump back to loop start
    .{ .opcode = .HALT }, // 16: Halt
};

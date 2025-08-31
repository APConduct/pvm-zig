const std = @import("std");
const print = std.debug.print;
const Vec = std.array_list.Managed;
const Allocator = std.mem.Allocator;

const OpCode = enum(u8) {
    // Basic stack operations
    PUSH,
    POP,
    DUP,
    SWAP,
    // Arithmetic operations
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    NEG,
    // Bitwise operations
    AND,
    OR,
    XOR,
    NOT,
    SHL,
    SHR,
    // Memory operations
    LOAD,
    STORE,
    LOAD_LOCAL,
    STORE_LOCAL,
    // Comparison operations
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,
    // Control flow
    JMP,
    JZ,
    JNZ,
    CALL,
    RET,
    // I/O and system
    PRINT,
    HALT,
};

const Instruction = struct {
    opcode: OpCode,
    operand: ?i32 = null,
};

pub const VMError = error{
    StackUnderflow,
    StackOverflow,
    CallStackUnderflow,
    CallStackOverflow,
    InvalidMemoryAddress,
    InvalidLocalAddress,
    DivisionByZero,
    ModuloByZero,
    InvalidInstruction,
    OutOfMemory,
};

const CallFrame = struct {
    return_address: usize,
    frame_pointer: usize,
};

pub const VM = struct {
    stack: Vec(i32),
    call_stack: Vec(CallFrame),
    memory: [4096]i32,
    pc: usize,
    sp: usize,
    fp: usize, // frame pointer
    running: bool,
    allocator: Allocator,

    const STACK_SIZE = 1024;
    const CALL_STACK_SIZE = 256;
    const MEMORY_SIZE = 4096;

    pub fn init(allocator: Allocator) VM {
        return VM{
            .stack = Vec(i32).init(allocator),
            .call_stack = Vec(CallFrame).init(allocator),
            .memory = std.mem.zeroes([MEMORY_SIZE]i32),
            .pc = 0,
            .sp = 0,
            .fp = 0,
            .running = true,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *VM) void {
        self.stack.deinit();
        self.call_stack.deinit();
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

    // Call stack operations
    fn pushCallFrame(self: *VM, return_address: usize, frame_pointer: usize) VMError!void {
        if (self.call_stack.items.len >= CALL_STACK_SIZE) {
            return VMError.CallStackOverflow;
        }
        try self.call_stack.append(CallFrame{
            .return_address = return_address,
            .frame_pointer = frame_pointer,
        });
    }

    fn popCallFrame(self: *VM) VMError!CallFrame {
        if (self.call_stack.items.len == 0) {
            return VMError.CallStackUnderflow;
        }
        return self.call_stack.orderedRemove(self.call_stack.items.len - 1);
    }

    // Execute a single instruction
    fn executeInstruction(self: *VM, instruction: Instruction) VMError!void {
        switch (instruction.opcode) {
            // Basic stack operations
            .PUSH => {
                const value = instruction.operand orelse return VMError.InvalidInstruction;
                try self.push(value);
            },

            .POP => {
                _ = try self.pop();
            },

            .DUP => {
                const value = try self.peek();
                try self.push(value);
            },

            .SWAP => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(b);
                try self.push(a);
            },

            // Arithmetic operations
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

            .MOD => {
                const b = try self.pop();
                if (b == 0) return VMError.ModuloByZero;
                const a = try self.pop();
                try self.push(@mod(a, b));
            },

            .NEG => {
                const a = try self.pop();
                try self.push(-a);
            },

            // Bitwise operations
            .AND => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(a & b);
            },

            .OR => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(a | b);
            },

            .XOR => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(a ^ b);
            },

            .NOT => {
                const a = try self.pop();
                try self.push(~a);
            },

            .SHL => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(a << @intCast(b));
            },

            .SHR => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(a >> @intCast(b));
            },

            // Memory operations
            .LOAD => {
                const addr = try self.pop();
                if (addr < 0 or addr >= MEMORY_SIZE) {
                    return VMError.InvalidMemoryAddress;
                }
                const value = self.memory[@intCast(addr)];
                try self.push(value);
            },

            .STORE => {
                const addr = try self.pop();
                const value = try self.pop();
                if (addr < 0 or addr >= MEMORY_SIZE) {
                    return VMError.InvalidMemoryAddress;
                }
                self.memory[@intCast(addr)] = value;
            },

            .LOAD_LOCAL => {
                const offset = instruction.operand orelse return VMError.InvalidInstruction;
                const addr = @as(i32, @intCast(self.fp)) + offset;
                if (addr < 0 or addr >= MEMORY_SIZE) {
                    return VMError.InvalidLocalAddress;
                }
                const value = self.memory[@intCast(addr)];
                try self.push(value);
            },

            .STORE_LOCAL => {
                const offset = instruction.operand orelse return VMError.InvalidInstruction;
                const value = try self.pop();
                const addr = @as(i32, @intCast(self.fp)) + offset;
                if (addr < 0 or addr >= MEMORY_SIZE) {
                    return VMError.InvalidLocalAddress;
                }
                self.memory[@intCast(addr)] = value;
            },

            // Control flow
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

            .CALL => {
                const addr = instruction.operand orelse return VMError.InvalidInstruction;
                if (addr < 0) return VMError.InvalidInstruction;
                try self.pushCallFrame(self.pc + 1, self.fp);
                self.fp = self.sp;
                self.pc = @intCast(addr);
                return; // Don't increment PC
            },

            .RET => {
                const frame = try self.popCallFrame();
                self.fp = frame.frame_pointer;
                self.pc = frame.return_address;
                return; // Don't increment PC
            },

            // Comparison operations
            .EQ => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a == b) 1 else 0);
            },

            .NE => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a != b) 1 else 0);
            },

            .LT => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a < b) 1 else 0);
            },

            .LE => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a <= b) 1 else 0);
            },

            .GT => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a > b) 1 else 0);
            },

            .GE => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a >= b) 1 else 0);
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

// Compile-time assembly function
pub fn assemble(comptime program: anytype) []const Instruction {
    const program_info = @typeInfo(@TypeOf(program));
    if (program_info != .@"struct") {
        @compileError("Program must be a tuple (struct)");
    }

    const fields = program_info.@"struct".fields;
    comptime var instructions: [fields.len]Instruction = undefined;

    inline for (fields, 0..) |field, i| {
        const tuple_item = @field(program, field.name);
        const tuple_info = @typeInfo(@TypeOf(tuple_item));

        if (tuple_info != .@"struct") {
            @compileError("Each instruction must be a tuple");
        }

        const tuple_fields = tuple_info.@"struct".fields;
        if (tuple_fields.len < 1 or tuple_fields.len > 2) {
            @compileError("Each instruction tuple must have 1 or 2 elements: {opcode} or {opcode, operand}");
        }

        const opcode = @field(tuple_item, tuple_fields[0].name);
        const operand = if (tuple_fields.len > 1) @field(tuple_item, tuple_fields[1].name) else null;

        instructions[i] = Instruction{
            .opcode = opcode,
            .operand = operand,
        };
    }

    const result = instructions;
    return &result;
}

// Helper macros for cleaner assembly syntax
pub inline fn instr(opcode: OpCode) Instruction {
    return Instruction{ .opcode = opcode };
}

pub inline fn instr_op(opcode: OpCode, operand: i32) Instruction {
    return Instruction{ .opcode = opcode, .operand = operand };
}

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

// Program 4: Function call example
pub const function_program = [_]Instruction{
    // Main function: call add_function(10, 20)
    .{ .opcode = .PUSH, .operand = 10 }, // 0: Push first argument
    .{ .opcode = .PUSH, .operand = 20 }, // 1: Push second argument
    .{ .opcode = .CALL, .operand = 6 }, // 2: Call function at address 6
    .{ .opcode = .PRINT }, // 3: Print result
    .{ .opcode = .HALT }, // 4: Halt
    .{ .opcode = .HALT }, // 5: Safety halt

    // add_function: adds two numbers passed on stack
    .{ .opcode = .STORE_LOCAL, .operand = 1 }, // 6: Store second arg as local[1]
    .{ .opcode = .STORE_LOCAL, .operand = 0 }, // 7: Store first arg as local[0]
    .{ .opcode = .LOAD_LOCAL, .operand = 0 }, // 8: Load first arg
    .{ .opcode = .LOAD_LOCAL, .operand = 1 }, // 9: Load second arg
    .{ .opcode = .ADD }, // 10: Add them
    .{ .opcode = .RET }, // 11: Return (result on stack)
};

// Program 5: Bitwise operations demo
pub const bitwise_program = [_]Instruction{
    .{ .opcode = .PUSH, .operand = 0b1010 }, // 10 in binary
    .{ .opcode = .PUSH, .operand = 0b1100 }, // 12 in binary
    .{ .opcode = .AND }, // 10 & 12 = 8 (0b1000)
    .{ .opcode = .PRINT },
    .{ .opcode = .PUSH, .operand = 0b1010 }, // 10 in binary
    .{ .opcode = .PUSH, .operand = 0b1100 }, // 12 in binary
    .{ .opcode = .OR }, // 10 | 12 = 14 (0b1110)
    .{ .opcode = .PRINT },
    .{ .opcode = .PUSH, .operand = 5 },
    .{ .opcode = .PUSH, .operand = 1 },
    .{ .opcode = .SHL }, // 5 << 1 = 10
    .{ .opcode = .PRINT },
    .{ .opcode = .HALT },
};

// Program 6: Advanced function with local variables (factorial)
pub const factorial_program = [_]Instruction{
    // Main: calculate factorial of 5
    .{ .opcode = .PUSH, .operand = 5 }, // 0: Push argument
    .{ .opcode = .CALL, .operand = 4 }, // 1: Call factorial function
    .{ .opcode = .PRINT }, // 2: Print result
    .{ .opcode = .HALT }, // 3: Halt

    // factorial(n): recursive factorial function
    .{ .opcode = .STORE_LOCAL, .operand = 0 }, // 4: Store n in local[0]
    .{ .opcode = .LOAD_LOCAL, .operand = 0 }, // 5: Load n
    .{ .opcode = .PUSH, .operand = 1 }, // 6: Push 1
    .{ .opcode = .LE }, // 7: Check if n <= 1
    .{ .opcode = .JZ, .operand = 11 }, // 8: If not, jump to recursive case
    .{ .opcode = .PUSH, .operand = 1 }, // 9: Base case: return 1
    .{ .opcode = .RET }, // 10: Return

    // Recursive case: n * factorial(n-1)
    .{ .opcode = .LOAD_LOCAL, .operand = 0 }, // 11: Load n
    .{ .opcode = .LOAD_LOCAL, .operand = 0 }, // 12: Load n again
    .{ .opcode = .PUSH, .operand = 1 }, // 13: Push 1
    .{ .opcode = .SUB }, // 14: n - 1
    .{ .opcode = .CALL, .operand = 4 }, // 15: Call factorial(n-1)
    .{ .opcode = .MUL }, // 16: n * factorial(n-1)
    .{ .opcode = .RET }, // 17: Return result
};

// Compile-time assembled program example
pub const compiled_program = assemble(.{
    .{ .PUSH, 42 },
    .{.DUP},
    .{.PRINT},
    .{.SWAP},
    .{.PRINT},
    .{.HALT},
});

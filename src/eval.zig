const std = @import("std");
const Expression = @import("parser.zig").Expression;
const Operator = @import("parser.zig").Operator;
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const Value = union(enum) {
    STRING: []const u8,
    NUMBER: f64,
    NIL,
    TRUE,
    FALSE,
    ERROR: anyerror,
};

test "evalHappy" {
    const TestCases = struct {
        input: []const u8,
        expected_output: []const u8,
    };
    const test_input = [_]TestCases{
        TestCases{ .input = "1 * 2", .expected_output = "2" },
        TestCases{ .input = "52 + 80 - 94", .expected_output = "38" },
        TestCases{ .input = "78 - 93 * 79 - 32", .expected_output = "-7301" },
        TestCases{ .input = "true", .expected_output = "true" },
        TestCases{ .input = "false", .expected_output = "false" },
        TestCases{ .input = "true", .expected_output = "true" },
        TestCases{ .input = "\"hello world!\"", .expected_output = "hello world!" },
        TestCases{ .input = "10.40", .expected_output = "10.4" },
        TestCases{ .input = "10", .expected_output = "10" },
        TestCases{ .input = "(\"hello world!\")", .expected_output = "hello world!" },
        TestCases{ .input = "(true)", .expected_output = "true" },
        TestCases{ .input = "(10.40)", .expected_output = "10.4" },
        TestCases{ .input = "((false))", .expected_output = "false" },
        TestCases{ .input = "-73", .expected_output = "-73" },
        TestCases{ .input = "!true", .expected_output = "false" },
        TestCases{ .input = "!10.40", .expected_output = "false" },
        TestCases{ .input = "!((false))", .expected_output = "true" },
        TestCases{ .input = "\"hello\" + \" world!\"", .expected_output = "hello world!" },
        TestCases{ .input = "\"42\" + \"24\"", .expected_output = "4224" },
        TestCases{ .input = "\"foo\" + \"bar\"", .expected_output = "foobar" },
        TestCases{ .input = "57 > -65", .expected_output = "true" },
        TestCases{ .input = "11 >= 11", .expected_output = "true" },
        TestCases{ .input = "(54 - 67) >= -(114 / 57 + 11)", .expected_output = "true" },
        TestCases{ .input = "\"hello\" == \"world\"", .expected_output = "false" },
        TestCases{ .input = "\"foo\" != \"bar\"", .expected_output = "true" },
        TestCases{ .input = "\"foo\" == \"foo\"", .expected_output = "true" },
        TestCases{ .input = "61 == \"61\"", .expected_output = "false" },
    };
    for (test_input) |test_case| {
        std.debug.print("test case: {s}\n", .{test_case.input});
        var inputTokens = lexer.Input{ .source = try std.fmt.allocPrint(std.heap.page_allocator, "{s}", .{test_case.input}) };
        const tokens = try lexer.lexer(&inputTokens, true);
        var input = parser.Input{ .source = tokens };
        const expression_tree = try parser.parser(&input, true);
        var buffer: [1024]u8 = undefined;
        var stream = std.io.fixedBufferStream(&buffer);
        const writer = stream.writer();
        const value = try evalulate(&expression_tree, false);
        try printValue(writer, &value);
        try std.testing.expectEqualStrings(test_case.expected_output, stream.buffer[0..stream.pos]);
        std.debug.print("\n\n\n", .{});
    }
}

pub fn printValue(writer: anytype, value: *const Value) !void {
    switch (value.*) {
        .STRING => |string| try writer.print("{s}", .{string}),
        .NUMBER => |number| try writer.print("{d}", .{number}),
        .NIL => try writer.print("nil", .{}),
        .TRUE => try writer.print("true", .{}),
        .FALSE => try writer.print("false", .{}),
        .ERROR => |err| try writer.print("error: {s}", .{@errorName(err)}),
    }
}

fn errorCheck(value: Value) !void {
    switch (value) {
        .ERROR => |err| return err,
        else => {},
    }
}

pub fn evalulate(ast: *const Expression) !Value {
    const value = try eval(ast);
    try errorCheck(value);
    return value;
}

fn eval(ast: *const Expression) error{OutOfMemory}!Value {
    return switch (ast.*) {
        .binary => try evalBinary(ast),
        .unary => try evalUnary(ast),
        .literal => |literal| switch (literal) {
            .NUMBER => Value{ .NUMBER = literal.NUMBER },
            .STRING => Value{ .STRING = literal.STRING },
            .NIL => Value{ .NIL = {} },
            .TRUE => Value{ .TRUE = {} },
            .FALSE => Value{ .FALSE = {} },
        },
        .grouping => |grouping| eval(grouping),
        .identifier => @panic("todo"),
        .parseError => @panic("todo"),
    };
}
fn evalBinary(binary: *const Expression) !Value {
    const left = try eval(binary.binary.left);
    const right = try eval(binary.binary.right);
    std.debug.print("evalBinary: {s}\n", .{binary.binary.operator.stringify()});
    std.debug.print("left: {}\n", .{left});
    std.debug.print("right: {}\n", .{right});
    const value = switch (binary.binary.operator) {
        .PLUS => try evalPlus(left, right),
        .MINUS => try evalMinus(left, right),
        .STAR => try evalStar(left, right),
        .SLASH => try evalSlash(left, right),
        .BANG_EQUAL => try evalBangEqual(left, right),
        .EQUAL_EQUAL => try evalEqualEqual(left, right),
        .LESS => try evalLess(left, right),
        .GREATER => try evalGreater(left, right),
        .LESS_EQUAL => try evalLessEqual(left, right),
        .GREATER_EQUAL => try evalGreaterEqual(left, right),
        else => @panic("we should never get here"),
    };
    std.debug.print("value: {}\n", .{value});
    return value;
}

fn evalMinus(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        return Value{ .NUMBER = left.NUMBER - right.NUMBER };
    }
    return Value{ .ERROR = error.operandNotNumber };
}

fn evalStar(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        return Value{ .NUMBER = left.NUMBER * right.NUMBER };
    }
    return Value{ .ERROR = error.operandNotNumber };
}

fn evalSlash(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        return Value{ .NUMBER = left.NUMBER / right.NUMBER };
    }
    return Value{ .ERROR = error.operandNotNumber };
}

fn evalBangEqual(left: Value, right: Value) !Value {
    if (left == .TRUE and right == .TRUE) {
        return Value{ .FALSE = {} };
    } else if (left == .FALSE and right == .FALSE) {
        return Value{ .FALSE = {} };
    } else if (left == .TRUE and right == .FALSE) {
        return Value{ .TRUE = {} };
    } else if (left == .FALSE and right == .TRUE) {
        return Value{ .TRUE = {} };
    } else if (left == .NUMBER and right == .NUMBER) {
        if (left.NUMBER != right.NUMBER) {
            return Value{ .TRUE = {} };
        }
        return Value{ .FALSE = {} };
    } else if (left == .STRING and right == .STRING) {
        if (!std.mem.eql(u8, left.STRING, right.STRING)) {
            return Value{ .TRUE = {} };
        }
        return Value{ .FALSE = {} };
    }
    @panic("todo");
}

fn evalEqualEqual(left: Value, right: Value) !Value {
    if (left == .TRUE and right == .TRUE) {
        return Value{ .TRUE = {} };
    } else if (left == .FALSE and right == .FALSE) {
        return Value{ .TRUE = {} };
    } else if (left == .TRUE and right == .FALSE) {
        return Value{ .FALSE = {} };
    } else if (left == .FALSE and right == .TRUE) {
        return Value{ .FALSE = {} };
    } else if (left == .NUMBER and right == .NUMBER) {
        if (left.NUMBER == right.NUMBER) {
            return Value{ .TRUE = {} };
        }
        return Value{ .FALSE = {} };
    } else if (left == .STRING and right == .STRING) {
        if (std.mem.eql(u8, left.STRING, right.STRING)) {
            return Value{ .TRUE = {} };
        }
        return Value{ .FALSE = {} };
    }
    return Value{ .FALSE = {} };
}
fn evalLessEqual(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        if (left.NUMBER <= right.NUMBER) {
            return Value{ .TRUE = {} };
        } else {
            return Value{ .FALSE = {} };
        }
    }
    return Value{ .ERROR = error.operandNotNumber };
}

fn evalGreaterEqual(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        if (left.NUMBER >= right.NUMBER) {
            return Value{ .TRUE = {} };
        } else {
            return Value{ .FALSE = {} };
        }
    }

    return Value{ .ERROR = error.operandNotNumber };
}

fn evalLess(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        if (left.NUMBER < right.NUMBER) {
            return Value{ .TRUE = {} };
        } else {
            return Value{ .FALSE = {} };
        }
    }
    return Value{ .ERROR = error.operandNotNumber };
}

fn evalGreater(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        if (left.NUMBER > right.NUMBER) {
            return Value{ .TRUE = {} };
        } else {
            return Value{ .FALSE = {} };
        }
    }
    return Value{ .ERROR = error.operandNotNumber };
}

fn evalPlus(left: Value, right: Value) !Value {
    if (left == .STRING and right == .STRING) {
        return Value{ .STRING = try std.fmt.allocPrint(std.heap.page_allocator, "{s}{s}", .{ left.STRING, right.STRING }) };
    } else if (left == .NUMBER and right == .NUMBER) {
        return Value{ .NUMBER = left.NUMBER + right.NUMBER };
    } else if (left == .STRING and right == .NUMBER) {
        return Value{ .STRING = try std.fmt.allocPrint(std.heap.page_allocator, "{s}{d}", .{ left.STRING, right.NUMBER }) };
    } else if (left == .NUMBER and right == .STRING) {
        return Value{ .STRING = try std.fmt.allocPrint(std.heap.page_allocator, "{d}{s}", .{ left.NUMBER, right.STRING }) };
    }
    return Value{ .ERROR = error.operandsTypeMustMatch };
}

fn evalUnary(unary: *const Expression) !Value {
    const right = try eval(unary.unary.right);
    switch (unary.unary.operator) {
        .MINUS => {
            if (right == .NUMBER) {
                return Value{ .NUMBER = -right.NUMBER };
            } else {
                return Value{ .ERROR = error.operandNotNumber };
            }
        },
        .BANG => return if (right == .TRUE) Value{ .FALSE = {} } else Value{ .TRUE = {} },
        else => @panic("we should never get here"),
    }
}

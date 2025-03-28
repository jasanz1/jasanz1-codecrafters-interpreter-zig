//! This is the evaluator for the interpreter
//! It takes an array of expressions and evaluates them based on lox spec
//! https://github.com/munificent/craftinginterpreters/blob/01e6f5b8f3e5dfa65674c2f9cf4700d73ab41cf8/book/the-lox-language.md
const std = @import("std");
const Expression = @import("parser.zig").Expression;
const printStatements = @import("parser.zig").printStatements;
const Statements = @import("parser.zig").Statements;
const Operator = @import("parser.zig").Operator;
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const printSingleExpression = @import("parser.zig").printSingleExpression;
/// big list of errors to make non infered errors more readable
const evalErrors = error{
    OutOfMemory,
    NoSpaceLeft,
    DiskQuota,
    FileTooBig,
    InputOutput,
    DeviceBusy,
    InvalidArgument,
    AccessDenied,
    BrokenPipe,
    SystemResources,
    OperationAborted,
    NotOpenForWriting,
    LockViolation,
    WouldBlock,
    ConnectionResetByPeer,
    Unexpected,
};
/// all possible values a expression can evaluate to
/// ERROR is used to wrap errors so the evaluator doesnt crash immedaitely and we can handle them later
const Value = union(enum) {
    STRING: []const u8,
    NUMBER: f64,
    NIL,
    TRUE,
    FALSE,
    ERROR: anyerror,
};

/// holds information about a variable
const Variable = struct {
    name: []const u8,
    value: Value,
};

/// holds all variables
var variable_map = std.StringHashMap(Variable).init(std.heap.page_allocator);

///takes a value and prints it to the passed writer
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

/// since we hold each expression in an array after evaluation, we can print all values in the array
pub fn printValues(writer: anytype, values: *const []Value) !void {
    for (values.*) |current| {
        try printValue(writer, &current);
    }
}

/// checks a single value for errors
pub fn valueErrorCheck(value: Value) !void {
    switch (value) {
        .ERROR => |err| return err,
        else => {},
    }
}

///checks all values in an array for errors
pub fn valuesErrorCheck(values: []Value) !void {
    for (values) |current| {
        try valueErrorCheck(current);
    }
}

/// entry point for the evaluator goes through all Statements and evaluates then appending the results to a list checks them for errors, then returns an the array
pub fn evalulate(ast: *Statements, ignore_errors: bool) ![]Value {
    var value = std.ArrayList(Value).init(std.heap.page_allocator);
    defer value.deinit();
    for (ast.*) |current| {
        std.debug.print("\n", .{});
        const currentValue = try eval(current);
        try value.append(currentValue);
        try valueErrorCheck(currentValue);
    }
    const valueArray = try value.toOwnedSlice();
    if (!ignore_errors) {
        try valuesErrorCheck(valueArray);
    }
    return valueArray;
}

/// evaluates an expression for literals it just wraps them in a Value otherwise calls recursive functions to handle the special cases
fn eval(ast: *const Expression) evalErrors!Value {
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
        .print => try evalPrint(ast),
        .grouping => try eval(ast.grouping),
        .variable => try evalVariable(ast),
        .identifier => switchReturn: {
            const variableValue = variable_map.get(ast.identifier) orelse return Value{ .ERROR = error.VariableNotFound };
            break :switchReturn variableValue.value;
        },
        .parseError => @panic("todo"),
    };
}
/// evaluates a variable, if the variable is not found it returns an error
/// once the value is found it adds it to the variable_map so it can be accessed later
fn evalVariable(value: *const Expression) !Value {
    const valueValue = try eval(value.variable.value);
    std.debug.print("valueValue: {any}\n", .{valueValue});
    if (valueValue == .ERROR) {
        return valueValue;
    }
    variable_map.put(value.variable.name, .{ .name = value.variable.name, .value = valueValue }) catch unreachable;
    return valueValue;
}

/// evaluates a print expression, prints the value and returns it
fn evalPrint(printExpression: *const Expression) !Value {
    const print = printExpression.print;
    const value = try eval(print);
    std.debug.print("print value: {any}\n", .{value});
    if (value == .ERROR) {
        return value;
    }
    try printValue(std.io.getStdOut().writer(), &value);
    try std.io.getStdOut().writer().print("\n", .{});
    return value;
}

/// evaluates a binary expression, calls the appropriate function for an operatorion and returns the result
fn evalBinary(binary: *const Expression) !Value {
    const left = try eval(binary.binary.left);
    const right = try eval(binary.binary.right);

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
    return value;
}

/// evaluates a minus expression, checks if both values are numbers and subtracts them
fn evalMinus(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        return Value{ .NUMBER = left.NUMBER - right.NUMBER };
    }
    return Value{ .ERROR = error.OperandNotNumber };
}

/// evaluates a star expression, checks if both values are numbers and multiplies them
fn evalStar(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        return Value{ .NUMBER = left.NUMBER * right.NUMBER };
    }
    return Value{ .ERROR = error.OperandNotNumber };
}

/// evaluates a slash expression, checks if both values are numbers and divides them
fn evalSlash(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        return Value{ .NUMBER = left.NUMBER / right.NUMBER };
    }
    return Value{ .ERROR = error.OperandNotNumber };
}

/// evaluates a bang equal expression, preforms a logical not equal check on booleans, numbers, and strings
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

/// evaluates an equal equal expression, preforms a logical equal check on booleans, numbers, and strings
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

/// evaluates an less equal expression, preforms a logical less equal check on numbers
fn evalLessEqual(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        if (left.NUMBER <= right.NUMBER) {
            return Value{ .TRUE = {} };
        } else {
            return Value{ .FALSE = {} };
        }
    }
    return Value{ .ERROR = error.OperandNotNumber };
}

/// evaluates a greater equal expression, preforms a logical greater equal check on numbers
fn evalGreaterEqual(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        if (left.NUMBER >= right.NUMBER) {
            return Value{ .TRUE = {} };
        } else {
            return Value{ .FALSE = {} };
        }
    }

    return Value{ .ERROR = error.OperandNotNumber };
}

/// evaluates a less expression, preforms a logical less check on numbers
fn evalLess(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        if (left.NUMBER < right.NUMBER) {
            return Value{ .TRUE = {} };
        } else {
            return Value{ .FALSE = {} };
        }
    }
    return Value{ .ERROR = error.OperandNotNumber };
}

/// evaluates a greator expression, preforms a logical greator check on numbers
fn evalGreater(left: Value, right: Value) !Value {
    if (left == .NUMBER and right == .NUMBER) {
        if (left.NUMBER > right.NUMBER) {
            return Value{ .TRUE = {} };
        } else {
            return Value{ .FALSE = {} };
        }
    }
    return Value{ .ERROR = error.OperandNotNumber };
}

/// evaluates a plus expression, checks if both values are strings or numbers and adds them
fn evalPlus(left: Value, right: Value) !Value {
    if (left == .STRING and right == .STRING) {
        return Value{ .STRING = try std.fmt.allocPrint(std.heap.page_allocator, "{s}{s}", .{ left.STRING, right.STRING }) };
    } else if (left == .NUMBER and right == .NUMBER) {
        return Value{ .NUMBER = left.NUMBER + right.NUMBER };
    }
    return Value{ .ERROR = error.operandsTypeMustMatch };
}

/// evaluates a unary expression,
/// if expression is minus checks if the right value is a number then makes number negative
/// if expression is bang negates bool values
fn evalUnary(unary: *const Expression) !Value {
    const right = try eval(unary.unary.right);
    switch (unary.unary.operator) {
        .MINUS => {
            if (right == .NUMBER) {
                return Value{ .NUMBER = -right.NUMBER };
            } else {
                return Value{ .ERROR = error.OperandNotNumber };
            }
        },
        .BANG => return if (right == .TRUE) Value{ .FALSE = {} } else Value{ .TRUE = {} },
        else => @panic("we should never get here"),
    }
}

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
        TestCases{ .input = "print \"Hello, World!\"", .expected_output = "Hello, World!" },
        TestCases{ .input = "print \"quz\" + \"world\" + \"hello\";", .expected_output = "quzworldhello" },
        TestCases{ .input = "print false;", .expected_output = "false" },
        TestCases{
            .input = " // This program prints the result of a comparison operation\n // It also tests multi-line strings and non-ASCII characters\n print false != true;\n \n print \"56\n 85\n 79\n \";\n \n print \"There should be an empty line above this.\";\n \n print \"(\" + \"\" + \")\";\n \n print \"non-ascii: ॐ\";",
            .expected_output = "true56\n 85\n 79\n There should be an empty line above this.()non-ascii: ॐ",
        },
        TestCases{
            .input = " // This program tests that statements are executed even if they don't have any side effects\n// It also tests complex arithmetic expressions and string concatenation\n(90 + 64 - 87) > (95 - 90) * 2;\nprint !true;\n\"hello\" + \"quz\" + \"foo\" + \"world\" == \"helloquzfooworld\";\nprint !true;",
            .expected_output = "truefalsetruefalse",
        },
        TestCases{
            .input = "65 - 27 >= -81 * 2 / 81 + 23;\nfalse == false;\n(\"quz\" == \"hello\") == (\"world\" != \"baz\");\nprint false;",
            .expected_output = "truetruefalsefalse",
        },
        TestCases{
            .input = "var a = \"foo\";\nprint a;",
            .expected_output = "foofoo",
        },
        TestCases{
            .input = " // This program assigns the result of an arithmetic expression to a variable\n // Then it prints the value of the variable\n var world = (8 * (36 + 36)) / 4 + 36;\n print world;",
            .expected_output = "180180",
        },
        TestCases{
            .input = "// This program tests that variables are initialized to the correct value\n var baz = 10;\n print baz;",
            .expected_output = "1010",
        },
        TestCases{
            .input = " // This program prints the result of an arithmetic expression \n print (80 * 2 + 77 * 2) / (2);",
            .expected_output = "157",
        },
        TestCases{
            .input = " var hello;\n print hello;",
            .expected_output = "nilnil",
        },
    };
    for (test_input) |test_case| {
        std.debug.print("test case:\n{s}\nEOF\n\n", .{test_case.input});
        var inputTokens = lexer.Input{ .source = try std.fmt.allocPrint(std.heap.page_allocator, "{s}", .{test_case.input}) };
        const tokens = try lexer.lexer(&inputTokens, true);
        var input = parser.Input{ .source = tokens };
        var expression_tree = try parser.parser(&input, true);
        var buffer: [1024]u8 = undefined;
        var stream = std.io.fixedBufferStream(&buffer);
        const writer = stream.writer();
        const value = try evalulate(&expression_tree, true);
        std.debug.print("value:\n", .{});
        try printValues(writer, &value);
        try std.testing.expectEqualStrings(test_case.expected_output, stream.buffer[0..stream.pos]);
        std.debug.print("\n\n\n", .{});
    }
}
test "parserUnhappy" {
    const TestCases = struct {
        input: []const u8,
        expected_error: anyerror,
    };
    const test_input = [_]TestCases{
        TestCases{
            .input = " // This program tests that the * operator is only supported when both operands are numbers\n print \"63\" + \"quz\";\n print false * (37 + 33);\n",
            .expected_error = error.OperandNotNumber,
        },
    };

    for (test_input) |test_case| {
        std.debug.print("test case:\n{s}\nEOF\n\n", .{test_case.input});
        var inputTokens = lexer.Input{ .source = try std.fmt.allocPrint(std.heap.page_allocator, "{s}", .{test_case.input}) };
        const tokens = try lexer.lexer(&inputTokens, true);
        var input = parser.Input{ .source = tokens };
        var expression_tree = try parser.parser(&input, true);
        const value = evalulate(&expression_tree, true);
        std.debug.print("errors: {any}\n", .{value});
        try std.testing.expectError(test_case.expected_error, value);
        std.debug.print("\n\n\n", .{});
    }
}

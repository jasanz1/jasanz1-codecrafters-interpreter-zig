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
        // TestCases{ .input = " \"hello\" + \"world\"", .expected_output = "(+ hello world)" },
        // TestCases{ .input = "(-30 + 65) * (46 * 46) / (92 + 29)", .expected_output = "(/ (* (group (+ (- 30.0) 65.0)) (group (* 46.0 46.0))) (group (+ 92.0 29.0)))" },
        // TestCases{ .input = "83 < 99 < 115", .expected_output = "(< (< 83.0 99.0) 115.0)" },
        // TestCases{ .input = "87 <= 179", .expected_output = "(<= 87.0 179.0)" },
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
        try printValue(writer, &evalulate(&expression_tree, false));
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
    }
}

pub fn evalulate(ast: *const Expression, ignore_errors: bool) Value {
    const value = eval(ast);
    if (!ignore_errors) {
        std.debug.print("parseError: huh\n", .{});
    }
    return value;
}

fn eval(ast: *const Expression) Value {
    return switch (ast.*) {
        .binary => evalBinary(ast),
        .unary => evalUnary(ast),
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
fn evalBinary(binary: *const Expression) Value {
    const left = eval(binary.binary.left);
    const right = eval(binary.binary.right);
    std.debug.print("evalBinary: {s}\n", .{binary.binary.operator.stringify()});
    std.debug.print("left: {d}\n", .{left.NUMBER});
    std.debug.print("right: {d}\n", .{right.NUMBER});
    const value = switch (binary.binary.operator) {
        .PLUS => Value{ .NUMBER = left.NUMBER + right.NUMBER },
        .MINUS => Value{ .NUMBER = left.NUMBER - right.NUMBER },
        .STAR => Value{ .NUMBER = left.NUMBER * right.NUMBER },
        .SLASH => Value{ .NUMBER = left.NUMBER / right.NUMBER },
        .BANG_EQUAL => @panic("todo"),
        .EQUAL_EQUAL => @panic("todo"),
        .LESS => @panic("todo"),
        .GREATER => @panic("todo"),
        .LESS_EQUAL => @panic("todo"),
        .GREATER_EQUAL => @panic("todo"),
        else => @panic("we should never get here"),
    };
    std.debug.print("value: {d}\n", .{value.NUMBER});
    return value;
}

fn evalUnary(unary: *const Expression) Value {
    const right = eval(unary.unary.right);
    switch (unary.unary.operator) {
        .MINUS => return Value{ .NUMBER = -right.NUMBER },
        .BANG => return if (right == .TRUE) Value{ .FALSE = {} } else Value{ .TRUE = {} },
        else => @panic("we should never get here"),
    }
}

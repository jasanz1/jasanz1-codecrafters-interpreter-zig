const std = @import("std");
const lexer = @import("lexer.zig");
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
pub const Input = @import("generics.zig").makeInput(Token);

pub const Operator = enum {
    PLUS,
    MINUS,
    STAR,
    SLASH,
    BANG,
    BANG_EQUAL,
    EQUAL_EQUAL,
    LESS,
    GREATER,
    LESS_EQUAL,
    GREATER_EQUAL,
    pub fn stringify(self: Operator) []const u8 {
        return switch (self) {
            .PLUS => "+",
            .MINUS => "-",
            .STAR => "*",
            .SLASH => "/",
            .BANG => "!",
            .BANG_EQUAL => "!=",
            .EQUAL_EQUAL => "==",
            .LESS => "<",
            .GREATER => ">",
            .LESS_EQUAL => "<=",
            .GREATER_EQUAL => ">=",
        };
    }
};

pub const Expression = union(enum) {
    binary: struct { left: *Expression, operator: Operator, right: *Expression },
    unary: struct { operator: Operator, right: *Expression },
    literal: union(enum) { NUMBER: f64, STRING: []const u8, NIL, TRUE, FALSE },
    grouping: *Expression,
    identifier: []const u8,
    parseError: anyerror,
};

pub fn printExpression(writer: anytype, expressionTree: *const Expression) !void {
    switch (expressionTree.*) {
        .binary => |*binary| {
            try writer.print("({s} ", .{binary.operator.stringify()});
            try printExpression(writer, binary.left);
            try writer.print(" ", .{});
            try printExpression(writer, binary.right);
            try writer.print(")", .{});
        },
        .unary => |unary| {
            try writer.print("({s} ", .{unary.operator.stringify()});
            try printExpression(writer, unary.right);
            try writer.print(")", .{});
        },
        .literal => |literal| {
            switch (literal) {
                .NUMBER => |number| {
                    //dont know of a clean way to set min precision
                    if (@ceil(number) == number) {
                        try writer.print("{d}.0", .{number});
                        return;
                    } else {
                        try writer.print("{d}", .{number});
                        return;
                    }
                },

                .STRING => |string| try writer.print("{s}", .{string}),
                .NIL => try writer.print("nil", .{}),
                .TRUE => try writer.print("true", .{}),
                .FALSE => try writer.print("false", .{}),
            }
        },
        .grouping => |grouping| {
            try writer.print("(group ", .{});
            try printExpression(writer, grouping);
            try writer.print(")", .{});
        },
        .identifier => |identifier| {
            try writer.print("{s}", .{identifier});
        },
        .parseError => |parseError| {
            try std.io.getStdErr().writer().print("parseError: {}", .{parseError});
            return;
        },
    }
}

pub fn parser(input: *Input, ignore_errors: bool) !Expression {
    var context = std.ArrayList(u8).init(std.heap.page_allocator);
    defer context.deinit();
    const expression_tree = expression(input, &context);
    if (!ignore_errors) {
        try errorCheck(expression_tree);
    }
    return expression_tree.*;
}

pub fn errorCheck(expression_tree: *const Expression) !void {
    switch (expression_tree.*) {
        .binary => |*binary| {
            try errorCheck(binary.left);
            try errorCheck(binary.right);
        },
        .unary => |unary| {
            try errorCheck(unary.right);
        },
        .grouping => |grouping| {
            try errorCheck(grouping);
        },
        .parseError => |parseError| {
            return parseError;
        },
        else => {
            return;
        },
    }
}

test "parserHappy" {
    // array of array of strings
    const TestCases = struct {
        input: []const u8,
        expected_output: []const u8,
    };
    const test_input = [_]TestCases{
        TestCases{ .input = "1 * 2", .expected_output = "(* 1.0 2.0)" },
        TestCases{ .input = "52 + 80 - 94", .expected_output = "(- (+ 52.0 80.0) 94.0)" },
        TestCases{ .input = "78 - 93 * 79 - 32", .expected_output = "(- (- 78.0 (* 93.0 79.0)) 32.0)" },
        TestCases{ .input = " \"hello\" + \"world\"", .expected_output = "(+ hello world)" },
        TestCases{ .input = "(-30 + 65) * (46 * 46) / (92 + 29)", .expected_output = "(/ (* (group (+ (- 30.0) 65.0)) (group (* 46.0 46.0))) (group (+ 92.0 29.0)))" },
        TestCases{ .input = "83 < 99 < 115", .expected_output = "(< (< 83.0 99.0) 115.0)" },
        TestCases{ .input = "87 <= 179", .expected_output = "(<= 87.0 179.0)" },
    };
    for (test_input) |test_case| {
        std.debug.print("test case: {s}\n", .{test_case.input});
        var inputTokens = lexer.Input{ .source = try std.fmt.allocPrint(std.heap.page_allocator, "{s}", .{test_case.input}) };
        const tokens = try lexer.lexer(&inputTokens, true);
        var input = Input{ .source = tokens };
        const expression_tree = try parser(&input, false);
        var buffer: [1024]u8 = undefined;
        var stream = std.io.fixedBufferStream(&buffer);
        const writer = stream.writer();
        try printExpression(writer, &expression_tree);
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
        TestCases{ .input = "(72 +)", .expected_error = error.UnterminatedBinary },
        TestCases{ .input = "(foo ", .expected_error = error.Unterminatedgroup },
        TestCases{ .input = "+", .expected_error = error.UnterminatedBinary },
    };

    for (test_input) |test_case| {
        std.debug.print("test case: {s}\n", .{test_case.input});
        var inputTokens = lexer.Input{ .source = try std.fmt.allocPrint(std.heap.page_allocator, "{s}", .{test_case.input}) };
        const tokens = try lexer.lexer(&inputTokens, true);
        var input = Input{ .source = tokens };
        const expression_tree = parser(&input, false);
        try std.testing.expectError(test_case.expected_error, expression_tree);
        std.debug.print("\n\n\n", .{});
    }
}

fn expression(input: *Input, context: *std.ArrayList(u8)) *Expression {
    var expression_helper: ?*Expression = null;
    while (input.peek()) |_| {
        expression_helper = expressionHelper(input, context, expression_helper);
        if (@import("builtin").is_test) {
            printExpression(std.io.getStdOut().writer(), expression_helper.?) catch @panic("error printing expression");
            std.debug.print("\n", .{});
        }
    }
    return expression_helper.?;
}

fn groupExpression(input: *Input, context: *std.ArrayList(u8)) *Expression {
    var expression_helper: ?*Expression = null;
    context.append('(') catch unreachable;
    while (input.peek()) |c| {
        if (c.token_type == .RIGHT_PAREN) {
            _ = input.next();
            break;
        }
        expression_helper = expressionHelper(input, context, expression_helper);
        if (@import("builtin").is_test) {
            printExpression(std.io.getStdOut().writer(), expression_helper.?) catch @panic("error printing expression");
            std.debug.print("\n", .{});
        }
    }

    if (context.pop() != '(' or expression_helper.?.* == .parseError) {
        if (expression_helper.?.*.parseError == error.unexpectedEOF) {
            return makeNewExpressionPointer(Expression{ .parseError = error.Unterminatedgroup }).?;
        }
    }
    return makeNewExpressionPointer(Expression{ .grouping = expression_helper.? }).?;
}
fn expressionHelper(input: *Input, context: *std.ArrayList(u8), previous: ?*Expression) ?*Expression {
    if (input.peek()) |c| {
        _ = input.next();
        const new_expression = switch (c.token_type) {
            .TRUE, .FALSE, .NIL, .NUMBER, .STRING => makeLiteral(c),
            .LEFT_PAREN => groupExpression(input, context),
            .RIGHT_PAREN => makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedGroup }).?,
            .LEFT_BRACE => @panic("TODO"),
            .RIGHT_BRACE => @panic("TODO"),
            .SEMICOLON => @panic("TODO"),
            .COMMA => @panic("TODO"),
            .PLUS => makeBinary(input, context, previous, .PLUS, plusOrMinusPercedence),
            .STAR => makeBinary(input, context, previous, .STAR, multipleOrDividePercedence),
            .SLASH => makeBinary(input, context, previous, .SLASH, multipleOrDividePercedence),
            .BANG => makeUnary(input, context, .BANG),
            .BANG_EQUAL => makeBinary(input, context, previous, .BANG_EQUAL, comparePercedence),
            .EQUAL => @panic("TODO"),
            .EQUAL_EQUAL => makeBinary(input, context, previous, .EQUAL_EQUAL, comparePercedence),
            .LESS => makeBinary(input, context, previous, .LESS, comparePercedence),
            .GREATER => makeBinary(input, context, previous, .GREATER, comparePercedence),
            .LESS_EQUAL => makeBinary(input, context, previous, .LESS_EQUAL, comparePercedence),
            .GREATER_EQUAL => makeBinary(input, context, previous, .GREATER_EQUAL, comparePercedence),
            .IDENTIFIER => makeIdentifier(c),
            .AND => @panic("TODO"),
            .CLASS => @panic("TODO"),
            .ELSE => @panic("TODO"),
            .FOR => @panic("TODO"),
            .FUN => @panic("TODO"),
            .IF => @panic("TODO IF"),
            .OR => @panic("TODO"),
            .PRINT => @panic("TODO"),
            .RETURN => @panic("TODO"),
            .SUPER => @panic("TODO"),
            .THIS => @panic("TODO"),
            .VAR => @panic("TODO"),
            .WHILE => @panic("TODO"),
            .DOT => @panic("TODO"),
            .INVALID_TOKEN => @panic("TODO"),
            .UNTERMINATED_STRING => @panic("TODO"),
            .EOF => handleEOF(context, previous),
            .MINUS => handleMinus(input, context, previous),
            // i dont like this
        };
        return new_expression;
    }

    return null;
}

fn handleEOF(context: *std.ArrayList(u8), previous: ?*Expression) *Expression {
    if (context.items.len == 0) {
        return previous orelse return makeNewExpressionPointer(Expression{ .parseError = error.unexpectedEOF }).?;
    }
    if (previous) |prev| {
        errorCheck(prev) catch return prev;
    }
    return makeNewExpressionPointer(Expression{ .parseError = error.unexpectedEOF }).?;
}

fn handleMinus(input: *Input, context: *std.ArrayList(u8), previous: ?*Expression) *Expression {
    if (previous) |_| {
        return makeBinary(input, context, previous, .MINUS, plusOrMinusPercedence);
    } else {
        return makeUnary(input, context, .MINUS);
    }
}

fn makeIdentifier(token: Token) *Expression {
    const identifier = token.lexeme;
    return makeNewExpressionPointer(Expression{ .identifier = identifier }).?;
}

fn makeLiteral(token: Token) ?*Expression {
    const tokenType = token.token_type;
    const new_expression = switch (tokenType) {
        .NUMBER => Expression{ .literal = .{ .NUMBER = token.literal.?.number } },
        .STRING => Expression{ .literal = .{ .STRING = token.literal.?.string } },
        .NIL => Expression{ .literal = .{ .NIL = {} } },
        .TRUE => Expression{ .literal = .{ .TRUE = {} } },
        .FALSE => Expression{ .literal = .{ .FALSE = {} } },
        else => null,
    };
    return makeNewExpressionPointer(new_expression);
}

fn convertToBoolean(new_expression: *Expression) *Expression {
    switch (new_expression.*) {
        .literal => |literal| {
            switch (literal) {
                .NUMBER => |number| {
                    if (number == 0) {
                        return makeNewExpressionPointer(Expression{ .literal = .{ .FALSE = {} } }).?;
                    } else {
                        return makeNewExpressionPointer(Expression{ .literal = .{ .TRUE = {} } }).?;
                    }
                },
                .TRUE => return makeNewExpressionPointer(Expression{ .literal = .{ .TRUE = {} } }).?,
                .FALSE => return makeNewExpressionPointer(Expression{ .literal = .{ .FALSE = {} } }).?,
                else => {},
            }
        },
        else => {},
    }
    return new_expression;
}

fn makeUnary(input: *Input, context: *std.ArrayList(u8), operator: Operator) *Expression {
    var right = expressionHelper(input, context, null) orelse makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedUnary }).?;
    if (operator == .BANG) {
        right = convertToBoolean(right);
    }
    return makeNewExpressionPointer(Expression{ .unary = .{ .operator = operator, .right = right } }).?;
}

fn makeBinary(input: *Input, context: *std.ArrayList(u8), previous: ?*Expression, operator: Operator, comptime comparefunc: anytype) *Expression {
    var right = expressionHelper(input, context, null) orelse makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedBinary }).?;
    if (right.* == .parseError) {
        right = makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedBinary }).?;
    }
    var new_expression = makeNewExpressionPointer(Expression{ .binary = .{
        .left = previous orelse makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedBinary }).?,
        .operator = operator,
        .right = right,
    } }).?;
    new_expression = rotateIfPrecedenceMismatch(previous, new_expression, comparefunc);
    return new_expression;
}

fn plusOrMinusPercedence(_: ?*Expression) bool {
    return false;
}

fn multipleOrDividePercedence(prev: ?*Expression) bool {
    return prev.?.binary.operator == .MINUS or prev.?.binary.operator == .PLUS;
}

fn comparePercedence(prev: ?*Expression) bool {
    return prev.?.binary.operator == .MINUS or prev.?.binary.operator == .PLUS or prev.?.binary.operator == .SLASH or prev.?.binary.operator == .STAR;
}

fn rotateIfPrecedenceMismatch(previous: ?*Expression, new_expression: *Expression, comptime precedence: anytype) *Expression {
    if (previous) |prev| {
        if (prev.* == .binary and @call(.auto, precedence, .{prev})) {
            const temp = prev.binary.right;
            prev.binary.right = new_expression;
            new_expression.*.binary.left = temp;
            return prev;
        }
    } else {
        return makeNewExpressionPointer(Expression{ .parseError = error.UnterminatedBinary }).?;
    }

    return new_expression;
}

fn tokenTypeIsOperator(token_type: TokenType) ?Operator {
    return switch (token_type) {
        .PLUS => Operator.PLUS,
        .MINUS => Operator.MINUS,
        .STAR => Operator.STAR,
        .SLASH => Operator.SLASH,
        .BANG => Operator.BANG,
        .BANG_EQUAL => Operator.BANG_EQUAL,
        .EQUAL_EQUAL => Operator.EQUAL_EQUAL,
        .LESS => Operator.LESS,
        .GREATER => Operator.GREATER,
        .LESS_EQUAL => Operator.LESS_EQUAL,
        .GREATER_EQUAL => Operator.GREATER_EQUAL,
        else => null,
    };
}

fn makeNewExpressionPointer(new_expression: ?Expression) ?*Expression {
    if (new_expression) |_| {
        const new_expression_pointer = std.heap.page_allocator.create(Expression) catch unreachable;
        new_expression_pointer.* = new_expression.?;
        return new_expression_pointer;
    } else {
        return null;
    }
}

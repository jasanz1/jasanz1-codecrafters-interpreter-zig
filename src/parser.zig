const std = @import("std");
const lexer = @import("lexer.zig");
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
pub const Input = @import("generics.zig").makeInput(Token);

const Operator = enum {
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
    pub fn char(self: Operator) u8 {
        return switch (self) {
            .PLUS => '+',
            .MINUS => '-',
            .STAR => '*',
            .SLASH => '/',
            .BANG => '!',
            .BANG_EQUAL => '!',
            .EQUAL_EQUAL => '=',
            .LESS => '<',
            .GREATER => '>',
            .LESS_EQUAL => '<',
            .GREATER_EQUAL => '>',
        };
    }
};

const Expression = union(enum) {
    binary: struct { left: *Expression, operator: Operator, right: *Expression },
    unary: struct { operator: Operator, right: *Expression },
    literal: union(enum) { NUMBER: f64, STRING: []const u8, NIL, TRUE, FALSE },
    grouping: *Expression,
};

pub fn printExpression(writer: anytype, expressionTree: *const Expression) !void {
    switch (expressionTree.*) {
        .binary => |*binary| {
            try writer.print("({c} ", .{binary.operator.char()});
            try printExpression(writer, binary.left);
            try writer.print(" ", .{});
            try printExpression(writer, binary.right);
            try writer.print(")", .{});
        },
        .unary => |unary| {
            try writer.print("({c} ", .{unary.operator.char()});
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
    }
}

pub fn parser(input: *Input) !Expression {
    var context = std.ArrayList(u8).init(std.heap.page_allocator);
    defer context.deinit();
    const expression_tree = try expression(input, &context);
    return expression_tree.*;
}

test "parser" {
    // array of array of strings
    const test_input = [_][2][]const u8{
        .{ "1 * 2", "(* 1.0 2.0)" },
        .{ "52 + 80 - 94", "(- (+ 52.0 80.0) 94.0)" },
        .{ "78 - 93 * 79 - 32", "(- (- 78.0 (* 93.0 79.0)) 32.0)" },
        .{ " \"hello\" + \"world\"", "(+ hello world)" },
        .{ "(-30 + 65) * (46 * 46) / (92 + 29)", "(/ (* (group (+ (- 30.0) 65.0)) (group (* 46.0 46.0))) (group (+ 92.0 29.0)))" },
    };
    for (test_input) |test_case| {
        std.debug.print("test case: {s}\n", .{test_case[0]});
        var inputTokens = lexer.Input{ .source = try std.fmt.allocPrint(std.heap.page_allocator, "{s}", .{test_case[0]}) };
        const tokens = try lexer.Tokenizer(&inputTokens);
        var input = Input{ .source = tokens };
        const expression_tree = try parser(&input);
        var buffer: [1024]u8 = undefined;
        var stream = std.io.fixedBufferStream(&buffer);
        const writer = stream.writer();
        try printExpression(writer, &expression_tree);
        try std.testing.expectEqualStrings(test_case[1], stream.buffer[0..stream.pos]);
        std.debug.print("\n\n\n", .{});
    }
}

fn expression(input: *Input, context: *std.ArrayList(u8)) error{ UnterminatedBinary, UnterminatedUnary, UnterminatedGroup }!*Expression {
    var expresion_helper: ?*Expression = null;
    while (input.peek()) |_| {
        expresion_helper = expressionHelper(input, context, expresion_helper) catch return error.UnterminatedBinary;
        if (@import("builtin").is_test) {
            std.debug.print("expression: {s} ", .{@tagName(expresion_helper.?.*)});
            printExpression(std.io.getStdOut().writer(), expresion_helper.?) catch @panic("error printing expression");
            std.debug.print("\n", .{});
        }
    }
    return expresion_helper.?;
}

fn expressionHelper(input: *Input, context: *std.ArrayList(u8), previous: ?*Expression) !*Expression {
    var new_expression = std.heap.page_allocator.create(Expression) catch unreachable;
    if (input.peek()) |c| {
        _ = input.next();
        switch (c.token_type) {
            .TRUE => new_expression.* = Expression{ .literal = .{ .TRUE = {} } },
            .FALSE => new_expression.* = Expression{ .literal = .{ .FALSE = {} } },
            .NIL => new_expression.* = Expression{ .literal = .{ .NIL = {} } },
            .NUMBER => new_expression.* = Expression{ .literal = .{ .NUMBER = c.literal.?.number } },
            .STRING => new_expression.* = Expression{ .literal = .{ .STRING = c.literal.?.string } },
            .LEFT_PAREN => {
                context.append('(') catch unreachable;
                new_expression = expression(input, context) catch return error.UnterminatedGroup;
            },
            .RIGHT_PAREN => {
                _ = context.pop();
                new_expression.* = Expression{ .grouping = previous.? };
            },
            .LEFT_BRACE => @panic("TODO"),
            .RIGHT_BRACE => @panic("TODO"),
            .SEMICOLON => @panic("TODO"),
            .COMMA => @panic("TODO"),
            .PLUS => {
                const right = expressionHelper(input, context, null) catch return error.UnterminatedBinary;
                new_expression.* = Expression{ .binary = .{ .left = previous orelse return error.UnterminatedBinary, .operator = .PLUS, .right = right } };
            },
            .MINUS => {
                if (previous) |left| {
                    new_expression.* = Expression{ .binary = .{ .left = left, .operator = .MINUS, .right = expressionHelper(input, context, null) catch return error.UnterminatedBinary } };
                } else {
                    new_expression.* = Expression{ .unary = .{ .operator = .MINUS, .right = expressionHelper(input, context, null) catch return error.UnterminatedUnary } };
                }
            },
            .STAR => {
                const right = expressionHelper(input, context, null) catch return error.UnterminatedBinary;
                new_expression.* = Expression{ .binary = .{ .left = previous orelse return error.UnterminatedBinary, .operator = .STAR, .right = right } };
                new_expression = rotateIfPrecedenceMismatch(previous, new_expression, struct {
                    fn check(prev: ?*Expression) bool {
                        return prev.?.binary.operator == .MINUS or prev.?.binary.operator == .PLUS;
                    }
                });
            },
            .SLASH => {
                const right = expressionHelper(input, context, null) catch return error.UnterminatedBinary;
                new_expression.* = Expression{ .binary = .{ .left = previous orelse return error.UnterminatedBinary, .operator = .SLASH, .right = right } };
                new_expression = rotateIfPrecedenceMismatch(previous, new_expression, struct {
                    fn check(prev: ?*Expression) bool {
                        return prev.?.binary.operator == .MINUS or prev.?.binary.operator == .PLUS;
                    }
                });
            },
            .BANG => new_expression.* = Expression{ .unary = .{ .operator = .BANG, .right = expressionHelper(input, context, null) catch return error.UnterminatedUnary } },
            .BANG_EQUAL => @panic("TODO"),
            .EQUAL => @panic("TODO"),
            .EQUAL_EQUAL => @panic("TODO"),
            .LESS => @panic("TODO"),
            .GREATER => @panic("TODO"),
            .LESS_EQUAL => @panic("TODO"),
            .GREATER_EQUAL => @panic("TODO"),
            .IDENTIFIER => @panic("TODO"),
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
            // i dont like this
            .EOF => {
                if (context.items.len > 0) {
                    return error.earlyEOF;
                }
            },
        }
        if (c.token_type == .EOF) {
            return previous.?;
        }
    }
    return new_expression;
}

fn rotateIfPrecedenceMismatch(previous: ?*Expression, new_expression: *Expression, comptime precedence: anytype) *Expression {
    if (previous.?.* == .binary and precedence.check(previous)) {
        const temp = previous.?.binary.right;
        previous.?.binary.right = new_expression;
        new_expression.*.binary.left = temp;
        return previous.?;
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

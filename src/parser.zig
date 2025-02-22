const std = @import("std");
const Token = @import("lexer.zig").Token;
pub const Input = @import("generics.zig").makeInput(Token);

const OperatorType = enum {
    PLUS,
    MINUS,
    STAR,
    SLASH,
    BANG_EQUAL,
    EQUAL_EQUAL,
    LESS,
    GREATER,
    LESS_EQUAL,
    GREATER_EQUAL,
};

const Operator = struct {
    operator: OperatorType,
    pub fn char(self: *const Operator) u8 {
        return switch (self.operator) {
            .PLUS => '+',
            .MINUS => '-',
            .STAR => '*',
            .SLASH => '/',
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

pub fn printExpression(expressionTree: *const Expression) !void {
    switch (expressionTree.*) {
        .binary => |*binary| {
            try std.io.getStdOut().writer().print("{c} ", .{binary.operator.char()});
            try printExpression(binary.left);
            try printExpression(binary.right);
        },
        .unary => |unary| {
            // try std.io.getStdOut().writer().print(" {s} ", .{@tagName(unary.operator)});
            try printExpression(unary.right);
        },
        .literal => |literal| {
            switch (literal) {
                .NUMBER => |number| {
                    //dont know of a clean way to set min precision
                    if (@ceil(number) == number) {
                        try std.io.getStdOut().writer().print("{d}.0", .{number});
                        return;
                    } else {
                        try std.io.getStdOut().writer().print("{d}", .{number});
                        return;
                    }
                },

                .STRING => |string| try std.io.getStdOut().writer().print("{s}", .{string}),
                .NIL => try std.io.getStdOut().writer().print("nil", .{}),
                .TRUE => try std.io.getStdOut().writer().print("true", .{}),
                .FALSE => try std.io.getStdOut().writer().print("false", .{}),
            }
        },
        .grouping => |grouping| {
            try std.io.getStdOut().writer().print("(group ", .{});
            try printExpression(grouping);
            try std.io.getStdOut().writer().print(")", .{});
        },
    }
}

pub fn parser(input: *Input) !Expression {
    var context = std.ArrayList(u8).init(std.heap.page_allocator);
    defer context.deinit();
    const expression_tree = try expression(input, &context);
    return expression_tree.*;
}

fn expression(input: *Input, context: *std.ArrayList(u8)) !*Expression {
    const new_expression = std.heap.page_allocator.create(Expression) catch unreachable;
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
                const group_expression = expression(input, context) catch return error.UnterminatedGroup;
                new_expression.* = Expression{ .grouping = group_expression };
            },
            .RIGHT_PAREN => {
                _ = context.pop();
            },
            .LEFT_BRACE => @panic("TODO"),
            .RIGHT_BRACE => @panic("TODO"),
            .SEMICOLON => @panic("TODO"),
            .COMMA => @panic("TODO"),
            .PLUS => @panic("TODO"),
            .MINUS => @panic("TODO"),
            .STAR => @panic("TODO"),
            .SLASH => @panic("TODO"),
            .BANG => @panic("TODO"),
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
                    return error.EOF;
                }
            },
        }
        return new_expression;
    }
    return error.syntaxError;
}

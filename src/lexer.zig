const std = @import("std");
pub const Input = struct {
    source: []const u8,
    index: usize,
    fn next(self: *Input) ?u8 {
        if (self.index >= self.source.len) {
            return null;
        }
        defer self.index += 1;
        return self.source[self.index];
    }
    fn peek(self: *Input) ?u8 {
        if (self.index >= self.source.len) {
            return null;
        }
        return self.source[self.index];
    }
};
pub fn makeInput(source: []const u8) Input {
    return Input{
        .source = source,
        .index = 0,
    };
}

const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    literal: ?Literal,
};

pub const Literal = union(enum) {
    number: usize,
    string: []const u8,
};
pub const TokenType = enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    SEMICOLON,
    COMMA,
    PLUS,
    MINUS,
    STAR,
    BANG_EQUAL,
    EQUAL_EQUAL,
    LESS_EQUAL,
    GREATER_EQUAL,
    LESS,
    GREATER,
    SLASH,
    DOT,
    INVALID_TOKEN,
    EOF,
};
pub fn printToken(token: Token) !void {
    const token_type = switch (token.token_type) {
        TokenType.EOF => "EOF",
        TokenType.LEFT_PAREN => "LEFT_PAREN",
        TokenType.RIGHT_PAREN => "RIGHT_PAREN",
        TokenType.LEFT_BRACE => "LEFT_BRACE",
        TokenType.RIGHT_BRACE => "RIGHT_BRACE",
        TokenType.SEMICOLON => "SEMICOLON",
        TokenType.COMMA => "COMMA",
        TokenType.PLUS => "PLUS",
        TokenType.MINUS => "MINUS",
        TokenType.STAR => "STAR",
        TokenType.BANG_EQUAL => "BANG_EQUAL",
        TokenType.EQUAL_EQUAL => "EQUAL_EQUAL",
        TokenType.LESS_EQUAL => "LESS_EQUAL",
        TokenType.GREATER_EQUAL => "GREATER_EQUAL",
        TokenType.LESS => "LESS",
        TokenType.GREATER => "GREATER",
        TokenType.SLASH => "SLASH",
        TokenType.DOT => "DOT",
        TokenType.INVALID_TOKEN => "INVALID_TOKEN",
    };
    if (token.literal) |literal| {
        if (token.token_type == TokenType.INVALID_TOKEN) {
            try std.io.getStdErr().writer().print("[line {d}] Error: Unexpected character: {s}\n", .{ literal.number, token.lexeme });
            return error.UnexpectedCharacter;
        } else {
            switch (literal) {
                .number => |number| try std.io.getStdOut().writer().print("{s} {s} {d}\n", .{ token_type, token.lexeme, number }),
                .string => |string| try std.io.getStdOut().writer().print("{s} {s} {s}\n", .{ token_type, token.lexeme, string }),
            }
        }
    } else {
        try std.io.getStdOut().writer().print("{s} {s} null\n", .{ token_type, token.lexeme });
    }
}
pub fn Tokenizer(source: *Input) ![]Token {
    var tokens = std.ArrayList(Token).init(std.heap.page_allocator);
    var line_number: usize = 1;
    while (source.next()) |c| {
        const token = switch (c) {
            ' ', '\t' => continue,
            '\n' => {
                line_number += 1;
                continue;
            },
            '(' => Token{ .token_type = TokenType.LEFT_PAREN, .lexeme = "(", .literal = null },
            ')' => Token{ .token_type = TokenType.RIGHT_PAREN, .lexeme = ")", .literal = null },
            '{' => Token{ .token_type = TokenType.LEFT_BRACE, .lexeme = "{", .literal = null },
            '}' => Token{ .token_type = TokenType.RIGHT_BRACE, .lexeme = "}", .literal = null },
            ';' => Token{ .token_type = TokenType.SEMICOLON, .lexeme = ";", .literal = null },
            ',' => Token{ .token_type = TokenType.COMMA, .lexeme = ",", .literal = null },
            '+' => Token{ .token_type = TokenType.PLUS, .lexeme = "+", .literal = null },
            '-' => Token{ .token_type = TokenType.MINUS, .lexeme = "-", .literal = null },
            '*' => Token{ .token_type = TokenType.STAR, .lexeme = "*", .literal = null },
            '.' => Token{ .token_type = TokenType.DOT, .lexeme = ".", .literal = null },
            else => Token{ .token_type = TokenType.INVALID_TOKEN, .lexeme = try std.fmt.allocPrint(std.heap.page_allocator, "{c}", .{c}), .literal = Literal{ .number = line_number } },
        };
        try tokens.append(token);
    }
    const token = Token{ .token_type = TokenType.EOF, .lexeme = "", .literal = null };
    try tokens.append(token);

    return tokens.toOwnedSlice();
}

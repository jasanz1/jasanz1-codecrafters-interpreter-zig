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
    number: f64,
    string: []const u8,
};

pub const TokenType = enum {
    EOF,
    IDENTIFIER,
    NUMBER,
    STRING,
    PLUS,
    MINUS,
    TIMES,
    DIVIDE,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    COMMA,
};
pub fn printToken(token: Token) !void {
    const token_type = switch (token.token_type) {
        TokenType.EOF => "EOF",
        TokenType.IDENTIFIER => "IDENTIFIER",
        TokenType.NUMBER => "NUMBER",
        TokenType.STRING => "STRING",
        TokenType.PLUS => "PLUS",
        TokenType.MINUS => "MINUS",
        TokenType.TIMES => "TIMES",
        TokenType.DIVIDE => "DIVIDE",
        TokenType.LPAREN => "LEFT_PAREN",
        TokenType.RPAREN => "RIGHT_PAREN",
        TokenType.LBRACE => "LEFT_BRACE",
        TokenType.RBRACE => "RIGHT_BRACE",
        TokenType.LBRACKET => "LEFT_BRACKET",
        TokenType.RBRACKET => "RIGHT_BRACKET",
        TokenType.COMMA => "COMMA",
    };
    if (token.literal) |literal| {
        switch (literal) {
            .number => |number| try std.io.getStdOut().writer().print("{s} {s} {d}\n", .{ token_type, token.lexeme, number }),
            .string => |string| try std.io.getStdOut().writer().print("{s} {s} {s}\n", .{ token_type, token.lexeme, string }),
        }
    } else {
        try std.io.getStdOut().writer().print("{s} {s} null\n", .{ token_type, token.lexeme });
    }
}
pub fn Tokenizer(source: *Input) ![]Token {
    var tokens = std.ArrayList(Token).init(std.heap.page_allocator);
    while (source.next()) |c| {
        const token = switch (c) {
            ' ', '\t', '\n' => continue,
            '{' => Token{ .token_type = TokenType.LBRACE, .lexeme = "{", .literal = null },
            '}' => Token{ .token_type = TokenType.RBRACE, .lexeme = "}", .literal = null },
            '[' => Token{ .token_type = TokenType.LBRACKET, .lexeme = "[", .literal = null },
            ']' => Token{ .token_type = TokenType.RBRACKET, .lexeme = "]", .literal = null },
            '(' => Token{ .token_type = TokenType.LPAREN, .lexeme = "(", .literal = null },
            ')' => Token{ .token_type = TokenType.RPAREN, .lexeme = ")", .literal = null },
            '+' => Token{ .token_type = TokenType.PLUS, .lexeme = "+", .literal = null },
            '-' => Token{ .token_type = TokenType.MINUS, .lexeme = "-", .literal = null },
            '*' => Token{ .token_type = TokenType.TIMES, .lexeme = "*", .literal = null },
            '/' => Token{ .token_type = TokenType.DIVIDE, .lexeme = "/", .literal = null },
            ',' => Token{ .token_type = TokenType.COMMA, .lexeme = ",", .literal = null },
            else => {
                std.debug.print("Unknown character: {any}\n", .{c});
                return error.UnknownCharacter;
            },
        };
        try tokens.append(token);
    }
    const token = Token{ .token_type = TokenType.EOF, .lexeme = "", .literal = null };
    try tokens.append(token);

    return tokens.toOwnedSlice();
}

const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const eval = @import("eval.zig");

pub fn main() !u8 {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    std.debug.print("Logs from your program will appear here!\n", .{});

    // takes 2 args
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh <tokenize|parse|evaluate|run> <filename>\n", .{});
        std.process.exit(1);
    }
    const command = args[1];
    const filename = args[2];
    // checks if the command is valid
    if (!std.mem.eql(u8, command, "tokenize") and !std.mem.eql(u8, command, "parse") and !std.mem.eql(u8, command, "evaluate") and !std.mem.eql(u8, command, "run")) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }

    // reads the file into memory
    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    // takes the file contents and turns it into tokens
    var tokenizerInput = lexer.Input{ .source = file_contents };
    const tokens = lexer.lexer(&tokenizerInput, std.mem.eql(u8, command, "tokenize")) catch return 65;
    defer std.heap.page_allocator.free(tokens);
    // prints the tokens if the command is tokenize then checks for errors and returns
    if (std.mem.eql(u8, command, "tokenize")) {
        try lexer.printTokens(tokens);
        lexer.errorCheck(tokens) catch return 65;
        return 0;
    }

    // takes the tokens and turns them into an ast
    var pasterInput = parser.Input{ .source = tokens };
    var ast = parser.parser(&pasterInput, std.mem.eql(u8, command, "parse")) catch return 65;
    // prints the ast if the command is parse then checks for errors and returns
    if (std.mem.eql(u8, command, "parse")) {
        parser.errorCheckStatements(ast) catch return 65;
        try parser.printStatements(std.io.getStdOut().writer(), ast);
        return 0;
    }

    // takes the ast and evalulates it
    const value = eval.evalulate(&ast, std.mem.eql(u8, command, "evaluate")) catch return 70;
    // prints the value if the command is evaluate then checks for errors and returns
    if (std.mem.eql(u8, command, "evaluate")) {
        eval.valuesErrorCheck(value) catch return 70;
        try eval.printValues(std.io.getStdOut().writer(), &value);
        return 0;
    }
    return 0;
}

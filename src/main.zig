const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
pub fn main() !u8 {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    std.debug.print("Logs from your program will appear here!\n", .{});

    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const command = args[1];
    const filename = args[2];

    if (!std.mem.eql(u8, command, "tokenize") and !std.mem.eql(u8, command, "parse")) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);
    var errored: ?anyerror = null;

    // Uncomment this block to pass the first stage
    if (file_contents.len > 0) errored: {
        var tokenizerInput = lexer.Input{ .source = file_contents };
        const tokens = try lexer.Tokenizer(&tokenizerInput);
        defer std.heap.page_allocator.free(tokens);
        if (std.mem.eql(u8, command, "tokenize")) {
            if (lexer.printTokens(tokens)) |_| {
                break :errored;
            } else |err| {
                errored = err;
                break :errored;
            }
        }
        if (lexer.errorCheck(tokens)) |_| {} else |err| {
            errored = err;
            break :errored;
        }
        var pasterInput = parser.Input{ .source = tokens };
        const ast = try parser.parser(&pasterInput);
        if (std.mem.eql(u8, command, "parse")) {
            if (parser.printExpression(&ast)) |_| {
                break :errored;
            } else |err| {
                errored = err;
                break :errored;
            }
        }
        if (std.mem.eql(u8, command, "parse")) {
            break :errored;
        }
        // if (parser.errorCheck(tokens)) |err| {
        //     break :errored err;
        // }
    }
    if (errored) |_| {
        return 65;
    }
    return 0;
}

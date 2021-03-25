const std = @import("std");
const Allocator = std.mem.Allocator;
const lexer = @import("./lexer.zig");
const Lexer = lexer.Lexer;
const Token = lexer.Token;

pub fn parse(allocator: *Allocator, filename: []const u8, source: []const u8) !void {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var it = Lexer.init(filename, source).iterator();
    while (it.next()) |token| {
        try tokens.append(token);
    }

    var parser = Parser{
        .allocator = allocator,
        .source = source,
        .tokens = tokens.items,
    };

    parser.parseRoot();
}

const Parser = struct {
    allocator: *Allocator,
    source: []const u8,
    tokens: []const Token,

    const Self = @This();

    fn parseRoot(self: *Self) void {}
};

test "simple parse" {
    try parse(std.testing.allocator, "", "hello {{ name }}!");
}

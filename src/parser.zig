// TODO: write a grammar for the template language
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
        .current = @as(usize, 0),
    };

    try parser.parseRoot();
}

const ParseError = error{
    EarlyEnd,
    UnexpectedToken,
};

const Parser = struct {
    allocator: *Allocator,
    source: []const u8,
    tokens: []const Token,
    current: usize,

    const Self = @This();

    fn peek(self: *Self) Token {
        std.debug.print("peeking current token: index={} of len={}\n", .{ self.current, self.tokens.len });
        return self.tokens[self.current];
    }

    fn consume(self: *Self) !void {
        if (self.isEof()) return ParseError.EarlyEnd;
        self.current += 1;
    }

    fn expect(self: *Self, kind: Token.Kind) !void {
        if (self.isKind(kind)) {
            try self.consume();
        } else {
            return ParseError.UnexpectedToken;
        }
    }

    fn isKind(self: *Self, kind: Token.Kind) bool {
        return self.peek().kind == kind;
    }

    fn isEof(self: *Self) bool {
        return self.current >= self.tokens.len;
    }

    fn parseStatement(self: *Self) !void {
        try self.expect(.statement_open);
        while (!self.isKind(.statement_close)) {
            self.peek().dump();
            try self.consume();
        }
        try self.expect(.statement_close);
    }

    fn parseExpression(self: *Self) !void {
        try self.expect(.expression_open);
        while (!self.isKind(.expression_close)) {
            self.peek().dump();
            try self.consume();
        }
        try self.expect(.expression_close);
    }

    fn parseRoot(self: *Self) !void {
        while (!self.isEof()) {
            if (self.isKind(.text)) {
                // do something with a text node
                self.peek().dump();
                try self.consume();
            } else if (self.isKind(.statement_open)) {
                try self.parseStatement();
            } else if (self.isKind(.expression_open)) {
                try self.parseExpression();
            } else {
                // this is a parse error, unexpected token kind
                return ParseError.UnexpectedToken;
            }
        }
    }
};

test "simple parse" {
    try parse(std.testing.allocator, "", "hello {{ name }}!");
}

// TODO: write a grammar for the template language
const std = @import("std");
const Allocator = std.mem.Allocator;
const lexer = @import("./lexer.zig");
const Lexer = lexer.Lexer;
const Token = lexer.Token;

const Statement = union(enum) {
    output: []const u8,
    block: struct {
        id: []const u8,
        body: []Statement,
    },
    extends: []const u8,
    expr: void,
};

pub fn parse(allocator: *Allocator, name: []const u8, source: []const u8) !void {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var lex = Lexer.init(name, source);
    var it = lex.iterator();
    while (it.next()) |token| try tokens.append(token);

    var parser = Parser{
        .name = lex.name,
        .allocator = allocator,
        .source = source,
        .tokens = tokens.items,
        .current = @as(usize, 0),
        .hadError = false,
        .endBlockTag = null,
    };

    const stmts = parser.parseRoot() catch |err| {
        std.debug.print("parse error: {} current: {}\n", .{ err, parser.peek() });
        return;
    };
    defer { // FIXME put this logic in a custom dealloc hanging on Statement type
        for (stmts) |stmt| {
            switch (stmt) {
                .block => |block| allocator.free(block.body),
                else => {},
            }
        }
        allocator.free(stmts);
    }

    for (stmts) |stmt| {
        std.debug.print("{}\n", .{stmt});
    }
}

const ParseError = error{UnexpectedToken};

const Parser = struct {
    name: []const u8,
    allocator: *Allocator,
    source: []const u8,
    tokens: []const Token,
    current: usize,
    hadError: bool,
    endBlockTag: ?Token.Kind,

    const Self = @This();

    fn peek(self: *Self) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Self) Token {
        return self.tokens[self.current - 1];
    }

    fn consume(self: *Self) ParseError!void {
        self.current += 1;
    }

    fn @"error"(self: *Self, token: Token) void {
        std.debug.print("{}:{}: {}\n", .{ self.name, token.line, token.value });
        self.hadError = true;
    }

    fn expect(self: *Self, kind: Token.Kind) ParseError!void {
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

    fn parseBlock(self: *Self) ParseError!Statement {
        try self.expect(.keyword_block);
        try self.expect(.identifier);
        const id = self.previous().value;
        try self.expect(.block_close);
        self.endBlockTag = .keyword_endblock;
        const body = try self.parseRoot();
        try self.expect(.block_close);
        return Statement{
            .block = .{ .id = id, .body = body },
        };
    }

    fn parseExtends(self: *Self) ParseError!Statement {
        try self.expect(.keyword_extends);
        // TODO can parse an identifier instead
        try self.expect(.string);
        const extends = self.previous().value;
        try self.expect(.block_close);
        return Statement{ .extends = extends };
    }

    fn parseStatement(self: *Self) ParseError!?Statement {
        try self.expect(.block_open);
        const token = self.peek();
        if (self.endBlockTag == token.kind) {
            try self.consume();
            return null;
        }
        switch (token.kind) {
            .keyword_block => return try self.parseBlock(),
            .keyword_extends => return try self.parseExtends(),
            .keyword_for => unreachable,
            .keyword_if => unreachable,
            .keyword_elif => unreachable,
            .keyword_else => unreachable,
            else => {
                if (token.kind == .identifier) {
                    // TODO support extensions
                }
                return ParseError.UnexpectedToken;
            },
        }
        return true;
    }

    fn parseExpression(self: *Self) ParseError!void {
        try self.expect(.variable_open);
        while (!self.isEof() and !self.isKind(.variable_close)) {
            self.peek().dump();
            try self.consume();
        }
        try self.expect(.variable_close);
    }

    fn parseRoot(self: *Self) ParseError![]Statement {
        var stmt_list = std.ArrayList(Statement).init(self.allocator);
        while (!self.isEof()) {
            if (self.isKind(.text)) {
                const stmt = Statement{ .output = self.peek().value };
                try self.consume();
                stmt_list.append(stmt) catch unreachable; // FIXME
            } else if (self.isKind(.block_open)) {
                const stmt = try self.parseStatement();
                if (stmt == null) break;
                stmt_list.append(stmt.?) catch unreachable; // FIXME
            } else if (self.isKind(.variable_open)) {
                _ = try self.parseExpression();
                stmt_list.append(Statement.expr) catch unreachable; // FIXME
            } else {
                // this is a parse error, unexpected token kind
                return ParseError.UnexpectedToken;
            }
        }
        return stmt_list.toOwnedSlice();
    }
};

test "simple parse" {
    try parse(std.testing.allocator, "", "hello {{ name }}!");
    try parse(std.testing.allocator, "", "{% block title %}Greetings{% endblock %}");
    try parse(std.testing.allocator, "",
        \\
        \\{% block title %}Greetings{% endblock %}
        \\
        \\{% block body %}
        \\  Hello, {{ name }}!
        \\{% endblock %}
    );
}

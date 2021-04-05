// TODO:
// - [ ] go back to 'statement' and 'expression' for tags .... ??
// - [ ] write a grammar for the template language
// - [ ] implement a toString() method for Statement
const std = @import("std");
const Allocator = std.mem.Allocator;
const lexer = @import("./lexer.zig");
const Lexer = lexer.Lexer;
const Token = lexer.Token;

// Change the allocators to handles and do central memory management
// https://floooh.github.io/2018/06/17/handles-vs-pointers.html

const Expression = union(enum) {
    number: []const u8,
    ident: []const u8,
    string: []const u8,
    bin_op: struct {
        op: []const u8,
        lhs: *Expression,
        rhs: *Expression,
    },

    const Self = @This();

    fn destroy(self: *Self, allocator: *Allocator) void {
        switch (self.*) {
            .bin_op => |val| {
                val.lhs.destroy(allocator);
                val.rhs.destroy(allocator);
            },
            else => {},
        }
        allocator.destroy(self);
    }

    // constructors for the variants

    fn number(allocator: *Allocator, n: []const u8) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .number = n };
        return expr;
    }

    fn ident(allocator: *Allocator, id: []const u8) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .ident = id };
        return expr;
    }

    fn string(allocator: *Allocator, str: []const u8) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .string = str };
        return expr;
    }

    fn binOp(allocator: *Allocator, op: []const u8, lhs: *Expression, rhs: *Expression) !*Self {
        const expr = try allocator.create(Self);
        expr.* = .{ .bin_op = .{ .op = op, .lhs = lhs, .rhs = rhs } };
        return expr;
    }
};

const Statement = union(enum) {
    output: struct {
        text: []const u8,
    },
    block: struct {
        id: []const u8,
        body: []*Statement,
    },
    extends: struct {
        filename: []const u8,
    },
    @"for": struct {
        element: []const u8,
        collection: *Expression,
        body: []*Statement,
    },
    @"if": struct {
        predicate: *Expression,
        consequent: []*Statement,
    },
    expr: *Expression,

    const Self = @This();

    fn destroy(self: *Self, allocator: *Allocator) void {
        switch (self.*) {
            .block => |val| {
                for (val.body) |stmt| stmt.destroy(allocator);
                allocator.free(val.body);
            },
            .@"for" => |val| {
                val.collection.destroy(allocator);
                for (val.body) |stmt| stmt.destroy(allocator);
                allocator.free(val.body);
            },
            .@"if" => |val| {
                val.predicate.destroy(allocator);
                for (val.consequent) |stmt| stmt.destroy(allocator);
                allocator.free(val.consequent);
            },
            .expr => |val| val.destroy(allocator),
            else => {},
        }
        allocator.destroy(self);
    }

    // constructors for the variants

    fn output(allocator: *Allocator, text: []const u8) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .output = .{ .text = text } };
        return stmt;
    }

    fn block(allocator: *Allocator, id: []const u8, body: []*Statement) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .block = .{ .id = id, .body = body } };
        return stmt;
    }

    fn extends(allocator: *Allocator, filename: []const u8) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .extends = .{ .filename = filename } };
        return stmt;
    }

    fn @"for"(allocator: *Allocator, element: []const u8, collection: *Expression, body: []*Statement) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .@"for" = .{ .element = element, .collection = collection, .body = body } };
        return stmt;
    }

    fn @"if"(allocator: *Allocator, predicate: *Expression, consequent: []*Statement) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .@"if" = .{ .predicate = predicate, .consequent = consequent } };
        return stmt;
    }

    fn expression(allocator: *Allocator, expr: *Expression) !*Self {
        const stmt = try allocator.create(Self);
        stmt.* = .{ .expr = expr };
        return stmt;
    }
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
        .stmts = std.ArrayList(*Statement).init(allocator),
    };

    const stmts = parser.parseRoot() catch |err| {
        std.debug.print("caught parser error, current token: {}\n", .{parser.peek()});
        for (parser.stmts.items) |stmt| {
            stmt.destroy(allocator);
        }
        parser.stmts.deinit();
        return err;
    };

    for (stmts) |stmt| {
        std.debug.print("{}\n", .{stmt});
        stmt.destroy(allocator);
    }
    allocator.free(stmts);
}

const Error = error{ParseError} || Allocator.Error;

const Parser = struct {
    name: []const u8,
    allocator: *Allocator,
    source: []const u8,
    tokens: []const Token,
    current: usize,
    hadError: bool,
    endBlockTag: ?Token.Kind,
    stmts: std.ArrayList(*Statement), // holds in-progress parse results

    const Self = @This();

    fn peek(self: *Self) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *Self) Token {
        return self.tokens[self.current - 1];
    }

    fn consume(self: *Self) !void {
        self.peek().dump();
        self.current += 1;
    }

    fn @"error"(self: *Self, token: Token) void {
        std.debug.print("{}:{}: {}\n", .{ self.name, token.line, token.value });
        self.hadError = true;
    }

    fn expect(self: *Self, kind: Token.Kind) !void {
        if (self.isKind(kind)) {
            try self.consume();
        } else {
            return error.ParseError;
        }
    }

    fn isKind(self: *Self, kind: Token.Kind) bool {
        return self.peek().kind == kind;
    }

    fn isEof(self: *Self) bool {
        return self.current >= self.tokens.len;
    }

    fn parseBlock(self: *Self) Error!*Statement {
        try self.expect(.keyword_block);
        try self.expect(.identifier);
        const id = self.previous().value;
        try self.expect(.block_close);
        self.endBlockTag = .keyword_endblock;
        const body = try self.parseRoot();
        try self.expect(.block_close); // FIXME should be consumed in parseRoot?
        return Statement.block(self.allocator, id, body);
    }

    fn parseExtends(self: *Self) !*Statement {
        try self.expect(.keyword_extends);
        // TODO can parse an identifier instead
        try self.expect(.string);
        const filename = self.previous().value;
        try self.expect(.block_close);
        return Statement.extends(self.allocator, filename);
    }

    fn parseFor(self: *Self) Error!*Statement {
        try self.expect(.keyword_for);
        try self.expect(.identifier); // TODO support parsing key,val from a map
        const element = self.previous().value;
        try self.expect(.keyword_in);
        const collection = try self.parseExpression(@enumToInt(Precedence.lowest));
        try self.expect(.block_close);
        self.endBlockTag = .keyword_endfor;
        const body = try self.parseRoot();
        try self.expect(.block_close); // FIXME same as above
        return Statement.@"for"(self.allocator, element, collection, body);
    }

    fn parseIf(self: *Self) Error!*Statement {
        try self.expect(.keyword_if);
        const pred = try self.parseExpression(@enumToInt(Precedence.lowest));
        try self.expect(.block_close);
        self.endBlockTag = .keyword_endif;
        const body = try self.parseRoot();
        try self.expect(.block_close); // FIXME same as above
        return try Statement.@"if"(self.allocator, pred, body);
    }

    fn parseAtom(self: *Self) Error!*Expression {
        if (self.isKind(.open_paren)) {
            try self.consume();
            const expr = try self.parseExpression(@enumToInt(Precedence.lowest));
            try self.expect(.close_paren);
            return expr;
        } else if (self.isKind(.number)) {
            const expr = try Expression.number(self.allocator, self.peek().value);
            try self.consume();
            return expr;
        } else if (self.isKind(.identifier)) {
            const expr = try Expression.ident(self.allocator, self.peek().value);
            try self.consume();
            return expr;
        } else if (self.isKind(.string)) {
            const expr = try Expression.string(self.allocator, self.peek().value);
            try self.consume();
            return expr;
        } else {
            return error.ParseError; // FIXME more descriptive error
        }
    }

    fn isBinOp(op: []const u8) bool {
        return bin_ops.has(op);
    }

    fn getOpInfo(op: []const u8) OpInfo {
        return bin_ops.get(op).?;
    }

    const OpInfo = struct {
        assoc: enum {
            left,
            right,
        }, prec: Precedence
    };

    const Precedence = enum(u8) {
        lowest,
        assignment, // =
        @"or", // or
        @"and", // and
        equality, // == !=
        comparison, // < > <= >=
        addition, // + -
        multiplication, // * / %
    };

    // FIXME map from token kind instead of strings, maybe? who cares
    const bin_ops = std.ComptimeStringMap(OpInfo, .{
        .{ "and", .{ .assoc = .left, .prec = .@"and" } },
        .{ "or", .{ .assoc = .left, .prec = .@"or" } },
        .{ "==", .{ .assoc = .left, .prec = .equality } },
        .{ "!=", .{ .assoc = .left, .prec = .equality } },
        .{ "<", .{ .assoc = .left, .prec = .comparison } },
        .{ ">", .{ .assoc = .left, .prec = .comparison } },
        .{ "<=", .{ .assoc = .left, .prec = .comparison } },
        .{ ">=", .{ .assoc = .left, .prec = .comparison } },
        .{ "+", .{ .assoc = .left, .prec = .addition } },
        .{ "-", .{ .assoc = .left, .prec = .addition } },
        .{ "*", .{ .assoc = .left, .prec = .multiplication } },
        .{ "/", .{ .assoc = .left, .prec = .multiplication } },
        .{ "%", .{ .assoc = .left, .prec = .multiplication } },
    });

    var levels: usize = 0;

    fn parseExpression(self: *Self, min_prec: u8) Error!*Expression {
        var result = try self.parseAtom();
        errdefer result.destroy(self.allocator);
        while (isBinOp(self.peek().value)) {
            const op = self.peek().value;
            const op_info = getOpInfo(op);
            const prec = @enumToInt(op_info.prec);
            if (prec < min_prec) break;
            try self.consume();
            var next_prec = prec;
            if (op_info.assoc == .left) next_prec += 1;
            levels += 1;
            const rhs = try self.parseExpression(next_prec);
            levels -= 1;
            result = try Expression.binOp(self.allocator, op, result, rhs);
        }
        return result;
    }

    fn parseStatement(self: *Self) !?*Statement {
        try self.expect(.block_open);
        const token = self.peek();
        if (self.endBlockTag == token.kind) {
            try self.consume();
            return null;
        }
        switch (token.kind) {
            .keyword_block => return try self.parseBlock(),
            .keyword_extends => return try self.parseExtends(),
            .keyword_for => return try self.parseFor(),
            .keyword_if => return try self.parseIf(),
            else => {
                if (token.kind == .identifier) {
                    // TODO support extensions
                }
                return error.ParseError;
            },
        }
        return true;
    }

    fn parseExpressionStatement(self: *Self) !*Statement {
        try self.expect(.variable_open);
        const expr = try self.parseExpression(@enumToInt(Precedence.lowest));
        try self.expect(.variable_close);
        return try Statement.expression(self.allocator, expr);
    }

    fn parseRoot(self: *Self) ![]*Statement {
        while (!self.isEof()) {
            if (self.isKind(.text)) {
                const stmt = try Statement.output(self.allocator, self.peek().value);
                try self.consume();
                try self.stmts.append(stmt);
            } else if (self.isKind(.block_open)) {
                const stmt = try self.parseStatement();
                if (stmt == null) break;
                try self.stmts.append(stmt.?);
            } else if (self.isKind(.variable_open)) {
                const stmt = try self.parseExpressionStatement();
                try self.stmts.append(stmt);
            } else {
                // this is a parse error, unexpected token kind
                return error.ParseError;
            }
        }
        return self.stmts.toOwnedSlice();
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
    try parse(std.testing.allocator, "",
        \\{% extends "base.html" %}
    );
    try parse(std.testing.allocator, "",
        \\<ul>
        \\{% for name in name_list %}
        \\  <li>{{ name }}</li>
        \\{% endfor %}
    );
    try parse(std.testing.allocator, "", "{% if 2 + 5 < 8 and 6 * 7 > 41 %}ok{% endif %}");
}

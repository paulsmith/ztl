// TODO:
// - [ ] write a grammar for the template language
const std = @import("std");
const Allocator = std.mem.Allocator;
const lex = @import("./lex.zig");
const Lexer = lex.Lexer;
const Token = lex.Token;
const ast = @import("./ast.zig");
const Expression = ast.Expression;
const Statement = ast.Statement;
const Tree = ast.Tree;

// TODO Change the allocators to handles and do central memory management
// https://floooh.github.io/2018/06/17/handles-vs-pointers.html

const Error = error{ParseError} || Allocator.Error;

pub fn parse(allocator: *Allocator, name: []const u8, source: []const u8) !Tree {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var lexer = Lexer.init(name, source);
    var it = lexer.iterator();
    while (it.next()) |token| try tokens.append(token);

    var parser = Parser{
        .name = lexer.name,
        .allocator = allocator,
        .source = source,
        .tokens = tokens.items,
        .current = @as(usize, 0),
        .hadError = false,
        .endBlockTag = null,
    };

    const stmts = parser.parseRoot() catch |err| {
        //std.debug.print("caught parser error, current token: {}\n", .{parser.peek()});
        return err;
    };

    const tree = Tree{
        .stmts = stmts,
        .allocator = allocator,
    };

    return tree;
}

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

    fn consume(self: *Self) !void {
        //self.peek().dump();
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
        const name = self.previous().value;
        try self.expect(.block_close);
        self.endBlockTag = .keyword_endblock;
        const body = try self.parseRoot();
        try self.expect(.block_close); // FIXME should be consumed in parseRoot?
        return Statement.block(self.allocator, name, body);
    }

    fn parseExtends(self: *Self) Error!*Statement {
        // per template language, must be first statement in template if it occurs
        if (self.current != 1) return error.ParseError;
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
        const collection = try self.parseExpression(@enumToInt(BindingPower.lowest));
        try self.expect(.block_close);
        self.endBlockTag = .keyword_endfor;
        const body = try self.parseRoot();
        try self.expect(.block_close); // FIXME same as above
        return Statement.@"for"(self.allocator, element, collection, body);
    }

    fn parseIf(self: *Self) Error!*Statement {
        try self.expect(.keyword_if);
        const pred = try self.parseExpression(@enumToInt(BindingPower.lowest));
        try self.expect(.block_close);
        self.endBlockTag = .keyword_endif;
        const body = try self.parseRoot();
        try self.expect(.block_close); // FIXME same as above
        return try Statement.@"if"(self.allocator, pred, body);
    }

    const BindingPower = enum(u8) {
        @"null",
        lowest = 1,
        assignment = 10, // =
        @"or" = 20, // or
        @"and" = 30, // and
        equality = 40, // == !=
        comparison = 50, // < > <= >=
        addition = 60, // + -
        multiplication = 70, // * / %
        filter = 80, // |
        prefix_op = 90, // - + !
        call = 100, // . ()
    };

    const NullExprParseFn = fn (parser: *Parser, token: Token, bp: u8) Error!*Expression;
    const LeftExprParseFn = fn (parser: *Parser, token: Token, lhs: *Expression, rbp: u8) Error!*Expression;

    const NullDenotationRule = struct {
        kind: Token.Kind,
        bp: BindingPower,
        parse_fn: NullExprParseFn,
    };

    const LeftDenotationRule = struct {
        kind: Token.Kind,
        rbp: BindingPower,
        parse_fn: LeftExprParseFn,
    };

    const null_denotation_rules = [_]NullDenotationRule{
        .{ .kind = .minus, .bp = .prefix_op, .parse_fn = parseNullPrefixOp },
        .{ .kind = .plus, .bp = .prefix_op, .parse_fn = parseNullPrefixOp },
        .{ .kind = .not, .bp = .prefix_op, .parse_fn = parseNullPrefixOp },
        .{ .kind = .number, .bp = .@"null", .parse_fn = parseLeafExpr },
        .{ .kind = .string, .bp = .@"null", .parse_fn = parseLeafExpr },
        .{ .kind = .identifier, .bp = .@"null", .parse_fn = parseLeafExpr },
    };

    const left_denotation_rules = [_]LeftDenotationRule{
        .{ .kind = .plus, .rbp = .addition, .parse_fn = parseLeftBinOp },
        .{ .kind = .minus, .rbp = .addition, .parse_fn = parseLeftBinOp },
        .{ .kind = .star, .rbp = .multiplication, .parse_fn = parseLeftBinOp },
        .{ .kind = .forward_slash, .rbp = .multiplication, .parse_fn = parseLeftBinOp },
        .{ .kind = .less_than, .rbp = .comparison, .parse_fn = parseLeftBinOp },
        .{ .kind = .greater_than, .rbp = .comparison, .parse_fn = parseLeftBinOp },
        .{ .kind = .keyword_and, .rbp = .@"and", .parse_fn = parseLeftBinOp },
        .{ .kind = .keyword_or, .rbp = .@"or", .parse_fn = parseLeftBinOp },
        .{ .kind = .pipe, .rbp = .filter, .parse_fn = parseLeftBinOp },
        .{ .kind = .dot, .rbp = .call, .parse_fn = parseLeftBinOp },
    };

    fn parseLeafExpr(parser: *Parser, token: Token, bp: u8) Error!*Expression {
        return switch (token.kind) {
            .number => try Expression.number(parser.allocator, token.value),
            .string => try Expression.string(parser.allocator, token.value),
            .identifier => try Expression.name(parser.allocator, token.value),
            else => error.ParseError,
        };
    }

    fn parseNullPrefixOp(parser: *Parser, token: Token, bp: u8) Error!*Expression {
        const expr = try parser.parseExpression(bp);
        return Expression.unaryOp(parser.allocator, token.value, expr); // FIXME copy token string value
    }

    fn parseLeftBinOp(parser: *Parser, token: Token, lhs: *Expression, bp: u8) Error!*Expression {
        const rhs = try parser.parseExpression(bp);
        return Expression.binOp(parser.allocator, token.value, lhs, rhs);
    }

    fn lookupNull(self: *Self) ?NullDenotationRule {
        const token = self.peek();
        for (null_denotation_rules) |rule| if (rule.kind == token.kind) return rule;
        return null;
    }

    fn parsePrefix(self: *Self) Error!*Expression {
        const rule = self.lookupNull() orelse return error.ParseError;
        const token = self.peek();
        try self.consume();
        return try rule.parse_fn(self, token, @enumToInt(rule.bp));
    }

    fn lookupLeft(self: *Self) ?LeftDenotationRule {
        const token = self.peek();
        for (left_denotation_rules) |rule| if (rule.kind == token.kind) return rule;
        return null;
    }

    fn rbp(self: *Self) Error!u8 {
        const rule = self.lookupLeft();
        const bp = if (rule != null) rule.?.rbp else BindingPower.@"null";
        return @enumToInt(bp);
    }

    fn parseInfix(self: *Self, lhs: *Expression) Error!*Expression {
        const rule = self.lookupLeft() orelse return error.ParseError;
        const token = self.peek();
        try self.consume();
        return try rule.parse_fn(self, token, lhs, @enumToInt(rule.rbp));
    }

    fn parseExpression(self: *Self, bp: u8) Error!*Expression {
        var lhs = try self.parsePrefix();
        while ((try self.rbp()) > bp) lhs = try self.parseInfix(lhs);
        return lhs;
    }

    fn parseStatement(self: *Self) Error!?*Statement {
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

    fn parseExpressionStatement(self: *Self) Error!*Statement {
        try self.expect(.variable_open);
        const expr = try self.parseExpression(@enumToInt(BindingPower.lowest));
        try self.expect(.variable_close);
        return try Statement.expression(self.allocator, expr);
    }

    fn parseRoot(self: *Self) Error![]*Statement {
        var stmt_list = std.ArrayList(*Statement).init(self.allocator);
        errdefer {
            // FIXME I don't love this ...
            for (stmt_list.items) |stmt| stmt.destroy(self.allocator);
            stmt_list.deinit();
        }
        while (!self.isEof()) {
            if (self.isKind(.text)) {
                const stmt = try Statement.output(self.allocator, self.peek().value);
                try self.consume();
                try stmt_list.append(stmt);
            } else if (self.isKind(.block_open)) {
                const stmt = try self.parseStatement();
                if (stmt == null) break;
                try stmt_list.append(stmt.?);
            } else if (self.isKind(.variable_open)) {
                const stmt = try self.parseExpressionStatement();
                try stmt_list.append(stmt);
            } else {
                // this is a parse error, unexpected token kind
                return error.ParseError;
            }
        }
        return stmt_list.toOwnedSlice();
    }
};

fn testParseDumpTree(source: []const u8) !void {
    var tree = try parse(std.testing.allocator, "", source);
    for (tree.stmts) |stmt| std.debug.print("{} ", .{stmt});
    std.debug.print("\n", .{});
    tree.destroy();
}

fn testParse(source: []const u8, want: []const u8) !void {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();
    var writer = buf.writer();
    var tree = parse(std.testing.allocator, "", source) catch @panic("couldn't parse");
    defer tree.destroy();
    for (tree.stmts) |stmt, i| {
        std.fmt.format(writer, "{}", .{stmt.formatter()}) catch @panic("couldn't format");
        if (i < tree.stmts.len - 1) std.fmt.format(writer, "\n", .{}) catch @panic("couldn't format");
    }
    std.testing.expectEqualStrings(want, buf.items);
}

test "simple parse" {
    try testParse("please say hello to {{ name }}!",
        \\(Output (text "please say..."))
        \\(Expr name)
        \\(Output (text "!"))
    );
    try testParse("{% block title %}Greetings{% endblock %}",
        \\(Block (name title) (body (Output (text "Greetings"))))
    );
    try testParse(
        \\{% extends "base.html" %}
    ,
        \\(Extends (filename "base.html"))
    );
    try testParse(
        \\{% block title %}Greetings{% endblock %}
        \\
        \\{% block body %}
        \\  Hello, {{ name }}!
        \\{% endblock %}
    ,
        \\(Block (name title) (body (Output (text "Greetings"))))
        \\(Output (text "\n\n"))
        \\(Block (name body) (body (Output (text "\n  Hello, ")) (Expr name) (Output (text "!\n"))))
    );
    try testParse(
        \\<ul>
        \\{% for name in name_list %}
        \\  <li>{{ name }}</li>
        \\{% endfor %}
    ,
        \\(Output (text "<ul>\n"))
        \\(For (element name) (collection name_list) (body (Output (text "\n  <li>")) (Expr name) (Output (text "</li>\n"))))
    );
    try testParse("{% if 2 + 5 < 8 and 6 * 7 > 41 %}ok{% endif %}",
        \\(If (predicate (and (> (* (< (+ 2 5) 8) 6 7) 41))) (consequent (Output (text "ok"))))
    );
    try testParse("{{ foo.quux.fnord | bar | baz }}",
        \\(Expr (| (| (. (. foo quux) fnord) bar) baz))
    );
}

fn testParseError(source: []const u8) void {
    var tree = parse(std.testing.allocator, "", source);
    std.testing.expectError(error.ParseError, tree);
}

test "parse errors" {
    testParseError(
        \\extends must be very first thing if it is present
        \\{% extends "blah" %}
    );
}

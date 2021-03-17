// TODO
// - [x] implement variables/expressions
// - [x] add line numbers
// - [ ] track name of file
// - [ ] compress some of the starting tag logic

const std = @import("std");
const ascii = std.ascii;
const testing = std.testing;
const mem = std.mem;
const fmt = std.fmt;
const assert = std.debug.assert;

pub const Token = struct {
    value: []const u8,
    kind: Kind,
    line: usize,

    pub const Kind = enum {
        text,
        statement_open,
        statement_close,
        expression_open,
        expression_close,
        comment_open,
        comment_close,
        identifier,
        keyword_block,
        keyword_endblock,
        keyword_extends,
        keyword_for,
        keyword_endfor,
        keyword_in,
        keyword_if,
        keyword_elif,
        keyword_else,
        keyword_endif,
        keyword_is,
        keyword_not,
        keyword_and,
        keyword_or,
        @"error",
        eof,
    };

    pub fn dump(token: Token) void {
        std.debug.print("{}, line {}: \"{}\"\n", .{ @tagName(token.kind), token.line, token.value });
    }
};

const keywords = std.ComptimeStringMap(Token.Kind, .{
    .{ "block", .keyword_block },
    .{ "endblock", .keyword_endblock },
    .{ "extends", .keyword_extends },
    .{ "for", .keyword_for },
    .{ "endfor", .keyword_endfor },
    .{ "in", .keyword_in },
    .{ "if", .keyword_if },
    .{ "elif", .keyword_elif },
    .{ "else", .keyword_else },
    .{ "endif", .keyword_endif },
    .{ "is", .keyword_is },
    .{ "not", .keyword_not },
    .{ "and", .keyword_and },
    .{ "or", .keyword_or },
});

const statement_open_delim = "{%";
const statement_close_delim = "%}";
const expression_open_delim = "{{";
const expression_close_delim = "}}";
const comment_open_delim = "{#";
const comment_close_delim = "#}";

var error_message_buffer: [100]u8 = undefined;
const error_message_buf = error_message_buffer[0..];

const Lexer = struct {
    source: []const u8,
    start: usize,
    pos: usize,
    state: State,
    line: usize,
    startline: usize,
    open_delim: ?Token.Kind,

    const State = enum {
        text,
        tag_open,
        inside_tag,
        identifier,
        tag_close,
        comment_open,
        eof,
    };

    const Self = @This();

    pub fn init(source: []const u8) Self {
        return Self{
            .source = source,
            .start = @as(usize, 0),
            .pos = @as(usize, 0),
            .state = .text,
            .line = @as(usize, 1),
            .startline = @as(usize, 1),
            .open_delim = null,
        };
    }

    const Iterator = struct {
        lexer: *Lexer,
        eof: bool,

        pub fn next(it: *Iterator) ?Token {
            if (it.eof) return null;
            const token = it.lexer.nextToken();
            if (token.kind == .eof) {
                it.eof = true;
                return null;
            }
            return token;
        }
    };

    pub fn iterator(self: *Self) Iterator {
        return Iterator{
            .lexer = self,
            .eof = false,
        };
    }

    // consumes the input between start and pos
    fn makeToken(self: *Self, kind: Token.Kind) Token {
        const token = Token{
            .value = self.source[self.start..self.pos],
            .kind = kind,
            .line = self.startline,
        };
        self.start = self.pos;
        self.startline = self.line;
        return token;
    }

    fn nextInput(self: *Self) ?u8 {
        assert(!(self.pos > self.source.len));
        if (self.pos == self.source.len) return null;
        const c = self.source[self.pos];
        if (c == '\n') self.line += 1;
        self.pos += 1;
        return c;
    }

    fn peekInput(self: *Self) ?u8 {
        const in = self.nextInput();
        self.pos -= 1;
        return in;
    }

    fn backup(self: *Self) void {
        self.pos -= 1;
        if (self.source[self.pos] == '\n') self.line -= 1;
    }

    fn ignore(self: *Self) void {
        self.start = self.pos;
    }

    pub fn nextToken(self: *Self) Token {
        while (true) {
            switch (self.state) {
                .text => {
                    if (mem.indexOf(u8, self.source[self.pos..], statement_open_delim)) |x| {
                        self.pos += x;
                        self.open_delim = .statement_open;
                        self.state = .tag_open;
                        if (self.pos > self.start) {
                            self.line += mem.count(u8, self.source[self.start..self.pos], "\n");
                            return self.makeToken(.text);
                        }
                        continue;
                    } else if (mem.indexOf(u8, self.source[self.pos..], expression_open_delim)) |x| {
                        self.pos += x;
                        self.open_delim = .expression_open;
                        self.state = .tag_open;
                        if (self.pos > self.start) {
                            self.line += mem.count(u8, self.source[self.start..self.pos], "\n");
                            return self.makeToken(.text);
                        }
                        continue;
                    } else if (mem.indexOf(u8, self.source[self.pos..], comment_open_delim)) |x| {
                        self.pos += x;
                        self.state = .comment_open;
                        if (self.pos > self.start) {
                            self.line += mem.count(u8, self.source[self.start..self.pos], "\n");
                            return self.makeToken(.text);
                        }
                        continue;
                    }
                    self.pos = self.source.len;
                    self.state = .eof;
                    if (self.pos > self.start) {
                        self.line += mem.count(u8, self.source[self.start..self.pos], "\n");
                        return self.makeToken(.text);
                    }
                },

                .tag_open => {
                    if (self.open_delim) |delim| {
                        self.pos += mem.len(switch (delim) {
                            .statement_open => statement_open_delim,
                            .expression_open => expression_open_delim,
                            else => unreachable,
                        });
                        self.state = .inside_tag;
                        return self.makeToken(delim);
                    } else std.debug.panic("invalid state, expected non-null open_delim", .{});
                },

                .inside_tag => {
                    // We test for matching correct closing delimiters here in
                    // the lexer and not the parser because from a tokenization
                    // POV we need to know if the lexer's state is inside a
                    // 'tag' (a statement or expression) or in regular raw
                    // template text.
                    const delim = self.open_delim orelse unreachable;
                    if (mem.startsWith(u8, self.source[self.pos..], statement_close_delim)) {
                        if (delim == .expression_open) return self.@"error"("invalid closing delimiter: expected '{}', found '{}'", .{ expression_close_delim, statement_close_delim });
                        self.state = .tag_close;
                    } else if (mem.startsWith(u8, self.source[self.pos..], expression_close_delim)) {
                        if (delim == .statement_open) return self.@"error"("invalid closing delimiter: expected '{}', found '{}'", .{ statement_close_delim, expression_close_delim });
                        self.state = .tag_close;
                    } else {
                        if (self.nextInput()) |c| {
                            switch (c) {
                                'A'...'Z', 'a'...'z', '_' => self.state = .identifier,
                                ' ', '\t', '\r', '\n' => {
                                    while (true) {
                                        if (self.peekInput()) |cc| {
                                            if (!ascii.isSpace(cc)) break;
                                        } else {
                                            break;
                                        }
                                        _ = self.nextInput();
                                    }
                                    self.start = self.pos;
                                },
                                else => return self.@"error"("unexpected char '{c}'", .{c}),
                            }
                        } else {
                            // TODO handle unclosed tag
                            self.state = .eof;
                            continue;
                        }
                    }
                },

                .identifier => {
                    while (true) {
                        if (self.nextInput()) |c| {
                            switch (c) {
                                'A'...'Z', 'a'...'z', '_', '0'...'9' => {},
                                else => {
                                    self.backup();
                                    self.state = .inside_tag;
                                    break;
                                },
                            }
                        } else {
                            self.state = .eof;
                            break;
                        }
                    }
                    const string = self.source[self.start..self.pos];
                    // TODO check terminator
                    if (keywords.get(string)) |kind| {
                        return self.makeToken(kind);
                    }
                    return self.makeToken(.identifier);
                },

                .tag_close => {
                    if (self.open_delim) |delim| {
                        self.pos += mem.len(switch (delim) {
                            .statement_open => statement_close_delim,
                            .expression_open => expression_close_delim,
                            else => unreachable,
                        });
                        self.state = .text;
                        const token = self.makeToken(switch (delim) {
                            .statement_open => .statement_close,
                            .expression_open => .expression_close,
                            else => unreachable,
                        });
                        self.open_delim = null;
                        return token;
                    } else std.debug.panic("invalid state, expected non-null open_delim", .{});
                },

                .comment_open => {
                    if (mem.indexOf(u8, self.source[self.pos..], comment_close_delim)) |x| {
                        self.pos += x + mem.len(comment_close_delim);
                        self.start = self.pos;
                        self.state = .text;
                    } else {
                        return self.@"error"("unclosed comment", .{});
                    }
                },

                .eof => {
                    return self.makeToken(.eof);
                },
            }
        }
        unreachable;
    }

    fn @"error"(self: *Self, comptime message: []const u8, args: anytype) Token {
        // TODO resynchronize or move to eof?
        self.start = self.pos;
        self.state = .eof;
        return Token{
            .value = fmt.bufPrint(error_message_buf, message, args) catch unreachable,
            .kind = .@"error",
            .line = self.line,
        };
    }
};

fn testLexer(source: []const u8, expected_tokens: []const Token.Kind) void {
    var lexer = Lexer.init(source);
    var it = lexer.iterator();
    for (expected_tokens) |expected, i| {
        if (it.next()) |token| {
            if (token.kind != expected) {
                std.debug.panic("want {}, got {} - value: '{}'", .{ @tagName(expected), @tagName(token.kind), token.value });
            }
        } else {
            std.debug.panic("expected a {} token at pos {d}", .{ @tagName(expected), i });
        }
    }
    const last = lexer.nextToken();
    std.testing.expectEqual(last.kind, .eof);
}

test "lexer - simple template" {
    testLexer("", &[_]Token.Kind{});
    testLexer("<title>", &[_]Token.Kind{.text});
    testLexer("{%", &[_]Token.Kind{.statement_open});
    testLexer("<title>\n{%", &[_]Token.Kind{ .text, .statement_open });
    testLexer("<title>\n{% block", &[_]Token.Kind{ .text, .statement_open, .keyword_block });
    testLexer("<title>\n{% block title", &[_]Token.Kind{ .text, .statement_open, .keyword_block, .identifier });
    testLexer("<title>\n{% block title %}", &[_]Token.Kind{ .text, .statement_open, .keyword_block, .identifier, .statement_close });
    testLexer("<title>\n{% block title %}\n{% endblock %}</title>", &[_]Token.Kind{ .text, .statement_open, .keyword_block, .identifier, .statement_close, .text, .statement_open, .keyword_endblock, .statement_close, .text });
    testLexer("comment test {# this is a comment #}\nrest of text", &[_]Token.Kind{ .text, .text });
    testLexer("{{ variable_test }}", &[_]Token.Kind{ .expression_open, .identifier, .expression_close });
}

fn testLexerLineNo(source: []const u8, line_numbers: []const usize, ending_line: usize) void {
    var lexer = Lexer.init(source);
    var it = lexer.iterator();
    for (line_numbers) |line, i| {
        if (it.next()) |token| {
            std.testing.expectEqual(line, token.line);
        } else {
            std.debug.panic("expected a {}th token, but ran out", .{i});
        }
    }
    const last = lexer.nextToken();
    std.testing.expectEqual(last.kind, .eof);
    std.testing.expectEqual(last.line, ending_line);
}

test "lexer - tracking line numbers" {
    testLexerLineNo("", &[_]usize{}, 1);
    testLexerLineNo("<title>", &[_]usize{1}, 1);
    testLexerLineNo("<title>\n", &[_]usize{1}, 2);
    testLexerLineNo("<title>\n{%", &[_]usize{ 1, 2 }, 2);
    testLexerLineNo("<title>\n{% block %}", &[_]usize{ 1, 2, 2, 2 }, 2);
    testLexerLineNo("<title>\n{% block %}\n</title>", &[_]usize{ 1, 2, 2, 2, 2 }, 3);
}

test "lexer - error invalid closing delimiter" {
    const strings = [_][]const u8{ "{% foo }}", "{{ bar %}" };
    for (strings) |source| {
        var lexer = Lexer.init(source);
        var it = lexer.iterator();
        var last: Token = undefined;
        while (it.next()) |token| {
            last = token;
        }
        std.testing.expectEqual(Token.Kind.@"error", last.kind);
    }
}

// test "lex template" {
//     const example =
//         \\<ul>
//         \\{% for item in mylist %}
//         \\  <li><a href="{{ item.url }}">{{ item.name }}</a></li>
//         \\{% endfor %}
//         \\</ul>
//         \\<p>{{ a_variable }}</p>
//         \\{# a comment #}
//     ;
//     var lexer = Lexer.init(example);
//     var token = lexer.nextToken();
//     while (token.kind != .eof) {
//         lexer.dumpToken(token);
//         token = lexer.nextToken();
//     }
// }

// NOTES
//
// token types inside tags and variables (rename to statements and expressions)
//
// "base.html" - string literal
// block - keyword
// foo - identifier
// ( - lparen (start a function call)
// ) - rparen
// . - field access
// | - pipe (filters)
// [ - lbracket (start array index notation)
// 0 - number literal
// ] - rbracket
// , - comma
// ~ - tilde (string concatenation)
// = - equals / assign
// - - unary minus
// + - * / %  - math operators
// < > == >= <= != - comparison operators

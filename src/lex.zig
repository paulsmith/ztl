const std = @import("std");
const ascii = std.ascii;
const testing = std.testing;
const mem = std.mem;
const fmt = std.fmt;
const assert = std.debug.assert;
const math = std.math;

pub const Token = struct {
    value: []const u8,
    kind: Kind,
    line: usize,

    pub const Kind = enum {
        text,
        block_open,
        block_close,
        variable_open,
        variable_close,
        comment_open,
        comment_close,
        identifier, // names
        string, // string literal
        number, // number literal
        open_paren, // (
        close_paren, // )
        dot, // .
        pipe, // |
        open_bracket, // [
        close_bracket, // ]
        comma, // ,
        tilde, // ~
        assign, // =
        minus, // -
        plus, // +
        star, // *
        forward_slash, // /
        percent, // %
        less_than, // <
        greater_than, // >
        equal_to, // ==
        gt_or_equal_to, // >=
        lt_or_equal_to, // <=
        not, // !
        not_equal, // !=
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
        keyword_raw,
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
    .{ "raw", .keyword_raw },
});

const block_open_delim = "{%";
const block_close_delim = "%}";
const variable_open_delim = "{{";
const variable_close_delim = "}}";
const comment_open_delim = "{#";
const comment_close_delim = "#}";

var error_message_buffer: [100]u8 = undefined;
const error_message_buf = error_message_buffer[0..];

pub const Lexer = struct {
    name: []const u8,
    source: []const u8,
    start: usize,
    pos: usize,
    state: State,
    line: usize,
    startline: usize,
    delim_kind: ?Token.Kind,
    paren_depth: usize,

    const State = enum {
        text,
        tag_open,
        inside_tag,
        identifier,
        string,
        number,
        tag_close,
        comment_open,
        eof,
    };

    const Self = @This();

    pub fn init(name: []const u8, source: []const u8) Self {
        return Self{
            .name = if (std.mem.eql(u8, name, "")) "<string>" else name,
            .source = source,
            .start = @as(usize, 0),
            .pos = @as(usize, 0),
            .state = .text,
            .line = @as(usize, 1),
            .startline = @as(usize, 1),
            .delim_kind = null,
            .paren_depth = @as(usize, 0),
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
                    const OpenDelimiter = struct {
                        delimiter: []const u8,
                        delimiter_kind: ?Token.Kind,
                        next_state: State,
                    };

                    const open_delimiters = [_]OpenDelimiter{
                        .{ .delimiter = block_open_delim, .delimiter_kind = .block_open, .next_state = .tag_open },
                        .{ .delimiter = variable_open_delim, .delimiter_kind = .variable_open, .next_state = .tag_open },
                        .{ .delimiter = comment_open_delim, .delimiter_kind = null, .next_state = .comment_open },
                    };

                    var nearest: ?OpenDelimiter = null;
                    var distance: usize = math.maxInt(usize);
                    for (open_delimiters) |d| {
                        if (mem.indexOf(u8, self.source[self.pos..], d.delimiter)) |x| {
                            if (x < distance) {
                                distance = x;
                                nearest = d;
                            }
                        }
                    }
                    if (nearest) |d| {
                        self.pos += distance;
                        self.delim_kind = d.delimiter_kind;
                        self.state = d.next_state;
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
                    const delim = self.delim_kind orelse std.debug.panic("invariant non-null delim_kind violated", .{});
                    self.pos += mem.len(switch (delim) {
                        .block_open => block_open_delim,
                        .variable_open => variable_open_delim,
                        else => unreachable,
                    });
                    self.paren_depth = 0;
                    self.state = .inside_tag;
                    return self.makeToken(delim);
                },

                .inside_tag => {
                    const CloseDelimiter = struct {
                        delimiter: []const u8,
                        open_delim_kind: Token.Kind,
                        close_delim_kind: Token.Kind,
                    };

                    const close_delimiters = [_]CloseDelimiter{
                        .{ .delimiter = block_close_delim, .open_delim_kind = .block_open, .close_delim_kind = .block_close },
                        .{ .delimiter = variable_close_delim, .open_delim_kind = .variable_open, .close_delim_kind = .variable_close },
                    };

                    const CompareEqOp = struct {
                        first_char: u8,
                        single_char_kind: Token.Kind,
                        eq_kind: Token.Kind,
                    };

                    const compare_eq_ops = [_]CompareEqOp{
                        .{ .first_char = '=', .single_char_kind = .assign, .eq_kind = .equal_to },
                        .{ .first_char = '<', .single_char_kind = .less_than, .eq_kind = .lt_or_equal_to },
                        .{ .first_char = '>', .single_char_kind = .greater_than, .eq_kind = .gt_or_equal_to },
                        .{ .first_char = '!', .single_char_kind = .not, .eq_kind = .not_equal },
                    };

                    // We test for matching correct closing delimiters here in
                    // the lexer and not the parser because from a tokenization
                    // POV we need to know if the lexer's state is inside a
                    // 'tag' (a block or a variable) or in regular raw
                    // template text.
                    const delim = self.delim_kind orelse std.debug.panic("invariant non-null delim_kind violated", .{});
                    var found = false;
                    for (close_delimiters) |d| {
                        if (mem.startsWith(u8, self.source[self.pos..], d.delimiter)) {
                            if (delim != d.open_delim_kind) return self.@"error"("invalid closing delimiter: expected '{}', found '{}'", .{ d.open_delim_kind, @tagName(delim) });
                            if (self.paren_depth != 0) return self.@"error"("unbalanced parens", .{});
                            self.delim_kind = d.close_delim_kind;
                            self.state = .tag_close;
                            found = true;
                            break;
                        }
                    }
                    if (found) continue;
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
                            '"' => self.state = .string,
                            '0'...'9' => self.state = .number,
                            '(' => {
                                self.paren_depth += 1;
                                return self.makeToken(.open_paren);
                            },
                            ')' => {
                                if (self.paren_depth == 0) return self.@"error"("unbalanced parens", .{});
                                self.paren_depth -= 1;
                                return self.makeToken(.close_paren);
                            },
                            // TODO yes these could probably be a lookup but
                            // unclear what the value would be
                            '.' => return self.makeToken(.dot),
                            '|' => return self.makeToken(.pipe),
                            '[' => return self.makeToken(.open_bracket),
                            ']' => return self.makeToken(.close_bracket),
                            ',' => return self.makeToken(.comma),
                            '~' => return self.makeToken(.tilde),
                            '-' => return self.makeToken(.minus),
                            '+' => return self.makeToken(.plus),
                            '*' => return self.makeToken(.star),
                            '/' => return self.makeToken(.forward_slash),
                            '%' => return self.makeToken(.percent),
                            '=', '<', '>', '!' => {
                                for (compare_eq_ops) |op| {
                                    if (op.first_char == c) {
                                        if (self.peekInput()) |peek| {
                                            if (peek == '=') {
                                                _ = self.nextInput();
                                                return self.makeToken(op.eq_kind);
                                            }
                                        }
                                        return self.makeToken(op.single_char_kind);
                                    }
                                }
                                unreachable;
                            },
                            else => return self.@"error"("unexpected char '{c}'", .{c}),
                        }
                    } else {
                        // TODO handle unclosed tag
                        self.state = .eof;
                        continue;
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

                .string => {
                    self.ignore();
                    while (true) {
                        if (self.nextInput()) |c| {
                            switch (c) {
                                '\n' => return self.@"error"("newline not allowed in string literal", .{}),
                                '"' => break,
                                else => {},
                            }
                        } else {
                            return self.@"error"("unterminated string literal", .{});
                        }
                    }
                    self.backup();
                    const token = self.makeToken(.string);
                    self.state = .inside_tag;
                    _ = self.nextInput();
                    return token;
                },

                // FIXME only supports simple integers at the moment
                .number => {
                    while (true) {
                        if (self.nextInput()) |c| {
                            if (!ascii.isDigit(c)) {
                                self.backup();
                                break;
                            }
                        } else {
                            // TODO if EOF then the token stream does not have a closing tag
                            break;
                        }
                    }
                    self.state = .inside_tag;
                    return self.makeToken(.number);
                },

                .tag_close => {
                    const delim = self.delim_kind orelse std.debug.panic("invariant non-null delim_kind violated", .{});
                    self.pos += mem.len(switch (delim) {
                        .block_close => block_close_delim,
                        .variable_close => variable_close_delim,
                        else => unreachable,
                    });
                    self.state = .text;
                    const token = self.makeToken(delim);
                    self.delim_kind = null;
                    return token;
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
            .line = self.startline,
        };
    }
};

fn testLexer(source: []const u8, expected_tokens: []const Token.Kind) void {
    var lexer = Lexer.init("", source);
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

test "lex simple template" {
    testLexer("", &[_]Token.Kind{});
    testLexer("<title>", &[_]Token.Kind{.text});
    testLexer("{%", &[_]Token.Kind{.block_open});
    testLexer("<title>\n{%", &[_]Token.Kind{ .text, .block_open });
    testLexer("<title>\n{% block", &[_]Token.Kind{ .text, .block_open, .keyword_block });
    testLexer("<title>\n{% block title", &[_]Token.Kind{ .text, .block_open, .keyword_block, .identifier });
    testLexer("<title>\n{% block title %}", &[_]Token.Kind{ .text, .block_open, .keyword_block, .identifier, .block_close });
    testLexer("<title>\n{% block title %}\n{% endblock %}</title>", &[_]Token.Kind{ .text, .block_open, .keyword_block, .identifier, .block_close, .text, .block_open, .keyword_endblock, .block_close, .text });
    testLexer("comment test {# this is a comment #}\nrest of text", &[_]Token.Kind{ .text, .text });
    testLexer("{{ variable_test }}", &[_]Token.Kind{ .variable_open, .identifier, .variable_close });
    testLexer("{% ( ( ) ) %}", &[_]Token.Kind{ .block_open, .open_paren, .open_paren, .close_paren, .close_paren, .block_close });
    testLexer("{% . | [ ] , ~ = - + * / % < > ! == <= >= != %}", &[_]Token.Kind{ .block_open, .dot, .pipe, .open_bracket, .close_bracket, .comma, .tilde, .assign, .minus, .plus, .star, .forward_slash, .percent, .less_than, .greater_than, .not, .equal_to, .lt_or_equal_to, .gt_or_equal_to, .not_equal, .block_close });
}

fn testLexerLineNo(source: []const u8, line_numbers: []const usize, ending_line: usize) void {
    var lexer = Lexer.init("", source);
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

test "tracking line numbers" {
    testLexerLineNo("", &[_]usize{}, 1);
    testLexerLineNo("<title>", &[_]usize{1}, 1);
    testLexerLineNo("<title>\n", &[_]usize{1}, 2);
    testLexerLineNo("<title>\n{%", &[_]usize{ 1, 2 }, 2);
    testLexerLineNo("<title>\n{% block %}", &[_]usize{ 1, 2, 2, 2 }, 2);
    testLexerLineNo("<title>\n{% block %}\n</title>", &[_]usize{ 1, 2, 2, 2, 2 }, 3);
}

test "error last token" {
    const strings = [_][]const u8{ "{% foo }}", "{{ bar %}", "{% ( ) ) %}", "{% ( ( ) %}" };
    for (strings) |source| {
        var lexer = Lexer.init("", source);
        var it = lexer.iterator();
        var last: Token = undefined;
        while (it.next()) |token| last = token;
        std.testing.expectEqual(Token.Kind.@"error", last.kind);
    }
}

test "string literals" {
    var lexer = Lexer.init("",
        \\{% "hello, world"
    );
    _ = lexer.nextToken();
    const token = lexer.nextToken();
    std.testing.expectEqual(Token.Kind.string, token.kind);
    std.testing.expect(mem.eql(u8, "hello, world", token.value));
}

test "numbers" {
    var lexer = Lexer.init("",
        \\{% 42 %}
    );
    _ = lexer.nextToken();
    const token = lexer.nextToken();
    std.testing.expectEqual(Token.Kind.number, token.kind);
    std.testing.expect(mem.eql(u8, "42", token.value));
}

test "lex next closest delimiter" {
    testLexer("{{ foo }}{% bar %}", &[_]Token.Kind{ .variable_open, .identifier, .variable_close, .block_open, .identifier, .block_close });
}

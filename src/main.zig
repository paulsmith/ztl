const std = @import("std");
const ascii = std.ascii;
const testing = std.testing;
const mem = std.mem;

pub const Token = struct {
    value: []const u8,
    kind: Kind,

    pub const Kind = enum {
        text,
        tag_open,
        tag_close,
        variable_open,
        variable_close,
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
        eof,
    };

    pub fn dump(token: Token) void {
        std.debug.print("{}: \"{}\"\n", .{ @tagName(token.kind), token.value });
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

const tag_open_delim = "{%";
const tag_close_delim = "%}";
const variable_open_delim = "{{";
const variable_close_delim = "}}";
const comment_open_delim = "{#";
const comment_close_delim = "#}";

const Lexer = struct {
    source: []const u8,
    start: usize,
    pos: usize,
    state: State,

    const State = enum {
        text,
        tag_open,
        inside_tag,
        identifier,
        tag_close,
        eof,
    };

    const Self = @This();

    pub fn init(source: []const u8) Self {
        return Self{
            .source = source,
            .start = @as(usize, 0),
            .pos = @as(usize, 0),
            .state = .text,
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
        };
        self.start = self.pos;
        return token;
    }

    fn nextInput(self: *Self) ?u8 {
        std.debug.assert(!(self.pos > self.source.len));
        if (self.pos == self.source.len) return null;
        const c = self.source[self.pos];
        self.pos += 1;
        return c;
    }

    fn peekInput(self: *Self) ?u8 {
        const in = self.nextInput();
        self.pos -= 1;
        return in;
    }

    pub fn nextToken(self: *Self) Token {
        while (true) {
            switch (self.state) {
                .text => {
                    if (mem.indexOf(u8, self.source[self.pos..], tag_open_delim)) |x| {
                        self.pos += x;
                        self.state = .tag_open;
                        if (self.pos > self.start) {
                            return self.makeToken(.text);
                        }
                        continue;
                    }
                    self.pos = self.source.len;
                    self.state = .eof;
                    if (self.pos > self.start) {
                        return self.makeToken(.text);
                    }
                },

                .tag_open => {
                    self.pos += mem.len(tag_open_delim);
                    self.state = .inside_tag;
                    return self.makeToken(.tag_open);
                },

                .inside_tag => {
                    if (mem.startsWith(u8, self.source[self.pos..], tag_close_delim)) {
                        self.state = .tag_close;
                    } else {
                        if (self.nextInput()) |c| {
                            switch (c) {
                                'A'...'Z', 'a'...'z', '_' => self.state = .identifier,
                                ' ', '\t', '\r', '\n' => {
                                    while (true) {
                                        const in = self.peekInput();
                                        if (self.peekInput()) |cc| {
                                            if (!ascii.isSpace(cc)) break;
                                        } else {
                                            break;
                                        }
                                        _ = self.nextInput();
                                    }
                                    self.start = self.pos;
                                },
                                else => self.@"error"(c),
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
                                    self.pos -= 1;
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
                    self.pos += mem.len(tag_close_delim);
                    self.state = .text;
                    return self.makeToken(.tag_close);
                },

                .eof => {
                    return self.makeToken(.eof);
                },
            }
        }
        unreachable;
    }

    fn @"error"(self: *Self, c: u8) void {
        std.debug.print("lexer error on char: {c}\n", .{c});
    }
};

fn testLexer(source: []const u8, expected_tokens: []const Token.Kind) void {
    var lexer = Lexer.init(source);
    var it = lexer.iterator();
    for (expected_tokens) |expected, i| {
        if (it.next()) |token| {
            if (token.kind != expected) {
                std.debug.panic("want {}, got {}", .{ @tagName(expected), @tagName(token.kind) });
            }
        } else {
            std.debug.panic("expected a {} token at pos {d}", .{ @tagName(expected), i });
        }
    }
    const last = lexer.nextToken();
    std.testing.expect(last.kind == .eof);
}

test "lexer - simple template" {
    testLexer("", &[_]Token.Kind{});
    testLexer("<title>", &[_]Token.Kind{.text});
    testLexer("{%", &[_]Token.Kind{.tag_open});
    testLexer("<title>\n{%", &[_]Token.Kind{ .text, .tag_open });
    testLexer("<title>\n{% block", &[_]Token.Kind{ .text, .tag_open, .keyword_block });
    testLexer("<title>\n{% block title", &[_]Token.Kind{ .text, .tag_open, .keyword_block, .identifier });
    testLexer("<title>\n{% block title %}", &[_]Token.Kind{ .text, .tag_open, .keyword_block, .identifier, .tag_close });
    testLexer("<title>\n{% block title %}\n{% endblock %}</title>", &[_]Token.Kind{ .text, .tag_open, .keyword_block, .identifier, .tag_close, .text, .tag_open, .keyword_endblock, .tag_close, .text });
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

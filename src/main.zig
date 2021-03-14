const std = @import("std");
const ascii = std.ascii;
const testing = std.testing;

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

const Lexer = struct {
    source: []const u8,
    start: usize,
    pos: usize,
    state: State,
    token: ?Token,

    const State = enum {
        start,
        text,
        start_delim,
        inside_tag,
        inside_variable,
        tag_name,
        after_tag_name,
        start_tag_close,
        variable_name,
    };

    const Self = @This();

    fn init(source: []const u8) Self {
        return Self{
            .source = source,
            .start = @as(usize, 0),
            .pos = @as(usize, 0),
            .state = .start,
            .token = null,
        };
    }

    fn dumpToken(self: *Self, token: Token) void {
        std.debug.print("{}: \"{}\"\n", .{ @tagName(token.kind), token.value });
    }

    pub fn next(self: *Self) Token {
        var token = Token{
            .value = "",
            .kind = .eof,
        };
        while (self.pos < self.source.len) : (self.pos += 1) {
            const c = self.source[self.pos];
            //std.debug.print("c:{c}\tstate:{}\n", .{ c, self.state });
            switch (self.state) {
                .start => switch (c) {
                    '{' => {
                        self.state = .start_delim;
                    },
                    else => {
                        token.kind = .text;
                        self.state = .text;
                    },
                },
                .text => switch (c) {
                    '{' => {
                        self.state = .start;
                        break;
                    },
                    else => {},
                },
                .start_delim => switch (c) {
                    '%' => {
                        self.pos += 1;
                        token.kind = .tag_open;
                        self.state = .inside_tag;
                        break;
                    },
                    '{' => {
                        self.pos += 1;
                        token.kind = .variable_open;
                        self.state = .inside_variable;
                        break;
                    },
                    else => {
                        self.state = .text;
                    },
                },
                .inside_tag => switch (c) {
                    ' ', '\t', '\r', '\n' => {
                        self.start += 1;
                    },
                    'a'...'z', 'A'...'Z', '_' => {
                        token.kind = .identifier;
                        self.state = .tag_name;
                    },
                    else => {},
                },
                .tag_name => switch (c) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                    else => {
                        if (keywords.get(self.source[self.start..self.pos])) |kind| {
                            token.kind = kind;
                        }
                        self.state = .after_tag_name;
                        break;
                    },
                },
                .after_tag_name => switch (c) {
                    'a'...'z', 'A'...'Z', '_' => {
                        token.kind = .identifier;
                    },
                    '%' => {
                        self.state = .start_tag_close;
                    },
                    else => {},
                },
                .start_tag_close => switch (c) {
                    '}' => {
                        token.kind = .tag_close;
                    },
                    else => {},
                },
                .inside_variable => switch (c) {
                    ' ', '\t', '\r', '\n' => {
                        self.start += 1;
                    },
                    'a'...'z', 'A'...'Z', '_' => {
                        token.kind = .identifier;
                        self.state = .variable_name;
                    },
                    '%' => {
                        self.state = .start_tag_close;
                    },
                    else => {},
                },
                .variable_name => switch (c) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                    else => {},
                },
            }
        } else {
            switch (self.state) {
                .tag_name => {
                    if (keywords.get(self.source[self.start..self.pos])) |kind| {
                        token.kind = kind;
                    }
                    self.state = .after_tag_name;
                },
                else => {},
            }
        }
        token.value = self.source[self.start..self.pos];
        self.start = self.pos;
        return token;
    }
};

fn testLexer(source: []const u8, expected_tokens: []const Token.Kind) void {
    var lexer = Lexer.init(source);
    for (expected_tokens) |expected| {
        const token = lexer.next();
        lexer.dumpToken(token);
        if (token.kind != expected) {
            std.debug.panic("want {}, got {}", .{ @tagName(expected), @tagName(token.kind) });
        }
    }
    const last = lexer.next();
    std.testing.expect(last.kind == .eof);
}

test "lexer - simple template" {
    testLexer("", &[_]Token.Kind{});
    testLexer("<ul>", &[_]Token.Kind{.text});
    testLexer("{%", &[_]Token.Kind{.tag_open});
    testLexer("<ul>\n{%", &[_]Token.Kind{ .text, .tag_open });
    testLexer("<ul>\n{% block", &[_]Token.Kind{ .text, .tag_open, .keyword_block });
    testLexer("<ul>\n{% block title", &[_]Token.Kind{ .text, .tag_open, .keyword_block, .identifier });
    //testLexer("<ul>\n{% block title %}", &[_]Token.Kind{ .text, .tag_open, .keyword_block, .identifier, .tag_close });
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

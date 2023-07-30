const std = @import("std");

pub const TokenType = enum {
    // single character tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_CURLY,
    RIGHT_CURLY,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMI,
    SLASH,
    STAR,
    // one or two character tokens
    NOT,
    NOT_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GT,
    GEQ,
    LT,
    LEQ,
    // literals
    IDENTIFIER,
    STRING,
    NUMBER,
    // keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    ERROR,
    EOF,
};

// probably not as efficent as the trie in the book
// but otoh it is much simpler
const identifier_type = std.ComptimeStringMap(TokenType, .{
    .{ "and", .AND },
    .{ "class", .CLASS },
    .{ "else", .ELSE },
    .{ "false", .FALSE },
    .{ "for", .FOR },
    .{ "fun", .FUN },
    .{ "if", .IF },
    .{ "nil", .NIL },
    .{ "or", .OR },
    .{ "print", .PRINT },
    .{ "return", .RETURN },
    .{ "super", .SUPER },
    .{ "this", .THIS },
    .{ "true", .TRUE },
    .{ "var", .VAR },
    .{ "while", .WHILE },
});

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8, // bakes the start and length into one object
    line: u32,
};

pub const Scanner = struct {
    // the pointer arithmetic is a bit awkward in zig, this feels nicer
    // I suppose it's slightly slower, but I don't care too much in the scanner
    source: []const u8,
    start: usize,
    current: usize,
    line: u32, // not for indexing, so doesn't have to be usize

    pub fn init(source: []const u8) Scanner {
        return Scanner{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn scanToken(scanner: *Scanner) Token {
        scanner.skipWhitespace();
        scanner.start = scanner.current;

        if (scanner.isAtEnd()) {
            return scanner.makeToken(.EOF);
        }

        const c = scanner.advance();
        switch (c) {
            '(' => return scanner.makeToken(.LEFT_PAREN),
            ')' => return scanner.makeToken(.RIGHT_PAREN),
            '{' => return scanner.makeToken(.LEFT_CURLY),
            '}' => return scanner.makeToken(.RIGHT_CURLY),
            ';' => return scanner.makeToken(.SEMI),
            ',' => return scanner.makeToken(.COMMA),
            '.' => return scanner.makeToken(.DOT),
            '-' => return scanner.makeToken(.MINUS),
            '+' => return scanner.makeToken(.PLUS),
            '/' => return scanner.makeToken(.SLASH),
            '*' => return scanner.makeToken(.STAR),
            '!' => return scanner.makeToken(if (scanner.match('=')) .NOT_EQUAL else .NOT),
            '=' => return scanner.makeToken(if (scanner.match('=')) .EQUAL_EQUAL else .EQUAL),
            '<' => return scanner.makeToken(if (scanner.match('=')) .LEQ else .LT),
            '>' => return scanner.makeToken(if (scanner.match('=')) .GEQ else .GT),
            '"' => return scanner.string(),
            'a'...'z', 'A'...'Z', '_' => return scanner.identifier(),
            '0'...'9' => return scanner.number(),
            else => {},
        }

        return scanner.errorToken("Unexpected character");
    }

    inline fn isAtEnd(scanner: *Scanner) bool {
        return scanner.current == scanner.source.len;
    }

    inline fn peek(scanner: *Scanner) u8 {
        // not sure why clox doesn't check for being at the end here
        // however, doing so avoids a bunch of errors for being off the end
        return if (scanner.isAtEnd()) 0 else scanner.source[scanner.current];
    }

    inline fn peekNext(scanner: *Scanner) u8 {
        return if (scanner.isAtEnd()) 0 else scanner.source[scanner.current + 1];
    }

    fn advance(scanner: *Scanner) u8 {
        const c = scanner.source[scanner.current];
        scanner.current += 1;
        return c;
    }

    fn match(scanner: *Scanner, expected: u8) bool {
        if (scanner.isAtEnd()) return false;
        if (scanner.source[scanner.current] != expected) return false;
        scanner.current += 1;
        return true;
    }

    fn makeToken(scanner: *Scanner, t: TokenType) Token {
        return Token{
            .type = t,
            .lexeme = scanner.source[scanner.start..scanner.current],
            .line = scanner.line,
        };
    }

    fn errorToken(scanner: *Scanner, err: []const u8) Token {
        return Token{
            .type = .ERROR,
            .lexeme = err,
            .line = scanner.line,
        };
    }

    fn string(scanner: *Scanner) Token {
        while (!scanner.isAtEnd() and scanner.peek() != '"') {
            if (scanner.peek() == '\n') {
                scanner.line += 1;
            }
            _ = scanner.advance();
        }

        if (scanner.isAtEnd()) {
            return scanner.errorToken("Unterminated string");
        }

        _ = scanner.advance();
        return scanner.makeToken(.STRING);
    }

    inline fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn number(scanner: *Scanner) Token {
        while (isDigit(scanner.peek())) {
            _ = scanner.advance();
        }
        if (scanner.peek() == '.' and isDigit(scanner.peekNext())) {
            _ = scanner.advance();
        }
        while (isDigit(scanner.peek())) {
            _ = scanner.advance();
        }

        return scanner.makeToken(.NUMBER);
    }

    inline fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn identifier(scanner: *Scanner) Token {
        while (isAlpha(scanner.peek()) or isDigit(scanner.peek())) {
            _ = scanner.advance();
        }
        return scanner.makeToken(scanner.identifierType());
    }

    fn identifierType(scanner: *Scanner) TokenType {
        if (identifier_type.get(scanner.source[scanner.start..scanner.current])) |t| {
            return t;
        }

        return .IDENTIFIER;
    }

    fn skipWhitespace(scanner: *Scanner) void {
        while (true) {
            const c = scanner.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = scanner.advance(),
                '\n' => {
                    scanner.line += 1;
                    _ = scanner.advance();
                },
                '/' => {
                    // skip past comments
                    if (scanner.peekNext() == '/') {
                        while (!scanner.isAtEnd() and scanner.peek() != '\n') {
                            _ = scanner.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }
};

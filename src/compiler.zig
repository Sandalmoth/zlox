const std = @import("std");

const _chunk = @import("chunk.zig");
const OpCode = _chunk.OpCode;
const Chunk = _chunk.Chunk;

const _scanner = @import("scanner.zig");
const TokenType = _scanner.TokenType;
const Token = _scanner.Token;
const Scanner = _scanner.Scanner;

pub fn compile(alloc: std.mem.Allocator, source: []const u8, chunk: *Chunk) bool {
    _ = alloc;
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner, chunk);

    parser.advance();
    parser.expression();
    parser.consume(.EOF, "Expect end of expression");
    parser.endCompiler();

    return !parser.had_error;
}

const Parser = struct {
    scanner: *Scanner,
    compiling_chunk: *Chunk,
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,

    fn init(scanner: *Scanner, chunk: *Chunk) Parser {
        var parser = Parser{
            .scanner = scanner,
            .compiling_chunk = chunk,
            .current = undefined,
            .previous = undefined,
            .had_error = false,
            .panic_mode = false,
        };
        return parser;
    }

    fn advance(parser: *Parser) void {
        parser.previous = parser.current;

        while (true) {
            parser.current = parser.scanner.scanToken();
            if (parser.current.type != .ERROR) {
                break;
            }

            parser.errAtCurrent(parser.current.lexeme);
        }
    }

    fn consume(parser: *Parser, t: TokenType, message: []const u8) void {
        if (parser.current.type == t) {
            parser.advance();
            return;
        }

        parser.errAtCurrent(message);
    }

    fn currentChunk(parser: *Parser) *Chunk {
        // presumably this function will have different returns later
        return parser.compiling_chunk;
    }

    fn emitByte(parser: *Parser, byte: u8) void {
        parser.currentChunk().write(byte, parser.previous.line);
    }

    // fn emitBytes(parser: *Parser, byte1: u8, byte2: u8) void {
    //     parser.currentChunk().write(byte1, parser.previous.line);
    //     parser.currentChunk().write(byte2, parser.previous.line);
    // }

    fn emitOp(parser: *Parser, op: OpCode) void {
        parser.currentChunk().writeOp(op, parser.previous.line);
    }

    fn emitOpByte(parser: *Parser, op: OpCode, byte: u8) void {
        // I suspect this will be more useful than emitBytes
        parser.currentChunk().writeOp(op, parser.previous.line);
        parser.currentChunk().write(byte, parser.previous.line);
    }

    fn emitReturn(parser: *Parser) void {
        parser.emitOp(.RETURN);
    }

    fn endCompiler(parser: *Parser) void {
        parser.emitReturn();
    }

    fn expression(parser: *Parser) void {
        _ = parser;
    }

    fn errAtCurrent(parser: *Parser, message: []const u8) void {
        parser.errAt(&parser.current, message);
    }

    fn err(parser: *Parser, message: []const u8) void {
        parser.errAt(&parser.previous, message);
    }

    fn errAt(parser: *Parser, token: *Token, message: []const u8) void {
        if (parser.panic_mode) return;
        parser.panic_mode = true;

        std.debug.print("[line {}] Error", .{token.line});
        if (token.type == .EOF) {
            std.debug.print(" at end", .{});
        } else if (token.type == .ERROR) {
            //
        } else {
            std.debug.print(" at {s}", .{token.lexeme});
        }

        std.debug.print(": {s}\n", .{message});
        parser.had_error = true;
    }
};

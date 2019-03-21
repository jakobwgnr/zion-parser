import { Token } from '../../Lexer/Token';
import { TokenType } from '../../Lexer/tokentype';
import { Ast } from '../../Parser/Ast';
import { Parser } from '../../Parser/Parser';
import { Syntax } from '../../Parser/syntax';

import * as zionParser from '../../zion-parser';

describe('Helper Functionality', () => {
  test('Validations work correctly', () => {
    const tokens: Token[] = [];
    const parser: Parser = new Parser(tokens);

    expect(parser.isOptionalKeyword('MODE', new Token('MODE', TokenType.Keyword))).toBeTruthy();
    expect(parser.isOptionalKeyword('MODE', new Token('IS', TokenType.Keyword))).toBeFalsy();
    expect(parser.isOptionalKeyword('MODE', new Token('Value', TokenType.Identifier))).toBeFalsy();

    expect(parser.isOptionalTerminator(new Token('', TokenType.Terminator))).toBeTruthy();
    expect(parser.isOptionalTerminator(new Token('IS', TokenType.Keyword))).toBeFalsy();

    expect(parser.expectModeIdentifier(new Token('F', TokenType.Identifier))).toBeTruthy();
    expect(parser.expectModeIdentifier(new Token('V', TokenType.Identifier))).toBeTruthy();
    expect(parser.expectModeIdentifier(new Token('U', TokenType.Identifier))).toBeTruthy();
    expect(parser.expectModeIdentifier(new Token('S', TokenType.Identifier))).toBeTruthy();
    expect(parser.expectModeIdentifier(new Token('A', TokenType.Identifier))).toBeFalsy();

    expect(parser.expectIdentifier(new Token('Value', TokenType.Identifier))).toBeTruthy();
    expect(parser.expectIdentifier(new Token('Keyword', TokenType.Keyword))).toBeFalsy();

    expect(parser.expectKeyword('TRUE', new Token('TRUE', TokenType.Keyword))).toBeTruthy();
    expect(parser.expectKeyword('FALSE', new Token('TEST', TokenType.Keyword))).toBeFalsy();
  });

  test('Nodes always start with a keyword so Identifier should create an error', () => {
    const ast: Ast = zionParser.parse('IDENTIFIER', { fromPath: false });
    expect(ast.errors.length).toBeGreaterThan(0);
  });
});

describe('parseRecordingModeClause working correctly', () => {
  test('Full statement', () => {
    const ast: Ast = zionParser.parse('RECORDING MODE IS F', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.RecordingModeClause);
    expect(ast.errors.length).toBe(0);
  });
  test('NO MODE', () => {
    const ast: Ast = zionParser.parse('RECORDING IS F', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.RecordingModeClause);
    expect(ast.errors.length).toBe(0);
  });
  test('NO IS', () => {
    const ast: Ast = zionParser.parse('RECORDING MODE F', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.RecordingModeClause);
    expect(ast.errors.length).toBe(0);
  });
  test('BOTH OPTIONAL MISSING', () => {
    const ast: Ast = zionParser.parse('RECORDING F', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.RecordingModeClause);
    expect(ast.errors.length).toBe(0);
  });
});

describe('parseProgramId working correctly', () => {
  test('Full statement', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME IS INITIAL PROGRAM.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
  });
  test('Minimum statement', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID PGMNAME', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
  });
  test('Variante 3', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
  });
  test('Keyword as name not allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. TRUE', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(1);
  });

  test('Initial alone allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME INITIAL', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
  });

  test('IS INITIAL  allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME IS INITIAL', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
  });

  test('INITIAL PROGRAM allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME INITIAL PROGRAM', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
  });

  test('IS alone not allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME IS', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(1);
  });

  test('Optional Terminator', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
  });

  test('Different order not allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME PROGRAM IS INITIAL.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(2);
  });
});

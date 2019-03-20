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

    expect(parser.expectModeIdentifier(new Token('F', TokenType.Identifier))).toBeTruthy();
    expect(parser.expectModeIdentifier(new Token('V', TokenType.Identifier))).toBeTruthy();
    expect(parser.expectModeIdentifier(new Token('U', TokenType.Identifier))).toBeTruthy();
    expect(parser.expectModeIdentifier(new Token('S', TokenType.Identifier))).toBeTruthy();
    expect(parser.expectModeIdentifier(new Token('A', TokenType.Identifier))).toBeFalsy();

    expect(parser.expectKeyword('TRUE', new Token('TRUE', TokenType.Keyword))).toBeTruthy();
    expect(parser.expectKeyword('FALSE', new Token('TEST', TokenType.Keyword))).toBeFalsy();
  });

  test('Nodes always start with a keyword so Identifier should create an error', () => {
    const tokens: Token[] = [];
    tokens.push(new Token('Value', TokenType.Identifier));
    const parser: Parser = new Parser(tokens);
    parser.execute();
    expect(parser.errorHandler.errors.length).toBeGreaterThan(0);
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

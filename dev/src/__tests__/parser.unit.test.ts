import { Token } from 'zion-lexer';
import { TokenType } from 'zion-lexer';
import { Ast } from '../Ast';
import { Parser } from '../Parser';

import * as zionParser from '../zion-parser';

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

    expect(parser.expectTerminator(new Token('', TokenType.Terminator))).toBeTruthy();
    expect(parser.expectTerminator(new Token('Keyword', TokenType.Keyword))).toBeFalsy();

    expect(parser.expectKeyword('TRUE', new Token('TRUE', TokenType.Keyword))).toBeTruthy();
    expect(parser.expectKeyword('FALSE', new Token('TEST', TokenType.Keyword))).toBeFalsy();
  });

  test('Nodes always start with a keyword so Identifier should create an error', () => {
    const ast: Ast = zionParser.parse('IDENTIFIER', { fromPath: false });
    expect(ast.errors.length).toBeGreaterThan(0);
  });
});

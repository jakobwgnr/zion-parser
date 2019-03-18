import { Lexer } from '../../Lexer/Lexer';
import { Token } from '../../Lexer/Token';
import { TokenType } from '../../Lexer/tokentype';

test('Whitespace Token correctly identified', () => {
    const lexer = new Lexer('        ');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.WhiteSpace);
});

test('SequenceNumber Token correctly identified', () => {
    const lexer = new Lexer('123456       ');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.SequenceNumberLiteral);
});

test('* Comment Token correctly identified', () => {
    const lexer = new Lexer('123456*Comment      ');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[1].type).toBe(TokenType.Comment);
});

test('/ Comment Token correctly identified', () => {
    const lexer = new Lexer('123456/Comment      ');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[1].type).toBe(TokenType.Comment);
});

test('Operator Token correctly identified', () => {
    const lexer = new Lexer('+');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.Operator);
});

test('Operator Token with 2 digits correctly identified', () => {
    const lexer = new Lexer('++');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.Operator);
});

test('Numeric Token with sign digits correctly identified', () => {
    const lexer = new Lexer('+1');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.NumericLiteral);
});

test('Numeric Token with sign digits correctly identified', () => {
    const lexer = new Lexer('-1');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.NumericLiteral);
});

test('Numeric Token without sign digits correctly identified', () => {
    const lexer = new Lexer('         123');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[1].type).toBe(TokenType.NumericLiteral);
});

test('Keyword Token correctly identified', () => {
    const lexer = new Lexer('IF');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.Keyword);
});

test('EXEC Token correctly identified', () => {
    const lexer = new Lexer('EXEC');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.EXEC);
});

test('IdentificationArea Token correctly identified', () => {
    const lexer = new Lexer('                                                                        IDENT');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[1].type).toBe(TokenType.IdentificationArea);
});

test('Identifier Token correctly identified', () => {
    const lexer = new Lexer('IDENTIFIER');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.Identifier);
});

test('String Token with single Quote correctly identified', () => {
    const lexer = new Lexer('\'THIS IS A STRING\'');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.StringLiteral);
});

test('String Token with double Quote correctly identified', () => {
    const lexer = new Lexer('\"THIS IS A STRING\"');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.StringLiteral);
});

test('Unidentified Token correctly identified', () => {
    const lexer = new Lexer('!!!&/&/)$');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe(TokenType.NotIdentified);
});
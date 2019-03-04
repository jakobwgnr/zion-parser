import { Lexer } from '../Lexer';
import { Token } from '../Token';

test('Lexer identifies 1 digit level', () => {
    const lexer = new Lexer('        1 LEVEL-INDICATOR');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe("Level");
    expect(tokenList[0].value).toBe('1');

    expect(tokenList[1].type).toBe("Identifier");
    expect(tokenList[1].value).toBe('LEVEL-INDICATOR');
});

test('Lexer identifies 2 digit level', () => {
    const lexer = new Lexer('        01 LEVEL-INDICATOR');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe("Level");
    expect(tokenList[0].value).toBe('01');

    expect(tokenList[1].type).toBe("Identifier");
    expect(tokenList[1].value).toBe('LEVEL-INDICATOR');
});

test('Lexer identifies  66 level', () => {
    const lexer = new Lexer('        66 LEVEL-INDICATOR');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe("Level");
    expect(tokenList[0].value).toBe('66');

    expect(tokenList[1].type).toBe("Identifier");
    expect(tokenList[1].value).toBe('LEVEL-INDICATOR');
});

test('Lexer identifies  77 level', () => {
    const lexer = new Lexer('        77 LEVEL-INDICATOR');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe("Level");
    expect(tokenList[0].value).toBe('77');

    expect(tokenList[1].type).toBe("Identifier");
    expect(tokenList[1].value).toBe('LEVEL-INDICATOR');
});

test('Lexer identifies  88 level', () => {
    const lexer = new Lexer('        88 LEVEL-INDICATOR');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe("Level");
    expect(tokenList[0].value).toBe('88');

    expect(tokenList[1].type).toBe("Identifier");
    expect(tokenList[1].value).toBe('LEVEL-INDICATOR');
});
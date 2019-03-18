import { Lexer } from '../../Lexer/Lexer';
import { Token } from '../../Lexer/Token';

test('Check if Lexer EOF token provides lines correctly', () => {
    const lexer = new Lexer('      * Line1\r\n      * Line2');

    const tokenList: Token[] = lexer.execute();
    const eofToken: Token = tokenList.find(token => token.type === "EOF") as Token;
    expect(eofToken.endLine).toBe(2);
});

test('Check if Lexer EOF token provides columns correctly', () => {
    const lexer = new Lexer('123456789\r\n123456789');

    const tokenList: Token[] = lexer.execute();
    const eofToken: Token = tokenList.find(token => token.type === "EOF") as Token;
    expect(eofToken.endColumnTotal).toBe(19);
    expect(eofToken.endLine).toBe(2);
});

test('Check if Lexer identifies IdentificationArea correctly', () => {
    const lexer = new Lexer('                                                                        Identify');

    const tokenList: Token[] = lexer.execute();
    const IdentificationAreaToken: Token = tokenList.find(token => token.type === "IdentificationArea") as Token;
    expect(IdentificationAreaToken.value).toBe("Identify");
});
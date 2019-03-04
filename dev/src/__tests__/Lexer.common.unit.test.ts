import { Lexer } from '../Lexer';
import { Token } from '../Token';

test('Check if Lexer EOF token provides lines correctly', () => {
    const lexer = new Lexer('      * Line1\r\n      * Line2');

    const tokenList: Token[] = lexer.execute();
    const eofToken: Token = tokenList.find(token => token.type === "EOF") as Token;
    expect(eofToken.endLine).toBe(2);
});

test('Check if Lexer EOF token provides columns correctly', () => {
    const lexer = new Lexer('TEST1234\r\nTEST1234');

    const tokenList: Token[] = lexer.execute();
    const eofToken: Token = tokenList.find(token => token.type === "EOF") as Token;
    expect(eofToken.endColumnTotal).toBe(18);
});
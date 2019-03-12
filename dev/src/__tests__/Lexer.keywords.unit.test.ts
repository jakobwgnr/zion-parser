import { Lexer } from '../Lexer/Lexer';
import { Token } from '../Lexer/Token';

test('Check if Lexer identifies the IDENTIFICATION DIVISION Keywords', () => {
    const lexer = new Lexer('        IDENTIFICATION DIVISION');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[1].type).toBe('Keyword');
    expect(tokenList[1].value).toBe('IDENTIFICATION');

    expect(tokenList[3].type).toBe('Keyword');
    expect(tokenList[3].value).toBe('DIVISION');
});

test('Check if Lexer can destinguish between keyword and value', () => {
    const lexer = new Lexer('       PROGRAM-ID.    PRGNAME.');

    const tokenList: Token[] = lexer.execute();
    const keywordToken: Token = tokenList.find(token => token.type === "Keyword") as Token;
    expect(keywordToken.type).toBe("Keyword");
    expect(keywordToken.value).toBe('PROGRAM-ID');

    const identifierToken: Token = tokenList.find(token => token.type === "Identifier") as Token;
    expect(identifierToken.type).toBe("Identifier");
    expect(identifierToken.value).toBe('PRGNAME');
});

test('Check if Lexer identifies EXEC', () => {
    const lexer = new Lexer('           EXEC CICS\n               LINK PROGRAM(TESTPROGRAM)\n           END-EXEC');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[1].type).toBe("EXEC");
    expect(tokenList[1].value).toBe('EXEC CICS\n               LINK PROGRAM(TESTPROGRAM)\n           END-EXEC');
    expect(tokenList[1].startLine).toBe(1);
    expect(tokenList[1].endLine).toBe(3);
});

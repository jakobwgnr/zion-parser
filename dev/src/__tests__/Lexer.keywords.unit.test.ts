import { Lexer } from '../Lexer';
import { Token } from '../Token';

test('Check if Lexer identifies the IDENTIFICATION DIVISION Keywords', () => {
    const lexer = new Lexer('        IDENTIFICATION DIVISION');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe('Keyword');
    expect(tokenList[0].value).toBe('IDENTIFICATION');

    expect(tokenList[1].type).toBe('Keyword');
    expect(tokenList[1].value).toBe('DIVISION');
});

test('Check if Lexer can destinguish between keyword and value', () => {
    const lexer = new Lexer('       PROGRAM-ID.    QC1CDPL.');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe("Keyword");
    expect(tokenList[0].value).toBe('PROGRAM-ID');

    expect(tokenList[1].type).toBe("Identifier");
    expect(tokenList[1].value).toBe('QC1CDPL');
});

test('Check if Lexer identifies EXEC', () => {
    const lexer = new Lexer('           EXEC CICS\n               LINK PROGRAM(TESTPROGRAM)\n           END-EXEC');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[0].type).toBe("EXEC");
    expect(tokenList[0].value).toBe('EXEC CICS\n               LINK PROGRAM(TESTPROGRAM)\n           END-EXEC');
    expect(tokenList[0].startLine).toBe(1);
    expect(tokenList[0].endLine).toBe(3);
});

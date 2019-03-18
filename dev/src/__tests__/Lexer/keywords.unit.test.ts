import { Keyword } from '../../Lexer/keyword';
import { Lexer } from '../../Lexer/Lexer';
import { Token } from '../../Lexer/Token';

// Unit-Test
test('Defaultvalue to be false', () => {
    expect(Keyword.isKeyword("A")).toBeFalsy();
    expect(Keyword.isKeyword("THISWORDISWAYTOLONGTOBEAKEYWORD")).toBeFalsy();
});

test('Keywords with length of 2 identified', () => {
    expect(Keyword.isKeyword("CD")).toBeTruthy();
    expect(Keyword.isKeyword("TO")).toBeTruthy();
});

test('Keywords with length of 3 identified', () => {
    expect(Keyword.isKeyword("ADD")).toBeTruthy();
    expect(Keyword.isKeyword("USE")).toBeTruthy();
});

test('Keywords with length of 4 identified', () => {
    expect(Keyword.isKeyword("CALL")).toBeTruthy();
    expect(Keyword.isKeyword("TYPE")).toBeTruthy();
});

test('Keywords with length of 5 identified', () => {
    expect(Keyword.isKeyword("AFTER")).toBeTruthy();
    expect(Keyword.isKeyword("ZEROS")).toBeTruthy();
});

test('Keywords with length of 6 identified', () => {
    expect(Keyword.isKeyword("ACCEPT")).toBeTruthy();
    expect(Keyword.isKeyword("STRING")).toBeTruthy();
});

test('Keywords with length of 7 identified', () => {
    expect(Keyword.isKeyword("ADDRESS")).toBeTruthy();
    expect(Keyword.isKeyword("VARYING")).toBeTruthy();
});

test('Keywords with length of 8 identified', () => {
    expect(Keyword.isKeyword("ALPHABET")).toBeTruthy();
    expect(Keyword.isKeyword("TRAILING")).toBeTruthy();
});

test('Keywords with length of 9 identified', () => {
    expect(Keyword.isKeyword("ADVANCING")).toBeTruthy();
    expect(Keyword.isKeyword("RETURNING")).toBeTruthy();
});

test('Keywords with length of 10 identified', () => {
    expect(Keyword.isKeyword("ALPHABETIC")).toBeTruthy();
    expect(Keyword.isKeyword("WRITE-ONLY")).toBeTruthy();
});

test('Keywords with length of 11 identified', () => {
    expect(Keyword.isKeyword("CLOCK-UNITS")).toBeTruthy();
    expect(Keyword.isKeyword("SUB-QUEUE-3")).toBeTruthy();
});

test('Keywords with length of 12 identified', () => {
    expect(Keyword.isKeyword("DATE-WRITTEN")).toBeTruthy();
    expect(Keyword.isKeyword("SYNCHRONIZED")).toBeTruthy();
});

test('Keywords with length of 13 identified', () => {
    expect(Keyword.isKeyword("DATE-COMPILED")).toBeTruthy();
    expect(Keyword.isKeyword("LOCAL-STORAGE")).toBeTruthy();
});

test('Keywords with length of 14 identified', () => {
    expect(Keyword.isKeyword("DEBUG-CONTENTS")).toBeTruthy();
    expect(Keyword.isKeyword("NUMERIC-EDITED")).toBeTruthy();
});

test('Keywords with length of 15 identified', () => {
    expect(Keyword.isKeyword("OBJECT-COMPUTER")).toBeTruthy();
    expect(Keyword.isKeyword("COMPUTATIONAL-5")).toBeTruthy();
});

test('Keywords with length of 16 identified', () => {
    expect(Keyword.isKeyword("ALPHABETIC-LOWER")).toBeTruthy();
    expect(Keyword.isKeyword("ALPHABETIC-UPPER")).toBeTruthy();
});

test('Keywords with length of 17 identified', () => {
    expect(Keyword.isKeyword("PROCEDURE-POINTER")).toBeTruthy();
});

test('Keywords with length of 19 identified', () => {
    expect(Keyword.isKeyword("ALPHANUMERIC-EDITED")).toBeTruthy();
});

test('Keyword to identify EXEC', () => {
    expect(Keyword.isExec("EXEC")).toBeTruthy();
    expect(Keyword.isExec("NOTEXEC")).toBeFalsy();
});

test('Keyword to identify END-EXEC', () => {
    expect(Keyword.containsEndExec("Test END-EXEC")).toBeTruthy();
    expect(Keyword.containsEndExec("END-EXEC")).toBeTruthy();
    expect(Keyword.containsEndExec("END-EXEC Test")).toBeTruthy();
    expect(Keyword.containsEndExec("EXEC NOT END EXEC")).toBeFalsy();
});

// Integration
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



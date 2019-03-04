import { Options } from '../Options';
import { Token } from '../Token';
import * as zionParser from '../zion-parser';

test('Zion-Parser.lex throws error on no such file', () => {
    expect(() => {
        zionParser.lex('/path/to/nowhere/test.txt');
    }).toThrow();
});

test('Zion-Parser.lex takes a String stdin as input', () => {
    const tokenList: Token[] = zionParser.lex('      * This is a comment', new Options(false))
    expect(tokenList[0].type).toBe('Comment');
})

test('Zion-Parser.lex takes a path as input', () => {
    const tokenList: Token[] = zionParser.lex('./__tests__/testfiles/comment.cbl')
    expect(tokenList[0].type).toBe('Comment');
})

test('Zion-Parser.lex able to lex a complete COBOL program', () => {
    const tokenList: Token[] = zionParser.lex('./__tests__/testfiles/QC1CDPL.cbl')
    expect(tokenList[0].type).toBe('Comment');

    const fs = require('fs');

    fs.writeFileSync("test.txt", tokenList.toString(), (err: any) => {
        if (err) {
            throw new Error("Error writing file");
        }
    });
})

// TODO: Tests for Parser


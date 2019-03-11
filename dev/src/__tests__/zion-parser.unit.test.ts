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
    const commentToken: Token = tokenList.find(token => token.type === "Comment") as Token;
    expect(commentToken.type).toBe("Comment");
});

test('Zion-Parser.lex takes a path as input', () => {
    const tokenList: Token[] = zionParser.lex('./__tests__/testfiles/comment.cbl')
    const commentToken: Token = tokenList.find(token => token.type === "Comment") as Token;
    expect(commentToken.type).toBe("Comment");
});

// TODO: Tests for Parser


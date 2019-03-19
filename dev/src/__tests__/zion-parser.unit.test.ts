import { Node } from '../Parser/nodes';

import { Token } from '../Lexer/Token';
import { Syntax } from '../Parser/syntax';
import * as zionParser from '../zion-parser';

// zionParser.lex
test('zionParser.lex throws error on no such file', () => {
  expect(() => {
    zionParser.lex('/path/to/nowhere/test.txt');
  }).toThrow();
});

test('zionParser.lex takes a String stdin as input', () => {
  const tokenList: Token[] = zionParser.lex('      * This is a comment', { fromPath: false });
  const commentToken: Token = tokenList.find(token => token.type === 'Comment') as Token;
  expect(commentToken.type).toBe('Comment');
});

test('zionParser.lex takes a path as input', () => {
  const tokenList: Token[] = zionParser.lex('./__tests__/testfiles/comment.cbl');
  const commentToken: Token = tokenList.find(token => token.type === 'Comment') as Token;
  expect(commentToken.type).toBe('Comment');
});

// zionParser.parse
test('zionParser.parse throws error on no such file', () => {
  expect(() => {
    zionParser.parse('/path/to/nowhere/test.txt');
  }).toThrow();
});

test('zionParser.parse takes a String stdin as input', () => {
  const nodeList: Node[] = zionParser.parse('RECORDING MODE IS F', { fromPath: false });
  expect(nodeList[0].type).toBe(Syntax.RecordingModeClause);
});

test('zionParser.parse takes a path as input', () => {
  const nodeList: Node[] = zionParser.parse('./__tests__/testfiles/recording.cbl');
  expect(nodeList[0].type).toBe(Syntax.RecordingModeClause);
});

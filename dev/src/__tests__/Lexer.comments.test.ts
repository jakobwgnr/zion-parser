import { Lexer } from '../Lexer';
import { Token } from '../Token';

test('Check if Lexer identifies a comment', () => {
  const lexer = new Lexer('      * This is a comment');

  const tokenList: Token[] = lexer.execute();
  expect(tokenList[0].type).toBe('Comment');
  expect(tokenList[0].value).toBe('* This is a comment');
  expect(tokenList[0].startLine).toBe(1);
  expect(tokenList[0].endLine).toBe(1);
  expect(tokenList[0].startColumnRelative).toBe(7);
  expect(tokenList[0].startColumnTotal).toBe(7);
  expect(tokenList[0].endColumnRelative).toBe(26);
  expect(tokenList[0].endColumnTotal).toBe(26);
});

test('Check if Lexer identifies 2 comments', () => {
  const lexer = new Lexer('      * This is the first comment\r\n      * This is the second comment');

  const tokenList: Token[] = lexer.execute();
  expect(tokenList[0].type).toBe('Comment');
  expect(tokenList[0].value).toBe('* This is the first comment');
  expect(tokenList[0].startLine).toBe(1);
  expect(tokenList[0].endLine).toBe(1);
  expect(tokenList[0].startColumnRelative).toBe(7);
  expect(tokenList[0].startColumnTotal).toBe(7);
  expect(tokenList[0].endColumnRelative).toBe(34);
  expect(tokenList[0].endColumnTotal).toBe(34);


  // expect(tokenList[1].type).toBe('Comment');
  // expect(tokenList[1].value).toBe(' This is the second comment');
  // expect(tokenList[1].startLine).toBe(2);
  // expect(tokenList[0].endLine).toBe(2);
  // expect(tokenList[0].startColumnRelative).toBe(7);
  // expect(tokenList[0].startColumnTotal).toBe(7);
  // expect(tokenList[0].endColumnRelative).toBe(40);
  // expect(tokenList[0].endColumnTotal).toBe(67);
});


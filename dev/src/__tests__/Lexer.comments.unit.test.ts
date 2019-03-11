import { Lexer } from '../Lexer';
import { Token } from '../Token';

test('Check if Lexer identifies a comment', () => {
  const lexer = new Lexer('      * This is a comment');

  const tokenList: Token[] = lexer.execute();
  expect(tokenList[1].type).toBe("Comment");
  expect(tokenList[1].value).toBe('* This is a comment');
});

test('Check if Lexer identifies 2 comments', () => {
  const lexer = new Lexer('      * This is the first comment\r\n      * This is the second comment');

  const tokenList: Token[] = lexer.execute();
  expect(tokenList[1].type).toBe("Comment");
  expect(tokenList[1].value).toBe('* This is the first comment');

  expect(tokenList[3].type).toBe("Comment");
  expect(tokenList[3].value).toBe('* This is the second comment');
});

test('Check if Lexer identifies / as comment indicator', () => {
  const lexer = new Lexer('      / This is the first comment');

  const tokenList: Token[] = lexer.execute();
  expect(tokenList[1].type).toBe("Comment");
  expect(tokenList[1].value).toBe('/ This is the first comment');
});


import { Lexer } from '../../Lexer/Lexer';
import { Token } from '../../Lexer/Token';

test('Lexer identifies 1 digit level', () => {
  const lexer = new Lexer('        1 LEVEL-INDICATOR');

  const tokenList: Token[] = lexer.execute();
  const levelToken: Token = tokenList.find(token => token.type === 'Level') as Token;
  expect(levelToken.type).toBe('Level');
  expect(levelToken.value).toBe('1');

  const identifierToken: Token = tokenList.find(token => token.type === 'Identifier') as Token;
  expect(identifierToken.type).toBe('Identifier');
  expect(identifierToken.value).toBe('LEVEL-INDICATOR');
});

test('Lexer identifies 2 digit level', () => {
  const lexer = new Lexer('        01 LEVEL-INDICATOR');

  const tokenList: Token[] = lexer.execute();
  const levelToken: Token = tokenList.find(token => token.type === 'Level') as Token;
  expect(levelToken.type).toBe('Level');
  expect(levelToken.value).toBe('01');

  const identifierToken: Token = tokenList.find(token => token.type === 'Identifier') as Token;
  expect(identifierToken.type).toBe('Identifier');
  expect(identifierToken.value).toBe('LEVEL-INDICATOR');
});

test('Lexer identifies  66 level', () => {
  const lexer = new Lexer('        66 LEVEL-INDICATOR');

  const tokenList: Token[] = lexer.execute();
  const levelToken: Token = tokenList.find(token => token.type === 'Level') as Token;
  expect(levelToken.type).toBe('Level');
  expect(levelToken.value).toBe('66');

  const identifierToken: Token = tokenList.find(token => token.type === 'Identifier') as Token;
  expect(identifierToken.type).toBe('Identifier');
  expect(identifierToken.value).toBe('LEVEL-INDICATOR');
});

test('Lexer identifies  77 level', () => {
  const lexer = new Lexer('        77 LEVEL-INDICATOR');

  const tokenList: Token[] = lexer.execute();
  const levelToken: Token = tokenList.find(token => token.type === 'Level') as Token;
  expect(levelToken.type).toBe('Level');
  expect(levelToken.value).toBe('77');

  const identifierToken: Token = tokenList.find(token => token.type === 'Identifier') as Token;
  expect(identifierToken.type).toBe('Identifier');
  expect(identifierToken.value).toBe('LEVEL-INDICATOR');
});

test('Lexer identifies  88 level', () => {
  const lexer = new Lexer('        88 LEVEL-INDICATOR');

  const tokenList: Token[] = lexer.execute();
  const levelToken: Token = tokenList.find(token => token.type === 'Level') as Token;
  expect(levelToken.type).toBe('Level');
  expect(levelToken.value).toBe('88');

  const identifierToken: Token = tokenList.find(token => token.type === 'Identifier') as Token;
  expect(identifierToken.type).toBe('Identifier');
  expect(identifierToken.value).toBe('LEVEL-INDICATOR');
});

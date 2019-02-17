import { Lexer } from '../Lexer';

test('Execute Lexer', () => {
  const lexer = new Lexer('Test');
  expect(lexer.execute()).toBe('Test');
});

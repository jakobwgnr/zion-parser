import { ParseError } from '../../../Parser/ErrorHandler/parse-error';

test('ParseError constructor works expected', () => {
  const error: ParseError = new ParseError('Message', 1, 2, 3, 'Description');
  expect(error.message).toBe('Message');
  expect(error.index).toBe(1);
  expect(error.lineNumber).toBe(2);
  expect(error.column).toBe(3);
  expect(error.description).toBe('Description');
});

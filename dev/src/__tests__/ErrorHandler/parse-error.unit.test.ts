import { ParseError } from '../../ErrorHandler/parse-error';

test('ParseError constructor works expected', () => {
  const error: ParseError = new ParseError(
    'Message',
    1,
    2,
    3,
    'Description',
    'receivedValue',
    'expectedValue',
    'receivedType',
    'expectedType',
  );
  expect(error.message).toBe('Message');
  expect(error.index).toBe(1);
  expect(error.lineNumber).toBe(2);
  expect(error.column).toBe(3);
  expect(error.description).toBe('Description');
  expect(error.receivedValue).toBe('receivedValue');
  expect(error.expectedValue).toBe('expectedValue');
  expect(error.receivedType).toBe('receivedType');
  expect(error.expectedType).toBe('expectedType');
});

test('ParseError constructor without optional works expected', () => {
  const error: ParseError = new ParseError('Message', 1, 2, 3, 'Description');
  expect(error.message).toBe('Message');
  expect(error.index).toBe(1);
  expect(error.lineNumber).toBe(2);
  expect(error.column).toBe(3);
  expect(error.description).toBe('Description');
  expect(error.receivedValue).toBe('');
  expect(error.expectedValue).toBe('');
  expect(error.receivedType).toBe('');
  expect(error.expectedType).toBe('');
});

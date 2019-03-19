import { ErrorHandler } from '../../../Parser/ErrorHandler/error-handler';

import { Token } from '../../../Lexer/Token';

test('ParseError constructor works expected', () => {
  const errorHandler: ErrorHandler = new ErrorHandler();
  errorHandler.unexpectedTokenError(new Token('TestValue', 'TestType', 1, 2, 3, 4, 5, 6));
  errorHandler.unexpectedTokenError(new Token('TestValue', 'TestType', 1, 2, 3, 4, 5, 6), 'Testmessage');
  expect(errorHandler.errors[0].index).toBe(1);
  expect(errorHandler.errors[0].column).toBe(2);
  expect(errorHandler.errors[0].lineNumber).toBe(3);
  expect(errorHandler.errors[0].description).toBe('Unexpected Token Type: TestType; Value: TestValue');
  expect(errorHandler.errors[0].message).toBe('Line 3: Unexpected Token Type: TestType; Value: TestValue');

  expect(errorHandler.errors[1].index).toBe(1);
  expect(errorHandler.errors[1].column).toBe(2);
  expect(errorHandler.errors[1].lineNumber).toBe(3);
  expect(errorHandler.errors[1].description).toBe('Testmessage');
  expect(errorHandler.errors[1].message).toBe('Line 3: Testmessage');
});

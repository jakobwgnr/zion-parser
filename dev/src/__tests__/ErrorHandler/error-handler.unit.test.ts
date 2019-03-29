import { ErrorHandler } from '../../ErrorHandler/error-handler';

import { Token } from 'zion-lexer';

test('ParseError constructor works expected', () => {
  const errorHandler: ErrorHandler = new ErrorHandler();
  errorHandler.unexpectedTokenError(new Token('TestValue', 'TestType', 1, 2, 3, 4, 5, 6), 'TestDescription');
  errorHandler.unexpectedTokenError(
    new Token('receivedTestValue', 'receivedTestType', 1, 2, 3, 4, 5, 6),
    'Testmessage',
    new Token('expectedTestValue', 'expectedTestType'),
  );
  expect(errorHandler.errors[0].index).toBe(1);
  expect(errorHandler.errors[0].column).toBe(2);
  expect(errorHandler.errors[0].lineNumber).toBe(3);
  expect(errorHandler.errors[0].description).toBe('TestDescription');
  expect(errorHandler.errors[0].message).toBe('Line 3:2 - Unexpected Token Type: TestType; Value: TestValue');

  expect(errorHandler.errors[1].expectedType).toBe('expectedTestType');
  expect(errorHandler.errors[1].expectedValue).toBe('expectedTestValue');
  expect(errorHandler.errors[1].receivedType).toBe('receivedTestType');
  expect(errorHandler.errors[1].receivedValue).toBe('receivedTestValue');
});

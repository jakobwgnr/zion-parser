import { ErrorHandler } from '../../ErrorHandler/error-handler';

import { Token } from 'zion-lexer';

test('ParseError constructor works expected', () => {
  const errorHandler: ErrorHandler = new ErrorHandler();
  errorHandler.unexpectedTokenError(new Token('TestValue', 'TestType', 1, 2, 3, 4, 5, 6));
  errorHandler.unexpectedTokenError(new Token('TestValue', 'TestType', 1, 2, 3, 4, 5, 6), 'Testmessage');
  errorHandler.unexpectedTokenError(
    new Token('receivedTestValue', 'receivedTestType', 1, 2, 3, 4, 5, 6),
    'Testmessage',
    new Token('expectedTestValue', 'expectedTestType'),
  );
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

  expect(errorHandler.errors[2].expectedType).toBe('expectedTestType');
  expect(errorHandler.errors[2].expectedValue).toBe('expectedTestValue');
  expect(errorHandler.errors[2].receivedType).toBe('receivedTestType');
  expect(errorHandler.errors[2].receivedValue).toBe('receivedTestValue');
});

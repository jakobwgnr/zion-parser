import { Character } from '../../Lexer/character';

test('isLineTerminator works correctly', () => {
  expect(Character.isLineTerminator('!')).toBeFalsy();
  expect(Character.isLineTerminator(String.fromCodePoint(0x0a))).toBeTruthy();
  expect(Character.isLineTerminator(String.fromCodePoint(0x0d))).toBeTruthy();
  expect(Character.isLineTerminator(String.fromCodePoint(0x2028))).toBeTruthy();
  expect(Character.isLineTerminator(String.fromCodePoint(0x2029))).toBeTruthy();
});

test('isLineTerminatorSequence works correctly', () => {
  expect(Character.isLineTerminatorSequence('!', '')).toBeFalsy();
  expect(Character.isLineTerminatorSequence(String.fromCodePoint(0x0d), ' ')).toBeFalsy();
  expect(Character.isLineTerminatorSequence(String.fromCodePoint(0x0d), String.fromCodePoint(0x0a))).toBeTruthy();
});

test('isCobolWordStart works correctly', () => {
  expect(Character.isCobolWordStart('!')).toBeFalsy();
  expect(Character.isCobolWordStart('a')).toBeTruthy();
  expect(Character.isCobolWordStart('A')).toBeTruthy();
  expect(Character.isCobolWordStart('z')).toBeTruthy();
  expect(Character.isCobolWordStart('Z')).toBeTruthy();
  expect(Character.isCobolWordStart('0')).toBeTruthy();
  expect(Character.isCobolWordStart('9')).toBeTruthy();
  expect(Character.isCobolWordStart('-')).toBeFalsy();
  expect(Character.isCobolWordStart('(')).toBeTruthy();
  expect(Character.isCobolWordStart(')')).toBeFalsy();
  expect(Character.isCobolWordStart(':')).toBeFalsy();
});

test('isCobolWordPart works correctly', () => {
  expect(Character.isCobolWordPart('!')).toBeFalsy();
  expect(Character.isCobolWordPart('a')).toBeTruthy();
  expect(Character.isCobolWordPart('A')).toBeTruthy();
  expect(Character.isCobolWordPart('z')).toBeTruthy();
  expect(Character.isCobolWordPart('Z')).toBeTruthy();
  expect(Character.isCobolWordPart('0')).toBeTruthy();
  expect(Character.isCobolWordPart('9')).toBeTruthy();
  expect(Character.isCobolWordPart('-')).toBeTruthy();
  expect(Character.isCobolWordPart('(')).toBeTruthy();
  expect(Character.isCobolWordPart(')')).toBeTruthy();
  expect(Character.isCobolWordPart(':')).toBeTruthy();
});

test('isNumberIndicator works correctly', () => {
  expect(Character.isNumberIndicator('A')).toBeFalsy();
  expect(Character.isNumberIndicator('-')).toBeTruthy();
  expect(Character.isNumberIndicator('+')).toBeTruthy();
});

test('isStringIndicator works correctly', () => {
  expect(Character.isStringIndicator('A')).toBeFalsy();
  expect(Character.isStringIndicator("'")).toBeTruthy();
  expect(Character.isStringIndicator('"')).toBeTruthy();
});

test('isOpeningBracket works correctly', () => {
  expect(Character.isOpeningBracket('A')).toBeFalsy();
  expect(Character.isOpeningBracket('(')).toBeTruthy();
});

test('IsClosingBracket works correctly', () => {
  expect(Character.isClosingBracket('A')).toBeFalsy();
  expect(Character.isClosingBracket(')')).toBeTruthy();
});

test('isCobolTerminator works correctly', () => {
  expect(Character.isCobolTerminator('A')).toBeFalsy();
  expect(Character.isCobolTerminator('.')).toBeTruthy();
});

test('isDecimalDigit works correctly', () => {
  expect(Character.isDecimalDigit('A')).toBeFalsy();
  expect(Character.isDecimalDigit('0')).toBeTruthy();
  expect(Character.isDecimalDigit('9')).toBeTruthy();
});

test('isNumeric works correctly', () => {
  expect(Character.isNumeric('A')).toBeFalsy();
  expect(Character.isNumeric('0')).toBeTruthy();
  expect(Character.isNumeric('-9')).toBeTruthy();
});

test('isCobolAritmeticOperator works correctly', () => {
  expect(Character.isCobolAritmeticOperator('A')).toBeFalsy();
  expect(Character.isCobolAritmeticOperator('*')).toBeTruthy();
  expect(Character.isCobolAritmeticOperator('+')).toBeTruthy();
  expect(Character.isCobolAritmeticOperator('/')).toBeTruthy();
  expect(Character.isCobolAritmeticOperator('=')).toBeTruthy();
  expect(Character.isCobolAritmeticOperator('>')).toBeTruthy();
  expect(Character.isCobolAritmeticOperator('<')).toBeTruthy();
  expect(Character.isCobolAritmeticOperator('-')).toBeTruthy();
});

test('isWhiteSpace works correctly', () => {
  expect(Character.isWhiteSpace('A')).toBeFalsy();
  expect(Character.isWhiteSpace(' ')).toBeTruthy();
  expect(Character.isWhiteSpace('    ')).toBeTruthy();
  expect(Character.isWhiteSpace(String.fromCodePoint(0x0b))).toBeTruthy();
  expect(Character.isWhiteSpace(String.fromCodePoint(0x0c))).toBeTruthy();
  expect(Character.isWhiteSpace(String.fromCodePoint(0xa0))).toBeTruthy();
  expect(Character.isWhiteSpace(String.fromCodePoint(0x1680))).toBeTruthy();
  expect(Character.isWhiteSpace(String.fromCodePoint(0x1680))).toBeTruthy();
  expect(Character.isWhiteSpace(String.fromCodePoint(0x3000))).toBeTruthy();
  expect(Character.isWhiteSpace(String.fromCodePoint(0xfeff))).toBeTruthy();
});

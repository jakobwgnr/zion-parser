import { SourceCode } from '../../Lexer/Sourcecode';

test('EOF works correct')
test('EOF works correct', () => {
    const sourceCode = new SourceCode('1');
    expect(sourceCode.eof()).toBeFalsy();
    sourceCode.NextChar();
    expect(sourceCode.eof()).toBeTruthy();
});

test('Check if SourceCode counts lines + columns correctly if CR', () => {
    const sourceCode = new SourceCode('123456789\r123456789');

    while (!sourceCode.eof()) {
        sourceCode.NextChar();
    }
    expect(sourceCode.currentLine).toBe(2);
    expect(sourceCode.columnsTotal).toBe(19);
});

test('Check if SourceCode counts + columns lines correctly if LF', () => {
    const sourceCode = new SourceCode('123456789\n123456789');

    while (!sourceCode.eof()) {
        sourceCode.NextChar();
    }
    expect(sourceCode.columnsTotal).toBe(19);
    expect(sourceCode.currentLine).toBe(2);
});

test('Check if SourceCode counts lines + columns correctly if CRLF', () => {
    const sourceCode = new SourceCode('123456789\r\n123456789');

    while (!sourceCode.eof()) {
        sourceCode.NextChar();
    }
    expect(sourceCode.columnsTotal).toBe(19);
    expect(sourceCode.currentLine).toBe(2);
});
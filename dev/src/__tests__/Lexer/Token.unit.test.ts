import { Token } from '../../Lexer/Token';

test('Token Constructor work expected', () => {
    const token = new Token("TestValue", "TestType", 1, 2, 3, 4, 5, 6);

    expect(token.value).toBe("TestValue");
    expect(token.type).toBe("TestType");
    expect(token.startColumnTotal).toBe(1);
    expect(token.startColumnRelative).toBe(2);
    expect(token.startLine).toBe(3);
    expect(token.endColumnTotal).toBe(4);
    expect(token.endColumnRelative).toBe(5);
    expect(token.endLine).toBe(6);
});

test('initToken work expected', () => {
    const token = new Token("TestValue", "TestType", 1, 2, 3, 4, 5, 6);

    token.initToken();
    expect(token.value).toBe("");
    expect(token.type).toBe("");
    expect(token.startColumnRelative).toBe(0);
    expect(token.startColumnTotal).toBe(0);
    expect(token.startLine).toBe(0);
    expect(token.endColumnRelative).toBe(0);
    expect(token.endColumnTotal).toBe(0);
    expect(token.endLine).toBe(0);
});
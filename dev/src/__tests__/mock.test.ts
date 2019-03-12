import { Token } from '../Lexer/Token';
import * as zionParser from '../zion-parser';

describe('mock', () => {
    test('Zion-Parser.lex able to lex a complete COBOL program', () => {
        const tokenList: Token[] = zionParser.lex('./__tests__/testfiles/QC1CDPL.cbl')
        expect(tokenList[0].type).toBe("SequenceNumberLiteral");

        const fs = require('fs');

        fs.writeFileSync("test.txt", tokenList.toString(), (err: any) => {
            if (err) {
                throw new Error("Error writing file");
            }
        });
    })
})
import { Token } from '../Lexer/Token';
import { Node } from '../Parser/nodes';
import * as zionParser from '../zion-parser';

describe('mock', () => {
    test('Zion-Parser.lex able to lex a complete COBOL program', () => {
        const tokenList: Token[] = zionParser.lex('./__tests__/testfiles/QC1CDPL.cbl')
        expect(tokenList[0].type).toBe("SequenceNumberLiteral");

        const fs = require('fs');

        fs.writeFileSync("mock-lexoutput.txt", tokenList.toString(), (err: any) => {
            if (err) {
                throw new Error("Error writing file");
            }
        });
    })

    test('Zion-Parser.parse able to parse a COBOL statement', () => {
        const nodeList: Node[] = zionParser.parse('RECORDING MODE IS F');

        const fs = require('fs');

        fs.writeFileSync("mock-parseoutput.txt", nodeList.toString(), (err: any) => {
            if (err) {
                throw new Error("Error writing file");
            }
        });
    });
})
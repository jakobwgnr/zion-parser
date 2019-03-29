import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

describe('parseSpecialNamesStatusPhrase working correctly', () => {
  test('Full statement', () => {
    const ast: Ast = zionParser.parse(
      '       SPECIAL-NAMES.\n' +
        '       ENVIR IS MNEMONIC\n' +
        '       ENVIR2 IS MNEMONIC2 ON STATUS IS CONDITION\n' +
        '       OFF STATUS IS CONDITION\n' +
        '       OFF STATUS IS CONDITION ON STATUS IS CONDITION\n' +
        '       ALPHABET ALPHABET-NAME IS EBCDIC\n' +
        '       ALPHABET ALPHABET-NAME2 IS STANDARD-1\n' +
        '       SYMBOLIC CHARACTERS A IS 100 \n' +
        '       B IS 200 \n' +
        '       C D ARE 300 400\n' +
        '       IN ASDF\n' +
        '       SYMBOLIC CHARACTERS Z IS 999 \n' +
        '       CLASS CLASS-NAME IS LITERAL-NAME\n' +
        '       CLASS CLASS-NAME2 IS LITERAL-NAME2\n' +
        '       CURRENCY SIGN IS EUR' +
        '       DECIMAL-POINT IS COMMA',
      {
        fromPath: false,
      },
    );

    expect(ast.nodes[0].type).toBe(Syntax.SpecialNamesParagraph);
    expect(ast.errors.length).toBe(0);

    expect((ast.nodes[0] as Nodes.SpecialNamesParagraph).specialNamesParagraphStatusPhrases[0].environmentValue).toBe(
      'ENVIR',
    );
    expect((ast.nodes[0] as Nodes.SpecialNamesParagraph).specialNamesParagraphStatusPhrases[0].mnemonicValue).toBe(
      'MNEMONIC',
    );

    expect((ast.nodes[0] as Nodes.SpecialNamesParagraph).specialNamesParagraphStatusPhrases[1].environmentValue).toBe(
      'ENVIR2',
    );
    expect((ast.nodes[0] as Nodes.SpecialNamesParagraph).specialNamesParagraphStatusPhrases[1].mnemonicValue).toBe(
      'MNEMONIC2',
    );
    expect(
      ((ast.nodes[0] as Nodes.SpecialNamesParagraph).specialNamesParagraphClauses[0] as Nodes.AlphabetClause)
        .alphabetName,
    ).toBe('ALPHABET-NAME');
    expect(
      ((ast.nodes[0] as Nodes.SpecialNamesParagraph).specialNamesParagraphClauses[1] as Nodes.AlphabetClause)
        .alphabetName,
    ).toBe('ALPHABET-NAME2');
    expect(
      ((ast.nodes[0] as Nodes.SpecialNamesParagraph).specialNamesParagraphClauses[2] as Nodes.SymbolicCharactersClause)
        .inOrdinalPosition,
    ).toBe('ASDF');
    expect(
      ((ast.nodes[0] as Nodes.SpecialNamesParagraph).specialNamesParagraphClauses[3] as Nodes.SymbolicCharactersClause)
        .symbolicCharacters[0],
    ).toBe('Z');
    expect(
      ((ast.nodes[0] as Nodes.SpecialNamesParagraph).specialNamesParagraphClauses[3] as Nodes.SymbolicCharactersClause)
        .symbolicIntegers[0],
    ).toBe('999');
    expect(
      ((ast.nodes[0] as Nodes.SpecialNamesParagraph).specialNamesParagraphClauses[4] as Nodes.ClassClause).className,
    ).toBe('CLASS-NAME');
    expect(
      ((ast.nodes[0] as Nodes.SpecialNamesParagraph).specialNamesParagraphClauses[5] as Nodes.ClassClause).className,
    ).toBe('CLASS-NAME2');
    if ((ast.nodes[0] as Nodes.SpecialNamesParagraph).currencySignClause !== null) {
      expect((ast.nodes[0] as Nodes.SpecialNamesParagraph).currencySignClause).not.toBeNull();
    }
    expect((ast.nodes[0] as Nodes.SpecialNamesParagraph).decimalPointClause).not.toBeNull();
  });
});

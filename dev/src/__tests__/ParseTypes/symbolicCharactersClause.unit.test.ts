import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse(
    'SYMBOLIC CHARACTERS A IS 100 \n' + '       B IS 200 \n' + 'C D ARE 300 400\n' + ' IN ASDF',
    {
      fromPath: false,
    },
  );
  expect(ast.nodes[0].type).toBe(Syntax.SymbolicCharactersClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.SymbolicCharactersClause).inOrdinalPosition).toBe('ASDF');
  expect((ast.nodes[0] as Nodes.SymbolicCharactersClause).symbolicCharacters[0]).toBe('A');
  expect((ast.nodes[0] as Nodes.SymbolicCharactersClause).symbolicCharacters[1]).toBe('B');
  expect((ast.nodes[0] as Nodes.SymbolicCharactersClause).symbolicCharacters[2]).toBe('C');
  expect((ast.nodes[0] as Nodes.SymbolicCharactersClause).symbolicCharacters[3]).toBe('D');
  expect((ast.nodes[0] as Nodes.SymbolicCharactersClause).symbolicIntegers[0]).toBe('100');
  expect((ast.nodes[0] as Nodes.SymbolicCharactersClause).symbolicIntegers[1]).toBe('200');
  expect((ast.nodes[0] as Nodes.SymbolicCharactersClause).symbolicIntegers[2]).toBe('300');
  expect((ast.nodes[0] as Nodes.SymbolicCharactersClause).symbolicIntegers[3]).toBe('400');
});

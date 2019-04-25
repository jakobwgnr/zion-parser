import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse('        FILE-CONTROL.\n' + '        SELECT FILE-NAME ASSIGN ASSIGNMENT', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.FileControlParagraph);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.FileControlParagraph).fileControlEntries[0].selectClause).not.toBeNull();
  expect((ast.nodes[0] as Nodes.FileControlParagraph).fileControlEntries[0].assignClause).not.toBeNull();
});

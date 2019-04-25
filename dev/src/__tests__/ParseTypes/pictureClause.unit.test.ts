import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('SyncronizedClause Data Description entry - should parse without errors', () => {
  const ast: Ast = zionParser.parse('         01 DATA-NAME PIC XXX VALUE "ABC".', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.DataDescriptionEntry);
  expect(ast.errors.length).toBe(0);
  expect(((ast.nodes[0] as Nodes.DataDescriptionEntry).level as Nodes.LevelNumber).value).toBe('01');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataName).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0].type).toBe(Syntax.PictureClause);
  expect((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[1].type).toBe(Syntax.DataValueClause);

  expect(
    ((ast.nodes[0] as Nodes.DataDescriptionEntry).dataDescriptonEntryClauses[0] as Nodes.PictureClause).pictureString
      .picChar[0],
  ).toBe('XXX');
});

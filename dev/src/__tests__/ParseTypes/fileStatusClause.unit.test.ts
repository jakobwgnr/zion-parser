import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse('FILE STATUS IS STATUS1 IN STATUS2 OF STATUS3\n STATUS4', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.FileStatusClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.FileStatusClause).value).toBe('STATUS1');
  expect((ast.nodes[0] as Nodes.FileStatusClause).dataNames[0]).toBe('STATUS2');
  expect((ast.nodes[0] as Nodes.FileStatusClause).dataNames[1]).toBe('STATUS3');
  expect((ast.nodes[0] as Nodes.FileStatusClause).optionalValue).toBe('STATUS4');
});

test('Full statement', () => {
  const ast: Ast = zionParser.parse('STATUS STATUS1', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.FileStatusClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.FileStatusClause).value).toBe('STATUS1');
});

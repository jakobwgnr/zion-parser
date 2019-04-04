import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse('SELECT OPTIONAL FILE-NAME', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.SelectClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.SelectClause).fileName).toBe('FILE-NAME');
});

test('Minimum statement', () => {
  const ast: Ast = zionParser.parse('SELECT FILE-NAME', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.SelectClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.SelectClause).fileName).toBe('FILE-NAME');
});

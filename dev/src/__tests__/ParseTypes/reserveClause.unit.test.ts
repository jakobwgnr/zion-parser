import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse('RESERVE 4711 AREAS', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.ReserveClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.ReserveClause).reserveAreaCount).toBe('4711');
});

test('minimum statement', () => {
  const ast: Ast = zionParser.parse('RESERVE 4711', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.ReserveClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.ReserveClause).reserveAreaCount).toBe('4711');
});

test('error statement', () => {
  const ast: Ast = zionParser.parse('RESERVE AREAS', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.ReserveClause);
  expect(ast.errors.length).toBeGreaterThan(0);
});

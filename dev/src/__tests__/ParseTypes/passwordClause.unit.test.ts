import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse('PASSWORD IS DATA-NAME', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.PasswordClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.PasswordClause).value).toBe('DATA-NAME');
});

test('minimum statement', () => {
  const ast: Ast = zionParser.parse('PASSWORD DATA-NAME', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.PasswordClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.PasswordClause).value).toBe('DATA-NAME');
});

test('error statement', () => {
  const ast: Ast = zionParser.parse('PASSWORD IS', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.PasswordClause);
  expect(ast.errors.length).toBe(1);
});

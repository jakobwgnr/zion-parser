import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse('CLASS CLASS-NAME IS LITERAL-NAME', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.ClassClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.ClassClause).className).toBe('CLASS-NAME');
  expect((ast.nodes[0] as Nodes.ClassClause).classLiterals[0].literal).toBe('LITERAL-NAME');
});

test('THROUGH statement', () => {
  const ast: Ast = zionParser.parse('CLASS CLASS-NAME IS LITERAL-NAME THROUGH THROUGH-LITERAL', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.ClassClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.ClassClause).className).toBe('CLASS-NAME');
  expect((ast.nodes[0] as Nodes.ClassClause).classLiterals[0].literal).toBe('LITERAL-NAME');
  expect((ast.nodes[0] as Nodes.ClassClause).classLiterals[0].throughLiteral).toBe('THROUGH-LITERAL');
});

test('THRU statement', () => {
  const ast: Ast = zionParser.parse('CLASS CLASS-NAME IS LITERAL-NAME THRU THROUGH-LITERAL', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.ClassClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.ClassClause).className).toBe('CLASS-NAME');
  expect((ast.nodes[0] as Nodes.ClassClause).classLiterals[0].literal).toBe('LITERAL-NAME');
  expect((ast.nodes[0] as Nodes.ClassClause).classLiterals[0].throughLiteral).toBe('THROUGH-LITERAL');
});

import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse('ASSIGN TO ASSIGNMENT1 ASSIGNMENT2', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.AssignClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.AssignClause).assignmentName[0]).toBe('ASSIGNMENT1');
  expect((ast.nodes[0] as Nodes.AssignClause).assignmentName[1]).toBe('ASSIGNMENT2');
});

test('minimum statement', () => {
  const ast: Ast = zionParser.parse('ASSIGN ASSIGNMENT1', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.AssignClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.AssignClause).assignmentName[0]).toBe('ASSIGNMENT1');
});

import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse('RECORD DELIMITER IS STANDARD-1', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.RecordDelimiterClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.RecordDelimiterClause).delimiterValue).toBe('STANDARD-1');
});

test('min statement', () => {
  const ast: Ast = zionParser.parse('RECORD DELIMITER STANDARD-1', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.RecordDelimiterClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.RecordDelimiterClause).delimiterValue).toBe('STANDARD-1');
});

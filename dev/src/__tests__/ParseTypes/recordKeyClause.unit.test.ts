import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse('RECORD KEY IS FILE1 IN FILE2 OF FILE3', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.RecordKeyClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.RecordKeyClause).delimiterValue).toBe('FILE1');
  expect((ast.nodes[0] as Nodes.RecordKeyClause).dataNames[0]).toBe('FILE2');
  expect((ast.nodes[0] as Nodes.RecordKeyClause).dataNames[1]).toBe('FILE3');
});

test('min statement', () => {
  const ast: Ast = zionParser.parse('RECORD FILE1', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.RecordKeyClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.RecordKeyClause).delimiterValue).toBe('FILE1');
});

import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Full statement', () => {
  const ast: Ast = zionParser.parse(
    '        ALTERNATE RECORD KEY IS DATA-NAME IN DATA-NAME2\r' +
      '        PASSWORD IS PWD WITH DUPLICATES                ',
    {
      fromPath: false,
    },
  );
  expect(ast.nodes[0].type).toBe(Syntax.AlternateRecordKeyClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.AlternateRecordKeyClause).value).toBe('DATA-NAME');
  expect((ast.nodes[0] as Nodes.AlternateRecordKeyClause).dataNames[0]).toBe('DATA-NAME2');
  expect(((ast.nodes[0] as Nodes.AlternateRecordKeyClause).passwordClause as Nodes.PasswordClause).value).toBe('PWD');
});

test('Minimum statement', () => {
  const ast: Ast = zionParser.parse('        ALTERNATE RECORD KEY IS DATA-NAME', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.AlternateRecordKeyClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.AlternateRecordKeyClause).value).toBe('DATA-NAME');
});

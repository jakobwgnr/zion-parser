import { Ast } from '../../Ast';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

describe('parseDecimalPointClause working correctly', () => {
  test('Full statement', () => {
    const ast: Ast = zionParser.parse('DECIMAL-POINT IS COMMA', {
      fromPath: false,
    });
    expect(ast.nodes[0].type).toBe(Syntax.DecimalPointClause);
    expect(ast.errors.length).toBe(0);
  });

  test('minimal statement', () => {
    const ast: Ast = zionParser.parse('DECIMAL-POINT COMMA', {
      fromPath: false,
    });
    expect(ast.nodes[0].type).toBe(Syntax.DecimalPointClause);
    expect(ast.errors.length).toBe(0);
  });
});

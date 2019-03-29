import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

describe('parseCurrencySignClause working correctly', () => {
  test('Full statement', () => {
    const ast: Ast = zionParser.parse('CURRENCY SIGN IS EUR', {
      fromPath: false,
    });
    expect(ast.nodes[0].type).toBe(Syntax.CurrencySignClause);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.CurrencySignClause).currencySignValue).toBe('EUR');
  });

  test('Minimal statement', () => {
    const ast: Ast = zionParser.parse('CURRENCY EUR', {
      fromPath: false,
    });
    expect(ast.nodes[0].type).toBe(Syntax.CurrencySignClause);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.CurrencySignClause).currencySignValue).toBe('EUR');
  });

  test('Statement - optional "IS" missing', () => {
    const ast: Ast = zionParser.parse('CURRENCY SIGN EUR', {
      fromPath: false,
    });
    expect(ast.nodes[0].type).toBe(Syntax.CurrencySignClause);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.CurrencySignClause).currencySignValue).toBe('EUR');
  });

  test('Statement - optional "SIGN" missing', () => {
    const ast: Ast = zionParser.parse('CURRENCY IS EUR', {
      fromPath: false,
    });
    expect(ast.nodes[0].type).toBe(Syntax.CurrencySignClause);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.CurrencySignClause).currencySignValue).toBe('EUR');
  });
});

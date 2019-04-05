import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Standard statement', () => {
  const ast: Ast = zionParser.parse('PADDING CHARACTER IS LITERAL IN FILE1 OF FILE2', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.PaddingCharacterClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.PaddingCharacterClause).paddingCaracterValue).toBe('LITERAL');
  expect((ast.nodes[0] as Nodes.PaddingCharacterClause).dataNames[0]).toBe('FILE1');
  expect((ast.nodes[0] as Nodes.PaddingCharacterClause).dataNames[1]).toBe('FILE2');
});

test('Minimum statement', () => {
  const ast: Ast = zionParser.parse('PADDING LITERAL', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.PaddingCharacterClause);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.PaddingCharacterClause).paddingCaracterValue).toBe('LITERAL');
});

test('Error statement', () => {
  const ast: Ast = zionParser.parse('PADDING CHARACTER', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.PaddingCharacterClause);
  expect(ast.errors.length).toBe(1);
});

test('SpecialRegister statement', () => {
  const ast: Ast = zionParser.parse('PADDING CHARACTER IS WHEN-COMPILED', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.PaddingCharacterClause);
  expect(ast.errors.length).toBe(0);
  expect(
    ((ast.nodes[0] as Nodes.PaddingCharacterClause).paddingCaracterValue as Nodes.SpecialRegister).specialRegisterType,
  ).toBe('WHEN-COMPILED');
});

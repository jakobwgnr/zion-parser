import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

test('Standard statement', () => {
  const ast: Ast = zionParser.parse('WHEN-COMPILED', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.SpecialRegister);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.SpecialRegister).specialRegisterType).toBe('WHEN-COMPILED');
});

test('ADDRESS statement', () => {
  const ast: Ast = zionParser.parse('ADDRESS OF 123', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.SpecialRegister);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.SpecialRegister).specialRegisterType).toBe('ADDRESS');
  expect((ast.nodes[0] as Nodes.SpecialRegister).optionalValue).toBe('123');
});

test('LENGTH statement', () => {
  const ast: Ast = zionParser.parse('LENGTH OF 123', {
    fromPath: false,
  });
  expect(ast.nodes[0].type).toBe(Syntax.SpecialRegister);
  expect(ast.errors.length).toBe(0);
  expect((ast.nodes[0] as Nodes.SpecialRegister).specialRegisterType).toBe('LENGTH');
  expect((ast.nodes[0] as Nodes.SpecialRegister).optionalValue).toBe('123');
});

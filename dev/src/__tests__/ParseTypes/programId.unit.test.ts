import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

describe('parseProgramId working correctly', () => {
  test('Full statement', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME IS INITIAL PROGRAM.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.ProgramId).programIdValue).toBe('PGMNAME');
  });

  test('Minimum statement', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID PGMNAME', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.ProgramId).programIdValue).toBe('PGMNAME');
  });
  test('ProgramID Variante 3', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.ProgramId).programIdValue).toBe('PGMNAME');
  });
  test('Keyword as name not allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. TRUE', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(1);
  });

  test('Initial alone allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME INITIAL', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.ProgramId).programIdValue).toBe('PGMNAME');
  });

  test('IS INITIAL  allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME IS INITIAL', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.ProgramId).programIdValue).toBe('PGMNAME');
  });

  test('INITIAL PROGRAM allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME INITIAL PROGRAM', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.ProgramId).programIdValue).toBe('PGMNAME');
  });

  test('IS alone not allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME IS', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(1);
  });

  test('Optional Terminator is allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.ProgramId).programIdValue).toBe('PGMNAME');
  });

  test('Different order not allowed', () => {
    const ast: Ast = zionParser.parse('PROGRAM-ID. PGMNAME PROGRAM IS INITIAL.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.ProgramId);
    expect(ast.errors.length).toBe(2);
  });
});

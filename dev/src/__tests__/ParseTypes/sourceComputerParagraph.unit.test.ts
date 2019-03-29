import { Ast } from '../../Ast';
import * as Nodes from '../../nodes';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

describe('sourceComputerParagraph working correctly', () => {
  test('Full statement', () => {
    const ast: Ast = zionParser.parse('SOURCE-COMPUTER. COMNAME WITH DEBUGGING MODE.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.SourceComputerParagraph);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.SourceComputerParagraph).sourceComputerValue).toBe('COMNAME');
  });

  test('minimum', () => {
    const ast: Ast = zionParser.parse('SOURCE-COMPUTER.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.SourceComputerParagraph);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.SourceComputerParagraph).sourceComputerValue).toBe('');
  });

  test('missing terminator should throw error', () => {
    const ast: Ast = zionParser.parse('SOURCE-COMPUTER', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.SourceComputerParagraph);
    expect(ast.errors.length).toBeGreaterThan(0);
    expect((ast.nodes[0] as Nodes.SourceComputerParagraph).sourceComputerValue).toBe('');
  });

  test('Computer name only', () => {
    const ast: Ast = zionParser.parse('SOURCE-COMPUTER. COMNAME.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.SourceComputerParagraph);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.SourceComputerParagraph).sourceComputerValue).toBe('COMNAME');
  });

  test('Without "WITH" Keyword', () => {
    const ast: Ast = zionParser.parse('SOURCE-COMPUTER. COMNAME DEBUGGING MODE.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.SourceComputerParagraph);
    expect(ast.errors.length).toBe(0);
    expect((ast.nodes[0] as Nodes.SourceComputerParagraph).sourceComputerValue).toBe('COMNAME');
  });

  test('"WITH" Keyword alone not allowed', () => {
    const ast: Ast = zionParser.parse('SOURCE-COMPUTER. COMNAME WITH.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.SourceComputerParagraph);
    expect(ast.errors.length).toBeGreaterThan(0);
    expect((ast.nodes[0] as Nodes.SourceComputerParagraph).sourceComputerValue).toBe('COMNAME');
  });

  test('Optional values without terminator not allowed', () => {
    const ast: Ast = zionParser.parse('SOURCE-COMPUTER. COMNAME WITH DEBUGGING MODE', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.SourceComputerParagraph);
    expect(ast.errors.length).toBeGreaterThan(0);
    expect((ast.nodes[0] as Nodes.SourceComputerParagraph).sourceComputerValue).toBe('COMNAME');
  });
});

import { Ast } from '../../Ast';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

describe('parseRecordingModeClause working correctly', () => {
  test('Full statement', () => {
    const ast: Ast = zionParser.parse('RECORDING MODE IS F', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.RecordingModeClause);
    expect(ast.errors.length).toBe(0);
  });
  test('NO MODE', () => {
    const ast: Ast = zionParser.parse('RECORDING IS F', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.RecordingModeClause);
    expect(ast.errors.length).toBe(0);
  });
  test('NO IS', () => {
    const ast: Ast = zionParser.parse('RECORDING MODE F', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.RecordingModeClause);
    expect(ast.errors.length).toBe(0);
  });
  test('BOTH OPTIONAL MISSING', () => {
    const ast: Ast = zionParser.parse('RECORDING F', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.RecordingModeClause);
    expect(ast.errors.length).toBe(0);
  });
});

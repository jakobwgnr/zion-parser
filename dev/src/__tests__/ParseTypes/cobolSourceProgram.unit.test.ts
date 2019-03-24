import { Ast } from '../../Ast';
import { Syntax } from '../../syntax';
import * as zionParser from '../../zion-parser';

describe('parseCobolSourceProgram working correctly', () => {
  test('Full statement', () => {
    const ast: Ast = zionParser.parse('IDENTIFICATION DIVISION.\rPROGRAM-ID. PGNAME.', { fromPath: false });
    expect(ast.nodes[0].type).toBe(Syntax.CobolSourceProgram);
    expect(ast.errors.length).toBe(0);
  });
});

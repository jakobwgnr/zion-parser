import { Ast } from '../Ast';
import { Syntax } from '../syntax';
import * as zionParser from '../zion-parser';

test('zionParser.parse throws error on no such file', () => {
  expect(() => {
    zionParser.parse('/path/to/nowhere/test.txt');
  }).toThrow();
});

test('zionParser.parse takes a String stdin as input', () => {
  const ast: Ast = zionParser.parse('RECORDING MODE IS F', { fromPath: false });
  expect(ast.nodes[0].type).toBe(Syntax.RecordingModeClause);
});

test('zionParser.parse takes a path as input', () => {
  const ast: Ast = zionParser.parse('./dev/src/__tests__/testfiles/recording.cbl');
  expect(ast.nodes[0].type).toBe(Syntax.RecordingModeClause);
});

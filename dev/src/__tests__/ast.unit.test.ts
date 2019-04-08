import { Token } from 'zion-lexer';
import { Ast } from '../Ast';
import { ParseError } from '../ErrorHandler/parse-error';
import * as Nodes from '../nodes';
import { Node } from '../nodes';

test('Ast Constructor works expected', () => {
  const nodes: Node[] = [];
  const tokens: Token[] = [];
  const errors: ParseError[] = [];
  nodes.push(
    new Nodes.RecordingModeClause(
      {
        startColumnTotal: 1,
        startColumnRelative: 2,
        startLine: 3,
      },
      'V',
    ),
  );
  tokens.push(new Token());
  errors.push(new ParseError('msg', 1, 2, 3, 'desc'));

  const ast: Ast = new Ast(nodes, tokens, errors);
  expect(ast.nodes.length).toBe(1);
  expect(ast.tokens.length).toBe(1);
  expect(ast.errors.length).toBe(1);
});

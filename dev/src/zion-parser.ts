/**
 * @fileoverview MAIN Script exporting the two functions parse & lex
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------
import { Lexer } from './Lexer';
import { Parser } from './Parser';

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export function parse(code: string) {
  const lexer = new Lexer(code);
  const parser = new Parser();
  const tokens = lexer.execute();

  return parser.execute(tokens);
}

export function lex(code: string) {
  const lexer = new Lexer(code);

  return lexer.execute();
}

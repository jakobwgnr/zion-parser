/**
 * @fileoverview MAIN Script exporting the two functions parse & lex
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------
require('debug').enable('zion-parser:*,-zion-parser:code-path');
import { fscheck } from 'zion-commons';

import * as zionLexer from 'zion-lexer';

import { Parser } from './Parser';

import { Ast } from './Ast';

// ------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------

function optionDefaults(): any {
  return {
    fromPath: true, // By default input is read from Path
  };
}

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export function parse(input: string, options?: any): Ast {
  if (!options) {
    options = optionDefaults();
  }
  let code: string;

  if (options.fromPath) {
    code = fscheck.codeFromPath(input);
  } else {
    code = input;
  }

  const parser = new Parser(zionLexer.lex(code, { fromPath: false }));
  const nodes = parser.execute();
  const ast: Ast = new Ast(nodes, parser.tokens, parser.errorHandler.errors);
  return ast;
}

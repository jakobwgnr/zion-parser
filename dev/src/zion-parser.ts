/**
 * @fileoverview MAIN Script exporting the two functions parse & lex
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------
require('debug').enable('zion-parser:*,-zion-parser:code-path');
import * as fs from 'fs';
import * as fscheck from './util/fscheck';

import { Lexer } from './Lexer/Lexer';
import { Parser } from './Parser/Parser';

import * as path from 'path';
import { Ast } from './Parser/Ast';

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export function parse(input: string, options?: any): Ast {
  if (!options) {
    options = optionDefaults();
  }
  let code: string;

  if (options.fromPath) {
    code = codeFromPath(input);
  } else {
    code = input;
  }

  const lexer = new Lexer(code);
  const parser = new Parser(lexer.execute());
  const nodes = parser.execute();
  const ast: Ast = new Ast(nodes, parser.tokens, parser.errorHandler.errors);
  return ast;
}

export function lex(input: string, options?: any) {
  if (!options) {
    options = optionDefaults();
  }
  let code: string;

  if (options.fromPath) {
    code = codeFromPath(input);
  } else {
    code = input;
  }

  const lexer = new Lexer(code);
  return lexer.execute();
}

function codeFromPath(filePath: string): string {
  if (fscheck.isFile(path.join(__dirname, filePath))) {
    return fs.readFileSync(path.join(__dirname, filePath), 'utf8');
  } else {
    throw new Error('Not a file');
  }
}

function optionDefaults(): any {
  return {
    fromPath: true, // By default input is read from Path
  };
}

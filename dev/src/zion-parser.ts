/**
 * @fileoverview MAIN Script exporting the two functions parse & lex
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------
import * as fs from 'fs';
import * as fscheck from './util/fscheck';

import { Lexer } from './Lexer/Lexer';
import { Node } from './Parser/nodes';
import { Parser } from './Parser/Parser';

import * as path from 'path';
require('debug').enable('zion-parser:*,-zion-parser:code-path');

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export function parse(input: string, options?: any): Node[] {
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
  const program = parser.execute();
  const ast = program as any;
  ast.tokens = parser.tokens;
  ast.errors = parser.errorHandler.errors;
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

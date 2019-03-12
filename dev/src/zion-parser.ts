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
import { Parser } from './Parser/Parser';
import { Options } from './zion-parser-options';

import * as path from 'path';


// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export function parse(input: string, options?: Options) {
  if (!options) {
    options = new Options();
  }

  if (options.fromPath) {
    if (fscheck.isFile(input)) {
      fs.readFile(input, 'utf8', (error, data) => {
        const lexer = new Lexer(data);
        const parser = new Parser();
        const tokens = lexer.execute();

        return parser.execute(tokens);
      })
    } else {
      throw new Error('Not a file');
    }
  } else {
    const lexer = new Lexer(input);
    const parser = new Parser();
    const tokens = lexer.execute();

    return parser.execute(tokens);
  }
}

export function lex(input: string, options?: Options) {
  if (!options) {
    options = new Options();
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
    return fs.readFileSync(path.join(__dirname, filePath), 'utf8')
  } else {
    throw new Error('Not a file');
  }

}

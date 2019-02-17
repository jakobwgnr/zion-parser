/**
 * @fileoverview MAIN Script exporting the two functions parse & lex
 * @author Jakob Wagner
 */

//------------------------------------------------------------------------------
// Requirements
//------------------------------------------------------------------------------
const Lexer = require('./Lexer');
const Parser = require('./Parser');

//------------------------------------------------------------------------------
// Public Interface
//------------------------------------------------------------------------------

export function parse(code) {
    const lexer = new Lexer(code);
    const parser = new Parser();
    const tokens = lexer.execute();

    return parser.execute(tokens);
}

export function lex(code) {
    const lexer = new Lexer(code);

    return lexer.execute();
}

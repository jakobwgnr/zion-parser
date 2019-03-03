/**
 * @fileoverview Responsible for lexing sourcecode and creating a list of tokens
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------

import { Character } from "./character";
import { SourceCode } from "./Sourcecode";
import { Token } from "./Token";

// ------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export class Lexer {
  private readonly sourcecode: SourceCode;
  private chars: string = "";
  private tokenIdentified: boolean = false;
  private token: Token = new Token();
  private tokenList: Token[] = [];

  constructor(code: string) {
    this.sourcecode = new SourceCode(code);
  }

  public execute() {

    while (!this.sourcecode.eof()) {

      if (Character.isLineTerminator(this.sourcecode.getCurrentChar())) {
        // TODO: currently I think no multiline tokens in COBOL...
        if (this.tokenIdentified) {
          this.tokenEnded();
        }
        this.sourcecode.NextChar();
      }

      this.identifyToken(this.sourcecode.getCurrentChar());
      this.sourcecode.NextChar();

    }

    // write last token if identified
    if (this.tokenIdentified) {
      this.tokenEnded();
    }

    this.tokenList.push(new Token("", "EOF", 1, undefined, 1, this.sourcecode.columnsTotal, undefined, this.sourcecode.currentLine));


    return this.tokenList;

  }

  private identifyToken(currentChar: string) {

    if (currentChar === " ") {
      if (this.tokenIdentified) {
        if (this.token.type === 'Comment' || this.token.type === 'String') {
          // just a char within the comment/string --> include
          this.chars = this.chars.concat(currentChar);
        } else {
          // a token has ended
          this.tokenEnded();
        }
      } else {
        // do nothing - just skip the character 
      }
    } else {
      if (currentChar === "*" && !this.tokenIdentified && this.sourcecode.currentColumnRelative === 7) {
        this.tokenStart('Comment');
      } else {
        this.chars = this.chars.concat(currentChar);
      }
    }
  }

  private tokenStart(tokenType: string) {
    this.tokenIdentified = true;

    this.token.startColumnRelative = this.sourcecode.currentColumnRelative;
    this.token.startColumnTotal = this.sourcecode.columnsTotal;
    this.token.startLine = this.sourcecode.currentLine;
    this.token.type = tokenType;
  }
  private tokenEnded() {
    this.token.value = this.chars;
    this.token.endLine = this.sourcecode.currentLine;
    this.token.endColumnRelative = this.sourcecode.currentColumnRelative;
    this.token.endColumnTotal = this.sourcecode.columnsTotal;

    this.chars = "";

    const insertToken: Token = new Token();
    insertToken.type = this.token.type;
    insertToken.value = this.token.value;
    insertToken.startLine = this.token.startLine;
    insertToken.startColumnTotal = this.token.startColumnTotal;
    insertToken.startColumnRelative = this.token.startColumnRelative;
    insertToken.endColumnRelative = this.token.endColumnRelative;
    insertToken.endColumnTotal = this.token.endColumnTotal;
    insertToken.endLine = this.token.endLine;

    this.tokenList.push(insertToken);

    this.token.initToken();
    this.tokenIdentified = false;
  }
}

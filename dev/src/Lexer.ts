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
  private token: Token = new Token();
  private tokenList: Token[] = [];

  constructor(code: string) {
    this.sourcecode = new SourceCode(code);
  }

  public execute() {

    while (!this.sourcecode.eof()) {
      this.identifyToken(this.sourcecode.getCurrentChar());
      this.sourcecode.NextChar();

    }

    this.tokenList.push(new Token("", "EOF", 1, undefined, 1, this.sourcecode.columnsTotal, undefined, this.sourcecode.currentLine));


    return this.tokenList;

  }

  private identifyToken(currentChar: string) {

    if (currentChar === " ") {
      // do nothing - just skip the Space - Not within a Token 
    } else {
      // Comment Token
      if (currentChar === "*" && this.sourcecode.currentColumnRelative === 7) {
        this.tokenStart();
        this.token.type = "Comment";

        while (!Character.isLineTerminator(this.sourcecode.getCurrentChar()) && !this.sourcecode.eof()) {
          this.chars = this.chars.concat(this.sourcecode.getCurrentChar());
          this.sourcecode.NextChar();
        }

        this.tokenEnded();

      } else {
        if (Character.isIdentifierStart(currentChar)) {
          this.tokenStart();

          while (Character.isIdentifierPart(currentChar)) {
            this.chars = this.chars.concat(currentChar);
          }

          if (this.isKeyword(this.chars)) {
            this.token.type = "Keyword";
          } else {
            this.token.type = "Value";
          }

          this.tokenEnded();

        }
        this.chars = this.chars.concat(currentChar);
      }
    }
  }

  private tokenStart() {
    this.token.startColumnRelative = this.sourcecode.currentColumnRelative;
    this.token.startColumnTotal = this.sourcecode.columnsTotal;
    this.token.startLine = this.sourcecode.currentLine;
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
  }

  private isKeyword(word: string) {
    // TODO: Implement
    return false;
  }
}

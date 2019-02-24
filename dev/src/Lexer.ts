/**
 * @fileoverview Responsible for lexing sourcecode and creating a list of tokens
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------

import { Token } from "./Token";

// ------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export class Lexer {
  private readonly sourcecode: string;
  private chars: string = "";
  private currentLine: number = 1;
  private currentColumnTotal: number = 1;
  private currentColumnRelative: number = 1;
  private tokenIdentified: boolean = false;
  private token: Token = new Token();
  private tokenList: Token[] = [];

  constructor(sourcecode: string) {
    this.sourcecode = sourcecode;
  }

  public execute() {

    for (const currentChar of this.sourcecode) {

      this.categorizeCurrentChar(currentChar);

      /* line counter - Also resets currentColumnRelative */
      if (this.chars === "\r\n") {
        this.currentColumnRelative = 1;
        this.currentLine++;
      }

      /* Don't count escape chars as char column*/
      if (currentChar !== "\r" && currentChar !== "\n") {
        this.currentColumnTotal++;
        this.currentColumnRelative++;
      }

    }

    // EOF
    if (this.tokenIdentified) {
      this.tokenEnded();
    }

    return this.tokenList;

  }

  private categorizeCurrentChar(currentChar: string) {
    switch (currentChar) {
      case " ": {
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
        break;
      }

      case "*": {
        if (!this.tokenIdentified && this.currentColumnRelative === 7) {
          this.tokenStart('Comment');
        } else {
          this.chars = this.chars.concat(currentChar);
        }
        break;
      }
      case "\r":
      case "\n": {
        if (this.tokenIdentified) {
          this.tokenEnded();
          this.chars = currentChar;
        } else {
          this.chars = this.chars.concat(currentChar);
        }
        break;
      }
      default: {
        this.chars = this.chars.concat(currentChar);
        break;
      }


    }
  }

  private tokenStart(tokenType: string) {
    this.tokenIdentified = true;

    this.token.startColumnRelative = this.currentColumnRelative;
    this.token.startColumnTotal = this.currentColumnTotal;
    this.token.startLine = this.currentLine;
    this.token.type = tokenType;
  }
  private tokenEnded() {
    this.token.value = this.chars;
    this.token.endLine = this.currentLine;
    this.token.endColumnRelative = this.currentColumnRelative;
    this.token.endColumnTotal = this.currentColumnTotal;

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

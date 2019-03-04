/**
 * @fileoverview Responsible for lexing sourcecode and creating a list of tokens
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------

import { Character } from "./character";
import { Keyword } from "./keyword";
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
  private token: Token = new Token();
  private tokenList: Token[] = [];

  constructor(code: string) {
    this.sourcecode = new SourceCode(code);
  }

  public execute() {

    while (!this.sourcecode.eof()) {

      if (this.sourcecode.getCurrentChar() === " ") {
        // do nothing - just skip the Space - Not within a Token 
        this.sourcecode.NextChar();
      } else {

        if ((this.sourcecode.getCurrentChar() === "*" ||
          this.sourcecode.getCurrentChar() === "/")
          && this.sourcecode.currentColumnRelative === 7) {

          this.processCommentToken();

        } else {
          if (Character.isCobolWordStart(this.sourcecode.getCurrentChar())) {
            // Needs to proccess a few chars as a Token can't be distinguished on first char
            while (Character.isCobolWordPart(this.sourcecode.getCurrentChar())) {
              this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
              this.sourcecode.NextChar();
            }

            if (Character.isBracket(this.sourcecode.getCurrentChar()) && this.token.value.length <= 2) {

              this.processPicValueToken();

            } else {

              this.processMiscIdentifierTokens();
            }

          } else {
            if (Character.isNumberIndicator(this.sourcecode.getCurrentChar())) {

              this.processNumericToken();

            } else {
              if (Character.isStringIndicator(this.sourcecode.getCurrentChar())) {

                this.processStringToken();

              }
            }
          }
        }
      }
    }

    this.tokenList.push(new Token("", "EOF", 1, undefined, 1, this.sourcecode.columnsTotal, undefined, this.sourcecode.currentLine));
    return this.tokenList;

  }

  private processCommentToken(): void {
    this.tokenStart();
    this.token.type = "Comment";

    while (!Character.isLineTerminator(this.sourcecode.getCurrentChar()) && !this.sourcecode.eof()) {
      this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
      this.sourcecode.NextChar();
    }

    this.tokenEnded();
  }

  private processPicValueToken(): void {
    this.tokenStart();
    this.token.type = "PIC";
    do {
      this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
      this.sourcecode.NextChar();

    } while (!Character.isBracket(this.sourcecode.getCurrentChar()))

    // Also add the closing bracket - TODO: is there a more beautiful way
    this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
    this.sourcecode.NextChar();

    this.tokenEnded();
  }

  private processMiscIdentifierTokens() {
    this.tokenStart();

    if (Character.isNumeric(this.token.value)) {
      if (Character.isLevelIndicator(this.token.value)) {
        this.token.type = "Level";
      } else {
        if (this.token.startColumnRelative === 1) {
          this.token.type = "SequenceNumberLiteral";
        } else {
          this.token.type = "NumericLiteral";
        }
      }
    } else {
      if (Keyword.isKeyword(this.token.value)) {
        this.token.type = "Keyword";
      } else {
        this.token.type = "Identifier";
      }
    }

    this.tokenEnded();
  }

  private processNumericToken(): void {
    this.tokenStart();

    this.token.type = "NumericLiteral";
    while (Character.isDecimalDigit(this.sourcecode.getCurrentChar())) {
      this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
      this.sourcecode.NextChar();
    }
    this.tokenEnded();
  }

  private processStringToken(): void {
    this.tokenStart();

    this.token.type = "StringLiteral";
    do {
      this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
      this.sourcecode.NextChar();
    } while (!Character.isStringIndicator(this.sourcecode.getCurrentChar()))

    // Still add the StringIndicator (' or ") to the chars value - TODO: Is there a better solution?
    this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
    this.sourcecode.NextChar();

    this.tokenEnded();

  }

  private tokenStart() {
    this.token.startColumnRelative = this.sourcecode.currentColumnRelative;
    this.token.startColumnTotal = this.sourcecode.columnsTotal;
    this.token.startLine = this.sourcecode.currentLine;
  }
  private tokenEnded() {
    this.token.endLine = this.sourcecode.currentLine;
    this.token.endColumnRelative = this.sourcecode.currentColumnRelative;
    this.token.endColumnTotal = this.sourcecode.columnsTotal;

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

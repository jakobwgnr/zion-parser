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
      this.identifyToken();
      this.sourcecode.NextChar();

    }

    this.tokenList.push(new Token("", "EOF", 1, undefined, 1, this.sourcecode.columnsTotal, undefined, this.sourcecode.currentLine));
    return this.tokenList;

  }

  private identifyToken() {

    if (this.sourcecode.getCurrentChar() === " ") {
      // do nothing - just skip the Space - Not within a Token 
    } else {
      // Comment Token
      if ((this.sourcecode.getCurrentChar() === "*" ||
        this.sourcecode.getCurrentChar() === "/")
        && this.sourcecode.currentColumnRelative === 7) {
        this.tokenStart();
        this.token.type = "Comment";

        while (!Character.isLineTerminator(this.sourcecode.getCurrentChar()) && !this.sourcecode.eof()) {
          this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
          this.sourcecode.NextChar();
        }

        this.tokenEnded();

      } else {
        if (Character.isIdentifierStart(this.sourcecode.getCurrentChar())) {
          // Could be Keyword or Value
          this.tokenStart();

          while (Character.isKeywordPart(this.sourcecode.getCurrentChar())) {
            this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
            this.sourcecode.NextChar();
          }

          if (Character.isBracket(this.sourcecode.getCurrentChar()) && this.token.value.length <= 2) {
            if (this.token.value === "X") {
              this.token.type = "PICAlfaNumeric";
            } else {
              this.token.type = "PICNumeric";
            }
            do {
              this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
              this.sourcecode.NextChar();

            } while (!Character.isBracket(this.sourcecode.getCurrentChar()))
            this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
          } else {

            if (Keyword.isKeyword(this.token.value)) {
              this.token.type = "Keyword";
            } else {
              this.token.type = "Value";
              while (Character.isIdentifierPart(this.sourcecode.getCurrentChar())) {
                this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
                this.sourcecode.NextChar();
              }
            }
          }
          this.tokenEnded();
        } else {
          if (Character.isDecimalDigit(this.sourcecode.getCurrentChar())) {
            // Could be Level indicator or just normal decimal value
            this.tokenStart();
            while (Character.isDecimalDigit(this.sourcecode.getCurrentChar())) {
              this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
              this.sourcecode.NextChar();
            }

            if (Character.isLevelIndicator(this.token.value)) {
              this.token.type = "Level";
            } else {
              this.token.type = "Number";
            }
            this.tokenEnded();
          } else {
            if (Character.isNumberIndicator(this.sourcecode.getCurrentChar())) {
              // is a Decimal value
              this.tokenStart();
              this.token.type = "Number";
              while (Character.isDecimalDigit(this.sourcecode.getCurrentChar())) {
                this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
                this.sourcecode.NextChar();
              }
              this.tokenEnded();
            } else {
              if (Character.isStringIndicator(this.sourcecode.getCurrentChar())) {
                // is a String value
                this.tokenStart();
                this.token.type = "String";
                do {
                  this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
                  this.sourcecode.NextChar();
                } while (!Character.isStringIndicator(this.sourcecode.getCurrentChar()))

                // Still add the Indicator to the chars value - TODO: Is there a better solution?
                this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
                this.tokenEnded();
              }
            }
          }
        }

      }
    }
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

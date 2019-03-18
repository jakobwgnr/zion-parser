/**
 * @fileoverview Responsible for lexing sourcecode and creating a list of tokens
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------

import * as logger from "../config/winston";
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
      if (Character.isWhiteSpace(this.sourcecode.getCurrentChar())) {
        this.processWhiteSpaceToken();
      } else {
        if (Character.isDecimalDigit(this.sourcecode.getCurrentChar()) && this.sourcecode.currentColumnRelative >= 1 && this.sourcecode.currentColumnRelative <= 6) {

          this.processSequenceNumberToken();

        } else {

          if ((this.sourcecode.getCurrentChar() === "*" ||
            this.sourcecode.getCurrentChar() === "/")
            && this.sourcecode.currentColumnRelative === 7) {

            this.processCommentToken();

          } else {
            if (Character.isCobolAritmeticOperator(this.sourcecode.getCurrentChar())) {

              this.processOperator();

            } else {
              if (Character.isCobolWordStart(this.sourcecode.getCurrentChar())) {

                this.processMiscIdentifierTokens();

              } else {
                if (Character.isStringIndicator(this.sourcecode.getCurrentChar())) {

                  this.processStringToken();

                } else {
                  if (Character.isCobolTerminator(this.sourcecode.getCurrentChar())) {
                    this.processTerminatorToken();
                  } else {
                    this.processNotIdentifiedToken();
                  }
                }
              }
            }

          }
        }
      }

      // this.sourcecode.NextChar();
    }

    this.tokenList.push(new Token("", "EOF", 1, undefined, 1, this.sourcecode.columnsTotal, undefined, this.sourcecode.currentLine));

    return this.tokenList;

  }

  private processWhiteSpaceToken(): void {
    this.tokenStart();
    this.token.type = "WhiteSpace";

    do {
      this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
      this.sourcecode.NextChar();
    } while (Character.isWhiteSpace(this.sourcecode.getCurrentChar()))

    this.tokenEnd();

    // this.sourcecode.NextChar();
  }

  private processSequenceNumberToken(): void {
    this.tokenStart();
    this.token.type = "SequenceNumberLiteral";

    do {
      this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
      this.sourcecode.NextChar();
    } while (this.sourcecode.currentColumnRelative <= 6)

    this.tokenEnd();
  }


  private processCommentToken(): void {
    this.tokenStart();
    this.token.type = "Comment";

    while (!Character.isLineTerminator(this.sourcecode.getCurrentChar()) && !this.sourcecode.eof()) {
      this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
      this.sourcecode.NextChar();
    }

    this.tokenEnd();

    this.sourcecode.NextChar();
  }

  private processOperator(): void {
    this.tokenStart();

    this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
    this.sourcecode.NextChar();

    if (Character.isDecimalDigit(this.sourcecode.getCurrentChar())) {
      this.token.type = "NumericLiteral";
      while (Character.isDecimalDigit(this.sourcecode.getCurrentChar())) {
        this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
        this.sourcecode.NextChar();
      }
    } else {
      this.token.type = "Operator";
      while (Character.isCobolAritmeticOperator(this.sourcecode.getCurrentChar())) {
        this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
        this.sourcecode.NextChar();
      }
    }

    this.tokenEnd();
  }

  private processMiscIdentifierTokens() {

    this.tokenStart();
    while (Character.isCobolWordPart(this.sourcecode.getCurrentChar())) {
      this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
      this.sourcecode.NextChar();
    }

    if (Character.isNumeric(this.token.value)) {
      if (Character.isLevelIndicator(this.token.value)) {
        this.token.type = "Level";
      } else {
        this.token.type = "NumericLiteral";
      }
    } else {
      if (Keyword.isKeyword(this.token.value)) {
        this.token.type = "Keyword";
      } else {
        if (Keyword.isExec(this.token.value)) {
          this.token.type = "EXEC";
          while (!Keyword.containsEndExec(this.token.value)) {
            this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
            this.sourcecode.NextChar();
          }
        } else {
          if (this.token.startColumnRelative >= 72) {
            this.token.type = "IdentificationArea";
          } else {
            this.token.type = "Identifier";
          }
        }
      }
    }

    this.tokenEnd();
  }

  private processTerminatorToken(): void {
    this.tokenStart();
    this.token.type = "Terminator"
    this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
    this.tokenEnd();

    this.sourcecode.NextChar();
  }

  private processNotIdentifiedToken(): void {
    this.tokenStart();
    this.token.type = "NotIdentified"
    while (this.sourcecode.getCurrentChar() !== ' ' && !Character.isLineTerminator(this.sourcecode.getCurrentChar())) {
      this.token.value = this.token.value.concat(this.sourcecode.getCurrentChar());
      this.sourcecode.NextChar();
    }
    if (this.token.value !== "") {
      this.tokenEnd();
    } else {
      this.token.initToken();
    }

    if (Character.isLineTerminator(this.sourcecode.getCurrentChar())) {
      this.sourcecode.NextChar();
    }
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

    this.tokenEnd();

  }

  private tokenStart() {
    this.token.startColumnRelative = this.sourcecode.currentColumnRelative;
    this.token.startColumnTotal = this.sourcecode.columnsTotal;
    this.token.startLine = this.sourcecode.currentLine;
  }
  private tokenEnd() {
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
}

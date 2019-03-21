/**
 * @fileoverview Responsible for parsing tokens and creating an AST
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------

// const debug = require('debug')('zion-parser:parser');
import { ErrorHandler } from './ErrorHandler/error-handler';

import { Token } from '../Lexer/Token';
import { TokenType } from '../Lexer/tokentype';
import * as Nodes from '../Parser/nodes';
import { Node } from '../Parser/nodes';
import { Syntax } from './syntax';

// ------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export class Parser {
  private index: number = -1;
  private currentToken: Token = new Token();
  private nodeList: Node[] = [];
  public tokens: Token[];
  public errorHandler: ErrorHandler;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
    this.errorHandler = new ErrorHandler();
  }

  public execute(): Node[] {
    this.nextToken();
    while (!(this.currentToken.type === TokenType.EOF)) {
      let node: any = '';
      switch (this.currentToken.type) {
        case TokenType.Keyword:
          switch (this.currentToken.value) {
            case 'IDENTIFICATION':
            case 'ID':
              // node = this.parseCobolSourceProgram();
              break;
            case 'PROGRAM-ID':
              node = this.parseProgramId();
              break;
            case 'AUTHOR':
            case 'INSTALLATION':
            case 'DATE-WRITTEN':
            case 'DATE-COMPILED':
            case 'SECURITY':
              // node = this.parseIdentificationDivisionContent();
              break;
            case 'RECORDING':
              node = this.parseRecordingModeClause();
              break;
          }
          this.nodeList.push(node);
          break;
        default:
          this.errorHandler.unexpectedTokenError(this.currentToken, undefined, new Token('any', TokenType.Keyword));
          break;
      }
      this.nextToken();
    }

    return this.nodeList;
  }

  private parseRecordingModeClause(): Nodes.RecordingModeClause {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.RecordingModeClause;
    this.nextToken();
    /* istanbul ignore else */
    if (this.isOptionalKeyword('MODE', this.currentToken)) {
      this.nextToken();
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('IS', this.currentToken)) {
      this.nextToken();
    }

    node.setHasError(!this.expectModeIdentifier(this.currentToken));

    return this.finalizeNode(
      node,
      new Nodes.RecordingModeClause(node.startColumnTotal, node.startColumnRelative, node.startLine),
      this.currentToken,
    );
  }

  private parseProgramId(): Nodes.ProgramId {
    const startNode = this.startNode(this.currentToken);
    startNode.type = Syntax.ProgramId;
    let initialRequired: boolean = false;
    let initialExists: boolean = false;
    this.nextToken();
    /* istanbul ignore else */
    if (this.isOptionalTerminator(this.currentToken)) {
      this.nextToken();
    }

    startNode.setHasError(!this.expectIdentifier(this.currentToken));
    this.nextToken();

    /* istanbul ignore else */
    if (this.isOptionalKeyword('IS', this.currentToken)) {
      initialRequired = true;
      this.nextToken();
    }

    /* istanbul ignore else */
    if (initialRequired) {
      if (this.expectKeyword('INITIAL', this.currentToken)) {
        initialExists = true;
      } else {
        startNode.setHasError(true);
      }
      this.nextToken();
    } else {
      if (this.isOptionalKeyword('INITIAL', this.currentToken)) {
        initialExists = true;
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('PROGRAM', this.currentToken)) {
      if (!initialExists) {
        this.errorHandler.unexpectedTokenError(this.currentToken);
      }
      this.nextToken();
    }

    /* istanbul ignore else */
    if (this.isOptionalTerminator(this.currentToken)) {
      this.nextToken();
    }

    return this.finalizeNode(
      startNode,
      new Nodes.RecordingModeClause(startNode.startColumnTotal, startNode.startColumnRelative, startNode.startLine),
      this.currentToken,
    );
  }

  // --------------------------
  // HELPERS
  // --------------------------
  private nextToken() {
    if (!(this.currentToken.type === TokenType.EOF)) {
      this.index++;
      this.currentToken = this.tokens[this.index];
      if (this.currentToken && this.isIrrelevantToken(this.currentToken)) {
        this.nextToken();
      }
    }
  }

  private isIrrelevantToken(token: Token) {
    return (
      token.type === TokenType.Comment ||
      token.type === TokenType.IdentificationArea ||
      token.type === TokenType.SequenceNumberLiteral ||
      token.type === TokenType.WhiteSpace
    );
  }

  private startNode(token: Token) {
    const node = new Node(token.startColumnTotal, token.startColumnRelative, token.startLine);
    return node;
  }

  private finalizeNode(startNode: Node, node: any, endToken: Token) {
    node.endColumnRelative = endToken.endColumnRelative;
    node.endColumnTotal = endToken.endColumnTotal;
    node.endLine = endToken.endLine;
    node.type = startNode.type;
    node.hasError = startNode.hasError;

    return node;
  }

  // Expect the next token to match the specified keyword.
  // If not, an exception will be thrown.

  expectKeyword(keyword: string, token: Token): boolean {
    if (token.type !== TokenType.Keyword || token.value !== keyword) {
      this.errorHandler.unexpectedTokenError(token, undefined, new Token(keyword, TokenType.Keyword));
      return false;
    }
    return true;
  }

  expectModeIdentifier(token: Token): boolean {
    if (
      token.type !== TokenType.Identifier ||
      !(token.value === 'F' || token.value === 'V' || token.value === 'U' || token.value === 'S')
    ) {
      this.errorHandler.unexpectedTokenError(token, undefined, new Token('F or V or U or S', 'ModeIdentifier'));
      return false;
    }
    return true;
  }

  expectIdentifier(token: Token): boolean {
    if (token.type !== TokenType.Identifier) {
      this.errorHandler.unexpectedTokenError(token, undefined, new Token(undefined, TokenType.Identifier));
      return false;
    }
    return true;
  }

  isOptionalKeyword(keyword: string, token: Token) {
    return token.type === TokenType.Keyword && token.value === keyword;
  }

  isOptionalTerminator(token: Token) {
    return token.type === TokenType.Terminator;
  }
}

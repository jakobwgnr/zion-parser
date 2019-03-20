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
    this.currentToken = this.tokens[this.index];
    this.errorHandler = new ErrorHandler();
  }

  public execute(): Node[] {
    this.nextToken();
    while (this.index !== this.tokens.length) {
      let node: any = '';
      switch (this.currentToken.type) {
        case TokenType.Keyword:
          switch (this.currentToken.value.toUpperCase()) {
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
    let hasError: boolean = false;
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

    hasError = !this.expectModeIdentifier(this.currentToken);

    return this.finalizeNode(
      node,
      new Nodes.RecordingModeClause(node.startColumnTotal, node.startColumnRelative, node.startLine),
      this.currentToken,
      hasError,
    );
  }

  // --------------------------
  // HELPERS
  // --------------------------
  private nextToken() {
    this.index++;
    this.currentToken = this.tokens[this.index];
    if (this.currentToken && this.isIrrelevantToken(this.currentToken)) {
      this.nextToken();
    }
  }

  private isIrrelevantToken(token: Token) {
    return (
      token.type === TokenType.Comment ||
      token.type === TokenType.IdentificationArea ||
      token.type === TokenType.SequenceNumberLiteral ||
      token.type === TokenType.WhiteSpace ||
      token.type === TokenType.EOF
    );
  }

  private startNode(token: Token) {
    const node = new Node(token.startColumnTotal, token.startColumnRelative, token.startLine);
    return node;
  }

  private finalizeNode(startNode: Node, node: any, endToken: Token, hasError: boolean) {
    node.endColumnRelative = endToken.endColumnRelative;
    node.endColumnTotal = endToken.endColumnTotal;
    node.endLine = endToken.endLine;
    node.type = startNode.type;
    node.hasError = hasError;

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

  isOptionalKeyword(keyword: string, token: Token) {
    return token.type === TokenType.Keyword && token.value === keyword;
  }
}

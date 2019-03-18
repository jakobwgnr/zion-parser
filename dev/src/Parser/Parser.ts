/**
 * @fileoverview Responsible for parsing tokens and creating an AST
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------

import { ErrorHandler } from "./error-handler";

import { Token } from "../Lexer/Token";
import { TokenType } from '../Lexer/tokentype';
import * as Nodes from "../Parser/nodes";
import { Node } from "../Parser/nodes";
import { Messages } from "./messages";
const debug = require('debug')('zion-parser:parser');


// ------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------


// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export class Parser {
  private index: number = 0;
  private currentToken: Token = new Token();
  private nodeList: Node[] = [];
  private tokens: Token[];
  private errorHandler: ErrorHandler;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
    this.currentToken = this.tokens[this.index];
    this.errorHandler = new ErrorHandler();
  }

  public execute(): Node[] {

    this.nodeList.push(this.parseRecordingModeClause());



    // while (this.tokens.length <= this.index) {
    //   let node: any = '';
    //   switch (this.currentToken.type) {
    //     case TokenType.Keyword:
    //       switch (this.currentToken.value.toUpperCase()) {
    //         case 'RECORDING':
    //           node = this.parseRecordingModeClause();
    //           break;
    //       }
    //       break;
    //     case TokenType.WhiteSpace:
    //       break;
    //     case TokenType.SequenceNumberLiteral:
    //       break;
    //     case TokenType.Comment:
    //       break;
    //     case TokenType.NumericLiteral:
    //       break;
    //     case TokenType.Operator:
    //       break;
    //     case TokenType.Level:
    //       break;
    //     case TokenType.EXEC:
    //       break;
    //     case TokenType.IdentificationArea:
    //       break;
    //     case TokenType.Identifier:
    //       break;
    //     case TokenType.StringLiteral:
    //       break;
    //     default:
    //       break;
    //   }
    //   this.nodeList.push(node);
    //   this.nextToken();
    // }



    return this.nodeList;
  }

  private parseRecordingModeClause(): Nodes.RecordingModeClause {
    const node = this.startNode(this.currentToken);
    debug("parse" + this.currentToken.toString());
    this.nextToken();
    if (this.isOptionalKeyword('MODE', this.currentToken)) {
      node.tokenList.push(this.currentToken);
      this.nextToken();
    }
    if (this.isOptionalKeyword('IS', this.currentToken)) {
      node.tokenList.push(this.currentToken);
      this.nextToken();
    }

    this.expectModeIdentifier(this.currentToken);
    node.tokenList.push(this.currentToken);

    return this.finalizeNode(node, new Nodes.RecordingModeClause(node.startColumnTotal, node.startColumnRelative, node.startLine), this.currentToken)
  }




  // --------------------------
  // HELPERS
  // --------------------------
  private nextToken() {
    this.index++;
    this.currentToken = this.tokens[this.index];
  }

  private startNode(token: Token) {
    const node = new Node(token.startColumnTotal, token.startColumnRelative, token.startLine);
    node.tokenList.push(token);
    return node;
  }

  private finalizeNode(startNode: Node, node: any, endToken: Token) {
    node.endColumnRelative = endToken.endColumnRelative;
    node.endColumnTotal = endToken.endColumnTotal;
    node.endLine = endToken.endLine;
    node.tokenList = startNode.tokenList;

    return node;
  }

  // Expect the next token to match the specified keyword.
  // If not, an exception will be thrown.

  expectKeyword(keyword: string, token: Token) {
    if (token.type !== TokenType.Keyword || token.value !== keyword) {
      this.throwUnexpectedToken(token);
    }
  }

  expectModeIdentifier(token: Token) {
    if (token.type !== TokenType.Identifier || !(token.value === 'F' || token.value === 'V' || token.value === 'U' || token.value === 'S')) {
      this.throwUnexpectedToken(token);
    }
  }

  isOptionalKeyword(keyword: string, token: Token) {
    return token.type === TokenType.Keyword && token.value === keyword;
  }


  // Throw an exception because of the token.
  unexpectedTokenError(token: Token, message?: string): Error {
    let msg = message || Messages.UnexpectedToken;

    if (!message) {
      msg = (token.type === TokenType.EOF) ? Messages.UnexpectedEOS :
        (token.type === TokenType.Identifier) ? Messages.UnexpectedIdentifier :
          (token.type === TokenType.NumericLiteral) ? Messages.UnexpectedNumber :
            (token.type === TokenType.StringLiteral) ? Messages.UnexpectedString :
              Messages.UnexpectedToken;
    }

    msg = msg.replace('%0', token.value);

    return this.errorHandler.createError(token.startColumnTotal, token.startLine, token.endColumnRelative, msg);
  }

  throwUnexpectedToken(token: Token, message?: string): never {
    throw this.unexpectedTokenError(token, message);
  }
}


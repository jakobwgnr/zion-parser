/**
 * @fileoverview Responsible for parsing tokens and creating an AST
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------

// const debug = require('debug')('zion-parser:parser');
import { ErrorHandler } from './ErrorHandler/error-handler';

import { Token } from 'zion-lexer';
import { TokenType } from 'zion-lexer';
import * as Nodes from './nodes';
import { Node } from './nodes';
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
              node = this.parseCobolSourceProgram();
              break;
            case 'PROGRAM-ID':
              node = this.parseProgramId();
              break;
            case 'AUTHOR':
            case 'INSTALLATION':
            case 'DATE-WRITTEN':
            case 'DATE-COMPILED':
            case 'SECURITY':
              node = this.parseIdentificationDivisionContent();
              break;
            case 'RECORDING':
              node = this.parseRecordingModeClause();
              break;
            default:
              this.nextToken();
              break;
          }
          this.nodeList.push(node);
          break;
        default:
          this.errorHandler.unexpectedTokenError(this.currentToken, undefined, new Token('any', TokenType.Keyword));
          this.nextToken();
          break;
      }
    }

    return this.nodeList;
  }

  // TODO: Not finished
  private parseCobolSourceProgram(): Nodes.CobolSourceProgram {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.CobolSourceProgram;
    this.nextToken();
    node.setHasError(this.expectKeyword('DIVISION', this.currentToken));
    this.nextToken();
    node.setHasError(this.expectTerminator(this.currentToken));
    this.nextToken();
    const programId: Nodes.ProgramId = this.parseProgramId();

    return this.finalizeNode(
      node,
      new Nodes.CobolSourceProgram(node.startColumnTotal, node.startColumnRelative, node.startLine, programId),
      this.currentToken,
    );
  }

  private parseIdentificationDivisionContent(): Nodes.IdentificationDivisionContent {
    const node = this.startNode(this.currentToken);
    node.type = Syntax.IdentificationDivisionContent;
    let author: string = '';
    let installation: string = '';
    let dateWritten: string = '';
    let dateCompiled: string = '';
    let security: string = '';

    /* istanbul ignore else */
    if (this.isOptionalKeyword('AUTHOR', this.currentToken)) {
      this.nextToken();
      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
      if (this.expectIdentifier(this.currentToken)) {
        author = author.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          author = author.concat(' ', this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
        this.nextToken();
      }

      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('INSTALLATION', this.currentToken)) {
      this.nextToken();
      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
      if (this.expectIdentifier(this.currentToken)) {
        installation = installation.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          installation = installation.concat(' ', this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
      }

      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('DATE-WRITTEN', this.currentToken)) {
      this.nextToken();
      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
      if (this.expectIdentifier(this.currentToken)) {
        dateWritten = dateWritten.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          dateWritten = dateWritten.concat(' ', this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
      }

      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('DATE-COMPILED', this.currentToken)) {
      this.nextToken();
      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
      if (this.expectIdentifier(this.currentToken)) {
        dateCompiled = dateCompiled.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          dateCompiled = dateCompiled.concat(' ', this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
      }

      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
    }

    /* istanbul ignore else */
    if (this.isOptionalKeyword('SECURITY', this.currentToken)) {
      this.nextToken();
      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
      if (this.expectIdentifier(this.currentToken)) {
        security = security.concat(this.currentToken.value);
        this.nextToken();
        while (this.currentToken.type === TokenType.Identifier) {
          security = security.concat(' ', this.currentToken.value);
          this.nextToken();
        }
      } else {
        node.setHasError(true);
      }

      /* istanbul ignore else */
      if (this.isOptionalTerminator(this.currentToken)) {
        this.nextToken();
      }
    }

    return this.finalizeNode(
      node,
      new Nodes.IdentificationDivisionContent(
        node.startColumnTotal,
        node.startColumnRelative,
        node.startLine,
        author,
        installation,
        dateWritten,
        dateCompiled,
        security,
      ),
      this.currentToken,
    );
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
    this.nextToken();

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
    let programIdValue: string = '';

    this.nextToken();
    /* istanbul ignore else */
    if (this.isOptionalTerminator(this.currentToken)) {
      this.nextToken();
    }

    if (this.expectIdentifier(this.currentToken)) {
      programIdValue = this.currentToken.value;
    } else {
      startNode.setHasError(true);
    }
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
      new Nodes.ProgramId(
        startNode.startColumnTotal,
        startNode.startColumnRelative,
        startNode.startLine,
        programIdValue,
      ),
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

  expectTerminator(token: Token) {
    if (token.type !== TokenType.Terminator) {
      this.errorHandler.unexpectedTokenError(token, undefined, new Token(undefined, TokenType.Terminator));
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

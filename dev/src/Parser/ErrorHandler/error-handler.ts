import { Token } from '../../Lexer/Token';
import { TokenType } from '../../Lexer/tokentype';
import { Messages } from './error-messages';
import { ParseError } from './parse-error';

export class ErrorHandler {
  readonly errors: ParseError[];

  constructor() {
    this.errors = [];
  }

  private constructError(msg: string, column: number): ParseError {
    let error = new ParseError(msg);
    try {
      throw error;
    } catch (base) {
      /* istanbul ignore else */
      if (Object.create && Object.defineProperty) {
        error = Object.create(base);
        Object.defineProperty(error, 'column', { value: column });
      }
    }
    /* istanbul ignore next */
    return error;
  }

  private createError(index: number, line: number, col: number, description: string): ParseError {
    const msg = 'Line ' + line + ': ' + description;
    const error = this.constructError(msg, col);
    error.index = index;
    error.lineNumber = line;
    error.description = description;
    return error;
  }

  private countError(index: number, line: number, col: number, description: string) {
    const error = this.createError(index, line, col, description);
    this.errors.push(error);
  }

  public unexpectedTokenError(token: Token, message?: string) {
    let msg = message || Messages.UnexpectedToken;

    if (!message) {
      msg =
        token.type === TokenType.EOF
          ? Messages.UnexpectedEOS
          : token.type === TokenType.Identifier
          ? Messages.UnexpectedIdentifier
          : token.type === TokenType.NumericLiteral
          ? Messages.UnexpectedNumber
          : token.type === TokenType.StringLiteral
          ? Messages.UnexpectedString
          : Messages.UnexpectedToken;
    }

    msg = msg.replace('%0', token.value);

    this.countError(token.startColumnTotal, token.startLine, token.endColumnRelative, msg);
  }
}

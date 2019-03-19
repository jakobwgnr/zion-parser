import { Token } from '../../Lexer/Token';
import { ParseError } from './parse-error';

export class ErrorHandler {
  readonly errors: ParseError[];

  constructor() {
    this.errors = [];
  }

  private countError(index: number, line: number, col: number, description: string) {
    const msg = 'Line ' + line + ': ' + description;
    const error = new ParseError(msg, index, line, col, description);
    this.errors.push(error);
  }

  public unexpectedTokenError(token: Token, message?: string) {
    if (!message) {
      message = 'Unexpected Token Type: ' + token.type + '; Value: ' + token.value;
    }

    this.countError(token.startColumnTotal, token.startLine, token.startColumnRelative, message);
  }
}

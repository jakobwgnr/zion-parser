import { Token } from '../../Lexer/Token';
import { ParseError } from './parse-error';

export class ErrorHandler {
  readonly errors: ParseError[];

  constructor() {
    this.errors = [];
  }

  public unexpectedTokenError(receivedtoken: Token, description?: string, expectedToken?: Token) {
    if (!description) {
      description = 'Unexpected Token Type: ' + receivedtoken.type + '; Value: ' + receivedtoken.value;
    }
    const message = 'Line ' + receivedtoken.startLine + ': ' + description;
    const error = new ParseError(
      message,
      receivedtoken.startColumnTotal,
      receivedtoken.startLine,
      receivedtoken.startColumnRelative,
      description,
      receivedtoken.value,
      expectedToken ? expectedToken.value : undefined,
      receivedtoken.type,
      expectedToken ? expectedToken.type : undefined,
    );
    this.errors.push(error);
  }
}

/**
 * @fileoverview Responsible for lexing sourcecode and creating a list of tokens
 * @author Jakob Wagner
 */

// ------------------------------------------------------------------------------
// Requirements
// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------
// Helpers
// ------------------------------------------------------------------------------

// ------------------------------------------------------------------------------
// Public Interface
// ------------------------------------------------------------------------------

export class Lexer {
  private readonly sourcecode: string;

  constructor(sourcecode: string) {
    this.sourcecode = sourcecode;
  }

  public execute() {
    return this.sourcecode;
  }
}

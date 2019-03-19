export class ParseError {
  public message: string;
  public index: number;
  public lineNumber: number;
  public column: number;
  public description: string;
  constructor(message: string, index: number, lineNumber: number, column: number, description: string) {
    this.message = message;
    this.index = index;
    this.lineNumber = lineNumber;
    this.column = column;
    this.description = description;
  }
}

export class ParseError {
  public message: string;
  public index: number;
  public lineNumber: number;
  public column: number;
  public description: string;
  public receivedValue: string;
  public expectedValue: string;
  public receivedType: string;
  public expectedType: string;
  constructor(
    message: string,
    index: number,
    lineNumber: number,
    column: number,
    description: string,
    receivedValue?: string,
    expectedValue?: string,
    receivedType?: string,
    expectedType?: string,
  ) {
    this.message = message;
    this.index = index;
    this.lineNumber = lineNumber;
    this.column = column;
    this.description = description;
    receivedValue ? (this.receivedValue = receivedValue) : (this.receivedValue = '');
    expectedValue ? (this.expectedValue = expectedValue) : (this.expectedValue = '');
    receivedType ? (this.receivedType = receivedType) : (this.receivedType = '');
    expectedType ? (this.expectedType = expectedType) : (this.expectedType = '');
  }
}

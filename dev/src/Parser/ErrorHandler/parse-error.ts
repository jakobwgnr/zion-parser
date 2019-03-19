export declare class ParseError {
  public name: string;
  public message: string;
  public index: number;
  public lineNumber: number;
  public column: number;
  public description: string;
  constructor(message: string);
}

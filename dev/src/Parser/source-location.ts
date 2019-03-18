/* tslint:disable:max-classes-per-file */

export interface Position {
    line: number;
    column: number;
}

export interface SourceLocation {
    start: Position;
    end: Position;
    source?: string;
}
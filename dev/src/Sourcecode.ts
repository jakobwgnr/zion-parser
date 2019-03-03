import { Character } from "./character";

export class SourceCode {
    public currentLine: number = 1;
    public currentColumnRelative: number = 1;
    public columnsTotal: number = 1;

    private code: string = "";
    private index: number = 0;

    constructor(code: string) {
        this.code = code;
    }

    public eof(): boolean {
        return this.index >= this.code.length;
    }

    public getCharAtIndex(index: number): string {
        return this.code.charAt(index);
    }

    public getCurrentChar(): string {
        return this.code.charAt(this.index);
    }

    public NextChar() {
        /* Don't count line separators column*/
        if (!Character.isLineTerminator(this.code.charAt(this.index))) {
            this.columnsTotal++;
            this.currentColumnRelative++;
            this.index++;
        } else {
            this.currentLine++;
            this.currentColumnRelative = 1;

            if (Character.isLineTerminatorSequence(this.code.charAt(this.index), this.code.charAt(this.index + 1))) {
                this.index = this.index + 2;
            } else {
                this.index++;
            }
        }
    }




}
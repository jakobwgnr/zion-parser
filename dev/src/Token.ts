/**
 * @fileoverview A Token produced by the lexer
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

export class Token {
    public value: string = "";
    public type: string = "";
    public startColumnTotal: number = 0;
    public startColumnRelative: number = 0;
    public startLine: number = 0;
    public endColumnTotal: number = 0;
    public endColumnRelative: number = 0;
    public endLine: number = 0;

    public initToken() {
        this.value = "";
        this.type = "";
        this.startColumnTotal = 0;
        this.startColumnRelative = 0;
        this.startLine = 0;
        this.endColumnTotal = 0;
        this.endColumnRelative = 0;
        this.endLine = 0;
    }
}

export const Character = {

    /* tslint:disable:no-bitwise */

    isLineTerminator(char: string): boolean {
        const cp: number = char.charCodeAt(0);
        return (cp === 0x0A) ||
            (cp === 0x0D) ||
            (cp === 0x2028) ||
            (cp === 0x2029);
    },

    isLineTerminatorSequence(char1: string, char2: string): boolean {
        const cp1: number = char1.charCodeAt(0);
        const cp2: number = char2.charCodeAt(0);
        return (cp1 === 0x0D) && (cp2 === 0x0A);
    },

    isCobolWordStart(char: string): boolean {
        const cp: number = char.charCodeAt(0);
        return (cp >= 0x41 && cp <= 0x5A) ||      // A..Z
            (cp >= 0x61 && cp <= 0x7A) ||         // a..z
            (cp >= 0x30 && cp <= 0x39) ||         // 0..9
            (cp === 0x28); // (

    },

    isCobolWordPart(char: string): boolean {
        const cp: number = char.charCodeAt(0);
        return (cp >= 0x41 && cp <= 0x5A) ||      // A..Z
            (cp >= 0x61 && cp <= 0x7A) ||         // a..z
            (cp >= 0x30 && cp <= 0x39) ||         // 0..9
            (cp === 0x2D) ||                      // - (Dash)
            (cp === 0x28) || // (
            (cp === 0x29) || // )
            (cp === 0x3A);   // :

    },

    isLevelIndicator(char: string): boolean {
        if (char.length === 1) {
            const cp1: number = char.charCodeAt(0);
            return (cp1 >= 0x30 && cp1 <= 0x39);   // 0..9
        } else {
            if (char.length === 2) {
                const cp1: number = char.charCodeAt(0);
                const cp2: number = char.charCodeAt(1);
                return ((cp1 >= 0x30 && cp1 <= 0x34)        // 0..4
                    && (cp2 >= 0x30 && cp2 <= 0x39))     // 0..9
                    || (cp1 === 0x36 && cp2 === 0x36)    // Level 66    
                    || (cp1 === 0x37 && cp2 === 0x37)    // Level 77    
                    || (cp1 === 0x38 && cp2 === 0x38);   // Level 88  
            } else {
                return false;
            }
        }
    },

    isNumberIndicator(char: string): boolean {
        const cp: number = char.charCodeAt(0);
        return (cp === 0x2B) || // + (Plus)
            (cp === 0x2D);    // - (Minus)
    },

    isStringIndicator(char: string): boolean {
        const cp: number = char.charCodeAt(0);
        return (cp === 0x27) || // ' (Quote)
            (cp === 0x22);    // " (Double-Quote)
    },

    isOpeningBracket(char: string): boolean {
        const cp: number = char.charCodeAt(0);
        return (cp === 0x28); // ( 
    },

    isClosingBracket(char: string): boolean {
        const cp: number = char.charCodeAt(0);
        return (cp === 0x29); // )
    },

    isDecimalDigit(char: string): boolean {
        const cp: number = char.charCodeAt(0);
        return (cp >= 0x30 && cp <= 0x39);      // 0..9
    },
    isCobolTerminator(char: string): boolean {
        const cp: number = char.charCodeAt(0);
        return cp === 0x2E;      // .
    },

    isNumeric(char: string): boolean {
        return !isNaN(Number(char));
    },

    isCobolAritmeticOperator(char: string): boolean {
        const cp: number = char.charCodeAt(0);
        return (cp === 0x2A) || // *
            (cp === 0x2B) ||    // +
            (cp === 0x2F) ||    // /
            (cp === 0x3D) ||    // =
            (cp === 0x3E) ||    // >
            (cp === 0x3C) ||    // <
            (cp === 0x2D);      // -
    },
    isWhiteSpace(char: string): boolean {
        const cp: number = char.charCodeAt(0);
        return (cp === 0x20) || // SPACE 
            (cp === 0x09) || // <TAB>
            (cp === 0x0B) || // <VT> Line Tabulation
            (cp === 0x0C) || // FORM FEED (FF)
            (cp === 0xA0) || // <NBSP> NO-BREAK SPACE
            // 	ZERO WIDTH NO-BREAK SPACE - U+FEFF & Any other Unicode “Space_Separator” code point	 
            (cp >= 0x1680 && [0x1680, 0x2000, 0x2001, 0x2002, 0x2003, 0x2004, 0x2005, 0x2006, 0x2007, 0x2008, 0x2009, 0x200A, 0x202F, 0x205F, 0x3000, 0xFEFF].indexOf(cp) >= 0);
    }
};

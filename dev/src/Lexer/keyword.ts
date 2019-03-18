

export const Keyword = {

    isKeyword(word: string): boolean {
        const wordUpper: string = word.toUpperCase();
        switch (word.length) {
            case 2: {
                return (wordUpper === 'CD') || (wordUpper === 'CF') || (wordUpper === 'CH') ||
                    (wordUpper === 'DE') || (wordUpper === 'FD') || (wordUpper === 'ID') ||
                    (wordUpper === 'IF') || (wordUpper === 'IN') || (wordUpper === 'OF') ||
                    (wordUpper === 'PF') || (wordUpper === 'PH') || (wordUpper === 'RD') ||
                    (wordUpper === 'SD') || (wordUpper === 'GO') || (wordUpper === 'NO') ||
                    (wordUpper === 'ON') || (wordUpper === 'OR') || (wordUpper === 'UP') ||
                    (wordUpper === 'AT') || (wordUpper === 'BY') || (wordUpper === 'IS') ||
                    (wordUpper === 'RF') || (wordUpper === 'RH') || (wordUpper === 'TO');
            }
            case 3: {
                return (wordUpper === 'ADD') || (wordUpper === 'ALL') || (wordUpper === 'CBL') ||
                    (wordUpper === 'DAY') || (wordUpper === 'EGI') || (wordUpper === 'EMI') ||
                    (wordUpper === 'END') || (wordUpper === 'I-O') || (wordUpper === 'OFF') ||
                    (wordUpper === 'SET') || (wordUpper === 'AND') || (wordUpper === 'ANY') ||
                    (wordUpper === 'KEY') || (wordUpper === 'NOT') || (wordUpper === 'PIC') ||
                    (wordUpper === 'ARE') || (wordUpper === 'EOP') || (wordUpper === 'ESI') ||
                    (wordUpper === 'FOR') || (wordUpper === 'RUN') || (wordUpper === 'SUM') ||
                    (wordUpper === 'TOP') || (wordUpper === 'USE');
            }
            case 4: {
                return (wordUpper === 'CALL') || (wordUpper === 'CODE') || (wordUpper === 'DATA') ||
                    (wordUpper === 'DBCS') || (wordUpper === 'EGCS') || (wordUpper === 'ELSE') ||
                    (wordUpper === 'FILE') || (wordUpper === 'JUST') || (wordUpper === 'LAST') ||
                    (wordUpper === 'LEFT') || (wordUpper === 'LESS') || (wordUpper === 'NEXT') ||
                    (wordUpper === 'PAGE') || (wordUpper === 'READ') || (wordUpper === 'REEL') ||
                    (wordUpper === 'SAME') || (wordUpper === 'SELF') || (wordUpper === 'SEND') ||
                    (wordUpper === 'TAPE') || (wordUpper === 'TEST') || (wordUpper === 'UNIT') ||
                    (wordUpper === 'WHEN') || (wordUpper === 'WITH') || (wordUpper === 'ZERO') ||
                    (wordUpper === 'ALSO') || (wordUpper === 'COMP') || (wordUpper === 'LINE') ||
                    (wordUpper === 'MODE') || (wordUpper === 'NULL') || (wordUpper === 'OPEN') ||
                    (wordUpper === 'PLUS') || (wordUpper === 'SIGN') || (wordUpper === 'SIZE') ||
                    (wordUpper === 'SORT') || (wordUpper === 'TEXT') || (wordUpper === 'THAN') ||
                    (wordUpper === 'THEN') || (wordUpper === 'THRU') || (wordUpper === 'TIME') ||
                    (wordUpper === 'UPON') || (wordUpper === 'AREA') || (wordUpper === 'COPY') ||
                    (wordUpper === 'CORR') || (wordUpper === 'DOWN') || (wordUpper === 'EXIT') ||
                    (wordUpper === 'FROM') || (wordUpper === 'INTO') || (wordUpper === 'LOCK') ||
                    (wordUpper === 'MOVE') || (wordUpper === 'STOP') || (wordUpper === 'SYNC') ||
                    (wordUpper === 'TRUE') || (wordUpper === 'TYPE');
            }
            case 5: {
                return (wordUpper === 'AFTER') || (wordUpper === 'BASIS') || (wordUpper === 'CLASS') ||
                    (wordUpper === 'CLOSE') || (wordUpper === 'COBOL') || (wordUpper === 'EJECT') ||
                    (wordUpper === 'FALSE') || (wordUpper === 'INDEX') || (wordUpper === 'KANJI') ||
                    (wordUpper === 'LABEL') || (wordUpper === 'MERGE') || (wordUpper === 'QUEUE') ||
                    (wordUpper === 'READY') || (wordUpper === 'TABLE') || (wordUpper === 'TALLY') ||
                    (wordUpper === 'UNTIL') || (wordUpper === 'VALUE') || (wordUpper === 'ALTER') ||
                    (wordUpper === 'BLANK') || (wordUpper === 'BLOCK') || (wordUpper === 'COMMA') ||
                    (wordUpper === 'FINAL') || (wordUpper === 'FIRST') || (wordUpper === 'INPUT') ||
                    (wordUpper === 'LIMIT') || (wordUpper === 'NULLS') || (wordUpper === 'ORDER') ||
                    (wordUpper === 'QUOTE') || (wordUpper === 'RERUN') || (wordUpper === 'SKIP1') ||
                    (wordUpper === 'SKIP2') || (wordUpper === 'SKIP3') || (wordUpper === 'SPACE') ||
                    (wordUpper === 'TIMES') || (wordUpper === 'USAGE') || (wordUpper === 'WORDS') ||
                    (wordUpper === 'WRITE') || (wordUpper === 'APPLY') || (wordUpper === 'AREAS') ||
                    (wordUpper === 'COUNT') || (wordUpper === 'ENTER') || (wordUpper === 'ENTRY') ||
                    (wordUpper === 'EQUAL') || (wordUpper === 'ERROR') || (wordUpper === 'EVERY') ||
                    (wordUpper === 'GROUP') || (wordUpper === 'LINES') || (wordUpper === 'OTHER') ||
                    (wordUpper === 'PURGE') || (wordUpper === 'RESET') || (wordUpper === 'RIGHT') ||
                    (wordUpper === 'START') || (wordUpper === 'SUPER') || (wordUpper === 'TITLE') ||
                    (wordUpper === 'TRACE') || (wordUpper === 'USING') || (wordUpper === 'ZEROS');

            } case 6: {
                return (wordUpper === 'ACCEPT') || (wordUpper === 'ACCESS') || (wordUpper === 'BEFORE') ||
                    (wordUpper === 'CANCEL') || (wordUpper === 'ENABLE') || (wordUpper === 'END-IF') ||
                    (wordUpper === 'GIVING') || (wordUpper === 'GLOBAL') || (wordUpper === 'LENGTH') ||
                    (wordUpper === 'MEMORY') || (wordUpper === 'NATIVE') || (wordUpper === 'OBJECT') ||
                    (wordUpper === 'OCCURS') || (wordUpper === 'RANDOM') || (wordUpper === 'RECORD') ||
                    (wordUpper === 'SEARCH') || (wordUpper === 'SELECT') || (wordUpper === 'BINARY') ||
                    (wordUpper === 'COLUMN') || (wordUpper === 'COMMON') || (wordUpper === 'COMP-1') ||
                    (wordUpper === 'COMP-2') || (wordUpper === 'COMP-3') || (wordUpper === 'COMP-4') ||
                    (wordUpper === 'COMP-5') || (wordUpper === 'DELETE') || (wordUpper === 'FILLER') ||
                    (wordUpper === 'GOBACK') || (wordUpper === 'INSERT') || (wordUpper === 'LIMITS') ||
                    (wordUpper === 'LINAGE') || (wordUpper === 'METHOD') || (wordUpper === 'RELOAD') ||
                    (wordUpper === 'REPORT') || (wordUpper === 'SOURCE') || (wordUpper === 'SPACES') ||
                    (wordUpper === 'VALUES') || (wordUpper === 'ZEROES') || (wordUpper === 'ASSIGN') ||
                    (wordUpper === 'AUTHOR') || (wordUpper === 'BOTTOM') || (wordUpper === 'DETAIL') ||
                    (wordUpper === 'DIVIDE') || (wordUpper === 'ENDING') || (wordUpper === 'EXTEND') ||
                    (wordUpper === 'INVOKE') || (wordUpper === 'NUMBER') || (wordUpper === 'OUTPUT') ||
                    (wordUpper === 'QUOTES') || (wordUpper === 'RETURN') || (wordUpper === 'REWIND') ||
                    (wordUpper === 'STATUS') || (wordUpper === 'STRING');

            } case 7: {
                return (wordUpper === 'ADDRESS') || (wordUpper === 'END-ADD') || (wordUpper === 'HEADING') ||
                    (wordUpper === 'INDEXED') || (wordUpper === 'LEADING') || (wordUpper === 'MESSAGE') ||
                    (wordUpper === 'OMITTED') || (wordUpper === 'PADDING') || (wordUpper === 'PERFORM') ||
                    (wordUpper === 'RECEIVE') || (wordUpper === 'RECORDS') || (wordUpper === 'SECTION') ||
                    (wordUpper === 'SEGMENT') || (wordUpper === 'SERVICE') || (wordUpper === 'COM-REG') ||
                    (wordUpper === 'FOOTING') || (wordUpper === 'GREATER') || (wordUpper === 'INITIAL') ||
                    (wordUpper === 'MODULES') || (wordUpper === 'PICTURE') || (wordUpper === 'POINTER') ||
                    (wordUpper === 'RELEASE') || (wordUpper === 'REMOVAL') || (wordUpper === 'RENAMES') ||
                    (wordUpper === 'REPLACE') || (wordUpper === 'REPORTS') || (wordUpper === 'THROUGH') ||
                    (wordUpper === 'COMPUTE') || (wordUpper === 'CONTENT') || (wordUpper === 'CONTROL') ||
                    (wordUpper === 'DISPLAY') || (wordUpper === 'DYNAMIC') || (wordUpper === 'INSPECT') ||
                    (wordUpper === 'INVALID') || (wordUpper === 'LINKAGE') || (wordUpper === 'NUMERIC') ||
                    (wordUpper === 'PROCEED') || (wordUpper === 'PROGRAM') || (wordUpper === 'RESERVE') ||
                    (wordUpper === 'REWRITE') || (wordUpper === 'ROUNDED') || (wordUpper === 'VARYING');
            } case 8: {
                return (wordUpper === 'ALPHABET') || (wordUpper === 'CLASS-ID') || (wordUpper === 'CODE-SET') ||
                    (wordUpper === 'END-CALL') || (wordUpper === 'GENERATE') || (wordUpper === 'NEGATIVE') ||
                    (wordUpper === 'PASSWORD') || (wordUpper === 'SECURITY') || (wordUpper === 'SENTENCE') ||
                    (wordUpper === 'SEPARATE') || (wordUpper === 'SEQUENCE') || (wordUpper === 'SHIFT-IN') ||
                    (wordUpper === 'TALLYING') || (wordUpper === 'TERMINAL') || (wordUpper === 'UNSTRING') ||
                    (wordUpper === 'END-READ') || (wordUpper === 'INDICATE') || (wordUpper === 'INHERITS') ||
                    (wordUpper === 'INITIATE') || (wordUpper === 'OPTIONAL') || (wordUpper === 'POSITION') ||
                    (wordUpper === 'POSITIVE') || (wordUpper === 'PRINTING') || (wordUpper === 'RELATIVE') ||
                    (wordUpper === 'CONTAINS') || (wordUpper === 'CONTINUE') || (wordUpper === 'CONTROLS') ||
                    (wordUpper === 'CURRENCY') || (wordUpper === 'DIVISION') || (wordUpper === 'EVALUATE') ||
                    (wordUpper === 'EXTERNAL') || (wordUpper === 'FUNCTION') || (wordUpper === 'MULTIPLE') ||
                    (wordUpper === 'MULTIPLY') || (wordUpper === 'OVERFLOW') || (wordUpper === 'OVERRIDE') ||
                    (wordUpper === 'REVERSED') || (wordUpper === 'STANDARD') || (wordUpper === 'SUBTRACT') ||
                    (wordUpper === 'SUPPRESS') || (wordUpper === 'SYMBOLIC') || (wordUpper === 'TRAILING');
            } case 9: {
                return (wordUpper === 'ADVANCING') || (wordUpper === 'BEGINNING') || (wordUpper === 'CHARACTER') ||
                    (wordUpper === 'COLLATING') || (wordUpper === 'METACLASS') || (wordUpper === 'RECORDING') ||
                    (wordUpper === 'RECURSIVE') || (wordUpper === 'REDEFINES') || (wordUpper === 'REFERENCE') ||
                    (wordUpper === 'SHIFT-OUT') || (wordUpper === 'TERMINATE') || (wordUpper === 'ALTERNATE') ||
                    (wordUpper === 'DEBUGGING') || (wordUpper === 'DELIMITED') || (wordUpper === 'DELIMITER') ||
                    (wordUpper === 'DEPENDING') || (wordUpper === 'END-START') || (wordUpper === 'END-WRITE') ||
                    (wordUpper === 'JUSTIFIED') || (wordUpper === 'METHOD-ID') || (wordUpper === 'PROCEDURE') ||
                    (wordUpper === 'REMAINDER') || (wordUpper === 'REPLACING') || (wordUpper === 'REPORTING') ||
                    (wordUpper === 'ASCENDING') || (wordUpper === 'DISPLAY-1') || (wordUpper === 'EXCEPTION') ||
                    (wordUpper === 'LOW-VALUE') || (wordUpper === 'RETURNING');
            } case 10: {
                return (wordUpper === 'ALPHABETIC') || (wordUpper === 'CHARACTERS') || (wordUpper === 'DEBUG-ITEM') ||
                    (wordUpper === 'DEBUG-LINE') || (wordUpper === 'DEBUG-NAME') || (wordUpper === 'END-DELETE') ||
                    (wordUpper === 'END-DIVIDE') || (wordUpper === 'REFERENCES') || (wordUpper === 'SEQUENTIAL') ||
                    (wordUpper === 'DESCENDING') || (wordUpper === 'END-INVOKE') || (wordUpper === 'END-RETURN') ||
                    (wordUpper === 'END-SEARCH') || (wordUpper === 'END-STRING') || (wordUpper === 'HIGH-VALUE') ||
                    (wordUpper === 'INITIALIZE') || (wordUpper === 'REPOSITORY') || (wordUpper === 'SORT-MERGE') ||
                    (wordUpper === 'CONVERTING') || (wordUpper === 'DUPLICATES') || (wordUpper === 'LOW-VALUES') ||
                    (wordUpper === 'PROCEDURES') || (wordUpper === 'PROCESSING') || (wordUpper === 'PROGRAM-ID') ||
                    (wordUpper === 'STANDARD-1') || (wordUpper === 'STANDARD-2') || (wordUpper === 'WRITE-ONLY');
            } case 11: {
                return (wordUpper === 'CLOCK-UNITS') || (wordUpper === 'DAY-OF-WEEK') || (wordUpper === 'END-COMPUTE') ||
                    (wordUpper === 'I-O-CONTROL') || (wordUpper === 'DEBUG-SUB-1') || (wordUpper === 'DEBUG-SUB-2') ||
                    (wordUpper === 'DEBUG-SUB-3') || (wordUpper === 'END-OF-PAGE') || (wordUpper === 'END-PERFORM') ||
                    (wordUpper === 'END-RECEIVE') || (wordUpper === 'END-REWRITE') || (wordUpper === 'SORT-RETURN') ||
                    (wordUpper === 'DESTINATION') || (wordUpper === 'ENVIRONMENT') || (wordUpper === 'HIGH-VALUES') ||
                    (wordUpper === 'MORE-LABELS') || (wordUpper === 'RETURN-CODE') || (wordUpper === 'SUB-QUEUE-1') ||
                    (wordUpper === 'SUB-QUEUE-2') || (wordUpper === 'SUB-QUEUE-3');
            } case 12: {
                return (wordUpper === 'DATE-WRITTEN') || (wordUpper === 'END-EVALUATE') || (wordUpper === 'FILE-CONTROL') ||
                    (wordUpper === 'PAGE-COUNTER') || (wordUpper === 'ALPHANUMERIC') || (wordUpper === 'DECLARATIVES') ||
                    (wordUpper === 'END-MULTIPLY') || (wordUpper === 'END-SUBTRACT') || (wordUpper === 'END-UNSTRING') ||
                    (wordUpper === 'INPUT-OUTPUT') || (wordUpper === 'LINE-COUNTER') || (wordUpper === 'ORGANIZATION') ||
                    (wordUpper === 'SORT-CONTROL') || (wordUpper === 'SORT-MESSAGE') || (wordUpper === 'INSTALLATION') ||
                    (wordUpper === 'SYNCHRONIZED');
            } case 13: {
                return (wordUpper === 'DATE-COMPILED') || (wordUpper === 'NATIVE_BINARY') || (wordUpper === 'SEGMENT-LIMIT') ||
                    (wordUpper === 'WHEN-COMPILED') || (wordUpper === 'COMMUNICATION') || (wordUpper === 'COMPUTATIONAL') ||
                    (wordUpper === 'DECIMAL-POINT') || (wordUpper === 'SPECIAL-NAMES') || (wordUpper === 'CONFIGURATION') ||
                    (wordUpper === 'CORRESPONDING') || (wordUpper === 'LOCAL-STORAGE');
            } case 14: {
                return (wordUpper === 'DEBUG-CONTENTS') || (wordUpper === 'IDENTIFICATION') || (wordUpper === 'PACKED-DECIMAL') ||
                    (wordUpper === 'LINAGE-COUNTER') || (wordUpper === 'SORT-CORE-SIZE') || (wordUpper === 'SORT-FILE-SIZE') ||
                    (wordUpper === 'SORT-MODE-SIZE') || (wordUpper === 'NUMERIC-EDITED');
            } case 15: {
                return (wordUpper === 'OBJECT-COMPUTER') || (wordUpper === 'COMPUTATIONAL-1') || (wordUpper === 'COMPUTATIONAL-2') ||
                    (wordUpper === 'COMPUTATIONAL-3') || (wordUpper === 'COMPUTATIONAL-4') || (wordUpper === 'SOURCE-COMPUTER') ||
                    (wordUpper === 'WORKING-STORAGE') || (wordUpper === 'COMPUTATIONAL-5');
            } case 16: {
                return (wordUpper === 'ALPHABETIC-LOWER') || (wordUpper === 'ALPHABETIC-UPPER');
            }
            case 17: {
                return (wordUpper === 'PROCEDURE-POINTER');
            } case 19: {
                return (wordUpper === 'ALPHANUMERIC-EDITED');
            }
            default: {
                return false;
            }
        }
    },

    isExec(word: string): boolean {
        const wordUpper: string = word.toUpperCase();
        return wordUpper === 'EXEC';
    },
    containsEndExec(word: string): boolean {
        const wordUpper: string = word.toUpperCase();
        return wordUpper.includes("END-EXEC");
    }

}
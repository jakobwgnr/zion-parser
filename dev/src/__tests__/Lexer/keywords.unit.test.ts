import { Keyword } from '../../Lexer/keyword';
import { Lexer } from '../../Lexer/Lexer';
import { Token } from '../../Lexer/Token';

// Unit-Test
test('Defaultvalue to be false', () => {
    expect(Keyword.isKeyword("A")).toBeFalsy();
    expect(Keyword.isKeyword("THISWORDISWAYTOLONGTOBEAKEYWORD")).toBeFalsy();
});

test('Keywords with length of 2 identified', () => {
    expect(Keyword.isKeyword("AT")).toBeTruthy();
    expect(Keyword.isKeyword("BY")).toBeTruthy();
    expect(Keyword.isKeyword("FD")).toBeTruthy();
    expect(Keyword.isKeyword("GO")).toBeTruthy();
    expect(Keyword.isKeyword("ID")).toBeTruthy();
    expect(Keyword.isKeyword("IF")).toBeTruthy();
    expect(Keyword.isKeyword("IN")).toBeTruthy();
    expect(Keyword.isKeyword("IS")).toBeTruthy();
    expect(Keyword.isKeyword("NO")).toBeTruthy();
    expect(Keyword.isKeyword("OF")).toBeTruthy();
    expect(Keyword.isKeyword("ON")).toBeTruthy();
    expect(Keyword.isKeyword("OR")).toBeTruthy();
    expect(Keyword.isKeyword("SD")).toBeTruthy();
    expect(Keyword.isKeyword("TO")).toBeTruthy();
    expect(Keyword.isKeyword("UP")).toBeTruthy();
});

test('Keywords with length of 3 identified', () => {
    expect(Keyword.isKeyword("ADD")).toBeTruthy();
    expect(Keyword.isKeyword("ALL")).toBeTruthy();
    expect(Keyword.isKeyword("AND")).toBeTruthy();
    expect(Keyword.isKeyword("ANY")).toBeTruthy();
    expect(Keyword.isKeyword("ARE")).toBeTruthy();
    expect(Keyword.isKeyword("CBL")).toBeTruthy();
    expect(Keyword.isKeyword("DAY")).toBeTruthy();
    expect(Keyword.isKeyword("END")).toBeTruthy();
    expect(Keyword.isKeyword("EOP")).toBeTruthy();
    expect(Keyword.isKeyword("FOR")).toBeTruthy();
    expect(Keyword.isKeyword("I-O")).toBeTruthy();
    expect(Keyword.isKeyword("KEY")).toBeTruthy();
    expect(Keyword.isKeyword("MAP")).toBeTruthy();
    expect(Keyword.isKeyword("NOT")).toBeTruthy();
    expect(Keyword.isKeyword("OFF")).toBeTruthy();
    expect(Keyword.isKeyword("PIC")).toBeTruthy();
    expect(Keyword.isKeyword("RUN")).toBeTruthy();
    expect(Keyword.isKeyword("SET")).toBeTruthy();
    expect(Keyword.isKeyword("TOP")).toBeTruthy();
    expect(Keyword.isKeyword("USE")).toBeTruthy();

});

test('Keywords with length of 4 identified', () => {
    expect(Keyword.isKeyword("ALSO")).toBeTruthy();
    expect(Keyword.isKeyword("AREA")).toBeTruthy();
    expect(Keyword.isKeyword("CALL")).toBeTruthy();
    expect(Keyword.isKeyword("COMP")).toBeTruthy();
    expect(Keyword.isKeyword("COPY")).toBeTruthy();
    expect(Keyword.isKeyword("CORR")).toBeTruthy();
    expect(Keyword.isKeyword("DATA")).toBeTruthy();
    expect(Keyword.isKeyword("DATE")).toBeTruthy();
    expect(Keyword.isKeyword("DBCS")).toBeTruthy();
    expect(Keyword.isKeyword("DOWN")).toBeTruthy();
    expect(Keyword.isKeyword("EGCS")).toBeTruthy();
    expect(Keyword.isKeyword("ELSE")).toBeTruthy();
    expect(Keyword.isKeyword("EXIT")).toBeTruthy();
    expect(Keyword.isKeyword("FILE")).toBeTruthy();
    expect(Keyword.isKeyword("FROM")).toBeTruthy();
    expect(Keyword.isKeyword("INTO")).toBeTruthy();
    expect(Keyword.isKeyword("JUST")).toBeTruthy();
    expect(Keyword.isKeyword("LEFT")).toBeTruthy();
    expect(Keyword.isKeyword("LESS")).toBeTruthy();
    expect(Keyword.isKeyword("LINE")).toBeTruthy();
    expect(Keyword.isKeyword("LIST")).toBeTruthy();
    expect(Keyword.isKeyword("LOCK")).toBeTruthy();
    expect(Keyword.isKeyword("MODE")).toBeTruthy();
    expect(Keyword.isKeyword("MOVE")).toBeTruthy();
    expect(Keyword.isKeyword("NEXT")).toBeTruthy();
    expect(Keyword.isKeyword("NULL")).toBeTruthy();
    expect(Keyword.isKeyword("OPEN")).toBeTruthy();
    expect(Keyword.isKeyword("PAGE")).toBeTruthy();
    expect(Keyword.isKeyword("READ")).toBeTruthy();
    expect(Keyword.isKeyword("REEL")).toBeTruthy();
    expect(Keyword.isKeyword("SAME")).toBeTruthy();
    expect(Keyword.isKeyword("SIGN")).toBeTruthy();
    expect(Keyword.isKeyword("SIZE")).toBeTruthy();
    expect(Keyword.isKeyword("SORT")).toBeTruthy();
    expect(Keyword.isKeyword("STOP")).toBeTruthy();
    expect(Keyword.isKeyword("SYNC")).toBeTruthy();
    expect(Keyword.isKeyword("TAPE")).toBeTruthy();
    expect(Keyword.isKeyword("TEST")).toBeTruthy();
    expect(Keyword.isKeyword("THAN")).toBeTruthy();
    expect(Keyword.isKeyword("THEN")).toBeTruthy();
    expect(Keyword.isKeyword("THRU")).toBeTruthy();
    expect(Keyword.isKeyword("TIME")).toBeTruthy();
    expect(Keyword.isKeyword("UNIT")).toBeTruthy();
    expect(Keyword.isKeyword("UPON")).toBeTruthy();
    expect(Keyword.isKeyword("WHEN")).toBeTruthy();
    expect(Keyword.isKeyword("WITH")).toBeTruthy();
    expect(Keyword.isKeyword("ZERO")).toBeTruthy();
    expect(Keyword.isKeyword("TRUE")).toBeTruthy();

});

test('Keywords with length of 5 identified', () => {
    expect(Keyword.isKeyword("AFTER")).toBeTruthy();
    expect(Keyword.isKeyword("ALTER")).toBeTruthy();
    expect(Keyword.isKeyword("APPLY")).toBeTruthy();
    expect(Keyword.isKeyword("AREAS")).toBeTruthy();
    expect(Keyword.isKeyword("BASIS")).toBeTruthy();
    expect(Keyword.isKeyword("BLANK")).toBeTruthy();
    expect(Keyword.isKeyword("BLOCK")).toBeTruthy();
    expect(Keyword.isKeyword("CLASS")).toBeTruthy();
    expect(Keyword.isKeyword("CLOSE")).toBeTruthy();
    expect(Keyword.isKeyword("COMMA")).toBeTruthy();
    expect(Keyword.isKeyword("COUNT")).toBeTruthy();
    expect(Keyword.isKeyword("EJECT")).toBeTruthy();
    expect(Keyword.isKeyword("ENTER")).toBeTruthy();
    expect(Keyword.isKeyword("ENTRY")).toBeTruthy();
    expect(Keyword.isKeyword("EQUAL")).toBeTruthy();
    expect(Keyword.isKeyword("ERROR")).toBeTruthy();
    expect(Keyword.isKeyword("EVERY")).toBeTruthy();
    expect(Keyword.isKeyword("FIRST")).toBeTruthy();
    expect(Keyword.isKeyword("INDEX")).toBeTruthy();
    expect(Keyword.isKeyword("INPUT")).toBeTruthy();
    expect(Keyword.isKeyword("KANJI")).toBeTruthy();
    expect(Keyword.isKeyword("LABEL")).toBeTruthy();
    expect(Keyword.isKeyword("LINES")).toBeTruthy();
    expect(Keyword.isKeyword("MERGE")).toBeTruthy();
    expect(Keyword.isKeyword("NOMAP")).toBeTruthy();
    expect(Keyword.isKeyword("NULLS")).toBeTruthy();
    expect(Keyword.isKeyword("ORDER")).toBeTruthy();
    expect(Keyword.isKeyword("OTHER")).toBeTruthy();
    expect(Keyword.isKeyword("QUOTE")).toBeTruthy();
    expect(Keyword.isKeyword("READY")).toBeTruthy();
    expect(Keyword.isKeyword("RERUN")).toBeTruthy();
    expect(Keyword.isKeyword("RESET")).toBeTruthy();
    expect(Keyword.isKeyword("RIGHT")).toBeTruthy();
    expect(Keyword.isKeyword("SKIP1")).toBeTruthy();
    expect(Keyword.isKeyword("SKIP2")).toBeTruthy();
    expect(Keyword.isKeyword("SKIP3")).toBeTruthy();
    expect(Keyword.isKeyword("SPACE")).toBeTruthy();
    expect(Keyword.isKeyword("START")).toBeTruthy();
    expect(Keyword.isKeyword("TALLY")).toBeTruthy();
    expect(Keyword.isKeyword("TIMES")).toBeTruthy();
    expect(Keyword.isKeyword("TITLE")).toBeTruthy();
    expect(Keyword.isKeyword("TRACE")).toBeTruthy();
    expect(Keyword.isKeyword("UNTIL")).toBeTruthy();
    expect(Keyword.isKeyword("USAGE")).toBeTruthy();
    expect(Keyword.isKeyword("USING")).toBeTruthy();
    expect(Keyword.isKeyword("VALUE")).toBeTruthy();
    expect(Keyword.isKeyword("WORDS")).toBeTruthy();
    expect(Keyword.isKeyword("WRITE")).toBeTruthy();
    expect(Keyword.isKeyword("ZEROS")).toBeTruthy();
    expect(Keyword.isKeyword("FALSE")).toBeTruthy();

});

test('Keywords with length of 6 identified', () => {
    expect(Keyword.isKeyword("ACCEPT")).toBeTruthy();
    expect(Keyword.isKeyword("ACCESS")).toBeTruthy();
    expect(Keyword.isKeyword("ASSIGN")).toBeTruthy();
    expect(Keyword.isKeyword("AUTHOR")).toBeTruthy();
    expect(Keyword.isKeyword("BEFORE")).toBeTruthy();
    expect(Keyword.isKeyword("BINARY")).toBeTruthy();
    expect(Keyword.isKeyword("BOTTOM")).toBeTruthy();
    expect(Keyword.isKeyword("CANCEL")).toBeTruthy();
    expect(Keyword.isKeyword("COMMON")).toBeTruthy();
    expect(Keyword.isKeyword("COMP-1")).toBeTruthy();
    expect(Keyword.isKeyword("COMP-2")).toBeTruthy();
    expect(Keyword.isKeyword("COMP-3")).toBeTruthy();
    expect(Keyword.isKeyword("COMP-4")).toBeTruthy();
    expect(Keyword.isKeyword("DELETE")).toBeTruthy();
    expect(Keyword.isKeyword("DIVIDE")).toBeTruthy();
    expect(Keyword.isKeyword("EBCDIC")).toBeTruthy();
    expect(Keyword.isKeyword("END-IF")).toBeTruthy();
    expect(Keyword.isKeyword("ENDING")).toBeTruthy();
    expect(Keyword.isKeyword("EXTEND")).toBeTruthy();
    expect(Keyword.isKeyword("FILLER")).toBeTruthy();
    expect(Keyword.isKeyword("GIVING")).toBeTruthy();
    expect(Keyword.isKeyword("GLOBAL")).toBeTruthy();
    expect(Keyword.isKeyword("GOBACK")).toBeTruthy();
    expect(Keyword.isKeyword("INSERT")).toBeTruthy();
    expect(Keyword.isKeyword("LENGTH")).toBeTruthy();
    expect(Keyword.isKeyword("LINAGE")).toBeTruthy();
    expect(Keyword.isKeyword("MEMORY")).toBeTruthy();
    expect(Keyword.isKeyword("NATIVE")).toBeTruthy();
    expect(Keyword.isKeyword("NOLIST")).toBeTruthy();
    expect(Keyword.isKeyword("OCCURS")).toBeTruthy();
    expect(Keyword.isKeyword("OUTPUT")).toBeTruthy();
    expect(Keyword.isKeyword("QUOTES")).toBeTruthy();
    expect(Keyword.isKeyword("RANDOM")).toBeTruthy();
    expect(Keyword.isKeyword("RECORD")).toBeTruthy();
    expect(Keyword.isKeyword("RELOAD")).toBeTruthy();
    expect(Keyword.isKeyword("RETURN")).toBeTruthy();
    expect(Keyword.isKeyword("REWIND")).toBeTruthy();
    expect(Keyword.isKeyword("SEARCH")).toBeTruthy();
    expect(Keyword.isKeyword("SELECT")).toBeTruthy();
    expect(Keyword.isKeyword("SOURCE")).toBeTruthy();
    expect(Keyword.isKeyword("SPACES")).toBeTruthy();
    expect(Keyword.isKeyword("STATUS")).toBeTruthy();
    expect(Keyword.isKeyword("STRING")).toBeTruthy();
    expect(Keyword.isKeyword("VALUES")).toBeTruthy();
    expect(Keyword.isKeyword("ZEROES")).toBeTruthy();

});

test('Keywords with length of 7 identified', () => {
    expect(Keyword.isKeyword("ADDRESS")).toBeTruthy();
    expect(Keyword.isKeyword("COMPUTE")).toBeTruthy();
    expect(Keyword.isKeyword("CONTENT")).toBeTruthy();
    expect(Keyword.isKeyword("DISPLAY")).toBeTruthy();
    expect(Keyword.isKeyword("DYNAMIC")).toBeTruthy();
    expect(Keyword.isKeyword("END-ADD")).toBeTruthy();
    expect(Keyword.isKeyword("FOOTING")).toBeTruthy();
    expect(Keyword.isKeyword("GREATER")).toBeTruthy();
    expect(Keyword.isKeyword("INDEXED")).toBeTruthy();
    expect(Keyword.isKeyword("INITIAL")).toBeTruthy();
    expect(Keyword.isKeyword("INSPECT")).toBeTruthy();
    expect(Keyword.isKeyword("INVALID")).toBeTruthy();
    expect(Keyword.isKeyword("LEADING")).toBeTruthy();
    expect(Keyword.isKeyword("LINKAGE")).toBeTruthy();
    expect(Keyword.isKeyword("MODULES")).toBeTruthy();
    expect(Keyword.isKeyword("NUMERIC")).toBeTruthy();
    expect(Keyword.isKeyword("OMITTED")).toBeTruthy();
    expect(Keyword.isKeyword("PADDING")).toBeTruthy();
    expect(Keyword.isKeyword("PERFORM")).toBeTruthy();
    expect(Keyword.isKeyword("PICTURE")).toBeTruthy();
    expect(Keyword.isKeyword("POINTER")).toBeTruthy();
    expect(Keyword.isKeyword("PROCEED")).toBeTruthy();
    expect(Keyword.isKeyword("PROCESS")).toBeTruthy();
    expect(Keyword.isKeyword("PROGRAM")).toBeTruthy();
    expect(Keyword.isKeyword("RECORDS")).toBeTruthy();
    expect(Keyword.isKeyword("RELEASE")).toBeTruthy();
    expect(Keyword.isKeyword("REMOVAL")).toBeTruthy();
    expect(Keyword.isKeyword("RENAMES")).toBeTruthy();
    expect(Keyword.isKeyword("REPLACE")).toBeTruthy();
    expect(Keyword.isKeyword("RESERVE")).toBeTruthy();
    expect(Keyword.isKeyword("REWRITE")).toBeTruthy();
    expect(Keyword.isKeyword("ROUNDED")).toBeTruthy();
    expect(Keyword.isKeyword("SECTION")).toBeTruthy();
    expect(Keyword.isKeyword("SERVICE")).toBeTruthy();
    expect(Keyword.isKeyword("THROUGH")).toBeTruthy();
    expect(Keyword.isKeyword("VARYING")).toBeTruthy();

});

test('Keywords with length of 8 identified', () => {
    expect(Keyword.isKeyword("ALPHABET")).toBeTruthy();
    expect(Keyword.isKeyword("CODE-SET")).toBeTruthy();
    expect(Keyword.isKeyword("CONTAINS")).toBeTruthy();
    expect(Keyword.isKeyword("CONTINUE")).toBeTruthy();
    expect(Keyword.isKeyword("CURRENCY")).toBeTruthy();
    expect(Keyword.isKeyword("DIVISION")).toBeTruthy();
    expect(Keyword.isKeyword("END-CALL")).toBeTruthy();
    expect(Keyword.isKeyword("END-READ")).toBeTruthy();
    expect(Keyword.isKeyword("EVALUATE")).toBeTruthy();
    expect(Keyword.isKeyword("EXTERNAL")).toBeTruthy();
    expect(Keyword.isKeyword("MULTIPLE")).toBeTruthy();
    expect(Keyword.isKeyword("MULTIPLY")).toBeTruthy();
    expect(Keyword.isKeyword("NEGATIVE")).toBeTruthy();
    expect(Keyword.isKeyword("NOSOURCE")).toBeTruthy();
    expect(Keyword.isKeyword("OPTIONAL")).toBeTruthy();
    expect(Keyword.isKeyword("OVERFLOW")).toBeTruthy();
    expect(Keyword.isKeyword("PASSWORD")).toBeTruthy();
    expect(Keyword.isKeyword("POSITION")).toBeTruthy();
    expect(Keyword.isKeyword("POSITIVE")).toBeTruthy();
    expect(Keyword.isKeyword("RELATIVE")).toBeTruthy();
    expect(Keyword.isKeyword("REVERSED")).toBeTruthy();
    expect(Keyword.isKeyword("SECURITY")).toBeTruthy();
    expect(Keyword.isKeyword("SENTENCE")).toBeTruthy();
    expect(Keyword.isKeyword("SEPARATE")).toBeTruthy();
    expect(Keyword.isKeyword("SEQUENCE")).toBeTruthy();
    expect(Keyword.isKeyword("SHIFT-IN")).toBeTruthy();
    expect(Keyword.isKeyword("STANDARD")).toBeTruthy();
    expect(Keyword.isKeyword("SUBTRACT")).toBeTruthy();
    expect(Keyword.isKeyword("SUPPRESS")).toBeTruthy();
    expect(Keyword.isKeyword("SYMBOLIC")).toBeTruthy();
    expect(Keyword.isKeyword("TALLYING")).toBeTruthy();
    expect(Keyword.isKeyword("TRAILING")).toBeTruthy();
    expect(Keyword.isKeyword("UNSTRING")).toBeTruthy();

});

test('Keywords with length of 9 identified', () => {
    expect(Keyword.isKeyword("ADVANCING")).toBeTruthy();
    expect(Keyword.isKeyword("ALTERNATE")).toBeTruthy();
    expect(Keyword.isKeyword("ASCENDING")).toBeTruthy();
    expect(Keyword.isKeyword("BEGINNING")).toBeTruthy();
    expect(Keyword.isKeyword("CHARACTER")).toBeTruthy();
    expect(Keyword.isKeyword("COLLATING")).toBeTruthy();
    expect(Keyword.isKeyword("DEBUGGING")).toBeTruthy();
    expect(Keyword.isKeyword("DELIMITED")).toBeTruthy();
    expect(Keyword.isKeyword("DELIMITER")).toBeTruthy();
    expect(Keyword.isKeyword("DEPENDING")).toBeTruthy();
    expect(Keyword.isKeyword("DISPLAY-1")).toBeTruthy();
    expect(Keyword.isKeyword("END-START")).toBeTruthy();
    expect(Keyword.isKeyword("END-WRITE")).toBeTruthy();
    expect(Keyword.isKeyword("EXCEPTION")).toBeTruthy();
    expect(Keyword.isKeyword("JUSTIFIED")).toBeTruthy();
    expect(Keyword.isKeyword("LOW-VALUE")).toBeTruthy();
    expect(Keyword.isKeyword("PROCEDURE")).toBeTruthy();
    expect(Keyword.isKeyword("RECORDING")).toBeTruthy();
    expect(Keyword.isKeyword("REDEFINES")).toBeTruthy();
    expect(Keyword.isKeyword("REFERENCE")).toBeTruthy();
    expect(Keyword.isKeyword("REMAINDER")).toBeTruthy();
    expect(Keyword.isKeyword("REPLACING")).toBeTruthy();
    expect(Keyword.isKeyword("SHIFT-OUT")).toBeTruthy();
    expect(Keyword.isKeyword("RETURNING")).toBeTruthy();

});

test('Keywords with length of 10 identified', () => {
    expect(Keyword.isKeyword("ALPHABETIC")).toBeTruthy();
    expect(Keyword.isKeyword("CHARACTERS")).toBeTruthy();
    expect(Keyword.isKeyword("CONVERTING")).toBeTruthy();
    expect(Keyword.isKeyword("DEBUG-ITEM")).toBeTruthy();
    expect(Keyword.isKeyword("DESCENDING")).toBeTruthy();
    expect(Keyword.isKeyword("DUPLICATES")).toBeTruthy();
    expect(Keyword.isKeyword("END-DELETE")).toBeTruthy();
    expect(Keyword.isKeyword("END-DIVIDE")).toBeTruthy();
    expect(Keyword.isKeyword("END-RETURN")).toBeTruthy();
    expect(Keyword.isKeyword("END-SEARCH")).toBeTruthy();
    expect(Keyword.isKeyword("END-STRING")).toBeTruthy();
    expect(Keyword.isKeyword("HIGH-VALUE")).toBeTruthy();
    expect(Keyword.isKeyword("INITIALIZE")).toBeTruthy();
    expect(Keyword.isKeyword("LOW-VALUES")).toBeTruthy();
    expect(Keyword.isKeyword("PROCEDURES")).toBeTruthy();
    expect(Keyword.isKeyword("PROGRAM-ID")).toBeTruthy();
    expect(Keyword.isKeyword("SEQUENTIAL")).toBeTruthy();
    expect(Keyword.isKeyword("SORT-MERGE")).toBeTruthy();
    expect(Keyword.isKeyword("STANDARD-1")).toBeTruthy();
    expect(Keyword.isKeyword("STANDARD-2")).toBeTruthy();
    expect(Keyword.isKeyword("WRITE-ONLY")).toBeTruthy();

});

test('Keywords with length of 11 identified', () => {
    expect(Keyword.isKeyword("CLOCK-UNITS")).toBeTruthy();
    expect(Keyword.isKeyword("DAY-OF-WEEK")).toBeTruthy();
    expect(Keyword.isKeyword("END-COMPUTE")).toBeTruthy();
    expect(Keyword.isKeyword("END-OF-PAGE")).toBeTruthy();
    expect(Keyword.isKeyword("END-PERFORM")).toBeTruthy();
    expect(Keyword.isKeyword("END-REWRITE")).toBeTruthy();
    expect(Keyword.isKeyword("ENVIRONMENT")).toBeTruthy();
    expect(Keyword.isKeyword("HIGH-VALUES")).toBeTruthy();
    expect(Keyword.isKeyword("I-O-CONTROL")).toBeTruthy();
    expect(Keyword.isKeyword("MORE-LABELS")).toBeTruthy();
    expect(Keyword.isKeyword("RETURN-CODE")).toBeTruthy();
    expect(Keyword.isKeyword("SORT-RETURN")).toBeTruthy();
    expect(Keyword.isKeyword("SUB-QUEUE-1")).toBeTruthy();
    expect(Keyword.isKeyword("SUB-QUEUE-2")).toBeTruthy();
    expect(Keyword.isKeyword("SUB-QUEUE-3")).toBeTruthy();
});

test('Keywords with length of 12 identified', () => {
    expect(Keyword.isKeyword("ALPHANUMERIC")).toBeTruthy();
    expect(Keyword.isKeyword("DATE-WRITTEN")).toBeTruthy();
    expect(Keyword.isKeyword("DECLARATIVES")).toBeTruthy();
    expect(Keyword.isKeyword("END-EVALUATE")).toBeTruthy();
    expect(Keyword.isKeyword("END-MULTIPLY")).toBeTruthy();
    expect(Keyword.isKeyword("END-SUBTRACT")).toBeTruthy();
    expect(Keyword.isKeyword("END-UNSTRING")).toBeTruthy();
    expect(Keyword.isKeyword("FILE-CONTROL")).toBeTruthy();
    expect(Keyword.isKeyword("INPUT-OUTPUT")).toBeTruthy();
    expect(Keyword.isKeyword("INSTALLATION")).toBeTruthy();
    expect(Keyword.isKeyword("ORGANIZATION")).toBeTruthy();
    expect(Keyword.isKeyword("SORT-CONTROL")).toBeTruthy();
    expect(Keyword.isKeyword("SORT-MESSAGE")).toBeTruthy();
    expect(Keyword.isKeyword("SYNCHRONIZED")).toBeTruthy();

});

test('Keywords with length of 13 identified', () => {
    expect(Keyword.isKeyword("COMPUTATIONAL")).toBeTruthy();
    expect(Keyword.isKeyword("CONFIGURATION")).toBeTruthy();
    expect(Keyword.isKeyword("CORRESPONDING")).toBeTruthy();
    expect(Keyword.isKeyword("DATE-COMPILED")).toBeTruthy();
    expect(Keyword.isKeyword("DECIMAL-POINT")).toBeTruthy();
    expect(Keyword.isKeyword("SEGMENT-LIMIT")).toBeTruthy();
    expect(Keyword.isKeyword("SPECIAL-NAMES")).toBeTruthy();
    expect(Keyword.isKeyword("WHEN-COMPILED")).toBeTruthy();
    expect(Keyword.isKeyword("LOCAL-STORAGE")).toBeTruthy();

});

test('Keywords with length of 14 identified', () => {
    expect(Keyword.isKeyword("IDENTIFICATION")).toBeTruthy();
    expect(Keyword.isKeyword("LINAGE-COUNTER")).toBeTruthy();
    expect(Keyword.isKeyword("NUMERIC-EDITED")).toBeTruthy();
    expect(Keyword.isKeyword("PACKED-DECIMAL")).toBeTruthy();
    expect(Keyword.isKeyword("SORT-CORE-SIZE")).toBeTruthy();
    expect(Keyword.isKeyword("SORT-FILE-SIZE")).toBeTruthy();
    expect(Keyword.isKeyword("SORT-MODE-SIZE")).toBeTruthy();

});

test('Keywords with length of 15 identified', () => {
    expect(Keyword.isKeyword("COMPUTATIONAL-1")).toBeTruthy();
    expect(Keyword.isKeyword("COMPUTATIONAL-2")).toBeTruthy();
    expect(Keyword.isKeyword("COMPUTATIONAL-3")).toBeTruthy();
    expect(Keyword.isKeyword("COMPUTATIONAL-4")).toBeTruthy();
    expect(Keyword.isKeyword("OBJECT-COMPUTER")).toBeTruthy();
    expect(Keyword.isKeyword("SOURCE-COMPUTER")).toBeTruthy();
    expect(Keyword.isKeyword("WORKING-STORAGE")).toBeTruthy();

});

test('Keywords with length of 16 identified', () => {
    expect(Keyword.isKeyword("ALPHABETIC-LOWER")).toBeTruthy();
    expect(Keyword.isKeyword("ALPHABETIC-UPPER")).toBeTruthy();
});

test('Keywords with length of 17 identified', () => {
    expect(Keyword.isKeyword("PROCEDURE-POINTER")).toBeTruthy();
});

test('Keywords with length of 19 identified', () => {
    expect(Keyword.isKeyword("ALPHANUMERIC-EDITED")).toBeTruthy();
});

test('Keyword to identify EXEC', () => {
    expect(Keyword.isExec("EXEC")).toBeTruthy();
    expect(Keyword.isExec("NOTEXEC")).toBeFalsy();
});

test('Keyword to identify END-EXEC', () => {
    expect(Keyword.containsEndExec("Test END-EXEC")).toBeTruthy();
    expect(Keyword.containsEndExec("END-EXEC")).toBeTruthy();
    expect(Keyword.containsEndExec("END-EXEC Test")).toBeTruthy();
    expect(Keyword.containsEndExec("EXEC NOT END EXEC")).toBeFalsy();
});

// Integration
test('Check if Lexer identifies the IDENTIFICATION DIVISION Keywords', () => {
    const lexer = new Lexer('        IDENTIFICATION DIVISION');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[1].type).toBe('Keyword');
    expect(tokenList[1].value).toBe('IDENTIFICATION');

    expect(tokenList[3].type).toBe('Keyword');
    expect(tokenList[3].value).toBe('DIVISION');
});

test('Check if Lexer can destinguish between keyword and value', () => {
    const lexer = new Lexer('       PROGRAM-ID.    PRGNAME.');

    const tokenList: Token[] = lexer.execute();
    const keywordToken: Token = tokenList.find(token => token.type === "Keyword") as Token;
    expect(keywordToken.type).toBe("Keyword");
    expect(keywordToken.value).toBe('PROGRAM-ID');

    const identifierToken: Token = tokenList.find(token => token.type === "Identifier") as Token;
    expect(identifierToken.type).toBe("Identifier");
    expect(identifierToken.value).toBe('PRGNAME');
});

test('Check if Lexer identifies EXEC', () => {
    const lexer = new Lexer('           EXEC CICS\n               LINK PROGRAM(TESTPROGRAM)\n           END-EXEC');

    const tokenList: Token[] = lexer.execute();
    expect(tokenList[1].type).toBe("EXEC");
    expect(tokenList[1].value).toBe('EXEC CICS\n               LINK PROGRAM(TESTPROGRAM)\n           END-EXEC');
    expect(tokenList[1].startLine).toBe(1);
    expect(tokenList[1].endLine).toBe(3);
});



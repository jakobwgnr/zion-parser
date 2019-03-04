      * QC1CDPL:  ENTRY PROGRAM IN ALNOVA FOR DPL INVOCATION.          *
      ******************************************************************
      *                  IDENTIFICATION DIVISION                       *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.    QC1CDPL.
      *
       AUTHOR.        ALNOVA TECHNOLOGIES CORPORATION.
      *
       DATE-WRITTEN.  2001-01-31.
      *
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *                                                                *
      *** T#132561     22.04.2011   G7   BEGIN  (KOENIG M.)      R11.2 *
      *  DESCRIPTION: DYNAMIC TERMINAL ALLOCATION                      *
      *  MOD. MARKER: @T#132561/22.04.2011                             *
      *** T#132561     22.04.2011   G7   END                           *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
      *** P#73622      10.06.2011   G7   BEGIN  (KOENIG M.)      R11.3 *
      *  DESCRIPTION: CICS JOURNAL FOR SELF SERVICE PLATFORM           *
      *  MOD. MARKER: @P#73622/10.06.2011                              *
      *** P#73622      10.06.2011   G7   END                           *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
      *** DEM#1069     21.06.2017   G7   BEGIN  (KOENIG M.)      R17.4 *
      *  DESCRIPTION: - Run terminalpool in CICSPlex environment       *
      *               - Deactivate CICS journal for SSP                *
      *  MOD. MARKER: @DEM#1069/21.06.2017                             *
      *** DEM#1069     21.06.2017   G7   END                           *
      ******************************************************************
      *                     ENVIRONMENT DIVISION                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SOURCE-COMPUTER.
      *
           IBM-3090.
       OBJECT-COMPUTER.
      *
           IBM-3090.
       SPECIAL-NAMES.
      *
           DECIMAL-POINT IS COMMA.
      *
      ******************************************************************
      *                       DATA DIVISION                            *
      ******************************************************************
       DATA DIVISION.
      *
      ******************************************************************
      *                  WORKING-STORAGE SECTION                       *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
       01 VA-QGECABC-01.
        COPY QGECABC.
      *
       01 VA-QAECCR2-01.
          COPY QAECCR2.
      *
       01 VA-QAECCR3-01.
          COPY QAECCR3.
      *
          COPY QACCCOEM.
      *
          COPY QAECCOR1.
      *
       COPY QAWCCO2C.
      *
       COPY QGECTUT.
      *
      *--MC.S @T#132561/22.04.2011
      ******************************************************************
      *    COMMAREA  PROGRAM  QG2CTP00                                 *
      *       DYNAMIC TERMINAl ALLOCATION                              *
      ******************************************************************
       01  VA-QGECTP00-01.
           COPY QGECTP00.

       01 VA-OC-MAP.
          05 FILLER                        PIC X(04)  VALUE '<OC>'.
          05 FILLER                        PIC X(02)  VALUE 'B1'.
          05 FILLER                        PIC X(08)  VALUE 'QGMTP01'.
             COPY QGNCTP01.
          05 FILLER                        PIC X(05)  VALUE '</OC>'.

       01 VA-AD-DTA01.
          05 FILLER                        PIC X(04)  VALUE '<AD>'.
          05 FILLER                        PIC X(05)  VALUE 'DTA01'.
          05 VA-DTA01-LOG-TRM              PIC X(04).
          05 VA-DTA01-ACC-TRM              PIC X(04).
          05 FILLER                        PIC X(05)  VALUE '</AD>'.

       01 VA-AV-MAP.
         05 FILLER                        PIC X(04)  VALUE '<AV>'.
          05 FILLER                        PIC X(07)  VALUE 'QGA0030'.
          05 FILLER                        PIC X(06)  VALUE '0</AV>'.

       01 VA-ER-MAP.
          05 FILLER                        PIC X(04)  VALUE '<ER>'.
          05 FILLER                        PIC X(07)  VALUE 'QGE0215'.
          05 FILLER                        PIC X(01)  VALUE '2'.
          05 FILLER                        PIC X(20)  VALUE 'QG2CTP00'.
          05 VA-ERR-MAP-VAR2               PIC X(20)  VALUE  SPACES.
          05 FILLER                        PIC X(05)  VALUE '</ER>'.
      *--MC.E @T#132561/22.04.2011

      *--MC.S @DEM#1069/21.06.2017
      *--MC.S @P#73622/10.06.2011
      *01 VA-JOURNAL.
      *** 05  VA-JOURNALNAME               PIC X(08).
      ***     88  VA-JOURNALNAME-SBPLOG               VALUE  'SBPLOG'.
      ***
      *** 05  VA-JOU-PREFIX.
      ***     10  FILLER                   PIC X(05)  VALUE  'TASK#'.
      ***     10  VN-JOU-PREFIX-TASK       PIC 9(07).
      ***     10  FILLER                   PIC X(03)  VALUE  ' | '.
      ***     10  VA-JOU-PREFIX-TS         PIC X(16).
      ***     10  FILLER                   PIC X(03)  VALUE  ' | '.
      ***     10  VA-JOU-PREFIX-TXT        PIC X(10).
      ***         88  VA-JOU-PREFIX-TXT-IN            VALUE  'PS9-IN'.
      ***         88  VA-JOU-PREFIX-TXT-OUT           VALUE  'PS9-OUT'.
      ***     10  FILLER                   PIC X(03)  VALUE  ' | '.
      ***
      ***  05  VN-JOU-LEN                  PIC 9(08)  BINARY.
      *--MC.E @P#73622/10.06.2011
      *--MC.E @DEM#1069/21.06.2017

       01 VA-VARIABLE.
          05 VA-HEADING.
             10 VA-IH-TAG-STT              PIC X(04).
             10 VA-PRTL-ID                 PIC X(02).
      *--MC.S @T#132561/22.04.2011
      ***    10 VA-LOG-TRM                 PIC X(08).
      ***    10 VA-ACC-TRM                 PIC X(08).
             10 VA-LOG-TRM                 PIC X(05).
             10 VA-TRM-ALLOCATE            PIC X(01).
                88 VA-TRM-ALLOCATE-YES                 VALUE '1'.
                88 VA-TRM-ALLOCATE-NO                  VALUE '0'.
             10 VA-TRM-RELEASE             PIC X(01).
                88 VA-TRM-RELEASE-YES                  VALUE '0'.
                88 VA-TRM-RELEASE-NO                   VALUE '1'.
             10 VA-TRM-SYNC                PIC X(01).
             10 VA-ACC-TRM                 PIC X(04).
             10 VA-ACC-TRM-ENT             PIC X(04).
      *--MC.E @T#132561/22.04.2011
             10 VA-USER                    PIC X(08).
             10 VA-NUM-SEQ                 PIC X(08).
             10 VA-COD-TX                  PIC X(08).
      *--MC.S @T#132561/22.04.2011
                88 VA-COD-TX-QGTP                      VALUE 'QGTP'.
      *--MC.E @T#132561/22.04.2011
             10 VA-USER-OPT                PIC X(02).
             10 VN-MSG-LTH                 PIC 9(05).
             10 VA-COMMIT                  PIC X(01).
             10 VA-TYP-MESSAGE             PIC X(01).
             10 VA-TYP-PROCESS             PIC X(01).
             10 VA-CHANNEL                 PIC X(02).
      *--MC.S @DEM#1069/21.06.2017
      *--MC.S @P#73622/10.06.2011
      ***       88  VA-CHANNEL-SBPLATT                 VALUE '10'.
      *--MC.E @P#73622/10.06.2011
      *--MC.E @DEM#1069/21.06.2017
             10 VA-FLG-PREFORMAT           PIC X(01).
             10 VA-LNG                     PIC X(01).
             10 VA-IH-TAG-END              PIC X(05).
             10 FILLER                     PIC X(12).

      *--MC.S @DEM#1069/21.06.2017
      *--MC.S @P#73622/10.06.2011
      *** 05 VA-OPERATOR-MSG               PIC X(99).
      *** 05 VN-EIBRESP1                   PIC 9(08).
      *** 05 VN-EIBRESP2                   PIC 9(08).
      *--MC.E @P#73622/10.06.2011
      *--MC.E @DEM#1069/21.06.2017

       01 VA-SWITCH.
          05 SW-PROCESS                    PIC X(1)    VALUE 'N'.
             88 SW-PRO-END                             VALUE 'S'.
             88 SW-PRO-NO-END                          VALUE 'N'.
          05 SW-TSQ                        PIC X(1)    VALUE 'N'.
             88 SW-TSQ-FOUND                           VALUE 'S'.
             88 SW-TSQ-NO-FND                          VALUE 'N'.
             88 SW-TSQ-NO-FND-ITEM                     VALUE 'I'.
          05 SW-ENCRYPTION                 PIC X(1)    VALUE 'N'.
             88 SW-ENCRYPTION-YES                      VALUE 'Y'.
             88 SW-ENCRYPTION-NO                       VALUE 'N'.
      *--MC.S @T#132561/22.04.2011
          05 FILLER                        PIC X(1)    VALUE  SPACES.
             88 SW-DYNAMIC-TERMINAL-GET                VALUE 'A'.
             88 SW-DYNAMIC-TERMINAL-RELEASE            VALUE 'R'.
      *--MC.E @T#132561/22.04.2011
      *
       01 VA-CONSTANT.
          05 CN-LENGTH                     PIC S9(5) COMP
                                   VALUE +31744.
      *--MC.S @T#132561/22.04.2011
      *** 05 CA-TERMINALPOOL-TX            PIC X(04)   VALUE 'QGTP'.
      *--MC.E @T#132561/22.04.2011
          05 CA-QG2CTP00                   PIC X(08)   VALUE 'QG2CTP00'.
          05 CA-QX6CAES                    PIC X(08)   VALUE 'QX6CAES'.
          05 CA-QA1CENT                    PIC X(08)   VALUE 'QA1CENT'.
          05 CA-QG1CABC                    PIC X(08)   VALUE 'QG1CABC'.
      *
          05 CA-QA1CCR2                    PIC X(08)   VALUE 'QA1CCR2'.
          05 CA-QA1CCR3                    PIC X(08)   VALUE 'QA1CCR3'.
          05 CA-QA7CROL                    PIC X(08)   VALUE 'QA7CROL'.
          05 CA-QA6CCOR1                   PIC X(08)   VALUE 'QA6CCOR1'.
          05 CA-COE-TSQ                    PIC X(4)    VALUE '+COE'.
          05 CA-END                        PIC X(4)    VALUE 'FIN '.
          05 CA-READ                       PIC X(1)    VALUE 'R'.
          05 CA-QCE0030                    PIC X(7)    VALUE 'QCE0030'.
          05 CA-QCE0004                    PIC X(7)    VALUE 'QCE0004'.
      *
          05 CA-QG6CTUT                    PIC X(08)   VALUE 'QG6CTUT'.
      *
          05 CA-MESSAGE                    PIC X(26)
                              VALUE '<OH>265A--------00026</OH>'.
          05 CA-READ-ERROR                 PIC X(7)
                              VALUE 'READ TS'.
          05 CA-XXXX                       PIC X(4)    VALUE 'XXXX'.
          05 CA-XX                         PIC X(2)    VALUE 'XX'.
          05 CA-S                          PIC X(1)    VALUE 'S'.
          05 CA-QC1CDPL                    PIC X(7)    VALUE 'QC1CDPL'.
          05 CA-LINK-ERROR.
             10 CA-LINK                    PIC X(11)
                              VALUE 'ERROR LINK '.
             10 CA-PGMID                   PIC X(8)    VALUE SPACES.
      *
      ******************************************************************
      *                      LINKAGE SECTION                           *
      ******************************************************************
       LINKAGE SECTION.
      *
       01 DFHCOMMAREA.
          COPY QAECINP.
      *--MC.S @T#132561/22.04.2011
          COPY QAECOUT.
      *--MC.E @T#132561/22.04.2011
      *
      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM 1-START
      *
           PERFORM 2-PROCESS
      *
           PERFORM 3-END.
      *
      ******************************************************************
      *.PN                 1-START                                    **
      ******************************************************************
       1-START.
      *
      *--MC.S @T#132561/22.04.2011
           SET  ADDRESS  OF  QAECOUT  TO  ADDRESS  OF  DFHCOMMAREA

      *    EXEC CICS
      *      IGNORE CONDITION ERROR
      *    END-EXEC
      *    IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
      *        MOVE QAECINP TO VA-HEADING
      *        MOVE CA-MESSAGE TO QAECINP
      *        PERFORM 999-CICS-ERROR
      *    END-IF.
           CONTINUE.
      *--MC.E @T#132561/22.04.2011
      *
      ******************************************************************
      *.PN                 2-PROCESS                                   *
      ******************************************************************
       2-PROCESS.
      *
           IF     EINP-HEAD-PS9(1:4) NOT = '<IH>'
           THEN
             PERFORM 20-CRYPT
             SET  SW-ENCRYPTION-YES   TO   TRUE
           END-IF

      *.MC.S @S75612 - TEMPORARY WORKAROUND ***************************
      *                                                               *
           IF EINP-SW-PRO-OFF
           THEN
             SET EINP-SW-HEA-1 TO TRUE
           END-IF
      *                                                               *
      *.MC.E @S75612 - TEMPORARY WORKAROUND ***************************

           MOVE QAECINP           TO VA-HEADING
      *
      *--MC.S @DEM#1069/21.06.2017
      *--MC.S @P#73622/10.06.2011
      ***  IF  VA-CHANNEL-SBPLATT
      ***  THEN
      ***    SET  VA-JOU-PREFIX-TXT-IN  TO  TRUE
      ***    PERFORM  EC-WRITE-CICS-JOURNAL
      ***  END-IF
      *--MC.E @P#73622/10.06.2011
      *--MC.E @DEM#1069/21.06.2017

           PERFORM 20-CHECK-INPUT-MESSAGE
      *
      *--MC.S @T#132561/22.04.2011
           IF    VA-COD-TX-QGTP
           AND ((VA-TRM-ALLOCATE-YES
           AND   VA-TRM-RELEASE-YES)
           OR   (VA-TRM-ALLOCATE-NO
           AND   VA-TRM-RELEASE-NO))
           THEN
             MOVE  CA-MESSAGE  TO  EINP-HEAD-PS9
             MOVE  VA-NUM-SEQ  TO  EOUT-NUM-SEQ-PS9
             MOVE  SPACES      TO  VA-TRM-ALLOCATE
                                   VA-TRM-RELEASE
           END-IF

           IF  VA-TRM-ALLOCATE-YES
           THEN
             SET      SW-DYNAMIC-TERMINAL-GET  TO  TRUE
             PERFORM  DYNAMIC-TERMINAL

             IF  ETP00-STATUS-ERROR
             THEN
               SET      EOUT-SW-PRO-TRM-ALLOC  TO  TRUE
               PERFORM  3-END
             END-IF
           END-IF

      ***  IF   VA-COD-TX EQUAL CA-TERMINALPOOL-TX
      ***  THEN
      ***    PERFORM  20-TERMINAL-POOL
      ***  ELSE
           IF NOT VA-COD-TX-QGTP
           THEN
      *--MC.E @T#132561/22.04.2011

             CALL CA-QA1CENT USING DFHEIBLK DFHCOMMAREA
      *
                ON EXCEPTION
                   MOVE WCO2C-CICS-LINK      TO EIBFN
                   MOVE WCO2C-CICS-PGMIDERR  TO EIBRESP
                  MOVE CA-QA1CENT      TO EIBRSRCE
             END-CALL
      *
             IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
      *
                 MOVE CA-QA1CENT           TO CA-PGMID
                 MOVE CA-LINK-ERROR        TO ABC-REFERENCE1
                PERFORM 999-CICS-ERROR
             END-IF
      *
             SET SW-TSQ-NO-FND           TO TRUE
             PERFORM 21-READ-QUEUE-COE
      *
             IF SW-TSQ-FOUND
                MOVE VA-COD-TX(1:4) TO EINP-COD-TRANSACTION-PS9(5:4)
                INITIALIZE QAECCR2
                           QAECCR3
                SET ECR2-SW-ERR-OK TO TRUE
                PERFORM 22-COORDINATOR-LOOP UNTIL SW-PRO-END
             END-IF
           END-IF
      *
           CONTINUE.
      *
      ******************************************************************
      *.PN                 20-DATA-ENCRYPTION/DECRYPTION              **
      ******************************************************************
       20-CRYPT.
      *
           EXEC CICS
               LINK PROGRAM(CA-QX6CAES)
               COMMAREA(DFHCOMMAREA)
           END-EXEC
      *
           IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
      *
                MOVE QAECINP              TO VA-HEADING
                MOVE CA-MESSAGE           TO QAECINP
                MOVE CA-QX6CAES           TO CA-PGMID
                MOVE CA-LINK-ERROR        TO ABC-REFERENCE1
                PERFORM 999-CICS-ERROR
           END-IF
           .
      *
      ******************************************************************
      *.PN                 20-CHECK-INPUT-MESSAGE                     **
      ******************************************************************
       20-CHECK-INPUT-MESSAGE.
     *
           IF EINP-HEAD-PS9(1:4) NOT = '<IH>'                 OR NOT
             (EINP-SW-PRT-PS9                                 OR
              EINP-SW-PRT-ATM                                 OR
              EINP-SW-PRT-NASB                                OR
              EINP-SW-PRT-POS                                 OR
              EINP-SW-PRT-SSMP-F                              OR
              EINP-SW-PRT-CEN-AUTHORIZATION)                  OR
              EINP-INP-LTH-PS9 IS NOT NUMERIC                 OR NOT
             (EINP-SW-CMM-YES                                 OR
              EINP-SW-CMM-NO)                                 OR NOT
             (EINP-SW-HEA-1                                   OR
              EINP-SW-HEA-2                                   OR
              EINP-SW-HEA-3                                   OR
              EINP-SW-HEA-5                                   OR
              EINP-SW-HEA-6)                                  OR NOT
             (EINP-SW-PRO-ON                                  OR
              EINP-SW-PRO-OFF)                                OR NOT
      *MC.S @80463
      *--     EINP-SW-PRE-NO                                  OR
             (EINP-SW-PRE-NO                                  OR
              EINP-SW-PRE-YES)                                OR
      *MC.E @80463
              EINP-HEAD-PS9(61:5) NOT = '</IH>'
           THEN
             MOVE    CA-MESSAGE TO QAECINP
             MOVE    VA-NUM-SEQ TO QAECINP(9:8)
             PERFORM 999-CICS-ERROR
           END-IF
           .
      *
      *--MC.S @T#132561/22.04.2011
      ******************************************************************
      *.PN                 20-TERMINAL-POOL                           **
      ******************************************************************
      *20-TERMINAL-POOL.
      *
      *    EXEC CICS
      *        LINK PROGRAM(CA-QG2CTP00)
      *        COMMAREA(DFHCOMMAREA)
      *    END-EXEC
      *
      *    IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
      *
      *         MOVE QAECINP              TO VA-HEADING
      *         MOVE CA-MESSAGE           TO QAECINP
      *         MOVE CA-QG2CTP00          TO CA-PGMID
      *         MOVE CA-LINK-ERROR        TO ABC-REFERENCE1
      *         PERFORM 999-CICS-ERROR
      *    END-IF
      *    .
      *--MC.E @T#132561/22.04.2011
      *
      ******************************************************************
      *.PN                21-READ-QUEUE-COE.                           *
      ******************************************************************
       21-READ-QUEUE-COE.
      *
           INITIALIZE QAECCOR1
           MOVE CA-COE-TSQ               TO ECOR1-PRFX-QUE-ID
           MOVE VA-LOG-TRM          TO ECOR1-LOG-TRM
           MOVE CA-READ                      TO ECOR1-OPTION
           MOVE 1                           TO ECOR1-NUM-ITEM
           MOVE LENGTH OF QACCCOEM          TO ECOR1-LENGTH
           CALL CA-QA6CCOR1 USING DFHEIBLK QAECCOR1
              ON EXCEPTION
                 MOVE WCO2C-CICS-LINK      TO EIBFN
                 MOVE WCO2C-CICS-PGMIDERR  TO EIBRESP
                 MOVE CA-QA6CCOR1     TO EIBRSRCE
           END-CALL
           EVALUATE EIBRESP
               WHEN WCO2C-CICS-ABENDERR
               WHEN WCO2C-CICS-PGMIDERR
                  IF  EINP-SW-PRT-PS9
                      MOVE CA-MESSAGE                   TO QAECINP
                      MOVE VA-NUM-SEQ                   TO QAECINP(9:8)
                  ELSE
                      CONTINUE
                  END-IF
      *
                  MOVE CA-QA6CCOR1           TO CA-PGMID
                  MOVE CA-LINK-ERROR         TO ABC-REFERENCE1
                  PERFORM 999-CICS-ERROR
               WHEN OTHER
                  MOVE WCO2C-CICS-NORMAL   TO EIBRESP
           END-EVALUATE
      *
           EVALUATE ECOR1-COD-ERR
              WHEN SPACES
                 SET SW-TSQ-FOUND TO TRUE
              WHEN CA-QCE0030
                 SET SW-TSQ-NO-FND-ITEM    TO TRUE
              WHEN CA-QCE0004
                 SET SW-TSQ-NO-FND TO TRUE
              WHEN OTHER
                 IF  EINP-SW-PRT-PS9
                     MOVE CA-MESSAGE                   TO QAECINP
                     MOVE VA-NUM-SEQ                   TO QAECINP(9:8)
                 ELSE
                     CONTINUE
                 END-IF
      *
                 MOVE CA-READ-ERROR   TO ABC-REFERENCE1
                 PERFORM 999-CICS-ERROR
           END-EVALUATE.
      *
      ******************************************************************
      *.PN                22-COORDINATOR-LOOP.                         *
      ******************************************************************
       22-COORDINATOR-LOOP.
      *
           MOVE VA-HEADING TO EINP-HEAD-PS9
           MOVE LOW-VALUES TO EINP-CONTENT-PS9
           SET ECR2-PS9-MSG-PNT  TO ADDRESS OF QAECINP
      *
           CALL CA-QA1CCR2  USING DFHEIBLK QAECCR2
              ON EXCEPTION
                 MOVE WCO2C-CICS-LINK      TO EIBFN
                 MOVE WCO2C-CICS-PGMIDERR  TO EIBRESP
                 MOVE CA-QA1CCR2      TO EIBRSRCE
           END-CALL
           EVALUATE EIBRESP
              WHEN WCO2C-CICS-NORMAL
                 CONTINUE
              WHEN WCO2C-CICS-ABENDERR
                PERFORM 9999-ROLLBACK-COORDINATOR
                IF  EINP-SW-PRT-PS9
                    MOVE CA-MESSAGE                   TO QAECINP
                    MOVE VA-NUM-SEQ                   TO QAECINP(9:8)
                ELSE
                    CONTINUE
                END-IF
                PERFORM 3-END
              WHEN OTHER
      *
               MOVE CA-QA1CCR2           TO CA-PGMID
               MOVE CA-LINK-ERROR        TO ABC-REFERENCE1
               PERFORM 999-CICS-ERROR
           END-EVALUATE
           MOVE VA-COD-TX TO ECR3-MULT-SERV
                             EINP-COD-TRANSACTION-PS9(5:4)
           MOVE VA-LOG-TRM TO ECR3-TERMINAL
      *
           IF ECR2-SW-ERR-OK OR ECR2-SW-ERR-PREVIOUS
              MOVE ECR2-UNIT-SERV  TO ECR3-UNIT-SERV
             MOVE ECR2-NUM-ORDER  TO ECR3-NUM-ORDER
      *
              CALL CA-QA1CENT USING DFHEIBLK DFHCOMMAREA
                 ON EXCEPTION
                    MOVE WCO2C-CICS-LINK      TO EIBFN
                    MOVE WCO2C-CICS-PGMIDERR  TO EIBRESP
                    MOVE CA-QA1CENT      TO EIBRSRCE
              END-CALL
      *
              IF EIBRESP NOT EQUAL DFHRESP(NORMAL)
      *
                 MOVE CA-QA1CENT           TO CA-PGMID
                 MOVE CA-LINK-ERROR        TO ABC-REFERENCE1
                 PERFORM 999-CICS-ERROR
              END-IF
      *
              MOVE ECR2-OUT-EXC TO ECR3-OUT-EXC
              MOVE ECR2-COD-ERR      TO ECR3-COD-ERR
              IF ECR3-COD-ERR NOT EQUAL SPACES
                 MOVE ECR2-OUT-FMT    TO ECR3-OUT-FMT
                 MOVE ECR2-ERR-VAL      TO ECR3-ERR-VAL
                 MOVE ECR2-OPERAND         TO ECR3-OPERAND
                 MOVE ECR2-ERR-PRV-ACC  TO ECR3-ERR-PRV-ACC
                 MOVE ECR2-COD-ERR-DISP TO ECR3-COD-ERR-DISP
                 MOVE ECR2-ERR-VAL-LTH  TO  ECR3-ERR-VAL-LTH
              END-IF
              PERFORM 23-CALL-UP-OUTPUT-MANAGER
           ELSE
              EVALUATE TRUE
              WHEN ECR2-SW-ERR-SERV-END-YES
                MOVE CA-END TO ECR3-UNIT-SERV
                PERFORM 23-CALL-UP-OUTPUT-MANAGER
              WHEN ECR2-SW-ERR-SERV-END-RBCK
                PERFORM 9999-ROLLBACK-COORDINATOR
              WHEN OTHER
                PERFORM 9999-ROLLBACK-COORDINATOR
              END-EVALUATE
              SET SW-PRO-END TO TRUE
           END-IF.
      ******************************************************************
      *.PN            23-CALL-UP-OUTPUT-MANAGER                        *
      ******************************************************************
       23-CALL-UP-OUTPUT-MANAGER.
      *
           SET ECR3-SW-ERR-FLG-OK TO TRUE
           SET ECR3-OUT-PNT   TO ADDRESS OF QAECINP
      *
           CALL CA-QA1CCR3  USING DFHEIBLK QAECCR3
              ON EXCEPTION
                 MOVE WCO2C-CICS-LINK      TO EIBFN
                 MOVE WCO2C-CICS-PGMIDERR  TO EIBRESP
                 MOVE CA-QA1CCR3      TO EIBRSRCE
           END-CALL
           EVALUATE EIBRESP
              WHEN WCO2C-CICS-NORMAL
                 CONTINUE
              WHEN WCO2C-CICS-ABENDERR
                PERFORM 9999-ROLLBACK-COORDINATOR
                IF  EINP-SW-PRT-PS9
                    MOVE CA-MESSAGE                   TO QAECINP
                    MOVE VA-NUM-SEQ                   TO QAECINP(9:8)
                ELSE
                    CONTINUE
                END-IF
                PERFORM 3-END
              WHEN OTHER
      *
               MOVE CA-QA1CCR3           TO CA-PGMID
               MOVE CA-LINK-ERROR        TO ABC-REFERENCE1
               PERFORM 999-CICS-ERROR
           END-EVALUATE
           EVALUATE TRUE
           WHEN ECR3-SW-ERR-FLG-OK
             CONTINUE
           WHEN ECR3-SW-ERR-FLG-CONT
             SET ECR2-SW-ERR-PREVIOUS TO TRUE
           WHEN ECR3-SW-ERR-FLG-AUTHORIZATION
             SET SW-PRO-END TO TRUE
           WHEN ECR3-SW-ERR-FLG-ROLLBACK
           WHEN ECR3-SW-ERR-FLG-PROCESS
             PERFORM 9999-ROLLBACK-COORDINATOR
             SET SW-PRO-END TO TRUE
           WHEN OTHER
             PERFORM 9999-ROLLBACK-COORDINATOR
             SET SW-PRO-END TO TRUE
           END-EVALUATE.
      ******************************************************************
      *.PN            3-END                                          * *
      ******************************************************************
       3-END.
      *
      *--MC.S @T#132561/22.04.2011
           IF      NOT  EOUT-SW-PRO-TRM-ALLOC
           THEN
             IF         VA-TRM-ALLOCATE-YES
             THEN
               IF       VA-COD-TX-QGTP
               THEN
                 INITIALIZE  QGNCTP01
                 MOVE   VA-ACC-TRM    TO  TP01-TERMID
                 MOVE   VA-OC-MAP     TO  QAECOUT(EOUT-MSG-LTH-PS9 + 1:
                                                  LENGTH OF VA-OC-MAP)
                 ADD    LENGTH OF VA-OC-MAP
                                      TO  EOUT-MSG-LTH-PS9
               ELSE
                 MOVE   VA-LOG-TRM    TO  VA-DTA01-LOG-TRM
                 MOVE   VA-ACC-TRM    TO  VA-DTA01-ACC-TRM
                 MOVE   VA-AD-DTA01   TO  QAECOUT(EOUT-MSG-LTH-PS9 + 1:
                                                  LENGTH OF VA-AD-DTA01)
                 ADD      LENGTH OF VA-AD-DTA01
                                      TO  EOUT-MSG-LTH-PS9
               END-IF
      *--MC.S @DEM#1069/21.06.2017
      ***    END-IF
             ELSE
      *--MC.E @DEM#1069/21.06.2017
               IF         VA-TRM-RELEASE-YES
               THEN
                 SET      SW-DYNAMIC-TERMINAL-RELEASE  TO  TRUE
                 PERFORM  DYNAMIC-TERMINAL
               END-IF
      *--MC.S @DEM#1069/21.06.2017
             END-IF
      *--MC.E @DEM#1069/21.06.2017
           END-IF
      *--MC.E @T#132561/22.04.2011

      *--MC.S @DEM#1069/21.06.2017
      *--MC.S @P#73622/10.06.2011
      ***  IF  VA-CHANNEL-SBPLATT
      ***  THEN
      ***    SET  VA-JOU-PREFIX-TXT-OUT  TO  TRUE
      ***    PERFORM  EC-WRITE-CICS-JOURNAL
      ***  END-IF
      *--MC.E @P#73622/10.06.2011
      *--MC.E @DEM#1069/21.06.2017

           IF           SW-ENCRYPTION-YES
           THEN
             PERFORM    20-CRYPT
           END-IF

           EXEC CICS
             RETURN
           END-EXEC.
      *

      *--MC.S @T#132561/22.04.2011
      ******************************************************************
      *    DYNAMIC TERMINAL ALLOCATION / DEALLOCATION                  *
      ******************************************************************
       DYNAMIC-TERMINAL.

           INITIALIZE  VA-QGECTP00-01
           MOVE        VA-CHANNEL              TO  ETP00-CHANNEL
           MOVE        VA-ACC-TRM-ENT          TO  ETP00-ENTITY
           MOVE        VA-TRM-SYNC             TO  ETP00-SYNC

           IF          SW-DYNAMIC-TERMINAL-GET
           THEN
             SET       ETP00-OPCODE-GET        TO  TRUE
           ELSE
             SET       ETP00-OPCODE-RELEASE    TO  TRUE
             MOVE      VA-ACC-TRM              TO  ETP00-TERMID
           END-IF

           IF          VA-TRM-ALLOCATE-YES
           AND         VA-TRM-RELEASE-YES
           THEN
             SET       ETP00-LOCK-SINGLE-TASK  TO  TRUE
           ELSE
             SET       ETP00-LOCK-MULTI-TASK   TO  TRUE
           END-IF

           PERFORM     EC-LINK-QG2CTP00

           EVALUATE    TRUE                ALSO  TRUE

             WHEN      ETP00-STATUS-OKAY   ALSO  VA-COD-TX-QGTP
               MOVE    ETP00-TERMID          TO  VA-LOG-TRM
                                                 VA-ACC-TRM
               MOVE    CA-MESSAGE            TO  EINP-HEAD-PS9
               MOVE    VA-NUM-SEQ            TO  EOUT-NUM-SEQ-PS9
               SET     EOUT-SW-RES-OK-CMMT   TO  TRUE
               SET     EOUT-SW-PRO-OK        TO  TRUE
               IF      ETP00-OPCODE-RELEASE
               THEN
                 MOVE  VA-AV-MAP             TO  EOUT-DTA-PS9
                                                (1:LENGTH OF VA-AV-MAP)
                 ADD   LENGTH OF VA-AV-MAP   TO  EOUT-MSG-LTH-PS9
               END-IF

             WHEN      ETP00-STATUS-OKAY   ALSO  ETP00-OPCODE-GET
               MOVE    ETP00-TERMID          TO  EINP-LOG-TRM-PS9-A1
                                                 EINP-ACC-TRM-PS9
                                                 VA-LOG-TRM
                                                 VA-ACC-TRM

             WHEN      ETP00-STATUS-ERROR  ALSO  ETP00-OPCODE-GET
             WHEN      ETP00-STATUS-ERROR  ALSO  VA-COD-TX-QGTP
               MOVE    SPACES                TO  QAECINP(1:VN-MSG-LTH)
               MOVE    CA-MESSAGE            TO  EOUT-HEAD-PS9
               MOVE    VA-NUM-SEQ            TO  EOUT-NUM-SEQ-PS9
               MOVE    ETP00-ERROR-REF       TO  VA-ERR-MAP-VAR2
               MOVE    VA-ER-MAP             TO  EOUT-DTA-PS9
                                                (1:LENGTH OF VA-ER-MAP)
               ADD     LENGTH OF VA-ER-MAP   TO  EOUT-MSG-LTH-PS9

             WHEN      ETP00-STATUS-ERROR  ALSO  ETP00-OPCODE-RELEASE
               SET     EOUT-SW-PRO-TRM-RELEASE
                                             TO  TRUE
           END-EVALUATE

           CONTINUE.

      ******************************************************************
      *    EXEC CICS LINK QG2CTP00                                     *
      ******************************************************************
       EC-LINK-QG2CTP00.

           EXEC CICS
             LINK
               PROGRAM  (CA-QG2CTP00)
               COMMAREA (VA-QGECTP00-01)
           END-EXEC

           CONTINUE.
      *--MC.E @T#132561/22.04.2011

      *--MC.S @DEM#1069/21.06.2017
      *--MC.S @P#73622/10.06.2011
      ******************************************************************
      *    WRITE CICS JOURNAL                                          *
      ******************************************************************
      *EC-WRITE-CICS-JOURNAL.
      ***
      ***  PERFORM  EC-IGNORE-CONDITION
      ***
      ***  SET   VA-JOURNALNAME-SBPLOG  TO  TRUE
      ***  MOVE  EIBTASKN               TO  VN-JOU-PREFIX-TASK
      ***  MOVE  FUNCTION CURRENT-DATE  TO  VA-JOU-PREFIX-TS
      ***
      ***  IF    VA-JOU-PREFIX-TXT-IN
      ***  THEN
      ***    MOVE  EINP-INP-LTH-PS9     TO  VN-JOU-LEN
      ***  ELSE
      ***    MOVE  EOUT-MSG-LTH-PS9     TO  VN-JOU-LEN
      ***  END-IF
      ***
      ***  EXEC CICS
      ***    WRITE
      ***      JOURNALNAME (VA-JOURNALNAME)
      ***      JTYPEID     ('SB')
      ***      FROM        (DFHCOMMAREA)
      ***      FLENGTH     (VN-JOU-LEN)
      ***      PREFIX      (VA-JOU-PREFIX)
      ***      PFXLENG     (LENGTH OF VA-JOU-PREFIX)
      ***  END-EXEC
      ***
      ***  IF    EIBRESP  NOT  EQUAL  DFHRESP(NORMAL)
      ***  THEN
      ***    MOVE     EIBRESP                TO  VN-EIBRESP1
      ***    MOVE     EIBRESP2               TO  VN-EIBRESP2
      ***    MOVE     SPACES                 TO  VA-OPERATOR-MSG
      ***    STRING  'ERROR WRITING JOURNAL ', VA-JOURNALNAME
      ***            ' / EIBRESP=<' , VN-EIBRESP1, '>'
      ***            ' / EIBRESP2=<', VN-EIBRESP2, '>'
      ***             DELIMITED  BY  SIZE  INTO  VA-OPERATOR-MSG
      ***    PERFORM  EC-WRITE-OPERATOR
      ***  END-IF
      ***
      ***  PERFORM  EC-PUSH-HANDLE
      ***
      ***  CONTINUE.
      ***
      ******************************************************************
      ***  IGNORE CONDITION                                            *
      ******************************************************************
      *EC-IGNORE-CONDITION.
      ***
      ***  EXEC CICS
      ***    IGNORE CONDITION ERROR
      ***  END-EXEC
      ***
      ***  CONTINUE.
      ***
      ******************************************************************
      ***  PUSH HANDLE                                                 *
      ******************************************************************
      *EC-PUSH-HANDLE.
      ***
      ***  EXEC CICS
      ***    PUSH HANDLE
      ***  END-EXEC
      ***
      ***  CONTINUE.
      ***
      ******************************************************************
      ***  WRITE OPERATOR                                              *
      ******************************************************************
      *EC-WRITE-OPERATOR.
      ***
      ***  EXEC CICS
      ***    WRITE OPERATOR
      ***      TEXT (VA-OPERATOR-MSG)
      ***      CRITICAL
      ***  END-EXEC
      ***
      ***  CONTINUE.
      ***
      *--MC.E @P#73622/10.06.2011
      *--MC.E @DEM#1069/21.06.2017

      ******************************************************************
      *.PN            999-CICS-ERROR                                   *
      ******************************************************************
       999-CICS-ERROR.
      *
           MOVE CA-S       TO ABC-ABEND
           MOVE CA-QC1CDPL TO ABC-DES-PROG
           MOVE EIBFN      TO ABC-EIBFN
           MOVE EIBRSRCE   TO ABC-EIBRSRCE
           MOVE EIBRCODE   TO ABC-EIBRCODE
           MOVE EIBRESP    TO ABC-EIBRESP1
           MOVE EIBRESP2   TO ABC-EIBRESP2
      *
           PERFORM 9-CREATE-TS-QGECTUT
      *
           CALL CA-QG1CABC USING DFHEIBLK QGECABC
      *
              ON EXCEPTION
                 CONTINUE
      *
           END-CALL
      *
           PERFORM 9999-ROLLBACK-COORDINATOR
           PERFORM 3-END.
      *
      ******************************************************************
      *.PN            9-CREATE-TS-QGECTUT                              *
      ******************************************************************
       9-CREATE-TS-QGECTUT.
      *
           SET  TUT-SW-OPE-WRIT          TO TRUE
           MOVE VA-LOG-TRM               TO TUT-TERMINAL
           MOVE VA-USER                  TO TUT-USERID
      *
           MOVE CA-XXXX                  TO TUT-CEN-ACCTTRM
           MOVE CA-XXXX                  TO TUT-ENT-COD
           MOVE VA-ACC-TRM               TO TUT-ACCT-TERMINAL
           MOVE VA-COD-TX                TO TUT-TRANSACTION
      *
           MOVE CA-XX                    TO TUT-SW-STTCOD
           MOVE VA-LNG                   TO TUT-LANGUA
           MOVE VA-CHANNEL               TO TUT-CHANN
      *
           CALL CA-QG6CTUT USING DFHEIBLK DFHCOMMAREA QGECTUT
      *
              ON EXCEPTION
                 CONTINUE
           END-CALL.
      *
      ******************************************************************
      *.PN            9999-ROLLBACK-COORDINATOR                        *
      ******************************************************************
       9999-ROLLBACK-COORDINATOR.
      *
           EXEC CICS
               SYNCPOINT ROLLBACK
           END-EXEC.
      *
      * ALNOVA SERIAL NUMBER: 94DC9756 ********* DO NOT REMOVE *********

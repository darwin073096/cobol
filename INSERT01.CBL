      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    INSERT01.
       AUTHOR.        DARWIN TERCENIO.
       DATE-WRITTEN.  05.13.2020.
      *--------------------
      *****************************************************************
      *PROGRAM DESCRIPTION.                                           *
      * --- THIS PROGRAM WILL BE EXECUTED USING XCTL WHEN USER        *
      * PRESSED F4                                                    *
      * FUNTIONS AVAILABLE:                                           *
      *    -- F3    > GO BACK TO MAIN LIST                            *
      *                                                               *
      * RELATED PROGRAMS:                                             *
      *    -- BOOK01DB > CALLING PROGRAM                              *
      *                                                               *
      * --- THIS PROGRAM WILL BE USE TO INSERT NEW RECORD IN          *
      * IBMUSER.BOOKS                                                 *
      *****************************************************************
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
      *----------------------------------------------------------------
       DATA DIVISION.
      *----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01  WS-COMMAREA.
           05  WS-TABLE-SELECT.
               10  WS-SELECTED  OCCURS 15 TIMES.
                   15  WS-SELECTED-ID  PIC 9(004)                     .
           05  WS-TABLE-UPDATE.
               10  WS-UPDATE    OCCURS 15 TIMES.
                   15  WS-UPDATE-ID    PIC 9(004)                     .
           05  WS-TABLE-DELETE.
               10  WS-DELETE    OCCURS 15 TIMES.
                   15  WS-DELETE-ID    PIC 9(004)                     .
           05  WS-SEARCH               PIC X(020)                     .
           05  BOOK-ITEM               PIC S9(4)  COMP  VALUE +1      .
           05  BOOK-NUMITEM            PIC S9(04) COMP  VALUE  0      .
           05  TSQ-PAGE-ENTRY.
               10  TSQ-PAGE     OCCURS 15 TIMES.
                   15  TSQ-BOOK-ID     PIC 9(004)                     .
                   15  TSQ-BOOK-TITLE  PIC X(067)                     .
           05  TS-QUEUE-NAME.
               10  FILLER              PIC X(004) VALUE 'DAR1'        .
               10  TS-TERMINAL-ID      PIC X(004)                     .

           05  WS-ALTER-RECORD.
               10  WS-INPUT-S          PIC X(001)                     .
               10  WS-INPUT-U          PIC X(001)                     .
               10  WS-INPUT-D          PIC X(001)                     .

      *FOR BOOK02DB-----------------------------------------------------
           05  TS-QUEUE-NAME-2.
               10  FILLER              PIC X(004) VALUE 'DAR2'        .
               10  TS-TERMINAL-ID-2    PIC X(004)                     .
           05  INFO-ITEM               PIC S9(04) COMP  VALUE +1      .
           05  INFO-NUMITEM            PIC S9(04) COMP  VALUE  0      .
      *-----------------------------------------------------------------

           05  WS-DEL-ID               PIC 9(004)                     .

       01  SWITCH.
           05  ERROR-HANDLING          PIC X    VALUE 'N'             .
               88  SOME-ERROR                   VALUE 'Y'             .

           05  INSERT-CONFIRM          PIC X                          .
               88  INSERT-YES                   VALUE 'Y'             .
               88  INSERT-NO                    VALUE 'N'             .

       01  WS-VARS.
           05  WS-TRANSID              PIC X(04) VALUE 'TBL3'         .
           05  WS-MAP                  PIC X(07) VALUE 'INSERTM'      .
           05  WS-MAPSET               PIC X(07) VALUE 'INSERT1'      .
           05  RESPONSE-CODE           PIC S9(4) COMP                .
           05  END-OF-SESSION          PIC X(13) VALUE 'SESSION ENDED'.
           05  WS-INVALID              PIC X(13) VALUE 'INVALID TRNID'.
           05  WS-MESSAGE.
               10  FILLER              PIC X(10) VALUE 'ERROR IN: '  .
               10  ERR-LOC             PIC X(30)                     .
               10  FILLER              PIC X(06) VALUE 'CODE: '      .
               10  ERR-CODE            PIC S9(4) COMP                .
           05  BK-TITLE.
               10  BK-TITLE1           PIC X(35)                     .
               10  BK-TITLE2           PIC X(35)                     .
               10  BK-TITLE3           PIC X(35)                     .
               10  BK-TITLE4           PIC X(35)                     .
               10  BK-TITLE5           PIC X(26)                     .
           05  WS-DEFAULT-RATING       PIC X(04) VALUE '1.00'        .
           05  WS-DEFAULT-DATE         PIC X(10) VALUE '2020-01-01'  .

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE DCLBOOKS
           END-EXEC.

           COPY INSERT1.
           COPY DFHAID.
           COPY ATTR.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  LS-TABLE-SELECT.
               10  LS-SELECTED OCCURS 15 TIMES.
                   15  LS-SELECTED-ID  PIC 9(004)                     .
           05  LS-TABLE-UPDATE.
               10  LS-UPDATE OCCURS 15 TIMES.
                   15  LS-UPDATE-ID  PIC 9(004)                     .
           05  LS-TABLE-DELETE.
               10  LS-DELETE OCCURS 15 TIMES.
                   15  LS-DELETE-ID  PIC 9(004)                     .
           05  LS-SEARCH               PIC X(020)                     .
           05  LS-BOOK-ITEM            PIC S9(4)  COMP  VALUE +1      .
           05  LS-BOOK-NUMITEM         PIC S9(04) COMP  VALUE  0      .
           05  LS-TSQ-PAGE-ENTRY.
               10  TSQ-PAGE OCCURS 15 TIMES.
                   15  LS-TSQ-BOOK-ID      PIC 9(004)                 .
                   15  LS-TSQ-BOOK-TITLE   PIC X(067)                 .
           05  LS-QUEUE-NAME.
               10  FILLER              PIC X(4)   VALUE 'DAR1'        .
               10  LS-TERMINAL-ID      PIC X(4)                       .

           05  LS-ALTER-RECORD.
               10  LS-INPUT-S          PIC X(1)                       .
               10  LS-INPUT-U          PIC X(1)                       .
               10  LS-INPUT-D          PIC X(1)                       .

      *FOR BOOK02DB-----------------------------------------------------
           05  LS-QUEUE-NAME-2.
               10  FILLER              PIC X(4)   VALUE 'DAR2'        .
               10  LS-TERMINAL-ID-2    PIC X(4)                       .
           05  LS-INFO-ITEM            PIC S9(4)  COMP  VALUE +1      .
           05  LS-INFO-NUMITEM         PIC S9(04) COMP  VALUE  0      .
      *-----------------------------------------------------------------

           05  LS-DEL-ID               PIC 9(4)                       .

      *----------------------------------------------------------------
       PROCEDURE DIVISION.
      *----------------------------------------------------------------
       A0000-MAIN-PROCESS.
           EVALUATE TRUE
               WHEN EIBCALEN > 0
                    MOVE DFHCOMMAREA         TO WS-COMMAREA
                    MOVE LOW-VALUES          TO INSERTMI
                    PERFORM A1000-EVALUATE THRU A1000-EXIT

               WHEN EIBCALEN = 0
                    EXEC CICS
                         SEND TEXT FROM(WS-INVALID)
                                   ERASE
                                   FREEKB
                    END-EXEC

                    EXEC CICS
                         RETURN
                    END-EXEC
           END-EVALUATE
           .
       A0000-EXIT.
           EXIT
           .
       A1000-EVALUATE.
           IF EIBTRNID NOT = WS-TRANSID
              EXEC CICS SET
                  TERMINAL  (EIBTRMID)
                  RESP      (RESPONSE-CODE)
                  TRANIDONLY
              END-EXEC

              PERFORM Z2000-MOVE-SPACES  THRU Z2000-EXIT
              PERFORM Z0000-SEND-MAP     THRU Z0000-EXIT

           ELSE
              EVALUATE TRUE
                  WHEN EIBAID = DFHPF3
                       EXEC CICS
                           XCTL PROGRAM('BOOK01DB')
                                COMMAREA(WS-COMMAREA)
                                RESP(RESPONSE-CODE)
                       END-EXEC

                  WHEN EIBAID = DFHENTER
                       EXEC CICS
                           RECEIVE MAP(WS-MAP)
                                   MAPSET(WS-MAPSET)
                                   INTO(INSERTMI)
                                   RESP(RESPONSE-CODE)
                       END-EXEC

                       MOVE    CONFIRMI               TO INSERT-CONFIRM
                       PERFORM A2000-INSERT-CONFIRM THRU A2000-EXIT

                  WHEN OTHER
                       MOVE 'INVALID KEY PRESSED.'    TO MESSAGEO
                       PERFORM  Z0000-SEND-MAP      THRU Z0000-EXIT
              END-EVALUATE
           END-IF
           .
       A1000-EXIT.
           EXIT
           .
       A2000-INSERT-CONFIRM.
           EVALUATE TRUE
               WHEN INSERT-YES
                    PERFORM A2100-MOVE-DATA     THRU A2100-EXIT
                    PERFORM A2200-INSERT-DATA   THRU A2200-EXIT
                    MOVE    LOW-VALUES            TO INSERTMO
                    MOVE    'INSERT SUCCESSFUL.'  TO MESSAGEO
                    PERFORM  Z0000-SEND-MAP     THRU Z0000-EXIT

               WHEN INSERT-NO
                    EXEC CICS
                        XCTL PROGRAM('BOOK01DB')
                             COMMAREA(WS-COMMAREA)
                             RESP(RESPONSE-CODE)
                    END-EXEC

               WHEN OTHER
                    MOVE    LOW-VALUES            TO INSERTMO
                    MOVE    'INVALID INPUT.'      TO MESSAGEO
                    PERFORM Z0000-SEND-MAP      THRU Z0000-EXIT
           END-EVALUATE
           .
       A2000-EXIT.
           EXIT
           .
       A2100-MOVE-DATA.
           IF MBOOKIDI = LOW-VALUE OR SPACES
              MOVE    SPACES                      TO CONFIRMO
              MOVE   'BOOK ID IS REQUIRED.'       TO MESSAGEO
              PERFORM Z0000-SEND-MAP            THRU Z0000-EXIT
           ELSE
              COMPUTE BOOKS-BOOK-ID = FUNCTION NUMVAL(MBOOKIDI)
           END-IF

           IF MTITLE1I = LOW-VALUE OR SPACES
              MOVE    SPACES                      TO CONFIRMO
              MOVE    'TITLE IS REQUIRED.'        TO MESSAGEO
              PERFORM Z0000-SEND-MAP            THRU Z0000-EXIT
           ELSE
              MOVE MTITLE1I                       TO BK-TITLE1
              MOVE MTITLE2I                       TO BK-TITLE2
              MOVE MTITLE3I                       TO BK-TITLE3
              MOVE MTITLE4I                       TO BK-TITLE4
              MOVE MTITLE5I                       TO BK-TITLE5
              MOVE LENGTH OF BK-TITLE             TO BOOKS-TITLE-LEN
              MOVE BK-TITLE                       TO BOOKS-TITLE-TEXT
           END-IF

           IF MPAGESI = LOW-VALUE OR SPACES
              MOVE ZEROS             TO BOOKS-TOTAL-PAGES
           ELSE
              COMPUTE BOOKS-TOTAL-PAGES = FUNCTION NUMVAL(MPAGESI)
           END-IF

           IF MRATINGI = LOW-VALUE OR SPACES
              MOVE ZEROS             TO BOOKS-RATING
           ELSE
              EVALUATE TRUE
                  WHEN MRATINGI = 0
                       CONTINUE
                  WHEN MRATINGI >= 1 OR <= 5
                       COMPUTE BOOKS-RATING = FUNCTION NUMVAL(MRATINGI)
                  WHEN OTHER
                       MOVE  SPACES                       TO CONFIRMO
                       MOVE 'PLEASE ENTER BETWEEN 1 & 5.' TO MESSAGEO
                       PERFORM Z0000-SEND-MAP           THRU Z0000-EXIT
              END-EVALUATE
           END-IF

           IF MISBNNOI = LOW-VALUE OR SPACES
              CONTINUE
           ELSE
              MOVE LENGTH OF MISBNNOI   TO BOOKS-ISBN-LEN
              MOVE MISBNNOI             TO BOOKS-ISBN-TEXT
           END-IF

           IF MPUBDTEI = LOW-VALUE OR SPACES OR 'YYYY-MM-DD'
              MOVE WS-DEFAULT-DATE   TO BOOKS-PUBLISHER-DATE
           ELSE
              MOVE MPUBDTEI          TO BOOKS-PUBLISHER-DATE
           END-IF

           IF MPUBIDI = LOW-VALUE OR SPACES
              MOVE ZEROS             TO BOOKS-PUBLISHER-ID
           ELSE
              COMPUTE BOOKS-PUBLISHER-ID = FUNCTION NUMVAL(MPUBIDI)
           END-IF
           .
       A2100-EXIT.
           EXIT
           .
       A2200-INSERT-DATA.
           EXEC SQL
                INSERT INTO IBMUSER.BOOKS
                (
                  BOOK_ID
                 ,TITLE
                 ,TOTAL_PAGES
                 ,RATING
                 ,ISBN
                 ,PUBLISHER_DATE
                 ,PUBLISHER_ID
                )
                VALUES
                (
                 :BOOKS-BOOK-ID
                ,:BOOKS-TITLE
                ,:BOOKS-TOTAL-PAGES
                ,:BOOKS-RATING
                ,:BOOKS-ISBN
                ,:BOOKS-PUBLISHER-DATE
                ,:BOOKS-PUBLISHER-ID
                )
           END-EXEC

           EVALUATE TRUE
               WHEN SQLCODE = 0
                    CONTINUE
               WHEN SQLCODE = -180
                    MOVE SPACES     TO CONFIRMO
                    MOVE 'INCORRECT DATE FORMAT.' TO MESSAGEO
                    PERFORM Z0000-SEND-MAP      THRU Z0000-EXIT
               WHEN SQLCODE = -803
                    MOVE SPACES     TO CONFIRMO
                    MOVE 'BOOK ID ALREADY EXIST.' TO MESSAGEO
                    PERFORM Z0000-SEND-MAP      THRU Z0000-EXIT
               WHEN OTHER
      *             MOVE SPACES     TO CONFIRMO
      *             MOVE  SQLCODE                 TO MESSAGEO
      *             PERFORM Z0000-SEND-MAP      THRU Z0000-EXIT
                    MOVE 'A2200-INSERT-DATA'      TO ERR-LOC
                    MOVE  SQLCODE                 TO ERR-CODE
                    MOVE  'Y'                     TO ERROR-HANDLING
                    PERFORM XXXX-ERROR-HANDLING
           END-EVALUATE
            .
       A2200-EXIT.
           EXIT
           .
      *SEND MAP PARAGRAPH-----------------------------------------------
       Z0000-SEND-MAP.
           PERFORM Z1000-MOVE-ATTR THRU Z1000-EXIT

           EXEC CICS
                SEND MAP(WS-MAP)
                     MAPSET(WS-MAPSET)
                     FROM(INSERTMO)
                     RESP(RESPONSE-CODE)
           END-EXEC

           EXEC CICS
               RETURN TRANSID(WS-TRANSID)
                      COMMAREA(WS-COMMAREA)
                      RESP(RESPONSE-CODE)
           END-EXEC
           .
       Z0000-EXIT.
           EXIT
           .
       Z1000-MOVE-ATTR.
           MOVE ATTR-UNPROT-NUM-MDT TO MBOOKIDA
           MOVE ATTR-UNPROT-MDT     TO MTITLE1A
           MOVE ATTR-UNPROT-MDT     TO MTITLE2A
           MOVE ATTR-UNPROT-MDT     TO MTITLE3A
           MOVE ATTR-UNPROT-MDT     TO MTITLE4A
           MOVE ATTR-UNPROT-MDT     TO MTITLE5A
           MOVE ATTR-UNPROT-NUM-MDT TO MPAGESA
           MOVE ATTR-UNPROT-NUM-MDT TO MRATINGA
           MOVE ATTR-UNPROT-MDT     TO MISBNNOA
           MOVE ATTR-UNPROT-NUM-MDT TO MPUBDTEA
           MOVE ATTR-UNPROT-NUM-MDT TO MPUBIDA
           .
       Z1000-EXIT.
           EXIT
           .
       Z2000-MOVE-SPACES.
           MOVE SPACES              TO MBOOKIDO
           MOVE SPACES              TO MTITLE1O
           MOVE SPACES              TO MTITLE2O
           MOVE SPACES              TO MTITLE3O
           MOVE SPACES              TO MTITLE4O
           MOVE SPACES              TO MTITLE5O
           MOVE SPACES              TO MPAGESO
           MOVE SPACES              TO MRATINGO
           MOVE SPACES              TO MISBNNOO
           MOVE SPACES              TO MPUBIDO
           MOVE SPACES              TO MESSAGEO
           MOVE SPACES              TO CONFIRMO
           .
       Z2000-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *SUPPORT PARAGRAPH------------------------------------------------
       XXXX-ERROR-HANDLING.
           IF SOME-ERROR
              EXEC CICS DELETEQ TS
                   QUEUE (TS-QUEUE-NAME)
                   RESP(RESPONSE-CODE)
              END-EXEC

              EXEC CICS
                  SEND TEXT FROM(WS-MESSAGE)
                            ERASE
                            FREEKB
              END-EXEC

              EXEC CICS
                   RETURN
              END-EXEC
           .
      *-----------------------------------------------------------------

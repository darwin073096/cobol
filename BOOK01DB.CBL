      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    BOOK01DB.
       AUTHOR.        DARWIN TERCENIO.
       DATE-WRITTEN.  04.23.2020.
      *-----------------------
      *****************************************************************
      *PROGRAM DESCRIPTION.                                           *
      * --- UPON ENTERING TRANSID, WILL DISPLAY FIRST 15 BOOKS.       *
      * FUNTIONS AVAILABLE:                                           *
      *    -- F7    > PREVIOUS PAGE                                   *
      *    -- F8    > NEXT PAGE                                       *
      *    -- F3    > EXIT                                            *
      *    -- F10   > EXIT                                            *
      *    -- ENTER > TO DISPLAY BOOK DETAILS                         *
      *                                                               *
      * RELATED PROGRAMS: TSQVSAM AND BOOK02DB                        *
      *    -- TSQVSAM > TO LOAD DATA ON TSQ                           *
      *    -- BOOK02DB > TO DISPLAY BOOK DETAILS                      *
      *                                                               *
      * --- THIS PROGRAM USED LINK TO EXECUTE PROGRAM TSQVSAM         *
      * --- UPON ENTERING 'S', BOOK02DB WILL BE EXECUTED USING XCTL   *
      *****************************************************************
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01  WS-COMMAREA.
           05  WS-TABLE-SELECT.
               10  WS-SELECTED OCCURS 15 TIMES.
                   15  WS-SELECTED-ID  PIC 9(004)                     .
           05  WS-TABLE-UPDATE.
               10  WS-UPDATE OCCURS 15 TIMES.
                   15  WS-UPDATE-ID    PIC 9(004)                     .
           05  WS-TABLE-DELETE.
               10  WS-DELETE OCCURS 15 TIMES.
                   15  WS-DELETE-ID    PIC 9(004)                     .
           05  WS-SEARCH               PIC X(020)                     .
           05  BOOK-ITEM               PIC S9(4)  COMP  VALUE +1      .
           05  BOOK-NUMITEM            PIC S9(04) COMP  VALUE  0      .
           05  TSQ-PAGE-ENTRY.
               10  TSQ-PAGE OCCURS 15 TIMES.
                   15  TSQ-BOOK-ID     PIC 9(004)                     .
                   15  TSQ-BOOK-TITLE  PIC X(067)                     .
           05  TS-QUEUE-NAME.
               10  FILLER              PIC X(4)   VALUE 'DAR1'        .
               10  TS-TERMINAL-ID      PIC X(4)                       .

           05  WS-ALTER-RECORD.
               10  WS-INPUT-S          PIC X(1)                       .
               10  WS-INPUT-U          PIC X(1)                       .
               10  WS-INPUT-D          PIC X(1)                       .

      *FOR BOOK02DB-----------------------------------------------------
           05  TS-QUEUE-NAME-2.
               10  FILLER              PIC X(4)   VALUE 'DAR2'        .
               10  TS-TERMINAL-ID-2    PIC X(4)                       .
           05  INFO-ITEM               PIC S9(4)  COMP  VALUE +1      .
           05  INFO-NUMITEM            PIC S9(04) COMP  VALUE  0      .
      *-----------------------------------------------------------------

           05  WS-DEL-ID               PIC 9(4)                       .

       01  WS-VARS.
           05  WS-TRANSID              PIC X(004) VALUE 'DAR1'        .
           05  WS-MAP                  PIC X(007) VALUE 'BOOKMP1'     .
           05  WS-MAPSET               PIC X(007) VALUE 'BOOKST1'     .
           05  TSQ-SUB                 PIC S9(04) COMP                .
           05  WS-DUMMY-2              PIC X(050)                     .
           05  BOOK-ITEM-X             PIC 9(03)                      .
           05  BOOK-NUMITEM-X          PIC 9(03)                     .
           05  RESPONSE-CODE           PIC S9(04) COMP                .
           05  END-OF-SESSION          PIC X(13) VALUE 'SESSION ENDED'.
           05  WS-MESSAGE.
               10  FILLER              PIC X(010) VALUE 'ERROR IN: '  .
               10  ERR-LOC             PIC X(030)                     .
               10  FILLER              PIC X(012) VALUE '|RESP CODE: '.
               10  ERR-CODE            PIC S9(04) COMP                .

       01  WS-DATE-VARS.
           05 WS-ABS-TIME              PIC S9(15) COMP-3              .
           05 WS-MMDDYYYY              PIC X(10)                      .
           05 WS-TIME                  PIC X(08)                      .

       01  FLAGS.
           05  SEND-FLAG               PIC X                          .
               88  SEND-ERASE                     VALUE '1'           .
               88  SEND-DATAONLY-CURSOR           VALUE '2'           .
               88  SEND-DATAONLY                  VALUE '3'           .

           05  SELECTION               PIC X                          .
               88 VALID-INPUT                     VALUE '1'           .
               88 INVALID-INPUT                   VALUE '2'           .

           05  ERROR-HANDLING          PIC X      VALUE 'N'           .
               88  SOME-ERROR                     VALUE 'Y'           .

           05  END-OF-FILE             PIC X                          .
               88 NO-MORE-RECORDS                 VALUE 'Y'           .

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE DCLBOOKS
           END-EXEC.

      *FOR SEARCH TSQ---------------------------------------------------
           EXEC SQL DECLARE SEARCH_CURSOR CURSOR FOR
                SELECT
                    BOOK_ID, TITLE
                FROM
                    IBMUSER.BOOKS
                WHERE
                    UPPER(TITLE) LIKE
                    "%" || LTRIM(RTRIM(:WS-SEARCH)) || "%"
                ORDER BY 1
           END-EXEC.
      *-----------------------------------------------------------------

           COPY BOOKST1.
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
                   15  LS-TSQ-BOOK-ID    PIC 9(004)                   .
                   15  LS-TSQ-BOOK-TITLE PIC X(067)                   .
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
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       A000O-MAIN-PROCESS.
           EVALUATE TRUE
               WHEN EIBCALEN = 0
                    PERFORM A1000-LINK-PROG    THRU A1000-EXIT
                    PERFORM A2000-SEND-PAGE1   THRU A2000-EXIT

               WHEN EIBCALEN > 0
                    MOVE DFHCOMMAREA             TO WS-COMMAREA
                    MOVE LOW-VALUES              TO BOOKMP1I
                    MOVE LOW-VALUES              TO BOOKMP1O
                    INITIALIZE FLAGS
                    PERFORM    A3000-EVALUATE  THRU A3000-EXIT
           END-EVALUATE
           .
       A0000-EXIT.
           EXIT
           .
       A1000-LINK-PROG.
           EXEC CICS
                LINK PROGRAM('TSQDB2')
                     COMMAREA(WS-COMMAREA)
                     RESP(RESPONSE-CODE)
           END-EXEC
           .
       A1000-EXIT.
           EXIT
           .
       A2000-SEND-PAGE1.
           IF RESPONSE-CODE = ZEROS
              MOVE LOW-VALUES TO BOOKMP1I
              MOVE LOW-VALUES TO BOOKMP1O
              PERFORM Z1000-READQ-TS            THRU Z1000-EXIT
              PERFORM Z2000-LOAD-MAP            THRU Z2000-EXIT
              MOVE    '1'                         TO SEND-FLAG
              PERFORM Z3000-SEND-MAP            THRU Z3000-EXIT
              PERFORM Z4000-RETURN-TRANSID      THRU Z4000-EXIT
           ELSE
              MOVE 'Y' TO ERROR-HANDLING
              MOVE 'ERROR IN A0000-MAIN-PROCESS.' TO ERR-LOC
              MOVE RESPONSE-CODE                  TO ERR-CODE
              PERFORM XXXX-ERROR-HANDLING
           END-IF
           .
       A2000-EXIT.
           EXIT
           .
       A3000-EVALUATE.
           IF EIBTRNID NOT = WS-TRANSID
              IF WS-DEL-ID = LOW-VALUE
                 CONTINUE
              ELSE
                 INITIALIZE WS-DEL-ID
                 PERFORM A1000-LINK-PROG    THRU A1000-EXIT
              END-IF

              PERFORM A3100-PAGING-PROCESS  THRU A3100-EXIT

           ELSE
              EVALUATE TRUE
                  WHEN EIBAID = DFHPF3
                       PERFORM A1000-LINK-PROG          THRU A1000-EXIT
                       PERFORM A2000-SEND-PAGE1         THRU A2000-EXIT

                  WHEN EIBAID = DFHPF4
                       PERFORM VARYING TSQ-SUB FROM 1 BY 1
                                              UNTIL TSQ-SUB > 15
                          MOVE ATTR-PROT TO SELBK01A(TSQ-SUB)
                       END-PERFORM

                       MOVE    ATTR-PROT                  TO SEARCHA
                       MOVE    '3'                        TO SEND-FLAG
                       PERFORM Z3000-SEND-MAP           THRU Z3000-EXIT

                       EXEC CICS
                            XCTL PROGRAM('INSERT01')
                                 COMMAREA(WS-COMMAREA)
                       END-EXEC

                  WHEN EIBAID = DFHPF5
                       EXEC CICS
                            LINK PROGRAM('GENRPT01')
                                 COMMAREA(WS-COMMAREA)
                                 RESP(RESPONSE-CODE)
                       END-EXEC

                  WHEN EIBAID = DFHPF7
                       SUBTRACT 1                       FROM BOOK-ITEM
                       PERFORM  A3100-PAGING-PROCESS    THRU A3100-EXIT

                  WHEN EIBAID = DFHPF8
                       ADD      1                         TO BOOK-ITEM
                       PERFORM  A3100-PAGING-PROCESS    THRU A3100-EXIT

                  WHEN EIBAID = DFHPF12
                       PERFORM Z5000-DELETEQ            THRU Z5000-EXIT
                       PERFORM XXXX-SEND-TERMINATION-MESSAGE

                       EXEC CICS
                            RETURN
                       END-EXEC

                  WHEN EIBAID = DFHENTER
                       PERFORM A3200-RECEIVE-MAP        THRU A3200-EXIT
                       PERFORM A3300-SEARCH-SELECT      THRU A3300-EXIT

                  WHEN OTHER
                       PERFORM  Z1000-READQ-TS          THRU Z1000-EXIT
                       PERFORM  Z2000-LOAD-MAP          THRU Z2000-EXIT
                       MOVE    'INVALID KEY PRESSED.'     TO MESSAGEO
                       MOVE    '1'                        TO SEND-FLAG
                       PERFORM Z3000-SEND-MAP           THRU Z3000-EXIT
                       PERFORM Z4000-RETURN-TRANSID     THRU Z4000-EXIT
              END-EVALUATE
           END-IF
           .
       A3000-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *PAGING PARAGRAPH-------------------------------------------------
       A3100-PAGING-PROCESS.
           EVALUATE TRUE
               WHEN BOOK-ITEM = 0
                    MOVE  +1                                TO BOOK-ITEM
                    MOVE 'YOU ARE ALREADY ON FIRST PAGE.'   TO MESSAGEO

               WHEN BOOK-ITEM > BOOK-NUMITEM
                    MOVE  BOOK-NUMITEM                      TO BOOK-ITEM
                    MOVE 'YOU HAVE REACHED THE END OF LIST.'TO MESSAGEO

               WHEN OTHER
                    CONTINUE
           END-EVALUATE

           PERFORM  Z1000-READQ-TS         THRU Z1000-EXIT
           PERFORM  Z2000-LOAD-MAP         THRU Z2000-EXIT
           MOVE     '1'                      TO SEND-FLAG
           PERFORM  Z3000-SEND-MAP         THRU Z3000-EXIT
           PERFORM  Z4000-RETURN-TRANSID   THRU Z4000-EXIT
           .
       A3100-EXIT.
           EXIT
           .
       A3200-RECEIVE-MAP.
           EXEC CICS
               RECEIVE MAP(WS-MAP)
                       MAPSET(WS-MAPSET)
                       INTO(BOOKMP1I)
                       RESP(RESPONSE-CODE)
           END-EXEC
           .
       A3200-EXIT.
           EXIT
           .
       A3300-SEARCH-SELECT.
           IF SEARCHI = LOW-VALUE OR SPACES
              PERFORM B1000-SELECTED-ID THRU B1000-EXIT
                      VARYING TSQ-SUB FROM 1 BY 1
                        UNTIL TSQ-SUB > 15 OR INVALID-INPUT

              PERFORM B2000-XCTL THRU B2000-EXIT

           ELSE
              PERFORM    Z5000-DELETEQ THRU Z5000-EXIT
              MOVE LOW-VALUE TO TSQ-PAGE-ENTRY
              MOVE LOW-VALUE TO WS-TABLE-SELECT
              INITIALIZE BOOK-ITEM
              INITIALIZE BOOK-NUMITEM
              INITIALIZE TSQ-SUB

              MOVE       SEARCHI         TO WS-SEARCH
              MOVE       EIBTRMID        TO TS-TERMINAL-ID

              EXEC SQL
                   OPEN SEARCH_CURSOR
              END-EXEC

              PERFORM C1000-FETCH-DATA THRU C1000-EXIT
                                      UNTIL NO-MORE-RECORDS

              EXEC SQL
                   CLOSE SEARCH_CURSOR
              END-EXEC

              MOVE 1 TO BOOK-ITEM

              PERFORM Z1000-READQ-TS       THRU Z1000-EXIT
              PERFORM Z2000-LOAD-MAP       THRU Z2000-EXIT
              MOVE    '1'                    TO SEND-FLAG
              PERFORM Z3000-SEND-MAP       THRU Z3000-EXIT
              PERFORM Z4000-RETURN-TRANSID THRU Z4000-EXIT
           END-IF
           .
       A3300-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *SELECTED-ID PARAGRAPH--------------------------------------------
       B1000-SELECTED-ID.
           EVALUATE TRUE
              WHEN SELBK01I(TSQ-SUB) = 'S'
                   MOVE TSQ-BOOK-ID(TSQ-SUB) TO WS-SELECTED-ID(TSQ-SUB)
                   MOVE '1'                  TO SELECTION
                   MOVE SELBK01I(TSQ-SUB)    TO WS-INPUT-S

              WHEN SELBK01I(TSQ-SUB) = 'U'
                   MOVE TSQ-BOOK-ID(TSQ-SUB) TO WS-UPDATE-ID(TSQ-SUB)
                   MOVE '1'                  TO SELECTION
                   MOVE SELBK01I(TSQ-SUB)    TO WS-INPUT-U

              WHEN SELBK01I(TSQ-SUB) = 'D'
                   MOVE TSQ-BOOK-ID(TSQ-SUB) TO WS-DELETE-ID(TSQ-SUB)
                   MOVE '1'                  TO SELECTION
                   MOVE SELBK01I(TSQ-SUB)    TO WS-INPUT-D

              WHEN SELBK01I(TSQ-SUB) = LOW-VALUE OR SPACES
                   MOVE 'DUMMY'              TO WS-DUMMY-2

              WHEN OTHER
                   MOVE '2'                  TO SELECTION
                   MOVE -1                   TO SELBK01L(TSQ-SUB)
           END-EVALUATE
           .
       B1000-EXIT.
           EXIT
           .
       B2000-XCTL.
           EVALUATE TRUE
              WHEN VALID-INPUT
                   PERFORM VARYING TSQ-SUB FROM 1 BY 1
                                   UNTIL TSQ-SUB > 15
                        MOVE ATTR-PROT  TO SELBK01A(TSQ-SUB)
                   END-PERFORM

                   MOVE ATTR-PROT  TO SEARCHA
                   MOVE    '3'  TO SEND-FLAG
                   PERFORM Z3000-SEND-MAP  THRU Z3000-EXIT

                   EXEC CICS
                        XCTL PROGRAM('BOOK02DB')
                             COMMAREA(WS-COMMAREA)
                   END-EXEC

              WHEN INVALID-INPUT
                   MOVE    '2'  TO SEND-FLAG
                   MOVE    'INVALID SELECT.'       TO   MESSAGEO

              WHEN OTHER
                   MOVE    '1'  TO SEND-FLAG
                   MOVE    'PLEASE ENTER "S" TO SELECT A BOOK.'
                                                   TO MESSAGEO
           END-EVALUATE

           PERFORM Z1000-READQ-TS          THRU Z1000-EXIT
           PERFORM Z2000-LOAD-MAP          THRU Z2000-EXIT
           PERFORM Z3000-SEND-MAP          THRU Z3000-EXIT
           PERFORM Z4000-RETURN-TRANSID    THRU Z4000-EXIT
           .
       B2000-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *LOADING TS QUEUE FOR SEARCH--------------------------------------
       C1000-FETCH-DATA.
           EXEC SQL
                FETCH SEARCH_CURSOR INTO :BOOKS-BOOK-ID,
                                         :BOOKS-TITLE
           END-EXEC

           EVALUATE TRUE
             WHEN SQLCODE = 0
                  PERFORM C1100-LOAD-PROCESS        THRU C1100-EXIT
             WHEN SQLCODE = 100
                  MOVE    'Y'                         TO END-OF-FILE
                  PERFORM C1200-LOAD-PROCESS        THRU C1200-EXIT
             WHEN OTHER
                  MOVE    'C1000-FETCH-DATA'          TO ERR-LOC
                  MOVE     SQLCODE                    TO ERR-CODE
                  PERFORM  XXXX-ERROR-HANDLING
           END-EVALUATE
           .
       C1000-EXIT.
           EXIT
           .
       C1100-LOAD-PROCESS.
           IF TSQ-SUB = 15
              PERFORM C1300-WRITEQ-TS    THRU C1300-EXIT
              MOVE BOOKS-BOOK-ID           TO TSQ-BOOK-ID(TSQ-SUB)
              MOVE BOOKS-TITLE-TEXT        TO TSQ-BOOK-TITLE(TSQ-SUB)
           ELSE
              ADD  1                       TO TSQ-SUB
              MOVE BOOKS-BOOK-ID           TO TSQ-BOOK-ID(TSQ-SUB)
              MOVE BOOKS-TITLE-TEXT        TO TSQ-BOOK-TITLE(TSQ-SUB)
           END-IF
           .
       C1100-EXIT.
           EXIT
           .
       C1200-LOAD-PROCESS.
           IF TSQ-SUB > 0
              PERFORM C1300-WRITEQ-TS    THRU C1300-EXIT
           END-IF
           .
       C1200-EXIT.
           EXIT
           .
       C1300-WRITEQ-TS.
           EXEC CICS
                WRITEQ TS QUEUE    (TS-QUEUE-NAME)
                          FROM     (TSQ-PAGE-ENTRY)
                          NUMITEMS (BOOK-NUMITEM)
                          RESP     (RESPONSE-CODE)
           END-EXEC

           IF RESPONSE-CODE = ZEROS
              MOVE LOW-VALUE     TO TSQ-PAGE-ENTRY
              MOVE 1             TO TSQ-SUB
           ELSE
              MOVE 'Y' TO ERROR-HANDLING
              MOVE 'ERROR IN C1300-WRITEQ-TS.' TO ERR-LOC
              MOVE RESPONSE-CODE               TO ERR-CODE
              PERFORM XXXX-ERROR-HANDLING
           END-IF
           .
       C1300-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *DISPLAY MAP PARAGRAPH--------------------------------------------
       Z1000-READQ-TS.
           EXEC CICS
                READQ TS QUEUE (TS-QUEUE-NAME)
                         INTO  (TSQ-PAGE-ENTRY)
                         ITEM  (BOOK-ITEM)
                         RESP  (RESPONSE-CODE)
           END-EXEC.

           IF RESPONSE-CODE > ZEROS
              MOVE 'Y' TO ERROR-HANDLING
              MOVE 'ERROR IN Z1000-READQ-TS.'  TO ERR-LOC
              MOVE RESPONSE-CODE               TO ERR-CODE
              PERFORM XXXX-ERROR-HANDLING
           END-IF
           .
       Z1000-EXIT.
           EXIT
           .
       Z2000-LOAD-MAP.
           PERFORM Z2100-MOVE-REC-ELEMENT THRU  Z2100-EXIT
                   VARYING TSQ-SUB FROM 1 BY 1 UNTIL TSQ-SUB > 15
           .
       Z2000-EXIT.
           EXIT
           .
       Z2100-MOVE-REC-ELEMENT.
           IF TSQ-BOOK-ID(TSQ-SUB) NOT = LOW-VALUE
              MOVE TSQ-BOOK-ID(TSQ-SUB)    TO BKID01O(TSQ-SUB)
              MOVE TSQ-BOOK-TITLE(TSQ-SUB) TO BOOK01O(TSQ-SUB)
           ELSE
              MOVE ATTR-PROT  TO SELBK01A(TSQ-SUB)
           END-IF
           .
       Z2100-EXIT.
           EXIT
           .
       Z3000-SEND-MAP.
           PERFORM Z6000-ASKTIME THRU Z6000-EXIT
           MOVE    WS-MMDDYYYY     TO MDATEO
           MOVE    WS-TIME         TO MTIMEO
           MOVE    BOOK-ITEM       TO BOOK-ITEM-X
           MOVE    BOOK-NUMITEM    TO BOOK-NUMITEM-X
           MOVE    BOOK-ITEM-X     TO ITEMO
           MOVE    BOOK-NUMITEM-X  TO NUMITEMO

           EVALUATE TRUE
              WHEN SEND-ERASE
                   EXEC CICS
                        SEND MAP(WS-MAP)
                             MAPSET(WS-MAPSET)
                             FROM(BOOKMP1O)
                             ERASE
                   END-EXEC

              WHEN SEND-DATAONLY-CURSOR
                   EXEC CICS
                        SEND MAP(WS-MAP)
                             MAPSET(WS-MAPSET)
                             FROM(BOOKMP1O)
                             CURSOR
                   END-EXEC

              WHEN SEND-DATAONLY
                   EXEC CICS
                        SEND MAP(WS-MAP)
                             MAPSET(WS-MAPSET)
                             FROM(BOOKMP1O)
                             DATAONLY
                   END-EXEC

              WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       Z3000-EXIT.
           EXIT
           .
       Z4000-RETURN-TRANSID.
           EXEC CICS
               RETURN TRANSID(WS-TRANSID)
                      COMMAREA(WS-COMMAREA)
           END-EXEC
           .
       Z4000-EXIT.
           EXIT
           .
       Z5000-DELETEQ.
           EXEC CICS DELETEQ TS
                QUEUE (TS-QUEUE-NAME)
           END-EXEC

           MOVE LOW-VALUES TO TSQ-PAGE-ENTRY
           .
       Z5000-EXIT.
           EXIT
           .
       Z6000-ASKTIME.
           EXEC CICS ASKTIME
                ABSTIME(WS-ABS-TIME)
           END-EXEC

           EXEC CICS FORMATTIME
                ABSTIME(WS-ABS-TIME)
                MMDDYYYY(WS-MMDDYYYY)
                DATESEP('-')
           END-EXEC

           EXEC CICS FORMATTIME
                ABSTIME(WS-ABS-TIME)
                TIME(WS-TIME)
                TIMESEP(':')
           END-EXEC
           .
       Z6000-EXIT.
           EXIT
           .
       XXXX-SEND-TERMINATION-MESSAGE.
           EXEC CICS
               SEND TEXT FROM(END-OF-SESSION)
                         ERASE
                         FREEKB
           END-EXEC
           .
       XXXX-ERROR-HANDLING.
           IF SOME-ERROR
              PERFORM Z5000-DELETEQ THRU Z5000-EXIT

              EXEC CICS
                  SEND TEXT FROM(WS-MESSAGE)
                            ERASE
                            FREEKB
              END-EXEC

              EXEC CICS
                   RETURN
              END-EXEC
           END-IF
           .
      *-----------------------------------------------------------------

      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    TSQDB2.
       AUTHOR.        DARWIN TERCENIO.
       DATE-WRITTEN.  04.23.2020.
      *-----------------------
      *****************************************************************
      *PROGRAM DESCRIPTION.                                           *
      * --- THIS PROGRAM WILL BE EXECUTED USING LINK                  *
      * PROCESS:                                                      *
      *    -- READ IBMUSER.BOOKS UNTIL END OF FILE                    *
      *    -- FOR EVERY 15 RECORD FETCHED, PERFORM WRITEQ             *
      *    -- IF END OF FILE REACHED, RETURN TO BOOK01                *
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
                   15  WS-SELECTED-ID          PIC 9(004)             .
           05  WS-TABLE-UPDATE.
               10  WS-UPDATE OCCURS 15 TIMES.
                   15  WS-UPDATE-ID  PIC 9(004)                     .
           05  WS-TABLE-DELETE.
               10  WS-DELETE OCCURS 15 TIMES.
                   15  WS-DELETE-ID  PIC 9(004)                     .
           05  WS-SEARCH               PIC X(020)                     .
           05  BOOK-ITEM               PIC S9(4)  COMP  VALUE +1      .
           05  BOOK-NUMITEM            PIC S9(04) COMP  VALUE  0      .
           05  TSQ-PAGE-ENTRY.
               10  TSQ-PAGE OCCURS 15 TIMES.
                   15  TSQ-BOOK-ID         PIC 9(004)                 .
                   15  TSQ-BOOK-TITLE      PIC X(067)                 .
           05  TS-QUEUE-NAME.
               10  FILLER              PIC X(4)   VALUE 'DAR1'        .
               10  TS-TERMINAL-ID      PIC X(4)                       .

       01  SWITCHES.
           05  END-OF-FILE             PIC X    VALUE 'N'             .
               88 NO-MORE-RECORDS               VALUE 'Y'             .

           05  ERROR-HANDLING          PIC X    VALUE 'N'             .
               88  SOME-ERROR                   VALUE 'Y'             .

       01  WS-VARS.
           05  TSQ-SUB                 PIC S9(04) COMP                .
           05  RESPONSE-CODE           PIC S9(04) COMP                .
           05  WS-MESSAGE.
               10  FILLER              PIC X(010) VALUE 'ERROR IN: '  .
               10  ERR-LOC             PIC X(030)                     .
               10  FILLER              PIC X(008) VALUE '| CODE: '    .
               10  ERR-CODE            PIC S9(04) COMP                .

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE DCLBOOKS
           END-EXEC.

      *FOR LOADING TSQ--------------------------------------------------
           EXEC SQL DECLARE  C1_CURSOR CURSOR FOR
                SELECT BOOK_ID,
                       TITLE
                FROM   IBMUSER.BOOKS
                ORDER BY 1
           END-EXEC.
      *-----------------------------------------------------------------

           COPY DFHAID.

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

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
       A0000-MAIN-PROCESS.
           MOVE EIBTRMID TO TS-TERMINAL-ID
           INITIALIZE BOOK-NUMITEM
           INITIALIZE TSQ-SUB
           INITIALIZE WS-TABLE-SELECT

           EXEC CICS DELETEQ TS
                QUEUE (TS-QUEUE-NAME)
                RESP  (RESPONSE-CODE)
           END-EXEC

           EXEC SQL
                OPEN C1_CURSOR
           END-EXEC

      *    PERFORM A1000-FETCH-DATA  THRU A1000-EXIT
      *                             UNTIL NO-MORE-RECORDS

           PERFORM A1000-FETCH-DATA  THRU A1000-EXIT
                                    UNTIL BOOK-NUMITEM = 3

           EXEC SQL
                CLOSE C1_CURSOR
           END-EXEC

           MOVE WS-COMMAREA TO DFHCOMMAREA

           EXEC CICS
                RETURN
           END-EXEC
           .
      *-----------------------------------------------------------------
      *LOADING TS QUEUE-------------------------------------------------
       A1000-FETCH-DATA.
           EXEC SQL
                FETCH C1_CURSOR INTO :BOOKS-BOOK-ID,
                                     :BOOKS-TITLE
           END-EXEC

           EVALUATE TRUE
             WHEN SQLCODE = 0
                  PERFORM A1100-LOAD-PROCESS        THRU A1100-EXIT
             WHEN SQLCODE = 100
                  MOVE    'Y'                         TO END-OF-FILE
                  PERFORM A1200-LOAD-PROCESS        THRU A1200-EXIT
             WHEN OTHER
                  MOVE    'A1000-FETCH-DATA'          TO  ERR-LOC
                  MOVE     SQLCODE                    TO  ERR-CODE
                  PERFORM  XXXX-ERROR-HANDLING
           END-EVALUATE
           .
       A1000-EXIT.
           EXIT
           .
       A1100-LOAD-PROCESS.
           IF TSQ-SUB = 15
              PERFORM A1300-WRITEQ-TS    THRU A1300-EXIT
              MOVE BOOKS-BOOK-ID           TO TSQ-BOOK-ID(TSQ-SUB)
              MOVE BOOKS-TITLE-TEXT        TO TSQ-BOOK-TITLE(TSQ-SUB)
           ELSE
              ADD  1                       TO TSQ-SUB
              MOVE BOOKS-BOOK-ID           TO TSQ-BOOK-ID(TSQ-SUB)
              MOVE BOOKS-TITLE-TEXT        TO TSQ-BOOK-TITLE(TSQ-SUB)
           END-IF
           .
       A1100-EXIT.
           EXIT
           .
       A1200-LOAD-PROCESS.
           IF TSQ-SUB > 0
              PERFORM A1300-WRITEQ-TS    THRU A1300-EXIT
           END-IF
           .
       A1200-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *WRITING QUEUE----------------------------------------------------
       A1300-WRITEQ-TS.
           EXEC CICS
                WRITEQ TS QUEUE    (TS-QUEUE-NAME)
                          FROM     (TSQ-PAGE-ENTRY)
                          NUMITEMS (BOOK-NUMITEM)
                          RESP     (RESPONSE-CODE)
           END-EXEC.

           IF RESPONSE-CODE = ZEROS
              MOVE LOW-VALUE     TO TSQ-PAGE-ENTRY
              MOVE 1             TO TSQ-SUB
           ELSE
              MOVE 'Y' TO ERROR-HANDLING
              MOVE 'ERROR IN A1300-WRITEQ-TS.' TO ERR-LOC
              MOVE RESPONSE-CODE               TO ERR-CODE
              PERFORM XXXX-ERROR-HANDLING
           END-IF
           .
       A1300-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *ERROR HANDLING---------------------------------------------------
       XXXX-ERROR-HANDLING.
           IF SOME-ERROR
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
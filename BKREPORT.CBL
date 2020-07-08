      *-----------------------
      *--- CBL COMPILE LIST
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    BKREPORT.
       AUTHOR.        DARWIN TERCENIO.
       DATE-WRITTEN.  05.21.2020.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------*
      *---------- FILE       --------- JCL    -*
      *---------- DESCRIPTOR --------- DDNAME -*
      *----------------------------------------*
           SELECT PRT-BOOK ASSIGN TO BOOKOUT.

      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRT-BOOK
           RECORD CONTAINS 230
           RECORDING MODE F.
       01  OP-FIELDS PIC X(230).

       WORKING-STORAGE SECTION.

       01  WS-BOOKS.
           05  WS-BOOK-ID            PIC 9(004)                       .
           05  FILLER                PIC X(004) VALUE ALL SPACES      .
           05  WS-TITLE              PIC X(166)                       .
           05  FILLER                PIC X(001) VALUE ALL SPACES      .
           05  WS-TOTAL-PAGES        PIC 9(004)                       .
           05  FILLER                PIC X(002) VALUE ALL SPACES      .
           05  WS-RATING             PIC 9.99                         .
           05  FILLER                PIC X(003) VALUE ALL SPACES      .
           05  WS-ISBN               PIC X(013)                       .
           05  FILLER                PIC X(001) VALUE ALL SPACES      .
           05  WS-PUBLISHED-DATE     PIC X(010)                       .
           05  FILLER                PIC X(005) VALUE ALL SPACES      .
           05  WS-PUBLISHER-ID       PIC 9(004)                       .
           05  FILLER                PIC X(009) VALUE ALL SPACES      .

       01  WS-PAGE-BREAKS.
           05  WS-SPACES           PIC X(230) VALUE ALL SPACES        .
           05  WS-BREAKER          PIC X(230) VALUE ALL '-'           .
           05  WS-HEADER.
               10  FILLER          PIC X(007) VALUE 'BOOK ID'         .
               10  FILLER          PIC X(001) VALUE ALL SPACES        .
               10  FILLER          PIC X(005) VALUE 'TITLE'           .
               10  FILLER          PIC X(162) VALUE ALL SPACES        .
               10  FILLER          PIC X(004) VALUE 'PAGE'            .
               10  FILLER          PIC X(002) VALUE ALL SPACES        .
               10  FILLER          PIC X(006) VALUE 'RATING'          .
               10  FILLER          PIC X(001) VALUE ALL SPACES        .
               10  FILLER          PIC X(004) VALUE 'ISBN'            .
               10  FILLER          PIC X(010) VALUE ALL SPACES        .
               10  FILLER          PIC X(014) VALUE 'PUBLISHED DATE'  .
               10  FILLER          PIC X(001) VALUE ALL SPACES        .
               10  FILLER          PIC X(012) VALUE 'PUBLISHER ID'    .
               10  FILLER          PIC X(001) VALUE ALL SPACES        .
           05  WS-FOOTER.
               10  FILLER          PIC X(108) VALUE ALL '-'           .
               10  FILLER          PIC X(015) VALUE 'NOTHING FOLLOWS' .
               10  FILLER          PIC X(107) VALUE ALL '-'           .

       01  WS-VARS.
           05  COUNTER             PIC  9(04) VALUE 0                 .
           05  LAST-REC            PIC  9                             .
               88  EOF                        VALUE 1                 .
           05  ERROR-HANDLING          PIC X                          .
               88  SOME-ERROR                   VALUE 'Y'             .
           05  WS-MESSAGE.
               10  FILLER              PIC X(10) VALUE 'ERROR IN: '  .
               10  ERR-LOC             PIC X(30)                     .
               10  FILLER              PIC X(06) VALUE 'CODE: '      .
               10  ERR-CODE            PIC S9(4) COMP                .


           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
              INCLUDE DCLBOOKS
           END-EXEC.

           EXEC SQL DECLARE  C1_CURSOR CURSOR FOR
                SELECT BOOK_ID,
                       TITLE,
                       TOTAL_PAGES,
                       RATING,
                       ISBN,
                       PUBLISHER_DATE,
                       PUBLISHER_ID
                FROM   IBMUSER.BOOKS
                ORDER BY 1
           END-EXEC.

      *    COPY DFHAID.

      *------------------
       PROCEDURE DIVISION.
      *------------------
       0000-MAIN-PROCESS.
           DISPLAY 'START 0000-MAIN-PROCESS'.
           PERFORM 1000-OPEN-SQL  THRU 1000-EXIT.
           PERFORM 2000-FETCH     THRU 2000-EXIT.
           PERFORM 3000-CLOSE-SQL THRU 3000-EXIT.
           DISPLAY 'END 0000-MAIN-PROCESS'.
           STOP RUN.
       0000-EXIT.
           EXIT
           .
       1000-OPEN-SQL.
           DISPLAY 'START 1000-OPEN-SQL'

           EXEC SQL
                OPEN C1_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
              DISPLAY 'ERROR OPEN CURSOR'
           ELSE
              CONTINUE
           END-IF

           OPEN OUTPUT PRT-BOOK

           DISPLAY 'END 1000-OPEN-SQL'
           .
       1000-EXIT.
           EXIT
           .
       2000-FETCH.
           DISPLAY 'START 2000-FETCH'
           PERFORM 2100-FETCH-DATA THRU 2100-EXIT UNTIL EOF
           DISPLAY 'END 2000-FETCH'
           .
       2000-EXIT.
           EXIT
           .
       2100-FETCH-DATA.
           EXEC SQL
                FETCH C1_CURSOR INTO :BOOKS-BOOK-ID
                                    ,:BOOKS-TITLE
                                    ,:BOOKS-TOTAL-PAGES
                                    ,:BOOKS-RATING
                                    ,:BOOKS-ISBN
                                    ,:BOOKS-PUBLISHER-DATE
                                    ,:BOOKS-PUBLISHER-ID
           END-EXEC

           EVALUATE TRUE
               WHEN SQLCODE = 0
                    DISPLAY 'SQLCODE 0'
                    ADD     1                       TO COUNTER
                    MOVE    BOOKS-BOOK-ID           TO WS-BOOK-ID
                    MOVE    BOOKS-TITLE-TEXT        TO WS-TITLE
                    MOVE    BOOKS-TOTAL-PAGES       TO WS-TOTAL-PAGES
                    MOVE    BOOKS-RATING            TO WS-RATING
                    MOVE    BOOKS-ISBN-TEXT         TO WS-ISBN
                    MOVE    BOOKS-PUBLISHER-DATE    TO WS-PUBLISHED-DATE
                    MOVE    BOOKS-PUBLISHER-ID      TO WS-PUBLISHER-ID
                    PERFORM 2110-WRITE-PROCESS    THRU 2110-EXIT
               WHEN SQLCODE = 100
                    DISPLAY 'SQLCODE 100'
                    MOVE    WS-SPACES               TO OP-FIELDS
                    WRITE   OP-FIELDS
                    MOVE    WS-FOOTER               TO OP-FIELDS
                    WRITE   OP-FIELDS
                    MOVE    1                       TO LAST-REC
               WHEN OTHER
                    MOVE    1                       TO LAST-REC
                    DISPLAY 'SQLCODE: ' SQLCODE
                    MOVE   '2100-FETCH-DATA'        TO ERR-LOC
                    MOVE    SQLCODE                 TO ERR-CODE
      *             PERFORM XXXX-ERROR-HANDLING
           END-EVALUATE
           .
       2100-EXIT.
           EXIT
           .
       2110-WRITE-PROCESS.
           DISPLAY 'START 2110-WRITE-PROCESS'
           EVALUATE TRUE
               WHEN COUNTER = 1
                    MOVE  WS-BREAKER TO OP-FIELDS
                    WRITE OP-FIELDS
                    MOVE  WS-HEADER  TO OP-FIELDS
                    WRITE OP-FIELDS
                    MOVE  WS-BREAKER TO OP-FIELDS
                    WRITE OP-FIELDS

               WHEN COUNTER = 15
                    MOVE  0          TO COUNTER
                    MOVE  WS-BOOKS   TO OP-FIELDS
                    WRITE OP-FIELDS
                    MOVE  WS-SPACES  TO OP-FIELDS
                    WRITE OP-FIELDS
               WHEN OTHER
                    MOVE  WS-BOOKS   TO OP-FIELDS
                    WRITE OP-FIELDS
           END-EVALUATE
           DISPLAY 'END 2110-WRITE-PROCESS'
           .
       2110-EXIT.
           EXIT
           .
       3000-CLOSE-SQL.
           DISPLAY 'START 3000-CLOSE-SQL'
           CLOSE PRT-BOOK

           EXEC SQL
                CLOSE C1_CURSOR
           END-EXEC

           IF SQLCODE NOT = 0
              DISPLAY 'ERROR CLOSE CURSOR'
           ELSE
              CONTINUE
           END-IF

           DISPLAY 'END 3000-CLOSE-SQL'
           .
       3000-EXIT.
           EXIT
           .
      *XXXX-ERROR-HANDLING.
      *    IF SOME-ERROR
      *       EXEC CICS
      *           SEND TEXT FROM(WS-MESSAGE)
      *                     ERASE
      *                     FREEKB
      *       END-EXEC
      *
      *       EXEC CICS
      *            RETURN
      *       END-EXEC
      *    END-IF
      *    .

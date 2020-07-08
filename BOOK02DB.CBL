      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    BOOK02DB.
       AUTHOR.        DARWIN TERCENIO.
       DATE-WRITTEN.  04.23.2020.
      *--------------------
      *****************************************************************
      *PROGRAM DESCRIPTION.                                           *
      * --- THIS PROGRAM WILL BE EXECUTED USING XCTL                  *
      * FUNTIONS AVAILABLE:                                           *
      *    -- F7    > PREVIOUS PAGE                                   *
      *    -- F8    > NEXT PAGE                                       *
      *    -- F3    > GO BACK USING XCTL                              *
      *    -- F10   > EXIT                                            *
      *                                                               *
      * RELATED PROGRAMS: BOOK01DB                                    *
      *    -- BOOK01DB > CALLING PROGRAM                              *
      *                                                               *
      * --- THIS PROGRAM WILL SHOW ON SECOND MAP THE DETAILS OF THE   *
      * SELECTED BOOK FROM THE FIRST MAP                              *
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

       01  FLAGS.
           05  SEND-FLAG               PIC X                          .
               88  SEND-ERASE                   VALUE '1'             .

       01  SWITCH.
           05  ERROR-HANDLING          PIC X    VALUE 'N'             .
               88  SOME-ERROR                   VALUE 'Y'             .

       01  WS-VARS.
           05  WS-TRANSID              PIC X(04) VALUE 'DAR2'         .
           05  WS-MAP                  PIC X(07) VALUE 'BOOKMP2'      .
           05  WS-MAPSET               PIC X(07) VALUE 'BOOKST2'      .
           05  TSQ-SUB                 PIC S9(4) COMP                .
           05  RESPONSE-CODE           PIC S9(4) COMP                .
           05  WS-STARTBR              PIC  9(8)                     .
           05  END-OF-SESSION          PIC X(13) VALUE 'SESSION ENDED'.
           05  WS-INVALID              PIC X(13) VALUE 'INVALID TRNID'.
           05  WS-MESSAGE.
               10  FILLER              PIC X(10) VALUE 'ERROR IN: '  .
               10  ERR-LOC             PIC X(30)                     .
               10  FILLER              PIC X(06) VALUE 'CODE: '      .
               10  ERR-CODE            PIC S9(4) COMP                .
           05  WS-MESSAGE-DUMMY        PIC X(20)                     .

       01  TSQ-BOOK-INFO.
           05  WS-BOOKID               PIC 9(04)                     .
           05  WS-TITLE.
               10  WS-TITLE1           PIC X(35)                     .
               10  WS-TITLE2           PIC X(35)                     .
               10  WS-TITLE3           PIC X(35)                     .
               10  WS-TITLE4           PIC X(35)                     .
               10  WS-TITLE5           PIC X(26)                     .
           05  WS-PAGES                PIC 9(04)                     .
           05  WS-RATING               PIC 9.99                      .
           05  WS-ISBNNO               PIC 9(13)                     .
           05  WS-PUBDTE               PIC X(10)                     .
           05  WS-PUBID                PIC 9(04)                     .

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
                INCLUDE DCLBOOKS
           END-EXEC.

           COPY BOOKST2.
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

      *----------------------------------------------------------------
       PROCEDURE DIVISION.
      *----------------------------------------------------------------
       A0000-MAIN-PROCESS.
           EVALUATE TRUE
               WHEN EIBCALEN > 0
                    MOVE DFHCOMMAREA         TO WS-COMMAREA
                    MOVE LOW-VALUES          TO BOOKMP2I
                    MOVE LOW-VALUES          TO BOOKMP2O
                    INITIALIZE                  TSQ-BOOK-INFO
                    INITIALIZE                  DCLBOOKS
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
              PERFORM A1100-CHECK-SELECT THRU A1100-EXIT

           ELSE
              EVALUATE TRUE
                 WHEN EIBAID = DFHPF5
                    IF WS-INPUT-U = 'U'
                       EXEC CICS DELETEQ TS
                            QUEUE (TS-QUEUE-NAME-2)
                            RESP(RESPONSE-CODE)
                       END-EXEC

                       EXEC CICS
                           XCTL PROGRAM('UPDATE01')
                                COMMAREA(WS-COMMAREA)
                       END-EXEC

                    ELSE
                       MOVE 'NO SELECTED ID TO UPDATE.' TO MESSAGEO
                       MOVE '1' TO SEND-FLAG
                       PERFORM  Z0000-SEND-MAP      THRU Z0000-EXIT
                    END-IF


                 WHEN EIBAID = DFHPF6
                    IF WS-INPUT-D = 'D'
                       EXEC CICS DELETEQ TS
                            QUEUE (TS-QUEUE-NAME-2)
                            RESP(RESPONSE-CODE)
                       END-EXEC

                       EXEC CICS
                           XCTL PROGRAM('DELETE01')
                                COMMAREA(WS-COMMAREA)
                       END-EXEC

                    ELSE
                       MOVE 'NO SELECTED ID TO DELETE.' TO MESSAGEO
                       MOVE '1' TO SEND-FLAG
                       PERFORM  Z0000-SEND-MAP      THRU Z0000-EXIT
                    END-IF

                 WHEN EIBAID = DFHPF7
                    SUBTRACT 1 FROM INFO-ITEM
                    PERFORM A3000-PAGING-PROCESS THRU A3000-EXIT

                 WHEN EIBAID = DFHPF8
                    ADD 1 TO INFO-ITEM
                    PERFORM A3000-PAGING-PROCESS THRU A3000-EXIT

                 WHEN EIBAID = DFHPF12
                    EXEC CICS DELETEQ TS
                         QUEUE (TS-QUEUE-NAME-2)
                         RESP(RESPONSE-CODE)
                    END-EXEC

                    EXEC CICS
                         SEND TEXT FROM(END-OF-SESSION)
                                   ERASE
                                   FREEKB
                    END-EXEC

                    EXEC CICS
                         RETURN
                    END-EXEC

                 WHEN EIBAID = DFHPF3
                    MOVE LOW-VALUE TO WS-TABLE-SELECT
                    MOVE LOW-VALUE TO WS-ALTER-RECORD

                    EXEC CICS DELETEQ TS
                         QUEUE (TS-QUEUE-NAME-2)
                         RESP(RESPONSE-CODE)
                    END-EXEC

                    EXEC CICS
                        XCTL PROGRAM('BOOK01DB')
                             COMMAREA(WS-COMMAREA)
                    END-EXEC

                 WHEN OTHER
                    MOVE '1' TO SEND-FLAG
                    MOVE 'INVALID KEY PRESSED.'  TO MESSAGEO
                    PERFORM  Z0000-SEND-MAP      THRU Z0000-EXIT
              END-EVALUATE
           END-IF
           .
       A1000-EXIT.
           EXIT
           .
       A1100-CHECK-SELECT.
           IF WS-INPUT-S = 'S'
              PERFORM A1110-SELECTED-ID  THRU A1110-EXIT
           ELSE
              EXEC CICS DELETEQ TS
                   QUEUE (TS-QUEUE-NAME-2)
                   RESP(RESPONSE-CODE)
              END-EXEC

              IF WS-INPUT-U = 'U'
                 EXEC CICS
                      XCTL PROGRAM('UPDATE01')
                           COMMAREA(WS-COMMAREA)
                 END-EXEC
              ELSE
                 IF WS-INPUT-D = 'D'
                 EXEC CICS
                      XCTL PROGRAM('DELETE01')
                           COMMAREA(WS-COMMAREA)
                 END-EXEC
                 END-IF
              END-IF
           END-IF
           .
       A1100-EXIT.
           EXIT
           .
       A1110-SELECTED-ID.
           MOVE EIBTRMID TO TS-TERMINAL-ID-2

           PERFORM VARYING TSQ-SUB FROM 1 BY 1 UNTIL TSQ-SUB > 15
                IF WS-SELECTED-ID(TSQ-SUB) NOT = LOW-VALUE
                   MOVE WS-SELECTED-ID(TSQ-SUB) TO BOOKS-BOOK-ID
                   PERFORM A1200-GET-BOOK-INFO THRU A1200-EXIT
                END-IF
           END-PERFORM

           MOVE  1  TO INFO-ITEM

           PERFORM A2000-READQ-PARA     THRU A2000-EXIT

           MOVE SPACES    TO MESSAGEO
           MOVE '1'       TO SEND-FLAG

           PERFORM  Z0000-SEND-MAP      THRU Z0000-EXIT
           .
       A1110-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *WRITEQ PARAGRAPH-------------------------------------------------
       A1200-GET-BOOK-INFO.
           EXEC SQL
                SELECT BOOK_ID,
                       TITLE,
                       TOTAL_PAGES,
                       RATING,
                       ISBN,
                       PUBLISHER_DATE,
                       PUBLISHER_ID
                INTO  :BOOKS-BOOK-ID
                     ,:BOOKS-TITLE
                     ,:BOOKS-TOTAL-PAGES
                     ,:BOOKS-RATING
                     ,:BOOKS-ISBN
                     ,:BOOKS-PUBLISHER-DATE
                     ,:BOOKS-PUBLISHER-ID
                FROM   IBMUSER.BOOKS
                WHERE  BOOK_ID = :BOOKS-BOOK-ID
           END-EXEC

           EVALUATE TRUE
             WHEN SQLCODE = 0
                  MOVE    BOOKS-BOOK-ID           TO WS-BOOKID
                  MOVE    BOOKS-TITLE-TEXT        TO WS-TITLE
                  MOVE    BOOKS-TOTAL-PAGES       TO WS-PAGES
                  MOVE    BOOKS-RATING            TO WS-RATING
                  MOVE    BOOKS-ISBN-TEXT         TO WS-ISBNNO
                  MOVE    BOOKS-PUBLISHER-DATE    TO WS-PUBDTE
                  MOVE    BOOKS-PUBLISHER-ID      TO WS-PUBID
                  PERFORM A1300-WRITEQ-PARA     THRU A1300-EXIT

             WHEN SQLCODE = 100
                  MOVE 'NO DATA FOUND'            TO MESSAGEO

             WHEN OTHER
                  MOVE    'A1100-GET-BOOK-INFO.'  TO ERR-LOC
                  MOVE     SQLCODE                TO ERR-CODE
                  PERFORM  XXXX-ERROR-HANDLING
           END-EVALUATE
           .
       A1200-EXIT.
           EXIT
           .
       A1300-WRITEQ-PARA.
           EXEC CICS
                WRITEQ TS QUEUE    (TS-QUEUE-NAME-2)
                          FROM     (TSQ-BOOK-INFO)
                          NUMITEMS (INFO-NUMITEM)
                          RESP     (RESPONSE-CODE)
           END-EXEC.

           IF RESPONSE-CODE = ZEROS
              MOVE LOW-VALUE     TO TSQ-BOOK-INFO
              MOVE LOW-VALUE     TO DCLBOOKS
           ELSE
              MOVE 'Y' TO ERROR-HANDLING
              MOVE 'ERROR IN A1200-WRITEQ-PARA.' TO ERR-LOC
              MOVE RESPONSE-CODE                 TO ERR-CODE
              PERFORM XXXX-ERROR-HANDLING
           .
       A1300-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *READQ PARAGRAPH--------------------------------------------------
       A2000-READQ-PARA.
           EXEC CICS
                READQ TS QUEUE (TS-QUEUE-NAME-2)
                         INTO  (TSQ-BOOK-INFO)
                         ITEM  (INFO-ITEM)
                         RESP  (RESPONSE-CODE)
           END-EXEC.

           IF RESPONSE-CODE = ZEROS
              MOVE WS-BOOKID       TO   MBOOKIDO
              MOVE WS-TITLE1       TO   MTITLE1O
              MOVE WS-TITLE2       TO   MTITLE2O
              MOVE WS-TITLE3       TO   MTITLE3O
              MOVE WS-TITLE4       TO   MTITLE4O
              MOVE WS-TITLE5       TO   MTITLE5O
              MOVE WS-PAGES        TO   MPAGESO
              MOVE WS-RATING       TO   MRATINGO
              MOVE WS-ISBNNO       TO   MISBNNOO
              MOVE WS-PUBDTE       TO   MPUBDTEO
              MOVE WS-PUBID        TO   MPUBIDO

           ELSE
              MOVE 'Y' TO ERROR-HANDLING
              MOVE 'ERROR IN A2000-READQ-PARA.'  TO ERR-LOC
              MOVE RESPONSE-CODE                 TO ERR-CODE
              PERFORM XXXX-ERROR-HANDLING
           END-IF
           .
       A2000-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *PAGING OF SELECTED IDS-------------------------------------------
       A3000-PAGING-PROCESS.
           EVALUATE TRUE
             WHEN INFO-ITEM = 0
                  MOVE +1                     TO INFO-ITEM
                  MOVE 'NO MORE SELECTED ID.' TO MESSAGEO

             WHEN INFO-ITEM > INFO-NUMITEM
                  MOVE INFO-NUMITEM           TO INFO-ITEM
                  MOVE 'NO MORE SELECTED ID.' TO MESSAGEO

             WHEN OTHER
                  MOVE SPACES     TO MESSAGEO
           END-EVALUATE

           PERFORM A2000-READQ-PARA THRU A2000-EXIT
           MOVE '1' TO SEND-FLAG
           PERFORM Z0000-SEND-MAP      THRU Z0000-EXIT
           .
       A3000-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *SEND MAP PARAGRAPH-----------------------------------------------
       Z0000-SEND-MAP.
           IF SEND-ERASE
               EXEC CICS
                    SEND MAP(WS-MAP)
                         MAPSET(WS-MAPSET)
                         FROM(BOOKMP2O)
               END-EXEC

               EXEC CICS
                   RETURN TRANSID(WS-TRANSID)
                          COMMAREA(WS-COMMAREA)
               END-EXEC
           END-IF
           .
       Z0000-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *SUPPORT PARAGRAPH------------------------------------------------
       XXXX-ERROR-HANDLING.
           IF SOME-ERROR
              EXEC CICS DELETEQ TS
                   QUEUE (TS-QUEUE-NAME)
              END-EXEC

              EXEC CICS DELETEQ TS
                   QUEUE (TS-QUEUE-NAME-2)
              END-EXEC

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
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    UPDATE01.
       AUTHOR.        DARWIN TERCENIO.
       DATE-WRITTEN.  05.05.2020.
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

           05  WS-DEL-ID               PIC 9(4)                       .

       01  SWITCH.
           05  ERROR-HANDLING          PIC X    VALUE 'N'             .
               88  SOME-ERROR                   VALUE 'Y'             .

           05  UPDATE-CONFIRM          PIC X                          .
               88  UPDATE-YES                   VALUE 'Y'             .
               88  UPDATE-NO                    VALUE 'N'             .

       01  WS-VARS.
           05  WS-TRANSID              PIC X(04) VALUE 'TBL1'         .
           05  WS-MAP                  PIC X(07) VALUE 'UPDATEM'      .
           05  WS-MAPSET               PIC X(07) VALUE 'UPDATE1'      .
           05  TSQ-SUB                 PIC S9(4) COMP                .
           05  RESPONSE-CODE           PIC S9(4) COMP                .
           05  END-OF-SESSION          PIC X(13) VALUE 'SESSION ENDED'.
           05  WS-INVALID              PIC X(13) VALUE 'INVALID TRNID'.
           05  WS-DEFAULT-DATE         PIC X(10) VALUE '2020-01-01'   .
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

           COPY UPDATE1.
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
                    MOVE LOW-VALUES          TO UPDATEMI
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
              EXEC CICS SET
                  TERMINAL  (EIBTRMID)
                  RESP      (RESPONSE-CODE)
                  TRANIDONLY
              END-EXEC

              INITIALIZE INFO-ITEM
              INITIALIZE INFO-NUMITEM
              PERFORM    A1100-UPDATE-ID   THRU A1100-EXIT

           ELSE
              EVALUATE TRUE
                 WHEN EIBAID = DFHPF3
                      MOVE LOW-VALUE         TO WS-TABLE-SELECT
                      MOVE LOW-VALUE         TO WS-TABLE-UPDATE
                      MOVE LOW-VALUE         TO WS-TABLE-DELETE
                      MOVE LOW-VALUE         TO WS-ALTER-RECORD

                      EXEC CICS DELETEQ TS
                           QUEUE (TS-QUEUE-NAME-2)
                           RESP(RESPONSE-CODE)
                      END-EXEC

                      EXEC CICS
                          XCTL PROGRAM('BOOK01DB')
                               COMMAREA(WS-COMMAREA)
                      END-EXEC

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
                         PERFORM  Z0000-SEND-MAP        THRU Z0000-EXIT
                      END-IF

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

                 WHEN EIBAID = DFHENTER
                      EXEC CICS
                           RECEIVE MAP(WS-MAP)
                                   MAPSET(WS-MAPSET)
                                   INTO(UPDATEMI)
                                   RESP(RESPONSE-CODE)
                      END-EXEC

                      MOVE    CONFIRMI               TO UPDATE-CONFIRM
                      PERFORM A1200-UPDATE-CONFIRM THRU A1200-EXIT

                 WHEN OTHER
                      MOVE 'INVALID KEY PRESSED.'    TO MESSAGEO
                      PERFORM  Z0000-SEND-MAP      THRU Z0000-EXIT
              END-EVALUATE
           END-IF
           .
       A1000-EXIT.
           EXIT
           .
       A1100-UPDATE-ID.
           MOVE EIBTRMID TO TS-TERMINAL-ID-2

           PERFORM VARYING TSQ-SUB FROM 1 BY 1 UNTIL TSQ-SUB > 15
                IF WS-UPDATE-ID(TSQ-SUB) NOT = LOW-VALUE
                   MOVE WS-UPDATE-ID(TSQ-SUB)    TO BOOKS-BOOK-ID
                   PERFORM A1110-GET-BOOK-INFO THRU A1110-EXIT
                END-IF
           END-PERFORM

           MOVE 1          TO INFO-ITEM
           MOVE LOW-VALUES TO UPDATEMO

           PERFORM A2000-READQ-PARA     THRU A2000-EXIT
           PERFORM Z0000-SEND-MAP       THRU Z0000-EXIT
           .
       A1100-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
      *WRITEQ PARAGRAPH-------------------------------------------------
       A1110-GET-BOOK-INFO.
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
                    MOVE    BOOKS-BOOK-ID        TO WS-BOOKID
                    MOVE    BOOKS-TITLE-TEXT     TO WS-TITLE
                    MOVE    BOOKS-TOTAL-PAGES    TO WS-PAGES
                    MOVE    BOOKS-RATING         TO WS-RATING
                    MOVE    BOOKS-ISBN-TEXT      TO WS-ISBNNO
                    MOVE    BOOKS-PUBLISHER-DATE TO WS-PUBDTE
                    MOVE    BOOKS-PUBLISHER-ID   TO WS-PUBID
                    PERFORM A1120-WRITEQ-PARA  THRU A1120-EXIT

               WHEN SQLCODE = 100
                    MOVE 'NO DATA FOUND'           TO MESSAGEO

               WHEN OTHER
                    MOVE    'A1100-GET-BOOK-INFO.' TO ERR-LOC
                    MOVE    SQLCODE                TO ERR-CODE
                    PERFORM XXXX-ERROR-HANDLING
           END-EVALUATE
           .
       A1110-EXIT.
           EXIT
           .
       A1120-WRITEQ-PARA.
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
           END-IF
           .
       A1120-EXIT.
           EXIT
           .
      *-----------------------------------------------------------------
       A1200-UPDATE-CONFIRM.
           EVALUATE TRUE
               WHEN UPDATE-YES
                    MOVE    WS-DEL-ID            TO BOOKS-BOOK-ID
                    PERFORM A1210-MOVE-DATA    THRU A1210-EXIT
                    PERFORM A1220-UPDATE-DATA  THRU A1220-EXIT

               WHEN UPDATE-NO
                    CONTINUE

               WHEN OTHER
                    MOVE    SPACES               TO CONFIRMO
                    MOVE    'INVALID INPUT.'     TO MESSAGEO
                    PERFORM Z0000-SEND-MAP     THRU Z0000-EXIT
           END-EVALUATE

           ADD 1 TO INFO-ITEM

           IF INFO-ITEM <= INFO-NUMITEM
              MOVE LOW-VALUES                    TO UPDATEMI
              MOVE LOW-VALUES                    TO UPDATEMO
              INITIALIZE                            TSQ-BOOK-INFO
              INITIALIZE                            DCLBOOKS
              PERFORM A2000-READQ-PARA         THRU A2000-EXIT
              PERFORM Z0000-SEND-MAP           THRU Z0000-EXIT

           ELSE
              EXEC CICS DELETEQ TS
                   QUEUE (TS-QUEUE-NAME-2)
                   RESP(RESPONSE-CODE)
              END-EXEC

              MOVE LOW-VALUES                    TO WS-INPUT-U
              MOVE 'NO MORE SELECTED ID TO UPDATE. PRESS F3 OR F6.'
                                                 TO MESSAGEO
              PERFORM Z2000-MOVE-SPACES        THRU Z2000-EXIT
              PERFORM Z0000-SEND-MAP           THRU Z0000-EXIT
           END-IF
           .
       A1200-EXIT.
           EXIT
           .
       A1210-MOVE-DATA.
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
              CONTINUE
           ELSE
              COMPUTE BOOKS-TOTAL-PAGES = FUNCTION NUMVAL(MPAGESI)
           END-IF

           IF MRATINGI = LOW-VALUE OR SPACES
              CONTINUE
           ELSE
              EVALUATE TRUE
                  WHEN MRATINGI = 0
                       CONTINUE
                  WHEN MRATINGI >= 1 OR <= 5
                       COMPUTE BOOKS-RATING = FUNCTION NUMVAL(MRATINGI)
                  WHEN OTHER
                       MOVE    SPACES                     TO CONFIRMO
                       MOVE 'PLEASE ENTER BETWEEN 1 & 5.' TO MESSAGEO
                       PERFORM Z0000-SEND-MAP           THRU Z0000-EXIT
              END-EVALUATE

           END-IF

           MOVE LENGTH OF MISBNNOI   TO BOOKS-ISBN-LEN
           MOVE MISBNNOI             TO BOOKS-ISBN-TEXT

           IF MPUBDTEI = LOW-VALUE OR SPACES OR 'YYYY-MM-DD'
              MOVE WS-DEFAULT-DATE   TO BOOKS-PUBLISHER-DATE
           ELSE
              MOVE MPUBDTEI          TO BOOKS-PUBLISHER-DATE
           END-IF

           IF MPUBIDI = LOW-VALUE OR SPACES
              CONTINUE
           ELSE
              COMPUTE BOOKS-PUBLISHER-ID = FUNCTION NUMVAL(MPUBIDI)
           END-IF
           .
       A1210-EXIT.
           EXIT
           .
       A1220-UPDATE-DATA.
           EXEC SQL
                UPDATE IBMUSER.BOOKS
                   SET TITLE          =   :BOOKS-TITLE
                      ,TOTAL_PAGES    =   :BOOKS-TOTAL-PAGES
                      ,RATING         =   :BOOKS-RATING
                      ,ISBN           =   :BOOKS-ISBN
                      ,PUBLISHER_DATE =   :BOOKS-PUBLISHER-DATE
                      ,PUBLISHER_ID   =   :BOOKS-PUBLISHER-ID
                 WHERE BOOK_ID        =   :BOOKS-BOOK-ID
           END-EXEC

           EVALUATE TRUE
               WHEN SQLCODE = 0
                    CONTINUE
               WHEN SQLCODE = -180
                    MOVE    SPACES                TO CONFIRMO
                    MOVE 'INCORRECT DATE FORMAT.' TO MESSAGEO
                    PERFORM Z0000-SEND-MAP      THRU Z0000-EXIT
               WHEN SQLCODE = -803
                    MOVE    SPACES                TO CONFIRMO
                    MOVE 'BOOK ID ALREADY EXIST.' TO MESSAGEO
                    PERFORM Z0000-SEND-MAP      THRU Z0000-EXIT
               WHEN OTHER
                    MOVE 'A1220-UPDATE-DATA'      TO ERR-LOC
                    MOVE  SQLCODE                 TO ERR-CODE
                    MOVE  'Y'                     TO ERROR-HANDLING
                    PERFORM XXXX-ERROR-HANDLING
           END-EVALUATE
            .
       A1220-EXIT.
           EXIT
           .
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
              MOVE WS-BOOKID       TO   WS-DEL-ID
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
              MOVE SPACES          TO   MESSAGEO
              MOVE SPACES          TO   CONFIRMO

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
      *SEND MAP PARAGRAPH-----------------------------------------------
       Z0000-SEND-MAP.
           PERFORM Z1000-MOVE-ATTR THRU Z1000-EXIT

           EXEC CICS
                SEND MAP(WS-MAP)
                     MAPSET(WS-MAPSET)
                     FROM(UPDATEMO)
           END-EXEC

           EXEC CICS
               RETURN TRANSID(WS-TRANSID)
                      COMMAREA(WS-COMMAREA)
           END-EXEC
           .
       Z0000-EXIT.
           EXIT
           .
       Z1000-MOVE-ATTR.
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
           MOVE SPACES              TO MPUBDTEO
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
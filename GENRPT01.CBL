      *-----------------------
      *--- CBL COMPILE LIST
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    GENRPT01.
       AUTHOR.        DARWIN TERCENIO.
       DATE-WRITTEN.  05.22.2020.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------*
      *---------- FILE       --------- JCL    -*
      *---------- DESCRIPTOR --------- DDNAME -*
      *----------------------------------------*

      *-------------
       DATA DIVISION.
      *-------------
       WORKING-STORAGE SECTION.

       01  WS-ARRAY.
           05  WS-JCL-LINE     PIC X(080) OCCURS 28 TIMES INDEXED BY I.
       01  WS-JCL-STMT         PIC X(080)                             .
       01  WS-RESP             PIC S9(04) COMP                        .
       01  WS-MSG-1            PIC X(30) VALUE "WRITE QUEUE ERROR"    .

      *------------------
       PROCEDURE DIVISION.
      *------------------
       0000-MAIN-PROCESS.
           PERFORM 1000-MOVE-PARA     THRU 1000-EXIT
           PERFORM 2000-WRITE-QUEUE   THRU 2000-EXIT

           EXEC CICS
                RETURN
           END-EXEC

           STOP RUN
           .
       1000-MOVE-PARA.
           MOVE '//IBMUSERR JOB  CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),'
                                          TO WS-JCL-LINE(1).
           MOVE '//          REGION=2048K,NOTIFY=&SYSUID'
                                          TO WS-JCL-LINE(2).
           MOVE '//       SET WSPC=500,MEM=BKREPORT'
                                          TO WS-JCL-LINE(3).
           MOVE '//DELET0 EXEC PGM=IEFBR14'
                                          TO WS-JCL-LINE(4).
           MOVE '//BOOKOUT  DD DSN=IBMUSER.PMR0001D.BMR001.REPORT,'
                                          TO WS-JCL-LINE(5).
           MOVE '//            UNIT=SYSDA,DISP=(MOD,DELETE,DELETE),'
                                          TO WS-JCL-LINE(6).
           MOVE '//            SPACE=(TRK,1)'
                                          TO WS-JCL-LINE(7).
           MOVE '//GO       EXEC PGM=IKJEFT01,DYNAMNBR=20,'
                                          TO WS-JCL-LINE(8).
           MOVE '//             REGION=4M,COND=(4,LT)'
                                          TO WS-JCL-LINE(9).
           MOVE '//STEPLIB  DD  DSN=DSN910.DB9G.RUNLIB.LOAD,DISP=SHR'
                                          TO WS-JCL-LINE(10).
           MOVE '//         DD  DISP=SHR,DSN=DSN910.DB9G.SDSNEXIT'
                                          TO WS-JCL-LINE(11).
           MOVE '//         DD  DISP=SHR,DSN=DSN910.SDSNLOAD'
                                          TO WS-JCL-LINE(12).
           MOVE '//         DD  DISP=SHR,DSN=ISP.SISPLOAD'
                                          TO WS-JCL-LINE(13).
           MOVE '//         DD  DISP=SHR,DSN=GDDM.SADMMOD'
                                          TO WS-JCL-LINE(14).
           MOVE '//SYSOUT   DD SYSOUT=*'
                                          TO WS-JCL-LINE(15).
           MOVE '//SYUDUMP  DD SYSOUT=*'
                                          TO WS-JCL-LINE(16).
           MOVE '//CEEDUMP  DD SYSOUT=*'
                                          TO WS-JCL-LINE(17).
           MOVE '//BOOKOUT  DD DSN=IBMUSER.PMR0001D.BMR001.REPORT,'
                                          TO WS-JCL-LINE(18).
           MOVE '//             UNIT=SYSDA,DISP=(NEW,CATLG),'
                                          TO WS-JCL-LINE(19).
           MOVE '//             SPACE=(32,(3,3)),'
                                          TO WS-JCL-LINE(20).
           MOVE '//             DCB=(RECFM=FB,LRECL=230,BLKSIZE=2300)'
                                          TO WS-JCL-LINE(21).
           MOVE '//SYSPRINT DD SYSOUT=*'
                                          TO WS-JCL-LINE(22).
           MOVE '//SYSTSPRT DD SYSOUT=*'
                                          TO WS-JCL-LINE(23).
           MOVE '//SYSTSIN  DD *'
                                          TO WS-JCL-LINE(24).
           MOVE '  DSN SYSTEM(DB9G)'
                                          TO WS-JCL-LINE(25).
           MOVE '  RUN PROGRAM(BKREPORT) PLAN(DSNTIA91)'
                                          TO WS-JCL-LINE(26).
           MOVE '  END'
                                          TO WS-JCL-LINE(27).
           MOVE '/*EOF'
                                          TO WS-JCL-LINE(28).
           .
       1000-EXIT.
           EXIT
           .
       2000-WRITE-QUEUE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 28

              MOVE WS-JCL-LINE(I) TO WS-JCL-STMT

              EXEC CICS WRITEQ TD
                        QUEUE('BRPT')
                        FROM(WS-JCL-STMT)
                        RESP(WS-RESP)
              END-EXEC

              EVALUATE WS-RESP
                  WHEN DFHRESP(NORMAL)
                       CONTINUE

                  WHEN OTHER
                       EXEC CICS SEND
                            FROM (WS-MSG-1)
                            LENGTH(LENGTH OF WS-MSG-1)
                            ERASE
                       END-EXEC

                       EXEC CICS
                           RETURN
                       END-EXEC
              END-EVALUATE

           END-PERFORM
           .
       2000-EXIT.
           EXIT
           .

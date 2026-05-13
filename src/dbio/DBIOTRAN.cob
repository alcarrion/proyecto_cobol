       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOTRAN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY PATHS-FILE FROM "..\copies".
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 2.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 2 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 2 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 2 TIMES.
           05 SQL-PREC   PIC X OCCURS 2 TIMES.
      **********************************************************************
      *    EXEC SQL INCLUDE SQLCA END-EXEC.
       01 SQLCA.
           05 SQLSTATE PIC X(5).
              88  SQL-SUCCESS           VALUE '00000'.
              88  SQL-RIGHT-TRUNC       VALUE '01004'.
              88  SQL-NODATA            VALUE '02000'.
              88  SQL-DUPLICATE         VALUE '23000' THRU '23999'.
              88  SQL-MULTIPLE-ROWS     VALUE '21000'.
              88  SQL-NULL-NO-IND       VALUE '22002'.
              88  SQL-INVALID-CURSOR-STATE VALUE '24000'.
           05 FILLER   PIC X.
           05 SQLVERSN PIC 99 VALUE 03.
           05 SQLCODE  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQLERRM.
               49 SQLERRML PIC S9(4) COMP-5 VALUE ZERO.
               49 SQLERRMC PIC X(486).
           05 SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5 VALUE ZERO.
           05 FILLER   PIC X(4).
           05 SQL-HCONN USAGE POINTER VALUE NULL.
       LINKAGE SECTION.
           01 LK-ACCION-TRANS PIC X(01). *> 'C'=Commit, 'R'=Rollback
       PROCEDURE DIVISION USING LK-ACCION-TRANS.
       0000-PRINCIPAL.
           EVALUATE LK-ACCION-TRANS
               WHEN 'C'
      *            EXEC SQL COMMIT WORK END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
               WHEN 'R'
      *            EXEC SQL ROLLBACK WORK END-EXEC
           CALL 'OCSQLRBK' USING SQLCA END-CALL
           END-EVALUATE.
           GOBACK.

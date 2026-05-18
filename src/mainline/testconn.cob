       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCONN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
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
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.
           01 DB-STR PIC X(50).
      *    EXEC SQL END DECLARE SECTION END-EXEC.
       PROCEDURE DIVISION.
           MOVE 'DSN=cobol;UID=root;PWD=;' TO DB-STR.
           INSPECT DB-STR REPLACING TRAILING SPACES BY LOW-VALUES.
      *    EXEC SQL CONNECT TO :DB-STR END-EXEC.
           MOVE 50 TO SQL-LEN(1)
           CALL 'OCSQL'    USING DB-STR
                               SQL-LEN(1)
                               SQLCA
           END-CALL
                                               .
           IF SQLCODE = 0
               DISPLAY "SUCCESS"
           ELSE
               DISPLAY "FAILED: " SQLCODE
           END-IF.
      *    EXEC SQL DISCONNECT CURRENT END-EXEC.
           CALL 'OCSQLDIS' USING SQLCA END-CALL
                                               .
           STOP RUN.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-STR                   IN USE CHAR(50)
      **********************************************************************

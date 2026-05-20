       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFSUMM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORTE-CIERRE ASSIGN TO WS-RUTA-REPORTE
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  REPORTE-CIERRE.
       01  REG-REP                     PIC X(120).

       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 4.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 4 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 4 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 4 TIMES.
           05 SQL-PREC   PIC X OCCURS 4 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 163.
           05 SQL-STMT   PIC X(163) VALUE 'SELECT COUNT(*),COALESCE(SUM(
      -    'CASE WHEN FASE = ''40'' THEN 1 ELSE 0 END),0),COALESCE(SUM(C
      -    'ASE WHEN FASE = ''99'' THEN 1 ELSE 0 END),0) FROM tffm WHERE
      -    ' FECHA_SISTEMA = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(5) COMP-3.
           05 SQL-VAR-0002  PIC S9(5) COMP-3.
           05 SQL-VAR-0003  PIC S9(5) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
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
       01  DB-DATOS-RESUMEN.
           05 DB-FECHA-ISO             PIC X(10).
           05 DB-TOT-ARCHIVOS          PIC 9(05).
           05 DB-TOT-OK                PIC 9(05).
           05 DB-TOT-ERR               PIC 9(05).
           05 DB-FILE-NAME             PIC X(120).
           05 DB-FASE                  PIC X(02).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-FECHA-HORA-SYS           PIC X(21).
       01  WS-FULL-DATE                PIC X(08).
       01  WS-RUTA-REPORTE             PIC X(250).
       01  WS-PRE-RUTA                 PIC X(100)
               VALUE "C:\banco\spool\Interfaces\BATCH-UPLOAD-S\".
       01  WS-FILE-NAME             PIC X(120) VALUE "CIERRE_GENERAL".
       01  WS-EOF-CURSOR               PIC X(01) VALUE 'N'.

       01  WS-PLANTILLAS.
           05 WS-SEPARADOR             PIC X(80) VALUE ALL "=".
           05 WS-LINEA-F               PIC X(80) VALUE ALL "-".

       PROCEDURE DIVISION.
       000-MAIN.
           DISPLAY ">>> TFSUMM: GENERANDO ACTA DE CUADRATURA <<<"

           MOVE FUNCTION CURRENT-DATE(1:21) TO WS-FECHA-HORA-SYS
           MOVE WS-FECHA-HORA-SYS(1:8) TO WS-FULL-DATE

           STRING WS-FULL-DATE(1:4) "-" WS-FULL-DATE(5:2) "-"
                  WS-FULL-DATE(7:2) DELIMITED BY SIZE INTO DB-FECHA-ISO

           INITIALIZE WS-RUTA-REPORTE
           STRING FUNCTION TRIM(WS-PRE-RUTA) DELIMITED BY SIZE
                  "TRICKLE-FEED-REPORT\" DELIMITED BY SIZE
                  FUNCTION TRIM(WS-FILE-NAME) DELIMITED BY SIZE
                  ".out" DELIMITED BY SIZE
                  INTO WS-RUTA-REPORTE
           END-STRING.

           OPEN OUTPUT REPORTE-CIERRE

           WRITE REG-REP FROM WS-SEPARADOR
           WRITE REG-REP FROM "   ACTA DE CUADRATURA - CORE BANCARIO "
           WRITE REG-REP FROM WS-SEPARADOR

           INITIALIZE REG-REP
           STRING " FECHA CONTABLE : " DELIMITED BY SIZE
                  DB-FECHA-ISO DELIMITED BY SIZE INTO REG-REP
           WRITE REG-REP
           WRITE REG-REP FROM WS-LINEA-F

      *    EXEC SQL
      *        SELECT COUNT(*),
      *               COALESCE(SUM(CASE WHEN FASE = '40'
      *                THEN 1 ELSE 0 END), 0),
      *               COALESCE(SUM(CASE WHEN FASE = '99'
      *               THEN 1 ELSE 0 END), 0)
      *        INTO :DB-TOT-ARCHIVOS, :DB-TOT-OK, :DB-TOT-ERR
      *        FROM tffm
      *        WHERE FECHA_SISTEMA = :DB-FECHA-ISO
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 3 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(3)
               MOVE 3 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 DB-FECHA-ISO
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-TOT-ARCHIVOS
           MOVE SQL-VAR-0002 TO DB-TOT-OK
           MOVE SQL-VAR-0003 TO DB-TOT-ERR
                   .

           IF DB-TOT-ARCHIVOS = 0
               WRITE REG-REP FROM ">>>ADVERTENCIA: NO HAY REGISTROS <<<"
               CLOSE REPORTE-CIERRE
               GOBACK
           END-IF.

           INITIALIZE REG-REP
           STRING " TOTAL ARCHIVOS : " DELIMITED BY SIZE
                  DB-TOT-ARCHIVOS DELIMITED BY SIZE INTO REG-REP
           WRITE REG-REP

           CLOSE REPORTE-CIERRE
           DISPLAY " [OK] ACTA GENERADA."
           GOBACK.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-DATOS-RESUMEN     NOT IN USE
      *  DB-DATOS-RESUMEN.DB-FASE NOT IN USE
      *  DB-DATOS-RESUMEN.DB-FECHA-ISO NOT IN USE
      *  DB-DATOS-RESUMEN.DB-FILE-NAME NOT IN USE
      *  DB-DATOS-RESUMEN.DB-TOT-ARCHIVOS NOT IN USE
      *  DB-DATOS-RESUMEN.DB-TOT-ERR NOT IN USE
      *  DB-DATOS-RESUMEN.DB-TOT-OK NOT IN USE
      *  DB-FASE              NOT IN USE
      *  DB-FECHA-ISO             IN USE CHAR(10)
      *  DB-FILE-NAME         NOT IN USE
      *  DB-TOT-ARCHIVOS          IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(5,0)
      *  DB-TOT-ERR               IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(5,0)
      *  DB-TOT-OK                IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(5,0)
      **********************************************************************

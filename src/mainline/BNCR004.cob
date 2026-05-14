       IDENTIFICATION DIVISION.
       PROGRAM-ID. BNCR004.
      *==========================================================
      * RETORNA LA FECHA CONTABLE DESDE LA BASE DE DATOS
      *==========================================================

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
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 52.
           05 SQL-STMT   PIC X(52) VALUE 'SELECT FECHA_PROCESO FROM CONT
      -    'ROL_SECUENCIAS LIMIT 1'.
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
       01  WS-FECHA-DB          PIC X(10).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.
       01  LK-FECHA-SALIDA      PIC X(10).

       PROCEDURE DIVISION USING LK-FECHA-SALIDA.
       0000-PRINCIPAL.
      * 1. Consultar la fecha de proceso en la tabla de control
      * Nota: Se asume una tabla de parámetros o control_secuencias
      *    EXEC SQL
      *        SELECT FECHA_PROCESO
      *        INTO :WS-FECHA-DB
      *        FROM CONTROL_SECUENCIAS
      *        LIMIT 1
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-FECHA-DB
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 10 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .

      * 2. Manejo de error: si no hay fecha en BD, usar la del sistema
           IF SQLCODE NOT = 0
               MOVE FUNCTION CURRENT-DATE(1:4) TO WS-FECHA-DB(1:4)
               MOVE "-" TO WS-FECHA-DB(5:1)
               MOVE FUNCTION CURRENT-DATE(5:2) TO WS-FECHA-DB(6:2)
               MOVE "-" TO WS-FECHA-DB(8:1)
               MOVE FUNCTION CURRENT-DATE(7:2) TO WS-FECHA-DB(9:2)
           END-IF.

           MOVE WS-FECHA-DB TO LK-FECHA-SALIDA.

           GOBACK.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-FECHA-DB              IN USE CHAR(10)
      **********************************************************************

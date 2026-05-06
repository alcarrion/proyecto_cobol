       IDENTIFICATION DIVISION.
       PROGRAM-ID. BAT000.
      *================================================================*
      * MODULO: Cierre Mensual Batch                                   *
      *================================================================*
       ENVIRONMENT DIVISION.
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
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 112.
           05 SQL-STMT   PIC X(112) VALUE 'UPDATE TARJETAS SET LIQUIDACI
      -    'ON_MES = LIQUIDACION_MES + ACUM_MES,ACUM_MES = 0 WHERE ESTAD
      -    'O = ''A'' OR ESTADO = ''B'''.
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
       01  WS-TARJETAS-PROCESADAS PIC 9(05).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.
       COPY LKCIF.

       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           DISPLAY "========================================".
           DISPLAY "    INICIANDO PROCESO BATCH (CIERRE)    ".
           DISPLAY "========================================".

      *    EXEC SQL
      *        UPDATE TARJETAS
      *        SET LIQUIDACION_MES = LIQUIDACION_MES + ACUM_MES,
      *            ACUM_MES = 0
      *        WHERE ESTADO = 'A' OR ESTADO = 'B'
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               MOVE 0 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .

           IF SQLCODE = 0 OR SQLCODE = 100
               DISPLAY "CIERRE DE TARJETAS COMPLETADO EXITOSAMENTE."
               MOVE 0 TO LK-COD-RETORNO
               MOVE "CIERRE BATCH EJECUTADO" TO LK-MENSAJE
           ELSE
               DISPLAY "ERROR EN CIERRE DE TARJETAS. SQLCODE: " SQLCODE
               MOVE 99 TO LK-COD-RETORNO
               MOVE "ERROR EN BATCH" TO LK-MENSAJE
           END-IF.

           DISPLAY "========================================".
           DISPLAY "         FIN DE PROCESO BATCH           ".
           DISPLAY "========================================".

           GOBACK.
       END PROGRAM BAT000.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-TARJETAS-PROCESADAS NOT IN USE
      **********************************************************************

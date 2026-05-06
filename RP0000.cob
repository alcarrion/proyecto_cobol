       IDENTIFICATION DIVISION.
       PROGRAM-ID. RP0000.
      *================================================================*
      * MODULO: Reportes Gerenciales (Consolidado Banco)               *
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
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 49.
           05 SQL-STMT   PIC X(49) VALUE 'SELECT COALESCE(SUM(SALDO_ACTU
      -    'AL),0) FROM ctactes'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 99.
           05 SQL-STMT   PIC X(99) VALUE 'SELECT COALESCE(SUM(LIQUIDACIO
      -    'N_MES + ACUM_MES),0) FROM TARJETAS WHERE ESTADO = ''A'' OR E
      -    'STADO = ''B'''.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 96.
           05 SQL-STMT   PIC X(96) VALUE 'SELECT COALESCE(COUNT(*),0),CO
      -    'ALESCE(SUM(SALDO_ACTUAL),0) FROM HIPOTECAS WHERE ESTADO = ''
      -    'Activa'''.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0002  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0003  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(5) COMP-3.
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
       01  WS-CTACTES       PIC S9(13)V99.
       01  WS-TARJ      PIC S9(13)V99.
       01  WS-HIPOT     PIC S9(13)V99.
       01  WS-C-HIP     PIC 9(05).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.
       COPY LKCIF.

       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           DISPLAY " "
           DISPLAY "=================================================="
           DISPLAY "          REPORTE GERENCIAL CONSOLIDADO           "
           DISPLAY "=================================================="

           MOVE 0 TO WS-CTACTES.
           MOVE 0 TO WS-TARJ.
           MOVE 0 TO WS-HIPOT.
           MOVE 0 TO WS-C-HIP.

      *    1. Total Saldo Cuentas Corrientes (Pasivo del Banco)
      *    EXEC SQL
      *        SELECT COALESCE(SUM(SALDO_ACTUAL), 0)
      *        INTO :WS-CTACTES
      *        FROM ctactes
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-CTACTES
                   .

      *    2. Total Deuda Tarjetas (Activo del Banco)
      *    EXEC SQL
      *        SELECT COALESCE(SUM(LIQUIDACION_MES + ACUM_MES), 0)
      *        INTO :WS-TARJ
      *        FROM TARJETAS
      *        WHERE ESTADO = 'A' OR ESTADO = 'B'
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0002 TO WS-TARJ
                   .

      *    3. Total Hipotecas Activas (Activo del Banco)
      *    EXEC SQL
      *        SELECT COALESCE(COUNT(*), 0),
      *               COALESCE(SUM(SALDO_ACTUAL), 0)
      *        INTO :WS-C-HIP,
      *             :WS-HIPOT
      *        FROM HIPOTECAS
      *        WHERE ESTADO = 'Activa'
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 8 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0004 TO WS-C-HIP
           MOVE SQL-VAR-0003 TO WS-HIPOT
                   .

           DISPLAY "Saldo Cta.Cte: $" WS-CTACTES
           DISPLAY "Deuda Tarjetas: $" WS-TARJ
           DISPLAY "Deuda Hipotecas: $" WS-HIPOT
           DISPLAY "Cant. Hipotecas: " WS-C-HIP
           DISPLAY " "
           DISPLAY "=================================================="

           MOVE 0 TO LK-COD-RETORNO.
           MOVE "REPORTE GENERADO EXITOSAMENTE" TO LK-MENSAJE.

           GOBACK.
       END PROGRAM RP0000.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-C-HIP                 IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(5,0)
      *  WS-CTACTES               IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(15,2)
      *  WS-HIPOT                 IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(15,2)
      *  WS-TARJ                  IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(15,2)
      **********************************************************************

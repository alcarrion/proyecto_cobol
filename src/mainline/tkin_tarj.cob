       IDENTIFICATION DIVISION.
       PROGRAM-ID. tkin_tarj.
      *==========================================================
      * SUBPROGRAMA: tkin_tarj.sqb
      * RESPONSABILIDAD: Especialista de Afectación de Tarjetas (004)
      *==========================================================
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 3.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 3 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 3 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 3 TIMES.
           05 SQL-PREC   PIC X OCCURS 3 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 73.
           05 SQL-STMT   PIC X(73) VALUE 'SELECT ESTADO_TARJETA,SALDO_UT
      -    'ILIZADO FROM tarjetas WHERE NRO_TARJETA = ?'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 52.
           05 SQL-STMT   PIC X(52) VALUE 'SELECT SALDO_ACTUAL FROM ctact
      -    'es WHERE ID_CUENTA = ?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'UPDATE ctactes SET SALDO_ACTUA
      -    'L = SALDO_ACTUAL - ? WHERE ID_CUENTA = ?'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 79.
           05 SQL-STMT   PIC X(79) VALUE 'UPDATE tarjetas SET SALDO_UTIL
      -    'IZADO = SALDO_UTILIZADO - ? WHERE NRO_TARJETA = ?'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 105.
           05 SQL-STMT   PIC X(105) VALUE 'INSERT INTO movimientos (ID_C
      -    'UENTA,TIPO_MOV,IMPORTE,SALDO_RESULTANTE,TERMINAL_ID) VALUES 
      -    '(?,6,?,?,''BTCH'')'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(11) COMP-3.
           05 SQL-VAR-0002  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0003  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0005  PIC S9(13)V9(2) COMP-3.
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
       01  DB-VARS-TARJ.
           05 DB-PAN-TARGET       PIC X(16).
           05 DB-CTA-DEBITO       PIC 9(10).
           05 DB-PAGO-MONTO       PIC S9(13)V99.
           05 DB-SALDO-UTILIZADO  PIC S9(13)V99.
           05 DB-ESTADO-TARJETA   PIC X(01).
           05 DB-SALDO-CTA        PIC S9(13)V99.
           05 DB-SALDO-RES-LEDG   PIC S9(13)V99.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.
       01  LK-CTA-DEBITO           PIC 9(10).
       01  LK-NRO-TARJETA          PIC X(16).
       01  LK-MONTO-PAGO           PIC S9(13)V99.
       COPY LKTF.

       PROCEDURE DIVISION USING LK-CTA-DEBITO, LK-NRO-TARJETA,
                                LK-MONTO-PAGO,
                                LK-TRICKLE-FEED-INTERFACE.
       000-MAIN.
           MOVE 00 TO LK-TF-COD-RETORNO
           MOVE LK-CTA-DEBITO TO DB-CTA-DEBITO
           MOVE LK-NRO-TARJETA TO DB-PAN-TARGET
           MOVE LK-MONTO-PAGO TO DB-PAGO-MONTO

      * 1. Validar existencia y estatus del plástico en el Core
      *    EXEC SQL
      *        SELECT ESTADO_TARJETA, SALDO_UTILIZADO
      *        INTO :DB-ESTADO-TARJETA, :DB-SALDO-UTILIZADO
      *        FROM tarjetas
      *        WHERE NRO_TARJETA = :DB-PAN-TARGET
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-ESTADO-TARJETA
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 8 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-PAN-TARGET
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 16 TO SQL-LEN(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0003 TO DB-SALDO-UTILIZADO
                   .

           IF SQLCODE NOT = 0
               MOVE 01 TO LK-TF-COD-RETORNO
               MOVE "TARJETA NO EXISTE EN EL CORE" TO LK-TF-MENSAJE
               GOBACK
           END-IF.

           IF DB-ESTADO-TARJETA NOT = "A"
               MOVE 01 TO LK-TF-COD-RETORNO
               MOVE "TARJETA BLOQUEADA O VENCIDA" TO LK-TF-MENSAJE
               GOBACK
           END-IF.

      * 2. Verificar cobertura de fondos en la Cuenta DDA asociada
      *    EXEC SQL
      *        SELECT SALDO_ACTUAL
      *        INTO :DB-SALDO-CTA
      *        FROM ctactes
      *        WHERE ID_CUENTA = :DB-CTA-DEBITO
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 6 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-CTA-DEBITO TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0004 TO DB-SALDO-CTA
                   .

           IF DB-SALDO-CTA < DB-PAGO-MONTO
               MOVE 07 TO LK-TF-COD-RETORNO
               MOVE "FONDOS INSUFICIENTES PARA PAGO" TO LK-TF-MENSAJE
               GOBACK
           END-IF.

      * 3. APLICACIÓN CONTABLE ATÓMICA
      * 3.1 Débito de la Cuenta Monetaria (Pasivo)
      *    EXEC SQL
      *        UPDATE ctactes
      *        SET SALDO_ACTUAL = SALDO_ACTUAL - :DB-PAGO-MONTO
      *        WHERE ID_CUENTA = :DB-CTA-DEBITO
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 6 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-PAGO-MONTO
             TO SQL-VAR-0002
           MOVE DB-CTA-DEBITO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .

      * 3.2 Crédito / Amortización del saldo utilizado de la tarjeta (
      *    EXEC SQL
      *        UPDATE tarjetas
      *        SET SALDO_UTILIZADO = SALDO_UTILIZADO - :DB-PAGO-MONTO
      *        WHERE NRO_TARJETA = :DB-PAN-TARGET
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-PAN-TARGET
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 16 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-PAGO-MONTO
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
                   .

      * 3.3 Asentar movimiento en el Ledger Contable (Código 6 = Débit
           COMPUTE DB-SALDO-RES-LEDG = DB-SALDO-CTA - DB-PAGO-MONTO
      *    EXEC SQL
      *        INSERT INTO movimientos (ID_CUENTA, TIPO_MOV, IMPORTE,
      *                                 SALDO_RESULTANTE, TERMINAL_ID)
      *        VALUES (:DB-CTA-DEBITO, 6, :DB-PAGO-MONTO,
      *                :DB-SALDO-RES-LEDG, 'BTCH')
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 8 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-CTA-DEBITO
             TO SQL-VAR-0001
           MOVE DB-PAGO-MONTO
             TO SQL-VAR-0002
           MOVE DB-SALDO-RES-LEDG
             TO SQL-VAR-0005
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
                   .

      *    EXEC SQL COMMIT END-EXEC.
           CALL 'OCSQLCMT' USING SQLCA END-CALL
                                   .
           MOVE 00 TO LK-TF-COD-RETORNO
           MOVE "PAGO TARJETA OK" TO LK-TF-MENSAJE
           GOBACK.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-CTA-DEBITO            IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(11,0)
      *  DB-ESTADO-TARJETA        IN USE CHAR(1)
      *  DB-PAGO-MONTO            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(15,2)
      *  DB-PAN-TARGET            IN USE CHAR(16)
      *  DB-SALDO-CTA             IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(15,2)
      *  DB-SALDO-RES-LEDG        IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(15,2)
      *  DB-SALDO-UTILIZADO       IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(15,2)
      *  DB-VARS-TARJ         NOT IN USE
      *  DB-VARS-TARJ.DB-CTA-DEBITO NOT IN USE
      *  DB-VARS-TARJ.DB-ESTADO-TARJETA NOT IN USE
      *  DB-VARS-TARJ.DB-PAGO-MONTO NOT IN USE
      *  DB-VARS-TARJ.DB-PAN-TARGET NOT IN USE
      *  DB-VARS-TARJ.DB-SALDO-CTA NOT IN USE
      *  DB-VARS-TARJ.DB-SALDO-RES-LEDG NOT IN USE
      *  DB-VARS-TARJ.DB-SALDO-UTILIZADO NOT IN USE
      **********************************************************************

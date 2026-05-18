       IDENTIFICATION DIVISION.
       PROGRAM-ID. tkin_hip.
      *==========================================================
      * SUBPROGRAMA PRECOMPILABLE: tkin_hip.sqb
      * RESPONSABILIDAD: Especialista de Amortización de Hipotecas
      * INTERFAZ: Canal Maestro PAG - Código de Operación 005
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
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 6.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 6 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 6 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 6 TIMES.
           05 SQL-PREC   PIC X OCCURS 6 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 110.
           05 SQL-STMT   PIC X(110) VALUE 'SELECT CUENTA_DEBITO,SALDO_DE
      -    'UDA,CUOTA_MENSUAL,MESES_MORA,ESTADO_PRESTAMO FROM hipotecas 
      -    'WHERE ID_HIPOTECA = ?'.
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
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 72.
           05 SQL-STMT   PIC X(72) VALUE 'UPDATE hipotecas SET SALDO_DEU
      -    'DA = SALDO_DEUDA - ? WHERE ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'UPDATE hipotecas SET MESES_MOR
      -    'A = MESES_MORA - 1 WHERE ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-5.
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
           05 SQL-VAR-0002  PIC S9(11) COMP-3.
           05 SQL-VAR-0003  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0005  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0006  PIC S9(3) COMP-3.
           05 SQL-VAR-0007  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0008  PIC S9(13)V9(2) COMP-3.
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
      * Variables de host para interactuar con MySQL
       01  DB-VARS-HIP.
           05 DB-HIP-ID-TARGET    PIC 9(10).
           05 DB-CTA-DEBITO       PIC 9(10).
           05 DB-PAGO-MONTO       PIC S9(13)V99.
           05 DB-SALDO-DEUDA      PIC S9(13)V99.
           05 DB-CUOTA-MENSUAL    PIC S9(13)V99.
           05 DB-MESES-MORA       PIC 9(02).
           05 DB-ESTADO-PRESTAMO  PIC X(10).
           05 DB-SALDO-CTA        PIC S9(13)V99.
           05 DB-SALDO-RES-LEDG   PIC S9(13)V99.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.
      * Argumentos atómicos desacoplados heredados desde el orquestador
       01  LK-HIPOTECA-ID          PIC 9(10).
       01  LK-NUM-CREDITO          PIC X(20).
       01  LK-MONTO-PAGO           PIC S9(13)V99.
           COPY LKTF.

       PROCEDURE DIVISION USING LK-HIPOTECA-ID, LK-NUM-CREDITO,
                                LK-MONTO-PAGO,
                                LK-TRICKLE-FEED-INTERFACE.
       0000-PRINCIPAL.
           MOVE 00 TO LK-TF-COD-RETORNO
           MOVE "OK TKIN_HIP" TO LK-TF-MENSAJE

           MOVE LK-HIPOTECA-ID TO DB-HIP-ID-TARGET
           MOVE LK-MONTO-PAGO   TO DB-PAGO-MONTO.

      * 1. VALIDACIÓN PREVIA DEL ACTIVO (Existencia del Préstamo)
      *    EXEC SQL
      *        SELECT CUENTA_DEBITO, SALDO_DEUDA, CUOTA_MENSUAL,
      *               MESES_MORA, ESTADO_PRESTAMO
      *        INTO :DB-CTA-DEBITO, :DB-SALDO-DEUDA, :DB-CUOTA-MENSUAL,
      *             :DB-MESES-MORA, :DB-ESTADO-PRESTAMO
      *        FROM hipotecas
      *        WHERE ID_HIPOTECA = :DB-HIP-ID-TARGET
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(2)
               MOVE 8 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(4)
               MOVE 2 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 DB-ESTADO-PRESTAMO
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 10 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(6)
               MOVE 6 TO SQL-LEN(6)
               MOVE X'00' TO SQL-PREC(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-HIP-ID-TARGET TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0002 TO DB-CTA-DEBITO
           MOVE SQL-VAR-0004 TO DB-SALDO-DEUDA
           MOVE SQL-VAR-0005 TO DB-CUOTA-MENSUAL
           MOVE SQL-VAR-0006 TO DB-MESES-MORA
                   .

           IF SQLCODE NOT = 0
               MOVE 01 TO LK-TF-COD-RETORNO
               MOVE "PRESTAMO HIPOTECARIO NO EXISTE" TO LK-TF-MENSAJE
               GOBACK
           END-IF.

           IF DB-ESTADO-PRESTAMO NOT = "ACTIVO"
               MOVE 01 TO LK-TF-COD-RETORNO
               MOVE "CREDITO EN PROCESO JUDICIAL O INACTIVO"
                 TO LK-TF-MENSAJE
               GOBACK
           END-IF.

      * 2. VALIDACIÓN DE COBERTURA DE FONDOS EN PASIVOS (Cuenta Monetar
      *    EXEC SQL
      *        SELECT SALDO_ACTUAL
      *        INTO :DB-SALDO-CTA
      *        FROM ctactes
      *        WHERE ID_CUENTA = :DB-CTA-DEBITO
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 6 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-CTA-DEBITO TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0007 TO DB-SALDO-CTA
                   .

           IF SQLCODE NOT = 0
               MOVE 01 TO LK-TF-COD-RETORNO
               MOVE "CUENTA MONETARIA DE DEBITO NO EXISTE"
                 TO LK-TF-MENSAJE
               GOBACK
           END-IF.

           IF DB-SALDO-CTA < DB-PAGO-MONTO
               MOVE 07 TO LK-TF-COD-RETORNO
               MOVE "FONDO INSUFICIENTE EN CUENTA VISTA"
                 TO LK-TF-MENSAJE
               GOBACK
           END-IF.

      * 3. APLICACIÓN CONTABLE DUAL CRUCIAL
      * 3.1 Carga al Pasivo (Restar dinero de la Cuenta Corriente)
      *    EXEC SQL
      *        UPDATE ctactes
      *        SET SALDO_ACTUAL = SALDO_ACTUAL - :DB-PAGO-MONTO
      *        WHERE ID_CUENTA = :DB-CTA-DEBITO
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
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
             TO SQL-VAR-0003
           MOVE DB-CTA-DEBITO
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .

      * 3.2 Abono al Activo (Disminuir la deuda de la Hipoteca)
      *    EXEC SQL
      *        UPDATE hipotecas
      *        SET SALDO_DEUDA = SALDO_DEUDA - :DB-PAGO-MONTO
      *        WHERE ID_HIPOTECA = :DB-HIP-ID-TARGET
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
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
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-PAGO-MONTO
             TO SQL-VAR-0003
           MOVE DB-HIP-ID-TARGET
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
                   .

      * 3.3 Mitigación de Riesgos: Si el pago cubre la cuota, bajamos m
           IF DB-PAGO-MONTO >= DB-CUOTA-MENSUAL AND DB-MESES-MORA > 0
      *        EXEC SQL
      *            UPDATE hipotecas
      *            SET MESES_MORA = MESES_MORA - 1
      *            WHERE ID_HIPOTECA = :DB-HIP-ID-TARGET
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-HIP-ID-TARGET
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
           END-IF.

      * 3.4 Inyección del Asiento en el Libro Mayor (Ledger)
      * Código de movimiento 6 = Cobro de interfaces Batch asíncronas
           COMPUTE DB-SALDO-RES-LEDG = DB-SALDO-CTA - DB-PAGO-MONTO.

      *    EXEC SQL
      *        INSERT INTO movimientos (ID_CUENTA, TIPO_MOV, IMPORTE,
      *                                 SALDO_RESULTANTE, TERMINAL_ID)
      *        VALUES (:DB-CTA-DEBITO, 6, :DB-PAGO-MONTO,
      *                :DB-SALDO-RES-LEDG, 'BTCH')
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 8 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-CTA-DEBITO
             TO SQL-VAR-0002
           MOVE DB-PAGO-MONTO
             TO SQL-VAR-0003
           MOVE DB-SALDO-RES-LEDG
             TO SQL-VAR-0008
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
                   .

      * 4. CIERRE CON EXITO EN BASE DE DATOS
      *    EXEC SQL COMMIT END-EXEC.
           CALL 'OCSQLCMT' USING SQLCA END-CALL
                                   .
           MOVE 00 TO LK-TF-COD-RETORNO.
           GOBACK.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-CTA-DEBITO            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(11,0)
      *  DB-CUOTA-MENSUAL         IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(15,2)
      *  DB-ESTADO-PRESTAMO       IN USE CHAR(10)
      *  DB-HIP-ID-TARGET         IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(11,0)
      *  DB-MESES-MORA            IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(3,0)
      *  DB-PAGO-MONTO            IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(15,2)
      *  DB-SALDO-CTA             IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(15,2)
      *  DB-SALDO-DEUDA           IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(15,2)
      *  DB-SALDO-RES-LEDG        IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(15,2)
      *  DB-VARS-HIP          NOT IN USE
      *  DB-VARS-HIP.DB-CTA-DEBITO NOT IN USE
      *  DB-VARS-HIP.DB-CUOTA-MENSUAL NOT IN USE
      *  DB-VARS-HIP.DB-ESTADO-PRESTAMO NOT IN USE
      *  DB-VARS-HIP.DB-HIP-ID-TARGET NOT IN USE
      *  DB-VARS-HIP.DB-MESES-MORA NOT IN USE
      *  DB-VARS-HIP.DB-PAGO-MONTO NOT IN USE
      *  DB-VARS-HIP.DB-SALDO-CTA NOT IN USE
      *  DB-VARS-HIP.DB-SALDO-DEUDA NOT IN USE
      *  DB-VARS-HIP.DB-SALDO-RES-LEDG NOT IN USE
      **********************************************************************

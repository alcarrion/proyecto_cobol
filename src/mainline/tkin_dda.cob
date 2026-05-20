       IDENTIFICATION DIVISION.
       PROGRAM-ID. tkin_dda.
      *================================================================*
      * PROGRAMA PRECOMPILABLE: tkin_dda.sqb                           *
      * PRODUCTO: Cuentas de Depósitos a la Vista (DDA - Corrientes)
      * RESPONSABILIDAD: Persistencia e impacto contable en saldos.    *
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

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
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 55.
           05 SQL-STMT   PIC X(55) VALUE 'UPDATE ctactes SET SALDO_ACTUA
      -    'L = ? WHERE ID_CUENTA = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0003  PIC S9(13)V9(2) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************
      * Inclusión de la estructura de comunicación nativa de SQL
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
      * Seccion de Variables Host obligatorias para el precompilador .sq
       01  WS-HOST-DDA.
           05 WS-CUENTA-SQL        PIC 9(09).
           05 WS-MONTO-MOV-SQL     PIC S9(13)V99.
           05 WS-NUEVO-SALDO-SQL   PIC S9(13)V99.
           05 WS-COD-MOV-SQL       PIC 9(04).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.
      * Estructura de comunicación compartida heredada desde tkin01
       01  REG-CTA.
           05 CTA-NRO-CUENTA       PIC 9(09).
           05 CTA-NUM-CREDITO      PIC X(20).
           05 CTA-SALDO-ACTUAL     PIC S9(13)V99.
           05 CTA-MONTO-MOV        PIC 9(13)V99.

           COPY LKTF.

       PROCEDURE DIVISION USING REG-CTA, LK-TRICKLE-FEED-INTERFACE.
       0000-PRINCIPAL.
           MOVE 00 TO LK-TF-COD-RETORNO
           MOVE "OK" TO LK-TF-MENSAJE

           MOVE CTA-NRO-CUENTA TO WS-CUENTA-SQL
           MOVE CTA-MONTO-MOV  TO WS-MONTO-MOV-SQL.

      *----------------------------------------------------------------*
      * EVALUACIÓN DEL SIGNO CONTABLE FINANCIERO (DDA)
      *----------------------------------------------------------------*
           EVALUATE LK-TF-ACCION
      * Caso: Crédito / Depósito (Aumenta el balance disponible)
               WHEN "C"
                   COMPUTE WS-NUEVO-SALDO-SQL =
                           CTA-SALDO-ACTUAL + CTA-MONTO-MOV
                   MOVE 7 TO WS-COD-MOV-SQL
                   PERFORM 1000-PERSISTIR-BALANCE

      * Caso: Débito / Retiro / Extracción (Disminuye el balance)
               WHEN "D"
      * ====== INICIO: REGLA DE NEGOCIO (FONDOS INSUFICIENTES) ======
                   IF CTA-MONTO-MOV > CTA-SALDO-ACTUAL
                       MOVE 07 TO LK-TF-COD-RETORNO
                       MOVE "ERR: FONDOS INSUFICIENTES PARA DEBITO DDA"
                         TO LK-TF-MENSAJE
                   ELSE
                       COMPUTE WS-NUEVO-SALDO-SQL =
                               CTA-SALDO-ACTUAL - CTA-MONTO-MOV
                       MOVE 6 TO WS-COD-MOV-SQL
                       PERFORM 1000-PERSISTIR-BALANCE
                   END-IF
      * ====== FIN: REGLA DE NEGOCIO ================================

               WHEN OTHER
                   MOVE 99 TO LK-TF-COD-RETORNO
                   MOVE "ERR: ACCION CONTABLE INVALIDA EN DDA"
                     TO LK-TF-MENSAJE
           END-EVALUATE.

      *================================================================*
      * ETAPA DE PERSISTENCIA: IMPACTO DIRECTO EN EL LIBRO MAYOR (DDA)
      *================================================================*
       1000-PERSISTIR-BALANCE.
      *    EXEC SQL
      *        UPDATE ctactes
      *        SET SALDO_ACTUAL  = :WS-NUEVO-SALDO-SQL
      *        WHERE ID_CUENTA  = :WS-CUENTA-SQL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-NUEVO-SALDO-SQL
             TO SQL-VAR-0003
           MOVE WS-CUENTA-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .

      * Validación de ejecución atómica del motor relacional de MySQL
           IF SQLCODE NOT = 0
               MOVE 99 TO LK-TF-COD-RETORNO
               STRING "ERR DB CORESALDO SQLCODE: " SQLCODE
                   DELIMITED BY SIZE INTO LK-TF-MENSAJE
           END-IF.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-COD-MOV-SQL       NOT IN USE
      *  WS-CUENTA-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  WS-HOST-DDA          NOT IN USE
      *  WS-HOST-DDA.WS-COD-MOV-SQL NOT IN USE
      *  WS-HOST-DDA.WS-CUENTA-SQL NOT IN USE
      *  WS-HOST-DDA.WS-MONTO-MOV-SQL NOT IN USE
      *  WS-HOST-DDA.WS-NUEVO-SALDO-SQL NOT IN USE
      *  WS-MONTO-MOV-SQL     NOT IN USE
      *  WS-NUEVO-SALDO-SQL       IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(15,2)
      **********************************************************************

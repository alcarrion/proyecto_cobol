       IDENTIFICATION DIVISION.
       PROGRAM-ID. tkin01.
      *================================================================*
      * PROGRAMA: tkin01.sqb                                           *
      * FUNCION: Afectación de saldos (Débitos, Créditos, Consultas,
      * CORRECCIÓN: Uso estricto de variables host planas en DISPLAY
      *================================================================*
       ENVIRONMENT DIVISION.
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
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 53.
           05 SQL-STMT   PIC X(53) VALUE 'SELECT SALDO_ACTUAL FROM ctact
      -    'es WHERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 126.
           05 SQL-STMT   PIC X(126) VALUE 'UPDATE ctactes SET SALDO_ACTU
      -    'AL = SALDO_ACTUAL - ?,COD_ULT_MOV = 6,IMPORTE_MOV = ?,FECHA_
      -    'HORA_ALT = NOW() WHERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 126.
           05 SQL-STMT   PIC X(126) VALUE 'UPDATE ctactes SET SALDO_ACTU
      -    'AL = SALDO_ACTUAL + ?,COD_ULT_MOV = 7,IMPORTE_MOV = ?,FECHA_
      -    'HORA_ALT = NOW() WHERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 53.
           05 SQL-STMT   PIC X(53) VALUE 'SELECT SALDO_ACTUAL FROM ctact
      -    'es WHERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 77.
           05 SQL-STMT   PIC X(77) VALUE 'SELECT SALDO_PENDIENTE FROM cr
      -    'editos WHERE NUM_CREDITO = ? AND ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 121.
           05 SQL-STMT   PIC X(121) VALUE 'UPDATE creditos SET SALDO_PEN
      -    'DIENTE = SALDO_PENDIENTE - ?,FECHA_ULT_PAGO = NOW() WHERE NU
      -    'M_CREDITO = ? AND ID_CLIENTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(13) COMP-3.
           05 SQL-VAR-0002  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0003  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(13)V9(2) COMP-3.
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

      *EXEC SQL BEGIN DECLARE SECTION END-EXEC.
      * Variables Host Planas en DISPLAY puro para compatibilidad con es
       01 WS-ID-CLIENTE-SQL    PIC 9(12).
       01 WS-NUM-CREDITO-SQL   PIC X(20).
       01 WS-SALDO-ACTUAL-SQL  PIC S9(13)V99.
       01 WS-MONTO-MOV-SQL     PIC S9(13)V99.
       01 WS-SALDO-AUX            PIC S9(13)V99 VALUE 0.


      *EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.
       01  REG-CTA.
           05 CTA-ID-CLIENTE       PIC 9(12).
           05 CTA-NUM-CREDITO      PIC X(20).
           05 CTA-SALDO-ACTUAL     PIC S9(13)V99.
           05 CTA-MONTO-MOV        PIC 9(13)V99.

      * El COPY se ubica estrictamente AFUERA del bloque EXEC SQL
           COPY LKTF.

       PROCEDURE DIVISION USING REG-CTA, LK-TRICKLE-FEED-INTERFACE.
       0000-PRINCIPAL.
           MOVE 0 TO LK-TF-COD-RETORNO
           MOVE SPACES TO LK-TF-MENSAJE

      * SINCRO: Pasamos los parámetros que llegan por Linkage a las var
           MOVE CTA-ID-CLIENTE  TO WS-ID-CLIENTE-SQL
           MOVE CTA-NUM-CREDITO TO WS-NUM-CREDITO-SQL
           MOVE CTA-MONTO-MOV   TO WS-MONTO-MOV-SQL

           EVALUATE LK-TF-ACCION
               WHEN 'D' PERFORM 1000-PROCESAR-DEBITO
               WHEN 'C' PERFORM 2000-PROCESAR-CREDITO
               WHEN 'R' PERFORM 3000-CONSULTAR-SALDO
               WHEN 'P' PERFORM 4000-PROCESAR-PAGO-CREDITO
               WHEN OTHER
                   MOVE 98 TO LK-TF-COD-RETORNO
                   MOVE "ACCION CONTABLE NO CONTEMPLADA"
                    TO LK-TF-MENSAJE
           END-EVALUATE.
           EXIT PROGRAM.

       1000-PROCESAR-DEBITO.
      * Aseguramos que la llave de búsqueda esté cargada en la variabl
           MOVE CTA-ID-CLIENTE TO WS-ID-CLIENTE-SQL

      *    EXEC SQL
      *        SELECT SALDO_ACTUAL INTO :WS-SALDO-ACTUAL-SQL
      *        FROM ctactes WHERE ID_CLIENTE = :WS-ID-CLIENTE-SQL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 7 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-CLIENTE-SQL TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0002 TO WS-SALDO-ACTUAL-SQL
                   .
           PERFORM 9000-EVALUAR-SQL.

      * Bajamos el resultado del buffer SQL a la Linkage para las valida
           MOVE WS-SALDO-ACTUAL-SQL TO CTA-SALDO-ACTUAL.

           IF LK-TF-COD-RETORNO = 0
               IF CTA-SALDO-ACTUAL < CTA-MONTO-MOV
                   MOVE 07 TO LK-TF-COD-RETORNO
                   MOVE "SALDO INSUFICIENTE PARA DEBITO"
                   TO LK-TF-MENSAJE
               ELSE
      * Sincronizamos el monto real antes de ejecutar el UPDATE masivo
                   MOVE CTA-MONTO-MOV TO WS-MONTO-MOV-SQL
      *            EXEC SQL
      *                UPDATE ctactes
      *                SET SALDO_ACTUAL =
      *                 SALDO_ACTUAL - :WS-MONTO-MOV-SQL,
      *                    COD_ULT_MOV = 6,
      *                    IMPORTE_MOV = :WS-MONTO-MOV-SQL,
      *                    FECHA_HORA_ALT = NOW()
      *                WHERE ID_CLIENTE = :WS-ID-CLIENTE-SQL
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 8 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 7 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-MONTO-MOV-SQL
             TO SQL-VAR-0003
           MOVE WS-MONTO-MOV-SQL
             TO SQL-VAR-0003
           MOVE WS-ID-CLIENTE-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
                   PERFORM 9000-EVALUAR-SQL
                   IF LK-TF-COD-RETORNO = 0
                       MOVE "DEBITO APLICADO EXITOSAMENTE"
                       TO LK-TF-MENSAJE
      *                EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
                       COMPUTE CTA-SALDO-ACTUAL =
                        CTA-SALDO-ACTUAL - CTA-MONTO-MOV
                   ELSE
      *                EXEC SQL ROLLBACK END-EXEC
           CALL 'OCSQLRBK' USING SQLCA END-CALL
                   END-IF
               END-IF
           END-IF.

       2000-PROCESAR-CREDITO.
           MOVE CTA-ID-CLIENTE TO WS-ID-CLIENTE-SQL
           MOVE CTA-MONTO-MOV  TO WS-MONTO-MOV-SQL
      *    EXEC SQL
      *        UPDATE ctactes
      *        SET SALDO_ACTUAL = SALDO_ACTUAL + :WS-MONTO-MOV-SQL,
      *            COD_ULT_MOV = 7,
      *            IMPORTE_MOV = :WS-MONTO-MOV-SQL,
      *            FECHA_HORA_ALT = NOW()
      *        WHERE ID_CLIENTE = :WS-ID-CLIENTE-SQL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 8 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 7 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-MONTO-MOV-SQL
             TO SQL-VAR-0003
           MOVE WS-MONTO-MOV-SQL
             TO SQL-VAR-0003
           MOVE WS-ID-CLIENTE-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-TF-COD-RETORNO = 0
               MOVE "CREDITO APLICADO EXITOSAMENTE" TO LK-TF-MENSAJE
      *        EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
               COMPUTE CTA-SALDO-ACTUAL =
               CTA-SALDO-ACTUAL + CTA-MONTO-MOV
           ELSE
      *        EXEC SQL ROLLBACK END-EXEC
           CALL 'OCSQLRBK' USING SQLCA END-CALL
           END-IF.

       3000-CONSULTAR-SALDO.
           MOVE CTA-ID-CLIENTE TO WS-ID-CLIENTE-SQL
      *    EXEC SQL
      *        SELECT SALDO_ACTUAL INTO :WS-SALDO-ACTUAL-SQL
      *        FROM ctactes WHERE ID_CLIENTE = :WS-ID-CLIENTE-SQL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 7 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-CLIENTE-SQL TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0002 TO WS-SALDO-ACTUAL-SQL
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-TF-COD-RETORNO = 0
               MOVE WS-SALDO-ACTUAL-SQL TO CTA-SALDO-ACTUAL
               MOVE "CONSULTA DE SALDO EXITOSA" TO LK-TF-MENSAJE
           END-IF.

       4000-PROCESAR-PAGO-CREDITO.
      * CORRECCIÓN: Sincronizamos las llaves compuestas usando variable
           MOVE CTA-NUM-CREDITO TO WS-NUM-CREDITO-SQL
           MOVE CTA-ID-CLIENTE  TO WS-ID-CLIENTE-SQL

      *    EXEC SQL
      *        SELECT SALDO_PENDIENTE INTO :WS-SALDO-AUX
      *        FROM creditos
      *        WHERE NUM_CREDITO = :WS-NUM-CREDITO-SQL AND
      *        ID_CLIENTE = :WS-ID-CLIENTE-SQL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-NUM-CREDITO-SQL
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 20 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 7 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-CLIENTE-SQL TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
           MOVE SQL-VAR-0004 TO WS-SALDO-AUX
                   .
           PERFORM 9000-EVALUAR-SQL.

           IF LK-TF-COD-RETORNO = 01
               MOVE "CREDITO O CLIENTE INEXISTENTE" TO LK-TF-MENSAJE
           END-IF.

           IF LK-TF-COD-RETORNO = 0
      * Sincronizamos el monto a disminuir en cartera
               MOVE CTA-MONTO-MOV TO WS-MONTO-MOV-SQL
      *        EXEC SQL
      *            UPDATE creditos
      *            SET SALDO_PENDIENTE =
      *             SALDO_PENDIENTE - :WS-MONTO-MOV-SQL,
      *                FECHA_ULT_PAGO = NOW()
      *            WHERE NUM_CREDITO = :WS-NUM-CREDITO-SQL
      *             AND ID_CLIENTE = :WS-ID-CLIENTE-SQL
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-NUM-CREDITO-SQL
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 20 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 7 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-MONTO-MOV-SQL
             TO SQL-VAR-0003
           MOVE WS-ID-CLIENTE-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
               PERFORM 9000-EVALUAR-SQL
               IF LK-TF-COD-RETORNO = 0
                   MOVE "PAGO DE CREDITO PROCESADO OK" TO LK-TF-MENSAJE
      *            EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
               ELSE
      *            EXEC SQL ROLLBACK END-EXEC
           CALL 'OCSQLRBK' USING SQLCA END-CALL
               END-IF
           END-IF.

       9000-EVALUAR-SQL.
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 00 TO LK-TF-COD-RETORNO
                   IF LK-TF-MENSAJE = SPACES
                       MOVE "OPERACION EXITOSA" TO LK-TF-MENSAJE
                   END-IF
               WHEN 100
                   MOVE 01 TO LK-TF-COD-RETORNO
                   MOVE "REGISTRO NO ENCONTRADO EN CORE"
                   TO LK-TF-MENSAJE
               WHEN OTHER
                   MOVE 99 TO LK-TF-COD-RETORNO
                   MOVE "ERROR TECNICO FINANCIERO EN DB"
                    TO LK-TF-MENSAJE
           END-EVALUATE.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-ID-CLIENTE-SQL        IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(13,0)
      *  WS-MONTO-MOV-SQL         IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(15,2)
      *  WS-NUM-CREDITO-SQL       IN USE CHAR(20)
      *  WS-SALDO-ACTUAL-SQL      IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(15,2)
      *  WS-SALDO-AUX             IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(15,2)
      **********************************************************************

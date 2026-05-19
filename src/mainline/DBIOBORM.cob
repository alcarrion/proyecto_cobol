      *================================================================*
      * PROGRAMA : DBIOBORM.sqb                                       *
      * FUNCION  : Capa de Acceso a Datos - Hipotecas v3.0            *
      * TABLA    : hipotecas, control_secuencias, clientes             *
      * ACCIONES : S=Secuencia  A=Alta  C=Consulta  M=Modificar       *
      * NOTA     : El Batch (BAT000) use CUENTA_DEBITO para debitar    *
      *            automaticamente la cuota cada cierre mensual.       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOBORM.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 10.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 10 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 10 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 10 TIMES.
           05 SQL-PREC   PIC X OCCURS 10 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 89.
           05 SQL-STMT   PIC X(89) VALUE 'UPDATE control_secuencias SET 
      -    'ULTIMO_VALOR = ULTIMO_VALOR + 1 WHERE PRODUCTO = ''HIPOTECA'
      -    '''.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 71.
           05 SQL-STMT   PIC X(71) VALUE 'SELECT ULTIMO_VALOR FROM contr
      -    'ol_secuencias WHERE PRODUCTO = ''HIPOTECA'''.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 10.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 186.
           05 SQL-STMT   PIC X(186) VALUE 'INSERT INTO hipotecas (ID_HIP
      -    'OTECA,ID_CLIENTE,CUENTA_DEBITO,MONTO_PRESTAMO,SALDO_DEUDA,TA
      -    'SA_ANUAL,CUOTA_MENSUAL,MESES_MORA,ESTADO_PRESTAMO,FECHA_VENC
      -    'IMIENTO) VALUES (?,?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 59.
           05 SQL-STMT   PIC X(59) VALUE 'UPDATE clientes SET TIENE_HIPO
      -    'TECA = 1 WHERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 165.
           05 SQL-STMT   PIC X(165) VALUE 'SELECT ID_CLIENTE,CUENTA_DEBI
      -    'TO,MONTO_PRESTAMO,SALDO_DEUDA,TASA_ANUAL,CUOTA_MENSUAL,MESES
      -    '_MORA,ESTADO_PRESTAMO,FECHA_VENCIMIENTO FROM hipotecas WHERE
      -    ' ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 93.
           05 SQL-STMT   PIC X(93) VALUE 'UPDATE hipotecas SET SALDO_DEU
      -    'DA = ?,ESTADO_PRESTAMO = ?,MESES_MORA = ? WHERE ID_HIPOTECA 
      -    '= ?'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 59.
           05 SQL-STMT   PIC X(59) VALUE 'UPDATE clientes SET TIENE_HIPO
      -    'TECA = 0 WHERE ID_CLIENTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(9) COMP-3.
           05 SQL-VAR-0003  PIC S9(9) COMP-3.
           05 SQL-VAR-0004  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0005  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0006  PIC S9(3)V9(4) COMP-3.
           05 SQL-VAR-0007  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0008  PIC S9(3) COMP-3.
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

       01  WS-ID-HIPOTECA         PIC 9(08).
       01  WS-ID-CLIENTE          PIC 9(08).
       01  WS-CUENTA-DEBITO       PIC 9(08).
       01  WS-MONTO-PRESTAMO      PIC S9(13)V99.
       01  WS-SALDO-DEUDA         PIC S9(13)V99.
       01  WS-TASA-ANUAL          PIC S9(03)V9999.
       01  WS-CUOTA-MENSUAL       PIC S9(13)V99.
       01  WS-MESES-MORA          PIC 9(03).
       01  WS-ESTADO-PRESTAMO     PIC X(15).
       01  WS-FECHA-VENCIMIENTO   PIC X(10).

      *    EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.

      *--- Espejo de REG-BORM (BORMREC.CPY) ---
       01  LK-REG-BORM.
           05 LK-BORM-ID-HIPOTECA       PIC 9(08).
           05 LK-BORM-ID-CLIENTE        PIC 9(08).
           05 LK-BORM-CUENTA-DEBITO     PIC 9(08).
           05 LK-BORM-MONTO-PRESTAMO    PIC S9(13)V99 COMP-3.
           05 LK-BORM-SALDO-DEUDA       PIC S9(13)V99 COMP-3.
           05 LK-BORM-TASA-ANUAL        PIC S9(03)V9999 COMP-3.
           05 LK-BORM-CUOTA-MENSUAL     PIC S9(13)V99 COMP-3.
           05 LK-BORM-MESES-MORA        PIC 9(03).
           05 LK-BORM-ESTADO-PRESTAMO   PIC X(15).
           05 LK-BORM-FECHA-VENCIMIENTO PIC X(10).

      *--- Espejo de LK-DATOS-SESION (LKCIF.CPY) ---
       01  LK-SESION.
           05 LK-SES-CLIENTE-ID         PIC 9(08).
           05 LK-SES-CEDULA             PIC X(12).
           05 LK-SES-NOMBRE             PIC X(50).
           05 LK-SES-USUARIO            PIC X(08).
           05 LK-SES-TERMINAL           PIC X(04).
           05 LK-SES-SCORE              PIC 9(03).
           05 LK-SES-ESTADO-MORA        PIC X(01).

      *--- Espejo de LK-DATOS-TRANSACCION (LKCIF.CPY) ---
       01  LK-TRANSACCION.
           05 LK-ACCION-DB              PIC X(01).
           05 LK-TX-CLIENTE-ID          PIC 9(08).
           05 LK-COD-RETORNO            PIC X(04).
           05 LK-MENSAJE                PIC X(50).

       PROCEDURE DIVISION USING LK-REG-BORM,
                                LK-SESION,
                                LK-TRANSACCION.

       0000-PRINCIPAL.
           MOVE '00  ' TO LK-COD-RETORNO.
           MOVE SPACES  TO LK-MENSAJE.

           EVALUATE LK-ACCION-DB
               WHEN 'S'
                   PERFORM 0500-GENERAR-SECUENCIA
               WHEN 'A'
                   PERFORM 1000-INSERTAR-HIPOTECA
               WHEN 'C'
                   PERFORM 2000-CONSULTAR-HIPOTECA
               WHEN 'M'
                   PERFORM 3000-ACTUALIZAR-HIPOTECA
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE "ACCION DB NO SOPORTADA" TO LK-MENSAJE
           END-EVALUATE.

           EXIT PROGRAM.

      *----------------------------------------------------------------*
      * 0500 - GENERAR NUMERO DE HIPOTECA DESDE SECUENCIA              *
      *----------------------------------------------------------------*
       0500-GENERAR-SECUENCIA.
      *    EXEC SQL
      *        UPDATE control_secuencias
      *        SET    ULTIMO_VALOR = ULTIMO_VALOR + 1
      *        WHERE  PRODUCTO = 'HIPOTECA'
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

           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = '00  '
      *        EXEC SQL
      *            SELECT ULTIMO_VALOR
      *            INTO   :WS-ID-HIPOTECA
      *            FROM   control_secuencias
      *            WHERE  PRODUCTO = 'HIPOTECA'
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-ID-HIPOTECA

               PERFORM 9000-EVALUAR-SQL

               IF LK-COD-RETORNO = '00  '
                   MOVE WS-ID-HIPOTECA TO LK-BORM-ID-HIPOTECA
                   MOVE "SECUENCIA GENERADA" TO LK-MENSAJE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * 1000 - INSERTAR HIPOTECA NUEVA                                 *
      * CUENTA_DEBITO: cuenta de donde el batch cobra cada mes         *
      *----------------------------------------------------------------*
       1000-INSERTAR-HIPOTECA.
           MOVE LK-BORM-ID-HIPOTECA       TO WS-ID-HIPOTECA.
           MOVE LK-BORM-ID-CLIENTE        TO WS-ID-CLIENTE.
           MOVE LK-BORM-CUENTA-DEBITO     TO WS-CUENTA-DEBITO.
           MOVE LK-BORM-MONTO-PRESTAMO    TO WS-MONTO-PRESTAMO.
           MOVE LK-BORM-SALDO-DEUDA       TO WS-SALDO-DEUDA.
           MOVE LK-BORM-TASA-ANUAL        TO WS-TASA-ANUAL.
           MOVE LK-BORM-CUOTA-MENSUAL     TO WS-CUOTA-MENSUAL.
           MOVE LK-BORM-MESES-MORA        TO WS-MESES-MORA.
           MOVE LK-BORM-ESTADO-PRESTAMO   TO WS-ESTADO-PRESTAMO.
           MOVE LK-BORM-FECHA-VENCIMIENTO TO WS-FECHA-VENCIMIENTO.

      *    EXEC SQL
      *        INSERT INTO hipotecas (
      *            ID_HIPOTECA,
      *            ID_CLIENTE,
      *            CUENTA_DEBITO,
      *            MONTO_PRESTAMO,
      *            SALDO_DEUDA,
      *            TASA_ANUAL,
      *            CUOTA_MENSUAL,
      *            MESES_MORA,
      *            ESTADO_PRESTAMO,
      *            FECHA_VENCIMIENTO
      *        ) VALUES (
      *            :WS-ID-HIPOTECA,
      *            :WS-ID-CLIENTE,
      *            :WS-CUENTA-DEBITO,
      *            :WS-MONTO-PRESTAMO,
      *            :WS-SALDO-DEUDA,
      *            :WS-TASA-ANUAL,
      *            :WS-CUOTA-MENSUAL,
      *            :WS-MESES-MORA,
      *            :WS-ESTADO-PRESTAMO,
      *            :WS-FECHA-VENCIMIENTO
      *        )
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(5)
               MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(6)
               MOVE 4 TO SQL-LEN(6)
               MOVE X'04' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(8)
               MOVE 2 TO SQL-LEN(8)
               MOVE X'00' TO SQL-PREC(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 WS-ESTADO-PRESTAMO
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 15 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 WS-FECHA-VENCIMIENTO
               MOVE 'X' TO SQL-TYPE(10)
               MOVE 10 TO SQL-LEN(10)
               MOVE 10 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-HIPOTECA
             TO SQL-VAR-0001
           MOVE WS-ID-CLIENTE
             TO SQL-VAR-0002
           MOVE WS-CUENTA-DEBITO
             TO SQL-VAR-0003
           MOVE WS-MONTO-PRESTAMO
             TO SQL-VAR-0004
           MOVE WS-SALDO-DEUDA
             TO SQL-VAR-0005
           MOVE WS-TASA-ANUAL
             TO SQL-VAR-0006
           MOVE WS-CUOTA-MENSUAL
             TO SQL-VAR-0007
           MOVE WS-MESES-MORA
             TO SQL-VAR-0008
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .

           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = '00  '
      *        EXEC SQL
      *            UPDATE clientes
      *            SET    TIENE_HIPOTECA = 1
      *            WHERE  ID_CLIENTE = :WS-ID-CLIENTE
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-CLIENTE
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA

               PERFORM 9000-EVALUAR-SQL

               IF LK-COD-RETORNO = '00  '
                   MOVE "HIPOTECA REGISTRADA" TO LK-MENSAJE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * 2000 - CONSULTAR HIPOTECA POR ID                               *
      *----------------------------------------------------------------*
       2000-CONSULTAR-HIPOTECA.
           MOVE LK-BORM-ID-HIPOTECA TO WS-ID-HIPOTECA.

      *    EXEC SQL
      *        SELECT ID_CLIENTE,
      *               CUENTA_DEBITO,
      *               MONTO_PRESTAMO,
      *               SALDO_DEUDA,
      *               TASA_ANUAL,
      *               CUOTA_MENSUAL,
      *               MESES_MORA,
      *               ESTADO_PRESTAMO,
      *               FECHA_VENCIMIENTO
      *        INTO   :WS-ID-CLIENTE,
      *               :WS-CUENTA-DEBITO,
      *               :WS-MONTO-PRESTAMO,
      *               :WS-SALDO-DEUDA,
      *               :WS-TASA-ANUAL,
      *               :WS-CUOTA-MENSUAL,
      *               :WS-MESES-MORA,
      *               :WS-ESTADO-PRESTAMO,
      *               :WS-FECHA-VENCIMIENTO
      *        FROM   hipotecas
      *        WHERE  ID_HIPOTECA = :WS-ID-HIPOTECA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(5)
               MOVE 4 TO SQL-LEN(5)
               MOVE X'04' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(7)
               MOVE 2 TO SQL-LEN(7)
               MOVE X'00' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 WS-ESTADO-PRESTAMO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 15 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 WS-FECHA-VENCIMIENTO
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 10 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(10)
               MOVE 5 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
               MOVE 10 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-HIPOTECA TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
           MOVE SQL-VAR-0002 TO WS-ID-CLIENTE
           MOVE SQL-VAR-0003 TO WS-CUENTA-DEBITO
           MOVE SQL-VAR-0004 TO WS-MONTO-PRESTAMO
           MOVE SQL-VAR-0005 TO WS-SALDO-DEUDA
           MOVE SQL-VAR-0006 TO WS-TASA-ANUAL
           MOVE SQL-VAR-0007 TO WS-CUOTA-MENSUAL
           MOVE SQL-VAR-0008 TO WS-MESES-MORA
                   .

           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = '00  '
               MOVE WS-ID-CLIENTE        TO LK-BORM-ID-CLIENTE
               MOVE WS-CUENTA-DEBITO     TO LK-BORM-CUENTA-DEBITO
               MOVE WS-MONTO-PRESTAMO    TO LK-BORM-MONTO-PRESTAMO
               MOVE WS-SALDO-DEUDA       TO LK-BORM-SALDO-DEUDA
               MOVE WS-TASA-ANUAL        TO LK-BORM-TASA-ANUAL
               MOVE WS-CUOTA-MENSUAL     TO LK-BORM-CUOTA-MENSUAL
               MOVE WS-MESES-MORA        TO LK-BORM-MESES-MORA
               MOVE WS-ESTADO-PRESTAMO   TO LK-BORM-ESTADO-PRESTAMO
               MOVE WS-FECHA-VENCIMIENTO TO LK-BORM-FECHA-VENCIMIENTO
               MOVE "CONSULTA EXITOSA"   TO LK-MENSAJE
           END-IF.

      *----------------------------------------------------------------*
      * 3000 - ACTUALIZAR HIPOTECA                                     *
      * Usado por BR0000 (pago manual) y por BAT000 (debito batch)     *
      *----------------------------------------------------------------*
       3000-ACTUALIZAR-HIPOTECA.
           MOVE LK-BORM-ID-HIPOTECA     TO WS-ID-HIPOTECA.
           MOVE LK-BORM-ID-CLIENTE      TO WS-ID-CLIENTE.
           MOVE LK-BORM-SALDO-DEUDA     TO WS-SALDO-DEUDA.
           MOVE LK-BORM-ESTADO-PRESTAMO TO WS-ESTADO-PRESTAMO.
           MOVE LK-BORM-MESES-MORA      TO WS-MESES-MORA.

      *    EXEC SQL
      *        UPDATE hipotecas
      *        SET    SALDO_DEUDA     = :WS-SALDO-DEUDA,
      *               ESTADO_PRESTAMO = :WS-ESTADO-PRESTAMO,
      *               MESES_MORA      = :WS-MESES-MORA
      *        WHERE  ID_HIPOTECA     = :WS-ID-HIPOTECA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-ESTADO-PRESTAMO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 15 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(3)
               MOVE 2 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(4)
               MOVE 5 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-SALDO-DEUDA
             TO SQL-VAR-0005
           MOVE WS-MESES-MORA
             TO SQL-VAR-0008
           MOVE WS-ID-HIPOTECA
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
                   .

           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = '00  '
               IF WS-ESTADO-PRESTAMO = "PAGADO"
      *            EXEC SQL
      *                UPDATE clientes
      *                SET    TIENE_HIPOTECA = 0
      *                WHERE  ID_CLIENTE = :WS-ID-CLIENTE
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-CLIENTE
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA
                   PERFORM 9000-EVALUAR-SQL
               END-IF

               IF LK-COD-RETORNO = '00  '
                   MOVE "HIPOTECA ACTUALIZADA" TO LK-MENSAJE
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * 9000 - EVALUAR RESULTADO SQL                                   *
      *----------------------------------------------------------------*
       9000-EVALUAR-SQL.
           EVALUATE SQLCODE
               WHEN 0
                   MOVE '00  ' TO LK-COD-RETORNO
                   IF LK-MENSAJE = SPACES
                       MOVE "OPERACION EXITOSA" TO LK-MENSAJE
                   END-IF
               WHEN 100
                   MOVE 'E404' TO LK-COD-RETORNO
                   MOVE "HIPOTECA NO ENCONTRADA" TO LK-MENSAJE
               WHEN -803
                   MOVE 'E409' TO LK-COD-RETORNO
                   MOVE "HIPOTECA DUPLICADA" TO LK-MENSAJE
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE "ERROR TECNICO EN BD HIPOTECAS" TO LK-MENSAJE
           END-EVALUATE.

      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-CUENTA-DEBITO         IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(9,0)
      *  WS-CUOTA-MENSUAL         IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(15,2)
      *  WS-ESTADO-PRESTAMO       IN USE CHAR(15)
      *  WS-FECHA-VENCIMIENTO     IN USE CHAR(10)
      *  WS-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(9,0)
      *  WS-ID-HIPOTECA           IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  WS-MESES-MORA            IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(3,0)
      *  WS-MONTO-PRESTAMO        IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(15,2)
      *  WS-SALDO-DEUDA           IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(15,2)
      *  WS-TASA-ANUAL            IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(7,4)
      **********************************************************************

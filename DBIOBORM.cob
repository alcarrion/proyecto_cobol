      *================================================================*
      * PROGRAMA : DBIOBORM.sqb                                       *
      * FUNCION  : Capa de Acceso a Datos - Hipotecas                 *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOBORM.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 12.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 12 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 12 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 12 TIMES.
           05 SQL-PREC   PIC X OCCURS 12 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 96.
           05 SQL-STMT   PIC X(96) VALUE 'UPDATE control_secuencias SET 
      -    'ULTIMO_NUMERO = ULTIMO_NUMERO + 1 WHERE TIPO_PRODUCTO = ''HI
      -    'POTECA'''.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 77.
           05 SQL-STMT   PIC X(77) VALUE 'SELECT ULTIMO_NUMERO FROM cont
      -    'rol_secuencias WHERE TIPO_PRODUCTO = ''HIPOTECA'''.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 11.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 205.
           05 SQL-STMT   PIC X(205) VALUE 'INSERT INTO hipotecas (ID_HIP
      -    'OTECA,ID_CLIENTE,FECHA_INICIO,MONTO_ORIGINAL,TASA_INTERES,SA
      -    'LDO_ACTUAL,FECHA_VENCTO,DIA_PAGO,ESTADO,CUOTA_MENSUAL,MESES_
      -    'MORA,FECHA_ULT_PAGO) VALUES (?,?,?,?,?,?,?,?,?,?,?,NULL)'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 12.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 202.
           05 SQL-STMT   PIC X(202) VALUE 'INSERT INTO hipotecas (ID_HIP
      -    'OTECA,ID_CLIENTE,FECHA_INICIO,MONTO_ORIGINAL,TASA_INTERES,SA
      -    'LDO_ACTUAL,FECHA_VENCTO,DIA_PAGO,ESTADO,CUOTA_MENSUAL,MESES_
      -    'MORA,FECHA_ULT_PAGO) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 65.
           05 SQL-STMT   PIC X(65) VALUE 'UPDATE clientes SET HIPOTECA =
      -    ' 1,CREDITO = 1 WHERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 200.
           05 SQL-STMT   PIC X(200) VALUE 'SELECT ID_CLIENTE,FECHA_INICI
      -    'O,MONTO_ORIGINAL,TASA_INTERES,SALDO_ACTUAL,FECHA_VENCTO,DIA_
      -    'PAGO,ESTADO,CUOTA_MENSUAL,MESES_MORA,COALESCE(FECHA_ULT_PAGO
      -    ',''0000-00-00'') FROM hipotecas WHERE ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 5.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 125.
           05 SQL-STMT   PIC X(125) VALUE 'UPDATE hipotecas SET SALDO_AC
      -    'TUAL = ?,ESTADO = ?,CUOTA_MENSUAL = ?,MESES_MORA = ?,FECHA_U
      -    'LT_PAGO = NULL WHERE ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 6.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 122.
           05 SQL-STMT   PIC X(122) VALUE 'UPDATE hipotecas SET SALDO_AC
      -    'TUAL = ?,ESTADO = ?,CUOTA_MENSUAL = ?,MESES_MORA = ?,FECHA_U
      -    'LT_PAGO = ? WHERE ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-8.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 53.
           05 SQL-STMT   PIC X(53) VALUE 'UPDATE clientes SET HIPOTECA =
      -    ' 0 WHERE ID_CLIENTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(9) COMP-3.
           05 SQL-VAR-0003  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(3)V9(4) COMP-3.
           05 SQL-VAR-0005  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0006  PIC S9(3) COMP-3.
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

      *----------------------------------------------------------------*
      * Variables de trabajo internas para calculos                    *
      *----------------------------------------------------------------*
       01  WS-FECHA-ULT-CHECK     PIC X(10).

      *----------------------------------------------------------------*
      * DECLARE SECTION solo con variables simples sin COPY            *
      * esqlOC necesita ver las variables host directamente            *
      *----------------------------------------------------------------*
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  WS-ID-HIPOTECA         PIC 9(09).
       01  WS-ID-CLIENTE          PIC 9(08).
       01  WS-FECHA-INICIO        PIC X(10).
       01  WS-MONTO-ORIGINAL      PIC S9(13)V99.
       01  WS-TASA-INTERES        PIC S9(03)V9999.
       01  WS-SALDO-ACTUAL        PIC S9(13)V99.
       01  WS-FECHA-VENCTO        PIC X(10).
       01  WS-DIA-PAGO            PIC 9(02).
       01  WS-ESTADO              PIC X(20).
       01  WS-CUOTA-MENSUAL       PIC S9(13)V99.
       01  WS-MESES-MORA          PIC 9(03).
       01  WS-FECHA-ULT-PAGO      PIC X(10).

      *    EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.

       01  LK-DATOS-TRANSACCION.
           05 LK-ACCION-DB             PIC X(01).
           05 LK-ID-CLIENTE            PIC 9(09).
           05 LK-IMPORTE-TRANSACCION   PIC S9(13)V99 COMP-3.
           05 LK-MODO-OPERACION        PIC X(01).
           05 LK-COD-RETORNO           PIC 9(02).
           05 LK-MENSAJE               PIC X(50).
           05 LK-USUARIO-ID            PIC X(08).
           05 LK-TERMINAL-ID           PIC X(04).
           05 LK-FECHA-PROCESO         PIC 9(08).

       01  BORM-REGISTRO.
           05 BORM-ID-HIPOTECA         PIC 9(09).
           05 BORM-ID-CLIENTE          PIC 9(08).
           05 BORM-FECHA-INICIO        PIC X(10).
           05 BORM-MONTO-ORIGINAL      PIC S9(13)V99.
           05 BORM-TASA-INTERES        PIC S9(03)V9999.
           05 BORM-SALDO-ACTUAL        PIC S9(13)V99.
           05 BORM-FECHA-VENCTO        PIC X(10).
           05 BORM-DIA-PAGO            PIC 9(02).
           05 BORM-ESTADO              PIC X(20).
           05 BORM-CUOTA-MENSUAL       PIC S9(13)V99.
           05 BORM-MESES-MORA          PIC 9(03).
           05 BORM-FECHA-ULT-PAGO      PIC X(10).

       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION,
                                BORM-REGISTRO.

       0000-PRINCIPAL.
           MOVE 0      TO LK-COD-RETORNO.
           MOVE SPACES TO LK-MENSAJE.

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
                   MOVE 98 TO LK-COD-RETORNO
                   MOVE "ACCION DB NO SOPORTADA"
                       TO LK-MENSAJE
           END-EVALUATE.

           EXIT PROGRAM.

      *================================================================*
      * 0500 - GENERAR SECUENCIA                                      *
      *================================================================*
       0500-GENERAR-SECUENCIA.
      *    EXEC SQL
      *        UPDATE control_secuencias
      *        SET    ULTIMO_NUMERO = ULTIMO_NUMERO + 1
      *        WHERE  TIPO_PRODUCTO = 'HIPOTECA'
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

           IF LK-COD-RETORNO = 0
      *        EXEC SQL
      *            SELECT ULTIMO_NUMERO
      *            INTO   :WS-ID-HIPOTECA
      *            FROM   control_secuencias
      *            WHERE  TIPO_PRODUCTO = 'HIPOTECA'
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
               IF LK-COD-RETORNO = 0
                   MOVE WS-ID-HIPOTECA TO BORM-ID-HIPOTECA
                   MOVE "SECUENCIA GENERADA" TO LK-MENSAJE
               END-IF
           END-IF.

      *================================================================*
      * 1000 - INSERTAR HIPOTECA                                      *
      *================================================================*
       1000-INSERTAR-HIPOTECA.
           MOVE BORM-ID-HIPOTECA    TO WS-ID-HIPOTECA.
           MOVE BORM-ID-CLIENTE     TO WS-ID-CLIENTE.
           MOVE BORM-FECHA-INICIO   TO WS-FECHA-INICIO.
           MOVE BORM-MONTO-ORIGINAL TO WS-MONTO-ORIGINAL.
           MOVE BORM-TASA-INTERES   TO WS-TASA-INTERES.
           MOVE BORM-SALDO-ACTUAL   TO WS-SALDO-ACTUAL.
           MOVE BORM-FECHA-VENCTO   TO WS-FECHA-VENCTO.
           MOVE BORM-DIA-PAGO       TO WS-DIA-PAGO.
           MOVE BORM-ESTADO         TO WS-ESTADO.
           MOVE BORM-CUOTA-MENSUAL  TO WS-CUOTA-MENSUAL.
           MOVE BORM-MESES-MORA     TO WS-MESES-MORA.
           MOVE BORM-FECHA-ULT-PAGO TO WS-FECHA-ULT-CHECK.

           IF WS-FECHA-ULT-CHECK = "0000-00-00"
              OR WS-FECHA-ULT-CHECK = SPACES
      *        EXEC SQL
      *            INSERT INTO hipotecas (
      *                ID_HIPOTECA,
      *                ID_CLIENTE,
      *                FECHA_INICIO,
      *                MONTO_ORIGINAL,
      *                TASA_INTERES,
      *                SALDO_ACTUAL,
      *                FECHA_VENCTO,
      *                DIA_PAGO,
      *                ESTADO,
      *                CUOTA_MENSUAL,
      *                MESES_MORA,
      *                FECHA_ULT_PAGO
      *            ) VALUES (
      *                :WS-ID-HIPOTECA,
      *                :WS-ID-CLIENTE,
      *                :WS-FECHA-INICIO,
      *                :WS-MONTO-ORIGINAL,
      *                :WS-TASA-INTERES,
      *                :WS-SALDO-ACTUAL,
      *                :WS-FECHA-VENCTO,
      *                :WS-DIA-PAGO,
      *                :WS-ESTADO,
      *                :WS-CUOTA-MENSUAL,
      *                :WS-MESES-MORA,
      *                NULL
      *            )
      *        END-EXEC
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
                 WS-FECHA-INICIO
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(5)
               MOVE 4 TO SQL-LEN(5)
               MOVE X'04' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 WS-FECHA-VENCTO
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 10 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(8)
               MOVE 2 TO SQL-LEN(8)
               MOVE X'00' TO SQL-PREC(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 WS-ESTADO
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 20 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(10)
               MOVE 8 TO SQL-LEN(10)
               MOVE X'02' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(11)
               MOVE 2 TO SQL-LEN(11)
               MOVE X'00' TO SQL-PREC(11)
               MOVE 11 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-HIPOTECA
             TO SQL-VAR-0001
           MOVE WS-ID-CLIENTE
             TO SQL-VAR-0002
           MOVE WS-MONTO-ORIGINAL
             TO SQL-VAR-0003
           MOVE WS-TASA-INTERES
             TO SQL-VAR-0004
           MOVE WS-SALDO-ACTUAL
             TO SQL-VAR-0005
           MOVE WS-DIA-PAGO
             TO SQL-VAR-0006
           MOVE WS-CUOTA-MENSUAL
             TO SQL-VAR-0007
           MOVE WS-MESES-MORA
             TO SQL-VAR-0008
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
           ELSE
               MOVE BORM-FECHA-ULT-PAGO TO WS-FECHA-ULT-PAGO
      *        EXEC SQL
      *            INSERT INTO hipotecas (
      *                ID_HIPOTECA,
      *                ID_CLIENTE,
      *                FECHA_INICIO,
      *                MONTO_ORIGINAL,
      *                TASA_INTERES,
      *                SALDO_ACTUAL,
      *                FECHA_VENCTO,
      *                DIA_PAGO,
      *                ESTADO,
      *                CUOTA_MENSUAL,
      *                MESES_MORA,
      *                FECHA_ULT_PAGO
      *            ) VALUES (
      *                :WS-ID-HIPOTECA,
      *                :WS-ID-CLIENTE,
      *                :WS-FECHA-INICIO,
      *                :WS-MONTO-ORIGINAL,
      *                :WS-TASA-INTERES,
      *                :WS-SALDO-ACTUAL,
      *                :WS-FECHA-VENCTO,
      *                :WS-DIA-PAGO,
      *                :WS-ESTADO,
      *                :WS-CUOTA-MENSUAL,
      *                :WS-MESES-MORA,
      *                :WS-FECHA-ULT-PAGO
      *            )
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-3 = 'N'
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
                 WS-FECHA-INICIO
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(5)
               MOVE 4 TO SQL-LEN(5)
               MOVE X'04' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 WS-FECHA-VENCTO
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 10 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(8)
               MOVE 2 TO SQL-LEN(8)
               MOVE X'00' TO SQL-PREC(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 WS-ESTADO
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 20 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(10)
               MOVE 8 TO SQL-LEN(10)
               MOVE X'02' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(11)
               MOVE 2 TO SQL-LEN(11)
               MOVE X'00' TO SQL-PREC(11)
               SET SQL-ADDR(12) TO ADDRESS OF
                 WS-FECHA-ULT-PAGO
               MOVE 'X' TO SQL-TYPE(12)
               MOVE 10 TO SQL-LEN(12)
               MOVE 12 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-HIPOTECA
             TO SQL-VAR-0001
           MOVE WS-ID-CLIENTE
             TO SQL-VAR-0002
           MOVE WS-MONTO-ORIGINAL
             TO SQL-VAR-0003
           MOVE WS-TASA-INTERES
             TO SQL-VAR-0004
           MOVE WS-SALDO-ACTUAL
             TO SQL-VAR-0005
           MOVE WS-DIA-PAGO
             TO SQL-VAR-0006
           MOVE WS-CUOTA-MENSUAL
             TO SQL-VAR-0007
           MOVE WS-MESES-MORA
             TO SQL-VAR-0008
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
           END-IF.

           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = 0
      *        EXEC SQL
      *            UPDATE clientes
      *            SET    HIPOTECA = 1,
      *                   CREDITO  = 1
      *            WHERE  ID_CLIENTE = :WS-ID-CLIENTE
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-CLIENTE
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
               PERFORM 9000-EVALUAR-SQL
               IF LK-COD-RETORNO = 0
                   MOVE "HIPOTECA REGISTRADA" TO LK-MENSAJE
               END-IF
           END-IF.

      *================================================================*
      * 2000 - CONSULTAR HIPOTECA                                     *
      *================================================================*
       2000-CONSULTAR-HIPOTECA.
           MOVE BORM-ID-HIPOTECA TO WS-ID-HIPOTECA.

      *    EXEC SQL
      *        SELECT ID_CLIENTE,
      *               FECHA_INICIO,
      *               MONTO_ORIGINAL,
      *               TASA_INTERES,
      *               SALDO_ACTUAL,
      *               FECHA_VENCTO,
      *               DIA_PAGO,
      *               ESTADO,
      *               CUOTA_MENSUAL,
      *               MESES_MORA,
      *               COALESCE(FECHA_ULT_PAGO, '0000-00-00')
      *        INTO   :WS-ID-CLIENTE,
      *               :WS-FECHA-INICIO,
      *               :WS-MONTO-ORIGINAL,
      *               :WS-TASA-INTERES,
      *               :WS-SALDO-ACTUAL,
      *               :WS-FECHA-VENCTO,
      *               :WS-DIA-PAGO,
      *               :WS-ESTADO,
      *               :WS-CUOTA-MENSUAL,
      *               :WS-MESES-MORA,
      *               :WS-FECHA-ULT-PAGO
      *        FROM   hipotecas
      *        WHERE  ID_HIPOTECA = :WS-ID-HIPOTECA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-FECHA-INICIO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(4)
               MOVE 4 TO SQL-LEN(4)
               MOVE X'04' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(5)
               MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 WS-FECHA-VENCTO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 10 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(7)
               MOVE 2 TO SQL-LEN(7)
               MOVE X'00' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 WS-ESTADO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 20 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(9)
               MOVE 8 TO SQL-LEN(9)
               MOVE X'02' TO SQL-PREC(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(10)
               MOVE 2 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 WS-FECHA-ULT-PAGO
               MOVE 'X' TO SQL-TYPE(11)
               MOVE 10 TO SQL-LEN(11)
               SET SQL-ADDR(12) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(12)
               MOVE 5 TO SQL-LEN(12)
               MOVE X'00' TO SQL-PREC(12)
               MOVE 12 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-HIPOTECA TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
           MOVE SQL-VAR-0002 TO WS-ID-CLIENTE
           MOVE SQL-VAR-0003 TO WS-MONTO-ORIGINAL
           MOVE SQL-VAR-0004 TO WS-TASA-INTERES
           MOVE SQL-VAR-0005 TO WS-SALDO-ACTUAL
           MOVE SQL-VAR-0006 TO WS-DIA-PAGO
           MOVE SQL-VAR-0007 TO WS-CUOTA-MENSUAL
           MOVE SQL-VAR-0008 TO WS-MESES-MORA
                   .
           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = 0
               MOVE WS-ID-CLIENTE     TO BORM-ID-CLIENTE
               MOVE WS-FECHA-INICIO   TO BORM-FECHA-INICIO
               MOVE WS-MONTO-ORIGINAL TO BORM-MONTO-ORIGINAL
               MOVE WS-TASA-INTERES   TO BORM-TASA-INTERES
               MOVE WS-SALDO-ACTUAL   TO BORM-SALDO-ACTUAL
               MOVE WS-FECHA-VENCTO   TO BORM-FECHA-VENCTO
               MOVE WS-DIA-PAGO       TO BORM-DIA-PAGO
               MOVE WS-ESTADO         TO BORM-ESTADO
               MOVE WS-CUOTA-MENSUAL  TO BORM-CUOTA-MENSUAL
               MOVE WS-MESES-MORA     TO BORM-MESES-MORA
               MOVE WS-FECHA-ULT-PAGO TO BORM-FECHA-ULT-PAGO
               MOVE "CONSULTA EXITOSA" TO LK-MENSAJE
           END-IF.

      *================================================================*
      * 3000 - ACTUALIZAR HIPOTECA                                    *
      *================================================================*
       3000-ACTUALIZAR-HIPOTECA.
           MOVE BORM-ID-HIPOTECA    TO WS-ID-HIPOTECA.
           MOVE BORM-ID-CLIENTE     TO WS-ID-CLIENTE.
           MOVE BORM-SALDO-ACTUAL   TO WS-SALDO-ACTUAL.
           MOVE BORM-ESTADO         TO WS-ESTADO.
           MOVE BORM-CUOTA-MENSUAL  TO WS-CUOTA-MENSUAL.
           MOVE BORM-MESES-MORA     TO WS-MESES-MORA.
           MOVE BORM-FECHA-ULT-PAGO TO WS-FECHA-ULT-CHECK.

           IF WS-FECHA-ULT-CHECK = "0000-00-00"
              OR WS-FECHA-ULT-CHECK = SPACES
      *        EXEC SQL
      *            UPDATE hipotecas
      *            SET    SALDO_ACTUAL   = :WS-SALDO-ACTUAL,
      *                   ESTADO         = :WS-ESTADO,
      *                   CUOTA_MENSUAL  = :WS-CUOTA-MENSUAL,
      *                   MESES_MORA     = :WS-MESES-MORA,
      *                   FECHA_ULT_PAGO = NULL
      *            WHERE  ID_HIPOTECA    = :WS-ID-HIPOTECA
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-ESTADO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 20 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(4)
               MOVE 2 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(5)
               MOVE 5 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               MOVE 5 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-SALDO-ACTUAL
             TO SQL-VAR-0005
           MOVE WS-CUOTA-MENSUAL
             TO SQL-VAR-0007
           MOVE WS-MESES-MORA
             TO SQL-VAR-0008
           MOVE WS-ID-HIPOTECA
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA
           ELSE
               MOVE BORM-FECHA-ULT-PAGO TO WS-FECHA-ULT-PAGO
      *        EXEC SQL
      *            UPDATE hipotecas
      *            SET    SALDO_ACTUAL   = :WS-SALDO-ACTUAL,
      *                   ESTADO         = :WS-ESTADO,
      *                   CUOTA_MENSUAL  = :WS-CUOTA-MENSUAL,
      *                   MESES_MORA     = :WS-MESES-MORA,
      *                   FECHA_ULT_PAGO = :WS-FECHA-ULT-PAGO
      *            WHERE  ID_HIPOTECA    = :WS-ID-HIPOTECA
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-7 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-ESTADO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 20 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(4)
               MOVE 2 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 WS-FECHA-ULT-PAGO
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 10 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(6)
               MOVE 5 TO SQL-LEN(6)
               MOVE X'00' TO SQL-PREC(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-7
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-SALDO-ACTUAL
             TO SQL-VAR-0005
           MOVE WS-CUOTA-MENSUAL
             TO SQL-VAR-0007
           MOVE WS-MESES-MORA
             TO SQL-VAR-0008
           MOVE WS-ID-HIPOTECA
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA
           END-IF.

           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = 0
               IF BORM-ESTADO = "CANCELADO"
      *            EXEC SQL
      *                UPDATE clientes
      *                SET    HIPOTECA = 0
      *                WHERE  ID_CLIENTE = :WS-ID-CLIENTE
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-8 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-8
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-CLIENTE
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-8
                               SQLCA
                   PERFORM 9000-EVALUAR-SQL
               END-IF
               IF LK-COD-RETORNO = 0
                   MOVE "HIPOTECA ACTUALIZADA" TO LK-MENSAJE
               END-IF
           END-IF.

      *================================================================*
      * 9000 - EVALUAR SQLCODE                                        *
      *================================================================*
       9000-EVALUAR-SQL.
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 00 TO LK-COD-RETORNO
                   IF LK-MENSAJE = SPACES
                       MOVE "OPERACION EXITOSA"
                           TO LK-MENSAJE
                   END-IF
               WHEN 100
                   MOVE 01 TO LK-COD-RETORNO
                   MOVE "HIPOTECA NO ENCONTRADA"
                       TO LK-MENSAJE
               WHEN -803
                   MOVE 02 TO LK-COD-RETORNO
                   MOVE "HIPOTECA DUPLICADA"
                       TO LK-MENSAJE
               WHEN OTHER
                   MOVE 99 TO LK-COD-RETORNO
                   MOVE "ERROR TECNICO EN DB HIPOTECAS"
                       TO LK-MENSAJE
           END-EVALUATE.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-CUOTA-MENSUAL         IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(15,2)
      *  WS-DIA-PAGO              IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(3,0)
      *  WS-ESTADO                IN USE CHAR(20)
      *  WS-FECHA-INICIO          IN USE CHAR(10)
      *  WS-FECHA-ULT-PAGO        IN USE CHAR(10)
      *  WS-FECHA-VENCTO          IN USE CHAR(10)
      *  WS-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(9,0)
      *  WS-ID-HIPOTECA           IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  WS-MESES-MORA            IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(3,0)
      *  WS-MONTO-ORIGINAL        IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(15,2)
      *  WS-SALDO-ACTUAL          IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(15,2)
      *  WS-TASA-INTERES          IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(7,4)
      **********************************************************************

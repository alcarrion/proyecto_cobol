      *================================================================*
      * PROGRAMA : DBIOBORM.sqb                                       *
      * FUNCION  : Capa de Acceso a Datos - Hipotecas                 *
      *                                                                *
      * IMPORTANTE:                                                   *
      * Este programa es llamado desde BR0000 con:                     *
      * CALL 'DBIOBORM' USING LK-DATOS-TRANSACCION, BORM-REGISTRO     *
      * Por eso debe tener PROCEDURE DIVISION USING esos dos registros.*
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
           05 SQL-VAR-0004  PIC S9(9) COMP-3.
           05 SQL-VAR-0005  PIC S9(9) COMP-3.
           05 SQL-VAR-0006  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0007  PIC S9(3)V9(4) COMP-3.
           05 SQL-VAR-0008  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0009  PIC S9(3) COMP-3.
           05 SQL-VAR-0010  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0011  PIC S9(3) COMP-3.
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

       LINKAGE SECTION.
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.

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
           05 BORM-ESTADO              PIC X(10).
           05 BORM-CUOTA-MENSUAL       PIC S9(13)V99.
           05 BORM-MESES-MORA          PIC 9(03).
           05 BORM-FECHA-ULT-PAGO      PIC X(10).

      *    EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION,
                                BORM-REGISTRO.

       0000-PRINCIPAL.
           MOVE 0 TO LK-COD-RETORNO.
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
                   MOVE "ACCION DB NO SOPORTADA" TO LK-MENSAJE
           END-EVALUATE.

           EXIT PROGRAM.

       0500-GENERAR-SECUENCIA.
      *    EXEC SQL
      *        UPDATE control_secuencias
      *        SET ULTIMO_NUMERO = ULTIMO_NUMERO + 1
      *        WHERE TIPO_PRODUCTO = 'HIPOTECA'
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
      *            INTO :BORM-ID-HIPOTECA
      *            FROM control_secuencias
      *            WHERE TIPO_PRODUCTO = 'HIPOTECA'
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0004
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
           MOVE SQL-VAR-0004 TO BORM-ID-HIPOTECA

               PERFORM 9000-EVALUAR-SQL

               IF LK-COD-RETORNO = 0
                   MOVE "SECUENCIA GENERADA" TO LK-MENSAJE
               END-IF
           END-IF.

       1000-INSERTAR-HIPOTECA.
           IF BORM-FECHA-ULT-PAGO = "0000-00-00"
              OR BORM-FECHA-ULT-PAGO = SPACES
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
      *                :BORM-ID-HIPOTECA,
      *                :BORM-ID-CLIENTE,
      *                :BORM-FECHA-INICIO,
      *                :BORM-MONTO-ORIGINAL,
      *                :BORM-TASA-INTERES,
      *                :BORM-SALDO-ACTUAL,
      *                :BORM-FECHA-VENCTO,
      *                :BORM-DIA-PAGO,
      *                :BORM-ESTADO,
      *                :BORM-CUOTA-MENSUAL,
      *                :BORM-MESES-MORA,
      *                NULL
      *            )
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 BORM-FECHA-INICIO
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(5)
               MOVE 4 TO SQL-LEN(5)
               MOVE X'04' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 BORM-FECHA-VENCTO
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 10 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(8)
               MOVE 2 TO SQL-LEN(8)
               MOVE X'00' TO SQL-PREC(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 BORM-ESTADO
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 10 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0010
               MOVE '3' TO SQL-TYPE(10)
               MOVE 8 TO SQL-LEN(10)
               MOVE X'02' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0011
               MOVE '3' TO SQL-TYPE(11)
               MOVE 2 TO SQL-LEN(11)
               MOVE X'00' TO SQL-PREC(11)
               MOVE 11 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE BORM-ID-HIPOTECA
             TO SQL-VAR-0004
           MOVE BORM-ID-CLIENTE
             TO SQL-VAR-0005
           MOVE BORM-MONTO-ORIGINAL
             TO SQL-VAR-0006
           MOVE BORM-TASA-INTERES
             TO SQL-VAR-0007
           MOVE BORM-SALDO-ACTUAL
             TO SQL-VAR-0008
           MOVE BORM-DIA-PAGO
             TO SQL-VAR-0009
           MOVE BORM-CUOTA-MENSUAL
             TO SQL-VAR-0010
           MOVE BORM-MESES-MORA
             TO SQL-VAR-0011
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
           ELSE
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
      *                :BORM-ID-HIPOTECA,
      *                :BORM-ID-CLIENTE,
      *                :BORM-FECHA-INICIO,
      *                :BORM-MONTO-ORIGINAL,
      *                :BORM-TASA-INTERES,
      *                :BORM-SALDO-ACTUAL,
      *                :BORM-FECHA-VENCTO,
      *                :BORM-DIA-PAGO,
      *                :BORM-ESTADO,
      *                :BORM-CUOTA-MENSUAL,
      *                :BORM-MESES-MORA,
      *                :BORM-FECHA-ULT-PAGO
      *            )
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 BORM-FECHA-INICIO
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(5)
               MOVE 4 TO SQL-LEN(5)
               MOVE X'04' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 BORM-FECHA-VENCTO
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 10 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(8)
               MOVE 2 TO SQL-LEN(8)
               MOVE X'00' TO SQL-PREC(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 BORM-ESTADO
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 10 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0010
               MOVE '3' TO SQL-TYPE(10)
               MOVE 8 TO SQL-LEN(10)
               MOVE X'02' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0011
               MOVE '3' TO SQL-TYPE(11)
               MOVE 2 TO SQL-LEN(11)
               MOVE X'00' TO SQL-PREC(11)
               SET SQL-ADDR(12) TO ADDRESS OF
                 BORM-FECHA-ULT-PAGO
               MOVE 'X' TO SQL-TYPE(12)
               MOVE 10 TO SQL-LEN(12)
               MOVE 12 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE BORM-ID-HIPOTECA
             TO SQL-VAR-0004
           MOVE BORM-ID-CLIENTE
             TO SQL-VAR-0005
           MOVE BORM-MONTO-ORIGINAL
             TO SQL-VAR-0006
           MOVE BORM-TASA-INTERES
             TO SQL-VAR-0007
           MOVE BORM-SALDO-ACTUAL
             TO SQL-VAR-0008
           MOVE BORM-DIA-PAGO
             TO SQL-VAR-0009
           MOVE BORM-CUOTA-MENSUAL
             TO SQL-VAR-0010
           MOVE BORM-MESES-MORA
             TO SQL-VAR-0011
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
           END-IF.

           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = 0
      *        EXEC SQL
      *            UPDATE clientes
      *            SET HIPOTECA = 1,
      *                CREDITO = 1
      *            WHERE ID_CLIENTE = :BORM-ID-CLIENTE
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE BORM-ID-CLIENTE
             TO SQL-VAR-0005
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA

               PERFORM 9000-EVALUAR-SQL

               IF LK-COD-RETORNO = 0
                   MOVE "HIPOTECA REGISTRADA" TO LK-MENSAJE
               END-IF
           END-IF.

       2000-CONSULTAR-HIPOTECA.
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
      *        INTO :BORM-ID-CLIENTE,
      *             :BORM-FECHA-INICIO,
      *             :BORM-MONTO-ORIGINAL,
      *             :BORM-TASA-INTERES,
      *             :BORM-SALDO-ACTUAL,
      *             :BORM-FECHA-VENCTO,
      *             :BORM-DIA-PAGO,
      *             :BORM-ESTADO,
      *             :BORM-CUOTA-MENSUAL,
      *             :BORM-MESES-MORA,
      *             :BORM-FECHA-ULT-PAGO
      *        FROM hipotecas
      *        WHERE ID_HIPOTECA = :BORM-ID-HIPOTECA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 BORM-FECHA-INICIO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(4)
               MOVE 4 TO SQL-LEN(4)
               MOVE X'04' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(5)
               MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 BORM-FECHA-VENCTO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 10 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(7)
               MOVE 2 TO SQL-LEN(7)
               MOVE X'00' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 BORM-ESTADO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 10 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 SQL-VAR-0010
               MOVE '3' TO SQL-TYPE(9)
               MOVE 8 TO SQL-LEN(9)
               MOVE X'02' TO SQL-PREC(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0011
               MOVE '3' TO SQL-TYPE(10)
               MOVE 2 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 BORM-FECHA-ULT-PAGO
               MOVE 'X' TO SQL-TYPE(11)
               MOVE 10 TO SQL-LEN(11)
               SET SQL-ADDR(12) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(12)
               MOVE 5 TO SQL-LEN(12)
               MOVE X'00' TO SQL-PREC(12)
               MOVE 12 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE BORM-ID-HIPOTECA TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
           MOVE SQL-VAR-0005 TO BORM-ID-CLIENTE
           MOVE SQL-VAR-0006 TO BORM-MONTO-ORIGINAL
           MOVE SQL-VAR-0007 TO BORM-TASA-INTERES
           MOVE SQL-VAR-0008 TO BORM-SALDO-ACTUAL
           MOVE SQL-VAR-0009 TO BORM-DIA-PAGO
           MOVE SQL-VAR-0010 TO BORM-CUOTA-MENSUAL
           MOVE SQL-VAR-0011 TO BORM-MESES-MORA
                   .

           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = 0
               MOVE "CONSULTA EXITOSA" TO LK-MENSAJE
           END-IF.

       3000-ACTUALIZAR-HIPOTECA.
           IF BORM-FECHA-ULT-PAGO = "0000-00-00"
              OR BORM-FECHA-ULT-PAGO = SPACES
      *        EXEC SQL
      *            UPDATE hipotecas
      *            SET SALDO_ACTUAL = :BORM-SALDO-ACTUAL,
      *                ESTADO = :BORM-ESTADO,
      *                CUOTA_MENSUAL = :BORM-CUOTA-MENSUAL,
      *                MESES_MORA = :BORM-MESES-MORA,
      *                FECHA_ULT_PAGO = NULL
      *            WHERE ID_HIPOTECA = :BORM-ID-HIPOTECA
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 BORM-ESTADO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0010
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0011
               MOVE '3' TO SQL-TYPE(4)
               MOVE 2 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(5)
               MOVE 5 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               MOVE 5 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE BORM-SALDO-ACTUAL
             TO SQL-VAR-0008
           MOVE BORM-CUOTA-MENSUAL
             TO SQL-VAR-0010
           MOVE BORM-MESES-MORA
             TO SQL-VAR-0011
           MOVE BORM-ID-HIPOTECA
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA
           ELSE
      *        EXEC SQL
      *            UPDATE hipotecas
      *            SET SALDO_ACTUAL = :BORM-SALDO-ACTUAL,
      *                ESTADO = :BORM-ESTADO,
      *                CUOTA_MENSUAL = :BORM-CUOTA-MENSUAL,
      *                MESES_MORA = :BORM-MESES-MORA,
      *                FECHA_ULT_PAGO = :BORM-FECHA-ULT-PAGO
      *            WHERE ID_HIPOTECA = :BORM-ID-HIPOTECA
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-7 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 BORM-ESTADO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0010
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0011
               MOVE '3' TO SQL-TYPE(4)
               MOVE 2 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 BORM-FECHA-ULT-PAGO
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 10 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(6)
               MOVE 5 TO SQL-LEN(6)
               MOVE X'00' TO SQL-PREC(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-7
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE BORM-SALDO-ACTUAL
             TO SQL-VAR-0008
           MOVE BORM-CUOTA-MENSUAL
             TO SQL-VAR-0010
           MOVE BORM-MESES-MORA
             TO SQL-VAR-0011
           MOVE BORM-ID-HIPOTECA
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA
           END-IF.

           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = 0
               IF BORM-ESTADO = "CANCELADO"
      *            EXEC SQL
      *                UPDATE clientes
      *                SET HIPOTECA = 0
      *                WHERE ID_CLIENTE = :BORM-ID-CLIENTE
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-8 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-8
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE BORM-ID-CLIENTE
             TO SQL-VAR-0005
           CALL 'OCSQLEXE' USING SQL-STMT-8
                               SQLCA

                   PERFORM 9000-EVALUAR-SQL
               END-IF

               IF LK-COD-RETORNO = 0
                   MOVE "HIPOTECA ACTUALIZADA" TO LK-MENSAJE
               END-IF
           END-IF.

       9000-EVALUAR-SQL.
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 00 TO LK-COD-RETORNO
                   IF LK-MENSAJE = SPACES
                       MOVE "OPERACION EXITOSA" TO LK-MENSAJE
                   END-IF
               WHEN 100
                   MOVE 01 TO LK-COD-RETORNO
                   MOVE "HIPOTECA NO ENCONTRADA" TO LK-MENSAJE
               WHEN -803
                   MOVE 02 TO LK-COD-RETORNO
                   MOVE "HIPOTECA DUPLICADA" TO LK-MENSAJE
               WHEN OTHER
                   MOVE 99 TO LK-COD-RETORNO
                   MOVE "ERROR TECNICO EN DB HIPOTECAS" TO LK-MENSAJE
           END-EVALUATE.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  BORM-CUOTA-MENSUAL       IN USE THROUGH TEMP VAR SQL-VAR-0010 DECIMAL(15,2)
      *  BORM-DIA-PAGO            IN USE THROUGH TEMP VAR SQL-VAR-0009 DECIMAL(3,0)
      *  BORM-ESTADO              IN USE CHAR(10)
      *  BORM-FECHA-INICIO        IN USE CHAR(10)
      *  BORM-FECHA-ULT-PAGO      IN USE CHAR(10)
      *  BORM-FECHA-VENCTO        IN USE CHAR(10)
      *  BORM-ID-CLIENTE          IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(9,0)
      *  BORM-ID-HIPOTECA         IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(9,0)
      *  BORM-MESES-MORA          IN USE THROUGH TEMP VAR SQL-VAR-0011 DECIMAL(3,0)
      *  BORM-MONTO-ORIGINAL      IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(15,2)
      *  BORM-REGISTRO        NOT IN USE
      *  BORM-REGISTRO.BORM-CUOTA-MENSUAL NOT IN USE
      *  BORM-REGISTRO.BORM-DIA-PAGO NOT IN USE
      *  BORM-REGISTRO.BORM-ESTADO NOT IN USE
      *  BORM-REGISTRO.BORM-FECHA-INICIO NOT IN USE
      *  BORM-REGISTRO.BORM-FECHA-ULT-PAGO NOT IN USE
      *  BORM-REGISTRO.BORM-FECHA-VENCTO NOT IN USE
      *  BORM-REGISTRO.BORM-ID-CLIENTE NOT IN USE
      *  BORM-REGISTRO.BORM-ID-HIPOTECA NOT IN USE
      *  BORM-REGISTRO.BORM-MESES-MORA NOT IN USE
      *  BORM-REGISTRO.BORM-MONTO-ORIGINAL NOT IN USE
      *  BORM-REGISTRO.BORM-SALDO-ACTUAL NOT IN USE
      *  BORM-REGISTRO.BORM-TASA-INTERES NOT IN USE
      *  BORM-SALDO-ACTUAL        IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(15,2)
      *  BORM-TASA-INTERES        IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(7,4)
      *  LK-ACCION-DB         NOT IN USE
      *  LK-COD-RETORNO       NOT IN USE
      *  LK-DATOS-TRANSACCION NOT IN USE
      *  LK-DATOS-TRANSACCION.LK-ACCION-DB NOT IN USE
      *  LK-DATOS-TRANSACCION.LK-COD-RETORNO NOT IN USE
      *  LK-DATOS-TRANSACCION.LK-FECHA-PROCESO NOT IN USE
      *  LK-DATOS-TRANSACCION.LK-ID-CLIENTE NOT IN USE
      *  LK-DATOS-TRANSACCION.LK-IMPORTE-TRANSACCION NOT IN USE
      *  LK-DATOS-TRANSACCION.LK-MENSAJE NOT IN USE
      *  LK-DATOS-TRANSACCION.LK-MODO-OPERACION NOT IN USE
      *  LK-DATOS-TRANSACCION.LK-TERMINAL-ID NOT IN USE
      *  LK-DATOS-TRANSACCION.LK-USUARIO-ID NOT IN USE
      *  LK-FECHA-PROCESO     NOT IN USE
      *  LK-ID-CLIENTE        NOT IN USE
      *  LK-IMPORTE-TRANSACCION NOT IN USE
      *  LK-MENSAJE           NOT IN USE
      *  LK-MODO-OPERACION    NOT IN USE
      *  LK-TERMINAL-ID       NOT IN USE
      *  LK-USUARIO-ID        NOT IN USE
      **********************************************************************

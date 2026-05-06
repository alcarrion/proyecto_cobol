       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOBORM.
      *================================================================*
      * MODULO: DBIOBORM (Embedded SQL para Hipotecas)                 *
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 9.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 9 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 9 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 9 TIMES.
           05 SQL-PREC   PIC X OCCURS 9 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 8.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 142.
           05 SQL-STMT   PIC X(142) VALUE 'INSERT INTO HIPOTECAS (ID_CLI
      -    'ENTE,FECHA_INICIO,MONTO_ORIGINAL,TASA_INTERES,SALDO_ACTUAL,F
      -    'ECHA_VENCTO,DIA_PAGO,ESTADO) VALUES (?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 145.
           05 SQL-STMT   PIC X(145) VALUE 'SELECT ID_HIPOTECA,FECHA_INIC
      -    'IO,MONTO_ORIGINAL,TASA_INTERES,SALDO_ACTUAL,FECHA_VENCTO,DIA
      -    '_PAGO,ESTADO FROM HIPOTECAS WHERE ID_CLIENTE = ? LIMIT 1'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 73.
           05 SQL-STMT   PIC X(73) VALUE 'UPDATE HIPOTECAS SET SALDO_ACT
      -    'UAL = SALDO_ACTUAL - ? WHERE ID_CLIENTE = ?'.
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
       01  WS-BORM.
           05 WS-ID-HIPOTECA       PIC 9(09).
           05 WS-ID-CLIENTE        PIC 9(09).
           05 WS-FECHA-INICIO      PIC X(10).
           05 WS-MONTO-ORIGINAL    PIC S9(13)V99.
           05 WS-TASA-INTERES      PIC S9(03)V9999.
           05 WS-SALDO-ACTUAL      PIC S9(13)V99.
           05 WS-FECHA-VENCTO      PIC X(10).
           05 WS-DIA-PAGO          PIC 9(02).
           05 WS-ESTADO            PIC X(20).

           05 WS-PAGO-M            PIC S9(13)V99.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.
       01  LK-BORM-REGISTRO.
           05 LK-BORM-ID-HIPOTECA       PIC 9(09).
           05 LK-BORM-ID-CLIENTE        PIC 9(09).
           05 LK-BORM-FECHA-INICIO      PIC X(10).
           05 LK-BORM-MONTO-ORIGINAL    PIC 9(13)V99.
           05 LK-BORM-TASA-INTERES      PIC 9(03)V9999.
           05 LK-BORM-SALDO-ACTUAL      PIC 9(13)V99.
           05 LK-BORM-FECHA-VENCTO      PIC X(10).
           05 LK-BORM-DIA-PAGO          PIC 9(02).
           05 LK-BORM-ESTADO            PIC X(20).

       COPY LKCIF.

       PROCEDURE DIVISION USING LK-BORM-REGISTRO, LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 0 TO LK-COD-RETORNO.
           MOVE SPACES TO LK-MENSAJE.

           EVALUATE LK-MODO-OPERACION
               WHEN 'A'
                   PERFORM 1000-INSERTAR
               WHEN 'C'
                   PERFORM 2000-CONSULTAR
               WHEN 'U'
                   PERFORM 3000-ACTUALIZAR-PAGO
               WHEN OTHER
                   MOVE 99 TO LK-COD-RETORNO
                   MOVE "OPERACION NO VALIDA PARA BORM" TO LK-MENSAJE
           END-EVALUATE.

           GOBACK.

       1000-INSERTAR.
      *    Generar un ID de Hipoteca simple usando la hora o algo fijo (
      *    pero COBOL debe pasarlo si no lo lee despues). Asumiremos que
           MOVE 9999 TO WS-ID-HIPOTECA.
           MOVE LK-BORM-ID-CLIENTE     TO WS-ID-CLIENTE.
           MOVE LK-BORM-FECHA-INICIO   TO WS-FECHA-INICIO.
           MOVE LK-BORM-MONTO-ORIGINAL TO WS-MONTO-ORIGINAL.
           MOVE LK-BORM-TASA-INTERES   TO WS-TASA-INTERES.
           MOVE LK-BORM-SALDO-ACTUAL   TO WS-SALDO-ACTUAL.
           MOVE LK-BORM-FECHA-VENCTO   TO WS-FECHA-VENCTO.
           MOVE LK-BORM-DIA-PAGO       TO WS-DIA-PAGO.
           MOVE LK-BORM-ESTADO         TO WS-ESTADO.

      *     EXEC SQL
      *         INSERT INTO HIPOTECAS (
      *             ID_CLIENTE, FECHA_INICIO, MONTO_ORIGINAL,
      *             TASA_INTERES, SALDO_ACTUAL, FECHA_VENCTO,
      *             DIA_PAGO, ESTADO
      *         ) VALUES (
      *             :WS-ID-CLIENTE, :WS-FECHA-INICIO,
      *             :WS-MONTO-ORIGINAL, :WS-TASA-INTERES,
      *             :WS-SALDO-ACTUAL, :WS-FECHA-VENCTO,
      *             :WS-DIA-PAGO, :WS-ESTADO
      *         )
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
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
               MOVE 8 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
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
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .

           IF SQLCODE = 0
               MOVE "HIPOTECA CREADA EXITOSAMENTE EN BD" TO LK-MENSAJE
           ELSE
               MOVE 99 TO LK-COD-RETORNO
               STRING "ERROR INSERT HIPOTECA: SQLCODE=" SQLCODE
                   DELIMITED BY SIZE INTO LK-MENSAJE
           END-IF.

       2000-CONSULTAR.
           MOVE LK-BORM-ID-CLIENTE TO WS-ID-CLIENTE.

      *     EXEC SQL
      *         SELECT ID_HIPOTECA, FECHA_INICIO, MONTO_ORIGINAL,
      *                TASA_INTERES, SALDO_ACTUAL, FECHA_VENCTO,
      *                DIA_PAGO, ESTADO
      *         INTO :WS-ID-HIPOTECA, :WS-FECHA-INICIO,
      *              :WS-MONTO-ORIGINAL, :WS-TASA-INTERES,
      *              :WS-SALDO-ACTUAL, :WS-FECHA-VENCTO,
      *              :WS-DIA-PAGO, :WS-ESTADO
      *         FROM HIPOTECAS
      *        WHERE ID_CLIENTE = :WS-ID-CLIENTE
      *        LIMIT 1
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
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
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(9)
               MOVE 5 TO SQL-LEN(9)
               MOVE X'00' TO SQL-PREC(9)
               MOVE 9 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-CLIENTE TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-ID-HIPOTECA
           MOVE SQL-VAR-0003 TO WS-MONTO-ORIGINAL
           MOVE SQL-VAR-0004 TO WS-TASA-INTERES
           MOVE SQL-VAR-0005 TO WS-SALDO-ACTUAL
           MOVE SQL-VAR-0006 TO WS-DIA-PAGO
                   .

           IF SQLCODE = 0
               MOVE WS-ID-HIPOTECA     TO LK-BORM-ID-HIPOTECA
               MOVE WS-FECHA-INICIO    TO LK-BORM-FECHA-INICIO
               MOVE WS-MONTO-ORIGINAL  TO LK-BORM-MONTO-ORIGINAL
               MOVE WS-TASA-INTERES    TO LK-BORM-TASA-INTERES
               MOVE WS-SALDO-ACTUAL    TO LK-BORM-SALDO-ACTUAL
               MOVE WS-FECHA-VENCTO    TO LK-BORM-FECHA-VENCTO
               MOVE WS-DIA-PAGO        TO LK-BORM-DIA-PAGO
               MOVE WS-ESTADO          TO LK-BORM-ESTADO
               MOVE "HIPOTECA ENCONTRADA" TO LK-MENSAJE
           ELSE
               MOVE 1 TO LK-COD-RETORNO
               MOVE "HIPOTECA NO ENCONTRADA" TO LK-MENSAJE
           END-IF.

       3000-ACTUALIZAR-PAGO.
           MOVE LK-BORM-ID-CLIENTE TO WS-ID-CLIENTE.
           MOVE LK-IMPORTE-TRANSACCION TO WS-PAGO-M.

      *    EXEC SQL
      *        UPDATE HIPOTECAS
      *        SET SALDO_ACTUAL = SALDO_ACTUAL - :WS-PAGO-M
      *        WHERE ID_CLIENTE = :WS-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-PAGO-M
             TO SQL-VAR-0007
           MOVE WS-ID-CLIENTE
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .

           IF SQLCODE = 0
               MOVE "PAGO REGISTRADO, SALDO ACTUALIZADO" TO LK-MENSAJE
           ELSE
               MOVE 99 TO LK-COD-RETORNO
               STRING "ERROR UPDATE HIPOTECA: SQLCODE=" SQLCODE
                   DELIMITED BY SIZE INTO LK-MENSAJE
           END-IF.

       END PROGRAM DBIOBORM.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-BORM              NOT IN USE
      *  WS-BORM.WS-DIA-PAGO  NOT IN USE
      *  WS-BORM.WS-ESTADO    NOT IN USE
      *  WS-BORM.WS-FECHA-INICIO NOT IN USE
      *  WS-BORM.WS-FECHA-VENCTO NOT IN USE
      *  WS-BORM.WS-ID-CLIENTE NOT IN USE
      *  WS-BORM.WS-ID-HIPOTECA NOT IN USE
      *  WS-BORM.WS-MONTO-ORIGINAL NOT IN USE
      *  WS-BORM.WS-PAGO-M    NOT IN USE
      *  WS-BORM.WS-SALDO-ACTUAL NOT IN USE
      *  WS-BORM.WS-TASA-INTERES NOT IN USE
      *  WS-DIA-PAGO              IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(3,0)
      *  WS-ESTADO                IN USE CHAR(20)
      *  WS-FECHA-INICIO          IN USE CHAR(10)
      *  WS-FECHA-VENCTO          IN USE CHAR(10)
      *  WS-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(9,0)
      *  WS-ID-HIPOTECA           IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  WS-MONTO-ORIGINAL        IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(15,2)
      *  WS-PAGO-M                IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(15,2)
      *  WS-SALDO-ACTUAL          IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(15,2)
      *  WS-TASA-INTERES          IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(7,4)
      **********************************************************************

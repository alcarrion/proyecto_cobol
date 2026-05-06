       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOBORM.
      *================================================================*
      * PROYECTO      : proyecto_cobol                                 *
      * MODULO        : CAPA DE ACCESO A DATOS (DBIO) - HIPOTECAS      *
      * DESCRIPCION   : Ejecuta operaciones CRUD en la tabla HIPOTECAS *
      *                 utilizando SQL embebido para MySQL.            *
      *================================================================*

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
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 9.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 156.
           05 SQL-STMT   PIC X(156) VALUE 'INSERT INTO HIPOTECAS (ID_HIP
      -    'OTECA,ID_CLIENTE,FECHA_INICIO,MONTO_ORIGINAL,TASA_INTERES,SA
      -    'LDO_ACTUAL,FECHA_VENCTO,DIA_PAGO,ESTADO) VALUES (?,?,?,?,?,?
      -    ',?,?,?)'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 137.
           05 SQL-STMT   PIC X(137) VALUE 'SELECT ID_CLIENTE,FECHA_INICI
      -    'O,MONTO_ORIGINAL,TASA_INTERES,SALDO_ACTUAL,FECHA_VENCTO,DIA_
      -    'PAGO,ESTADO FROM HIPOTECAS WHERE ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'UPDATE HIPOTECAS SET SALDO_ACT
      -    'UAL = ?,ESTADO = ? WHERE ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 43.
           05 SQL-STMT   PIC X(43) VALUE 'DELETE FROM HIPOTECAS WHERE ID
      -    '_HIPOTECA = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(3) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************
      *=================================================================
      * SQL COMMUNICATION AREA: Captura errores y estados de MySQL
      *=================================================================
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
      *=================================================================
      * BLOQUE DE DECLARACION PARA EL PRE-COMPILADOR (esqlOC)
      *=================================================================
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.

      * Se expande BORM-REGISTRO para asegurar visibilidad en esqlOC
       01  BORM-REGISTRO.
           05 BORM-ID-HIPOTECA      PIC 9(09)  COMP-5.
           05 BORM-ID-CLIENTE       PIC 9(08)  COMP-5.
           05 BORM-FECHA-INICIO     PIC X(10).
           05 BORM-MONTO-ORIGINAL   PIC S9(13)V99 COMP-3.
           05 BORM-TASA-INTERES     PIC S9(03)V9999 COMP-3.
           05 BORM-SALDO-ACTUAL     PIC S9(13)V99 COMP-3.
           05 BORM-FECHA-VENCTO     PIC X(10).
           05 BORM-DIA-PAGO         PIC 9(02).
           05 BORM-ESTADO           PIC X(20).

      * Sincronizado con el COPY LKCIF del BANCSMENU y BR0000
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

      *    EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION BORM-REGISTRO.
       0000-MAIN-DBIO.
           INITIALIZE LK-COD-RETORNO

           EVALUATE LK-ACCION-DB
               WHEN 'I'
                   PERFORM 1000-INSERTAR-HIPOTECA
               WHEN 'C'
                   PERFORM 2000-CONSULTAR-HIPOTECA
               WHEN 'U'
                   PERFORM 3000-ACTUALIZAR-HIPOTECA
               WHEN 'D'
                   PERFORM 4000-ELIMINAR-HIPOTECA
               WHEN OTHER
                   MOVE 01 TO LK-COD-RETORNO *> Código de error de acci
           END-EVALUATE

           *> Mapeo de SQLCODE al estándar de retorno del core bancario
           IF SQLCODE = 0
               MOVE 00 TO LK-COD-RETORNO
           ELSE
               *> 99 indica error crítico de base de datos
               MOVE 99 TO LK-COD-RETORNO
           END-IF

           GOBACK.

      *----------------------------------------------------------------*
      * 1000-INSERTAR-HIPOTECA: Alta de nuevo préstamo                 *
      *----------------------------------------------------------------*
       1000-INSERTAR-HIPOTECA.
      *    EXEC SQL
      *        INSERT INTO HIPOTECAS (
      *            ID_HIPOTECA,
      *            ID_CLIENTE,
      *            FECHA_INICIO,
      *            MONTO_ORIGINAL,
      *            TASA_INTERES,
      *            SALDO_ACTUAL,
      *            FECHA_VENCTO,
      *            DIA_PAGO,
      *            ESTADO
      *        ) VALUES (
      *            :BORM-ID-HIPOTECA,
      *            :BORM-ID-CLIENTE,
      *            :BORM-FECHA-INICIO,
      *            :BORM-MONTO-ORIGINAL,
      *            :BORM-TASA-INTERES,
      *            :BORM-SALDO-ACTUAL,
      *            :BORM-FECHA-VENCTO,
      *            :BORM-DIA-PAGO,
      *            :BORM-ESTADO
      *        )
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 BORM-ID-HIPOTECA
               MOVE 'I' TO SQL-TYPE(1)
               MOVE 4 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 BORM-ID-CLIENTE
               MOVE 'I' TO SQL-TYPE(2)
               MOVE 4 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 BORM-FECHA-INICIO
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 BORM-MONTO-ORIGINAL
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 BORM-TASA-INTERES
               MOVE '3' TO SQL-TYPE(5)
               MOVE 4 TO SQL-LEN(5)
               MOVE X'04' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 BORM-SALDO-ACTUAL
               MOVE '3' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 BORM-FECHA-VENCTO
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 10 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(8)
               MOVE 2 TO SQL-LEN(8)
               MOVE X'00' TO SQL-PREC(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 BORM-ESTADO
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 20 TO SQL-LEN(9)
               MOVE 9 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE BORM-DIA-PAGO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .

      *----------------------------------------------------------------*
      * 2000-CONSULTAR-HIPOTECA: Recupera datos por ID_HIPOTECA        *
      *----------------------------------------------------------------*
       2000-CONSULTAR-HIPOTECA.
      *    EXEC SQL
      *        SELECT
      *            ID_CLIENTE,
      *            FECHA_INICIO,
      *            MONTO_ORIGINAL,
      *            TASA_INTERES,
      *            SALDO_ACTUAL,
      *            FECHA_VENCTO,
      *            DIA_PAGO,
      *            ESTADO
      *        INTO
      *            :BORM-ID-CLIENTE,
      *            :BORM-FECHA-INICIO,
      *            :BORM-MONTO-ORIGINAL,
      *            :BORM-TASA-INTERES,
      *            :BORM-SALDO-ACTUAL,
      *            :BORM-FECHA-VENCTO,
      *            :BORM-DIA-PAGO,
      *            :BORM-ESTADO
      *        FROM HIPOTECAS
      *        WHERE ID_HIPOTECA = :BORM-ID-HIPOTECA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 BORM-ID-CLIENTE
               MOVE 'I' TO SQL-TYPE(1)
               MOVE 4 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 BORM-FECHA-INICIO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 BORM-MONTO-ORIGINAL
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 BORM-TASA-INTERES
               MOVE '3' TO SQL-TYPE(4)
               MOVE 4 TO SQL-LEN(4)
               MOVE X'04' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 BORM-SALDO-ACTUAL
               MOVE '3' TO SQL-TYPE(5)
               MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 BORM-FECHA-VENCTO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 10 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(7)
               MOVE 2 TO SQL-LEN(7)
               MOVE X'00' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 BORM-ESTADO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 20 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 BORM-ID-HIPOTECA
               MOVE 'I' TO SQL-TYPE(9)
               MOVE 4 TO SQL-LEN(9)
               MOVE 9 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0001 TO BORM-DIA-PAGO
                   .

      *----------------------------------------------------------------*
      * 3000-ACTUALIZAR-HIPOTECA: Modifica saldo y estado              *
      *----------------------------------------------------------------*
       3000-ACTUALIZAR-HIPOTECA.
      *    EXEC SQL
      *        UPDATE HIPOTECAS
      *        SET SALDO_ACTUAL = :BORM-SALDO-ACTUAL,
      *            ESTADO       = :BORM-ESTADO
      *        WHERE ID_HIPOTECA = :BORM-ID-HIPOTECA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 BORM-SALDO-ACTUAL
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 BORM-ESTADO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 20 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 BORM-ID-HIPOTECA
               MOVE 'I' TO SQL-TYPE(3)
               MOVE 4 TO SQL-LEN(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .

      *----------------------------------------------------------------*
      * 4000-ELIMINAR-HIPOTECA: Baja física                            *
      *----------------------------------------------------------------*
       4000-ELIMINAR-HIPOTECA.
      *    EXEC SQL
      *        DELETE FROM HIPOTECAS
      *        WHERE ID_HIPOTECA = :BORM-ID-HIPOTECA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 BORM-ID-HIPOTECA
               MOVE 'I' TO SQL-TYPE(1)
               MOVE 4 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
                   .
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  BORM-DIA-PAGO            IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(3,0)
      *  BORM-ESTADO              IN USE CHAR(20)
      *  BORM-FECHA-INICIO        IN USE CHAR(10)
      *  BORM-FECHA-VENCTO        IN USE CHAR(10)
      *  BORM-ID-CLIENTE          IN USE INTEGER(4 BYTES)
      *  BORM-ID-HIPOTECA         IN USE INTEGER(4 BYTES)
      *  BORM-MONTO-ORIGINAL      IN USE DECIMAL(15,2)
      *  BORM-REGISTRO        NOT IN USE
      *  BORM-REGISTRO.BORM-DIA-PAGO NOT IN USE
      *  BORM-REGISTRO.BORM-ESTADO NOT IN USE
      *  BORM-REGISTRO.BORM-FECHA-INICIO NOT IN USE
      *  BORM-REGISTRO.BORM-FECHA-VENCTO NOT IN USE
      *  BORM-REGISTRO.BORM-ID-CLIENTE NOT IN USE
      *  BORM-REGISTRO.BORM-ID-HIPOTECA NOT IN USE
      *  BORM-REGISTRO.BORM-MONTO-ORIGINAL NOT IN USE
      *  BORM-REGISTRO.BORM-SALDO-ACTUAL NOT IN USE
      *  BORM-REGISTRO.BORM-TASA-INTERES NOT IN USE
      *  BORM-SALDO-ACTUAL        IN USE DECIMAL(15,2)
      *  BORM-TASA-INTERES        IN USE DECIMAL(7,4)
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

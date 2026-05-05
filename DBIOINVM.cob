      *================================================================*
      * PROGRAMA: DBIOINVM.sqb                                         *
      * FUNCION:  CRUD de la tabla ctactes (Cuentas Corrientes)        *
      * ARQUITECTURA: Capa de Acceso a Datos (DBIO)                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOINVM.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 5.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 5 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 5 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 5 TIMES.
           05 SQL-PREC   PIC X OCCURS 5 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 5.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 102.
           05 SQL-STMT   PIC X(102) VALUE 'INSERT INTO ctactes (ID_CLIEN
      -    'TE,COD_ULT_MOV,FECHA_ULT_MOV,IMPORTE_MOV,SALDO_ACTUAL) VALUE
      -    'S (?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 91.
           05 SQL-STMT   PIC X(91) VALUE 'SELECT COD_ULT_MOV,FECHA_ULT_M
      -    'OV,IMPORTE_MOV,SALDO_ACTUAL FROM ctactes WHERE ID_CLIENTE = 
      -    '?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 5.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 106.
           05 SQL-STMT   PIC X(106) VALUE 'UPDATE ctactes SET COD_ULT_MO
      -    'V = ?,FECHA_ULT_MOV = ?,IMPORTE_MOV = ?,SALDO_ACTUAL = ? WHE
      -    'RE ID_CLIENTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(3) COMP-3.
           05 SQL-VAR-0003  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(11)V9(2) COMP-3.
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
       01  REG-INVM.
           05 INVM-ID-CLIENTE      PIC 9(08).
           05 INVM-COD-ULT-MOV     PIC 9(02).
           05 INVM-FECHA-ULT-MOV   PIC X(10).
           05 INVM-IMPORTE-MOV     PIC S9(10)V99.
           05 INVM-SALDO-ACTUAL    PIC S9(10)V99.
           COPY LKCIF.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION USING REG-INVM, LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 0 TO LK-COD-RETORNO
           MOVE SPACES TO LK-MENSAJE

           EVALUATE LK-ACCION-DB
               WHEN 'A'
                   PERFORM 1000-INSERTAR-CUENTA
               WHEN 'C'
                   PERFORM 2000-CONSULTAR-CUENTA
               WHEN 'M'
                   PERFORM 3000-ACTUALIZAR-SALDO
               WHEN OTHER
                   MOVE 98 TO LK-COD-RETORNO
                   MOVE "ACCION DB NO SOPORTADA" TO LK-MENSAJE
           END-EVALUATE.

           EXIT PROGRAM.

       1000-INSERTAR-CUENTA.
      *    EXEC SQL
      *        INSERT INTO ctactes (
      *            ID_CLIENTE,
      *            COD_ULT_MOV,
      *            FECHA_ULT_MOV,
      *            IMPORTE_MOV,
      *            SALDO_ACTUAL
      *        ) VALUES (
      *            :INVM-ID-CLIENTE,
      *            :INVM-COD-ULT-MOV,
      *            :INVM-FECHA-ULT-MOV,
      *            :INVM-IMPORTE-MOV,
      *            :INVM-SALDO-ACTUAL
      *        )
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 INVM-FECHA-ULT-MOV
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(4)
               MOVE 7 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               MOVE 5 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE INVM-ID-CLIENTE
             TO SQL-VAR-0001
           MOVE INVM-COD-ULT-MOV
             TO SQL-VAR-0002
           MOVE INVM-IMPORTE-MOV
             TO SQL-VAR-0003
           MOVE INVM-SALDO-ACTUAL
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = 0
      *        EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
               MOVE "CUENTA CORRIENTE REGISTRADA CON EXITO" TO LK-MENSAJ
           END-IF.

       2000-CONSULTAR-CUENTA.
      *    EXEC SQL
      *        SELECT COD_ULT_MOV, FECHA_ULT_MOV,
      *               IMPORTE_MOV, SALDO_ACTUAL
      *        INTO :INVM-COD-ULT-MOV,
      *             :INVM-FECHA-ULT-MOV,
      *             :INVM-IMPORTE-MOV,
      *             :INVM-SALDO-ACTUAL
      *        FROM ctactes
      *        WHERE ID_CLIENTE = :INVM-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 INVM-FECHA-ULT-MOV
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(3)
               MOVE 7 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(4)
               MOVE 7 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(5)
               MOVE 5 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               MOVE 5 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE INVM-ID-CLIENTE TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0002 TO INVM-COD-ULT-MOV
           MOVE SQL-VAR-0003 TO INVM-IMPORTE-MOV
           MOVE SQL-VAR-0004 TO INVM-SALDO-ACTUAL
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = 0
      *        EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
               MOVE "CONSULTA EXITOSA" TO LK-MENSAJE
           END-IF.

       3000-ACTUALIZAR-SALDO.
      *    EXEC SQL
      *        UPDATE ctactes
      *        SET COD_ULT_MOV = :INVM-COD-ULT-MOV,
      *            FECHA_ULT_MOV = :INVM-FECHA-ULT-MOV,
      *            IMPORTE_MOV = :INVM-IMPORTE-MOV,
      *            SALDO_ACTUAL = :INVM-SALDO-ACTUAL
      *        WHERE ID_CLIENTE = :INVM-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 INVM-FECHA-ULT-MOV
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(3)
               MOVE 7 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(4)
               MOVE 7 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(5)
               MOVE 5 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               MOVE 5 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE INVM-COD-ULT-MOV
             TO SQL-VAR-0002
           MOVE INVM-IMPORTE-MOV
             TO SQL-VAR-0003
           MOVE INVM-SALDO-ACTUAL
             TO SQL-VAR-0004
           MOVE INVM-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = 0
      *        EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
               MOVE "SALDO ACTUALIZADO" TO LK-MENSAJE
           END-IF.

       9000-EVALUAR-SQL.
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 00 TO LK-COD-RETORNO
               WHEN 100
                   MOVE 01 TO LK-COD-RETORNO
                   MOVE "CUENTA NO ENCONTRADA" TO LK-MENSAJE
               WHEN OTHER
                   MOVE 99 TO LK-COD-RETORNO
                   MOVE "ERROR CRITICO EN BASE DE DATOS" TO LK-MENSAJE
           END-EVALUATE.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  INVM-COD-ULT-MOV         IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(3,0)
      *  INVM-FECHA-ULT-MOV       IN USE CHAR(10)
      *  INVM-ID-CLIENTE          IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  INVM-IMPORTE-MOV         IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(13,2)
      *  INVM-SALDO-ACTUAL        IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(13,2)
      *  LKCIF                NOT IN USE
      *  REG-INVM             NOT IN USE
      *  REG-INVM.INVM-COD-ULT-MOV NOT IN USE
      *  REG-INVM.INVM-FECHA-ULT-MOV NOT IN USE
      *  REG-INVM.INVM-ID-CLIENTE NOT IN USE
      *  REG-INVM.INVM-IMPORTE-MOV NOT IN USE
      *  REG-INVM.INVM-SALDO-ACTUAL NOT IN USE
      *  REG-INVM.LKCIF       NOT IN USE
      **********************************************************************

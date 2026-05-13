      *================================================================*
      * PROGRAMA: DBIOTARJ.sqb                                         *
      * FUNCION:  Capa de Acceso a Datos - Tarjetas de Credito         *
      * ARQUITECTURA: Capa de Acceso a Datos (DBIO)                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOTARJ.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 8.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 8 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 8 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 8 TIMES.
           05 SQL-PREC   PIC X OCCURS 8 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 8.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 149.
           05 SQL-STMT   PIC X(149) VALUE 'INSERT INTO TARJETAS (ID_CLIE
      -    'NTE,NRO_TARJETA,FECHA_EMISION,FECHA_VENCIMIENTO,LIMITE_TARJE
      -    'TA,ACUM_MES,LIQUIDACION_MES,ESTADO) VALUES (?,?,?,?,?,?,?,?)
      -    ''.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 52.
           05 SQL-STMT   PIC X(52) VALUE 'UPDATE clientes SET TARJETA = 
      -    '1 WHERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 132.
           05 SQL-STMT   PIC X(132) VALUE 'SELECT ID_CLIENTE,FECHA_EMISI
      -    'ON,FECHA_VENCIMIENTO,LIMITE_TARJETA,ACUM_MES,LIQUIDACION_MES
      -    ',ESTADO FROM TARJETAS WHERE NRO_TARJETA = ?'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 85.
           05 SQL-STMT   PIC X(85) VALUE 'UPDATE TARJETAS SET ACUM_MES =
      -    ' ?,LIQUIDACION_MES = ?,ESTADO = ? WHERE NRO_TARJETA = ?'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 54.
           05 SQL-STMT   PIC X(54) VALUE 'UPDATE TARJETAS SET ESTADO = '
      -    ''I'' WHERE NRO_TARJETA = ?'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 52.
           05 SQL-STMT   PIC X(52) VALUE 'UPDATE clientes SET TARJETA = 
      -    '0 WHERE ID_CLIENTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(11)V9(2) COMP-3.
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
       01  REG-TARJ.
           05 TARJ-ID-CLIENTE          PIC 9(08).
           05 TARJ-NRO-TARJETA         PIC X(16).
           05 TARJ-FECHA-EMISION       PIC X(10).
           05 TARJ-FECHA-VENCIM        PIC X(10).
           05 TARJ-LIMITE-TARJETA      PIC S9(10)V99.
           05 TARJ-ACUM-MES            PIC S9(10)V99.
           05 TARJ-LIQUIDACION-MES     PIC S9(10)V99.
           05 TARJ-ESTADO              PIC X(01).
           05 TARJ-IMPORTE-MOV         PIC S9(10)V99.

           COPY LKCIF.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION USING REG-TARJ, LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 0 TO LK-COD-RETORNO
           MOVE SPACES TO LK-MENSAJE

           EVALUATE LK-ACCION-DB
               WHEN 'A'
                   PERFORM 1000-INSERTAR-TARJETA
               WHEN 'C'
                   PERFORM 2000-CONSULTAR-TARJETA
               WHEN 'M'
                   PERFORM 3000-ACTUALIZAR-TARJETA
               WHEN 'B'
                   PERFORM 4000-CANCELAR-TARJETA
               WHEN OTHER
                   MOVE 98 TO LK-COD-RETORNO
                   MOVE "ACCION DB NO SOPORTADA" TO LK-MENSAJE
           END-EVALUATE

           EXIT PROGRAM.

       1000-INSERTAR-TARJETA.
      *    EXEC SQL
      *        INSERT INTO TARJETAS (
      *            ID_CLIENTE,
      *            NRO_TARJETA,
      *            FECHA_EMISION,
      *            FECHA_VENCIMIENTO,
      *            LIMITE_TARJETA,
      *            ACUM_MES,
      *            LIQUIDACION_MES,
      *            ESTADO
      *        ) VALUES (
      *            :TARJ-ID-CLIENTE,
      *            :TARJ-NRO-TARJETA,
      *            :TARJ-FECHA-EMISION,
      *            :TARJ-FECHA-VENCIM,
      *            :TARJ-LIMITE-TARJETA,
      *            :TARJ-ACUM-MES,
      *            :TARJ-LIQUIDACION-MES,
      *            :TARJ-ESTADO
      *        )
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 16 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 TARJ-FECHA-EMISION
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 TARJ-FECHA-VENCIM
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(6)
               MOVE 7 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(7)
               MOVE 7 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 TARJ-ESTADO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 1 TO SQL-LEN(8)
               MOVE 8 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE TARJ-ID-CLIENTE
             TO SQL-VAR-0001
           MOVE TARJ-LIMITE-TARJETA
             TO SQL-VAR-0002
           MOVE TARJ-ACUM-MES
             TO SQL-VAR-0003
           MOVE TARJ-LIQUIDACION-MES
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA

           PERFORM 9000-EVALUAR-SQL

           IF LK-COD-RETORNO = 0
      *        EXEC SQL
      *            UPDATE clientes
      *            SET TARJETA = 1
      *            WHERE ID_CLIENTE = :TARJ-ID-CLIENTE
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
           MOVE TARJ-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA

               PERFORM 9000-EVALUAR-SQL

               IF LK-COD-RETORNO = 0
                   MOVE "TARJETA REGISTRADA" TO LK-MENSAJE
               END-IF
           END-IF.

       2000-CONSULTAR-TARJETA.
      *    EXEC SQL
      *        SELECT ID_CLIENTE,
      *               FECHA_EMISION,
      *               FECHA_VENCIMIENTO,
      *               LIMITE_TARJETA,
      *               ACUM_MES,
      *               LIQUIDACION_MES,
      *               ESTADO
      *        INTO :TARJ-ID-CLIENTE,
      *             :TARJ-FECHA-EMISION,
      *             :TARJ-FECHA-VENCIM,
      *             :TARJ-LIMITE-TARJETA,
      *             :TARJ-ACUM-MES,
      *             :TARJ-LIQUIDACION-MES,
      *             :TARJ-ESTADO
      *        FROM TARJETAS
      *        WHERE NRO_TARJETA = :TARJ-NRO-TARJETA
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 TARJ-FECHA-EMISION
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 TARJ-FECHA-VENCIM
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(4)
               MOVE 7 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(6)
               MOVE 7 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 TARJ-ESTADO
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 1 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 16 TO SQL-LEN(8)
               MOVE 8 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0001 TO TARJ-ID-CLIENTE
           MOVE SQL-VAR-0002 TO TARJ-LIMITE-TARJETA
           MOVE SQL-VAR-0003 TO TARJ-ACUM-MES
           MOVE SQL-VAR-0004 TO TARJ-LIQUIDACION-MES

           PERFORM 9000-EVALUAR-SQL

           IF LK-COD-RETORNO = 0
               MOVE "CONSULTA EXITOSA" TO LK-MENSAJE
           END-IF.

       3000-ACTUALIZAR-TARJETA.
      *    EXEC SQL
      *        UPDATE TARJETAS
      *        SET ACUM_MES = :TARJ-ACUM-MES,
      *            LIQUIDACION_MES = :TARJ-LIQUIDACION-MES,
      *            ESTADO = :TARJ-ESTADO
      *        WHERE NRO_TARJETA = :TARJ-NRO-TARJETA
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 7 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(2)
               MOVE 7 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 TARJ-ESTADO
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 1 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 16 TO SQL-LEN(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE TARJ-ACUM-MES
             TO SQL-VAR-0003
           MOVE TARJ-LIQUIDACION-MES
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA

           PERFORM 9000-EVALUAR-SQL

           IF LK-COD-RETORNO = 0
               MOVE "TARJETA ACTUALIZADA" TO LK-MENSAJE
           END-IF.

       4000-CANCELAR-TARJETA.
      *    EXEC SQL
      *        UPDATE TARJETAS
      *        SET ESTADO = 'I'
      *        WHERE NRO_TARJETA = :TARJ-NRO-TARJETA
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 16 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA

           PERFORM 9000-EVALUAR-SQL

           IF LK-COD-RETORNO = 0
      *        EXEC SQL
      *            UPDATE clientes
      *            SET TARJETA = 0
      *            WHERE ID_CLIENTE = :TARJ-ID-CLIENTE
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE TARJ-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA

               PERFORM 9000-EVALUAR-SQL

               IF LK-COD-RETORNO = 0
                   MOVE "TARJETA CANCELADA" TO LK-MENSAJE
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
                   MOVE "TARJETA NO ENCONTRADA" TO LK-MENSAJE
               WHEN -803
                   MOVE 02 TO LK-COD-RETORNO
                   MOVE "NRO DE TARJETA DUPLICADO" TO LK-MENSAJE
               WHEN OTHER
                   MOVE 99 TO LK-COD-RETORNO
                   MOVE "ERROR TECNICO EN DB TARJETAS" TO LK-MENSAJE
           END-EVALUATE.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  LKCIF                NOT IN USE
      *  REG-TARJ             NOT IN USE
      *  REG-TARJ.LKCIF       NOT IN USE
      *  REG-TARJ.TARJ-ACUM-MES NOT IN USE
      *  REG-TARJ.TARJ-ESTADO NOT IN USE
      *  REG-TARJ.TARJ-FECHA-EMISION NOT IN USE
      *  REG-TARJ.TARJ-FECHA-VENCIM NOT IN USE
      *  REG-TARJ.TARJ-ID-CLIENTE NOT IN USE
      *  REG-TARJ.TARJ-IMPORTE-MOV NOT IN USE
      *  REG-TARJ.TARJ-LIMITE-TARJETA NOT IN USE
      *  REG-TARJ.TARJ-LIQUIDACION-MES NOT IN USE
      *  REG-TARJ.TARJ-NRO-TARJETA NOT IN USE
      *  TARJ-ACUM-MES            IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(13,2)
      *  TARJ-ESTADO              IN USE CHAR(1)
      *  TARJ-FECHA-EMISION       IN USE CHAR(10)
      *  TARJ-FECHA-VENCIM        IN USE CHAR(10)
      *  TARJ-ID-CLIENTE          IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  TARJ-IMPORTE-MOV     NOT IN USE
      *  TARJ-LIMITE-TARJETA      IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(13,2)
      *  TARJ-LIQUIDACION-MES     IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(13,2)
      *  TARJ-NRO-TARJETA         IN USE CHAR(16)
      **********************************************************************

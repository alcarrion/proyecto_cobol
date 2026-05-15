      *================================================================*
      * PROGRAMA: DBIOINVM.cob                                         *
      * FUNCION:  CRUD de la tabla ctactes - BANCO LAF v2.0            *
      * NIVEL:    1000 TCS ECUADOR                                      *
      * ARQUITECTURA: Capa de Acceso a Datos (DBIO)                    *
      * ACCIONES:                                                       *
      *   A = INSERT ctactes (nueva cuenta)                            *
      *   C = SELECT por ID_CUENTA                                     *
      *   L = SELECT FOR UPDATE por ID_CUENTA                          *
      *   X = SELECT por ID_CLIENTE (lista todas las cuentas)          *
      *   M = UPDATE SALDO_ACTUAL WHERE ID_CUENTA                      *
      *   B = UPDATE ESTADO_CUENTA='C' (cierre)                        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOINVM.

       ENVIRONMENT DIVISION.
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
      * STMT-0: INSERT ctactes
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 5.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 111.
           05 SQL-STMT   PIC X(111) VALUE 'INSERT INTO ctactes (ID_CLIEN
      -    'TE,TIPO_CUENTA,SALDO_ACTUAL,FECHA_APERTURA,ESTADO_CUENTA) VA
      -    'LUES (?,?,?,?,?)'.
      **********************************************************************
      * STMT-1: SELECT por ID_CUENTA (consulta)
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 115.
           05 SQL-STMT   PIC X(115) VALUE 'SELECT ID_CLIENTE,TIPO_CUENTA
      -    ',SALDO_ACTUAL,FECHA_APERTURA,ESTADO_CUENTA FROM ctactes WHER
      -    'E ID_CUENTA = ?'.
      **********************************************************************
      * STMT-2: SELECT FOR UPDATE por ID_CUENTA (bloqueo)
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 126.
           05 SQL-STMT   PIC X(126) VALUE 'SELECT ID_CLIENTE,TIPO_CUENTA
      -    ',SALDO_ACTUAL,FECHA_APERTURA,ESTADO_CUENTA FROM ctactes WHER
      -    'E ID_CUENTA = ? FOR UPDATE'.
      **********************************************************************
      * STMT-3: UPDATE SALDO_ACTUAL WHERE ID_CUENTA
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 58.
           05 SQL-STMT   PIC X(58) VALUE 'UPDATE ctactes SET SALDO_ACTUA
      -    'L = ? WHERE ID_CUENTA = ?'.
      **********************************************************************
      * STMT-4: UPDATE ESTADO_CUENTA='C' (cierre)
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 62.
           05 SQL-STMT   PIC X(62) VALUE 'UPDATE ctactes SET ESTADO_CUEN
      -    'TA = ''C'' WHERE ID_CUENTA = ?'.
      **********************************************************************
      * STMT-5: SELECT por ID_CLIENTE (lista cuentas - usa cursor)
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 120.
           05 SQL-STMT   PIC X(120) VALUE 'SELECT ID_CUENTA,TIPO_CUENTA,
      -    'SALDO_ACTUAL,FECHA_APERTURA,ESTADO_CUENTA FROM ctactes WHERE
      -    ' ID_CLIENTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(9) COMP-3.
           05 SQL-VAR-0003  PIC S9(9)V9(2) COMP-3.
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

       01  WS-ULTIMO-ID    PIC 9(09).

       LINKAGE SECTION.
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  REG-INVM.
           05 INVM-ID-CUENTA           PIC 9(09).
           05 INVM-ID-CLIENTE          PIC 9(09).
           05 INVM-TIPO-CUENTA         PIC X(01).
           05 INVM-SALDO-ACTUAL        PIC S9(15)V99.
           05 INVM-FECHA-APERTURA      PIC X(10).
           05 INVM-ESTADO-CUENTA       PIC X(01).

           COPY LKCIF.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION USING REG-INVM, LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE '00  ' TO LK-COD-RETORNO
           MOVE SPACES TO LK-MENSAJE

           EVALUATE LK-ACCION-DB
               WHEN 'A'
                   PERFORM 1000-INSERTAR-CUENTA
               WHEN 'C'
                   PERFORM 2000-CONSULTAR-CUENTA
               WHEN 'L'
                   PERFORM 2100-CONSULTAR-BLOQUEO
               WHEN 'X'
                   PERFORM 2200-LISTAR-CUENTAS-CLIENTE
               WHEN 'M'
                   PERFORM 3000-ACTUALIZAR-SALDO
               WHEN 'B'
                   PERFORM 4000-CERRAR-CUENTA
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE "ACCION DB NO SOPORTADA" TO LK-MENSAJE
           END-EVALUATE.

           EXIT PROGRAM.

       1000-INSERTAR-CUENTA.
      *    EXEC SQL
      *        INSERT INTO ctactes (
      *            ID_CLIENTE, TIPO_CUENTA, SALDO_ACTUAL,
      *            FECHA_APERTURA, ESTADO_CUENTA
      *        ) VALUES (?,?,?,?,?)
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 INVM-TIPO-CUENTA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(3)
               MOVE 9 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 INVM-FECHA-APERTURA
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 INVM-ESTADO-CUENTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 1 TO SQL-LEN(5)
               MOVE 5 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE INVM-ID-CLIENTE
             TO SQL-VAR-0001
           MOVE INVM-SALDO-ACTUAL
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = '00  '
      *        Recuperar ID_CUENTA generado
               MOVE 'SELECT LAST_INSERT_ID()' TO WS-ULTIMO-ID
               SET SQL-ADDR(1) TO ADDRESS OF SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLEXE' USING SQL-STMT-0
                                   SQLCA
               MOVE SQL-VAR-0002 TO INVM-ID-CUENTA
               MOVE "CUENTA CREADA CON EXITO" TO LK-MENSAJE
           END-IF.

       2000-CONSULTAR-CUENTA.
      *    EXEC SQL
      *        SELECT ID_CLIENTE, TIPO_CUENTA, SALDO_ACTUAL,
      *               FECHA_APERTURA, ESTADO_CUENTA
      *        FROM ctactes WHERE ID_CUENTA = :INVM-ID-CUENTA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 INVM-TIPO-CUENTA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(3)
               MOVE 9 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 INVM-FECHA-APERTURA
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 INVM-ESTADO-CUENTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 1 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(6)
               MOVE 5 TO SQL-LEN(6)
               MOVE X'00' TO SQL-PREC(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE INVM-ID-CUENTA TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0001 TO INVM-ID-CLIENTE
           MOVE SQL-VAR-0003 TO INVM-SALDO-ACTUAL
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = '00  '
               MOVE "CONSULTA EXITOSA" TO LK-MENSAJE
           END-IF.

       2100-CONSULTAR-BLOQUEO.
      *    EXEC SQL
      *        SELECT ID_CLIENTE, TIPO_CUENTA, SALDO_ACTUAL,
      *               FECHA_APERTURA, ESTADO_CUENTA
      *        FROM ctactes WHERE ID_CUENTA = :INVM-ID-CUENTA
      *        FOR UPDATE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 INVM-TIPO-CUENTA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(3)
               MOVE 9 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 INVM-FECHA-APERTURA
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 INVM-ESTADO-CUENTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 1 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(6)
               MOVE 5 TO SQL-LEN(6)
               MOVE X'00' TO SQL-PREC(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE INVM-ID-CUENTA TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0001 TO INVM-ID-CLIENTE
           MOVE SQL-VAR-0003 TO INVM-SALDO-ACTUAL
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = '00  '
               MOVE "CUENTA BLOQUEADA PARA UPDATE" TO LK-MENSAJE
           END-IF.

       2200-LISTAR-CUENTAS-CLIENTE.
      *    Abre cursor y hace un primer fetch.
      *    El mainline itera llamando repetidamente hasta NODATA.
      *    EXEC SQL OPEN CUR_CTAS END-EXEC / FETCH / CLOSE
      *    Simulado con OCSQLOCU / OCSQLFTC / OCSQLCCU
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 INVM-TIPO-CUENTA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(3)
               MOVE 9 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 INVM-FECHA-APERTURA
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 INVM-ESTADO-CUENTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 1 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(6)
               MOVE 5 TO SQL-LEN(6)
               MOVE X'00' TO SQL-PREC(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE INVM-ID-CLIENTE TO SQL-VAR-0001
           CALL 'OCSQLOCU' USING SQL-STMT-5
                               SQLCA
           CALL 'OCSQLFTC' USING SQL-STMT-5
                               SQLCA
           IF SQL-SUCCESS
               MOVE SQL-VAR-0002 TO INVM-ID-CUENTA
               MOVE SQL-VAR-0003 TO INVM-SALDO-ACTUAL
               MOVE '00  ' TO LK-COD-RETORNO
               MOVE "PRIMERA CUENTA ENCONTRADA" TO LK-MENSAJE
           ELSE
               CALL 'OCSQLCCU' USING SQL-STMT-5
                                   SQLCA
               PERFORM 9000-EVALUAR-SQL
           END-IF.

       3000-ACTUALIZAR-SALDO.
      *    EXEC SQL
      *        UPDATE ctactes
      *        SET SALDO_ACTUAL = :INVM-SALDO-ACTUAL
      *        WHERE ID_CUENTA = :INVM-ID-CUENTA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 9 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE INVM-SALDO-ACTUAL
             TO SQL-VAR-0003
           MOVE INVM-ID-CUENTA
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = '00  '
               MOVE "SALDO ACTUALIZADO" TO LK-MENSAJE
           END-IF.

       4000-CERRAR-CUENTA.
      *    EXEC SQL
      *        UPDATE ctactes SET ESTADO_CUENTA = 'C'
      *        WHERE ID_CUENTA = :INVM-ID-CUENTA
      *    END-EXEC.
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
           MOVE INVM-ID-CUENTA
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = '00  '
               MOVE "CUENTA CERRADA" TO LK-MENSAJE
           END-IF.

       9000-EVALUAR-SQL.
           EVALUATE TRUE
               WHEN SQL-SUCCESS
                   MOVE '00  ' TO LK-COD-RETORNO
                   IF LK-MENSAJE = SPACES
                       MOVE "OPERACION EXITOSA" TO LK-MENSAJE
                   END-IF
               WHEN SQL-NODATA
                   MOVE 'E001' TO LK-COD-RETORNO
                   MOVE "CUENTA NO ENCONTRADA" TO LK-MENSAJE
               WHEN SQL-DUPLICATE
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE "CUENTA DUPLICADA EN BASE DE DATOS"
                       TO LK-MENSAJE
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE "ERROR CRITICO EN BASE DE DATOS CUENTAS"
                       TO LK-MENSAJE
           END-EVALUATE.
      **********************************************************************
      *  BANCO LAF v2.0 - NIVEL 1000 TCS ECUADOR
      *  DBIOINVM: Capa DBIO ctactes multi-cuenta - schema v2.0
      *  Acciones: A C L X M B
      **********************************************************************

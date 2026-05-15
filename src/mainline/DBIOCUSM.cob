      *================================================================*
      * PROGRAMA: DBIOCUSM.cob                                         *
      * FUNCION:  CRUD de la tabla clientes - BANCO LAF v2.0           *
      * NIVEL:    1000 TCS ECUADOR                                      *
      * ARQUITECTURA: Capa de Acceso a Datos (DBIO)                    *
      * ACCIONES:                                                       *
      *   A = INSERT clientes (nuevo schema scoring)                   *
      *   C = SELECT por DOC_CLIENTE                                   *
      *   M = UPDATE DIRECCION, TELEF, EMAIL                           *
      *   B = UPDATE ESTADO_CLIENTE='I' (baja logica)                  *
      *   K = UPDATE SCORE_CREDITICIO, INGRESOS_MENSUALES              *
      *   T = UPDATE SALDO_TOTAL_VISTA, TIENE_TARJETA, TIENE_HIPOTECA  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOCUSM.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 15.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 15 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 15 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 15 TIMES.
           05 SQL-PREC   PIC X OCCURS 15 TIMES.
      **********************************************************************
      * STMT-0: INSERT clientes (14 parametros + FECHA_NACIMIENTO,SCORE,
      *         INGRESOS,ESTADO_CLIENTE)
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 14.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 305.
           05 SQL-STMT   PIC X(305) VALUE 'INSERT INTO clientes (TIPO_DO
      -    'C,DOC_CLIENTE,FECHA_ALTA,NOMBRE_CLIENTE,APELLIDOS_CLIENTE,FE
      -    'CHA_NACIMIENTO,DIRECCION_CLIENTE,TELEF_CLIENTE,EMAIL_CLIENTE,
      -    'SCORE_CREDITICIO,INGRESOS_MENSUALES,TIENE_TARJETA,TIENE_HIPO
      -    'TECA,ESTADO_CLIENTE) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
      * STMT-1: SELECT LAST_INSERT_ID() INTO :CUSM-ID-CLIENTE
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 23.
           05 SQL-STMT   PIC X(23) VALUE 'SELECT LAST_INSERT_ID()'.
      **********************************************************************
      * STMT-2: SELECT por DOC_CLIENTE
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 340.
           05 SQL-STMT   PIC X(340) VALUE 'SELECT ID_CLIENTE,TIPO_DOC,FE
      -    'CHA_ALTA,NOMBRE_CLIENTE,APELLIDOS_CLIENTE,FECHA_NACIMIENTO,D
      -    'IRECCION_CLIENTE,TELEF_CLIENTE,EMAIL_CLIENTE,SCORE_CREDITICIO
      -    ',INGRESOS_MENSUALES,TIENE_TARJETA,TIENE_HIPOTECA,ESTADO_CLIE
      -    'NTE,SALDO_TOTAL_VISTA FROM clientes WHERE TRIM(DOC_CLIENTE) =
      -    ' TRIM(?)'.
      **********************************************************************
      * STMT-3: UPDATE direccion/telefono/email
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 111.
           05 SQL-STMT   PIC X(111) VALUE 'UPDATE clientes SET DIRECCION
      -    '_CLIENTE = ?,TELEF_CLIENTE = ?,EMAIL_CLIENTE = ? WHERE TRIM(
      -    'DOC_CLIENTE) = TRIM(?)'.
      **********************************************************************
      * STMT-4: UPDATE ESTADO_CLIENTE='I' (baja logica)
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 79.
           05 SQL-STMT   PIC X(79) VALUE 'UPDATE clientes SET ESTADO_CLI
      -    'ENTE = ''I'' WHERE TRIM(DOC_CLIENTE) = TRIM(?)'.
      **********************************************************************
      * STMT-5: UPDATE SCORE_CREDITICIO, INGRESOS_MENSUALES (accion K)
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 95.
           05 SQL-STMT   PIC X(95) VALUE 'UPDATE clientes SET SCORE_CRED
      -    'ITICIO = ?,INGRESOS_MENSUALES = ? WHERE ID_CLIENTE = ?'.
      **********************************************************************
      * STMT-6: UPDATE SALDO_TOTAL_VISTA, TIENE_TARJETA, TIENE_HIPOTECA
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 110.
           05 SQL-STMT   PIC X(110) VALUE 'UPDATE clientes SET SALDO_TOT
      -    'AL_VISTA = ?,TIENE_TARJETA = ?,TIENE_HIPOTECA = ? WHERE ID_C
      -    'LIENTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(3) COMP-3.
           05 SQL-VAR-0003  PIC S9(9)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(1) COMP-3.
           05 SQL-VAR-0005  PIC S9(1) COMP-3.
           05 SQL-VAR-0006  PIC S9(9)V9(2) COMP-3.
           05 SQL-VAR-0007  PIC S9(9)V9(2) COMP-3.
           05 SQL-VAR-0008  PIC S9(3) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************
      *    Area de comunicacion de SQL
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
       01  REG-CUSM.
           05 CUSM-ID-CLIENTE          PIC 9(09).
           05 CUSM-TIPO-DOC            PIC X(03).
           05 CUSM-DOC-CLIENTE         PIC X(12).
           05 CUSM-FECHA-ALTA          PIC X(10).
           05 CUSM-NOMBRE              PIC X(25).
           05 CUSM-APELLIDOS           PIC X(25).
           05 CUSM-FECHA-NACIMIENTO    PIC X(10).
           05 CUSM-DIRECCION           PIC X(45).
           05 CUSM-TELEFONO            PIC X(12).
           05 CUSM-EMAIL               PIC X(40).
           05 CUSM-SCORE-CREDITICIO    PIC 9(03).
           05 CUSM-INGRESOS            PIC S9(12)V99.
           05 CUSM-TIENE-TARJETA       PIC 9(01).
           05 CUSM-TIENE-HIPOTECA      PIC 9(01).
           05 CUSM-ESTADO-CLIENTE      PIC X(01).
           05 CUSM-SALDO-TOTAL-VISTA   PIC S9(12)V99.
           COPY LKCIF.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION USING REG-CUSM, LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE '00  ' TO LK-COD-RETORNO
           MOVE SPACES TO LK-MENSAJE

           EVALUATE LK-ACCION-DB
               WHEN 'A'
                   PERFORM 1000-INSERTAR-CLIENTE
               WHEN 'C'
                   PERFORM 2000-CONSULTAR-CLIENTE
               WHEN 'M'
                   PERFORM 3000-ACTUALIZAR-CLIENTE
               WHEN 'B'
                   PERFORM 4000-BAJA-LOGICA-CLIENTE
               WHEN 'K'
                   PERFORM 5000-ACTUALIZAR-SCORING
               WHEN 'T'
                   PERFORM 6000-SYNC-FLAGS-SALDO
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE "ACCION DB NO SOPORTADA" TO LK-MENSAJE
           END-EVALUATE.

           EXIT PROGRAM.

       1000-INSERTAR-CLIENTE.
      *    EXEC SQL
      *        INSERT INTO clientes (
      *            TIPO_DOC, DOC_CLIENTE, FECHA_ALTA,
      *            NOMBRE_CLIENTE, APELLIDOS_CLIENTE,
      *            FECHA_NACIMIENTO,
      *            DIRECCION_CLIENTE, TELEF_CLIENTE, EMAIL_CLIENTE,
      *            SCORE_CREDITICIO, INGRESOS_MENSUALES,
      *            TIENE_TARJETA, TIENE_HIPOTECA, ESTADO_CLIENTE
      *        ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 CUSM-TIPO-DOC
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 CUSM-DOC-CLIENTE
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 12 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 CUSM-FECHA-ALTA
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 CUSM-NOMBRE
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 25 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 CUSM-APELLIDOS
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 25 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 CUSM-FECHA-NACIMIENTO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 10 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 CUSM-DIRECCION
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 45 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 CUSM-TELEFONO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 12 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 CUSM-EMAIL
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 40 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(10)
               MOVE 2 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(11)
               MOVE 7 TO SQL-LEN(11)
               MOVE X'02' TO SQL-PREC(11)
               SET SQL-ADDR(12) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(12)
               MOVE 1 TO SQL-LEN(12)
               MOVE X'00' TO SQL-PREC(12)
               SET SQL-ADDR(13) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(13)
               MOVE 1 TO SQL-LEN(13)
               MOVE X'00' TO SQL-PREC(13)
               SET SQL-ADDR(14) TO ADDRESS OF
                 CUSM-ESTADO-CLIENTE
               MOVE 'X' TO SQL-TYPE(14)
               MOVE 1 TO SQL-LEN(14)
               MOVE 14 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE CUSM-SCORE-CREDITICIO
             TO SQL-VAR-0002
           MOVE CUSM-INGRESOS
             TO SQL-VAR-0003
           MOVE CUSM-TIENE-TARJETA
             TO SQL-VAR-0004
           MOVE CUSM-TIENE-HIPOTECA
             TO SQL-VAR-0005
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .

           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = '00  '
      *        SELECT LAST_INSERT_ID() INTO :CUSM-ID-CLIENTE
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
           MOVE SQL-VAR-0001 TO CUSM-ID-CLIENTE
               MOVE "CLIENTE REGISTRADO CON EXITO" TO LK-MENSAJE
           END-IF.

       2000-CONSULTAR-CLIENTE.
      *    EXEC SQL
      *        SELECT ID_CLIENTE, TIPO_DOC, FECHA_ALTA,
      *               NOMBRE_CLIENTE, APELLIDOS_CLIENTE,
      *               FECHA_NACIMIENTO,
      *               DIRECCION_CLIENTE, TELEF_CLIENTE, EMAIL_CLIENTE,
      *               SCORE_CREDITICIO, INGRESOS_MENSUALES,
      *               TIENE_TARJETA, TIENE_HIPOTECA,
      *               ESTADO_CLIENTE, SALDO_TOTAL_VISTA
      *        FROM clientes
      *        WHERE TRIM(DOC_CLIENTE) = TRIM(:CUSM-DOC-CLIENTE)
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 CUSM-TIPO-DOC
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 3 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 CUSM-FECHA-ALTA
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 CUSM-NOMBRE
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 25 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 CUSM-APELLIDOS
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 25 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 CUSM-FECHA-NACIMIENTO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 10 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 CUSM-DIRECCION
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 45 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 CUSM-TELEFONO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 12 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 CUSM-EMAIL
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 40 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(10)
               MOVE 2 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(11)
               MOVE 7 TO SQL-LEN(11)
               MOVE X'02' TO SQL-PREC(11)
               SET SQL-ADDR(12) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(12)
               MOVE 1 TO SQL-LEN(12)
               MOVE X'00' TO SQL-PREC(12)
               SET SQL-ADDR(13) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(13)
               MOVE 1 TO SQL-LEN(13)
               MOVE X'00' TO SQL-PREC(13)
               SET SQL-ADDR(14) TO ADDRESS OF
                 CUSM-ESTADO-CLIENTE
               MOVE 'X' TO SQL-TYPE(14)
               MOVE 1 TO SQL-LEN(14)
               SET SQL-ADDR(15) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(15)
               MOVE 7 TO SQL-LEN(15)
               MOVE X'02' TO SQL-PREC(15)
               MOVE 15 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
               SET SQL-ADDR(15) TO ADDRESS OF
                 CUSM-DOC-CLIENTE
               MOVE 'X' TO SQL-TYPE(15)
               MOVE 12 TO SQL-LEN(15)
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0001 TO CUSM-ID-CLIENTE
           MOVE SQL-VAR-0002 TO CUSM-SCORE-CREDITICIO
           MOVE SQL-VAR-0003 TO CUSM-INGRESOS
           MOVE SQL-VAR-0004 TO CUSM-TIENE-TARJETA
           MOVE SQL-VAR-0005 TO CUSM-TIENE-HIPOTECA
           MOVE SQL-VAR-0006 TO CUSM-SALDO-TOTAL-VISTA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = '00  '
               MOVE "CONSULTA EXITOSA" TO LK-MENSAJE
           END-IF.

       3000-ACTUALIZAR-CLIENTE.
      *    EXEC SQL
      *        UPDATE clientes
      *        SET DIRECCION_CLIENTE = :CUSM-DIRECCION,
      *            TELEF_CLIENTE      = :CUSM-TELEFONO,
      *            EMAIL_CLIENTE      = :CUSM-EMAIL
      *        WHERE TRIM(DOC_CLIENTE) = TRIM(:CUSM-DOC-CLIENTE)
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 CUSM-DIRECCION
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 45 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 CUSM-TELEFONO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 12 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 CUSM-EMAIL
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 40 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 CUSM-DOC-CLIENTE
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 12 TO SQL-LEN(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = '00  '
               MOVE "CLIENTE ACTUALIZADO" TO LK-MENSAJE
           END-IF.

       4000-BAJA-LOGICA-CLIENTE.
      *    EXEC SQL
      *        UPDATE clientes
      *        SET ESTADO_CLIENTE = 'I'
      *        WHERE TRIM(DOC_CLIENTE) = TRIM(:CUSM-DOC-CLIENTE)
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 CUSM-DOC-CLIENTE
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 12 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = '00  '
               MOVE "BAJA LOGICA REALIZADA (ESTADO_CLIENTE=I)"
                   TO LK-MENSAJE
           END-IF.

       5000-ACTUALIZAR-SCORING.
      *    EXEC SQL
      *        UPDATE clientes
      *        SET SCORE_CREDITICIO   = :CUSM-SCORE-CREDITICIO,
      *            INGRESOS_MENSUALES = :CUSM-INGRESOS
      *        WHERE ID_CLIENTE = :CUSM-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(2)
               MOVE 7 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE CUSM-SCORE-CREDITICIO
             TO SQL-VAR-0002
           MOVE CUSM-INGRESOS
             TO SQL-VAR-0003
           MOVE CUSM-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = '00  '
               MOVE "SCORING ACTUALIZADO" TO LK-MENSAJE
           END-IF.

       6000-SYNC-FLAGS-SALDO.
      *    EXEC SQL
      *        UPDATE clientes
      *        SET SALDO_TOTAL_VISTA = :CUSM-SALDO-TOTAL-VISTA,
      *            TIENE_TARJETA     = :CUSM-TIENE-TARJETA,
      *            TIENE_HIPOTECA    = :CUSM-TIENE-HIPOTECA
      *        WHERE ID_CLIENTE = :CUSM-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(1)
               MOVE 7 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(3)
               MOVE 1 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(4)
               MOVE 5 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE CUSM-SALDO-TOTAL-VISTA
             TO SQL-VAR-0006
           MOVE CUSM-TIENE-TARJETA
             TO SQL-VAR-0004
           MOVE CUSM-TIENE-HIPOTECA
             TO SQL-VAR-0005
           MOVE CUSM-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = '00  '
               MOVE "FLAGS Y SALDO SINCRONIZADOS" TO LK-MENSAJE
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
                   MOVE "CLIENTE NO ENCONTRADO" TO LK-MENSAJE
               WHEN SQL-DUPLICATE
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE "REGISTRO DUPLICADO EN BASE DE DATOS"
                       TO LK-MENSAJE
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE "ERROR TECNICO EN DB CLIENTES" TO LK-MENSAJE
           END-EVALUATE.
      **********************************************************************
      *  BANCO LAF v2.0 - NIVEL 1000 TCS ECUADOR
      *  DBIOCUSM: Capa DBIO clientes - schema v2.0
      *  Acciones: A C M B K T
      **********************************************************************

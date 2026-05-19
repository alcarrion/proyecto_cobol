      ******************************************************************
      * DBIOCUSM.SQB - PERSISTENCIA DE CLIENTES
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOCUSM.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 16.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 16 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 16 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 16 TIMES.
           05 SQL-PREC   PIC X OCCURS 16 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 289.
           05 SQL-STMT   PIC X(289) VALUE 'SELECT ID_CLIENTE,FECHA_ALTA,
      -    'NOMBRE_CLIENTE,APELLIDOS_CLIENTE,FECHA_NACIMIENTO,DIRECCION_
      -    'CLIENTE,TELEF_CLIENTE,EMAIL_CLIENTE,SCORE_CREDITICIO,INGRESO
      -    'S_MENSUALES,TIENE_TARJETA,TIENE_HIPOTECA,ESTADO_CLIENTE,SALD
      -    'O_TOTAL_VISTA FROM clientes WHERE TIPO_DOC = RTRIM(?) AND DO
      -    'C_CLIENTE = RTRIM(?)'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 15.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 287.
           05 SQL-STMT   PIC X(287) VALUE 'INSERT INTO clientes (TIPO_DO
      -    'C,DOC_CLIENTE,FECHA_ALTA,NOMBRE_CLIENTE,APELLIDOS_CLIENTE,FE
      -    'CHA_NACIMIENTO,DIRECCION_CLIENTE,TELEF_CLIENTE,EMAIL_CLIENTE
      -    ',SCORE_CREDITICIO,INGRESOS_MENSUALES,TIENE_TARJETA,TIENE_HIP
      -    'OTECA,ESTADO_CLIENTE,SALDO_TOTAL_VISTA) VALUES (?,?,?,?,?,?,
      -    '?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 23.
           05 SQL-STMT   PIC X(23) VALUE 'SELECT LAST_INSERT_ID()'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 98.
           05 SQL-STMT   PIC X(98) VALUE 'UPDATE clientes SET DIRECCION_
      -    'CLIENTE = ?,TELEF_CLIENTE = ?,EMAIL_CLIENTE = ? WHERE ID_CLI
      -    'ENTE = ?'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 61.
           05 SQL-STMT   PIC X(61) VALUE 'UPDATE clientes SET ESTADO_CLI
      -    'ENTE = ''I'' WHERE ID_CLIENTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(3) COMP-3.
           05 SQL-VAR-0003  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(1) COMP-3.
           05 SQL-VAR-0005  PIC S9(1) COMP-3.
           05 SQL-VAR-0006  PIC S9(11)V9(2) COMP-3.
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
       01  DB-ID-CLIENTE             PIC 9(08).
       01  DB-TIPO-DOC               PIC X(03).
       01  DB-DOC-CLIENTE            PIC X(12).
       01  DB-FECHA-ALTA             PIC X(10).
       01  DB-NOMBRE-CLIENTE         PIC X(25).
       01  DB-APELLIDOS-CLIENTE      PIC X(25).
       01  DB-FECHA-NACIMIENTO       PIC X(10).
       01  DB-DIRECCION-CLIENTE      PIC X(45).
       01  DB-TELEF-CLIENTE          PIC X(12).
       01  DB-EMAIL-CLIENTE          PIC X(40).
       01  DB-SCORE-CREDITICIO       PIC 9(03).
       01  DB-INGRESOS-MENSUALES     PIC S9(10)V99 COMP-3.
       01  DB-TIENE-TARJETA          PIC 9(01).
       01  DB-TIENE-HIPOTECA         PIC 9(01).
       01  DB-ESTADO-CLIENTE         PIC X(01).
       01  DB-SALDO-TOTAL-VISTA      PIC S9(10)V99 COMP-3.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.
           COPY CUSMREC.
           COPY LKCIF.

       PROCEDURE DIVISION USING REG-CUSM,
                                LK-DATOS-SESION,
                                LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE '00  ' TO LK-COD-RETORNO.
           MOVE SPACES TO LK-MENSAJE.

           EVALUATE LK-ACCION-DB
              WHEN 'C'
                 PERFORM 1000-CONSULTAR-CLIENTE
              WHEN 'A'
                 PERFORM 2000-INSERTAR-CLIENTE
              WHEN 'M'
                 PERFORM 3000-ACTUALIZAR-CLIENTE
              WHEN 'B'
                 PERFORM 4000-BAJA-CLIENTE
              WHEN OTHER
                 MOVE 'E999' TO LK-COD-RETORNO
                 MOVE "ACCION NO SOPORTADA" TO LK-MENSAJE
           END-EVALUATE.

           GOBACK.

       1000-CONSULTAR-CLIENTE.
           MOVE CUSM-TIPO-DOC TO DB-TIPO-DOC.
           MOVE CUSM-DOC-CLIENTE TO DB-DOC-CLIENTE.

      *    EXEC SQL
      *       SELECT ID_CLIENTE,
      *              FECHA_ALTA,
      *              NOMBRE_CLIENTE,
      *              APELLIDOS_CLIENTE,
      *              FECHA_NACIMIENTO,
      *              DIRECCION_CLIENTE,
      *              TELEF_CLIENTE,
      *              EMAIL_CLIENTE,
      *              SCORE_CREDITICIO,
      *              INGRESOS_MENSUALES,
      *              TIENE_TARJETA,
      *              TIENE_HIPOTECA,
      *              ESTADO_CLIENTE,
      *              SALDO_TOTAL_VISTA
      *         INTO :DB-ID-CLIENTE,
      *              :DB-FECHA-ALTA,
      *              :DB-NOMBRE-CLIENTE,
      *              :DB-APELLIDOS-CLIENTE,
      *              :DB-FECHA-NACIMIENTO,
      *              :DB-DIRECCION-CLIENTE,
      *              :DB-TELEF-CLIENTE,
      *              :DB-EMAIL-CLIENTE,
      *              :DB-SCORE-CREDITICIO,
      *              :DB-INGRESOS-MENSUALES,
      *              :DB-TIENE-TARJETA,
      *              :DB-TIENE-HIPOTECA,
      *              :DB-ESTADO-CLIENTE,
      *              :DB-SALDO-TOTAL-VISTA
      *         FROM clientes
      *        WHERE TIPO_DOC    = RTRIM(:DB-TIPO-DOC)
      *          AND DOC_CLIENTE = RTRIM(:DB-DOC-CLIENTE)
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-FECHA-ALTA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-NOMBRE-CLIENTE
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 25 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 DB-APELLIDOS-CLIENTE
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 25 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 DB-FECHA-NACIMIENTO
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 10 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DB-DIRECCION-CLIENTE
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 45 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 DB-TELEF-CLIENTE
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 12 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 DB-EMAIL-CLIENTE
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 40 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(9)
               MOVE 2 TO SQL-LEN(9)
               MOVE X'00' TO SQL-PREC(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(10)
               MOVE 7 TO SQL-LEN(10)
               MOVE X'02' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(11)
               MOVE 1 TO SQL-LEN(11)
               MOVE X'00' TO SQL-PREC(11)
               SET SQL-ADDR(12) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(12)
               MOVE 1 TO SQL-LEN(12)
               MOVE X'00' TO SQL-PREC(12)
               SET SQL-ADDR(13) TO ADDRESS OF
                 DB-ESTADO-CLIENTE
               MOVE 'X' TO SQL-TYPE(13)
               MOVE 1 TO SQL-LEN(13)
               SET SQL-ADDR(14) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(14)
               MOVE 7 TO SQL-LEN(14)
               MOVE X'02' TO SQL-PREC(14)
               SET SQL-ADDR(15) TO ADDRESS OF
                 DB-TIPO-DOC
               MOVE 'X' TO SQL-TYPE(15)
               MOVE 3 TO SQL-LEN(15)
               SET SQL-ADDR(16) TO ADDRESS OF
                 DB-DOC-CLIENTE
               MOVE 'X' TO SQL-TYPE(16)
               MOVE 12 TO SQL-LEN(16)
               MOVE 16 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-CLIENTE
           MOVE SQL-VAR-0002 TO DB-SCORE-CREDITICIO
           MOVE SQL-VAR-0003 TO DB-INGRESOS-MENSUALES
           MOVE SQL-VAR-0004 TO DB-TIENE-TARJETA
           MOVE SQL-VAR-0005 TO DB-TIENE-HIPOTECA
           MOVE SQL-VAR-0006 TO DB-SALDO-TOTAL-VISTA
                   .

           PERFORM 9000-EVALUAR-SQL.

           IF LK-EXITO
              MOVE DB-ID-CLIENTE TO CUSM-ID-CLIENTE
              MOVE DB-FECHA-ALTA TO CUSM-FECHA-ALTA
              MOVE DB-NOMBRE-CLIENTE TO CUSM-NOMBRE-CLIENTE
              MOVE DB-APELLIDOS-CLIENTE TO CUSM-APELLIDOS-CLIENTE
              MOVE DB-FECHA-NACIMIENTO TO CUSM-FECHA-NACIMIENTO
              MOVE DB-DIRECCION-CLIENTE TO CUSM-DIRECCION-CLIENTE
              MOVE DB-TELEF-CLIENTE TO CUSM-TELEF-CLIENTE
              MOVE DB-EMAIL-CLIENTE TO CUSM-EMAIL-CLIENTE
              MOVE DB-SCORE-CREDITICIO TO CUSM-SCORE-CREDITICIO
              MOVE DB-INGRESOS-MENSUALES
                 TO CUSM-INGRESOS-MENSUALES
              MOVE DB-TIENE-TARJETA TO CUSM-TIENE-TARJETA
              MOVE DB-TIENE-HIPOTECA TO CUSM-TIENE-HIPOTECA
              MOVE DB-ESTADO-CLIENTE TO CUSM-ESTADO-CLIENTE
              MOVE DB-SALDO-TOTAL-VISTA TO CUSM-SALDO-TOTAL-VISTA
              MOVE "CONSULTA DE CLIENTE EXITOSA" TO LK-MENSAJE
           END-IF.

       2000-INSERTAR-CLIENTE.
           MOVE CUSM-TIPO-DOC TO DB-TIPO-DOC.
           MOVE CUSM-DOC-CLIENTE TO DB-DOC-CLIENTE.
           MOVE CUSM-FECHA-ALTA TO DB-FECHA-ALTA.
           MOVE CUSM-NOMBRE-CLIENTE TO DB-NOMBRE-CLIENTE.
           MOVE CUSM-APELLIDOS-CLIENTE TO DB-APELLIDOS-CLIENTE.
           MOVE CUSM-FECHA-NACIMIENTO TO DB-FECHA-NACIMIENTO.
           MOVE CUSM-DIRECCION-CLIENTE TO DB-DIRECCION-CLIENTE.
           MOVE CUSM-TELEF-CLIENTE TO DB-TELEF-CLIENTE.
           MOVE CUSM-EMAIL-CLIENTE TO DB-EMAIL-CLIENTE.
           MOVE CUSM-SCORE-CREDITICIO TO DB-SCORE-CREDITICIO.
           MOVE CUSM-INGRESOS-MENSUALES TO DB-INGRESOS-MENSUALES.
           MOVE CUSM-TIENE-TARJETA TO DB-TIENE-TARJETA.
           MOVE CUSM-TIENE-HIPOTECA TO DB-TIENE-HIPOTECA.
           MOVE CUSM-ESTADO-CLIENTE TO DB-ESTADO-CLIENTE.
           MOVE CUSM-SALDO-TOTAL-VISTA TO DB-SALDO-TOTAL-VISTA.

      *    EXEC SQL
      *       INSERT INTO clientes (
      *          TIPO_DOC,
      *          DOC_CLIENTE,
      *          FECHA_ALTA,
      *          NOMBRE_CLIENTE,
      *          APELLIDOS_CLIENTE,
      *          FECHA_NACIMIENTO,
      *          DIRECCION_CLIENTE,
      *          TELEF_CLIENTE,
      *          EMAIL_CLIENTE,
      *          SCORE_CREDITICIO,
      *          INGRESOS_MENSUALES,
      *          TIENE_TARJETA,
      *          TIENE_HIPOTECA,
      *          ESTADO_CLIENTE,
      *          SALDO_TOTAL_VISTA
      *       ) VALUES (
      *          :DB-TIPO-DOC,
      *          :DB-DOC-CLIENTE,
      *          :DB-FECHA-ALTA,
      *          :DB-NOMBRE-CLIENTE,
      *          :DB-APELLIDOS-CLIENTE,
      *          :DB-FECHA-NACIMIENTO,
      *          :DB-DIRECCION-CLIENTE,
      *          :DB-TELEF-CLIENTE,
      *          :DB-EMAIL-CLIENTE,
      *          :DB-SCORE-CREDITICIO,
      *          :DB-INGRESOS-MENSUALES,
      *          :DB-TIENE-TARJETA,
      *          :DB-TIENE-HIPOTECA,
      *          :DB-ESTADO-CLIENTE,
      *          :DB-SALDO-TOTAL-VISTA
      *       )
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-TIPO-DOC
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 3 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-DOC-CLIENTE
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 12 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-FECHA-ALTA
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 DB-NOMBRE-CLIENTE
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 25 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 DB-APELLIDOS-CLIENTE
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 25 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DB-FECHA-NACIMIENTO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 10 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 DB-DIRECCION-CLIENTE
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 45 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 DB-TELEF-CLIENTE
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 12 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 DB-EMAIL-CLIENTE
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
                 DB-ESTADO-CLIENTE
               MOVE 'X' TO SQL-TYPE(14)
               MOVE 1 TO SQL-LEN(14)
               SET SQL-ADDR(15) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(15)
               MOVE 7 TO SQL-LEN(15)
               MOVE X'02' TO SQL-PREC(15)
               MOVE 15 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-SCORE-CREDITICIO
             TO SQL-VAR-0002
           MOVE DB-INGRESOS-MENSUALES
             TO SQL-VAR-0003
           MOVE DB-TIENE-TARJETA
             TO SQL-VAR-0004
           MOVE DB-TIENE-HIPOTECA
             TO SQL-VAR-0005
           MOVE DB-SALDO-TOTAL-VISTA
             TO SQL-VAR-0006
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
                   .

           PERFORM 9000-EVALUAR-SQL.

           IF LK-EXITO
      *       EXEC SQL
      *          SELECT LAST_INSERT_ID()
      *            INTO :DB-ID-CLIENTE
      *       END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-CLIENTE
              MOVE DB-ID-CLIENTE TO CUSM-ID-CLIENTE
              MOVE "CLIENTE CREADO" TO LK-MENSAJE
           END-IF.

       3000-ACTUALIZAR-CLIENTE.
           MOVE CUSM-ID-CLIENTE TO DB-ID-CLIENTE.
           MOVE CUSM-DIRECCION-CLIENTE TO DB-DIRECCION-CLIENTE.
           MOVE CUSM-TELEF-CLIENTE TO DB-TELEF-CLIENTE.
           MOVE CUSM-EMAIL-CLIENTE TO DB-EMAIL-CLIENTE.

      *    EXEC SQL
      *       UPDATE clientes
      *          SET DIRECCION_CLIENTE = :DB-DIRECCION-CLIENTE,
      *              TELEF_CLIENTE = :DB-TELEF-CLIENTE,
      *              EMAIL_CLIENTE = :DB-EMAIL-CLIENTE
      *        WHERE ID_CLIENTE = :DB-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-DIRECCION-CLIENTE
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 45 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-TELEF-CLIENTE
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 12 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-EMAIL-CLIENTE
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 40 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(4)
               MOVE 5 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
                   .

           PERFORM 9000-EVALUAR-SQL.

           IF LK-EXITO
              MOVE "CLIENTE ACTUALIZADO" TO LK-MENSAJE
           END-IF.

       4000-BAJA-CLIENTE.
           MOVE CUSM-ID-CLIENTE TO DB-ID-CLIENTE.

      *    EXEC SQL
      *       UPDATE clientes
      *          SET ESTADO_CLIENTE = 'I'
      *        WHERE ID_CLIENTE = :DB-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
                   .

           PERFORM 9000-EVALUAR-SQL.

           IF LK-EXITO
              MOVE "CLIENTE INACTIVADO" TO LK-MENSAJE
           END-IF.

       9000-EVALUAR-SQL.
           EVALUATE SQLSTATE
              WHEN '00000'
                 MOVE '00  ' TO LK-COD-RETORNO
              WHEN '02000'
                 MOVE 'E404' TO LK-COD-RETORNO
                 MOVE "CLIENTE NO ENCONTRADO" TO LK-MENSAJE
              WHEN '23000'
                 MOVE 'E409' TO LK-COD-RETORNO
                 MOVE "REGISTRO DUPLICADO" TO LK-MENSAJE
              WHEN OTHER
                 MOVE 'E999' TO LK-COD-RETORNO
                 MOVE "ERROR SQL" TO LK-MENSAJE
           END-EVALUATE.

       END PROGRAM DBIOCUSM.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-APELLIDOS-CLIENTE     IN USE CHAR(25)
      *  DB-DIRECCION-CLIENTE     IN USE CHAR(45)
      *  DB-DOC-CLIENTE           IN USE CHAR(12)
      *  DB-EMAIL-CLIENTE         IN USE CHAR(40)
      *  DB-ESTADO-CLIENTE        IN USE CHAR(1)
      *  DB-FECHA-ALTA            IN USE CHAR(10)
      *  DB-FECHA-NACIMIENTO      IN USE CHAR(10)
      *  DB-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  DB-INGRESOS-MENSUALES     IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(13,2)
      *  DB-NOMBRE-CLIENTE        IN USE CHAR(25)
      *  DB-SALDO-TOTAL-VISTA     IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(13,2)
      *  DB-SCORE-CREDITICIO      IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(3,0)
      *  DB-TELEF-CLIENTE         IN USE CHAR(12)
      *  DB-TIENE-HIPOTECA        IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(1,0)
      *  DB-TIENE-TARJETA         IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(1,0)
      *  DB-TIPO-DOC              IN USE CHAR(3)
      **********************************************************************

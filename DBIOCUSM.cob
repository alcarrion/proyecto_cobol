      *================================================================*
      * PROGRAMA: DBIOCUSM.sqb                                         *
      * FUNCION:  CRUD de la tabla clientes                            *
      * ARQUITECTURA: Capa de Acceso a Datos (DBIO)                    *
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
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 15.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 246.
           05 SQL-STMT   PIC X(246) VALUE 'INSERT INTO clientes (ID_CLIE
      -    'NTE,TIPO_DOC,DOC_CLIENTE,FECHA_ALTA,NOMBRE_CLIENTE,APELLIDOS
      -    '_CLIENTE,DIRECCION_CLIENTE,TELEF_CLIENTE,EMAIL_CLIENTE,TARJE
      -    'TA,CREDITO,HIPOTECA,CTA_ACTIVA,FECHA_CIERRE,SALDO_CLIENTE) V
      -    'ALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 215.
           05 SQL-STMT   PIC X(215) VALUE 'SELECT TIPO_DOC,DOC_CLIENTE,F
      -    'ECHA_ALTA,NOMBRE_CLIENTE,APELLIDOS_CLIENTE,DIRECCION_CLIENTE
      -    ',TELEF_CLIENTE,EMAIL_CLIENTE,TARJETA,CREDITO,HIPOTECA,CTA_AC
      -    'TIVA,FECHA_CIERRE,SALDO_CLIENTE FROM clientes WHERE ID_CLIEN
      -    'TE = ?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 9.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 168.
           05 SQL-STMT   PIC X(168) VALUE 'UPDATE clientes SET DIRECCION
      -    '_CLIENTE = ?,TELEF_CLIENTE = ?,EMAIL_CLIENTE = ?,TARJETA = ?
      -    ',CREDITO = ?,HIPOTECA = ?,CTA_ACTIVA = ?,SALDO_CLIENTE = ? W
      -    'HERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 55.
           05 SQL-STMT   PIC X(55) VALUE 'UPDATE clientes SET CTA_ACTIVA
      -    ' = 0 WHERE ID_CLIENTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(1) COMP-3.
           05 SQL-VAR-0003  PIC S9(1) COMP-3.
           05 SQL-VAR-0004  PIC S9(1) COMP-3.
           05 SQL-VAR-0005  PIC S9(1) COMP-3.
           05 SQL-VAR-0006  PIC S9(11)V9(2) COMP-3.
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
      *    Recibimos los datos del cliente y el control de transaccion
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  REG-CUSM.
           05 CUSM-ID-CLIENTE          PIC 9(08).
           05 CUSM-TIPO-DOC            PIC X(03).
           05 CUSM-DOC-CLIENTE         PIC X(12).
           05 CUSM-FECHA-ALTA          PIC X(10).
           05 CUSM-NOMBRE              PIC X(25).
           05 CUSM-APELLIDOS           PIC X(25).
           05 CUSM-DIRECCION           PIC X(45).
           05 CUSM-TELEFONO            PIC X(12).
           05 CUSM-EMAIL               PIC X(40).
           05 CUSM-TARJETA             PIC 9(01).
           05 CUSM-CREDITO             PIC 9(01).
           05 CUSM-HIPOTECA            PIC 9(01).
           05 CUSM-CTA-ACTIVA          PIC 9(01).
           05 CUSM-FECHA-CIERRE        PIC X(10).
           05 CUSM-SALDO-CLIENTE       PIC S9(10)V99.
           COPY LKCIF.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION USING REG-CUSM, LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 0 TO LK-COD-RETORNO
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
               WHEN OTHER
                   MOVE 98 TO LK-COD-RETORNO
                   MOVE "ACCION DB NO SOPORTADA" TO LK-MENSAJE
           END-EVALUATE.

           EXIT PROGRAM.

       1000-INSERTAR-CLIENTE.
      *    EXEC SQL
      *        INSERT INTO clientes (
      *            ID_CLIENTE, TIPO_DOC, DOC_CLIENTE,
      *            FECHA_ALTA, NOMBRE_CLIENTE, APELLIDOS_CLIENTE,
      *            DIRECCION_CLIENTE, TELEF_CLIENTE, EMAIL_CLIENTE,
      *            TARJETA, CREDITO, HIPOTECA, CTA_ACTIVA,
      *            FECHA_CIERRE, SALDO_CLIENTE
      *        ) VALUES (
      *            :CUSM-ID-CLIENTE,
      *            :CUSM-TIPO-DOC,
      *            :CUSM-DOC-CLIENTE,
      *            :CUSM-FECHA-ALTA,
      *            :CUSM-NOMBRE,
      *            :CUSM-APELLIDOS,
      *            :CUSM-DIRECCION,
      *            :CUSM-TELEFONO,
      *            :CUSM-EMAIL,
      *            :CUSM-TARJETA,
      *            :CUSM-CREDITO,
      *            :CUSM-HIPOTECA,
      *            :CUSM-CTA-ACTIVA,
      *            :CUSM-FECHA-CIERRE,
      *            :CUSM-SALDO-CLIENTE
      *        )
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
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
                 CUSM-DOC-CLIENTE
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 12 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 CUSM-FECHA-ALTA
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 CUSM-NOMBRE
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 25 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 CUSM-APELLIDOS
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 25 TO SQL-LEN(6)
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
               MOVE 1 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(11)
               MOVE 1 TO SQL-LEN(11)
               MOVE X'00' TO SQL-PREC(11)
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
                 CUSM-FECHA-CIERRE
               MOVE 'X' TO SQL-TYPE(14)
               MOVE 10 TO SQL-LEN(14)
               SET SQL-ADDR(15) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(15)
               MOVE 7 TO SQL-LEN(15)
               MOVE X'02' TO SQL-PREC(15)
               MOVE 15 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE CUSM-ID-CLIENTE
             TO SQL-VAR-0001
           MOVE CUSM-TARJETA
             TO SQL-VAR-0002
           MOVE CUSM-CREDITO
             TO SQL-VAR-0003
           MOVE CUSM-HIPOTECA
             TO SQL-VAR-0004
           MOVE CUSM-CTA-ACTIVA
             TO SQL-VAR-0005
           MOVE CUSM-SALDO-CLIENTE
             TO SQL-VAR-0006
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .

           PERFORM 9000-EVALUAR-SQL.

           IF LK-COD-RETORNO = 0
      *        EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
               MOVE "CLIENTE REGISTRADO CON EXITO" TO LK-MENSAJE
           END-IF.

       2000-CONSULTAR-CLIENTE.
      *    EXEC SQL
      *        SELECT TIPO_DOC, DOC_CLIENTE, FECHA_ALTA,
      *               NOMBRE_CLIENTE, APELLIDOS_CLIENTE,
      *               DIRECCION_CLIENTE, TELEF_CLIENTE, EMAIL_CLIENTE,
      *               TARJETA, CREDITO, HIPOTECA, CTA_ACTIVA,
      *               FECHA_CIERRE, SALDO_CLIENTE
      *        INTO :CUSM-TIPO-DOC,
      *             :CUSM-DOC-CLIENTE,
      *             :CUSM-FECHA-ALTA,
      *             :CUSM-NOMBRE,
      *             :CUSM-APELLIDOS,
      *             :CUSM-DIRECCION,
      *             :CUSM-TELEFONO,
      *             :CUSM-EMAIL,
      *             :CUSM-TARJETA,
      *             :CUSM-CREDITO,
      *             :CUSM-HIPOTECA,
      *             :CUSM-CTA-ACTIVA,
      *             :CUSM-FECHA-CIERRE,
      *             :CUSM-SALDO-CLIENTE
      *        FROM clientes
      *        WHERE ID_CLIENTE = :CUSM-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
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
                 CUSM-DIRECCION
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 45 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 CUSM-TELEFONO
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 12 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 CUSM-EMAIL
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 40 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(9)
               MOVE 1 TO SQL-LEN(9)
               MOVE X'00' TO SQL-PREC(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(10)
               MOVE 1 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
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
                 CUSM-FECHA-CIERRE
               MOVE 'X' TO SQL-TYPE(13)
               MOVE 10 TO SQL-LEN(13)
               SET SQL-ADDR(14) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(14)
               MOVE 7 TO SQL-LEN(14)
               MOVE X'02' TO SQL-PREC(14)
               SET SQL-ADDR(15) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(15)
               MOVE 5 TO SQL-LEN(15)
               MOVE X'00' TO SQL-PREC(15)
               MOVE 15 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE CUSM-ID-CLIENTE TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0002 TO CUSM-TARJETA
           MOVE SQL-VAR-0003 TO CUSM-CREDITO
           MOVE SQL-VAR-0004 TO CUSM-HIPOTECA
           MOVE SQL-VAR-0005 TO CUSM-CTA-ACTIVA
           MOVE SQL-VAR-0006 TO CUSM-SALDO-CLIENTE
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = 0
               MOVE "CONSULTA EXITOSA" TO LK-MENSAJE
           END-IF.

       3000-ACTUALIZAR-CLIENTE.
      *    EXEC SQL
      *        UPDATE clientes
      *        SET DIRECCION_CLIENTE = :CUSM-DIRECCION,
      *            TELEF_CLIENTE = :CUSM-TELEFONO,
      *            EMAIL_CLIENTE = :CUSM-EMAIL,
      *            TARJETA = :CUSM-TARJETA,
      *            CREDITO = :CUSM-CREDITO,
      *            HIPOTECA = :CUSM-HIPOTECA,
      *            CTA_ACTIVA = :CUSM-CTA-ACTIVA,
      *            SALDO_CLIENTE = :CUSM-SALDO-CLIENTE
      *        WHERE ID_CLIENTE = :CUSM-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
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
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(4)
               MOVE 1 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(5)
               MOVE 1 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(6)
               MOVE 1 TO SQL-LEN(6)
               MOVE X'00' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(7)
               MOVE 1 TO SQL-LEN(7)
               MOVE X'00' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(8)
               MOVE 7 TO SQL-LEN(8)
               MOVE X'02' TO SQL-PREC(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(9)
               MOVE 5 TO SQL-LEN(9)
               MOVE X'00' TO SQL-PREC(9)
               MOVE 9 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE CUSM-TARJETA
             TO SQL-VAR-0002
           MOVE CUSM-CREDITO
             TO SQL-VAR-0003
           MOVE CUSM-HIPOTECA
             TO SQL-VAR-0004
           MOVE CUSM-CTA-ACTIVA
             TO SQL-VAR-0005
           MOVE CUSM-SALDO-CLIENTE
             TO SQL-VAR-0006
           MOVE CUSM-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = 0
      *        EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
               MOVE "CLIENTE ACTUALIZADO" TO LK-MENSAJE
           END-IF.

       4000-BAJA-LOGICA-CLIENTE.
      *    La tabla no tiene ESTADO. La baja se considera desactivar la
      *    EXEC SQL
      *        UPDATE clientes
      *        SET CTA_ACTIVA = 0
      *        WHERE ID_CLIENTE = :CUSM-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE CUSM-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = 0
               MOVE "BAJA LOGICA REALIZADA (CTA_ACTIVA=0)" TO LK-MENSAJE
           END-IF.

       9000-EVALUAR-SQL.
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 00 TO LK-COD-RETORNO
               WHEN 100
                   MOVE 01 TO LK-COD-RETORNO
                   MOVE "REGISTRO NO ENCONTRADO" TO LK-MENSAJE
               WHEN OTHER
                   MOVE 99 TO LK-COD-RETORNO
                   MOVE "ERROR CRITICO EN BASE DE DATOS" TO LK-MENSAJE
           END-EVALUATE.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  CUSM-APELLIDOS           IN USE CHAR(25)
      *  CUSM-CREDITO             IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(1,0)
      *  CUSM-CTA-ACTIVA          IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(1,0)
      *  CUSM-DIRECCION           IN USE CHAR(45)
      *  CUSM-DOC-CLIENTE         IN USE CHAR(12)
      *  CUSM-EMAIL               IN USE CHAR(40)
      *  CUSM-FECHA-ALTA          IN USE CHAR(10)
      *  CUSM-FECHA-CIERRE        IN USE CHAR(10)
      *  CUSM-HIPOTECA            IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(1,0)
      *  CUSM-ID-CLIENTE          IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  CUSM-NOMBRE              IN USE CHAR(25)
      *  CUSM-SALDO-CLIENTE       IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(13,2)
      *  CUSM-TARJETA             IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(1,0)
      *  CUSM-TELEFONO            IN USE CHAR(12)
      *  CUSM-TIPO-DOC            IN USE CHAR(3)
      *  LKCIF                NOT IN USE
      *  REG-CUSM             NOT IN USE
      *  REG-CUSM.CUSM-APELLIDOS NOT IN USE
      *  REG-CUSM.CUSM-CREDITO NOT IN USE
      *  REG-CUSM.CUSM-CTA-ACTIVA NOT IN USE
      *  REG-CUSM.CUSM-DIRECCION NOT IN USE
      *  REG-CUSM.CUSM-DOC-CLIENTE NOT IN USE
      *  REG-CUSM.CUSM-EMAIL  NOT IN USE
      *  REG-CUSM.CUSM-FECHA-ALTA NOT IN USE
      *  REG-CUSM.CUSM-FECHA-CIERRE NOT IN USE
      *  REG-CUSM.CUSM-HIPOTECA NOT IN USE
      *  REG-CUSM.CUSM-ID-CLIENTE NOT IN USE
      *  REG-CUSM.CUSM-NOMBRE NOT IN USE
      *  REG-CUSM.CUSM-SALDO-CLIENTE NOT IN USE
      *  REG-CUSM.CUSM-TARJETA NOT IN USE
      *  REG-CUSM.CUSM-TELEFONO NOT IN USE
      *  REG-CUSM.CUSM-TIPO-DOC NOT IN USE
      *  REG-CUSM.LKCIF       NOT IN USE
      **********************************************************************

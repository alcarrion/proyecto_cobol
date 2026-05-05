      *================================================================*
      * PROGRAMA: DBIOTARJ.sqb                                         *
      * FUNCION:  CRUD de la tabla TARJETAS (Tarjetas de Credito)      *
      * ARQUITECTURA: Capa de Acceso a Datos (DBIO)                    *
      * OPERACIONES: INSERT/SELECT/UPDATE + BAJA LOGICA                *
      * PK COMPUESTA: (ID_CLIENTE, NRO_TARJETA)                        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOTARJ.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 10.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 10 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 10 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 10 TIMES.
           05 SQL-PREC   PIC X OCCURS 10 TIMES.
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
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 163.
           05 SQL-STMT   PIC X(163) VALUE 'SELECT ID_CLIENTE,NRO_TARJETA
      -    ',FECHA_EMISION,FECHA_VENCIMIENTO,LIMITE_TARJETA,ACUM_MES,LIQ
      -    'UIDACION_MES,ESTADO FROM TARJETAS WHERE ID_CLIENTE = ? AND N
      -    'RO_TARJETA = ?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 6.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 123.
           05 SQL-STMT   PIC X(123) VALUE 'UPDATE TARJETAS SET ACUM_MES 
      -    '= ?,LIQUIDACION_MES = ?,LIMITE_TARJETA = ?,ESTADO = ? WHERE 
      -    'ID_CLIENTE = ? AND NRO_TARJETA = ?'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 73.
           05 SQL-STMT   PIC X(73) VALUE 'UPDATE TARJETAS SET ESTADO = '
      -    ''I'' WHERE ID_CLIENTE = ? AND NRO_TARJETA = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0003  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(11)V9(2) COMP-3.
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

      *    Host variables para SQL (deben estar en WORKING-STORAGE)
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.
           01  WS-TARJ.
               05 WS-TARJ-ID-CLIENTE          PIC 9(08).
               05 WS-TARJ-NRO-TARJETA         PIC X(16).
               05 WS-TARJ-FECHA-EMISION       PIC X(10).
               05 WS-TARJ-FECHA-VENCIM        PIC X(10).
               05 WS-TARJ-LIMITE-TARJETA      PIC S9(10)V99 COMP-3.
               05 WS-TARJ-ACUM-MES            PIC S9(10)V99 COMP-3.
               05 WS-TARJ-LIQUIDACION-MES     PIC S9(10)V99 COMP-3.
               05 WS-TARJ-ESTADO              PIC X(01).
           01  WS-LK.
               05 WS-LK-ACCION-DB             PIC X(01).
               05 WS-LK-ID-CLIENTE            PIC 9(09).
               05 WS-LK-IMPORTE               PIC S9(13)V99 COMP-3.
               05 WS-LK-MODO-OPERACION        PIC X(01).
               05 WS-LK-COD-RETORNO           PIC 9(02).
               05 WS-LK-MENSAJE               PIC X(50).
               05 WS-LK-USUARIO-ID            PIC X(08).
               05 WS-LK-TERMINAL-ID           PIC X(04).
               05 WS-LK-FECHA-PROCESO         PIC 9(08).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       LINKAGE SECTION.
           COPY TARJREC.
           COPY LKCIF.

       PROCEDURE DIVISION USING REG-TARJ, LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
      *    Copiar datos de LINKAGE a WORKING-STORAGE (host variables)
           MOVE TARJ-ID-CLIENTE       TO WS-TARJ-ID-CLIENTE
           MOVE TARJ-NRO-TARJETA      TO WS-TARJ-NRO-TARJETA
           MOVE TARJ-FECHA-EMISION    TO WS-TARJ-FECHA-EMISION
           MOVE TARJ-FECHA-VENCIM     TO WS-TARJ-FECHA-VENCIM
           MOVE TARJ-LIMITE-TARJETA   TO WS-TARJ-LIMITE-TARJETA
           MOVE TARJ-ACUM-MES         TO WS-TARJ-ACUM-MES
           MOVE TARJ-LIQUIDACION-MES  TO WS-TARJ-LIQUIDACION-MES
           MOVE TARJ-ESTADO           TO WS-TARJ-ESTADO
           MOVE LK-ACCION-DB          TO WS-LK-ACCION-DB

           MOVE 0      TO WS-LK-COD-RETORNO
           MOVE SPACES TO WS-LK-MENSAJE

           EVALUATE WS-LK-ACCION-DB
               WHEN 'A'
                   PERFORM 1000-INSERTAR-TARJETA
               WHEN 'C'
                   PERFORM 2000-CONSULTAR-TARJETA
               WHEN 'M'
                   PERFORM 3000-ACTUALIZAR-TARJETA
               WHEN 'B'
                   PERFORM 4000-BAJA-LOGICA-TARJETA
               WHEN OTHER
                   MOVE 98 TO WS-LK-COD-RETORNO
                   MOVE "ACCION DB NO SOPORTADA" TO WS-LK-MENSAJE
           END-EVALUATE.

      *    Devolver resultados al LINKAGE
           MOVE WS-LK-COD-RETORNO     TO LK-COD-RETORNO
           MOVE WS-LK-MENSAJE         TO LK-MENSAJE
           MOVE WS-TARJ-ID-CLIENTE    TO TARJ-ID-CLIENTE
           MOVE WS-TARJ-NRO-TARJETA   TO TARJ-NRO-TARJETA
           MOVE WS-TARJ-FECHA-EMISION TO TARJ-FECHA-EMISION
           MOVE WS-TARJ-FECHA-VENCIM  TO TARJ-FECHA-VENCIM
           MOVE WS-TARJ-LIMITE-TARJETA TO TARJ-LIMITE-TARJETA
           MOVE WS-TARJ-ACUM-MES      TO TARJ-ACUM-MES
           MOVE WS-TARJ-LIQUIDACION-MES TO TARJ-LIQUIDACION-MES
           MOVE WS-TARJ-ESTADO        TO TARJ-ESTADO

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
      *            :WS-TARJ-ID-CLIENTE,
      *            :WS-TARJ-NRO-TARJETA,
      *            :WS-TARJ-FECHA-EMISION,
      *            :WS-TARJ-FECHA-VENCIM,
      *            :WS-TARJ-LIMITE-TARJETA,
      *            :WS-TARJ-ACUM-MES,
      *            :WS-TARJ-LIQUIDACION-MES,
      *            :WS-TARJ-ESTADO
      *        )
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 16 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-TARJ-FECHA-EMISION
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WS-TARJ-FECHA-VENCIM
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
                 WS-TARJ-ESTADO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 1 TO SQL-LEN(8)
               MOVE 8 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-TARJ-ID-CLIENTE
             TO SQL-VAR-0001
           MOVE WS-TARJ-LIMITE-TARJETA
             TO SQL-VAR-0002
           MOVE WS-TARJ-ACUM-MES
             TO SQL-VAR-0003
           MOVE WS-TARJ-LIQUIDACION-MES
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF WS-LK-COD-RETORNO = 0
               MOVE "TARJETA REGISTRADA CON EXITO" TO WS-LK-MENSAJE
           END-IF.

       2000-CONSULTAR-TARJETA.
      *    EXEC SQL
      *        SELECT ID_CLIENTE, NRO_TARJETA,
      *               FECHA_EMISION, FECHA_VENCIMIENTO,
      *               LIMITE_TARJETA, ACUM_MES,
      *               LIQUIDACION_MES, ESTADO
      *        INTO :WS-TARJ-ID-CLIENTE, :WS-TARJ-NRO-TARJETA,
      *             :WS-TARJ-FECHA-EMISION, :WS-TARJ-FECHA-VENCIM,
      *             :WS-TARJ-LIMITE-TARJETA, :WS-TARJ-ACUM-MES,
      *             :WS-TARJ-LIQUIDACION-MES, :WS-TARJ-ESTADO
      *        FROM TARJETAS
      *        WHERE ID_CLIENTE = :WS-TARJ-ID-CLIENTE
      *          AND NRO_TARJETA = :WS-TARJ-NRO-TARJETA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 16 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-TARJ-FECHA-EMISION
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WS-TARJ-FECHA-VENCIM
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
                 WS-TARJ-ESTADO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 1 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(9)
               MOVE 5 TO SQL-LEN(9)
               MOVE X'00' TO SQL-PREC(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 WS-TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(10)
               MOVE 16 TO SQL-LEN(10)
               MOVE 10 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-TARJ-ID-CLIENTE TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-TARJ-ID-CLIENTE
           MOVE SQL-VAR-0002 TO WS-TARJ-LIMITE-TARJETA
           MOVE SQL-VAR-0003 TO WS-TARJ-ACUM-MES
           MOVE SQL-VAR-0004 TO WS-TARJ-LIQUIDACION-MES
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF WS-LK-COD-RETORNO = 0
               MOVE "CONSULTA EXITOSA" TO WS-LK-MENSAJE
           END-IF.

       3000-ACTUALIZAR-TARJETA.
      *    EXEC SQL
      *        UPDATE TARJETAS
      *        SET ACUM_MES = :WS-TARJ-ACUM-MES,
      *            LIQUIDACION_MES = :WS-TARJ-LIQUIDACION-MES,
      *            LIMITE_TARJETA = :WS-TARJ-LIMITE-TARJETA,
      *            ESTADO = :WS-TARJ-ESTADO
      *        WHERE ID_CLIENTE = :WS-TARJ-ID-CLIENTE
      *          AND NRO_TARJETA = :WS-TARJ-NRO-TARJETA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
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
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(3)
               MOVE 7 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WS-TARJ-ESTADO
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 1 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(5)
               MOVE 5 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 WS-TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 16 TO SQL-LEN(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-TARJ-ACUM-MES
             TO SQL-VAR-0003
           MOVE WS-TARJ-LIQUIDACION-MES
             TO SQL-VAR-0004
           MOVE WS-TARJ-LIMITE-TARJETA
             TO SQL-VAR-0002
           MOVE WS-TARJ-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF WS-LK-COD-RETORNO = 0
               MOVE "TARJETA ACTUALIZADA" TO WS-LK-MENSAJE
           END-IF.

       4000-BAJA-LOGICA-TARJETA.
      *    EXEC SQL
      *        UPDATE TARJETAS
      *        SET ESTADO = 'I'
      *        WHERE ID_CLIENTE = :WS-TARJ-ID-CLIENTE
      *          AND NRO_TARJETA = :WS-TARJ-NRO-TARJETA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 16 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-TARJ-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF WS-LK-COD-RETORNO = 0
               MOVE "BAJA LOGICA REALIZADA" TO WS-LK-MENSAJE
           END-IF.

       9000-EVALUAR-SQL.
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 00 TO WS-LK-COD-RETORNO
               WHEN 100
                   MOVE 01 TO WS-LK-COD-RETORNO
                   MOVE "REGISTRO NO ENCONTRADO" TO WS-LK-MENSAJE
               WHEN OTHER
                   MOVE 99 TO WS-LK-COD-RETORNO
                   MOVE "ERROR CRITICO EN BASE DE DATOS" TO WS-LK-MENSAJ
           END-EVALUATE.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-LK                NOT IN USE
      *  WS-LK-ACCION-DB      NOT IN USE
      *  WS-LK-COD-RETORNO    NOT IN USE
      *  WS-LK-FECHA-PROCESO  NOT IN USE
      *  WS-LK-ID-CLIENTE     NOT IN USE
      *  WS-LK-IMPORTE        NOT IN USE
      *  WS-LK-MENSAJE        NOT IN USE
      *  WS-LK-MODO-OPERACION NOT IN USE
      *  WS-LK-TERMINAL-ID    NOT IN USE
      *  WS-LK-USUARIO-ID     NOT IN USE
      *  WS-LK.WS-LK-ACCION-DB NOT IN USE
      *  WS-LK.WS-LK-COD-RETORNO NOT IN USE
      *  WS-LK.WS-LK-FECHA-PROCESO NOT IN USE
      *  WS-LK.WS-LK-ID-CLIENTE NOT IN USE
      *  WS-LK.WS-LK-IMPORTE  NOT IN USE
      *  WS-LK.WS-LK-MENSAJE  NOT IN USE
      *  WS-LK.WS-LK-MODO-OPERACION NOT IN USE
      *  WS-LK.WS-LK-TERMINAL-ID NOT IN USE
      *  WS-LK.WS-LK-USUARIO-ID NOT IN USE
      *  WS-TARJ              NOT IN USE
      *  WS-TARJ-ACUM-MES         IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(13,2)
      *  WS-TARJ-ESTADO           IN USE CHAR(1)
      *  WS-TARJ-FECHA-EMISION     IN USE CHAR(10)
      *  WS-TARJ-FECHA-VENCIM     IN USE CHAR(10)
      *  WS-TARJ-ID-CLIENTE       IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  WS-TARJ-LIMITE-TARJETA     IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(13,2)
      *  WS-TARJ-LIQUIDACION-MES     IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(13,2)
      *  WS-TARJ-NRO-TARJETA      IN USE CHAR(16)
      *  WS-TARJ.WS-TARJ-ACUM-MES NOT IN USE
      *  WS-TARJ.WS-TARJ-ESTADO NOT IN USE
      *  WS-TARJ.WS-TARJ-FECHA-EMISION NOT IN USE
      *  WS-TARJ.WS-TARJ-FECHA-VENCIM NOT IN USE
      *  WS-TARJ.WS-TARJ-ID-CLIENTE NOT IN USE
      *  WS-TARJ.WS-TARJ-LIMITE-TARJETA NOT IN USE
      *  WS-TARJ.WS-TARJ-LIQUIDACION-MES NOT IN USE
      *  WS-TARJ.WS-TARJ-NRO-TARJETA NOT IN USE
      **********************************************************************

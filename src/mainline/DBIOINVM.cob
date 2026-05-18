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
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 135.
           05 SQL-STMT   PIC X(135) VALUE 'SELECT ID_CUENTA,TIPO_CUENTA,
      -    'SALDO_ACTUAL,FECHA_APERTURA,ESTADO_CUENTA FROM ctactes WHERE
      -    ' ID_CLIENTE = ? ORDER BY TIPO_CUENTA,ID_CUENTA'.
           05 SQL-CNAME  PIC X(8) VALUE 'CUR_CTAS'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 50.
           05 SQL-STMT   PIC X(50) VALUE 'SELECT DATE_FORMAT(CURDATE(),'
      -    ''%Y-%m-%d'') FROM DUAL'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 5.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 105.
           05 SQL-STMT   PIC X(105) VALUE 'INSERT INTO ctactes (ID_CLIEN
      -    'TE,TIPO_CUENTA,SALDO_ACTUAL,FECHA_APERTURA,ESTADO_CUENTA) VA
      -    'LUES (?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 33.
           05 SQL-STMT   PIC X(33) VALUE 'SELECT LAST_INSERT_ID() FROM D
      -    'UAL'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 77.
           05 SQL-STMT   PIC X(77) VALUE 'UPDATE ctactes SET ESTADO_CUEN
      -    'TA = ''C'' WHERE ID_CUENTA = ? AND ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 112.
           05 SQL-STMT   PIC X(112) VALUE 'SELECT TIPO_CUENTA,SALDO_ACTU
      -    'AL,FECHA_APERTURA,ESTADO_CUENTA FROM ctactes WHERE ID_CUENTA
      -    ' = ? AND ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 85.
           05 SQL-STMT   PIC X(85) VALUE 'SELECT SALDO_ACTUAL,ESTADO_CUE
      -    'NTA FROM ctactes WHERE ID_CUENTA = ? AND ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 55.
           05 SQL-STMT   PIC X(55) VALUE 'UPDATE ctactes SET SALDO_ACTUA
      -    'L = ? WHERE ID_CUENTA = ?'.
      **********************************************************************
       01 SQL-STMT-8.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 6.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 113.
           05 SQL-STMT   PIC X(113) VALUE 'INSERT INTO movimientos (ID_C
      -    'UENTA,TIPO_MOV,IMPORTE,SALDO_RESULTANTE,TERMINAL_ID,USUARIO_
      -    'ID) VALUES (?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-9.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 97.
           05 SQL-STMT   PIC X(97) VALUE 'SELECT SALDO_ACTUAL,ESTADO_CUE
      -    'NTA,TIPO_CUENTA FROM ctactes WHERE ID_CUENTA = ? AND ID_CLIE
      -    'NTE = ?'.
      **********************************************************************
       01 SQL-STMT-10.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 55.
           05 SQL-STMT   PIC X(55) VALUE 'UPDATE ctactes SET SALDO_ACTUA
      -    'L = ? WHERE ID_CUENTA = ?'.
      **********************************************************************
       01 SQL-STMT-11.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 6.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 113.
           05 SQL-STMT   PIC X(113) VALUE 'INSERT INTO movimientos (ID_C
      -    'UENTA,TIPO_MOV,IMPORTE,SALDO_RESULTANTE,TERMINAL_ID,USUARIO_
      -    'ID) VALUES (?,?,?,?,?,?)'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(9) COMP-3.
           05 SQL-VAR-0003  PIC S9(9) COMP-3.
           05 SQL-VAR-0004  PIC S9(1) COMP-3.
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
       01  DB-INVM-ID-CUENTA      PIC 9(08).
       01  DB-INVM-ID-CLIENTE     PIC 9(08).
       01  DB-INVM-TIPO-CUENTA    PIC X(01).
       01  DB-INVM-SALDO-ACTUAL   PIC S9(13)V99 COMP-3.
       01  DB-INVM-FECHA-APERTURA PIC X(10).
       01  DB-INVM-ESTADO-CUENTA  PIC X(01).
       01  DB-INVM-FECHA-HOY      PIC X(10).
       01  DB-MOV-ID-CUENTA       PIC 9(08).
       01  DB-MOV-TIPO-MOV        PIC 9(01).
       01  DB-MOV-IMPORTE         PIC S9(13)V99 COMP-3.
       01  DB-MOV-SALDO-RES       PIC S9(13)V99 COMP-3.
       01  DB-MOV-TERMINAL        PIC X(04).
       01  DB-MOV-USUARIO         PIC X(08).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

      *    EXEC SQL
      *        DECLARE cur_ctas CURSOR FOR
      *        SELECT ID_CUENTA, TIPO_CUENTA, SALDO_ACTUAL,
      *               FECHA_APERTURA, ESTADO_CUENTA
      *        FROM   ctactes
      *        WHERE  ID_CLIENTE = :DB-INVM-ID-CLIENTE
      *        ORDER  BY TIPO_CUENTA, ID_CUENTA
      *    END-EXEC.

       01  WS-TX-IMPORTE          PIC S9(13)V99 COMP-3.

       LINKAGE SECTION.
           COPY INVMREC.
           COPY LKCIF.

       PROCEDURE DIVISION USING REG-INVM, LK-DATOS-SESION,
                                LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE '00  ' TO LK-COD-RETORNO.
           MOVE SPACES TO LK-MENSAJE.

           EVALUATE LK-ACCION-DB
               WHEN 'C'
                   PERFORM 1000-ABRIR-CURSOR-FETCH
               WHEN 'F'
                   PERFORM 2000-FETCH-SIGUIENTE
               WHEN 'Z'
                   PERFORM 3000-CERRAR-CURSOR
               WHEN 'A'
                   PERFORM 4000-INSERTAR-CUENTA
               WHEN 'B'
                   PERFORM 5000-CERRAR-CUENTA
               WHEN 'S'
                   PERFORM 6000-CONSULTAR-UNA
               WHEN 'D'
                   PERFORM 7000-DEPOSITAR
               WHEN 'R'
                   PERFORM 8000-RETIRAR
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE 'Accion no reconocida en DBIOINVM'
                     TO LK-MENSAJE
           END-EVALUATE.
           GOBACK.

      ************************************************************
      * C - Abrir cursor y primer FETCH
      ************************************************************
       1000-ABRIR-CURSOR-FETCH.
           MOVE INVM-ID-CLIENTE TO DB-INVM-ID-CLIENTE.

      *    EXEC SQL OPEN cur_ctas END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
           END-IF
           MOVE DB-INVM-ID-CLIENTE TO SQL-VAR-0002
           CALL 'OCSQLOCU' USING SQL-STMT-0
                               SQLCA
           END-CALL
                                          .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF NOT LK-EXITO EXIT PARAGRAPH END-IF.

      *    EXEC SQL
      *        FETCH cur_ctas
      *        INTO  :DB-INVM-ID-CUENTA, :DB-INVM-TIPO-CUENTA,
      *              :DB-INVM-SALDO-ACTUAL, :DB-INVM-FECHA-APERTURA,
      *              :DB-INVM-ESTADO-CUENTA
      *    END-EXEC.
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             DB-INVM-TIPO-CUENTA
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 1 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             DB-INVM-SALDO-ACTUAL
           MOVE '3' TO SQL-TYPE(3)
           MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             DB-INVM-FECHA-APERTURA
           MOVE 'X' TO SQL-TYPE(4)
           MOVE 10 TO SQL-LEN(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             DB-INVM-ESTADO-CUENTA
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 1 TO SQL-LEN(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-INVM-ID-CUENTA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF LK-EXITO PERFORM 9100-MOVER-A-REG END-IF.

      ************************************************************
      * F - Fetch siguiente fila
      ************************************************************
       2000-FETCH-SIGUIENTE.
      *    EXEC SQL
      *        FETCH cur_ctas
      *        INTO  :DB-INVM-ID-CUENTA, :DB-INVM-TIPO-CUENTA,
      *              :DB-INVM-SALDO-ACTUAL, :DB-INVM-FECHA-APERTURA,
      *              :DB-INVM-ESTADO-CUENTA
      *    END-EXEC.
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             DB-INVM-TIPO-CUENTA
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 1 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             DB-INVM-SALDO-ACTUAL
           MOVE '3' TO SQL-TYPE(3)
           MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             DB-INVM-FECHA-APERTURA
           MOVE 'X' TO SQL-TYPE(4)
           MOVE 10 TO SQL-LEN(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             DB-INVM-ESTADO-CUENTA
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 1 TO SQL-LEN(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-INVM-ID-CUENTA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF LK-EXITO PERFORM 9100-MOVER-A-REG END-IF.

      ************************************************************
      * Z - Cerrar cursor
      ************************************************************
       3000-CERRAR-CURSOR.
      *    EXEC SQL CLOSE cur_ctas END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA
                                           .
           PERFORM 9000-EVALUAR-SQLSTATE.

      ************************************************************
      * A - Insertar cuenta nueva
      ************************************************************
       4000-INSERTAR-CUENTA.
           MOVE INVM-ID-CLIENTE    TO DB-INVM-ID-CLIENTE.
           MOVE INVM-TIPO-CUENTA   TO DB-INVM-TIPO-CUENTA.
           MOVE INVM-SALDO-ACTUAL  TO DB-INVM-SALDO-ACTUAL.
           MOVE INVM-ESTADO-CUENTA TO DB-INVM-ESTADO-CUENTA.

      *    EXEC SQL
      *        SELECT DATE_FORMAT(CURDATE(),'%Y-%m-%d')
      *        INTO   :DB-INVM-FECHA-HOY FROM DUAL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-INVM-FECHA-HOY
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 10 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
                   .

      *    EXEC SQL
      *        INSERT INTO ctactes
      *            (ID_CLIENTE, TIPO_CUENTA, SALDO_ACTUAL,
      *             FECHA_APERTURA, ESTADO_CUENTA)
      *        VALUES
      *            (:DB-INVM-ID-CLIENTE, :DB-INVM-TIPO-CUENTA,
      *             :DB-INVM-SALDO-ACTUAL,
      *             :DB-INVM-FECHA-HOY, :DB-INVM-ESTADO-CUENTA)
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-INVM-TIPO-CUENTA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-INVM-SALDO-ACTUAL
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 DB-INVM-FECHA-HOY
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 DB-INVM-ESTADO-CUENTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 1 TO SQL-LEN(5)
               MOVE 5 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-INVM-ID-CLIENTE
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.

           IF LK-EXITO
      *        EXEC SQL
      *            SELECT LAST_INSERT_ID()
      *            INTO   :DB-INVM-ID-CUENTA FROM DUAL
      *        END-EXEC
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
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-INVM-ID-CUENTA
               MOVE DB-INVM-ID-CUENTA TO INVM-ID-CUENTA
               MOVE DB-INVM-FECHA-HOY TO INVM-FECHA-APERTURA
               MOVE 'Cuenta abierta correctamente' TO LK-MENSAJE
           END-IF.

      ************************************************************
      * B - Cerrar cuenta logicamente (estado = C)
      ************************************************************
       5000-CERRAR-CUENTA.
           MOVE INVM-ID-CUENTA  TO DB-INVM-ID-CUENTA.
           MOVE INVM-ID-CLIENTE TO DB-INVM-ID-CLIENTE.

      *    EXEC SQL
      *        UPDATE ctactes SET ESTADO_CUENTA = 'C'
      *        WHERE  ID_CUENTA  = :DB-INVM-ID-CUENTA
      *        AND    ID_CLIENTE = :DB-INVM-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-INVM-ID-CUENTA
             TO SQL-VAR-0001
           MOVE DB-INVM-ID-CLIENTE
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF LK-EXITO
               MOVE 'Cuenta cerrada correctamente' TO LK-MENSAJE
           END-IF.

      ************************************************************
      * S - Consultar una cuenta por ID_CUENTA + ID_CLIENTE
      ************************************************************
       6000-CONSULTAR-UNA.
           MOVE INVM-ID-CUENTA  TO DB-INVM-ID-CUENTA.
           MOVE INVM-ID-CLIENTE TO DB-INVM-ID-CLIENTE.

      *    EXEC SQL
      *        SELECT TIPO_CUENTA, SALDO_ACTUAL,
      *               FECHA_APERTURA, ESTADO_CUENTA
      *        INTO   :DB-INVM-TIPO-CUENTA, :DB-INVM-SALDO-ACTUAL,
      *               :DB-INVM-FECHA-APERTURA,
      *               :DB-INVM-ESTADO-CUENTA
      *        FROM   ctactes
      *        WHERE  ID_CUENTA  = :DB-INVM-ID-CUENTA
      *        AND    ID_CLIENTE = :DB-INVM-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-INVM-TIPO-CUENTA
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-INVM-SALDO-ACTUAL
               MOVE '3' TO SQL-TYPE(2)
               MOVE 8 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-INVM-FECHA-APERTURA
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 DB-INVM-ESTADO-CUENTA
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 1 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(5)
               MOVE 5 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(6)
               MOVE 5 TO SQL-LEN(6)
               MOVE X'00' TO SQL-PREC(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-INVM-ID-CUENTA TO SQL-VAR-0001
           MOVE DB-INVM-ID-CLIENTE TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF LK-EXITO PERFORM 9100-MOVER-A-REG END-IF.

      ************************************************************
      * D - Deposito: INVM-SALDO-ACTUAL contiene el monto
      ************************************************************
       7000-DEPOSITAR.
           MOVE INVM-SALDO-ACTUAL TO WS-TX-IMPORTE.
           MOVE INVM-ID-CUENTA    TO DB-INVM-ID-CUENTA.
           MOVE INVM-ID-CLIENTE   TO DB-INVM-ID-CLIENTE.

      *    EXEC SQL
      *        SELECT SALDO_ACTUAL, ESTADO_CUENTA
      *        INTO   :DB-INVM-SALDO-ACTUAL, :DB-INVM-ESTADO-CUENTA
      *        FROM   ctactes
      *        WHERE  ID_CUENTA  = :DB-INVM-ID-CUENTA
      *        AND    ID_CLIENTE = :DB-INVM-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-INVM-SALDO-ACTUAL
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-INVM-ESTADO-CUENTA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(4)
               MOVE 5 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-INVM-ID-CUENTA TO SQL-VAR-0001
           MOVE DB-INVM-ID-CLIENTE TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF NOT LK-EXITO EXIT PARAGRAPH END-IF.

           IF DB-INVM-ESTADO-CUENTA NOT = 'A'
               MOVE 'E001' TO LK-COD-RETORNO
               MOVE 'Cuenta no activa para depositar'
                 TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF.

           COMPUTE DB-INVM-SALDO-ACTUAL =
               DB-INVM-SALDO-ACTUAL + WS-TX-IMPORTE.

      *    EXEC SQL
      *        UPDATE ctactes
      *        SET    SALDO_ACTUAL = :DB-INVM-SALDO-ACTUAL
      *        WHERE  ID_CUENTA    = :DB-INVM-ID-CUENTA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-7 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-INVM-SALDO-ACTUAL
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-7
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-INVM-ID-CUENTA
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF NOT LK-EXITO EXIT PARAGRAPH END-IF.

           MOVE DB-INVM-ID-CUENTA    TO DB-MOV-ID-CUENTA.
           MOVE 2                    TO DB-MOV-TIPO-MOV.
           MOVE WS-TX-IMPORTE        TO DB-MOV-IMPORTE.
           MOVE DB-INVM-SALDO-ACTUAL TO DB-MOV-SALDO-RES.
           MOVE LKCIF-TERMINAL       TO DB-MOV-TERMINAL.
           MOVE LKCIF-USUARIO        TO DB-MOV-USUARIO.

      *    EXEC SQL
      *        INSERT INTO movimientos (
      *            ID_CUENTA, TIPO_MOV, IMPORTE,
      *            SALDO_RESULTANTE, TERMINAL_ID, USUARIO_ID)
      *        VALUES (
      *            :DB-MOV-ID-CUENTA, :DB-MOV-TIPO-MOV,
      *            :DB-MOV-IMPORTE,   :DB-MOV-SALDO-RES,
      *            :DB-MOV-TERMINAL,  :DB-MOV-USUARIO)
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-8 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-MOV-IMPORTE
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 DB-MOV-SALDO-RES
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 DB-MOV-TERMINAL
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 4 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DB-MOV-USUARIO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-8
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-MOV-ID-CUENTA
             TO SQL-VAR-0003
           MOVE DB-MOV-TIPO-MOV
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-8
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.

           IF LK-EXITO
               MOVE DB-INVM-SALDO-ACTUAL TO INVM-SALDO-ACTUAL
               MOVE 'Deposito procesado' TO LK-MENSAJE
           END-IF.

      ************************************************************
      * R - Retiro: INVM-SALDO-ACTUAL contiene el monto
      ************************************************************
       8000-RETIRAR.
           MOVE INVM-SALDO-ACTUAL TO WS-TX-IMPORTE.
           MOVE INVM-ID-CUENTA    TO DB-INVM-ID-CUENTA.
           MOVE INVM-ID-CLIENTE   TO DB-INVM-ID-CLIENTE.

      *    EXEC SQL
      *        SELECT SALDO_ACTUAL, ESTADO_CUENTA, TIPO_CUENTA
      *        INTO   :DB-INVM-SALDO-ACTUAL, :DB-INVM-ESTADO-CUENTA,
      *               :DB-INVM-TIPO-CUENTA
      *        FROM   ctactes
      *        WHERE  ID_CUENTA  = :DB-INVM-ID-CUENTA
      *        AND    ID_CLIENTE = :DB-INVM-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-9 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-INVM-SALDO-ACTUAL
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-INVM-ESTADO-CUENTA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-INVM-TIPO-CUENTA
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 1 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(4)
               MOVE 5 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(5)
               MOVE 5 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               MOVE 5 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-9
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-INVM-ID-CUENTA TO SQL-VAR-0001
           MOVE DB-INVM-ID-CLIENTE TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-9
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF NOT LK-EXITO EXIT PARAGRAPH END-IF.

           IF DB-INVM-ESTADO-CUENTA NOT = 'A'
               MOVE 'E001' TO LK-COD-RETORNO
               MOVE 'Cuenta no activa para retirar'
                 TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF.

           IF DB-INVM-TIPO-CUENTA = 'H'
               MOVE 'E001' TO LK-COD-RETORNO
               MOVE 'Retiro no permitido en cuenta hipotecaria'
                 TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF.

           IF WS-TX-IMPORTE > DB-INVM-SALDO-ACTUAL
               MOVE 'E001' TO LK-COD-RETORNO
               MOVE 'Saldo insuficiente para el retiro'
                 TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF.

           COMPUTE DB-INVM-SALDO-ACTUAL =
               DB-INVM-SALDO-ACTUAL - WS-TX-IMPORTE.

      *    EXEC SQL
      *        UPDATE ctactes
      *        SET    SALDO_ACTUAL = :DB-INVM-SALDO-ACTUAL
      *        WHERE  ID_CUENTA    = :DB-INVM-ID-CUENTA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-10 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-INVM-SALDO-ACTUAL
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-10
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-INVM-ID-CUENTA
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-10
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF NOT LK-EXITO EXIT PARAGRAPH END-IF.

           MOVE DB-INVM-ID-CUENTA    TO DB-MOV-ID-CUENTA.
           MOVE 3                    TO DB-MOV-TIPO-MOV.
           MOVE WS-TX-IMPORTE        TO DB-MOV-IMPORTE.
           MOVE DB-INVM-SALDO-ACTUAL TO DB-MOV-SALDO-RES.
           MOVE LKCIF-TERMINAL       TO DB-MOV-TERMINAL.
           MOVE LKCIF-USUARIO        TO DB-MOV-USUARIO.

      *    EXEC SQL
      *        INSERT INTO movimientos (
      *            ID_CUENTA, TIPO_MOV, IMPORTE,
      *            SALDO_RESULTANTE, TERMINAL_ID, USUARIO_ID)
      *        VALUES (
      *            :DB-MOV-ID-CUENTA, :DB-MOV-TIPO-MOV,
      *            :DB-MOV-IMPORTE,   :DB-MOV-SALDO-RES,
      *            :DB-MOV-TERMINAL,  :DB-MOV-USUARIO)
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-11 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-MOV-IMPORTE
               MOVE '3' TO SQL-TYPE(3)
               MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 DB-MOV-SALDO-RES
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 DB-MOV-TERMINAL
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 4 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DB-MOV-USUARIO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-11
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-MOV-ID-CUENTA
             TO SQL-VAR-0003
           MOVE DB-MOV-TIPO-MOV
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-11
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.

           IF LK-EXITO
               MOVE DB-INVM-SALDO-ACTUAL TO INVM-SALDO-ACTUAL
               MOVE 'Retiro procesado' TO LK-MENSAJE
           END-IF.

      ************************************************************
      * Mover campos DB a REG-INVM
      ************************************************************
       9100-MOVER-A-REG.
           MOVE DB-INVM-ID-CUENTA      TO INVM-ID-CUENTA.
           MOVE DB-INVM-TIPO-CUENTA    TO INVM-TIPO-CUENTA.
           MOVE DB-INVM-SALDO-ACTUAL   TO INVM-SALDO-ACTUAL.
           MOVE DB-INVM-FECHA-APERTURA TO INVM-FECHA-APERTURA.
           MOVE DB-INVM-ESTADO-CUENTA  TO INVM-ESTADO-CUENTA.

      ************************************************************
      * Evaluador SQLSTATE centralizado
      ************************************************************
       9000-EVALUAR-SQLSTATE.
           EVALUATE SQLSTATE
               WHEN '00000'
                   MOVE '00  ' TO LK-COD-RETORNO
               WHEN '02000'
                   MOVE 'E404' TO LK-COD-RETORNO
                   MOVE 'Cuenta no encontrada' TO LK-MENSAJE
               WHEN '23000' THRU '23999'
                   MOVE 'E409' TO LK-COD-RETORNO
                   MOVE 'Ya tiene una cuenta de ese tipo'
                     TO LK-MENSAJE
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE 'Error tecnico en base de datos'
                     TO LK-MENSAJE
           END-EVALUATE.

       END PROGRAM DBIOINVM.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  CUR_CTAS                 IN USE CURSOR
      *  DB-INVM-ESTADO-CUENTA     IN USE CHAR(1)
      *  DB-INVM-FECHA-APERTURA     IN USE CHAR(10)
      *  DB-INVM-FECHA-HOY        IN USE CHAR(10)
      *  DB-INVM-ID-CLIENTE       IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(9,0)
      *  DB-INVM-ID-CUENTA        IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  DB-INVM-SALDO-ACTUAL     IN USE DECIMAL(15,2)
      *  DB-INVM-TIPO-CUENTA      IN USE CHAR(1)
      *  DB-MOV-ID-CUENTA         IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(9,0)
      *  DB-MOV-IMPORTE           IN USE DECIMAL(15,2)
      *  DB-MOV-SALDO-RES         IN USE DECIMAL(15,2)
      *  DB-MOV-TERMINAL          IN USE CHAR(4)
      *  DB-MOV-TIPO-MOV          IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(1,0)
      *  DB-MOV-USUARIO           IN USE CHAR(8)
      **********************************************************************

      ******************************************************************
      * DBIOTARJ.SQB - CAPA DE ACCESO A DATOS TARJETAS v3.0
      * TABLAS   : tarjetas, tarjetas_diferidos, clientes
      * ACCIONES : C=Consultar  A=Insert-Tarjeta  I=Insert-Diferido
      *            B=Cancelar
      ******************************************************************
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
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 135.
           05 SQL-STMT   PIC X(135) VALUE 'SELECT ID_DIFERIDO,MONTO_COMP
      -    'RA,CUOTAS_TOTALES,CUOTAS_PENDIENTES,VALOR_CUOTA,TASA_INTERES
      -    ' FROM tarjetas_diferidos WHERE NRO_TARJETA = ?'.
           05 SQL-CNAME  PIC X(8) VALUE 'CUR-DIFD'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 142.
           05 SQL-STMT   PIC X(142) VALUE 'SELECT NRO_TARJETA,CUENTA_PRI
      -    'NCIPAL,FECHA_EMISION,FECHA_VENCTO,CUPO_APROBADO,SALDO_UTILIZ
      -    'ADO,ESTADO_TARJETA FROM tarjetas WHERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 8.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 159.
           05 SQL-STMT   PIC X(159) VALUE 'INSERT INTO tarjetas (NRO_TAR
      -    'JETA,ID_CLIENTE,CUENTA_PRINCIPAL,FECHA_EMISION,FECHA_VENCTO,
      -    'CUPO_APROBADO,SALDO_UTILIZADO,ESTADO_TARJETA) VALUES (?,?,?,
      -    '?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 58.
           05 SQL-STMT   PIC X(58) VALUE 'UPDATE clientes SET TIENE_TARJ
      -    'ETA = 1 WHERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 6.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 136.
           05 SQL-STMT   PIC X(136) VALUE 'INSERT INTO tarjetas_diferido
      -    's (NRO_TARJETA,MONTO_COMPRA,CUOTAS_TOTALES,CUOTAS_PENDIENTES
      -    ',VALOR_CUOTA,TASA_INTERES) VALUES (?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 79.
           05 SQL-STMT   PIC X(79) VALUE 'UPDATE tarjetas SET SALDO_UTIL
      -    'IZADO = SALDO_UTILIZADO + ? WHERE NRO_TARJETA = ?'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 81.
           05 SQL-STMT   PIC X(81) VALUE 'UPDATE tarjetas SET ESTADO_TAR
      -    'JETA = ''I'' WHERE NRO_TARJETA = ? AND ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 58.
           05 SQL-STMT   PIC X(58) VALUE 'UPDATE clientes SET TIENE_TARJ
      -    'ETA = 0 WHERE ID_CLIENTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(9) COMP-3.
           05 SQL-VAR-0003  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0005  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0006  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0007  PIC S9(3) COMP-3.
           05 SQL-VAR-0008  PIC S9(3) COMP-3.
           05 SQL-VAR-0009  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0010  PIC S9(9) COMP-3.
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
       01  DB-TARJ-NRO-TARJETA     PIC X(16).
       01  DB-TARJ-ID-CLIENTE      PIC 9(08).
       01  DB-TARJ-CTA-PRINCIPAL   PIC 9(08).
       01  DB-TARJ-FECHA-EMISION   PIC X(10).
       01  DB-TARJ-FECHA-VENCTO    PIC X(10).
       01  DB-TARJ-CUPO-APROBADO   PIC S9(10)V99 COMP-3.
       01  DB-TARJ-SALDO-UTILIZADO PIC S9(10)V99 COMP-3.
       01  DB-TARJ-ESTADO          PIC X(01).
       01  DB-TARJ-SALDO-INC       PIC S9(10)V99 COMP-3.
       01  DB-DIFD-NRO-TARJETA     PIC X(16).
       01  DB-DIFD-MONTO-COMPRA    PIC S9(10)V99 COMP-3.
       01  DB-DIFD-CUOTAS-TOT      PIC 9(03).
       01  DB-DIFD-CUOTAS-PEND     PIC 9(03).
       01  DB-DIFD-VALOR-CUOTA     PIC S9(10)V99 COMP-3.
       01  DB-DIFD-TASA-INTERES    PIC S9(03)V99 COMP-3.
       01  DB-DIFD-ID-DIFERIDO     PIC 9(08).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

      *    EXEC SQL DECLARE CUR-DIFD CURSOR FOR
      *        SELECT ID_DIFERIDO,
      *               MONTO_COMPRA, CUOTAS_TOTALES,
      *               CUOTAS_PENDIENTES, VALOR_CUOTA,
      *               TASA_INTERES
      *        FROM   tarjetas_diferidos
      *        WHERE  NRO_TARJETA = :DB-TARJ-NRO-TARJETA
      *    END-EXEC.

       LINKAGE SECTION.
           COPY TARJREC.
           COPY LKCIF.

       PROCEDURE DIVISION USING REG-TARJ, REG-DIFD,
                                LK-DATOS-SESION,
                                LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE '00  ' TO LK-COD-RETORNO.
           MOVE SPACES TO LK-MENSAJE.

           EVALUATE LK-ACCION-DB
               WHEN 'C'
                   PERFORM 1000-CONSULTAR-TARJETA
               WHEN 'A'
                   PERFORM 2000-INSERTAR-TARJETA
               WHEN 'I'
                   PERFORM 3000-INSERTAR-DIFERIDO
               WHEN 'B'
                   PERFORM 4000-CANCELAR-TARJETA
               WHEN 'L'
                   PERFORM 5000-ABRIR-CURSOR-DIFD
               WHEN 'F'
                   PERFORM 6000-FETCH-DIFERIDO
               WHEN 'Z'
                   PERFORM 7000-CERRAR-CURSOR-DIFD
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE 'Accion no reconocida en DBIOTARJ'
                     TO LK-MENSAJE
           END-EVALUATE.
           GOBACK.

      ************************************************************
      * C - Consultar tarjeta por ID_CLIENTE (un cliente = una tarjeta)
      ************************************************************
       1000-CONSULTAR-TARJETA.
           MOVE TARJ-ID-CLIENTE TO DB-TARJ-ID-CLIENTE.

      *    EXEC SQL
      *        SELECT NRO_TARJETA, CUENTA_PRINCIPAL,
      *               FECHA_EMISION, FECHA_VENCTO,
      *               CUPO_APROBADO, SALDO_UTILIZADO,
      *               ESTADO_TARJETA
      *        INTO   :DB-TARJ-NRO-TARJETA, :DB-TARJ-CTA-PRINCIPAL,
      *               :DB-TARJ-FECHA-EMISION, :DB-TARJ-FECHA-VENCTO,
      *               :DB-TARJ-CUPO-APROBADO, :DB-TARJ-SALDO-UTILIZADO,
      *               :DB-TARJ-ESTADO
      *        FROM   tarjetas
      *        WHERE  ID_CLIENTE = :DB-TARJ-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 16 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-TARJ-FECHA-EMISION
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 DB-TARJ-FECHA-VENCTO
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
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
                 DB-TARJ-ESTADO
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 1 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(8)
               MOVE 5 TO SQL-LEN(8)
               MOVE X'00' TO SQL-PREC(8)
               MOVE 8 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-TARJ-ID-CLIENTE TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0002 TO DB-TARJ-CTA-PRINCIPAL
           MOVE SQL-VAR-0003 TO DB-TARJ-CUPO-APROBADO
           MOVE SQL-VAR-0004 TO DB-TARJ-SALDO-UTILIZADO
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF LK-EXITO
               PERFORM 9100-MOVER-A-REG-TARJ
               MOVE 'Tarjeta consultada correctamente' TO LK-MENSAJE
           END-IF.

      ************************************************************
      * A - Insertar tarjeta nueva + activar flag TIENE_TARJETA
      ************************************************************
       2000-INSERTAR-TARJETA.
           MOVE TARJ-NRO-TARJETA      TO DB-TARJ-NRO-TARJETA.
           MOVE TARJ-ID-CLIENTE       TO DB-TARJ-ID-CLIENTE.
           MOVE TARJ-CUENTA-PRINCIPAL TO DB-TARJ-CTA-PRINCIPAL.
           MOVE TARJ-FECHA-EMISION    TO DB-TARJ-FECHA-EMISION.
           MOVE TARJ-FECHA-VENCTO     TO DB-TARJ-FECHA-VENCTO.
           MOVE TARJ-CUPO-APROBADO    TO DB-TARJ-CUPO-APROBADO.
           MOVE ZEROS                 TO DB-TARJ-SALDO-UTILIZADO.
           MOVE TARJ-ESTADO           TO DB-TARJ-ESTADO.

      *    EXEC SQL
      *        INSERT INTO tarjetas
      *            (NRO_TARJETA, ID_CLIENTE, CUENTA_PRINCIPAL,
      *             FECHA_EMISION, FECHA_VENCTO, CUPO_APROBADO,
      *             SALDO_UTILIZADO, ESTADO_TARJETA)
      *        VALUES
      *            (:DB-TARJ-NRO-TARJETA, :DB-TARJ-ID-CLIENTE,
      *             :DB-TARJ-CTA-PRINCIPAL, :DB-TARJ-FECHA-EMISION,
      *             :DB-TARJ-FECHA-VENCTO, :DB-TARJ-CUPO-APROBADO,
      *             :DB-TARJ-SALDO-UTILIZADO, :DB-TARJ-ESTADO)
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 16 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 DB-TARJ-FECHA-EMISION
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 DB-TARJ-FECHA-VENCTO
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 10 TO SQL-LEN(5)
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
                 DB-TARJ-ESTADO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 1 TO SQL-LEN(8)
               MOVE 8 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-TARJ-ID-CLIENTE
             TO SQL-VAR-0001
           MOVE DB-TARJ-CTA-PRINCIPAL
             TO SQL-VAR-0002
           MOVE DB-TARJ-CUPO-APROBADO
             TO SQL-VAR-0003
           MOVE DB-TARJ-SALDO-UTILIZADO
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF NOT LK-EXITO EXIT PARAGRAPH END-IF.

      *    EXEC SQL
      *        UPDATE clientes SET TIENE_TARJETA = 1
      *        WHERE  ID_CLIENTE = :DB-TARJ-ID-CLIENTE
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
           MOVE DB-TARJ-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.

           IF LK-EXITO
               MOVE 'Tarjeta emitida correctamente' TO LK-MENSAJE
           END-IF.

      ************************************************************
      * I - Insertar diferido + sumar monto a SALDO_UTILIZADO
      ************************************************************
       3000-INSERTAR-DIFERIDO.
           MOVE DIFD-NRO-TARJETA       TO DB-DIFD-NRO-TARJETA.
           MOVE TARJ-NRO-TARJETA       TO DB-TARJ-NRO-TARJETA.
           MOVE DIFD-MONTO-COMPRA      TO DB-DIFD-MONTO-COMPRA.
           COMPUTE DB-TARJ-SALDO-INC =
               DIFD-VALOR-CUOTA * DIFD-CUOTAS-TOTALES.
           MOVE DIFD-CUOTAS-TOTALES    TO DB-DIFD-CUOTAS-TOT.
           MOVE DIFD-CUOTAS-PENDIENTES TO DB-DIFD-CUOTAS-PEND.
           MOVE DIFD-VALOR-CUOTA       TO DB-DIFD-VALOR-CUOTA.
           MOVE DIFD-TASA-INTERES      TO DB-DIFD-TASA-INTERES.

      *    EXEC SQL
      *        INSERT INTO tarjetas_diferidos
      *            (NRO_TARJETA, MONTO_COMPRA, CUOTAS_TOTALES,
      *             CUOTAS_PENDIENTES, VALOR_CUOTA, TASA_INTERES)
      *        VALUES
      *            (:DB-DIFD-NRO-TARJETA, :DB-DIFD-MONTO-COMPRA,
      *             :DB-DIFD-CUOTAS-TOT, :DB-DIFD-CUOTAS-PEND,
      *             :DB-DIFD-VALOR-CUOTA, :DB-DIFD-TASA-INTERES)
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-DIFD-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 16 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(2)
               MOVE 7 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(3)
               MOVE 2 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(4)
               MOVE 2 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DB-DIFD-TASA-INTERES
               MOVE '3' TO SQL-TYPE(6)
               MOVE 3 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-DIFD-MONTO-COMPRA
             TO SQL-VAR-0006
           MOVE DB-DIFD-CUOTAS-TOT
             TO SQL-VAR-0007
           MOVE DB-DIFD-CUOTAS-PEND
             TO SQL-VAR-0008
           MOVE DB-DIFD-VALOR-CUOTA
             TO SQL-VAR-0009
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF NOT LK-EXITO EXIT PARAGRAPH END-IF.

      *    EXEC SQL
      *        UPDATE tarjetas
      *        SET    SALDO_UTILIZADO = SALDO_UTILIZADO
      *                               + :DB-TARJ-SALDO-INC
      *        WHERE  NRO_TARJETA = :DB-DIFD-NRO-TARJETA
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 7 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-DIFD-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 16 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-TARJ-SALDO-INC
             TO SQL-VAR-0005
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.

           IF LK-EXITO
               MOVE 'Diferido registrado correctamente' TO LK-MENSAJE
           END-IF.

      ************************************************************
      * B - Cancelar tarjeta (estado I) + limpiar flag TIENE_TARJETA
      ************************************************************
       4000-CANCELAR-TARJETA.
           MOVE TARJ-NRO-TARJETA TO DB-TARJ-NRO-TARJETA.
           MOVE TARJ-ID-CLIENTE  TO DB-TARJ-ID-CLIENTE.

      *    EXEC SQL
      *        UPDATE tarjetas
      *        SET    ESTADO_TARJETA = 'I'
      *        WHERE  NRO_TARJETA = :DB-TARJ-NRO-TARJETA
      *        AND    ID_CLIENTE  = :DB-TARJ-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 16 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-TARJ-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF NOT LK-EXITO EXIT PARAGRAPH END-IF.

      *    EXEC SQL
      *        UPDATE clientes SET TIENE_TARJETA = 0
      *        WHERE  ID_CLIENTE = :DB-TARJ-ID-CLIENTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-7 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-7
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-TARJ-ID-CLIENTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.

           IF LK-EXITO
               MOVE 'Tarjeta cancelada correctamente' TO LK-MENSAJE
           END-IF.

      ************************************************************
      * Mover campos DB a REG-TARJ
      ************************************************************
       9100-MOVER-A-REG-TARJ.
           MOVE DB-TARJ-NRO-TARJETA     TO TARJ-NRO-TARJETA.
           MOVE DB-TARJ-CTA-PRINCIPAL   TO TARJ-CUENTA-PRINCIPAL.
           MOVE DB-TARJ-FECHA-EMISION   TO TARJ-FECHA-EMISION.
           MOVE DB-TARJ-FECHA-VENCTO    TO TARJ-FECHA-VENCTO.
           MOVE DB-TARJ-CUPO-APROBADO   TO TARJ-CUPO-APROBADO.
           MOVE DB-TARJ-SALDO-UTILIZADO TO TARJ-SALDO-UTILIZADO.
           MOVE DB-TARJ-ESTADO          TO TARJ-ESTADO.

      ************************************************************
      * Evaluador SQLSTATE centralizado
      ************************************************************
       9000-EVALUAR-SQLSTATE.
           EVALUATE SQLSTATE
               WHEN '00000'
                   MOVE '00  ' TO LK-COD-RETORNO
               WHEN '02000'
                   MOVE 'E404' TO LK-COD-RETORNO
                   MOVE 'Tarjeta no encontrada' TO LK-MENSAJE
               WHEN '23000' THRU '23999'
                   MOVE 'E409' TO LK-COD-RETORNO
                   MOVE 'El cliente ya tiene una tarjeta activa'
                     TO LK-MENSAJE
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE 'Error tecnico en base de datos'
                     TO LK-MENSAJE
           END-EVALUATE.

      ************************************************************
      * L - Abrir cursor de diferidos para NRO_TARJETA
      ************************************************************
       5000-ABRIR-CURSOR-DIFD.
           MOVE TARJ-NRO-TARJETA TO DB-TARJ-NRO-TARJETA.
      *    EXEC SQL OPEN CUR-DIFD END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-TARJ-NRO-TARJETA
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 16 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-0
                               SQLCA
           END-CALL
                                          .
           PERFORM 9000-EVALUAR-SQLSTATE.

      ************************************************************
      * F - Fetch siguiente diferido activo
      ************************************************************
       6000-FETCH-DIFERIDO.
      *    EXEC SQL
      *        FETCH CUR-DIFD
      *        INTO  :DB-DIFD-ID-DIFERIDO,
      *              :DB-DIFD-MONTO-COMPRA,
      *              :DB-DIFD-CUOTAS-TOT,
      *              :DB-DIFD-CUOTAS-PEND,
      *              :DB-DIFD-VALOR-CUOTA,
      *              :DB-DIFD-TASA-INTERES
      *    END-EXEC.
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0010
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             SQL-VAR-0006
           MOVE '3' TO SQL-TYPE(2)
           MOVE 7 TO SQL-LEN(2)
               MOVE X'02' TO SQL-PREC(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0007
           MOVE '3' TO SQL-TYPE(3)
           MOVE 2 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0008
           MOVE '3' TO SQL-TYPE(4)
           MOVE 2 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0009
           MOVE '3' TO SQL-TYPE(5)
           MOVE 7 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-DIFD-TASA-INTERES
           MOVE '3' TO SQL-TYPE(6)
           MOVE 3 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
           MOVE 6 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0010 TO DB-DIFD-ID-DIFERIDO
           MOVE SQL-VAR-0006 TO DB-DIFD-MONTO-COMPRA
           MOVE SQL-VAR-0007 TO DB-DIFD-CUOTAS-TOT
           MOVE SQL-VAR-0008 TO DB-DIFD-CUOTAS-PEND
           MOVE SQL-VAR-0009 TO DB-DIFD-VALOR-CUOTA
                   .
           PERFORM 9000-EVALUAR-SQLSTATE.
           IF LK-EXITO
               MOVE DB-DIFD-ID-DIFERIDO    TO DIFD-ID-DIFERIDO
               MOVE DB-DIFD-MONTO-COMPRA   TO DIFD-MONTO-COMPRA
               MOVE DB-DIFD-CUOTAS-TOT     TO DIFD-CUOTAS-TOTALES
               MOVE DB-DIFD-CUOTAS-PEND    TO DIFD-CUOTAS-PENDIENTES
               MOVE DB-DIFD-VALOR-CUOTA    TO DIFD-VALOR-CUOTA
               MOVE DB-DIFD-TASA-INTERES   TO DIFD-TASA-INTERES
           END-IF.

      ************************************************************
      * Z - Cerrar cursor de diferidos
      ************************************************************
       7000-CERRAR-CURSOR-DIFD.
      *    EXEC SQL CLOSE CUR-DIFD END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA
                                           .
           MOVE '00  ' TO LK-COD-RETORNO.

       END PROGRAM DBIOTARJ.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  CUR-DIFD                 IN USE CURSOR
      *  DB-DIFD-CUOTAS-PEND      IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(3,0)
      *  DB-DIFD-CUOTAS-TOT       IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(3,0)
      *  DB-DIFD-ID-DIFERIDO      IN USE THROUGH TEMP VAR SQL-VAR-0010 DECIMAL(9,0)
      *  DB-DIFD-MONTO-COMPRA     IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(13,2)
      *  DB-DIFD-NRO-TARJETA      IN USE CHAR(16)
      *  DB-DIFD-TASA-INTERES     IN USE DECIMAL(5,2)
      *  DB-DIFD-VALOR-CUOTA      IN USE THROUGH TEMP VAR SQL-VAR-0009 DECIMAL(13,2)
      *  DB-TARJ-CTA-PRINCIPAL     IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(9,0)
      *  DB-TARJ-CUPO-APROBADO     IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(13,2)
      *  DB-TARJ-ESTADO           IN USE CHAR(1)
      *  DB-TARJ-FECHA-EMISION     IN USE CHAR(10)
      *  DB-TARJ-FECHA-VENCTO     IN USE CHAR(10)
      *  DB-TARJ-ID-CLIENTE       IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  DB-TARJ-NRO-TARJETA      IN USE CHAR(16)
      *  DB-TARJ-SALDO-INC        IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(13,2)
      *  DB-TARJ-SALDO-UTILIZADO     IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(13,2)
      **********************************************************************

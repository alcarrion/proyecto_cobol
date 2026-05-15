      *================================================================*
      * PROGRAMA: DBIOMOVS.cob                                         *
      * FUNCION:  INSERT en tabla movimientos - BANCO LAF v2.0          *
      * NIVEL:    1000 TCS ECUADOR                                      *
      * ARQUITECTURA: Capa de Acceso a Datos (DBIO)                    *
      * ACCIONES:                                                       *
      *   A = INSERT movimientos (auditoria de cada operacion de saldo) *
      * NOTA: Sin COMMIT propio. El mainline gestiona la transaccion.   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBIOMOVS.

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
      * STMT-0: INSERT INTO movimientos
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 6.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 155.
           05 SQL-STMT   PIC X(155) VALUE 'INSERT INTO movimientos (ID_C
      -    'UENTA,TIPO_MOV,IMPORTE,SALDO_RESULTANTE,TERMINAL_ID,USUARIO_
      -    'ID) VALUES (?,?,?,?,?,?)'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(2) COMP-3.
           05 SQL-VAR-0003  PIC S9(9)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(9)V9(2) COMP-3.
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
       01  REG-MOVS.
           05 MOVS-ID-MOVIMIENTO       PIC 9(09).
           05 MOVS-ID-CUENTA           PIC 9(09).
           05 MOVS-TIPO-MOV            PIC 9(02).
           05 MOVS-IMPORTE             PIC S9(15)V99.
           05 MOVS-SALDO-RESULTANTE    PIC S9(15)V99.
           05 MOVS-TERMINAL-ID         PIC X(04).
           05 MOVS-USUARIO-ID          PIC X(08).

           COPY LKCIF.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION USING REG-MOVS, LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE '00  ' TO LK-COD-RETORNO
           MOVE SPACES TO LK-MENSAJE

           EVALUATE LK-ACCION-DB
               WHEN 'A'
                   PERFORM 1000-INSERTAR-MOVIMIENTO
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE "ACCION DB NO SOPORTADA" TO LK-MENSAJE
           END-EVALUATE.

           EXIT PROGRAM.

       1000-INSERTAR-MOVIMIENTO.
      *    EXEC SQL
      *        INSERT INTO movimientos (
      *            ID_CUENTA, TIPO_MOV, IMPORTE,
      *            SALDO_RESULTANTE, TERMINAL_ID, USUARIO_ID
      *        ) VALUES (?,?,?,?,?,?)
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
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(3)
               MOVE 9 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(4)
               MOVE 9 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 MOVS-TERMINAL-ID
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 4 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 MOVS-USUARIO-ID
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE MOVS-ID-CUENTA
             TO SQL-VAR-0001
           MOVE MOVS-TIPO-MOV
             TO SQL-VAR-0002
           MOVE MOVS-IMPORTE
             TO SQL-VAR-0003
           MOVE MOVS-SALDO-RESULTANTE
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .
           PERFORM 9000-EVALUAR-SQL.
           IF LK-COD-RETORNO = '00  '
               MOVE "MOVIMIENTO REGISTRADO" TO LK-MENSAJE
           END-IF.

       9000-EVALUAR-SQL.
           EVALUATE TRUE
               WHEN SQL-SUCCESS
                   MOVE '00  ' TO LK-COD-RETORNO
                   IF LK-MENSAJE = SPACES
                       MOVE "OPERACION EXITOSA" TO LK-MENSAJE
                   END-IF
               WHEN OTHER
                   MOVE 'E999' TO LK-COD-RETORNO
                   MOVE "ERROR TECNICO EN DB MOVIMIENTOS"
                       TO LK-MENSAJE
           END-EVALUATE.
      **********************************************************************
      *  BANCO LAF v2.0 - NIVEL 1000 TCS ECUADOR
      *  DBIOMOVS: Capa DBIO movimientos - schema v2.0
      *  Accion: A (INSERT auditado)
      **********************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFMX.
      *==========================================================
      * FASE 10: INGESTA DINÁMICA A RÉPLICAS CORREGIDA (TF01 - TF06)
      * Soporta parseo adaptativo para Cuentas (3 col) y Créditos (4 co
      * CORRECCIÓN: Reseteo de bandera WS-EOF y parseo numérico seguro
      *==========================================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-ENTRADA ASSIGN TO WS-RUTA-COMPLETA
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO-ENTRADA.
       01  REG-LINEA-ENTRADA           PIC X(500).

       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 7.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 7 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 7 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 7 TIMES.
           05 SQL-PREC   PIC X OCCURS 7 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 63.
           05 SQL-STMT   PIC X(63) VALUE 'SELECT VALOR FROM TF_PARAMETRO
      -    'S WHERE PARAMETRO = ''RUTA_UPLOAD'''.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 7.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 101.
           05 SQL-STMT   PIC X(101) VALUE 'INSERT INTO TF01 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO) VALUES (?,
      -    '?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 7.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 101.
           05 SQL-STMT   PIC X(101) VALUE 'INSERT INTO TF02 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO) VALUES (?,
      -    '?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 7.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 101.
           05 SQL-STMT   PIC X(101) VALUE 'INSERT INTO TF03 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO) VALUES (?,
      -    '?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 7.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 101.
           05 SQL-STMT   PIC X(101) VALUE 'INSERT INTO TF04 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO) VALUES (?,
      -    '?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 7.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 101.
           05 SQL-STMT   PIC X(101) VALUE 'INSERT INTO TF05 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO) VALUES (?,
      -    '?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 7.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 101.
           05 SQL-STMT   PIC X(101) VALUE 'INSERT INTO TF06 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO) VALUES (?,
      -    '?,?,?,?,?,?)'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(1) COMP-3.
           05 SQL-VAR-0003  PIC S9(13) COMP-3.
           05 SQL-VAR-0004  PIC S9(13)V9(2) COMP-3.
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
       01  WS-HOST-VARS-TFMX.
           05 WS-ID-LOTE-SQL       PIC 9(09).
           05 WS-DATOS-TX-SQL      PIC X(500).
           05 WS-ESTADO-SQL        PIC 9(01) VALUE 2.
           05 WS-REPLICA-SQL       PIC X(04).
           05 WS-CUENTA-SQL        PIC 9(12).
           05 WS-CREDITO-SQL       PIC X(20).
           05 WS-MONTO-SQL        PIC 9(13)V99.
           05 WS-OP-SQL            PIC X(03).
           05 WS-DIR-BASE-SQL      PIC X(120).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-CONTROL-FILE.
           05 WS-RUTA-COMPLETA     PIC X(200).
           05 WS-EOF               PIC X(01) VALUE 'N'.
           05 WS-CONT-REGS         PIC 9(09) VALUE 0.
           05 WS-FS                PIC X(02).

      * Variable temporal para evitar desborde de decimales implícitos
       01  WS-MONTO-TEMP           PIC 9(15) VALUE 0.

       LINKAGE SECTION.
           COPY LKTF.
       01  WS-TFFM-VARS.
           05 WS-ID-LOTE           PIC 9(09).
           05 WS-FILE-NAME         PIC X(120).
           05 WS-FASE              PIC X(02).
           05 WS-TYPE-UPDATE       PIC X(10).
           05 WS-REPLICA-ASIG      PIC X(04).
           05 WS-RETRY-COUNT       PIC 9(02).

       PROCEDURE DIVISION USING WS-TFFM-VARS,
       LK-TRICKLE-FEED-INTERFACE.
       0000-PRINCIPAL.
      * 1. Preparación de Entorno
               INITIALIZE WS-HOST-VARS-TFMX, WS-RUTA-COMPLETA


               MOVE 'N' TO WS-EOF
               MOVE 0   TO WS-CONT-REGS

               MOVE 2 TO WS-ESTADO-SQL
      *        EXEC SQL
      *            SELECT VALOR INTO :WS-DIR-BASE-SQL
      *            FROM TF_PARAMETROS
      *            WHERE PARAMETRO = 'RUTA_UPLOAD'
      *        END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-DIR-BASE-SQL
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 120 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                       .

               IF SQLCODE NOT = 0
                   MOVE "C:\banco\spool\Interfaces\BATCH-UPLOAD-S\"
                     TO WS-DIR-BASE-SQL
               END-IF.

               INSPECT WS-DIR-BASE-SQL REPLACING TRAILING
                    SPACES BY LOW-VALUES.

               STRING WS-DIR-BASE-SQL    DELIMITED BY LOW-VALUES
                      WS-FILE-NAME        DELIMITED BY SPACE
                      INTO WS-RUTA-COMPLETA.

               DISPLAY "TFMX - REPLICA DESTINO: " WS-REPLICA-ASIG
               DISPLAY "TFMX - CARGANDO DESDE : " WS-RUTA-COMPLETA

      * 2. Apertura de Archivo Troceado
               OPEN INPUT ARCHIVO-ENTRADA
               IF WS-FS NOT = "00"
                   DISPLAY "ERROR: NO SE ENCONTRO ARCHIVO "
                    WS-RUTA-COMPLETA
                   MOVE 99 TO LK-TF-COD-RETORNO
                   GOBACK
               END-IF

      * 3. Proceso de Carga Masiva
               MOVE WS-ID-LOTE TO WS-ID-LOTE-SQL
               MOVE WS-REPLICA-ASIG TO WS-REPLICA-SQL

               READ ARCHIVO-ENTRADA
                    AT END MOVE 'Y' TO WS-EOF
               END-READ

               PERFORM 1000-PROCESAR-LINEA UNTIL WS-EOF = 'Y'

      * 4. Cierre y Confirmación
               CLOSE ARCHIVO-ENTRADA
      *        EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL

               DISPLAY "TFMX - CARGA FINALIZADA. TOTAL: " WS-CONT-REGS
               MOVE 0 TO LK-TF-COD-RETORNO
               GOBACK.

           1000-PROCESAR-LINEA.
               MOVE REG-LINEA-ENTRADA TO WS-DATOS-TX-SQL
               INITIALIZE WS-MONTO-TEMP

      * PARSEO INTELIGENTE: Evaluamos la cabecera antes del desempaque
               IF REG-LINEA-ENTRADA(1:3) = "PAG"
      * Formato Créditos: TIPO|CUENTA|NUM_CREDITO|MONTO (4 campos)
                   UNSTRING REG-LINEA-ENTRADA DELIMITED BY "|"
                   INTO WS-OP-SQL, WS-CUENTA-SQL, WS-CREDITO-SQL,
                        WS-MONTO-TEMP
               ELSE
      * Formato Cuentas: TIPO|NUM_CUENTA|MONTO (3 campos)
                   MOVE SPACES TO WS-CREDITO-SQL
                   UNSTRING REG-LINEA-ENTRADA DELIMITED BY "|"
                   INTO WS-OP-SQL, WS-CUENTA-SQL, WS-MONTO-TEMP
               END-IF

      * CORRECCIÓN: Asignación con escala decimal real dividiendo para
               COMPUTE WS-MONTO-SQL = WS-MONTO-TEMP / 100

      * Ejecutar la inserción en la réplica asignada
               PERFORM 2000-INSERTAR-DINAMICO

               READ ARCHIVO-ENTRADA
                    AT END MOVE 'Y' TO WS-EOF
               END-READ.

           2000-INSERTAR-DINAMICO.
               EVALUATE WS-REPLICA-ASIG
                   WHEN "TF01"
      *                EXEC SQL
      *                    INSERT INTO TF01 (ID_LOTE, ESTADO, DATOS_TX,
      *                    REPLICA_NO, CUENTA, NUM_CREDITO, MONTO)
      *                    VALUES (:WS-ID-LOTE-SQL, :WS-ESTADO-SQL,
      *                    :WS-DATOS-TX-SQL, :WS-REPLICA-SQL,
      *                    :WS-CUENTA-SQL, :WS-CREDITO-SQL,
      *                    :WS-MONTO-SQL)
      *                END-EXEC
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-DATOS-TX-SQL
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 500 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WS-REPLICA-SQL
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 4 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 WS-CREDITO-SQL
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               MOVE 7 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-CUENTA-SQL
             TO SQL-VAR-0003
           MOVE WS-MONTO-SQL
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
                   WHEN "TF02"
      *                EXEC SQL
      *                    INSERT INTO TF02 (ID_LOTE, ESTADO, DATOS_TX,
      *                    REPLICA_NO, CUENTA, NUM_CREDITO, MONTO)
      *                    VALUES (:WS-ID-LOTE-SQL, :WS-ESTADO-SQL,
      *                    :WS-DATOS-TX-SQL, :WS-REPLICA-SQL,
      *                    :WS-CUENTA-SQL, :WS-CREDITO-SQL,
      *                    :WS-MONTO-SQL)
      *                END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-DATOS-TX-SQL
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 500 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WS-REPLICA-SQL
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 4 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 WS-CREDITO-SQL
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               MOVE 7 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-CUENTA-SQL
             TO SQL-VAR-0003
           MOVE WS-MONTO-SQL
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   WHEN "TF03"
      *                EXEC SQL
      *                    INSERT INTO TF03 (ID_LOTE, ESTADO, DATOS_TX,
      *                    REPLICA_NO, CUENTA, NUM_CREDITO, MONTO)
      *                    VALUES (:WS-ID-LOTE-SQL, :WS-ESTADO-SQL,
      *                    :WS-DATOS-TX-SQL, :WS-REPLICA-SQL,
      *                    :WS-CUENTA-SQL, :WS-CREDITO-SQL,
      *                     :WS-MONTO-SQL)
      *                END-EXEC
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-DATOS-TX-SQL
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 500 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WS-REPLICA-SQL
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 4 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 WS-CREDITO-SQL
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               MOVE 7 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-CUENTA-SQL
             TO SQL-VAR-0003
           MOVE WS-MONTO-SQL
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
                   WHEN "TF04"
      *                EXEC SQL
      *                    INSERT INTO TF04 (ID_LOTE, ESTADO, DATOS_TX,
      *                    REPLICA_NO, CUENTA, NUM_CREDITO, MONTO)
      *                    VALUES (:WS-ID-LOTE-SQL, :WS-ESTADO-SQL,
      *                    :WS-DATOS-TX-SQL, :WS-REPLICA-SQL,
      *                    :WS-CUENTA-SQL, :WS-CREDITO-SQL,
      *                    :WS-MONTO-SQL)
      *                END-EXEC
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-DATOS-TX-SQL
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 500 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WS-REPLICA-SQL
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 4 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 WS-CREDITO-SQL
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               MOVE 7 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-CUENTA-SQL
             TO SQL-VAR-0003
           MOVE WS-MONTO-SQL
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
                   WHEN "TF05"
      *                EXEC SQL
      *                    INSERT INTO TF05 (ID_LOTE, ESTADO, DATOS_TX,
      *                    REPLICA_NO, CUENTA, NUM_CREDITO, MONTO)
      *                    VALUES (:WS-ID-LOTE-SQL, :WS-ESTADO-SQL,
      *                    :WS-DATOS-TX-SQL, :WS-REPLICA-SQL,
      *                    :WS-CUENTA-SQL, :WS-CREDITO-SQL,
      *                    :WS-MONTO-SQL)
      *                END-EXEC
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-DATOS-TX-SQL
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 500 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WS-REPLICA-SQL
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 4 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 WS-CREDITO-SQL
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               MOVE 7 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-CUENTA-SQL
             TO SQL-VAR-0003
           MOVE WS-MONTO-SQL
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
                   WHEN "TF06"
      *                EXEC SQL
      *                    INSERT INTO TF06 (ID_LOTE, ESTADO, DATOS_TX,
      *                    REPLICA_NO, CUENTA, NUM_CREDITO, MONTO)
      *                    VALUES (:WS-ID-LOTE-SQL, :WS-ESTADO-SQL,
      *                    :WS-DATOS-TX-SQL, :WS-REPLICA-SQL,
      *                    :WS-CUENTA-SQL, :WS-CREDITO-SQL,
      *                    :WS-MONTO-SQL)
      *                END-EXEC
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-DATOS-TX-SQL
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 500 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WS-REPLICA-SQL
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 4 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 WS-CREDITO-SQL
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               MOVE 7 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-CUENTA-SQL
             TO SQL-VAR-0003
           MOVE WS-MONTO-SQL
             TO SQL-VAR-0004
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA
                   WHEN OTHER
                       DISPLAY "ERROR: REPLICA NO CONFIGURADA: "
                       WS-REPLICA-ASIG
                       MOVE 99 TO LK-TF-COD-RETORNO
                       MOVE "Y" TO WS-EOF
               END-EVALUATE.

               IF SQLCODE NOT = 0
                   DISPLAY "FALLO INSERT EN REPLICA " WS-REPLICA-ASIG
                   DISPLAY "SQLCODE: " SQLCODE
                   MOVE 99 TO LK-TF-COD-RETORNO
                   MOVE "Y" TO WS-EOF
               ELSE
                   ADD 1 TO WS-CONT-REGS
               END-IF.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-CREDITO-SQL           IN USE CHAR(20)
      *  WS-CUENTA-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(13,0)
      *  WS-DATOS-TX-SQL          IN USE CHAR(500)
      *  WS-DIR-BASE-SQL          IN USE CHAR(120)
      *  WS-ESTADO-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(1,0)
      *  WS-HOST-VARS-TFMX    NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-CREDITO-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-CUENTA-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-DATOS-TX-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-DIR-BASE-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-ESTADO-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-ID-LOTE-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-MONTO-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-OP-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-REPLICA-SQL NOT IN USE
      *  WS-ID-LOTE-SQL           IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  WS-MONTO-SQL             IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(15,2)
      *  WS-OP-SQL            NOT IN USE
      *  WS-REPLICA-SQL           IN USE CHAR(4)
      **********************************************************************

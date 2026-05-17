       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFMX.

      *==========================================================*
      * FASE 00: INGESTA DINÁMICA A RÉPLICAS
      * FORMATO POSICIONAL
      *
      * OPTIMIZACIÓN:
      * Ajuste estricto de longitud de hash para
      * ID_TRANSACCION (40 chars)
      *==========================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT ARCHIVO-ENTRADA
               ASSIGN       TO WS-RUTA-COMPLETA
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS WS-FS.

       DATA DIVISION.

       FILE SECTION.

       FD  ARCHIVO-ENTRADA.
       01  REG-LINEA-ENTRADA         PIC X(500).

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
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 63.
           05 SQL-STMT   PIC X(63) VALUE 'SELECT VALOR FROM TF_PARAMETRO
      -    'S WHERE PARAMETRO = ''RUTA_UPLOAD'''.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 10.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 143.
           05 SQL-STMT   PIC X(143) VALUE 'INSERT INTO TF01 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO,TRACE_ID,ID
      -    '_TRANSACCION,TYPE_UPDATE) VALUES (?,?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 10.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 143.
           05 SQL-STMT   PIC X(143) VALUE 'INSERT INTO TF02 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO,TRACE_ID,ID
      -    '_TRANSACCION,TYPE_UPDATE) VALUES (?,?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 10.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 143.
           05 SQL-STMT   PIC X(143) VALUE 'INSERT INTO TF03 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO,TRACE_ID,ID
      -    '_TRANSACCION,TYPE_UPDATE) VALUES (?,?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 10.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 143.
           05 SQL-STMT   PIC X(143) VALUE 'INSERT INTO TF04 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO,TRACE_ID,ID
      -    '_TRANSACCION,TYPE_UPDATE) VALUES (?,?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 10.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 143.
           05 SQL-STMT   PIC X(143) VALUE 'INSERT INTO TF05 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO,TRACE_ID,ID
      -    '_TRANSACCION,TYPE_UPDATE) VALUES (?,?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 10.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 143.
           05 SQL-STMT   PIC X(143) VALUE 'INSERT INTO TF06 (ID_LOTE,EST
      -    'ADO,DATOS_TX,REPLICA_NO,CUENTA,NUM_CREDITO,MONTO,TRACE_ID,ID
      -    '_TRANSACCION,TYPE_UPDATE) VALUES (?,?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(1) COMP-3.
           05 SQL-VAR-0003  PIC S9(13)V9(2) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************

      *    EXEC SQL
      *        INCLUDE SQLCA
      *    END-EXEC.
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

      *    EXEC SQL
      *        BEGIN DECLARE SECTION
      *    END-EXEC.

       01  WS-HOST-VARS-TFMX.
           05 WS-ID-LOTE-SQL         PIC 9(09).
           05 WS-DATOS-TX-SQL        PIC X(500).
           05 WS-ESTADO-SQL          PIC 9(01) VALUE 2.
           05 WS-REPLICA-SQL         PIC X(04).
           05 WS-TIPO-TX-SQL         PIC X(03).
           05 WS-TRACE-ID-SQL        PIC X(40).
           05 WS-DIR-BASE-SQL        PIC X(120).

      * VARIABLES HOSTEADAS GENÉRICAS
      * PARA EMPAREJAR CON LA BD REAL
           05 DB-INS-CUENTA          PIC X(16).
           05 DB-INS-NUM-CREDITO     PIC X(20).
           05 DB-INS-MONTO           PIC S9(13)V99.
           05 DB-INS-TYPE-UPDATE     PIC X(10).

      * CORRECCIÓN:
      * Variable de 40 bytes exactos
      * para evitar SQL Error 1406
           05 DB-INS-TXID            PIC X(40).

      *    EXEC SQL
      *        END DECLARE SECTION
      *    END-EXEC.

       01  WS-CONTROL-FILE.
           05 WS-RUTA-COMPLETA       PIC X(200).
           05 WS-EOF                 PIC X(01) VALUE 'N'.
           05 WS-CONT-REGS           PIC 9(09) VALUE 0.
           05 WS-FS                  PIC X(02).
           05 WS-TIPO-ARCHIVO        PIC X(03) VALUE SPACES.

       01  WS-HASH-COMPLETO          PIC X(64).

       LINKAGE SECTION.

           COPY LKTF.

       01  WS-TFFM-VARS.
           05 WS-ID-LOTE             PIC 9(09).
           05 WS-FILE-NAME           PIC X(120).
           05 WS-FASE                PIC X(02).
           05 WS-TYPE-UPDATE         PIC X(10).
           05 WS-REPLICA-ASIG        PIC X(04).
           05 WS-RETRY-COUNT         PIC 9(02).

       PROCEDURE DIVISION
           USING WS-TFFM-VARS,
                 LK-TRICKLE-FEED-INTERFACE.

      *==========================================================*
      * 0000 - PRINCIPAL
      *==========================================================*
       0000-PRINCIPAL.

           INITIALIZE
               WS-HOST-VARS-TFMX
               WS-RUTA-COMPLETA

           MOVE 'N' TO WS-EOF
           MOVE 0   TO WS-CONT-REGS

           MOVE WS-FILE-NAME(1:3)
             TO WS-TIPO-ARCHIVO

           MOVE WS-FILE-NAME(1:3)
             TO WS-TIPO-TX-SQL

           MOVE WS-TYPE-UPDATE
             TO DB-INS-TYPE-UPDATE

           MOVE 2
             TO WS-ESTADO-SQL

      *    EXEC SQL
      *        SELECT VALOR
      *          INTO :WS-DIR-BASE-SQL
      *          FROM TF_PARAMETROS
      *         WHERE PARAMETRO = 'RUTA_UPLOAD'
      *    END-EXEC
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

           IF SQLCODE NOT = 0
               MOVE
               "C:\banco\spool\Interfaces\BATCH-UPLOAD-S\"
                 TO WS-DIR-BASE-SQL
           END-IF

           INSPECT WS-DIR-BASE-SQL
               REPLACING TRAILING SPACES BY LOW-VALUES

           STRING
               WS-DIR-BASE-SQL DELIMITED BY LOW-VALUES
               WS-FILE-NAME    DELIMITED BY SPACE
               INTO WS-RUTA-COMPLETA
           END-STRING

           DISPLAY "TFMX - TIPO TRANSACCION: "
                   WS-TIPO-ARCHIVO

           DISPLAY "TFMX - REPLICA DESTINO: "
                   WS-REPLICA-ASIG

           DISPLAY "TFMX - CARGANDO DESDE : "
                   WS-RUTA-COMPLETA

           OPEN INPUT ARCHIVO-ENTRADA

           IF WS-FS NOT = "00"
               DISPLAY "ERROR: NO SE ENCONTRO ARCHIVO "
                       WS-RUTA-COMPLETA

               MOVE 99 TO LK-TF-COD-RETORNO
               GOBACK
           END-IF

           MOVE WS-ID-LOTE
             TO WS-ID-LOTE-SQL

           MOVE WS-REPLICA-ASIG
             TO WS-REPLICA-SQL

           READ ARCHIVO-ENTRADA
               AT END
                   MOVE 'Y' TO WS-EOF
           END-READ

           PERFORM 1000-PROCESAR-LINEA
               UNTIL WS-EOF = 'Y'

           CLOSE ARCHIVO-ENTRADA

      *    EXEC SQL
      *        COMMIT
      *    END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL

           DISPLAY "TFMX - TIPO: "
                   WS-TIPO-ARCHIVO
                   " - CARGA FINALIZADA. TOTAL: "
                   WS-CONT-REGS

           MOVE 0 TO LK-TF-COD-RETORNO

           GOBACK.

      *==========================================================*
      * 1000 - PROCESAR LINEA
      *==========================================================*
       1000-PROCESAR-LINEA.

           MOVE REG-LINEA-ENTRADA
             TO WS-DATOS-TX-SQL

           INITIALIZE
               DB-INS-CUENTA
               DB-INS-NUM-CREDITO
               DB-INS-MONTO
               WS-TRACE-ID-SQL
               WS-HASH-COMPLETO
               DB-INS-TXID

           EVALUATE WS-TIPO-ARCHIVO

               WHEN "DEP"
               WHEN "RET"
                   PERFORM 2100-PARSEAR-DEP-RET

               WHEN "TAR"
                   PERFORM 2200-PARSEAR-TAR

               WHEN "HIP"
                   PERFORM 2300-PARSEAR-HIP

               WHEN "IMP"
                   PERFORM 2400-PARSEAR-IMP

           END-EVALUATE

      * Ajuste atómico de seguridad:
      * Tomamos los primeros 40 bytes de la firma
           MOVE WS-HASH-COMPLETO(1:40)
             TO DB-INS-TXID

           PERFORM 3000-INSERTAR-EN-REPLICA

           READ ARCHIVO-ENTRADA
               AT END
                   MOVE 'Y' TO WS-EOF
           END-READ.

      *==========================================================*
      * 2100 - PARSEAR DEP / RET
      *==========================================================*
       2100-PARSEAR-DEP-RET.

           MOVE REG-LINEA-ENTRADA(1:10)
             TO DB-INS-CUENTA

           MOVE REG-LINEA-ENTRADA(11:3)
             TO DB-INS-NUM-CREDITO

           COMPUTE DB-INS-MONTO =
               FUNCTION NUMVAL(
                   REG-LINEA-ENTRADA(14:15)
               ) / 100

           MOVE REG-LINEA-ENTRADA(29:40)
             TO WS-TRACE-ID-SQL

           MOVE REG-LINEA-ENTRADA(73:64)
             TO WS-HASH-COMPLETO.

      *==========================================================*
      * 2200 - PARSEAR TAR
      *==========================================================*
       2200-PARSEAR-TAR.

           MOVE REG-LINEA-ENTRADA(1:16)
             TO DB-INS-CUENTA

           MOVE REG-LINEA-ENTRADA(32:3)
             TO DB-INS-NUM-CREDITO

           COMPUTE DB-INS-MONTO =
               FUNCTION NUMVAL(
                   REG-LINEA-ENTRADA(17:15)
               ) / 100

           MOVE REG-LINEA-ENTRADA(64:40)
             TO WS-TRACE-ID-SQL

           MOVE REG-LINEA-ENTRADA(104:64)
             TO WS-HASH-COMPLETO.

      *==========================================================*
      * 2300 - PARSEAR HIP
      *==========================================================*
       2300-PARSEAR-HIP.

           MOVE REG-LINEA-ENTRADA(1:10)
             TO DB-INS-CUENTA

           MOVE REG-LINEA-ENTRADA(38:2)
             TO DB-INS-NUM-CREDITO

           COMPUTE DB-INS-MONTO =
               FUNCTION NUMVAL(
                   REG-LINEA-ENTRADA(11:15)
               ) / 100

           MOVE REG-LINEA-ENTRADA(40:40)
             TO WS-TRACE-ID-SQL

           MOVE REG-LINEA-ENTRADA(80:64)
             TO WS-HASH-COMPLETO.

      *==========================================================*
      * 2400 - PARSEAR IMP
      *==========================================================*
       2400-PARSEAR-IMP.

           MOVE REG-LINEA-ENTRADA(1:10)
             TO DB-INS-CUENTA

           MOVE REG-LINEA-ENTRADA(11:3)
             TO DB-INS-NUM-CREDITO

           COMPUTE DB-INS-MONTO =
               FUNCTION NUMVAL(
                   REG-LINEA-ENTRADA(14:15)
               ) / 100

           MOVE REG-LINEA-ENTRADA(29:40)
             TO WS-TRACE-ID-SQL

           MOVE REG-LINEA-ENTRADA(69:64)
             TO WS-HASH-COMPLETO.

      *==========================================================*
      * 3000 - INSERTAR EN REPLICA
      *==========================================================*
       3000-INSERTAR-EN-REPLICA.

           EVALUATE WS-REPLICA-ASIG

               WHEN "TF01"
                   PERFORM 3100-INSERT-TF01

               WHEN "TF02"
                   PERFORM 3100-INSERT-TF02

               WHEN "TF03"
                   PERFORM 3100-INSERT-TF03

               WHEN "TF04"
                   PERFORM 3100-INSERT-TF04

               WHEN "TF05"
                   PERFORM 3100-INSERT-TF05

               WHEN "TF06"
                   PERFORM 3100-INSERT-TF06

           END-EVALUATE.

      *==========================================================*
      * 3100 - INSERT TF01
      *==========================================================*
       3100-INSERT-TF01.

      *    EXEC SQL
      *        INSERT INTO TF01
      *        (
      *            ID_LOTE,
      *            ESTADO,
      *            DATOS_TX,
      *            REPLICA_NO,
      *            CUENTA,
      *            NUM_CREDITO,
      *            MONTO,
      *            TRACE_ID,
      *            ID_TRANSACCION,
      *            TYPE_UPDATE
      *        )
      *        VALUES
      *        (
      *            :WS-ID-LOTE-SQL,
      *            :WS-ESTADO-SQL,
      *            :WS-DATOS-TX-SQL,
      *            :WS-REPLICA-SQL,
      *            :DB-INS-CUENTA,
      *            :DB-INS-NUM-CREDITO,
      *            :DB-INS-MONTO,
      *            :WS-TRACE-ID-SQL,
      *            :DB-INS-TXID,
      *            :DB-INS-TYPE-UPDATE
      *        )
      *    END-EXEC
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
                 DB-INS-CUENTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 16 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DB-INS-NUM-CREDITO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 WS-TRACE-ID-SQL
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 40 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 DB-INS-TXID
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 40 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 DB-INS-TYPE-UPDATE
               MOVE 'X' TO SQL-TYPE(10)
               MOVE 10 TO SQL-LEN(10)
               MOVE 10 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE DB-INS-MONTO
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA

           PERFORM 3200-VALIDAR-INSERT.

      *==========================================================*
      * 3100 - INSERT TF02
      *==========================================================*
       3100-INSERT-TF02.

      *    EXEC SQL
      *        INSERT INTO TF02
      *        (
      *            ID_LOTE,
      *            ESTADO,
      *            DATOS_TX,
      *            REPLICA_NO,
      *            CUENTA,
      *            NUM_CREDITO,
      *            MONTO,
      *            TRACE_ID,
      *            ID_TRANSACCION,
      *            TYPE_UPDATE
      *        )
      *        VALUES
      *        (
      *            :WS-ID-LOTE-SQL,
      *            :WS-ESTADO-SQL,
      *            :WS-DATOS-TX-SQL,
      *            :WS-REPLICA-SQL,
      *            :DB-INS-CUENTA,
      *            :DB-INS-NUM-CREDITO,
      *            :DB-INS-MONTO,
      *            :WS-TRACE-ID-SQL,
      *            :DB-INS-TXID,
      *            :DB-INS-TYPE-UPDATE
      *        )
      *    END-EXEC
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
                 DB-INS-CUENTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 16 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DB-INS-NUM-CREDITO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 WS-TRACE-ID-SQL
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 40 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 DB-INS-TXID
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 40 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 DB-INS-TYPE-UPDATE
               MOVE 'X' TO SQL-TYPE(10)
               MOVE 10 TO SQL-LEN(10)
               MOVE 10 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE DB-INS-MONTO
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA

           PERFORM 3200-VALIDAR-INSERT.

      *==========================================================*
      * 3100 - INSERT TF03
      *==========================================================*
       3100-INSERT-TF03.

      *    EXEC SQL
      *        INSERT INTO TF03
      *        (
      *            ID_LOTE,
      *            ESTADO,
      *            DATOS_TX,
      *            REPLICA_NO,
      *            CUENTA,
      *            NUM_CREDITO,
      *            MONTO,
      *            TRACE_ID,
      *            ID_TRANSACCION,
      *            TYPE_UPDATE
      *        )
      *        VALUES
      *        (
      *            :WS-ID-LOTE-SQL,
      *            :WS-ESTADO-SQL,
      *            :WS-DATOS-TX-SQL,
      *            :WS-REPLICA-SQL,
      *            :DB-INS-CUENTA,
      *            :DB-INS-NUM-CREDITO,
      *            :DB-INS-MONTO,
      *            :WS-TRACE-ID-SQL,
      *            :DB-INS-TXID,
      *            :DB-INS-TYPE-UPDATE
      *        )
      *    END-EXEC
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
                 DB-INS-CUENTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 16 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DB-INS-NUM-CREDITO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 WS-TRACE-ID-SQL
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 40 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 DB-INS-TXID
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 40 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 DB-INS-TYPE-UPDATE
               MOVE 'X' TO SQL-TYPE(10)
               MOVE 10 TO SQL-LEN(10)
               MOVE 10 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE DB-INS-MONTO
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA

           PERFORM 3200-VALIDAR-INSERT.

      *==========================================================*
      * 3100 - INSERT TF04
      *==========================================================*
       3100-INSERT-TF04.

      *    EXEC SQL
      *        INSERT INTO TF04
      *        (
      *            ID_LOTE,
      *            ESTADO,
      *            DATOS_TX,
      *            REPLICA_NO,
      *            CUENTA,
      *            NUM_CREDITO,
      *            MONTO,
      *            TRACE_ID,
      *            ID_TRANSACCION,
      *            TYPE_UPDATE
      *        )
      *        VALUES
      *        (
      *            :WS-ID-LOTE-SQL,
      *            :WS-ESTADO-SQL,
      *            :WS-DATOS-TX-SQL,
      *            :WS-REPLICA-SQL,
      *            :DB-INS-CUENTA,
      *            :DB-INS-NUM-CREDITO,
      *            :DB-INS-MONTO,
      *            :WS-TRACE-ID-SQL,
      *            :DB-INS-TXID,
      *            :DB-INS-TYPE-UPDATE
      *        )
      *    END-EXEC
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
                 DB-INS-CUENTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 16 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DB-INS-NUM-CREDITO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 WS-TRACE-ID-SQL
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 40 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 DB-INS-TXID
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 40 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 DB-INS-TYPE-UPDATE
               MOVE 'X' TO SQL-TYPE(10)
               MOVE 10 TO SQL-LEN(10)
               MOVE 10 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE DB-INS-MONTO
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA

           PERFORM 3200-VALIDAR-INSERT.

      *==========================================================*
      * 3100 - INSERT TF05
      *==========================================================*
       3100-INSERT-TF05.

      *    EXEC SQL
      *        INSERT INTO TF05
      *        (
      *            ID_LOTE,
      *            ESTADO,
      *            DATOS_TX,
      *            REPLICA_NO,
      *            CUENTA,
      *            NUM_CREDITO,
      *            MONTO,
      *            TRACE_ID,
      *            ID_TRANSACCION,
      *            TYPE_UPDATE
      *        )
      *        VALUES
      *        (
      *            :WS-ID-LOTE-SQL,
      *            :WS-ESTADO-SQL,
      *            :WS-DATOS-TX-SQL,
      *            :WS-REPLICA-SQL,
      *            :DB-INS-CUENTA,
      *            :DB-INS-NUM-CREDITO,
      *            :DB-INS-MONTO,
      *            :WS-TRACE-ID-SQL,
      *            :DB-INS-TXID,
      *            :DB-INS-TYPE-UPDATE
      *        )
      *    END-EXEC
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
                 DB-INS-CUENTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 16 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DB-INS-NUM-CREDITO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 WS-TRACE-ID-SQL
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 40 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 DB-INS-TXID
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 40 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 DB-INS-TYPE-UPDATE
               MOVE 'X' TO SQL-TYPE(10)
               MOVE 10 TO SQL-LEN(10)
               MOVE 10 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE DB-INS-MONTO
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA

           PERFORM 3200-VALIDAR-INSERT.

      *==========================================================*
      * 3100 - INSERT TF06
      *==========================================================*
       3100-INSERT-TF06.

      *    EXEC SQL
      *        INSERT INTO TF06
      *        (
      *            ID_LOTE,
      *            ESTADO,
      *            DATOS_TX,
      *            REPLICA_NO,
      *            CUENTA,
      *            NUM_CREDITO,
      *            MONTO,
      *            TRACE_ID,
      *            ID_TRANSACCION,
      *            TYPE_UPDATE
      *        )
      *        VALUES
      *        (
      *            :WS-ID-LOTE-SQL,
      *            :WS-ESTADO-SQL,
      *            :WS-DATOS-TX-SQL,
      *            :WS-REPLICA-SQL,
      *            :DB-INS-CUENTA,
      *            :DB-INS-NUM-CREDITO,
      *            :DB-INS-MONTO,
      *            :WS-TRACE-ID-SQL,
      *            :DB-INS-TXID,
      *            :DB-INS-TYPE-UPDATE
      *        )
      *    END-EXEC
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
                 DB-INS-CUENTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 16 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 DB-INS-NUM-CREDITO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 20 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(7)
               MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 WS-TRACE-ID-SQL
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 40 TO SQL-LEN(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 DB-INS-TXID
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 40 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 DB-INS-TYPE-UPDATE
               MOVE 'X' TO SQL-TYPE(10)
               MOVE 10 TO SQL-LEN(10)
               MOVE 10 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE DB-INS-MONTO
             TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA

           PERFORM 3200-VALIDAR-INSERT.

      *==========================================================*
      * 3200 - VALIDAR INSERT
      *==========================================================*
       3200-VALIDAR-INSERT.

           IF SQLCODE NOT = 0
               DISPLAY "FALLO INSERT EN REPLICA "
                       WS-REPLICA-ASIG

               DISPLAY "SQLCODE: "
                       SQLCODE

               MOVE 99 TO LK-TF-COD-RETORNO
               MOVE "Y" TO WS-EOF

           ELSE
               ADD 1 TO WS-CONT-REGS
           END-IF.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-INS-CUENTA            IN USE CHAR(16)
      *  DB-INS-MONTO             IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(15,2)
      *  DB-INS-NUM-CREDITO       IN USE CHAR(20)
      *  DB-INS-TXID              IN USE CHAR(40)
      *  DB-INS-TYPE-UPDATE       IN USE CHAR(10)
      *  WS-DATOS-TX-SQL          IN USE CHAR(500)
      *  WS-DIR-BASE-SQL          IN USE CHAR(120)
      *  WS-ESTADO-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(1,0)
      *  WS-HOST-VARS-TFMX    NOT IN USE
      *  WS-HOST-VARS-TFMX.DB-INS-CUENTA NOT IN USE
      *  WS-HOST-VARS-TFMX.DB-INS-MONTO NOT IN USE
      *  WS-HOST-VARS-TFMX.DB-INS-NUM-CREDITO NOT IN USE
      *  WS-HOST-VARS-TFMX.DB-INS-TXID NOT IN USE
      *  WS-HOST-VARS-TFMX.DB-INS-TYPE-UPDATE NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-DATOS-TX-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-DIR-BASE-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-ESTADO-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-ID-LOTE-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-REPLICA-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-TIPO-TX-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-TRACE-ID-SQL NOT IN USE
      *  WS-ID-LOTE-SQL           IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  WS-REPLICA-SQL           IN USE CHAR(4)
      *  WS-TIPO-TX-SQL       NOT IN USE
      *  WS-TRACE-ID-SQL          IN USE CHAR(40)
      **********************************************************************

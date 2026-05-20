       IDENTIFICATION DIVISION.
       PROGRAM-ID. XXXREP.
      *==========================================================
      * PROGRAMA PRECOMPILABLE: XXXREP.sqb
      * FASE 30: GENERACIÓN DE REPORTES DE CONCILIACIÓN
      * INCLUYE: Trazabilidad (ID_REG, TRACE_ID) y Anti-Colisión
      *==========================================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-SALIDA ASSIGN TO WS-RUTA-FINAL
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO-SALIDA.
       01  REG-SALIDA                  PIC X(800).

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
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 132.
           05 SQL-STMT   PIC X(132) VALUE 'SELECT ID_REGISTRO,ID_TRANSAC
      -    'CION,DATOS_TX,ESTADO,COALESCE(COD_ERROR,''000''),COALESCE(ER
      -    'ROR_MESSAGE,''OK'') FROM tf01 WHERE ID_LOTE = ?'.
           05 SQL-CNAME  PIC X(2) VALUE 'R1'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 132.
           05 SQL-STMT   PIC X(132) VALUE 'SELECT ID_REGISTRO,ID_TRANSAC
      -    'CION,DATOS_TX,ESTADO,COALESCE(COD_ERROR,''000''),COALESCE(ER
      -    'ROR_MESSAGE,''OK'') FROM tf02 WHERE ID_LOTE = ?'.
           05 SQL-CNAME  PIC X(2) VALUE 'R2'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 132.
           05 SQL-STMT   PIC X(132) VALUE 'SELECT ID_REGISTRO,ID_TRANSAC
      -    'CION,DATOS_TX,ESTADO,COALESCE(COD_ERROR,''000''),COALESCE(ER
      -    'ROR_MESSAGE,''OK'') FROM tf03 WHERE ID_LOTE = ?'.
           05 SQL-CNAME  PIC X(2) VALUE 'R3'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 132.
           05 SQL-STMT   PIC X(132) VALUE 'SELECT ID_REGISTRO,ID_TRANSAC
      -    'CION,DATOS_TX,ESTADO,COALESCE(COD_ERROR,''000''),COALESCE(ER
      -    'ROR_MESSAGE,''OK'') FROM tf04 WHERE ID_LOTE = ?'.
           05 SQL-CNAME  PIC X(2) VALUE 'R4'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 132.
           05 SQL-STMT   PIC X(132) VALUE 'SELECT ID_REGISTRO,ID_TRANSAC
      -    'CION,DATOS_TX,ESTADO,COALESCE(COD_ERROR,''000''),COALESCE(ER
      -    'ROR_MESSAGE,''OK'') FROM tf05 WHERE ID_LOTE = ?'.
           05 SQL-CNAME  PIC X(2) VALUE 'R5'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 132.
           05 SQL-STMT   PIC X(132) VALUE 'SELECT ID_REGISTRO,ID_TRANSAC
      -    'CION,DATOS_TX,ESTADO,COALESCE(COD_ERROR,''000''),COALESCE(ER
      -    'ROR_MESSAGE,''OK'') FROM tf06 WHERE ID_LOTE = ?'.
           05 SQL-CNAME  PIC X(2) VALUE 'R6'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(1) COMP-3.
           05 SQL-VAR-0003  PIC S9(9) COMP-3.
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
      * --- ESTRUCTURA SQL ÚNICA (Evita error processexec) ---
       01  DB-SQL-BUFFER.
           05 DB-ID-REG-SQL            PIC 9(09).
           05 DB-TRACE-ID-SQL          PIC X(40).
           05 DB-DATOS-TX-SQL          PIC X(500).
           05 DB-ESTADO-SQL            PIC 9(01).
           05 DB-COD-ERR-SQL           PIC X(10).
           05 DB-MSG-ERR-SQL           PIC X(200).
           05 DB-ID-LOTE-SQL           PIC 9(09).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-CONTROL-REP.
           05 WS-RUTA-FINAL            PIC X(250).
           05 WS-STATUS-TXT            PIC X(05).
           05 WS-FS                    PIC X(02).
           05 WS-EOF-REP               PIC X(01) VALUE 'N'.
           05 WS-PRE-RUTA              PIC X(100).

       LINKAGE SECTION.
           COPY LKTF.

       01  WS-TFFM-VARS.
           05 WS-ID-LOTE               PIC 9(09).
           05 WS-FILE-NAME             PIC X(120).
           05 WS-FASE                  PIC X(02).
           05 WS-TYPE-UPDATE           PIC X(10).
           05 WS-REPLICA-ASIG          PIC X(04).
           05 WS-RETRY-COUNT           PIC 9(02).

       PROCEDURE DIVISION USING WS-TFFM-VARS,
                                LK-TRICKLE-FEED-INTERFACE.

       000-PRINCIPAL.
           MOVE "N" TO WS-EOF-REP
           MOVE 00 TO LK-TF-COD-RETORNO
           MOVE WS-ID-LOTE TO DB-ID-LOTE-SQL

           MOVE "C:\banco\spool\Interfaces\BATCH-UPLOAD-S\"
             TO WS-PRE-RUTA

           INITIALIZE WS-RUTA-FINAL
           STRING FUNCTION TRIM(WS-PRE-RUTA)
                  "TRICKLE-FEED-REPORT\"
                  FUNCTION TRIM(WS-FILE-NAME) ".out"
                  DELIMITED BY SIZE INTO WS-RUTA-FINAL

           OPEN OUTPUT ARCHIVO-SALIDA
           IF WS-FS NOT = "00"
               DISPLAY "ERR REPORTE: " WS-RUTA-FINAL
               MOVE 99 TO LK-TF-COD-RETORNO
               GOBACK
           END-IF.

      * --- CABECERA DE AUDITORÍA ---
           MOVE "ID_REG|TRACE_ID|TRAMA_TX|ESTADO|COD_ERR|DESCRIPCION"
             TO REG-SALIDA
           WRITE REG-SALIDA
           INITIALIZE REG-SALIDA

           PERFORM 100-ABRIR

           PERFORM 200-FETCH
           PERFORM UNTIL WS-EOF-REP = 'Y'
               PERFORM 300-ESCRIBIR
               PERFORM 200-FETCH
           END-PERFORM

           PERFORM 400-CERRAR
           CLOSE ARCHIVO-SALIDA

           DISPLAY "  [REP] REPORTE GENERADO CON EXITO: " WS-FILE-NAME
           GOBACK.

       100-ABRIR.
           EVALUATE WS-REPLICA-ASIG
               WHEN "TF01" PERFORM 101-OR
               WHEN "TF02" PERFORM 102-OR
               WHEN "TF03" PERFORM 103-OR
               WHEN "TF04" PERFORM 104-OR
               WHEN "TF05" PERFORM 105-OR
               WHEN "TF06" PERFORM 106-OR
           END-EVALUATE.

       101-OR.
      *    EXEC SQL DECLARE R1 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_TRANSACCION, DATOS_TX, ESTADO,
      *               COALESCE(COD_ERROR, '000'),
      *               COALESCE(ERROR_MESSAGE, 'OK')
      *        FROM tf01 WHERE ID_LOTE = :DB-ID-LOTE-SQL
      *    END-EXEC.
                   .
      *    EXEC SQL OPEN R1 END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
           END-IF
           MOVE DB-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-0
                               SQLCA
           END-CALL
                                    .

       102-OR.
      *    EXEC SQL DECLARE R2 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_TRANSACCION, DATOS_TX, ESTADO,
      *               COALESCE(COD_ERROR, '000'),
      *               COALESCE(ERROR_MESSAGE, 'OK')
      *        FROM tf02 WHERE ID_LOTE = :DB-ID-LOTE-SQL
      *    END-EXEC.
                   .
      *    EXEC SQL OPEN R2 END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
           END-IF
           MOVE DB-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-1
                               SQLCA
           END-CALL
                                    .

       103-OR.
      *    EXEC SQL DECLARE R3 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_TRANSACCION, DATOS_TX, ESTADO,
      *               COALESCE(COD_ERROR, '000'),
      *               COALESCE(ERROR_MESSAGE, 'OK')
      *        FROM tf03 WHERE ID_LOTE = :DB-ID-LOTE-SQL
      *    END-EXEC.
                   .
      *    EXEC SQL OPEN R3 END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
           END-IF
           MOVE DB-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-2
                               SQLCA
           END-CALL
                                    .

       104-OR.
      *    EXEC SQL DECLARE R4 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_TRANSACCION, DATOS_TX, ESTADO,
      *               COALESCE(COD_ERROR, '000'),
      *               COALESCE(ERROR_MESSAGE, 'OK')
      *        FROM tf04 WHERE ID_LOTE = :DB-ID-LOTE-SQL
      *    END-EXEC.
                   .
      *    EXEC SQL OPEN R4 END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
           END-IF
           MOVE DB-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-3
                               SQLCA
           END-CALL
                                    .

       105-OR.
      *    EXEC SQL DECLARE R5 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_TRANSACCION, DATOS_TX, ESTADO,
      *               COALESCE(COD_ERROR, '000'),
      *               COALESCE(ERROR_MESSAGE, 'OK')
      *        FROM tf05 WHERE ID_LOTE = :DB-ID-LOTE-SQL
      *    END-EXEC.
                   .
      *    EXEC SQL OPEN R5 END-EXEC.
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
           END-IF
           MOVE DB-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-4
                               SQLCA
           END-CALL
                                    .

       106-OR.
      *    EXEC SQL DECLARE R6 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_TRANSACCION, DATOS_TX, ESTADO,
      *               COALESCE(COD_ERROR, '000'),
      *               COALESCE(ERROR_MESSAGE, 'OK')
      *        FROM tf06 WHERE ID_LOTE = :DB-ID-LOTE-SQL
      *    END-EXEC.
                   .
      *    EXEC SQL OPEN R6 END-EXEC.
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
           END-IF
           MOVE DB-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-5
                               SQLCA
           END-CALL
                                    .

       200-FETCH.
           EVALUATE WS-REPLICA-ASIG
               WHEN "TF01"
      *            EXEC SQL FETCH R1 INTO :DB-ID-REG-SQL,
      *            :DB-TRACE-ID-SQL, :DB-DATOS-TX-SQL, :DB-ESTADO-SQL,
      *            :DB-COD-ERR-SQL, :DB-MSG-ERR-SQL
      *            END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             DB-TRACE-ID-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 40 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             DB-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 500 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(4)
           MOVE 1 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             DB-COD-ERR-SQL
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 10 TO SQL-LEN(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-MSG-ERR-SQL
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 200 TO SQL-LEN(6)
           MOVE 6 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REG-SQL
           MOVE SQL-VAR-0002 TO DB-ESTADO-SQL
               WHEN "TF02"
      *            EXEC SQL FETCH R2 INTO :DB-ID-REG-SQL,
      *            :DB-TRACE-ID-SQL, :DB-DATOS-TX-SQL, :DB-ESTADO-SQL,
      *            :DB-COD-ERR-SQL, :DB-MSG-ERR-SQL
      *            END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             DB-TRACE-ID-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 40 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             DB-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 500 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(4)
           MOVE 1 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             DB-COD-ERR-SQL
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 10 TO SQL-LEN(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-MSG-ERR-SQL
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 200 TO SQL-LEN(6)
           MOVE 6 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REG-SQL
           MOVE SQL-VAR-0002 TO DB-ESTADO-SQL
               WHEN "TF03"
      *            EXEC SQL FETCH R3 INTO :DB-ID-REG-SQL,
      *            :DB-TRACE-ID-SQL, :DB-DATOS-TX-SQL, :DB-ESTADO-SQL,
      *            :DB-COD-ERR-SQL, :DB-MSG-ERR-SQL
      *            END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             DB-TRACE-ID-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 40 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             DB-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 500 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(4)
           MOVE 1 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             DB-COD-ERR-SQL
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 10 TO SQL-LEN(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-MSG-ERR-SQL
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 200 TO SQL-LEN(6)
           MOVE 6 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REG-SQL
           MOVE SQL-VAR-0002 TO DB-ESTADO-SQL
               WHEN "TF04"
      *            EXEC SQL FETCH R4 INTO :DB-ID-REG-SQL,
      *            :DB-TRACE-ID-SQL, :DB-DATOS-TX-SQL, :DB-ESTADO-SQL,
      *            :DB-COD-ERR-SQL, :DB-MSG-ERR-SQL
      *            END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             DB-TRACE-ID-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 40 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             DB-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 500 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(4)
           MOVE 1 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             DB-COD-ERR-SQL
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 10 TO SQL-LEN(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-MSG-ERR-SQL
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 200 TO SQL-LEN(6)
           MOVE 6 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REG-SQL
           MOVE SQL-VAR-0002 TO DB-ESTADO-SQL
               WHEN "TF05"
      *            EXEC SQL FETCH R5 INTO :DB-ID-REG-SQL,
      *            :DB-TRACE-ID-SQL, :DB-DATOS-TX-SQL, :DB-ESTADO-SQL,
      *            :DB-COD-ERR-SQL, :DB-MSG-ERR-SQL
      *            END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             DB-TRACE-ID-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 40 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             DB-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 500 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(4)
           MOVE 1 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             DB-COD-ERR-SQL
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 10 TO SQL-LEN(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-MSG-ERR-SQL
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 200 TO SQL-LEN(6)
           MOVE 6 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-4
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REG-SQL
           MOVE SQL-VAR-0002 TO DB-ESTADO-SQL
               WHEN "TF06"
      *            EXEC SQL FETCH R6 INTO :DB-ID-REG-SQL,
      *            :DB-TRACE-ID-SQL, :DB-DATOS-TX-SQL, :DB-ESTADO-SQL,
      *            :DB-COD-ERR-SQL, :DB-MSG-ERR-SQL
      *            END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             DB-TRACE-ID-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 40 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             DB-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 500 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(4)
           MOVE 1 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             DB-COD-ERR-SQL
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 10 TO SQL-LEN(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-MSG-ERR-SQL
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 200 TO SQL-LEN(6)
           MOVE 6 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-5
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REG-SQL
           MOVE SQL-VAR-0002 TO DB-ESTADO-SQL
           END-EVALUATE.

           IF SQLCODE = 100
               MOVE 'Y' TO WS-EOF-REP
           ELSE
               IF SQLCODE < 0
                   MOVE 99 TO LK-TF-COD-RETORNO
                   MOVE "Y" TO WS-EOF-REP
               END-IF
           END-IF.

       300-ESCRIBIR.
           INITIALIZE REG-SALIDA

           IF DB-ESTADO-SQL = 4
               MOVE "OK" TO WS-STATUS-TXT
               MOVE "000" TO DB-COD-ERR-SQL
               MOVE "PROCESADO" TO DB-MSG-ERR-SQL
           ELSE
               MOVE "ERROR" TO WS-STATUS-TXT
           END-IF.

      * --- ENSAMBLAJE DE TRAMA DE AUDITORÍA ---
           STRING FUNCTION TRIM(DB-ID-REG-SQL)    "|"
                  FUNCTION TRIM(DB-TRACE-ID-SQL)  "|"
                  FUNCTION TRIM(DB-DATOS-TX-SQL)  "|"
                  FUNCTION TRIM(WS-STATUS-TXT)    "|"
                  FUNCTION TRIM(DB-COD-ERR-SQL)   "|"
                  FUNCTION TRIM(DB-MSG-ERR-SQL)
                  DELIMITED BY SIZE INTO REG-SALIDA.

           WRITE REG-SALIDA.

       400-CERRAR.
           EVALUATE WS-REPLICA-ASIG
               WHEN "TF01" PERFORM 401-CLOSE-R1
               WHEN "TF02" PERFORM 402-CLOSE-R2
               WHEN "TF03" PERFORM 403-CLOSE-R3
               WHEN "TF04" PERFORM 404-CLOSE-R4
               WHEN "TF05" PERFORM 405-CLOSE-R5
               WHEN "TF06" PERFORM 406-CLOSE-R6
           END-EVALUATE.

      * CORRECCIÓN CRÍTICA: Nombre de párrafo aislado en su propia l
       401-CLOSE-R1.
      *    EXEC SQL CLOSE R1 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA
                                     .
       402-CLOSE-R2.
      *    EXEC SQL CLOSE R2 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-1
                               SQLCA
                                     .
       403-CLOSE-R3.
      *    EXEC SQL CLOSE R3 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-2
                               SQLCA
                                     .
       404-CLOSE-R4.
      *    EXEC SQL CLOSE R4 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-3
                               SQLCA
                                     .
       405-CLOSE-R5.
      *    EXEC SQL CLOSE R5 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-4
                               SQLCA
                                     .
       406-CLOSE-R6.
      *    EXEC SQL CLOSE R6 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-5
                               SQLCA
                                     .
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-COD-ERR-SQL           IN USE CHAR(10)
      *  DB-DATOS-TX-SQL          IN USE CHAR(500)
      *  DB-ESTADO-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(1,0)
      *  DB-ID-LOTE-SQL           IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(9,0)
      *  DB-ID-REG-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  DB-MSG-ERR-SQL           IN USE CHAR(200)
      *  DB-SQL-BUFFER        NOT IN USE
      *  DB-SQL-BUFFER.DB-COD-ERR-SQL NOT IN USE
      *  DB-SQL-BUFFER.DB-DATOS-TX-SQL NOT IN USE
      *  DB-SQL-BUFFER.DB-ESTADO-SQL NOT IN USE
      *  DB-SQL-BUFFER.DB-ID-LOTE-SQL NOT IN USE
      *  DB-SQL-BUFFER.DB-ID-REG-SQL NOT IN USE
      *  DB-SQL-BUFFER.DB-MSG-ERR-SQL NOT IN USE
      *  DB-SQL-BUFFER.DB-TRACE-ID-SQL NOT IN USE
      *  DB-TRACE-ID-SQL          IN USE CHAR(40)
      *  R1                       IN USE CURSOR
      *  R2                       IN USE CURSOR
      *  R3                       IN USE CURSOR
      *  R4                       IN USE CURSOR
      *  R5                       IN USE CURSOR
      *  R6                       IN USE CURSOR
      **********************************************************************

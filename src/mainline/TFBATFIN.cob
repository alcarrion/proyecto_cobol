       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFBATFIN.
      *================================================================*
      * PROGRAMA: TFBATFIN.sqb                                         *
      * FUNCION: Procesador Batch de transacciones por réplica        *
      * CORRECCIÓN: Soporte Créditos, alineación Linkage y Logs de Er
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 5.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 5 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 5 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 5 TIMES.
           05 SQL-PREC   PIC X OCCURS 5 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 94.
           05 SQL-STMT   PIC X(94) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CUE
      -    'NTA,NUM_CREDITO,MONTO FROM TF01 WHERE ID_LOTE = ? AND ESTADO
      -    ' = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C1'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF01 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE TF01 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 94.
           05 SQL-STMT   PIC X(94) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CUE
      -    'NTA,NUM_CREDITO,MONTO FROM TF02 WHERE ID_LOTE = ? AND ESTADO
      -    ' = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C2'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF02 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE TF02 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 94.
           05 SQL-STMT   PIC X(94) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CUE
      -    'NTA,NUM_CREDITO,MONTO FROM TF03 WHERE ID_LOTE = ? AND ESTADO
      -    ' = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C3'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF03 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-8.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE TF03 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-9.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 94.
           05 SQL-STMT   PIC X(94) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CUE
      -    'NTA,NUM_CREDITO,MONTO FROM TF04 WHERE ID_LOTE = ? AND ESTADO
      -    ' = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C4'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-10.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF04 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-11.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE TF04 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-12.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 94.
           05 SQL-STMT   PIC X(94) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CUE
      -    'NTA,NUM_CREDITO,MONTO FROM TF05 WHERE ID_LOTE = ? AND ESTADO
      -    ' = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C5'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-13.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF05 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-14.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE TF05 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-15.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 94.
           05 SQL-STMT   PIC X(94) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CUE
      -    'NTA,NUM_CREDITO,MONTO FROM TF06 WHERE ID_LOTE = ? AND ESTADO
      -    ' = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C6'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-16.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF06 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-17.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE TF06 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(9) COMP-3.
           05 SQL-VAR-0003  PIC S9(13) COMP-3.
           05 SQL-VAR-0004  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0005  PIC S9(9) COMP-3.
           05 SQL-VAR-0006  PIC S9(3) COMP-3.
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
      * 01  WS-REG-REPLICA.
           01 DB-ID-REGISTRO       PIC 9(09).
           01 DB-ID-LOTE           PIC 9(09).
           01 DB-CUENTA            PIC 9(12).
           01 DB-NUM-CREDITO       PIC X(20).
           01 DB-MONTO             PIC S9(13)V99.
           01 DB-OP                PIC X(03).
           01 DB-LOTE-BUSQUEDA     PIC 9(09).
           01 DB-ESTADO-FINAL      PIC 9(02).
           01 DB-COD-ERROR         PIC X(10).
           01 DB-MSG-ERROR         PIC X(200).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-FLAGS.
           05 WS-EOF-CURSOR        PIC X(01) VALUE 'N'.

       01  REG-CTA.
           05 CTA-ID-CLIENTE       PIC 9(12).  *> Alineado a DB
           05 CTA-NUM-CREDITO     PIC X(20).  *> NUEVO
           05 CTA-SALDO-ACTUAL     PIC S9(13)V99.
           05 CTA-MONTO-MOV        PIC 9(13)V99.

       LINKAGE SECTION.
           COPY LKTF.
       01  WS-TFFM-VARS.
           05 WS-ID-LOTE           PIC 9(09).
           05 WS-FILE-NAME         PIC X(120).
           05 WS-FASE              PIC X(02).
           05 WS-TYPE-UPDATE       PIC X(10).
           05 WS-REPLICA-ASIG      PIC X(04).
           05 WS-RETRY-COUNT       PIC 9(02).

       PROCEDURE DIVISION USING WS-TFFM-VARS, LK-TRICKLE-FEED-INTERFACE.
       0000-PRINCIPAL.
           MOVE "N" TO WS-EOF-CURSOR
           MOVE 00 TO LK-TF-COD-RETORNO
           MOVE WS-ID-LOTE TO DB-LOTE-BUSQUEDA.

           EVALUATE WS-REPLICA-ASIG
               WHEN "TF01" PERFORM 1000-PROCESAR-TF01
               WHEN "TF02" PERFORM 2000-PROCESAR-TF02
               WHEN "TF03" PERFORM 3000-PROCESAR-TF03
               WHEN "TF04" PERFORM 4000-PROCESAR-TF04
               WHEN "TF05" PERFORM 5000-PROCESAR-TF05
               WHEN "TF06" PERFORM 6000-PROCESAR-TF06
           END-EVALUATE.
           GOBACK.

       1000-PROCESAR-TF01.
      *    EXEC SQL DECLARE C1 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_LOTE, CUENTA, NUM_CREDITO, MONTO
      *        FROM TF01 WHERE ID_LOTE = :DB-LOTE-BUSQUEDA
      *        AND ESTADO = 2
      *    END-EXEC.
                   .
      *    EXEC SQL OPEN C1 END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
           END-IF
           MOVE DB-LOTE-BUSQUEDA TO SQL-VAR-0005
           CALL 'OCSQLOCU' USING SQL-STMT-0
                               SQLCA
           END-CALL
                                    .
           PERFORM UNTIL WS-EOF-CURSOR = 'Y'
      *        EXEC SQL FETCH C1 INTO :DB-ID-REGISTRO, :DB-ID-LOTE,
      *                               :DB-CUENTA, :DB-NUM-CREDITO,
      *                               :DB-MONTO END-EXEC
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
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(3)
           MOVE 7 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             DB-NUM-CREDITO
           MOVE 'X' TO SQL-TYPE(4)
           MOVE 20 TO SQL-LEN(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(5)
           MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO
               IF SQLCODE = 0
      *            EXEC SQL UPDATE TF01 SET ESTADO = 3
      *                     WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *                     END-EXEC
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
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA

                   PERFORM 9000-AFECTAR-CORE

      *            EXEC SQL
      *                UPDATE TF01
      *                SET ESTADO = :DB-ESTADO-FINAL,
      *                    COD_ERROR = :DB-COD-ERROR,
      *                    ERROR_MESSAGE = :DB-MSG-ERROR
      *                WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-COD-ERROR
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-MSG-ERROR
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 200 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(4)
               MOVE 5 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ESTADO-FINAL
             TO SQL-VAR-0006
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
               ELSE
                   MOVE 'Y' TO WS-EOF-CURSOR
               END-IF
           END-PERFORM.
      *    EXEC SQL CLOSE C1 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA
                                     .

      * PÁRRAFOS 2000 A 6000 REPLICAN EXACTAMENTE LA LÓGICA DE 1000
      *CAMBIANDO LA TABLA (TF02-TF06)
       2000-PROCESAR-TF02.
      *    EXEC SQL DECLARE C2 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_LOTE, CUENTA, NUM_CREDITO, MONTO
      *        FROM TF02 WHERE ID_LOTE = :DB-LOTE-BUSQUEDA AND
      *        ESTADO = 2 END-EXEC.
                                  .
      *    EXEC SQL OPEN C2 END-EXEC.
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
           END-IF
           MOVE DB-LOTE-BUSQUEDA TO SQL-VAR-0005
           CALL 'OCSQLOCU' USING SQL-STMT-3
                               SQLCA
           END-CALL
                                    .
           PERFORM UNTIL WS-EOF-CURSOR = 'Y'
      *        EXEC SQL FETCH C2 INTO :DB-ID-REGISTRO, :DB-ID-LOTE,
      *        :DB-CUENTA, :DB-NUM-CREDITO, :DB-MONTO END-EXEC
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
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(3)
           MOVE 7 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             DB-NUM-CREDITO
           MOVE 'X' TO SQL-TYPE(4)
           MOVE 20 TO SQL-LEN(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(5)
           MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO
               IF SQLCODE = 0
      *            EXEC SQL UPDATE TF02 SET ESTADO = 3
      *            WHERE ID_REGISTRO = :DB-ID-REGISTRO END-EXEC
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
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
                   PERFORM 9000-AFECTAR-CORE
      *            EXEC SQL UPDATE TF02 SET ESTADO = :DB-ESTADO-FINAL,
      *            COD_ERROR = :DB-COD-ERROR, ERROR_MESSAGE =
      *            :DB-MSG-ERROR WHERE ID_REGISTRO =
      *            :DB-ID-REGISTRO END-EXEC
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-COD-ERROR
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-MSG-ERROR
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 200 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(4)
               MOVE 5 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ESTADO-FINAL
             TO SQL-VAR-0006
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
               ELSE MOVE 'Y' TO WS-EOF-CURSOR END-IF
           END-PERFORM.
      *    EXEC SQL CLOSE C2 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-3
                               SQLCA
                                     .

       3000-PROCESAR-TF03.
      *    EXEC SQL DECLARE C3 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_LOTE, CUENTA, NUM_CREDITO, MONTO
      *        FROM TF03 WHERE ID_LOTE = :DB-LOTE-BUSQUEDA AND
      *        ESTADO = 2 END-EXEC.
                                  .
      *    EXEC SQL OPEN C3 END-EXEC.
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
           END-IF
           MOVE DB-LOTE-BUSQUEDA TO SQL-VAR-0005
           CALL 'OCSQLOCU' USING SQL-STMT-6
                               SQLCA
           END-CALL
                                    .
           PERFORM UNTIL WS-EOF-CURSOR = 'Y'
      *        EXEC SQL FETCH C3 INTO :DB-ID-REGISTRO, :DB-ID-LOTE,
      *         :DB-CUENTA, :DB-NUM-CREDITO, :DB-MONTO END-EXEC
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
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(3)
           MOVE 7 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             DB-NUM-CREDITO
           MOVE 'X' TO SQL-TYPE(4)
           MOVE 20 TO SQL-LEN(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(5)
           MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-6
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO
               IF SQLCODE = 0
      *            EXEC SQL UPDATE TF03 SET ESTADO = 3
      *            WHERE ID_REGISTRO = :DB-ID-REGISTRO END-EXEC
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
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA
                   PERFORM 9000-AFECTAR-CORE
      *            EXEC SQL UPDATE TF03 SET ESTADO = :DB-ESTADO-FINAL,
      *            COD_ERROR = :DB-COD-ERROR, ERROR_MESSAGE =
      *            :DB-MSG-ERROR WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-8 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-COD-ERROR
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-MSG-ERROR
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 200 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(4)
               MOVE 5 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-8
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ESTADO-FINAL
             TO SQL-VAR-0006
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-8
                               SQLCA
               ELSE MOVE 'Y' TO WS-EOF-CURSOR END-IF
           END-PERFORM.
      *    EXEC SQL CLOSE C3 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-6
                               SQLCA
                                     .

       4000-PROCESAR-TF04.
      *    EXEC SQL DECLARE C4 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_LOTE, CUENTA, NUM_CREDITO, MONTO
      *        FROM TF04 WHERE ID_LOTE = :DB-LOTE-BUSQUEDA
      *        AND ESTADO = 2 END-EXEC.
                                      .
      *    EXEC SQL OPEN C4 END-EXEC.
           IF SQL-PREP OF SQL-STMT-9 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-9
                                   SQLCA
           END-IF
           MOVE DB-LOTE-BUSQUEDA TO SQL-VAR-0005
           CALL 'OCSQLOCU' USING SQL-STMT-9
                               SQLCA
           END-CALL
                                    .
           PERFORM UNTIL WS-EOF-CURSOR = 'Y'
      *        EXEC SQL FETCH C4 INTO :DB-ID-REGISTRO, :DB-ID-LOTE,
      *        :DB-CUENTA, :DB-NUM-CREDITO, :DB-MONTO END-EXEC
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
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(3)
           MOVE 7 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             DB-NUM-CREDITO
           MOVE 'X' TO SQL-TYPE(4)
           MOVE 20 TO SQL-LEN(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(5)
           MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-9
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO
               IF SQLCODE = 0
      *            EXEC SQL UPDATE TF04 SET ESTADO = 3
      *            WHERE ID_REGISTRO = :DB-ID-REGISTRO END-EXEC
           IF SQL-PREP OF SQL-STMT-10 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-10
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-10
                               SQLCA
                   PERFORM 9000-AFECTAR-CORE
      *            EXEC SQL UPDATE TF04 SET ESTADO = :DB-ESTADO-FINAL,
      *            COD_ERROR = :DB-COD-ERROR, ERROR_MESSAGE =
      *            :DB-MSG-ERROR WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
           IF SQL-PREP OF SQL-STMT-11 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-COD-ERROR
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-MSG-ERROR
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 200 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(4)
               MOVE 5 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-11
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ESTADO-FINAL
             TO SQL-VAR-0006
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-11
                               SQLCA
               ELSE MOVE 'Y' TO WS-EOF-CURSOR END-IF
           END-PERFORM.
      *    EXEC SQL CLOSE C4 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-9
                               SQLCA
                                     .

       5000-PROCESAR-TF05.
      *    EXEC SQL DECLARE C5 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_LOTE, CUENTA, NUM_CREDITO, MONTO
      *        FROM TF05 WHERE ID_LOTE = :DB-LOTE-BUSQUEDA
      *        AND ESTADO = 2 END-EXEC.
                                      .
      *    EXEC SQL OPEN C5 END-EXEC.
           IF SQL-PREP OF SQL-STMT-12 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-12
                                   SQLCA
           END-IF
           MOVE DB-LOTE-BUSQUEDA TO SQL-VAR-0005
           CALL 'OCSQLOCU' USING SQL-STMT-12
                               SQLCA
           END-CALL
                                    .
           PERFORM UNTIL WS-EOF-CURSOR = 'Y'
      *        EXEC SQL FETCH C5 INTO :DB-ID-REGISTRO, :DB-ID-LOTE,
      *        :DB-CUENTA, :DB-NUM-CREDITO, :DB-MONTO END-EXEC
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
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(3)
           MOVE 7 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             DB-NUM-CREDITO
           MOVE 'X' TO SQL-TYPE(4)
           MOVE 20 TO SQL-LEN(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(5)
           MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-12
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO
               IF SQLCODE = 0
      *            EXEC SQL UPDATE TF05 SET ESTADO = 3
      *             WHERE ID_REGISTRO = :DB-ID-REGISTRO END-EXEC
           IF SQL-PREP OF SQL-STMT-13 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-13
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-13
                               SQLCA
                   PERFORM 9000-AFECTAR-CORE
      *            EXEC SQL UPDATE TF05 SET ESTADO = :DB-ESTADO-FINAL,
      *             COD_ERROR = :DB-COD-ERROR, ERROR_MESSAGE =
      *             :DB-MSG-ERROR WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *             END-EXEC
           IF SQL-PREP OF SQL-STMT-14 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-COD-ERROR
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-MSG-ERROR
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 200 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(4)
               MOVE 5 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-14
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ESTADO-FINAL
             TO SQL-VAR-0006
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-14
                               SQLCA
               ELSE MOVE 'Y' TO WS-EOF-CURSOR END-IF
           END-PERFORM.
      *    EXEC SQL CLOSE C5 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-12
                               SQLCA
                                     .

       6000-PROCESAR-TF06.
      *    EXEC SQL DECLARE C6 CURSOR FOR
      *        SELECT ID_REGISTRO, ID_LOTE, CUENTA, NUM_CREDITO, MONTO
      *        FROM TF06 WHERE ID_LOTE = :DB-LOTE-BUSQUEDA AND ESTADO
      *        = 2 END-EXEC.
                           .
      *    EXEC SQL OPEN C6 END-EXEC.
           IF SQL-PREP OF SQL-STMT-15 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-15
                                   SQLCA
           END-IF
           MOVE DB-LOTE-BUSQUEDA TO SQL-VAR-0005
           CALL 'OCSQLOCU' USING SQL-STMT-15
                               SQLCA
           END-CALL
                                    .
           PERFORM UNTIL WS-EOF-CURSOR = 'Y'
      *        EXEC SQL FETCH C6 INTO :DB-ID-REGISTRO, :DB-ID-LOTE,
      *        :DB-CUENTA, :DB-NUM-CREDITO, :DB-MONTO END-EXEC
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
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(3)
           MOVE 7 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             DB-NUM-CREDITO
           MOVE 'X' TO SQL-TYPE(4)
           MOVE 20 TO SQL-LEN(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(5)
           MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-15
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO
               IF SQLCODE = 0
      *            EXEC SQL UPDATE TF06 SET ESTADO = 3
      *            WHERE ID_REGISTRO = :DB-ID-REGISTRO END-EXEC
           IF SQL-PREP OF SQL-STMT-16 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-16
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-16
                               SQLCA
                   PERFORM 9000-AFECTAR-CORE
      *            EXEC SQL UPDATE TF06 SET ESTADO = :DB-ESTADO-FINAL,
      *            COD_ERROR = :DB-COD-ERROR, ERROR_MESSAGE =
      *            :DB-MSG-ERROR WHERE ID_REGISTRO =
      *            :DB-ID-REGISTRO END-EXEC
           IF SQL-PREP OF SQL-STMT-17 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-COD-ERROR
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-MSG-ERROR
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 200 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(4)
               MOVE 5 TO SQL-LEN(4)
               MOVE X'00' TO SQL-PREC(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-17
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-ESTADO-FINAL
             TO SQL-VAR-0006
           MOVE DB-ID-REGISTRO
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-17
                               SQLCA
               ELSE MOVE 'Y' TO WS-EOF-CURSOR END-IF
           END-PERFORM.
      *    EXEC SQL CLOSE C6 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-15
                               SQLCA
                                     .

       9000-AFECTAR-CORE.
           MOVE DB-CUENTA        TO CTA-ID-CLIENTE
           MOVE DB-NUM-CREDITO   TO CTA-NUM-CREDITO
           MOVE DB-MONTO         TO CTA-MONTO-MOV

           EVALUATE WS-TYPE-UPDATE
               WHEN "RRD000" MOVE "D" TO LK-TF-ACCION
               WHEN "RRR000" MOVE "D" TO LK-TF-ACCION
               WHEN "RRC000" MOVE "C" TO LK-TF-ACCION
               WHEN "RRP000" MOVE "P" TO LK-TF-ACCION
               WHEN OTHER    MOVE "C" TO LK-TF-ACCION
           END-EVALUATE.

           CALL "tkin01" USING REG-CTA, LK-TRICKLE-FEED-INTERFACE.

      * FORMATEO DE AUDITORÍA BANCARIA DE RESULTADOS
           IF LK-TF-COD-RETORNO = 0
               MOVE 4 TO DB-ESTADO-FINAL
               MOVE "000" TO DB-COD-ERROR
               MOVE "OK" TO DB-MSG-ERROR
           ELSE
               MOVE 7 TO DB-ESTADO-FINAL
               MOVE LK-TF-MENSAJE        TO DB-MSG-ERROR
               EVALUATE LK-TF-COD-RETORNO
                   WHEN 07 MOVE "INSFONDOS" TO DB-COD-ERROR
                   WHEN 01 MOVE "NOEXISTE"  TO DB-COD-ERROR
                   WHEN OTHER MOVE "ERRDB"  TO DB-COD-ERROR
               END-EVALUATE
           END-IF.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  C1                       IN USE CURSOR
      *  C2                       IN USE CURSOR
      *  C3                       IN USE CURSOR
      *  C4                       IN USE CURSOR
      *  C5                       IN USE CURSOR
      *  C6                       IN USE CURSOR
      *  DB-COD-ERROR             IN USE CHAR(10)
      *  DB-CUENTA                IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(13,0)
      *  DB-ESTADO-FINAL          IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(3,0)
      *  DB-ID-LOTE               IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(9,0)
      *  DB-ID-REGISTRO           IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  DB-LOTE-BUSQUEDA         IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(9,0)
      *  DB-MONTO                 IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(15,2)
      *  DB-MSG-ERROR             IN USE CHAR(200)
      *  DB-NUM-CREDITO           IN USE CHAR(20)
      *  DB-OP                NOT IN USE
      **********************************************************************

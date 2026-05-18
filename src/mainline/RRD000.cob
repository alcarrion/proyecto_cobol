       IDENTIFICATION DIVISION.
       PROGRAM-ID. RRD000.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 3.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 3 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 3 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 3 TIMES.
           05 SQL-PREC   PIC X OCCURS 3 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'SELECT ID_REGISTRO,DATOS_TX FR
      -    'OM TF01 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C1'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'SELECT ID_REGISTRO,DATOS_TX FR
      -    'OM TF02 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C2'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'SELECT ID_REGISTRO,DATOS_TX FR
      -    'OM TF03 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C3'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'SELECT ID_REGISTRO,DATOS_TX FR
      -    'OM TF04 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C4'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'SELECT ID_REGISTRO,DATOS_TX FR
      -    'OM TF05 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C5'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'SELECT ID_REGISTRO,DATOS_TX FR
      -    'OM TF06 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C6'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF01 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF02 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-8.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF03 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-9.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF04 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-10.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF05 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-11.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF06 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-12.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 62.
           05 SQL-STMT   PIC X(62) VALUE 'UPDATE TF01 SET ESTADO = ?,COD
      -    '_ERROR = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-13.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 62.
           05 SQL-STMT   PIC X(62) VALUE 'UPDATE TF02 SET ESTADO = ?,COD
      -    '_ERROR = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-14.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 62.
           05 SQL-STMT   PIC X(62) VALUE 'UPDATE TF03 SET ESTADO = ?,COD
      -    '_ERROR = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-15.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 62.
           05 SQL-STMT   PIC X(62) VALUE 'UPDATE TF04 SET ESTADO = ?,COD
      -    '_ERROR = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-16.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 62.
           05 SQL-STMT   PIC X(62) VALUE 'UPDATE TF05 SET ESTADO = ?,COD
      -    '_ERROR = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-17.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 62.
           05 SQL-STMT   PIC X(62) VALUE 'UPDATE TF06 SET ESTADO = ?,COD
      -    '_ERROR = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(1) COMP-3.
           05 SQL-VAR-0003  PIC S9(9) COMP-3.
           05 SQL-VAR-0004  PIC S9(3) COMP-3.
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
       01  WS-HOST-VARS.
           05 WS-ID-REG-SQL        PIC 9(09).
           05 WS-DATOS-TX-SQL      PIC X(500).
           05 WS-ESTADO-SQL        PIC 9(01).
           05 WS-ID-LOTE-SQL       PIC 9(09).
           05 WS-RETORNO-CORE      PIC 9(02).
           05 WS-REPLICA-SQL       PIC X(04).
      *    EXEC SQL END DECLARE SECTION END-EXEC.
       01  WS-CONT-LOTE            PIC 9(09) VALUE 0.
       01  WS-EOF-CURSOR           PIC X(01) VALUE 'N'.
       LINKAGE SECTION.
           COPY LKCIF.
       01  WS-TFFM-VARS.
           05 WS-ID-LOTE           PIC 9(09).
           05 WS-FILE-NAME         PIC X(120).
           05 WS-FASE              PIC X(02).
           05 WS-TYPE-UPDATE       PIC X(10).
           05 WS-REPLICA-ASIG      PIC X(04).
           05 WS-RETRY-COUNT       PIC 9(02).
       PROCEDURE DIVISION USING WS-TFFM-VARS,
                                LK-DATOS-TRANSACCION.
       000-PRINCIPAL.
           MOVE WS-ID-LOTE TO WS-ID-LOTE-SQL.
           MOVE WS-REPLICA-ASIG TO WS-REPLICA-SQL.
           PERFORM 100-ABRIR.
           PERFORM 200-FETCH.
           PERFORM UNTIL WS-EOF-CURSOR = 'Y'
               PERFORM 300-PROC
               PERFORM 200-FETCH
           END-PERFORM.
           PERFORM 400-CERRAR.
      *    EXEC SQL COMMIT END-EXEC.
           CALL 'OCSQLCMT' USING SQLCA END-CALL
                                   .
           MOVE 0 TO LK-COD-RETORNO.
           GOBACK.
       100-ABRIR.
           IF WS-REPLICA-ASIG = "TF01" PERFORM 101-O.
           IF WS-REPLICA-ASIG = "TF02" PERFORM 102-O.
           IF WS-REPLICA-ASIG = "TF03" PERFORM 103-O.
           IF WS-REPLICA-ASIG = "TF04" PERFORM 104-O.
           IF WS-REPLICA-ASIG = "TF05" PERFORM 105-O.
           IF WS-REPLICA-ASIG = "TF06" PERFORM 106-O.
       101-O.
      *    EXEC SQL DECLARE C1 CURSOR FOR SELECT ID_REGISTRO,
      *    DATOS_TX FROM TF01 WHERE ID_LOTE = :WS-ID-LOTE-SQL
      *    AND ESTADO = 2 END-EXEC EXEC SQL OPEN C1 END-EXEC.
      *                            EXEC SQL OPEN C1 END-EXEC.
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
           MOVE WS-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-0
                               SQLCA
           END-CALL
                                                            .
       102-O.
      *    EXEC SQL DECLARE C2 CURSOR FOR SELECT ID_REGISTRO,
      *    DATOS_TX FROM TF02 WHERE ID_LOTE = :WS-ID-LOTE-SQL
      *    AND ESTADO = 2 END-EXEC EXEC SQL OPEN C2 END-EXEC.
      *                            EXEC SQL OPEN C2 END-EXEC.
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
           MOVE WS-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-1
                               SQLCA
           END-CALL
                                                            .
       103-O.
      *    EXEC SQL DECLARE C3 CURSOR FOR SELECT ID_REGISTRO,
      *    DATOS_TX FROM TF03 WHERE ID_LOTE = :WS-ID-LOTE-SQL
      *    AND ESTADO = 2 END-EXEC EXEC SQL OPEN C3 END-EXEC.
      *                            EXEC SQL OPEN C3 END-EXEC.
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
           MOVE WS-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-2
                               SQLCA
           END-CALL
                                                            .
       104-O.
      *    EXEC SQL DECLARE C4 CURSOR FOR SELECT ID_REGISTRO,
      *    DATOS_TX FROM TF04 WHERE ID_LOTE = :WS-ID-LOTE-SQL
      *    AND ESTADO = 2 END-EXEC EXEC SQL OPEN C4 END-EXEC.
      *                            EXEC SQL OPEN C4 END-EXEC.
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
           MOVE WS-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-3
                               SQLCA
           END-CALL
                                                            .
       105-O.
      *    EXEC SQL DECLARE C5 CURSOR FOR SELECT ID_REGISTRO,
      *    DATOS_TX FROM TF05 WHERE ID_LOTE = :WS-ID-LOTE-SQL
      *    AND ESTADO = 2 END-EXEC EXEC SQL OPEN C5 END-EXEC.
      *                            EXEC SQL OPEN C5 END-EXEC.
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
           MOVE WS-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-4
                               SQLCA
           END-CALL
                                                            .
       106-O.
      *    EXEC SQL DECLARE C6 CURSOR FOR SELECT ID_REGISTRO,
      *    DATOS_TX FROM TF06 WHERE ID_LOTE = :WS-ID-LOTE-SQL
      *    AND ESTADO = 2 END-EXEC EXEC SQL OPEN C6 END-EXEC.
      *                            EXEC SQL OPEN C6 END-EXEC.
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
           MOVE WS-ID-LOTE-SQL TO SQL-VAR-0003
           CALL 'OCSQLOCU' USING SQL-STMT-5
                               SQLCA
           END-CALL
                                                            .
       200-FETCH.
           IF WS-REPLICA-ASIG = "TF01"
      *    EXEC SQL FETCH C1 INTO :WS-ID-REG-SQL, :WS-DATOS-TX-SQL
      *    END-EXEC.
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             WS-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 500 TO SQL-LEN(2)
           MOVE 2 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-ID-REG-SQL
                   .
           IF WS-REPLICA-ASIG = "TF02"
      *    EXEC SQL FETCH C2 INTO :WS-ID-REG-SQL, :WS-DATOS-TX-SQL
      *    END-EXEC.
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             WS-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 500 TO SQL-LEN(2)
           MOVE 2 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-ID-REG-SQL
                   .
           IF WS-REPLICA-ASIG = "TF03"
      *    EXEC SQL FETCH C3 INTO :WS-ID-REG-SQL, :WS-DATOS-TX-SQL
      *    END-EXEC.
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             WS-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 500 TO SQL-LEN(2)
           MOVE 2 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-ID-REG-SQL
                   .
           IF WS-REPLICA-ASIG = "TF04"
      *    EXEC SQL FETCH C4 INTO :WS-ID-REG-SQL, :WS-DATOS-TX-SQL
      *    END-EXEC.
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             WS-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 500 TO SQL-LEN(2)
           MOVE 2 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-ID-REG-SQL
                   .
           IF WS-REPLICA-ASIG = "TF05"
      *    EXEC SQL FETCH C5 INTO :WS-ID-REG-SQL, :WS-DATOS-TX-SQL
      *     END-EXEC.
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             WS-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 500 TO SQL-LEN(2)
           MOVE 2 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-4
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-ID-REG-SQL
                    .
           IF WS-REPLICA-ASIG = "TF06"
      *    EXEC SQL FETCH C6 INTO :WS-ID-REG-SQL, :WS-DATOS-TX-SQL
      *    END-EXEC.
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             WS-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 500 TO SQL-LEN(2)
           MOVE 2 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-5
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-ID-REG-SQL
                   .
           IF SQLCODE = 100 OR SQLCODE < 0 MOVE 'Y' TO WS-EOF-CURSOR.
       300-PROC.
           PERFORM 310-UP.
           INITIALIZE LK-DATOS-TRANSACCION.
           CALL "IN0000" USING WS-DATOS-TX-SQL, LK-DATOS-TRANSACCION.
           MOVE LK-COD-RETORNO TO WS-RETORNO-CORE.
           IF LK-COD-RETORNO = 0 MOVE 4 TO WS-ESTADO-SQL
           ELSE MOVE 7 TO WS-ESTADO-SQL END-IF.
           PERFORM 320-FIN.
           ADD 1 TO WS-CONT-LOTE.
       310-UP.
           IF WS-REPLICA-ASIG = "TF01"
      *    EXEC SQL UPDATE TF01 SET ESTADO = 3
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA
                                                      .
           IF WS-REPLICA-ASIG = "TF02"
      *    EXEC SQL UPDATE TF02 SET ESTADO = 3
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
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
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA
                                                      .
           IF WS-REPLICA-ASIG = "TF03"
      *    EXEC SQL UPDATE TF03 SET ESTADO = 3
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
           IF SQL-PREP OF SQL-STMT-8 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-8
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-8
                               SQLCA
                                                      .
           IF WS-REPLICA-ASIG = "TF04"
      *    EXEC SQL UPDATE TF04 SET ESTADO = 3
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
           IF SQL-PREP OF SQL-STMT-9 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-9
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-9
                               SQLCA
                                                      .
           IF WS-REPLICA-ASIG = "TF05"
      *    EXEC SQL UPDATE TF05 SET ESTADO = 3
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
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
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-10
                               SQLCA
                                                      .
           IF WS-REPLICA-ASIG = "TF06"
      *    EXEC SQL UPDATE TF06 SET ESTADO = 3
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
           IF SQL-PREP OF SQL-STMT-11 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-11
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-11
                               SQLCA
                                                      .
       320-FIN.
           IF WS-REPLICA-ASIG = "TF01"
      *    EXEC SQL UPDATE TF01 SET ESTADO = :WS-ESTADO-SQL,
      *    COD_ERROR = :WS-RETORNO-CORE
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
           IF SQL-PREP OF SQL-STMT-12 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-12
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-RETORNO-CORE
             TO SQL-VAR-0004
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-12
                               SQLCA
                                                      .
           IF WS-REPLICA-ASIG = "TF02"
      *    EXEC SQL UPDATE TF02 SET ESTADO = :WS-ESTADO-SQL,
      *    COD_ERROR = :WS-RETORNO-CORE
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
           IF SQL-PREP OF SQL-STMT-13 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-13
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-RETORNO-CORE
             TO SQL-VAR-0004
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-13
                               SQLCA
                                                      .
           IF WS-REPLICA-ASIG = "TF03"
      *    EXEC SQL UPDATE TF03 SET ESTADO = :WS-ESTADO-SQL,
      *    COD_ERROR = :WS-RETORNO-CORE
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
           IF SQL-PREP OF SQL-STMT-14 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-14
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-RETORNO-CORE
             TO SQL-VAR-0004
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-14
                               SQLCA
                                                      .
           IF WS-REPLICA-ASIG = "TF04"
      *    EXEC SQL UPDATE TF04 SET ESTADO = :WS-ESTADO-SQL,
      *    COD_ERROR = :WS-RETORNO-CORE
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
           IF SQL-PREP OF SQL-STMT-15 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-15
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-RETORNO-CORE
             TO SQL-VAR-0004
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-15
                               SQLCA
                                                      .
           IF WS-REPLICA-ASIG = "TF05"
      *    EXEC SQL UPDATE TF05 SET ESTADO = :WS-ESTADO-SQL,
      *    COD_ERROR = :WS-RETORNO-CORE
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
           IF SQL-PREP OF SQL-STMT-16 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-16
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-RETORNO-CORE
             TO SQL-VAR-0004
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-16
                               SQLCA
                                                      .
           IF WS-REPLICA-ASIG = "TF06"
      *    EXEC SQL UPDATE TF06 SET ESTADO = :WS-ESTADO-SQL,
      *    COD_ERROR = :WS-RETORNO-CORE
      *    WHERE ID_REGISTRO = :WS-ID-REG-SQL END-EXEC.
           IF SQL-PREP OF SQL-STMT-17 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(2)
               MOVE 2 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-17
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-RETORNO-CORE
             TO SQL-VAR-0004
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-17
                               SQLCA
                                                      .
       400-CERRAR.
      *    IF WS-REPLICA-ASIG = "TF01" EXEC SQL CLOSE C1 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA
                                                                 .
      *    IF WS-REPLICA-ASIG = "TF02" EXEC SQL CLOSE C2 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-1
                               SQLCA
                                                                 .
      *    IF WS-REPLICA-ASIG = "TF03" EXEC SQL CLOSE C3 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-2
                               SQLCA
                                                                 .
      *    IF WS-REPLICA-ASIG = "TF04" EXEC SQL CLOSE C4 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-3
                               SQLCA
                                                                 .
      *    IF WS-REPLICA-ASIG = "TF05" EXEC SQL CLOSE C5 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-4
                               SQLCA
                                                                 .
      *    IF WS-REPLICA-ASIG = "TF06" EXEC SQL CLOSE C6 END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-5
                               SQLCA
                                                                 .
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  C1                       IN USE CURSOR
      *  C2                       IN USE CURSOR
      *  C3                       IN USE CURSOR
      *  C4                       IN USE CURSOR
      *  C5                       IN USE CURSOR
      *  C6                       IN USE CURSOR
      *  WS-DATOS-TX-SQL          IN USE CHAR(500)
      *  WS-ESTADO-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(1,0)
      *  WS-HOST-VARS         NOT IN USE
      *  WS-HOST-VARS.WS-DATOS-TX-SQL NOT IN USE
      *  WS-HOST-VARS.WS-ESTADO-SQL NOT IN USE
      *  WS-HOST-VARS.WS-ID-LOTE-SQL NOT IN USE
      *  WS-HOST-VARS.WS-ID-REG-SQL NOT IN USE
      *  WS-HOST-VARS.WS-REPLICA-SQL NOT IN USE
      *  WS-HOST-VARS.WS-RETORNO-CORE NOT IN USE
      *  WS-ID-LOTE-SQL           IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(9,0)
      *  WS-ID-REG-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  WS-REPLICA-SQL       NOT IN USE
      *  WS-RETORNO-CORE          IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(3,0)
      **********************************************************************

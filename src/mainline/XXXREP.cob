       IDENTIFICATION DIVISION.
       PROGRAM-ID. XXXREP.
      *==========================================================
      * FASE 30: GENERACI�N DE ARCHIVO DE SALIDA CON RESULTADOS
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
       01  REG-SALIDA             PIC X(600).

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
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 60.
           05 SQL-STMT   PIC X(60) VALUE 'SELECT DATOS_TX,ESTADO,COD_ERR
      -    'OR FROM TF06 WHERE ID_LOTE = ?'.
           05 SQL-CNAME  PIC X(7) VALUE 'CUR_REP'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(1) COMP-3.
           05 SQL-VAR-0002  PIC S9(9) COMP-3.
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
       01  WS-TF06-REGS.
           05 WS-DATOS-TX-SQL      PIC X(500).
           05 WS-ESTADO-SQL        PIC 9(01).
           05 WS-COD-ERR-SQL       PIC X(04).
           05 WS-ID-LOTE-SQL       PIC 9(09).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-CONTROL-REP.
           05 WS-DIR-SALIDA        PIC X(250).
           05 WS-RUTA-FINAL        PIC X(250).
           05 WS-STATUS-TXT        PIC X(05).
           05 WS-LINEA-OUT         PIC X(600).
           05 WS-FS                PIC X(02).

       LINKAGE SECTION.
       COPY LKCIF.
       01  WS-TFFM-VARS.
           05 WS-ID-LOTE           PIC 9(09).
           05 WS-NOMBRE-ARCHIVO    PIC X(50).
           05 WS-FASE              PIC X(02).
           05 WS-ESTADO-REPLICA    PIC X(01).
           05 WS-TIPO-PROG         PIC X(03).
           05 WS-FECHA-SIST        PIC X(10).

       PROCEDURE DIVISION USING WS-TFFM-VARS, LK-DATOS-TRANSACCION.
       0000-PRINCIPAL.
      * Construccion segura de ruta de salida
           INITIALIZE WS-DIR-SALIDA
           STRING "C:\Users\dell\Desktop\proyecto_cobol\"
                  "banco\spool\Interfaces\BATCH-UPLOAD-S\"
                  "TRICKLE-FEED-REPORT\"

                  DELIMITED BY SIZE INTO WS-DIR-SALIDA

           INITIALIZE WS-RUTA-FINAL
           STRING WS-DIR-SALIDA     DELIMITED BY "  "
                  WS-NOMBRE-ARCHIVO DELIMITED BY SPACE
                  ".out"            DELIMITED BY SIZE
                  INTO WS-RUTA-FINAL

           OPEN OUTPUT ARCHIVO-SALIDA

           IF WS-FS NOT = "00"
               MOVE 99 TO LK-COD-RETORNO
               GOBACK
           END-IF

           MOVE WS-ID-LOTE TO WS-ID-LOTE-SQL
      *    EXEC SQL
      *        DECLARE CUR_REP CURSOR FOR
      *        SELECT DATOS_TX, ESTADO, COD_ERROR
      *        FROM TF06
      *        WHERE ID_LOTE = :WS-ID-LOTE-SQL
      *    END-EXEC

      *    EXEC SQL OPEN CUR_REP END-EXEC
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
           MOVE WS-ID-LOTE-SQL TO SQL-VAR-0002
           CALL 'OCSQLOCU' USING SQL-STMT-0
                               SQLCA
           END-CALL
           PERFORM 1000-ESCRIBIR-REPORTE UNTIL SQLCODE NOT = 0
      *    EXEC SQL CLOSE CUR_REP END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA
           CLOSE ARCHIVO-SALIDA

           MOVE 0 TO LK-COD-RETORNO
           GOBACK.

       1000-ESCRIBIR-REPORTE.
      *    EXEC SQL
      *        FETCH CUR_REP INTO :WS-DATOS-TX-SQL, :WS-ESTADO-SQL,
      *                           :WS-COD-ERR-SQL
      *    END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             WS-DATOS-TX-SQL
           MOVE 'X' TO SQL-TYPE(1)
           MOVE 500 TO SQL-LEN(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(2)
           MOVE 1 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             WS-COD-ERR-SQL
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 4 TO SQL-LEN(3)
           MOVE 3 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-ESTADO-SQL

           IF SQLCODE = 0
               IF WS-ESTADO-SQL = 4
                   MOVE "OK" TO WS-STATUS-TXT
               ELSE
                   MOVE "ERROR" TO WS-STATUS-TXT
               END-IF

               INITIALIZE WS-LINEA-OUT
               STRING WS-DATOS-TX-SQL   DELIMITED BY SPACE
                      "|" WS-STATUS-TXT DELIMITED BY SIZE
                      "|" WS-COD-ERR-SQL DELIMITED BY SIZE
                      INTO WS-LINEA-OUT

               WRITE REG-SALIDA FROM WS-LINEA-OUT
           END-IF.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  CUR_REP                  IN USE CURSOR
      *  WS-COD-ERR-SQL           IN USE CHAR(4)
      *  WS-DATOS-TX-SQL          IN USE CHAR(500)
      *  WS-ESTADO-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(1,0)
      *  WS-ID-LOTE-SQL           IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(9,0)
      *  WS-TF06-REGS         NOT IN USE
      *  WS-TF06-REGS.WS-COD-ERR-SQL NOT IN USE
      *  WS-TF06-REGS.WS-DATOS-TX-SQL NOT IN USE
      *  WS-TF06-REGS.WS-ESTADO-SQL NOT IN USE
      *  WS-TF06-REGS.WS-ID-LOTE-SQL NOT IN USE
      **********************************************************************

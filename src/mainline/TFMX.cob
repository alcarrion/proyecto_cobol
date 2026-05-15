       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFMX.
      *==========================================================
      * FASE 10: CARGA DE ARCHIVO PLANO A TABLA DE REPLICA TF06
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
       01  REG-LINEA-ENTRADA       PIC X(500).

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
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 57.
           05 SQL-STMT   PIC X(57) VALUE 'INSERT INTO TF06 (ID_LOTE,ESTA
      -    'DO,DATOS_TX) VALUES (?,?,?)'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(1) COMP-3.
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
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-CONTROL-FILE.
           05 WS-DIR-BASE          PIC X(200).
           05 WS-RUTA-COMPLETA     PIC X(250).
           05 WS-EOF               PIC X(01) VALUE 'N'.
           05 WS-CONT-REGS         PIC 9(05) VALUE 0.
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
      * Construccion segura de la ruta para no exceder columna 72
           INITIALIZE WS-DIR-BASE
           STRING "C:\Users\dell\Desktop\proyecto_cobol\"
                    "banco\spool\Interfaces\BATCH-UPLOAD-S\"


                  DELIMITED BY SIZE INTO WS-DIR-BASE

           INITIALIZE WS-RUTA-COMPLETA
           STRING WS-DIR-BASE       DELIMITED BY "  "
                  WS-NOMBRE-ARCHIVO DELIMITED BY SPACE
                  INTO WS-RUTA-COMPLETA

           OPEN INPUT ARCHIVO-ENTRADA

           IF WS-FS NOT = "00"
               MOVE 99 TO LK-COD-RETORNO
               STRING "ERR: NO SE PUDO ABRIR ARCHIVO. STATUS: " WS-FS
                      DELIMITED BY SIZE INTO LK-MENSAJE
               GOBACK
           END-IF

           MOVE WS-ID-LOTE TO WS-ID-LOTE-SQL
           PERFORM 1000-LEER-Y-CARGAR UNTIL WS-EOF = 'Y'

           CLOSE ARCHIVO-ENTRADA
      *    EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL

           MOVE 0 TO LK-COD-RETORNO
           GOBACK.

       1000-LEER-Y-CARGAR.
           READ ARCHIVO-ENTRADA
               AT END
                   MOVE 'Y' TO WS-EOF
               NOT AT END
                   MOVE REG-LINEA-ENTRADA TO WS-DATOS-TX-SQL
                   PERFORM 2000-INSERTAR-TF06
           END-READ.

       2000-INSERTAR-TF06.
      *    EXEC SQL
      *        INSERT INTO TF06 (ID_LOTE, ESTADO, DATOS_TX)
      *        VALUES (:WS-ID-LOTE-SQL, :WS-ESTADO-SQL,
      *        :WS-DATOS-TX-SQL)
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
               MOVE 1 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-DATOS-TX-SQL
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 500 TO SQL-LEN(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE-SQL
             TO SQL-VAR-0001
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .

           IF SQLCODE NOT = 0
               DISPLAY "ERROR AL INSERTAR REGISTRO EN TF06: " SQLCODE
               MOVE 99 TO LK-COD-RETORNO
               MOVE "Y" TO WS-EOF
           ELSE
               ADD 1 TO WS-CONT-REGS
           END-IF.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-DATOS-TX-SQL          IN USE CHAR(500)
      *  WS-ESTADO-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(1,0)
      *  WS-HOST-VARS-TFMX    NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-DATOS-TX-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-ESTADO-SQL NOT IN USE
      *  WS-HOST-VARS-TFMX.WS-ID-LOTE-SQL NOT IN USE
      *  WS-ID-LOTE-SQL           IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      **********************************************************************

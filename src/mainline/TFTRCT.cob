       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFTRCT.
      *==========================================================
      * MOTOR AUTOMATICO (DAEMON) - FORMATO SQB
      * Procesa TODOS los lotes y TODAS las fases en un solo ciclo
      *==========================================================

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 4.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 4 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 4 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 4 TIMES.
           05 SQL-PREC   PIC X OCCURS 4 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 126.
           05 SQL-STMT   PIC X(126) VALUE 'SELECT ID_LOTE,NOMBRE_ARCHIVO
      -    ',FASE,TIPO_PROG FROM TFFM WHERE FASE < ''40'' AND ESTADO_REP
      -    'LICA = ''R'' ORDER BY ID_LOTE ASC LIMIT 1'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 157.
           05 SQL-STMT   PIC X(157) VALUE 'UPDATE TFFM SET FASE = CASE W
      -    'HEN FASE = ''00'' THEN ''10'' WHEN FASE = ''10'' THEN ''20''
      -    ' WHEN FASE = ''20'' THEN ''30'' WHEN FASE = ''30'' THEN ''40
      -    ''' END WHERE ID_LOTE = ?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 39.
           05 SQL-STMT   PIC X(39) VALUE 'SELECT FASE FROM TFFM WHERE ID
      -    '_LOTE = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
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
       01  WS-TFFM-VARS.
           05 WS-ID-LOTE           PIC 9(09).
           05 WS-NOMBRE-ARCHIVO    PIC X(50).
           05 WS-FASE              PIC X(02).
           05 WS-ESTADO-REPLICA    PIC X(01).
           05 WS-TIPO-PROG         PIC X(03).
           05 WS-FECHA-SIST        PIC X(10).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-FECHA-CONTABLE       PIC X(10).
       01  WS-PROG-FASE            PIC X(08).
       01  WS-FLAGS.
           05 WS-HAY-LOTES-PEND    PIC X(01) VALUE 'S'.
           05 WS-LOTE-TERMINADO    PIC X(01) VALUE 'N'.

           COPY LKCIF.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY "--- INICIANDO DEMONIO DE PROCESAMIENTO ---"
           CALL "BNCR004" USING WS-FECHA-CONTABLE

      * BUCLE DE COLA: Mientras existan archivos registrados para hoy
           PERFORM UNTIL WS-HAY-LOTES-PEND = 'N'
               PERFORM 1000-BUSCAR-PROXIMO-LOTE

               IF SQLCODE = 0
                   PERFORM 2000-PROCESAR-LOTE-COMPLETO
               ELSE
                   MOVE 'N' TO WS-HAY-LOTES-PEND
               END-IF
           END-PERFORM.

           DISPLAY "--- COLA VACIA. DEMONIO FINALIZADO ---"
           GOBACK.

       1000-BUSCAR-PROXIMO-LOTE.
      * Busca el lote mas antiguo que no haya llegado a fase 40
      *    EXEC SQL
      *        SELECT ID_LOTE, NOMBRE_ARCHIVO, FASE, TIPO_PROG
      *        INTO :WS-ID-LOTE, :WS-NOMBRE-ARCHIVO,
      *             :WS-FASE, :WS-TIPO-PROG
      *        FROM TFFM
      *        WHERE FASE < '40'
      *          AND ESTADO_REPLICA = 'R'
      *        ORDER BY ID_LOTE ASC
      *        LIMIT 1
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-NOMBRE-ARCHIVO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 50 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-FASE
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 2 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WS-TIPO-PROG
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 3 TO SQL-LEN(4)
               MOVE 4 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO WS-ID-LOTE
                   .

      * LOG DE DEPURACION:
           IF SQLCODE = 0
               DISPLAY " [OK] LOTE ENCONTRADO: " WS-ID-LOTE
               DISPLAY "      ARCHIVO: " WS-NOMBRE-ARCHIVO
           ELSE
               IF SQLCODE = 100
                   DISPLAY " [!] NO HAY LOTES PENDIENTES (SQLCODE 100)"
               ELSE
                   DISPLAY " [X] ERROR SQL AL BUSCAR LOTE: " SQLCODE
               END-IF
           END-IF.

       2000-PROCESAR-LOTE-COMPLETO.
           DISPLAY ">>> TRABAJANDO EN LOTE: " WS-ID-LOTE
           MOVE 'N' TO WS-LOTE-TERMINADO

      * BUCLE DE FASES: No suelta el lote hasta que llega a 40
           PERFORM UNTIL WS-LOTE-TERMINADO = 'S'
               PERFORM 3000-EJECUTAR-FASE

               IF LK-COD-RETORNO = 0
                   PERFORM 4000-AVANZAR-FASE
                   IF WS-FASE = '40'
                       MOVE 'S' TO WS-LOTE-TERMINADO
                   END-IF
               ELSE
      * Si una fase falla, marcamos error y saltamos al siguiente archiv
                   DISPLAY "ERROR CRITICO EN LOTE " WS-ID-LOTE
                   MOVE 'S' TO WS-LOTE-TERMINADO
               END-IF
           END-PERFORM.

       3000-EJECUTAR-FASE.
           INITIALIZE WS-PROG-FASE
           EVALUATE WS-FASE
               WHEN '00' MOVE "SKIP"   TO WS-PROG-FASE
               WHEN '10' MOVE "TFMX"   TO WS-PROG-FASE
               WHEN '20' MOVE "RRD000" TO WS-PROG-FASE
               WHEN '30' MOVE "XXXREP" TO WS-PROG-FASE
           END-EVALUATE

           IF WS-PROG-FASE = "SKIP"
               MOVE 0 TO LK-COD-RETORNO
               DISPLAY " FASE 00 OK (VALIDACION)"
           ELSE
               DISPLAY " EJECUTANDO " WS-PROG-FASE "..."
               CALL WS-PROG-FASE USING WS-TFFM-VARS,
                                       LK-DATOS-TRANSACCION
           END-IF.

       4000-AVANZAR-FASE.
      *    EXEC SQL
      *        UPDATE TFFM
      *        SET FASE = CASE
      *            WHEN FASE = '00' THEN '10'
      *            WHEN FASE = '10' THEN '20'
      *            WHEN FASE = '20' THEN '30'
      *            WHEN FASE = '30' THEN '40'
      *        END
      *        WHERE ID_LOTE = :WS-ID-LOTE
      *    END-EXEC
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
           MOVE WS-ID-LOTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA

           IF SQLCODE = 0
      *        EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
      * Refrescamos la fase actual para el bucle
      *        EXEC SQL
      *            SELECT FASE INTO :WS-FASE FROM TFFM
      *            WHERE ID_LOTE = :WS-ID-LOTE
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-FASE
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 2 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
           END-IF.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  WS-ESTADO-REPLICA    NOT IN USE
      *  WS-FASE                  IN USE CHAR(2)
      *  WS-FECHA-SIST        NOT IN USE
      *  WS-ID-LOTE               IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  WS-NOMBRE-ARCHIVO        IN USE CHAR(50)
      *  WS-TFFM-VARS         NOT IN USE
      *  WS-TFFM-VARS.WS-ESTADO-REPLICA NOT IN USE
      *  WS-TFFM-VARS.WS-FASE NOT IN USE
      *  WS-TFFM-VARS.WS-FECHA-SIST NOT IN USE
      *  WS-TFFM-VARS.WS-ID-LOTE NOT IN USE
      *  WS-TFFM-VARS.WS-NOMBRE-ARCHIVO NOT IN USE
      *  WS-TFFM-VARS.WS-TIPO-PROG NOT IN USE
      *  WS-TIPO-PROG             IN USE CHAR(3)
      **********************************************************************

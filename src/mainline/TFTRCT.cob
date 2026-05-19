       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFTRCT.
      *==========================================================*
      * PROGRAMA PRECOMPILABLE: TFTRCT.sqb                       *
      * RESPONSABILIDAD: Orquestador y Máquina de Estados Core    *
      * CORRECCIÓN: Variable host puente para LK-TF-MENSAJE      *
      *==========================================================*
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
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 124.
           05 SQL-STMT   PIC X(124) VALUE 'SELECT ID_LOTE,FILE_NAME,FASE
      -    ',TYPE_UPDATE FROM tffm WHERE FASE < ''40'' AND ESTADO_REPLIC
      -    'A = ''R'' ORDER BY ID_LOTE ASC LIMIT 100'.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 108.
           05 SQL-STMT   PIC X(108) VALUE 'SELECT REPLICA_NO FROM tf_repl
      -    'icas WHERE STATUS = ''L'' ORDER BY REPLICA_NO ASC LIMIT 10'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 56.
           05 SQL-STMT   PIC X(56) VALUE 'UPDATE tf_replicas SET STATUS 
      -    '= ''O'' WHERE REPLICA_NO = ?'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE tffm SET REPLICA_NO = ?
      -    ' WHERE ID_LOTE = ?'.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 42.
           05 SQL-STMT   PIC X(42) VALUE 'UPDATE tffm SET FASE = ? WHERE
      -    ' ID_LOTE = ?'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 56.
           05 SQL-STMT   PIC X(56) VALUE 'UPDATE tf_replicas SET STATUS 
      -    '= ''L'' WHERE REPLICA_NO = ?'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 88.
           05 SQL-STMT   PIC X(88) VALUE 'UPDATE tffm SET FASE = ''99'',
      -    'ERROR_CODE = ''ERR-BATCH'',ERROR_MESSAGE = ? WHERE ID_LOTE =
      -    ' ?'.
      **********************************************************************
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 56.
           05 SQL-STMT   PIC X(56) VALUE 'UPDATE tf_replicas SET STATUS 
      -    '= ''L'' WHERE REPLICA_NO = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************
      * Area de comunicacion de SQL
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
           05 WS-FILE-NAME         PIC X(120).
           05 WS-FASE              PIC X(02).
           05 WS-TYPE-UPDATE       PIC X(10).
           05 WS-REPLICA-ASIG      PIC X(04).
           05 WS-RETRY-COUNT       PIC 9(02).
       01  WS-REP-DISPONIBLE       PIC X(04).
       01  WS-PROG-FASE            PIC X(08).
       01  WS-FECHA-CONTABLE       PIC X(10).
      * CORRECCIÓN: Variable Host asignada para el precompilador ESQL
       01  DB-LK-MENSAJE           PIC X(200).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-FLAGS.
           05 WS-HAY-LOTES-PEND    PIC X(01) VALUE 'S'.
           05 WS-LOTE-TERMINADO    PIC X(01) VALUE 'N'.

       01  WS-BALANCEO-CARGA.
           05 WS-REPLICA-ACTUAL    PIC 9(02) VALUE 1.
           05 WS-REPLICA-ARRAY     PIC X(04) OCCURS 10 TIMES VALUE SPACES.
           05 WS-REPLICA-COUNT     PIC 9(02) VALUE 0.
           05 WS-REPLICA-IDX       PIC 9(02) VALUE 0.
           05 WS-LOTES-PROCESADOS  PIC 9(09) VALUE 0.

           COPY LKTF.

       PROCEDURE DIVISION.
       000-MAIN.
           DISPLAY " "
           DISPLAY " [CORE ENGINE] INICIANDO ORQUESTADOR BATCH 3.0 ---"
           CALL "BNCR004" USING WS-FECHA-CONTABLE.

           PERFORM 150-CARGAR-REPLICAS-DISPONIBLES

           PERFORM UNTIL WS-HAY-LOTES-PEND = 'N'
               PERFORM 100-BUSCAR-LOTE
               IF SQLCODE = 0
                   IF WS-REPLICA-COUNT > 0
                       PERFORM 200-BUSCAR-REPLICA
                       IF WS-REP-DISPONIBLE NOT = SPACES
                           PERFORM 300-PROCESAR-LOTE
                       END-IF
                   ELSE
                       DISPLAY "AVISO: NO HAY RÉPLICAS DISPONIBLES."
                       " REINTENTANDO..."
                       MOVE 'N' TO WS-HAY-LOTES-PEND
                   END-IF
               ELSE
                   MOVE 'N' TO WS-HAY-LOTES-PEND
               END-IF
           END-PERFORM.

           DISPLAY "--- [CORE ENGINE] MÁQUINA DE ESTADOS EN REPOSO ---"
           DISPLAY "TOTAL LOTES PROCESADOS: " WS-LOTES-PROCESADOS
           GOBACK.

       150-CARGAR-REPLICAS-DISPONIBLES.
      * Carga inicial de todas las réplicas disponibles
           MOVE 0 TO WS-REPLICA-COUNT
           MOVE 1 TO WS-REPLICA-ACTUAL
           MOVE 0 TO WS-LOTES-PROCESADOS

           PERFORM VARYING WS-REPLICA-IDX FROM 1 BY 1
               UNTIL WS-REPLICA-IDX > 6
               MOVE "TF0" TO WS-REPLICA-ARRAY(WS-REPLICA-IDX)
               STRING WS-REPLICA-ARRAY(WS-REPLICA-IDX)
                   DELIMITED BY SPACE
                   WS-REPLICA-IDX DELIMITED BY SIZE
                   INTO WS-REPLICA-ARRAY(WS-REPLICA-IDX)
               END-STRING
               ADD 1 TO WS-REPLICA-COUNT
           END-PERFORM

           DISPLAY "RÉPLICAS DISPONIBLES CARGADAS: " WS-REPLICA-COUNT.

       100-BUSCAR-LOTE.
      *    EXEC SQL
      *        SELECT ID_LOTE, FILE_NAME, FASE, TYPE_UPDATE
      *        INTO :WS-ID-LOTE, :WS-FILE-NAME, :WS-FASE,
      *             :WS-TYPE-UPDATE
      *        FROM tffm
      *        WHERE FASE < '40' AND ESTADO_REPLICA = 'R'
      *        ORDER BY ID_LOTE ASC LIMIT 1
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-FILE-NAME
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 120 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 WS-FASE
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 2 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 WS-TYPE-UPDATE
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 10 TO SQL-LEN(4)
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

       200-BUSCAR-REPLICA.
           MOVE SPACES TO WS-REP-DISPONIBLE.

      * Implementar selección round-robin de réplicas
           IF WS-REPLICA-ACTUAL > WS-REPLICA-COUNT
               MOVE 1 TO WS-REPLICA-ACTUAL
           END-IF

           MOVE WS-REPLICA-ARRAY(WS-REPLICA-ACTUAL)
               TO WS-REP-DISPONIBLE

           ADD 1 TO WS-REPLICA-ACTUAL

           IF WS-REP-DISPONIBLE NOT = SPACES
      *        Marcar réplica como ocupada
               IF SQL-PREP OF SQL-STMT-2 = 'N'
                   SET SQL-ADDR(1) TO ADDRESS OF
                     WS-REP-DISPONIBLE
                   MOVE 'X' TO SQL-TYPE(1)
                   MOVE 4 TO SQL-LEN(1)
                   MOVE 1 TO SQL-COUNT
                   CALL 'OCSQLPRE' USING SQLV
                                       SQL-STMT-2
                                       SQLCA
                   SET SQL-HCONN OF SQLCA TO NULL
               END-IF
               CALL 'OCSQLEXE' USING SQL-STMT-2
                                   SQLCA

      *        Asignar réplica al lote
               IF SQL-PREP OF SQL-STMT-3 = 'N'
                   SET SQL-ADDR(1) TO ADDRESS OF
                     WS-REP-DISPONIBLE
                   MOVE 'X' TO SQL-TYPE(1)
                   MOVE 4 TO SQL-LEN(1)
                   SET SQL-ADDR(2) TO ADDRESS OF
                     SQL-VAR-0001
                   MOVE '3' TO SQL-TYPE(2)
                   MOVE 5 TO SQL-LEN(2)
                   MOVE X'00' TO SQL-PREC(2)
                   MOVE 2 TO SQL-COUNT
                   CALL 'OCSQLPRE' USING SQLV
                                       SQL-STMT-3
                                       SQLCA
                   SET SQL-HCONN OF SQLCA TO NULL
               END-IF
               MOVE WS-ID-LOTE
                 TO SQL-VAR-0001
               CALL 'OCSQLEXE' USING SQL-STMT-3
                                   SQLCA

               CALL 'OCSQLCMT' USING SQLCA END-CALL
           END-IF.

       300-PROCESAR-LOTE.
           ADD 1 TO WS-LOTES-PROCESADOS
           MOVE 'N' TO WS-LOTE-TERMINADO.
           PERFORM UNTIL WS-LOTE-TERMINADO = 'S'
               PERFORM 400-EVALUAR-FASE

               IF LK-TF-COD-RETORNO = 0
                   PERFORM 500-AVANZAR-FASE
                   IF WS-FASE = '40'
                      MOVE 'S' TO WS-LOTE-TERMINADO
                   END-IF
               ELSE
                   DISPLAY " CRÍTICO: FALLO EN FASE " WS-FASE
                           " PARA EL LOTE " WS-ID-LOTE
                   PERFORM 600-EVACUAR-LOTE-ERRONEO
                   MOVE 'S' TO WS-LOTE-TERMINADO
               END-IF
           END-PERFORM.

       400-EVALUAR-FASE.
           MOVE "SKIP" TO WS-PROG-FASE.

           IF WS-FASE = '00' MOVE "TFMX"     TO WS-PROG-FASE.
           IF WS-FASE = '10' MOVE "TFBATFIN" TO WS-PROG-FASE.
           IF WS-FASE = '20' MOVE "XXXREP"   TO WS-PROG-FASE.

           IF WS-PROG-FASE NOT = "SKIP"
               MOVE WS-REP-DISPONIBLE TO WS-REPLICA-ASIG
               CALL WS-PROG-FASE USING WS-TFFM-VARS,
                                       LK-TRICKLE-FEED-INTERFACE
           ELSE
               MOVE 0 TO LK-TF-COD-RETORNO
           END-IF.

       500-AVANZAR-FASE.
           IF WS-FASE = '00'      MOVE '10' TO WS-FASE
           ELSE IF WS-FASE = '10' MOVE '20' TO WS-FASE
           ELSE IF WS-FASE = '20' MOVE '30' TO WS-FASE
           ELSE IF WS-FASE = '30' MOVE '40' TO WS-FASE.

      *    EXEC SQL
      *        UPDATE tffm SET FASE = :WS-FASE
      *        WHERE ID_LOTE = :WS-ID-LOTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-4 = 'N'
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
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ID-LOTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
                   .

           IF WS-FASE = '40'
      *        EXEC SQL
      *            UPDATE tf_replicas SET STATUS = 'L'
      *            WHERE REPLICA_NO = :WS-REP-DISPONIBLE
      *        END-EXEC
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-REP-DISPONIBLE
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 4 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
           END-IF.
      *    EXEC SQL COMMIT END-EXEC.
           CALL 'OCSQLCMT' USING SQLCA END-CALL
                                   .

       600-EVACUAR-LOTE-ERRONEO.
      * Mover el valor del copybook a la variable host aceptada por el S
           MOVE LK-TF-MENSAJE TO DB-LK-MENSAJE

      * Forzamos el lote a fase 99 (Abortado/Error) y liberamos la répl
      *    EXEC SQL
      *        UPDATE tffm
      *        SET FASE = '99',
      *            ERROR_CODE = 'ERR-BATCH',
      *            ERROR_MESSAGE = :DB-LK-MENSAJE
      *        WHERE ID_LOTE = :WS-ID-LOTE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-LK-MENSAJE
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 200 TO SQL-LEN(1)
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
           MOVE WS-ID-LOTE
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA
                   .

      *    EXEC SQL
      *        UPDATE tf_replicas SET STATUS = 'L'
      *        WHERE REPLICA_NO = :WS-REP-DISPONIBLE
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-7 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-REP-DISPONIBLE
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 4 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-7
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA
                   .
      *    EXEC SQL COMMIT END-EXEC.
           CALL 'OCSQLCMT' USING SQLCA END-CALL
                                   .
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-LK-MENSAJE            IN USE CHAR(200)
      *  WS-FASE                  IN USE CHAR(2)
      *  WS-FECHA-CONTABLE    NOT IN USE
      *  WS-FILE-NAME             IN USE CHAR(120)
      *  WS-ID-LOTE               IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  WS-PROG-FASE         NOT IN USE
      *  WS-REP-DISPONIBLE        IN USE CHAR(4)
      *  WS-REPLICA-ASIG      NOT IN USE
      *  WS-RETRY-COUNT       NOT IN USE
      *  WS-TFFM-VARS         NOT IN USE
      *  WS-TFFM-VARS.WS-FASE NOT IN USE
      *  WS-TFFM-VARS.WS-FILE-NAME NOT IN USE
      *  WS-TFFM-VARS.WS-ID-LOTE NOT IN USE
      *  WS-TFFM-VARS.WS-REPLICA-ASIG NOT IN USE
      *  WS-TFFM-VARS.WS-RETRY-COUNT NOT IN USE
      *  WS-TFFM-VARS.WS-TYPE-UPDATE NOT IN USE
      *  WS-TYPE-UPDATE           IN USE CHAR(10)
      **********************************************************************

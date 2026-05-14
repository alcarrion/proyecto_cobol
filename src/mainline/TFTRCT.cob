       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFTRCT.
      *==========================================================
      * ORQUESTADOR PRINCIPAL (tfdrmain) - FORMATO SQB
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
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 123.
           05 SQL-STMT   PIC X(123) VALUE 'SELECT ID_LOTE,NOMBRE_ARCHIVO
      -    ',FASE,TIPO_PROG FROM TFFM WHERE FASE < ''40'' AND ESTADO_REP
      -    'LICA = ''R'' ORDER BY FASE ASC LIMIT 1'.
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

      * Variables de Host para la tabla TFFM
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-TFFM-VARS.
           05 WS-ID-LOTE           PIC 9(09).
           05 WS-NOMBRE-ARCHIVO    PIC X(50).
           05 WS-FASE              PIC X(02).
           05 WS-ESTADO-REPLICA    PIC X(01).
           05 WS-TIPO-PROG         PIC X(03).
           05 WS-FECHA-SIST        PIC X(10).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

      * Variables de control interno
       01  WS-FECHA-CONTABLE       PIC X(10).
       01  WS-PROG-FASE            PIC X(08).
       01  WS-STATUS-SYS           PIC X(01) VALUE 'R'.

      * Estructura compartida (Definida en WORKING para el llamador)
           COPY LKCIF.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
      * 1. Validar la fecha de operacion con bncr004
           PERFORM 1000-VALIDAR-SISTEMA

      * 2. Bucle principal de orquestacion
           IF WS-STATUS-SYS = 'R'
               PERFORM 2000-ORQUESTAR-LOTES
           END-IF.

           GOBACK.

       1000-VALIDAR-SISTEMA.
           CALL "BNCR004" USING WS-FECHA-CONTABLE.
           DISPLAY "FECHA CONTABLE DEL SISTEMA: " WS-FECHA-CONTABLE.

       2000-ORQUESTAR-LOTES.
      *    EXEC SQL
      *        SELECT ID_LOTE, NOMBRE_ARCHIVO, FASE, TIPO_PROG
      *        INTO :WS-ID-LOTE, :WS-NOMBRE-ARCHIVO,
      *        :WS-FASE, :WS-TIPO-PROG
      *        FROM TFFM
      *        WHERE FASE < '40' AND ESTADO_REPLICA = 'R'
      *        ORDER BY FASE ASC
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

           IF SQLCODE = 0
               DISPLAY "PROCESANDO LOTE: " WS-ID-LOTE " FASE: " WS-FASE
               PERFORM 3000-EJECUTAR-FASE
           ELSE
               DISPLAY "NO HAY LOTES PENDIENTES PARA PROCESAR."
           END-IF.

       3000-EJECUTAR-FASE.
           INITIALIZE WS-PROG-FASE.
           EVALUATE WS-FASE
               WHEN '00'
                   MOVE "SKIP"     TO WS-PROG-FASE
               WHEN '10'
                   MOVE "TFMX"     TO WS-PROG-FASE
               WHEN '20'
                   MOVE "RRD000"   TO WS-PROG-FASE
               WHEN '30'
                   MOVE "XXXREP"   TO WS-PROG-FASE
           END-EVALUATE.

           IF WS-PROG-FASE = "SKIP"
               MOVE 0 TO LK-COD-RETORNO
               DISPLAY "FASE 00 COMPLETADA LOGICAMENTE."
           ELSE
      * Dividimos el CALL en dos lineas para evitar margen de col 72
               CALL WS-PROG-FASE
                   USING WS-TFFM-VARS, LK-DATOS-TRANSACCION
           END-IF.

           IF LK-COD-RETORNO = 0
               PERFORM 4000-AVANZAR-FASE
           ELSE
               DISPLAY "ERROR EN FASE " WS-FASE " RETORNO: "
                       LK-COD-RETORNO
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
      *    END-EXEC.
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
                   .

           IF SQLCODE = 0
      *        EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
               DISPLAY "LOTE ACTUALIZADO EXITOSAMENTE."
           ELSE
               DISPLAY "ERROR AL ACTUALIZAR FASE: " SQLCODE
      *        EXEC SQL ROLLBACK END-EXEC
           CALL 'OCSQLRBK' USING SQLCA END-CALL
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

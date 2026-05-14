       IDENTIFICATION DIVISION.
       PROGRAM-ID. RRD000.
      *==========================================================
      * MOTOR DE NEGOCIO - PROCESAMIENTO REGISTRO A REGISTRO
      *==========================================================

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
      -    'OM TF06 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(14) VALUE 'CUR_PENDIENTES'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE TF06 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-2.
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

      * Variables de Host para MySQL (Mapeo directo con BD)
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-HOST-VARS.
           05 WS-ID-REG-SQL        PIC 9(09).
           05 WS-DATOS-TX-SQL      PIC X(500).
           05 WS-ESTADO-SQL        PIC 9(01).
           05 WS-ID-LOTE-SQL       PIC 9(09).
           05 WS-RETORNO-CORE      PIC 9(02).
      *EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-CONT-LOTE            PIC 9(05) VALUE 0.

      * Constantes de Estado
       01  WS-CONSTANTES.
           05 ST-PENDIENTE         PIC 9(01) VALUE 2.
           05 ST-PROCESANDO        PIC 9(01) VALUE 3.
           05 ST-EXITO             PIC 9(01) VALUE 4.
           05 ST-ERROR             PIC 9(01) VALUE 7.
           05 ST-CRITICO           PIC 9(01) VALUE 8.

       LINKAGE SECTION.
      * El COPY LKCIF debe estar aquí para que LK-DATOS-TRANSACCION sea
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
           MOVE WS-ID-LOTE TO WS-ID-LOTE-SQL.

      * 1. Declarar cursor para registros pendientes de este lote
      *    EXEC SQL
      *        DECLARE CUR_PENDIENTES CURSOR FOR
      *        SELECT ID_REGISTRO, DATOS_TX
      *        FROM TF06
      *        WHERE ID_LOTE = :WS-ID-LOTE-SQL AND ESTADO = 2
      *    END-EXEC.
                   .

      *    EXEC SQL OPEN CUR_PENDIENTES END-EXEC.
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

           PERFORM 1000-PROCESAR-REGISTRO UNTIL SQLCODE NOT = 0.

      *    EXEC SQL CLOSE CUR_PENDIENTES END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA
                                                 .

           MOVE 0 TO LK-COD-RETORNO.
           GOBACK.

       1000-PROCESAR-REGISTRO.
      * Obtener siguiente registro pendiente
      *    EXEC SQL
      *        FETCH CUR_PENDIENTES INTO :WS-ID-REG-SQL,
      *        :WS-DATOS-TX-SQL
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

           IF SQLCODE = 0
      * A. Marcar como "Procesando" (Estado 3)
               PERFORM 2000-CAMBIAR-ESTADO-PROCESANDO

      * B. Invocar lógica del Core (Módulos Online)
               PERFORM 3000-LLAMAR-NEGOCIO

      * C. Actualizar con resultado final (4 o 7)
               PERFORM 4000-ACTUALIZAR-RESULTADO

      * D. Control de Lote (Trickle Feed)
               ADD 1 TO WS-CONT-LOTE
               IF WS-CONT-LOTE >= 500
      *            EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
                   MOVE 0 TO WS-CONT-LOTE
                   DISPLAY "LOTE DE 500 REGISTROS PROCESADO - COMMIT OK"
               END-IF
           END-IF.

       2000-CAMBIAR-ESTADO-PROCESANDO.
      *    EXEC SQL
      *        UPDATE TF06 SET ESTADO = 3
      *        WHERE ID_REGISTRO = :WS-ID-REG-SQL
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
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
                   .

       3000-LLAMAR-NEGOCIO.
           INITIALIZE LK-DATOS-TRANSACCION.
      * Invocación a lógica de negocio según el tipo de programa
           EVALUATE WS-TIPO-PROG
               WHEN "EPG"
                   CALL "IN0000" USING WS-DATOS-TX-SQL,
                   LK-DATOS-TRANSACCION
               WHEN "TRF"
                   CALL "BR0000" USING WS-DATOS-TX-SQL,
                   LK-DATOS-TRANSACCION
           END-EVALUATE.

       4000-ACTUALIZAR-RESULTADO.
      * Sincronizar código de retorno con variable de host para SQL
           MOVE LK-COD-RETORNO TO WS-RETORNO-CORE.

      * Definir estado final basado en el retorno del Core
           IF LK-COD-RETORNO = 0
               MOVE 4 TO WS-ESTADO-SQL
           ELSE
               MOVE 7 TO WS-ESTADO-SQL
           END-IF.

      *    EXEC SQL
      *        UPDATE TF06
      *        SET ESTADO = :WS-ESTADO-SQL,
      *            COD_ERROR = :WS-RETORNO-CORE
      *        WHERE ID_REGISTRO = :WS-ID-REG-SQL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
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
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE WS-ESTADO-SQL
             TO SQL-VAR-0002
           MOVE WS-RETORNO-CORE
             TO SQL-VAR-0004
           MOVE WS-ID-REG-SQL
             TO SQL-VAR-0001
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  CUR_PENDIENTES           IN USE CURSOR
      *  WS-DATOS-TX-SQL          IN USE CHAR(500)
      *  WS-ESTADO-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(1,0)
      *  WS-HOST-VARS         NOT IN USE
      *  WS-HOST-VARS.WS-DATOS-TX-SQL NOT IN USE
      *  WS-HOST-VARS.WS-ESTADO-SQL NOT IN USE
      *  WS-HOST-VARS.WS-ID-LOTE-SQL NOT IN USE
      *  WS-HOST-VARS.WS-ID-REG-SQL NOT IN USE
      *  WS-HOST-VARS.WS-RETORNO-CORE NOT IN USE
      *  WS-ID-LOTE-SQL           IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(9,0)
      *  WS-ID-REG-SQL            IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  WS-RETORNO-CORE          IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(3,0)
      **********************************************************************

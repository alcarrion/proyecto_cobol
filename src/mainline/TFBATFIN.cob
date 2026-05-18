       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFBATFIN.
      *================================================================*
      * PROGRAMA: TFBATFIN.sqb                                         *
      * RESPONSABILIDAD: Procesador Core Contable Masivo Asíncrono
      * OPTIMIZACIÓN: Extracción de DATOS_TX desde réplicas y parseo
      * posicional dinámico para subprogramas activos (004 y 005)
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
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 8.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 8 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 8 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 8 TIMES.
           05 SQL-PREC   PIC X OCCURS 8 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 130.
           05 SQL-STMT   PIC X(130) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CU
      -    'ENTA,NUM_CREDITO,MONTO,ID_TRANSACCION,TYPE_UPDATE,DATOS_TX F
      -    'ROM tf01 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C1'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE tf01 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE tf01 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 130.
           05 SQL-STMT   PIC X(130) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CU
      -    'ENTA,NUM_CREDITO,MONTO,ID_TRANSACCION,TYPE_UPDATE,DATOS_TX F
      -    'ROM tf02 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C2'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE tf02 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE tf02 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 130.
           05 SQL-STMT   PIC X(130) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CU
      -    'ENTA,NUM_CREDITO,MONTO,ID_TRANSACCION,TYPE_UPDATE,DATOS_TX F
      -    'ROM tf03 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C3'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE tf03 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-8.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE tf03 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-9.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 130.
           05 SQL-STMT   PIC X(130) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CU
      -    'ENTA,NUM_CREDITO,MONTO,ID_TRANSACCION,TYPE_UPDATE,DATOS_TX F
      -    'ROM tf04 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C4'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-10.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE tf04 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-11.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE tf04 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-12.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 130.
           05 SQL-STMT   PIC X(130) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CU
      -    'ENTA,NUM_CREDITO,MONTO,ID_TRANSACCION,TYPE_UPDATE,DATOS_TX F
      -    'ROM tf05 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C5'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-13.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE tf05 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-14.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE tf05 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-15.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 130.
           05 SQL-STMT   PIC X(130) VALUE 'SELECT ID_REGISTRO,ID_LOTE,CU
      -    'ENTA,NUM_CREDITO,MONTO,ID_TRANSACCION,TYPE_UPDATE,DATOS_TX F
      -    'ROM tf06 WHERE ID_LOTE = ? AND ESTADO = 2'.
           05 SQL-CNAME  PIC X(2) VALUE 'C6'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-16.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 48.
           05 SQL-STMT   PIC X(48) VALUE 'UPDATE tf06 SET ESTADO = 3 WHE
      -    'RE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-17.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 4.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 80.
           05 SQL-STMT   PIC X(80) VALUE 'UPDATE tf06 SET ESTADO = ?,COD
      -    '_ERROR = ?,ERROR_MESSAGE = ? WHERE ID_REGISTRO = ?'.
      **********************************************************************
       01 SQL-STMT-18.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 233.
           05 SQL-STMT   PIC X(233) VALUE 'SELECT COUNT(*),c.TIPO_DOC,c.
      -    'DOC_CLIENTE,cc.ESTADO_CUENTA,cc.SALDO_ACTUAL FROM ctactes cc
      -    ' INNER JOIN clientes c ON cc.ID_CLIENTE = c.ID_CLIENTE WHERE
      -    ' cc.ID_CUENTA = ? GROUP BY c.TIPO_DOC,c.DOC_CLIENTE,cc.ESTAD
      -    'O_CUENTA,cc.SALDO_ACTUAL'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(9) COMP-3.
           05 SQL-VAR-0003  PIC S9(9) COMP-3.
           05 SQL-VAR-0004  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0005  PIC S9(9) COMP-3.
           05 SQL-VAR-0006  PIC S9(3) COMP-3.
           05 SQL-VAR-0007  PIC S9(1) COMP-3.
           05 SQL-VAR-0008  PIC S9(13)V9(2) COMP-3.
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

      * Variables de Control de Cursores de Réplicas
       01  DB-ID-REGISTRO          PIC 9(09).
       01  DB-ID-LOTE              PIC 9(09).
       01  DB-CUENTA               PIC 9(09).
       01  DB-NUM-CREDITO          PIC X(20).
       01  DB-MONTO                PIC S9(13)V99.
       01  DB-TXID                 PIC X(40).
       01  DB-TYPE-UPD             PIC X(10).
       01  DB-LOTE-BUSQUEDA        PIC 9(09).
       01  DB-ESTADO-FINAL         PIC 9(02).
       01  DB-COD-ERROR            PIC X(10).
       01  DB-MSG-ERROR            PIC X(200).

      * MEJORA: Variable host SQL para capturar la trama cruda
       01  DB-DATOS-TX             PIC X(500).

      * Variables para Consulta de Cobertura e Identidad Bancaria
       01  DB-VALIDACION-MASTER.
           05 MS-COUNT-MATCH       PIC 9(01).
           05 MS-TIPO-DOC          PIC X(03).
           05 MS-DOC-CLIENTE       PIC X(12).
           05 MS-ESTADO-CTA        PIC X(01).
           05 MS-SALDO-ACTUAL      PIC S9(13)V99.

      *    EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-FLAGS.
           05 WS-EOF-CURSOR        PIC X(01) VALUE 'N'.

      * Variables Locales para Cálculo de Longitud Exoficial
       01  WS-AUDITORIA-INTERNA.
           05 WS-ESPACIOS-DERECHA  PIC 9(02) VALUE 0.
           05 WS-LONGITUD-REAL     PIC 9(02) VALUE 0.

      * Registro de Comunicación de Interfaz con tkin01
       01  REG-CTA.
           05 CTA-NRO-CUENTA       PIC 9(09).
           05 CTA-NUM-CREDITO      PIC X(20).
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

       PROCEDURE DIVISION USING WS-TFFM-VARS,
                                LK-TRICKLE-FEED-INTERFACE.

       0000-PRINCIPAL.

           MOVE "N" TO WS-EOF-CURSOR
           MOVE 00 TO LK-TF-COD-RETORNO
           MOVE WS-ID-LOTE TO DB-LOTE-BUSQUEDA.

      * El orquestador deriva dinámicamente al hilo de la réplica asig
           EVALUATE WS-REPLICA-ASIG
               WHEN "TF01"
                   PERFORM 1000-PROCESAR-TF01

               WHEN "TF02"
                   PERFORM 2000-PROCESAR-TF02

               WHEN "TF03"
                   PERFORM 3000-PROCESAR-TF03

               WHEN "TF04"
                   PERFORM 4000-PROCESAR-TF04

               WHEN "TF05"
                   PERFORM 5000-PROCESAR-TF05

               WHEN "TF06"
                   PERFORM 6000-PROCESAR-TF06
           END-EVALUATE.

           GOBACK.

      *================================================================*
      * CONTROLADORES DE CURSORES ASIGNADOS (Fase Extracción con DATOS_
      *================================================================*

       1000-PROCESAR-TF01.

      *    EXEC SQL
      *        DECLARE C1 CURSOR FOR
      *        SELECT ID_REGISTRO,
      *               ID_LOTE,
      *               CUENTA,
      *               NUM_CREDITO,
      *               MONTO,
      *               ID_TRANSACCION,
      *               TYPE_UPDATE,
      *               DATOS_TX
      *        FROM tf01
      *        WHERE ID_LOTE = :DB-LOTE-BUSQUEDA
      *          AND ESTADO = 2
      *    END-EXEC.
                   .

      *    EXEC SQL
      *        OPEN C1
      *    END-EXEC.
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

      *        EXEC SQL
      *            FETCH C1 INTO
      *                :DB-ID-REGISTRO,
      *                :DB-ID-LOTE,
      *                :DB-CUENTA,
      *                :DB-NUM-CREDITO,
      *                :DB-MONTO,
      *                :DB-TXID,
      *                :DB-TYPE-UPD,
      *                :DB-DATOS-TX
      *        END-EXEC
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
           MOVE 5 TO SQL-LEN(3)
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
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-TXID
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 40 TO SQL-LEN(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             DB-TYPE-UPD
           MOVE 'X' TO SQL-TYPE(7)
           MOVE 10 TO SQL-LEN(7)
           SET SQL-ADDR(8) TO ADDRESS OF
             DB-DATOS-TX
           MOVE 'X' TO SQL-TYPE(8)
           MOVE 500 TO SQL-LEN(8)
           MOVE 8 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO

               IF SQLCODE = 0

      *            EXEC SQL
      *                UPDATE tf01
      *                   SET ESTADO = 3
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
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

                   PERFORM 9000-EJECUTAR-CORE-REGLAS

      *            EXEC SQL
      *                UPDATE tf01
      *                   SET ESTADO       = :DB-ESTADO-FINAL,
      *                       COD_ERROR    = :DB-COD-ERROR,
      *                       ERROR_MESSAGE = :DB-MSG-ERROR
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
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

      *    EXEC SQL
      *        CLOSE C1
      *    END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA
                   .

       2000-PROCESAR-TF02.

      *    EXEC SQL
      *        DECLARE C2 CURSOR FOR
      *        SELECT ID_REGISTRO,
      *               ID_LOTE,
      *               CUENTA,
      *               NUM_CREDITO,
      *               MONTO,
      *               ID_TRANSACCION,
      *               TYPE_UPDATE,
      *               DATOS_TX
      *        FROM tf02
      *        WHERE ID_LOTE = :DB-LOTE-BUSQUEDA
      *          AND ESTADO = 2
      *    END-EXEC.
                   .

      *    EXEC SQL
      *        OPEN C2
      *    END-EXEC.
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

      *        EXEC SQL
      *            FETCH C2 INTO
      *                :DB-ID-REGISTRO,
      *                :DB-ID-LOTE,
      *                :DB-CUENTA,
      *                :DB-NUM-CREDITO,
      *                :DB-MONTO,
      *                :DB-TXID,
      *                :DB-TYPE-UPD,
      *                :DB-DATOS-TX
      *        END-EXEC
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
           MOVE 5 TO SQL-LEN(3)
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
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-TXID
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 40 TO SQL-LEN(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             DB-TYPE-UPD
           MOVE 'X' TO SQL-TYPE(7)
           MOVE 10 TO SQL-LEN(7)
           SET SQL-ADDR(8) TO ADDRESS OF
             DB-DATOS-TX
           MOVE 'X' TO SQL-TYPE(8)
           MOVE 500 TO SQL-LEN(8)
           MOVE 8 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO

               IF SQLCODE = 0

      *            EXEC SQL
      *                UPDATE tf02
      *                   SET ESTADO = 3
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
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

                   PERFORM 9000-EJECUTAR-CORE-REGLAS

      *            EXEC SQL
      *                UPDATE tf02
      *                   SET ESTADO        = :DB-ESTADO-FINAL,
      *                       COD_ERROR     = :DB-COD-ERROR,
      *                       ERROR_MESSAGE = :DB-MSG-ERROR
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
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

               ELSE
                   MOVE 'Y' TO WS-EOF-CURSOR
               END-IF

           END-PERFORM.

      *    EXEC SQL
      *        CLOSE C2
      *    END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-3
                               SQLCA
                   .

       3000-PROCESAR-TF03.

      *    EXEC SQL
      *        DECLARE C3 CURSOR FOR
      *        SELECT ID_REGISTRO,
      *               ID_LOTE,
      *               CUENTA,
      *               NUM_CREDITO,
      *               MONTO,
      *               ID_TRANSACCION,
      *               TYPE_UPDATE,
      *               DATOS_TX
      *        FROM tf03
      *        WHERE ID_LOTE = :DB-LOTE-BUSQUEDA
      *          AND ESTADO = 2
      *    END-EXEC.
                   .

      *    EXEC SQL
      *        OPEN C3
      *    END-EXEC.
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

      *        EXEC SQL
      *            FETCH C3 INTO
      *                :DB-ID-REGISTRO,
      *                :DB-ID-LOTE,
      *                :DB-CUENTA,
      *                :DB-NUM-CREDITO,
      *                :DB-MONTO,
      *                :DB-TXID,
      *                :DB-TYPE-UPD,
      *                :DB-DATOS-TX
      *        END-EXEC
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
           MOVE 5 TO SQL-LEN(3)
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
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-TXID
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 40 TO SQL-LEN(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             DB-TYPE-UPD
           MOVE 'X' TO SQL-TYPE(7)
           MOVE 10 TO SQL-LEN(7)
           SET SQL-ADDR(8) TO ADDRESS OF
             DB-DATOS-TX
           MOVE 'X' TO SQL-TYPE(8)
           MOVE 500 TO SQL-LEN(8)
           MOVE 8 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-6
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO

               IF SQLCODE = 0

      *            EXEC SQL
      *                UPDATE tf03
      *                   SET ESTADO = 3
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
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

                   PERFORM 9000-EJECUTAR-CORE-REGLAS

      *            EXEC SQL
      *                UPDATE tf03
      *                   SET ESTADO        = :DB-ESTADO-FINAL,
      *                       COD_ERROR     = :DB-COD-ERROR,
      *                       ERROR_MESSAGE = :DB-MSG-ERROR
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
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

               ELSE
                   MOVE 'Y' TO WS-EOF-CURSOR
               END-IF

           END-PERFORM.

      *    EXEC SQL
      *        CLOSE C3
      *    END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-6
                               SQLCA
                   .

       4000-PROCESAR-TF04.

      *    EXEC SQL
      *        DECLARE C4 CURSOR FOR
      *        SELECT ID_REGISTRO,
      *               ID_LOTE,
      *               CUENTA,
      *               NUM_CREDITO,
      *               MONTO,
      *               ID_TRANSACCION,
      *               TYPE_UPDATE,
      *               DATOS_TX
      *        FROM tf04
      *        WHERE ID_LOTE = :DB-LOTE-BUSQUEDA
      *          AND ESTADO = 2
      *    END-EXEC.
                   .

      *    EXEC SQL
      *        OPEN C4
      *    END-EXEC.
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

      *        EXEC SQL
      *            FETCH C4 INTO
      *                :DB-ID-REGISTRO,
      *                :DB-ID-LOTE,
      *                :DB-CUENTA,
      *                :DB-NUM-CREDITO,
      *                :DB-MONTO,
      *                :DB-TXID,
      *                :DB-TYPE-UPD,
      *                :DB-DATOS-TX
      *        END-EXEC
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
           MOVE 5 TO SQL-LEN(3)
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
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-TXID
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 40 TO SQL-LEN(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             DB-TYPE-UPD
           MOVE 'X' TO SQL-TYPE(7)
           MOVE 10 TO SQL-LEN(7)
           SET SQL-ADDR(8) TO ADDRESS OF
             DB-DATOS-TX
           MOVE 'X' TO SQL-TYPE(8)
           MOVE 500 TO SQL-LEN(8)
           MOVE 8 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-9
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO

               IF SQLCODE = 0

      *            EXEC SQL
      *                UPDATE tf04
      *                   SET ESTADO = 3
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
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

                   PERFORM 9000-EJECUTAR-CORE-REGLAS

      *            EXEC SQL
      *                UPDATE tf04
      *                   SET ESTADO        = :DB-ESTADO-FINAL,
      *                       COD_ERROR     = :DB-COD-ERROR,
      *                       ERROR_MESSAGE = :DB-MSG-ERROR
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
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

               ELSE
                   MOVE 'Y' TO WS-EOF-CURSOR
               END-IF

           END-PERFORM.

      *    EXEC SQL
      *        CLOSE C4
      *    END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-9
                               SQLCA
                   .

       5000-PROCESAR-TF05.

      *    EXEC SQL
      *        DECLARE C5 CURSOR FOR
      *        SELECT ID_REGISTRO,
      *               ID_LOTE,
      *               CUENTA,
      *               NUM_CREDITO,
      *               MONTO,
      *               ID_TRANSACCION,
      *               TYPE_UPDATE,
      *               DATOS_TX
      *        FROM tf05
      *        WHERE ID_LOTE = :DB-LOTE-BUSQUEDA
      *          AND ESTADO = 2
      *    END-EXEC.
                   .

      *    EXEC SQL
      *        OPEN C5
      *    END-EXEC.
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

      *        EXEC SQL
      *            FETCH C5 INTO
      *                :DB-ID-REGISTRO,
      *                :DB-ID-LOTE,
      *                :DB-CUENTA,
      *                :DB-NUM-CREDITO,
      *                :DB-MONTO,
      *                :DB-TXID,
      *                :DB-TYPE-UPD,
      *                :DB-DATOS-TX
      *        END-EXEC
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
           MOVE 5 TO SQL-LEN(3)
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
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-TXID
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 40 TO SQL-LEN(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             DB-TYPE-UPD
           MOVE 'X' TO SQL-TYPE(7)
           MOVE 10 TO SQL-LEN(7)
           SET SQL-ADDR(8) TO ADDRESS OF
             DB-DATOS-TX
           MOVE 'X' TO SQL-TYPE(8)
           MOVE 500 TO SQL-LEN(8)
           MOVE 8 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-12
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO

               IF SQLCODE = 0

      *            EXEC SQL
      *                UPDATE tf05
      *                   SET ESTADO = 3
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
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

                   PERFORM 9000-EJECUTAR-CORE-REGLAS

      *            EXEC SQL
      *                UPDATE tf05
      *                   SET ESTADO        = :DB-ESTADO-FINAL,
      *                       COD_ERROR     = :DB-COD-ERROR,
      *                       ERROR_MESSAGE = :DB-MSG-ERROR
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
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

               ELSE
                   MOVE 'Y' TO WS-EOF-CURSOR
               END-IF

           END-PERFORM.

      *    EXEC SQL
      *        CLOSE C5
      *    END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-12
                               SQLCA
                   .

       6000-PROCESAR-TF06.

      *    EXEC SQL
      *        DECLARE C6 CURSOR FOR
      *        SELECT ID_REGISTRO,
      *               ID_LOTE,
      *               CUENTA,
      *               NUM_CREDITO,
      *               MONTO,
      *               ID_TRANSACCION,
      *               TYPE_UPDATE,
      *               DATOS_TX
      *        FROM tf06
      *        WHERE ID_LOTE = :DB-LOTE-BUSQUEDA
      *          AND ESTADO = 2
      *    END-EXEC.
                   .

      *    EXEC SQL
      *        OPEN C6
      *    END-EXEC.
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

      *        EXEC SQL
      *            FETCH C6 INTO
      *                :DB-ID-REGISTRO,
      *                :DB-ID-LOTE,
      *                :DB-CUENTA,
      *                :DB-NUM-CREDITO,
      *                :DB-MONTO,
      *                :DB-TXID,
      *                :DB-TYPE-UPD,
      *                :DB-DATOS-TX
      *        END-EXEC
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
           MOVE 5 TO SQL-LEN(3)
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
           SET SQL-ADDR(6) TO ADDRESS OF
             DB-TXID
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 40 TO SQL-LEN(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             DB-TYPE-UPD
           MOVE 'X' TO SQL-TYPE(7)
           MOVE 10 TO SQL-LEN(7)
           SET SQL-ADDR(8) TO ADDRESS OF
             DB-DATOS-TX
           MOVE 'X' TO SQL-TYPE(8)
           MOVE 500 TO SQL-LEN(8)
           MOVE 8 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-15
                               SQLCA
           MOVE SQL-VAR-0001 TO DB-ID-REGISTRO
           MOVE SQL-VAR-0002 TO DB-ID-LOTE
           MOVE SQL-VAR-0003 TO DB-CUENTA
           MOVE SQL-VAR-0004 TO DB-MONTO

               IF SQLCODE = 0

      *            EXEC SQL
      *                UPDATE tf06
      *                   SET ESTADO = 3
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
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

                   PERFORM 9000-EJECUTAR-CORE-REGLAS

      *            EXEC SQL
      *                UPDATE tf06
      *                   SET ESTADO        = :DB-ESTADO-FINAL,
      *                       COD_ERROR     = :DB-COD-ERROR,
      *                       ERROR_MESSAGE = :DB-MSG-ERROR
      *                 WHERE ID_REGISTRO = :DB-ID-REGISTRO
      *            END-EXEC
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

               ELSE
                   MOVE 'Y' TO WS-EOF-CURSOR
               END-IF

           END-PERFORM.

      *    EXEC SQL
      *        CLOSE C6
      *    END-EXEC.
           CALL 'OCSQLCCU' USING SQL-STMT-15
                               SQLCA
                   .

      *================================================================*
      * 9000-EJECUTAR-CORE-REGLAS: ENRUTADOR DE ACCIONES REALES
      *================================================================*

       9000-EJECUTAR-CORE-REGLAS.

      *----------------------------------------------------------------*
      * CANAL MAESTRO: PAGOS DE ACTIVOS / CRÉDITOS (PAG_CR)
      *----------------------------------------------------------------*

           IF DB-TYPE-UPD = "PAG_CR"

               EVALUATE DB-NUM-CREDITO

                   WHEN "005"

      * === CASO 005: AMORTIZACIÓN DE HIPOTECA ===
      * Recortamos los 10 dígitos del ID_HIPOTECA desde la trama cruda
      * (posiciones 14-23)

                       CALL "tkin_hip"
                           USING FUNCTION NUMVAL(DB-DATOS-TX(14:10)),
                                 DB-NUM-CREDITO,
                                 DB-MONTO,
                                 LK-TRICKLE-FEED-INTERFACE

                       PERFORM 9100-EVALUAR-RESPUESTA-ACTIVO
                       EXIT PARAGRAPH

                   WHEN "004"

      * === CASO 004: PAGO DE TARJETA DE CRÉDITO ===
      * Recortamos los 16 dígitos del PAN de la tarjeta desde la trama
      * cruda (posiciones 14-29)

                       CALL "tkin_tarj"
                           USING DB-CUENTA,
                                 DB-DATOS-TX(14:16),
                                 DB-MONTO,
                                 LK-TRICKLE-FEED-INTERFACE

                       PERFORM 9100-EVALUAR-RESPUESTA-ACTIVO
                       EXIT PARAGRAPH

               END-EVALUATE

           END-IF.

      *----------------------------------------------------------------*
      * CANAL MAESTRO: MOVIMIENTOS DE PASIVOS / CUENTAS (DEP_DDA)
      *----------------------------------------------------------------*

           MOVE 0 TO MS-COUNT-MATCH

           INITIALIZE MS-TIPO-DOC,
                      MS-DOC-CLIENTE,
                      MS-ESTADO-CTA,
                      MS-SALDO-ACTUAL

      *    EXEC SQL
      *        SELECT COUNT(*),
      *               c.TIPO_DOC,
      *               c.DOC_CLIENTE,
      *               cc.ESTADO_CUENTA,
      *               cc.SALDO_ACTUAL
      *          INTO :MS-COUNT-MATCH,
      *               :MS-TIPO-DOC,
      *               :MS-DOC-CLIENTE,
      *               :MS-ESTADO-CTA,
      *               :MS-SALDO-ACTUAL
      *          FROM ctactes cc
      *          INNER JOIN clientes c
      *                  ON cc.ID_CLIENTE = c.ID_CLIENTE
      *         WHERE cc.ID_CUENTA = :DB-CUENTA
      *         GROUP BY c.TIPO_DOC,
      *                  c.DOC_CLIENTE,
      *                  cc.ESTADO_CUENTA,
      *                  cc.SALDO_ACTUAL
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-18 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 MS-TIPO-DOC
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 3 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 MS-DOC-CLIENTE
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 12 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 MS-ESTADO-CTA
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 1 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(5)
               MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(6)
               MOVE 5 TO SQL-LEN(6)
               MOVE X'00' TO SQL-PREC(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-18
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE DB-CUENTA TO SQL-VAR-0003
           CALL 'OCSQLEXE' USING SQL-STMT-18
                               SQLCA
           MOVE SQL-VAR-0007 TO MS-COUNT-MATCH
           MOVE SQL-VAR-0008 TO MS-SALDO-ACTUAL
                   .

           IF MS-COUNT-MATCH = 0

               MOVE 7 TO DB-ESTADO-FINAL
               MOVE "NOEXISTE" TO DB-COD-ERROR
               MOVE "CUENTA INEXISTENTE EN CATALOGO CORE"
                   TO DB-MSG-ERROR

               EXIT PARAGRAPH

           END-IF.

           MOVE 0 TO WS-ESPACIOS-DERECHA

           INSPECT FUNCTION REVERSE(MS-DOC-CLIENTE)
               TALLYING WS-ESPACIOS-DERECHA
               FOR LEADING SPACES

           COMPUTE WS-LONGITUD-REAL =
               FUNCTION LENGTH(MS-DOC-CLIENTE)
               - WS-ESPACIOS-DERECHA.

           EVALUATE MS-TIPO-DOC

               WHEN "CED"

                   IF WS-LONGITUD-REAL NOT = 10

                       MOVE 7 TO DB-ESTADO-FINAL
                       MOVE "ID_INVALID" TO DB-COD-ERROR

                       MOVE
                       "LONGITUD DE CEDULA RECHAZADA (DEBE SER 10)"
                           TO DB-MSG-ERROR

                       EXIT PARAGRAPH

                   END-IF

               WHEN "PAS"

                   IF WS-LONGITUD-REAL < 8
                      OR WS-LONGITUD-REAL > 12

                       MOVE 7 TO DB-ESTADO-FINAL
                       MOVE "ID_INVALID" TO DB-COD-ERROR

                       MOVE
                       "LONGITUD DE PASAPORTE RECHAZADA (8-12)"
                           TO DB-MSG-ERROR

                       EXIT PARAGRAPH

                   END-IF

           END-EVALUATE.

           IF MS-ESTADO-CTA NOT = "A"

               MOVE 7 TO DB-ESTADO-FINAL
               MOVE "CTA_CONGEL" TO DB-COD-ERROR

               MOVE
               "CUENTA INACTIVA O BLOQUEADA ADMINISTRATIVAMENTE"
                   TO DB-MSG-ERROR

               EXIT PARAGRAPH

           END-IF.

           MOVE DB-CUENTA       TO CTA-NRO-CUENTA
           MOVE DB-NUM-CREDITO  TO CTA-NUM-CREDITO
           MOVE DB-MONTO        TO CTA-MONTO-MOV
           MOVE MS-SALDO-ACTUAL TO CTA-SALDO-ACTUAL

           EVALUATE DB-NUM-CREDITO
               WHEN "002"
                   MOVE "C" TO LK-TF-ACCION

               WHEN "003"
                   MOVE "D" TO LK-TF-ACCION

               WHEN OTHER
                   MOVE "C" TO LK-TF-ACCION
           END-EVALUATE.

           CALL "tkin01"
               USING REG-CTA,
                     LK-TRICKLE-FEED-INTERFACE.

           IF LK-TF-COD-RETORNO = 0

               MOVE 4 TO DB-ESTADO-FINAL
               MOVE "000" TO DB-COD-ERROR
               MOVE "OK" TO DB-MSG-ERROR

           ELSE

               MOVE 7 TO DB-ESTADO-FINAL
               MOVE LK-TF-MENSAJE TO DB-MSG-ERROR

               EVALUATE LK-TF-COD-RETORNO
                   WHEN 07
                       MOVE "INSFONDOS" TO DB-COD-ERROR

                   WHEN 01
                       MOVE "NOEXISTE" TO DB-COD-ERROR

                   WHEN OTHER
                       MOVE "ERRCORE" TO DB-COD-ERROR
               END-EVALUATE

           END-IF.

      *================================================================*
      * 9100-EVALUAR-RESPUESTA-ACTIVO: SUBRUTINA DE FORMATEO FINANCIERO
      *================================================================*

       9100-EVALUAR-RESPUESTA-ACTIVO.

           IF LK-TF-COD-RETORNO = 0

               MOVE 4 TO DB-ESTADO-FINAL
               MOVE "000" TO DB-COD-ERROR
               MOVE "OK" TO DB-MSG-ERROR

           ELSE

               MOVE 7 TO DB-ESTADO-FINAL
               MOVE LK-TF-MENSAJE TO DB-MSG-ERROR

               EVALUATE LK-TF-COD-RETORNO
                   WHEN 01
                       MOVE "PROD_NO_FX" TO DB-COD-ERROR

                   WHEN 07
                       MOVE "INSFONDOS" TO DB-COD-ERROR

                   WHEN OTHER
                       MOVE "ERR_ACTV" TO DB-COD-ERROR
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
      *  DB-CUENTA                IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(9,0)
      *  DB-DATOS-TX              IN USE CHAR(500)
      *  DB-ESTADO-FINAL          IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(3,0)
      *  DB-ID-LOTE               IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(9,0)
      *  DB-ID-REGISTRO           IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  DB-LOTE-BUSQUEDA         IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(9,0)
      *  DB-MONTO                 IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(15,2)
      *  DB-MSG-ERROR             IN USE CHAR(200)
      *  DB-NUM-CREDITO           IN USE CHAR(20)
      *  DB-TXID                  IN USE CHAR(40)
      *  DB-TYPE-UPD              IN USE CHAR(10)
      *  DB-VALIDACION-MASTER NOT IN USE
      *  DB-VALIDACION-MASTER.MS-COUNT-MATCH NOT IN USE
      *  DB-VALIDACION-MASTER.MS-DOC-CLIENTE NOT IN USE
      *  DB-VALIDACION-MASTER.MS-ESTADO-CTA NOT IN USE
      *  DB-VALIDACION-MASTER.MS-SALDO-ACTUAL NOT IN USE
      *  DB-VALIDACION-MASTER.MS-TIPO-DOC NOT IN USE
      *  MS-COUNT-MATCH           IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(1,0)
      *  MS-DOC-CLIENTE           IN USE CHAR(12)
      *  MS-ESTADO-CTA            IN USE CHAR(1)
      *  MS-SALDO-ACTUAL          IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(15,2)
      *  MS-TIPO-DOC              IN USE CHAR(3)
      **********************************************************************

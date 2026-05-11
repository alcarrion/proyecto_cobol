      *================================================================*
      * PROGRAMA : BAT000.sqb                                         *
      * FUNCION  : PROCESO BATCH DE CIERRE MENSUAL                    *
      *            - CONSOLIDACION                                    *
      *            - PROCESAR MORA EN HIPOTECAS                       *
      *            - DESCONTAR CUOTA DE CUENTA SI HAY FONDOS          *
      *            - DAR DE BAJA TARJETAS VENCIDAS                    *
      *            - RESET OPERATIVO DE TARJETAS                      *
      *            - ROLLBACK si algo falla*
      * LLAMADO  : BANCSMENU (OPCION 5)                               *
      * PRECOMP  : esqlOC BAT000.sqb  ->  BAT000.cob                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BAT000.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 18.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 18 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 18 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 18 TIMES.
           05 SQL-PREC   PIC X OCCURS 18 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 292.
           05 SQL-STMT   PIC X(292) VALUE 'SELECT C.ID_CLIENTE,C.TIPO_DO
      -    'C,C.DOC_CLIENTE,C.FECHA_ALTA,C.NOMBRE_CLIENTE,C.APELLIDOS_CL
      -    'IENTE,C.SALDO_CLIENTE,C.CTA_ACTIVA,C.TARJETA,C.HIPOTECA,I.SA
      -    'LDO_ACTUAL,I.COD_ULT_MOV,I.FECHA_ULT_MOV,I.IMPORTE_MOV FROM 
      -    'clientes C LEFT JOIN ctactes I ON C.ID_CLIENTE = I.ID_CLIENT
      -    'E ORDER BY C.ID_CLIENTE'.
           05 SQL-CNAME  PIC X(11) VALUE 'CUR-MAESTRA'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 122.
           05 SQL-STMT   PIC X(122) VALUE 'SELECT ID_CLIENTE,NRO_TARJETA
      -    ',LIMITE_TARJETA,ACUM_MES,LIQUIDACION_MES,ESTADO FROM TARJETA
      -    'S ORDER BY ID_CLIENTE,NRO_TARJETA'.
           05 SQL-CNAME  PIC X(12) VALUE 'CUR-TARJETAS'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 137.
           05 SQL-STMT   PIC X(137) VALUE 'SELECT ID_HIPOTECA,ID_CLIENTE
      -    ',MONTO_ORIGINAL,TASA_INTERES,SALDO_ACTUAL,FECHA_VENCTO,ESTAD
      -    'O FROM HIPOTECAS ORDER BY ID_CLIENTE,ID_HIPOTECA'.
           05 SQL-CNAME  PIC X(13) VALUE 'CUR-HIPOTECAS'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 610.
           05 SQL-STMT   PIC X(610) VALUE 'SELECT H.ID_HIPOTECA,H.ID_CLI
      -    'ENTE,H.SALDO_ACTUAL,H.MONTO_ORIGINAL,H.ESTADO,TIMESTAMPDIFF(
      -    'MONTH,H.FECHA_INICIO,H.FECHA_VENCTO),TIMESTAMPDIFF(MONTH,H.F
      -    'ECHA_INICIO,CURDATE()),(H.MONTO_ORIGINAL * H.TASA_INTERES) /
      -    ' TIMESTAMPDIFF(MONTH,H.FECHA_INICIO,H.FECHA_VENCTO),H.MONTO_
      -    'ORIGINAL - ((H.MONTO_ORIGINAL * H.TASA_INTERES) / TIMESTAMPD
      -    'IFF(MONTH,H.FECHA_INICIO,H.FECHA_VENCTO)) * TIMESTAMPDIFF(MO
      -    'NTH,H.FECHA_INICIO,CURDATE()),(SELECT COUNT(*) FROM AUDIT_HI
      -    'POTECAS A WHERE A.ID_HIPOTECA = H.ID_HIPOTECA AND A.ESTADO =
      -    ' ''MOROSO'') FROM HIPOTECAS H WHERE H.ESTADO IN (''ACTIVO'',
      -    '''MOROSO'') ORDER BY H.ID_CLIENTE,H.ID_HIPOTECA'.
           05 SQL-CNAME  PIC X(8) VALUE 'CUR-MORA'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 52.
           05 SQL-STMT   PIC X(52) VALUE 'SELECT COUNT(*) FROM AUDIT_MAE
      -    'STRA WHERE PERIODO = ?'.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 15.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 252.
           05 SQL-STMT   PIC X(252) VALUE 'INSERT INTO AUDIT_MAESTRA (PE
      -    'RIODO,ID_CLIENTE,TIPO_DOC,DOC_CLIENTE,FECHA_ALTA,NOMBRE_CLIE
      -    'NTE,APELLIDOS_CLIENTE,SALDO_CLIENTE,CTA_ACTIVA,TIENE_TARJETA
      -    ',TIENE_HIPOTECA,SALDO_CTA,COD_ULT_MOV,FECHA_ULT_MOV,IMPORTE_
      -    'MOV) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 7.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 129.
           05 SQL-STMT   PIC X(129) VALUE 'INSERT INTO AUDIT_TARJETAS (P
      -    'ERIODO,ID_CLIENTE,NRO_TARJETA,LIMITE_TARJETA,ACUM_MES,LIQUID
      -    'ACION_MES,ESTADO) VALUES (?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 8.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 146.
           05 SQL-STMT   PIC X(146) VALUE 'INSERT INTO AUDIT_HIPOTECAS (
      -    'PERIODO,ID_HIPOTECA,ID_CLIENTE,MONTO_ORIGINAL,TASA_INTERES,S
      -    'ALDO_ACTUAL,FECHA_VENCTO,ESTADO) VALUES (?,?,?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-8.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 53.
           05 SQL-STMT   PIC X(53) VALUE 'UPDATE HIPOTECAS SET ESTADO = 
      -    '? WHERE ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-9.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 55.
           05 SQL-STMT   PIC X(55) VALUE 'SELECT SALDO_CLIENTE FROM clie
      -    'ntes WHERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-10.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 74.
           05 SQL-STMT   PIC X(74) VALUE 'UPDATE clientes SET SALDO_CLIE
      -    'NTE = SALDO_CLIENTE - ? WHERE ID_CLIENTE = ?'.
      **********************************************************************
       01 SQL-STMT-11.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 60.
           05 SQL-STMT   PIC X(60) VALUE 'UPDATE HIPOTECAS SET ESTADO = 
      -    '''ACTIVO'' WHERE ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-12.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 85.
           05 SQL-STMT   PIC X(85) VALUE 'UPDATE TARJETAS SET ESTADO = '
      -    ''I'' WHERE FECHA_VENCIMIENTO < CURDATE() AND ESTADO = ''A'''
           .
      **********************************************************************
       01 SQL-STMT-13.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 78.
           05 SQL-STMT   PIC X(78) VALUE 'UPDATE TARJETAS SET LIQUIDACIO
      -    'N_MES = ACUM_MES,ACUM_MES = 0 WHERE ESTADO = ''A'''.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0003  PIC S9(1) COMP-3.
           05 SQL-VAR-0004  PIC S9(1) COMP-3.
           05 SQL-VAR-0005  PIC S9(1) COMP-3.
           05 SQL-VAR-0006  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0007  PIC S9(3) COMP-3.
           05 SQL-VAR-0008  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0009  PIC S9(9) COMP-3.
           05 SQL-VAR-0010  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0011  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0012  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0013  PIC S9(9) COMP-3.
           05 SQL-VAR-0014  PIC S9(9) COMP-3.
           05 SQL-VAR-0015  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0016  PIC S9(3)V9(4) COMP-3.
           05 SQL-VAR-0017  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0018  PIC S9(9) COMP-3.
           05 SQL-VAR-0019  PIC S9(9) COMP-3.
           05 SQL-VAR-0020  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0021  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0022  PIC S9(5) COMP-3.
           05 SQL-VAR-0023  PIC S9(5) COMP-3.
           05 SQL-VAR-0024  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0025  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0026  PIC S9(5) COMP-3.
           05 SQL-VAR-0027  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0028  PIC S9(7) COMP-3.
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

      *================================================================*
      *   DECLARE SECTION - VARIABLES HOST SQL                        *
      *================================================================*
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.

      *    Control de periodo
       01  WS-PERIODO              PIC X(6).

      *    Variables host - AUDIT_MAESTRA / clientes + ctactes
       01  WS-HOST-MAESTRA.
           05 HV-ID-CLIENTE        PIC 9(8).
           05 HV-TIPO-DOC          PIC X(3).
           05 HV-DOC-CLIENTE       PIC X(12).
           05 HV-FECHA-ALTA        PIC X(10).
           05 HV-NOMBRE            PIC X(25).
           05 HV-APELLIDOS         PIC X(25).
           05 HV-SALDO-CLI         PIC S9(10)V99.
           05 HV-CTA-ACTIVA        PIC 9(1).
           05 HV-TIENE-TARJETA     PIC 9(1).
           05 HV-TIENE-HIPOTECA    PIC 9(1).
           05 HV-SALDO-CTA         PIC S9(10)V99.
           05 HV-COD-ULT-MOV       PIC 9(2).
           05 HV-FECHA-ULT-MOV     PIC X(10).
           05 HV-IMPORTE-MOV       PIC S9(10)V99.
      *    Indicadores de NULL para el LEFT JOIN con ctactes
           05 HV-IND-SALDO-CTA     PIC S9(4) COMP-5.
           05 HV-IND-COD-MOV       PIC S9(4) COMP-5.
           05 HV-IND-FECHA-MOV     PIC S9(4) COMP-5.
           05 HV-IND-IMPORTE-MOV   PIC S9(4) COMP-5.

      *    Variables host - AUDIT_TARJETAS
       01  WS-HOST-TARJETAS.
           05 HV-TARJ-ID-CLI       PIC 9(8).
           05 HV-TARJ-NRO          PIC X(16).
           05 HV-TARJ-LIMITE       PIC S9(10)V99.
           05 HV-TARJ-ACUM         PIC S9(10)V99.
           05 HV-TARJ-LIQUID       PIC S9(10)V99.
           05 HV-TARJ-ESTADO       PIC X(1).

      *    Variables host - AUDIT_HIPOTECAS
       01  WS-HOST-HIPOTECAS.
           05 HV-HIPO-ID-HIPO      PIC 9(9).
           05 HV-HIPO-ID-CLI       PIC 9(8).
           05 HV-HIPO-MONTO-ORIG   PIC S9(13)V99.
           05 HV-HIPO-TASA         PIC S9(3)V9999.
           05 HV-HIPO-SALDO        PIC S9(13)V99.
           05 HV-HIPO-FECHA-VENC   PIC X(10).
           05 HV-HIPO-ESTADO       PIC X(20).

      *    Variables host - PROCESAMIENTO DE MORA
       01  WS-HOST-MORA.
           05 HV-MORA-ID-HIPO      PIC 9(9).
           05 HV-MORA-ID-CLI       PIC 9(8).
           05 HV-MORA-SALDO-ACT    PIC S9(13)V99.
           05 HV-MORA-MONTO-ORIG   PIC S9(13)V99.
           05 HV-MORA-ESTADO       PIC X(20).
           05 HV-MORA-MESES-TOT    PIC 9(4).
           05 HV-MORA-MESES-TRANS  PIC 9(4).
           05 HV-MORA-PAGO-MENS    PIC S9(13)V99.
           05 HV-MORA-SALDO-ESP    PIC S9(13)V99.
           05 HV-MORA-MESES-MORA   PIC 9(4).

      *    Variables host para UPDATE de estado hipoteca y descuento
           05 HV-MORA-NUEVO-ESTADO PIC X(20).
           05 HV-MORA-CLI-SALDO    PIC S9(10)V99.

      *    Verificacion de periodo duplicado
       01  HV-COUNT-PERIODO        PIC 9(6).

      *    EXEC SQL END DECLARE SECTION END-EXEC.

      *================================================================*
      *   DECLARACION DE CURSORES                                      *
      *================================================================*

      *    EXEC SQL DECLARE CUR-MAESTRA CURSOR FOR
      *        SELECT C.ID_CLIENTE,
      *               C.TIPO_DOC,
      *               C.DOC_CLIENTE,
      *               C.FECHA_ALTA,
      *               C.NOMBRE_CLIENTE,
      *               C.APELLIDOS_CLIENTE,
      *               C.SALDO_CLIENTE,
      *               C.CTA_ACTIVA,
      *               C.TARJETA,
      *               C.HIPOTECA,
      *               I.SALDO_ACTUAL,
      *               I.COD_ULT_MOV,
      *               I.FECHA_ULT_MOV,
      *               I.IMPORTE_MOV
      *        FROM   clientes C
      *        LEFT JOIN ctactes I
      *               ON C.ID_CLIENTE = I.ID_CLIENTE
      *        ORDER BY C.ID_CLIENTE
      *    END-EXEC.

      *    EXEC SQL DECLARE CUR-TARJETAS CURSOR FOR
      *        SELECT ID_CLIENTE,
      *               NRO_TARJETA,
      *               LIMITE_TARJETA,
      *               ACUM_MES,
      *               LIQUIDACION_MES,
      *               ESTADO
      *        FROM   TARJETAS
      *        ORDER BY ID_CLIENTE, NRO_TARJETA
      *    END-EXEC.

      *    EXEC SQL DECLARE CUR-HIPOTECAS CURSOR FOR
      *        SELECT ID_HIPOTECA,
      *               ID_CLIENTE,
      *               MONTO_ORIGINAL,
      *               TASA_INTERES,
      *               SALDO_ACTUAL,
      *               FECHA_VENCTO,
      *               ESTADO
      *        FROM   HIPOTECAS
      *        ORDER BY ID_CLIENTE, ID_HIPOTECA
      *    END-EXEC.

      *    Cursor de mora: MySQL calcula todos los valores necesarios
      *    EXEC SQL DECLARE CUR-MORA CURSOR FOR
      *        SELECT H.ID_HIPOTECA,
      *               H.ID_CLIENTE,
      *               H.SALDO_ACTUAL,
      *               H.MONTO_ORIGINAL,
      *               H.ESTADO,
      *               TIMESTAMPDIFF(MONTH,
      *                   H.FECHA_INICIO, H.FECHA_VENCTO),
      *               TIMESTAMPDIFF(MONTH,
      *                   H.FECHA_INICIO, CURDATE()),
      *               (H.MONTO_ORIGINAL * H.TASA_INTERES) /
      *               TIMESTAMPDIFF(MONTH,
      *                   H.FECHA_INICIO, H.FECHA_VENCTO),
      *               H.MONTO_ORIGINAL -
      *               ((H.MONTO_ORIGINAL * H.TASA_INTERES) /
      *               TIMESTAMPDIFF(MONTH,
      *                   H.FECHA_INICIO, H.FECHA_VENCTO)) *
      *               TIMESTAMPDIFF(MONTH,
      *                   H.FECHA_INICIO, CURDATE()),
      *               (SELECT COUNT(*) FROM AUDIT_HIPOTECAS A
      *                WHERE  A.ID_HIPOTECA = H.ID_HIPOTECA
      *                AND    A.ESTADO      = 'MOROSO')
      *        FROM   HIPOTECAS H
      *        WHERE  H.ESTADO IN ('ACTIVO', 'MOROSO')
      *        ORDER BY H.ID_CLIENTE, H.ID_HIPOTECA
      *    END-EXEC.

      *================================================================*
      *   WORKING STORAGE - CONTROL INTERNO                            *
      *================================================================*
       01  WS-FECHA-SISTEMA.
           05 WS-ANIO              PIC X(4).
           05 WS-MES               PIC X(2).
           05 WS-DIA               PIC X(2).
           05 FILLER               PIC X(13).

       01  WS-FECHA-HOY            PIC X(10).

       01  WS-CONTROL.
           05 WS-ABORT             PIC X VALUE 'N'.
           05 WS-FIN-MAESTRA       PIC X VALUE 'N'.
           05 WS-FIN-TARJETAS      PIC X VALUE 'N'.
           05 WS-FIN-HIPOTECAS     PIC X VALUE 'N'.
           05 WS-FIN-MORA          PIC X VALUE 'N'.

       01  WS-CONTADORES.
           05 WS-CTR-CLIENTES      PIC 9(6) VALUE 0.
           05 WS-CTR-TARJETAS      PIC 9(6) VALUE 0.
           05 WS-CTR-HIPOTECAS     PIC 9(6) VALUE 0.
           05 WS-CTR-ERRORES       PIC 9(6) VALUE 0.
           05 WS-CTR-MOROSAS       PIC 9(6) VALUE 0.
           05 WS-CTR-CASTIGADAS    PIC 9(6) VALUE 0.
           05 WS-CTR-TARJ-BAJA     PIC 9(6) VALUE 0.

      *================================================================*
      *   LINKAGE SECTION                                              *
      *================================================================*
       LINKAGE SECTION.
           COPY LKCIF.

      *================================================================*
      *   PROCEDURE DIVISION                                           *
      *================================================================*
       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 0       TO LK-COD-RETORNO
           MOVE SPACES  TO LK-MENSAJE

           PERFORM 1000-INICIALIZAR
           IF WS-ABORT = 'N'
               PERFORM 2000-VALIDAR-PERIODO
           END-IF
           IF WS-ABORT = 'N'
               PERFORM 3000-SNAPSHOT-MAESTRA
           END-IF
           IF WS-ABORT = 'N'
               PERFORM 4000-SNAPSHOT-TARJETAS
           END-IF
           IF WS-ABORT = 'N'
               PERFORM 5000-SNAPSHOT-HIPOTECAS
           END-IF
           IF WS-ABORT = 'N'
               PERFORM 5100-PROCESAR-MORA-HIPOTECAS
           END-IF
           IF WS-ABORT = 'N'
               PERFORM 5200-BAJA-TARJETAS-VENCIDAS
           END-IF
           IF WS-ABORT = 'N'
               PERFORM 6000-RESET-OPERATIVO
           END-IF
           PERFORM 7000-FINALIZAR

           EXIT PROGRAM.

      *================================================================*
      *   1000 - INICIALIZAR: FECHA Y PERIODO                         *
      *================================================================*
       1000-INICIALIZAR.
           MOVE FUNCTION CURRENT-DATE TO WS-FECHA-SISTEMA

           STRING WS-ANIO DELIMITED SIZE
                  WS-MES  DELIMITED SIZE
                  INTO WS-PERIODO
           END-STRING

           STRING WS-ANIO DELIMITED SIZE
                  '-'     DELIMITED SIZE
                  WS-MES  DELIMITED SIZE
                  '-'     DELIMITED SIZE
                  WS-DIA  DELIMITED SIZE
                  INTO WS-FECHA-HOY
           END-STRING

           DISPLAY '================================================'
           DISPLAY ' BAT000 - PROCESO DE CIERRE MENSUAL'
           DISPLAY ' PERIODO : ' WS-PERIODO
           DISPLAY ' FECHA   : ' WS-FECHA-HOY
           DISPLAY '================================================'.

      *================================================================*
      *   2000 - VALIDAR QUE EL PERIODO NO EXISTA YA EN AUDITORIA     *
      *================================================================*
       2000-VALIDAR-PERIODO.
           MOVE 0 TO HV-COUNT-PERIODO

      *    EXEC SQL
      *        SELECT COUNT(*)
      *        INTO   :HV-COUNT-PERIODO
      *        FROM   AUDIT_MAESTRA
      *        WHERE  PERIODO = :WS-PERIODO
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0028
               MOVE '3' TO SQL-TYPE(1)
               MOVE 4 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-PERIODO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 6 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-4
                               SQLCA
           MOVE SQL-VAR-0028 TO HV-COUNT-PERIODO

           PERFORM 9000-EVALUAR-SQL

           IF LK-COD-RETORNO = 99
               DISPLAY 'ERROR AL VERIFICAR PERIODO. SQLCODE: '
                       SQLCODE
               MOVE 'S' TO WS-ABORT
           ELSE
               IF HV-COUNT-PERIODO > 0
                   DISPLAY 'ADVERTENCIA: PERIODO ' WS-PERIODO
                           ' YA FUE PROCESADO.'
                   MOVE 10 TO LK-COD-RETORNO
                   MOVE 'PERIODO YA PROCESADO. CIERRE CANCELADO.'
                       TO LK-MENSAJE
                   MOVE 'S' TO WS-ABORT
               ELSE
                   MOVE 0 TO LK-COD-RETORNO
                   DISPLAY 'PERIODO ' WS-PERIODO ' HABILITADO OK.'
               END-IF
           END-IF.

      *================================================================*
      *   3000 - SNAPSHOT AUDIT_MAESTRA (CLIENTES + CTACTES)          *
      *================================================================*
       3000-SNAPSHOT-MAESTRA.
           DISPLAY '>>> INICIANDO SNAPSHOT AUDIT_MAESTRA...'

      *    EXEC SQL OPEN CUR-MAESTRA END-EXEC
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               MOVE 0 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-0
                               SQLCA
           END-CALL

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR ABRIENDO CUR-MAESTRA: ' SQLCODE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR CRITICO ABRIENDO CURSOR CLIENTES'
                   TO LK-MENSAJE
               MOVE 'S' TO WS-ABORT
               EXIT PARAGRAPH
           END-IF

           MOVE 'N' TO WS-FIN-MAESTRA

           PERFORM UNTIL WS-FIN-MAESTRA = 'S'

      *        EXEC SQL
      *            FETCH CUR-MAESTRA INTO
      *                :HV-ID-CLIENTE,
      *                :HV-TIPO-DOC,
      *                :HV-DOC-CLIENTE,
      *                :HV-FECHA-ALTA,
      *                :HV-NOMBRE,
      *                :HV-APELLIDOS,
      *                :HV-SALDO-CLI,
      *                :HV-CTA-ACTIVA,
      *                :HV-TIENE-TARJETA,
      *                :HV-TIENE-HIPOTECA,
      *                :HV-SALDO-CTA      :HV-IND-SALDO-CTA,
      *                :HV-COD-ULT-MOV    :HV-IND-COD-MOV,
      *                :HV-FECHA-ULT-MOV  :HV-IND-FECHA-MOV,
      *                :HV-IMPORTE-MOV    :HV-IND-IMPORTE-MOV
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             HV-TIPO-DOC
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 3 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             HV-DOC-CLIENTE
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 12 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             HV-FECHA-ALTA
           MOVE 'X' TO SQL-TYPE(4)
           MOVE 10 TO SQL-LEN(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             HV-NOMBRE
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 25 TO SQL-LEN(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             HV-APELLIDOS
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 25 TO SQL-LEN(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(7)
           MOVE 7 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
           SET SQL-ADDR(8) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(8)
           MOVE 1 TO SQL-LEN(8)
               MOVE X'00' TO SQL-PREC(8)
           SET SQL-ADDR(9) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(9)
           MOVE 1 TO SQL-LEN(9)
               MOVE X'00' TO SQL-PREC(9)
           SET SQL-ADDR(10) TO ADDRESS OF
             SQL-VAR-0005
           MOVE '3' TO SQL-TYPE(10)
           MOVE 1 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
           SET SQL-ADDR(11) TO ADDRESS OF
             SQL-VAR-0006
           MOVE '3' TO SQL-TYPE(11)
           MOVE 7 TO SQL-LEN(11)
               MOVE X'02' TO SQL-PREC(11)
           SET SQL-ADDR(12) TO ADDRESS OF
             HV-IND-SALDO-CTA
           MOVE 'i' TO SQL-TYPE(12)
           SET SQL-ADDR(13) TO ADDRESS OF
             SQL-VAR-0007
           MOVE '3' TO SQL-TYPE(13)
           MOVE 2 TO SQL-LEN(13)
               MOVE X'00' TO SQL-PREC(13)
           SET SQL-ADDR(14) TO ADDRESS OF
             HV-IND-COD-MOV
           MOVE 'i' TO SQL-TYPE(14)
           SET SQL-ADDR(15) TO ADDRESS OF
             HV-FECHA-ULT-MOV
           MOVE 'X' TO SQL-TYPE(15)
           MOVE 10 TO SQL-LEN(15)
           SET SQL-ADDR(16) TO ADDRESS OF
             HV-IND-FECHA-MOV
           MOVE 'i' TO SQL-TYPE(16)
           SET SQL-ADDR(17) TO ADDRESS OF
             SQL-VAR-0008
           MOVE '3' TO SQL-TYPE(17)
           MOVE 7 TO SQL-LEN(17)
               MOVE X'02' TO SQL-PREC(17)
           SET SQL-ADDR(18) TO ADDRESS OF
             HV-IND-IMPORTE-MOV
           MOVE 'i' TO SQL-TYPE(18)
           MOVE 18 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO HV-ID-CLIENTE
           MOVE SQL-VAR-0002 TO HV-SALDO-CLI
           MOVE SQL-VAR-0003 TO HV-CTA-ACTIVA
           MOVE SQL-VAR-0004 TO HV-TIENE-TARJETA
           MOVE SQL-VAR-0005 TO HV-TIENE-HIPOTECA
           MOVE SQL-VAR-0006 TO HV-SALDO-CTA
           MOVE SQL-VAR-0007 TO HV-COD-ULT-MOV
           MOVE SQL-VAR-0008 TO HV-IMPORTE-MOV

               EVALUATE SQLCODE
                   WHEN 0
                       IF HV-IND-SALDO-CTA   < 0
                           MOVE ZERO   TO HV-SALDO-CTA
                       END-IF
                       IF HV-IND-COD-MOV     < 0
                           MOVE ZERO   TO HV-COD-ULT-MOV
                       END-IF
                       IF HV-IND-FECHA-MOV   < 0
                           MOVE SPACES TO HV-FECHA-ULT-MOV
                       END-IF
                       IF HV-IND-IMPORTE-MOV < 0
                           MOVE ZERO   TO HV-IMPORTE-MOV
                       END-IF

      *                EXEC SQL
      *                    INSERT INTO AUDIT_MAESTRA (
      *                        PERIODO,           ID_CLIENTE,
      *                        TIPO_DOC,          DOC_CLIENTE,
      *                        FECHA_ALTA,        NOMBRE_CLIENTE,
      *                        APELLIDOS_CLIENTE,
      *                        SALDO_CLIENTE,     CTA_ACTIVA,
      *                        TIENE_TARJETA,     TIENE_HIPOTECA,
      *                        SALDO_CTA,         COD_ULT_MOV,
      *                        FECHA_ULT_MOV,     IMPORTE_MOV
      *                    ) VALUES (
      *                        :WS-PERIODO,       :HV-ID-CLIENTE,
      *                        :HV-TIPO-DOC,      :HV-DOC-CLIENTE,
      *                        :HV-FECHA-ALTA,    :HV-NOMBRE,
      *                        :HV-APELLIDOS,
      *                        :HV-SALDO-CLI,     :HV-CTA-ACTIVA,
      *                        :HV-TIENE-TARJETA, :HV-TIENE-HIPOTECA,
      *                        :HV-SALDO-CTA,     :HV-COD-ULT-MOV,
      *                        :HV-FECHA-ULT-MOV, :HV-IMPORTE-MOV
      *                    )
      *                END-EXEC
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-PERIODO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0001
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 HV-TIPO-DOC
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 3 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 HV-DOC-CLIENTE
               MOVE 'X' TO SQL-TYPE(4)
               MOVE 12 TO SQL-LEN(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 HV-FECHA-ALTA
               MOVE 'X' TO SQL-TYPE(5)
               MOVE 10 TO SQL-LEN(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 HV-NOMBRE
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 25 TO SQL-LEN(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 HV-APELLIDOS
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 25 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(8)
               MOVE 7 TO SQL-LEN(8)
               MOVE X'02' TO SQL-PREC(8)
               SET SQL-ADDR(9) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(9)
               MOVE 1 TO SQL-LEN(9)
               MOVE X'00' TO SQL-PREC(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(10)
               MOVE 1 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(11)
               MOVE 1 TO SQL-LEN(11)
               MOVE X'00' TO SQL-PREC(11)
               SET SQL-ADDR(12) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(12)
               MOVE 7 TO SQL-LEN(12)
               MOVE X'02' TO SQL-PREC(12)
               SET SQL-ADDR(13) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(13)
               MOVE 2 TO SQL-LEN(13)
               MOVE X'00' TO SQL-PREC(13)
               SET SQL-ADDR(14) TO ADDRESS OF
                 HV-FECHA-ULT-MOV
               MOVE 'X' TO SQL-TYPE(14)
               MOVE 10 TO SQL-LEN(14)
               SET SQL-ADDR(15) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(15)
               MOVE 7 TO SQL-LEN(15)
               MOVE X'02' TO SQL-PREC(15)
               MOVE 15 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-ID-CLIENTE
             TO SQL-VAR-0001
           MOVE HV-SALDO-CLI
             TO SQL-VAR-0002
           MOVE HV-CTA-ACTIVA
             TO SQL-VAR-0003
           MOVE HV-TIENE-TARJETA
             TO SQL-VAR-0004
           MOVE HV-TIENE-HIPOTECA
             TO SQL-VAR-0005
           MOVE HV-SALDO-CTA
             TO SQL-VAR-0006
           MOVE HV-COD-ULT-MOV
             TO SQL-VAR-0007
           MOVE HV-IMPORTE-MOV
             TO SQL-VAR-0008
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA

                       IF SQLCODE = 0
                           ADD 1 TO WS-CTR-CLIENTES
                       ELSE
                           DISPLAY 'ERROR INSERT MAESTRA ID: '
                                   HV-ID-CLIENTE
                                   ' SQLCODE: ' SQLCODE
                           ADD 1   TO WS-CTR-ERRORES
                           MOVE 'S' TO WS-ABORT
                           MOVE 'S' TO WS-FIN-MAESTRA
                       END-IF

                   WHEN 100
                       MOVE 'S' TO WS-FIN-MAESTRA

                   WHEN OTHER
                       DISPLAY 'ERROR FETCH CUR-MAESTRA: ' SQLCODE
                       ADD 1   TO WS-CTR-ERRORES
                       MOVE 'S' TO WS-ABORT
                       MOVE 'S' TO WS-FIN-MAESTRA
               END-EVALUATE

           END-PERFORM

      *    EXEC SQL CLOSE CUR-MAESTRA END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA

           IF WS-ABORT = 'N'
               DISPLAY ' AUDIT_MAESTRA  : ' WS-CTR-CLIENTES
                       ' REGISTROS INSERTADOS.'
           ELSE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR EN SNAPSHOT MAESTRA - SE HARA ROLLBACK'
                   TO LK-MENSAJE
           END-IF.

      *================================================================*
      *   4000 - SNAPSHOT AUDIT_TARJETAS                              *
      *================================================================*
       4000-SNAPSHOT-TARJETAS.
           DISPLAY '>>> INICIANDO SNAPSHOT AUDIT_TARJETAS...'

      *    EXEC SQL OPEN CUR-TARJETAS END-EXEC
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               MOVE 0 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-1
                               SQLCA
           END-CALL

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR ABRIENDO CUR-TARJETAS: ' SQLCODE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR CRITICO ABRIENDO CURSOR TARJETAS'
                   TO LK-MENSAJE
               MOVE 'S' TO WS-ABORT
               EXIT PARAGRAPH
           END-IF

           MOVE 'N' TO WS-FIN-TARJETAS

           PERFORM UNTIL WS-FIN-TARJETAS = 'S'

      *        EXEC SQL
      *            FETCH CUR-TARJETAS INTO
      *                :HV-TARJ-ID-CLI,
      *                :HV-TARJ-NRO,
      *                :HV-TARJ-LIMITE,
      *                :HV-TARJ-ACUM,
      *                :HV-TARJ-LIQUID,
      *                :HV-TARJ-ESTADO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0009
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             HV-TARJ-NRO
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 16 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0010
           MOVE '3' TO SQL-TYPE(3)
           MOVE 7 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0011
           MOVE '3' TO SQL-TYPE(4)
           MOVE 7 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0012
           MOVE '3' TO SQL-TYPE(5)
           MOVE 7 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             HV-TARJ-ESTADO
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 1 TO SQL-LEN(6)
           MOVE 6 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0009 TO HV-TARJ-ID-CLI
           MOVE SQL-VAR-0010 TO HV-TARJ-LIMITE
           MOVE SQL-VAR-0011 TO HV-TARJ-ACUM
           MOVE SQL-VAR-0012 TO HV-TARJ-LIQUID

               EVALUATE SQLCODE
                   WHEN 0
      *                EXEC SQL
      *                    INSERT INTO AUDIT_TARJETAS (
      *                        PERIODO,       ID_CLIENTE,
      *                        NRO_TARJETA,   LIMITE_TARJETA,
      *                        ACUM_MES,      LIQUIDACION_MES,
      *                        ESTADO
      *                    ) VALUES (
      *                        :WS-PERIODO,   :HV-TARJ-ID-CLI,
      *                        :HV-TARJ-NRO,  :HV-TARJ-LIMITE,
      *                        :HV-TARJ-ACUM, :HV-TARJ-LIQUID,
      *                        :HV-TARJ-ESTADO
      *                    )
      *                END-EXEC
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-PERIODO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 HV-TARJ-NRO
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 16 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0010
               MOVE '3' TO SQL-TYPE(4)
               MOVE 7 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0011
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0012
               MOVE '3' TO SQL-TYPE(6)
               MOVE 7 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 HV-TARJ-ESTADO
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 1 TO SQL-LEN(7)
               MOVE 7 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-TARJ-ID-CLI
             TO SQL-VAR-0009
           MOVE HV-TARJ-LIMITE
             TO SQL-VAR-0010
           MOVE HV-TARJ-ACUM
             TO SQL-VAR-0011
           MOVE HV-TARJ-LIQUID
             TO SQL-VAR-0012
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA

                       IF SQLCODE = 0
                           ADD 1 TO WS-CTR-TARJETAS
                       ELSE
                           DISPLAY 'ERROR INSERT TARJETA NRO: '
                                   HV-TARJ-NRO
                                   ' SQLCODE: ' SQLCODE
                           ADD 1   TO WS-CTR-ERRORES
                           MOVE 'S' TO WS-ABORT
                           MOVE 'S' TO WS-FIN-TARJETAS
                       END-IF

                   WHEN 100
                       MOVE 'S' TO WS-FIN-TARJETAS

                   WHEN OTHER
                       DISPLAY 'ERROR FETCH CUR-TARJETAS: ' SQLCODE
                       ADD 1   TO WS-CTR-ERRORES
                       MOVE 'S' TO WS-ABORT
                       MOVE 'S' TO WS-FIN-TARJETAS
               END-EVALUATE

           END-PERFORM

      *    EXEC SQL CLOSE CUR-TARJETAS END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-1
                               SQLCA

           IF WS-ABORT = 'N'
               DISPLAY ' AUDIT_TARJETAS : ' WS-CTR-TARJETAS
                       ' REGISTROS INSERTADOS.'
           ELSE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR EN SNAPSHOT TARJETAS - SE HARA ROLLBACK'
                   TO LK-MENSAJE
           END-IF.

      *================================================================*
      *   5000 - SNAPSHOT AUDIT_HIPOTECAS                             *
      *================================================================*
       5000-SNAPSHOT-HIPOTECAS.
           DISPLAY '>>> INICIANDO SNAPSHOT AUDIT_HIPOTECAS...'

      *    EXEC SQL OPEN CUR-HIPOTECAS END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               MOVE 0 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-2
                               SQLCA
           END-CALL

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR ABRIENDO CUR-HIPOTECAS: ' SQLCODE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR CRITICO ABRIENDO CURSOR HIPOTECAS'
                   TO LK-MENSAJE
               MOVE 'S' TO WS-ABORT
               EXIT PARAGRAPH
           END-IF

           MOVE 'N' TO WS-FIN-HIPOTECAS

           PERFORM UNTIL WS-FIN-HIPOTECAS = 'S'

      *        EXEC SQL
      *            FETCH CUR-HIPOTECAS INTO
      *                :HV-HIPO-ID-HIPO,
      *                :HV-HIPO-ID-CLI,
      *                :HV-HIPO-MONTO-ORIG,
      *                :HV-HIPO-TASA,
      *                :HV-HIPO-SALDO,
      *                :HV-HIPO-FECHA-VENC,
      *                :HV-HIPO-ESTADO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0013
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             SQL-VAR-0014
           MOVE '3' TO SQL-TYPE(2)
           MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0015
           MOVE '3' TO SQL-TYPE(3)
           MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0016
           MOVE '3' TO SQL-TYPE(4)
           MOVE 4 TO SQL-LEN(4)
               MOVE X'04' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0017
           MOVE '3' TO SQL-TYPE(5)
           MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             HV-HIPO-FECHA-VENC
           MOVE 'X' TO SQL-TYPE(6)
           MOVE 10 TO SQL-LEN(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             HV-HIPO-ESTADO
           MOVE 'X' TO SQL-TYPE(7)
           MOVE 20 TO SQL-LEN(7)
           MOVE 7 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0013 TO HV-HIPO-ID-HIPO
           MOVE SQL-VAR-0014 TO HV-HIPO-ID-CLI
           MOVE SQL-VAR-0015 TO HV-HIPO-MONTO-ORIG
           MOVE SQL-VAR-0016 TO HV-HIPO-TASA
           MOVE SQL-VAR-0017 TO HV-HIPO-SALDO

               EVALUATE SQLCODE
                   WHEN 0
      *                EXEC SQL
      *                    INSERT INTO AUDIT_HIPOTECAS (
      *                        PERIODO,             ID_HIPOTECA,
      *                        ID_CLIENTE,          MONTO_ORIGINAL,
      *                        TASA_INTERES,        SALDO_ACTUAL,
      *                        FECHA_VENCTO,        ESTADO
      *                    ) VALUES (
      *                        :WS-PERIODO,         :HV-HIPO-ID-HIPO,
      *                        :HV-HIPO-ID-CLI,     :HV-HIPO-MONTO-ORIG,
      *                        :HV-HIPO-TASA,       :HV-HIPO-SALDO,
      *                        :HV-HIPO-FECHA-VENC, :HV-HIPO-ESTADO
      *                    )
      *                END-EXEC
           IF SQL-PREP OF SQL-STMT-7 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-PERIODO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0013
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0014
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0015
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0016
               MOVE '3' TO SQL-TYPE(5)
               MOVE 4 TO SQL-LEN(5)
               MOVE X'04' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0017
               MOVE '3' TO SQL-TYPE(6)
               MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
               SET SQL-ADDR(7) TO ADDRESS OF
                 HV-HIPO-FECHA-VENC
               MOVE 'X' TO SQL-TYPE(7)
               MOVE 10 TO SQL-LEN(7)
               SET SQL-ADDR(8) TO ADDRESS OF
                 HV-HIPO-ESTADO
               MOVE 'X' TO SQL-TYPE(8)
               MOVE 20 TO SQL-LEN(8)
               MOVE 8 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-7
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-HIPO-ID-HIPO
             TO SQL-VAR-0013
           MOVE HV-HIPO-ID-CLI
             TO SQL-VAR-0014
           MOVE HV-HIPO-MONTO-ORIG
             TO SQL-VAR-0015
           MOVE HV-HIPO-TASA
             TO SQL-VAR-0016
           MOVE HV-HIPO-SALDO
             TO SQL-VAR-0017
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA

                       IF SQLCODE = 0
                           ADD 1 TO WS-CTR-HIPOTECAS
                       ELSE
                           DISPLAY 'ERROR INSERT HIPOTECA ID: '
                                   HV-HIPO-ID-HIPO
                                   ' SQLCODE: ' SQLCODE
                           ADD 1   TO WS-CTR-ERRORES
                           MOVE 'S' TO WS-ABORT
                           MOVE 'S' TO WS-FIN-HIPOTECAS
                       END-IF

                   WHEN 100
                       MOVE 'S' TO WS-FIN-HIPOTECAS

                   WHEN OTHER
                       DISPLAY 'ERROR FETCH CUR-HIPOTECAS: ' SQLCODE
                       ADD 1   TO WS-CTR-ERRORES
                       MOVE 'S' TO WS-ABORT
                       MOVE 'S' TO WS-FIN-HIPOTECAS
               END-EVALUATE

           END-PERFORM

      *    EXEC SQL CLOSE CUR-HIPOTECAS END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-2
                               SQLCA

           IF WS-ABORT = 'N'
               DISPLAY ' AUDIT_HIPOTECAS: ' WS-CTR-HIPOTECAS
                       ' REGISTROS INSERTADOS.'
           ELSE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR EN SNAPSHOT HIPOTECAS - SE HARA ROLLBACK'
                   TO LK-MENSAJE
           END-IF.

      *================================================================*
      *   5100 - PROCESAR MORA EN HIPOTECAS                           *
      *================================================================*
       5100-PROCESAR-MORA-HIPOTECAS.
           DISPLAY '>>> PROCESANDO MORA EN HIPOTECAS...'

      *    EXEC SQL OPEN CUR-MORA END-EXEC
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               MOVE 0 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-3
                               SQLCA
           END-CALL

           IF SQLCODE NOT = 0
               DISPLAY 'ERROR ABRIENDO CUR-MORA: ' SQLCODE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR CRITICO ABRIENDO CURSOR MORA'
                   TO LK-MENSAJE
               MOVE 'S' TO WS-ABORT
               EXIT PARAGRAPH
           END-IF

           MOVE 'N' TO WS-FIN-MORA

           PERFORM UNTIL WS-FIN-MORA = 'S'

      *        EXEC SQL
      *            FETCH CUR-MORA INTO
      *                :HV-MORA-ID-HIPO,
      *                :HV-MORA-ID-CLI,
      *                :HV-MORA-SALDO-ACT,
      *                :HV-MORA-MONTO-ORIG,
      *                :HV-MORA-ESTADO,
      *                :HV-MORA-MESES-TOT,
      *                :HV-MORA-MESES-TRANS,
      *                :HV-MORA-PAGO-MENS,
      *                :HV-MORA-SALDO-ESP,
      *                :HV-MORA-MESES-MORA
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0018
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             SQL-VAR-0019
           MOVE '3' TO SQL-TYPE(2)
           MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0020
           MOVE '3' TO SQL-TYPE(3)
           MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0021
           MOVE '3' TO SQL-TYPE(4)
           MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             HV-MORA-ESTADO
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 20 TO SQL-LEN(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             SQL-VAR-0022
           MOVE '3' TO SQL-TYPE(6)
           MOVE 3 TO SQL-LEN(6)
               MOVE X'00' TO SQL-PREC(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             SQL-VAR-0023
           MOVE '3' TO SQL-TYPE(7)
           MOVE 3 TO SQL-LEN(7)
               MOVE X'00' TO SQL-PREC(7)
           SET SQL-ADDR(8) TO ADDRESS OF
             SQL-VAR-0024
           MOVE '3' TO SQL-TYPE(8)
           MOVE 8 TO SQL-LEN(8)
               MOVE X'02' TO SQL-PREC(8)
           SET SQL-ADDR(9) TO ADDRESS OF
             SQL-VAR-0025
           MOVE '3' TO SQL-TYPE(9)
           MOVE 8 TO SQL-LEN(9)
               MOVE X'02' TO SQL-PREC(9)
           SET SQL-ADDR(10) TO ADDRESS OF
             SQL-VAR-0026
           MOVE '3' TO SQL-TYPE(10)
           MOVE 3 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
           MOVE 10 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0018 TO HV-MORA-ID-HIPO
           MOVE SQL-VAR-0019 TO HV-MORA-ID-CLI
           MOVE SQL-VAR-0020 TO HV-MORA-SALDO-ACT
           MOVE SQL-VAR-0021 TO HV-MORA-MONTO-ORIG
           MOVE SQL-VAR-0022 TO HV-MORA-MESES-TOT
           MOVE SQL-VAR-0023 TO HV-MORA-MESES-TRANS
           MOVE SQL-VAR-0024 TO HV-MORA-PAGO-MENS
           MOVE SQL-VAR-0025 TO HV-MORA-SALDO-ESP
           MOVE SQL-VAR-0026 TO HV-MORA-MESES-MORA

               EVALUATE SQLCODE
                   WHEN 0
      *                Verificar si el saldo actual es mayor al esperado
                       IF HV-MORA-SALDO-ACT > HV-MORA-SALDO-ESP

      *                    Determinar nuevo estado segun meses en mora
                           IF HV-MORA-MESES-MORA >= 3
                               MOVE 'CASTIGADO' TO HV-MORA-NUEVO-ESTADO
                               ADD 1 TO WS-CTR-CASTIGADAS
                               DISPLAY ' HIPOTECA CASTIGADA: '
                                       HV-MORA-ID-HIPO
                                       ' CLIENTE: '
                                       HV-MORA-ID-CLI
                           ELSE
                               MOVE 'MOROSO' TO HV-MORA-NUEVO-ESTADO
                               ADD 1 TO WS-CTR-MOROSAS
                               DISPLAY ' HIPOTECA MOROSA: '
                                       HV-MORA-ID-HIPO
                                       ' CLIENTE: '
                                       HV-MORA-ID-CLI
                           END-IF

      *                    Actualizar estado de la hipoteca
      *                    EXEC SQL
      *                        UPDATE HIPOTECAS
      *                        SET    ESTADO = :HV-MORA-NUEVO-ESTADO
      *                        WHERE  ID_HIPOTECA = :HV-MORA-ID-HIPO
      *                    END-EXEC
           IF SQL-PREP OF SQL-STMT-8 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 HV-MORA-NUEVO-ESTADO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 20 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0018
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-8
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-MORA-ID-HIPO
             TO SQL-VAR-0018
           CALL 'OCSQLEXE' USING SQL-STMT-8
                               SQLCA

                           IF SQLCODE NOT = 0
                               DISPLAY 'ERROR UPDATE ESTADO HIPOTECA: '
                                       HV-MORA-ID-HIPO
                                       ' SQLCODE: ' SQLCODE
                               ADD 1 TO WS-CTR-ERRORES
                               MOVE 'S' TO WS-ABORT
                               MOVE 'S' TO WS-FIN-MORA
                           END-IF

      *                    Consultar saldo actual del cliente
      *                    EXEC SQL
      *                        SELECT SALDO_CLIENTE
      *                        INTO   :HV-MORA-CLI-SALDO
      *                        FROM   clientes
      *                        WHERE  ID_CLIENTE = :HV-MORA-ID-CLI
      *                    END-EXEC
           IF SQL-PREP OF SQL-STMT-9 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0027
               MOVE '3' TO SQL-TYPE(1)
               MOVE 7 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0019
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-9
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-MORA-ID-CLI TO SQL-VAR-0019
           CALL 'OCSQLEXE' USING SQL-STMT-9
                               SQLCA
           MOVE SQL-VAR-0027 TO HV-MORA-CLI-SALDO

                           IF SQLCODE = 0
      *                        Si el cliente tiene fondos descontar
                               IF HV-MORA-CLI-SALDO >=
                                  HV-MORA-PAGO-MENS
      *                            EXEC SQL
      *                                UPDATE clientes
      *                                SET    SALDO_CLIENTE =
      *                                       SALDO_CLIENTE -
      *                                       :HV-MORA-PAGO-MENS
      *                                WHERE  ID_CLIENTE =
      *                                       :HV-MORA-ID-CLI
      *                            END-EXEC
           IF SQL-PREP OF SQL-STMT-10 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0024
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0019
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-10
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-MORA-PAGO-MENS
             TO SQL-VAR-0024
           MOVE HV-MORA-ID-CLI
             TO SQL-VAR-0019
           CALL 'OCSQLEXE' USING SQL-STMT-10
                               SQLCA
                                   IF SQLCODE NOT = 0
                                       DISPLAY 'ERROR DESCUENTO CLI: '
                                               HV-MORA-ID-CLI
                                               ' SQLCODE: ' SQLCODE
                                       ADD 1 TO WS-CTR-ERRORES
                                       MOVE 'S' TO WS-ABORT
                                       MOVE 'S' TO WS-FIN-MORA
                                   ELSE
                                       DISPLAY ' DESCUENTO APLICADO'
                                               ' CLI: '
                                               HV-MORA-ID-CLI
                                               ' MONTO: '
                                               HV-MORA-PAGO-MENS
                                   END-IF
                               ELSE
                                   DISPLAY ' SIN FONDOS CLI: '
                                           HV-MORA-ID-CLI
                                           ' SALDO: '
                                           HV-MORA-CLI-SALDO
                               END-IF
                           ELSE
                               DISPLAY 'ERROR CONSULTA SALDO CLI: '
                                       HV-MORA-ID-CLI
                                       ' SQLCODE: ' SQLCODE
                               ADD 1 TO WS-CTR-ERRORES
                           END-IF

                       ELSE
      *                    Saldo correcto, hipoteca activa
                           IF HV-MORA-ESTADO NOT = 'ACTIVO'
      *                        EXEC SQL
      *                            UPDATE HIPOTECAS
      *                            SET    ESTADO = 'ACTIVO'
      *                            WHERE  ID_HIPOTECA =
      *                                   :HV-MORA-ID-HIPO
      *                        END-EXEC
           IF SQL-PREP OF SQL-STMT-11 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0018
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-11
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-MORA-ID-HIPO
             TO SQL-VAR-0018
           CALL 'OCSQLEXE' USING SQL-STMT-11
                               SQLCA
                           END-IF
                       END-IF

                   WHEN 100
                       MOVE 'S' TO WS-FIN-MORA

                   WHEN OTHER
                       DISPLAY 'ERROR FETCH CUR-MORA: ' SQLCODE
                       ADD 1   TO WS-CTR-ERRORES
                       MOVE 'S' TO WS-ABORT
                       MOVE 'S' TO WS-FIN-MORA
               END-EVALUATE

           END-PERFORM

      *    EXEC SQL CLOSE CUR-MORA END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-3
                               SQLCA

           IF WS-ABORT = 'N'
               DISPLAY ' HIPOTECAS MOROSAS   : ' WS-CTR-MOROSAS
               DISPLAY ' HIPOTECAS CASTIGADAS: ' WS-CTR-CASTIGADAS
           ELSE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR EN PROC. MORA - SE HARA ROLLBACK'
                   TO LK-MENSAJE
           END-IF.

      *================================================================*
      *   5200 - DAR DE BAJA TARJETAS VENCIDAS                        *
      *                                                                *
      *================================================================*
       5200-BAJA-TARJETAS-VENCIDAS.
           DISPLAY '>>> BAJA DE TARJETAS VENCIDAS...'

      *    EXEC SQL
      *        UPDATE TARJETAS
      *        SET    ESTADO = 'I'
      *        WHERE  FECHA_VENCIMIENTO < CURDATE()
      *        AND    ESTADO = 'A'
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-12 = 'N'
               MOVE 0 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-12
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-12
                               SQLCA

           IF SQLCODE = 0
               MOVE SQLERRD(3)       TO WS-CTR-TARJ-BAJA
               DISPLAY ' TARJETAS DADAS DE BAJA: '
                       WS-CTR-TARJ-BAJA
           ELSE
               DISPLAY 'ERROR EN BAJA TARJETAS: ' SQLCODE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR EN BAJA TARJETAS - SE HARA ROLLBACK'
                   TO LK-MENSAJE
               MOVE 'S' TO WS-ABORT
               ADD 1 TO WS-CTR-ERRORES
           END-IF.

      *================================================================*
      *   6000 - RESET OPERATIVO: CIERRE DE TARJETAS                  *
      *   Mueve ACUM_MES a LIQUIDACION_MES y resetea ACUM_MES a 0    *
      *   Solo aplica a tarjetas ACTIVAS (ESTADO = 'A')               *
      *================================================================*
       6000-RESET-OPERATIVO.
           DISPLAY '>>> RESET OPERATIVO TARJETAS...'

      *    EXEC SQL
      *        UPDATE TARJETAS
      *        SET    LIQUIDACION_MES = ACUM_MES,
      *               ACUM_MES        = 0
      *        WHERE  ESTADO = 'A'
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-13 = 'N'
               MOVE 0 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-13
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-13
                               SQLCA

           IF SQLCODE = 0
               DISPLAY ' RESET TARJETAS: OK'
           ELSE
               DISPLAY 'ERROR EN RESET TARJETAS. SQLCODE: ' SQLCODE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR EN RESET OPERATIVO - SE HARA ROLLBACK'
                   TO LK-MENSAJE
               MOVE 'S' TO WS-ABORT
               ADD 1 TO WS-CTR-ERRORES
           END-IF.

      *================================================================*
      *   7000 - FINALIZAR: COMMIT GLOBAL O ROLLBACK                  *
      *================================================================*
       7000-FINALIZAR.
           DISPLAY '================================================'
           DISPLAY ' RESUMEN - CIERRE MENSUAL PERIODO: ' WS-PERIODO
           DISPLAY ' CLIENTES  PROCESADOS : ' WS-CTR-CLIENTES
           DISPLAY ' TARJETAS  PROCESADAS : ' WS-CTR-TARJETAS
           DISPLAY ' HIPOTECAS PROCESADAS : ' WS-CTR-HIPOTECAS
           DISPLAY ' HIPOTECAS MOROSAS    : ' WS-CTR-MOROSAS
           DISPLAY ' HIPOTECAS CASTIGADAS : ' WS-CTR-CASTIGADAS
           DISPLAY ' TARJETAS DADAS BAJA  : ' WS-CTR-TARJ-BAJA
           DISPLAY ' ERRORES   DETECTADOS : ' WS-CTR-ERRORES
           DISPLAY '================================================'

           IF WS-ABORT = 'N' AND WS-CTR-ERRORES = 0
      *        EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
               MOVE 0  TO LK-COD-RETORNO
               MOVE 'CIERRE MENSUAL COMPLETADO EXITOSAMENTE'
                   TO LK-MENSAJE
               DISPLAY 'COMMIT REALIZADO.'
           ELSE
      *        EXEC SQL ROLLBACK END-EXEC
           CALL 'OCSQLRBK' USING SQLCA END-CALL
               IF LK-COD-RETORNO = 0
                   MOVE 50 TO LK-COD-RETORNO
               END-IF
               MOVE 'CIERRE FALLIDO - DATOS REVERTIDOS'
                   TO LK-MENSAJE
               DISPLAY 'ROLLBACK EJECUTADO - DATOS REVERTIDOS.'
           END-IF

           DISPLAY ' RESULTADO: ' LK-MENSAJE
           DISPLAY '================================================'.

      *================================================================*
      *   9000 - EVALUAR SQL             *
      *================================================================*
       9000-EVALUAR-SQL.
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 00 TO LK-COD-RETORNO
               WHEN 100
                   MOVE 01 TO LK-COD-RETORNO
                   MOVE 'REGISTRO NO ENCONTRADO' TO LK-MENSAJE
               WHEN OTHER
                   MOVE 99 TO LK-COD-RETORNO
                   MOVE 'ERROR CRITICO EN BASE DE DATOS'
                       TO LK-MENSAJE
           END-EVALUATE.

       END PROGRAM BAT000.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  CUR-HIPOTECAS            IN USE CURSOR
      *  CUR-MAESTRA              IN USE CURSOR
      *  CUR-MORA                 IN USE CURSOR
      *  CUR-TARJETAS             IN USE CURSOR
      *  HV-APELLIDOS             IN USE CHAR(25)
      *  HV-COD-ULT-MOV           IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(3,0)
      *  HV-COUNT-PERIODO         IN USE THROUGH TEMP VAR SQL-VAR-0028 DECIMAL(7,0)
      *  HV-CTA-ACTIVA            IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(1,0)
      *  HV-DOC-CLIENTE           IN USE CHAR(12)
      *  HV-FECHA-ALTA            IN USE CHAR(10)
      *  HV-FECHA-ULT-MOV         IN USE CHAR(10)
      *  HV-HIPO-ESTADO           IN USE CHAR(20)
      *  HV-HIPO-FECHA-VENC       IN USE CHAR(10)
      *  HV-HIPO-ID-CLI           IN USE THROUGH TEMP VAR SQL-VAR-0014 DECIMAL(9,0)
      *  HV-HIPO-ID-HIPO          IN USE THROUGH TEMP VAR SQL-VAR-0013 DECIMAL(9,0)
      *  HV-HIPO-MONTO-ORIG       IN USE THROUGH TEMP VAR SQL-VAR-0015 DECIMAL(15,2)
      *  HV-HIPO-SALDO            IN USE THROUGH TEMP VAR SQL-VAR-0017 DECIMAL(15,2)
      *  HV-HIPO-TASA             IN USE THROUGH TEMP VAR SQL-VAR-0016 DECIMAL(7,4)
      *  HV-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  HV-IMPORTE-MOV           IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(13,2)
      *  HV-IND-COD-MOV           IN USE INTEGER(2 BYTES)
      *  HV-IND-FECHA-MOV         IN USE INTEGER(2 BYTES)
      *  HV-IND-IMPORTE-MOV       IN USE INTEGER(2 BYTES)
      *  HV-IND-SALDO-CTA         IN USE INTEGER(2 BYTES)
      *  HV-MORA-CLI-SALDO        IN USE THROUGH TEMP VAR SQL-VAR-0027 DECIMAL(13,2)
      *  HV-MORA-ESTADO           IN USE CHAR(20)
      *  HV-MORA-ID-CLI           IN USE THROUGH TEMP VAR SQL-VAR-0019 DECIMAL(9,0)
      *  HV-MORA-ID-HIPO          IN USE THROUGH TEMP VAR SQL-VAR-0018 DECIMAL(9,0)
      *  HV-MORA-MESES-MORA       IN USE THROUGH TEMP VAR SQL-VAR-0026 DECIMAL(5,0)
      *  HV-MORA-MESES-TOT        IN USE THROUGH TEMP VAR SQL-VAR-0022 DECIMAL(5,0)
      *  HV-MORA-MESES-TRANS      IN USE THROUGH TEMP VAR SQL-VAR-0023 DECIMAL(5,0)
      *  HV-MORA-MONTO-ORIG       IN USE THROUGH TEMP VAR SQL-VAR-0021 DECIMAL(15,2)
      *  HV-MORA-NUEVO-ESTADO     IN USE CHAR(20)
      *  HV-MORA-PAGO-MENS        IN USE THROUGH TEMP VAR SQL-VAR-0024 DECIMAL(15,2)
      *  HV-MORA-SALDO-ACT        IN USE THROUGH TEMP VAR SQL-VAR-0020 DECIMAL(15,2)
      *  HV-MORA-SALDO-ESP        IN USE THROUGH TEMP VAR SQL-VAR-0025 DECIMAL(15,2)
      *  HV-NOMBRE                IN USE CHAR(25)
      *  HV-SALDO-CLI             IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(13,2)
      *  HV-SALDO-CTA             IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(13,2)
      *  HV-TARJ-ACUM             IN USE THROUGH TEMP VAR SQL-VAR-0011 DECIMAL(13,2)
      *  HV-TARJ-ESTADO           IN USE CHAR(1)
      *  HV-TARJ-ID-CLI           IN USE THROUGH TEMP VAR SQL-VAR-0009 DECIMAL(9,0)
      *  HV-TARJ-LIMITE           IN USE THROUGH TEMP VAR SQL-VAR-0010 DECIMAL(13,2)
      *  HV-TARJ-LIQUID           IN USE THROUGH TEMP VAR SQL-VAR-0012 DECIMAL(13,2)
      *  HV-TARJ-NRO              IN USE CHAR(16)
      *  HV-TIENE-HIPOTECA        IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(1,0)
      *  HV-TIENE-TARJETA         IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(1,0)
      *  HV-TIPO-DOC              IN USE CHAR(3)
      *  WS-HOST-HIPOTECAS    NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-ESTADO NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-FECHA-VENC NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-ID-CLI NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-ID-HIPO NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-MONTO-ORIG NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-SALDO NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-TASA NOT IN USE
      *  WS-HOST-MAESTRA      NOT IN USE
      *  WS-HOST-MAESTRA.HV-APELLIDOS NOT IN USE
      *  WS-HOST-MAESTRA.HV-COD-ULT-MOV NOT IN USE
      *  WS-HOST-MAESTRA.HV-CTA-ACTIVA NOT IN USE
      *  WS-HOST-MAESTRA.HV-DOC-CLIENTE NOT IN USE
      *  WS-HOST-MAESTRA.HV-FECHA-ALTA NOT IN USE
      *  WS-HOST-MAESTRA.HV-FECHA-ULT-MOV NOT IN USE
      *  WS-HOST-MAESTRA.HV-ID-CLIENTE NOT IN USE
      *  WS-HOST-MAESTRA.HV-IMPORTE-MOV NOT IN USE
      *  WS-HOST-MAESTRA.HV-IND-COD-MOV NOT IN USE
      *  WS-HOST-MAESTRA.HV-IND-FECHA-MOV NOT IN USE
      *  WS-HOST-MAESTRA.HV-IND-IMPORTE-MOV NOT IN USE
      *  WS-HOST-MAESTRA.HV-IND-SALDO-CTA NOT IN USE
      *  WS-HOST-MAESTRA.HV-NOMBRE NOT IN USE
      *  WS-HOST-MAESTRA.HV-SALDO-CLI NOT IN USE
      *  WS-HOST-MAESTRA.HV-SALDO-CTA NOT IN USE
      *  WS-HOST-MAESTRA.HV-TIENE-HIPOTECA NOT IN USE
      *  WS-HOST-MAESTRA.HV-TIENE-TARJETA NOT IN USE
      *  WS-HOST-MAESTRA.HV-TIPO-DOC NOT IN USE
      *  WS-HOST-MORA         NOT IN USE
      *  WS-HOST-MORA.HV-MORA-CLI-SALDO NOT IN USE
      *  WS-HOST-MORA.HV-MORA-ESTADO NOT IN USE
      *  WS-HOST-MORA.HV-MORA-ID-CLI NOT IN USE
      *  WS-HOST-MORA.HV-MORA-ID-HIPO NOT IN USE
      *  WS-HOST-MORA.HV-MORA-MESES-MORA NOT IN USE
      *  WS-HOST-MORA.HV-MORA-MESES-TOT NOT IN USE
      *  WS-HOST-MORA.HV-MORA-MESES-TRANS NOT IN USE
      *  WS-HOST-MORA.HV-MORA-MONTO-ORIG NOT IN USE
      *  WS-HOST-MORA.HV-MORA-NUEVO-ESTADO NOT IN USE
      *  WS-HOST-MORA.HV-MORA-PAGO-MENS NOT IN USE
      *  WS-HOST-MORA.HV-MORA-SALDO-ACT NOT IN USE
      *  WS-HOST-MORA.HV-MORA-SALDO-ESP NOT IN USE
      *  WS-HOST-TARJETAS     NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-ACUM NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-ESTADO NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-ID-CLI NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-LIMITE NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-LIQUID NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-NRO NOT IN USE
      *  WS-PERIODO               IN USE CHAR(6)
      **********************************************************************

      *================================================================*
      * PROGRAMA : BAT000.sqb                                         *
      * FUNCION  : PROCESO BATCH DE CIERRE MENSUAL                    *
      *            - CONSOLIDACION                                    *
      *            - PROCESAR MORA EN HIPOTECAS                       *
      *            - DESCONTAR CUOTA DE CUENTA SI HAY FONDOS          *
      *            - DAR DE BAJA TARJETAS VENCIDAS                    *
      *            - RESET OPERATIVO DE TARJETAS                      *
      *            - ROLLBACK si algo falla
      * LLAMADO  : BANCSMENU (OPCION 5)                               *
      * PRECOMP  : esqlOC BAT000.sqb  ->  BAT000.cob                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BAT000.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 12.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 12 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 12 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 12 TIMES.
           05 SQL-PREC   PIC X OCCURS 12 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 286.
           05 SQL-STMT   PIC X(286) VALUE 'SELECT C.ID_CLIENTE,C.TIPO_DO
      -    'C,C.DOC_CLIENTE,C.FECHA_ALTA,C.NOMBRE_CLIENTE,C.APELLIDOS_CL
      -    'IENTE,C.SALDO_TOTAL_VISTA,C.ESTADO_CLIENTE,C.TIENE_TARJETA,C
      -    '.TIENE_HIPOTECA,COALESCE((SELECT SUM(SALDO_ACTUAL) FROM CTAC
      -    'TES WHERE ID_CLIENTE = C.ID_CLIENTE),0) FROM CLIENTES C ORDE
      -    'R BY C.ID_CLIENTE'.
           05 SQL-CNAME  PIC X(11) VALUE 'CUR-MAESTRA'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 120.
           05 SQL-STMT   PIC X(120) VALUE 'SELECT ID_CLIENTE,NRO_TARJETA
      -    ',CUPO_APROBADO,SALDO_UTILIZADO,ESTADO_TARJETA FROM TARJETAS 
      -    'ORDER BY ID_CLIENTE,NRO_TARJETA'.
           05 SQL-CNAME  PIC X(12) VALUE 'CUR-TARJETAS'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 148.
           05 SQL-STMT   PIC X(148) VALUE 'SELECT ID_HIPOTECA,ID_CLIENTE
      -    ',MONTO_PRESTAMO,TASA_ANUAL,SALDO_DEUDA,FECHA_VENCIMIENTO,EST
      -    'ADO_PRESTAMO FROM HIPOTECAS ORDER BY ID_CLIENTE,ID_HIPOTECA'
           .
           05 SQL-CNAME  PIC X(13) VALUE 'CUR-HIPOTECAS'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 308.
           05 SQL-STMT   PIC X(308) VALUE 'SELECT H.ID_HIPOTECA,H.ID_CLI
      -    'ENTE,H.SALDO_DEUDA,H.MONTO_PRESTAMO,H.ESTADO_PRESTAMO,H.CUOT
      -    'A_MENSUAL,H.CUOTA_MENSUAL * GREATEST(TIMESTAMPDIFF(MONTH,CUR
      -    'DATE(),H.FECHA_VENCIMIENTO),0),H.MESES_MORA,H.CUENTA_DEBITO 
      -    'FROM HIPOTECAS H WHERE H.ESTADO_PRESTAMO IN (''ACTIVO'',''MO
      -    'ROSO'') ORDER BY H.ID_CLIENTE,H.ID_HIPOTECA'.
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
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 12.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 216.
           05 SQL-STMT   PIC X(216) VALUE 'INSERT INTO AUDIT_MAESTRA (PE
      -    'RIODO,ID_CLIENTE,TIPO_DOC,DOC_CLIENTE,FECHA_ALTA,NOMBRE_CLIE
      -    'NTE,APELLIDOS_CLIENTE,SALDO_TOTAL_VISTA,ESTADO_CLIENTE,TIENE
      -    '_TARJETA,TIENE_HIPOTECA,SALDO_CTA) VALUES (?,?,?,?,?,?,?,?,?
      -    ',?,?,?)'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 6.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 125.
           05 SQL-STMT   PIC X(125) VALUE 'INSERT INTO AUDIT_TARJETAS (P
      -    'ERIODO,ID_CLIENTE,NRO_TARJETA,CUPO_APROBADO,SALDO_UTILIZADO,
      -    'ESTADO_TARJETA) VALUES (?,?,?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-7.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 8.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 157.
           05 SQL-STMT   PIC X(157) VALUE 'INSERT INTO AUDIT_HIPOTECAS (
      -    'PERIODO,ID_HIPOTECA,ID_CLIENTE,MONTO_PRESTAMO,TASA_ANUAL,SAL
      -    'DO_DEUDA,FECHA_VENCIMIENTO,ESTADO_PRESTAMO) VALUES (?,?,?,?,
      -    '?,?,?,?)'.
      **********************************************************************
       01 SQL-STMT-8.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 90.
           05 SQL-STMT   PIC X(90) VALUE 'UPDATE HIPOTECAS SET ESTADO_PR
      -    'ESTAMO = ?,MESES_MORA = MESES_MORA + 1 WHERE ID_HIPOTECA = ?
      -    ''.
      **********************************************************************
       01 SQL-STMT-9.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 76.
           05 SQL-STMT   PIC X(76) VALUE 'SELECT SALDO_ACTUAL FROM CTACT
      -    'ES WHERE ID_CUENTA = ? AND ESTADO_CUENTA = ''A'''.
      **********************************************************************
       01 SQL-STMT-10.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'UPDATE CTACTES SET SALDO_ACTUA
      -    'L = SALDO_ACTUAL - ? WHERE ID_CUENTA = ?'.
      **********************************************************************
       01 SQL-STMT-11.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 72.
           05 SQL-STMT   PIC X(72) VALUE 'UPDATE HIPOTECAS SET SALDO_DEU
      -    'DA = SALDO_DEUDA - ? WHERE ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-12.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 84.
           05 SQL-STMT   PIC X(84) VALUE 'UPDATE HIPOTECAS SET ESTADO_PR
      -    'ESTAMO = ''ACTIVO'',MESES_MORA = 0 WHERE ID_HIPOTECA = ?'.
      **********************************************************************
       01 SQL-STMT-13.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 92.
           05 SQL-STMT   PIC X(92) VALUE 'UPDATE TARJETAS SET ESTADO_TAR
      -    'JETA = ? WHERE FECHA_VENCTO < CURDATE() AND ESTADO_TARJETA =
      -    ' ?'.
      **********************************************************************
       01 SQL-STMT-14.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 64.
           05 SQL-STMT   PIC X(64) VALUE 'UPDATE TARJETAS SET SALDO_UTIL
      -    'IZADO = 0 WHERE ESTADO_TARJETA = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0003  PIC S9(1) COMP-3.
           05 SQL-VAR-0004  PIC S9(1) COMP-3.
           05 SQL-VAR-0005  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0006  PIC S9(9) COMP-3.
           05 SQL-VAR-0007  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0008  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0009  PIC S9(9) COMP-3.
           05 SQL-VAR-0010  PIC S9(9) COMP-3.
           05 SQL-VAR-0011  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0012  PIC S9(3)V9(4) COMP-3.
           05 SQL-VAR-0013  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0014  PIC S9(9) COMP-3.
           05 SQL-VAR-0015  PIC S9(9) COMP-3.
           05 SQL-VAR-0016  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0017  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0018  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0019  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0020  PIC S9(5) COMP-3.
           05 SQL-VAR-0022  PIC S9(9) COMP-3.
           05 SQL-VAR-0023  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0024  PIC S9(7) COMP-3.
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

      *    Variables host - AUDIT_MAESTRA / CLIENTES + CTACTES
      *    Adaptado a schema v3: SALDO_TOTAL_VISTA, ESTADO_CLIENTE,
      *    TIENE_TARJETA, TIENE_HIPOTECA. CTACTES solo expone
      *    SALDO_ACTUAL (no hay COD/FECHA/IMPORTE de ultimo movimiento).
       01  WS-HOST-MAESTRA.
           05 HV-ID-CLIENTE        PIC 9(8).
           05 HV-TIPO-DOC          PIC X(3).
           05 HV-DOC-CLIENTE       PIC X(12).
           05 HV-FECHA-ALTA        PIC X(10).
           05 HV-NOMBRE            PIC X(25).
           05 HV-APELLIDOS         PIC X(25).
           05 HV-SALDO-CLI         PIC S9(10)V99.
           05 HV-ESTADO-CLI        PIC X(1).
           05 HV-TIENE-TARJETA     PIC 9(1).
           05 HV-TIENE-HIPOTECA    PIC 9(1).
           05 HV-SALDO-CTA         PIC S9(10)V99.
      *    Indicador de NULL para el LEFT JOIN con CTACTES
           05 HV-IND-SALDO-CTA     PIC S9(4) COMP-5.

      *    Variables host - AUDIT_TARJETAS (schema v3: CUPO_APROBADO,
      *    SALDO_UTILIZADO, ESTADO_TARJETA. LIQUIDACION_MES eliminada.)
       01  WS-HOST-TARJETAS.
           05 HV-TARJ-ID-CLI       PIC 9(8).
           05 HV-TARJ-NRO          PIC X(16).
           05 HV-TARJ-LIMITE       PIC S9(10)V99.
           05 HV-TARJ-ACUM         PIC S9(10)V99.
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
           05 HV-MORA-PAGO-MENS    PIC S9(13)V99.
           05 HV-MORA-SALDO-ESP    PIC S9(13)V99.
           05 HV-MORA-MESES-MORA   PIC 9(4).

      *    Variables host para UPDATE de estado hipoteca y descuento
           05 HV-MORA-NUEVO-ESTADO PIC X(20).
           05 HV-MORA-CLI-SALDO    PIC S9(10)V99.
      *    Cuenta de debito (FK a ctactes) y saldo de esa cuenta.
      *    El debito mensual se aplica sobre CTACTES, no sobre el
      *    agregado CLIENTES.SALDO_TOTAL_VISTA.
           05 HV-MORA-CUENTA-DEB   PIC 9(8).
           05 HV-MORA-CTA-SALDO    PIC S9(13)V99.

      *    Variables host para estados de tarjeta (evita literales SQL)
       01  WS-HOST-ESTADOS.
           05 HV-ESTADO-INACTIVO   PIC X(1).
           05 HV-ESTADO-ACTIVO     PIC X(1).

      *    Verificacion de periodo duplicado
       01  HV-COUNT-PERIODO        PIC 9(6).

      *    EXEC SQL END DECLARE SECTION END-EXEC.

      *================================================================*
      *   DECLARACION DE CURSORES                                      *
      *================================================================*

      *    NOTA: CTACTES es 1:N (multi-cuenta). Un LEFT JOIN
      *    plano produce filas duplicadas por cliente y rompe la
      *    UNIQUE (PERIODO, ID_CLIENTE) de AUDIT_MAESTRA con
      *    SQLCODE -1062. Sumamos los saldos via subquery para
      *    obtener EXACTAMENTE una fila por cliente. COALESCE
      *    cubre clientes sin cuenta (antes LEFT JOIN devolvia
      *    NULL).
      *    EXEC SQL DECLARE CUR-MAESTRA CURSOR FOR
      *        SELECT C.ID_CLIENTE,
      *               C.TIPO_DOC,
      *               C.DOC_CLIENTE,
      *               C.FECHA_ALTA,
      *               C.NOMBRE_CLIENTE,
      *               C.APELLIDOS_CLIENTE,
      *               C.SALDO_TOTAL_VISTA,
      *               C.ESTADO_CLIENTE,
      *               C.TIENE_TARJETA,
      *               C.TIENE_HIPOTECA,
      *               COALESCE((SELECT SUM(SALDO_ACTUAL)
      *                         FROM   CTACTES
      *                         WHERE  ID_CLIENTE = C.ID_CLIENTE), 0)
      *        FROM   CLIENTES C
      *        ORDER BY C.ID_CLIENTE
      *    END-EXEC.

      *    EXEC SQL DECLARE CUR-TARJETAS CURSOR FOR
      *        SELECT ID_CLIENTE,
      *               NRO_TARJETA,
      *               CUPO_APROBADO,
      *               SALDO_UTILIZADO,
      *               ESTADO_TARJETA
      *        FROM   TARJETAS
      *        ORDER BY ID_CLIENTE, NRO_TARJETA
      *    END-EXEC.

      *    EXEC SQL DECLARE CUR-HIPOTECAS CURSOR FOR
      *        SELECT ID_HIPOTECA,
      *               ID_CLIENTE,
      *               MONTO_PRESTAMO,
      *               TASA_ANUAL,
      *               SALDO_DEUDA,
      *               FECHA_VENCIMIENTO,
      *               ESTADO_PRESTAMO
      *        FROM   HIPOTECAS
      *        ORDER BY ID_CLIENTE, ID_HIPOTECA
      *    END-EXEC.

      *    Cursor de mora: usa CUOTA_MENSUAL y MESES_MORA de la BD.
      *    SALDO_ESPERADO = cuota * meses_restantes_hasta_vencimiento.
      *    Schema v3 no tiene FECHA_INICIO: calculamos meses restantes
      *    directamente con TIMESTAMPDIFF(CURDATE, FECHA_VENCIMIENTO).
      *    GREATEST(...,0) evita saldos negativos para creditos vencidos
      *    EXEC SQL DECLARE CUR-MORA CURSOR FOR
      *        SELECT H.ID_HIPOTECA,
      *               H.ID_CLIENTE,
      *               H.SALDO_DEUDA,
      *               H.MONTO_PRESTAMO,
      *               H.ESTADO_PRESTAMO,
      *               H.CUOTA_MENSUAL,
      *               H.CUOTA_MENSUAL * GREATEST(
      *                   TIMESTAMPDIFF(MONTH,
      *                       CURDATE(), H.FECHA_VENCIMIENTO),
      *                   0),
      *               H.MESES_MORA,
      *               H.CUENTA_DEBITO
      *        FROM   HIPOTECAS H
      *        WHERE  H.ESTADO_PRESTAMO IN ('ACTIVO', 'MOROSO')
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
           05 WS-CTR-REGULARIZADAS PIC 9(6) VALUE 0.
           05 WS-CTR-TARJ-BAJA     PIC 9(6) VALUE 0.

      *================================================================*
      *   WORKING STORAGE - LOG A ARCHIVO                              *
      *================================================================*
       01  WS-LOG-LINE             PIC X(200).
       01  WS-MODULO-LOG           PIC X(03) VALUE 'BAT'.
       01  WS-CLS-CMD              PIC X(04) VALUE 'cls'.
       01  WS-AUX-SQLCODE          PIC +9(9).
       01  WS-AUX-ID8              PIC 9(8).
       01  WS-AUX-ID9              PIC 9(9).
       01  WS-AUX-CTR              PIC 9(6).
       01  WS-AUX-MONTO13         PIC -9(13).99.
       01  WS-AUX-MONTO10         PIC -9(10).99.
       01  WS-TECLA                PIC X(20).

      *================================================================*
      *   LINKAGE SECTION                                              *
      *================================================================*
       LINKAGE SECTION.
           COPY LKCIF.

      *================================================================*
      *   SCREEN SECTION - UI estilo BANCSMENU (pdcurses)              *
      *================================================================*
       SCREEN SECTION.
       01  SCR-MARCO.
           05 BLANK SCREEN.
           05 LINE 02 COL 02 VALUE
              "+------------------------------------------+".
           05 LINE 03 COL 02 VALUE
              "|     BAT000 - CIERRE MENSUAL v3.0         |".
           05 LINE 04 COL 02 VALUE
              "+------------------------------------------+".

       01  SCR-RESUMEN.
           05 LINE 06 COL 05 VALUE "Periodo procesado:".
           05 LINE 06 COL 25 PIC X(6) FROM WS-PERIODO.
           05 LINE 07 COL 05 VALUE "Fecha sistema    :".
           05 LINE 07 COL 25 PIC X(10) FROM WS-FECHA-HOY.
           05 LINE 09 COL 05 VALUE
              "-------- CONTADORES DE PROCESO --------".
           05 LINE 10 COL 05 VALUE "Clientes procesados      :".
           05 LINE 10 COL 35 PIC ZZZ,ZZ9 FROM WS-CTR-CLIENTES.
           05 LINE 11 COL 05 VALUE "Tarjetas procesadas      :".
           05 LINE 11 COL 35 PIC ZZZ,ZZ9 FROM WS-CTR-TARJETAS.
           05 LINE 12 COL 05 VALUE "Hipotecas procesadas     :".
           05 LINE 12 COL 35 PIC ZZZ,ZZ9 FROM WS-CTR-HIPOTECAS.
           05 LINE 13 COL 05 VALUE "Hipotecas en MORA        :".
           05 LINE 13 COL 35 PIC ZZZ,ZZ9 FROM WS-CTR-MOROSAS.
           05 LINE 14 COL 05 VALUE "Hipotecas CASTIGADAS     :".
           05 LINE 14 COL 35 PIC ZZZ,ZZ9 FROM WS-CTR-CASTIGADAS.
           05 LINE 15 COL 05 VALUE "Hipotecas regularizadas  :".
           05 LINE 15 COL 35 PIC ZZZ,ZZ9 FROM WS-CTR-REGULARIZADAS.
           05 LINE 16 COL 05 VALUE "Tarjetas dadas de BAJA   :".
           05 LINE 16 COL 35 PIC ZZZ,ZZ9 FROM WS-CTR-TARJ-BAJA.
           05 LINE 17 COL 05 VALUE "Errores detectados       :".
           05 LINE 17 COL 35 PIC ZZZ,ZZ9 FROM WS-CTR-ERRORES.

       01  SCR-MSG-OK.
           05 LINE 19 COL 05 VALUE
              ">>> CIERRE MENSUAL COMPLETADO EXITOSAMENTE".
           05 LINE 20 COL 05 VALUE
              "    COMMIT aplicado en base de datos.".

       01  SCR-MSG-ERR.
           05 LINE 19 COL 05 VALUE
              ">>> CIERRE MENSUAL FALLO".
           05 LINE 20 COL 05 VALUE
              "    ROLLBACK ejecutado - datos revertidos.".

       01  SCR-LOG-INFO.
           05 LINE 22 COL 05 VALUE
              "Detalle completo en:".
           05 LINE 23 COL 05 VALUE
              "  docs/logs/BAT_LOGS_".
           05 LINE 23 COL 26 PIC X(6) FROM WS-PERIODO.
           05 LINE 23 COL 32 VALUE ".txt".

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
           PERFORM 8000-PAUSA-RESULTADO

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

           MOVE 'A' TO HV-ESTADO-ACTIVO
           MOVE 'I' TO HV-ESTADO-INACTIVO



           MOVE '================================================'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE ' BAT000 - PROCESO DE CIERRE MENSUAL'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE SPACES TO WS-LOG-LINE
           STRING ' PERIODO : ' DELIMITED BY SIZE
                  WS-PERIODO    DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE SPACES TO WS-LOG-LINE
           STRING ' FECHA   : ' DELIMITED BY SIZE
                  WS-FECHA-HOY  DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE '================================================'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE.

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
                 SQL-VAR-0024
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
           MOVE SQL-VAR-0024 TO HV-COUNT-PERIODO

           PERFORM 9000-EVALUAR-SQL

           IF LK-COD-RETORNO = 99
               MOVE SQLCODE TO WS-AUX-SQLCODE
               MOVE SPACES TO WS-LOG-LINE
               STRING 'ERROR AL VERIFICAR PERIODO. SQLCODE: '
                          DELIMITED BY SIZE
                      WS-AUX-SQLCODE
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
               MOVE 'S' TO WS-ABORT
           ELSE
               IF HV-COUNT-PERIODO > 0
                   MOVE SPACES TO WS-LOG-LINE
                   STRING 'ADVERTENCIA: PERIODO '
                              DELIMITED BY SIZE
                          WS-PERIODO
                              DELIMITED BY SIZE
                          ' YA FUE PROCESADO.'
                              DELIMITED BY SIZE
                       INTO WS-LOG-LINE
                   PERFORM 9100-LOG-WRITE
                   MOVE 10 TO LK-COD-RETORNO
                   MOVE 'PERIODO YA PROCESADO. CIERRE CANCELADO.'
                       TO LK-MENSAJE
                   MOVE 'S' TO WS-ABORT
               ELSE
                   MOVE 0 TO LK-COD-RETORNO
                   MOVE SPACES TO WS-LOG-LINE
                   STRING 'PERIODO '
                              DELIMITED BY SIZE
                          WS-PERIODO
                              DELIMITED BY SIZE
                          ' HABILITADO OK.'
                              DELIMITED BY SIZE
                       INTO WS-LOG-LINE
                   PERFORM 9100-LOG-WRITE
               END-IF
           END-IF.

      *================================================================*
      *   3000 - SNAPSHOT AUDIT_MAESTRA (CLIENTES + CTACTES)          *
      *================================================================*
       3000-SNAPSHOT-MAESTRA.
           MOVE '>>> INICIANDO SNAPSHOT AUDIT_MAESTRA...'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE

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
               MOVE SQLCODE TO WS-AUX-SQLCODE
               MOVE SPACES TO WS-LOG-LINE
               STRING 'ERROR ABRIENDO CUR-MAESTRA: '
                          DELIMITED BY SIZE
                      WS-AUX-SQLCODE
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
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
      *                :HV-ESTADO-CLI,
      *                :HV-TIENE-TARJETA,
      *                :HV-TIENE-HIPOTECA,
      *                :HV-SALDO-CTA      :HV-IND-SALDO-CTA
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
             HV-ESTADO-CLI
           MOVE 'X' TO SQL-TYPE(8)
           MOVE 1 TO SQL-LEN(8)
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
           MOVE 7 TO SQL-LEN(11)
               MOVE X'02' TO SQL-PREC(11)
           SET SQL-ADDR(12) TO ADDRESS OF
             HV-IND-SALDO-CTA
           MOVE 'i' TO SQL-TYPE(12)
           MOVE 12 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO HV-ID-CLIENTE
           MOVE SQL-VAR-0002 TO HV-SALDO-CLI
           MOVE SQL-VAR-0003 TO HV-TIENE-TARJETA
           MOVE SQL-VAR-0004 TO HV-TIENE-HIPOTECA
           MOVE SQL-VAR-0005 TO HV-SALDO-CTA

               EVALUATE SQLCODE
                   WHEN 0
                       IF HV-IND-SALDO-CTA   < 0
                           MOVE ZERO   TO HV-SALDO-CTA
                       END-IF

      *                EXEC SQL
      *                    INSERT INTO AUDIT_MAESTRA (
      *                        PERIODO,           ID_CLIENTE,
      *                        TIPO_DOC,          DOC_CLIENTE,
      *                        FECHA_ALTA,        NOMBRE_CLIENTE,
      *                        APELLIDOS_CLIENTE,
      *                        SALDO_TOTAL_VISTA, ESTADO_CLIENTE,
      *                        TIENE_TARJETA,     TIENE_HIPOTECA,
      *                        SALDO_CTA
      *                    ) VALUES (
      *                        :WS-PERIODO,       :HV-ID-CLIENTE,
      *                        :HV-TIPO-DOC,      :HV-DOC-CLIENTE,
      *                        :HV-FECHA-ALTA,    :HV-NOMBRE,
      *                        :HV-APELLIDOS,
      *                        :HV-SALDO-CLI,     :HV-ESTADO-CLI,
      *                        :HV-TIENE-TARJETA, :HV-TIENE-HIPOTECA,
      *                        :HV-SALDO-CTA
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
                 HV-ESTADO-CLI
               MOVE 'X' TO SQL-TYPE(9)
               MOVE 1 TO SQL-LEN(9)
               SET SQL-ADDR(10) TO ADDRESS OF
                 SQL-VAR-0003
               MOVE '3' TO SQL-TYPE(10)
               MOVE 1 TO SQL-LEN(10)
               MOVE X'00' TO SQL-PREC(10)
               SET SQL-ADDR(11) TO ADDRESS OF
                 SQL-VAR-0004
               MOVE '3' TO SQL-TYPE(11)
               MOVE 1 TO SQL-LEN(11)
               MOVE X'00' TO SQL-PREC(11)
               SET SQL-ADDR(12) TO ADDRESS OF
                 SQL-VAR-0005
               MOVE '3' TO SQL-TYPE(12)
               MOVE 7 TO SQL-LEN(12)
               MOVE X'02' TO SQL-PREC(12)
               MOVE 12 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-ID-CLIENTE
             TO SQL-VAR-0001
           MOVE HV-SALDO-CLI
             TO SQL-VAR-0002
           MOVE HV-TIENE-TARJETA
             TO SQL-VAR-0003
           MOVE HV-TIENE-HIPOTECA
             TO SQL-VAR-0004
           MOVE HV-SALDO-CTA
             TO SQL-VAR-0005
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA

                       IF SQLCODE = 0
                           ADD 1 TO WS-CTR-CLIENTES
                       ELSE
                           MOVE SQLCODE      TO WS-AUX-SQLCODE
                           MOVE HV-ID-CLIENTE TO WS-AUX-ID8
                           MOVE SPACES TO WS-LOG-LINE
                           STRING 'ERROR INSERT MAESTRA ID: '
                                      DELIMITED BY SIZE
                                  WS-AUX-ID8
                                      DELIMITED BY SIZE
                                  ' SQLCODE: '
                                      DELIMITED BY SIZE
                                  WS-AUX-SQLCODE
                                      DELIMITED BY SIZE
                               INTO WS-LOG-LINE
                           PERFORM 9100-LOG-WRITE
                           ADD 1   TO WS-CTR-ERRORES
                           MOVE 'S' TO WS-ABORT
                           MOVE 'S' TO WS-FIN-MAESTRA
                       END-IF

                   WHEN 100
                       MOVE 'S' TO WS-FIN-MAESTRA

                   WHEN OTHER
                       MOVE SQLCODE TO WS-AUX-SQLCODE
                       MOVE SPACES TO WS-LOG-LINE
                       STRING 'ERROR FETCH CUR-MAESTRA: '
                                  DELIMITED BY SIZE
                              WS-AUX-SQLCODE
                                  DELIMITED BY SIZE
                           INTO WS-LOG-LINE
                       PERFORM 9100-LOG-WRITE
                       ADD 1   TO WS-CTR-ERRORES
                       MOVE 'S' TO WS-ABORT
                       MOVE 'S' TO WS-FIN-MAESTRA
               END-EVALUATE

           END-PERFORM

      *    EXEC SQL CLOSE CUR-MAESTRA END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA

           IF WS-ABORT = 'N'
               MOVE WS-CTR-CLIENTES TO WS-AUX-CTR
               MOVE SPACES TO WS-LOG-LINE
               STRING ' AUDIT_MAESTRA  : '
                          DELIMITED BY SIZE
                      WS-AUX-CTR
                          DELIMITED BY SIZE
                      ' REGISTROS INSERTADOS.'
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
           ELSE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR EN SNAPSHOT MAESTRA - SE HARA ROLLBACK'
                   TO LK-MENSAJE
           END-IF.

      *================================================================*
      *   4000 - SNAPSHOT AUDIT_TARJETAS                              *
      *================================================================*
       4000-SNAPSHOT-TARJETAS.
           MOVE '>>> INICIANDO SNAPSHOT AUDIT_TARJETAS...'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE

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
               MOVE SQLCODE TO WS-AUX-SQLCODE
               MOVE SPACES TO WS-LOG-LINE
               STRING 'ERROR ABRIENDO CUR-TARJETAS: '
                          DELIMITED BY SIZE
                      WS-AUX-SQLCODE
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
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
      *                :HV-TARJ-ESTADO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0006
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             HV-TARJ-NRO
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 16 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0007
           MOVE '3' TO SQL-TYPE(3)
           MOVE 7 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0008
           MOVE '3' TO SQL-TYPE(4)
           MOVE 7 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             HV-TARJ-ESTADO
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 1 TO SQL-LEN(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0006 TO HV-TARJ-ID-CLI
           MOVE SQL-VAR-0007 TO HV-TARJ-LIMITE
           MOVE SQL-VAR-0008 TO HV-TARJ-ACUM

               EVALUATE SQLCODE
                   WHEN 0
      *                EXEC SQL
      *                    INSERT INTO AUDIT_TARJETAS (
      *                        PERIODO,       ID_CLIENTE,
      *                        NRO_TARJETA,   CUPO_APROBADO,
      *                        SALDO_UTILIZADO,
      *                        ESTADO_TARJETA
      *                    ) VALUES (
      *                        :WS-PERIODO,   :HV-TARJ-ID-CLI,
      *                        :HV-TARJ-NRO,  :HV-TARJ-LIMITE,
      *                        :HV-TARJ-ACUM,
      *                        :HV-TARJ-ESTADO
      *                    )
      *                END-EXEC
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-PERIODO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0006
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 HV-TARJ-NRO
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 16 TO SQL-LEN(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0007
               MOVE '3' TO SQL-TYPE(4)
               MOVE 7 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0008
               MOVE '3' TO SQL-TYPE(5)
               MOVE 7 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 HV-TARJ-ESTADO
               MOVE 'X' TO SQL-TYPE(6)
               MOVE 1 TO SQL-LEN(6)
               MOVE 6 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-TARJ-ID-CLI
             TO SQL-VAR-0006
           MOVE HV-TARJ-LIMITE
             TO SQL-VAR-0007
           MOVE HV-TARJ-ACUM
             TO SQL-VAR-0008
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA

                       IF SQLCODE = 0
                           ADD 1 TO WS-CTR-TARJETAS
                       ELSE
                           MOVE SQLCODE    TO WS-AUX-SQLCODE
                           MOVE SPACES TO WS-LOG-LINE
                           STRING 'ERROR INSERT TARJETA NRO: '
                                      DELIMITED BY SIZE
                                  HV-TARJ-NRO
                                      DELIMITED BY SIZE
                                  ' SQLCODE: '
                                      DELIMITED BY SIZE
                                  WS-AUX-SQLCODE
                                      DELIMITED BY SIZE
                               INTO WS-LOG-LINE
                           PERFORM 9100-LOG-WRITE
                           ADD 1   TO WS-CTR-ERRORES
                           MOVE 'S' TO WS-ABORT
                           MOVE 'S' TO WS-FIN-TARJETAS
                       END-IF

                   WHEN 100
                       MOVE 'S' TO WS-FIN-TARJETAS

                   WHEN OTHER
                       MOVE SQLCODE TO WS-AUX-SQLCODE
                       MOVE SPACES TO WS-LOG-LINE
                       STRING 'ERROR FETCH CUR-TARJETAS: '
                                  DELIMITED BY SIZE
                              WS-AUX-SQLCODE
                                  DELIMITED BY SIZE
                           INTO WS-LOG-LINE
                       PERFORM 9100-LOG-WRITE
                       ADD 1   TO WS-CTR-ERRORES
                       MOVE 'S' TO WS-ABORT
                       MOVE 'S' TO WS-FIN-TARJETAS
               END-EVALUATE

           END-PERFORM

      *    EXEC SQL CLOSE CUR-TARJETAS END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-1
                               SQLCA

           IF WS-ABORT = 'N'
               MOVE WS-CTR-TARJETAS TO WS-AUX-CTR
               MOVE SPACES TO WS-LOG-LINE
               STRING ' AUDIT_TARJETAS : '
                          DELIMITED BY SIZE
                      WS-AUX-CTR
                          DELIMITED BY SIZE
                      ' REGISTROS INSERTADOS.'
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
           ELSE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR EN SNAPSHOT TARJETAS - SE HARA ROLLBACK'
                   TO LK-MENSAJE
           END-IF.

      *================================================================*
      *   5000 - SNAPSHOT AUDIT_HIPOTECAS                             *
      *================================================================*
       5000-SNAPSHOT-HIPOTECAS.
           MOVE '>>> INICIANDO SNAPSHOT AUDIT_HIPOTECAS...'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE

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
               MOVE SQLCODE TO WS-AUX-SQLCODE
               MOVE SPACES TO WS-LOG-LINE
               STRING 'ERROR ABRIENDO CUR-HIPOTECAS: '
                          DELIMITED BY SIZE
                      WS-AUX-SQLCODE
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
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
             SQL-VAR-0009
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             SQL-VAR-0010
           MOVE '3' TO SQL-TYPE(2)
           MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0011
           MOVE '3' TO SQL-TYPE(3)
           MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0012
           MOVE '3' TO SQL-TYPE(4)
           MOVE 4 TO SQL-LEN(4)
               MOVE X'04' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0013
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
           MOVE SQL-VAR-0009 TO HV-HIPO-ID-HIPO
           MOVE SQL-VAR-0010 TO HV-HIPO-ID-CLI
           MOVE SQL-VAR-0011 TO HV-HIPO-MONTO-ORIG
           MOVE SQL-VAR-0012 TO HV-HIPO-TASA
           MOVE SQL-VAR-0013 TO HV-HIPO-SALDO

               EVALUATE SQLCODE
                   WHEN 0
      *                EXEC SQL
      *                    INSERT INTO AUDIT_HIPOTECAS (
      *                        PERIODO,             ID_HIPOTECA,
      *                        ID_CLIENTE,          MONTO_PRESTAMO,
      *                        TASA_ANUAL,          SALDO_DEUDA,
      *                        FECHA_VENCIMIENTO,   ESTADO_PRESTAMO
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
                 SQL-VAR-0009
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 SQL-VAR-0010
               MOVE '3' TO SQL-TYPE(3)
               MOVE 5 TO SQL-LEN(3)
               MOVE X'00' TO SQL-PREC(3)
               SET SQL-ADDR(4) TO ADDRESS OF
                 SQL-VAR-0011
               MOVE '3' TO SQL-TYPE(4)
               MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
               SET SQL-ADDR(5) TO ADDRESS OF
                 SQL-VAR-0012
               MOVE '3' TO SQL-TYPE(5)
               MOVE 4 TO SQL-LEN(5)
               MOVE X'04' TO SQL-PREC(5)
               SET SQL-ADDR(6) TO ADDRESS OF
                 SQL-VAR-0013
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
             TO SQL-VAR-0009
           MOVE HV-HIPO-ID-CLI
             TO SQL-VAR-0010
           MOVE HV-HIPO-MONTO-ORIG
             TO SQL-VAR-0011
           MOVE HV-HIPO-TASA
             TO SQL-VAR-0012
           MOVE HV-HIPO-SALDO
             TO SQL-VAR-0013
           CALL 'OCSQLEXE' USING SQL-STMT-7
                               SQLCA

                       IF SQLCODE = 0
                           ADD 1 TO WS-CTR-HIPOTECAS
                       ELSE
                           MOVE SQLCODE       TO WS-AUX-SQLCODE
                           MOVE HV-HIPO-ID-HIPO TO WS-AUX-ID9
                           MOVE SPACES TO WS-LOG-LINE
                           STRING 'ERROR INSERT HIPOTECA ID: '
                                      DELIMITED BY SIZE
                                  WS-AUX-ID9
                                      DELIMITED BY SIZE
                                  ' SQLCODE: '
                                      DELIMITED BY SIZE
                                  WS-AUX-SQLCODE
                                      DELIMITED BY SIZE
                               INTO WS-LOG-LINE
                           PERFORM 9100-LOG-WRITE
                           ADD 1   TO WS-CTR-ERRORES
                           MOVE 'S' TO WS-ABORT
                           MOVE 'S' TO WS-FIN-HIPOTECAS
                       END-IF

                   WHEN 100
                       MOVE 'S' TO WS-FIN-HIPOTECAS

                   WHEN OTHER
                       MOVE SQLCODE TO WS-AUX-SQLCODE
                       MOVE SPACES TO WS-LOG-LINE
                       STRING 'ERROR FETCH CUR-HIPOTECAS: '
                                  DELIMITED BY SIZE
                              WS-AUX-SQLCODE
                                  DELIMITED BY SIZE
                           INTO WS-LOG-LINE
                       PERFORM 9100-LOG-WRITE
                       ADD 1   TO WS-CTR-ERRORES
                       MOVE 'S' TO WS-ABORT
                       MOVE 'S' TO WS-FIN-HIPOTECAS
               END-EVALUATE

           END-PERFORM

      *    EXEC SQL CLOSE CUR-HIPOTECAS END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-2
                               SQLCA

           IF WS-ABORT = 'N'
               MOVE WS-CTR-HIPOTECAS TO WS-AUX-CTR
               MOVE SPACES TO WS-LOG-LINE
               STRING ' AUDIT_HIPOTECAS: '
                          DELIMITED BY SIZE
                      WS-AUX-CTR
                          DELIMITED BY SIZE
                      ' REGISTROS INSERTADOS.'
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
           ELSE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR EN SNAPSHOT HIPOTECAS - SE HARA ROLLBACK'
                   TO LK-MENSAJE
           END-IF.

      *================================================================*
      *   5100 - PROCESAR MORA EN HIPOTECAS                           *
      *================================================================*
       5100-PROCESAR-MORA-HIPOTECAS.
           MOVE '>>> PROCESANDO MORA EN HIPOTECAS...'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE

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
               MOVE SQLCODE TO WS-AUX-SQLCODE
               MOVE SPACES TO WS-LOG-LINE
               STRING 'ERROR ABRIENDO CUR-MORA: '
                          DELIMITED BY SIZE
                      WS-AUX-SQLCODE
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
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
      *                :HV-MORA-PAGO-MENS,
      *                :HV-MORA-SALDO-ESP,
      *                :HV-MORA-MESES-MORA,
      *                :HV-MORA-CUENTA-DEB
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0014
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             SQL-VAR-0015
           MOVE '3' TO SQL-TYPE(2)
           MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             SQL-VAR-0016
           MOVE '3' TO SQL-TYPE(3)
           MOVE 8 TO SQL-LEN(3)
               MOVE X'02' TO SQL-PREC(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0017
           MOVE '3' TO SQL-TYPE(4)
           MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             HV-MORA-ESTADO
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 20 TO SQL-LEN(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             SQL-VAR-0018
           MOVE '3' TO SQL-TYPE(6)
           MOVE 8 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             SQL-VAR-0019
           MOVE '3' TO SQL-TYPE(7)
           MOVE 8 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
           SET SQL-ADDR(8) TO ADDRESS OF
             SQL-VAR-0020
           MOVE '3' TO SQL-TYPE(8)
           MOVE 3 TO SQL-LEN(8)
               MOVE X'00' TO SQL-PREC(8)
           SET SQL-ADDR(9) TO ADDRESS OF
             SQL-VAR-0022
           MOVE '3' TO SQL-TYPE(9)
           MOVE 5 TO SQL-LEN(9)
               MOVE X'00' TO SQL-PREC(9)
           MOVE 9 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0014 TO HV-MORA-ID-HIPO
           MOVE SQL-VAR-0015 TO HV-MORA-ID-CLI
           MOVE SQL-VAR-0016 TO HV-MORA-SALDO-ACT
           MOVE SQL-VAR-0017 TO HV-MORA-MONTO-ORIG
           MOVE SQL-VAR-0018 TO HV-MORA-PAGO-MENS
           MOVE SQL-VAR-0019 TO HV-MORA-SALDO-ESP
           MOVE SQL-VAR-0020 TO HV-MORA-MESES-MORA
           MOVE SQL-VAR-0022 TO HV-MORA-CUENTA-DEB

               EVALUATE SQLCODE
                   WHEN 0
      *                Verificar si el saldo actual es mayor al esperado
                       IF HV-MORA-SALDO-ACT > HV-MORA-SALDO-ESP

      *                    Determinar nuevo estado segun meses en mora
                           IF HV-MORA-MESES-MORA >= 3
                               MOVE 'CASTIGADO' TO HV-MORA-NUEVO-ESTADO
                               ADD 1 TO WS-CTR-CASTIGADAS
                               MOVE HV-MORA-ID-HIPO TO WS-AUX-ID9
                               MOVE HV-MORA-ID-CLI  TO WS-AUX-ID8
                               MOVE SPACES TO WS-LOG-LINE
                               STRING ' HIPOTECA CASTIGADA: '
                                          DELIMITED BY SIZE
                                      WS-AUX-ID9
                                          DELIMITED BY SIZE
                                      ' CLIENTE: '
                                          DELIMITED BY SIZE
                                      WS-AUX-ID8
                                          DELIMITED BY SIZE
                                   INTO WS-LOG-LINE
                               PERFORM 9100-LOG-WRITE
                           ELSE
                               MOVE 'MOROSO' TO HV-MORA-NUEVO-ESTADO
                               ADD 1 TO WS-CTR-MOROSAS
                               MOVE HV-MORA-ID-HIPO TO WS-AUX-ID9
                               MOVE HV-MORA-ID-CLI  TO WS-AUX-ID8
                               MOVE SPACES TO WS-LOG-LINE
                               STRING ' HIPOTECA MOROSA: '
                                          DELIMITED BY SIZE
                                      WS-AUX-ID9
                                          DELIMITED BY SIZE
                                      ' CLIENTE: '
                                          DELIMITED BY SIZE
                                      WS-AUX-ID8
                                          DELIMITED BY SIZE
                                   INTO WS-LOG-LINE
                               PERFORM 9100-LOG-WRITE
                           END-IF

      *                    Actualizar estado e incrementar MESES_MORA
      *                    EXEC SQL
      *                        UPDATE HIPOTECAS
      *                        SET    ESTADO_PRESTAMO =
      *                                   :HV-MORA-NUEVO-ESTADO,
      *                               MESES_MORA = MESES_MORA + 1
      *                        WHERE  ID_HIPOTECA = :HV-MORA-ID-HIPO
      *                    END-EXEC
           IF SQL-PREP OF SQL-STMT-8 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 HV-MORA-NUEVO-ESTADO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 20 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0014
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
             TO SQL-VAR-0014
           CALL 'OCSQLEXE' USING SQL-STMT-8
                               SQLCA

                           IF SQLCODE NOT = 0
                               MOVE SQLCODE         TO WS-AUX-SQLCODE
                               MOVE HV-MORA-ID-HIPO TO WS-AUX-ID9
                               MOVE SPACES TO WS-LOG-LINE
                               STRING 'ERROR UPDATE ESTADO HIPOTECA: '
                                          DELIMITED BY SIZE
                                      WS-AUX-ID9
                                          DELIMITED BY SIZE
                                      ' SQLCODE: '
                                          DELIMITED BY SIZE
                                      WS-AUX-SQLCODE
                                          DELIMITED BY SIZE
                                   INTO WS-LOG-LINE
                               PERFORM 9100-LOG-WRITE
                               ADD 1 TO WS-CTR-ERRORES
                               MOVE 'S' TO WS-ABORT
                               MOVE 'S' TO WS-FIN-MORA
                           END-IF

      *                    Consultar saldo de la cuenta debito de
      *                    la hipoteca. Solo cuentas ACTIVAS: si la
      *                    cuenta fue bloqueada/cerrada, SQLCODE=100
      *                    y caera al ELSE general.
      *                    EXEC SQL
      *                        SELECT SALDO_ACTUAL
      *                        INTO   :HV-MORA-CTA-SALDO
      *                        FROM   CTACTES
      *                        WHERE  ID_CUENTA =
      *                               :HV-MORA-CUENTA-DEB
      *                        AND    ESTADO_CUENTA = 'A'
      *                    END-EXEC
           IF SQL-PREP OF SQL-STMT-9 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0023
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0022
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-9
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-MORA-CUENTA-DEB TO SQL-VAR-0022
           CALL 'OCSQLEXE' USING SQL-STMT-9
                               SQLCA
           MOVE SQL-VAR-0023 TO HV-MORA-CTA-SALDO

                           IF SQLCODE = 0
      *                        Si la cuenta tiene fondos, debitar.
                               IF HV-MORA-CTA-SALDO >=
                                  HV-MORA-PAGO-MENS
      *                            EXEC SQL
      *                                UPDATE CTACTES
      *                                SET    SALDO_ACTUAL =
      *                                       SALDO_ACTUAL -
      *                                       :HV-MORA-PAGO-MENS
      *                                WHERE  ID_CUENTA =
      *                                       :HV-MORA-CUENTA-DEB
      *                            END-EXEC
           IF SQL-PREP OF SQL-STMT-10 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0018
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0022
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
             TO SQL-VAR-0018
           MOVE HV-MORA-CUENTA-DEB
             TO SQL-VAR-0022
           CALL 'OCSQLEXE' USING SQL-STMT-10
                               SQLCA
                                   IF SQLCODE NOT = 0
                                       MOVE SQLCODE        TO
                                            WS-AUX-SQLCODE
                                       MOVE HV-MORA-CUENTA-DEB TO
                                            WS-AUX-ID8
                                       MOVE SPACES TO WS-LOG-LINE
                                       STRING 'ERROR DEBITO CUENTA: '
                                                  DELIMITED BY SIZE
                                              WS-AUX-ID8
                                                  DELIMITED BY SIZE
                                              ' SQLCODE: '
                                                  DELIMITED BY SIZE
                                              WS-AUX-SQLCODE
                                                  DELIMITED BY SIZE
                                           INTO WS-LOG-LINE
                                       PERFORM 9100-LOG-WRITE
                                       ADD 1 TO WS-CTR-ERRORES
                                       MOVE 'S' TO WS-ABORT
                                       MOVE 'S' TO WS-FIN-MORA
                                   ELSE
      *                                Descontar cuota tambien del
      *                                SALDO_DEUDA de la hipoteca.
      *                                EXEC SQL
      *                                    UPDATE HIPOTECAS
      *                                    SET    SALDO_DEUDA =
      *                                           SALDO_DEUDA -
      *                                           :HV-MORA-PAGO-MENS
      *                                    WHERE  ID_HIPOTECA =
      *                                           :HV-MORA-ID-HIPO
      *                                END-EXEC
           IF SQL-PREP OF SQL-STMT-11 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0018
               MOVE '3' TO SQL-TYPE(1)
               MOVE 8 TO SQL-LEN(1)
               MOVE X'02' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 SQL-VAR-0014
               MOVE '3' TO SQL-TYPE(2)
               MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-11
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-MORA-PAGO-MENS
             TO SQL-VAR-0018
           MOVE HV-MORA-ID-HIPO
             TO SQL-VAR-0014
           CALL 'OCSQLEXE' USING SQL-STMT-11
                               SQLCA

                                       MOVE HV-MORA-CUENTA-DEB TO
                                            WS-AUX-ID8
                                       MOVE HV-MORA-PAGO-MENS TO
                                            WS-AUX-MONTO13
                                       MOVE SPACES TO WS-LOG-LINE
                                       STRING ' DEBITO APLICADO'
                                                  DELIMITED BY SIZE
                                              ' CTA: '
                                                  DELIMITED BY SIZE
                                              WS-AUX-ID8
                                                  DELIMITED BY SIZE
                                              ' MONTO: '
                                                  DELIMITED BY SIZE
                                              WS-AUX-MONTO13
                                                  DELIMITED BY SIZE
                                           INTO WS-LOG-LINE
                                       PERFORM 9100-LOG-WRITE
                                   END-IF
                               ELSE
                                   MOVE HV-MORA-CUENTA-DEB TO
                                        WS-AUX-ID8
                                   MOVE HV-MORA-CTA-SALDO  TO
                                        WS-AUX-MONTO13
                                   MOVE SPACES TO WS-LOG-LINE
                                   STRING ' SIN FONDOS CTA: '
                                              DELIMITED BY SIZE
                                          WS-AUX-ID8
                                              DELIMITED BY SIZE
                                          ' SALDO: '
                                              DELIMITED BY SIZE
                                          WS-AUX-MONTO13
                                              DELIMITED BY SIZE
                                       INTO WS-LOG-LINE
                                   PERFORM 9100-LOG-WRITE
                               END-IF
                           ELSE
                               MOVE SQLCODE        TO WS-AUX-SQLCODE
                               MOVE HV-MORA-CUENTA-DEB TO WS-AUX-ID8
                               MOVE SPACES TO WS-LOG-LINE
                               STRING 'CTA NO DISPONIBLE: '
                                          DELIMITED BY SIZE
                                      WS-AUX-ID8
                                          DELIMITED BY SIZE
                                      ' SQLCODE: '
                                          DELIMITED BY SIZE
                                      WS-AUX-SQLCODE
                                          DELIMITED BY SIZE
                                   INTO WS-LOG-LINE
                               PERFORM 9100-LOG-WRITE
                           END-IF

                       ELSE
      *                    Saldo correcto - si estaba MOROSO, regulariza
      *                    Resetea MESES_MORA. FECHA_HORA_ALT se
      *                    actualiza automaticamente en schema v3.
                           IF HV-MORA-ESTADO = 'MOROSO'
      *                        EXEC SQL
      *                            UPDATE HIPOTECAS
      *                            SET    ESTADO_PRESTAMO = 'ACTIVO',
      *                                   MESES_MORA = 0
      *                            WHERE  ID_HIPOTECA =
      *                                   :HV-MORA-ID-HIPO
      *                        END-EXEC
           IF SQL-PREP OF SQL-STMT-12 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0014
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-12
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           MOVE HV-MORA-ID-HIPO
             TO SQL-VAR-0014
           CALL 'OCSQLEXE' USING SQL-STMT-12
                               SQLCA

                               IF SQLCODE = 0
                                   ADD 1 TO WS-CTR-REGULARIZADAS
                                   MOVE HV-MORA-ID-HIPO TO WS-AUX-ID9
                                   MOVE HV-MORA-ID-CLI  TO WS-AUX-ID8
                                   MOVE SPACES TO WS-LOG-LINE
                                   STRING ' HIPOTECA REGULARIZADA: '
                                              DELIMITED BY SIZE
                                          WS-AUX-ID9
                                              DELIMITED BY SIZE
                                          ' CLIENTE: '
                                              DELIMITED BY SIZE
                                          WS-AUX-ID8
                                              DELIMITED BY SIZE
                                       INTO WS-LOG-LINE
                                   PERFORM 9100-LOG-WRITE
                               ELSE
                                   MOVE SQLCODE         TO
                                        WS-AUX-SQLCODE
                                   MOVE HV-MORA-ID-HIPO TO WS-AUX-ID9
                                   MOVE SPACES TO WS-LOG-LINE
                                   STRING 'ERROR REGULARIZAR HIPOTECA: '
                                          DELIMITED BY SIZE
                                          WS-AUX-ID9
                                          DELIMITED BY SIZE
                                          ' SQLCODE: '
                                          DELIMITED BY SIZE
                                          WS-AUX-SQLCODE
                                          DELIMITED BY SIZE
                                       INTO WS-LOG-LINE
                                   PERFORM 9100-LOG-WRITE
                                   ADD 1 TO WS-CTR-ERRORES
                                   MOVE 'S' TO WS-ABORT
                                   MOVE 'S' TO WS-FIN-MORA
                               END-IF
                           END-IF
                       END-IF

                   WHEN 100
                       MOVE 'S' TO WS-FIN-MORA

                   WHEN OTHER
                       MOVE SQLCODE TO WS-AUX-SQLCODE
                       MOVE SPACES TO WS-LOG-LINE
                       STRING 'ERROR FETCH CUR-MORA: '
                                  DELIMITED BY SIZE
                              WS-AUX-SQLCODE
                                  DELIMITED BY SIZE
                           INTO WS-LOG-LINE
                       PERFORM 9100-LOG-WRITE
                       ADD 1   TO WS-CTR-ERRORES
                       MOVE 'S' TO WS-ABORT
                       MOVE 'S' TO WS-FIN-MORA
               END-EVALUATE

           END-PERFORM

      *    EXEC SQL CLOSE CUR-MORA END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-3
                               SQLCA

           IF WS-ABORT = 'N'
               MOVE WS-CTR-MOROSAS TO WS-AUX-CTR
               MOVE SPACES TO WS-LOG-LINE
               STRING ' HIPOTECAS MOROSAS   : '
                          DELIMITED BY SIZE
                      WS-AUX-CTR
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
               MOVE WS-CTR-CASTIGADAS TO WS-AUX-CTR
               MOVE SPACES TO WS-LOG-LINE
               STRING ' HIPOTECAS CASTIGADAS: '
                          DELIMITED BY SIZE
                      WS-AUX-CTR
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
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
           MOVE '>>> BAJA DE TARJETAS VENCIDAS...'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE

      *    EXEC SQL
      *        UPDATE TARJETAS
      *        SET    ESTADO_TARJETA = :HV-ESTADO-INACTIVO
      *        WHERE  FECHA_VENCTO   < CURDATE()
      *        AND    ESTADO_TARJETA = :HV-ESTADO-ACTIVO
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-13 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 HV-ESTADO-INACTIVO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 HV-ESTADO-ACTIVO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 1 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-13
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-13
                               SQLCA

           IF SQLCODE = 0
               MOVE SQLERRD(3)       TO WS-CTR-TARJ-BAJA
               MOVE WS-CTR-TARJ-BAJA TO WS-AUX-CTR
               MOVE SPACES TO WS-LOG-LINE
               STRING ' TARJETAS DADAS DE BAJA: '
                          DELIMITED BY SIZE
                      WS-AUX-CTR
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
           ELSE
               MOVE SQLCODE TO WS-AUX-SQLCODE
               MOVE SPACES TO WS-LOG-LINE
               STRING 'ERROR EN BAJA TARJETAS: '
                          DELIMITED BY SIZE
                      WS-AUX-SQLCODE
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
               MOVE 99 TO LK-COD-RETORNO
               MOVE 'ERROR EN BAJA TARJETAS - SE HARA ROLLBACK'
                   TO LK-MENSAJE
               MOVE 'S' TO WS-ABORT
               ADD 1 TO WS-CTR-ERRORES
           END-IF.

      *================================================================*
      *   6000 - RESET OPERATIVO: CIERRE DE TARJETAS                  *
      *   Schema v3: resetea SALDO_UTILIZADO a 0 en tarjetas ACTIVAS  *
      *   (el saldo del periodo ya quedo en AUDIT_TARJETAS).          *
      *================================================================*
       6000-RESET-OPERATIVO.
           MOVE '>>> RESET OPERATIVO TARJETAS...'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE

      *    EXEC SQL
      *        UPDATE TARJETAS
      *        SET    SALDO_UTILIZADO = 0
      *        WHERE  ESTADO_TARJETA = :HV-ESTADO-ACTIVO
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-14 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 HV-ESTADO-ACTIVO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 1 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-14
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-14
                               SQLCA

           IF SQLCODE = 0
               MOVE SQLERRD(3)        TO WS-AUX-CTR
               MOVE SPACES TO WS-LOG-LINE
               STRING ' RESET TARJETAS OK - SALDO_UTILIZADO=0: '
                          DELIMITED BY SIZE
                      WS-AUX-CTR
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
           ELSE
               MOVE SQLCODE TO WS-AUX-SQLCODE
               MOVE SPACES TO WS-LOG-LINE
               STRING 'ERROR EN RESET TARJETAS. SQLCODE: '
                          DELIMITED BY SIZE
                      WS-AUX-SQLCODE
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
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
           MOVE '================================================'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE SPACES TO WS-LOG-LINE
           STRING ' RESUMEN - CIERRE MENSUAL PERIODO: '
                      DELIMITED BY SIZE
                  WS-PERIODO
                      DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE WS-CTR-CLIENTES TO WS-AUX-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' CLIENTES  PROCESADOS : '
                      DELIMITED BY SIZE
                  WS-AUX-CTR DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE WS-CTR-TARJETAS TO WS-AUX-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' TARJETAS  PROCESADAS : '
                      DELIMITED BY SIZE
                  WS-AUX-CTR DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE WS-CTR-HIPOTECAS TO WS-AUX-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' HIPOTECAS PROCESADAS : '
                      DELIMITED BY SIZE
                  WS-AUX-CTR DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE WS-CTR-MOROSAS TO WS-AUX-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' HIPOTECAS MOROSAS    : '
                      DELIMITED BY SIZE
                  WS-AUX-CTR DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE WS-CTR-CASTIGADAS TO WS-AUX-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' HIPOTECAS CASTIGADAS : '
                      DELIMITED BY SIZE
                  WS-AUX-CTR DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE WS-CTR-REGULARIZADAS TO WS-AUX-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' HIPOTECAS REGULARIZADAS: '
                      DELIMITED BY SIZE
                  WS-AUX-CTR DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE WS-CTR-TARJ-BAJA TO WS-AUX-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' TARJETAS DADAS BAJA  : '
                      DELIMITED BY SIZE
                  WS-AUX-CTR DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE WS-CTR-ERRORES TO WS-AUX-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' ERRORES   DETECTADOS : '
                      DELIMITED BY SIZE
                  WS-AUX-CTR DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE '================================================'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE

           IF WS-ABORT = 'N' AND WS-CTR-ERRORES = 0
      *        EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL
               MOVE 0  TO LK-COD-RETORNO
               MOVE 'CIERRE MENSUAL COMPLETADO EXITOSAMENTE'
                   TO LK-MENSAJE
               MOVE 'COMMIT REALIZADO.' TO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
           ELSE
      *        EXEC SQL ROLLBACK END-EXEC
           CALL 'OCSQLRBK' USING SQLCA END-CALL
               IF LK-COD-RETORNO = 0
                   MOVE 50 TO LK-COD-RETORNO
               END-IF
               MOVE 'CIERRE FALLIDO - DATOS REVERTIDOS'
                   TO LK-MENSAJE
               MOVE 'ROLLBACK EJECUTADO - DATOS REVERTIDOS.'
                   TO WS-LOG-LINE
               PERFORM 9100-LOG-WRITE
           END-IF

           MOVE SPACES TO WS-LOG-LINE
           STRING ' RESULTADO: '
                      DELIMITED BY SIZE
                  LK-MENSAJE
                      DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE
           MOVE '================================================'
               TO WS-LOG-LINE
           PERFORM 9100-LOG-WRITE.


      *================================================================*
      *   8000 - PAUSA FINAL: MUESTRA RESULTADO Y ESPERA TECLA       *
      *================================================================*
       8000-PAUSA-RESULTADO.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-RESUMEN.
           IF WS-ABORT = 'N' AND WS-CTR-ERRORES = 0
               DISPLAY SCR-MSG-OK
           ELSE
               DISPLAY SCR-MSG-ERR
           END-IF.
           DISPLAY SCR-LOG-INFO.
           DISPLAY "Presione ENTER para regresar al menu."
              LINE 24 COL 05.
           ACCEPT WS-TECLA LINE 24 COL 41.

      *================================================================*
      *   9900 - LIMPIAR PANTALLA (Windows: cls)                       *
      *================================================================*
       9900-LIMPIAR-PANTALLA.
           CALL "SYSTEM" USING WS-CLS-CMD.

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

      *================================================================*
      *   9100 - ESCRIBIR LINEA AL ARCHIVO DE LOG                     *
      *           Destino: ..\docs\logs\BAT_LOGS_<PERIODO>.txt         *
      *================================================================*
       9100-LOG-WRITE.
           CALL 'LOGFILE' USING WS-MODULO-LOG,
                                WS-PERIODO,
                                WS-LOG-LINE.

       END PROGRAM BAT000.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  CUR-HIPOTECAS            IN USE CURSOR
      *  CUR-MAESTRA              IN USE CURSOR
      *  CUR-MORA                 IN USE CURSOR
      *  CUR-TARJETAS             IN USE CURSOR
      *  HV-APELLIDOS             IN USE CHAR(25)
      *  HV-COUNT-PERIODO         IN USE THROUGH TEMP VAR SQL-VAR-0024 DECIMAL(7,0)
      *  HV-DOC-CLIENTE           IN USE CHAR(12)
      *  HV-ESTADO-ACTIVO         IN USE CHAR(1)
      *  HV-ESTADO-CLI            IN USE CHAR(1)
      *  HV-ESTADO-INACTIVO       IN USE CHAR(1)
      *  HV-FECHA-ALTA            IN USE CHAR(10)
      *  HV-HIPO-ESTADO           IN USE CHAR(20)
      *  HV-HIPO-FECHA-VENC       IN USE CHAR(10)
      *  HV-HIPO-ID-CLI           IN USE THROUGH TEMP VAR SQL-VAR-0010 DECIMAL(9,0)
      *  HV-HIPO-ID-HIPO          IN USE THROUGH TEMP VAR SQL-VAR-0009 DECIMAL(9,0)
      *  HV-HIPO-MONTO-ORIG       IN USE THROUGH TEMP VAR SQL-VAR-0011 DECIMAL(15,2)
      *  HV-HIPO-SALDO            IN USE THROUGH TEMP VAR SQL-VAR-0013 DECIMAL(15,2)
      *  HV-HIPO-TASA             IN USE THROUGH TEMP VAR SQL-VAR-0012 DECIMAL(7,4)
      *  HV-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  HV-IND-SALDO-CTA         IN USE INTEGER(2 BYTES)
      *  HV-MORA-CLI-SALDO    NOT IN USE
      *  HV-MORA-CTA-SALDO        IN USE THROUGH TEMP VAR SQL-VAR-0023 DECIMAL(15,2)
      *  HV-MORA-CUENTA-DEB       IN USE THROUGH TEMP VAR SQL-VAR-0022 DECIMAL(9,0)
      *  HV-MORA-ESTADO           IN USE CHAR(20)
      *  HV-MORA-ID-CLI           IN USE THROUGH TEMP VAR SQL-VAR-0015 DECIMAL(9,0)
      *  HV-MORA-ID-HIPO          IN USE THROUGH TEMP VAR SQL-VAR-0014 DECIMAL(9,0)
      *  HV-MORA-MESES-MORA       IN USE THROUGH TEMP VAR SQL-VAR-0020 DECIMAL(5,0)
      *  HV-MORA-MONTO-ORIG       IN USE THROUGH TEMP VAR SQL-VAR-0017 DECIMAL(15,2)
      *  HV-MORA-NUEVO-ESTADO     IN USE CHAR(20)
      *  HV-MORA-PAGO-MENS        IN USE THROUGH TEMP VAR SQL-VAR-0018 DECIMAL(15,2)
      *  HV-MORA-SALDO-ACT        IN USE THROUGH TEMP VAR SQL-VAR-0016 DECIMAL(15,2)
      *  HV-MORA-SALDO-ESP        IN USE THROUGH TEMP VAR SQL-VAR-0019 DECIMAL(15,2)
      *  HV-NOMBRE                IN USE CHAR(25)
      *  HV-SALDO-CLI             IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(13,2)
      *  HV-SALDO-CTA             IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(13,2)
      *  HV-TARJ-ACUM             IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(13,2)
      *  HV-TARJ-ESTADO           IN USE CHAR(1)
      *  HV-TARJ-ID-CLI           IN USE THROUGH TEMP VAR SQL-VAR-0006 DECIMAL(9,0)
      *  HV-TARJ-LIMITE           IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(13,2)
      *  HV-TARJ-NRO              IN USE CHAR(16)
      *  HV-TIENE-HIPOTECA        IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(1,0)
      *  HV-TIENE-TARJETA         IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(1,0)
      *  HV-TIPO-DOC              IN USE CHAR(3)
      *  WS-HOST-ESTADOS      NOT IN USE
      *  WS-HOST-ESTADOS.HV-ESTADO-ACTIVO NOT IN USE
      *  WS-HOST-ESTADOS.HV-ESTADO-INACTIVO NOT IN USE
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
      *  WS-HOST-MAESTRA.HV-DOC-CLIENTE NOT IN USE
      *  WS-HOST-MAESTRA.HV-ESTADO-CLI NOT IN USE
      *  WS-HOST-MAESTRA.HV-FECHA-ALTA NOT IN USE
      *  WS-HOST-MAESTRA.HV-ID-CLIENTE NOT IN USE
      *  WS-HOST-MAESTRA.HV-IND-SALDO-CTA NOT IN USE
      *  WS-HOST-MAESTRA.HV-NOMBRE NOT IN USE
      *  WS-HOST-MAESTRA.HV-SALDO-CLI NOT IN USE
      *  WS-HOST-MAESTRA.HV-SALDO-CTA NOT IN USE
      *  WS-HOST-MAESTRA.HV-TIENE-HIPOTECA NOT IN USE
      *  WS-HOST-MAESTRA.HV-TIENE-TARJETA NOT IN USE
      *  WS-HOST-MAESTRA.HV-TIPO-DOC NOT IN USE
      *  WS-HOST-MORA         NOT IN USE
      *  WS-HOST-MORA.HV-MORA-CLI-SALDO NOT IN USE
      *  WS-HOST-MORA.HV-MORA-CTA-SALDO NOT IN USE
      *  WS-HOST-MORA.HV-MORA-CUENTA-DEB NOT IN USE
      *  WS-HOST-MORA.HV-MORA-ESTADO NOT IN USE
      *  WS-HOST-MORA.HV-MORA-ID-CLI NOT IN USE
      *  WS-HOST-MORA.HV-MORA-ID-HIPO NOT IN USE
      *  WS-HOST-MORA.HV-MORA-MESES-MORA NOT IN USE
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
      *  WS-HOST-TARJETAS.HV-TARJ-NRO NOT IN USE
      *  WS-PERIODO               IN USE CHAR(6)
      **********************************************************************

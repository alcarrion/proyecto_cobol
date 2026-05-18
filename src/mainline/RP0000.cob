      *================================================================*
      * PROGRAMA : RP0000.sqb (precompilado a RP0000.cob)             *
      * FUNCION  : MODULO DE REPORTES GERENCIALES                     *
      * LLAMADO  : BANCSMENU (OPCION 6)                               *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RP0000.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FS-CLIENTES  ASSIGN TO WS-NOM-CLIENTES
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-STATUS.
           SELECT FS-HIPOTECAS ASSIGN TO WS-NOM-HIPOTECAS
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-STATUS.
           SELECT FS-TARJETAS  ASSIGN TO WS-NOM-TARJETAS
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-STATUS.
           SELECT FS-CUENTAS   ASSIGN TO WS-NOM-CUENTAS
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-STATUS.
           SELECT FS-GENERAL   ASSIGN TO WS-NOM-GENERAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  FS-CLIENTES.
       01  FS-REG-CLIENTES         PIC X(132).
       FD  FS-HIPOTECAS.
       01  FS-REG-HIPOTECAS        PIC X(132).
       FD  FS-TARJETAS.
       01  FS-REG-TARJETAS         PIC X(132).
       FD  FS-CUENTAS.
       01  FS-REG-CUENTAS          PIC X(132).
       FD  FS-GENERAL.
       01  FS-REG-GENERAL          PIC X(132).

       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 10.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 10 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 10 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 10 TIMES.
           05 SQL-PREC   PIC X OCCURS 10 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 154.
           05 SQL-STMT   PIC X(154) VALUE 'SELECT ID_CLIENTE,NOMBRE_CLIE
      -    'NTE,APELLIDOS_CLIENTE,FECHA_ALTA,SALDO_CLIENTE FROM AUDIT_MA
      -    'ESTRA WHERE PERIODO = ? AND FECHA_ALTA LIKE ? ORDER BY ID_CL
      -    'IENTE'.
           05 SQL-CNAME  PIC X(12) VALUE 'CUR-CLIENTES'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 269.
           05 SQL-STMT   PIC X(269) VALUE 'SELECT H.ID_HIPOTECA,H.ID_CLI
      -    'ENTE,M.NOMBRE_CLIENTE,H.MONTO_ORIGINAL,H.SALDO_ACTUAL,H.TASA
      -    '_INTERES,H.ESTADO FROM AUDIT_HIPOTECAS H INNER JOIN AUDIT_MA
      -    'ESTRA M ON H.ID_CLIENTE = M.ID_CLIENTE AND H.PERIODO = M.PER
      -    'IODO WHERE H.PERIODO = ? ORDER BY H.ID_CLIENTE,H.ID_HIPOTECA
      -    ''.
           05 SQL-CNAME  PIC X(13) VALUE 'CUR-HIPOTECAS'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 267.
           05 SQL-STMT   PIC X(267) VALUE 'SELECT T.ID_CLIENTE,M.NOMBRE_
      -    'CLIENTE,T.NRO_TARJETA,T.LIMITE_TARJETA,T.ACUM_MES,T.LIQUIDAC
      -    'ION_MES,T.ESTADO FROM AUDIT_TARJETAS T INNER JOIN AUDIT_MAES
      -    'TRA M ON T.ID_CLIENTE = M.ID_CLIENTE AND T.PERIODO = M.PERIO
      -    'DO WHERE T.PERIODO = ? ORDER BY T.ID_CLIENTE,T.NRO_TARJETA'.
           05 SQL-CNAME  PIC X(12) VALUE 'CUR-TARJETAS'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 160.
           05 SQL-STMT   PIC X(160) VALUE 'SELECT ID_CLIENTE,NOMBRE_CLIE
      -    'NTE,APELLIDOS_CLIENTE,SALDO_CTA,COD_ULT_MOV,IMPORTE_MOV FROM
      -    ' AUDIT_MAESTRA WHERE PERIODO = ? AND CTA_ACTIVA = 1 ORDER BY
      -    ' ID_CLIENTE'.
           05 SQL-CNAME  PIC X(11) VALUE 'CUR-CUENTAS'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-4.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE 'C'.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 340.
           05 SQL-STMT   PIC X(340) VALUE 'SELECT M.ID_CLIENTE,M.NOMBRE_
      -    'CLIENTE,M.APELLIDOS_CLIENTE,M.SALDO_CTA,T.NRO_TARJETA,T.ACUM
      -    '_MES,H.SALDO_ACTUAL FROM AUDIT_MAESTRA M LEFT JOIN AUDIT_TAR
      -    'JETAS T ON M.ID_CLIENTE = T.ID_CLIENTE AND M.PERIODO = T.PER
      -    'IODO LEFT JOIN AUDIT_HIPOTECAS H ON M.ID_CLIENTE = H.ID_CLIE
      -    'NTE AND M.PERIODO = H.PERIODO WHERE M.PERIODO = ? ORDER BY M
      -    '.ID_CLIENTE'.
           05 SQL-CNAME  PIC X(11) VALUE 'CUR-GENERAL'.
           05 FILLER     PIC X VALUE LOW-VALUE.
      **********************************************************************
       01 SQL-STMT-5.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 38.
           05 SQL-STMT   PIC X(38) VALUE 'SELECT MAX(PERIODO) FROM AUDIT
      -    '_MAESTRA'.
      **********************************************************************
       01 SQL-STMT-6.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 1.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 52.
           05 SQL-STMT   PIC X(52) VALUE 'SELECT COUNT(*) FROM AUDIT_MAE
      -    'STRA WHERE PERIODO = ?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0001  PIC S9(9) COMP-3.
           05 SQL-VAR-0002  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0003  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0004  PIC S9(3) COMP-3.
           05 SQL-VAR-0005  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0007  PIC S9(9) COMP-3.
           05 SQL-VAR-0008  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0009  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0010  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0011  PIC S9(9) COMP-3.
           05 SQL-VAR-0012  PIC S9(9) COMP-3.
           05 SQL-VAR-0013  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0014  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0015  PIC S9(3)V9(4) COMP-3.
           05 SQL-VAR-0016  PIC S9(9) COMP-3.
           05 SQL-VAR-0017  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0018  PIC S9(11)V9(2) COMP-3.
           05 SQL-VAR-0019  PIC S9(13)V9(2) COMP-3.
           05 SQL-VAR-0020  PIC S9(9) COMP-3.
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
      *   VARIABLES HOST SQL                                           *
      *================================================================*
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  WS-PERIODO              PIC X(6).
       01  WS-PERIODO-DEF          PIC X(6).
       01  WS-ANIO-MES-LIKE        PIC X(9).

       01  WS-HOST-MAESTRA.
           05 HV-ID-CLIENTE        PIC 9(8).
           05 HV-NOMBRE            PIC X(25).
           05 HV-APELLIDOS         PIC X(25).
           05 HV-FECHA-ALTA        PIC X(10).
           05 HV-SALDO-CLI         PIC S9(10)V99.
           05 HV-SALDO-CTA         PIC S9(10)V99.
           05 HV-COD-ULT-MOV       PIC 9(2).
           05 HV-IMPORTE-MOV       PIC S9(10)V99.
           05 HV-CTA-ACTIVA        PIC 9(1).

       01  WS-HOST-TARJETAS.
           05 HV-TARJ-ID-CLI       PIC 9(8).
           05 HV-TARJ-NOMBRE       PIC X(25).
           05 HV-TARJ-NRO          PIC X(16).
           05 HV-TARJ-LIMITE       PIC S9(10)V99.
           05 HV-TARJ-ACUM         PIC S9(10)V99.
           05 HV-TARJ-LIQUID       PIC S9(10)V99.
           05 HV-TARJ-ESTADO       PIC X(1).

       01  WS-HOST-HIPOTECAS.
           05 HV-HIPO-ID-HIPO      PIC 9(9).
           05 HV-HIPO-ID-CLI       PIC 9(8).
           05 HV-HIPO-NOMBRE       PIC X(25).
           05 HV-HIPO-MONTO-ORIG   PIC S9(13)V99.
           05 HV-HIPO-SALDO        PIC S9(13)V99.
           05 HV-HIPO-TASA         PIC S9(3)V9999.
           05 HV-HIPO-ESTADO       PIC X(20).

       01  WS-HOST-GENERAL.
           05 HV-GEN-ID-CLI        PIC 9(8).
           05 HV-GEN-NOMBRE        PIC X(25).
           05 HV-GEN-APELLIDOS     PIC X(25).
           05 HV-GEN-SALDO-CTA     PIC S9(10)V99.
           05 HV-GEN-NRO-TARJ      PIC X(16).
           05 HV-GEN-ACUM-MES      PIC S9(10)V99.
           05 HV-GEN-SALDO-HIPO    PIC S9(13)V99.
           05 HV-IND-NRO-TARJ      PIC S9(4) COMP-5.
           05 HV-IND-ACUM-MES      PIC S9(4) COMP-5.
           05 HV-IND-SALDO-HIPO    PIC S9(4) COMP-5.

       01  HV-IND-COUNT            PIC 9(9).

      *    EXEC SQL END DECLARE SECTION END-EXEC.

      *================================================================*
      *   CURSORES                                                     *
      *================================================================*

      *    EXEC SQL DECLARE CUR-CLIENTES CURSOR FOR
      *        SELECT ID_CLIENTE,
      *               NOMBRE_CLIENTE,
      *               APELLIDOS_CLIENTE,
      *               FECHA_ALTA,
      *               SALDO_CLIENTE
      *        FROM   AUDIT_MAESTRA
      *        WHERE  PERIODO    = :WS-PERIODO
      *        AND    FECHA_ALTA LIKE :WS-ANIO-MES-LIKE
      *        ORDER BY ID_CLIENTE
      *    END-EXEC.

      *    EXEC SQL DECLARE CUR-HIPOTECAS CURSOR FOR
      *        SELECT H.ID_HIPOTECA,
      *               H.ID_CLIENTE,
      *               M.NOMBRE_CLIENTE,
      *               H.MONTO_ORIGINAL,
      *               H.SALDO_ACTUAL,
      *               H.TASA_INTERES,
      *               H.ESTADO
      *        FROM   AUDIT_HIPOTECAS H
      *        INNER JOIN AUDIT_MAESTRA M
      *               ON  H.ID_CLIENTE = M.ID_CLIENTE
      *               AND H.PERIODO    = M.PERIODO
      *        WHERE  H.PERIODO = :WS-PERIODO
      *        ORDER BY H.ID_CLIENTE, H.ID_HIPOTECA
      *    END-EXEC.

      *    EXEC SQL DECLARE CUR-TARJETAS CURSOR FOR
      *        SELECT T.ID_CLIENTE,
      *               M.NOMBRE_CLIENTE,
      *               T.NRO_TARJETA,
      *               T.LIMITE_TARJETA,
      *               T.ACUM_MES,
      *               T.LIQUIDACION_MES,
      *               T.ESTADO
      *        FROM   AUDIT_TARJETAS T
      *        INNER JOIN AUDIT_MAESTRA M
      *               ON  T.ID_CLIENTE = M.ID_CLIENTE
      *               AND T.PERIODO    = M.PERIODO
      *        WHERE  T.PERIODO = :WS-PERIODO
      *        ORDER BY T.ID_CLIENTE, T.NRO_TARJETA
      *    END-EXEC.

      *    EXEC SQL DECLARE CUR-CUENTAS CURSOR FOR
      *        SELECT ID_CLIENTE,
      *               NOMBRE_CLIENTE,
      *               APELLIDOS_CLIENTE,
      *               SALDO_CTA,
      *               COD_ULT_MOV,
      *               IMPORTE_MOV
      *        FROM   AUDIT_MAESTRA
      *        WHERE  PERIODO    = :WS-PERIODO
      *        AND    CTA_ACTIVA = 1
      *        ORDER BY ID_CLIENTE
      *    END-EXEC.

      *    EXEC SQL DECLARE CUR-GENERAL CURSOR FOR
      *        SELECT M.ID_CLIENTE,
      *               M.NOMBRE_CLIENTE,
      *               M.APELLIDOS_CLIENTE,
      *               M.SALDO_CTA,
      *               T.NRO_TARJETA,
      *               T.ACUM_MES,
      *               H.SALDO_ACTUAL
      *        FROM   AUDIT_MAESTRA M
      *        LEFT JOIN AUDIT_TARJETAS T
      *               ON  M.ID_CLIENTE = T.ID_CLIENTE
      *               AND M.PERIODO    = T.PERIODO
      *        LEFT JOIN AUDIT_HIPOTECAS H
      *               ON  M.ID_CLIENTE = H.ID_CLIENTE
      *               AND M.PERIODO    = H.PERIODO
      *        WHERE  M.PERIODO = :WS-PERIODO
      *        ORDER BY M.ID_CLIENTE
      *    END-EXEC.

      *================================================================*
      *   CONTROL INTERNO                                              *
      *================================================================*
       01  WS-FECHA-SISTEMA.
           05 WS-ANIO              PIC X(4).
           05 WS-MES               PIC X(2).
           05 WS-DIA               PIC X(2).
           05 FILLER               PIC X(13).

       01  WS-CONTROL.
           05 WS-OPCION            PIC 9(1).
           05 WS-CONTINUAR         PIC X VALUE 'S'.
           05 WS-FIN-CURSOR        PIC X VALUE 'N'.
           05 WS-ABORT             PIC X VALUE 'N'.

       01  WS-ULTIMO-ID-CLI        PIC 9(8) VALUE 0.

       01  WS-NOM-CLIENTES         PIC X(80).
       01  WS-NOM-HIPOTECAS        PIC X(80).
       01  WS-NOM-TARJETAS         PIC X(80).
       01  WS-NOM-CUENTAS          PIC X(80).
       01  WS-NOM-GENERAL          PIC X(80).
       01  WS-PAUSA                PIC X(1).

      *================================================================*
      *   CONTADORES                                                   *
      *================================================================*
       01  WS-CONTADORES.
           05 WS-CTR-CLIENTES-NVO  PIC 9(6) VALUE 0.
           05 WS-CTR-HIPOTECAS     PIC 9(6) VALUE 0.
           05 WS-SUM-SALDO-HIPO    PIC S9(13)V99 VALUE 0.
           05 WS-CTR-TARJETAS      PIC 9(6) VALUE 0.
           05 WS-SUM-ACUM-TARJ     PIC S9(10)V99 VALUE 0.
           05 WS-CTR-CUENTAS       PIC 9(6) VALUE 0.
           05 WS-SUM-SALDO-CTA     PIC S9(10)V99 VALUE 0.
           05 WS-CTR-GEN-CLIENTES  PIC 9(6) VALUE 0.
           05 WS-CTR-GEN-TARJETAS  PIC 9(6) VALUE 0.
           05 WS-CTR-GEN-HIPOTECAS PIC 9(6) VALUE 0.

      *================================================================*
      *   WORKING STORAGE - LOG A ARCHIVO                              *
      *================================================================*
       01  WS-LOG-LINE             PIC X(200).
       01  WS-MODULO-LOG           PIC X(03) VALUE 'RPT'.
       01  WS-CLS-CMD              PIC X(04) VALUE 'cls'.
       01  WS-AUX-LOG-CTR          PIC 9(6).
       01  WS-FS-STATUS            PIC X(02) VALUE '00'.
       01  WS-FS-OK                PIC X    VALUE 'S'.

      *================================================================*
      *   FORMATO DE REPORTE                                           *
      *================================================================*
       01  RP-FORMAT.
           05  RP-TITULO.
               10  FILLER          PIC X(5)  VALUE SPACES.
               10  RP-TIT-TEXT     PIC X(60).
               10  FILLER          PIC X(67) VALUE SPACES.
           05  RP-HEADER           PIC X(132).
           05  RP-SEPAR            PIC X(132) VALUE ALL '-'.
           05  RP-BLANK            PIC X(132) VALUE SPACES.
           05  RP-INDIC            PIC X(132).

       01  WS-DET-CLIENTES.
           05 WS-DC-ID             PIC 9(8).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DC-NOMBRE         PIC X(25).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DC-APELLIDO       PIC X(25).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DC-FECHA-ALTA     PIC X(10).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DC-SALDO          PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X(51) VALUE SPACES.

       01  WS-DET-HIPOTECAS.
           05 WS-DH-ID-HIPO        PIC 9(9).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DH-ID-CLI         PIC 9(8).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DH-NOMBRE         PIC X(25).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DH-MONTO-ORIG     PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DH-SALDO          PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DH-TASA           PIC ZZ9.9999.
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DH-ESTADO         PIC X(20).
           05 FILLER               PIC X(10) VALUE SPACES.

       01  WS-DET-TARJETAS.
           05 WS-DT-ID-CLI         PIC 9(8).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DT-NOMBRE         PIC X(25).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DT-NRO            PIC X(16).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DT-LIMITE         PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DT-ACUM           PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DT-LIQUID         PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DT-ESTADO         PIC X(1).
           05 FILLER               PIC X(17) VALUE SPACES.

       01  WS-DET-CUENTAS.
           05 WS-DCU-ID            PIC 9(8).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DCU-NOMBRE        PIC X(25).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DCU-APELLIDO      PIC X(25).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DCU-SALDO         PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DCU-COD-MOV       PIC 99.
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DCU-IMPORTE       PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X(22) VALUE SPACES.

       01  WS-DET-GENERAL.
           05 WS-DG-ID-CLI         PIC 9(8).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DG-NOMBRE         PIC X(25).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DG-APELLIDOS      PIC X(25).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DG-SALDO-CTA      PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DG-NRO-TARJ       PIC X(16).
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DG-ACUM-MES       PIC ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X VALUE SPACE.
           05 WS-DG-SALDO-HIPO     PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER               PIC X(5) VALUE SPACES.

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
           MOVE 0      TO LK-COD-RETORNO
           MOVE SPACES TO LK-MENSAJE
           PERFORM 1000-INICIALIZAR
           IF WS-ABORT = 'N'
               PERFORM 2000-VALIDAR-PERIODO
           END-IF
           IF WS-ABORT = 'N'
               PERFORM 3000-MENU-REPORTES
           END-IF
           EXIT PROGRAM.

      *================================================================*
      *   1000 - INICIALIZAR                                           *
      *================================================================*
       1000-INICIALIZAR.
           PERFORM 9900-LIMPIAR-PANTALLA
           MOVE FUNCTION CURRENT-DATE TO WS-FECHA-SISTEMA
      *    EXEC SQL
      *        SELECT MAX(PERIODO)
      *        INTO   :WS-PERIODO-DEF
      *        FROM   AUDIT_MAESTRA
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-5 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-PERIODO-DEF
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-5
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-5
                               SQLCA
           IF SQLCODE NOT = 0 OR WS-PERIODO-DEF = SPACES
               MOVE '000000' TO WS-PERIODO-DEF
           END-IF
           DISPLAY ' ULTIMO PERIODO: ' WS-PERIODO-DEF
           DISPLAY ' INGRESE PERIODO (YYYYMM) O ENTER PARA DEFAULT:'
           ACCEPT WS-PERIODO
           IF WS-PERIODO = SPACES
               MOVE WS-PERIODO-DEF TO WS-PERIODO
           END-IF
           DISPLAY ' PERIODO: ' WS-PERIODO
           PERFORM 1100-ARMAR-FILTRO-LIKE.

       1100-ARMAR-FILTRO-LIKE.
           MOVE SPACES TO WS-ANIO-MES-LIKE
           STRING WS-PERIODO(1:4) DELIMITED SIZE
                  '-'             DELIMITED SIZE
                  WS-PERIODO(5:2) DELIMITED SIZE
                  '-%'            DELIMITED SIZE
                  INTO WS-ANIO-MES-LIKE
           END-STRING
           DISPLAY ' FILTRO LIKE: [' WS-ANIO-MES-LIKE ']'.

      *================================================================*
      *   2000 - VALIDAR PERIODO                                       *
      *================================================================*
       2000-VALIDAR-PERIODO.
           MOVE 0 TO HV-IND-COUNT
      *    EXEC SQL
      *        SELECT COUNT(*)
      *        INTO   :HV-IND-COUNT
      *        FROM   AUDIT_MAESTRA
      *        WHERE  PERIODO = :WS-PERIODO
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-6 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0020
               MOVE '3' TO SQL-TYPE(1)
               MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-PERIODO
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 6 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-6
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-6
                               SQLCA
           MOVE SQL-VAR-0020 TO HV-IND-COUNT
           PERFORM 9000-EVALUAR-SQL
           IF LK-COD-RETORNO = 99
               MOVE 'ERROR AL VERIFICAR PERIODO' TO LK-MENSAJE
               MOVE 'S' TO WS-ABORT
               MOVE 'ERROR SQL AL VERIFICAR PERIODO'
                   TO WS-LOG-LINE
               PERFORM 9200-LOG-WRITE
           ELSE
               IF HV-IND-COUNT = 0
                   DISPLAY 'PERIODO ' WS-PERIODO
                           ' NO EXISTE EN AUDITORIA.'
                   DISPLAY 'EJECUTE PRIMERO EL CIERRE MENSUAL.'
                   MOVE 11 TO LK-COD-RETORNO
                   MOVE 'PERIODO NO ENCONTRADO EN AUDITORIA'
                       TO LK-MENSAJE
                   MOVE 'S' TO WS-ABORT
                   MOVE SPACES TO WS-LOG-LINE
                   STRING 'PERIODO NO ENCONTRADO: '
                              DELIMITED BY SIZE
                          WS-PERIODO
                              DELIMITED BY SIZE
                       INTO WS-LOG-LINE
                   PERFORM 9200-LOG-WRITE
               ELSE
                   MOVE SPACES TO WS-LOG-LINE
                   STRING '>>> INGRESO A REPORTES - PERIODO: '
                              DELIMITED BY SIZE
                          WS-PERIODO
                              DELIMITED BY SIZE
                       INTO WS-LOG-LINE
                   PERFORM 9200-LOG-WRITE
               END-IF
           END-IF.

      *================================================================*
      *   3000 - MENU DE REPORTES                                      *
      *================================================================*
       3000-MENU-REPORTES.
           PERFORM UNTIL WS-CONTINUAR = 'N'
               PERFORM 9900-LIMPIAR-PANTALLA
               DISPLAY '================================'
               DISPLAY ' RP0000 - REPORTES GERENCIALES'
               DISPLAY ' PERIODO: ' WS-PERIODO
               DISPLAY '================================'
               DISPLAY '1. Reporte Clientes Nuevos'
               DISPLAY '2. Reporte Hipotecas'
               DISPLAY '3. Reporte Tarjetas'
               DISPLAY '4. Reporte Cuentas Corrientes'
               DISPLAY '5. Reporte General'
               DISPLAY '0. Volver al Menu Principal'
               DISPLAY '================================'
               ACCEPT WS-OPCION
               EVALUATE WS-OPCION
                   WHEN 1  PERFORM 4000-RPT-CLIENTES
                   WHEN 2  PERFORM 5000-RPT-HIPOTECAS
                   WHEN 3  PERFORM 6000-RPT-TARJETAS
                   WHEN 4  PERFORM 7000-RPT-CUENTAS
                   WHEN 5  PERFORM 8000-RPT-GENERAL
                   WHEN 0  MOVE 'N' TO WS-CONTINUAR
                   WHEN OTHER DISPLAY 'OPCION INVALIDA.'
               END-EVALUATE
           END-PERFORM.

      *================================================================*
      *   9100 - ABRIR CURSOR GENERICO (verifica SQLCODE)             *
      *================================================================*
       9100-VERIFICAR-OPEN.
           IF SQLCODE NOT = 0
               MOVE 'S' TO WS-FIN-CURSOR
           ELSE
               MOVE 'N' TO WS-FIN-CURSOR
           END-IF.

      *================================================================*
      *   4000 - REPORTE CLIENTES NUEVOS                              *
      *================================================================*
       4000-RPT-CLIENTES.
           MOVE ZERO TO WS-CTR-CLIENTES-NVO
           MOVE '>>> INICIO REPORTE CLIENTES NUEVOS'
               TO WS-LOG-LINE
           PERFORM 9200-LOG-WRITE
           PERFORM 1100-ARMAR-FILTRO-LIKE
           STRING '../docs/reporteria/RPT_CLIENTES_' DELIMITED SIZE
                  WS-PERIODO                       DELIMITED SIZE
                  '.DAT'                           DELIMITED SIZE
                  INTO WS-NOM-CLIENTES
           END-STRING
           OPEN OUTPUT FS-CLIENTES
           PERFORM 9300-VERIFICAR-FS
           IF WS-FS-OK = 'N'
               MOVE 'ERROR ABRIENDO ARCHIVO CLIENTES'
                   TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF
           MOVE 'REPORTE CLIENTES NUEVOS - PERIODO: '
               TO RP-TIT-TEXT
           WRITE FS-REG-CLIENTES FROM RP-TITULO
           WRITE FS-REG-CLIENTES FROM RP-BLANK
           MOVE 'ID       NOMBRE                   '
             & 'APELLIDO                  FECHA-ALTA SALDO'
               TO RP-HEADER
           WRITE FS-REG-CLIENTES FROM RP-HEADER
           WRITE FS-REG-CLIENTES FROM RP-SEPAR
      *    EXEC SQL OPEN CUR-CLIENTES END-EXEC
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-PERIODO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 WS-ANIO-MES-LIKE
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 9 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-0
                               SQLCA
           END-CALL
           PERFORM 9100-VERIFICAR-OPEN
           IF WS-FIN-CURSOR = 'S'
               MOVE 'ERROR ABRIENDO CUR-CLIENTES' TO LK-MENSAJE
               CLOSE FS-CLIENTES
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-FIN-CURSOR = 'S'
      *        EXEC SQL
      *            FETCH CUR-CLIENTES INTO
      *                :HV-ID-CLIENTE,
      *                :HV-NOMBRE,
      *                :HV-APELLIDOS,
      *                :HV-FECHA-ALTA,
      *                :HV-SALDO-CLI
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             HV-NOMBRE
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 25 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             HV-APELLIDOS
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 25 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             HV-FECHA-ALTA
           MOVE 'X' TO SQL-TYPE(4)
           MOVE 10 TO SQL-LEN(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0002
           MOVE '3' TO SQL-TYPE(5)
           MOVE 7 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           MOVE 5 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-0
                               SQLCA
           MOVE SQL-VAR-0001 TO HV-ID-CLIENTE
           MOVE SQL-VAR-0002 TO HV-SALDO-CLI
               EVALUATE SQLCODE
                   WHEN 0
                       MOVE HV-ID-CLIENTE   TO WS-DC-ID
                       MOVE HV-NOMBRE       TO WS-DC-NOMBRE
                       MOVE HV-APELLIDOS    TO WS-DC-APELLIDO
                       MOVE HV-FECHA-ALTA   TO WS-DC-FECHA-ALTA
                       MOVE HV-SALDO-CLI    TO WS-DC-SALDO
                       WRITE FS-REG-CLIENTES FROM WS-DET-CLIENTES
                       ADD 1 TO WS-CTR-CLIENTES-NVO
                   WHEN 100
                       MOVE 'S' TO WS-FIN-CURSOR
                   WHEN OTHER
                       MOVE 'ERROR FETCH CUR-CLIENTES' TO LK-MENSAJE
                       MOVE 'S' TO WS-FIN-CURSOR
               END-EVALUATE
           END-PERFORM
      *    EXEC SQL CLOSE CUR-CLIENTES END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-0
                               SQLCA
           WRITE FS-REG-CLIENTES FROM RP-SEPAR
           MOVE SPACES TO RP-INDIC
           STRING '  CLIENTES NUEVOS DEL PERIODO: ' DELIMITED SIZE
                  WS-CTR-CLIENTES-NVO               DELIMITED SIZE
                  INTO RP-INDIC
           END-STRING
           WRITE FS-REG-CLIENTES FROM RP-INDIC
           CLOSE FS-CLIENTES
           MOVE WS-CTR-CLIENTES-NVO TO WS-AUX-LOG-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' RPT CLIENTES - REGISTROS: '
                      DELIMITED BY SIZE
                  WS-AUX-LOG-CTR
                      DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9200-LOG-WRITE
           DISPLAY 'REPORTE CLIENTES GENERADO. PRESIONE ENTER.'
           ACCEPT WS-PAUSA
           PERFORM 9900-LIMPIAR-PANTALLA
           MOVE 0 TO LK-COD-RETORNO
           MOVE 'REPORTE CLIENTES GENERADO EXITOSAMENTE' TO LK-MENSAJE.

      *================================================================*
      *   5000 - REPORTE HIPOTECAS                                     *
      *================================================================*
       5000-RPT-HIPOTECAS.
           MOVE ZERO TO WS-CTR-HIPOTECAS
           MOVE '>>> INICIO REPORTE HIPOTECAS'
               TO WS-LOG-LINE
           PERFORM 9200-LOG-WRITE
           MOVE ZERO TO WS-SUM-SALDO-HIPO
           STRING '../docs/reporteria/RPT_HIPOTECAS_' DELIMITED SIZE
                  WS-PERIODO                        DELIMITED SIZE
                  '.DAT'                            DELIMITED SIZE
                  INTO WS-NOM-HIPOTECAS
           END-STRING
           OPEN OUTPUT FS-HIPOTECAS
           PERFORM 9300-VERIFICAR-FS
           IF WS-FS-OK = 'N'
               MOVE 'ERROR ABRIENDO ARCHIVO HIPOTECAS'
                   TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF
           MOVE 'REPORTE HIPOTECAS - PERIODO: ' TO RP-TIT-TEXT
           WRITE FS-REG-HIPOTECAS FROM RP-TITULO
           WRITE FS-REG-HIPOTECAS FROM RP-BLANK
           MOVE 'ID-HIPOT  ID-CLI   NOMBRE                   '
             & 'MONTO-ORIG       SALDO-ACT        TASA    ESTADO'
               TO RP-HEADER
           WRITE FS-REG-HIPOTECAS FROM RP-HEADER
           WRITE FS-REG-HIPOTECAS FROM RP-SEPAR
      *    EXEC SQL OPEN CUR-HIPOTECAS END-EXEC
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-PERIODO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-1
                               SQLCA
           END-CALL
           PERFORM 9100-VERIFICAR-OPEN
           IF WS-FIN-CURSOR = 'S'
               MOVE 'ERROR ABRIENDO CUR-HIPOTECAS' TO LK-MENSAJE
               CLOSE FS-HIPOTECAS
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-FIN-CURSOR = 'S'
      *        EXEC SQL
      *            FETCH CUR-HIPOTECAS INTO
      *                :HV-HIPO-ID-HIPO,
      *                :HV-HIPO-ID-CLI,
      *                :HV-HIPO-NOMBRE,
      *                :HV-HIPO-MONTO-ORIG,
      *                :HV-HIPO-SALDO,
      *                :HV-HIPO-TASA,
      *                :HV-HIPO-ESTADO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0011
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             SQL-VAR-0012
           MOVE '3' TO SQL-TYPE(2)
           MOVE 5 TO SQL-LEN(2)
               MOVE X'00' TO SQL-PREC(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             HV-HIPO-NOMBRE
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 25 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0013
           MOVE '3' TO SQL-TYPE(4)
           MOVE 8 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0014
           MOVE '3' TO SQL-TYPE(5)
           MOVE 8 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             SQL-VAR-0015
           MOVE '3' TO SQL-TYPE(6)
           MOVE 4 TO SQL-LEN(6)
               MOVE X'04' TO SQL-PREC(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             HV-HIPO-ESTADO
           MOVE 'X' TO SQL-TYPE(7)
           MOVE 20 TO SQL-LEN(7)
           MOVE 7 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-1
                               SQLCA
           MOVE SQL-VAR-0011 TO HV-HIPO-ID-HIPO
           MOVE SQL-VAR-0012 TO HV-HIPO-ID-CLI
           MOVE SQL-VAR-0013 TO HV-HIPO-MONTO-ORIG
           MOVE SQL-VAR-0014 TO HV-HIPO-SALDO
           MOVE SQL-VAR-0015 TO HV-HIPO-TASA
               EVALUATE SQLCODE
                   WHEN 0
                       MOVE HV-HIPO-ID-HIPO    TO WS-DH-ID-HIPO
                       MOVE HV-HIPO-ID-CLI     TO WS-DH-ID-CLI
                       MOVE HV-HIPO-NOMBRE     TO WS-DH-NOMBRE
                       MOVE HV-HIPO-MONTO-ORIG TO WS-DH-MONTO-ORIG
                       MOVE HV-HIPO-SALDO      TO WS-DH-SALDO
                       MOVE HV-HIPO-TASA       TO WS-DH-TASA
                       MOVE HV-HIPO-ESTADO     TO WS-DH-ESTADO
                       WRITE FS-REG-HIPOTECAS FROM WS-DET-HIPOTECAS
                       ADD 1             TO WS-CTR-HIPOTECAS
                       ADD HV-HIPO-SALDO TO WS-SUM-SALDO-HIPO
                   WHEN 100
                       MOVE 'S' TO WS-FIN-CURSOR
                   WHEN OTHER
                       MOVE 'ERROR FETCH CUR-HIPOTECAS' TO LK-MENSAJE
                       MOVE 'S' TO WS-FIN-CURSOR
               END-EVALUATE
           END-PERFORM
      *    EXEC SQL CLOSE CUR-HIPOTECAS END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-1
                               SQLCA
           WRITE FS-REG-HIPOTECAS FROM RP-SEPAR
           MOVE SPACES TO RP-INDIC
           STRING '  TOTAL HIPOTECAS: '  DELIMITED SIZE
                  WS-CTR-HIPOTECAS       DELIMITED SIZE
                  '  SALDO TOTAL: '      DELIMITED SIZE
                  WS-SUM-SALDO-HIPO      DELIMITED SIZE
                  INTO RP-INDIC
           END-STRING
           WRITE FS-REG-HIPOTECAS FROM RP-INDIC
           CLOSE FS-HIPOTECAS
           MOVE WS-CTR-HIPOTECAS TO WS-AUX-LOG-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' RPT HIPOTECAS - REGISTROS: '
                      DELIMITED BY SIZE
                  WS-AUX-LOG-CTR
                      DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9200-LOG-WRITE
           DISPLAY 'REPORTE HIPOTECAS GENERADO. PRESIONE ENTER.'
           ACCEPT WS-PAUSA
           PERFORM 9900-LIMPIAR-PANTALLA
           MOVE 0 TO LK-COD-RETORNO
           MOVE 'REPORTE HIPOTECAS GENERADO EXITOSAMENTE'
               TO LK-MENSAJE.

      *================================================================*
      *   6000 - REPORTE TARJETAS                                      *
      *================================================================*
       6000-RPT-TARJETAS.
           MOVE ZERO TO WS-CTR-TARJETAS
           MOVE ZERO TO WS-SUM-ACUM-TARJ
           MOVE '>>> INICIO REPORTE TARJETAS'
               TO WS-LOG-LINE
           PERFORM 9200-LOG-WRITE
           STRING '../docs/reporteria/RPT_TARJETAS_' DELIMITED SIZE
                  WS-PERIODO                       DELIMITED SIZE
                  '.DAT'                           DELIMITED SIZE
                  INTO WS-NOM-TARJETAS
           END-STRING
           OPEN OUTPUT FS-TARJETAS
           PERFORM 9300-VERIFICAR-FS
           IF WS-FS-OK = 'N'
               MOVE 'ERROR ABRIENDO ARCHIVO TARJETAS'
                   TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF
           MOVE 'REPORTE TARJETAS - PERIODO: ' TO RP-TIT-TEXT
           WRITE FS-REG-TARJETAS FROM RP-TITULO
           WRITE FS-REG-TARJETAS FROM RP-BLANK
           MOVE 'ID-CLI   NOMBRE                   NRO-TARJETA'
             & '     LIMITE       ACUM-MES     LIQUIDACION  E'
               TO RP-HEADER
           WRITE FS-REG-TARJETAS FROM RP-HEADER
           WRITE FS-REG-TARJETAS FROM RP-SEPAR
      *    EXEC SQL OPEN CUR-TARJETAS END-EXEC
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-PERIODO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-2
                               SQLCA
           END-CALL
           PERFORM 9100-VERIFICAR-OPEN
           IF WS-FIN-CURSOR = 'S'
               MOVE 'ERROR ABRIENDO CUR-TARJETAS' TO LK-MENSAJE
               CLOSE FS-TARJETAS
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-FIN-CURSOR = 'S'
      *        EXEC SQL
      *            FETCH CUR-TARJETAS INTO
      *                :HV-TARJ-ID-CLI,
      *                :HV-TARJ-NOMBRE,
      *                :HV-TARJ-NRO,
      *                :HV-TARJ-LIMITE,
      *                :HV-TARJ-ACUM,
      *                :HV-TARJ-LIQUID,
      *                :HV-TARJ-ESTADO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0007
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             HV-TARJ-NOMBRE
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 25 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             HV-TARJ-NRO
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 16 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0008
           MOVE '3' TO SQL-TYPE(4)
           MOVE 7 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0009
           MOVE '3' TO SQL-TYPE(5)
           MOVE 7 TO SQL-LEN(5)
               MOVE X'02' TO SQL-PREC(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             SQL-VAR-0010
           MOVE '3' TO SQL-TYPE(6)
           MOVE 7 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             HV-TARJ-ESTADO
           MOVE 'X' TO SQL-TYPE(7)
           MOVE 1 TO SQL-LEN(7)
           MOVE 7 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-2
                               SQLCA
           MOVE SQL-VAR-0007 TO HV-TARJ-ID-CLI
           MOVE SQL-VAR-0008 TO HV-TARJ-LIMITE
           MOVE SQL-VAR-0009 TO HV-TARJ-ACUM
           MOVE SQL-VAR-0010 TO HV-TARJ-LIQUID
               EVALUATE SQLCODE
                   WHEN 0
                       MOVE HV-TARJ-ID-CLI  TO WS-DT-ID-CLI
                       MOVE HV-TARJ-NOMBRE  TO WS-DT-NOMBRE
                       MOVE HV-TARJ-NRO     TO WS-DT-NRO
                       MOVE HV-TARJ-LIMITE  TO WS-DT-LIMITE
                       MOVE HV-TARJ-ACUM    TO WS-DT-ACUM
                       MOVE HV-TARJ-LIQUID  TO WS-DT-LIQUID
                       MOVE HV-TARJ-ESTADO  TO WS-DT-ESTADO
                       WRITE FS-REG-TARJETAS FROM WS-DET-TARJETAS
                       ADD 1            TO WS-CTR-TARJETAS
                       ADD HV-TARJ-ACUM TO WS-SUM-ACUM-TARJ
                   WHEN 100
                       MOVE 'S' TO WS-FIN-CURSOR
                   WHEN OTHER
                       MOVE 'ERROR FETCH CUR-TARJETAS' TO LK-MENSAJE
                       MOVE 'S' TO WS-FIN-CURSOR
               END-EVALUATE
           END-PERFORM
      *    EXEC SQL CLOSE CUR-TARJETAS END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-2
                               SQLCA
           WRITE FS-REG-TARJETAS FROM RP-SEPAR
           MOVE SPACES TO RP-INDIC
           STRING '  TOTAL TARJETAS: '   DELIMITED SIZE
                  WS-CTR-TARJETAS        DELIMITED SIZE
                  '  ACUMULADO TOTAL: '  DELIMITED SIZE
                  WS-SUM-ACUM-TARJ       DELIMITED SIZE
                  INTO RP-INDIC
           END-STRING
           WRITE FS-REG-TARJETAS FROM RP-INDIC
           CLOSE FS-TARJETAS
           MOVE WS-CTR-TARJETAS TO WS-AUX-LOG-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' RPT TARJETAS - REGISTROS: '
                      DELIMITED BY SIZE
                  WS-AUX-LOG-CTR
                      DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9200-LOG-WRITE
           DISPLAY 'REPORTE TARJETAS GENERADO. PRESIONE ENTER.'
           ACCEPT WS-PAUSA
           PERFORM 9900-LIMPIAR-PANTALLA
           MOVE 0 TO LK-COD-RETORNO
           MOVE 'REPORTE TARJETAS GENERADO EXITOSAMENTE'
               TO LK-MENSAJE.

      *================================================================*
      *   7000 - REPORTE CUENTAS CORRIENTES                            *
      *================================================================*
       7000-RPT-CUENTAS.
           MOVE ZERO TO WS-CTR-CUENTAS
           MOVE ZERO TO WS-SUM-SALDO-CTA
           MOVE '>>> INICIO REPORTE CUENTAS CORRIENTES'
               TO WS-LOG-LINE
           PERFORM 9200-LOG-WRITE
           STRING '../docs/reporteria/RPT_CUENTAS_' DELIMITED SIZE
                  WS-PERIODO                      DELIMITED SIZE
                  '.DAT'                          DELIMITED SIZE
                  INTO WS-NOM-CUENTAS
           END-STRING
           OPEN OUTPUT FS-CUENTAS
           PERFORM 9300-VERIFICAR-FS
           IF WS-FS-OK = 'N'
               MOVE 'ERROR ABRIENDO ARCHIVO CUENTAS'
                   TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF
           MOVE 'REPORTE CUENTAS CORRIENTES - PERIODO: '
               TO RP-TIT-TEXT
           WRITE FS-REG-CUENTAS FROM RP-TITULO
           WRITE FS-REG-CUENTAS FROM RP-BLANK
           MOVE 'ID-CLI   NOMBRE                   APELLIDO'
             & '                 SALDO-CTA    COD IMPORTE-MOV'
               TO RP-HEADER
           WRITE FS-REG-CUENTAS FROM RP-HEADER
           WRITE FS-REG-CUENTAS FROM RP-SEPAR
      *    EXEC SQL OPEN CUR-CUENTAS END-EXEC
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-PERIODO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-3
                               SQLCA
           END-CALL
           PERFORM 9100-VERIFICAR-OPEN
           IF WS-FIN-CURSOR = 'S'
               MOVE 'ERROR ABRIENDO CUR-CUENTAS' TO LK-MENSAJE
               CLOSE FS-CUENTAS
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-FIN-CURSOR = 'S'
      *        EXEC SQL
      *            FETCH CUR-CUENTAS INTO
      *                :HV-ID-CLIENTE,
      *                :HV-NOMBRE,
      *                :HV-APELLIDOS,
      *                :HV-SALDO-CTA,
      *                :HV-COD-ULT-MOV,
      *                :HV-IMPORTE-MOV
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0001
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             HV-NOMBRE
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 25 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             HV-APELLIDOS
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 25 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0003
           MOVE '3' TO SQL-TYPE(4)
           MOVE 7 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             SQL-VAR-0004
           MOVE '3' TO SQL-TYPE(5)
           MOVE 2 TO SQL-LEN(5)
               MOVE X'00' TO SQL-PREC(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             SQL-VAR-0005
           MOVE '3' TO SQL-TYPE(6)
           MOVE 7 TO SQL-LEN(6)
               MOVE X'02' TO SQL-PREC(6)
           MOVE 6 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-3
                               SQLCA
           MOVE SQL-VAR-0001 TO HV-ID-CLIENTE
           MOVE SQL-VAR-0003 TO HV-SALDO-CTA
           MOVE SQL-VAR-0004 TO HV-COD-ULT-MOV
           MOVE SQL-VAR-0005 TO HV-IMPORTE-MOV
               EVALUATE SQLCODE
                   WHEN 0
                       MOVE HV-ID-CLIENTE   TO WS-DCU-ID
                       MOVE HV-NOMBRE       TO WS-DCU-NOMBRE
                       MOVE HV-APELLIDOS    TO WS-DCU-APELLIDO
                       MOVE HV-SALDO-CTA    TO WS-DCU-SALDO
                       MOVE HV-COD-ULT-MOV  TO WS-DCU-COD-MOV
                       MOVE HV-IMPORTE-MOV  TO WS-DCU-IMPORTE
                       WRITE FS-REG-CUENTAS FROM WS-DET-CUENTAS
                       ADD 1            TO WS-CTR-CUENTAS
                       ADD HV-SALDO-CTA TO WS-SUM-SALDO-CTA
                   WHEN 100
                       MOVE 'S' TO WS-FIN-CURSOR
                   WHEN OTHER
                       MOVE 'ERROR FETCH CUR-CUENTAS' TO LK-MENSAJE
                       MOVE 'S' TO WS-FIN-CURSOR
               END-EVALUATE
           END-PERFORM
      *    EXEC SQL CLOSE CUR-CUENTAS END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-3
                               SQLCA
           WRITE FS-REG-CUENTAS FROM RP-SEPAR
           MOVE SPACES TO RP-INDIC
           STRING '  CUENTAS ACTIVAS: '  DELIMITED SIZE
                  WS-CTR-CUENTAS         DELIMITED SIZE
                  '  SALDO TOTAL: '      DELIMITED SIZE
                  WS-SUM-SALDO-CTA       DELIMITED SIZE
                  INTO RP-INDIC
           END-STRING
           WRITE FS-REG-CUENTAS FROM RP-INDIC
           CLOSE FS-CUENTAS
           MOVE WS-CTR-CUENTAS TO WS-AUX-LOG-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' RPT CUENTAS - REGISTROS: '
                      DELIMITED BY SIZE
                  WS-AUX-LOG-CTR
                      DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9200-LOG-WRITE
           DISPLAY 'REPORTE CUENTAS GENERADO. PRESIONE ENTER.'
           ACCEPT WS-PAUSA
           PERFORM 9900-LIMPIAR-PANTALLA
           MOVE 0 TO LK-COD-RETORNO
           MOVE 'REPORTE CUENTAS GENERADO EXITOSAMENTE'
               TO LK-MENSAJE.

      *================================================================*
      *   8000 - REPORTE GENERAL                                       *
      *================================================================*
       8000-RPT-GENERAL.
           MOVE ZERO TO WS-CTR-GEN-CLIENTES
           MOVE ZERO TO WS-CTR-GEN-TARJETAS
           MOVE ZERO TO WS-CTR-GEN-HIPOTECAS
           MOVE ZERO TO WS-ULTIMO-ID-CLI
           MOVE '>>> INICIO REPORTE GENERAL'
               TO WS-LOG-LINE
           PERFORM 9200-LOG-WRITE
           STRING '../docs/reporteria/RPT_GENERAL_' DELIMITED SIZE
                  WS-PERIODO                      DELIMITED SIZE
                  '.DAT'                          DELIMITED SIZE
                  INTO WS-NOM-GENERAL
           END-STRING
           OPEN OUTPUT FS-GENERAL
           PERFORM 9300-VERIFICAR-FS
           IF WS-FS-OK = 'N'
               MOVE 'ERROR ABRIENDO ARCHIVO GENERAL'
                   TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF
           MOVE 'REPORTE GENERAL - PERIODO: ' TO RP-TIT-TEXT
           WRITE FS-REG-GENERAL FROM RP-TITULO
           WRITE FS-REG-GENERAL FROM RP-BLANK
           MOVE 'ID-CLI   NOMBRE                   APELLIDO'
             & '                 SALDO-CTA    NRO-TARJETA'
             & '     ACUM-MES    SALDO-HIPOT'
               TO RP-HEADER
           WRITE FS-REG-GENERAL FROM RP-HEADER
           MOVE 'NOTA: una fila por producto (tarjeta/hipoteca)'
               TO RP-INDIC
           WRITE FS-REG-GENERAL FROM RP-INDIC
           WRITE FS-REG-GENERAL FROM RP-SEPAR
      *    EXEC SQL OPEN CUR-GENERAL END-EXEC
           IF SQL-PREP OF SQL-STMT-4 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-PERIODO
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 6 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-4
                                   SQLCA
           END-IF
           CALL 'OCSQLOCU' USING SQL-STMT-4
                               SQLCA
           END-CALL
           PERFORM 9100-VERIFICAR-OPEN
           IF WS-FIN-CURSOR = 'S'
               MOVE 'ERROR ABRIENDO CUR-GENERAL' TO LK-MENSAJE
               CLOSE FS-GENERAL
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-FIN-CURSOR = 'S'
      *        EXEC SQL
      *            FETCH CUR-GENERAL INTO
      *                :HV-GEN-ID-CLI,
      *                :HV-GEN-NOMBRE,
      *                :HV-GEN-APELLIDOS,
      *                :HV-GEN-SALDO-CTA,
      *                :HV-GEN-NRO-TARJ   :HV-IND-NRO-TARJ,
      *                :HV-GEN-ACUM-MES   :HV-IND-ACUM-MES,
      *                :HV-GEN-SALDO-HIPO :HV-IND-SALDO-HIPO
      *        END-EXEC
           SET SQL-ADDR(1) TO ADDRESS OF
             SQL-VAR-0016
           MOVE '3' TO SQL-TYPE(1)
           MOVE 5 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
           SET SQL-ADDR(2) TO ADDRESS OF
             HV-GEN-NOMBRE
           MOVE 'X' TO SQL-TYPE(2)
           MOVE 25 TO SQL-LEN(2)
           SET SQL-ADDR(3) TO ADDRESS OF
             HV-GEN-APELLIDOS
           MOVE 'X' TO SQL-TYPE(3)
           MOVE 25 TO SQL-LEN(3)
           SET SQL-ADDR(4) TO ADDRESS OF
             SQL-VAR-0017
           MOVE '3' TO SQL-TYPE(4)
           MOVE 7 TO SQL-LEN(4)
               MOVE X'02' TO SQL-PREC(4)
           SET SQL-ADDR(5) TO ADDRESS OF
             HV-GEN-NRO-TARJ
           MOVE 'X' TO SQL-TYPE(5)
           MOVE 16 TO SQL-LEN(5)
           SET SQL-ADDR(6) TO ADDRESS OF
             HV-IND-NRO-TARJ
           MOVE 'i' TO SQL-TYPE(6)
           SET SQL-ADDR(7) TO ADDRESS OF
             SQL-VAR-0018
           MOVE '3' TO SQL-TYPE(7)
           MOVE 7 TO SQL-LEN(7)
               MOVE X'02' TO SQL-PREC(7)
           SET SQL-ADDR(8) TO ADDRESS OF
             HV-IND-ACUM-MES
           MOVE 'i' TO SQL-TYPE(8)
           SET SQL-ADDR(9) TO ADDRESS OF
             SQL-VAR-0019
           MOVE '3' TO SQL-TYPE(9)
           MOVE 8 TO SQL-LEN(9)
               MOVE X'02' TO SQL-PREC(9)
           SET SQL-ADDR(10) TO ADDRESS OF
             HV-IND-SALDO-HIPO
           MOVE 'i' TO SQL-TYPE(10)
           MOVE 10 TO SQL-COUNT
           CALL 'OCSQLFTC' USING SQLV
                               SQL-STMT-4
                               SQLCA
           MOVE SQL-VAR-0016 TO HV-GEN-ID-CLI
           MOVE SQL-VAR-0017 TO HV-GEN-SALDO-CTA
           MOVE SQL-VAR-0018 TO HV-GEN-ACUM-MES
           MOVE SQL-VAR-0019 TO HV-GEN-SALDO-HIPO
               EVALUATE SQLCODE
                   WHEN 0
                       IF HV-IND-NRO-TARJ   < 0
                           MOVE 'SIN TARJETA     '
                               TO HV-GEN-NRO-TARJ
                       END-IF
                       IF HV-IND-ACUM-MES   < 0
                           MOVE ZERO TO HV-GEN-ACUM-MES
                       END-IF
                       IF HV-IND-SALDO-HIPO < 0
                           MOVE ZERO TO HV-GEN-SALDO-HIPO
                       END-IF
                       IF HV-GEN-ID-CLI NOT = WS-ULTIMO-ID-CLI
                           ADD 1 TO WS-CTR-GEN-CLIENTES
                           MOVE HV-GEN-ID-CLI TO WS-ULTIMO-ID-CLI
                       END-IF
                       IF HV-IND-NRO-TARJ >= 0
                           ADD 1 TO WS-CTR-GEN-TARJETAS
                       END-IF
                       IF HV-IND-SALDO-HIPO >= 0
                           ADD 1 TO WS-CTR-GEN-HIPOTECAS
                       END-IF
                       MOVE HV-GEN-ID-CLI     TO WS-DG-ID-CLI
                       MOVE HV-GEN-NOMBRE     TO WS-DG-NOMBRE
                       MOVE HV-GEN-APELLIDOS  TO WS-DG-APELLIDOS
                       MOVE HV-GEN-SALDO-CTA  TO WS-DG-SALDO-CTA
                       MOVE HV-GEN-NRO-TARJ   TO WS-DG-NRO-TARJ
                       MOVE HV-GEN-ACUM-MES   TO WS-DG-ACUM-MES
                       MOVE HV-GEN-SALDO-HIPO TO WS-DG-SALDO-HIPO
                       WRITE FS-REG-GENERAL FROM WS-DET-GENERAL
                   WHEN 100
                       MOVE 'S' TO WS-FIN-CURSOR
                   WHEN OTHER
                       MOVE 'ERROR FETCH CUR-GENERAL' TO LK-MENSAJE
                       MOVE 'S' TO WS-FIN-CURSOR
               END-EVALUATE
           END-PERFORM
      *    EXEC SQL CLOSE CUR-GENERAL END-EXEC
           CALL 'OCSQLCCU' USING SQL-STMT-4
                               SQLCA
           WRITE FS-REG-GENERAL FROM RP-SEPAR
           MOVE SPACES TO RP-INDIC
           STRING '  CLIENTES: '         DELIMITED SIZE
                  WS-CTR-GEN-CLIENTES    DELIMITED SIZE
                  '  TARJETAS: '         DELIMITED SIZE
                  WS-CTR-GEN-TARJETAS    DELIMITED SIZE
                  '  HIPOTECAS: '        DELIMITED SIZE
                  WS-CTR-GEN-HIPOTECAS   DELIMITED SIZE
                  INTO RP-INDIC
           END-STRING
           WRITE FS-REG-GENERAL FROM RP-INDIC
           CLOSE FS-GENERAL
           MOVE WS-CTR-GEN-CLIENTES TO WS-AUX-LOG-CTR
           MOVE SPACES TO WS-LOG-LINE
           STRING ' RPT GENERAL - CLIENTES: '
                      DELIMITED BY SIZE
                  WS-AUX-LOG-CTR
                      DELIMITED BY SIZE
               INTO WS-LOG-LINE
           PERFORM 9200-LOG-WRITE
           DISPLAY 'REPORTE GENERAL GENERADO. PRESIONE ENTER.'
           ACCEPT WS-PAUSA
           PERFORM 9900-LIMPIAR-PANTALLA
           MOVE 0 TO LK-COD-RETORNO
           MOVE 'REPORTE GENERAL GENERADO EXITOSAMENTE'
               TO LK-MENSAJE.

      *================================================================*
      *   9000 - EVALUAR SQL                                           *
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

       9900-LIMPIAR-PANTALLA.
           CALL "SYSTEM" USING WS-CLS-CMD.

      *================================================================*
      *   9300 - VERIFICAR FILE STATUS tras OPEN OUTPUT
      *================================================================*
       9300-VERIFICAR-FS.
           IF WS-FS-STATUS = '00'
               MOVE 'S' TO WS-FS-OK
           ELSE
               MOVE 'N' TO WS-FS-OK
               MOVE SPACES TO WS-LOG-LINE
               STRING 'ERROR FILE STATUS: '
                          DELIMITED BY SIZE
                      WS-FS-STATUS
                          DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9200-LOG-WRITE
               DISPLAY 'ERROR ABRIENDO ARCHIVO. STATUS: '
                       WS-FS-STATUS
               DISPLAY 'Verifique que exista la carpeta '
                       'docs/reporteria'
           END-IF.

      *================================================================*
      *   9200 - ESCRIBIR LINEA AL ARCHIVO DE LOG                     *
      *           Destino: ..\docs\logs\RPT_LOGS_<PERIODO>.txt         *
      *================================================================*
       9200-LOG-WRITE.
           CALL 'LOGFILE' USING WS-MODULO-LOG,
                                WS-PERIODO,
                                WS-LOG-LINE.

       END PROGRAM RP0000.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  CUR-CLIENTES             IN USE CURSOR
      *  CUR-CUENTAS              IN USE CURSOR
      *  CUR-GENERAL              IN USE CURSOR
      *  CUR-HIPOTECAS            IN USE CURSOR
      *  CUR-TARJETAS             IN USE CURSOR
      *  HV-APELLIDOS             IN USE CHAR(25)
      *  HV-COD-ULT-MOV           IN USE THROUGH TEMP VAR SQL-VAR-0004 DECIMAL(3,0)
      *  HV-CTA-ACTIVA        NOT IN USE
      *  HV-FECHA-ALTA            IN USE CHAR(10)
      *  HV-GEN-ACUM-MES          IN USE THROUGH TEMP VAR SQL-VAR-0018 DECIMAL(13,2)
      *  HV-GEN-APELLIDOS         IN USE CHAR(25)
      *  HV-GEN-ID-CLI            IN USE THROUGH TEMP VAR SQL-VAR-0016 DECIMAL(9,0)
      *  HV-GEN-NOMBRE            IN USE CHAR(25)
      *  HV-GEN-NRO-TARJ          IN USE CHAR(16)
      *  HV-GEN-SALDO-CTA         IN USE THROUGH TEMP VAR SQL-VAR-0017 DECIMAL(13,2)
      *  HV-GEN-SALDO-HIPO        IN USE THROUGH TEMP VAR SQL-VAR-0019 DECIMAL(15,2)
      *  HV-HIPO-ESTADO           IN USE CHAR(20)
      *  HV-HIPO-ID-CLI           IN USE THROUGH TEMP VAR SQL-VAR-0012 DECIMAL(9,0)
      *  HV-HIPO-ID-HIPO          IN USE THROUGH TEMP VAR SQL-VAR-0011 DECIMAL(9,0)
      *  HV-HIPO-MONTO-ORIG       IN USE THROUGH TEMP VAR SQL-VAR-0013 DECIMAL(15,2)
      *  HV-HIPO-NOMBRE           IN USE CHAR(25)
      *  HV-HIPO-SALDO            IN USE THROUGH TEMP VAR SQL-VAR-0014 DECIMAL(15,2)
      *  HV-HIPO-TASA             IN USE THROUGH TEMP VAR SQL-VAR-0015 DECIMAL(7,4)
      *  HV-ID-CLIENTE            IN USE THROUGH TEMP VAR SQL-VAR-0001 DECIMAL(9,0)
      *  HV-IMPORTE-MOV           IN USE THROUGH TEMP VAR SQL-VAR-0005 DECIMAL(13,2)
      *  HV-IND-ACUM-MES          IN USE INTEGER(2 BYTES)
      *  HV-IND-COUNT             IN USE THROUGH TEMP VAR SQL-VAR-0020 DECIMAL(9,0)
      *  HV-IND-NRO-TARJ          IN USE INTEGER(2 BYTES)
      *  HV-IND-SALDO-HIPO        IN USE INTEGER(2 BYTES)
      *  HV-NOMBRE                IN USE CHAR(25)
      *  HV-SALDO-CLI             IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(13,2)
      *  HV-SALDO-CTA             IN USE THROUGH TEMP VAR SQL-VAR-0003 DECIMAL(13,2)
      *  HV-TARJ-ACUM             IN USE THROUGH TEMP VAR SQL-VAR-0009 DECIMAL(13,2)
      *  HV-TARJ-ESTADO           IN USE CHAR(1)
      *  HV-TARJ-ID-CLI           IN USE THROUGH TEMP VAR SQL-VAR-0007 DECIMAL(9,0)
      *  HV-TARJ-LIMITE           IN USE THROUGH TEMP VAR SQL-VAR-0008 DECIMAL(13,2)
      *  HV-TARJ-LIQUID           IN USE THROUGH TEMP VAR SQL-VAR-0010 DECIMAL(13,2)
      *  HV-TARJ-NOMBRE           IN USE CHAR(25)
      *  HV-TARJ-NRO              IN USE CHAR(16)
      *  WS-ANIO-MES-LIKE         IN USE CHAR(9)
      *  WS-HOST-GENERAL      NOT IN USE
      *  WS-HOST-GENERAL.HV-GEN-ACUM-MES NOT IN USE
      *  WS-HOST-GENERAL.HV-GEN-APELLIDOS NOT IN USE
      *  WS-HOST-GENERAL.HV-GEN-ID-CLI NOT IN USE
      *  WS-HOST-GENERAL.HV-GEN-NOMBRE NOT IN USE
      *  WS-HOST-GENERAL.HV-GEN-NRO-TARJ NOT IN USE
      *  WS-HOST-GENERAL.HV-GEN-SALDO-CTA NOT IN USE
      *  WS-HOST-GENERAL.HV-GEN-SALDO-HIPO NOT IN USE
      *  WS-HOST-GENERAL.HV-IND-ACUM-MES NOT IN USE
      *  WS-HOST-GENERAL.HV-IND-NRO-TARJ NOT IN USE
      *  WS-HOST-GENERAL.HV-IND-SALDO-HIPO NOT IN USE
      *  WS-HOST-HIPOTECAS    NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-ESTADO NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-ID-CLI NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-ID-HIPO NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-MONTO-ORIG NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-NOMBRE NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-SALDO NOT IN USE
      *  WS-HOST-HIPOTECAS.HV-HIPO-TASA NOT IN USE
      *  WS-HOST-MAESTRA      NOT IN USE
      *  WS-HOST-MAESTRA.HV-APELLIDOS NOT IN USE
      *  WS-HOST-MAESTRA.HV-COD-ULT-MOV NOT IN USE
      *  WS-HOST-MAESTRA.HV-CTA-ACTIVA NOT IN USE
      *  WS-HOST-MAESTRA.HV-FECHA-ALTA NOT IN USE
      *  WS-HOST-MAESTRA.HV-ID-CLIENTE NOT IN USE
      *  WS-HOST-MAESTRA.HV-IMPORTE-MOV NOT IN USE
      *  WS-HOST-MAESTRA.HV-NOMBRE NOT IN USE
      *  WS-HOST-MAESTRA.HV-SALDO-CLI NOT IN USE
      *  WS-HOST-MAESTRA.HV-SALDO-CTA NOT IN USE
      *  WS-HOST-TARJETAS     NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-ACUM NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-ESTADO NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-ID-CLI NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-LIMITE NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-LIQUID NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-NOMBRE NOT IN USE
      *  WS-HOST-TARJETAS.HV-TARJ-NRO NOT IN USE
      *  WS-PERIODO               IN USE CHAR(6)
      *  WS-PERIODO-DEF           IN USE CHAR(6)
      **********************************************************************

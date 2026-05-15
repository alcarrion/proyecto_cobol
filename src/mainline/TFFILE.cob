       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFFILE.
      *========================================================*
      * MOTOR DE INGESTA, VALIDACION Y LOTEO DINAMICO 2.0
      * REGLA DE NOMENCLATURA:
      * XXX-999-DDMMYY-XXXXXX-DDMMYY-00N.TXT
      * (Proceso-Agencia-F.Ref-H.Ref-F.REAL-Secuencia)
      *========================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Archivo temporal para listar archivos del directorio
           SELECT LISTA-ARCHIVOS ASSIGN TO "filelist.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

      * Archivo de entrada (El que está en BATCH-INPUT)
           SELECT DATA-IN ASSIGN TO WS-FULLPATH-IN
           ORGANIZATION IS LINE SEQUENTIAL.

      * Archivo de salida (Los fragmentos en BATCH-UPLOAD-S)
           SELECT DATA-OUT ASSIGN TO WS-FULLPATH-OUT
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  LISTA-ARCHIVOS.
       01  REG-LISTA               PIC X(120).

       FD  DATA-IN.
       01  REG-DATA-IN             PIC X(500).

       FD  DATA-OUT.
       01  REG-DATA-OUT            PIC X(500).

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
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 70.
           05 SQL-STMT   PIC X(70) VALUE 'SELECT VALOR FROM TF_PARAMETRO
      -    'S WHERE PARAMETRO = ''MAX_REGISTROS_LOTE'''.
      **********************************************************************
       01 SQL-STMT-1.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 62.
           05 SQL-STMT   PIC X(62) VALUE 'SELECT VALOR FROM TF_PARAMETRO
      -    'S WHERE PARAMETRO = ''RUTA_INPUT'''.
      **********************************************************************
       01 SQL-STMT-2.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 0.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 63.
           05 SQL-STMT   PIC X(63) VALUE 'SELECT VALOR FROM TF_PARAMETRO
      -    'S WHERE PARAMETRO = ''RUTA_UPLOAD'''.
      **********************************************************************
       01 SQL-STMT-3.
           05 SQL-IPTR   POINTER VALUE NULL.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 3.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 109.
           05 SQL-STMT   PIC X(109) VALUE 'INSERT INTO TFFM (FILE_NAME,T
      -    'YPE_UPDATE,FASE,ESTADO_REPLICA,FECHA_SISTEMA,PRIORITY) VALUE
      -    'S (?,?,''00'',''R'',?,5)'.
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

      * Variables para SQL
      *    EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-PARAM-VARS-SQL.
           05 WS-MAX-LOTE-STR      PIC X(10).
           05 WS-RUTA-IN-SQL       PIC X(150).
           05 WS-RUTA-UP-SQL       PIC X(150).

       01  WS-TFFM-INSERT.
           05 DB-FILE-NAME         PIC X(120).
           05 DB-TYPE-UPDATE       PIC X(10).
           05 DB-FECHA-PROC        PIC X(10).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

      * Variables de Control
       01  WS-MAX-LOTE-NUM         PIC 9(09).
       01  WS-CONTROL-LOTEO.
           05 WS-REG-CONT          PIC 9(09) VALUE 0.
           05 WS-LOTE-SEC          PIC 9(03) VALUE 1.
           05 WS-EOF-LISTA         PIC X(01) VALUE 'N'.
           05 WS-EOF-DATA          PIC X(01) VALUE 'N'.

      * Desglose del nombre de archivo (Estructura pedida)
       01  WS-FILE-PARSING.
           05 WS-TIPO-PROC         PIC X(03).
           05 WS-AGENCIA           PIC X(03).
           05 WS-FECHA-REF         PIC X(06).
           05 WS-HORA-REF          PIC X(06).
           05 WS-FECHA-REAL        PIC X(06).
           05 WS-SEC-ORIGINAL      PIC X(03).
           05 WS-FECHA-ISO         PIC X(10).

       01  WS-RUTAS-TRABAJO.
           05 WS-FILE-NAME-CUR     PIC X(120).
           05 WS-FILE-NAME-NEW     PIC X(120).
           05 WS-FULLPATH-IN       PIC X(250).
           05 WS-FULLPATH-OUT      PIC X(250).
           05 WS-CMD               PIC X(500).

       PROCEDURE DIVISION.
       000-MAIN.
           DISPLAY " "
           DISPLAY ">>> TFFILE: INICIANDO ETAPA DE INGESTA 2.0 <<<"

           PERFORM 100-CARGAR-PARAMETROS-BD
           PERFORM 200-GENERAR-LISTADO-INPUT

           OPEN INPUT LISTA-ARCHIVOS
           PERFORM UNTIL WS-EOF-LISTA = 'Y'
               READ LISTA-ARCHIVOS INTO WS-FILE-NAME-CUR
                   AT END MOVE 'Y' TO WS-EOF-LISTA
                   NOT AT END
                       IF WS-FILE-NAME-CUR NOT = SPACES
                           PERFORM 300-PROCESAR-ARCHIVO
                       END-IF
               END-READ
           END-PERFORM
           CLOSE LISTA-ARCHIVOS.

           DISPLAY ">>> TFFILE: INGESTA FINALIZADA <<<"
           GOBACK.

       100-CARGAR-PARAMETROS-BD.
      * Obtenemos configuracion dinámica de la tabla TF_PARAMETROS
      *    EXEC SQL
      *        SELECT VALOR INTO :WS-MAX-LOTE-STR FROM TF_PARAMETROS
      *        WHERE PARAMETRO = 'MAX_REGISTROS_LOTE'
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-MAX-LOTE-STR
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 10 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
                   .
           MOVE WS-MAX-LOTE-STR TO WS-MAX-LOTE-NUM.

      *    EXEC SQL
      *        SELECT VALOR INTO :WS-RUTA-IN-SQL FROM TF_PARAMETROS
      *        WHERE PARAMETRO = 'RUTA_INPUT'
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-1 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-RUTA-IN-SQL
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 150 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-1
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-1
                               SQLCA
                   .

      *    EXEC SQL
      *        SELECT VALOR INTO :WS-RUTA-UP-SQL FROM TF_PARAMETROS
      *        WHERE PARAMETRO = 'RUTA_UPLOAD'
      *    END-EXEC.
           IF SQL-PREP OF SQL-STMT-2 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 WS-RUTA-UP-SQL
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 150 TO SQL-LEN(1)
               MOVE 1 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-2
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-2
                               SQLCA
                   .

       200-GENERAR-LISTADO-INPUT.
      * Comando de sistema para ver qué hay en la carpeta de entrada
           INITIALIZE WS-CMD
           STRING "dir /b " FUNCTION TRIM(WS-RUTA-IN-SQL)
                  "\*.TXT > filelist.txt"
                  DELIMITED BY SIZE INTO WS-CMD
           CALL "SYSTEM" USING WS-CMD.

       300-PROCESAR-ARCHIVO.
           DISPLAY "  [FILE] " WS-FILE-NAME-CUR

      * PARSEO SEGUN ESTANDAR: XXX-999-DDMMYY-XXXXXX-DDMMYY-00N.TXT
           UNSTRING WS-FILE-NAME-CUR DELIMITED BY "-"
               INTO WS-TIPO-PROC, WS-AGENCIA, WS-FECHA-REF,
                    WS-HORA-REF, WS-FECHA-REAL, WS-SEC-ORIGINAL

      * CONVERSION DE FECHA REAL (DDMMYY) A FORMATO SQL (YYYY-MM-DD)
           STRING "20" WS-FECHA-REAL(5:2) "-"
                  WS-FECHA-REAL(3:2) "-"
                  WS-FECHA-REAL(1:2)
                  INTO WS-FECHA-ISO

      * DETERMINAR PROGRAMA DE NEGOCIO SEGUN TIPO
           EVALUATE WS-TIPO-PROC
               WHEN "DEB" MOVE "RRD000" TO DB-TYPE-UPDATE
               WHEN "RET" MOVE "RRR000" TO DB-TYPE-UPDATE
               WHEN "CRE" MOVE "RRC000" TO DB-TYPE-UPDATE
               WHEN OTHER MOVE "UNKNOWN" TO DB-TYPE-UPDATE
           END-EVALUATE

      * RUTA COMPLETA PARA LECTURA
           STRING FUNCTION TRIM(WS-RUTA-IN-SQL) "\"
                  FUNCTION TRIM(WS-FILE-NAME-CUR)
                  DELIMITED BY SIZE INTO WS-FULLPATH-IN

           PERFORM 400-EJECUTAR-LOTEO.

       400-EJECUTAR-LOTEO.
           MOVE 1 TO WS-LOTE-SEC
           MOVE 0 TO WS-REG-CONT
           MOVE "N" TO WS-EOF-DATA

           OPEN INPUT DATA-IN
           PERFORM UNTIL WS-EOF-DATA = 'Y'
               IF WS-REG-CONT = 0
                   PERFORM 500-GENERAR-FRAGMENTO-FISICO
               END-IF

               READ DATA-IN INTO REG-DATA-IN
                   AT END
                       MOVE 'Y' TO WS-EOF-DATA
                       CLOSE DATA-OUT
                   NOT AT END
                       ADD 1 TO WS-REG-CONT
                       WRITE REG-DATA-OUT FROM REG-DATA-IN

      * Si llegamos al limite del parametro, cerramos este lote y abrimo
                       IF WS-REG-CONT >= WS-MAX-LOTE-NUM
                           CLOSE DATA-OUT
                           MOVE 0 TO WS-REG-CONT
                           ADD 1 TO WS-LOTE-SEC
                       END-IF
               END-READ
           END-PERFORM
           CLOSE DATA-IN.

      * MOVER ORIGINAL A BATCH-DONE PARA NO REPROCESAR
           STRING "move " FUNCTION TRIM(WS-FULLPATH-IN)
                  " ..\banco\spool\Interfaces\BATCH-DONE\"
                  DELIMITED BY SIZE INTO WS-CMD
           CALL "SYSTEM" USING WS-CMD.

       500-GENERAR-FRAGMENTO-FISICO.
      * Generamos el nombre del nuevo lote respetando la secuencia 00N
           INITIALIZE WS-FILE-NAME-NEW
           STRING WS-TIPO-PROC "-" WS-AGENCIA "-" WS-FECHA-REF "-"
                  WS-HORA-REF "-" WS-FECHA-REAL "-"
                  WS-LOTE-SEC ".TXT"
                  DELIMITED BY SIZE INTO WS-FILE-NAME-NEW

           STRING FUNCTION TRIM(WS-RUTA-UP-SQL) "\" WS-FILE-NAME-NEW
                  DELIMITED BY SIZE INTO WS-FULLPATH-OUT

           OPEN OUTPUT DATA-OUT

      * Registro del nuevo lote en la tabla TFFM (Control Maestro)
           MOVE WS-FILE-NAME-NEW TO DB-FILE-NAME
           MOVE WS-FECHA-ISO     TO DB-FECHA-PROC

      *    EXEC SQL
      *        INSERT INTO TFFM (FILE_NAME, TYPE_UPDATE, FASE,
      *                          ESTADO_REPLICA, FECHA_SISTEMA,
      *                          PRIORITY)
      *        VALUES (:DB-FILE-NAME, :DB-TYPE-UPDATE, '00', 'R',
      *                :DB-FECHA-PROC, 5)
      *    END-EXEC
           IF SQL-PREP OF SQL-STMT-3 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 DB-FILE-NAME
               MOVE 'X' TO SQL-TYPE(1)
               MOVE 120 TO SQL-LEN(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 DB-TYPE-UPDATE
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 10 TO SQL-LEN(2)
               SET SQL-ADDR(3) TO ADDRESS OF
                 DB-FECHA-PROC
               MOVE 'X' TO SQL-TYPE(3)
               MOVE 10 TO SQL-LEN(3)
               MOVE 3 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-3
                                   SQLCA
               SET SQL-HCONN OF SQLCA TO NULL
           END-IF
           CALL 'OCSQLEXE' USING SQL-STMT-3
                               SQLCA
      *    EXEC SQL COMMIT END-EXEC
           CALL 'OCSQLCMT' USING SQLCA END-CALL

           DISPLAY "    + Lote Generado: " WS-FILE-NAME-NEW.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-FECHA-PROC            IN USE CHAR(10)
      *  DB-FILE-NAME             IN USE CHAR(120)
      *  DB-TYPE-UPDATE           IN USE CHAR(10)
      *  WS-MAX-LOTE-STR          IN USE CHAR(10)
      *  WS-PARAM-VARS-SQL    NOT IN USE
      *  WS-PARAM-VARS-SQL.WS-MAX-LOTE-STR NOT IN USE
      *  WS-PARAM-VARS-SQL.WS-RUTA-IN-SQL NOT IN USE
      *  WS-PARAM-VARS-SQL.WS-RUTA-UP-SQL NOT IN USE
      *  WS-RUTA-IN-SQL           IN USE CHAR(150)
      *  WS-RUTA-UP-SQL           IN USE CHAR(150)
      *  WS-TFFM-INSERT       NOT IN USE
      *  WS-TFFM-INSERT.DB-FECHA-PROC NOT IN USE
      *  WS-TFFM-INSERT.DB-FILE-NAME NOT IN USE
      *  WS-TFFM-INSERT.DB-TYPE-UPDATE NOT IN USE
      **********************************************************************

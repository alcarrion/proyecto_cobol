       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFDRMAIN.
      *==========================================================
      * LANZADOR DEL MOTOR DE PROCESAMIENTO (MODO BATCH)
      * CONEXIÓN INTEGRADA SEGÚN ESTÁNDAR BANCSMENU
      *==========================================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 2.
           05 SQL-COUNT  PIC S9(9) COMP-5 VALUE ZERO.
           05 SQL-ADDR   POINTER OCCURS 2 TIMES VALUE NULL.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 2 TIMES VALUE ZERO.
           05 SQL-TYPE   PIC X OCCURS 2 TIMES.
           05 SQL-PREC   PIC X OCCURS 2 TIMES.
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
      * Variable para la cadena de conexión (Mismo tamaño que BANCSMEN
       01  DB-CONN-STR             PIC X(100) VALUE SPACES.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

           COPY LKCIF.

       01  WS-PROGRAMAS.
           05 PGM-TFTRCT           PIC X(08) VALUE "TFTRCT".

       PROCEDURE DIVISION.
       0000-INICIO.
           DISPLAY " "
           DISPLAY "--- INICIANDO EJECUCION BATCH TRICKLE FEED ---"

      * 1. PREPARAR CADENA DE CONEXIÓN (Igual que en BANCSMENU)
           MOVE SPACES TO DB-CONN-STR
           STRING
               "DRIVER={MySQL ODBC 8.0 ANSI Driver};"
               "SERVER=localhost;"
               "DATABASE=proyecto_cobol;"
               "UID=root;PWD=tata;"
               DELIMITED BY SIZE INTO DB-CONN-STR
           END-STRING

           INSPECT DB-CONN-STR REPLACING TRAILING SPACES BY LOW-VALUES

      * 2. CONECTAR A LA BASE DE DATOS
           DISPLAY "CONECTANDO A LA BASE DE DATOS (MODO BATCH)..."
      *    EXEC SQL
      *        CONNECT TO :DB-CONN-STR
      *    END-EXEC
           MOVE 100 TO SQL-LEN(1)
           CALL 'OCSQL'    USING DB-CONN-STR
                               SQL-LEN(1)
                               SQLCA
           END-CALL

           IF SQLCODE NOT = 0
               DISPLAY " [X] ERROR: NO SE PUDO CONECTAR. SQLCODE: "
               SQLCODE
               STOP RUN
           ELSE
               DISPLAY " [OK] CONEXION EXITOSA. INICIANDO ORQUESTADOR."
           END-IF

      * 3. LLAMADA AL ORQUESTADOR (TFTRCT)
           CALL PGM-TFTRCT USING LK-DATOS-TRANSACCION

      * 4. DESCONEXIÓN FINAL
      *    EXEC SQL DISCONNECT CURRENT END-EXEC
           CALL 'OCSQLDIS' USING SQLCA END-CALL

           DISPLAY "--- PROCESO FINALIZADO - MOTOR CERRADO ---"
           DISPLAY " "
           STOP RUN.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-CONN-STR              IN USE CHAR(100)
      **********************************************************************

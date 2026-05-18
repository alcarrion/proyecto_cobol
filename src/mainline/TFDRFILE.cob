       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFDRFILE.
      *==========================================================
      * LANZADOR DEL MOTOR DE INGESTA DE ARCHIVOS (STAGE 1)
      * CONEXIÓN INTEGRADA Y TOTALMENTE LIMPIA DE CÓDIGO HUÉRFANO
      *==========================================================

       ENVIRONMENT DIVISION.
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
      * Cadena de conexión en DISPLAY puro para MySQL ODBC
       01  DB-CONN-STR         PIC X(100) VALUE SPACES.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-PROGRAMAS.
           05 PGM-TFFILE           PIC X(08) VALUE "TFFILE".

       LINKAGE SECTION.
      * Se incluye el nuevo copybook de la tubería Trickle Feed aislada
           COPY LKTF.

       PROCEDURE DIVISION.
       0000-INICIO.
           DISPLAY " "
           DISPLAY "--- INICIANDO ETAPA DE INGESTA BATCH (STAGE 1) ---"

      * 1. PREPARAR CADENA DE CONEXIÓN
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
           DISPLAY "CONECTANDO A LA BASE DE DATOS (MODO INGESTA)..."
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
               DISPLAY SQLCODE
               STOP RUN
           ELSE
               DISPLAY " [OK] CONEXION BBDD SELECCIONADA CON EXITO."
           END-IF

      * 3. LLAMADA AL MÓDULO DE INGESTA REAL (TFFILE) USANDO EL NUEVO C
           CALL PGM-TFFILE USING LK-TRICKLE-FEED-INTERFACE

      * 4. DESCONEXIÓN FINAL
      *    EXEC SQL DISCONNECT CURRENT END-EXEC
           CALL 'OCSQLDIS' USING SQLCA END-CALL

           DISPLAY "--- ETAPA DE INGESTA FINALIZADA EXITOSAMENTE ---"
           DISPLAY " "
           STOP RUN.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-CONN-STR              IN USE CHAR(100)
      **********************************************************************

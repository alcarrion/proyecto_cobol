******************************************************************
      * Author: EQUIPO (LUIS. ALISON. FRANKLIN.)
      * INTEGRACIÓN: TRICKLE FEED (FASE 00-40)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCSMENU.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FS-CONFIG-FILE ASSIGN TO "db_config.cfg"
           ORGANIZATION is LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FS-CONFIG-FILE.
       01  REG-CONFIG-LINE       PIC X(50).
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
           01 DB-CONN-STR       PIC X(100) VALUE SPACES.
      *    EXEC SQL END DECLARE SECTION END-EXEC.

           01 WS-MENU.
               05 WS-OPCION      PIC 9(2).

           01 WS-CONTINUAR       PIC X VALUE 'S'.

           01 WS-DATOS-TRANSACCIONALES.
               05 WS-COD-RETORNO PIC 9(2) VALUE 0.
               05 WS-MENSAJE     PIC X(50).

           01 WS-PROGRAMAS.
               05 PGM-CI0000     PIC X(8).
               05 PGM-IN0000     PIC X(8).
               05 PGM-TC0000     PIC X(8).
               05 PGM-BR0000     PIC X(8).
               05 PGM-BAT000     PIC X(8).
               05 PGM-RP0000     PIC X(8).
      * NUEVO: Programa Orquestador del Trickle Feed
               05 PGM-TFTRCT     PIC X(8).

           COPY LKCIF.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE 'CI0000' TO PGM-CI0000.
            MOVE 'IN0000' TO PGM-IN0000.
            MOVE 'TC0000' TO PGM-TC0000.
            MOVE 'BR0000' TO PGM-BR0000.
            MOVE 'BAT000' TO PGM-BAT000.
            MOVE 'RP0000' TO PGM-RP0000.
      * NUEVO: Inicializar nombre del orquestador
            MOVE 'TFTRCT' TO PGM-TFTRCT.

            PERFORM 8000-CARGAR-CONFIG.
            PERFORM 8100-CONECTAR-BD.
            IF SQLCODE = 0
                PERFORM 001-MENU-OPCION
            ELSE
                DISPLAY "ERROR: NO SE PUDO INICIAR EL SISTEMA."
            END-IF.

      *     EXEC SQL DISCONNECT CURRENT END-EXEC.
           CALL 'OCSQLDIS' USING SQLCA END-CALL
                                                .
            DISPLAY 'SALIENDO DEL PROGRAMA...'
            STOP RUN.

       001-MENU-OPCION.
           PERFORM UNTIL WS-CONTINUAR = 'N' OR 'n'
               DISPLAY "========================================"
               DISPLAY "   SISTEMA BANCARIO INTEGRADO v1.0"
               DISPLAY "========================================"
               DISPLAY "1. Gestion de Clientes"
               DISPLAY "2. Cuentas Corrientes"
               DISPLAY "3. Tarjetas de Credito"
               DISPLAY "4. Hipotecas"
               DISPLAY "5. Cierre Mensual"
               DISPLAY "6. Reportes Gerenciales"
               DISPLAY "7. PROCESAR TRICKLE FEED (LOTES)"
               DISPLAY "0. Salir"
               DISPLAY "========================================"
               ACCEPT WS-OPCION

           EVALUATE WS-OPCION
               WHEN 1
                   CALL PGM-CI0000 USING LK-DATOS-TRANSACCION
               WHEN 2
                   CALL PGM-IN0000 USING LK-DATOS-TRANSACCION
               WHEN 3
                   CALL PGM-TC0000 USING LK-DATOS-TRANSACCION
               WHEN 4
                   CALL PGM-BR0000 USING LK-DATOS-TRANSACCION
               WHEN 5
                   CALL PGM-BAT000 USING LK-DATOS-TRANSACCION
               WHEN 6
                   CALL PGM-RP0000 USING LK-DATOS-TRANSACCION
      * NUEVO: Llamada al proceso de Trickle Feed
               WHEN 7
                   CALL PGM-TFTRCT USING LK-DATOS-TRANSACCION
               WHEN 0
                   MOVE 'N' TO WS-CONTINUAR
               WHEN OTHER
                   DISPLAY 'Opcion Invalida. Intente de nuevo...'
           END-EVALUATE

               IF WS-OPCION NOT = 0 AND WS-CONTINUAR NOT = 'N'
                   DISPLAY '------------------------------------'
                   DISPLAY 'RESULTADO: ' LK-MENSAJE
                   DISPLAY 'CODIGO:    ' LK-COD-RETORNO
                   DISPLAY '------------------------------------'
                   DISPLAY 'Presione una tecla para continuar...'
                   ACCEPT WS-OPCION
               END-IF
           END-PERFORM.

       8000-CARGAR-CONFIG.
      * Mantiene tu logica de conexion ODBC a MySQL...
           MOVE SPACES TO DB-CONN-STR.
           STRING
               "DRIVER={MySQL ODBC 8.0 ANSI Driver};"
               "SERVER=localhost;"
               "DATABASE=proyecto_cobol;"
               "UID=root;PWD=tata;"
               DELIMITED BY SIZE INTO DB-CONN-STR
           END-STRING.
           INSPECT DB-CONN-STR REPLACING TRAILING SPACES BY LOW-VALUES.

       8100-CONECTAR-BD.
           DISPLAY "CONECTANDO A LA BASE..."
      *    EXEC SQL
      *        CONNECT TO :DB-CONN-STR
      *    END-EXEC.
           MOVE 100 TO SQL-LEN(1)
           CALL 'OCSQL'    USING DB-CONN-STR
                               SQL-LEN(1)
                               SQLCA
           END-CALL
                   .
           IF SQLCODE NOT = 0
               DISPLAY "FALLO DE CONEXION. SQLCODE: " SQLCODE
           ELSE
               DISPLAY "CONEXION EXITOSA AL SISTEMA."
           END-IF.

       END PROGRAM BANCSMENU.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-CONN-STR              IN USE CHAR(100)
      **********************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCSMENU.

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
       01  DB-CONN-STR             PIC X(100).
      *    EXEC SQL END DECLARE SECTION END-EXEC.

       01  WS-OPCION                PIC 9(02).
       01  WS-CONTINUAR             PIC X VALUE 'S'.
       01  WS-PAUSA                 PIC X.

           COPY CUSMREC.
           COPY INVMREC.
           COPY TARJREC.
           COPY BORMREC.
           COPY LKCIF.

       SCREEN SECTION.
       01  SCR-CLEAR.
           05 BLANK SCREEN.

       01  SCR-MENU.
           05 BLANK SCREEN.
           05 LINE 02 COL 02 VALUE
              "+------------------------------------------+".
           05 LINE 03 COL 02 VALUE
              "|      BANCO LAF - CORE BANCARIO v3.0      |".
           05 LINE 04 COL 02 VALUE
              "+------------------------------------------+".
           05 LINE 06 COL 04 VALUE
              "OPERADOR: ALIZBETH".
           05 LINE 07 COL 04 VALUE
              "TERMINAL: TERM01".
           05 LINE 09 COL 02 VALUE
              "+------------------------------------------+".
           05 LINE 10 COL 04 VALUE
              "1. Gestion de Clientes".
           05 LINE 11 COL 04 VALUE
              "2. Cuentas Bancarias".
           05 LINE 12 COL 04 VALUE
              "3. Tarjetas de Credito".
           05 LINE 13 COL 04 VALUE
              "4. Hipotecas             ".
           05 LINE 14 COL 04 VALUE
              "5. Cierre Mensual - pendiente".
           05 LINE 15 COL 04 VALUE
              "6. Reportes - pendiente".
           05 LINE 16 COL 04 VALUE
              "7. Procesos Batch - pendiente".
           05 LINE 17 COL 04 VALUE
              "8. Salir".
           05 LINE 19 COL 02 VALUE
              "+------------------------------------------+".
           05 LINE 21 COL 04 VALUE
              "Seleccione opcion: [  ]".
           05 SCR-OPC LINE 21 COL 25 PIC 9(02)
              USING WS-OPCION REQUIRED.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 'TERM' TO LKCIF-TERMINAL.
           MOVE 'ALIZBETH' TO LKCIF-USUARIO.
           MOVE 'N' TO LKCIF-ESTADO-MORA.

           PERFORM 8000-CARGAR-CONFIG.
           PERFORM 8100-CONECTAR-BD.

           IF SQLCODE = 0
              PERFORM 9000-BIENVENIDA
              PERFORM 1000-MENU
                 UNTIL WS-CONTINUAR = 'N'
           ELSE
              DISPLAY SCR-CLEAR
              DISPLAY "ERROR: No se pudo conectar a la BD."
              DISPLAY "SQLCODE: " SQLCODE
              DISPLAY "Presione ENTER para finalizar."
              ACCEPT WS-PAUSA
           END-IF.

      *    EXEC SQL DISCONNECT CURRENT END-EXEC.
           CALL 'OCSQLDIS' USING SQLCA END-CALL
                                               .
           DISPLAY SCR-CLEAR.
           DISPLAY "Sesion finalizada correctamente.".
           STOP RUN.

       1000-MENU.
           DISPLAY SCR-MENU.
           ACCEPT SCR-OPC.

           EVALUATE WS-OPCION
              WHEN 1
                 DISPLAY SCR-CLEAR
                 CALL 'CI0000' USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION
              WHEN 2
                 DISPLAY SCR-CLEAR
                 CALL 'IN0000' USING REG-CUSM,
                                     REG-INVM,
                                     LK-DATOS-SESION,
                                     LK-DATOS-TRANSACCION
              WHEN 3
                 DISPLAY SCR-CLEAR
                 CALL 'TC0000' USING REG-CUSM,
                                     REG-INVM,
                                     REG-TARJ,
                                     REG-DIFD,
                                     LK-DATOS-SESION,
                                     LK-DATOS-TRANSACCION
              WHEN 4
                 DISPLAY SCR-CLEAR
                 CALL 'BR0000' USING LK-DATOS-SESION,
                                     LK-DATOS-TRANSACCION
              WHEN 5
                 PERFORM 2000-PENDIENTE
              WHEN 6
                 PERFORM 2000-PENDIENTE
              WHEN 7
                 PERFORM 2000-PENDIENTE
              WHEN 8
                 MOVE 'N' TO WS-CONTINUAR
              WHEN OTHER
                 DISPLAY "Opcion no valida." LINE 23 COL 04
                 ACCEPT WS-PAUSA
           END-EVALUATE.

       2000-PENDIENTE.
           DISPLAY SCR-CLEAR.
           DISPLAY "Modulo pendiente en esta fase.".
           DISPLAY "Presione ENTER para volver al menu.".
           ACCEPT WS-PAUSA.

       8000-CARGAR-CONFIG.
           MOVE SPACES TO DB-CONN-STR.
           STRING
              "DRIVER={MySQL ODBC 8.0 ANSI Driver};"
              "SERVER=localhost;"
              "DATABASE=proyecto_cobol;"
              "UID=root;PWD=12345;"
              DELIMITED BY SIZE
              INTO DB-CONN-STR
           END-STRING.
           INSPECT DB-CONN-STR
              REPLACING TRAILING SPACES BY LOW-VALUES.

       8100-CONECTAR-BD.
      *    EXEC SQL
      *       CONNECT TO :DB-CONN-STR
      *    END-EXEC.
           MOVE 100 TO SQL-LEN(1)
           CALL 'OCSQL'    USING DB-CONN-STR
                               SQL-LEN(1)
                               SQLCA
           END-CALL
                   .

       9000-BIENVENIDA.
           DISPLAY SCR-CLEAR.
           DISPLAY " ".
           DISPLAY "+------------------------------------------+".
           DISPLAY "|      BANCO LAF - CORE BANCARIO          |".
           DISPLAY "+------------------------------------------+".
           DISPLAY "| Conexion SQL establecida correctamente  |".
           DISPLAY "+------------------------------------------+".
           DISPLAY " ".
           DISPLAY "Presione ENTER para ingresar al menu.".
           ACCEPT WS-PAUSA.

       END PROGRAM BANCSMENU.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-CONN-STR              IN USE CHAR(100)
      **********************************************************************

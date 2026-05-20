      ******************************************************************
      * BANCSMENU.SQB - MENU PRINCIPAL DEL SISTEMA BANCARIO
      * v4.0 - Layout con cajas y colores + opcion de 1 digito
      * AUTORES: EQUIPO (LUIS. ALISON. FRANKLIN.)
      ******************************************************************
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

       01  WS-OPCION               PIC 9(01) VALUE ZERO.
       01  WS-CONTINUAR            PIC X     VALUE 'S'.
       01  WS-PAUSA                PIC X.

           COPY CUSMREC.
           COPY INVMREC.
           COPY TARJREC.
           COPY BORMREC.
           COPY LKCIF.

       SCREEN SECTION.
      *> Colores: 0=Negro 1=Azul 2=Verde 3=Cyan
      *>          4=Rojo 5=Magenta 6=Amarillo 7=Blanco

      *> ----- Cabecera general -----
       01  SCR-MARCO.
           05 BLANK SCREEN BACKGROUND-COLOR 0.
           05 LINE 01 COL 01 BACKGROUND-COLOR 1 FOREGROUND-COLOR 7
              HIGHLIGHT VALUE
              "    BANCO LAF - SISTEMA BANCARIO INTEGRAL v4.0  ".
           05 LINE 02 COL 02 FOREGROUND-COLOR 3 VALUE "Operador: ".
           05 LINE 02 COL 12 PIC X(08) FROM LKCIF-USUARIO
              FOREGROUND-COLOR 7 HIGHLIGHT.
           05 LINE 02 COL 35 FOREGROUND-COLOR 3 VALUE "Terminal: ".
           05 LINE 02 COL 45 PIC X(04) FROM LKCIF-TERMINAL
              FOREGROUND-COLOR 7 HIGHLIGHT.
           05 LINE 03 COL 02 FOREGROUND-COLOR 3 VALUE
              "=================================================".

      *> ----- Menu principal -----
       01  SCR-MENU.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------- [ MENU PRINCIPAL ] ---------------+".
           05 LINE 06 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 06 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[1]".
           05 LINE 07 COL 09 FOREGROUND-COLOR 7 VALUE
              "Gestion de Clientes".
           05 LINE 07 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[2]".
           05 LINE 08 COL 09 FOREGROUND-COLOR 7 VALUE
              "Cuentas Bancarias".
           05 LINE 08 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[3]".
           05 LINE 09 COL 09 FOREGROUND-COLOR 7 VALUE
              "Tarjetas de Credito".
           05 LINE 09 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[4]".
           05 LINE 10 COL 09 FOREGROUND-COLOR 7 VALUE
              "Hipotecas".
           05 LINE 10 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[5]".
           05 LINE 12 COL 09 FOREGROUND-COLOR 7 VALUE
              "Cierre Mensual (Batch)".
           05 LINE 12 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[6]".
           05 LINE 13 COL 09 FOREGROUND-COLOR 7 VALUE
              "Reportes Gerenciales".
           05 LINE 13 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 15 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 15 COL 05 FOREGROUND-COLOR 4 HIGHLIGHT VALUE "[8]".
           05 LINE 15 COL 09 FOREGROUND-COLOR 3 VALUE
              "Salir del sistema".
           05 LINE 15 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 16 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".
           05 LINE 18 COL 02 FOREGROUND-COLOR 3 VALUE "Opcion: [ ]".
           05 SCR-MENU-OPC LINE 18 COL 11 PIC 9
              USING WS-OPCION REQUIRED
              FOREGROUND-COLOR 7 HIGHLIGHT.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 'TERM'     TO LKCIF-TERMINAL.
           MOVE 'ALIZBETH' TO LKCIF-USUARIO.
           MOVE 'N'        TO LKCIF-ESTADO-MORA.

           PERFORM 8000-CARGAR-CONFIG.
           PERFORM 8100-CONECTAR-BD.

           IF SQLCODE = 0
               PERFORM 9000-BIENVENIDA
               PERFORM 1000-MENU
                   UNTIL WS-CONTINUAR = 'N'
           ELSE
               DISPLAY SCR-MARCO
               DISPLAY "+-------- [ ERROR DE CONEXION ] ----------+"
                  LINE 05 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "No se pudo conectar a la base de datos."
                  LINE 07 COL 04 FOREGROUND-COLOR 4
               DISPLAY "Verifique el servidor MySQL y reintente."
                  LINE 08 COL 04 FOREGROUND-COLOR 4
               DISPLAY "Presione ENTER para salir..."
                  LINE 10 COL 04
               ACCEPT WS-PAUSA
           END-IF.

      *    EXEC SQL DISCONNECT CURRENT END-EXEC.
           CALL 'OCSQLDIS' USING SQLCA END-CALL
                                               .
           DISPLAY SCR-MARCO.
           DISPLAY "[OK] Sesion finalizada. Hasta pronto."
              LINE 10 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           ACCEPT WS-PAUSA LINE 12 COL 02.
           STOP RUN.

      ******************************************************************
      * 1000 - MENU PRINCIPAL
      ******************************************************************
       1000-MENU.
           MOVE 0 TO WS-OPCION.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-MENU.
           ACCEPT SCR-MENU-OPC.

           EVALUATE WS-OPCION
               WHEN 1
                   CALL 'CI0000' USING REG-CUSM,
                                       LK-DATOS-SESION,
                                       LK-DATOS-TRANSACCION
               WHEN 2
                   CALL 'IN0000' USING REG-CUSM,
                                       REG-INVM,
                                       LK-DATOS-SESION,
                                       LK-DATOS-TRANSACCION
               WHEN 3
                   CALL 'TC0000' USING REG-CUSM,
                                       REG-INVM,
                                       REG-TARJ,
                                       REG-DIFD,
                                       LK-DATOS-SESION,
                                       LK-DATOS-TRANSACCION
               WHEN 4
                   CALL 'BR0000' USING LK-DATOS-SESION,
                                       LK-DATOS-TRANSACCION
               WHEN 5
                   CALL 'BAT000' USING LK-DATOS-TRANSACCION
               WHEN 6
                   CALL 'RP0000' USING LK-DATOS-TRANSACCION
               WHEN 7
                   PERFORM 2000-PENDIENTE
               WHEN 8
                   MOVE 'N' TO WS-CONTINUAR
               WHEN OTHER
                   DISPLAY "[!] Opcion no valida (1-6 o 8)."
                      LINE 20 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                   ACCEPT WS-PAUSA LINE 21 COL 02
           END-EVALUATE.

      ******************************************************************
      * 2000 - MODULO PENDIENTE (opcion 7 - no visible en menu)
      ******************************************************************
       2000-PENDIENTE.
           DISPLAY SCR-MARCO.
           DISPLAY "[i] Este modulo esta pendiente en esta fase."
              LINE 10 COL 02 FOREGROUND-COLOR 5 HIGHLIGHT.
           DISPLAY "Presione ENTER para volver al menu..."
              LINE 12 COL 02.
           ACCEPT WS-PAUSA LINE 12 COL 41.

      ******************************************************************
      * 8000 - CARGAR CONFIGURACION DE CONEXION
      ******************************************************************
       8000-CARGAR-CONFIG.
           MOVE SPACES TO DB-CONN-STR.
           STRING
               "DRIVER={MySQL ODBC 8.0 ANSI Driver};"
               "SERVER=localhost;"
               "DATABASE=proyecto_cobol;"
               "UID=root;PWD=tata;"
               DELIMITED BY SIZE
               INTO DB-CONN-STR
           END-STRING.
           INSPECT DB-CONN-STR
               REPLACING TRAILING SPACES BY LOW-VALUES.

      ******************************************************************
      * 8100 - CONECTAR A BASE DE DATOS
      ******************************************************************
       8100-CONECTAR-BD.
      *    EXEC SQL
      *        CONNECT TO :DB-CONN-STR
      *    END-EXEC.
           MOVE 100 TO SQL-LEN(1)
           CALL 'OCSQL'    USING DB-CONN-STR
                               SQL-LEN(1)
                               SQLCA
           END-CALL
                   .

      ******************************************************************
      * 9000 - PANTALLA DE BIENVENIDA
      ******************************************************************
       9000-BIENVENIDA.
           DISPLAY SCR-MARCO.
           DISPLAY "+------ [ BANCO LAF - BIENVENIDO ] ---------+"
              LINE 07 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 08 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 08 COL 46 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "Conexion a base de datos: OK"
              LINE 08 COL 04 FOREGROUND-COLOR 2.
           DISPLAY "|" LINE 09 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 09 COL 46 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "Sistema Bancario Integral listo."
              LINE 09 COL 04 FOREGROUND-COLOR 7.
           DISPLAY "|" LINE 10 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 10 COL 46 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "+-------------------------------------------+"
              LINE 11 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "Presione ENTER para ingresar al menu..."
              LINE 13 COL 02.
           ACCEPT WS-PAUSA LINE 13 COL 41.

       END PROGRAM BANCSMENU.
      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCOBOL Version 3 (2024.04.30) Build May 10 2024

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  DB-CONN-STR              IN USE CHAR(100)
      **********************************************************************

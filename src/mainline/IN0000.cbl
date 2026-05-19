      ******************************************************************
      * IN0000.CBL - MODULO DE CUENTAS BANCARIAS
      * v3.3 - Layout cajas + ceros a la izq + UX de reintentos
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IN0000.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPCION-INVM           PIC X(01) VALUE SPACES.
       01  WS-CONTINUAR-INVM        PIC X(01) VALUE 'S'.
       01  WS-CONFIRMAR             PIC X(01).

       01  WS-PROGRAMAS.
           05 WS-PGM-DBIOINVM       PIC X(08) VALUE 'DBIOINVM'.
           05 WS-PGM-DBIOCUSM       PIC X(08) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOTRAN       PIC X(08) VALUE 'DBIOTRAN'.

       01  WS-ACCION-TRANS          PIC X(01).

       01  WS-DOC-ENTRADA           PIC X(12).
       01  WS-DOC-LEN               PIC 9(02).
       01  WS-I                     PIC 9(02).

       01  WS-TIPO-INGRESADO        PIC X(01).
       01  WS-ID-SELEC-ENTRADA      PIC X(08).
       01  WS-ID-SELEC              PIC 9(08).
       01  WS-IDX                   PIC 9(02).
       01  WS-LIN                   PIC 9(02).
       01  WS-TOTAL-REAL            PIC 9(02) VALUE 0.
       01  WS-SALDO-EDIT            PIC $ZZ,ZZZ,ZZ9.99.
       01  WS-BLANK-LINE            PIC X(55) VALUE SPACES.

       01  WS-MONTO-ENTRADA         PIC X(12).
       01  WS-MONTO-TX              PIC S9(13)V99 COMP-3.
       01  WS-CUENTA-FMT            PIC 9(08).

       01  WS-VALIDACIONES.
           05 WS-DOC-OK             PIC X VALUE 'N'.
           05 WS-CANCELO            PIC X VALUE 'N'.
           05 WS-TIPO-OK            PIC X VALUE 'N'.
           05 WS-MONTO-OK           PIC X VALUE 'N'.
           05 WS-CTA-OK             PIC X VALUE 'N'.

       01  WS-CUENTAS-TABLA.
           05 WS-CTA OCCURS 5 TIMES.
               10 WS-CTA-ID-CUENTA  PIC 9(08).
               10 WS-CTA-TIPO       PIC X(01).
               10 WS-CTA-SALDO      PIC S9(13)V99 COMP-3.
               10 WS-CTA-FECHA      PIC X(10).
               10 WS-CTA-ESTADO     PIC X(01).

       01  WS-MOD10.
           05 WS-MOD10-OK           PIC X     VALUE 'N'.
           05 WS-DIGITO             PIC 9(01).
           05 WS-RESULTADO          PIC 9(02).
           05 WS-SUMA-MOD10         PIC 9(04) VALUE 0.
           05 WS-DIGITO-VERIF       PIC 9(01).
           05 WS-DIGITO-CALC        PIC 9(01).
           05 WS-K                  PIC 9(02).
           05 WS-PROV-DOC           PIC 9(02).

       LINKAGE SECTION.
           COPY CUSMREC.
           COPY INVMREC.
           COPY LKCIF.

       SCREEN SECTION.
      *> ----- Cabecera reutilizable: barra de titulo + operador -----
       01  SCR-MARCO.
           05 BLANK SCREEN BACKGROUND-COLOR 0.
           05 LINE 01 COL 01 BACKGROUND-COLOR 1 FOREGROUND-COLOR 7
              HIGHLIGHT VALUE 
              "       BANCO LAF - MODULO DE CUENTAS v3.3       ".
           05 LINE 02 COL 02 FOREGROUND-COLOR 3 VALUE "Operador: ".
           05 LINE 02 COL 12 PIC X(08) FROM LKCIF-USUARIO
              FOREGROUND-COLOR 7 HIGHLIGHT.
           05 LINE 02 COL 35 FOREGROUND-COLOR 3 VALUE "Terminal: ".
           05 LINE 02 COL 45 PIC X(04) FROM LKCIF-TERMINAL
              FOREGROUND-COLOR 7 HIGHLIGHT.
           05 LINE 03 COL 02 FOREGROUND-COLOR 3 VALUE
              "=================================================".

      *> ----- Menu de cuentas -----
       01  SCR-MENU-INVM.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------- [ MODULO DE CUENTAS ] ------------+".
           05 LINE 06 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 06 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[1]".
           05 LINE 07 COL 09 FOREGROUND-COLOR 7 VALUE
              "Abrir cuenta corriente".
           05 LINE 07 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[2]".
           05 LINE 08 COL 09 FOREGROUND-COLOR 7 VALUE
              "Consultar mis cuentas".
           05 LINE 08 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[3]".
           05 LINE 09 COL 09 FOREGROUND-COLOR 7 VALUE
              "Depositar en cuenta".
           05 LINE 09 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[4]".
           05 LINE 10 COL 09 FOREGROUND-COLOR 7 VALUE
              "Retirar de cuenta".
           05 LINE 10 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[5]".
           05 LINE 11 COL 09 FOREGROUND-COLOR 7 VALUE
              "Cerrar una cuenta".
           05 LINE 11 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[6]".
           05 LINE 12 COL 09 FOREGROUND-COLOR 3 VALUE
              "Volver al menu principal".
           05 LINE 12 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".
           05 LINE 15 COL 02 FOREGROUND-COLOR 3 VALUE "Opcion: [ ]".
           05 SCR-INVM-OPC LINE 15 COL 11 PIC X
              USING WS-OPCION-INVM FOREGROUND-COLOR 7 HIGHLIGHT.

      *> ----- Caja para buscar cliente -----
       01  SCR-DOC-BOX.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------- [ OPERACION EN CUENTA ] ----------+".
           05 LINE 06 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 06 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 04 FOREGROUND-COLOR 6 VALUE 
              "Documento (X=salir):".
           05 LINE 07 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".

       01  SCR-LIMPIAR.
           05 BLANK SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.

       PROCEDURE DIVISION USING REG-CUSM,
                                REG-INVM,
                                LK-DATOS-SESION,
                                LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 'S' TO WS-CONTINUAR-INVM.
           PERFORM 1000-MENU-INVM
              UNTIL WS-CONTINUAR-INVM = 'N'.
           DISPLAY SCR-LIMPIAR.
           GOBACK.

      ************************************************************
      * MENU PRINCIPAL DE CUENTAS
      ************************************************************
       1000-MENU-INVM.
           MOVE SPACES TO WS-OPCION-INVM.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-MENU-INVM.
           ACCEPT WS-OPCION-INVM LINE 15 COL 11.

           EVALUATE WS-OPCION-INVM
               WHEN '1' PERFORM 2000-ABRIR-CUENTA
               WHEN '2' PERFORM 3000-CONSULTAR-CUENTAS
               WHEN '3' PERFORM 4000-DEPOSITAR
               WHEN '4' PERFORM 5000-RETIRAR
               WHEN '5' PERFORM 6000-CERRAR-CUENTA
               WHEN '6' 
               WHEN '0'
               WHEN 'X'
               WHEN 'x'
                   MOVE 'N' TO WS-CONTINUAR-INVM
               WHEN OTHER
                   DISPLAY "Opcion no valida." LINE 17 COL 02
                      FOREGROUND-COLOR 4
                   DISPLAY "Presione ENTER..." LINE 18 COL 02
                   ACCEPT WS-CONFIRMAR LINE 18 COL 20
           END-EVALUATE.

      ************************************************************
      * OPCION 1 - ABRIR CUENTA NUEVA (solo C)
      ************************************************************
       2000-ABRIR-CUENTA.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-DOC-BOX.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "[!] Cliente inactivo. No se puede operar."
                  LINE 20 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               PERFORM 9910-PAUSA
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9200-CAPTURAR-TIPO-CUENTA.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           MOVE WS-TIPO-INGRESADO TO INVM-TIPO-CUENTA.
           MOVE CUSM-ID-CLIENTE   TO INVM-ID-CLIENTE.
           MOVE ZEROS             TO INVM-SALDO-ACTUAL.
           MOVE 'A'               TO INVM-ESTADO-CUENTA.

           MOVE 'A' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               MOVE INVM-ID-CUENTA TO WS-CUENTA-FMT
               DISPLAY "[OK] Cuenta creada exitosamente." 
                  LINE 20 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT
               DISPLAY "     Numero: " LINE 21 COL 02
                  FOREGROUND-COLOR 7
               DISPLAY WS-CUENTA-FMT LINE 21 COL 15
                  FOREGROUND-COLOR 6 HIGHLIGHT
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               IF LK-ERROR-DUPLICADO
                   DISPLAY "[!] Ya tiene una cuenta de este tipo."
                      LINE 20 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               ELSE
                   DISPLAY "[X] Error al crear: " LINE 20 COL 02
                      FOREGROUND-COLOR 4 HIGHLIGHT
                   DISPLAY LK-MENSAJE LINE 21 COL 02 FOREGROUND-COLOR 4
               END-IF
           END-IF.

           PERFORM 9910-PAUSA.

      ************************************************************
      * OPCION 2 - CONSULTAR CUENTAS
      ************************************************************
       3000-CONSULTAR-CUENTAS.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-DOC-BOX.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           PERFORM 9300-CARGAR-CUENTAS.

           IF WS-TOTAL-REAL = 0
               DISPLAY "[!] Este cliente no tiene cuentas."
                  LINE 18 COL 02 FOREGROUND-COLOR 6
               PERFORM 9910-PAUSA
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9400-MOSTRAR-CUENTAS.
           PERFORM 9910-PAUSA.

      ************************************************************
      * OPCION 3 - DEPOSITAR EN CUENTA
      ************************************************************
       4000-DEPOSITAR.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-DOC-BOX.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "[!] Cliente inactivo." LINE 18 COL 02
                  FOREGROUND-COLOR 4
               PERFORM 9910-PAUSA
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9300-CARGAR-CUENTAS.
           IF WS-TOTAL-REAL = 0
               DISPLAY "[!] No hay cuentas disponibles." LINE 18 COL 02
                  FOREGROUND-COLOR 6
               PERFORM 9910-PAUSA
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9400-MOSTRAR-CUENTAS.

      *> --- LOOP DE SELECCION DE CUENTA ---
           MOVE 'N' TO WS-CTA-OK.
           PERFORM UNTIL WS-CTA-OK = 'S' OR WS-CANCELO = 'S'
               DISPLAY "Numero de cuenta (X=salir):" LINE 20 COL 02
                  FOREGROUND-COLOR 6
               DISPLAY "        " LINE 20 COL 30
               MOVE SPACES TO WS-ID-SELEC-ENTRADA
               ACCEPT WS-ID-SELEC-ENTRADA LINE 20 COL 30

               IF WS-ID-SELEC-ENTRADA(1:1) = "X" 
                 OR WS-ID-SELEC-ENTRADA(1:1) = "x"
                  PERFORM 9900-AVISO-CANCELO
                  MOVE 'S' TO WS-CANCELO
               ELSE
                  IF WS-ID-SELEC-ENTRADA = SPACES
                     MOVE 0 TO WS-ID-SELEC
                  ELSE
                     COMPUTE WS-ID-SELEC = 
                        FUNCTION NUMVAL(WS-ID-SELEC-ENTRADA)
                  END-IF

                  PERFORM 9500-BUSCAR-EN-TABLA
                  IF WS-IDX = 0
                      DISPLAY "[!] Cuenta no encontrada en su lista." 
                         LINE 22 COL 02 FOREGROUND-COLOR 4
                      PERFORM 9910-PAUSA
                      DISPLAY WS-BLANK-LINE LINE 22 COL 02
                      DISPLAY WS-BLANK-LINE LINE 24 COL 02
                  ELSE IF WS-CTA-ESTADO(WS-IDX) NOT = 'A'
                      DISPLAY "[!] Cuenta no activa." LINE 22 COL 02 
                         FOREGROUND-COLOR 4
                      PERFORM 9910-PAUSA
                      DISPLAY WS-BLANK-LINE LINE 22 COL 02
                      DISPLAY WS-BLANK-LINE LINE 24 COL 02
                  ELSE
                      MOVE 'S' TO WS-CTA-OK
                  END-IF
               END-IF
           END-PERFORM.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           PERFORM 9600-CAPTURAR-MONTO.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           MOVE WS-CTA-ID-CUENTA(WS-IDX) TO INVM-ID-CUENTA.
           MOVE CUSM-ID-CLIENTE          TO INVM-ID-CLIENTE.
           MOVE WS-MONTO-TX              TO INVM-SALDO-ACTUAL.

           MOVE 'D' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               MOVE INVM-SALDO-ACTUAL TO WS-SALDO-EDIT
               DISPLAY "[OK] Deposito exitoso. Saldo actual: " 
                  LINE 23 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT
               DISPLAY WS-SALDO-EDIT LINE 23 COL 39
                  FOREGROUND-COLOR 6 HIGHLIGHT
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY "[X] Error: " LK-MENSAJE LINE 23 COL 02
                  FOREGROUND-COLOR 4
           END-IF.

           PERFORM 9910-PAUSA.

      ************************************************************
      * OPCION 4 - RETIRAR DE CUENTA
      ************************************************************
       5000-RETIRAR.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-DOC-BOX.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "[!] Cliente inactivo." LINE 18 COL 02
                  FOREGROUND-COLOR 4
               PERFORM 9910-PAUSA
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9300-CARGAR-CUENTAS.
           IF WS-TOTAL-REAL = 0
               DISPLAY "[!] No hay cuentas disponibles." LINE 18 COL 02
                  FOREGROUND-COLOR 6
               PERFORM 9910-PAUSA
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9400-MOSTRAR-CUENTAS.

      *> --- LOOP DE SELECCION DE CUENTA ---
           MOVE 'N' TO WS-CTA-OK.
           PERFORM UNTIL WS-CTA-OK = 'S' OR WS-CANCELO = 'S'
               DISPLAY "Numero de cuenta (X=salir):" LINE 20 COL 02
                  FOREGROUND-COLOR 6
               DISPLAY "        " LINE 20 COL 30
               MOVE SPACES TO WS-ID-SELEC-ENTRADA
               ACCEPT WS-ID-SELEC-ENTRADA LINE 20 COL 30

               IF WS-ID-SELEC-ENTRADA(1:1) = "X" 
                 OR WS-ID-SELEC-ENTRADA(1:1) = "x"
                  PERFORM 9900-AVISO-CANCELO
                  MOVE 'S' TO WS-CANCELO
               ELSE
                  IF WS-ID-SELEC-ENTRADA = SPACES
                     MOVE 0 TO WS-ID-SELEC
                  ELSE
                     COMPUTE WS-ID-SELEC = 
                        FUNCTION NUMVAL(WS-ID-SELEC-ENTRADA)
                  END-IF

                  PERFORM 9500-BUSCAR-EN-TABLA
                  IF WS-IDX = 0
                      DISPLAY "[!] Cuenta no encontrada en su lista." 
                         LINE 22 COL 02 FOREGROUND-COLOR 4
                      PERFORM 9910-PAUSA
                      DISPLAY WS-BLANK-LINE LINE 22 COL 02
                      DISPLAY WS-BLANK-LINE LINE 24 COL 02
                  ELSE IF WS-CTA-ESTADO(WS-IDX) NOT = 'A'
                      DISPLAY "[!] Cuenta no activa." LINE 22 COL 02 
                         FOREGROUND-COLOR 4
                      PERFORM 9910-PAUSA
                      DISPLAY WS-BLANK-LINE LINE 22 COL 02
                      DISPLAY WS-BLANK-LINE LINE 24 COL 02
                  ELSE IF WS-CTA-TIPO(WS-IDX) = 'H'
                      DISPLAY "[!] Retiros no valen en cta hipoteca."
                         LINE 22 COL 02 FOREGROUND-COLOR 4
                      PERFORM 9910-PAUSA
                      DISPLAY WS-BLANK-LINE LINE 22 COL 02
                      DISPLAY WS-BLANK-LINE LINE 24 COL 02
                  ELSE
                      MOVE 'S' TO WS-CTA-OK
                  END-IF
               END-IF
           END-PERFORM.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           MOVE WS-CTA-SALDO(WS-IDX) TO WS-SALDO-EDIT.
           DISPLAY "Saldo disponible: " LINE 21 COL 02
              FOREGROUND-COLOR 7.
           DISPLAY WS-SALDO-EDIT LINE 21 COL 20 
              FOREGROUND-COLOR 6 HIGHLIGHT.

           PERFORM 9600-CAPTURAR-MONTO.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           MOVE WS-CTA-ID-CUENTA(WS-IDX) TO INVM-ID-CUENTA.
           MOVE CUSM-ID-CLIENTE          TO INVM-ID-CLIENTE.
           MOVE WS-MONTO-TX              TO INVM-SALDO-ACTUAL.

           MOVE 'R' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               MOVE INVM-SALDO-ACTUAL TO WS-SALDO-EDIT
               DISPLAY "[OK] Retiro exitoso. Saldo actual: " 
                  LINE 23 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT
               DISPLAY WS-SALDO-EDIT LINE 23 COL 37
                  FOREGROUND-COLOR 6 HIGHLIGHT
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY "[X] Error: " LK-MENSAJE LINE 23 COL 02
                  FOREGROUND-COLOR 4
           END-IF.

           PERFORM 9910-PAUSA.

      ************************************************************
      * OPCION 5 - CERRAR UNA CUENTA
      ************************************************************
       6000-CERRAR-CUENTA.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-DOC-BOX.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "[!] Cliente inactivo." LINE 18 COL 02
                  FOREGROUND-COLOR 4
               PERFORM 9910-PAUSA
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9300-CARGAR-CUENTAS.
           IF WS-TOTAL-REAL = 0
               DISPLAY "[!] No hay cuentas para cerrar." LINE 18 COL 02
                  FOREGROUND-COLOR 6
               PERFORM 9910-PAUSA
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9400-MOSTRAR-CUENTAS.

      *> --- LOOP DE SELECCION DE CUENTA ---
           MOVE 'N' TO WS-CTA-OK.
           PERFORM UNTIL WS-CTA-OK = 'S' OR WS-CANCELO = 'S'
               DISPLAY "Cuenta a cerrar (X=salir): " LINE 20 COL 02
                  FOREGROUND-COLOR 6
               DISPLAY "        " LINE 20 COL 30
               MOVE SPACES TO WS-ID-SELEC-ENTRADA
               ACCEPT WS-ID-SELEC-ENTRADA LINE 20 COL 30

               IF WS-ID-SELEC-ENTRADA(1:1) = "X" 
                 OR WS-ID-SELEC-ENTRADA(1:1) = "x"
                  PERFORM 9900-AVISO-CANCELO
                  MOVE 'S' TO WS-CANCELO
               ELSE
                  IF WS-ID-SELEC-ENTRADA = SPACES
                     MOVE 0 TO WS-ID-SELEC
                  ELSE
                     COMPUTE WS-ID-SELEC = 
                        FUNCTION NUMVAL(WS-ID-SELEC-ENTRADA)
                  END-IF

                  PERFORM 9500-BUSCAR-EN-TABLA
                  IF WS-IDX = 0
                      DISPLAY "[!] Cuenta no encontrada." LINE 22 COL 02
                         FOREGROUND-COLOR 4
                      PERFORM 9910-PAUSA
                      DISPLAY WS-BLANK-LINE LINE 22 COL 02
                      DISPLAY WS-BLANK-LINE LINE 24 COL 02
                  ELSE IF WS-CTA-ESTADO(WS-IDX) = 'C'
                      DISPLAY "[!] Esa cuenta ya esta cerrada." 
                         LINE 22 COL 02 FOREGROUND-COLOR 4
                      PERFORM 9910-PAUSA
                      DISPLAY WS-BLANK-LINE LINE 22 COL 02
                      DISPLAY WS-BLANK-LINE LINE 24 COL 02
                  ELSE IF WS-CTA-SALDO(WS-IDX) > ZEROS
                      MOVE WS-CTA-SALDO(WS-IDX) TO WS-SALDO-EDIT
                      DISPLAY "Retire saldo antes: " LINE 22 COL 02
                         FOREGROUND-COLOR 4
                      DISPLAY WS-SALDO-EDIT LINE 22 COL 22
                         FOREGROUND-COLOR 4
                      PERFORM 9910-PAUSA
                      DISPLAY WS-BLANK-LINE LINE 22 COL 02
                      DISPLAY WS-BLANK-LINE LINE 24 COL 02
                  ELSE
                      MOVE 'S' TO WS-CTA-OK
                  END-IF
               END-IF
           END-PERFORM.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           DISPLAY "Confirmar cierre (S/N): " LINE 22 COL 02
              FOREGROUND-COLOR 6 HIGHLIGHT.
           ACCEPT WS-CONFIRMAR LINE 22 COL 26.

           IF WS-CONFIRMAR NOT = 'S' AND WS-CONFIRMAR NOT = 's'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-CTA-ID-CUENTA(WS-IDX) TO INVM-ID-CUENTA.
           MOVE CUSM-ID-CLIENTE          TO INVM-ID-CLIENTE.

           MOVE 'B' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY "[OK] Cuenta cerrada correctamente." 
                  LINE 23 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY "[X] Error: " LK-MENSAJE LINE 23 COL 02
                  FOREGROUND-COLOR 4
           END-IF.

           PERFORM 9910-PAUSA.

      ************************************************************
      * 9100 - Buscar cliente y validar documento (CON LOOP)
      ************************************************************
       9100-BUSCAR-CLIENTE.
           MOVE 'N' TO WS-DOC-OK.

           PERFORM UNTIL WS-DOC-OK = 'S' OR WS-CANCELO = 'S'
              DISPLAY "                                " LINE 07 COL 25
              MOVE SPACES TO WS-DOC-ENTRADA
              ACCEPT WS-DOC-ENTRADA LINE 07 COL 25
              
              EVALUATE TRUE
                 WHEN WS-DOC-ENTRADA = SPACES
                    PERFORM 9900-AVISO-CANCELO
                    MOVE 'S' TO WS-CANCELO
                 WHEN WS-DOC-ENTRADA = "X" OR WS-DOC-ENTRADA = "x"
                    PERFORM 9900-AVISO-CANCELO
                    MOVE 'S' TO WS-CANCELO
                 WHEN OTHER
                    PERFORM 9150-CLASIFICAR-Y-BUSCAR
              END-EVALUATE
           END-PERFORM.

      ************************************************************
      * 9150 - Clasificar doc y buscar en DB
      ************************************************************
       9150-CLASIFICAR-Y-BUSCAR.
           MOVE 12 TO WS-I.
           PERFORM UNTIL WS-I = 0 
                      OR WS-DOC-ENTRADA(WS-I:1) NOT = SPACE
              SUBTRACT 1 FROM WS-I
           END-PERFORM.
           MOVE WS-I TO WS-DOC-LEN.

           IF WS-DOC-LEN = 10
              IF WS-DOC-ENTRADA(1:10) IS NUMERIC
                 PERFORM 9800-VALIDAR-MODULO-10
                 IF WS-MOD10-OK = 'S'
                    MOVE 'CED' TO CUSM-TIPO-DOC
                 ELSE
                    DISPLAY "[!] Cedula invalida (mod 10)." 
                       LINE 11 COL 02 FOREGROUND-COLOR 4
                    PERFORM 9910-PAUSA
                    DISPLAY WS-BLANK-LINE LINE 11 COL 02
                    DISPLAY WS-BLANK-LINE LINE 24 COL 02
                    EXIT PARAGRAPH
                 END-IF
              ELSE
                 MOVE 'PAS' TO CUSM-TIPO-DOC
              END-IF
           ELSE IF WS-DOC-LEN >= 6 AND WS-DOC-LEN <= 12
              MOVE 'PAS' TO CUSM-TIPO-DOC
           ELSE
              DISPLAY "[!] Longitud invalida (6-12)." LINE 11 COL 02 
                 FOREGROUND-COLOR 4
              PERFORM 9910-PAUSA
              DISPLAY WS-BLANK-LINE LINE 11 COL 02
              DISPLAY WS-BLANK-LINE LINE 24 COL 02
              EXIT PARAGRAPH
           END-IF.

           MOVE WS-DOC-ENTRADA TO CUSM-DOC-CLIENTE.
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-SESION, 
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = '00  '
              DISPLAY "Cliente: " LINE 11 COL 02 FOREGROUND-COLOR 3
              DISPLAY CUSM-NOMBRE-CLIENTE LINE 11 COL 12 
                 FOREGROUND-COLOR 7 HIGHLIGHT
              DISPLAY CUSM-APELLIDOS-CLIENTE LINE 11 COL 38 
                 FOREGROUND-COLOR 7 HIGHLIGHT
              MOVE 'S' TO WS-DOC-OK
           ELSE
              DISPLAY "[X] Cliente no encontrado en BD." LINE 11 COL 02
                 FOREGROUND-COLOR 4 HIGHLIGHT
              PERFORM 9910-PAUSA
              DISPLAY WS-BLANK-LINE LINE 11 COL 02
              DISPLAY WS-BLANK-LINE LINE 24 COL 02
           END-IF.

      ************************************************************
      * 9200 - Capturar tipo de cuenta (solo C) CON LOOP
      ************************************************************
       9200-CAPTURAR-TIPO-CUENTA.
           MOVE 'N' TO WS-TIPO-OK.
           PERFORM UNTIL WS-TIPO-OK = 'S' OR WS-CANCELO = 'S'
               MOVE SPACES TO WS-TIPO-INGRESADO
               DISPLAY "Tipo de cuenta:" LINE 12 COL 05
               DISPLAY "  C = Corriente (Ahorros se abre en Alta)"
                  LINE 13 COL 05
               DISPLAY "Ingrese tipo (ENTER=cancelar): "
                  LINE 14 COL 05
               DISPLAY "   " LINE 14 COL 36
               ACCEPT WS-TIPO-INGRESADO LINE 14 COL 36

               EVALUATE TRUE
                  WHEN WS-TIPO-INGRESADO = SPACES
                     PERFORM 9900-AVISO-CANCELO
                     MOVE 'S' TO WS-CANCELO
                  WHEN WS-TIPO-INGRESADO = "X" 
                    OR WS-TIPO-INGRESADO = "x"
                     PERFORM 9900-AVISO-CANCELO
                     MOVE 'S' TO WS-CANCELO
                  WHEN WS-TIPO-INGRESADO = "C" 
                    OR WS-TIPO-INGRESADO = "c"
                     MOVE "C" TO WS-TIPO-INGRESADO
                     MOVE 'S' TO WS-TIPO-OK
                  WHEN WS-TIPO-INGRESADO = "A" 
                    OR WS-TIPO-INGRESADO = "a"
                     DISPLAY "[!] Ahorros se crea en Alta Cliente"
                        LINE 19 COL 02 FOREGROUND-COLOR 4
                     PERFORM 9910-PAUSA
                     DISPLAY WS-BLANK-LINE LINE 19 COL 02
                     DISPLAY WS-BLANK-LINE LINE 24 COL 02
                  WHEN WS-TIPO-INGRESADO = "H" 
                    OR WS-TIPO-INGRESADO = "h"
                     DISPLAY "[!] Cuentas H se crean en Hipotecas"
                        LINE 19 COL 02 FOREGROUND-COLOR 4
                     PERFORM 9910-PAUSA
                     DISPLAY WS-BLANK-LINE LINE 19 COL 02
                     DISPLAY WS-BLANK-LINE LINE 24 COL 02
                  WHEN OTHER
                     DISPLAY "[!] Tipo invalido. Use C."
                        LINE 19 COL 02 FOREGROUND-COLOR 4
                     PERFORM 9910-PAUSA
                     DISPLAY WS-BLANK-LINE LINE 19 COL 02
                     DISPLAY WS-BLANK-LINE LINE 24 COL 02
               END-EVALUATE
           END-PERFORM.

      ************************************************************
      * 9300 - Cargar cuentas
      ************************************************************
       9300-CARGAR-CUENTAS.
           MOVE 0 TO WS-TOTAL-REAL.
           MOVE CUSM-ID-CLIENTE TO INVM-ID-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           PERFORM UNTIL NOT LK-EXITO OR WS-TOTAL-REAL >= 5
               ADD 1 TO WS-TOTAL-REAL
               MOVE INVM-ID-CUENTA   TO WS-CTA-ID-CUENTA(WS-TOTAL-REAL)
               MOVE INVM-TIPO-CUENTA TO WS-CTA-TIPO(WS-TOTAL-REAL)
               MOVE INVM-SALDO-ACTUAL TO WS-CTA-SALDO(WS-TOTAL-REAL)
               MOVE INVM-FECHA-APERTURA TO WS-CTA-FECHA(WS-TOTAL-REAL)
               MOVE INVM-ESTADO-CUENTA TO WS-CTA-ESTADO(WS-TOTAL-REAL)
               
               MOVE 'F' TO LK-ACCION-DB
               CALL WS-PGM-DBIOINVM USING REG-INVM,
                                          LK-DATOS-SESION,
                                          LK-DATOS-TRANSACCION
           END-PERFORM.

           MOVE 'Z' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

      ************************************************************
      * 9400 - Mostrar tabla de cuentas
      ************************************************************
       9400-MOSTRAR-CUENTAS.
           DISPLAY "CUENTA     TIPO SALDO           EST"
              LINE 13 COL 02 FOREGROUND-COLOR 3.
           DISPLAY "--------   ---- -------------   ---"
              LINE 14 COL 02 FOREGROUND-COLOR 3.

           MOVE 15 TO WS-LIN.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-TOTAL-REAL
               MOVE WS-CTA-SALDO(WS-I) TO WS-SALDO-EDIT
               MOVE WS-CTA-ID-CUENTA(WS-I) TO WS-CUENTA-FMT
               DISPLAY WS-CUENTA-FMT LINE WS-LIN COL 02
                  FOREGROUND-COLOR 6 HIGHLIGHT
               DISPLAY WS-CTA-TIPO(WS-I) LINE WS-LIN COL 14
                  FOREGROUND-COLOR 7
               DISPLAY WS-SALDO-EDIT LINE WS-LIN COL 17
                  FOREGROUND-COLOR 7
               DISPLAY WS-CTA-ESTADO(WS-I) LINE WS-LIN COL 33
                  FOREGROUND-COLOR 7
               ADD 1 TO WS-LIN
           END-PERFORM.

      ************************************************************
      * 9500 - Buscar cuenta en memoria
      ************************************************************
       9500-BUSCAR-EN-TABLA.
           MOVE 0 TO WS-IDX.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-TOTAL-REAL
               IF WS-CTA-ID-CUENTA(WS-I) = WS-ID-SELEC
                   MOVE WS-I TO WS-IDX
               END-IF
           END-PERFORM.

      ************************************************************
      * 9600 - Capturar monto transaccion (CON LOOP)
      ************************************************************
       9600-CAPTURAR-MONTO.
           MOVE 'N' TO WS-MONTO-OK.
           PERFORM UNTIL WS-MONTO-OK = 'S' OR WS-CANCELO = 'S'
               MOVE ZEROS TO WS-MONTO-TX
               MOVE SPACES TO WS-MONTO-ENTRADA
               
               DISPLAY "Monto a operar (X=salir):" LINE 22 COL 02
                  FOREGROUND-COLOR 6
               DISPLAY "            " LINE 22 COL 28
               ACCEPT WS-MONTO-ENTRADA LINE 22 COL 28

               EVALUATE TRUE
                  WHEN WS-MONTO-ENTRADA(1:1) = "X" 
                    OR WS-MONTO-ENTRADA(1:1) = "x"
                     PERFORM 9900-AVISO-CANCELO
                     MOVE 'S' TO WS-CANCELO
                  WHEN WS-MONTO-ENTRADA(1:1) IS NOT NUMERIC 
                   AND WS-MONTO-ENTRADA NOT = SPACES
                     DISPLAY "[!] Solo numeros." LINE 23 COL 02 
                        FOREGROUND-COLOR 4
                     PERFORM 9910-PAUSA
                     DISPLAY WS-BLANK-LINE LINE 23 COL 02
                     DISPLAY WS-BLANK-LINE LINE 24 COL 02
                  WHEN OTHER
                     COMPUTE WS-MONTO-TX = 
                        FUNCTION NUMVAL(WS-MONTO-ENTRADA)
                     IF WS-MONTO-TX <= ZEROS
                         DISPLAY "[!] Monto debe ser mayor a 0." 
                            LINE 23 COL 02 FOREGROUND-COLOR 4
                         PERFORM 9910-PAUSA
                         DISPLAY WS-BLANK-LINE LINE 23 COL 02
                         DISPLAY WS-BLANK-LINE LINE 24 COL 02
                     ELSE
                         MOVE 'S' TO WS-MONTO-OK
                     END-IF
               END-EVALUATE
           END-PERFORM.

      ************************************************************
      * 9800 - Modulo 10
      ************************************************************
       9800-VALIDAR-MODULO-10.
           MOVE 'N' TO WS-MOD10-OK.
           MOVE 0   TO WS-SUMA-MOD10.

           MOVE WS-DOC-ENTRADA(1:2) TO WS-PROV-DOC.
           IF WS-PROV-DOC < 1 OR WS-PROV-DOC > 24
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-DOC-ENTRADA(10:1) TO WS-DIGITO-VERIF.

           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > 9
               MOVE WS-DOC-ENTRADA(WS-K:1) TO WS-DIGITO
               IF FUNCTION MOD(WS-K, 2) = 1
                   COMPUTE WS-RESULTADO = WS-DIGITO * 2
                   IF WS-RESULTADO >= 10
                       SUBTRACT 9 FROM WS-RESULTADO
                   END-IF
               ELSE
                   MOVE WS-DIGITO TO WS-RESULTADO
               END-IF
               ADD WS-RESULTADO TO WS-SUMA-MOD10
           END-PERFORM.

           COMPUTE WS-DIGITO-CALC = FUNCTION MOD(WS-SUMA-MOD10, 10).
           IF WS-DIGITO-CALC NOT = 0
               COMPUTE WS-DIGITO-CALC = 10 - WS-DIGITO-CALC
           END-IF.

           IF WS-DIGITO-CALC = WS-DIGITO-VERIF
               MOVE 'S' TO WS-MOD10-OK
           END-IF.

      ************************************************************
      * UTILERIAS DE INTERFAZ
      ************************************************************
       9900-AVISO-CANCELO.
           DISPLAY "[i] Operacion cancelada por el usuario."
              LINE 22 COL 02 FOREGROUND-COLOR 5 HIGHLIGHT.
           PERFORM 9910-PAUSA.

       9910-PAUSA.
           DISPLAY "Presione ENTER para volver..." LINE 24 COL 02
              FOREGROUND-COLOR 7.
           ACCEPT WS-CONFIRMAR LINE 24 COL 33.

       END PROGRAM IN0000.
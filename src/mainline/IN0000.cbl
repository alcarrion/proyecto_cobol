      ******************************************************************
      * IN0000.CBL - MODULO DE CUENTAS BANCARIAS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IN0000.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPCION-INVM         PIC 9(01).
       01  WS-CONTINUAR-INVM      PIC X(01) VALUE 'S'.
       01  WS-CONFIRMAR           PIC X(01).
       01  WS-PAUSA               PIC X(01).

       01  WS-PROGRAMAS.
           05 WS-PGM-DBIOINVM     PIC X(08) VALUE 'DBIOINVM'.
           05 WS-PGM-DBIOCUSM     PIC X(08) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOTRAN     PIC X(08) VALUE 'DBIOTRAN'.

       01  WS-ACCION-TRANS        PIC X(01).

       01  WS-DOC-ENTRADA         PIC X(12).
       01  WS-DOC-LEN             PIC 9(02).
       01  WS-I                   PIC 9(02).
       01  WS-DOC-OK              PIC X(01) VALUE 'N'.

       01  WS-TIPO-INGRESADO      PIC X(01).
       01  WS-ID-SELEC            PIC 9(08).
       01  WS-IDX                 PIC 9(02).
       01  WS-LIN                 PIC 9(02).
       01  WS-TOTAL-REAL          PIC 9(02) VALUE 0.
       01  WS-SALDO-EDIT          PIC $ZZ,ZZZ,ZZ9.99.
       01  WS-BLANK-LINE          PIC X(55) VALUE SPACES.

       01  WS-MONTO-ENTRADA       PIC X(12).
       01  WS-MONTO-TX            PIC S9(13)V99 COMP-3.

       01  WS-CUENTAS-TABLA.
           05 WS-CTA OCCURS 5 TIMES.
               10 WS-CTA-ID-CUENTA  PIC 9(08).
               10 WS-CTA-TIPO       PIC X(01).
               10 WS-CTA-SALDO      PIC S9(13)V99 COMP-3.
               10 WS-CTA-FECHA      PIC X(10).
               10 WS-CTA-ESTADO     PIC X(01).

       01  WS-MOD10.
           05 WS-MOD10-OK          PIC X     VALUE 'N'.
           05 WS-DIGITO            PIC 9(01).
           05 WS-RESULTADO         PIC 9(02).
           05 WS-SUMA-MOD10        PIC 9(04) VALUE 0.
           05 WS-DIGITO-VERIF      PIC 9(01).
           05 WS-DIGITO-CALC       PIC 9(01).
           05 WS-K                 PIC 9(02).
           05 WS-PROV-DOC          PIC 9(02).

       LINKAGE SECTION.
           COPY CUSMREC.
           COPY INVMREC.
           COPY LKCIF.

       SCREEN SECTION.
       01  SCR-MARCO-INVM.
           05 BLANK SCREEN.
           05 LINE 02 COL 02 VALUE
              "---------------------------------------------------".
           05 LINE 03 COL 02 VALUE
              "      BANCO LAF - MODULO DE CUENTAS v3.0".
           05 LINE 04 COL 02 VALUE
              "---------------------------------------------------".
           05 LINE 05 COL 04 VALUE "Operador: ".
           05 LINE 05 COL 14 PIC X(08) FROM LKCIF-USUARIO.
           05 LINE 05 COL 26 VALUE "Terminal: ".
           05 LINE 05 COL 36 PIC X(04) FROM LKCIF-TERMINAL.

       01  SCR-MENU-INVM.
           05 LINE 08 COL 05 VALUE "1. Abrir cuenta nueva".
           05 LINE 09 COL 05 VALUE "2. Consultar mis cuentas".
           05 LINE 10 COL 05 VALUE "3. Depositar en cuenta".
           05 LINE 11 COL 05 VALUE "4. Retirar de cuenta".
           05 LINE 12 COL 05 VALUE "5. Cerrar una cuenta".
           05 LINE 13 COL 05 VALUE "6. Volver al menu principal".
           05 LINE 15 COL 05 VALUE "Opcion: [ ]".
           05 SCR-INVM-OPC LINE 15 COL 14 PIC 9
              USING WS-OPCION-INVM REQUIRED.

       PROCEDURE DIVISION USING REG-CUSM,
                                REG-INVM,
                                LK-DATOS-SESION,
                                LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 'S' TO WS-CONTINUAR-INVM.
           PERFORM 1000-MENU-INVM
              UNTIL WS-CONTINUAR-INVM = 'N'.
           GOBACK.

      ************************************************************
      * MENU PRINCIPAL DE CUENTAS
      ************************************************************
       1000-MENU-INVM.
           DISPLAY SCR-MARCO-INVM.
           DISPLAY SCR-MENU-INVM.
           ACCEPT SCR-INVM-OPC.

           EVALUATE WS-OPCION-INVM
               WHEN 1
                   PERFORM 2000-ABRIR-CUENTA
               WHEN 2
                   PERFORM 3000-CONSULTAR-CUENTAS
               WHEN 3
                   PERFORM 4000-DEPOSITAR
               WHEN 4
                   PERFORM 5000-RETIRAR
               WHEN 5
                   PERFORM 6000-CERRAR-CUENTA
               WHEN 6
                   MOVE 'N' TO WS-CONTINUAR-INVM
               WHEN OTHER
                   DISPLAY "Opcion no valida." LINE 17 COL 05
                   ACCEPT WS-PAUSA LINE 17 COL 25
           END-EVALUATE.

      ************************************************************
      * OPCION 1 - ABRIR CUENTA NUEVA (solo A o C)
      ************************************************************
       2000-ABRIR-CUENTA.
           DISPLAY SCR-MARCO-INVM.
           DISPLAY "ALTA DE CUENTA BANCARIA" LINE 07 COL 15.

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-DOC-OK = 'N' EXIT PARAGRAPH END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "Cliente inactivo. No se puede operar."
                  LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9200-CAPTURAR-TIPO-CUENTA.
           IF WS-TIPO-INGRESADO = 'X' EXIT PARAGRAPH END-IF.

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
               DISPLAY "Cuenta abierta. Numero: " INVM-ID-CUENTA
                  LINE 22 COL 05
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               IF LK-ERROR-DUPLICADO
                   DISPLAY "Ya tiene una cuenta de ese tipo."
                      LINE 22 COL 05
               ELSE
                   DISPLAY LK-MENSAJE LINE 22 COL 05
               END-IF
           END-IF.

           DISPLAY "Presione ENTER para continuar." LINE 23 COL 05.
           ACCEPT WS-PAUSA LINE 23 COL 36.

      ************************************************************
      * OPCION 2 - CONSULTAR CUENTAS
      ************************************************************
       3000-CONSULTAR-CUENTAS.
           DISPLAY SCR-MARCO-INVM.
           DISPLAY "CONSULTA DE CUENTAS" LINE 07 COL 17.

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-DOC-OK = 'N' EXIT PARAGRAPH END-IF.

           PERFORM 9300-CARGAR-CUENTAS.

           IF WS-TOTAL-REAL = 0
               DISPLAY "Este cliente no tiene cuentas."
                  LINE 16 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9400-MOSTRAR-CUENTAS.
           DISPLAY "Presione ENTER para continuar." LINE 23 COL 05.
           ACCEPT WS-PAUSA LINE 23 COL 36.

      ************************************************************
      * OPCION 3 - DEPOSITAR EN CUENTA
      ************************************************************
       4000-DEPOSITAR.
           DISPLAY SCR-MARCO-INVM.
           DISPLAY "DEPOSITAR EN CUENTA" LINE 07 COL 17.

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-DOC-OK = 'N' EXIT PARAGRAPH END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "Cliente inactivo. No se puede operar."
                  LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9300-CARGAR-CUENTAS.
           IF WS-TOTAL-REAL = 0
               DISPLAY "No hay cuentas disponibles." LINE 16 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9400-MOSTRAR-CUENTAS.

           DISPLAY "Numero de cuenta: " LINE 21 COL 05.
           ACCEPT WS-ID-SELEC LINE 21 COL 24.

           PERFORM 9500-BUSCAR-EN-TABLA.
           IF WS-IDX = 0
               DISPLAY "Cuenta no encontrada." LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           IF WS-CTA-ESTADO(WS-IDX) NOT = 'A'
               DISPLAY "Cuenta no activa para operar." LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9600-CAPTURAR-MONTO.
           IF WS-MONTO-TX <= ZEROS
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-CTA-ID-CUENTA(WS-IDX) TO INVM-ID-CUENTA.
           MOVE CUSM-ID-CLIENTE           TO INVM-ID-CLIENTE.
           MOVE WS-MONTO-TX               TO INVM-SALDO-ACTUAL.

           MOVE 'D' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               MOVE INVM-SALDO-ACTUAL TO WS-SALDO-EDIT
               DISPLAY "Deposito OK. Saldo: " WS-SALDO-EDIT
                  LINE 22 COL 05
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY LK-MENSAJE LINE 22 COL 05
           END-IF.

           DISPLAY "Presione ENTER para continuar." LINE 23 COL 05.
           ACCEPT WS-PAUSA LINE 23 COL 36.

      ************************************************************
      * OPCION 4 - RETIRAR DE CUENTA
      ************************************************************
       5000-RETIRAR.
           DISPLAY SCR-MARCO-INVM.
           DISPLAY "RETIRAR DE CUENTA" LINE 07 COL 19.

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-DOC-OK = 'N' EXIT PARAGRAPH END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "Cliente inactivo. No se puede operar."
                  LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9300-CARGAR-CUENTAS.
           IF WS-TOTAL-REAL = 0
               DISPLAY "No hay cuentas disponibles." LINE 16 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9400-MOSTRAR-CUENTAS.

           DISPLAY "Numero de cuenta: " LINE 21 COL 05.
           ACCEPT WS-ID-SELEC LINE 21 COL 24.

           PERFORM 9500-BUSCAR-EN-TABLA.
           IF WS-IDX = 0
               DISPLAY "Cuenta no encontrada." LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           IF WS-CTA-ESTADO(WS-IDX) NOT = 'A'
               DISPLAY "Cuenta no activa para operar." LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           IF WS-CTA-TIPO(WS-IDX) = 'H'
               DISPLAY "Retiros no permitidos en cta hipotecaria."
                  LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-CTA-SALDO(WS-IDX) TO WS-SALDO-EDIT.
           DISPLAY "Saldo disponible: " WS-SALDO-EDIT LINE 22 COL 05.

           PERFORM 9600-CAPTURAR-MONTO.
           IF WS-MONTO-TX <= ZEROS
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-CTA-ID-CUENTA(WS-IDX) TO INVM-ID-CUENTA.
           MOVE CUSM-ID-CLIENTE           TO INVM-ID-CLIENTE.
           MOVE WS-MONTO-TX               TO INVM-SALDO-ACTUAL.

           MOVE 'R' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               MOVE INVM-SALDO-ACTUAL TO WS-SALDO-EDIT
               DISPLAY "Retiro OK. Saldo: " WS-SALDO-EDIT
                  LINE 23 COL 05
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY LK-MENSAJE LINE 23 COL 05
           END-IF.

           DISPLAY "Presione ENTER para continuar." LINE 24 COL 05.
           ACCEPT WS-PAUSA LINE 24 COL 36.

      ************************************************************
      * OPCION 5 - CERRAR UNA CUENTA
      ************************************************************
       6000-CERRAR-CUENTA.
           DISPLAY SCR-MARCO-INVM.
           DISPLAY "CIERRE DE CUENTA" LINE 07 COL 19.

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-DOC-OK = 'N' EXIT PARAGRAPH END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "Cliente inactivo. No se puede operar."
                  LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9300-CARGAR-CUENTAS.
           IF WS-TOTAL-REAL = 0
               DISPLAY "No hay cuentas para cerrar." LINE 16 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9400-MOSTRAR-CUENTAS.

           DISPLAY "Numero de cuenta a cerrar: " LINE 21 COL 05.
           ACCEPT WS-ID-SELEC LINE 21 COL 32.

           PERFORM 9500-BUSCAR-EN-TABLA.
           IF WS-IDX = 0
               DISPLAY "Cuenta no encontrada." LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           IF WS-CTA-ESTADO(WS-IDX) = 'C'
               DISPLAY "Esa cuenta ya esta cerrada." LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           IF WS-CTA-SALDO(WS-IDX) > ZEROS
               MOVE WS-CTA-SALDO(WS-IDX) TO WS-SALDO-EDIT
               DISPLAY "Saldo pendiente: " WS-SALDO-EDIT
                  LINE 22 COL 05
               DISPLAY "Retire el saldo antes de cerrar."
                  LINE 23 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 24 COL 05
               ACCEPT WS-PAUSA LINE 24 COL 36
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Confirmar cierre (S/N): " LINE 22 COL 05.
           ACCEPT WS-CONFIRMAR LINE 22 COL 30.

           IF WS-CONFIRMAR NOT = 'S' AND WS-CONFIRMAR NOT = 's'
               DISPLAY "Operacion cancelada." LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-CTA-ID-CUENTA(WS-IDX) TO INVM-ID-CUENTA.
           MOVE CUSM-ID-CLIENTE           TO INVM-ID-CLIENTE.

           MOVE 'B' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY "Cuenta cerrada correctamente." LINE 22 COL 05
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY LK-MENSAJE LINE 22 COL 05
           END-IF.

           DISPLAY "Presione ENTER para continuar." LINE 23 COL 05.
           ACCEPT WS-PAUSA LINE 23 COL 36.

      ************************************************************
      * 9100 - Buscar cliente por documento
      ************************************************************
       9100-BUSCAR-CLIENTE.
           MOVE 'N' TO WS-DOC-OK.
           DISPLAY "Documento del cliente: " LINE 08 COL 05.
           ACCEPT WS-DOC-ENTRADA LINE 08 COL 29.

           MOVE 12 TO WS-I.
           PERFORM UNTIL WS-I = 0
                      OR WS-DOC-ENTRADA(WS-I:1) NOT = SPACE
               SUBTRACT 1 FROM WS-I
           END-PERFORM.
           MOVE WS-I TO WS-DOC-LEN.

           IF WS-DOC-LEN < 6 OR WS-DOC-LEN > 12
               DISPLAY "Documento invalido (6-12 caracteres)."
                  LINE 10 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           IF WS-DOC-LEN = 10
               IF WS-DOC-ENTRADA(1:10) IS NUMERIC
                   PERFORM 9800-VALIDAR-MODULO-10
                   IF WS-MOD10-OK = 'N'
                       DISPLAY "Cedula invalida (Modulo 10)."
                          LINE 10 COL 05
                       DISPLAY "Presione ENTER para continuar."
                          LINE 23 COL 05
                       ACCEPT WS-PAUSA LINE 23 COL 36
                       EXIT PARAGRAPH
                   END-IF
                   MOVE 'CED' TO CUSM-TIPO-DOC
               ELSE
                   MOVE 'PAS' TO CUSM-TIPO-DOC
               END-IF
           ELSE
               MOVE 'PAS' TO CUSM-TIPO-DOC
           END-IF.

           MOVE WS-DOC-ENTRADA TO CUSM-DOC-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-ERROR-NODATA
               DISPLAY "Cliente no encontrado." LINE 10 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           IF NOT LK-EXITO
               DISPLAY "Error tecnico al buscar cliente."
                  LINE 10 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           DISPLAY CUSM-NOMBRE-CLIENTE LINE 10 COL 05.
           DISPLAY CUSM-APELLIDOS-CLIENTE LINE 10 COL 32.
           MOVE 'S' TO WS-DOC-OK.

      ************************************************************
      * 9200 - Capturar tipo de cuenta (solo A o C)
      ************************************************************
       9200-CAPTURAR-TIPO-CUENTA.
           MOVE 'X' TO WS-TIPO-INGRESADO.

           DISPLAY "Tipo de cuenta:" LINE 12 COL 05.
           DISPLAY "  A = Ahorros     C = Corriente"
              LINE 13 COL 05.
           DISPLAY "Ingrese tipo (ENTER=cancelar): "
              LINE 14 COL 05.
           ACCEPT WS-TIPO-INGRESADO LINE 14 COL 36.

           IF WS-TIPO-INGRESADO = SPACE
               DISPLAY "Operacion cancelada." LINE 16 COL 05
               EXIT PARAGRAPH
           END-IF.

           IF WS-TIPO-INGRESADO = 'a'
               MOVE 'A' TO WS-TIPO-INGRESADO
           END-IF.
           IF WS-TIPO-INGRESADO = 'c'
               MOVE 'C' TO WS-TIPO-INGRESADO
           END-IF.

           IF WS-TIPO-INGRESADO = 'H' OR 'h'
               DISPLAY "Solo A=Ahorros o C=Corriente." LINE 16 COL 05
               DISPLAY "Cuentas H se crean al registrar hipoteca."
                  LINE 17 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               MOVE 'X' TO WS-TIPO-INGRESADO
               EXIT PARAGRAPH
           END-IF.

           IF WS-TIPO-INGRESADO NOT = 'A'
           AND WS-TIPO-INGRESADO NOT = 'C'
               DISPLAY "Tipo invalido. Ingrese A o C."
                  LINE 16 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               MOVE 'X' TO WS-TIPO-INGRESADO
               EXIT PARAGRAPH
           END-IF.

      ************************************************************
      * 9300 - Cargar hasta 5 cuentas con cursor
      ************************************************************
       9300-CARGAR-CUENTAS.
           MOVE 0 TO WS-TOTAL-REAL.
           MOVE CUSM-ID-CLIENTE TO INVM-ID-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           PERFORM UNTIL NOT LK-EXITO
                      OR WS-TOTAL-REAL >= 5
               ADD 1 TO WS-TOTAL-REAL
               MOVE INVM-ID-CUENTA
                 TO WS-CTA-ID-CUENTA(WS-TOTAL-REAL)
               MOVE INVM-TIPO-CUENTA
                 TO WS-CTA-TIPO(WS-TOTAL-REAL)
               MOVE INVM-SALDO-ACTUAL
                 TO WS-CTA-SALDO(WS-TOTAL-REAL)
               MOVE INVM-FECHA-APERTURA
                 TO WS-CTA-FECHA(WS-TOTAL-REAL)
               MOVE INVM-ESTADO-CUENTA
                 TO WS-CTA-ESTADO(WS-TOTAL-REAL)
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
           DISPLAY "  N/CUENTA   TIPO  SALDO          EST"
              LINE 13 COL 05.
           DISPLAY "  ---------  ----  -------------  ---"
              LINE 14 COL 05.

           MOVE 15 TO WS-LIN.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-TOTAL-REAL
               MOVE WS-CTA-SALDO(WS-I) TO WS-SALDO-EDIT
               DISPLAY WS-CTA-ID-CUENTA(WS-I)
                  LINE WS-LIN COL 05
               DISPLAY WS-CTA-TIPO(WS-I)
                  LINE WS-LIN COL 16
               DISPLAY WS-SALDO-EDIT
                  LINE WS-LIN COL 22
               DISPLAY WS-CTA-ESTADO(WS-I)
                  LINE WS-LIN COL 37
               ADD 1 TO WS-LIN
           END-PERFORM.

      ************************************************************
      * 9500 - Buscar cuenta seleccionada en la tabla local
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
      * 9600 - Capturar monto de transaccion
      ************************************************************
       9600-CAPTURAR-MONTO.
           MOVE ZEROS TO WS-MONTO-TX.
           MOVE SPACES TO WS-MONTO-ENTRADA.
           DISPLAY "Monto (ej: 150.00): " LINE 22 COL 05.
           ACCEPT WS-MONTO-ENTRADA LINE 22 COL 26.

           IF WS-MONTO-ENTRADA = SPACES
               DISPLAY "Operacion cancelada." LINE 22 COL 05
               EXIT PARAGRAPH
           END-IF.

           IF WS-MONTO-ENTRADA(1:1) IS NOT NUMERIC
               DISPLAY "Solo numeros. Ejemplo: 150.00"
                  LINE 22 COL 05
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-MONTO-TX =
               FUNCTION NUMVAL(WS-MONTO-ENTRADA).

           IF WS-MONTO-TX <= ZEROS
               DISPLAY "El monto debe ser mayor a cero."
                  LINE 22 COL 05
               MOVE ZEROS TO WS-MONTO-TX
           END-IF.

      ************************************************************
      * 9800 - Modulo 10 (cedula ecuatoriana SRI)
      ************************************************************
       9800-VALIDAR-MODULO-10.
           MOVE 'N' TO WS-MOD10-OK.
           MOVE 0   TO WS-SUMA-MOD10.

           MOVE WS-DOC-ENTRADA(1:2) TO WS-PROV-DOC.
           IF WS-PROV-DOC < 1 OR WS-PROV-DOC > 24
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-DOC-ENTRADA(10:1) TO WS-DIGITO-VERIF.

           PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > 9
               MOVE WS-DOC-ENTRADA(WS-K:1) TO WS-DIGITO
               EVALUATE TRUE
                   WHEN FUNCTION MOD(WS-K, 2) = 1
                       COMPUTE WS-RESULTADO = WS-DIGITO * 2
                       IF WS-RESULTADO >= 10
                           SUBTRACT 9 FROM WS-RESULTADO
                       END-IF
                   WHEN OTHER
                       MOVE WS-DIGITO TO WS-RESULTADO
               END-EVALUATE
               ADD WS-RESULTADO TO WS-SUMA-MOD10
           END-PERFORM.

           COMPUTE WS-DIGITO-CALC =
               FUNCTION MOD(WS-SUMA-MOD10, 10).
           IF WS-DIGITO-CALC NOT = 0
               COMPUTE WS-DIGITO-CALC = 10 - WS-DIGITO-CALC
           END-IF.

           IF WS-DIGITO-CALC = WS-DIGITO-VERIF
               MOVE 'S' TO WS-MOD10-OK
           END-IF.

       END PROGRAM IN0000.

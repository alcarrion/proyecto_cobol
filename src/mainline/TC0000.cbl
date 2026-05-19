      ******************************************************************
      * TC0000.CBL - MODULO DE TARJETAS DE CREDITO
      * v4.0 - Layout en cajas + cancelar con 'X' o vacio
      *      - Validaciones en bucle (sin salirse al menu por un error)
      *      - Mensajes mas entendibles
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TC0000.
       AUTHOR. TCS ECUADOR.

      *================================================================*
      * MODULO: TC0000 - GESTION DE TARJETAS DE CREDITO
      * FUNCIONES: Emitir, consultar, diferir, cancelar, pagar.
      * TASA FIJA: 35.00% anual (calculos en DF0000).
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPCION-TARJ         PIC 9(01).
       01  WS-CONTINUAR-TARJ      PIC X(01) VALUE 'S'.
       01  WS-CONFIRMAR           PIC X(01).
       01  WS-PAUSA               PIC X(01).

       01  WS-BLANK-LINE          PIC X(75) VALUE SPACES.

       01  WS-PROGRAMAS.
           05 WS-PGM-DBIOCUSM     PIC X(08) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOINVM     PIC X(08) VALUE 'DBIOINVM'.
           05 WS-PGM-DBIOTARJ     PIC X(08) VALUE 'DBIOTARJ'.
           05 WS-PGM-DBIOTRAN     PIC X(08) VALUE 'DBIOTRAN'.
           05 WS-PGM-DF0000       PIC X(08) VALUE 'DF0000'.

       01  WS-ACCION-TRANS        PIC X(01).

      *> Captura del documento
       01  WS-DOC-ENTRADA         PIC X(12).
       01  WS-DOC-LEN             PIC 9(02).
       01  WS-I                   PIC 9(02).

       01  WS-VALIDACIONES.
           05 WS-DOC-OK           PIC X(01) VALUE 'N'.
           05 WS-CANCELO          PIC X(01) VALUE 'N'.
           05 WS-MONTO-OK         PIC X(01) VALUE 'N'.
           05 WS-CUOTAS-OK        PIC X(01) VALUE 'N'.
           05 WS-CUENTA-OK        PIC X(01) VALUE 'N'.
           05 WS-CONFIRM-OK       PIC X(01) VALUE 'N'.

      *> Entradas / Calculos
       01  WS-ENTRADA-MONTO       PIC X(12).
       01  WS-ENTRADA-CUOTAS      PIC X(03).
       01  WS-ENTRADA-CUENTA      PIC X(08).
       01  WS-MONTO-TX            PIC S9(10)V99 COMP-3.
       01  WS-CUOTAS-TX           PIC 9(03).
       01  WS-ID-CUENTA-PRINC     PIC 9(08).

       01  WS-CALCULOS-TC.
           05 WS-CUPO-DISPONIBLE  PIC S9(10)V99 COMP-3.
           05 WS-RESULTADO-DIF    PIC X(01).
           05 WS-CUOTA-EDIT       PIC $ZZZ,ZZZ,ZZ9.99.
           05 WS-TOTAL-PAGAR      PIC S9(10)V99 COMP-3.
           05 WS-INTERES-DISP     PIC S9(10)V99 COMP-3.

       01  WS-FECHA-SISTEMA.
           05 WS-ANIO             PIC 9(04).
           05 WS-MES              PIC 9(02).
           05 WS-DIA              PIC 9(02).
       01  WS-ANIO-VENC           PIC 9(04).

       01  WS-AUX-GEN.
           05 WS-SEMILLA          PIC 9(08).
           05 WS-RANDOM           PIC 9(04).

      *> Formateadores de salida
       01  WS-SALDO-EDIT          PIC $ZZZ,ZZZ,ZZ9.99.
       01  WS-CLI-ID-FMT          PIC ZZZZZZZ9.
       01  WS-CTA-ID-FMT          PIC ZZZZZZZ9.
       01  WS-CUOTAS-FMT          PIC ZZ9.
       01  WS-SCORE-FMT           PIC ZZ9.

       01  WS-MOD10.
           05 WS-MOD10-OK         PIC X(01) VALUE 'N'.
           05 WS-DIGITO           PIC 9(01).
           05 WS-RESULTADO        PIC 9(02).
           05 WS-SUMA-MOD10       PIC 9(04) VALUE 0.
           05 WS-DIGITO-VERIF     PIC 9(01).
           05 WS-DIGITO-CALC      PIC 9(01).
           05 WS-K                PIC 9(02).
           05 WS-PROV-DOC         PIC 9(02).

       LINKAGE SECTION.
           COPY CUSMREC.
           COPY INVMREC.
           COPY TARJREC.
           COPY LKCIF.

       SCREEN SECTION.
      *> Colores: 0=Negro 1=Azul 2=Verde 3=Cyan
      *>          4=Rojo 5=Magenta 6=Amarillo 7=Blanco

      *> ----- Cabecera (titulo + operador) -----
       01  SCR-MARCO.
           05 BLANK SCREEN BACKGROUND-COLOR 0.
           05 LINE 01 COL 01 BACKGROUND-COLOR 1 FOREGROUND-COLOR 7
              HIGHLIGHT VALUE
              "     BANCO LAF - MODULO DE TARJETAS v4.0        ".
           05 LINE 02 COL 02 FOREGROUND-COLOR 3 VALUE "Operador: ".
           05 LINE 02 COL 12 PIC X(08) FROM LKCIF-USUARIO
              FOREGROUND-COLOR 7 HIGHLIGHT.
           05 LINE 02 COL 35 FOREGROUND-COLOR 3 VALUE "Terminal: ".
           05 LINE 02 COL 45 PIC X(04) FROM LKCIF-TERMINAL
              FOREGROUND-COLOR 7 HIGHLIGHT.
           05 LINE 03 COL 02 FOREGROUND-COLOR 3 VALUE
              "=================================================".

      *> ----- Menu principal de tarjetas -----
       01  SCR-MENU-TARJ.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------- [ TARJETAS DE CREDITO ] ----------+".
           05 LINE 06 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 06 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[1]".
           05 LINE 07 COL 09 FOREGROUND-COLOR 7 VALUE
              "Emitir tarjeta nueva".
           05 LINE 07 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[2]".
           05 LINE 08 COL 09 FOREGROUND-COLOR 7 VALUE
              "Consultar tarjeta y cupo".
           05 LINE 08 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[3]".
           05 LINE 09 COL 09 FOREGROUND-COLOR 7 VALUE
              "Diferir un consumo (compra a cuotas)".
           05 LINE 09 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[4]".
           05 LINE 10 COL 09 FOREGROUND-COLOR 7 VALUE
              "Cancelar tarjeta".
           05 LINE 10 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[5]".
           05 LINE 11 COL 09 FOREGROUND-COLOR 7 VALUE
              "Pagar saldo de la tarjeta".
           05 LINE 11 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[6]".
           05 LINE 13 COL 09 FOREGROUND-COLOR 3 VALUE
              "Volver al menu principal".
           05 LINE 13 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".
           05 LINE 16 COL 02 FOREGROUND-COLOR 3 VALUE "Opcion: [ ]".
           05 SCR-TARJ-OPC LINE 16 COL 11 PIC 9
              USING WS-OPCION-TARJ REQUIRED
              FOREGROUND-COLOR 7 HIGHLIGHT.

      *> ----- Caja para buscar cliente (un solo dato) -----
       01  SCR-DOC-BOX.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+------------ [ BUSCAR CLIENTE ] --------------+".
           05 LINE 06 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 06 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 04 FOREGROUND-COLOR 6 VALUE
              "Cedula o pasaporte :".
           05 LINE 07 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".
           05 LINE 10 COL 02 FOREGROUND-COLOR 5 VALUE
              "[i] Tip: escriba 'X' o deje vacio para cancelar".

      *> ----- Caja del formulario de emision de tarjeta -----
       01  SCR-EMITIR-FORM.
           05 LINE 11 COL 02 FOREGROUND-COLOR 3 VALUE
              "+--------- [ DATOS DE LA NUEVA TARJETA ] ------+".
           05 LINE 12 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 04 FOREGROUND-COLOR 7 VALUE
              "Cuenta para debitos :".
           05 LINE 13 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 04 FOREGROUND-COLOR 7 VALUE
              "Cupo a otorgar  USD :".
           05 LINE 14 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 15 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 15 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 16 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".

      *> ----- Caja del formulario de consumo diferido -----
       01  SCR-DIFERIR-FORM.
           05 LINE 11 COL 02 FOREGROUND-COLOR 3 VALUE
              "+-------- [ DETALLE DE LA COMPRA ] ------------+".
           05 LINE 12 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 04 FOREGROUND-COLOR 7 VALUE
              "Monto de la compra  :".
           05 LINE 13 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 04 FOREGROUND-COLOR 7 VALUE
              "Numero de cuotas    :".
           05 LINE 14 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 15 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 15 COL 04 FOREGROUND-COLOR 5 VALUE
              " (rango 1 a 36, 1 = sin interes)".
           05 LINE 15 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 16 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".

      *> ----- Caja del formulario de pago de tarjeta -----
       01  SCR-PAGAR-FORM.
           05 LINE 11 COL 02 FOREGROUND-COLOR 3 VALUE
              "+------------ [ PAGO DE TARJETA ] -------------+".
           05 LINE 12 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 04 FOREGROUND-COLOR 7 VALUE
              "Monto a pagar   USD :".
           05 LINE 13 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 15 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".

       PROCEDURE DIVISION USING REG-CUSM,
                                REG-INVM,
                                REG-TARJ,
                                REG-DIFD,
                                LK-DATOS-SESION,
                                LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 'S' TO WS-CONTINUAR-TARJ.
           PERFORM 1000-MENU-TARJ
               UNTIL WS-CONTINUAR-TARJ = 'N'.
           GOBACK.

       1000-MENU-TARJ.
           MOVE 0 TO WS-OPCION-TARJ.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-MENU-TARJ.
           ACCEPT SCR-TARJ-OPC.

           EVALUATE WS-OPCION-TARJ
               WHEN 1 PERFORM 2000-EMITIR-TARJETA
               WHEN 2 PERFORM 3000-CONSULTAR-TARJETA
               WHEN 3 PERFORM 4000-CONSUMO-DIFERIDO
               WHEN 4 PERFORM 5000-CANCELAR-TARJETA
               WHEN 5 PERFORM 6000-PAGAR-TARJETA
               WHEN 6 MOVE 'N' TO WS-CONTINUAR-TARJ
               WHEN OTHER
                   DISPLAY "[!] Opcion no valida. Use 1-6."
                      LINE 18 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                   DISPLAY "Presione ENTER..." LINE 19 COL 02
                   ACCEPT WS-CONFIRMAR LINE 19 COL 20
           END-EVALUATE.

      ******************************************************************
      * OPCION 1 - EMITIR TARJETA NUEVA
      ******************************************************************
       2000-EMITIR-TARJETA.
           DISPLAY SCR-MARCO.
           DISPLAY "[*] EMISION DE TARJETA NUEVA" LINE 04 COL 02
              FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY SCR-DOC-BOX.
           INITIALIZE REG-CUSM.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-CAPTURAR-DOCUMENTO.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9120-BUSCAR-CLIENTE-EN-BD.
           IF WS-DOC-OK = 'N'
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9130-MOSTRAR-CLIENTE.

      *> Validaciones de elegibilidad
           IF CLIENTE-INACTIVO
               DISPLAY "[X] Cliente inactivo. No se puede emitir."
                  LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           IF CUSM-SCORE-CREDITICIO < 450
               MOVE CUSM-SCORE-CREDITICIO TO WS-SCORE-FMT
               DISPLAY "[X] Score crediticio insuficiente."
                  LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "    Score actual:" LINE 20 COL 02
                  FOREGROUND-COLOR 4
               DISPLAY WS-SCORE-FMT LINE 20 COL 22
                  FOREGROUND-COLOR 6 HIGHLIGHT
               DISPLAY "(minimo requerido: 450)" LINE 20 COL 28
                  FOREGROUND-COLOR 4
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           IF CUSM-TIENE-TARJETA = 1
               DISPLAY "[X] El cliente ya posee una tarjeta activa."
                  LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "    No se pueden emitir duplicadas."
                  LINE 20 COL 02 FOREGROUND-COLOR 4
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           DISPLAY SCR-EMITIR-FORM.

      *> Capturar cuenta (loop con cancelar)
           PERFORM 9300-CAPTURAR-CUENTA-DEBITO.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

      *> Capturar cupo (loop con cancelar)
           PERFORM 9310-CAPTURAR-CUPO-EMISION.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

      *> Inserta tarjeta
           INITIALIZE REG-TARJ.
           PERFORM 9200-GENERAR-NRO-TARJETA.

           ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD.
           ADD 5 TO WS-ANIO GIVING WS-ANIO-VENC.

           STRING WS-ANIO "-" WS-MES "-" WS-DIA
               DELIMITED BY SIZE INTO TARJ-FECHA-EMISION.
           STRING WS-ANIO-VENC "-" WS-MES "-" WS-DIA
               DELIMITED BY SIZE INTO TARJ-FECHA-VENCTO.

           MOVE CUSM-ID-CLIENTE      TO TARJ-ID-CLIENTE.
           MOVE WS-ID-CUENTA-PRINC   TO TARJ-CUENTA-PRINCIPAL.
           MOVE WS-MONTO-TX          TO TARJ-CUPO-APROBADO.
           MOVE ZEROS                TO TARJ-SALDO-UTILIZADO.
           MOVE 'A'                  TO TARJ-ESTADO.

           MOVE 'A' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                                      REG-DIFD,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               PERFORM 2100-MOSTRAR-RESULTADO-EMISION
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               IF LK-ERROR-DUPLICADO
                   DISPLAY "[X] El cliente ya tiene tarjeta activa."
                      LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               ELSE
                   DISPLAY "[X] No se pudo crear la tarjeta."
                      LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                   DISPLAY LK-MENSAJE LINE 20 COL 02
                      FOREGROUND-COLOR 4
               END-IF
           END-IF.

           PERFORM 9910-PAUSA-MENU.

      ******************************************************************
      * 2100 - Caja con resultado de emision exitosa
      ******************************************************************
       2100-MOSTRAR-RESULTADO-EMISION.
           MOVE WS-MONTO-TX TO WS-SALDO-EDIT.

           DISPLAY "+--------- [ TARJETA EMITIDA OK ] ---------+"
              LINE 18 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 19 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "Numero  :" LINE 19 COL 04 FOREGROUND-COLOR 7.
           DISPLAY TARJ-NRO-TARJETA LINE 19 COL 16
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "|" LINE 19 COL 45 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 20 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "Vence   :" LINE 20 COL 04 FOREGROUND-COLOR 7.
           DISPLAY TARJ-FECHA-VENCTO LINE 20 COL 16
              FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY "|" LINE 20 COL 45 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 21 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "Cupo    :" LINE 21 COL 04 FOREGROUND-COLOR 7.
           DISPLAY WS-SALDO-EDIT LINE 21 COL 16
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "|" LINE 21 COL 45 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "+------------------------------------------+"
              LINE 22 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.

      ******************************************************************
      * OPCION 2 - CONSULTAR TARJETA Y CUPO DISPONIBLE
      ******************************************************************
       3000-CONSULTAR-TARJETA.
           DISPLAY SCR-MARCO.
           DISPLAY "[*] CONSULTA DE TARJETA" LINE 04 COL 02
              FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY SCR-DOC-BOX.
           INITIALIZE REG-CUSM.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-CAPTURAR-DOCUMENTO.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9120-BUSCAR-CLIENTE-EN-BD.
           IF WS-DOC-OK = 'N'
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9130-MOSTRAR-CLIENTE.
           PERFORM 9140-LEER-TARJETA-CLIENTE.
           IF WS-DOC-OK = 'N'
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-CUPO-DISPONIBLE =
               TARJ-CUPO-APROBADO - TARJ-SALDO-UTILIZADO.

           DISPLAY "+--------- [ DATOS DE LA TARJETA ] --------+"
              LINE 12 COL 02 FOREGROUND-COLOR 3.
           DISPLAY "|" LINE 13 COL 02 FOREGROUND-COLOR 3.
           DISPLAY "Numero  :" LINE 13 COL 04 FOREGROUND-COLOR 7.
           DISPLAY TARJ-NRO-TARJETA LINE 13 COL 16
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "|" LINE 13 COL 45 FOREGROUND-COLOR 3.
           DISPLAY "|" LINE 14 COL 02 FOREGROUND-COLOR 3.
           DISPLAY "Estado  :" LINE 14 COL 04 FOREGROUND-COLOR 7.
           DISPLAY TARJ-ESTADO LINE 14 COL 16
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "Emitida :" LINE 14 COL 22 FOREGROUND-COLOR 7.
           DISPLAY TARJ-FECHA-EMISION LINE 14 COL 33
              FOREGROUND-COLOR 7.
           DISPLAY "|" LINE 14 COL 45 FOREGROUND-COLOR 3.
           DISPLAY "|" LINE 15 COL 02 FOREGROUND-COLOR 3.
           DISPLAY "Vence   :" LINE 15 COL 04 FOREGROUND-COLOR 7.
           DISPLAY TARJ-FECHA-VENCTO LINE 15 COL 16
              FOREGROUND-COLOR 7.
           DISPLAY "|" LINE 15 COL 45 FOREGROUND-COLOR 3.
           DISPLAY "+------------------------------------------+"
              LINE 16 COL 02 FOREGROUND-COLOR 3.

           MOVE TARJ-CUPO-APROBADO TO WS-SALDO-EDIT.
           DISPLAY "Cupo total      :" LINE 18 COL 02
              FOREGROUND-COLOR 7.
           DISPLAY WS-SALDO-EDIT LINE 18 COL 22
              FOREGROUND-COLOR 6 HIGHLIGHT.

           MOVE TARJ-SALDO-UTILIZADO TO WS-SALDO-EDIT.
           DISPLAY "Deuda actual    :" LINE 19 COL 02
              FOREGROUND-COLOR 7.
           DISPLAY WS-SALDO-EDIT LINE 19 COL 22
              FOREGROUND-COLOR 4 HIGHLIGHT.

           MOVE WS-CUPO-DISPONIBLE TO WS-SALDO-EDIT.
           DISPLAY "Cupo disponible :" LINE 20 COL 02
              FOREGROUND-COLOR 7.
           DISPLAY WS-SALDO-EDIT LINE 20 COL 22
              FOREGROUND-COLOR 2 HIGHLIGHT.

           PERFORM 9910-PAUSA-MENU.

      ******************************************************************
      * OPCION 3 - DIFERIR CONSUMO (LLAMA A DF0000)
      ******************************************************************
       4000-CONSUMO-DIFERIDO.
           DISPLAY SCR-MARCO.
           DISPLAY "[*] CONSUMO DIFERIDO (COMPRA A CUOTAS)"
              LINE 04 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY SCR-DOC-BOX.
           INITIALIZE REG-CUSM.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-CAPTURAR-DOCUMENTO.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9120-BUSCAR-CLIENTE-EN-BD.
           IF WS-DOC-OK = 'N'
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "[X] Cliente inactivo. No se puede operar."
                  LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9130-MOSTRAR-CLIENTE.
           PERFORM 9140-LEER-TARJETA-CLIENTE.
           IF WS-DOC-OK = 'N'
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           IF NOT TARJETA-ACTIVA
               DISPLAY "[X] La tarjeta no esta activa."
                  LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "    Estado actual: " LINE 20 COL 02
                  FOREGROUND-COLOR 4
               DISPLAY TARJ-ESTADO LINE 20 COL 22
                  FOREGROUND-COLOR 6 HIGHLIGHT
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-CUPO-DISPONIBLE =
               TARJ-CUPO-APROBADO - TARJ-SALDO-UTILIZADO.

           DISPLAY "Tarjeta         :" LINE 11 COL 02
              FOREGROUND-COLOR 7.
           DISPLAY TARJ-NRO-TARJETA LINE 11 COL 22
              FOREGROUND-COLOR 6 HIGHLIGHT.

           MOVE WS-CUPO-DISPONIBLE TO WS-SALDO-EDIT.
           DISPLAY "Cupo disponible :" LINE 12 COL 02
              FOREGROUND-COLOR 7.
           DISPLAY WS-SALDO-EDIT LINE 12 COL 22
              FOREGROUND-COLOR 2 HIGHLIGHT.

           DISPLAY SCR-DIFERIR-FORM.

      *> Capturar monto (loop)
           PERFORM 9320-CAPTURAR-MONTO-DIFERIDO.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

      *> Capturar cuotas (loop)
           PERFORM 9330-CAPTURAR-CUOTAS.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

      *> Confirmacion (loop)
           PERFORM 9340-CONFIRMAR-OPERACION.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

      *> Llamar al subprograma DF0000
           CALL WS-PGM-DF0000 USING REG-TARJ,
                                    REG-DIFD,
                                    LK-DATOS-SESION,
                                    LK-DATOS-TRANSACCION,
                                    WS-MONTO-TX,
                                    WS-CUOTAS-TX,
                                    WS-RESULTADO-DIF.

           IF WS-RESULTADO-DIF = 'S'
               DISPLAY SCR-MARCO
               DISPLAY "[OK] CONSUMO DIFERIDO REGISTRADO"
                  LINE 04 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT
               PERFORM 4100-MOSTRAR-RESULTADO-DIFERIDO
           ELSE
               DISPLAY "[X] No se pudo registrar el diferido."
                  LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY LK-MENSAJE LINE 20 COL 02 FOREGROUND-COLOR 4
           END-IF.

           PERFORM 9910-PAUSA-MENU.

      ******************************************************************
      * 4100 - Caja resumen del diferido
      ******************************************************************
       4100-MOSTRAR-RESULTADO-DIFERIDO.
           COMPUTE WS-TOTAL-PAGAR  = DIFD-VALOR-CUOTA * WS-CUOTAS-TX.
           COMPUTE WS-INTERES-DISP = WS-TOTAL-PAGAR - WS-MONTO-TX.

           MOVE WS-CUOTAS-TX TO WS-CUOTAS-FMT.

           DISPLAY "+--------- [ RESUMEN DEL DIFERIDO ] -------+"
              LINE 10 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 11 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "Cuotas pactadas :" LINE 11 COL 04
              FOREGROUND-COLOR 7.
           DISPLAY WS-CUOTAS-FMT LINE 11 COL 22
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "|" LINE 11 COL 45 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 12 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           MOVE DIFD-VALOR-CUOTA TO WS-CUOTA-EDIT.
           DISPLAY "Valor por cuota :" LINE 12 COL 04
              FOREGROUND-COLOR 7.
           DISPLAY WS-CUOTA-EDIT LINE 12 COL 22
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "|" LINE 12 COL 45 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 13 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           MOVE WS-MONTO-TX TO WS-SALDO-EDIT.
           DISPLAY "Monto de compra :" LINE 13 COL 04
              FOREGROUND-COLOR 7.
           DISPLAY WS-SALDO-EDIT LINE 13 COL 22
              FOREGROUND-COLOR 7.
           DISPLAY "|" LINE 13 COL 45 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 14 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           MOVE WS-INTERES-DISP TO WS-SALDO-EDIT.
           DISPLAY "Interes total   :" LINE 14 COL 04
              FOREGROUND-COLOR 7.
           DISPLAY WS-SALDO-EDIT LINE 14 COL 22
              FOREGROUND-COLOR 4 HIGHLIGHT.
           DISPLAY "|" LINE 14 COL 45 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 15 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           MOVE WS-TOTAL-PAGAR TO WS-SALDO-EDIT.
           DISPLAY "TOTAL A PAGAR   :" LINE 15 COL 04
              FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY WS-SALDO-EDIT LINE 15 COL 22
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "|" LINE 15 COL 45 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "+------------------------------------------+"
              LINE 16 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.

      ******************************************************************
      * OPCION 4 - CANCELAR TARJETA (SALDO DEBE SER CERO)
      ******************************************************************
       5000-CANCELAR-TARJETA.
           DISPLAY SCR-MARCO.
           DISPLAY "[*] CANCELACION DE TARJETA" LINE 04 COL 02
              FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY SCR-DOC-BOX.
           INITIALIZE REG-CUSM.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-CAPTURAR-DOCUMENTO.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9120-BUSCAR-CLIENTE-EN-BD.
           IF WS-DOC-OK = 'N'
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9130-MOSTRAR-CLIENTE.
           PERFORM 9140-LEER-TARJETA-CLIENTE.
           IF WS-DOC-OK = 'N'
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           MOVE TARJ-SALDO-UTILIZADO TO WS-SALDO-EDIT.
           DISPLAY "Tarjeta : " LINE 12 COL 02 FOREGROUND-COLOR 7.
           DISPLAY TARJ-NRO-TARJETA LINE 12 COL 16
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "Estado  : " LINE 13 COL 02 FOREGROUND-COLOR 7.
           DISPLAY TARJ-ESTADO LINE 13 COL 16
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "Saldo   : " LINE 14 COL 02 FOREGROUND-COLOR 7.
           DISPLAY WS-SALDO-EDIT LINE 14 COL 16
              FOREGROUND-COLOR 4 HIGHLIGHT.

           IF TARJ-SALDO-UTILIZADO > ZEROS
               DISPLAY "[X] No se puede cancelar."
                  LINE 16 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "    La tarjeta tiene saldo pendiente."
                  LINE 17 COL 02 FOREGROUND-COLOR 4
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9340-CONFIRMAR-OPERACION.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           MOVE 'B' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                                      REG-DIFD,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY "[OK] Tarjeta cancelada correctamente."
                  LINE 19 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY "[X] No se pudo cancelar."
                  LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY LK-MENSAJE LINE 20 COL 02 FOREGROUND-COLOR 4
           END-IF.

           PERFORM 9910-PAUSA-MENU.

      ******************************************************************
      * OPCION 5 - PAGAR TARJETA DE CREDITO
      ******************************************************************
       6000-PAGAR-TARJETA.
           DISPLAY SCR-MARCO.
           DISPLAY "[*] PAGO DE TARJETA DE CREDITO"
              LINE 04 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY SCR-DOC-BOX.
           INITIALIZE REG-CUSM.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-CAPTURAR-DOCUMENTO.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9120-BUSCAR-CLIENTE-EN-BD.
           IF WS-DOC-OK = 'N'
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "[X] Cliente inactivo. No se puede operar."
                  LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9130-MOSTRAR-CLIENTE.
           PERFORM 9140-LEER-TARJETA-CLIENTE.
           IF WS-DOC-OK = 'N'
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           IF TARJ-SALDO-UTILIZADO <= ZEROS
               DISPLAY "[i] La tarjeta no tiene saldo pendiente."
                  LINE 19 COL 02 FOREGROUND-COLOR 5 HIGHLIGHT
               DISPLAY "    No hay nada que pagar."
                  LINE 20 COL 02 FOREGROUND-COLOR 5
               PERFORM 9910-PAUSA-MENU
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Tarjeta       :" LINE 11 COL 02
              FOREGROUND-COLOR 7.
           DISPLAY TARJ-NRO-TARJETA LINE 11 COL 22
              FOREGROUND-COLOR 6 HIGHLIGHT.
           MOVE TARJ-SALDO-UTILIZADO TO WS-SALDO-EDIT.
           DISPLAY "Deuda actual  :" LINE 12 COL 02
              FOREGROUND-COLOR 7.
           DISPLAY WS-SALDO-EDIT LINE 12 COL 22
              FOREGROUND-COLOR 4 HIGHLIGHT.

           DISPLAY SCR-PAGAR-FORM.

      *> Capturar monto (loop)
           PERFORM 9350-CAPTURAR-MONTO-PAGO.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

      *> Confirmacion (loop)
           PERFORM 9340-CONFIRMAR-OPERACION.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

      *> Pasa el monto en TARJ-SALDO-UTILIZADO para la accion P
           MOVE WS-MONTO-TX TO TARJ-SALDO-UTILIZADO.

           MOVE 'P' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                                      REG-DIFD,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               MOVE WS-MONTO-TX TO WS-SALDO-EDIT
               DISPLAY "[OK] Pago registrado exitosamente."
                  LINE 18 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT
               DISPLAY "    Monto pagado: " LINE 19 COL 02
                  FOREGROUND-COLOR 7
               DISPLAY WS-SALDO-EDIT LINE 19 COL 22
                  FOREGROUND-COLOR 6 HIGHLIGHT
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY "[X] No se pudo registrar el pago."
                  LINE 18 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY LK-MENSAJE LINE 19 COL 02
                  FOREGROUND-COLOR 4
           END-IF.

           PERFORM 9910-PAUSA-MENU.

      ******************************************************************
      * 9100 - CAPTURAR DOCUMENTO (loop con X o vacio = cancelar)
      ******************************************************************
       9100-CAPTURAR-DOCUMENTO.
           MOVE 'N' TO WS-DOC-OK.
           MOVE SPACES TO WS-DOC-ENTRADA.

           PERFORM UNTIL WS-DOC-OK NOT = 'N'
               MOVE SPACES TO WS-DOC-ENTRADA
               ACCEPT WS-DOC-ENTRADA LINE 07 COL 25

               EVALUATE TRUE
                   WHEN WS-DOC-ENTRADA = SPACES
                       MOVE 'C' TO WS-DOC-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN WS-DOC-ENTRADA(1:1) = "X"
                     OR WS-DOC-ENTRADA(1:1) = "x"
                       MOVE 'C' TO WS-DOC-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN OTHER
                       PERFORM 9150-CLASIFICAR-DOC
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 9150 - Clasifica documento (CED 10 / PAS 6-12)
      ******************************************************************
       9150-CLASIFICAR-DOC.
           MOVE 12 TO WS-I.
           PERFORM UNTIL WS-I = 0
                      OR WS-DOC-ENTRADA(WS-I:1) NOT = SPACE
               SUBTRACT 1 FROM WS-I
           END-PERFORM.
           MOVE WS-I TO WS-DOC-LEN.

           EVALUATE TRUE
               WHEN WS-DOC-LEN = 10
                   IF WS-DOC-ENTRADA(1:10) IS NUMERIC
                       PERFORM 9400-VALIDAR-MODULO-10
                       IF WS-MOD10-OK = 'S'
                           MOVE 'CED' TO CUSM-TIPO-DOC
                           MOVE 'S' TO WS-DOC-OK
                           DISPLAY WS-BLANK-LINE LINE 19 COL 02
                       ELSE
                           DISPLAY "[!] Cedula invalida (modulo 10)."
                              LINE 19 COL 02
                              FOREGROUND-COLOR 4 HIGHLIGHT
                       END-IF
                   ELSE
                       MOVE 'PAS' TO CUSM-TIPO-DOC
                       MOVE 'S' TO WS-DOC-OK
                       DISPLAY WS-BLANK-LINE LINE 19 COL 02
                   END-IF
               WHEN WS-DOC-LEN >= 6 AND WS-DOC-LEN <= 12
                   MOVE 'PAS' TO CUSM-TIPO-DOC
                   MOVE 'S' TO WS-DOC-OK
                   DISPLAY WS-BLANK-LINE LINE 19 COL 02
               WHEN OTHER
                   DISPLAY "[!] Doc invalido: CED=10, PAS=6-12 digitos."
                      LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
           END-EVALUATE.

      ******************************************************************
      * 9120 - Buscar cliente en BD usando WS-DOC-ENTRADA
      ******************************************************************
       9120-BUSCAR-CLIENTE-EN-BD.
           MOVE 'N' TO WS-DOC-OK.
           MOVE WS-DOC-ENTRADA TO CUSM-DOC-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           EVALUATE TRUE
               WHEN LK-ERROR-NODATA
                   DISPLAY "[X] Cliente no encontrado."
                      LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               WHEN NOT LK-EXITO
                   DISPLAY "[X] Error tecnico al buscar cliente."
                      LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                   DISPLAY LK-MENSAJE LINE 20 COL 02
                      FOREGROUND-COLOR 4
               WHEN OTHER
                   MOVE 'S' TO WS-DOC-OK
           END-EVALUATE.

      ******************************************************************
      * 9130 - Mostrar nombre del cliente arriba del area de trabajo
      ******************************************************************
       9130-MOSTRAR-CLIENTE.
           MOVE CUSM-ID-CLIENTE TO WS-CLI-ID-FMT.
           DISPLAY "Cliente : " LINE 10 COL 02 FOREGROUND-COLOR 3.
           DISPLAY CUSM-NOMBRE-CLIENTE LINE 10 COL 13
              FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY CUSM-APELLIDOS-CLIENTE LINE 10 COL 39
              FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY "ID: " LINE 10 COL 65 FOREGROUND-COLOR 3.
           DISPLAY WS-CLI-ID-FMT LINE 10 COL 69
              FOREGROUND-COLOR 6 HIGHLIGHT.

      ******************************************************************
      * 9140 - Leer tarjeta del cliente (todas las opciones la usan)
      ******************************************************************
       9140-LEER-TARJETA-CLIENTE.
           MOVE 'N' TO WS-DOC-OK.
           MOVE CUSM-ID-CLIENTE TO TARJ-ID-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                                      REG-DIFD,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           EVALUATE TRUE
               WHEN LK-ERROR-NODATA
                   DISPLAY "[X] El cliente no tiene tarjeta."
                      LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               WHEN NOT LK-EXITO
                   DISPLAY "[X] Error al leer la tarjeta."
                      LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                   DISPLAY LK-MENSAJE LINE 20 COL 02
                      FOREGROUND-COLOR 4
               WHEN OTHER
                   MOVE 'S' TO WS-DOC-OK
           END-EVALUATE.

      ******************************************************************
      * 9200 - Generar numero de tarjeta de 16 digitos
      ******************************************************************
       9200-GENERAR-NRO-TARJETA.
           ACCEPT WS-SEMILLA FROM TIME.
           COMPUTE WS-RANDOM = FUNCTION MOD(WS-SEMILLA, 10000).
           STRING "4555" CUSM-ID-CLIENTE WS-RANDOM
               DELIMITED BY SIZE
               INTO TARJ-NRO-TARJETA.

      ******************************************************************
      * 9300 - CAPTURAR CUENTA PARA DEBITO (emision) - LOOP
      ******************************************************************
       9300-CAPTURAR-CUENTA-DEBITO.
           MOVE 'N' TO WS-CUENTA-OK.

           PERFORM UNTIL WS-CUENTA-OK NOT = 'N'
               MOVE SPACES TO WS-ENTRADA-CUENTA
               ACCEPT WS-ENTRADA-CUENTA LINE 13 COL 26

               EVALUATE TRUE
                   WHEN WS-ENTRADA-CUENTA = SPACES
                       MOVE 'C' TO WS-CUENTA-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN WS-ENTRADA-CUENTA(1:1) = "X"
                     OR WS-ENTRADA-CUENTA(1:1) = "x"
                       MOVE 'C' TO WS-CUENTA-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN WS-ENTRADA-CUENTA IS NOT NUMERIC
                       DISPLAY
                       "[!] Solo numeros (ID de cuenta del cliente)."
                          LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                   WHEN OTHER
                       COMPUTE WS-ID-CUENTA-PRINC =
                          FUNCTION NUMVAL(WS-ENTRADA-CUENTA)
                       PERFORM 9305-VERIFICAR-CUENTA
               END-EVALUATE
           END-PERFORM.

       9305-VERIFICAR-CUENTA.
           MOVE WS-ID-CUENTA-PRINC TO INVM-ID-CUENTA.
           MOVE CUSM-ID-CLIENTE    TO INVM-ID-CLIENTE.

           MOVE 'S' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           EVALUATE TRUE
               WHEN LK-ERROR-NODATA
                   DISPLAY "[!] La cuenta no existe o no es suya."
                      LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               WHEN NOT LK-EXITO
                   DISPLAY LK-MENSAJE LINE 19 COL 02
                      FOREGROUND-COLOR 4 HIGHLIGHT
               WHEN INVM-ESTADO-CUENTA NOT = 'A'
                   DISPLAY "[!] La cuenta seleccionada no esta activa."
                      LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               WHEN INVM-TIPO-CUENTA = 'H'
                   DISPLAY "[!] No se permite cuenta hipotecaria."
                      LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               WHEN OTHER
                   MOVE 'S' TO WS-CUENTA-OK
                   DISPLAY WS-BLANK-LINE LINE 19 COL 02
           END-EVALUATE.

      ******************************************************************
      * 9310 - CAPTURAR CUPO (emision) - LOOP
      ******************************************************************
       9310-CAPTURAR-CUPO-EMISION.
           MOVE 'N' TO WS-MONTO-OK.

           PERFORM UNTIL WS-MONTO-OK NOT = 'N'
               MOVE SPACES TO WS-ENTRADA-MONTO
               ACCEPT WS-ENTRADA-MONTO LINE 14 COL 26

               EVALUATE TRUE
                   WHEN WS-ENTRADA-MONTO = SPACES
                       MOVE 'C' TO WS-MONTO-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN WS-ENTRADA-MONTO(1:1) = "X"
                     OR WS-ENTRADA-MONTO(1:1) = "x"
                       MOVE 'C' TO WS-MONTO-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN OTHER
                       COMPUTE WS-MONTO-TX =
                          FUNCTION NUMVAL(WS-ENTRADA-MONTO)
                       IF WS-MONTO-TX <= ZEROS
                           DISPLAY
                           "[!] El cupo debe ser mayor a cero."
                              LINE 19 COL 02
                              FOREGROUND-COLOR 4 HIGHLIGHT
                       ELSE
                           MOVE 'S' TO WS-MONTO-OK
                           DISPLAY WS-BLANK-LINE LINE 19 COL 02
                       END-IF
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 9320 - CAPTURAR MONTO DIFERIDO - LOOP
      ******************************************************************
       9320-CAPTURAR-MONTO-DIFERIDO.
           MOVE 'N' TO WS-MONTO-OK.

           PERFORM UNTIL WS-MONTO-OK NOT = 'N'
               MOVE SPACES TO WS-ENTRADA-MONTO
               ACCEPT WS-ENTRADA-MONTO LINE 13 COL 26

               EVALUATE TRUE
                   WHEN WS-ENTRADA-MONTO = SPACES
                       MOVE 'C' TO WS-MONTO-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN WS-ENTRADA-MONTO(1:1) = "X"
                     OR WS-ENTRADA-MONTO(1:1) = "x"
                       MOVE 'C' TO WS-MONTO-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN OTHER
                       COMPUTE WS-MONTO-TX =
                          FUNCTION NUMVAL(WS-ENTRADA-MONTO)
                       EVALUATE TRUE
                           WHEN WS-MONTO-TX <= ZEROS
                               DISPLAY
                               "[!] El monto debe ser mayor a cero."
                                  LINE 19 COL 02
                                  FOREGROUND-COLOR 4 HIGHLIGHT
                           WHEN WS-MONTO-TX > WS-CUPO-DISPONIBLE
                               MOVE WS-CUPO-DISPONIBLE TO WS-SALDO-EDIT
                               DISPLAY
                               "[!] Supera el cupo. Disponible:"
                                  LINE 19 COL 02
                                  FOREGROUND-COLOR 4 HIGHLIGHT
                               DISPLAY WS-SALDO-EDIT LINE 19 COL 35
                                  FOREGROUND-COLOR 6 HIGHLIGHT
                           WHEN OTHER
                               MOVE 'S' TO WS-MONTO-OK
                               DISPLAY WS-BLANK-LINE LINE 19 COL 02
                       END-EVALUATE
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 9330 - CAPTURAR CUOTAS - LOOP (1 a 36)
      ******************************************************************
       9330-CAPTURAR-CUOTAS.
           MOVE 'N' TO WS-CUOTAS-OK.

           PERFORM UNTIL WS-CUOTAS-OK NOT = 'N'
               MOVE SPACES TO WS-ENTRADA-CUOTAS
               ACCEPT WS-ENTRADA-CUOTAS LINE 14 COL 26

               EVALUATE TRUE
                   WHEN WS-ENTRADA-CUOTAS = SPACES
                       MOVE 'C' TO WS-CUOTAS-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN WS-ENTRADA-CUOTAS(1:1) = "X"
                     OR WS-ENTRADA-CUOTAS(1:1) = "x"
                       MOVE 'C' TO WS-CUOTAS-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN OTHER
                       COMPUTE WS-CUOTAS-TX =
                          FUNCTION NUMVAL(WS-ENTRADA-CUOTAS)
                       IF WS-CUOTAS-TX < 1 OR WS-CUOTAS-TX > 36
                           DISPLAY
                           "[!] Cuotas invalidas. Rango: 1 a 36."
                              LINE 19 COL 02
                              FOREGROUND-COLOR 4 HIGHLIGHT
                       ELSE
                           MOVE 'S' TO WS-CUOTAS-OK
                           DISPLAY WS-BLANK-LINE LINE 19 COL 02
                       END-IF
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 9340 - CONFIRMAR OPERACION (S/N) - LOOP
      ******************************************************************
       9340-CONFIRMAR-OPERACION.
           MOVE 'N' TO WS-CONFIRM-OK.

           PERFORM UNTIL WS-CONFIRM-OK NOT = 'N'
               DISPLAY "Confirmar la operacion (S/N): "
                  LINE 17 COL 02 FOREGROUND-COLOR 6 HIGHLIGHT
               MOVE SPACES TO WS-CONFIRMAR
               ACCEPT WS-CONFIRMAR LINE 17 COL 33

               EVALUATE TRUE
                   WHEN WS-CONFIRMAR = 'S' OR WS-CONFIRMAR = 's'
                       MOVE 'S' TO WS-CONFIRM-OK
                       DISPLAY WS-BLANK-LINE LINE 19 COL 02
                   WHEN WS-CONFIRMAR = 'N' OR WS-CONFIRMAR = 'n'
                     OR WS-CONFIRMAR = 'X' OR WS-CONFIRMAR = 'x'
                     OR WS-CONFIRMAR = SPACE
                       MOVE 'C' TO WS-CONFIRM-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN OTHER
                       DISPLAY "[!] Responda S para si o N para no."
                          LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 9350 - CAPTURAR MONTO DE PAGO - LOOP
      ******************************************************************
       9350-CAPTURAR-MONTO-PAGO.
           MOVE 'N' TO WS-MONTO-OK.

           PERFORM UNTIL WS-MONTO-OK NOT = 'N'
               MOVE SPACES TO WS-ENTRADA-MONTO
               ACCEPT WS-ENTRADA-MONTO LINE 13 COL 26

               EVALUATE TRUE
                   WHEN WS-ENTRADA-MONTO = SPACES
                       MOVE 'C' TO WS-MONTO-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN WS-ENTRADA-MONTO(1:1) = "X"
                     OR WS-ENTRADA-MONTO(1:1) = "x"
                       MOVE 'C' TO WS-MONTO-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN OTHER
                       COMPUTE WS-MONTO-TX =
                          FUNCTION NUMVAL(WS-ENTRADA-MONTO)
                       EVALUATE TRUE
                           WHEN WS-MONTO-TX <= ZEROS
                               DISPLAY
                               "[!] El monto debe ser mayor a cero."
                                  LINE 19 COL 02
                                  FOREGROUND-COLOR 4 HIGHLIGHT
                           WHEN WS-MONTO-TX > TARJ-SALDO-UTILIZADO
                               MOVE TARJ-SALDO-UTILIZADO
                                  TO WS-SALDO-EDIT
                               DISPLAY
                               "[!] Supera la deuda. Deuda actual:"
                                  LINE 19 COL 02
                                  FOREGROUND-COLOR 4 HIGHLIGHT
                               DISPLAY WS-SALDO-EDIT LINE 19 COL 38
                                  FOREGROUND-COLOR 6 HIGHLIGHT
                           WHEN OTHER
                               MOVE 'S' TO WS-MONTO-OK
                               DISPLAY WS-BLANK-LINE LINE 19 COL 02
                       END-EVALUATE
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 9400 - VALIDAR CEDULA ECUATORIANA (MODULO 10)
      ******************************************************************
       9400-VALIDAR-MODULO-10.
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

           COMPUTE WS-DIGITO-CALC =
               FUNCTION MOD(WS-SUMA-MOD10, 10).

           IF WS-DIGITO-CALC NOT = 0
               COMPUTE WS-DIGITO-CALC = 10 - WS-DIGITO-CALC
           END-IF.

           IF WS-DIGITO-CALC = WS-DIGITO-VERIF
               MOVE 'S' TO WS-MOD10-OK
           END-IF.

      ******************************************************************
      * 9900 - Aviso de cancelacion (operador escribio X o vacio)
      ******************************************************************
       9900-AVISO-CANCELO.
           DISPLAY "[i] Operacion cancelada por el usuario."
              LINE 21 COL 02 FOREGROUND-COLOR 5 HIGHLIGHT.
           DISPLAY "Presione ENTER para volver al menu..."
              LINE 23 COL 02.
           ACCEPT WS-CONFIRMAR LINE 23 COL 41.

      ******************************************************************
      * 9910 - Pausa estandar antes de volver al menu
      ******************************************************************
       9910-PAUSA-MENU.
           DISPLAY "Presione ENTER para volver al menu..."
              LINE 23 COL 02.
           ACCEPT WS-CONFIRMAR LINE 23 COL 41.

       END PROGRAM TC0000.

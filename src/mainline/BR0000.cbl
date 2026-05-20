      ******************************************************************
      * BR0000.CBL - MODULO DE HIPOTECAS
      * v4.0 - Layout con cajas + cancelar con 'X' o vacio
      *      + validaciones en bucle + colores consistentes con CI0000
      * NOTA: El debito automatico mensual lo ejecuta BAT000.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BR0000.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPCION-HIP            PIC 9(01) VALUE ZERO.
       01  WS-CONTINUAR-HIP         PIC X(01) VALUE 'S'.
       01  WS-CONFIRMAR             PIC X(01).
       01  WS-BLANK-LINE            PIC X(75) VALUE SPACES.

       01  WS-DATOS-CLIENTE.
           05 WS-NOMBRE-CLIENTE     PIC X(25).
           05 WS-APELLIDO-CLIENTE   PIC X(25).

       01  WS-RESUMEN-CREDITO.
           05 WS-TOTAL-FINANCIADO   PIC S9(13)V99 VALUE ZERO.
           05 WS-INTERES-TOTAL      PIC S9(13)V99 VALUE ZERO.

           COPY BORMREC.
           COPY CUSMREC.
           COPY INVMREC.

       01  WS-PROGRAMAS.
           05 WS-PGM-DBIOCUSM       PIC X(08) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOBORM       PIC X(08) VALUE 'DBIOBORM'.
           05 WS-PGM-DBIOINVM       PIC X(08) VALUE 'DBIOINVM'.
           05 WS-PGM-TRAN           PIC X(08) VALUE 'DBIOTRAN'.

       01  WS-ENTRADAS.
           05 WS-ENTRADA-TXT        PIC X(15).
           05 WS-MONTO-PAGO         PIC S9(13)V99 VALUE ZERO.
           05 WS-PLAZO-MESES        PIC 9(03)     VALUE ZERO.
           05 WS-CUENTA-DEBITO      PIC 9(08)     VALUE ZERO.

       01  WS-CALCULOS.
           05 WS-TASA-MENSUAL       PIC S9(03)V9(9) VALUE ZERO.
           05 WS-FACTOR-BASE        PIC S9(07)V9(9) VALUE ZERO.
           05 WS-FACTOR-POT         PIC S9(07)V9(9) VALUE ZERO.
           05 WS-IDX-POT            PIC 9(03)       VALUE ZERO.
           05 WS-NUMERADOR          PIC S9(13)V9(9) VALUE ZERO.
           05 WS-DENOMINADOR        PIC S9(13)V9(9) VALUE ZERO.

       01  WS-FECHA-HOY             PIC 9(08).
       01  WS-FECHA-WORK            REDEFINES WS-FECHA-HOY.
           05 WS-ANIO               PIC 9(04).
           05 WS-MES                PIC 9(02).
           05 WS-DIA                PIC 9(02).

       01  WS-LOG-LINE              PIC X(200).
       01  WS-MODULO-LOG            PIC X(03) VALUE 'HIP'.
       01  WS-PERIODO-LOG           PIC X(06).
       01  WS-AUX-HIP               PIC 9(09).
       01  WS-AUX-CLI               PIC 9(08).
       01  WS-AUX-MONTO             PIC -9(13).99.

       01  WS-FMT-MONTO             PIC ZZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-FMT-MONTO-S           PIC ZZZ,ZZ9.99.
       01  WS-FMT-TASA              PIC ZZ9.99.
       01  WS-FMT-CUOTA             PIC ZZZ,ZZZ,ZZ9.99.

       01  WS-FECHA-EDITADA.
           05 WS-ED-ANIO            PIC 9(04).
           05 FILLER                PIC X(01) VALUE "-".
           05 WS-ED-MES             PIC 9(02).
           05 FILLER                PIC X(01) VALUE "-".
           05 WS-ED-DIA             PIC 9(02).

       01  WS-CALC-FECHA.
           05 WS-ANIO-ADIC          PIC 9(03) VALUE ZERO.
           05 WS-MES-REMAN          PIC 9(02) VALUE ZERO.
           05 WS-CALC-ANIO          PIC 9(04) VALUE ZERO.
           05 WS-CALC-MES           PIC 9(02) VALUE ZERO.

       01  WS-VALIDACION.
           05 WS-VAL-OK             PIC X(01) VALUE 'N'.
              88 VAL-OK             VALUE 'S'.
              88 VAL-ERROR          VALUE 'N'.
           05 WS-IDX                PIC 9(02) VALUE ZERO.
           05 WS-CARACTER           PIC X(01).
           05 WS-PUNTOS             PIC 9(02) VALUE ZERO.
           05 WS-DIGITOS            PIC 9(02) VALUE ZERO.

       01  WS-FLAGS.
           05 WS-DOC-OK             PIC X VALUE 'N'.
           05 WS-CUENTA-OK          PIC X VALUE 'N'.
           05 WS-MONTO-OK           PIC X VALUE 'N'.
           05 WS-PLAZO-OK           PIC X VALUE 'N'.
           05 WS-CANCELO            PIC X VALUE 'N'.
           05 WS-MONTO-PAGO-OK      PIC X VALUE 'N'.

       01  WS-DOC-LEN               PIC 9(02) VALUE 0.
       01  WS-I                     PIC 9(02) VALUE 0.

       LINKAGE SECTION.
           COPY LKCIF.

       SCREEN SECTION.
      *> Colores: 0=Negro 1=Azul 2=Verde 3=Cyan
      *>          4=Rojo 5=Magenta 6=Amarillo 7=Blanco

      *> ----- Cabecera: barra de titulo + operador -----
       01  SCR-MARCO.
           05 BLANK SCREEN BACKGROUND-COLOR 0.
           05 LINE 01 COL 01 BACKGROUND-COLOR 1 FOREGROUND-COLOR 7
              HIGHLIGHT VALUE
              "       BANCO LAF - MODULO DE HIPOTECAS v4.0     ".
           05 LINE 02 COL 02 FOREGROUND-COLOR 3 VALUE "Operador: ".
           05 LINE 02 COL 12 PIC X(08) FROM LKCIF-USUARIO
              FOREGROUND-COLOR 7 HIGHLIGHT.
           05 LINE 02 COL 35 FOREGROUND-COLOR 3 VALUE "Terminal: ".
           05 LINE 02 COL 45 PIC X(04) FROM LKCIF-TERMINAL
              FOREGROUND-COLOR 7 HIGHLIGHT.
           05 LINE 03 COL 02 FOREGROUND-COLOR 3 VALUE
              "=================================================".

      *> ----- Menu de hipotecas -----
       01  SCR-MENU-HIP.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------- [ MODULO DE HIPOTECAS ] ----------+".
           05 LINE 06 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 06 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[1]".
           05 LINE 07 COL 09 FOREGROUND-COLOR 7 VALUE
              "Registrar nuevo prestamo hipotecario".
           05 LINE 07 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[2]".
           05 LINE 08 COL 09 FOREGROUND-COLOR 7 VALUE
              "Consultar estado de hipoteca".
           05 LINE 08 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[3]".
           05 LINE 09 COL 09 FOREGROUND-COLOR 7 VALUE
              "Procesar pago manual de cuota".
           05 LINE 09 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[4]".
           05 LINE 11 COL 09 FOREGROUND-COLOR 3 VALUE
              "Volver al menu principal".
           05 LINE 11 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".
           05 LINE 14 COL 02 FOREGROUND-COLOR 3 VALUE "Opcion: [ ]".
           05 SCR-HIP-OPC LINE 14 COL 11 PIC 9
              USING WS-OPCION-HIP REQUIRED
              FOREGROUND-COLOR 7 HIGHLIGHT.

      *> ----- Caja para buscar cliente (alta) -----
       01  SCR-BUSCAR-CLI.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+--------- [ REGISTRAR PRESTAMO HIPOTECARIO ] -+".
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
           05 LINE 10 COL 02 FOREGROUND-COLOR 5 VALUE
              "[i] Tip: 'X' o vacio = cancelar.".

      *> ----- Caja para buscar hipoteca (consulta / pago) -----
       01  SCR-BUSCAR-HIP.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------- [ BUSCAR HIPOTECA ] --------------+".
           05 LINE 06 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 06 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 04 FOREGROUND-COLOR 6 VALUE
              "Documento (X=salir):".
           05 LINE 07 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 04 FOREGROUND-COLOR 6 VALUE
              "Nro hipoteca      :".
           05 LINE 08 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".
           05 LINE 11 COL 02 FOREGROUND-COLOR 5 VALUE
              "[i] Tip: 'X' o vacio = cancelar.".

      *> ----- Caja del formulario de alta -----
       01  SCR-ALTA-FORM.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+------- [ DATOS DEL PRESTAMO HIPOTECARIO ] ----+".
           05 LINE 06 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 06 COL 51 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 04 FOREGROUND-COLOR 7 VALUE
              "Cliente          :".
           05 LINE 07 COL 51 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 04 FOREGROUND-COLOR 7 VALUE
              "Nro Hipoteca     :".
           05 LINE 08 COL 51 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 51 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 04 FOREGROUND-COLOR 6 VALUE
              "Cuenta debito (X=salir):".
           05 LINE 10 COL 51 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 04 FOREGROUND-COLOR 6 VALUE
              "Monto prestamo   :".
           05 LINE 11 COL 51 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 04 FOREGROUND-COLOR 6 VALUE
              "Plazo (meses)    :".
           05 LINE 12 COL 51 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 04 FOREGROUND-COLOR 7 VALUE
              "Tasa anual %     :".
           05 LINE 13 COL 51 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 51 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 15 COL 02 FOREGROUND-COLOR 3 VALUE
              "+--------------------------------------------------+".
           05 LINE 16 COL 02 FOREGROUND-COLOR 5 VALUE
              "[i] Tip: 'X' o vacio en cualquier campo = cancelar.".

       PROCEDURE DIVISION USING LK-DATOS-SESION
                                LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           ACCEPT WS-FECHA-HOY FROM DATE YYYYMMDD.
           STRING WS-ANIO DELIMITED SIZE
                  WS-MES  DELIMITED SIZE
                  INTO WS-PERIODO-LOG
           END-STRING.
           MOVE SPACES TO WS-LOG-LINE.
           STRING '>>> INICIO MODULO HIPOTECAS - USR: '
                      DELIMITED BY SIZE
                  LKCIF-USUARIO DELIMITED BY SIZE
               INTO WS-LOG-LINE.
           PERFORM 9800-LOG-WRITE.
           MOVE 'S' TO WS-CONTINUAR-HIP.
           PERFORM 1000-MENU-HIP
               UNTIL WS-CONTINUAR-HIP = 'N'.
           MOVE '<<< FIN MODULO HIPOTECAS' TO WS-LOG-LINE.
           PERFORM 9800-LOG-WRITE.
           GOBACK.

      ******************************************************************
      * MENU PRINCIPAL
      ******************************************************************
       1000-MENU-HIP.
           MOVE 0 TO WS-OPCION-HIP.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-MENU-HIP.
           ACCEPT SCR-HIP-OPC.

           EVALUATE WS-OPCION-HIP
               WHEN 1 PERFORM 2100-REGISTRAR-ALTA
               WHEN 2 PERFORM 2200-CONSULTAR-HIPOTECA
               WHEN 3 PERFORM 2300-PROCESAR-PAGO
               WHEN 4 MOVE 'N' TO WS-CONTINUAR-HIP
               WHEN OTHER
                   DISPLAY "[!] Opcion no valida (1-4)."
                      LINE 17 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                   DISPLAY "Presione ENTER..." LINE 23 COL 02
                   ACCEPT WS-CONFIRMAR LINE 23 COL 20
           END-EVALUATE.

      ******************************************************************
      * ALTA DE PRESTAMO HIPOTECARIO
      ******************************************************************
       2100-REGISTRAR-ALTA.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-BUSCAR-CLI.
           INITIALIZE REG-BORM.
           INITIALIZE REG-CUSM.
           MOVE SPACES TO WS-NOMBRE-CLIENTE.
           MOVE SPACES TO WS-APELLIDO-CLIENTE.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9000-CAPTURAR-DOCUMENTO.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           SET ACCION-SELECT TO TRUE.
           CALL WS-PGM-DBIOCUSM
               USING REG-CUSM,
                     LK-DATOS-SESION,
                     LK-DATOS-TRANSACCION.

           IF NOT LK-EXITO
               DISPLAY "[X] Cliente no encontrado."
                  LINE 11 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "    " LK-MENSAJE LINE 12 COL 02
                  FOREGROUND-COLOR 4
               DISPLAY "Presione ENTER..." LINE 23 COL 02
               ACCEPT WS-CONFIRMAR LINE 23 COL 20
               EXIT PARAGRAPH
           END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "[X] RECHAZADA: el cliente esta INACTIVO."
                  LINE 11 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "Presione ENTER..." LINE 23 COL 02
               ACCEPT WS-CONFIRMAR LINE 23 COL 20
               EXIT PARAGRAPH
           END-IF.

           MOVE CUSM-NOMBRE-CLIENTE    TO WS-NOMBRE-CLIENTE.
           MOVE CUSM-APELLIDOS-CLIENTE TO WS-APELLIDO-CLIENTE.
           MOVE CUSM-ID-CLIENTE        TO BORM-ID-CLIENTE.

           SET ACCION-SECUENCIA TO TRUE.
           CALL WS-PGM-DBIOBORM
               USING REG-BORM,
                     LK-DATOS-SESION,
                     LK-DATOS-TRANSACCION.

           IF NOT LK-EXITO
               DISPLAY "[X] Error generando nro de hipoteca."
                  LINE 11 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "    " LK-MENSAJE LINE 12 COL 02
                  FOREGROUND-COLOR 4
               DISPLAY "Presione ENTER..." LINE 23 COL 02
               ACCEPT WS-CONFIRMAR LINE 23 COL 20
               EXIT PARAGRAPH
           END-IF.

           PERFORM 2110-CAPTURAR-DATOS-ALTA.

      ******************************************************************
      * 2110 - CAPTURA DE DATOS DEL PRESTAMO (VALIDACIONES EN BUCLE)
      * En cualquier campo: 'X' o vacio = cancela la operacion.
      ******************************************************************
       2110-CAPTURAR-DATOS-ALTA.
           MOVE 'N' TO WS-CUENTA-OK.
           MOVE 'N' TO WS-MONTO-OK.
           MOVE 'N' TO WS-PLAZO-OK.

           DISPLAY SCR-MARCO.
           DISPLAY SCR-ALTA-FORM.
           DISPLAY WS-NOMBRE-CLIENTE    LINE 07 COL 23
               FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY WS-APELLIDO-CLIENTE  LINE 07 COL 49
               FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY BORM-ID-HIPOTECA     LINE 08 COL 23
               FOREGROUND-COLOR 6 HIGHLIGHT.

      *> CUENTA DEBITO: numero de 8 digitos, activa, del cliente
           PERFORM UNTIL WS-CUENTA-OK NOT = 'N'
               MOVE ZERO TO WS-CUENTA-DEBITO
               ACCEPT WS-CUENTA-DEBITO LINE 10 COL 28
               EVALUATE TRUE
                   WHEN WS-CUENTA-DEBITO = ZERO
                       MOVE 'C' TO WS-CUENTA-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN OTHER
                       PERFORM 2115-VALIDAR-CUENTA-DEBITO
                       IF LK-EXITO
                           MOVE 'S' TO WS-CUENTA-OK
                           DISPLAY WS-BLANK-LINE LINE 19 COL 02
                       ELSE
                           DISPLAY "[!] " LK-MENSAJE
                              LINE 19 COL 02
                              FOREGROUND-COLOR 4 HIGHLIGHT
                       END-IF
               END-EVALUATE
           END-PERFORM.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

      *> MONTO PRESTAMO: numero positivo
           PERFORM UNTIL WS-MONTO-OK NOT = 'N'
               MOVE SPACES TO WS-ENTRADA-TXT
               ACCEPT WS-ENTRADA-TXT LINE 11 COL 23
               EVALUATE TRUE
                   WHEN WS-ENTRADA-TXT = SPACES
                       MOVE 'C' TO WS-MONTO-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN WS-ENTRADA-TXT(1:1) = 'X'
                     OR WS-ENTRADA-TXT(1:1) = 'x'
                       MOVE 'C' TO WS-MONTO-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN OTHER
                       PERFORM 9300-VALIDAR-NUMERO
                       IF VAL-ERROR
                           DISPLAY "[!] Monto invalido (solo digitos)."
                           LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                       ELSE
                           COMPUTE BORM-MONTO-PRESTAMO =
                               FUNCTION NUMVAL(WS-ENTRADA-TXT)
                           IF BORM-MONTO-PRESTAMO <= 0
                               DISPLAY "[!] Monto debe ser > 0."
                                  LINE 19 COL 02
                                  FOREGROUND-COLOR 4 HIGHLIGHT
                           ELSE
                               MOVE 'S' TO WS-MONTO-OK
                               DISPLAY WS-BLANK-LINE LINE 19 COL 02
                           END-IF
                       END-IF
               END-EVALUATE
           END-PERFORM.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

      *> PLAZO: entre 1 y 360 meses
           PERFORM UNTIL WS-PLAZO-OK NOT = 'N'
               MOVE ZERO TO WS-PLAZO-MESES
               ACCEPT WS-PLAZO-MESES LINE 12 COL 23
               EVALUATE TRUE
                   WHEN WS-PLAZO-MESES = ZERO
                       MOVE 'C' TO WS-PLAZO-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN WS-PLAZO-MESES > 360
                       DISPLAY "[!] Plazo entre 1 y 360 meses."
                          LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                   WHEN OTHER
                       MOVE 'S' TO WS-PLAZO-OK
                       DISPLAY WS-BLANK-LINE LINE 19 COL 02
               END-EVALUATE
           END-PERFORM.
           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           PERFORM 9130-CALCULAR-TASA.
           MOVE BORM-TASA-ANUAL TO WS-FMT-TASA.
           DISPLAY WS-FMT-TASA LINE 13 COL 23
               FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "(asignada por plazo)" LINE 13 COL 31
               FOREGROUND-COLOR 3.

           PERFORM 9110-CALCULAR-VENCIMIENTO.
           PERFORM 9120-CALCULAR-CUOTA.
           PERFORM 2140-GUARDAR-HIPOTECA.

      ******************************************************************
      * 2115 - VALIDAR CUENTA DEBITO (activa, no hipotecaria, del cliente)
      ******************************************************************
       2115-VALIDAR-CUENTA-DEBITO.
           INITIALIZE REG-INVM.
           MOVE WS-CUENTA-DEBITO TO INVM-ID-CUENTA.
           MOVE CUSM-ID-CLIENTE  TO INVM-ID-CLIENTE.
           MOVE 'S' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM
               USING REG-INVM,
                     LK-DATOS-SESION,
                     LK-DATOS-TRANSACCION.

           IF LK-EXITO
               IF INVM-ESTADO-CUENTA NOT = 'A'
                   MOVE 'E001' TO LK-COD-RETORNO
                   MOVE "Cuenta no esta activa" TO LK-MENSAJE
               END-IF
           END-IF.

           IF LK-EXITO
               IF INVM-TIPO-CUENTA = 'H'
                   MOVE 'E001' TO LK-COD-RETORNO
                   MOVE "Cuenta hipotecaria no es valida"
                       TO LK-MENSAJE
               END-IF
           END-IF.

           IF NOT LK-EXITO
               IF LK-ERROR-NODATA
                   MOVE "Cuenta no pertenece al cliente"
                       TO LK-MENSAJE
               END-IF
           END-IF.

      ******************************************************************
      * 2140 - GUARDAR HIPOTECA EN BD
      ******************************************************************
       2140-GUARDAR-HIPOTECA.
           MOVE WS-CUENTA-DEBITO    TO BORM-CUENTA-DEBITO.
           MOVE BORM-MONTO-PRESTAMO TO BORM-SALDO-DEUDA.
           MOVE "ACTIVO"            TO BORM-ESTADO-PRESTAMO.
           MOVE ZERO                TO BORM-MESES-MORA.

           SET ACCION-INSERT TO TRUE.
           CALL WS-PGM-DBIOBORM
               USING REG-BORM,
                     LK-DATOS-SESION,
                     LK-DATOS-TRANSACCION.

           IF LK-EXITO
               CALL WS-PGM-TRAN USING 'C'
               MOVE BORM-ID-HIPOTECA    TO WS-AUX-HIP
               MOVE BORM-ID-CLIENTE     TO WS-AUX-CLI
               MOVE BORM-MONTO-PRESTAMO TO WS-AUX-MONTO
               MOVE SPACES TO WS-LOG-LINE
               STRING 'ALTA HIPOTECA OK - HIP: '
                          DELIMITED BY SIZE
                      WS-AUX-HIP   DELIMITED BY SIZE
                      ' CLI: '     DELIMITED BY SIZE
                      WS-AUX-CLI   DELIMITED BY SIZE
                      ' MONTO: '   DELIMITED BY SIZE
                      WS-AUX-MONTO DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9800-LOG-WRITE
               COMPUTE WS-TOTAL-FINANCIADO =
                   BORM-CUOTA-MENSUAL * WS-PLAZO-MESES
               COMPUTE WS-INTERES-TOTAL =
                   WS-TOTAL-FINANCIADO - BORM-MONTO-PRESTAMO
               PERFORM 2150-MOSTRAR-RESULTADO-ALTA
           ELSE
               CALL WS-PGM-TRAN USING 'R'
               MOVE SPACES TO WS-LOG-LINE
               STRING 'ALTA HIPOTECA FALLO - MOTIVO: '
                          DELIMITED BY SIZE
                      LK-MENSAJE DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9800-LOG-WRITE
               DISPLAY "[X] ERROR - Operacion reversada."
                  LINE 18 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "    " LK-MENSAJE
                  LINE 19 COL 02 FOREGROUND-COLOR 4
           END-IF.

           DISPLAY "Presione ENTER para volver al menu..."
              LINE 23 COL 02.
           ACCEPT WS-CONFIRMAR LINE 23 COL 41.

      ******************************************************************
      * 2150 - CAJA DE RESULTADO DE ALTA EXITOSA
      ******************************************************************
       2150-MOSTRAR-RESULTADO-ALTA.
           DISPLAY "+------ [ HIPOTECA REGISTRADA EXITOSAMENTE ] ---+"
              LINE 17 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 18 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "Nro Hipoteca   :" LINE 18 COL 04 FOREGROUND-COLOR 7.
           DISPLAY BORM-ID-HIPOTECA   LINE 18 COL 21
               FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "|" LINE 18 COL 49 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 19 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           MOVE BORM-CUOTA-MENSUAL TO WS-FMT-CUOTA.
           DISPLAY "Cuota mensual  : $" LINE 19 COL 04
               FOREGROUND-COLOR 7.
           DISPLAY WS-FMT-CUOTA LINE 19 COL 22
               FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "|" LINE 19 COL 49 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 20 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           MOVE WS-INTERES-TOTAL TO WS-FMT-MONTO.
           DISPLAY "Interes total  : $" LINE 20 COL 04
               FOREGROUND-COLOR 7.
           DISPLAY WS-FMT-MONTO LINE 20 COL 22
               FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "|" LINE 20 COL 49 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 21 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "Vencimiento    :" LINE 21 COL 04 FOREGROUND-COLOR 7.
           DISPLAY BORM-FECHA-VENCIMIENTO LINE 21 COL 21
               FOREGROUND-COLOR 7.
           DISPLAY "|" LINE 21 COL 49 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "+-----------------------------------------------+"
              LINE 22 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "[i] El batch descontara la cuota mensualmente."
              LINE 23 COL 02 FOREGROUND-COLOR 3.

      ******************************************************************
      * CONSULTA DE HIPOTECA
      ******************************************************************
       2200-CONSULTAR-HIPOTECA.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-BUSCAR-HIP.
           INITIALIZE REG-BORM.
           INITIALIZE REG-CUSM.
           MOVE SPACES TO WS-NOMBRE-CLIENTE.
           MOVE SPACES TO WS-APELLIDO-CLIENTE.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9010-BUSCAR-HIPOTECA.

           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           IF NOT LK-EXITO
               DISPLAY "[X] " LK-MENSAJE
                  LINE 12 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "Presione ENTER..." LINE 23 COL 02
               ACCEPT WS-CONFIRMAR LINE 23 COL 20
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "+---------- [ ESTADO DE LA HIPOTECA ] ----------+"
              LINE 12 COL 02 FOREGROUND-COLOR 3.
           DISPLAY "Nro Hipoteca   :" LINE 13 COL 04 FOREGROUND-COLOR 7.
           DISPLAY BORM-ID-HIPOTECA   LINE 13 COL 21
               FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "Cliente        :" LINE 14 COL 04 FOREGROUND-COLOR 7.
           DISPLAY WS-NOMBRE-CLIENTE  LINE 14 COL 21
               FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY WS-APELLIDO-CLIENTE LINE 14 COL 47
               FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY "Cuenta debito  :" LINE 15 COL 04 FOREGROUND-COLOR 7.
           DISPLAY BORM-CUENTA-DEBITO LINE 15 COL 21 FOREGROUND-COLOR 7.
           MOVE BORM-MONTO-PRESTAMO TO WS-FMT-MONTO.
           DISPLAY "Monto prestamo : $" LINE 16 COL 04
               FOREGROUND-COLOR 7.
           DISPLAY WS-FMT-MONTO     LINE 16 COL 22
               FOREGROUND-COLOR 6 HIGHLIGHT.
           MOVE BORM-SALDO-DEUDA TO WS-FMT-MONTO.
           DISPLAY "Saldo deuda    : $" LINE 17 COL 04
               FOREGROUND-COLOR 7.
           DISPLAY WS-FMT-MONTO     LINE 17 COL 22
               FOREGROUND-COLOR 6 HIGHLIGHT.
           MOVE BORM-TASA-ANUAL TO WS-FMT-TASA.
           DISPLAY "Tasa anual     :" LINE 18 COL 04 FOREGROUND-COLOR 7.
           DISPLAY WS-FMT-TASA   LINE 18 COL 21 FOREGROUND-COLOR 7.
           DISPLAY "%" LINE 18 COL 28 FOREGROUND-COLOR 7.
           MOVE BORM-CUOTA-MENSUAL TO WS-FMT-CUOTA.
           DISPLAY "Cuota mensual  : $" LINE 19 COL 04
               FOREGROUND-COLOR 7.
           DISPLAY WS-FMT-CUOTA   LINE 19 COL 22
               FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "Meses en mora  :" LINE 20 COL 04 FOREGROUND-COLOR 7.
           DISPLAY BORM-MESES-MORA LINE 20 COL 21 FOREGROUND-COLOR 7.
           DISPLAY "Vencimiento    :" LINE 21 COL 04 FOREGROUND-COLOR 7.
           DISPLAY BORM-FECHA-VENCIMIENTO LINE 21 COL 21
               FOREGROUND-COLOR 7.
           DISPLAY "Estado         :" LINE 22 COL 04 FOREGROUND-COLOR 7.
           DISPLAY BORM-ESTADO-PRESTAMO LINE 22 COL 21
               FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "+-----------------------------------------------+"
              LINE 23 COL 02 FOREGROUND-COLOR 3.

           IF HIPO-PAGADO
               DISPLAY "[OK] HIPOTECA TOTALMENTE SALDADA."
                  LINE 24 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT
           END-IF.
           IF HIPO-MOROSO
               DISPLAY "[!]  ATENCION: HIPOTECA EN MORA."
                  LINE 24 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
           END-IF.

           DISPLAY "Presione ENTER para volver al menu..."
              LINE 25 COL 02.
           ACCEPT WS-CONFIRMAR LINE 25 COL 41.

      ******************************************************************
      * PAGO MANUAL DE CUOTA
      ******************************************************************
       2300-PROCESAR-PAGO.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-BUSCAR-HIP.
           INITIALIZE REG-BORM.
           INITIALIZE REG-CUSM.
           MOVE SPACES TO WS-NOMBRE-CLIENTE.
           MOVE SPACES TO WS-APELLIDO-CLIENTE.
           MOVE 'N' TO WS-CANCELO.
           MOVE 'N' TO WS-MONTO-PAGO-OK.

           PERFORM 9010-BUSCAR-HIPOTECA.

           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           IF NOT LK-EXITO
               DISPLAY "[X] " LK-MENSAJE
                  LINE 12 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "Presione ENTER..." LINE 23 COL 02
               ACCEPT WS-CONFIRMAR LINE 23 COL 20
               EXIT PARAGRAPH
           END-IF.

           IF HIPO-PAGADO
               DISPLAY "[!] RECHAZADA: la hipoteca ya esta pagada."
                  LINE 12 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "Presione ENTER..." LINE 23 COL 02
               ACCEPT WS-CONFIRMAR LINE 23 COL 20
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "+------------ [ PAGO MANUAL DE CUOTA ] ---------+"
              LINE 12 COL 02 FOREGROUND-COLOR 3.
           DISPLAY "Cliente       :" LINE 13 COL 04 FOREGROUND-COLOR 7.
           DISPLAY WS-NOMBRE-CLIENTE LINE 13 COL 20
               FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY "Nro hipoteca  :" LINE 14 COL 04 FOREGROUND-COLOR 7.
           DISPLAY BORM-ID-HIPOTECA  LINE 14 COL 20
               FOREGROUND-COLOR 6 HIGHLIGHT.
           MOVE BORM-SALDO-DEUDA TO WS-FMT-MONTO.
           DISPLAY "Saldo deuda   : $" LINE 15 COL 04 FOREGROUND-COLOR 7.
           DISPLAY WS-FMT-MONTO    LINE 15 COL 21
               FOREGROUND-COLOR 6 HIGHLIGHT.
           MOVE BORM-CUOTA-MENSUAL TO WS-FMT-CUOTA.
           DISPLAY "Cuota mensual : $" LINE 16 COL 04 FOREGROUND-COLOR 7.
           DISPLAY WS-FMT-CUOTA    LINE 16 COL 21
               FOREGROUND-COLOR 6 HIGHLIGHT.
           IF HIPO-MOROSO
               DISPLAY "[!] EN MORA - Meses: " LINE 17 COL 04
                  FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY BORM-MESES-MORA LINE 17 COL 26
                  FOREGROUND-COLOR 4 HIGHLIGHT
           END-IF.
           DISPLAY "+-----------------------------------------------+"
              LINE 18 COL 02 FOREGROUND-COLOR 3.

      *> MONTO A PAGAR: validar en bucle hasta valor correcto
           PERFORM UNTIL WS-MONTO-PAGO-OK NOT = 'N'
               MOVE SPACES TO WS-ENTRADA-TXT
               DISPLAY "Monto a pagar : $" LINE 20 COL 04
                  FOREGROUND-COLOR 6 HIGHLIGHT
               ACCEPT WS-ENTRADA-TXT LINE 20 COL 21

               EVALUATE TRUE
                   WHEN WS-ENTRADA-TXT = SPACES
                       MOVE 'C' TO WS-MONTO-PAGO-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN WS-ENTRADA-TXT(1:1) = 'X'
                     OR WS-ENTRADA-TXT(1:1) = 'x'
                       MOVE 'C' TO WS-MONTO-PAGO-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN OTHER
                       PERFORM 9300-VALIDAR-NUMERO
                       PERFORM 9250-VALIDAR-MONTO-PAGO
               END-EVALUATE
           END-PERFORM.

           IF WS-CANCELO = 'S'
               PERFORM 9900-AVISO-CANCELO
               EXIT PARAGRAPH
           END-IF.

           PERFORM 2310-APLICAR-PAGO.

      ******************************************************************
      * 2305 - DEBITAR CUENTA DEL CLIENTE
      ******************************************************************
       2305-DEBITAR-CUENTA.
           INITIALIZE REG-INVM.
           MOVE BORM-CUENTA-DEBITO TO INVM-ID-CUENTA.
           MOVE BORM-ID-CLIENTE    TO INVM-ID-CLIENTE.
           MOVE WS-MONTO-PAGO      TO INVM-SALDO-ACTUAL.
           MOVE 'R' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM
               USING REG-INVM,
                     LK-DATOS-SESION,
                     LK-DATOS-TRANSACCION.

      ******************************************************************
      * 2310 - APLICAR PAGO A LA HIPOTECA
      ******************************************************************
       2310-APLICAR-PAGO.
           PERFORM 2305-DEBITAR-CUENTA.

           IF NOT LK-EXITO
               CALL WS-PGM-TRAN USING 'R'
               MOVE BORM-ID-HIPOTECA TO WS-AUX-HIP
               MOVE WS-MONTO-PAGO    TO WS-AUX-MONTO
               MOVE SPACES TO WS-LOG-LINE
               STRING 'PAGO HIPOTECA RECHAZADO - HIP: '
                          DELIMITED BY SIZE
                      WS-AUX-HIP   DELIMITED BY SIZE
                      ' MONTO: '   DELIMITED BY SIZE
                      WS-AUX-MONTO DELIMITED BY SIZE
                      ' MOTIVO: '  DELIMITED BY SIZE
                      LK-MENSAJE   DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9800-LOG-WRITE
               DISPLAY "[X] Pago rechazado: " LK-MENSAJE
                  LINE 23 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "Presione ENTER..." LINE 24 COL 02
               ACCEPT WS-CONFIRMAR LINE 24 COL 20
               EXIT PARAGRAPH
           END-IF.

           SUBTRACT WS-MONTO-PAGO FROM BORM-SALDO-DEUDA.
           MOVE 'N' TO WS-VAL-OK.

           EVALUATE TRUE
               WHEN BORM-SALDO-DEUDA = 0
                   MOVE "PAGADO" TO BORM-ESTADO-PRESTAMO
                   MOVE ZERO     TO BORM-MESES-MORA
               WHEN WS-MONTO-PAGO >= BORM-CUOTA-MENSUAL
                   MOVE "ACTIVO" TO BORM-ESTADO-PRESTAMO
                   MOVE ZERO     TO BORM-MESES-MORA
               WHEN OTHER
                   MOVE 'S' TO WS-VAL-OK
           END-EVALUATE.

           SET ACCION-UPDATE TO TRUE.
           CALL WS-PGM-DBIOBORM
               USING REG-BORM,
                     LK-DATOS-SESION,
                     LK-DATOS-TRANSACCION.

           IF LK-EXITO
               CALL WS-PGM-TRAN USING 'C'
               MOVE BORM-ID-HIPOTECA TO WS-AUX-HIP
               MOVE WS-MONTO-PAGO    TO WS-AUX-MONTO
               MOVE SPACES TO WS-LOG-LINE
               STRING 'PAGO HIPOTECA OK - HIP: '
                          DELIMITED BY SIZE
                      WS-AUX-HIP   DELIMITED BY SIZE
                      ' MONTO: '   DELIMITED BY SIZE
                      WS-AUX-MONTO DELIMITED BY SIZE
                      ' EST: '     DELIMITED BY SIZE
                      BORM-ESTADO-PRESTAMO DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9800-LOG-WRITE
               PERFORM 2320-MOSTRAR-RESULTADO-PAGO
           ELSE
               CALL WS-PGM-TRAN USING 'R'
               MOVE SPACES TO WS-LOG-LINE
               STRING 'PAGO HIPOTECA FALLO - MOTIVO: '
                          DELIMITED BY SIZE
                      LK-MENSAJE DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9800-LOG-WRITE
               DISPLAY "[X] ERROR - Operacion reversada."
                  LINE 23 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
               DISPLAY "    " LK-MENSAJE
                  LINE 24 COL 02 FOREGROUND-COLOR 4
           END-IF.

           DISPLAY "Presione ENTER para volver al menu..."
              LINE 25 COL 02.
           ACCEPT WS-CONFIRMAR LINE 25 COL 41.

      ******************************************************************
      * 2320 - CAJA DE RESULTADO DE PAGO EXITOSO
      ******************************************************************
       2320-MOSTRAR-RESULTADO-PAGO.
           DISPLAY "+---------- [ PAGO PROCESADO EXITOSAMENTE ] ----+"
              LINE 23 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           MOVE WS-MONTO-PAGO TO WS-FMT-MONTO.
           DISPLAY "Monto pagado  : $" LINE 24 COL 04 FOREGROUND-COLOR 7.
           DISPLAY WS-FMT-MONTO LINE 24 COL 21
               FOREGROUND-COLOR 6 HIGHLIGHT.
           MOVE BORM-SALDO-DEUDA TO WS-FMT-MONTO.
           DISPLAY "Saldo restante: $" LINE 25 COL 04 FOREGROUND-COLOR 7.
           DISPLAY WS-FMT-MONTO LINE 25 COL 21
               FOREGROUND-COLOR 6 HIGHLIGHT.
           IF VAL-OK
               DISPLAY "[i] Pago parcial: BAT000 evaluara la mora."
                  LINE 26 COL 02 FOREGROUND-COLOR 5
           END-IF.
           IF HIPO-PAGADO
               DISPLAY "[OK] HIPOTECA TOTALMENTE PAGADA."
                  LINE 26 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT
           END-IF.

      ******************************************************************
      * 9000 - CAPTURA DEL DOCUMENTO (X o vacio = cancelar)
      ******************************************************************
       9000-CAPTURAR-DOCUMENTO.
           MOVE 'N' TO WS-DOC-OK.

           PERFORM UNTIL WS-DOC-OK NOT = 'N'
               MOVE SPACES TO CUSM-DOC-CLIENTE
               ACCEPT CUSM-DOC-CLIENTE LINE 07 COL 25

               EVALUATE TRUE
                   WHEN CUSM-DOC-CLIENTE = SPACES
                       MOVE 'C' TO WS-DOC-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN CUSM-DOC-CLIENTE = "X"
                     OR CUSM-DOC-CLIENTE = "x"
                       MOVE 'C' TO WS-DOC-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN OTHER
                       PERFORM 9050-CLASIFICAR-DOC
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 9010 - BUSCAR HIPOTECA (documento + nro hipoteca)
      ******************************************************************
       9010-BUSCAR-HIPOTECA.
           MOVE 'N' TO WS-DOC-OK.

      *> Fase 1: documento del cliente
           PERFORM UNTIL WS-DOC-OK NOT = 'N'
               MOVE SPACES TO CUSM-DOC-CLIENTE
               ACCEPT CUSM-DOC-CLIENTE LINE 07 COL 25

               EVALUATE TRUE
                   WHEN CUSM-DOC-CLIENTE = SPACES
                       MOVE 'C' TO WS-DOC-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN CUSM-DOC-CLIENTE = "X"
                     OR CUSM-DOC-CLIENTE = "x"
                       MOVE 'C' TO WS-DOC-OK
                       MOVE 'S' TO WS-CANCELO
                   WHEN OTHER
                       PERFORM 9050-CLASIFICAR-DOC
               END-EVALUATE
           END-PERFORM.

           IF WS-CANCELO = 'S'
               EXIT PARAGRAPH
           END-IF.

           SET ACCION-SELECT TO TRUE.
           CALL WS-PGM-DBIOCUSM
               USING REG-CUSM,
                     LK-DATOS-SESION,
                     LK-DATOS-TRANSACCION.

           IF NOT LK-EXITO
               EXIT PARAGRAPH
           END-IF.

           MOVE CUSM-NOMBRE-CLIENTE    TO WS-NOMBRE-CLIENTE.
           MOVE CUSM-APELLIDOS-CLIENTE TO WS-APELLIDO-CLIENTE.

      *> Fase 2: numero de hipoteca
           MOVE SPACES TO WS-ENTRADA-TXT.
           ACCEPT WS-ENTRADA-TXT LINE 08 COL 25.

           EVALUATE TRUE
               WHEN WS-ENTRADA-TXT = SPACES
                   MOVE 'S' TO WS-CANCELO
                   EXIT PARAGRAPH
               WHEN WS-ENTRADA-TXT(1:1) = 'X'
                 OR WS-ENTRADA-TXT(1:1) = 'x'
                   MOVE 'S' TO WS-CANCELO
                   EXIT PARAGRAPH
               WHEN OTHER
                   PERFORM 9300-VALIDAR-NUMERO
                   IF VAL-ERROR
                       MOVE 'E404' TO LK-COD-RETORNO
                       MOVE "Numero de hipoteca invalido" TO LK-MENSAJE
                       EXIT PARAGRAPH
                   END-IF
           END-EVALUATE.

           COMPUTE BORM-ID-HIPOTECA =
               FUNCTION NUMVAL(WS-ENTRADA-TXT).

           IF BORM-ID-HIPOTECA = ZERO
               MOVE 'E404' TO LK-COD-RETORNO
               MOVE "Numero de hipoteca invalido" TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF.

           SET ACCION-SELECT TO TRUE.
           CALL WS-PGM-DBIOBORM
               USING REG-BORM,
                     LK-DATOS-SESION,
                     LK-DATOS-TRANSACCION.

           IF LK-EXITO
               IF BORM-ID-CLIENTE NOT = CUSM-ID-CLIENTE
                   MOVE 'E404' TO LK-COD-RETORNO
                   MOVE "Hipoteca no pertenece al cliente"
                       TO LK-MENSAJE
               END-IF
           END-IF.

      ******************************************************************
      * 9050 - CLASIFICAR DOCUMENTO (CED 10 digitos / PAS 6-12)
      ******************************************************************
       9050-CLASIFICAR-DOC.
           MOVE 12 TO WS-I.
           PERFORM UNTIL WS-I = 0
                      OR CUSM-DOC-CLIENTE(WS-I:1) NOT = SPACE
               SUBTRACT 1 FROM WS-I
           END-PERFORM.
           MOVE WS-I TO WS-DOC-LEN.

           EVALUATE TRUE
               WHEN WS-DOC-LEN = 10
                   IF CUSM-DOC-CLIENTE(1:10) IS NUMERIC
                       MOVE 'CED' TO CUSM-TIPO-DOC
                   ELSE
                       MOVE 'PAS' TO CUSM-TIPO-DOC
                   END-IF
                   MOVE 'S' TO WS-DOC-OK
                   DISPLAY WS-BLANK-LINE LINE 19 COL 02
               WHEN WS-DOC-LEN >= 6 AND WS-DOC-LEN <= 12
                   MOVE 'PAS' TO CUSM-TIPO-DOC
                   MOVE 'S' TO WS-DOC-OK
                   DISPLAY WS-BLANK-LINE LINE 19 COL 02
               WHEN OTHER
                   DISPLAY "[!] Doc invalido: CED=10 dig, PAS=6-12."
                      LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
           END-EVALUATE.

      ******************************************************************
      * 9110 - CALCULAR FECHA DE VENCIMIENTO
      ******************************************************************
       9110-CALCULAR-VENCIMIENTO.
           MOVE WS-ANIO TO WS-CALC-ANIO.
           MOVE WS-MES  TO WS-CALC-MES.

           ADD WS-PLAZO-MESES TO WS-CALC-MES.

           DIVIDE WS-CALC-MES BY 12
               GIVING    WS-ANIO-ADIC
               REMAINDER WS-MES-REMAN.

           IF WS-MES-REMAN = 0
               SUBTRACT 1 FROM WS-ANIO-ADIC
               MOVE 12 TO WS-CALC-MES
           ELSE
               MOVE WS-MES-REMAN TO WS-CALC-MES
           END-IF.

           ADD WS-ANIO-ADIC TO WS-CALC-ANIO.

           MOVE WS-CALC-ANIO TO WS-ED-ANIO.
           MOVE WS-CALC-MES  TO WS-ED-MES.
           MOVE 28            TO WS-ED-DIA.
           MOVE WS-FECHA-EDITADA TO BORM-FECHA-VENCIMIENTO.

      ******************************************************************
      * 9120 - CALCULAR CUOTA MENSUAL (AMORTIZACION FRANCESA)
      ******************************************************************
       9120-CALCULAR-CUOTA.
           COMPUTE WS-TASA-MENSUAL =
               BORM-TASA-ANUAL / 1200.

           COMPUTE WS-FACTOR-BASE = 1 + WS-TASA-MENSUAL.
           MOVE 1 TO WS-FACTOR-POT.

           PERFORM VARYING WS-IDX-POT FROM 1 BY 1
               UNTIL WS-IDX-POT > WS-PLAZO-MESES
               COMPUTE WS-FACTOR-POT =
                   WS-FACTOR-POT * WS-FACTOR-BASE
           END-PERFORM.

           COMPUTE WS-NUMERADOR =
               BORM-MONTO-PRESTAMO *
               WS-TASA-MENSUAL *
               WS-FACTOR-POT.

           COMPUTE WS-DENOMINADOR = WS-FACTOR-POT - 1.

           IF WS-DENOMINADOR = 0
               COMPUTE BORM-CUOTA-MENSUAL =
                   BORM-MONTO-PRESTAMO / WS-PLAZO-MESES
           ELSE
               COMPUTE BORM-CUOTA-MENSUAL =
                   WS-NUMERADOR / WS-DENOMINADOR
           END-IF.

      ******************************************************************
      * 9130 - TABLA DE TASAS POR PLAZO (modelo de negocio v3)
      *   meses <  3 => 2%  |  meses < 7 => 4%  |  meses >= 7 => 9%
      ******************************************************************
       9130-CALCULAR-TASA.
           EVALUATE TRUE
               WHEN WS-PLAZO-MESES < 3
                   MOVE 2 TO BORM-TASA-ANUAL
               WHEN WS-PLAZO-MESES < 7
                   MOVE 4 TO BORM-TASA-ANUAL
               WHEN OTHER
                   MOVE 9 TO BORM-TASA-ANUAL
           END-EVALUATE.

      ******************************************************************
      * 9250 - VALIDAR MONTO DE PAGO (llamado desde el loop en 2300)
      ******************************************************************
       9250-VALIDAR-MONTO-PAGO.
           PERFORM 9300-VALIDAR-NUMERO.
           IF VAL-ERROR
               DISPLAY "[!] Monto invalido (solo digitos)."
                  LINE 22 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
           ELSE
               COMPUTE WS-MONTO-PAGO =
                   FUNCTION NUMVAL(WS-ENTRADA-TXT)
               EVALUATE TRUE
                   WHEN WS-MONTO-PAGO <= 0
                       DISPLAY "[!] Monto debe ser > 0."
                          LINE 22 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                   WHEN WS-MONTO-PAGO > BORM-SALDO-DEUDA
                       DISPLAY "[!] Pago excede el saldo de la deuda."
                          LINE 22 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                   WHEN WS-MONTO-PAGO > BORM-CUOTA-MENSUAL
                       DISPLAY "[!] Pago excede la cuota mensual."
                          LINE 22 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                   WHEN OTHER
                       MOVE 'S' TO WS-MONTO-PAGO-OK
                       DISPLAY WS-BLANK-LINE LINE 22 COL 02
               END-EVALUATE
           END-IF.

      ******************************************************************
      * 9300 - VALIDAR NUMERO (digitos y un solo punto decimal)
      ******************************************************************
       9300-VALIDAR-NUMERO.
           MOVE 'S' TO WS-VAL-OK.
           MOVE ZERO TO WS-PUNTOS.
           MOVE ZERO TO WS-DIGITOS.

           IF WS-ENTRADA-TXT = SPACES
               MOVE 'N' TO WS-VAL-OK
           ELSE
               PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > 15
                   MOVE WS-ENTRADA-TXT(WS-IDX:1) TO WS-CARACTER
                   IF WS-CARACTER NOT = SPACE
                       IF WS-CARACTER >= '0' AND WS-CARACTER <= '9'
                           ADD 1 TO WS-DIGITOS
                       ELSE
                           IF WS-CARACTER = '.'
                               ADD 1 TO WS-PUNTOS
                               IF WS-PUNTOS > 1
                                   MOVE 'N' TO WS-VAL-OK
                               END-IF
                           ELSE
                               MOVE 'N' TO WS-VAL-OK
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF WS-DIGITOS = 0
                   MOVE 'N' TO WS-VAL-OK
               END-IF
           END-IF.

      ******************************************************************
      * 9800 - ESCRIBIR EN LOGFILE
      ******************************************************************
       9800-LOG-WRITE.
           CALL 'LOGFILE' USING WS-MODULO-LOG,
                                WS-PERIODO-LOG,
                                WS-LOG-LINE.

      ******************************************************************
      * 9900 - AVISO DE CANCELACION
      ******************************************************************
       9900-AVISO-CANCELO.
           DISPLAY "[i] Operacion cancelada por el usuario."
              LINE 20 COL 02 FOREGROUND-COLOR 5 HIGHLIGHT.
           DISPLAY "Presione ENTER para volver al menu..."
              LINE 23 COL 02.
           ACCEPT WS-CONFIRMAR LINE 23 COL 41.

       END PROGRAM BR0000.

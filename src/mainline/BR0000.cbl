       IDENTIFICATION DIVISION.
       PROGRAM-ID. BR0000.
      *================================================================*
      * PROGRAMA : BR0000.cbl                                          *
      * MODULO   : HIPOTECAS                                           *
      * FUNCION  : Alta, consulta y pago manual de cuota.              *
      * NOTA     : El debito automatico mensual lo ejecuta BAT000.     *
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPTION              PIC 9(01) VALUE ZERO.
       01  WS-CONTINUAR           PIC X(01) VALUE 'S'.
       01  WS-PUEDE-CONTINUAR     PIC X(01) VALUE 'S'.

       01  WS-DATOS-CLIENTE.
           05 WS-NOMBRE-CLIENTE   PIC X(25).
           05 WS-APELLIDO-CLIENTE PIC X(25).

       01  WS-RESUMEN-CREDITO.
           05 WS-TOTAL-FINANCIADO PIC S9(13)V99 VALUE ZERO.
           05 WS-INTERES-TOTAL    PIC S9(13)V99 VALUE ZERO.

           COPY BORMREC.
           COPY CUSMREC.

       01  WS-PROGRAMAS.
           05 WS-PGM-DBIOCUSM     PIC X(8) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOBORM     PIC X(8) VALUE 'DBIOBORM'.
           05 WS-PGM-TRAN         PIC X(8) VALUE 'DBIOTRAN'.

       01  WS-ENTRADAS.
           05 WS-ENTRADA-TXT      PIC X(15).
           05 WS-MONTO-PAGO       PIC S9(13)V99 VALUE ZERO.
           05 WS-PLAZO-MESES      PIC 9(03)     VALUE ZERO.
           05 WS-CUENTA-DEBITO    PIC 9(08)     VALUE ZERO.

       01  WS-CALCULOS.
           05 WS-TASA-MENSUAL     PIC S9(03)V9(9) VALUE ZERO.
           05 WS-FACTOR-BASE      PIC S9(07)V9(9) VALUE ZERO.
           05 WS-FACTOR-POT       PIC S9(07)V9(9) VALUE ZERO.
           05 WS-IDX-POT          PIC 9(03)       VALUE ZERO.
           05 WS-NUMERADOR        PIC S9(13)V9(9) VALUE ZERO.
           05 WS-DENOMINADOR      PIC S9(13)V9(9) VALUE ZERO.

       01  WS-FECHA-HOY           PIC 9(08).
       01  WS-FECHA-WORK          REDEFINES WS-FECHA-HOY.
           05 WS-ANIO             PIC 9(04).
           05 WS-MES              PIC 9(02).
           05 WS-DIA              PIC 9(02).

      *    Variables para LOGFILE
       01  WS-LOG-LINE            PIC X(200).
       01  WS-MODULO-LOG          PIC X(03) VALUE 'HIP'.
       01  WS-PERIODO-LOG         PIC X(06).
       01  WS-AUX-HIP             PIC 9(09).
       01  WS-AUX-CLI             PIC 9(08).
       01  WS-AUX-MONTO           PIC -9(13).99.

       01  WS-FECHA-EDITADA.
           05 WS-ED-ANIO          PIC 9(04).
           05 FILLER              PIC X(01) VALUE "-".
           05 WS-ED-MES           PIC 9(02).
           05 FILLER              PIC X(01) VALUE "-".
           05 WS-ED-DIA           PIC 9(02).

       01  WS-CALC-FECHA.
           05 WS-ANIO-ADIC        PIC 9(03) VALUE ZERO.
           05 WS-MES-REMAN        PIC 9(02) VALUE ZERO.
           05 WS-CALC-ANIO        PIC 9(04) VALUE ZERO.
           05 WS-CALC-MES         PIC 9(02) VALUE ZERO.

       01  WS-VALIDACION.
           05 WS-VAL-OK           PIC X(01) VALUE 'N'.
              88 VAL-OK           VALUE 'S'.
              88 VAL-ERROR        VALUE 'N'.
           05 WS-IDX              PIC 9(02) VALUE ZERO.
           05 WS-CARACTER         PIC X(01).
           05 WS-PUNTOS           PIC 9(02) VALUE ZERO.
           05 WS-DIGITOS          PIC 9(02) VALUE ZERO.

       LINKAGE SECTION.
           COPY LKCIF.

       SCREEN SECTION.
       01  SCR-MARCO.
           05 BLANK SCREEN.
           05 LINE 02 COL 02 VALUE
              "+------------------------------------------+".
           05 LINE 03 COL 02 VALUE
              "|       MODULO DE HIPOTECAS v3.0           |".
           05 LINE 04 COL 02 VALUE
              "+------------------------------------------+".

       01  SCR-MENU-HIP.
           05 LINE 07 COL 05 VALUE "1. Registrar nuevo prestamo".
           05 LINE 08 COL 05 VALUE "2. Consultar estado hipoteca".
           05 LINE 09 COL 05 VALUE "3. Procesar pago manual cuota".
           05 LINE 10 COL 05 VALUE "4. Volver al menu principal".
           05 LINE 13 COL 05 VALUE "Opcion: [ ]".
           05 SCR-HIP-OPC LINE 13 COL 14 PIC 9
              USING WS-OPTION REQUIRED.

       PROCEDURE DIVISION USING LK-DATOS-SESION
                                LK-DATOS-TRANSACCION.

       0000-MAIN.
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
           MOVE 'S' TO WS-CONTINUAR.
           PERFORM 1000-MOSTRAR-MENU
               UNTIL WS-CONTINUAR = 'N'.
           MOVE '<<< FIN MODULO HIPOTECAS' TO WS-LOG-LINE.
           PERFORM 9800-LOG-WRITE.
           GOBACK.

       9800-LOG-WRITE.
           CALL 'LOGFILE' USING WS-MODULO-LOG,
                                WS-PERIODO-LOG,
                                WS-LOG-LINE.

       1000-MOSTRAR-MENU.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-MENU-HIP.
           ACCEPT SCR-HIP-OPC.

           EVALUATE WS-OPTION
               WHEN 1
                   PERFORM 2100-REGISTRAR-ALTA
               WHEN 2
                   PERFORM 2200-CONSULTAR-HIPOTECA
               WHEN 3
                   PERFORM 2300-PROCESAR-PAGO
               WHEN 4
                   MOVE 'N' TO WS-CONTINUAR
               WHEN OTHER
                   DISPLAY "Opcion invalida. Use 1 al 4."
                      LINE 17 COL 05
                   DISPLAY "Presione ENTER para continuar."
                      LINE 24 COL 05
                   ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
           END-EVALUATE.

       2100-REGISTRAR-ALTA.
           DISPLAY SCR-MARCO.
           DISPLAY "ALTA DE PRESTAMO HIPOTECARIO"
              LINE 06 COL 05.
           MOVE 'S' TO WS-PUEDE-CONTINUAR.
           INITIALIZE REG-BORM.
           MOVE SPACES TO WS-NOMBRE-CLIENTE.
           MOVE SPACES TO WS-APELLIDO-CLIENTE.

           PERFORM 9000-BUSCAR-CLIENTE.

           IF NOT LK-EXITO
               PERFORM 9400-MSG-ERROR-CLIENTE
               MOVE 'N' TO WS-PUEDE-CONTINUAR
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               MOVE CUSM-NOMBRE-CLIENTE    TO WS-NOMBRE-CLIENTE
               MOVE CUSM-APELLIDOS-CLIENTE TO WS-APELLIDO-CLIENTE
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               IF CLIENTE-INACTIVO
                   DISPLAY "RECHAZADA: cliente INACTIVO."
                      LINE 18 COL 05
                   DISPLAY "Presione ENTER para continuar."
                      LINE 24 COL 05
                   ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

      *    NOTA: Un cliente puede tener MULTIPLES hipotecas en el
      *    modelo v3. Se elimina la validacion de hipoteca activa.
           IF WS-PUEDE-CONTINUAR = 'S'
               MOVE CUSM-ID-CLIENTE TO BORM-ID-CLIENTE
               SET ACCION-SECUENCIA TO TRUE
               CALL WS-PGM-DBIOBORM
                   USING REG-BORM,
                         LK-DATOS-SESION,
                         LK-DATOS-TRANSACCION
               IF NOT LK-EXITO
                   DISPLAY "ERROR generando numero: " LK-MENSAJE
                      LINE 18 COL 05
                   DISPLAY "Presione ENTER para continuar."
                      LINE 24 COL 05
                   ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               ACCEPT WS-FECHA-HOY FROM DATE YYYYMMDD
               PERFORM 2110-CAPTURAR-DATOS-ALTA
           END-IF.

       2110-CAPTURAR-DATOS-ALTA.
           DISPLAY SCR-MARCO.
           DISPLAY "ALTA - DATOS DEL PRESTAMO"
              LINE 06 COL 05.
           DISPLAY "Cliente : " LINE 08 COL 05.
           DISPLAY WS-NOMBRE-CLIENTE LINE 08 COL 16.
           DISPLAY WS-APELLIDO-CLIENTE LINE 08 COL 42.
           DISPLAY "Nro Hip.: " LINE 09 COL 05.
           DISPLAY BORM-ID-HIPOTECA LINE 09 COL 16.

           MOVE 'S' TO WS-PUEDE-CONTINUAR.

           DISPLAY "Cuenta debito : " LINE 12 COL 05.
           ACCEPT WS-CUENTA-DEBITO LINE 12 COL 22.
           IF WS-CUENTA-DEBITO = ZERO
               DISPLAY "ERROR: cuenta invalida."
                  LINE 22 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 24 COL 05
               ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
               MOVE 'N' TO WS-PUEDE-CONTINUAR
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Monto prestamo: " LINE 13 COL 05
               ACCEPT WS-ENTRADA-TXT LINE 13 COL 22
               PERFORM 9300-VALIDAR-NUMERO
               IF VAL-ERROR
                   DISPLAY "ERROR: monto invalido."
                      LINE 22 COL 05
                   DISPLAY "Presione ENTER para continuar."
                      LINE 24 COL 05
                   ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               ELSE
                   MOVE FUNCTION NUMVAL(WS-ENTRADA-TXT)
                       TO BORM-MONTO-PRESTAMO
                   IF BORM-MONTO-PRESTAMO <= 0
                       DISPLAY "ERROR: monto mayor a cero."
                          LINE 22 COL 05
                       DISPLAY "Presione ENTER para continuar."
                          LINE 24 COL 05
                       ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
                       MOVE 'N' TO WS-PUEDE-CONTINUAR
                   END-IF
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Tasa anual % : " LINE 14 COL 05
               ACCEPT WS-ENTRADA-TXT LINE 14 COL 22
               PERFORM 9300-VALIDAR-NUMERO
               IF VAL-ERROR
                   DISPLAY "ERROR: tasa invalida."
                      LINE 22 COL 05
                   DISPLAY "Presione ENTER para continuar."
                      LINE 24 COL 05
                   ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               ELSE
                   MOVE FUNCTION NUMVAL(WS-ENTRADA-TXT)
                       TO BORM-TASA-ANUAL
                   IF BORM-TASA-ANUAL <= 0
                       DISPLAY "ERROR: tasa mayor a cero."
                          LINE 22 COL 05
                       DISPLAY "Presione ENTER para continuar."
                          LINE 24 COL 05
                       ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
                       MOVE 'N' TO WS-PUEDE-CONTINUAR
                   END-IF
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Plazo (meses): " LINE 15 COL 05
               ACCEPT WS-PLAZO-MESES LINE 15 COL 22
               IF WS-PLAZO-MESES <= 0 OR WS-PLAZO-MESES > 360
                   DISPLAY "ERROR: plazo entre 1 y 360."
                      LINE 22 COL 05
                   DISPLAY "Presione ENTER para continuar."
                      LINE 24 COL 05
                   ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               PERFORM 9110-CALCULAR-VENCIMIENTO
               PERFORM 9120-CALCULAR-CUOTA
               PERFORM 2140-GUARDAR-HIPOTECA
           END-IF.

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

               DISPLAY SCR-MARCO
               DISPLAY "HIPOTECA REGISTRADA EXITOSAMENTE"
                  LINE 06 COL 05
               DISPLAY "Nro Hip. : " LINE 08 COL 05
               DISPLAY BORM-ID-HIPOTECA LINE 08 COL 18
               DISPLAY "Cliente  : " LINE 09 COL 05
               DISPLAY WS-NOMBRE-CLIENTE LINE 09 COL 18
               DISPLAY "Apellido : " LINE 10 COL 05
               DISPLAY WS-APELLIDO-CLIENTE LINE 10 COL 18
               DISPLAY "Prestamo : " LINE 12 COL 05
               DISPLAY BORM-MONTO-PRESTAMO LINE 12 COL 18
               DISPLAY "Tasa A.  : " LINE 13 COL 05
               DISPLAY BORM-TASA-ANUAL LINE 13 COL 18
               DISPLAY "Plazo    : " LINE 14 COL 05
               DISPLAY WS-PLAZO-MESES LINE 14 COL 18
               DISPLAY "Cuota M. : " LINE 15 COL 05
               DISPLAY BORM-CUOTA-MENSUAL LINE 15 COL 18
               DISPLAY "Total Fin: " LINE 16 COL 05
               DISPLAY WS-TOTAL-FINANCIADO LINE 16 COL 18
               DISPLAY "Cta Deb. : " LINE 17 COL 05
               DISPLAY BORM-CUENTA-DEBITO LINE 17 COL 18
               DISPLAY "Vence    : " LINE 18 COL 05
               DISPLAY BORM-FECHA-VENCIMIENTO LINE 18 COL 18
               DISPLAY "Estado   : " LINE 19 COL 05
               DISPLAY BORM-ESTADO-PRESTAMO LINE 19 COL 18
               DISPLAY "El batch descontara la cuota mensual."
                  LINE 21 COL 05
           ELSE
               CALL WS-PGM-TRAN USING 'R'
               MOVE SPACES TO WS-LOG-LINE
               STRING 'ALTA HIPOTECA FALLO - MOTIVO: '
                          DELIMITED BY SIZE
                      LK-MENSAJE DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9800-LOG-WRITE
               DISPLAY SCR-MARCO
               DISPLAY "ERROR - OPERACION REVERSADA"
                  LINE 06 COL 05
               DISPLAY "Motivo : " LINE 09 COL 05
               DISPLAY LK-MENSAJE LINE 09 COL 16
           END-IF.

           DISPLAY "Presione ENTER para continuar."
              LINE 24 COL 05.
           ACCEPT WS-ENTRADA-TXT LINE 24 COL 36.

       2200-CONSULTAR-HIPOTECA.
           DISPLAY SCR-MARCO.
           DISPLAY "CONSULTA DE HIPOTECA"
              LINE 06 COL 05.
           PERFORM 9010-BUSCAR-HIPOTECA.

           IF NOT LK-EXITO
               PERFORM 9500-MSG-ERROR-HIPOTECA
           ELSE
               DISPLAY SCR-MARCO
               DISPLAY "ESTADO DE LA HIPOTECA"
                  LINE 06 COL 05
               DISPLAY "Nro Hip. : " LINE 08 COL 05
               DISPLAY BORM-ID-HIPOTECA LINE 08 COL 18
               DISPLAY "Cliente  : " LINE 09 COL 05
               DISPLAY WS-NOMBRE-CLIENTE LINE 09 COL 18
               DISPLAY "Apellido : " LINE 10 COL 05
               DISPLAY WS-APELLIDO-CLIENTE LINE 10 COL 18
               DISPLAY "Cta Deb. : " LINE 12 COL 05
               DISPLAY BORM-CUENTA-DEBITO LINE 12 COL 18
               DISPLAY "Prestamo : " LINE 13 COL 05
               DISPLAY BORM-MONTO-PRESTAMO LINE 13 COL 18
               DISPLAY "Saldo    : " LINE 14 COL 05
               DISPLAY BORM-SALDO-DEUDA LINE 14 COL 18
               DISPLAY "Tasa A.  : " LINE 15 COL 05
               DISPLAY BORM-TASA-ANUAL LINE 15 COL 18
               DISPLAY "Cuota M. : " LINE 16 COL 05
               DISPLAY BORM-CUOTA-MENSUAL LINE 16 COL 18
               DISPLAY "Mora     : " LINE 17 COL 05
               DISPLAY BORM-MESES-MORA LINE 17 COL 18
               DISPLAY " meses" LINE 17 COL 25
               DISPLAY "Vence    : " LINE 18 COL 05
               DISPLAY BORM-FECHA-VENCIMIENTO LINE 18 COL 18
               DISPLAY "Estado   : " LINE 19 COL 05
               DISPLAY BORM-ESTADO-PRESTAMO LINE 19 COL 18
           END-IF.

           DISPLAY "Presione ENTER para continuar."
              LINE 24 COL 05.
           ACCEPT WS-ENTRADA-TXT LINE 24 COL 36.

       2300-PROCESAR-PAGO.
           DISPLAY SCR-MARCO.
           DISPLAY "PAGO MANUAL DE CUOTA"
              LINE 06 COL 05.
           MOVE 'S' TO WS-PUEDE-CONTINUAR.
           PERFORM 9010-BUSCAR-HIPOTECA.

           IF NOT LK-EXITO
               PERFORM 9500-MSG-ERROR-HIPOTECA
               MOVE 'N' TO WS-PUEDE-CONTINUAR
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               IF HIPO-PAGADO
                   DISPLAY "RECHAZADA: hipoteca ya pagada."
                      LINE 18 COL 05
                   DISPLAY "Presione ENTER para continuar."
                      LINE 24 COL 05
                   ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY SCR-MARCO
               DISPLAY "PAGO MANUAL DE CUOTA"
                  LINE 06 COL 05
               DISPLAY "Cliente      : " LINE 08 COL 05
               DISPLAY WS-NOMBRE-CLIENTE LINE 08 COL 22
               DISPLAY "Nro hipoteca : " LINE 09 COL 05
               DISPLAY BORM-ID-HIPOTECA LINE 09 COL 22
               DISPLAY "Saldo deuda  : " LINE 10 COL 05
               DISPLAY BORM-SALDO-DEUDA LINE 10 COL 22
               DISPLAY "Cuota mens.  : " LINE 11 COL 05
               DISPLAY BORM-CUOTA-MENSUAL LINE 11 COL 22
               IF HIPO-MOROSO
                   DISPLAY "AVISO: hipoteca EN MORA - "
                      LINE 13 COL 05
                   DISPLAY BORM-MESES-MORA LINE 13 COL 30
                   DISPLAY " meses" LINE 13 COL 36
               END-IF
               DISPLAY "Monto a pagar: " LINE 15 COL 05
               ACCEPT WS-ENTRADA-TXT LINE 15 COL 22
               PERFORM 9300-VALIDAR-NUMERO
               IF VAL-ERROR
                   DISPLAY "ERROR: monto invalido."
                      LINE 22 COL 05
                   DISPLAY "Presione ENTER para continuar."
                      LINE 24 COL 05
                   ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               MOVE FUNCTION NUMVAL(WS-ENTRADA-TXT)
                   TO WS-MONTO-PAGO
               IF WS-MONTO-PAGO <= 0
                   DISPLAY "ERROR: pago mayor a cero."
                      LINE 22 COL 05
                   DISPLAY "Presione ENTER para continuar."
                      LINE 24 COL 05
                   ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               IF WS-MONTO-PAGO > BORM-SALDO-DEUDA
                   DISPLAY "RECHAZADA: pago excede el saldo."
                      LINE 22 COL 05
                   DISPLAY "Presione ENTER para continuar."
                      LINE 24 COL 05
                   ACCEPT WS-ENTRADA-TXT LINE 24 COL 36
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               PERFORM 2310-APLICAR-PAGO
           END-IF.

       2310-APLICAR-PAGO.
           SUBTRACT WS-MONTO-PAGO FROM BORM-SALDO-DEUDA.

           EVALUATE TRUE
               WHEN BORM-SALDO-DEUDA = 0
                   MOVE "PAGADO" TO BORM-ESTADO-PRESTAMO
                   MOVE ZERO     TO BORM-MESES-MORA
               WHEN WS-MONTO-PAGO >= BORM-CUOTA-MENSUAL
                   MOVE "ACTIVO" TO BORM-ESTADO-PRESTAMO
                   MOVE ZERO     TO BORM-MESES-MORA
               WHEN OTHER
                   CONTINUE
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
               DISPLAY SCR-MARCO
               DISPLAY "PAGO PROCESADO EXITOSAMENTE"
                  LINE 06 COL 05
               DISPLAY "Pagado : " LINE 09 COL 05
               DISPLAY WS-MONTO-PAGO LINE 09 COL 18
               DISPLAY "Saldo  : " LINE 10 COL 05
               DISPLAY BORM-SALDO-DEUDA LINE 10 COL 18
               DISPLAY "Estado : " LINE 11 COL 05
               DISPLAY BORM-ESTADO-PRESTAMO LINE 11 COL 18
               DISPLAY "Mora   : " LINE 12 COL 05
               DISPLAY BORM-MESES-MORA LINE 12 COL 18
           ELSE
               CALL WS-PGM-TRAN USING 'R'
               MOVE SPACES TO WS-LOG-LINE
               STRING 'PAGO HIPOTECA FALLO - MOTIVO: '
                          DELIMITED BY SIZE
                      LK-MENSAJE DELIMITED BY SIZE
                   INTO WS-LOG-LINE
               PERFORM 9800-LOG-WRITE
               DISPLAY SCR-MARCO
               DISPLAY "ERROR - OPERACION REVERSADA"
                  LINE 06 COL 05
               DISPLAY "Motivo : " LINE 09 COL 05
               DISPLAY LK-MENSAJE LINE 09 COL 18
           END-IF.

           DISPLAY "Presione ENTER para continuar."
              LINE 24 COL 05.
           ACCEPT WS-ENTRADA-TXT LINE 24 COL 36.

       9000-BUSCAR-CLIENTE.
           INITIALIZE REG-CUSM.
           DISPLAY "Documento (CED 10 dig / PAS 6-12): "
              LINE 12 COL 05.
           ACCEPT CUSM-DOC-CLIENTE LINE 12 COL 40.

           MOVE FUNCTION TRIM(CUSM-DOC-CLIENTE)
               TO CUSM-DOC-CLIENTE.

           IF CUSM-DOC-CLIENTE = SPACES
               MOVE 'E404' TO LK-COD-RETORNO
               MOVE "DOCUMENTO INVALIDO" TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF.

      *    Determinar TIPO_DOC por longitud y contenido (igual CI0000):
      *    10 digitos numericos => CED, resto (6-12) => PAS.
           MOVE 12 TO WS-IDX.
           PERFORM UNTIL WS-IDX = 0
                      OR CUSM-DOC-CLIENTE(WS-IDX:1) NOT = SPACE
              SUBTRACT 1 FROM WS-IDX
           END-PERFORM.

           EVALUATE TRUE
              WHEN WS-IDX = 10
                 IF CUSM-DOC-CLIENTE(1:10) IS NUMERIC
                    MOVE 'CED' TO CUSM-TIPO-DOC
                 ELSE
                    MOVE 'PAS' TO CUSM-TIPO-DOC
                 END-IF
              WHEN WS-IDX >= 6 AND WS-IDX <= 12
                 MOVE 'PAS' TO CUSM-TIPO-DOC
              WHEN OTHER
                 MOVE 'E404' TO LK-COD-RETORNO
                 MOVE "DOCUMENTO INVALIDO (LONG 6-12)"
                    TO LK-MENSAJE
                 EXIT PARAGRAPH
           END-EVALUATE.

           SET ACCION-SELECT TO TRUE.
           CALL WS-PGM-DBIOCUSM
               USING REG-CUSM,
                     LK-DATOS-SESION,
                     LK-DATOS-TRANSACCION.
           IF LK-EXITO
              MOVE CUSM-NOMBRE-CLIENTE    TO WS-NOMBRE-CLIENTE
              MOVE CUSM-APELLIDOS-CLIENTE TO WS-APELLIDO-CLIENTE
           END-IF.

       9010-BUSCAR-HIPOTECA.
           MOVE 'S' TO WS-PUEDE-CONTINUAR.
           INITIALIZE REG-BORM.
           MOVE SPACES TO WS-NOMBRE-CLIENTE.
           MOVE SPACES TO WS-APELLIDO-CLIENTE.

           PERFORM 9000-BUSCAR-CLIENTE.

           IF NOT LK-EXITO
               MOVE 'N' TO WS-PUEDE-CONTINUAR
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Nro hipoteca  : " LINE 13 COL 05
               ACCEPT BORM-ID-HIPOTECA LINE 13 COL 22

               IF BORM-ID-HIPOTECA = ZERO
                   MOVE 'E404' TO LK-COD-RETORNO
                   MOVE "NUMERO DE HIPOTECA INVALIDO"
                       TO LK-MENSAJE
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               SET ACCION-SELECT TO TRUE
               CALL WS-PGM-DBIOBORM
                   USING REG-BORM,
                         LK-DATOS-SESION,
                         LK-DATOS-TRANSACCION

               IF LK-EXITO
                   IF BORM-ID-CLIENTE NOT = CUSM-ID-CLIENTE
                       MOVE 'E404' TO LK-COD-RETORNO
                       MOVE "HIPOTECA NO PERTENECE AL CLIENTE"
                           TO LK-MENSAJE
                   END-IF
               END-IF
           END-IF.

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

       9300-VALIDAR-NUMERO.
           MOVE 'S' TO WS-VAL-OK.
           MOVE ZERO TO WS-PUNTOS.
           MOVE ZERO TO WS-DIGITOS.

           IF WS-ENTRADA-TXT = SPACES
               MOVE 'N' TO WS-VAL-OK
           ELSE
               PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > 15
                   MOVE WS-ENTRADA-TXT(WS-IDX:1)
                       TO WS-CARACTER
                   IF WS-CARACTER NOT = SPACE
                       IF WS-CARACTER >= '0'
                       AND WS-CARACTER <= '9'
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

       9400-MSG-ERROR-CLIENTE.
           DISPLAY "ERROR - Cliente no encontrado."
              LINE 18 COL 05.
           DISPLAY "Motivo: " LINE 19 COL 05.
           DISPLAY LK-MENSAJE LINE 19 COL 13.
           DISPLAY "Verifique la cedula e intente de nuevo."
              LINE 20 COL 05.
           DISPLAY "Presione ENTER para continuar."
              LINE 24 COL 05.
           ACCEPT WS-ENTRADA-TXT LINE 24 COL 36.

       9500-MSG-ERROR-HIPOTECA.
           DISPLAY "ERROR - Hipoteca no encontrada."
              LINE 18 COL 05.
           DISPLAY "Motivo: " LINE 19 COL 05.
           DISPLAY LK-MENSAJE LINE 19 COL 13.
           DISPLAY "Verifique el numero e intente de nuevo."
              LINE 20 COL 05.
           DISPLAY "Presione ENTER para continuar."
              LINE 24 COL 05.
           ACCEPT WS-ENTRADA-TXT LINE 24 COL 36.

       9900-PAUSA.
           DISPLAY "Presione ENTER para continuar."
              LINE 24 COL 05.
           ACCEPT WS-ENTRADA-TXT LINE 24 COL 36.

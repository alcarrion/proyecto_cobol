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

       PROCEDURE DIVISION USING LK-DATOS-SESION
                                LK-DATOS-TRANSACCION.

       0000-MAIN.
           MOVE 'S' TO WS-CONTINUAR.
           PERFORM 1000-MOSTRAR-MENU
               UNTIL WS-CONTINUAR = 'N'.
           GOBACK.

       1000-MOSTRAR-MENU.
           DISPLAY " ".
           DISPLAY "====================================".
           DISPLAY "       MODULO DE HIPOTECAS          ".
           DISPLAY "====================================".
           DISPLAY "  1. Registrar nuevo prestamo".
           DISPLAY "  2. Consultar estado de hipoteca".
           DISPLAY "  3. Procesar pago manual de cuota".
           DISPLAY "  4. Volver al menu principal".
           DISPLAY "====================================".
           DISPLAY "Seleccione opcion: ".
           ACCEPT WS-OPTION.

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
                   DISPLAY "--------------------------------"
                   DISPLAY " OPCION INVALIDA. Use 1 al 4."
                   DISPLAY "--------------------------------"
           END-EVALUATE.

       2100-REGISTRAR-ALTA.
           MOVE 'S' TO WS-PUEDE-CONTINUAR.
           DISPLAY "--- ALTA PRESTAMO HIPOTECARIO ---".
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
                   DISPLAY "================================"
                   DISPLAY " OPERACION RECHAZADA"
                   DISPLAY "================================"
                   DISPLAY "MOTIVO: CLIENTE INACTIVO."
                   DISPLAY "================================"
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               IF CUSM-TIENE-HIPOTECA = 1
                   DISPLAY "================================"
                   DISPLAY " OPERACION RECHAZADA"
                   DISPLAY "================================"
                   DISPLAY "MOTIVO: YA TIENE HIPOTECA ACTIVA."
                   DISPLAY "================================"
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               MOVE CUSM-ID-CLIENTE TO BORM-ID-CLIENTE
               SET ACCION-SECUENCIA TO TRUE
               CALL WS-PGM-DBIOBORM
                   USING REG-BORM,
                         LK-DATOS-SESION,
                         LK-DATOS-TRANSACCION
               IF NOT LK-EXITO
                   DISPLAY "ERROR GENERANDO NUMERO: " LK-MENSAJE
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               ACCEPT WS-FECHA-HOY FROM DATE YYYYMMDD
               PERFORM 2110-CAPTURAR-DATOS-ALTA
           END-IF.

       2110-CAPTURAR-DATOS-ALTA.
           MOVE 'S' TO WS-PUEDE-CONTINUAR.
           DISPLAY "--------------------------------".
           DISPLAY "CLIENTE : " WS-NOMBRE-CLIENTE.
           DISPLAY "APELLIDO: " WS-APELLIDO-CLIENTE.
           DISPLAY "NRO HIP.: " BORM-ID-HIPOTECA.
           DISPLAY "--------------------------------".

           DISPLAY "Nro cuenta para debito mensual: ".
           ACCEPT WS-CUENTA-DEBITO.
           IF WS-CUENTA-DEBITO = ZERO
               DISPLAY "ERROR: NUMERO DE CUENTA INVALIDO."
               MOVE 'N' TO WS-PUEDE-CONTINUAR
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Monto del prestamo: "
               ACCEPT WS-ENTRADA-TXT
               PERFORM 9300-VALIDAR-NUMERO
               IF VAL-ERROR
                   DISPLAY "ERROR: MONTO INVALIDO."
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               MOVE FUNCTION NUMVAL(WS-ENTRADA-TXT)
                   TO BORM-MONTO-PRESTAMO
               IF BORM-MONTO-PRESTAMO <= 0
                   DISPLAY "ERROR: MONTO MAYOR A CERO."
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Tasa interes ANUAL% (ej: 12.00): "
               ACCEPT WS-ENTRADA-TXT
               PERFORM 9300-VALIDAR-NUMERO
               IF VAL-ERROR
                   DISPLAY "ERROR: TASA INVALIDA."
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               MOVE FUNCTION NUMVAL(WS-ENTRADA-TXT)
                   TO BORM-TASA-ANUAL
               IF BORM-TASA-ANUAL <= 0
                   DISPLAY "ERROR: TASA MAYOR A CERO."
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Plazo en meses (max 360): "
               ACCEPT WS-PLAZO-MESES
               IF WS-PLAZO-MESES <= 0 OR WS-PLAZO-MESES > 360
                   DISPLAY "ERROR: PLAZO ENTRE 1 Y 360 MESES."
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

               COMPUTE WS-TOTAL-FINANCIADO =
                   BORM-CUOTA-MENSUAL * WS-PLAZO-MESES
               COMPUTE WS-INTERES-TOTAL =
                   WS-TOTAL-FINANCIADO - BORM-MONTO-PRESTAMO

               DISPLAY "================================"
               DISPLAY " HIPOTECA REGISTRADA OK"
               DISPLAY "================================"
               DISPLAY "NRO HIP. : " BORM-ID-HIPOTECA
               DISPLAY "CLIENTE  : " WS-NOMBRE-CLIENTE
               DISPLAY "APELLIDO : " WS-APELLIDO-CLIENTE
               DISPLAY "PRESTAMO : " BORM-MONTO-PRESTAMO
               DISPLAY "TASA A.  : " BORM-TASA-ANUAL " %"
               DISPLAY "PLAZO    : " WS-PLAZO-MESES " meses"
               DISPLAY "CUOTA M. : " BORM-CUOTA-MENSUAL
               DISPLAY "INTERES  : " WS-INTERES-TOTAL
               DISPLAY "TOTAL    : " WS-TOTAL-FINANCIADO
               DISPLAY "VENCE    : " BORM-FECHA-VENCIMIENTO
               DISPLAY "CTA DEB. : " BORM-CUENTA-DEBITO
               DISPLAY "ESTADO   : " BORM-ESTADO-PRESTAMO
               DISPLAY "================================"
               DISPLAY "El batch descontara la cuota"
               DISPLAY "mensualmente de la cuenta."
               DISPLAY "================================"
           ELSE
               CALL WS-PGM-TRAN USING 'R'
               DISPLAY "================================"
               DISPLAY " ERROR - OPERACION REVERSADA"
               DISPLAY "================================"
               DISPLAY "MOTIVO: " LK-MENSAJE
               DISPLAY "================================"
           END-IF.

       2200-CONSULTAR-HIPOTECA.
           DISPLAY "--- CONSULTA DE HIPOTECA ---".
           PERFORM 9010-BUSCAR-HIPOTECA.

           IF NOT LK-EXITO
               PERFORM 9500-MSG-ERROR-HIPOTECA
           ELSE
               DISPLAY "================================"
               DISPLAY "     ESTADO DE HIPOTECA"
               DISPLAY "================================"
               DISPLAY "NRO HIP. : " BORM-ID-HIPOTECA
               DISPLAY "CLIENTE  : " WS-NOMBRE-CLIENTE
               DISPLAY "APELLIDO : " WS-APELLIDO-CLIENTE
               DISPLAY "CTA DEB. : " BORM-CUENTA-DEBITO
               DISPLAY "PRESTAMO : " BORM-MONTO-PRESTAMO
               DISPLAY "SALDO    : " BORM-SALDO-DEUDA
               DISPLAY "TASA A.  : " BORM-TASA-ANUAL " %"
               DISPLAY "CUOTA M. : " BORM-CUOTA-MENSUAL
               DISPLAY "MORA     : " BORM-MESES-MORA " meses"
               DISPLAY "VENCE    : " BORM-FECHA-VENCIMIENTO
               DISPLAY "ESTADO   : " BORM-ESTADO-PRESTAMO
               DISPLAY "================================"
           END-IF.

       2300-PROCESAR-PAGO.
           MOVE 'S' TO WS-PUEDE-CONTINUAR.
           DISPLAY "--- PAGO MANUAL DE CUOTA ---".
           PERFORM 9010-BUSCAR-HIPOTECA.

           IF NOT LK-EXITO
               PERFORM 9500-MSG-ERROR-HIPOTECA
               MOVE 'N' TO WS-PUEDE-CONTINUAR
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               IF HIPO-PAGADO
                   DISPLAY "================================"
                   DISPLAY " OPERACION RECHAZADA"
                   DISPLAY "================================"
                   DISPLAY "MOTIVO: HIPOTECA YA PAGADA."
                   DISPLAY "================================"
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               IF HIPO-MOROSO
                   DISPLAY "================================"
                   DISPLAY " AVISO - HIPOTECA EN MORA"
                   DISPLAY "================================"
                   DISPLAY " Meses de mora: " BORM-MESES-MORA
                   DISPLAY " Puede abonar a la deuda."
                   DISPLAY "================================"
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Cliente       : " WS-NOMBRE-CLIENTE
               DISPLAY "Nro hipoteca  : " BORM-ID-HIPOTECA
               DISPLAY "Saldo deuda   : " BORM-SALDO-DEUDA
               DISPLAY "Cuota mensual : " BORM-CUOTA-MENSUAL
               DISPLAY "Monto a pagar : "
               ACCEPT WS-ENTRADA-TXT
               PERFORM 9300-VALIDAR-NUMERO
               IF VAL-ERROR
                   DISPLAY "ERROR: MONTO INVALIDO."
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               MOVE FUNCTION NUMVAL(WS-ENTRADA-TXT)
                   TO WS-MONTO-PAGO
               IF WS-MONTO-PAGO <= 0
                   DISPLAY "ERROR: PAGO MAYOR A CERO."
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               IF WS-MONTO-PAGO > BORM-SALDO-DEUDA
                   DISPLAY "================================"
                   DISPLAY " OPERACION RECHAZADA"
                   DISPLAY "================================"
                   DISPLAY "MOTIVO: PAGO EXCEDE EL SALDO."
                   DISPLAY "SALDO : " BORM-SALDO-DEUDA
                   DISPLAY "================================"
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
                   DISPLAY "================================"
                   DISPLAY " AVISO - ABONO PARCIAL"
                   DISPLAY "================================"
                   DISPLAY "El pago no cubre la cuota."
                   DISPLAY "La mora la evalua el batch."
                   DISPLAY "================================"
           END-EVALUATE.

           SET ACCION-UPDATE TO TRUE.
           CALL WS-PGM-DBIOBORM
               USING REG-BORM,
                     LK-DATOS-SESION,
                     LK-DATOS-TRANSACCION.

           IF LK-EXITO
               CALL WS-PGM-TRAN USING 'C'
               DISPLAY "================================"
               DISPLAY " PAGO PROCESADO OK"
               DISPLAY "================================"
               DISPLAY "PAGADO  : " WS-MONTO-PAGO
               DISPLAY "SALDO   : " BORM-SALDO-DEUDA
               DISPLAY "ESTADO  : " BORM-ESTADO-PRESTAMO
               DISPLAY "MORA    : " BORM-MESES-MORA
               DISPLAY "================================"
           ELSE
               CALL WS-PGM-TRAN USING 'R'
               DISPLAY "================================"
               DISPLAY " ERROR - OPERACION REVERSADA"
               DISPLAY "================================"
               DISPLAY "MOTIVO: " LK-MENSAJE
               DISPLAY "================================"
           END-IF.

       9000-BUSCAR-CLIENTE.
           INITIALIZE REG-CUSM.
           DISPLAY "Ingrese cedula del cliente: ".
           ACCEPT CUSM-DOC-CLIENTE.

           MOVE FUNCTION TRIM(CUSM-DOC-CLIENTE)
               TO CUSM-DOC-CLIENTE.

           IF CUSM-DOC-CLIENTE = SPACES
               MOVE 'E404' TO LK-COD-RETORNO
               MOVE "CEDULA INVALIDA" TO LK-MENSAJE
           ELSE
               SET ACCION-SELECT TO TRUE
               CALL WS-PGM-DBIOCUSM
                   USING REG-CUSM,
                         LK-DATOS-SESION,
                         LK-DATOS-TRANSACCION
               IF LK-EXITO
                   MOVE CUSM-NOMBRE-CLIENTE    TO WS-NOMBRE-CLIENTE
                   MOVE CUSM-APELLIDOS-CLIENTE TO WS-APELLIDO-CLIENTE
               END-IF
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
               DISPLAY "Ingrese numero de hipoteca: "
               ACCEPT BORM-ID-HIPOTECA

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
           DISPLAY "================================".
           DISPLAY " ERROR - CLIENTE".
           DISPLAY "================================".
           DISPLAY "MOTIVO : " LK-MENSAJE.
           DISPLAY "ACCION : Verifique la cedula.".
           DISPLAY "================================".

       9500-MSG-ERROR-HIPOTECA.
           DISPLAY "================================".
           DISPLAY " ERROR - HIPOTECA".
           DISPLAY "================================".
           DISPLAY "MOTIVO : " LK-MENSAJE.
           DISPLAY "ACCION : Verifique el numero.".
           DISPLAY "================================".

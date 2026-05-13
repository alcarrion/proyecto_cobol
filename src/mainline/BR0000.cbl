       IDENTIFICATION DIVISION.
       PROGRAM-ID. BR0000.
      *================================================================*
      * PROGRAMA : BR0000.cbl                                          *
      * MODULO   : HIPOTECAS                                           *
      * FUNCION  : Reglas de negocio. Alta, consulta y pago.           *
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPTION              PIC 9(01) VALUE ZERO.
       01  WS-CONTINUAR           PIC X(01) VALUE 'S'.
       01  WS-PUEDE-CONTINUAR     PIC X(01) VALUE 'S'.

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

       01  WS-CALCULOS.
           05 WS-TASA-MENSUAL     PIC S9(03)V9(9) VALUE ZERO.
           05 WS-FACTOR-BASE      PIC S9(07)V9(9) VALUE ZERO.
           05 WS-FACTOR-POT       PIC S9(07)V9(9) VALUE ZERO.
           05 WS-IDX-POT          PIC 9(03)       VALUE ZERO.
           05 WS-NUMERADOR        PIC S9(13)V9(9) VALUE ZERO.
           05 WS-DENOMINADOR      PIC S9(13)V9(9) VALUE ZERO.

      *--- Fecha del sistema - se llena con FROM DATE YYYYMMDD ---
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

       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.

      *================================================================*
       0000-MAIN.
           MOVE 'S' TO WS-CONTINUAR.
           PERFORM 1000-MOSTRAR-MENU
               UNTIL WS-CONTINUAR = 'N'.
           GOBACK.

      *================================================================*
       1000-MOSTRAR-MENU.
           DISPLAY " ".
           DISPLAY "====================================".
           DISPLAY "       MODULO DE HIPOTECAS          ".
           DISPLAY "====================================".
           DISPLAY "  1. Registrar nuevo prestamo".
           DISPLAY "  2. Consultar estado de hipoteca".
           DISPLAY "  3. Procesar pago de cuota".
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

      *================================================================*
      * 2100 - REGISTRAR ALTA                                         *
      * Usa WS-PUEDE-CONTINUAR como flag para evitar GO TO            *
      *================================================================*
       2100-REGISTRAR-ALTA.
           MOVE 'S' TO WS-PUEDE-CONTINUAR.
           DISPLAY "--- ALTA PRESTAMO HIPOTECARIO ---".
           INITIALIZE BORM-REGISTRO.

           PERFORM 9000-BUSCAR-CLIENTE.

           IF LK-COD-RETORNO NOT = 0
               PERFORM 9400-MSG-ERROR-CLIENTE
               MOVE 'N' TO WS-PUEDE-CONTINUAR
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               IF CUSM-CTA-ACTIVA = 0
                   DISPLAY "================================"
                   DISPLAY " OPERACION RECHAZADA"
                   DISPLAY "================================"
                   DISPLAY "MOTIVO: CLIENTE INACTIVO."
                   DISPLAY "================================"
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               IF CUSM-HIPOTECA = 1
                   DISPLAY "================================"
                   DISPLAY " OPERACION RECHAZADA"
                   DISPLAY "================================"
                   DISPLAY "MOTIVO: YA TIENE HIPOTECA."
                   DISPLAY "================================"
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               MOVE CUSM-ID-CLIENTE TO BORM-ID-CLIENTE
               MOVE 'S' TO LK-ACCION-DB
               CALL WS-PGM-DBIOBORM
                   USING LK-DATOS-TRANSACCION,
                         BORM-REGISTRO
               IF LK-COD-RETORNO NOT = 0
                   DISPLAY "ERROR GENERANDO ID: " LK-MENSAJE
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               PERFORM 9100-ARMAR-FECHA-INICIO
               PERFORM 2110-CAPTURAR-DATOS-ALTA
           END-IF.

      *================================================================*
      * 2110 - CAPTURAR Y VALIDAR DATOS DEL PRESTAMO                  *
      *================================================================*
       2110-CAPTURAR-DATOS-ALTA.
           MOVE 'S' TO WS-PUEDE-CONTINUAR.
           DISPLAY "--------------------------------".
           DISPLAY "CLIENTE : " CUSM-NOMBRE.
           DISPLAY "APELLIDO: " CUSM-APELLIDOS.
           DISPLAY "ID HIP. : " BORM-ID-HIPOTECA.
           DISPLAY "--------------------------------".

           DISPLAY "Monto del prestamo: ".
           ACCEPT WS-ENTRADA-TXT.
           PERFORM 9300-VALIDAR-NUMERO.
           IF VAL-ERROR
               DISPLAY "ERROR: MONTO INVALIDO."
               MOVE 'N' TO WS-PUEDE-CONTINUAR
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               MOVE FUNCTION NUMVAL(WS-ENTRADA-TXT)
                   TO BORM-MONTO-ORIGINAL
               IF BORM-MONTO-ORIGINAL <= 0
                   DISPLAY "ERROR: MONTO MAYOR A CERO."
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Tasa interes mensual% (ej:4.50): "
               ACCEPT WS-ENTRADA-TXT
               PERFORM 9300-VALIDAR-NUMERO
               IF VAL-ERROR
                   DISPLAY "ERROR: TASA INVALIDA."
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               MOVE FUNCTION NUMVAL(WS-ENTRADA-TXT)
                   TO BORM-TASA-INTERES
               IF BORM-TASA-INTERES <= 0
                   DISPLAY "ERROR: TASA MAYOR A CERO."
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Plazo en meses: "
               ACCEPT WS-PLAZO-MESES
               IF WS-PLAZO-MESES <= 0
                   DISPLAY "ERROR: PLAZO MAYOR A CERO."
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Dia de pago (1-28): "
               ACCEPT BORM-DIA-PAGO
               IF BORM-DIA-PAGO < 1 OR BORM-DIA-PAGO > 28
                   DISPLAY "ERROR: DIA ENTRE 1 Y 28."
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               PERFORM 9120-CALCULAR-VENCIMIENTO
               PERFORM 9130-CALCULAR-CUOTA
               PERFORM 2140-GUARDAR-HIPOTECA
           END-IF.

      *================================================================*
      * 2140 - GUARDAR HIPOTECA EN BASE DE DATOS                      *
      *================================================================*
       2140-GUARDAR-HIPOTECA.
           MOVE BORM-MONTO-ORIGINAL TO BORM-SALDO-ACTUAL.
           MOVE "ACTIVO"     TO BORM-ESTADO.
           MOVE ZERO         TO BORM-MESES-MORA.
           MOVE "0000-00-00" TO BORM-FECHA-ULT-PAGO.

           MOVE 'A' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOBORM
               USING LK-DATOS-TRANSACCION,
                     BORM-REGISTRO.

           IF LK-COD-RETORNO = 0
               CALL WS-PGM-TRAN USING 'C'
               DISPLAY "================================"
               DISPLAY " HIPOTECA REGISTRADA OK"
               DISPLAY "================================"
               DISPLAY "ID HIP.  : " BORM-ID-HIPOTECA
               DISPLAY "CLIENTE  : " BORM-ID-CLIENTE
               DISPLAY "MONTO    : " BORM-MONTO-ORIGINAL
               DISPLAY "TASA M.  : " BORM-TASA-INTERES
               DISPLAY "CUOTA M. : " BORM-CUOTA-MENSUAL
               DISPLAY "VENCE    : " BORM-FECHA-VENCTO
               DISPLAY "ESTADO   : " BORM-ESTADO
               DISPLAY "================================"
           ELSE
               CALL WS-PGM-TRAN USING 'R'
               DISPLAY "================================"
               DISPLAY " ERROR - OPERACION REVERSADA"
               DISPLAY "================================"
               DISPLAY "MOTIVO: " LK-MENSAJE
               DISPLAY "================================"
           END-IF.

      *================================================================*
      * 2200 - CONSULTAR HIPOTECA                                     *
      *================================================================*
       2200-CONSULTAR-HIPOTECA.
           DISPLAY "--- CONSULTA DE HIPOTECA ---".
           PERFORM 9010-BUSCAR-HIPOTECA.

           IF LK-COD-RETORNO NOT = 0
               PERFORM 9500-MSG-ERROR-HIPOTECA
           ELSE
               DISPLAY "================================"
               DISPLAY "     ESTADO DE HIPOTECA"
               DISPLAY "================================"
               DISPLAY "ID HIP.  : " BORM-ID-HIPOTECA
               DISPLAY "CLIENTE  : " BORM-ID-CLIENTE
               DISPLAY "INICIO   : " BORM-FECHA-INICIO
               DISPLAY "VENCE    : " BORM-FECHA-VENCTO
               DISPLAY "MONTO    : " BORM-MONTO-ORIGINAL
               DISPLAY "SALDO    : " BORM-SALDO-ACTUAL
               DISPLAY "TASA M.  : " BORM-TASA-INTERES
               DISPLAY "CUOTA M. : " BORM-CUOTA-MENSUAL
               DISPLAY "DIA PAGO : " BORM-DIA-PAGO
               DISPLAY "MORA     : " BORM-MESES-MORA
               DISPLAY "ULT PAGO : " BORM-FECHA-ULT-PAGO
               DISPLAY "ESTADO   : " BORM-ESTADO
               DISPLAY "================================"
           END-IF.

      *================================================================*
      * 2300 - PROCESAR PAGO                                          *
      *================================================================*
       2300-PROCESAR-PAGO.
           MOVE 'S' TO WS-PUEDE-CONTINUAR.
           DISPLAY "--- PROCESAR PAGO DE CUOTA ---".
           PERFORM 9010-BUSCAR-HIPOTECA.

           IF LK-COD-RETORNO NOT = 0
               PERFORM 9500-MSG-ERROR-HIPOTECA
               MOVE 'N' TO WS-PUEDE-CONTINUAR
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               IF BORM-ESTADO = "CANCELADO"
                   DISPLAY "================================"
                   DISPLAY " OPERACION RECHAZADA"
                   DISPLAY "================================"
                   DISPLAY "MOTIVO: HIPOTECA CANCELADA."
                   DISPLAY "================================"
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               IF BORM-ESTADO = "CASTIGADO"
                   DISPLAY "================================"
                   DISPLAY " AVISO - HIPOTECA CASTIGADA"
                   DISPLAY "================================"
                   DISPLAY "Puede abonar a la deuda."
                   DISPLAY "Pagando >= cuota: ACTIVO"
                   DISPLAY "================================"
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               DISPLAY "Saldo actual  : " BORM-SALDO-ACTUAL
               DISPLAY "Cuota mensual : " BORM-CUOTA-MENSUAL
               DISPLAY "Meses en mora : " BORM-MESES-MORA
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
               IF WS-MONTO-PAGO > BORM-SALDO-ACTUAL
                   DISPLAY "================================"
                   DISPLAY " OPERACION RECHAZADA"
                   DISPLAY "================================"
                   DISPLAY "MOTIVO: PAGO EXCEDE EL SALDO."
                   DISPLAY "SALDO : " BORM-SALDO-ACTUAL
                   DISPLAY "================================"
                   MOVE 'N' TO WS-PUEDE-CONTINUAR
               END-IF
           END-IF.

           IF WS-PUEDE-CONTINUAR = 'S'
               PERFORM 2310-APLICAR-PAGO
           END-IF.

      *================================================================*
      * 2310 - APLICAR PAGO Y ACTUALIZAR ESTADO                       *
      *================================================================*
       2310-APLICAR-PAGO.
           SUBTRACT WS-MONTO-PAGO FROM BORM-SALDO-ACTUAL.

           ACCEPT WS-FECHA-HOY FROM DATE YYYYMMDD.
           MOVE WS-ANIO TO WS-ED-ANIO.
           MOVE WS-MES  TO WS-ED-MES.
           MOVE WS-DIA  TO WS-ED-DIA.
           MOVE WS-FECHA-EDITADA TO BORM-FECHA-ULT-PAGO.

           EVALUATE TRUE
               WHEN BORM-SALDO-ACTUAL = 0
                   MOVE "CANCELADO" TO BORM-ESTADO
                   MOVE ZERO        TO BORM-MESES-MORA
               WHEN WS-MONTO-PAGO >= BORM-CUOTA-MENSUAL
                   MOVE "ACTIVO"    TO BORM-ESTADO
                   MOVE ZERO        TO BORM-MESES-MORA
               WHEN OTHER
                   IF BORM-ESTADO NOT = "CASTIGADO"
                       MOVE "MOROSO" TO BORM-ESTADO
                   END-IF
                   ADD 1 TO BORM-MESES-MORA
           END-EVALUATE.

           MOVE 'M' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOBORM
               USING LK-DATOS-TRANSACCION,
                     BORM-REGISTRO.

           IF LK-COD-RETORNO = 0
               CALL WS-PGM-TRAN USING 'C'
               DISPLAY "================================"
               DISPLAY " PAGO PROCESADO OK"
               DISPLAY "================================"
               DISPLAY "PAGADO  : " WS-MONTO-PAGO
               DISPLAY "SALDO   : " BORM-SALDO-ACTUAL
               DISPLAY "PAGO    : " BORM-FECHA-ULT-PAGO
               DISPLAY "ESTADO  : " BORM-ESTADO
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

      *================================================================*
      * 9000 - BUSCAR CLIENTE POR CEDULA                              *
      *================================================================*
       9000-BUSCAR-CLIENTE.
           INITIALIZE REG-CUSM.
           DISPLAY "Ingrese cedula del cliente: ".
           ACCEPT CUSM-DOC-CLIENTE.

      *    Eliminar espacios del ingreso para que el WHERE matchee
           MOVE FUNCTION TRIM(CUSM-DOC-CLIENTE)
               TO CUSM-DOC-CLIENTE.

           IF CUSM-DOC-CLIENTE = SPACES
               MOVE 01 TO LK-COD-RETORNO
               MOVE "CEDULA INVALIDA" TO LK-MENSAJE
           ELSE
               MOVE 'C' TO LK-ACCION-DB
               CALL WS-PGM-DBIOCUSM
                   USING REG-CUSM,
                         LK-DATOS-TRANSACCION
               IF LK-COD-RETORNO NOT = 0
                   MOVE "CLIENTE NO ENCONTRADO"
                       TO LK-MENSAJE
               END-IF
           END-IF.

      *================================================================*
      * 9010 - BUSCAR HIPOTECA POR ID                                 *
      *================================================================*
       9010-BUSCAR-HIPOTECA.
           INITIALIZE BORM-REGISTRO.
           DISPLAY "Ingrese ID de hipoteca: ".
           ACCEPT BORM-ID-HIPOTECA.

           IF BORM-ID-HIPOTECA = ZERO
               MOVE 01 TO LK-COD-RETORNO
               MOVE "ID HIPOTECA INVALIDO" TO LK-MENSAJE
           ELSE
               MOVE 'C' TO LK-ACCION-DB
               CALL WS-PGM-DBIOBORM
                   USING LK-DATOS-TRANSACCION,
                         BORM-REGISTRO
           END-IF.

      *================================================================*
      * 9100 - ARMAR FECHA DE INICIO                                  *
      * ACCEPT ... FROM DATE YYYYMMDD llena 8 digitos en WS-FECHA-HOY *
      * REDEFINES divide automaticamente en ANIO/MES/DIA              *
      *================================================================*
       9100-ARMAR-FECHA-INICIO.
           ACCEPT WS-FECHA-HOY FROM DATE YYYYMMDD.
           MOVE WS-ANIO TO WS-ED-ANIO.
           MOVE WS-MES  TO WS-ED-MES.
           MOVE WS-DIA  TO WS-ED-DIA.
           MOVE WS-FECHA-EDITADA TO BORM-FECHA-INICIO.

      *================================================================*
      * 9120 - CALCULAR FECHA DE VENCIMIENTO                          *
      *================================================================*
       9120-CALCULAR-VENCIMIENTO.
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

           MOVE WS-CALC-ANIO  TO WS-ED-ANIO.
           MOVE WS-CALC-MES   TO WS-ED-MES.
           MOVE BORM-DIA-PAGO TO WS-ED-DIA.
           MOVE WS-FECHA-EDITADA TO BORM-FECHA-VENCTO.

      *================================================================*
      * 9130 - CALCULAR CUOTA - AMORTIZACION FRANCESA                 *
      *   Cuota = P*(i*(1+i)^n) / ((1+i)^n - 1)                       *
      *================================================================*
       9130-CALCULAR-CUOTA.
           COMPUTE WS-TASA-MENSUAL =
               BORM-TASA-INTERES / 100.

           COMPUTE WS-FACTOR-BASE = 1 + WS-TASA-MENSUAL.
           MOVE 1 TO WS-FACTOR-POT.

           PERFORM VARYING WS-IDX-POT FROM 1 BY 1
               UNTIL WS-IDX-POT > WS-PLAZO-MESES
               COMPUTE WS-FACTOR-POT =
                   WS-FACTOR-POT * WS-FACTOR-BASE
           END-PERFORM.

           COMPUTE WS-NUMERADOR =
               BORM-MONTO-ORIGINAL *
               WS-TASA-MENSUAL *
               WS-FACTOR-POT.

           COMPUTE WS-DENOMINADOR = WS-FACTOR-POT - 1.

           IF WS-DENOMINADOR = 0
               COMPUTE BORM-CUOTA-MENSUAL =
                   BORM-MONTO-ORIGINAL +
                   (BORM-MONTO-ORIGINAL * WS-TASA-MENSUAL)
           ELSE
               COMPUTE BORM-CUOTA-MENSUAL =
                   WS-NUMERADOR / WS-DENOMINADOR
           END-IF.

      *================================================================*
      * 9300 - VALIDAR NUMERO                                         *
      *================================================================*
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

      *================================================================*
      * 9400 - MENSAJE ERROR CLIENTE                                  *
      *================================================================*
       9400-MSG-ERROR-CLIENTE.
           DISPLAY "================================".
           DISPLAY " ERROR - CLIENTE".
           DISPLAY "================================".
           DISPLAY "MOTIVO : " LK-MENSAJE.
           DISPLAY "ACCION : Verifique la cedula.".
           DISPLAY "================================".

      *================================================================*
      * 9500 - MENSAJE ERROR HIPOTECA                                 *
      *================================================================*
       9500-MSG-ERROR-HIPOTECA.
           DISPLAY "================================".
           DISPLAY " ERROR - HIPOTECA".
           DISPLAY "================================".
           DISPLAY "MOTIVO : " LK-MENSAJE.
           DISPLAY "ACCION : Verifique el ID.".
           DISPLAY "================================".

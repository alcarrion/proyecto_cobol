       IDENTIFICATION DIVISION.
       PROGRAM-ID. TC0000.
      *================================================================*
      * PROGRAMA: MAINLINE TARJETAS DE CREDITO                         *
      * FUNCION:  Gestiona Emision, Consulta, Cargos, Pagos y Bajas.   *
      * REGLA:    El cliente debe existir en CLIENTES y estar Activo.   *
      * REGLA:    La tarjeta debe estar Activa para operar.             *
      * PK TABLA: (ID_CLIENTE, NRO_TARJETA)                            *
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPCION-TARJ       PIC 9(01).
       01  WS-CONTINUAR-TARJ    PIC X(01) VALUE 'S'.
       01  WS-ENTRADA-MONTO     PIC X(13).
       01  WS-CONFIRMA          PIC X(01).
      * COPY de la tabla maestra de Tarjetas (TARJETAS)
           COPY TARJREC.

      * COPY de la tabla maestra de Clientes (CUSM) - Validacion
           COPY CUSMREC.

      * Variables de trabajo para operaciones
       01  WS-CALCULOS-TC.
           05 WS-MONTO-TX      PIC S9(10)V99 VALUE ZERO.
           05 WS-DISPONIBLE    PIC S9(10)V99 VALUE ZERO.
           05 WS-DEUDA-TOTAL   PIC S9(10)V99 VALUE ZERO.

       01  WS-PROGRAMAS.
           05 WS-PGM-DBIOCUSM    PIC X(8) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOTARJ    PIC X(8) VALUE 'DBIOTARJ'.
           05 WS-PGM-TRAN        PIC X(08) VALUE 'DBIOTRAN'.

       01  WS-FECHA-SISTEMA.
           05 WS-ANIO           PIC 9(04).
           05 WS-MES            PIC 9(02).
           05 WS-DIA            PIC 9(02).

       01  WS-ANIO-VENC         PIC 9(04).

       01  WS-AUX-GEN.
           05 WS-SEMILLA        PIC 9(08).
           05 WS-RANDOM         PIC 9(04).

       LINKAGE SECTION.
           COPY LKCIF.

       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           PERFORM 1000-PROCESAR-OPCIONES
                   UNTIL WS-CONTINUAR-TARJ = 'N' OR 'n'.
           GOBACK.

       1000-PROCESAR-OPCIONES.
           DISPLAY "========================================".
           DISPLAY "     MODULO TARJETAS DE CREDITO (TC)    ".
           DISPLAY "========================================".
           DISPLAY "1. Emision de Tarjeta".
           DISPLAY "2. Consulta de Tarjeta".
           DISPLAY "3. Cargo a Tarjeta (Consumo)".
           DISPLAY "4. Pago de Tarjeta".
           DISPLAY "5. Consulta de Deuda".
           DISPLAY "6. Bloqueo de Tarjeta".
           DISPLAY "7. Baja (Cancelacion) de Tarjeta".
           DISPLAY "0. Volver al Menu Principal".
           DISPLAY "========================================".
           DISPLAY "Seleccione operacion: ".
           ACCEPT WS-OPCION-TARJ.

           EVALUATE WS-OPCION-TARJ
               WHEN 1
                   PERFORM 2000-EMISION-TARJETA
               WHEN 2
                   PERFORM 3000-CONSULTA-TARJETA
               WHEN 3
                   PERFORM 4000-CARGO-TARJETA
               WHEN 4
                   PERFORM 5000-PAGO-TARJETA
               WHEN 5
                   PERFORM 6000-CONSULTA-DEUDA
               WHEN 6
                   PERFORM 7000-BLOQUEO-TARJETA
               WHEN 7
                   PERFORM 8000-BAJA-TARJETA
               WHEN 0
                   MOVE 'N' TO WS-CONTINUAR-TARJ
               WHEN OTHER
                   DISPLAY "Opcion invalida."
           END-EVALUATE.

       2000-EMISION-TARJETA.
           DISPLAY "--- EMISION DE NUEVA TARJETA ---".
           PERFORM 9000-BUSCAR-CLIENTE.
      *    Validar que el cliente exista y este activo
           IF LK-COD-RETORNO = 0
      * Regla de Negocio: 1 Tarjeta por cliente
               IF CUSM-TARJETA = 1
                   DISPLAY "ERROR: EL CLIENTE YA POSEE UNA TARJETA."
               ELSE
                   INITIALIZE REG-TARJ
      * LOGICA DE GENERACION: Prefijo '4555' + ID_CLIENTE + Random
                   MOVE CUSM-ID-CLIENTE TO TARJ-ID-CLIENTE

                   PERFORM 9200-GENERAR-NUMERO-TARJETA

                   ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD

                   STRING WS-ANIO "-" WS-MES "-" WS-DIA
                          DELIMITED BY SIZE INTO TARJ-FECHA-EMISION

                   ADD 4 TO WS-ANIO GIVING WS-ANIO-VENC
                   STRING WS-ANIO-VENC "-" WS-MES "-" WS-DIA
                          DELIMITED BY SIZE INTO TARJ-FECHA-VENCIM

                   DISPLAY "TARJETA GENERADA: " TARJ-NRO-TARJETA
                   DISPLAY "EMISION: " TARJ-FECHA-EMISION
                   DISPLAY "VENCE  : " TARJ-FECHA-VENCIM

                   DISPLAY "LIMITE DE CREDITO: "
                   ACCEPT WS-ENTRADA-MONTO
                   MOVE FUNCTION NUMVAL(WS-ENTRADA-MONTO)
                   TO TARJ-LIMITE-TARJETA

                   MOVE 'A'          TO TARJ-ESTADO
                   MOVE ZERO         TO TARJ-ACUM-MES
                   MOVE ZERO         TO TARJ-LIQUIDACION-MES

      * PASO 1: Emitir Tarjeta
                   MOVE 'A' TO LK-ACCION-DB
                   CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                   LK-DATOS-TRANSACCION

                   IF LK-COD-RETORNO = 0
      * PASO 2: Actualizar Flag en Maestro de Clientes
                       MOVE 1 TO CUSM-TARJETA
                       MOVE 'M' TO LK-ACCION-DB
                       CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                       LK-DATOS-TRANSACCION

                       CALL WS-PGM-TRAN USING 'C'
                       DISPLAY "TARJETA EMITIDA CON EXITO."
                   ELSE
                       CALL WS-PGM-TRAN USING 'R'
                       DISPLAY "ERROR EN EMISION: " LK-MENSAJE
                   END-IF
               END-IF
           ELSE
               DISPLAY "ERROR: " LK-MENSAJE
           END-IF.

       3000-CONSULTA-TARJETA.


           PERFORM 9100-BUSCAR-TARJETA.
           IF LK-COD-RETORNO = 0
               DISPLAY "--------------------------------------"
               DISPLAY "TARJETA: " TARJ-NRO-TARJETA
               DISPLAY "ID-Cliente: " TARJ-ID-CLIENTE
               DISPLAY "ESTADO: " TARJ-ESTADO
               DISPLAY "LIMITE : " TARJ-LIMITE-TARJETA
               DISPLAY "DEUDA ACUMULADA: " TARJ-ACUM-MES
               COMPUTE WS-DISPONIBLE =
               TARJ-LIMITE-TARJETA - TARJ-ACUM-MES
               DISPLAY "DISPONIBLE: " WS-DISPONIBLE
               DISPLAY "--------------------------------------"
           END-IF.

       4000-CARGO-TARJETA.
           DISPLAY "--- CARGO A TARJETA (CONSUMO) ---".
           PERFORM 9100-BUSCAR-TARJETA.

           IF LK-COD-RETORNO = 0 AND TARJ-ESTADO = 'A'
               DISPLAY "MONTO DEL CONSUMO: "
               ACCEPT WS-ENTRADA-MONTO
               MOVE FUNCTION NUMVAL(WS-ENTRADA-MONTO)
               TO TARJ-IMPORTE-MOV

               IF TARJ-IMPORTE-MOV >
                   (TARJ-LIMITE-TARJETA - TARJ-ACUM-MES)
                   DISPLAY "ERROR: EXCEDE LIMITE DE CREDITO."
               ELSE
                   ADD TARJ-IMPORTE-MOV TO TARJ-ACUM-MES
                   MOVE 'M' TO LK-ACCION-DB
                   CALL WS-PGM-DBIOTARJ
                   USING REG-TARJ, LK-DATOS-TRANSACCION

                   IF LK-COD-RETORNO = 0
                       CALL WS-PGM-TRAN USING 'C'
                       DISPLAY "CONSUMO REGISTRADO."
                   ELSE
                       CALL WS-PGM-TRAN USING 'R'
                   END-IF
               END-IF
           END-IF.

       5000-PAGO-TARJETA.
           DISPLAY "--- PAGO DE TARJETA ---".
           PERFORM 9100-BUSCAR-TARJETA.

           IF LK-COD-RETORNO = 0
               IF TARJ-ESTADO = 'I'
                   DISPLAY "ERROR: LA TARJETA ESTA CANCELADA."
               ELSE
                   COMPUTE WS-DEUDA-TOTAL = TARJ-ACUM-MES +
                                            TARJ-LIQUIDACION-MES
                   DISPLAY "DEUDA TOTAL A LA FECHA: " WS-DEUDA-TOTAL
                   DISPLAY "INGRESE MONTO DEL PAGO: "
                   ACCEPT WS-ENTRADA-MONTO
                   MOVE FUNCTION NUMVAL(WS-ENTRADA-MONTO) TO WS-MONTO-TX

      * Lógica: El pago cubre primero la liquidación del mes anterior
                   IF WS-MONTO-TX > TARJ-LIQUIDACION-MES
                       SUBTRACT TARJ-LIQUIDACION-MES FROM WS-MONTO-TX
                       MOVE ZERO TO TARJ-LIQUIDACION-MES
                       SUBTRACT WS-MONTO-TX FROM TARJ-ACUM-MES
                   ELSE
                       SUBTRACT WS-MONTO-TX FROM TARJ-LIQUIDACION-MES
                   END-IF

                   MOVE 'M' TO LK-ACCION-DB
                   CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                   LK-DATOS-TRANSACCION
                   IF LK-COD-RETORNO = 0
                       CALL WS-PGM-TRAN USING 'C'
                       DISPLAY "PAGO PROCESADO EXITOSAMENTE."
                   END-IF
               END-IF
           END-IF.

       6000-CONSULTA-DEUDA.
           DISPLAY "--- ESTADO DE DEUDA ---".
           PERFORM 9100-BUSCAR-TARJETA.
           IF LK-COD-RETORNO = 0
               COMPUTE WS-DEUDA-TOTAL = TARJ-ACUM-MES +
                                        TARJ-LIQUIDACION-MES
               COMPUTE WS-DISPONIBLE  = TARJ-LIMITE-TARJETA -
                                        TARJ-ACUM-MES
               DISPLAY "========================================"
               DISPLAY "TARJETA: " TARJ-NRO-TARJETA
               DISPLAY "DEUDA MES ACTUAL    : " TARJ-ACUM-MES
               DISPLAY "DEUDA MES ANTERIOR  : " TARJ-LIQUIDACION-MES
               DISPLAY "----------------------------------------"
               DISPLAY "TOTAL A PAGAR       : " WS-DEUDA-TOTAL
               DISPLAY "CREDITO DISPONIBLE  : " WS-DISPONIBLE
               DISPLAY "========================================"
           END-IF.

       7000-BLOQUEO-TARJETA.
           DISPLAY "--- GESTION DE ESTADO (BLOQUEO) ---".
           PERFORM 9100-BUSCAR-TARJETA.
           IF LK-COD-RETORNO = 0
               IF TARJ-ESTADO = 'I'
                   DISPLAY "ERROR: TARJETA CANCELADA DEFINITIVAMENTE."
               ELSE
                   IF TARJ-ESTADO = 'B'
                   DISPLAY "TARJETA BLOQUEADA. DESEA ACTIVAR? (S/N): "
                       ACCEPT WS-CONFIRMA
                       IF WS-CONFIRMA = 'S' OR 's' MOVE 'A'
                           TO TARJ-ESTADO
                   ELSE
                       DISPLAY "TARJETA ACTIVA. DESEA BLOQUEAR? (S/N): "
                       ACCEPT WS-CONFIRMA
                       IF WS-CONFIRMA = 'S' OR 's' MOVE 'B'
                           TO TARJ-ESTADO
                   END-IF

                   IF WS-CONFIRMA = 'S' OR 's'
                       MOVE 'M' TO LK-ACCION-DB
                       CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                                                  LK-DATOS-TRANSACCION
                       CALL WS-PGM-TRAN USING 'C'
                       DISPLAY "ESTADO ACTUALIZADO CON EXITO."
                   END-IF
               END-IF
           END-IF.

       8000-BAJA-TARJETA.
           DISPLAY "--- CANCELACION DEFINITIVA ---".
           PERFORM 9100-BUSCAR-TARJETA.
           IF LK-COD-RETORNO = 0
               COMPUTE WS-DEUDA-TOTAL = TARJ-ACUM-MES +
                                        TARJ-LIQUIDACION-MES

               *> Regla de negocio: No se puede cancelar con deuda
               IF WS-DEUDA-TOTAL > 0
               DISPLAY "ERROR: NO PUEDE CANCELAR CON DEUDA PENDIENTE."
               DISPLAY "DEUDA ACTUAL: " WS-DEUDA-TOTAL
               ELSE
                   DISPLAY "CONFIRMA CANCELACION DEFINITIVA? (S/N): "
                   ACCEPT WS-CONFIRMA
                   IF WS-CONFIRMA = 'S' OR 's'
                       MOVE 'I' TO TARJ-ESTADO *> I = Inactiva/Cancelada
                       MOVE 'M' TO LK-ACCION-DB
                       CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                                                  LK-DATOS-TRANSACCION
                       CALL WS-PGM-TRAN USING 'C'
                       DISPLAY "TARJETA CANCELADA EXITOSAMENTE."
                   END-IF
               END-IF
           END-IF.

       9000-BUSCAR-CLIENTE.
           INITIALIZE REG-CUSM.
           DISPLAY "DOC (CEDULA) DEL CLIENTE: " ACCEPT CUSM-DOC-CLIENTE.
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

       9100-BUSCAR-TARJETA.
           PERFORM 9000-BUSCAR-CLIENTE.
           IF LK-COD-RETORNO = 0
               MOVE CUSM-ID-CLIENTE TO TARJ-ID-CLIENTE
               DISPLAY "NUMERO DE TARJETA: " ACCEPT TARJ-NRO-TARJETA
               MOVE 'C' TO LK-ACCION-DB
               CALL WS-PGM-DBIOTARJ USING REG-TARJ, LK-DATOS-TRANSACCION
           END-IF.

       9200-GENERAR-NUMERO-TARJETA.
           ACCEPT WS-SEMILLA FROM TIME.
           COMPUTE WS-RANDOM = FUNCTION RANDOM(WS-SEMILLA) * 10000.
           STRING "4555" CUSM-ID-CLIENTE WS-RANDOM
                  DELIMITED BY SIZE INTO TARJ-NRO-TARJETA.

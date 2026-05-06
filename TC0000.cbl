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

       01 WS-OPCION-TARJ       PIC 9(01).
       01 WS-CONTINUAR-TARJ    PIC X(01) VALUE 'S'.

      * COPY de la tabla maestra de Tarjetas (TARJETAS)
           COPY TARJREC.

      * COPY de la tabla maestra de Clientes (CUSM) - Validacion
           COPY CUSMREC.

      * Variables de trabajo para operaciones
           01 WS-MONTO-CARGO       PIC S9(10)V99 VALUE ZERO.
           01 WS-MONTO-PAGO        PIC S9(10)V99 VALUE ZERO.
           01 WS-DISPONIBLE        PIC S9(10)V99 VALUE ZERO.
           01 WS-DEUDA-TOTAL       PIC S9(10)V99 VALUE ZERO.
           01 WS-CONFIRMA          PIC X(01) VALUE SPACES.

       01 WS-PROGRAMAS.
           05 WS-PGM-DBIOCUSM    PIC X(8) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOTARJ    PIC X(8) VALUE 'DBIOTARJ'.

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
           DISPLAY "--- EMISION DE TARJETA ---".

      *    Validar que el cliente exista y este activo
           DISPLAY "Ingrese ID de Cliente (8 digitos): "
.
           ACCEPT TARJ-ID-CLIENTE.

           MOVE TARJ-ID-CLIENTE TO CUSM-ID-CLIENTE.
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "ERROR: Cliente no encontrado."
               DISPLAY LK-MENSAJE
           ELSE
               IF CUSM-CTA-ACTIVA NOT = 'A'
                   DISPLAY "ERROR: El cliente no esta activo."
               ELSE
      *            Cliente valido, proceder con la emision
                   MOVE 'A' TO LK-ACCION-DB

                   DISPLAY "Ingrese Numero de Tarjeta (16 digitos): "

                   ACCEPT TARJ-NRO-TARJETA

                   DISPLAY "Ingrese Fecha Emision (YYYY-MM-DD): "

                   ACCEPT TARJ-FECHA-EMISION

                   DISPLAY "Ingrese Fecha Vencimiento (YYYY-MM-DD): "

                   ACCEPT TARJ-FECHA-VENCIM

                   DISPLAY "Ingrese Limite de Credito: "

                   ACCEPT TARJ-LIMITE-TARJETA

                   MOVE ZERO TO TARJ-ACUM-MES
                   MOVE ZERO TO TARJ-LIQUIDACION-MES
                   MOVE 'A'  TO TARJ-ESTADO

      *            Activar flag de tarjeta en cliente si no lo tiene
                   IF CUSM-TARJETA = 0
                       MOVE 1 TO CUSM-TARJETA
                       MOVE 'M' TO LK-ACCION-DB
                       CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                           LK-DATOS-TRANSACCION
                       MOVE 'A' TO LK-ACCION-DB
                   END-IF

                   CALL 'DBIOTARJ' USING REG-TARJ,
                       LK-DATOS-TRANSACCION

                   IF LK-COD-RETORNO = 0
                       DISPLAY "Tarjeta emitida exitosamente."
                       DISPLAY "Estado: ACTIVA"
                   ELSE
                       DISPLAY "Error al emitir tarjeta: " LK-MENSAJE
                   END-IF
               END-IF
           END-IF.

       3000-CONSULTA-TARJETA.
           DISPLAY "--- CONSULTA DE TARJETA ---".
           MOVE 'C' TO LK-ACCION-DB.

           DISPLAY "Ingrese ID de Cliente: ".
           ACCEPT TARJ-ID-CLIENTE.
           DISPLAY "Ingrese Numero de Tarjeta: ".
           ACCEPT TARJ-NRO-TARJETA.

           CALL WS-PGM-DBIOTARJ USING REG-TARJ, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               DISPLAY "Datos de la Tarjeta: "
               DISPLAY "Cliente:         " TARJ-ID-CLIENTE
               DISPLAY "Numero:          " TARJ-NRO-TARJETA
               DISPLAY "Fecha Emision:   " TARJ-FECHA-EMISION
               DISPLAY "Vencimiento:     " TARJ-FECHA-VENCIM
               DISPLAY "Limite:          " TARJ-LIMITE-TARJETA
               DISPLAY "Acumulado Mes:   " TARJ-ACUM-MES
               DISPLAY "Liquidacion:     " TARJ-LIQUIDACION-MES
               DISPLAY "Estado:          " TARJ-ESTADO
               COMPUTE WS-DISPONIBLE =
                   TARJ-LIMITE-TARJETA - TARJ-ACUM-MES
               DISPLAY "Disponible:      " WS-DISPONIBLE
           ELSE
               DISPLAY "Error: " LK-MENSAJE
           END-IF.

       4000-CARGO-TARJETA.
           DISPLAY "--- CARGO A TARJETA (CONSUMO) ---".

           DISPLAY "Ingrese ID de Cliente: ".
           ACCEPT TARJ-ID-CLIENTE.
           DISPLAY "Ingrese Numero de Tarjeta: ".
           ACCEPT TARJ-NRO-TARJETA.

      *    Consultar la tarjeta para validar
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "Error: " LK-MENSAJE
           ELSE
      *        Validar que la tarjeta este activa
               IF TARJ-ESTADO NOT = 'A'
                   DISPLAY "ERROR: La tarjeta no esta activa."
                   DISPLAY "Estado actual: " TARJ-ESTADO
               ELSE
                   DISPLAY "Ingrese monto del cargo: "

                   ACCEPT WS-MONTO-CARGO

      *            Validar que no exceda el limite de credito
                   COMPUTE WS-DISPONIBLE =
                       TARJ-LIMITE-TARJETA - TARJ-ACUM-MES

                   IF WS-MONTO-CARGO > WS-DISPONIBLE
                       DISPLAY "ERROR: El cargo excede el credito "
                       "disponible."
                       DISPLAY "Disponible: " WS-DISPONIBLE
                   ELSE
                       ADD WS-MONTO-CARGO TO TARJ-ACUM-MES
                       MOVE 'M' TO LK-ACCION-DB
                       CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                           LK-DATOS-TRANSACCION
                       IF LK-COD-RETORNO = 0
                           DISPLAY "Cargo realizado exitosamente."
                           DISPLAY "Acumulado mes: " TARJ-ACUM-MES
                           COMPUTE WS-DISPONIBLE =
                               TARJ-LIMITE-TARJETA - TARJ-ACUM-MES
                           DISPLAY "Disponible:    " WS-DISPONIBLE
                       ELSE
                           SUBTRACT WS-MONTO-CARGO FROM TARJ-ACUM-MES
                           DISPLAY "Error al procesar cargo: "
                               LK-MENSAJE
                       END-IF
                   END-IF
               END-IF
           END-IF.

       5000-PAGO-TARJETA.
           DISPLAY "--- PAGO DE TARJETA ---".

           DISPLAY "Ingrese ID de Cliente: ".
           ACCEPT TARJ-ID-CLIENTE.
           DISPLAY "Ingrese Numero de Tarjeta: ".
           ACCEPT TARJ-NRO-TARJETA.

      *    Consultar tarjeta
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "Error: " LK-MENSAJE
           ELSE
      *        Permitir pago aunque este bloqueada (solo no inactiva)
               IF TARJ-ESTADO = 'I'
                   DISPLAY "ERROR: La tarjeta esta cancelada."
               ELSE
                   DISPLAY "Liquidacion pendiente: "
                       TARJ-LIQUIDACION-MES
                   DISPLAY "Acumulado mes:         " TARJ-ACUM-MES
                   COMPUTE WS-DEUDA-TOTAL =
                       TARJ-ACUM-MES + TARJ-LIQUIDACION-MES
                   DISPLAY "Deuda total:           " WS-DEUDA-TOTAL
                   DISPLAY "Ingrese monto del pago: "

                   ACCEPT WS-MONTO-PAGO

      *            El pago reduce primero la liquidacion pendiente
                   IF WS-MONTO-PAGO > TARJ-LIQUIDACION-MES
      *                Si el pago cubre la liquidacion, el resto
      *                va contra el acumulado del mes
                       SUBTRACT TARJ-LIQUIDACION-MES
                           FROM WS-MONTO-PAGO
                       MOVE ZERO TO TARJ-LIQUIDACION-MES
                       SUBTRACT WS-MONTO-PAGO FROM TARJ-ACUM-MES
                       IF TARJ-ACUM-MES < ZERO
                           MOVE ZERO TO TARJ-ACUM-MES
                       END-IF
                   ELSE
                       SUBTRACT WS-MONTO-PAGO
                           FROM TARJ-LIQUIDACION-MES
                   END-IF

                   MOVE 'M' TO LK-ACCION-DB
                   CALL 'DBIOTARJ' USING REG-TARJ,
                       LK-DATOS-TRANSACCION

                   IF LK-COD-RETORNO = 0
                       DISPLAY "Pago realizado exitosamente."
                       DISPLAY "Liquidacion restante: "
                           TARJ-LIQUIDACION-MES
                       DISPLAY "Acumulado mes:        "
                           TARJ-ACUM-MES
                   ELSE
                       DISPLAY "Error al procesar pago: " LK-MENSAJE
                   END-IF
               END-IF
           END-IF.

       6000-CONSULTA-DEUDA.
           DISPLAY "--- CONSULTA DE DEUDA ---".

           DISPLAY "Ingrese ID de Cliente: ".
           ACCEPT TARJ-ID-CLIENTE.
           DISPLAY "Ingrese Numero de Tarjeta: ".
           ACCEPT TARJ-NRO-TARJETA.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "Error: " LK-MENSAJE
           ELSE
               COMPUTE WS-DISPONIBLE =
                   TARJ-LIMITE-TARJETA - TARJ-ACUM-MES
               COMPUTE WS-DEUDA-TOTAL =
                   TARJ-ACUM-MES + TARJ-LIQUIDACION-MES

               DISPLAY "======================================"
               DISPLAY "  ESTADO DE CUENTA - TARJETA"
               DISPLAY "======================================"
               DISPLAY "Cliente:           " TARJ-ID-CLIENTE
               DISPLAY "Numero:            " TARJ-NRO-TARJETA
               DISPLAY "Emision:           " TARJ-FECHA-EMISION
               DISPLAY "Vencimiento:       " TARJ-FECHA-VENCIM
               DISPLAY "Estado:            " TARJ-ESTADO
               DISPLAY "--------------------------------------"
               DISPLAY "Limite credito:    " TARJ-LIMITE-TARJETA
               DISPLAY "Acumulado mes:     " TARJ-ACUM-MES
               DISPLAY "Liquidacion pend.: " TARJ-LIQUIDACION-MES
               DISPLAY "--------------------------------------"
               DISPLAY "DEUDA TOTAL:       " WS-DEUDA-TOTAL
               DISPLAY "DISPONIBLE:        " WS-DISPONIBLE
               DISPLAY "======================================"
           END-IF.

       7000-BLOQUEO-TARJETA.
           DISPLAY "--- BLOQUEO DE TARJETA ---".

           DISPLAY "Ingrese ID de Cliente: ".
           ACCEPT TARJ-ID-CLIENTE.
           DISPLAY "Ingrese Numero de Tarjeta: ".
           ACCEPT TARJ-NRO-TARJETA.

      *    Consultar tarjeta
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "Error: " LK-MENSAJE
           ELSE
               IF TARJ-ESTADO = 'I'
                   DISPLAY "ERROR: La tarjeta ya esta cancelada."
               ELSE IF TARJ-ESTADO = 'B'
                   DISPLAY "La tarjeta ya esta bloqueada."
                   DISPLAY "Desea desbloquear? (S/N): "

                   ACCEPT WS-CONFIRMA
                   IF WS-CONFIRMA = 'S' OR 's'
                       MOVE 'A' TO TARJ-ESTADO
                       MOVE 'M' TO LK-ACCION-DB
                       CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                           LK-DATOS-TRANSACCION
                       DISPLAY "Tarjeta desbloqueada exitosamente."
                   END-IF
               ELSE
                   DISPLAY "Confirma bloqueo? (S/N): "

                   ACCEPT WS-CONFIRMA
                   IF WS-CONFIRMA = 'S' OR 's'
                       MOVE 'B' TO TARJ-ESTADO
                       MOVE 'M' TO LK-ACCION-DB
                       CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                           LK-DATOS-TRANSACCION
                       DISPLAY "Tarjeta bloqueada exitosamente."
                       DISPLAY "El cliente puede pagar pero no"
                       " realizar nuevos cargos."
                   END-IF
               END-IF
           END-IF.

       8000-BAJA-TARJETA.
           DISPLAY "--- BAJA (CANCELACION) DE TARJETA ---".

           DISPLAY "Ingrese ID de Cliente: ".
           ACCEPT TARJ-ID-CLIENTE.
           DISPLAY "Ingrese Numero de Tarjeta: ".
           ACCEPT TARJ-NRO-TARJETA.

      *    Consultar primero para validar
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "Error: " LK-MENSAJE
           ELSE
               IF TARJ-ESTADO = 'I'
                   DISPLAY "La tarjeta ya esta cancelada."
               ELSE
                   COMPUTE WS-DEUDA-TOTAL =
                       TARJ-ACUM-MES + TARJ-LIQUIDACION-MES

                   IF WS-DEUDA-TOTAL > ZERO
                       DISPLAY "ERROR: No se puede cancelar una"
                       " tarjeta con deuda pendiente."
                       DISPLAY "Deuda total: " WS-DEUDA-TOTAL
                   ELSE
                       DISPLAY "Confirma cancelacion? (S/N): "

                       ACCEPT WS-CONFIRMA
                       IF WS-CONFIRMA = 'S' OR 's'
                           MOVE 'B' TO LK-ACCION-DB
                           CALL 'DBIOTARJ' USING REG-TARJ,
                               LK-DATOS-TRANSACCION
                           DISPLAY LK-MENSAJE
                       END-IF
                   END-IF
               END-IF
           END-IF.

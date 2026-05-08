       IDENTIFICATION DIVISION.
       PROGRAM-ID. TC0000.
      *================================================================*
      * PROGRAMA: MAINLINE TARJETAS DE CREDITO                         *
      * FUNCION:  Gestiona Emision, Consulta, Cargos, Pagos y Bajas.   *
      * REGLA:    Cliente debe existir y estar activo.                  *
      * REGLA:    Tarjeta debe estar activa para operar.                *
      * REGLA:    Un cliente solo puede tener una tarjeta de credito.   *
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPCION-TARJ       PIC 9(01).
       01  WS-CONTINUAR-TARJ    PIC X(01) VALUE 'S'.
       01  WS-ENTRADA-MONTO     PIC X(13).
       01  WS-CONFIRMA          PIC X(01).

           COPY TARJREC.
           COPY CUSMREC.

       01  WS-CALCULOS-TC.
           05 WS-MONTO-TX        PIC S9(10)V99 VALUE ZERO.
           05 WS-DISPONIBLE      PIC S9(10)V99 VALUE ZERO.
           05 WS-DEUDA-TOTAL     PIC S9(10)V99 VALUE ZERO.
           05 WS-TASA-MENSUAL    PIC S9(03)V9999 VALUE ZERO.
           05 WS-INTERES         PIC S9(10)V99 VALUE ZERO.
           05 WS-TASA-ANUAL      PIC S9(05)V99 VALUE 35.

       01  WS-PROGRAMAS.
           05 WS-PGM-DBIOCUSM    PIC X(8) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOTARJ    PIC X(8) VALUE 'DBIOTARJ'.
           05 WS-PGM-TRAN        PIC X(8) VALUE 'DBIOTRAN'.

       01  WS-FECHA-SISTEMA.
           05 WS-ANIO            PIC 9(04).
           05 WS-MES             PIC 9(02).
           05 WS-DIA             PIC 9(02).

       01  WS-ANIO-VENC          PIC 9(04).

       01  WS-AUX-GEN.
           05 WS-SEMILLA         PIC 9(08).
           05 WS-RANDOM          PIC 9(04).

       01  WS-FECHA-VENC-NUM.
           05 WS-VENC-ANIO       PIC 9(04).
           05 WS-VENC-MES        PIC 9(02).
           05 WS-VENC-DIA        PIC 9(02).

       01  WS-FECHA-HOY          PIC 9(08).
       01  WS-FECHA-VENC-8       PIC 9(08).

       LINKAGE SECTION.
           COPY LKCIF.

       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           PERFORM 1000-PROCESAR-OPCIONES
               UNTIL WS-CONTINUAR-TARJ = 'N' OR 'n'
           GOBACK.

       1000-PROCESAR-OPCIONES.
           DISPLAY "========================================".
           DISPLAY "   MODULO TARJETAS DE CREDITO           ".
           DISPLAY "========================================".
           DISPLAY "1. Emitir Nueva Tarjeta".
           DISPLAY "2. Consultar Tarjeta".
           DISPLAY "3. Registrar Consumo".
           DISPLAY "4. Realizar Pago".
           DISPLAY "5. Consultar Estado de Deuda".
           DISPLAY "6. Bloquear/Activar Tarjeta".
           DISPLAY "7. Cancelar Tarjeta".
           DISPLAY "0. Volver al Menu Principal".
           DISPLAY "========================================".
           DISPLAY "Seleccione operacion: "
           ACCEPT WS-OPCION-TARJ

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
                   DISPLAY "Opcion invalida. Intente de nuevo."
           END-EVALUATE.

       2000-EMISION-TARJETA.
           DISPLAY "--- EMISION DE NUEVA TARJETA ---".
           PERFORM 9000-BUSCAR-CLIENTE

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "ERROR: " LK-MENSAJE
           ELSE
               IF CUSM-CTA-ACTIVA = 0
                   DISPLAY "ERROR: CLIENTE INACTIVO."
                   DISPLAY "NO SE PUEDE EMITIR TARJETA."
               ELSE
                   IF CUSM-TARJETA = 1
                       DISPLAY "ERROR: CLIENTE YA TIENE TARJETA."
                       DISPLAY "NO SE PUEDE EMITIR OTRA TARJETA."
                   ELSE
                       INITIALIZE REG-TARJ
                       MOVE CUSM-ID-CLIENTE TO TARJ-ID-CLIENTE

                       PERFORM 9200-GENERAR-NUMERO-TARJETA

                       ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD
                       STRING WS-ANIO "-" WS-MES "-" WS-DIA
                           DELIMITED BY SIZE
                           INTO TARJ-FECHA-EMISION
                       END-STRING

                       ADD 4 TO WS-ANIO GIVING WS-ANIO-VENC

                       STRING WS-ANIO-VENC "-" WS-MES "-" WS-DIA
                           DELIMITED BY SIZE
                           INTO TARJ-FECHA-VENCIM
                       END-STRING

                       DISPLAY "TARJETA : " TARJ-NRO-TARJETA
                       DISPLAY "EMISION : " TARJ-FECHA-EMISION
                       DISPLAY "VENCE   : " TARJ-FECHA-VENCIM
                       DISPLAY "LIMITE DE CREDITO ($): "
                       ACCEPT WS-ENTRADA-MONTO

                       MOVE FUNCTION NUMVAL(WS-ENTRADA-MONTO)
                           TO TARJ-LIMITE-TARJETA

                       IF TARJ-LIMITE-TARJETA <= 0
                           DISPLAY "ERROR: LIMITE DEBE SER > 0."
                       ELSE
                           MOVE 'A'  TO TARJ-ESTADO
                           MOVE ZERO TO TARJ-ACUM-MES
                           MOVE ZERO TO TARJ-LIQUIDACION-MES

                           MOVE 'A' TO LK-ACCION-DB
                           CALL WS-PGM-DBIOTARJ
                               USING REG-TARJ,
                                     LK-DATOS-TRANSACCION

                           IF LK-COD-RETORNO = 0
                               CALL WS-PGM-TRAN USING 'C'
                               DISPLAY "TARJETA EMITIDA CON EXITO."
                               DISPLAY "NRO: " TARJ-NRO-TARJETA
                           ELSE
                               CALL WS-PGM-TRAN USING 'R'
                               DISPLAY "ERROR: " LK-MENSAJE
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       3000-CONSULTA-TARJETA.
           DISPLAY "--- CONSULTA DE TARJETA ---".
           PERFORM 9100-BUSCAR-TARJETA

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "ERROR: " LK-MENSAJE
           ELSE
               COMPUTE WS-DISPONIBLE =
                   TARJ-LIMITE-TARJETA - TARJ-ACUM-MES

               DISPLAY "========================================"
               DISPLAY "TITULAR   : " CUSM-NOMBRE
               DISPLAY "APELLIDO  : " CUSM-APELLIDOS
               DISPLAY "TARJETA   : " TARJ-NRO-TARJETA
               DISPLAY "ESTADO    : " TARJ-ESTADO
               DISPLAY "EMISION   : " TARJ-FECHA-EMISION
               DISPLAY "VENCE     : " TARJ-FECHA-VENCIM
               DISPLAY "LIMITE    : " TARJ-LIMITE-TARJETA
               DISPLAY "DISPONIBLE: " WS-DISPONIBLE
               DISPLAY "========================================"
           END-IF.

       4000-CARGO-TARJETA.
           DISPLAY "--- REGISTRAR CONSUMO ---".
           PERFORM 9100-BUSCAR-TARJETA

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "ERROR: " LK-MENSAJE
           ELSE
               IF CUSM-CTA-ACTIVA = 0
                   DISPLAY "ERROR: CLIENTE INACTIVO."
               ELSE
                   IF TARJ-ESTADO NOT = 'A'
                       DISPLAY "ERROR: TARJETA NO ACTIVA."
                       DISPLAY "ESTADO: " TARJ-ESTADO
                   ELSE
                       COMPUTE WS-DISPONIBLE =
                           TARJ-LIMITE-TARJETA - TARJ-ACUM-MES

                       DISPLAY "DISPONIBLE ($): "
                       DISPLAY WS-DISPONIBLE
                       DISPLAY "MONTO CONSUMO ($): "
                       ACCEPT WS-ENTRADA-MONTO

                       MOVE FUNCTION NUMVAL(WS-ENTRADA-MONTO)
                           TO TARJ-IMPORTE-MOV

                       IF TARJ-IMPORTE-MOV <= 0
                           DISPLAY "ERROR: MONTO DEBE SER > 0."
                       ELSE
                           IF TARJ-IMPORTE-MOV > WS-DISPONIBLE
                               DISPLAY "ERROR: EXCEDE LIMITE."
                               DISPLAY "DISPONIBLE: "
                               DISPLAY WS-DISPONIBLE
                           ELSE
                               ADD TARJ-IMPORTE-MOV TO TARJ-ACUM-MES

                               MOVE 'M' TO LK-ACCION-DB
                               CALL WS-PGM-DBIOTARJ
                                   USING REG-TARJ,
                                         LK-DATOS-TRANSACCION

                               IF LK-COD-RETORNO = 0
                                   CALL WS-PGM-TRAN USING 'C'
                                   DISPLAY "CONSUMO REGISTRADO."

                                   COMPUTE WS-DISPONIBLE =
                                       TARJ-LIMITE-TARJETA -
                                       TARJ-ACUM-MES

                                   DISPLAY "NUEVO DISPONIBLE: "
                                   DISPLAY WS-DISPONIBLE
                               ELSE
                                   CALL WS-PGM-TRAN USING 'R'
                                   DISPLAY "ERROR: " LK-MENSAJE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       5000-PAGO-TARJETA.
           DISPLAY "--- PAGO DE TARJETA ---".
           PERFORM 9100-BUSCAR-TARJETA

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "ERROR: " LK-MENSAJE
           ELSE
               IF TARJ-ESTADO = 'I'
                   DISPLAY "ERROR: TARJETA CANCELADA."
               ELSE
                   COMPUTE WS-DEUDA-TOTAL =
                       TARJ-ACUM-MES + TARJ-LIQUIDACION-MES

                   IF WS-DEUDA-TOTAL <= 0
                       DISPLAY "NO TIENE DEUDA PENDIENTE."
                   ELSE
                       DISPLAY "==============================="
                       DISPLAY "CONSUMOS MES ACTUAL : "
                       DISPLAY TARJ-ACUM-MES
                       DISPLAY "DEUDA MES ANTERIOR  : "
                       DISPLAY TARJ-LIQUIDACION-MES
                       DISPLAY "TOTAL A PAGAR       : "
                       DISPLAY WS-DEUDA-TOTAL
                       DISPLAY "==============================="
                       DISPLAY "MONTO DEL PAGO ($): "
                       ACCEPT WS-ENTRADA-MONTO

                       MOVE FUNCTION NUMVAL(WS-ENTRADA-MONTO)
                           TO WS-MONTO-TX

                       IF WS-MONTO-TX <= 0
                           DISPLAY "ERROR: MONTO DEBE SER > 0."
                       ELSE
                           IF WS-MONTO-TX > WS-DEUDA-TOTAL
                               DISPLAY "ERROR: PAGO EXCEDE DEUDA."
                               DISPLAY "DEUDA TOTAL: "
                               DISPLAY WS-DEUDA-TOTAL
                           ELSE
                               IF WS-MONTO-TX >= TARJ-LIQUIDACION-MES
                                   SUBTRACT TARJ-LIQUIDACION-MES
                                       FROM WS-MONTO-TX
                                   MOVE ZERO TO TARJ-LIQUIDACION-MES
                                   SUBTRACT WS-MONTO-TX
                                       FROM TARJ-ACUM-MES
                               ELSE
                                   SUBTRACT WS-MONTO-TX
                                       FROM TARJ-LIQUIDACION-MES
                               END-IF

                               MOVE 'M' TO LK-ACCION-DB
                               CALL WS-PGM-DBIOTARJ
                                   USING REG-TARJ,
                                         LK-DATOS-TRANSACCION

                               IF LK-COD-RETORNO = 0
                                   CALL WS-PGM-TRAN USING 'C'
                                   DISPLAY "PAGO PROCESADO."

                                   COMPUTE WS-DEUDA-TOTAL =
                                       TARJ-ACUM-MES +
                                       TARJ-LIQUIDACION-MES

                                   DISPLAY "DEUDA RESTANTE: "
                                   DISPLAY WS-DEUDA-TOTAL
                               ELSE
                                   CALL WS-PGM-TRAN USING 'R'
                                   DISPLAY "ERROR: " LK-MENSAJE
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.

       6000-CONSULTA-DEUDA.
           DISPLAY "--- ESTADO DE DEUDA ---".
           PERFORM 9100-BUSCAR-TARJETA

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "ERROR: " LK-MENSAJE
           ELSE
               COMPUTE WS-DEUDA-TOTAL =
                   TARJ-ACUM-MES + TARJ-LIQUIDACION-MES

               COMPUTE WS-DISPONIBLE =
                   TARJ-LIMITE-TARJETA - TARJ-ACUM-MES

               COMPUTE WS-TASA-MENSUAL =
                   WS-TASA-ANUAL / 12 / 100

               COMPUTE WS-INTERES =
                   TARJ-LIQUIDACION-MES * WS-TASA-MENSUAL

               DISPLAY "==============================="
               DISPLAY "TITULAR    : " CUSM-NOMBRE
               DISPLAY "TARJETA    : " TARJ-NRO-TARJETA
               DISPLAY "ESTADO     : " TARJ-ESTADO
               DISPLAY "-------------------------------"
               DISPLAY "CONSUMOS MES ACTUAL : "
               DISPLAY TARJ-ACUM-MES
               DISPLAY "DEUDA MES ANTERIOR  : "
               DISPLAY TARJ-LIQUIDACION-MES
               DISPLAY "INTERES PROYECTADO  : "
               DISPLAY WS-INTERES
               DISPLAY "-------------------------------"
               DISPLAY "TOTAL A PAGAR       : "
               DISPLAY WS-DEUDA-TOTAL
               DISPLAY "CREDITO DISPONIBLE  : "
               DISPLAY WS-DISPONIBLE
               DISPLAY "TASA ANUAL VIGENTE  : "
               DISPLAY WS-TASA-ANUAL
               DISPLAY "==============================="
           END-IF.

       7000-BLOQUEO-TARJETA.
           DISPLAY "--- BLOQUEO/ACTIVACION ---".
           PERFORM 9100-BUSCAR-TARJETA

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "ERROR: " LK-MENSAJE
           ELSE
               IF TARJ-ESTADO = 'I'
                   DISPLAY "ERROR: TARJETA CANCELADA."
               ELSE
                   IF TARJ-ESTADO = 'B'
                       DISPLAY "TARJETA BLOQUEADA."
                       DISPLAY "DESEA ACTIVARLA? (S/N): "
                       ACCEPT WS-CONFIRMA

                       IF WS-CONFIRMA = 'S' OR 's'
                           MOVE 'A' TO TARJ-ESTADO
                           MOVE 'M' TO LK-ACCION-DB

                           CALL WS-PGM-DBIOTARJ
                               USING REG-TARJ,
                                     LK-DATOS-TRANSACCION

                           IF LK-COD-RETORNO = 0
                               CALL WS-PGM-TRAN USING 'C'
                               DISPLAY "TARJETA ACTIVADA."
                           ELSE
                               CALL WS-PGM-TRAN USING 'R'
                               DISPLAY "ERROR: " LK-MENSAJE
                           END-IF
                       ELSE
                           DISPLAY "OPERACION CANCELADA."
                       END-IF
                   ELSE
                       DISPLAY "TARJETA ACTIVA."
                       DISPLAY "DESEA BLOQUEARLA? (S/N): "
                       ACCEPT WS-CONFIRMA

                       IF WS-CONFIRMA = 'S' OR 's'
                           MOVE 'B' TO TARJ-ESTADO
                           MOVE 'M' TO LK-ACCION-DB

                           CALL WS-PGM-DBIOTARJ
                               USING REG-TARJ,
                                     LK-DATOS-TRANSACCION

                           IF LK-COD-RETORNO = 0
                               CALL WS-PGM-TRAN USING 'C'
                               DISPLAY "TARJETA BLOQUEADA."
                           ELSE
                               CALL WS-PGM-TRAN USING 'R'
                               DISPLAY "ERROR: " LK-MENSAJE
                           END-IF
                       ELSE
                           DISPLAY "OPERACION CANCELADA."
                       END-IF
                   END-IF
               END-IF
           END-IF.

       8000-BAJA-TARJETA.
           DISPLAY "--- CANCELACION DEFINITIVA ---".
           PERFORM 9100-BUSCAR-TARJETA

           IF LK-COD-RETORNO NOT = 0
               DISPLAY "ERROR: " LK-MENSAJE
           ELSE
               COMPUTE WS-DEUDA-TOTAL =
                   TARJ-ACUM-MES + TARJ-LIQUIDACION-MES

               IF WS-DEUDA-TOTAL > 0
                   DISPLAY "ERROR: NO PUEDE CANCELAR CON DEUDA."
                   DISPLAY "DEUDA: " WS-DEUDA-TOTAL
               ELSE
                   DISPLAY "CONFIRMA CANCELACION? (S/N): "
                   ACCEPT WS-CONFIRMA

                   IF WS-CONFIRMA = 'S' OR 's'
                       MOVE 'B' TO LK-ACCION-DB

                       CALL WS-PGM-DBIOTARJ
                           USING REG-TARJ,
                                 LK-DATOS-TRANSACCION

                       IF LK-COD-RETORNO = 0
                           CALL WS-PGM-TRAN USING 'C'
                           DISPLAY "TARJETA CANCELADA."
                       ELSE
                           CALL WS-PGM-TRAN USING 'R'
                           DISPLAY "ERROR: " LK-MENSAJE
                       END-IF
                   ELSE
                       DISPLAY "OPERACION CANCELADA."
                   END-IF
               END-IF
           END-IF.

       9000-BUSCAR-CLIENTE.
           INITIALIZE REG-CUSM
           DISPLAY "Ingrese Cedula del Cliente: "
           ACCEPT CUSM-DOC-CLIENTE

           IF CUSM-DOC-CLIENTE = SPACES
               DISPLAY "ERROR: Cedula no puede estar vacia."
               MOVE 01 TO LK-COD-RETORNO
               MOVE "CEDULA VACIA" TO LK-MENSAJE
           ELSE
               MOVE 'C' TO LK-ACCION-DB

               CALL WS-PGM-DBIOCUSM
                   USING REG-CUSM,
                         LK-DATOS-TRANSACCION

               IF LK-COD-RETORNO NOT = 0
                   MOVE "CLIENTE NO ENCONTRADO" TO LK-MENSAJE
               END-IF
           END-IF.

       9100-BUSCAR-TARJETA.
           PERFORM 9000-BUSCAR-CLIENTE

           IF LK-COD-RETORNO = 0
               INITIALIZE REG-TARJ
               MOVE CUSM-ID-CLIENTE TO TARJ-ID-CLIENTE

               DISPLAY "Numero de Tarjeta: "
               ACCEPT TARJ-NRO-TARJETA

               IF TARJ-NRO-TARJETA = SPACES
                   DISPLAY "ERROR: Numero de tarjeta vacio."
                   MOVE 01 TO LK-COD-RETORNO
                   MOVE "TARJETA VACIA" TO LK-MENSAJE
               ELSE
                   MOVE 'C' TO LK-ACCION-DB

                   CALL WS-PGM-DBIOTARJ
                       USING REG-TARJ,
                             LK-DATOS-TRANSACCION

                   IF LK-COD-RETORNO NOT = 0
                       DISPLAY "ERROR: TARJETA NO ENCONTRADA."
                   ELSE
                       ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD

                       STRING WS-ANIO WS-MES WS-DIA
                           DELIMITED BY SIZE
                           INTO WS-FECHA-HOY
                       END-STRING

                       UNSTRING TARJ-FECHA-VENCIM
                           DELIMITED BY '-'
                           INTO WS-VENC-ANIO
                                WS-VENC-MES
                                WS-VENC-DIA
                       END-UNSTRING

                       STRING WS-VENC-ANIO WS-VENC-MES WS-VENC-DIA
                           DELIMITED BY SIZE
                           INTO WS-FECHA-VENC-8
                       END-STRING

                       IF WS-FECHA-VENC-8 < WS-FECHA-HOY
                           DISPLAY "ERROR: TARJETA VENCIDA."
                           DISPLAY "VENCIMIENTO: "
                           DISPLAY TARJ-FECHA-VENCIM
                           MOVE 01 TO LK-COD-RETORNO
                           MOVE "TARJETA VENCIDA" TO LK-MENSAJE
                       END-IF
                   END-IF
               END-IF
           END-IF.

       9200-GENERAR-NUMERO-TARJETA.
           ACCEPT WS-SEMILLA FROM TIME

           COMPUTE WS-RANDOM =
               FUNCTION RANDOM(WS-SEMILLA) * 10000

           STRING "4555" CUSM-ID-CLIENTE WS-RANDOM
               DELIMITED BY SIZE
               INTO TARJ-NRO-TARJETA
           END-STRING.

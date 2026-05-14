       IDENTIFICATION DIVISION.
       PROGRAM-ID. IN0000.
      *================================================================*
      * PROGRAMA: MAINLINE INVM (CUENTAS CORRIENTES)                   *
      * FUNCION:  Gestiona Depositos, Extracciones y Consultas.        *
      * REGLA:    No permitir saldo negativo.                          *
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPCION-INVM      PIC 9(01).
       01  WS-CONTINUAR-INVM   PIC X(01) VALUE 'S'.
       01  WS-MONTO-TX         PIC 9(10)V99.

       01  WS-PROGRAMAS.
           05 WS-PGM-DBIOINVM    PIC X(8) VALUE 'DBIOINVM'.
           05 WS-PGM-DBIOCUSM    PIC X(08) VALUE 'DBIOCUSM'.
           COPY INVMREC.
           COPY CUSMREC.

       01  WS-VALIDACION.
           05  WS-MONTO-ENTRADA    PIC X(12).
           05  WS-ES-VALIDO        PIC X(01).
               88  ES-NUMERO       VALUE 'S'.
               88  NO-ES-NUMERO    VALUE 'N'.

       01  WS-AUXILIARES-VALIDACION.
           05 WS-I                PIC 9(02).
           05 WS-DOC-LEN          PIC 9(02).
           05 WS-DOC-VALIDO       PIC X(01) VALUE 'N'.

       01  WS-AUXILIARES.
           05  WS-ACCION-TEMP      PIC X(01).
       LINKAGE SECTION.
           COPY LKCIF.


       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           PERFORM 1000-PROCESAR-OPCIONES
                   UNTIL WS-CONTINUAR-INVM = 'N' OR 'n'.
           GOBACK.

       1000-PROCESAR-OPCIONES.
           DISPLAY "========================================".
           DISPLAY "            BANCOCORE S.A.             ".
           DISPLAY "   MODULO DE CUENTAS CORRIENTES (INVM)  ".
           DISPLAY "========================================".
           DISPLAY "Sucursal: 001    Operador: MGONZALEZ".
           DISPLAY "----------------------------------------".
           DISPLAY " 1. Consultar Saldo".
           DISPLAY " 2. Realizar Deposito".
           DISPLAY " 3. Realizar Extraccion".
           DISPLAY " 0. Volver al Menu Principal".
           DISPLAY "========================================".
           DISPLAY "Ingrese opcion y presione ENTER: "
           ACCEPT WS-OPCION-INVM.

           EVALUATE WS-OPCION-INVM
               WHEN 1
                   PERFORM 2000-CONSULTA-SALDO
               WHEN 2
                   PERFORM 3000-DEPOSITO
               WHEN 3
                   PERFORM 4000-EXTRACCION
               WHEN 0
                   MOVE 'N' TO WS-CONTINUAR-INVM
               WHEN OTHER
                   DISPLAY "Opcion invalida."
           END-EVALUATE.

       2000-CONSULTA-SALDO.
           DISPLAY "--- CONSULTA DE SALDO ---".
           MOVE 'C' TO LK-ACCION-DB.
           PERFORM 9000-BUSCAR-CLIENTE-Y-CUENTA.

           IF LK-COD-RETORNO = 0
               DISPLAY "CLIENTE: " CUSM-NOMBRE " " CUSM-APELLIDOS
               DISPLAY "SALDO ACTUAL: " INVM-SALDO-ACTUAL
           ELSE
               DISPLAY "ERROR: " LK-MENSAJE
           END-IF.

       3000-DEPOSITO.
           DISPLAY "--- DEPOSITO EN EFECTIVO ---".
           MOVE 'L' TO LK-ACCION-DB.

           PERFORM 9000-BUSCAR-CLIENTE-Y-CUENTA.

           IF LK-COD-RETORNO = 0
               *> Validamos estado antes de pedir dinero
               IF CUSM-CTA-ACTIVA = 0
                   PERFORM 9110-MOSTRAR-ERROR-INACTIVA
                   CALL 'DBIOTRAN' USING 'R'
               ELSE

                   PERFORM 9300-VALIDAR-MONTO

                   IF WS-MONTO-TX > 0
                       ADD WS-MONTO-TX TO INVM-SALDO-ACTUAL ROUNDED
                       MOVE 2            TO INVM-COD-ULT-MOV
                       MOVE WS-MONTO-TX  TO INVM-IMPORTE-MOV

                       MOVE 'M' TO LK-ACCION-DB
                       CALL WS-PGM-DBIOINVM USING REG-INVM,
                                                  LK-DATOS-TRANSACCION
                       IF LK-COD-RETORNO = 0
                           *> 2. ACTUALIZAR SALDO GLOBAL DEL CLIENTE
                           ADD WS-MONTO-TX TO CUSM-SALDO-CLIENTE ROUNDED

                           MOVE 'M' TO LK-ACCION-DB
                           CALL WS-PGM-DBIOCUSM
                           USING REG-CUSM, LK-DATOS-TRANSACCION


                           PERFORM 9200-GESTIONAR-TRANSACCION
                       ELSE
                           CALL 'DBIOTRAN' USING 'R'
                           DISPLAY "ERROR AL ACTUALIZAR CUENTA."
                       END-IF

                   ELSE
                       DISPLAY "ERROR: EL MONTO DEBE SER MAYOR A CERO."
                   END-IF
               END-IF
           ELSE
               DISPLAY "ERROR: " LK-MENSAJE
           END-IF.

       4000-EXTRACCION.
           DISPLAY "--- EXTRACCION DE EFECTIVO ---".
           MOVE 'L' TO LK-ACCION-DB.
           PERFORM 9000-BUSCAR-CLIENTE-Y-CUENTA.

           IF LK-COD-RETORNO = 0
               IF CUSM-CTA-ACTIVA = 0
                   PERFORM 9110-MOSTRAR-ERROR-INACTIVA
                   CALL 'DBIOTRAN' USING 'R'
               ELSE
                   DISPLAY "SALDO DISPONIBLE: " INVM-SALDO-ACTUAL
                   PERFORM 9300-VALIDAR-MONTO

                   IF WS-MONTO-TX > 0
                       IF WS-MONTO-TX <= INVM-SALDO-ACTUAL
                           *> 1. ACTUALIZAR SALDO DE LA CUENTA
                           SUBTRACT WS-MONTO-TX
                                    FROM INVM-SALDO-ACTUAL ROUNDED
                           MOVE 3 TO INVM-COD-ULT-MOV
                           COMPUTE INVM-IMPORTE-MOV = WS-MONTO-TX * -1

                           MOVE 'M' TO LK-ACCION-DB
                           CALL WS-PGM-DBIOINVM
                           USING REG-INVM, LK-DATOS-TRANSACCION

                           IF LK-COD-RETORNO = 0
                               *> 2. SINCRONIZAR SALDO MAESTRO DEL CLIENTE
                               SUBTRACT WS-MONTO-TX
                                        FROM CUSM-SALDO-CLIENTE ROUNDED

                               MOVE 'M' TO LK-ACCION-DB
                               CALL WS-PGM-DBIOCUSM
                               USING REG-CUSM, LK-DATOS-TRANSACCION

                               *> 3. GESTIONAR EL COMMIT O ROLLBACK
                               PERFORM 9200-GESTIONAR-TRANSACCION
                           ELSE
                               CALL 'DBIOTRAN' USING 'R'
                               DISPLAY "ERROR AL ACTUALIZAR CUENTA."
                           END-IF
                       ELSE
                           DISPLAY "ERROR: FONDOS INSUFICIENTES."
                           CALL 'DBIOTRAN' USING 'R'
                       END-IF
                   ELSE
                       DISPLAY "ERROR: EL MONTO DEBE SER MAYOR A CERO."
                   END-IF
               END-IF
           ELSE
               DISPLAY "ERROR: " LK-MENSAJE
           END-IF.
       9000-BUSCAR-CLIENTE-Y-CUENTA.
           INITIALIZE REG-CUSM.
           INITIALIZE REG-INVM.

           PERFORM 9100-VALIDAR-DOC-CAPTURA.

           *> Resguardamos la intenci�n original ('L' o 'C')
           MOVE LK-ACCION-DB TO WS-ACCION-TEMP.

           *> Buscamos primero al cliente (Siempre con 'C')
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               MOVE CUSM-ID-CLIENTE TO INVM-ID-CLIENTE

               *> Restauramos la acci�n para la cuenta
               MOVE WS-ACCION-TEMP TO LK-ACCION-DB
               CALL WS-PGM-DBIOINVM USING REG-INVM, LK-DATOS-TRANSACCION

               IF LK-COD-RETORNO NOT = 0
                   MOVE "CUENTA NO ENCONTRADA PARA ESTE CLIENTE"
                   TO LK-MENSAJE
               END-IF
           ELSE
               MOVE "CLIENTE NO EXISTE" TO LK-MENSAJE
               MOVE 01 TO LK-COD-RETORNO
           END-IF.

       9100-VALIDAR-DOC-CAPTURA.
           MOVE 'N' TO WS-DOC-VALIDO.

           PERFORM UNTIL WS-DOC-VALIDO = 'S'
               DISPLAY "----------------------------------------------"
               DISPLAY "INGRESE DOCUMENTO (8 CED / 12 PAS): "
               ACCEPT CUSM-DOC-CLIENTE

      * Calcular longitud real (eliminando espacios a la derecha)
               MOVE 0 TO WS-DOC-LEN
               MOVE 12 TO WS-I
               PERFORM UNTIL WS-I = 0 OR CUSM-DOC-CLIENTE(WS-I:1)
               NOT = SPACE
                   SUBTRACT 1 FROM WS-I
               END-PERFORM
               MOVE WS-I TO WS-DOC-LEN


               IF WS-DOC-LEN >= 8 AND WS-DOC-LEN <= 12
                   MOVE 'S' TO WS-DOC-VALIDO

                   IF WS-DOC-LEN = 8
                       MOVE "CED" TO CUSM-TIPO-DOC
                   ELSE
                       MOVE "PAS" TO CUSM-TIPO-DOC
                   END-IF

                   DISPLAY ">>> SISTEMA: DOCUMENTO VALIDO"
                   DISPLAY ">>> TIPO ASIGNADO: " CUSM-TIPO-DOC
               ELSE
                   DISPLAY "ERROR: LONGITUD INVALIDA (" WS-DOC-LEN ")"
                   DISPLAY "DEBE TENER ENTRE 8 Y 12 CARACTERES."
                   DISPLAY "POR FAVOR, INTENTE DE NUEVO."
               END-IF
           END-PERFORM.

       9110-MOSTRAR-ERROR-INACTIVA.
           DISPLAY "******************************************"
           DISPLAY "ERROR: CUENTA CERRADA / INACTIVA"
           DISPLAY "OPERACION NO PERMITIDA."
           DISPLAY "******************************************".

       9200-GESTIONAR-TRANSACCION.
           IF LK-COD-RETORNO = 0
               CALL 'DBIOTRAN' USING 'C'
               DISPLAY "TRANSACCION EXITOSA. NUEVO SALDO: "
                       INVM-SALDO-ACTUAL
           ELSE
               CALL 'DBIOTRAN' USING 'R'
               DISPLAY "ERROR CRITICO AL REGISTRAR EN BD."
           END-IF.

       9300-VALIDAR-MONTO.
           SET NO-ES-NUMERO TO TRUE
           PERFORM UNTIL ES-NUMERO
               MOVE SPACES TO WS-MONTO-ENTRADA
               DISPLAY "INGRESE MONTO (0.00): "
               ACCEPT WS-MONTO-ENTRADA

               IF WS-MONTO-ENTRADA = SPACES
                   DISPLAY "ERROR: DEBE INGRESAR UN VALOR"
               ELSE

                   IF WS-MONTO-ENTRADA(1:1)
                       IS NUMERIC OR WS-MONTO-ENTRADA(1:1) = "."
                       COMPUTE WS-MONTO-TX =
                           FUNCTION NUMVAL(WS-MONTO-ENTRADA)
                       IF WS-MONTO-TX > 0
                           SET ES-NUMERO TO TRUE
                       ELSE
                           DISPLAY "ERROR: MONTO DEBE SER MAYOR A CERO"
                       END-IF
                   ELSE
                       DISPLAY "ERROR: SOLO SE PERMITEN NUMEROS Y PUNTO"
                   END-IF
               END-IF
           END-PERFORM.

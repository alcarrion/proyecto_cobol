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

       LINKAGE SECTION.
           COPY LKCIF.


       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           PERFORM 1000-PROCESAR-OPCIONES
                   UNTIL WS-CONTINUAR-INVM = 'N' OR 'n'.
           GOBACK.

       1000-PROCESAR-OPCIONES.
           DISPLAY "========================================".
           DISPLAY "   MODULO DE CUENTAS CORRIENTES (INVM)  ".
           DISPLAY "========================================".
           DISPLAY "1. Consultar Saldo".
           DISPLAY "2. Realizar Deposito".
           DISPLAY "3. Realizar Extraccion".
           DISPLAY "0. Volver al Menu Principal".
           DISPLAY "========================================".
           DISPLAY "Seleccione operacion: "
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
           PERFORM 9000-BUSCAR-CLIENTE-Y-CUENTA.

           IF LK-COD-RETORNO = 0
               DISPLAY "CLIENTE: " CUSM-NOMBRE " " CUSM-APELLIDOS
               DISPLAY "SALDO ACTUAL: " INVM-SALDO-ACTUAL
           ELSE
               DISPLAY "ERROR: " LK-MENSAJE
           END-IF.

       3000-DEPOSITO.
           DISPLAY "--- DEPOSITO EN EFECTIVO ---".
           PERFORM 9000-BUSCAR-CLIENTE-Y-CUENTA.
           IF LK-COD-RETORNO = 0
               DISPLAY "INGRESE MONTO A DEPOSITAR: "
               ACCEPT WS-MONTO-TX

               IF WS-MONTO-TX > 0
                   ADD WS-MONTO-TX TO INVM-SALDO-ACTUAL
                   MOVE 2            TO INVM-COD-ULT-MOV
                   MOVE WS-MONTO-TX  TO INVM-IMPORTE-MOV

                   MOVE 'M' TO LK-ACCION-DB
                   CALL WS-PGM-DBIOINVM
                   USING REG-INVM, LK-DATOS-TRANSACCION

                   IF LK-COD-RETORNO = 0
                       CALL 'DBIOTRAN' USING 'C'
                       DISPLAY "DEPOSITO EXITOSO. NUEVO SALDO: "
                               INVM-SALDO-ACTUAL
                   ELSE
                       CALL 'DBIOTRAN' USING 'R'
                       DISPLAY "ERROR AL REGISTRAR MOVIMIENTO."
                   END-IF
               ELSE
                   DISPLAY "ERROR: EL MONTO DEBE SER MAYOR A CERO."
               END-IF
           ELSE
               DISPLAY "ERROR: " LK-MENSAJE
           END-IF.

       4000-EXTRACCION.
           DISPLAY "--- EXTRACCION DE EFECTIVO ---".
           PERFORM 9000-BUSCAR-CLIENTE-Y-CUENTA.

           IF LK-COD-RETORNO = 0
               DISPLAY "SALDO DISPONIBLE: " INVM-SALDO-ACTUAL
               DISPLAY "INGRESE MONTO A EXTRAER: "
               ACCEPT WS-MONTO-TX

               IF WS-MONTO-TX <= 0
                   DISPLAY "ERROR: EL MONTO DEBE SER MAYOR A CERO."
               ELSE
                   IF WS-MONTO-TX > INVM-SALDO-ACTUAL
                       DISPLAY "FONDOS INSUFICIENTES."
                   ELSE
                       SUBTRACT WS-MONTO-TX FROM INVM-SALDO-ACTUAL
                       MOVE 3            TO INVM-COD-ULT-MOV *> Codigo Retiro
                       COMPUTE INVM-IMPORTE-MOV = WS-MONTO-TX * -1

                       MOVE 'M' TO LK-ACCION-DB
                       CALL WS-PGM-DBIOINVM USING REG-INVM,
                                                  LK-DATOS-TRANSACCION

                       IF LK-COD-RETORNO = 0
                           CALL 'DBIOTRAN' USING 'C'
                           DISPLAY "RETIRO EXITOSO. NUEVO SALDO: "
                                   INVM-SALDO-ACTUAL
                       ELSE
                           CALL 'DBIOTRAN' USING 'R'
                           DISPLAY "ERROR AL PROCESAR RETIRO."
                       END-IF
                   END-IF
               END-IF
           ELSE
               DISPLAY "ERROR: " LK-MENSAJE
           END-IF.

       9000-BUSCAR-CLIENTE-Y-CUENTA.
      *    Busca primero al cliente para obtener el ID interno via Cedula
           INITIALIZE REG-CUSM.
           DISPLAY "INGRESE DOC DEL CLIENTE: "
           ACCEPT CUSM-DOC-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
      *        Si el cliente existe, usamos su ID para buscar la cuenta
               MOVE CUSM-ID-CLIENTE TO INVM-ID-CLIENTE
               MOVE 'C' TO LK-ACCION-DB
               CALL WS-PGM-DBIOINVM USING REG-INVM, LK-DATOS-TRANSACCION
           ELSE
               MOVE "CLIENTE NO ENCONTRADO" TO LK-MENSAJE
               MOVE 01 TO LK-COD-RETORNO
           END-IF.

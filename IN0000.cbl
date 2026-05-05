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
 
       01 WS-OPCION-INVM      PIC 9(01).
       01 WS-CONTINUAR-INVM   PIC X(01) VALUE 'S'.
       01 WS-MONTO-TX         PIC 9(10)V99.
 
           COPY INVMREC.
       
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
           MOVE 'C' TO LK-ACCION-DB.
           DISPLAY "Ingrese ID de Cliente: " 
           ACCEPT INVM-ID-CLIENTE.
           
           CALL 'DBIOINVM' USING REG-INVM, LK-DATOS-TRANSACCION.
           
           IF LK-COD-RETORNO = 0
               DISPLAY "Saldo Actual: " INVM-SALDO-ACTUAL
           ELSE
               DISPLAY LK-MENSAJE
           END-IF.
 
       3000-DEPOSITO.
           DISPLAY "--- DEPOSITO EN EFECTIVO ---".
      *    Primero leemos el saldo actual
           MOVE 'C' TO LK-ACCION-DB.
           DISPLAY "Ingrese ID de Cliente: " 
           ACCEPT INVM-ID-CLIENTE.
           CALL 'DBIOINVM' USING REG-INVM, LK-DATOS-TRANSACCION.
           
           IF LK-COD-RETORNO = 0
               DISPLAY "Ingrese Monto a Depositar: " 
               ACCEPT WS-MONTO-TX
               
               ADD WS-MONTO-TX TO INVM-SALDO-ACTUAL
               MOVE 2 TO INVM-COD-ULT-MOV  *> 2 = Deposito
               MOVE WS-MONTO-TX TO INVM-IMPORTE-MOV
               
      *        Actualizamos en BD
               MOVE 'M' TO LK-ACCION-DB
               CALL 'DBIOINVM' USING REG-INVM, LK-DATOS-TRANSACCION
               DISPLAY "Deposito realizado. " LK-MENSAJE
               DISPLAY "Nuevo Saldo: " INVM-SALDO-ACTUAL
           ELSE
               DISPLAY LK-MENSAJE
           END-IF.
 
       4000-EXTRACCION.
           DISPLAY "--- EXTRACCION DE EFECTIVO ---".
           MOVE 'C' TO LK-ACCION-DB.
           DISPLAY "Ingrese ID de Cliente: " 
           ACCEPT INVM-ID-CLIENTE.
           CALL 'DBIOINVM' USING REG-INVM, LK-DATOS-TRANSACCION.
           
           IF LK-COD-RETORNO = 0
               DISPLAY "Saldo Disponible: " INVM-SALDO-ACTUAL
               DISPLAY "Ingrese Monto a Extraer: " 
               ACCEPT WS-MONTO-TX
               
      *        REGLA: Validar saldo negativo
               IF WS-MONTO-TX > INVM-SALDO-ACTUAL
                   DISPLAY "FONDOS INSUFICIENTES."
               ELSE
                   SUBTRACT WS-MONTO-TX FROM INVM-SALDO-ACTUAL
                   MOVE 15 TO INVM-COD-ULT-MOV *> Asumiendo un codigo de retiro
                   COMPUTE INVM-IMPORTE-MOV = WS-MONTO-TX * -1
                   
                   MOVE 'M' TO LK-ACCION-DB
                   CALL 'DBIOINVM' USING REG-INVM, LK-DATOS-TRANSACCION
                   DISPLAY "Extraccion realizada. " LK-MENSAJE
                   DISPLAY "Nuevo Saldo: " INVM-SALDO-ACTUAL
               END-IF
           ELSE
               DISPLAY LK-MENSAJE
           END-IF.

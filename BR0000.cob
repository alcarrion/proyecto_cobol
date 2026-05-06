      ******************************************************************
      * Author: CORE-BANCARIO-TEAM
      * Date: 02-05-2026
      * Purpose: Modulo para hipotecas
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BR0000.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPCION-HIP      PIC 9.
       01 WS-CONFIRMACION    PIC X.
       01 WS-PROGRAMAS.
           05 WS-PGM-DBIOBORM    PIC X(8) VALUE 'DBIOBORM'.
       COPY BORMREC.

       LINKAGE SECTION.
       COPY LKCIF.

       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.
       MAIN-BR0000.
           PERFORM UNTIL WS-OPCION-HIP = 0
               DISPLAY " "
               DISPLAY "========================================"
               DISPLAY "       MODULO DE HIPOTECAS (BORM)       "
               DISPLAY "========================================"
               DISPLAY "1. Registrar nuevo prestamo hipotecario"
               DISPLAY "2. Consultar saldo adeudado"
               DISPLAY "3. Actualizar estado de pagos (Manual)"
               DISPLAY "0. Volver al menu principal"
               DISPLAY "========================================"
               DISPLAY "Seleccione una opcion: "
               ACCEPT WS-OPCION-HIP

               EVALUATE WS-OPCION-HIP
                   WHEN 1
                       PERFORM 1000-REGISTRAR
                   WHEN 2
                       PERFORM 2000-CONSULTAR
                   WHEN 3
                       PERFORM 3000-ACTUALIZAR
                   WHEN 0
                       MOVE 0 TO LK-COD-RETORNO
                       MOVE "OPERACION FINALIZADA" TO LK-MENSAJE
                       EXIT PROGRAM
                   WHEN OTHER
                       DISPLAY "OPCION INVALIDA"
               END-EVALUATE
           END-PERFORM.
           EXIT PROGRAM.
       1000-REGISTRAR.
           DISPLAY "--- REGISTRO DE NUEVA HIPOTECA ---"
           DISPLAY "Ingrese ID del Cliente: "
           ACCEPT BORM-ID-CLIENTE
           DISPLAY "Monto del Prestamo: "
           ACCEPT BORM-MONTO-ORIGINAL
           DISPLAY "Tasa de Interes (%): "
           ACCEPT BORM-TASA-INTERES
           DISPLAY "Fecha Inicio (AAAA-MM-DD): "
           ACCEPT BORM-FECHA-INICIO
           DISPLAY "Fecha Vencimiento (AAAA-MM-DD): "
           ACCEPT BORM-FECHA-VENCTO

           MOVE BORM-MONTO-ORIGINAL TO BORM-SALDO-ACTUAL
           MOVE "Activa" TO BORM-ESTADO
           MOVE "A" TO LK-MODO-OPERACION

           DISPLAY "Confirmar registro? (S/N): "
           ACCEPT WS-CONFIRMACION

           IF WS-CONFIRMACION = 'S' OR 's'
               CALL WS-PGM-DBIOBORM USING BORM-REGISTRO LK-DATOS-TRANSACCION
               DISPLAY "RESULTADO: " LK-MENSAJE
           ELSE
               DISPLAY "OPERACION CANCELADA"
           END-IF.

       2000-CONSULTAR.
           DISPLAY "--- CONSULTA DE SALDO ---"
           DISPLAY "Ingrese ID del Cliente: "
           ACCEPT BORM-ID-CLIENTE
           MOVE "C" TO LK-MODO-OPERACION

           CALL WS-PGM-DBIOBORM USING BORM-REGISTRO LK-DATOS-TRANSACCION

           IF LK-COD-RETORNO = 0
               DISPLAY "------------------------------------"
               DISPLAY "CLIENTE: " BORM-ID-CLIENTE
               DISPLAY "SALDO ACTUAL: $" BORM-SALDO-ACTUAL
               DISPLAY "ESTADO: " BORM-ESTADO
               DISPLAY "VENCE: " BORM-FECHA-VENCTO
               DISPLAY "------------------------------------"
           ELSE
               DISPLAY "ERROR: " LK-MENSAJE
           END-IF.

       3000-ACTUALIZAR.
           DISPLAY "--- ACTUALIZACION DE PAGO MENSUAL ---"
           DISPLAY "Ingrese ID del Cliente: "
           ACCEPT BORM-ID-CLIENTE
           MOVE "U" TO LK-MODO-OPERACION

           MOVE 0 TO LK-IMPORTE-TRANSACCION
           PERFORM UNTIL
               LK-IMPORTE-TRANSACCION < 0
           DISPLAY "Ingrese monto del pago: "
           ACCEPT LK-IMPORTE-TRANSACCION

           IF LK-IMPORTE-TRANSACCION <= 0
                   DISPLAY "ERROR: El monto debe ser positivo."
               END-IF
           END-PERFORM
           CALL WS-PGM-DBIOBORM USING BORM-REGISTRO LK-DATOS-TRANSACCION
           DISPLAY "RESULTADO: " LK-MENSAJE.

       END PROGRAM BR0000.

      ******************************************************************
      * Author: EQUIPO (LUIS. ALISON. FRANKLIN.)
      * Date: 02-05-2025
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCO-CORE.
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WS-MENU.
               05 WS-OPCION      PIC 9(2).

           01 WS-CONTINUAR       PIC X VALUE 'S'.

           01 WS-DATOS-TRANSACCIONALES.
               05 WS-COD-RETORNO PIC 9(2) VALUE 0.
               05 WS-MENSAJE     PIC X(50).

           COPY LKCIF.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            PERFORM 001-MENU-OPCION.
            DISPLAY 'SALIENDO DEL PROGRAMA'
            STOP RUN.

       001-MENU-OPCION.
           PERFORM UNTIL WS-CONTINUAR = 'N' OR 'n'
               DISPLAY "========================================"
               DISPLAY "   SISTEMA BANCARIO INTEGRADO v1.0"
               DISPLAY "========================================"
               DISPLAY "1. Gestion de Clientes"
               DISPLAY "2. Cuentas Corrientes"
               DISPLAY "3. Tarjetas de Credito"
               DISPLAY "4. Hipotecas"
               DISPLAY "5. Cierre Mensual"
               DISPLAY "6. Reportes Gerenciales"
               DISPLAY "0. Salir"
               DISPLAY "========================================"
               ACCEPT WS-OPCION

           EVALUATE WS-OPCION
      *    MAIN-LINE-CLIENTES
               WHEN 1
                   CALL 'CI0000' USING LK-DATOS-TRANSACCION
               WHEN 2
                   CALL 'IN0000' USING WS-DATOS-TRANSACCIONALES
               WHEN 3
                   CALL 'TC0000' USING WS-DATOS-TRANSACCIONALES
               WHEN 4
                   CALL 'BR0000' USING WS-DATOS-TRANSACCIONALES
               WHEN 5
                   CALL 'BAT000' USING WS-DATOS-TRANSACCIONALES
               WHEN 0
                   MOVE 'N' TO WS-CONTINUAR
               WHEN OTHER
                   DISPLAY 'Opcion Invalida. Intente de nuevo...'
           END-EVALUATE

               IF WS-OPCION NOT = 0
                   DISPLAY 'Resultado: ' WS-MENSAJE
                   DISPLAY 'Presione Enter para continuar...'
                   ACCEPT WS-OPCION
               END-IF
           END-PERFORM.


       END PROGRAM BANCO-CORE.

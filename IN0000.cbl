       IDENTIFICATION DIVISION.
       PROGRAM-ID. IN0000.
      *AUTHOR.     CORE-BANCARIO-TEAM.
      *================================================================*
      * PROGRAMA: MAINLINE CIF (CUSTOMER INFORMATION FACILITY)         *
      * FUNCION:  Modulo de Depositos / Cuentas Corrientes (CTA.CTE)
      *           Maneja: Alta, Deposito, Extraccion, Consulta
      *           de la tabla INVM (Maestro de Cuentas)
      * REGLA:    Valida que el cliente tenga una cuenta asociada (INVM)*
      *================================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OPTION              PIC 9(01).
       01 WS-TIPO-MOV-TEXTO      PIC X(12).

      *=================================================================
      *     AREA DE ENTRADA AL DBIO
      *=================================================================
       01 INPUT-AREA.
           05 OPER                PIC X(01).
      *        'I' = Movimiento (deposito o retiro)
      *        'C' = Consulta saldo
      *        'D' = Extracto detallado
           05 IN-ID-CLIENTE       PIC 9(08).
           05 IN-COD-MOV          PIC 9(02).
      *        2 = Deposito
      *        3 = Retiro
           05 IN-FECHA-ULT-MOV   PIC X(08).
           05 IN-IMPORTE-MOV      PIC S9(10)V99 COMP-3.
           05 IN-SALDO-ACTUAL     PIC S9(10)V99 COMP-3.

      *=================================================================
      *     AREA DE SALIDA DEL DBIO
      *=================================================================
       01 OUTPUT-AREA.
           05 COD-ERROR           PIC 9(02).
           05 MENSAJE-OUTPUT      PIC X(50).
           05 OUT-ID-CLIENTE      PIC 9(08).
           05 OUT-COD-MOV         PIC 9(02).
           05 OUT-FECHA-ULT-MOV  PIC X(08).
           05 OUT-IMPORTE-MOV     PIC S9(10)V99 COMP-3.
           05 OUT-SALDO-ACTUAL    PIC S9(10)V99 COMP-3.

      *=================================================================
      *     VARIABLES DE DISPLAY (FORMATO LEGIBLE PARA PANTALLA)
      *=================================================================
       01 WS-SALDO-DISPLAY        PIC Z(10).99.
       01 WS-IMPORTE-DISPLAY      PIC Z(10).99.

      *=================================================================
      *     COPYBOOKS
      *=================================================================
           COPY INVMREC.

       LINKAGE SECTION.
           COPY LKCIF.

      ******************************************************************
       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.
      ******************************************************************

       0000-PRINCIPAL.
           INITIALIZE INPUT-AREA
                      OUTPUT-AREA
           PERFORM 200-PROCESO
           GOBACK.

      *=================================================================
      *  200-PROCESO: Loop del menu hasta que el usuario elige salir
      *=================================================================
       200-PROCESO.
           PERFORM UNTIL WS-OPTION = 4
               PERFORM DISPLAY-MENU
               PERFORM 100-MENU
           END-PERFORM
           DISPLAY "========================================"
           DISPLAY "    VOLVIENDO AL MENU PRINCIPAL..."
           DISPLAY "========================================".

      *=================================================================
      *  DISPLAY-MENU
      *=================================================================
       DISPLAY-MENU.
           DISPLAY " "
           DISPLAY "========================================"
           DISPLAY "    MODULO: CUENTAS CORRIENTES / DEP."
           DISPLAY "========================================"
           DISPLAY "  1. Registrar Movimiento"
           DISPLAY "  2. Consultar Saldo"
           DISPLAY "  3. Generar Extracto Detallado"
           DISPLAY "  4. Volver al Menu Principal"
           DISPLAY "========================================"
           DISPLAY "  Seleccione opcion (1-4): "
               WITH NO ADVANCING
           ACCEPT WS-OPTION.

      *=================================================================
      *  100-MENU: Dispatcher de opciones
      *=================================================================
       100-MENU.
           EVALUATE WS-OPTION
               WHEN 1
                   PERFORM 1000-REGISTRAR-MOVIMIENTO
               WHEN 2
                   PERFORM 2000-CONSULTAR-SALDO
               WHEN 3
                   PERFORM 3000-GENERAR-EXTRACTO
               WHEN 4
                   DISPLAY " "
                   DISPLAY "  Saliendo del modulo..."
               WHEN OTHER
                   DISPLAY " "
                   DISPLAY "  ** Opcion invalida. Intente de nuevo."
           END-EVALUATE.

      *=================================================================
      *  1000 - REGISTRAR MOVIMIENTO
      *         Valida tipo en loop, acepta importe, llama DBIOINVM
      *=================================================================
       1000-REGISTRAR-MOVIMIENTO.
           DISPLAY " "
           DISPLAY "  --- REGISTRAR MOVIMIENTO ---"
           MOVE 'I'               TO OPER

           DISPLAY "  Ingrese ID Cliente (8 dig): "
               WITH NO ADVANCING
           ACCEPT IN-ID-CLIENTE

      *    Validar tipo de movimiento: loop hasta recibir 2 o 3
           MOVE 0                 TO IN-COD-MOV
           PERFORM UNTIL IN-COD-MOV = 2 OR IN-COD-MOV = 3
               DISPLAY "  Tipo de movimiento:"
               DISPLAY "    2 = Deposito   3 = Retiro"
               DISPLAY "  Ingrese opcion: " WITH NO ADVANCING
               ACCEPT IN-COD-MOV
               IF IN-COD-MOV NOT = 2 AND IN-COD-MOV NOT = 3
                   DISPLAY "  ** Ingrese una opcion valida (2 o 3)."
               END-IF
           END-PERFORM

           DISPLAY "  Ingrese importe: " WITH NO ADVANCING
           ACCEPT IN-IMPORTE-MOV

           CALL 'DBIOINVM' USING INPUT-AREA
                                 OUTPUT-AREA

           DISPLAY " "
           DISPLAY "  ----------------------------------------"
           IF COD-ERROR = 0
               EVALUATE IN-COD-MOV
                   WHEN 2  MOVE "Deposito"  TO WS-TIPO-MOV-TEXTO
                   WHEN 3  MOVE "Retiro"    TO WS-TIPO-MOV-TEXTO
               END-EVALUATE
               MOVE OUT-SALDO-ACTUAL        TO WS-SALDO-DISPLAY
               DISPLAY "  Operacion    : " WS-TIPO-MOV-TEXTO
               DISPLAY "  Nuevo saldo  : $ " WS-SALDO-DISPLAY
           ELSE
               DISPLAY "  ERROR: " MENSAJE-OUTPUT
           END-IF
           DISPLAY "  ----------------------------------------"

           MOVE COD-ERROR         TO LK-COD-RETORNO
           MOVE MENSAJE-OUTPUT    TO LK-MENSAJE.

      *=================================================================
      *  2000 - CONSULTAR SALDO
      *         Muestra saldo actual y datos del ultimo movimiento
      *=================================================================
       2000-CONSULTAR-SALDO.
           DISPLAY " "
           DISPLAY "  --- CONSULTA DE SALDO ---"
           MOVE 'C'               TO OPER

           DISPLAY "  Ingrese ID Cliente (8 dig): "
               WITH NO ADVANCING
           ACCEPT IN-ID-CLIENTE

           CALL 'DBIOINVM' USING INPUT-AREA
                                 OUTPUT-AREA

           DISPLAY " "
           DISPLAY "  ========================================"
           IF COD-ERROR = 0
               MOVE OUT-SALDO-ACTUAL  TO WS-SALDO-DISPLAY
               MOVE OUT-IMPORTE-MOV   TO WS-IMPORTE-DISPLAY
               EVALUATE OUT-COD-MOV
                   WHEN 2  MOVE "Deposito"  TO WS-TIPO-MOV-TEXTO
                   WHEN 3  MOVE "Retiro"    TO WS-TIPO-MOV-TEXTO
                   WHEN OTHER
                           MOVE "Apertura"  TO WS-TIPO-MOV-TEXTO
               END-EVALUATE
               DISPLAY "  ESTADO DE CUENTA"
               DISPLAY "  ========================================"
               DISPLAY "  Cliente ID     : " OUT-ID-CLIENTE
               DISPLAY "  Saldo actual   : $ " WS-SALDO-DISPLAY
               DISPLAY "  Ultimo mov.    : $ " WS-IMPORTE-DISPLAY
               DISPLAY "  Tipo ult. mov. : "   WS-TIPO-MOV-TEXTO
               DISPLAY "  Fecha ult. mov.: "   OUT-FECHA-ULT-MOV
           ELSE
               DISPLAY "  ERROR: " MENSAJE-OUTPUT
           END-IF
           DISPLAY "  ========================================"

           MOVE COD-ERROR         TO LK-COD-RETORNO
           MOVE MENSAJE-OUTPUT    TO LK-MENSAJE.

      *=================================================================
      *  3000 - GENERAR EXTRACTO DETALLADO
      *         Estado completo del registro: saldo + ultimo movimiento
      *=================================================================
       3000-GENERAR-EXTRACTO.
           DISPLAY " "
           DISPLAY "  --- EXTRACTO DETALLADO ---"
           MOVE 'D'               TO OPER

           DISPLAY "  Ingrese ID Cliente (8 dig): "
               WITH NO ADVANCING
           ACCEPT IN-ID-CLIENTE

           CALL 'DBIOINVM' USING INPUT-AREA
                                 OUTPUT-AREA

           DISPLAY " "
           IF COD-ERROR = 0
               MOVE OUT-SALDO-ACTUAL  TO WS-SALDO-DISPLAY
               MOVE OUT-IMPORTE-MOV   TO WS-IMPORTE-DISPLAY
               EVALUATE OUT-COD-MOV
                   WHEN 2  MOVE "DEPOSITO"  TO WS-TIPO-MOV-TEXTO
                   WHEN 3  MOVE "RETIRO"    TO WS-TIPO-MOV-TEXTO
                   WHEN OTHER
                           MOVE "APERTURA"  TO WS-TIPO-MOV-TEXTO
               END-EVALUATE
               DISPLAY "  ========================================"
               DISPLAY "      EXTRACTO DE CUENTA CORRIENTE"
               DISPLAY "  ========================================"
               DISPLAY "  Cliente ID       : " OUT-ID-CLIENTE
               DISPLAY "  ----------------------------------------"
               DISPLAY "  ULTIMO MOVIMIENTO"
               DISPLAY "    Tipo           : " WS-TIPO-MOV-TEXTO
               DISPLAY "    Importe        : $ " WS-IMPORTE-DISPLAY
               DISPLAY "    Fecha          : " OUT-FECHA-ULT-MOV
               DISPLAY "  ----------------------------------------"
               DISPLAY "  SALDO ACTUAL     : $ " WS-SALDO-DISPLAY
               DISPLAY "  ========================================"
           ELSE
               DISPLAY "  ERROR: " MENSAJE-OUTPUT
           END-IF

           MOVE COD-ERROR         TO LK-COD-RETORNO
           MOVE MENSAJE-OUTPUT    TO LK-MENSAJE.

       END PROGRAM IN0000.

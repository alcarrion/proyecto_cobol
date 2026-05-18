       IDENTIFICATION DIVISION.
       PROGRAM-ID. DF0000.
       AUTHOR. TCS ECUADOR.
       DATE-WRITTEN. 2025-03-29.

      *================================================================*
      * MODULO: DF0000 - REGISTRO DE CONSUMOS DIFERIDOS (TARJETA CREDITO)
      * INVOCADO POR: TC0000
      * FUNCION: Calcula cuotas con interes fijo 35% anual, registra
      *          el diferido y actualiza el saldo de la tarjeta.
      * PARAMETROS: REG-TARJ (con datos de la tarjeta ya cargados),
      *             REG-DIFD (para salida),
      *             LK-DATOS-SESION, LK-DATOS-TRANSACCION,
      *             LK-MONTO-COMPRA, LK-CUOTAS, LK-RESULTADO.
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CALCULOS.
           05 WS-TASA-ANUAL          PIC S9(03)V99 COMP-3 VALUE 35.00.
           05 WS-INTERES-UNIDAD      PIC S9(10)V99 COMP-3.
           05 WS-INTERES-TOTAL       PIC S9(10)V99 COMP-3.
           05 WS-MONTO-TOTAL         PIC S9(10)V99 COMP-3.
           05 WS-VALOR-CUOTA         PIC S9(10)V99 COMP-3.

       LINKAGE SECTION.
           COPY TARJREC.
           COPY LKCIF.

       01  LK-MONTO-COMPRA           PIC S9(10)V99 COMP-3.
       01  LK-CUOTAS                 PIC 9(03).
       01  LK-RESULTADO              PIC X(01).
           88 LK-OP-OK               VALUE 'S'.
           88 LK-OP-FALLO            VALUE 'N'.

       PROCEDURE DIVISION USING REG-TARJ,
                                REG-DIFD,
                                LK-DATOS-SESION,
                                LK-DATOS-TRANSACCION,
                                LK-MONTO-COMPRA,
                                LK-CUOTAS,
                                LK-RESULTADO.

       0000-PRINCIPAL.
           MOVE 'N' TO LK-RESULTADO.
           MOVE ZEROS TO WS-INTERES-TOTAL WS-MONTO-TOTAL.

      *> Validacion de tarjeta activa
           IF NOT TARJETA-ACTIVA
               MOVE 'E999' TO LK-COD-RETORNO
               MOVE 'Tarjeta no activa' TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF.

      *> Validacion de monto
           IF LK-MONTO-COMPRA <= ZEROS
               MOVE 'E001' TO LK-COD-RETORNO
               MOVE 'Monto debe ser mayor a cero' TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF.

      *> Validacion de cuotas
           IF LK-CUOTAS < 1 OR LK-CUOTAS > 36
               MOVE 'E001' TO LK-COD-RETORNO
               MOVE 'Numero de cuotas invalido (1-36)' TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF.

      *> Inicializar registro de diferido
           INITIALIZE REG-DIFD.
           MOVE TARJ-NRO-TARJETA   TO DIFD-NRO-TARJETA.
           MOVE LK-MONTO-COMPRA    TO DIFD-MONTO-COMPRA.
           MOVE LK-CUOTAS          TO DIFD-CUOTAS-TOTALES.
           MOVE LK-CUOTAS          TO DIFD-CUOTAS-PENDIENTES.

      *> Calcular valor cuota segun numero de cuotas
           IF LK-CUOTAS = 1
               MOVE LK-MONTO-COMPRA TO DIFD-VALOR-CUOTA
               MOVE ZEROS           TO DIFD-TASA-INTERES
           ELSE
      *>     Interes por unidad = monto * tasa_anual / 1200
               COMPUTE WS-INTERES-UNIDAD =
                   LK-MONTO-COMPRA * WS-TASA-ANUAL / 1200
      *>     Interes total = interes_unidad * cuotas
               COMPUTE WS-INTERES-TOTAL =
                   WS-INTERES-UNIDAD * LK-CUOTAS
               COMPUTE WS-MONTO-TOTAL =
                   LK-MONTO-COMPRA + WS-INTERES-TOTAL
               COMPUTE DIFD-VALOR-CUOTA ROUNDED =
                   WS-MONTO-TOTAL / LK-CUOTAS
               MOVE WS-TASA-ANUAL TO DIFD-TASA-INTERES
           END-IF.

      *> Llamar a DBIO para insertar el diferido y actualizar saldo tarjeta
           MOVE 'I' TO LK-ACCION-DB.
           CALL 'DBIOTARJ' USING REG-TARJ,
                                 REG-DIFD,
                                 LK-DATOS-SESION,
                                 LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO LK-ACCION-DB
               CALL 'DBIOTRAN' USING LK-ACCION-DB
               MOVE 'S' TO LK-RESULTADO
               MOVE '00  ' TO LK-COD-RETORNO
               MOVE 'Consumo diferido registrado' TO LK-MENSAJE
           ELSE
               MOVE 'R' TO LK-ACCION-DB
               CALL 'DBIOTRAN' USING LK-ACCION-DB
               MOVE 'N' TO LK-RESULTADO
      *>       LK-COD-RETORNO y LK-MENSAJE ya fueron puestos por DBIOTARJ
           END-IF.

           GOBACK.
       END PROGRAM DF0000.

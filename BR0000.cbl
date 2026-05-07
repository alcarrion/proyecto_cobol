       IDENTIFICATION DIVISION.
       PROGRAM-ID. BR0000.
      *================================================================*
      * MODULO        : PRESTAMOS / HIPOTECAS (MAINLINE)               *
      * DESCRIPCION   : Módulo de reglas de negocio para la gestión de *
      *                 hipotecas. Interactúa con el usuario y delega  *
      *                 la persistencia a DBIOBORM.PCO.                *
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PGM-ID                     PIC X(06) VALUE 'BR0000'.
       01  WS-OPTION                  PIC 9(01) VALUE ZERO.

       *> Estructuras de Datos
       COPY BORMREC.
       COPY CUSMREC.

       *> Variables de Trabajo para Interfaz y Cálculos
       01  WS-INTERFAZ.
           05 WS-ENTRADA-TXT          PIC X(15).
           05 WS-MONTO-PAGO           PIC S9(13)V99.
           05 WS-PLAZO-MESES          PIC 9(03).

       01  WS-FECHA-SISTEMA.
           05 WS-ANIO                 PIC 9(04).
           05 WS-MES                  PIC 9(02).
           05 WS-DIA                  PIC 9(02).

       01  WS-AUX-ID.
           05 WS-PREFIX-HIP           PIC 9(01) VALUE 9.
           05 WS-YY-ACTUAL            PIC 9(02).
           05 WS-CORRELATIVO          PIC 9(06).

       01  WS-CALCULOS-FECHA.
           05 WS-ANIO-ADIC            PIC 9(02).
           05 WS-MES-REMAN            PIC 9(02).
           05 WS-CALC-ANIO            PIC 9(04).
           05 WS-CALC-MES             PIC 9(02).

       01  WS-FECHA-EDITADA.
           05 WS-ED-ANIO              PIC 9(04).
           05 FILLER                  PIC X VALUE "-".
           05 WS-ED-MES               PIC 9(02).
           05 FILLER                  PIC X VALUE "-".
           05 WS-ED-DIA               PIC 9(02).

       LINKAGE SECTION.
           COPY LKCIF.

       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.
       0000-MAIN.
           PERFORM 1000-INICIALIZACION.
           PERFORM 2000-MOSTRAR-MENU UNTIL WS-OPTION = 4.
           PERFORM 3000-FINALIZACION.
           GOBACK.

       1000-INICIALIZACION.
           INITIALIZE BORM-REGISTRO.
           MOVE ZERO TO WS-OPTION.
           DISPLAY '========================================'.
           DISPLAY 'INICIANDO MODULO DE HIPOTECAS '.
           DISPLAY '========================================'.

       2000-MOSTRAR-MENU.
           DISPLAY ' '.
           DISPLAY '========================================'.
           DISPLAY '            MENU HIPOTECAS              '.
           DISPLAY '========================================'.
           DISPLAY '1. REGISTRAR NUEVO PRESTAMO (ALTA)'.
           DISPLAY '2. CONSULTAR ESTADO DE HIPOTECA'.
           DISPLAY '3. PROCESAR PAGO DE CUOTA'.
           DISPLAY '4. SALIR AL MENU PRINCIPAL'.
           DISPLAY '========================================'.
           DISPLAY 'SELECCIONE OPCION: ' WITH NO ADVANCING.
           ACCEPT WS-OPTION.

           EVALUATE WS-OPTION
               WHEN 1 PERFORM 2100-REGISTRAR-ALTA
               WHEN 2 PERFORM 2200-CONSULTAR-SALDO
               WHEN 3 PERFORM 2300-PROCESAR-PAGO
           END-EVALUATE.

       2100-REGISTRAR-ALTA.
           DISPLAY '--- ALTA DE PRESTAMO HIPOTECARIO ---'.

           *> 1. VALIDAR CLIENTE
           DISPLAY 'INGRESE CEDULA DEL CLIENTE: '
           ACCEPT CUSM-DOC-CLIENTE.
           MOVE 'C' TO LK-ACCION-DB.
           CALL 'DBIOCUSM' USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO NOT = 0
               DISPLAY '*** ERROR: EL CLIENTE NO ESTA REGISTRADO ***'
           ELSE
               DISPLAY 'CLIENTE: ' CUSM-NOMBRE ' 'CUSM-APELLIDOS
               MOVE CUSM-ID-CLIENTE TO BORM-ID-CLIENTE

               *> 2. GENERAR ID DINAMICO Y FECHA INICIO
               MOVE 'S' TO LK-ACCION-DB
               CALL 'DBIOBORM' USING LK-DATOS-TRANSACCION, BORM-REGISTRO

               IF LK-COD-RETORNO = 0
                   PERFORM 9100-ARMAR-ID-DINAMICO
                   DISPLAY 'ID ASIGNADO: ' BORM-ID-HIPOTECA

                   *> 3. CAPTURA DE DATOS FINANCIEROS
                   DISPLAY 'MONTO PRESTAMO: ' ACCEPT WS-ENTRADA-TXT
                   MOVE FUNCTION NUMVAL(WS-ENTRADA-TXT)
                   TO BORM-MONTO-ORIGINAL
                   MOVE BORM-MONTO-ORIGINAL TO BORM-SALDO-ACTUAL

                   DISPLAY 'TASA INTERES ANUAL (EJ: 10.5): '
                   ACCEPT WS-ENTRADA-TXT
                   MOVE FUNCTION NUMVAL(WS-ENTRADA-TXT)
                   TO BORM-TASA-INTERES

                   DISPLAY 'PLAZO EN MESES: ' ACCEPT WS-PLAZO-MESES
                   PERFORM 9300-CALCULAR-VENCIMIENTO

                   DISPLAY 'DIA DE PAGO (1-31): ' ACCEPT BORM-DIA-PAGO
                   MOVE 'ACTIVO' TO BORM-ESTADO

                   *> 4. GUARDAR EN BD
                   MOVE 'A' TO LK-ACCION-DB
                   PERFORM 2900-LLAMAR-DBIO

                   IF LK-COD-RETORNO = 0
                       DISPLAY '*** PRESTAMO CREADO EXITOSAMENTE ***'
                   END-IF
               END-IF
           END-IF.

       2300-PROCESAR-PAGO.
           DISPLAY '--- PROCESAR PAGO DE CUOTA ---'.
           DISPLAY 'ID HIPOTECA: ' ACCEPT BORM-ID-HIPOTECA.

           MOVE 'C' TO LK-ACCION-DB
           PERFORM 2900-LLAMAR-DBIO

           IF LK-COD-RETORNO = 0
               DISPLAY 'SALDO ACTUAL: ' BORM-SALDO-ACTUAL
               DISPLAY 'MONTO A PAGAR: ' ACCEPT WS-ENTRADA-TXT
               MOVE FUNCTION NUMVAL(WS-ENTRADA-TXT) TO WS-MONTO-PAGO

               IF WS-MONTO-PAGO > BORM-SALDO-ACTUAL
                   DISPLAY 'ERROR: EL PAGO EXCEDE LA DEUDA.'
               ELSE
                   *> CALCULO AUTOMATICO DE SALDO
                   SUBTRACT WS-MONTO-PAGO FROM BORM-SALDO-ACTUAL

                   IF BORM-SALDO-ACTUAL = 0
                       MOVE 'CANCELADO' TO BORM-ESTADO
                   END-IF

                   MOVE 'M' TO LK-ACCION-DB
                   PERFORM 2900-LLAMAR-DBIO

                   IF LK-COD-RETORNO = 0
                       CALL 'DBIOTRAN' USING 'C'
                       DISPLAY '*** PAGO EXITOSO. NUEVO SALDO: '
                               BORM-SALDO-ACTUAL
                   END-IF
               END-IF
           ELSE
               DISPLAY '*** ERROR: HIPOTECA NO ENCONTRADA ***'
           END-IF.

       9100-ARMAR-ID-DINAMICO.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-FECHA-SISTEMA
           MOVE WS-ANIO TO WS-ED-ANIO
           MOVE WS-MES  TO WS-ED-MES
           MOVE WS-DIA  TO WS-ED-DIA
           MOVE WS-FECHA-EDITADA TO BORM-FECHA-INICIO

           MOVE WS-ANIO(3:2) TO WS-YY-ACTUAL
           MOVE BORM-ID-HIPOTECA TO WS-CORRELATIVO
           STRING WS-PREFIX-HIP WS-YY-ACTUAL WS-CORRELATIVO
                  DELIMITED BY SIZE INTO BORM-ID-HIPOTECA.

       9300-CALCULAR-VENCIMIENTO.
           MOVE WS-ANIO TO WS-CALC-ANIO
           MOVE WS-MES  TO WS-CALC-MES
           ADD WS-PLAZO-MESES TO WS-CALC-MES
           DIVIDE WS-CALC-MES BY 12 GIVING WS-ANIO-ADIC
                                 REMAINDER WS-MES-REMAN
           IF WS-MES-REMAN = 0
               SUBTRACT 1 FROM WS-ANIO-ADIC
               MOVE 12 TO WS-CALC-MES
           ELSE
               MOVE WS-MES-REMAN TO WS-CALC-MES
           END-IF
           ADD WS-ANIO-ADIC TO WS-CALC-ANIO
           MOVE WS-CALC-ANIO TO WS-ED-ANIO
           MOVE WS-CALC-MES  TO WS-ED-MES
           MOVE WS-DIA       TO WS-ED-DIA
           MOVE WS-FECHA-EDITADA TO BORM-FECHA-VENCTO.

       2900-LLAMAR-DBIO.
           CALL 'DBIOBORM' USING LK-DATOS-TRANSACCION BORM-REGISTRO
           IF LK-COD-RETORNO NOT = 0
               DISPLAY 'ERROR DB: ' LK-MENSAJE
           END-IF.

       2200-CONSULTAR-SALDO.
           DISPLAY '--- CONSULTA DE HIPOTECA ---'
           DISPLAY 'ID HIPOTECA: ' ACCEPT BORM-ID-HIPOTECA
           MOVE 'C' TO LK-ACCION-DB
           PERFORM 2900-LLAMAR-DBIO
           IF LK-COD-RETORNO = 0
               DISPLAY 'ID CLIENTE: ' BORM-ID-CLIENTE
               DISPLAY 'SALDO     : ' BORM-SALDO-ACTUAL
               DISPLAY 'VENCE     : ' BORM-FECHA-VENCTO
               DISPLAY 'ESTADO    : ' BORM-ESTADO
           END-IF.

       3000-FINALIZACION.
           DISPLAY 'CERRANDO MODULO HIPOTECARIO.'.

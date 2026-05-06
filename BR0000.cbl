       IDENTIFICATION DIVISION.
       PROGRAM-ID. BR0000.
      *================================================================*
      * MODULO        : PRESTAMOS / HIPOTECAS (MAINLINE)               *
      * DESCRIPCION   : Módulo de reglas de negocio para la gestión de *
      *                 hipotecas. Interactúa con el usuario y delega  *
      *                 la persistencia a DBIOBORM.PCO.                *
      *================================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PGM-ID                     PIC X(06) VALUE 'BR0000'.
       01  WS-OPTION                  PIC 9(01) VALUE ZERO.
       01  WS-CONTINUAR               PIC X(01) VALUE 'S'.

       COPY BORMREC.

       01  WS-MONTO-ORIGINAL-TMP      PIC 9(13)V99.
       01  WS-TASA-INTERES-TMP        PIC 9(03)V9999.
       01  WS-SALDO-ACTUAL-TMP        PIC 9(13)V99.
       01  WS-DIA-PAGO-TMP            PIC 9(02).

       LINKAGE SECTION.
           COPY LKCIF.


       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.
       0000-MAIN-LOGIC.
           PERFORM 1000-INICIALIZACION.

           PERFORM 2000-MOSTRAR-MENU UNTIL WS-OPTION = 4.

           PERFORM 3000-FINALIZACION.
           GOBACK.

      *----------------------------------------------------------------*
      * 1000-INICIALIZACION: Limpieza de variables                     *
      *----------------------------------------------------------------*
       1000-INICIALIZACION.
           INITIALIZE BORM-REGISTRO

           MOVE ZERO TO WS-OPTION
           DISPLAY '========================================'
           DISPLAY 'INICIANDO MODULO DE HIPOTECAS (' PGM-ID ')'
           DISPLAY '========================================'.

      *----------------------------------------------------------------*
      * 2000-MOSTRAR-MENU: Despliegue de opciones                      *
      *----------------------------------------------------------------*
       2000-MOSTRAR-MENU.
           DISPLAY ' '.
           DISPLAY '========================================'.
           DISPLAY '            MENU HIPOTECAS              '.
           DISPLAY '========================================'.
           DISPLAY '1. REGISTRAR NUEVO PRESTAMO (ALTA)'.
           DISPLAY '2. CONSULTAR SALDO ADEUDADO   (CONSULTA)'.
           DISPLAY '3. ACTUALIZAR PAGO MENSUAL    (MODIFICACION)'.
           DISPLAY '4. SALIR AL MENU PRINCIPAL'.
           DISPLAY '========================================'.
           DISPLAY 'SELECCIONE UNA OPCION (1-4): ' WITH NO ADVANCING.
           ACCEPT WS-OPTION.

           EVALUATE WS-OPTION
               WHEN 1
                   PERFORM 2100-REGISTRAR-ALTA
               WHEN 2
                   PERFORM 2200-CONSULTAR-SALDO
               WHEN 3
                   PERFORM 2300-ACTUALIZAR-PAGO
               WHEN 4
                   CONTINUE
               WHEN OTHER
                   DISPLAY '*** ERROR: OPCION INVALIDA ***'
           END-EVALUATE.

      *----------------------------------------------------------------*
      * 2100-REGISTRAR-ALTA: Captura datos para un nuevo préstamo      *
      *----------------------------------------------------------------*
       2100-REGISTRAR-ALTA.
           DISPLAY '--- REGISTRO DE NUEVO PRESTAMO HIPOTECARIO ---'
           INITIALIZE BORM-REGISTRO

           DISPLAY 'INGRESE ID HIPOTECA (9 DIGITOS): '

           ACCEPT BORM-ID-HIPOTECA

           DISPLAY 'INGRESE ID CLIENTE  (8 DIGITOS): '

           ACCEPT BORM-ID-CLIENTE

           DISPLAY 'FECHA INICIO (YYYY-MM-DD)      : '

           ACCEPT BORM-FECHA-INICIO

           DISPLAY 'MONTO ORIGINAL (SIN DECIMALES, SE ASUMEN .00): '

           ACCEPT WS-MONTO-ORIGINAL-TMP
           MOVE WS-MONTO-ORIGINAL-TMP TO BORM-MONTO-ORIGINAL
      * Inicialmente, el saldo actual es igual al monto original
           MOVE WS-MONTO-ORIGINAL-TMP TO BORM-SALDO-ACTUAL

           DISPLAY 'TASA DE INTERES (EJ. 0125000 = 12.5%): '

           ACCEPT WS-TASA-INTERES-TMP
           MOVE WS-TASA-INTERES-TMP TO BORM-TASA-INTERES

           DISPLAY 'FECHA VENCIMIENTO (YYYY-MM-DD) : '

           ACCEPT BORM-FECHA-VENCTO

           DISPLAY 'DIA DE PAGO MENSUAL (1-31)     : '

           ACCEPT WS-DIA-PAGO-TMP
           MOVE WS-DIA-PAGO-TMP TO BORM-DIA-PAGO

           MOVE 'ACTIVO' TO BORM-ESTADO

           SET LK-ALTA TO TRUE
           PERFORM 2900-LLAMAR-DBIO

           IF LK-COD-RETORNO = 0
               DISPLAY '*** PRESTAMO REGISTRADO CON EXITO ***'
           END-IF.

      *----------------------------------------------------------------*
      * 2200-CONSULTAR-SALDO: Recupera datos de una hipoteca           *
      *----------------------------------------------------------------*
       2200-CONSULTAR-SALDO.
           DISPLAY ' '.
           DISPLAY '--- CONSULTA DE SALDO ADEUDADO ---'.
           INITIALIZE BORM-REGISTRO.

           DISPLAY 'INGRESE ID HIPOTECA A CONSULTAR: '

           ACCEPT BORM-ID-HIPOTECA.

           SET LK-CONSULTA TO TRUE.
           PERFORM 2900-LLAMAR-DBIO.

           IF LK-COD-RETORNO = 0
               DISPLAY 'DATOS RECUPERADOS EXITOSAMENTE:'
               DISPLAY '- CLIENTE ID: ' BORM-ID-CLIENTE
               DISPLAY '- ESTADO    : ' BORM-ESTADO
               *> Movemos a campos DISPLAY para mostrar en pantalla
               MOVE BORM-MONTO-ORIGINAL TO WS-MONTO-ORIGINAL-TMP
               DISPLAY '- MONTO ORIG: ' WS-MONTO-ORIGINAL-TMP
               MOVE BORM-SALDO-ACTUAL TO WS-SALDO-ACTUAL-TMP
               DISPLAY '- SALDO ACT : ' WS-SALDO-ACTUAL-TMP
           END-IF.

      *----------------------------------------------------------------*
      * 2300-ACTUALIZAR-PAGO: Actualiza saldo y estado de la hipoteca  *
      *----------------------------------------------------------------*
       2300-ACTUALIZAR-PAGO.
           DISPLAY ' '
           DISPLAY '--- ACTUALIZACION DE PAGOS MENSUALES ---'
           INITIALIZE BORM-REGISTRO

           DISPLAY 'INGRESE ID HIPOTECA A ACTUALIZAR: '

           ACCEPT BORM-ID-HIPOTECA

      * Primero, consultamos para asegurarnos de que la hipoteca existe
           SET LK-CONSULTA TO TRUE
           PERFORM 2900-LLAMAR-DBIO

           IF LK-COD-RETORNO = 0
               DISPLAY 'SALDO ACTUAL: ' BORM-SALDO-ACTUAL

               DISPLAY 'NUEVO SALDO TRAS EL PAGO: ' WITH NO ADVANCING
               ACCEPT WS-SALDO-ACTUAL-TMP
               MOVE WS-SALDO-ACTUAL-TMP TO BORM-SALDO-ACTUAL

               DISPLAY 'NUEVO ESTADO (EJ. AL DIA, MORA, PAGADO): '
                       WITH NO ADVANCING
               ACCEPT BORM-ESTADO

               SET LK-CONSULTA TO TRUE
               PERFORM 2900-LLAMAR-DBIO

               IF LK-COD-RETORNO = 0
                   DISPLAY '*** ACTUALIZACION REALIZADA CON EXITO ***'
               END-IF
           ELSE
               DISPLAY '*** ERROR: HIPOTECA NO ENCONTRADA. IMPOSIBLE '
                       'ACTUALIZAR. ***'
           END-IF.

      *----------------------------------------------------------------*
      * 2900-LLAMAR-DBIO: Ejecuta el CALL al módulo de acceso a datos  *
      *----------------------------------------------------------------*
       2900-LLAMAR-DBIO.
           CALL 'DBIOBORM' USING LK-DATOS-TRANSACCION BORM-REGISTRO

           *> Manejo básico de errores SQL
           IF LK-COD-RETORNO NOT = 0
               DISPLAY ' '
               DISPLAY '*** ERROR FATAL EN BASE DE DATOS ***'

               DISPLAY 'SQLCODE  : ' LK-COD-RETORNO
               IF LK-ACCION-DB = 'A' AND LK-COD-RETORNO = 99
                   DISPLAY 'DETALLE  : ERROR DE INTEGRIDAD. EL CLIENTE'
                           'NO EXISTE EN LA TABLA CLIENTES.'
               END-IF
           END-IF.

      *----------------------------------------------------------------*
      * 3000-FINALIZACION: Tareas de cierre del programa               *
      *----------------------------------------------------------------*
       3000-FINALIZACION.
           DISPLAY ' '.
           DISPLAY '========================================'.
           DISPLAY 'CERRANDO MODULO DE HIPOTECAS (' PGM-ID ')'.
           DISPLAY '========================================'.

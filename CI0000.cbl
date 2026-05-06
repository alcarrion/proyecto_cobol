       IDENTIFICATION DIVISION.
       PROGRAM-ID. CI0000.
      *================================================================*
      * PROGRAMA: MAINLINE CIF (CUSTOMER INFORMATION FACILITY)         *
      * FUNCION:  Gestiona Altas, Bajas, Modificaciones y Consultas.   *
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPCION-CIF       PIC 9(01).
       01  WS-CONTINUAR-CIF    PIC X(01) VALUE 'S'.
      * 01 WS-TIENE-CUENTA     PIC X(01) VALUE 'N'.

       01  WS-PROGRAMAS.
           05 WS-PGM-DBIOCUSM    PIC X(8) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOINVM    PIC X(8) VALUE 'DBIOINVM'.
       01  WS-FECHA-SISTEMA.
           05 WS-ANIO         PIC 9(04).
           05 WS-MES          PIC 9(02).
           05 WS-DIA          PIC 9(02).
       01  WS-FECHA-ISO        PIC X(10).
      * COPY de la tabla maestra de Clientes (CUSM)
           COPY CUSMREC.
      * COPY de la tabla maestra de Cuentas (INVM)
           COPY INVMREC.
      * COPY de variables de Linkage movido a Linkage Section

       LINKAGE SECTION.
           COPY LKCIF.


       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           PERFORM 1000-PROCESAR-OPCIONES
                   UNTIL WS-CONTINUAR-CIF = 'N' OR 'n'.
           GOBACK.

       1000-PROCESAR-OPCIONES.
           DISPLAY "========================================".
           DISPLAY "       MODULO ABM CLIENTES (CIF)        ".
           DISPLAY "========================================".
           DISPLAY "1. Alta de Cliente".
           DISPLAY "2. Consulta de Cliente".
           DISPLAY "3. Modificacion de Cliente".
           DISPLAY "4. Baja (Logica) de Cliente".
           DISPLAY "0. Volver al Menu Principal".
           DISPLAY "========================================".
           DISPLAY "Seleccione operacion: "
           ACCEPT WS-OPCION-CIF.

           EVALUATE WS-OPCION-CIF
               WHEN 1
                   PERFORM 2000-ALTA-CLIENTE
               WHEN 2
                   PERFORM 3000-CONSULTA-CLIENTE
               WHEN 3
                   PERFORM 4000-MODIFICA-CLIENTE
               WHEN 4
                   PERFORM 5000-BAJA-CLIENTE
               WHEN 0
                   MOVE 'N' TO WS-CONTINUAR-CIF
               WHEN OTHER
                   DISPLAY "Opcion invalida."
           END-EVALUATE.

       2000-ALTA-CLIENTE.
           DISPLAY "--- ALTA DE CLIENTE ---".
           INITIALIZE REG-CUSM.

           DISPLAY "Tipo de Documento (CED/PAS): "
           ACCEPT CUSM-TIPO-DOC.
           DISPLAY "Numero de Documento        : "
           ACCEPT CUSM-DOC-CLIENTE.
      *    Validacion de cuenta existente
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               DISPLAY "Alert: EL DOCUMENTO YA EXISTE EN EL SISTEMA."
           ELSE
               DISPLAY "Nombre(s)    : " ACCEPT CUSM-NOMBRE
               DISPLAY "Apellido(s)  : " ACCEPT CUSM-APELLIDOS
               DISPLAY "Direccion    : " ACCEPT CUSM-DIRECCION
               DISPLAY "Telefono     : " ACCEPT CUSM-TELEFONO
               DISPLAY "E-mail       : " ACCEPT CUSM-EMAIL

               MOVE 'A' TO LK-ACCION-DB
               CALL 'DBIOCUSM' USING REG-CUSM, LK-DATOS-TRANSACCION


           MOVE 1 TO CUSM-CTA-ACTIVA
               MOVE 0 TO CUSM-SALDO-CLIENTE
               ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD
               STRING WS-ANIO "-" WS-MES "-" WS-DIA INTO
               CUSM-FECHA-ALTA
               MOVE '9999-12-31' TO CUSM-FECHA-CIERRE

      *    PASO 1: INSERTAR CLIENTE (Capa de Acceso a Datos)
               MOVE 'A' TO LK-ACCION-DB
               CALL 'DBIOCUSM' USING REG-CUSM, LK-DATOS-TRANSACCION

               IF LK-COD-RETORNO = 0
      *            5. PASO 2: CREAR CUENTA CORRIENTE (Usa el ID generado)
                   INITIALIZE REG-INVM
                   MOVE CUSM-ID-CLIENTE TO INVM-ID-CLIENTE
                   MOVE 0 TO INVM-SALDO-ACTUAL
                   MOVE CUSM-FECHA-ALTA TO INVM-FECHA-ULT-MOV

                   MOVE 'A' TO LK-ACCION-DB
                   CALL 'DBIOINVM' USING REG-INVM, LK-DATOS-TRANSACCION

                   IF LK-COD-RETORNO = 0
      *                EXITO TOTAL: LLAMAMOS AL COMMIT
                   CALL 'DBIOTRAN' USING 'C'
                   DISPLAY "ALTA EXITOSA. ID ASIGNADO: " CUSM-ID-CLIENTE
                   ELSE
      *                FALLO LA CUENTA: ROLLBACK
                       CALL 'DBIOTRAN' USING 'R'
                   DISPLAY "ERROR EN CUENTA CORRIENTE. NADA SE GUARDO."
                   END-IF
               ELSE
      *            FALLO EL CLIENTE: ROLLBACK
                   CALL 'DBIOTRAN' USING 'R'
                   DISPLAY "FALLO EN ALTA DE CLIENTE: " LK-MENSAJE
               END-IF
           END-IF.

       3000-CONSULTA-CLIENTE.
           DISPLAY "--- CONSULTA DE CLIENTE ---".
           INITIALIZE REG-CUSM.

           DISPLAY "Ingrese Documento: " ACCEPT CUSM-DOC-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               DISPLAY "-----------------------------------"
               DISPLAY "DOCUMENTO :" CUSM-DOC-CLIENTE
               DISPLAY "CLIENTE   : " CUSM-NOMBRE " " CUSM-APELLIDOS
               DISPLAY "SALDO CT  : " CUSM-SALDO-CLIENTE
               DISPLAY "Direccion : " CUSM-DIRECCION
               DISPLAY "Telefono  : " CUSM-TELEFONO
               DISPLAY "Cuenta Activa: " CUSM-CTA-ACTIVA
               DISPLAY "-----------------------------------"
           ELSE
               DISPLAY LK-MENSAJE
           END-IF.

       4000-MODIFICA-CLIENTE.
           DISPLAY "--- MODIFICACION DE CLIENTE ---".
      *    Primero consultamos para verificar que existe
           DISPLAY "Ingrese Documento del Cliente: "
           ACCEPT CUSM-DOC-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               DISPLAY "Dato Actual - Direccion: " CUSM-DIRECCION
               DISPLAY "Nueva Direccion: "        ACCEPT CUSM-DIRECCION
               DISPLAY "Dato Actual - Telefono : " CUSM-TELEFONO
               DISPLAY "Nuevo Telefono : "        ACCEPT CUSM-TELEFONO

               MOVE 'M' TO LK-ACCION-DB
               CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION
               DISPLAY "ACTUALIZACION COMPLETADA."
           ELSE
               DISPLAY "CLIENTE NO EXISTE."
           END-IF.

       5000-BAJA-CLIENTE.
           DISPLAY "--- BAJA LOGICA DE CLIENTE ---".
           DISPLAY "Ingrese Documento: " ACCEPT CUSM-DOC-CLIENTE.

           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.
           DISPLAY LK-MENSAJE.

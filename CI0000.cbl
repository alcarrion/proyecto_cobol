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
       01  WS-CONFIRMAR        PIC X(01).

       01  WS-PROGRAMAS.
           05 WS-PGM-DBIOCUSM    PIC X(8) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOINVM    PIC X(8) VALUE 'DBIOINVM'.
       01  WS-FECHA-SISTEMA.
           05 WS-ANIO         PIC 9(04).
           05 WS-MES          PIC 9(02).
           05 WS-DIA          PIC 9(02).
       01  WS-FECHA-ISO        PIC X(10).
           COPY CUSMREC.
           COPY INVMREC.

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
                   DISPLAY "Opcion invalida. Intente de nuevo."
           END-EVALUATE.

       2000-ALTA-CLIENTE.
           DISPLAY "--- ALTA DE CLIENTE ---".
           INITIALIZE REG-CUSM.

      *    Validar tipo de documento
           PERFORM 2100-PEDIR-TIPO-DOC.

           DISPLAY "Cedula/Pasaporte: "
           ACCEPT CUSM-DOC-CLIENTE.

      *    Validar cedula no vacia
           IF CUSM-DOC-CLIENTE = SPACES
               DISPLAY "ERROR: La cedula no puede estar vacia."
               GO TO 2000-ALTA-CLIENTE
           END-IF.

      *    Verificar si ya existe
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               DISPLAY "ERROR: ESA CEDULA YA EXISTE EN EL SISTEMA."
           ELSE
               DISPLAY "Nombre(s)    : "
               ACCEPT CUSM-NOMBRE
               IF CUSM-NOMBRE = SPACES
                   DISPLAY "ERROR: El nombre no puede estar vacio."
                   GO TO 2000-ALTA-CLIENTE
               END-IF

               DISPLAY "Apellido(s)  : "
               ACCEPT CUSM-APELLIDOS
               IF CUSM-APELLIDOS = SPACES
                   DISPLAY "ERROR: El apellido no puede estar vacio."
                   GO TO 2000-ALTA-CLIENTE
               END-IF

               DISPLAY "Direccion    : "
               ACCEPT CUSM-DIRECCION
               DISPLAY "Telefono     : "
               ACCEPT CUSM-TELEFONO
               DISPLAY "E-mail       : "
               ACCEPT CUSM-EMAIL

      *        Setear campos automaticos
               MOVE 1 TO CUSM-CTA-ACTIVA
               MOVE 0 TO CUSM-SALDO-CLIENTE
               MOVE 0 TO CUSM-TARJETA
               MOVE 0 TO CUSM-CREDITO
               MOVE 0 TO CUSM-HIPOTECA
               ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD
               STRING WS-ANIO "-" WS-MES "-" WS-DIA
                   DELIMITED BY SIZE INTO CUSM-FECHA-ALTA
               MOVE '9999-12-31' TO CUSM-FECHA-CIERRE

      *        PASO 1: INSERTAR CLIENTE
               MOVE 'A' TO LK-ACCION-DB
               CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                   LK-DATOS-TRANSACCION

               IF LK-COD-RETORNO = 0
      *            PASO 2: CREAR CUENTA CORRIENTE
                   INITIALIZE REG-INVM
                   MOVE CUSM-ID-CLIENTE TO INVM-ID-CLIENTE
                   MOVE 0 TO INVM-SALDO-ACTUAL
                   MOVE CUSM-FECHA-ALTA TO INVM-FECHA-ULT-MOV

                   MOVE 'A' TO LK-ACCION-DB
                   CALL WS-PGM-DBIOINVM USING REG-INVM,
                       LK-DATOS-TRANSACCION

                   IF LK-COD-RETORNO = 0
                       CALL 'DBIOTRAN' USING 'C'
                       DISPLAY "ALTA EXITOSA. ID: " CUSM-ID-CLIENTE
                   ELSE
                       CALL 'DBIOTRAN' USING 'R'
                       DISPLAY "ERROR EN CUENTA. NADA SE GUARDO."
                   END-IF
               ELSE
                   CALL 'DBIOTRAN' USING 'R'
                   DISPLAY "FALLO ALTA CLIENTE: " LK-MENSAJE
               END-IF
           END-IF.

       2100-PEDIR-TIPO-DOC.
           DISPLAY "Tipo de Documento (CED/PAS): "
           ACCEPT CUSM-TIPO-DOC.
           IF CUSM-TIPO-DOC = 'ced'
               MOVE 'CED' TO CUSM-TIPO-DOC
           END-IF.
           IF CUSM-TIPO-DOC = 'pas'
               MOVE 'PAS' TO CUSM-TIPO-DOC
           END-IF.
           IF CUSM-TIPO-DOC NOT = 'CED' AND
              CUSM-TIPO-DOC NOT = 'PAS'
               DISPLAY "ERROR: Solo se acepta CED o PAS."
               PERFORM 2100-PEDIR-TIPO-DOC
           END-IF.

       3000-CONSULTA-CLIENTE.
           DISPLAY "--- CONSULTA DE CLIENTE ---".
           INITIALIZE REG-CUSM.

           DISPLAY "Ingrese Cedula: "
           ACCEPT CUSM-DOC-CLIENTE.

           IF CUSM-DOC-CLIENTE = SPACES
               DISPLAY "ERROR: La cedula no puede estar vacia."
               GO TO 3000-CONSULTA-CLIENTE
           END-IF.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               DISPLAY "-----------------------------------"
               DISPLAY "CEDULA    : " CUSM-DOC-CLIENTE
               DISPLAY "CLIENTE   : " CUSM-NOMBRE " " CUSM-APELLIDOS
               DISPLAY "DIRECCION : " CUSM-DIRECCION
               DISPLAY "TELEFONO  : " CUSM-TELEFONO
               DISPLAY "EMAIL     : " CUSM-EMAIL
               DISPLAY "SALDO CT  : " CUSM-SALDO-CLIENTE
               DISPLAY "CTA ACTIVA: " CUSM-CTA-ACTIVA
               DISPLAY "-----------------------------------"
           ELSE
               DISPLAY "CLIENTE NO ENCONTRADO."
           END-IF.

       4000-MODIFICA-CLIENTE.
           DISPLAY "--- MODIFICACION DE CLIENTE ---".
           INITIALIZE REG-CUSM.

           DISPLAY "Ingrese Cedula del Cliente: "
           ACCEPT CUSM-DOC-CLIENTE.

           IF CUSM-DOC-CLIENTE = SPACES
               DISPLAY "ERROR: La cedula no puede estar vacia."
               GO TO 4000-MODIFICA-CLIENTE
           END-IF.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               DISPLAY "Direccion actual : " CUSM-DIRECCION
               DISPLAY "Nueva Direccion  : "
               ACCEPT CUSM-DIRECCION
               DISPLAY "Telefono actual  : " CUSM-TELEFONO
               DISPLAY "Nuevo Telefono   : "
               ACCEPT CUSM-TELEFONO
               DISPLAY "Email actual     : " CUSM-EMAIL
               DISPLAY "Nuevo Email      : "
               ACCEPT CUSM-EMAIL

               MOVE 'M' TO LK-ACCION-DB
               CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                   LK-DATOS-TRANSACCION

               IF LK-COD-RETORNO = 0
                   DISPLAY "CLIENTE ACTUALIZADO CORRECTAMENTE."
               ELSE
                   DISPLAY "ERROR AL ACTUALIZAR: " LK-MENSAJE
               END-IF
           ELSE
               DISPLAY "CLIENTE NO EXISTE."
           END-IF.

       5000-BAJA-CLIENTE.
           DISPLAY "--- BAJA LOGICA DE CLIENTE ---".
           INITIALIZE REG-CUSM.

           DISPLAY "Ingrese Cedula: "
           ACCEPT CUSM-DOC-CLIENTE.

           IF CUSM-DOC-CLIENTE = SPACES
               DISPLAY "ERROR: La cedula no puede estar vacia."
               GO TO 5000-BAJA-CLIENTE
           END-IF.

      *    Verificar que existe
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               IF CUSM-CTA-ACTIVA = 0
                   DISPLAY "AVISO: Este cliente ya esta dado de baja."
               ELSE
                   DISPLAY "Cliente  : " CUSM-NOMBRE " " CUSM-APELLIDOS
                   DISPLAY "Confirmar baja? (S/N): "
                   ACCEPT WS-CONFIRMAR
                   IF WS-CONFIRMAR = 'S' OR 's'
                       MOVE 'B' TO LK-ACCION-DB
                       CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                           LK-DATOS-TRANSACCION
                       DISPLAY LK-MENSAJE
                   ELSE
                       DISPLAY "BAJA CANCELADA."
                   END-IF
               END-IF
           ELSE
               DISPLAY "CLIENTE NO EXISTE."
           END-IF.

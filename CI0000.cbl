       IDENTIFICATION DIVISION.
       PROGRAM-ID. CI0000.
      *================================================================*
      * PROGRAMA: MAINLINE CIF (CUSTOMER INFORMATION FACILITY)         *
      * FUNCION:  Gestiona Altas, Bajas, Modificaciones y Consultas.   *
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-OPCION-CIF       PIC 9(01).
       01 WS-CONTINUAR-CIF    PIC X(01) VALUE 'S'.
       01 WS-TIENE-CUENTA     PIC X(01) VALUE 'N'.

       01 WS-PROGRAMAS.
           05 WS-PGM-DBIOCUSM    PIC X(8) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOINVM    PIC X(8) VALUE 'DBIOINVM'.

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
           MOVE 'A' TO LK-ACCION-DB.
           INITIALIZE REG-CUSM.

           DISPLAY "Ingrese ID de Cliente (8 digitos): "

           ACCEPT CUSM-ID-CLIENTE.

           DISPLAY "Ingrese Nombre Completo: "
           ACCEPT CUSM-NOMBRE.

           DISPLAY "Ingrese Apellidos: "
           ACCEPT CUSM-APELLIDOS.

           DISPLAY "Ingrese Direccion: "
           ACCEPT CUSM-DIRECCION.

           DISPLAY "Ingrese Telefono: "
           ACCEPT CUSM-TELEFONO.

           DISPLAY "Asignar Cuenta Corriente Inicial (S/N)?: "

           ACCEPT WS-TIENE-CUENTA.

      *    REGLA DE NEGOCIO: Un cliente no puede existir sin cuenta cte
           IF WS-TIENE-CUENTA = 'S' OR 's'
               MOVE 1 TO CUSM-CTA-ACTIVA

      *        1. Insertamos el Cliente
               MOVE 'CI' TO CUSM-TIPO-DOC
               MOVE CUSM-ID-CLIENTE TO CUSM-DOC-CLIENTE
               MOVE '2024-05-05' TO CUSM-FECHA-ALTA
               MOVE 'correo@banco.com' TO CUSM-EMAIL
               MOVE 0 TO CUSM-TARJETA
               MOVE 0 TO CUSM-CREDITO
               MOVE 0 TO CUSM-HIPOTECA
               MOVE '9999-12-31' TO CUSM-FECHA-CIERRE
               MOVE 0 TO CUSM-SALDO-CLIENTE

               CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION

               IF LK-COD-RETORNO = 0
                   DISPLAY LK-MENSAJE
                   DISPLAY "Creando cuenta corriente asociada..."

      *            2. Creamos la cuenta en DBIOINVM
                   INITIALIZE REG-INVM
                   MOVE CUSM-ID-CLIENTE TO INVM-ID-CLIENTE
                   MOVE 0 TO INVM-COD-ULT-MOV
                   MOVE '2023-01-01' TO INVM-FECHA-ULT-MOV
                   MOVE 0 TO INVM-IMPORTE-MOV
                   MOVE 0 TO INVM-SALDO-ACTUAL

                   MOVE 'A' TO LK-ACCION-DB
                   CALL WS-PGM-DBIOINVM USING REG-INVM,
                   LK-DATOS-TRANSACCION

                   DISPLAY LK-MENSAJE
               ELSE
                   DISPLAY "Error al crear cliente: " LK-MENSAJE
               END-IF
           ELSE
               DISPLAY "ERROR: REGLA DE NEGOCIO."
               DISPLAY "El cliente DEBE tener una cuenta corriente."
               DISPLAY "Alta cancelada."
           END-IF.

       3000-CONSULTA-CLIENTE.
           DISPLAY "--- CONSULTA DE CLIENTE ---".
           MOVE 'C' TO LK-ACCION-DB.

           DISPLAY "Ingrese ID de Cliente a consultar: "

           ACCEPT CUSM-ID-CLIENTE.

           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               DISPLAY "-----------------------------------"
               DISPLAY "Nombre: " CUSM-NOMBRE
               DISPLAY "Apellidos: " CUSM-APELLIDOS
               DISPLAY "Direccion: " CUSM-DIRECCION
               DISPLAY "Telefono: " CUSM-TELEFONO
               DISPLAY "Cuenta Activa: " CUSM-CTA-ACTIVA
               DISPLAY "-----------------------------------"
           ELSE
               DISPLAY LK-MENSAJE
           END-IF.

       4000-MODIFICA-CLIENTE.
           DISPLAY "--- MODIFICACION DE CLIENTE ---".
      *    Primero consultamos para verificar que existe
           MOVE 'C' TO LK-ACCION-DB.
           DISPLAY "Ingrese ID de Cliente a modificar: "

           ACCEPT CUSM-ID-CLIENTE.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               DISPLAY "Cliente encontrado: " CUSM-NOMBRE
               DISPLAY "Nueva Direccion: "
               ACCEPT CUSM-DIRECCION
               DISPLAY "Nuevo Telefono: "
               ACCEPT CUSM-TELEFONO

               MOVE 'M' TO LK-ACCION-DB
               CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION
               DISPLAY LK-MENSAJE
           ELSE
               DISPLAY "Cliente no encontrado."
           END-IF.

       5000-BAJA-CLIENTE.
           DISPLAY "--- BAJA LOGICA DE CLIENTE ---".
           MOVE 'B' TO LK-ACCION-DB.
           DISPLAY "Ingrese ID de Cliente a dar de baja: "

           ACCEPT CUSM-ID-CLIENTE.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.
           DISPLAY LK-MENSAJE.

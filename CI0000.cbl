       IDENTIFICATION DIVISION.
       PROGRAM-ID. CI0000.
      *AUTHOR.     CORE-BANCARIO-TEAM.
      *================================================================*
      * PROGRAMA: MAINLINE CIF (CUSTOMER INFORMATION FACILITY)         *
      * FUNCION:  Gestiona Altas, Bajas, Modificaciones y Consultas.   *
      * REGLA:    Valida que el cliente tenga una cuenta asociada (INVM)*
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-OPCION-CIF       PIC 9(01).
       01 WS-CONTINUAR-CIF    PIC X(01) VALUE 'S'.

      * COPY de la tabla maestra de Clientes (CUSM)
           COPY CUSMREC.

      * COPY de la tabla maestra de Cuentas (INVM) - Usado para validar
           COPY INVMREC.



      * Variables para validación de Cuenta Obligatoria
           01 WS-TIENE-CUENTA     PIC X(01) VALUE 'N'.

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
           DISPLAY "Seleccione operacion: " WITH NO ADVANCING.
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

           DISPLAY "Ingrese ID de Cliente (8 digitos): "
               WITH NO ADVANCING.
           ACCEPT CUSM-ID-CLIENTE.

           DISPLAY "Ingrese Nombre Completo: " WITH NO ADVANCING.
           ACCEPT CUSM-NOMBRE.

           DISPLAY "Ingrese Direccion: " WITH NO ADVANCING.
           ACCEPT CUSM-DIRECCION.

           DISPLAY "Ingrese Telefono: " WITH NO ADVANCING.
           ACCEPT CUSM-TELEFONO.

           MOVE 'A' TO CUSM-ESTADO.

           DISPLAY "Asignar Cuenta Corriente Inicial (S/N)?: "
                   WITH NO ADVANCING.
           ACCEPT WS-TIENE-CUENTA.

      * REGLA DE NEGOCIO: Un cliente no puede existir sin cuenta cte
           IF WS-TIENE-CUENTA = 'S' OR 's'
               MOVE 1 TO CUSM-FLG-CTA-ACTIVA

      *        1. Insertamos el Cliente en la base de datos
               CALL 'DBIOCUSM' USING REG-CUSM, LK-DATOS-TRANSACCION

               IF LK-COD-RETORNO = 0
                   DISPLAY LK-MENSAJE
                   DISPLAY "Creando cuenta corriente asociada..."

      *            2. Preparamos datos para la tabla INVM (Cuentas)
                   MOVE CUSM-ID-CLIENTE TO INVM-ID-CLIENTE
                   MOVE 0 TO INVM-SALDO-ACTUAL
                   MOVE 'A' TO LK-ACCION-DB

      *            3. Insertamos la Cuenta asociada
                   CALL 'DBIOINVM' USING REG-INVM, LK-DATOS-TRANSACCION

                   IF LK-COD-RETORNO = 0
                       DISPLAY "Cuenta Corriente creada exitosamente."
                   ELSE
                       DISPLAY "Error al crear la cuenta: " LK-MENSAJE
      *                AQUI DEBERIA IR UN ROLLBACK DE LA CREACION DEL
                   END-IF
               ELSE
                   DISPLAY "Error al registrar cliente: " LK-MENSAJE
               END-IF
           ELSE
               DISPLAY "ERROR DE NEGOCIO: El cliente DEBE tener una"
               "cuenta asociada."
               DISPLAY "Alta cancelada."
           END-IF.

       3000-CONSULTA-CLIENTE.
           DISPLAY "--- CONSULTA DE CLIENTE ---".
           MOVE 'C' TO LK-ACCION-DB.

           DISPLAY "Ingrese ID de Cliente a buscar: " WITH NO ADVANCING.
           ACCEPT CUSM-ID-CLIENTE.

           CALL 'DBIOCUSM' USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               DISPLAY "Datos del Cliente: "
               DISPLAY "Nombre:    " CUSM-NOMBRE
               DISPLAY "Direccion: " CUSM-DIRECCION
               DISPLAY "Telefono:  " CUSM-TELEFONO
               DISPLAY "Estado:    " CUSM-ESTADO
           ELSE
               DISPLAY "Error: " LK-MENSAJE
           END-IF.

       4000-MODIFICA-CLIENTE.
           DISPLAY "--- MODIFICACION DE CLIENTE ---".
           DISPLAY "Solo se permite modificar Direccion y Telefono.".
           MOVE 'M' TO LK-ACCION-DB.

           DISPLAY "Ingrese ID de Cliente a modificar: "
           WITH NO ADVANCING.
           ACCEPT CUSM-ID-CLIENTE.

           DISPLAY "Ingrese nueva Direccion: " WITH NO ADVANCING.
           ACCEPT CUSM-DIRECCION.

           DISPLAY "Ingrese nuevo Telefono: " WITH NO ADVANCING.
           ACCEPT CUSM-TELEFONO.

           CALL 'DBIOCUSM' USING REG-CUSM, LK-DATOS-TRANSACCION.

           DISPLAY LK-MENSAJE.

       5000-BAJA-CLIENTE.
           DISPLAY "--- BAJA DE CLIENTE ---".
           MOVE 'B' TO LK-ACCION-DB.

           DISPLAY "Ingrese ID de Cliente a dar de baja: " NO ADVANCING.
           ACCEPT CUSM-ID-CLIENTE.

           MOVE 'I' TO CUSM-ESTADO. *> Baja Lógica (Pasa a Inactivo)

           CALL 'DBIOCUSM' USING REG-CUSM, LK-DATOS-TRANSACCION.

           DISPLAY LK-MENSAJE.

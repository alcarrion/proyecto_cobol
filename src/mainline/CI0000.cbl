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

       01  WS-AUXILIARES-VALIDACION.
           05 WS-I                PIC 9(02).
           05 WS-DOC-LEN          PIC 9(02).
           05 WS-DOC-VALIDO       PIC X(01) VALUE 'N'.


       01  WS-EMAIL-VAL.
           05  WS-EMAIL-TEMP       PIC X(40).
           05  WS-LOCAL-PART       PIC X(40).
           05  WS-DOMAIN-PART      PIC X(40).
           05  WS-AT-COUNT         PIC 9(02).
           05  WS-DOT-COUNT        PIC 9(02).
           05  WS-LEN              PIC 9(03).
           05  WS-IND              PIC 9(03).
           05  WS-EMAIL-VALIDO     PIC X(01).
               88 EMAIL-OK         VALUE 'S'.
               88 EMAIL-ERR        VALUE 'N'.

      * BANDERAS DE REINTENTO
       01  WS-VAL-CAMPOS.
           05 WS-NOMBRE-OK        PIC X(01) VALUE 'N'.
           05 WS-APELLIDO-OK      PIC X(01) VALUE 'N'.
           05 WS-EMAIL-OK         PIC X(01) VALUE 'N'.

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
           DISPLAY "            BANCOCORE S.A.             ".
           DISPLAY "        MODULO ABM CLIENTES (CIF)      ".
           DISPLAY "========================================".
           DISPLAY "Sucursal: 001    Operador: MGONZALEZ".
           DISPLAY "----------------------------------------".
           DISPLAY " 1. Alta de Cliente".
           DISPLAY " 2. Consulta de Cliente".
           DISPLAY " 3. Modificacion de Cliente".
           DISPLAY " 4. Baja (Logica) de Cliente".
           DISPLAY " 0. Volver al Menu Principal".
           DISPLAY "========================================".
           DISPLAY "Ingrese opcion y presione ENTER: "
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
           PERFORM 9100-VALIDAR-DOC-CAPTURA.


      *    Verificar si ya existe
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               DISPLAY "ERROR: ESA CEDULA YA EXISTE EN EL SISTEMA."
           ELSE
               MOVE 'N' TO WS-NOMBRE-OK
               PERFORM UNTIL WS-NOMBRE-OK = 'S'
                   DISPLAY "Nombre(s)    : "
                   ACCEPT CUSM-NOMBRE
                   IF CUSM-NOMBRE = SPACES
                       DISPLAY "ERROR: El nombre no puede estar vacio."
                   ELSE
                       MOVE 'S' TO WS-NOMBRE-OK
                   END-IF
               END-PERFORM

               MOVE 'N' TO WS-APELLIDO-OK
               PERFORM UNTIL WS-APELLIDO-OK = 'S'
                   DISPLAY "Apellido(s)  : "
                   ACCEPT CUSM-APELLIDOS
                   IF CUSM-APELLIDOS = SPACES
                      DISPLAY "ERROR: El apellido no puede estar vacio."
                   ELSE
                       MOVE 'S' TO WS-APELLIDO-OK
                   END-IF
               END-PERFORM

               DISPLAY "Direccion    : "
               ACCEPT CUSM-DIRECCION
               DISPLAY "Telefono     : "
               ACCEPT CUSM-TELEFONO


               MOVE 'N' TO WS-EMAIL-OK
               PERFORM UNTIL WS-EMAIL-OK = 'S'
                   DISPLAY "E-mail       : "
                   ACCEPT CUSM-EMAIL
                   PERFORM 9200-VALIDAR-EMAIL
                   IF EMAIL-ERR
                       DISPLAY ">>> ERROR DE VALIDACION: " LK-MENSAJE
                       DISPLAY "POR FAVOR REINGRESE EL CORREO."
                   ELSE
                       MOVE 'S' TO WS-EMAIL-OK
                   END-IF
               END-PERFORM


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



       3000-CONSULTA-CLIENTE.
           DISPLAY "--- CONSULTA DE CLIENTE ---".
           INITIALIZE REG-CUSM.

           PERFORM 9100-VALIDAR-DOC-CAPTURA.

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

           PERFORM 9100-VALIDAR-DOC-CAPTURA.

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

           PERFORM 9100-VALIDAR-DOC-CAPTURA.

      *    Verificar que existe
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = 0
               IF CUSM-CTA-ACTIVA = 0
                   DISPLAY "AVISO: Este cliente ya esta dado de baja."
               ELSE
                   IF CUSM-SALDO-CLIENTE NOT = ZERO OR
                      CUSM-TARJETA NOT = ZERO OR
                      CUSM-HIPOTECA NOT = ZERO

               DISPLAY "**********************************************"
               DISPLAY "ERROR: NO SE PUEDE DAR DE BAJA AL CLIENTE"
               DISPLAY "MOTIVO: EL CLIENTE TIENE PRODUCTOS ACTIVOS"
               DISPLAY "----------------------------------------------"
               DISPLAY "SALDO ACTUAL : " CUSM-SALDO-CLIENTE
               DISPLAY "TIENE TARJETA: " CUSM-TARJETA " (1=SI, 0=NO)"
               DISPLAY "TIENE HIPOT. : " CUSM-HIPOTECA " (1=SI, 0=NO)"
               DISPLAY "**********************************************"
               DISPLAY "SOLUCION: DEBE CANCELAR PRODUCTOS Y RETIRAR"
               DISPLAY "SALDO ANTES DE CONTINUAR."

                   ELSE
               DISPLAY "CLIENTE  : " CUSM-NOMBRE " " CUSM-APELLIDOS
                       DISPLAY "CONFIRMAR BAJA DEFINITIVA? (S/N): "
                       ACCEPT WS-CONFIRMAR

                       IF WS-CONFIRMAR = 'S' OR 's'
                           MOVE 'B' TO LK-ACCION-DB
                           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                                  LK-DATOS-TRANSACCION
                           DISPLAY ">>> " LK-MENSAJE
                       ELSE
                           DISPLAY "OPERACION CANCELADA POR EL USUARIO."
                       END-IF
                   END-IF
               END-IF
           ELSE
              DISPLAY "ERROR: EL CLIENTE NO EXISTE EN LA BASE DE DATOS."
           END-IF.


       9100-VALIDAR-DOC-CAPTURA.
           MOVE 'N' TO WS-DOC-VALIDO.

           PERFORM UNTIL WS-DOC-VALIDO = 'S'
               DISPLAY "----------------------------------------------"
               DISPLAY "INGRESE DOCUMENTO (8 CED / 12 PAS): "
               ACCEPT CUSM-DOC-CLIENTE

      * Calcular longitud real (eliminando espacios a la derecha)
               MOVE 0 TO WS-DOC-LEN
               MOVE 12 TO WS-I
               PERFORM UNTIL WS-I = 0 OR CUSM-DOC-CLIENTE(WS-I:1)
               NOT = SPACE
                   SUBTRACT 1 FROM WS-I
               END-PERFORM
               MOVE WS-I TO WS-DOC-LEN

      * L�gica de Validaci�n y Asignaci�n Autom�tica
               IF WS-DOC-LEN >= 8 AND WS-DOC-LEN <= 12
                   MOVE 'S' TO WS-DOC-VALIDO

      * Identificaci�n autom�tica
                   IF WS-DOC-LEN = 8
                       MOVE "CED" TO CUSM-TIPO-DOC
                   ELSE
                       MOVE "PAS" TO CUSM-TIPO-DOC
                   END-IF

                   DISPLAY ">>> SISTEMA: DOCUMENTO VALIDO"
                   DISPLAY ">>> TIPO ASIGNADO: " CUSM-TIPO-DOC
               ELSE
                   DISPLAY "ERROR: LONGITUD INVALIDA (" WS-DOC-LEN ")"
                   DISPLAY "DEBE TENER ENTRE 8 Y 12 CARACTERES."
                   DISPLAY "POR FAVOR, INTENTE DE NUEVO."
               END-IF
           END-PERFORM.

       9200-VALIDAR-EMAIL.
           SET EMAIL-OK TO TRUE
           INITIALIZE WS-LOCAL-PART WS-DOMAIN-PART
           MOVE 0 TO WS-AT-COUNT WS-DOT-COUNT WS-IND

           MOVE CUSM-EMAIL TO WS-EMAIL-TEMP

           *> 1. Buscar espacios en blanco (No permitidos en banca)
           INSPECT WS-EMAIL-TEMP TALLYING WS-IND FOR ALL ' '
           *> Calculamos longitud real restando espacios finales
           COMPUTE WS-LEN = 40 - WS-IND

           *> 2. Validar Arrobas (Debe haber exactamente una)
           INSPECT WS-EMAIL-TEMP TALLYING WS-AT-COUNT FOR ALL '@'

           IF WS-AT-COUNT NOT = 1
               SET EMAIL-ERR TO TRUE
               MOVE "EL EMAIL DEBE TENER EXACTAMENTE UNA @"
               TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF

           *> 3. Descomponer Email
           UNSTRING WS-EMAIL-TEMP DELIMITED BY '@'
               INTO WS-LOCAL-PART, WS-DOMAIN-PART
           END-UNSTRING

           *> 4. Validar que local y dominio no est�n vac�os
           IF WS-LOCAL-PART = SPACES OR WS-DOMAIN-PART = SPACES
               SET EMAIL-ERR TO TRUE
               MOVE "FORMATO DE EMAIL INCOMPLETO" TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF

           *> 5. Validar puntos en el dominio (Debe tener al menos uno)
           INSPECT WS-DOMAIN-PART TALLYING WS-DOT-COUNT FOR ALL '.'

           IF WS-DOT-COUNT < 1
               SET EMAIL-ERR TO TRUE
               MOVE "DOMINIO SIN PUNTO (EJ. .COM, .EC)" TO LK-MENSAJE
           END-IF.

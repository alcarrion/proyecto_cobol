       IDENTIFICATION DIVISION.
       PROGRAM-ID. CI0000.
      *================================================================*
      * PROGRAMA: MAINLINE CIF (CUSTOMER INFORMATION FACILITY)         *
      * FUNCION:  Gestiona Altas, Bajas, Modificaciones y Consultas.   *
      * NIVEL:    1000 TCS ECUADOR                                      *
      * CAMBIOS v2.0:                                                   *
      *   - Alta captura FECHA_NACIMIENTO e INGRESOS_MENSUALES          *
      *   - Validacion de mayoria de edad (18 anios)                    *
      *   - Calculo de score crediticio en alta                         *
      *   - Tipo de cuenta elegido por operador al alta                 *
      *   - LK-COD-RETORNO es ahora PIC X(04)                          *
      *   - DBIOCUSM usa accion 'K' para scoring, 'T' para flags        *
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

       01  WS-CEDULA-VALID.
           05 WS-CED-IDX          PIC 9(02) VALUE 0.
           05 WS-CED-PROV         PIC 9(02) VALUE 0.
           05 WS-CED-TERCERO      PIC 9(01) VALUE 0.
           05 WS-CED-DIGITO       PIC 9(01) VALUE 0.
           05 WS-CED-PRODUCTO     PIC 9(02) VALUE 0.
           05 WS-CED-SUMA         PIC 9(03) VALUE 0.
           05 WS-CED-VERIF-CALC   PIC 9(01) VALUE 0.
           05 WS-CED-VERIF-DOC    PIC 9(01) VALUE 0.
           05 WS-CED-VALIDA       PIC X(01) VALUE 'N'.

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

       01  WS-LOG-MSG           PIC X(80).

      * BANDERAS DE REINTENTO
       01  WS-VAL-CAMPOS.
           05 WS-NOMBRE-OK        PIC X(01) VALUE 'N'.
           05 WS-APELLIDO-OK      PIC X(01) VALUE 'N'.
           05 WS-EMAIL-OK         PIC X(01) VALUE 'N'.
           05 WS-TELEFONO-OK      PIC X(01) VALUE 'N'.
           05 WS-DIGITOS-EN-CAMPO PIC 9(02) VALUE 0.
           05 WS-FECHA-OK         PIC X(01) VALUE 'N'.

      * VARIABLES PARA CALCULO DE EDAD Y SCORE
       01  WS-EDAD-CALCULO.
           05 WS-ANIO-NACIMIENTO  PIC 9(04).
           05 WS-MES-NACIMIENTO   PIC 9(02).
           05 WS-DIA-NACIMIENTO   PIC 9(02).
           05 WS-EDAD-CALCULADA   PIC 9(03).
           05 WS-SCORE-CALCULADO  PIC 9(03).

      * ENTRADA PARA INGRESOS
       01  WS-INGRESO-ENTRADA     PIC X(15).

      * TIPO DE CUENTA PARA ALTA
       01  WS-TIPO-CTA-SEL        PIC X(01).

           COPY CUSMREC.
           COPY INVMREC.

       LINKAGE SECTION.
           COPY LKCIF.

       PROCEDURE DIVISION USING LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 'S' TO WS-CONTINUAR-CIF.
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
           PERFORM 9100-VALIDAR-DOC-CAPTURA.

      *    Verificar si ya existe
           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = '00  '
               DISPLAY "ERROR: ESA CEDULA YA EXISTE EN EL SISTEMA."
               STRING "[WARN] CI0000 ALTA RECHAZADA DUPLICADO DOC:"
                      CUSM-DOC-CLIENTE
                      DELIMITED BY SIZE INTO WS-LOG-MSG
               CALL 'SYSLOG' USING WS-LOG-MSG
           ELSE
      *        Capturar nombre
               MOVE 'N' TO WS-NOMBRE-OK
               PERFORM UNTIL WS-NOMBRE-OK = 'S'
                   DISPLAY "Nombre(s)    : "
                   ACCEPT CUSM-NOMBRE
                   IF CUSM-NOMBRE = SPACES
                       DISPLAY "ERROR: Nombre no puede estar vacio."
                   ELSE
                       MOVE ZERO TO WS-DIGITOS-EN-CAMPO
                       PERFORM VARYING WS-I FROM 1 BY 1
                               UNTIL WS-I > 25
                           IF CUSM-NOMBRE(WS-I:1) IS NUMERIC
                              OR CUSM-NOMBRE(WS-I:1) = '@'
                              OR CUSM-NOMBRE(WS-I:1) = '#'
                              OR CUSM-NOMBRE(WS-I:1) = '$'
                              OR CUSM-NOMBRE(WS-I:1) = '%'
                              OR CUSM-NOMBRE(WS-I:1) = '!'
                               ADD 1 TO WS-DIGITOS-EN-CAMPO
                           END-IF
                       END-PERFORM
                       IF WS-DIGITOS-EN-CAMPO > 0
                           DISPLAY "ERROR: Solo letras y espacios."
                       ELSE
                           MOVE 'S' TO WS-NOMBRE-OK
                       END-IF
                   END-IF
               END-PERFORM

      *        Capturar apellidos
               MOVE 'N' TO WS-APELLIDO-OK
               PERFORM UNTIL WS-APELLIDO-OK = 'S'
                   DISPLAY "Apellido(s)  : "
                   ACCEPT CUSM-APELLIDOS
                   IF CUSM-APELLIDOS = SPACES
                       DISPLAY "ERROR: Apellido no puede estar vacio."
                   ELSE
                       MOVE ZERO TO WS-DIGITOS-EN-CAMPO
                       PERFORM VARYING WS-I FROM 1 BY 1
                               UNTIL WS-I > 25
                           IF CUSM-APELLIDOS(WS-I:1) IS NUMERIC
                              OR CUSM-APELLIDOS(WS-I:1) = '@'
                              OR CUSM-APELLIDOS(WS-I:1) = '#'
                              OR CUSM-APELLIDOS(WS-I:1) = '$'
                              OR CUSM-APELLIDOS(WS-I:1) = '%'
                              OR CUSM-APELLIDOS(WS-I:1) = '!'
                               ADD 1 TO WS-DIGITOS-EN-CAMPO
                           END-IF
                       END-PERFORM
                       IF WS-DIGITOS-EN-CAMPO > 0
                           DISPLAY "ERROR: Solo letras y espacios."
                       ELSE
                           MOVE 'S' TO WS-APELLIDO-OK
                       END-IF
                   END-IF
               END-PERFORM

      *        Capturar fecha de nacimiento y validar mayoria de edad
               MOVE 'N' TO WS-FECHA-OK
               PERFORM UNTIL WS-FECHA-OK = 'S'
                   DISPLAY "Fecha Nacim. (AAAA-MM-DD): "
                   ACCEPT CUSM-FECHA-NACIMIENTO
                   IF CUSM-FECHA-NACIMIENTO = SPACES
                       DISPLAY "ERROR: Fecha no puede estar vacia."
                   ELSE
                       PERFORM 9200-VALIDAR-MAYORIA-EDAD
                       IF LK-COD-RETORNO = 'E018'
                           DISPLAY ">>> " LK-MENSAJE
                       ELSE
                           MOVE 'S' TO WS-FECHA-OK
                       END-IF
                   END-IF
               END-PERFORM

      *        Capturar direccion
               MOVE 'N' TO WS-NOMBRE-OK
               PERFORM UNTIL WS-NOMBRE-OK = 'S'
                   DISPLAY "Direccion    : "
                   ACCEPT CUSM-DIRECCION
                   IF CUSM-DIRECCION = SPACES
                       DISPLAY "ERROR: Direccion no puede estar vacia."
                   ELSE
                       MOVE 'S' TO WS-NOMBRE-OK
                   END-IF
               END-PERFORM

      *        Capturar telefono
               MOVE 'N' TO WS-TELEFONO-OK
               PERFORM UNTIL WS-TELEFONO-OK = 'S'
                   DISPLAY "Telefono     : "
                   ACCEPT CUSM-TELEFONO
                   IF CUSM-TELEFONO = SPACES
                       DISPLAY "ERROR: Telefono no puede estar vacio."
                   ELSE
                       MOVE 12 TO WS-I
                       PERFORM UNTIL WS-I = 0 OR
                               CUSM-TELEFONO(WS-I:1) NOT = SPACE
                           SUBTRACT 1 FROM WS-I
                       END-PERFORM
                       MOVE WS-I TO WS-DOC-LEN
                       IF WS-DOC-LEN < 7 OR WS-DOC-LEN > 12
                           DISPLAY "ERROR: Telefono: 7 a 12 digitos."
                       ELSE
                           MOVE 'S' TO WS-TELEFONO-OK
                           PERFORM VARYING WS-I FROM 1 BY 1
                                   UNTIL WS-I > WS-DOC-LEN
                               IF CUSM-TELEFONO(WS-I:1) NOT NUMERIC
                                   MOVE 'N' TO WS-TELEFONO-OK
                               END-IF
                           END-PERFORM
                           IF WS-TELEFONO-OK = 'N'
                               DISPLAY "ERROR: Solo digitos permitidos."
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM

      *        Capturar email
               MOVE 'N' TO WS-EMAIL-OK
               PERFORM UNTIL WS-EMAIL-OK = 'S'
                   DISPLAY "E-mail       : "
                   ACCEPT CUSM-EMAIL
                   PERFORM 9250-VALIDAR-EMAIL
                   IF EMAIL-ERR
                       DISPLAY ">>> ERROR DE VALIDACION: " LK-MENSAJE
                       DISPLAY "POR FAVOR REINGRESE EL CORREO."
                   ELSE
                       MOVE 'S' TO WS-EMAIL-OK
                   END-IF
               END-PERFORM

      *        Capturar ingresos mensuales
               PERFORM UNTIL WS-INGRESO-ENTRADA NOT = SPACES
                   DISPLAY "Ingresos Mensuales: "
                   ACCEPT WS-INGRESO-ENTRADA
                   IF WS-INGRESO-ENTRADA = SPACES
                       DISPLAY "ERROR: Ingrese sus ingresos mensuales."
                   END-IF
               END-PERFORM
               COMPUTE CUSM-INGRESOS =
                   FUNCTION NUMVAL(WS-INGRESO-ENTRADA)

      *        Capturar tipo de cuenta
               MOVE 'N' TO WS-FECHA-OK
               PERFORM UNTIL WS-FECHA-OK = 'S'
                   DISPLAY "Tipo Cuenta (A=Ahorros C=Corriente H=Cheques): "
                   ACCEPT WS-TIPO-CTA-SEL
                   IF WS-TIPO-CTA-SEL = 'A' OR 'C' OR 'H'
                       MOVE 'S' TO WS-FECHA-OK
                   ELSE
                       DISPLAY "ERROR: Use A, C o H."
                   END-IF
               END-PERFORM

      *        Calcular score
               PERFORM 9300-CALCULAR-SCORE

      *        Setear campos automaticos
               MOVE 'A' TO CUSM-ESTADO-CLIENTE
               MOVE 0 TO CUSM-SALDO-TOTAL-VISTA
               MOVE 0 TO CUSM-TIENE-TARJETA
               MOVE 0 TO CUSM-TIENE-HIPOTECA
               ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD
               STRING WS-ANIO "-" WS-MES "-" WS-DIA
                   DELIMITED BY SIZE INTO CUSM-FECHA-ALTA

               DISPLAY "Score calculado: " WS-SCORE-CALCULADO
               MOVE WS-SCORE-CALCULADO TO CUSM-SCORE-CREDITICIO

      *        PASO 1: INSERTAR CLIENTE
               MOVE 'A' TO LK-ACCION-DB
               CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                   LK-DATOS-TRANSACCION

               IF LK-COD-RETORNO = '00  '
      *            PASO 2: CREAR CUENTA
                   INITIALIZE REG-INVM
                   MOVE CUSM-ID-CLIENTE    TO INVM-ID-CLIENTE
                   MOVE WS-TIPO-CTA-SEL    TO INVM-TIPO-CUENTA
                   MOVE 0                  TO INVM-SALDO-ACTUAL
                   MOVE CUSM-FECHA-ALTA    TO INVM-FECHA-APERTURA
                   MOVE 'A'                TO INVM-ESTADO-CUENTA

                   MOVE 'A' TO LK-ACCION-DB
                   CALL WS-PGM-DBIOINVM USING REG-INVM,
                       LK-DATOS-TRANSACCION

                   IF LK-COD-RETORNO = '00  '
                       CALL 'DBIOTRAN' USING 'C'
                       DISPLAY "ALTA EXITOSA. ID: " CUSM-ID-CLIENTE
                       DISPLAY "CUENTA ID: " INVM-ID-CUENTA
                       DISPLAY "SCORE    : " CUSM-SCORE-CREDITICIO
                       STRING "[OK  ] CI0000 ALTA DOC:"
                              CUSM-DOC-CLIENTE " ID:" CUSM-ID-CLIENTE
                              DELIMITED BY SIZE INTO WS-LOG-MSG
                       CALL 'SYSLOG' USING WS-LOG-MSG
                   ELSE
                       CALL 'DBIOTRAN' USING 'R'
                       DISPLAY "ERROR EN CUENTA. NADA SE GUARDO."
                       MOVE "[ERR ] CI0000 ALTA ERROR CREANDO CUENTA"
                           TO WS-LOG-MSG
                       CALL 'SYSLOG' USING WS-LOG-MSG
                   END-IF
               ELSE
                   CALL 'DBIOTRAN' USING 'R'
                   DISPLAY "FALLO ALTA CLIENTE: " LK-MENSAJE
                   STRING "[ERR ] CI0000 ALTA ERROR BD " LK-MENSAJE
                          DELIMITED BY SIZE INTO WS-LOG-MSG
                   CALL 'SYSLOG' USING WS-LOG-MSG
               END-IF
           END-IF.

       3000-CONSULTA-CLIENTE.
           DISPLAY "--- CONSULTA DE CLIENTE ---".
           INITIALIZE REG-CUSM.

           PERFORM 9100-VALIDAR-DOC-CAPTURA.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = '00  '
               DISPLAY "-----------------------------------"
               DISPLAY "CEDULA    : " CUSM-DOC-CLIENTE
               DISPLAY "CLIENTE   : " CUSM-NOMBRE " " CUSM-APELLIDOS
               DISPLAY "F.NACIM.  : " CUSM-FECHA-NACIMIENTO
               DISPLAY "DIRECCION : " CUSM-DIRECCION
               DISPLAY "TELEFONO  : " CUSM-TELEFONO
               DISPLAY "EMAIL     : " CUSM-EMAIL
               DISPLAY "SCORE     : " CUSM-SCORE-CREDITICIO
               DISPLAY "INGRESOS  : " CUSM-INGRESOS
               DISPLAY "SALDO TOT : " CUSM-SALDO-TOTAL-VISTA
               DISPLAY "ESTADO    : " CUSM-ESTADO-CLIENTE
               DISPLAY "TIE.TARJ. : " CUSM-TIENE-TARJETA
               DISPLAY "TIE.HIPOT.: " CUSM-TIENE-HIPOTECA
               DISPLAY "-----------------------------------"
               STRING "[OK  ] CI0000 CONSULTA ID:" CUSM-ID-CLIENTE
                      " DOC:" CUSM-DOC-CLIENTE
                      DELIMITED BY SIZE INTO WS-LOG-MSG
               CALL 'SYSLOG' USING WS-LOG-MSG
           ELSE
               DISPLAY "CLIENTE NO ENCONTRADO."
               STRING "[WARN] CI0000 CONSULTA NO ENCONTRADO DOC:"
                      CUSM-DOC-CLIENTE
                      DELIMITED BY SIZE INTO WS-LOG-MSG
               CALL 'SYSLOG' USING WS-LOG-MSG
           END-IF.

       4000-MODIFICA-CLIENTE.
           DISPLAY "--- MODIFICACION DE CLIENTE ---".
           INITIALIZE REG-CUSM.

           PERFORM 9100-VALIDAR-DOC-CAPTURA.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM, LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = '00  '
               DISPLAY "Direccion actual : " CUSM-DIRECCION
               DISPLAY "Nueva Direccion  : "
               ACCEPT CUSM-DIRECCION
               DISPLAY "Telefono actual  : " CUSM-TELEFONO
               MOVE 'N' TO WS-TELEFONO-OK
               PERFORM UNTIL WS-TELEFONO-OK = 'S'
                   DISPLAY "Nuevo Telefono   : "
                   ACCEPT CUSM-TELEFONO
                   IF CUSM-TELEFONO = SPACES
                       DISPLAY "ERROR: Telefono no puede estar vacio."
                   ELSE
                       MOVE 12 TO WS-I
                       PERFORM UNTIL WS-I = 0 OR
                               CUSM-TELEFONO(WS-I:1) NOT = SPACE
                           SUBTRACT 1 FROM WS-I
                       END-PERFORM
                       MOVE WS-I TO WS-DOC-LEN
                       IF WS-DOC-LEN < 7 OR WS-DOC-LEN > 12
                           DISPLAY "ERROR: Telefono: 7 a 12 digitos."
                       ELSE
                           MOVE 'S' TO WS-TELEFONO-OK
                           PERFORM VARYING WS-I FROM 1 BY 1
                                   UNTIL WS-I > WS-DOC-LEN
                               IF CUSM-TELEFONO(WS-I:1) NOT NUMERIC
                                   MOVE 'N' TO WS-TELEFONO-OK
                               END-IF
                           END-PERFORM
                           IF WS-TELEFONO-OK = 'N'
                               DISPLAY "ERROR: Solo digitos permitidos."
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM

               DISPLAY "Email actual     : " CUSM-EMAIL
               MOVE 'N' TO WS-EMAIL-OK
               PERFORM UNTIL WS-EMAIL-OK = 'S'
                   DISPLAY "Nuevo Email      : "
                   ACCEPT CUSM-EMAIL
                   PERFORM 9250-VALIDAR-EMAIL
                   IF EMAIL-ERR
                       DISPLAY "ERROR: " LK-MENSAJE
                   ELSE
                       MOVE 'S' TO WS-EMAIL-OK
                   END-IF
               END-PERFORM

               MOVE 'M' TO LK-ACCION-DB
               CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                   LK-DATOS-TRANSACCION

               IF LK-COD-RETORNO = '00  '
                   CALL 'DBIOTRAN' USING 'C'
                   DISPLAY "CLIENTE ACTUALIZADO CORRECTAMENTE."
                   STRING "[OK  ] CI0000 MODIF DOC:"
                          CUSM-DOC-CLIENTE " ID:" CUSM-ID-CLIENTE
                          DELIMITED BY SIZE INTO WS-LOG-MSG
                   CALL 'SYSLOG' USING WS-LOG-MSG
               ELSE
                   CALL 'DBIOTRAN' USING 'R'
                   DISPLAY "ERROR AL ACTUALIZAR: " LK-MENSAJE
                   STRING "[ERR ] CI0000 MODIF ERROR BD " LK-MENSAJE
                          DELIMITED BY SIZE INTO WS-LOG-MSG
                   CALL 'SYSLOG' USING WS-LOG-MSG
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

           IF LK-COD-RETORNO = '00  '
               IF CUSM-ESTADO-CLIENTE = 'I'
                   DISPLAY "AVISO: Este cliente ya esta dado de baja."
               ELSE
                   IF CUSM-SALDO-TOTAL-VISTA NOT = ZERO OR
                      CUSM-TIENE-TARJETA NOT = ZERO OR
                      CUSM-TIENE-HIPOTECA NOT = ZERO

               DISPLAY "**********************************************"
               DISPLAY "ERROR: NO SE PUEDE DAR DE BAJA AL CLIENTE"
               DISPLAY "MOTIVO: EL CLIENTE TIENE PRODUCTOS ACTIVOS"
               DISPLAY "----------------------------------------------"
               DISPLAY "SALDO TOTAL : " CUSM-SALDO-TOTAL-VISTA
               DISPLAY "TIENE TARJ. : " CUSM-TIENE-TARJETA
               DISPLAY "TIENE HIPOT.: " CUSM-TIENE-HIPOTECA
               DISPLAY "**********************************************"
               DISPLAY "SOLUCION: CANCELE PRODUCTOS ANTES DE CONTINUAR"
               STRING "[WARN] CI0000 BAJA RECHAZADA PRODUCTOS ACTIVOS"
                      " ID:" CUSM-ID-CLIENTE
                      DELIMITED BY SIZE INTO WS-LOG-MSG
               CALL 'SYSLOG' USING WS-LOG-MSG

                   ELSE
               DISPLAY "CLIENTE  : " CUSM-NOMBRE " " CUSM-APELLIDOS
                       DISPLAY "CONFIRMAR BAJA DEFINITIVA? (S/N): "
                       ACCEPT WS-CONFIRMAR

                       IF WS-CONFIRMAR = 'S' OR 's'
                           MOVE 'B' TO LK-ACCION-DB
                           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                                  LK-DATOS-TRANSACCION
                           DISPLAY ">>> " LK-MENSAJE
                           IF LK-COD-RETORNO = '00  '
                               CALL 'DBIOTRAN' USING 'C'
                               STRING "[OK  ] CI0000 BAJA DOC:"
                                      CUSM-DOC-CLIENTE
                                      " ID:" CUSM-ID-CLIENTE
                                      DELIMITED BY SIZE
                                      INTO WS-LOG-MSG
                               CALL 'SYSLOG' USING WS-LOG-MSG
                           ELSE
                               CALL 'DBIOTRAN' USING 'R'
                               STRING "[ERR ] CI0000 BAJA ERROR BD "
                                      LK-MENSAJE
                                      DELIMITED BY SIZE
                                      INTO WS-LOG-MSG
                               CALL 'SYSLOG' USING WS-LOG-MSG
                           END-IF
                       ELSE
                           DISPLAY "OPERACION CANCELADA POR EL USUARIO."
                       END-IF
                   END-IF
               END-IF
           ELSE
              DISPLAY "ERROR: EL CLIENTE NO EXISTE EN LA BASE DE DATOS."
              STRING "[WARN] CI0000 BAJA DOC NO ENCONTRADO:"
                     CUSM-DOC-CLIENTE
                     DELIMITED BY SIZE INTO WS-LOG-MSG
              CALL 'SYSLOG' USING WS-LOG-MSG
           END-IF.

       9100-VALIDAR-DOC-CAPTURA.
           MOVE 'N' TO WS-DOC-VALIDO.

           PERFORM UNTIL WS-DOC-VALIDO = 'S'
               DISPLAY "  Cedula (10 digitos) o Pasaporte (6-12): "
               ACCEPT CUSM-DOC-CLIENTE

               MOVE 0 TO WS-DOC-LEN
               MOVE 12 TO WS-I
               PERFORM UNTIL WS-I = 0 OR
                       CUSM-DOC-CLIENTE(WS-I:1) NOT = SPACE
                   SUBTRACT 1 FROM WS-I
               END-PERFORM
               MOVE WS-I TO WS-DOC-LEN

               IF WS-DOC-LEN = 0
                   DISPLAY "  Debe ingresar el documento."
               ELSE IF CUSM-DOC-CLIENTE(1:1) IS NUMERIC
                   IF WS-DOC-LEN = 10
                       PERFORM 9150-VALIDAR-CEDULA-EC
                       IF WS-CED-VALIDA = 'S'
                           MOVE "CED" TO CUSM-TIPO-DOC
                           MOVE 'S' TO WS-DOC-VALIDO
                       ELSE
                           DISPLAY "  " LK-MENSAJE
                       END-IF
                   ELSE
                       DISPLAY "  Cedula invalida: debe tener"
                       DISPLAY "  exactamente 10 digitos."
                   END-IF
               ELSE IF WS-DOC-LEN >= 6 AND WS-DOC-LEN <= 12
                   MOVE "PAS" TO CUSM-TIPO-DOC
                   MOVE 'S' TO WS-DOC-VALIDO
               ELSE
                   DISPLAY "  Pasaporte invalido: 6 a 12 caracteres."
               END-IF
           END-PERFORM.

       9200-VALIDAR-MAYORIA-EDAD.
      *    Verifica que (anio actual - anio nacimiento) >= 18
      *    Si falla: LK-COD-RETORNO = 'E018'
           MOVE '00  ' TO LK-COD-RETORNO.

           ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD.

           MOVE CUSM-FECHA-NACIMIENTO(1:4) TO WS-ANIO-NACIMIENTO.
           MOVE CUSM-FECHA-NACIMIENTO(6:2) TO WS-MES-NACIMIENTO.
           MOVE CUSM-FECHA-NACIMIENTO(9:2) TO WS-DIA-NACIMIENTO.

           COMPUTE WS-EDAD-CALCULADA =
               WS-ANIO - WS-ANIO-NACIMIENTO.

           IF WS-MES < WS-MES-NACIMIENTO OR
              (WS-MES = WS-MES-NACIMIENTO AND
               WS-DIA < WS-DIA-NACIMIENTO)
               SUBTRACT 1 FROM WS-EDAD-CALCULADA
           END-IF.

           IF WS-EDAD-CALCULADA < 18
               MOVE 'E018' TO LK-COD-RETORNO
               MOVE "CLIENTE MENOR DE EDAD" TO LK-MENSAJE
           END-IF.

       9300-CALCULAR-SCORE.
      *    SCORE = (INGRESOS / 10) + (EDAD * 2), maximo 999
           COMPUTE WS-SCORE-CALCULADO =
               (CUSM-INGRESOS / 10) + (WS-EDAD-CALCULADA * 2).
           IF WS-SCORE-CALCULADO > 999
               MOVE 999 TO WS-SCORE-CALCULADO
           END-IF.

       9250-VALIDAR-EMAIL.
           SET EMAIL-OK TO TRUE
           INITIALIZE WS-LOCAL-PART WS-DOMAIN-PART
           MOVE 0 TO WS-AT-COUNT WS-DOT-COUNT WS-IND

           MOVE CUSM-EMAIL TO WS-EMAIL-TEMP

           INSPECT WS-EMAIL-TEMP TALLYING WS-IND FOR ALL ' '
           COMPUTE WS-LEN = 40 - WS-IND

           INSPECT WS-EMAIL-TEMP TALLYING WS-AT-COUNT FOR ALL '@'

           IF WS-AT-COUNT NOT = 1
               SET EMAIL-ERR TO TRUE
               MOVE "EL EMAIL DEBE TENER EXACTAMENTE UNA @"
               TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF

           UNSTRING WS-EMAIL-TEMP DELIMITED BY '@'
               INTO WS-LOCAL-PART, WS-DOMAIN-PART
           END-UNSTRING

           IF WS-LOCAL-PART = SPACES OR WS-DOMAIN-PART = SPACES
               SET EMAIL-ERR TO TRUE
               MOVE "FORMATO DE EMAIL INCOMPLETO" TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF

           INSPECT WS-DOMAIN-PART TALLYING WS-DOT-COUNT FOR ALL '.'

           IF WS-DOT-COUNT < 1
               SET EMAIL-ERR TO TRUE
               MOVE "DOMINIO SIN PUNTO (EJ. .COM, .EC)" TO LK-MENSAJE
           END-IF.

       9150-VALIDAR-CEDULA-EC.
           MOVE 'N' TO WS-CED-VALIDA.
           MOVE SPACES TO LK-MENSAJE.

           MOVE CUSM-DOC-CLIENTE(1:2) TO WS-CED-PROV.
           IF WS-CED-PROV < 1 OR WS-CED-PROV > 24
               MOVE "Cedula invalida: codigo de provincia"
                   TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF.

           MOVE CUSM-DOC-CLIENTE(3:1) TO WS-CED-TERCERO.
           IF WS-CED-TERCERO > 5
               MOVE "Cedula invalida: tipo de persona incorrecto"
                   TO LK-MENSAJE
               EXIT PARAGRAPH
           END-IF.

           MOVE ZERO TO WS-CED-SUMA.
           PERFORM VARYING WS-CED-IDX FROM 1 BY 1
                   UNTIL WS-CED-IDX > 9
               MOVE CUSM-DOC-CLIENTE(WS-CED-IDX:1)
                   TO WS-CED-DIGITO
               IF FUNCTION MOD(WS-CED-IDX, 2) = 1
                   COMPUTE WS-CED-PRODUCTO = WS-CED-DIGITO * 2
               ELSE
                   MOVE WS-CED-DIGITO TO WS-CED-PRODUCTO
               END-IF
               IF WS-CED-PRODUCTO >= 10
                   SUBTRACT 9 FROM WS-CED-PRODUCTO
               END-IF
               ADD WS-CED-PRODUCTO TO WS-CED-SUMA
           END-PERFORM.

           COMPUTE WS-CED-VERIF-CALC =
               FUNCTION MOD(WS-CED-SUMA, 10).
           IF WS-CED-VERIF-CALC NOT = 0
               COMPUTE WS-CED-VERIF-CALC = 10 - WS-CED-VERIF-CALC
           END-IF.
           MOVE CUSM-DOC-CLIENTE(10:1) TO WS-CED-VERIF-DOC.
           IF WS-CED-VERIF-CALC = WS-CED-VERIF-DOC
               MOVE 'S' TO WS-CED-VALIDA
           ELSE
               MOVE "Cedula invalida: digito verificador incorrecto"
                   TO LK-MENSAJE
           END-IF.

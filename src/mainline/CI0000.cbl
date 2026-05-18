      ******************************************************************
      * CI0000.CBL - MODULO DE CLIENTES
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CI0000.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPCION-CIF            PIC 9(01).
       01  WS-CONTINUAR-CIF         PIC X(01) VALUE 'S'.
       01  WS-CONFIRMAR             PIC X(01).
       01  WS-DOC-LEN               PIC 9(02) VALUE 0.
       01  WS-I                     PIC 9(02) VALUE 0.
       01  WS-INGRESO-TEXTO         PIC X(15).
       01  WS-SCORE                 PIC 9(03) VALUE 500.
       01  WS-BLANK-LINE            PIC X(55) VALUE SPACES.

       01  WS-PGM-DBIOCUSM          PIC X(08) VALUE 'DBIOCUSM'.
       01  WS-PGM-DBIOTRAN          PIC X(08) VALUE 'DBIOTRAN'.
       01  WS-ACCION-TRANS          PIC X(01).

       01  WS-FECHA-SISTEMA.
           05 WS-ANIO               PIC 9(04).
           05 WS-MES                PIC 9(02).
           05 WS-DIA                PIC 9(02).

       01  WS-ANIO-NAC              PIC 9(04).
       01  WS-EDAD-CALCULADA        PIC 9(03).

       01  WS-VALIDACIONES.
           05 WS-DOC-OK             PIC X VALUE 'N'.
           05 WS-NOMBRE-OK          PIC X VALUE 'N'.
           05 WS-APELLIDO-OK        PIC X VALUE 'N'.
           05 WS-EMAIL-OK           PIC X VALUE 'N'.
           05 WS-FECHA-OK           PIC X VALUE 'N'.

       01  WS-EMAIL-REV.
           05 WS-AT-COUNT           PIC 9(02) VALUE 0.
           05 WS-DOT-COUNT          PIC 9(02) VALUE 0.

       01  WS-MOD10.
           05 WS-MOD10-OK           PIC X     VALUE 'N'.
           05 WS-DIGITO             PIC 9(01).
           05 WS-RESULTADO          PIC 9(02).
           05 WS-SUMA-MOD10         PIC 9(04) VALUE 0.
           05 WS-DIGITO-VERIF       PIC 9(01).
           05 WS-DIGITO-CALC        PIC 9(01).
           05 WS-K                  PIC 9(02).
           05 WS-PROV-DOC           PIC 9(02).

       LINKAGE SECTION.
           COPY CUSMREC.
           COPY LKCIF.

       SCREEN SECTION.
       01  SCR-MARCO.
           05 BLANK SCREEN.
           05 LINE 02 COL 02 VALUE
              "---------------------------------------------------".
           05 LINE 03 COL 02 VALUE
              "        BANCO LAF - MODULO DE CLIENTES".
           05 LINE 04 COL 02 VALUE
              "---------------------------------------------------".

       01  SCR-MENU-CIF.
           05 LINE 07 COL 05 VALUE "1. Alta de cliente".
           05 LINE 08 COL 05 VALUE "2. Consulta de cliente".
           05 LINE 09 COL 05 VALUE "3. Modificar contacto".
           05 LINE 10 COL 05 VALUE "4. Baja logica".
           05 LINE 11 COL 05 VALUE "5. Volver".
           05 LINE 14 COL 05 VALUE "Opcion: [ ]".
           05 SCR-CIF-OPC LINE 14 COL 14 PIC 9
              USING WS-OPCION-CIF REQUIRED.

       PROCEDURE DIVISION USING REG-CUSM,
                                LK-DATOS-SESION,
                                LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 'S' TO WS-CONTINUAR-CIF.
           PERFORM 1000-MENU-CIF
              UNTIL WS-CONTINUAR-CIF = 'N'.
           GOBACK.

       1000-MENU-CIF.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-MENU-CIF.
           ACCEPT SCR-CIF-OPC.

           EVALUATE WS-OPCION-CIF
              WHEN 1
                 PERFORM 2000-ALTA-CLIENTE
              WHEN 2
                 PERFORM 3000-CONSULTA-CLIENTE
              WHEN 3
                 PERFORM 4000-MODIFICA-CLIENTE
              WHEN 4
                 PERFORM 5000-BAJA-CLIENTE
              WHEN 5
                 MOVE 'N' TO WS-CONTINUAR-CIF
              WHEN OTHER
                 DISPLAY "Opcion no valida." LINE 17 COL 05
                 ACCEPT WS-CONFIRMAR LINE 17 COL 25
           END-EVALUATE.

      ******************************************************************
      * ALTA DE CLIENTE
      ******************************************************************
       2000-ALTA-CLIENTE.
           DISPLAY SCR-MARCO.
           INITIALIZE REG-CUSM.

           PERFORM 9100-CAPTURAR-DOCUMENTO.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = '00  '
              DISPLAY "Este cliente ya existe. Use consulta o"
                 LINE 16 COL 05
              DISPLAY "modificacion para actualizar datos."
                 LINE 17 COL 05
              DISPLAY "Presione ENTER para continuar."
                 LINE 24 COL 05
              ACCEPT WS-CONFIRMAR LINE 24 COL 36
              EXIT PARAGRAPH
           END-IF.

           PERFORM 9200-CAPTURAR-DATOS-ALTA.

           MOVE 'A' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = '00  '
              MOVE 'C' TO WS-ACCION-TRANS
              CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
              DISPLAY "CLIENTE CREADO EXITOSAMENTE."
                 LINE 22 COL 05
              DISPLAY "ID asignado: " LINE 23 COL 05
              DISPLAY CUSM-ID-CLIENTE LINE 23 COL 20
           ELSE
              MOVE 'R' TO WS-ACCION-TRANS
              CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
              DISPLAY "ERROR: No se pudo crear el cliente."
                 LINE 22 COL 05
              DISPLAY LK-MENSAJE LINE 23 COL 05
           END-IF.

           DISPLAY "Presione ENTER para continuar."
              LINE 24 COL 05.
           ACCEPT WS-CONFIRMAR LINE 24 COL 36.

      ******************************************************************
      * CONSULTA DE CLIENTE
      ******************************************************************
       3000-CONSULTA-CLIENTE.
           DISPLAY SCR-MARCO.
           INITIALIZE REG-CUSM.

           PERFORM 9100-CAPTURAR-DOCUMENTO.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = '00  '
              DISPLAY "ID        : " LINE 14 COL 05
              DISPLAY CUSM-ID-CLIENTE LINE 14 COL 18
              DISPLAY "Tipo/Doc  : " LINE 15 COL 05
              DISPLAY CUSM-TIPO-DOC LINE 15 COL 18
              DISPLAY " / " LINE 15 COL 22
              DISPLAY CUSM-DOC-CLIENTE LINE 15 COL 26
              DISPLAY "Nombre    : " LINE 16 COL 05
              DISPLAY CUSM-NOMBRE-CLIENTE LINE 16 COL 18
              DISPLAY "Apellidos : " LINE 17 COL 05
              DISPLAY CUSM-APELLIDOS-CLIENTE LINE 17 COL 18
              DISPLAY "F.Alta    : " LINE 18 COL 05
              DISPLAY CUSM-FECHA-ALTA LINE 18 COL 18
              DISPLAY "F.Nacim.  : " LINE 18 COL 33
              DISPLAY CUSM-FECHA-NACIMIENTO LINE 18 COL 46
              DISPLAY "Email     : " LINE 19 COL 05
              DISPLAY CUSM-EMAIL-CLIENTE LINE 19 COL 18
              DISPLAY "Telefono  : " LINE 20 COL 05
              DISPLAY CUSM-TELEF-CLIENTE LINE 20 COL 18
              DISPLAY "Direccion : " LINE 21 COL 05
              DISPLAY CUSM-DIRECCION-CLIENTE LINE 21 COL 18
              DISPLAY "Estado    : " LINE 22 COL 05
              DISPLAY CUSM-ESTADO-CLIENTE LINE 22 COL 18
              DISPLAY "Score     : " LINE 22 COL 25
              DISPLAY CUSM-SCORE-CREDITICIO LINE 22 COL 38
           ELSE
              DISPLAY "Cliente no encontrado."
                 LINE 16 COL 05
           END-IF.

           DISPLAY "Presione ENTER para continuar."
              LINE 24 COL 05.
           ACCEPT WS-CONFIRMAR LINE 24 COL 36.

      ******************************************************************
      * MODIFICAR CONTACTO
      ******************************************************************
       4000-MODIFICA-CLIENTE.
           DISPLAY SCR-MARCO.
           INITIALIZE REG-CUSM.

           PERFORM 9100-CAPTURAR-DOCUMENTO.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO NOT = '00  '
              DISPLAY "Cliente no encontrado."
                 LINE 16 COL 05
              DISPLAY "Presione ENTER para continuar."
                 LINE 24 COL 05
              ACCEPT WS-CONFIRMAR LINE 24 COL 36
              EXIT PARAGRAPH
           END-IF.

           DISPLAY "Cliente : " LINE 14 COL 05.
           DISPLAY CUSM-NOMBRE-CLIENTE LINE 14 COL 16.
           DISPLAY CUSM-APELLIDOS-CLIENTE LINE 14 COL 42.

           DISPLAY "Nueva direccion : " LINE 16 COL 05.
           ACCEPT CUSM-DIRECCION-CLIENTE LINE 16 COL 24.

           DISPLAY "Nuevo telefono  : " LINE 17 COL 05.
           ACCEPT CUSM-TELEF-CLIENTE LINE 17 COL 24.

           DISPLAY "Nuevo email     : " LINE 18 COL 05.
           ACCEPT CUSM-EMAIL-CLIENTE LINE 18 COL 24.

           MOVE 'M' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = '00  '
              MOVE 'C' TO WS-ACCION-TRANS
              CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
              DISPLAY "Cliente actualizado correctamente."
                 LINE 22 COL 05
           ELSE
              MOVE 'R' TO WS-ACCION-TRANS
              CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
              DISPLAY "ERROR: No se pudo actualizar."
                 LINE 22 COL 05
              DISPLAY LK-MENSAJE LINE 23 COL 05
           END-IF.

           DISPLAY "Presione ENTER para continuar."
              LINE 24 COL 05.
           ACCEPT WS-CONFIRMAR LINE 24 COL 36.

      ******************************************************************
      * BAJA LOGICA
      ******************************************************************
       5000-BAJA-CLIENTE.
           DISPLAY SCR-MARCO.
           INITIALIZE REG-CUSM.

           PERFORM 9100-CAPTURAR-DOCUMENTO.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO NOT = '00  '
              DISPLAY "Cliente no encontrado."
                 LINE 16 COL 05
              DISPLAY "Presione ENTER para continuar."
                 LINE 24 COL 05
              ACCEPT WS-CONFIRMAR LINE 24 COL 36
              EXIT PARAGRAPH
           END-IF.

           DISPLAY "Cliente : " LINE 14 COL 05.
           DISPLAY CUSM-NOMBRE-CLIENTE LINE 14 COL 16.
           DISPLAY CUSM-APELLIDOS-CLIENTE LINE 14 COL 42.
           DISPLAY "Estado actual: " LINE 15 COL 05.
           DISPLAY CUSM-ESTADO-CLIENTE LINE 15 COL 22.

           DISPLAY "Confirma baja logica? (S/N):"
              LINE 17 COL 05.
           ACCEPT WS-CONFIRMAR LINE 17 COL 35.

           IF WS-CONFIRMAR = 'S' OR WS-CONFIRMAR = 's'

              MOVE 'B' TO LK-ACCION-DB
              CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                         LK-DATOS-SESION,
                                         LK-DATOS-TRANSACCION

              IF LK-COD-RETORNO = '00  '
                 MOVE 'C' TO WS-ACCION-TRANS
                 CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
                 DISPLAY "Cliente inactivado correctamente."
                    LINE 20 COL 05
              ELSE
                 MOVE 'R' TO WS-ACCION-TRANS
                 CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
                 DISPLAY "ERROR: No se pudo inactivar."
                    LINE 20 COL 05
                 DISPLAY LK-MENSAJE LINE 21 COL 05
              END-IF
           ELSE
              DISPLAY "Operacion cancelada."
                 LINE 20 COL 05
           END-IF.

           DISPLAY "Presione ENTER para continuar."
              LINE 24 COL 05.
           ACCEPT WS-CONFIRMAR LINE 24 COL 36.

      ******************************************************************
      * CAPTURA Y VALIDACION DEL DOCUMENTO
      ******************************************************************
       9100-CAPTURAR-DOCUMENTO.
           MOVE 'N' TO WS-DOC-OK.
           DISPLAY WS-BLANK-LINE LINE 13 COL 02.

           PERFORM UNTIL WS-DOC-OK = 'S'
              DISPLAY "Documento (cedula 10 dig / pasaporte 6-12):"
                 LINE 12 COL 05
              ACCEPT CUSM-DOC-CLIENTE LINE 12 COL 51

              MOVE 12 TO WS-I
              PERFORM UNTIL WS-I = 0
                         OR CUSM-DOC-CLIENTE(WS-I:1) NOT = SPACE
                 SUBTRACT 1 FROM WS-I
              END-PERFORM
              MOVE WS-I TO WS-DOC-LEN

              EVALUATE TRUE
                 WHEN WS-DOC-LEN = 10
                    IF CUSM-DOC-CLIENTE(1:10) IS NUMERIC
                       PERFORM 9400-VALIDAR-MODULO-10
                       IF WS-MOD10-OK = 'S'
                          MOVE 'CED' TO CUSM-TIPO-DOC
                          MOVE 'S' TO WS-DOC-OK
                          DISPLAY WS-BLANK-LINE LINE 13 COL 02
                       ELSE
                          DISPLAY "Cedula invalida (falla Modulo 10)."
                             LINE 13 COL 05
                       END-IF
                    ELSE
                       MOVE 'PAS' TO CUSM-TIPO-DOC
                       MOVE 'S' TO WS-DOC-OK
                       DISPLAY WS-BLANK-LINE LINE 13 COL 02
                    END-IF
                 WHEN WS-DOC-LEN >= 6 AND WS-DOC-LEN <= 12
                    MOVE 'PAS' TO CUSM-TIPO-DOC
                    MOVE 'S' TO WS-DOC-OK
                    DISPLAY WS-BLANK-LINE LINE 13 COL 02
                 WHEN OTHER
                    DISPLAY "Invalido: CED=10 dig, PAS=6-12 dig."
                       LINE 13 COL 05
              END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * CAPTURA DE DATOS PARA ALTA (CON VALIDACIONES)
      ******************************************************************
       9200-CAPTURAR-DATOS-ALTA.
           MOVE 'N' TO WS-NOMBRE-OK.
           MOVE 'N' TO WS-APELLIDO-OK.
           MOVE 'N' TO WS-EMAIL-OK.
           MOVE 'N' TO WS-FECHA-OK.
           ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD.

           PERFORM UNTIL WS-NOMBRE-OK = 'S'
              DISPLAY "Nombres   :" LINE 14 COL 05
              ACCEPT CUSM-NOMBRE-CLIENTE LINE 14 COL 18
              IF CUSM-NOMBRE-CLIENTE NOT = SPACES
                 MOVE 'S' TO WS-NOMBRE-OK
                 DISPLAY WS-BLANK-LINE LINE 22 COL 02
              ELSE
                 DISPLAY "Nombres no puede estar vacio."
                    LINE 22 COL 05
              END-IF
           END-PERFORM.

           PERFORM UNTIL WS-APELLIDO-OK = 'S'
              DISPLAY "Apellidos :" LINE 15 COL 05
              ACCEPT CUSM-APELLIDOS-CLIENTE LINE 15 COL 18
              IF CUSM-APELLIDOS-CLIENTE NOT = SPACES
                 MOVE 'S' TO WS-APELLIDO-OK
                 DISPLAY WS-BLANK-LINE LINE 22 COL 02
              ELSE
                 DISPLAY "Apellidos no puede estar vacio."
                    LINE 22 COL 05
              END-IF
           END-PERFORM.

           PERFORM 9250-VALIDAR-FECHA-NACIMIENTO.

           DISPLAY "Direccion :" LINE 17 COL 05.
           ACCEPT CUSM-DIRECCION-CLIENTE LINE 17 COL 18.

           DISPLAY "Telefono  :" LINE 18 COL 05.
           ACCEPT CUSM-TELEF-CLIENTE LINE 18 COL 18.

           PERFORM UNTIL WS-EMAIL-OK = 'S'
              DISPLAY "Email     :" LINE 19 COL 05
              ACCEPT CUSM-EMAIL-CLIENTE LINE 19 COL 18
              PERFORM 9300-VALIDAR-EMAIL
           END-PERFORM.
           DISPLAY WS-BLANK-LINE LINE 22 COL 02.

           DISPLAY "Ingresos mensuales:" LINE 20 COL 05.
           ACCEPT WS-INGRESO-TEXTO LINE 20 COL 26.
           COMPUTE CUSM-INGRESOS-MENSUALES =
              FUNCTION NUMVAL(WS-INGRESO-TEXTO).

           STRING WS-ANIO "-" WS-MES "-" WS-DIA
              DELIMITED BY SIZE INTO CUSM-FECHA-ALTA.

           MOVE WS-SCORE          TO CUSM-SCORE-CREDITICIO.
           MOVE 0                 TO CUSM-TIENE-TARJETA.
           MOVE 0                 TO CUSM-TIENE-HIPOTECA.
           MOVE 0                 TO CUSM-SALDO-TOTAL-VISTA.
           MOVE 'A'               TO CUSM-ESTADO-CLIENTE.

      ******************************************************************
      * VALIDACION FECHA NACIMIENTO Y EDAD MINIMA 18 ANIOS
      ******************************************************************
       9250-VALIDAR-FECHA-NACIMIENTO.
           MOVE 'N' TO WS-FECHA-OK.

           PERFORM UNTIL WS-FECHA-OK = 'S'
              DISPLAY "F.Nacim.  :" LINE 16 COL 05
              DISPLAY "(AAAA-MM-DD)" LINE 16 COL 44
              ACCEPT CUSM-FECHA-NACIMIENTO LINE 16 COL 18

              IF CUSM-FECHA-NACIMIENTO(5:1) NOT = '-'
                 OR CUSM-FECHA-NACIMIENTO(8:1) NOT = '-'
                 DISPLAY "Formato invalido. Ingrese AAAA-MM-DD."
                    LINE 22 COL 05
              ELSE
                 MOVE CUSM-FECHA-NACIMIENTO(1:4) TO WS-ANIO-NAC
                 COMPUTE WS-EDAD-CALCULADA =
                    WS-ANIO - WS-ANIO-NAC
                 IF WS-EDAD-CALCULADA < 18
                    DISPLAY "El cliente debe ser mayor de 18 anios."
                       LINE 22 COL 05
                 ELSE IF WS-EDAD-CALCULADA > 120
                    DISPLAY "Fecha de nacimiento no valida."
                       LINE 22 COL 05
                 ELSE
                    DISPLAY WS-BLANK-LINE LINE 22 COL 02
                    MOVE 'S' TO WS-FECHA-OK
                 END-IF
              END-IF
           END-PERFORM.

      ******************************************************************
      * VALIDACION DE EMAIL
      ******************************************************************
       9300-VALIDAR-EMAIL.
           MOVE 0 TO WS-AT-COUNT.
           MOVE 0 TO WS-DOT-COUNT.

           INSPECT CUSM-EMAIL-CLIENTE
              TALLYING WS-AT-COUNT FOR ALL '@'.
           INSPECT CUSM-EMAIL-CLIENTE
              TALLYING WS-DOT-COUNT FOR ALL '.'.

           IF WS-AT-COUNT = 1 AND WS-DOT-COUNT >= 1
              MOVE 'S' TO WS-EMAIL-OK
              DISPLAY WS-BLANK-LINE LINE 22 COL 02
           ELSE
              DISPLAY "Email invalido (ej: usuario@dominio.com)."
                 LINE 22 COL 05
           END-IF.

      ******************************************************************
      * MODULO 10 - VALIDACION CEDULA ECUATORIANA (SRI)
      ******************************************************************
       9400-VALIDAR-MODULO-10.
           MOVE 'N' TO WS-MOD10-OK.
           MOVE 0 TO WS-SUMA-MOD10.
           MOVE CUSM-DOC-CLIENTE(1:2) TO WS-PROV-DOC.

           IF WS-PROV-DOC < 1 OR WS-PROV-DOC > 24
              EXIT PARAGRAPH
           END-IF.

           PERFORM VARYING WS-K FROM 1 BY 1
                          UNTIL WS-K > 9
              MOVE CUSM-DOC-CLIENTE(WS-K:1) TO WS-DIGITO
              IF FUNCTION MOD(WS-K, 2) = 1
                 COMPUTE WS-RESULTADO = WS-DIGITO * 2
                 IF WS-RESULTADO >= 10
                    SUBTRACT 9 FROM WS-RESULTADO
                 END-IF
              ELSE
                 MOVE WS-DIGITO TO WS-RESULTADO
              END-IF
              ADD WS-RESULTADO TO WS-SUMA-MOD10
           END-PERFORM.

           COMPUTE WS-DIGITO-CALC =
              FUNCTION MOD(WS-SUMA-MOD10, 10).
           IF WS-DIGITO-CALC NOT = 0
              COMPUTE WS-DIGITO-CALC = 10 - WS-DIGITO-CALC
           END-IF.
           MOVE CUSM-DOC-CLIENTE(10:1) TO WS-DIGITO-VERIF.
           IF WS-DIGITO-CALC = WS-DIGITO-VERIF
              MOVE 'S' TO WS-MOD10-OK
           END-IF.

       END PROGRAM CI0000.

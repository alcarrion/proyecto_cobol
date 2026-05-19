      ******************************************************************
      * CI0000.CBL - MODULO DE CLIENTES
      * v3.2 - Layout con caja + cancelar con 'X' o vacio
      * + cuenta Ahorros automatica + caja de resultado
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
       01  WS-BLANK-LINE            PIC X(75) VALUE SPACES.
       01  WS-MSG-LINE              PIC X(65) VALUE SPACES.

       01  WS-PGM-DBIOCUSM          PIC X(08) VALUE 'DBIOCUSM'.
       01  WS-PGM-DBIOINVM          PIC X(08) VALUE 'DBIOINVM'.
       01  WS-PGM-DBIOTRAN          PIC X(08) VALUE 'DBIOTRAN'.
       01  WS-ACCION-TRANS          PIC X(01).

       01  WS-FECHA-SISTEMA.
           05 WS-ANIO               PIC 9(04).
           05 WS-MES                PIC 9(02).
           05 WS-DIA                PIC 9(02).

       01  WS-ANIO-NAC              PIC 9(04).
       01  WS-EDAD-CALCULADA        PIC 9(03).

      *> Buffers de entrada
       01  WS-TELEF-IN              PIC X(10).
       01  WS-INVM-ID-FMT           PIC ZZZZZZZ9.
       01  WS-CLI-ID-FMT            PIC ZZZZZZZ9.

       01  WS-VALIDACIONES.
           05 WS-DOC-OK             PIC X VALUE 'N'.
           05 WS-NOMBRE-OK          PIC X VALUE 'N'.
           05 WS-APELLIDO-OK        PIC X VALUE 'N'.
           05 WS-EMAIL-OK           PIC X VALUE 'N'.
           05 WS-FECHA-OK           PIC X VALUE 'N'.
           05 WS-TELEF-OK           PIC X VALUE 'N'.
           05 WS-DIR-OK             PIC X VALUE 'N'.
           05 WS-CANCELO            PIC X VALUE 'N'.

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

      *> Registro local de cuenta Ahorros (01 nivel independiente)
       COPY INVMREC.

       LINKAGE SECTION.
           COPY CUSMREC.
           COPY LKCIF.

       SCREEN SECTION.
      *> Colores: 0=Negro 1=Azul 2=Verde 3=Cyan
      *>          4=Rojo 5=Magenta 6=Amarillo 7=Blanco

      *> ----- Cabecera reutilizable: barra de titulo + operador -----
       01  SCR-MARCO.
           05 BLANK SCREEN BACKGROUND-COLOR 0.
           05 LINE 01 COL 01 BACKGROUND-COLOR 1 FOREGROUND-COLOR 7
              HIGHLIGHT VALUE 
              "       BANCO LAF - MODULO DE CLIENTES v3.2      ".
           05 LINE 02 COL 02 FOREGROUND-COLOR 3 VALUE "Operador: ".
           05 LINE 02 COL 12 PIC X(08) FROM LKCIF-USUARIO
              FOREGROUND-COLOR 7 HIGHLIGHT.
           05 LINE 02 COL 35 FOREGROUND-COLOR 3 VALUE "Terminal: ".
           05 LINE 02 COL 45 PIC X(04) FROM LKCIF-TERMINAL
              FOREGROUND-COLOR 7 HIGHLIGHT.
           05 LINE 03 COL 02 FOREGROUND-COLOR 3 VALUE
              "=================================================".

      *> ----- Menu de clientes -----
       01  SCR-MENU-CIF.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------- [ MODULO DE CLIENTES ] -----------+".
           05 LINE 06 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 06 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[1]".
           05 LINE 07 COL 09 FOREGROUND-COLOR 7 VALUE
              "Alta de cliente (abre cta Ahorros)".
           05 LINE 07 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[2]".
           05 LINE 08 COL 09 FOREGROUND-COLOR 7 VALUE
              "Consulta de cliente".
           05 LINE 08 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[3]".
           05 LINE 09 COL 09 FOREGROUND-COLOR 7 VALUE
              "Modificar contacto".
           05 LINE 09 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[4]".
           05 LINE 10 COL 09 FOREGROUND-COLOR 7 VALUE "Baja logica".
           05 LINE 10 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 05 FOREGROUND-COLOR 6 HIGHLIGHT VALUE "[5]".
           05 LINE 12 COL 09 FOREGROUND-COLOR 3 VALUE
              "Volver al menu principal".
           05 LINE 12 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".
           05 LINE 15 COL 02 FOREGROUND-COLOR 3 VALUE "Opcion: [ ]".
           05 SCR-CIF-OPC LINE 15 COL 11 PIC 9
              USING WS-OPCION-CIF REQUIRED
              FOREGROUND-COLOR 7 HIGHLIGHT.

      *> ----- Caja del formulario de alta de cliente -----
       01  SCR-ALTA-FORM.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+--------- [ REGISTRO DE NUEVO CLIENTE ] ---------+".
           05 LINE 06 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 06 COL 53 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 04 FOREGROUND-COLOR 6 VALUE 
              "Documento (X=salir):".
           05 LINE 07 COL 53 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 04 FOREGROUND-COLOR 7 VALUE 
              "Nombres            :".
           05 LINE 08 COL 53 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 04 FOREGROUND-COLOR 7 VALUE 
              "Apellidos          :".
           05 LINE 09 COL 53 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 10 COL 04 FOREGROUND-COLOR 7 VALUE 
              "F.Nacim AAAA-MM-DD :".
           05 LINE 10 COL 53 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 11 COL 04 FOREGROUND-COLOR 7 VALUE 
              "Direccion          :".
           05 LINE 11 COL 53 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 12 COL 04 FOREGROUND-COLOR 7 VALUE 
              "Telefono (10 dig)  :".
           05 LINE 12 COL 53 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 13 COL 04 FOREGROUND-COLOR 7 VALUE 
              "Email              :".
           05 LINE 13 COL 53 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 14 COL 04 FOREGROUND-COLOR 7 VALUE 
              "Ingresos mensuales :".
           05 LINE 14 COL 53 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 15 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 15 COL 53 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 16 COL 02 FOREGROUND-COLOR 3 VALUE
              "+-------------------------------------------------+".
           05 LINE 18 COL 02 FOREGROUND-COLOR 5 VALUE
              "[i] Tip: escriba 'X' en cualquier campo = cancelar".

      *> ----- Caja para consulta/baja/modif (un solo dato) -----
       01  SCR-DOC-BOX.
           05 LINE 05 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------- [ BUSCAR CLIENTE ] ---------------+".
           05 LINE 06 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 06 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 07 COL 04 FOREGROUND-COLOR 6 VALUE 
              "Documento (X=salir):".
           05 LINE 07 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 02 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 08 COL 49 FOREGROUND-COLOR 3 VALUE "|".
           05 LINE 09 COL 02 FOREGROUND-COLOR 3 VALUE
              "+----------------------------------------------+".

       PROCEDURE DIVISION USING REG-CUSM,
                                LK-DATOS-SESION,
                                LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 'S' TO WS-CONTINUAR-CIF.
           PERFORM 1000-MENU-CIF
              UNTIL WS-CONTINUAR-CIF = 'N'.
           GOBACK.

       1000-MENU-CIF.
           MOVE 0 TO WS-OPCION-CIF.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-MENU-CIF.
           ACCEPT SCR-CIF-OPC.

           EVALUATE WS-OPCION-CIF
              WHEN 1 PERFORM 2000-ALTA-CLIENTE
              WHEN 2 PERFORM 3000-CONSULTA-CLIENTE
              WHEN 3 PERFORM 4000-MODIFICA-CLIENTE
              WHEN 4 PERFORM 5000-BAJA-CLIENTE
              WHEN 5 MOVE 'N' TO WS-CONTINUAR-CIF
              WHEN OTHER
                 DISPLAY "Opcion no valida (1-4 o 5)."
                    LINE 17 COL 02
                 DISPLAY "Presione ENTER..." LINE 18 COL 02
                 ACCEPT WS-CONFIRMAR LINE 18 COL 20
           END-EVALUATE.

      ******************************************************************
      * ALTA DE CLIENTE  (al final abre cuenta Ahorros automatica)
      ******************************************************************
       2000-ALTA-CLIENTE.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-ALTA-FORM.
           INITIALIZE REG-CUSM.
           MOVE 'N' TO WS-CANCELO.
           MOVE SPACES TO WS-MSG-LINE.

           PERFORM 9100-CAPTURAR-DOCUMENTO.
           IF WS-CANCELO = 'S'
              PERFORM 9900-AVISO-CANCELO
              EXIT PARAGRAPH
           END-IF.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = '00  '
              DISPLAY "[!] Este cliente ya existe."
                 LINE 20 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
              DISPLAY "    Use Consulta o Modificacion."
                 LINE 21 COL 02 FOREGROUND-COLOR 4
              DISPLAY "Presione ENTER para volver al menu..."
                 LINE 23 COL 02
              ACCEPT WS-CONFIRMAR LINE 23 COL 41
              EXIT PARAGRAPH
           END-IF.

           PERFORM 9200-CAPTURAR-DATOS-ALTA.
           IF WS-CANCELO = 'S'
              PERFORM 9900-AVISO-CANCELO
              EXIT PARAGRAPH
           END-IF.

           MOVE 'A' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = '00  '
              MOVE 'C' TO WS-ACCION-TRANS
              CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
              PERFORM 2100-CREAR-CUENTA-AHORROS
              PERFORM 2200-MOSTRAR-RESULTADO-ALTA
           ELSE
              MOVE 'R' TO WS-ACCION-TRANS
              CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
              DISPLAY "[X] ERROR al crear cliente."
                 LINE 20 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
              DISPLAY "    " LK-MENSAJE LINE 21 COL 02
                 FOREGROUND-COLOR 4
           END-IF.

           DISPLAY "Presione ENTER para volver al menu..."
              LINE 23 COL 02.
           ACCEPT WS-CONFIRMAR LINE 23 COL 41.

      ******************************************************************
      * 2100 - ALTA AUTOMATICA DE CUENTA DE AHORROS (vacio si falla)
      ******************************************************************
       2100-CREAR-CUENTA-AHORROS.
           INITIALIZE REG-INVM.
           MOVE CUSM-ID-CLIENTE TO INVM-ID-CLIENTE.
           MOVE 'A'             TO INVM-TIPO-CUENTA.
           MOVE ZEROS           TO INVM-SALDO-ACTUAL.
           MOVE 'A'             TO INVM-ESTADO-CUENTA.

           MOVE 'A' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
              MOVE 'C' TO WS-ACCION-TRANS
              CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
           ELSE
              MOVE 'R' TO WS-ACCION-TRANS
              CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
              MOVE ZEROS TO INVM-ID-CUENTA
           END-IF.

      ******************************************************************
      * 2200 - CAJA DE RESULTADO (cliente + cuenta de Ahorros)
      ******************************************************************
       2200-MOSTRAR-RESULTADO-ALTA.
           MOVE CUSM-ID-CLIENTE TO WS-CLI-ID-FMT.
           MOVE INVM-ID-CUENTA  TO WS-INVM-ID-FMT.

           DISPLAY "+----------- [ ALTA EXITOSA ] -------------+"
              LINE 18 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 19 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 19 COL 45 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 20 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "Cliente ID  :" LINE 20 COL 04
              FOREGROUND-COLOR 7.
           DISPLAY WS-CLI-ID-FMT LINE 20 COL 18
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "|" LINE 20 COL 45 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "|" LINE 21 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.

           IF INVM-ID-CUENTA > 0
              DISPLAY "Cta Ahorros :" LINE 21 COL 04
                 FOREGROUND-COLOR 7
              DISPLAY WS-INVM-ID-FMT LINE 21 COL 18
                 FOREGROUND-COLOR 6 HIGHLIGHT
              DISPLAY "(saldo $0.00)" LINE 21 COL 28
                 FOREGROUND-COLOR 7
           ELSE
              DISPLAY "Cta Ahorros : (no se pudo crear)"
                 LINE 21 COL 04 FOREGROUND-COLOR 4
           END-IF.

           DISPLAY "|" LINE 21 COL 45 FOREGROUND-COLOR 2 HIGHLIGHT.
           DISPLAY "+------------------------------------------+"
              LINE 22 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT.

      ******************************************************************
      * 9900 - Aviso de cancelacion
      ******************************************************************
       9900-AVISO-CANCELO.
           DISPLAY "[i] Operacion cancelada por el usuario."
              LINE 20 COL 02 FOREGROUND-COLOR 5 HIGHLIGHT.
           DISPLAY "Presione ENTER para volver al menu..."
              LINE 23 COL 02.
           ACCEPT WS-CONFIRMAR LINE 23 COL 41.

      ******************************************************************
      * CONSULTA DE CLIENTE
      ******************************************************************
       3000-CONSULTA-CLIENTE.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-DOC-BOX.
           INITIALIZE REG-CUSM.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-CAPTURAR-DOCUMENTO.
           IF WS-CANCELO = 'S'
              PERFORM 9900-AVISO-CANCELO
              EXIT PARAGRAPH
           END-IF.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO NOT = '00  '
              DISPLAY "[X] Cliente no encontrado."
                 LINE 11 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
              DISPLAY "Presione ENTER..."
                 LINE 23 COL 02
              ACCEPT WS-CONFIRMAR LINE 23 COL 20
              EXIT PARAGRAPH
           END-IF.

           DISPLAY "+--------- [ DATOS DEL CLIENTE ] ---------+"
              LINE 11 COL 02 FOREGROUND-COLOR 3.
           DISPLAY "ID        :" LINE 12 COL 04 FOREGROUND-COLOR 7.
           DISPLAY CUSM-ID-CLIENTE LINE 12 COL 18
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "Doc       :" LINE 13 COL 04 FOREGROUND-COLOR 7.
           DISPLAY CUSM-TIPO-DOC LINE 13 COL 18 FOREGROUND-COLOR 7.
           DISPLAY CUSM-DOC-CLIENTE LINE 13 COL 22 FOREGROUND-COLOR 7.
           DISPLAY "Nombre    :" LINE 14 COL 04 FOREGROUND-COLOR 7.
           DISPLAY CUSM-NOMBRE-CLIENTE LINE 14 COL 18
              FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY "Apellidos :" LINE 15 COL 04 FOREGROUND-COLOR 7.
           DISPLAY CUSM-APELLIDOS-CLIENTE LINE 15 COL 18
              FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY "F.Nacim   :" LINE 16 COL 04 FOREGROUND-COLOR 7.
           DISPLAY CUSM-FECHA-NACIMIENTO LINE 16 COL 18
              FOREGROUND-COLOR 7.
           DISPLAY "Email     :" LINE 17 COL 04 FOREGROUND-COLOR 7.
           DISPLAY CUSM-EMAIL-CLIENTE LINE 17 COL 18
              FOREGROUND-COLOR 7.
           DISPLAY "Telefono  :" LINE 18 COL 04 FOREGROUND-COLOR 7.
           DISPLAY CUSM-TELEF-CLIENTE LINE 18 COL 18
              FOREGROUND-COLOR 7.
           DISPLAY "Direccion :" LINE 19 COL 04 FOREGROUND-COLOR 7.
           DISPLAY CUSM-DIRECCION-CLIENTE LINE 19 COL 18
              FOREGROUND-COLOR 7.
           DISPLAY "Estado    :" LINE 20 COL 04 FOREGROUND-COLOR 7.
           DISPLAY CUSM-ESTADO-CLIENTE LINE 20 COL 18
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "Score     :" LINE 20 COL 22 FOREGROUND-COLOR 7.
           DISPLAY CUSM-SCORE-CREDITICIO LINE 20 COL 34
              FOREGROUND-COLOR 6 HIGHLIGHT.
           DISPLAY "+-----------------------------------------+"
              LINE 21 COL 02 FOREGROUND-COLOR 3.

           DISPLAY "Presione ENTER para volver al menu..."
              LINE 23 COL 02.
           ACCEPT WS-CONFIRMAR LINE 23 COL 41.

      ******************************************************************
      * MODIFICAR CONTACTO
      ******************************************************************
       4000-MODIFICA-CLIENTE.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-DOC-BOX.
           INITIALIZE REG-CUSM.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-CAPTURAR-DOCUMENTO.
           IF WS-CANCELO = 'S'
              PERFORM 9900-AVISO-CANCELO
              EXIT PARAGRAPH
           END-IF.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO NOT = '00  '
              DISPLAY "[X] Cliente no encontrado."
                 LINE 11 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
              DISPLAY "Presione ENTER..." LINE 23 COL 02
              ACCEPT WS-CONFIRMAR LINE 23 COL 20
              EXIT PARAGRAPH
           END-IF.

           DISPLAY "Cliente: " LINE 11 COL 02 FOREGROUND-COLOR 3.
           DISPLAY CUSM-NOMBRE-CLIENTE LINE 11 COL 12
              FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY CUSM-APELLIDOS-CLIENTE LINE 11 COL 38
              FOREGROUND-COLOR 7 HIGHLIGHT.

           DISPLAY "Nueva direccion        :" LINE 13 COL 02
              FOREGROUND-COLOR 7.
           ACCEPT CUSM-DIRECCION-CLIENTE LINE 13 COL 28.

           DISPLAY "Nuevo telef.(10 dig)   :" LINE 14 COL 02
              FOREGROUND-COLOR 7.
           MOVE SPACES TO WS-TELEF-IN.
           ACCEPT WS-TELEF-IN LINE 14 COL 28.
           IF WS-TELEF-IN NOT = SPACES
              IF WS-TELEF-IN IS NUMERIC
                 MOVE WS-TELEF-IN TO CUSM-TELEF-CLIENTE
              ELSE
                 DISPLAY "[!] Telefono invalido. Se conserva anterior." 
                    LINE 20 COL 02 FOREGROUND-COLOR 4
              END-IF
           END-IF.

           DISPLAY "Nuevo email            :" LINE 15 COL 02
              FOREGROUND-COLOR 7.
           ACCEPT CUSM-EMAIL-CLIENTE LINE 15 COL 28.

           MOVE 'M' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO = '00  '
              MOVE 'C' TO WS-ACCION-TRANS
              CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
              DISPLAY "[OK] Cliente actualizado correctamente."
                 LINE 21 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT
           ELSE
              MOVE 'R' TO WS-ACCION-TRANS
              CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
              DISPLAY "[X] ERROR: No se pudo actualizar."
                 LINE 21 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
              DISPLAY LK-MENSAJE LINE 22 COL 02 FOREGROUND-COLOR 4
           END-IF.

           DISPLAY "Presione ENTER..." LINE 23 COL 02.
           ACCEPT WS-CONFIRMAR LINE 23 COL 20.

      ******************************************************************
      * BAJA LOGICA
      ******************************************************************
       5000-BAJA-CLIENTE.
           DISPLAY SCR-MARCO.
           DISPLAY SCR-DOC-BOX.
           INITIALIZE REG-CUSM.
           MOVE 'N' TO WS-CANCELO.

           PERFORM 9100-CAPTURAR-DOCUMENTO.
           IF WS-CANCELO = 'S'
              PERFORM 9900-AVISO-CANCELO
              EXIT PARAGRAPH
           END-IF.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-COD-RETORNO NOT = '00  '
              DISPLAY "[X] Cliente no encontrado."
                 LINE 11 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
              DISPLAY "Presione ENTER..." LINE 23 COL 02
              ACCEPT WS-CONFIRMAR LINE 23 COL 20
              EXIT PARAGRAPH
           END-IF.

           DISPLAY "Cliente: " LINE 11 COL 02 FOREGROUND-COLOR 3.
           DISPLAY CUSM-NOMBRE-CLIENTE LINE 11 COL 12
              FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY CUSM-APELLIDOS-CLIENTE LINE 11 COL 38
              FOREGROUND-COLOR 7 HIGHLIGHT.
           DISPLAY "Estado actual: " LINE 12 COL 02 FOREGROUND-COLOR 7.
           DISPLAY CUSM-ESTADO-CLIENTE LINE 12 COL 18
              FOREGROUND-COLOR 6 HIGHLIGHT.

           DISPLAY "Confirma baja logica? (S/N): " LINE 14 COL 02
              FOREGROUND-COLOR 6 HIGHLIGHT.
           ACCEPT WS-CONFIRMAR LINE 14 COL 32.

           IF WS-CONFIRMAR = 'S' OR WS-CONFIRMAR = 's'
              MOVE 'B' TO LK-ACCION-DB
              CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                         LK-DATOS-SESION,
                                         LK-DATOS-TRANSACCION

              IF LK-COD-RETORNO = '00  '
                 MOVE 'C' TO WS-ACCION-TRANS
                 CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
                 DISPLAY "[OK] Cliente inactivado correctamente."
                    LINE 16 COL 02 FOREGROUND-COLOR 2 HIGHLIGHT
              ELSE
                 MOVE 'R' TO WS-ACCION-TRANS
                 CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
                 DISPLAY "[X] ERROR: No se pudo inactivar."
                    LINE 16 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                 DISPLAY LK-MENSAJE LINE 17 COL 02 FOREGROUND-COLOR 4
              END-IF
           ELSE
              DISPLAY "[i] Operacion cancelada."
                 LINE 16 COL 02 FOREGROUND-COLOR 5
           END-IF.

           DISPLAY "Presione ENTER..." LINE 23 COL 02.
           ACCEPT WS-CONFIRMAR LINE 23 COL 20.

      ******************************************************************
      * 9100 - CAPTURA DEL DOCUMENTO (X o vacio = cancelar)
      ******************************************************************
       9100-CAPTURAR-DOCUMENTO.
           MOVE 'N' TO WS-DOC-OK.

           PERFORM UNTIL WS-DOC-OK NOT = 'N'
              MOVE SPACES TO CUSM-DOC-CLIENTE
              ACCEPT CUSM-DOC-CLIENTE LINE 07 COL 25

              EVALUATE TRUE
                 WHEN CUSM-DOC-CLIENTE = SPACES
                    MOVE 'C' TO WS-DOC-OK
                    MOVE 'S' TO WS-CANCELO
                 WHEN CUSM-DOC-CLIENTE = "X" 
                   OR CUSM-DOC-CLIENTE = "x"
                    MOVE 'C' TO WS-DOC-OK
                    MOVE 'S' TO WS-CANCELO
                 WHEN OTHER
                    PERFORM 9150-CLASIFICAR-DOC
              END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 9150 - Clasifica el documento ya capturado (CED 10 / PAS 6-12)
      ******************************************************************
       9150-CLASIFICAR-DOC.
           MOVE 12 TO WS-I.
           PERFORM UNTIL WS-I = 0
                      OR CUSM-DOC-CLIENTE(WS-I:1) NOT = SPACE
              SUBTRACT 1 FROM WS-I
           END-PERFORM.
           MOVE WS-I TO WS-DOC-LEN.

           EVALUATE TRUE
              WHEN WS-DOC-LEN = 10
                 IF CUSM-DOC-CLIENTE(1:10) IS NUMERIC
                    PERFORM 9400-VALIDAR-MODULO-10
                    IF WS-MOD10-OK = 'S'
                       MOVE 'CED' TO CUSM-TIPO-DOC
                       MOVE 'S' TO WS-DOC-OK
                       DISPLAY WS-BLANK-LINE LINE 19 COL 02
                    ELSE
                       DISPLAY "[!] Cedula invalida (modulo 10)."
                          LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                    END-IF
                 ELSE
                    MOVE 'PAS' TO CUSM-TIPO-DOC
                    MOVE 'S' TO WS-DOC-OK
                    DISPLAY WS-BLANK-LINE LINE 19 COL 02
                 END-IF
              WHEN WS-DOC-LEN >= 6 AND WS-DOC-LEN <= 12
                 MOVE 'PAS' TO CUSM-TIPO-DOC
                 MOVE 'S' TO WS-DOC-OK
                 DISPLAY WS-BLANK-LINE LINE 19 COL 02
              WHEN OTHER
                 DISPLAY "[!] Doc invalido: CED=10, PAS=6-12 digitos."
                    LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
           END-EVALUATE.

      ******************************************************************
      * 9200 - CAPTURA DE DATOS PARA ALTA (CON VALIDACIONES)
      * En cualquier campo: 'X' o vacio = cancela la operacion.
      ******************************************************************
       9200-CAPTURAR-DATOS-ALTA.
           MOVE 'N' TO WS-NOMBRE-OK.
           MOVE 'N' TO WS-APELLIDO-OK.
           MOVE 'N' TO WS-EMAIL-OK.
           MOVE 'N' TO WS-FECHA-OK.
           MOVE 'N' TO WS-TELEF-OK.
           MOVE 'N' TO WS-DIR-OK.
           ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD.

      *> NOMBRES: solo letras y espacios
           PERFORM UNTIL WS-NOMBRE-OK NOT = 'N'
              MOVE SPACES TO CUSM-NOMBRE-CLIENTE
              ACCEPT CUSM-NOMBRE-CLIENTE LINE 08 COL 25
              PERFORM 9210-VAL-NOMBRE
           END-PERFORM.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

      *> APELLIDOS: solo letras y espacios
           PERFORM UNTIL WS-APELLIDO-OK NOT = 'N'
              MOVE SPACES TO CUSM-APELLIDOS-CLIENTE
              ACCEPT CUSM-APELLIDOS-CLIENTE LINE 09 COL 25
              PERFORM 9220-VAL-APELLIDO
           END-PERFORM.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

           PERFORM 9250-VALIDAR-FECHA-NACIMIENTO.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

      *> DIRECCION: no vacia, ni "X" sola
           PERFORM UNTIL WS-DIR-OK NOT = 'N'
              MOVE SPACES TO CUSM-DIRECCION-CLIENTE
              ACCEPT CUSM-DIRECCION-CLIENTE LINE 11 COL 25
              EVALUATE TRUE
                 WHEN CUSM-DIRECCION-CLIENTE = SPACES
                    MOVE 'C' TO WS-DIR-OK
                    MOVE 'S' TO WS-CANCELO
                 WHEN CUSM-DIRECCION-CLIENTE = "X"
                   OR CUSM-DIRECCION-CLIENTE = "x"
                    MOVE 'C' TO WS-DIR-OK
                    MOVE 'S' TO WS-CANCELO
                 WHEN OTHER
                    MOVE 'S' TO WS-DIR-OK
                    DISPLAY WS-BLANK-LINE LINE 19 COL 02
              END-EVALUATE
           END-PERFORM.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

      *> TELEFONO: 10 digitos exactos
           PERFORM UNTIL WS-TELEF-OK NOT = 'N'
              MOVE SPACES TO WS-TELEF-IN
              ACCEPT WS-TELEF-IN LINE 12 COL 25
              PERFORM 9230-VAL-TELEFONO
           END-PERFORM.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.

      *> EMAIL: debe tener @ y .
           PERFORM UNTIL WS-EMAIL-OK NOT = 'N'
              MOVE SPACES TO CUSM-EMAIL-CLIENTE
              ACCEPT CUSM-EMAIL-CLIENTE LINE 13 COL 25
              EVALUATE TRUE
                 WHEN CUSM-EMAIL-CLIENTE = SPACES
                    MOVE 'C' TO WS-EMAIL-OK
                    MOVE 'S' TO WS-CANCELO
                 WHEN CUSM-EMAIL-CLIENTE = "X" 
                   OR CUSM-EMAIL-CLIENTE = "x"
                    MOVE 'C' TO WS-EMAIL-OK
                    MOVE 'S' TO WS-CANCELO
                 WHEN OTHER
                    PERFORM 9300-VALIDAR-EMAIL
              END-EVALUATE
           END-PERFORM.
           IF WS-CANCELO = 'S' EXIT PARAGRAPH END-IF.
           DISPLAY WS-BLANK-LINE LINE 19 COL 02.

      *> INGRESOS: numerico
           MOVE SPACES TO WS-INGRESO-TEXTO.
           ACCEPT WS-INGRESO-TEXTO LINE 14 COL 25.
           IF WS-INGRESO-TEXTO = SPACES OR
              WS-INGRESO-TEXTO(1:1) = "X" OR 
              WS-INGRESO-TEXTO(1:1) = "x"
              MOVE 'S' TO WS-CANCELO
              EXIT PARAGRAPH
           END-IF.
           COMPUTE CUSM-INGRESOS-MENSUALES =
              FUNCTION NUMVAL(WS-INGRESO-TEXTO).

           STRING WS-ANIO "-" WS-MES "-" WS-DIA
              DELIMITED BY SIZE INTO CUSM-FECHA-ALTA.

           COMPUTE CUSM-SCORE-CREDITICIO =
              (CUSM-INGRESOS-MENSUALES / 2) + (WS-EDAD-CALCULADA * 4).
           IF CUSM-SCORE-CREDITICIO > 999
              MOVE 999 TO CUSM-SCORE-CREDITICIO
           END-IF.
           MOVE 0   TO CUSM-TIENE-TARJETA.
           MOVE 0   TO CUSM-TIENE-HIPOTECA.
           MOVE 0   TO CUSM-SALDO-TOTAL-VISTA.
           MOVE 'A' TO CUSM-ESTADO-CLIENTE.

      ******************************************************************
      * 9210 - Validar nombre (alfabetico)
      ******************************************************************
       9210-VAL-NOMBRE.
           EVALUATE TRUE
              WHEN CUSM-NOMBRE-CLIENTE = SPACES
                 MOVE 'C' TO WS-NOMBRE-OK
                 MOVE 'S' TO WS-CANCELO
              WHEN CUSM-NOMBRE-CLIENTE = "X" 
                OR CUSM-NOMBRE-CLIENTE = "x"
                 MOVE 'C' TO WS-NOMBRE-OK
                 MOVE 'S' TO WS-CANCELO
              WHEN CUSM-NOMBRE-CLIENTE IS NOT ALPHABETIC
                 DISPLAY "[!] Solo letras y espacios (sin numeros)."
                    LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
              WHEN OTHER
                 MOVE 'S' TO WS-NOMBRE-OK
                 DISPLAY WS-BLANK-LINE LINE 19 COL 02
           END-EVALUATE.

      ******************************************************************
      * 9220 - Validar apellido (alfabetico)
      ******************************************************************
       9220-VAL-APELLIDO.
           EVALUATE TRUE
              WHEN CUSM-APELLIDOS-CLIENTE = SPACES
                 MOVE 'C' TO WS-APELLIDO-OK
                 MOVE 'S' TO WS-CANCELO
              WHEN CUSM-APELLIDOS-CLIENTE = "X" 
                OR CUSM-APELLIDOS-CLIENTE = "x"
                 MOVE 'C' TO WS-APELLIDO-OK
                 MOVE 'S' TO WS-CANCELO
              WHEN CUSM-APELLIDOS-CLIENTE IS NOT ALPHABETIC
                 DISPLAY "[!] Solo letras y espacios (sin numeros)."
                    LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
              WHEN OTHER
                 MOVE 'S' TO WS-APELLIDO-OK
                 DISPLAY WS-BLANK-LINE LINE 19 COL 02
           END-EVALUATE.

      ******************************************************************
      * 9230 - Validar telefono (10 digitos exactos)
      ******************************************************************
       9230-VAL-TELEFONO.
           EVALUATE TRUE
              WHEN WS-TELEF-IN = SPACES
                 MOVE 'C' TO WS-TELEF-OK
                 MOVE 'S' TO WS-CANCELO
              WHEN WS-TELEF-IN(1:1) = "X" OR WS-TELEF-IN(1:1) = "x"
                 MOVE 'C' TO WS-TELEF-OK
                 MOVE 'S' TO WS-CANCELO
              WHEN WS-TELEF-IN IS NOT NUMERIC
                 DISPLAY "[!] Solo numeros, 10 digitos exactos."
                    LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
              WHEN OTHER
                 MOVE WS-TELEF-IN TO CUSM-TELEF-CLIENTE
                 MOVE 'S' TO WS-TELEF-OK
                 DISPLAY WS-BLANK-LINE LINE 19 COL 02
           END-EVALUATE.

      ******************************************************************
      * 9250 - VALIDACION FECHA NACIMIENTO (edad 18-120)
      ******************************************************************
       9250-VALIDAR-FECHA-NACIMIENTO.
           MOVE 'N' TO WS-FECHA-OK.

           PERFORM UNTIL WS-FECHA-OK NOT = 'N'
              MOVE SPACES TO CUSM-FECHA-NACIMIENTO
              ACCEPT CUSM-FECHA-NACIMIENTO LINE 10 COL 25

              EVALUATE TRUE
                 WHEN CUSM-FECHA-NACIMIENTO = SPACES
                    MOVE 'C' TO WS-FECHA-OK
                    MOVE 'S' TO WS-CANCELO
                 WHEN CUSM-FECHA-NACIMIENTO(1:1) = "X"
                   OR CUSM-FECHA-NACIMIENTO(1:1) = "x"
                    MOVE 'C' TO WS-FECHA-OK
                    MOVE 'S' TO WS-CANCELO
                 WHEN CUSM-FECHA-NACIMIENTO(5:1) NOT = '-'
                    DISPLAY "[!] Formato AAAA-MM-DD."
                       LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                 WHEN CUSM-FECHA-NACIMIENTO(8:1) NOT = '-'
                    DISPLAY "[!] Formato AAAA-MM-DD."
                       LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                 WHEN CUSM-FECHA-NACIMIENTO(1:4) IS NOT NUMERIC
                    DISPLAY "[!] Anio invalido."
                       LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
                 WHEN OTHER
                    MOVE CUSM-FECHA-NACIMIENTO(1:4) TO WS-ANIO-NAC
                    COMPUTE WS-EDAD-CALCULADA =
                       WS-ANIO - WS-ANIO-NAC
                    PERFORM 9260-VERIF-EDAD
              END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 9260 - Verifica rango de edad
      ******************************************************************
       9260-VERIF-EDAD.
           EVALUATE TRUE
              WHEN WS-EDAD-CALCULADA < 18
                 DISPLAY "[!] El cliente debe ser mayor de 18."
                    LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
              WHEN WS-EDAD-CALCULADA > 120
                 DISPLAY "[!] Fecha de nacimiento no valida."
                    LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
              WHEN OTHER
                 MOVE 'S' TO WS-FECHA-OK
                 DISPLAY WS-BLANK-LINE LINE 19 COL 02
           END-EVALUATE.

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
              DISPLAY WS-BLANK-LINE LINE 19 COL 02
           ELSE
              DISPLAY "[!] Email invalido (ej: usuario@dom.com)."
                 LINE 19 COL 02 FOREGROUND-COLOR 4 HIGHLIGHT
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
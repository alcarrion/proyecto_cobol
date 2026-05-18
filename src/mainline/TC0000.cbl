       IDENTIFICATION DIVISION.
       PROGRAM-ID. TC0000.
       AUTHOR. TCS ECUADOR.
       DATE-WRITTEN. 2025-03-29.

      *================================================================*
      * MODULO: TC0000 - GESTION DE TARJETAS DE CREDITO
      * FUNCIONES: Emitir, consultar, cancelar y diferir consumos.
      * TASA FIJA: 35.00% anual (calculos en DF0000).
      *================================================================*

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-OPCION-TARJ         PIC 9(01).
       01  WS-CONTINUAR-TARJ      PIC X(01) VALUE 'S'.
       01  WS-CONFIRMAR           PIC X(01).
       01  WS-PAUSA               PIC X(01).

       01  WS-PROGRAMAS.
           05 WS-PGM-DBIOCUSM     PIC X(08) VALUE 'DBIOCUSM'.
           05 WS-PGM-DBIOINVM     PIC X(08) VALUE 'DBIOINVM'.
           05 WS-PGM-DBIOTARJ     PIC X(08) VALUE 'DBIOTARJ'.
           05 WS-PGM-DBIOTRAN     PIC X(08) VALUE 'DBIOTRAN'.
           05 WS-PGM-DF0000       PIC X(08) VALUE 'DF0000'.

       01  WS-ACCION-TRANS        PIC X(01).

       01  WS-DOC-ENTRADA         PIC X(12).
       01  WS-DOC-LEN             PIC 9(02).
       01  WS-I                   PIC 9(02).
       01  WS-DOC-OK              PIC X(01) VALUE 'N'.
       01  WS-TARJ-OK             PIC X(01) VALUE 'N'.

       01  WS-ENTRADA-MONTO       PIC X(12).
       01  WS-ENTRADA-CUOTAS      PIC X(03).
       01  WS-MONTO-TX            PIC S9(10)V99 COMP-3.
       01  WS-CUOTAS-TX           PIC 9(03).
       01  WS-ID-CUENTA-PRINC     PIC 9(08).

       01  WS-CALCULOS-TC.
           05 WS-CUPO-DISPONIBLE  PIC S9(10)V99 COMP-3.
           05 WS-RESULTADO-DIF    PIC X(01).
           05 WS-CUOTA-EDIT       PIC $ZZZ,ZZZ,ZZ9.99.
           05 WS-TOTAL-PAGAR      PIC S9(10)V99 COMP-3.
           05 WS-INTERES-DISP     PIC S9(10)V99 COMP-3.

       01  WS-FECHA-SISTEMA.
           05 WS-ANIO             PIC 9(04).
           05 WS-MES              PIC 9(02).
           05 WS-DIA              PIC 9(02).
       01  WS-ANIO-VENC           PIC 9(04).

       01  WS-AUX-GEN.
           05 WS-SEMILLA          PIC 9(08).
           05 WS-RANDOM           PIC 9(04).

       01  WS-SALDO-EDIT          PIC $ZZZ,ZZZ,ZZ9.99.

       01  WS-MOD10.
           05 WS-MOD10-OK         PIC X(01)  VALUE 'N'.
           05 WS-DIGITO           PIC 9(01).
           05 WS-RESULTADO        PIC 9(02).
           05 WS-SUMA-MOD10       PIC 9(04)  VALUE 0.
           05 WS-DIGITO-VERIF     PIC 9(01).
           05 WS-DIGITO-CALC      PIC 9(01).
           05 WS-K                PIC 9(02).
           05 WS-PROV-DOC         PIC 9(02).

       LINKAGE SECTION.
           COPY CUSMREC.
           COPY INVMREC.
           COPY TARJREC.
           COPY LKCIF.

       SCREEN SECTION.
       01  SCR-CLEAR.
           05 BLANK SCREEN.

       01  SCR-MARCO-TARJ.
           05 BLANK SCREEN.
           05 LINE 02 COL 02 VALUE
              "---------------------------------------------------".
           05 LINE 03 COL 02 VALUE
              "    BANCO LAF - TARJETAS DE CREDITO v3.0".
           05 LINE 04 COL 02 VALUE
              "---------------------------------------------------".
           05 LINE 05 COL 04 VALUE "Operador: ".
           05 LINE 05 COL 14 PIC X(08) FROM LKCIF-USUARIO.
           05 LINE 05 COL 26 VALUE "Terminal: ".
           05 LINE 05 COL 36 PIC X(04) FROM LKCIF-TERMINAL.

       01  SCR-MENU-TARJ.
           05 LINE 08 COL 05 VALUE "1. Emitir Tarjeta Nueva".
           05 LINE 09 COL 05 VALUE "2. Consultar Tarjeta".
           05 LINE 10 COL 05 VALUE "3. Diferir Consumo".
           05 LINE 11 COL 05 VALUE "4. Cancelar Tarjeta".
           05 LINE 12 COL 05 VALUE "5. Volver al menu principal".
           05 LINE 14 COL 05 VALUE
              "---------------------------------------------".
           05 LINE 16 COL 05 VALUE "Opcion: [ ]".
           05 SCR-TARJ-OPC LINE 16 COL 14 PIC 9
              USING WS-OPCION-TARJ REQUIRED.

       PROCEDURE DIVISION USING REG-CUSM,
                                REG-INVM,
                                REG-TARJ,
                                REG-DIFD,
                                LK-DATOS-SESION,
                                LK-DATOS-TRANSACCION.

       0000-PRINCIPAL.
           MOVE 'S' TO WS-CONTINUAR-TARJ.
           PERFORM 1000-MENU-TARJ
               UNTIL WS-CONTINUAR-TARJ = 'N'.
           GOBACK.

       1000-MENU-TARJ.
           PERFORM 9050-MARCO-TARJ.
           PERFORM 9051-MENU-TARJ.

           EVALUATE WS-OPCION-TARJ
               WHEN 1
                   PERFORM 2000-EMITIR-TARJETA
               WHEN 2
                   PERFORM 3000-CONSULTAR-TARJETA
               WHEN 3
                   PERFORM 4000-CONSUMO-DIFERIDO
               WHEN 4
                   PERFORM 5000-CANCELAR-TARJETA
               WHEN 5
                   MOVE 'N' TO WS-CONTINUAR-TARJ
               WHEN OTHER
                   DISPLAY "Opcion no valida."
                   ACCEPT WS-PAUSA
           END-EVALUATE.

      ************************************************************
      * OPCION 1 - EMITIR TARJETA NUEVA
      ************************************************************
       2000-EMITIR-TARJETA.
           PERFORM 9050-MARCO-TARJ.
           DISPLAY "=== EMISION DE TARJETA NUEVA ===".

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-DOC-OK = 'N' EXIT PARAGRAPH END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "ERROR: Cliente inactivo. No se puede operar."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           IF CUSM-SCORE-CREDITICIO < 450
               DISPLAY "RECHAZO: Score crediticio insuficiente."
               DISPLAY "Score actual:"
               DISPLAY CUSM-SCORE-CREDITICIO
               DISPLAY "Minimo requerido: 450 puntos."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           IF CUSM-TIENE-TARJETA = 1
               DISPLAY "ERROR: El cliente ya posee una tarjeta activa."
               DISPLAY "No se pueden emitir tarjetas duplicadas."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "ID cuenta para debito de pagos: "
           ACCEPT WS-ID-CUENTA-PRINC.

           IF WS-ID-CUENTA-PRINC = ZEROS
               DISPLAY "ERROR: ID de cuenta invalido."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-ID-CUENTA-PRINC TO INVM-ID-CUENTA.
           MOVE CUSM-ID-CLIENTE    TO INVM-ID-CLIENTE.

           MOVE 'S' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOINVM USING REG-INVM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-ERROR-NODATA
               DISPLAY "ERROR: Cuenta no encontrada en el sistema."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           IF NOT LK-EXITO
               DISPLAY LK-MENSAJE
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           IF INVM-ESTADO-CUENTA NOT = 'A'
               DISPLAY "ERROR: La cuenta seleccionada no esta activa."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           IF INVM-TIPO-CUENTA = 'H'
               DISPLAY "ERROR: No se permite cuenta hipotecaria."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Cupo a otorgar (ej: 2000.00): "
           ACCEPT WS-ENTRADA-MONTO.

           IF WS-ENTRADA-MONTO = SPACES
               DISPLAY "Operacion cancelada."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-MONTO-TX = FUNCTION NUMVAL(WS-ENTRADA-MONTO).

           IF WS-MONTO-TX <= ZEROS
               DISPLAY "ERROR: El cupo debe ser mayor a cero."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           INITIALIZE REG-TARJ.
           PERFORM 9200-GENERAR-NRO-TARJETA.

           ACCEPT WS-FECHA-SISTEMA FROM DATE YYYYMMDD.
           ADD 5 TO WS-ANIO GIVING WS-ANIO-VENC.

           STRING WS-ANIO "-" WS-MES "-" WS-DIA
               DELIMITED BY SIZE
               INTO TARJ-FECHA-EMISION.
           STRING WS-ANIO-VENC "-" WS-MES "-" WS-DIA
               DELIMITED BY SIZE
               INTO TARJ-FECHA-VENCTO.

           MOVE CUSM-ID-CLIENTE       TO TARJ-ID-CLIENTE.
           MOVE WS-ID-CUENTA-PRINC    TO TARJ-CUENTA-PRINCIPAL.
           MOVE WS-MONTO-TX           TO TARJ-CUPO-APROBADO.
           MOVE ZEROS                 TO TARJ-SALDO-UTILIZADO.
           MOVE 'A'                   TO TARJ-ESTADO.

           MOVE 'A' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                                      REG-DIFD,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               MOVE WS-MONTO-TX TO WS-SALDO-EDIT
               DISPLAY ">>> Tarjeta emitida exitosamente <<<"
               DISPLAY "Numero : "
               DISPLAY TARJ-NRO-TARJETA
               DISPLAY "Vencto : "
               DISPLAY TARJ-FECHA-VENCTO
               DISPLAY "Cupo   : "
               DISPLAY WS-SALDO-EDIT
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               IF LK-ERROR-DUPLICADO
                   DISPLAY "ERROR: El cliente ya tiene tarjeta activa."
               ELSE
                   DISPLAY LK-MENSAJE
               END-IF
           END-IF.

           DISPLAY "Presione ENTER para continuar."
           ACCEPT WS-PAUSA.

      ************************************************************
      * OPCION 2 - CONSULTAR TARJETA Y CUPO DISPONIBLE
      ************************************************************
       3000-CONSULTAR-TARJETA.
           PERFORM 9050-MARCO-TARJ.
           DISPLAY "=== CONSULTA DE TARJETA ===".

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-DOC-OK = 'N' EXIT PARAGRAPH END-IF.

           MOVE CUSM-ID-CLIENTE TO TARJ-ID-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                                      REG-DIFD,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-ERROR-NODATA
               DISPLAY "El cliente no tiene tarjeta registrada."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           IF NOT LK-EXITO
               DISPLAY LK-MENSAJE
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-CUPO-DISPONIBLE =
               TARJ-CUPO-APROBADO - TARJ-SALDO-UTILIZADO.

           DISPLAY "------------------------------------------".
           DISPLAY "Numero  : "
           DISPLAY TARJ-NRO-TARJETA.
           DISPLAY "Estado  : "
           DISPLAY TARJ-ESTADO.
           DISPLAY "Emision : "
           DISPLAY TARJ-FECHA-EMISION.
           DISPLAY "Vencto  : "
           DISPLAY TARJ-FECHA-VENCTO.
           DISPLAY "------------------------------------------".
           MOVE TARJ-CUPO-APROBADO TO WS-SALDO-EDIT.
           DISPLAY "Cupo Total   : "
           DISPLAY WS-SALDO-EDIT.
           MOVE TARJ-SALDO-UTILIZADO TO WS-SALDO-EDIT.
           DISPLAY "Deuda Actual : "
           DISPLAY WS-SALDO-EDIT.
           MOVE WS-CUPO-DISPONIBLE TO WS-SALDO-EDIT.
           DISPLAY "Cupo Libre   : "
           DISPLAY WS-SALDO-EDIT.
           DISPLAY "------------------------------------------".

           DISPLAY "Presione ENTER para continuar."
           ACCEPT WS-PAUSA.

      ************************************************************
      * OPCION 3 - DIFERIR CONSUMO (LLAMA A DF0000)
      ************************************************************
       4000-CONSUMO-DIFERIDO.
           PERFORM 9050-MARCO-TARJ.
           DISPLAY "=== CONSUMO DIFERIDO (COMPRA A CUOTAS) ===".

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-DOC-OK = 'N' EXIT PARAGRAPH END-IF.

           IF CLIENTE-INACTIVO
               DISPLAY "ERROR: Cliente inactivo. No se puede operar."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           MOVE CUSM-ID-CLIENTE TO TARJ-ID-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                                      REG-DIFD,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-ERROR-NODATA
               DISPLAY "El cliente no tiene tarjeta registrada."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           IF NOT LK-EXITO
               DISPLAY LK-MENSAJE
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           IF NOT TARJETA-ACTIVA
               DISPLAY "ERROR: Solo tarjetas activas admiten consumos."
               DISPLAY "Estado de tarjeta: "
               DISPLAY TARJ-ESTADO
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-CUPO-DISPONIBLE =
               TARJ-CUPO-APROBADO - TARJ-SALDO-UTILIZADO.
           MOVE WS-CUPO-DISPONIBLE TO WS-SALDO-EDIT.

           DISPLAY "Tarjeta        : "
           DISPLAY TARJ-NRO-TARJETA.
           DISPLAY "Cupo Disponible: "
           DISPLAY WS-SALDO-EDIT.

           DISPLAY "Monto de compra (ej: 500.00): "
           ACCEPT WS-ENTRADA-MONTO.

           IF WS-ENTRADA-MONTO = SPACES
               DISPLAY "Operacion cancelada."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           COMPUTE WS-MONTO-TX = FUNCTION NUMVAL(WS-ENTRADA-MONTO).

           IF WS-MONTO-TX <= ZEROS
               DISPLAY "ERROR: El monto debe ser mayor a cero."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           IF WS-MONTO-TX > WS-CUPO-DISPONIBLE
               MOVE WS-CUPO-DISPONIBLE TO WS-SALDO-EDIT
               DISPLAY "ERROR: Monto supera el cupo disponible."
               DISPLAY "Cupo libre: "
               DISPLAY WS-SALDO-EDIT
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           MOVE 0 TO WS-CUOTAS-TX.
           DISPLAY "Num cuotas (1-36, 1=sin interes): "
           ACCEPT WS-CUOTAS-TX.

           IF WS-CUOTAS-TX < 1 OR WS-CUOTAS-TX > 36
               DISPLAY "ERROR: Cuotas invalidas. Rango: 1 a 36."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Confirmar consumo (S/N): "
           ACCEPT WS-CONFIRMAR.

           IF WS-CONFIRMAR NOT = 'S' AND WS-CONFIRMAR NOT = 's'
               DISPLAY "Operacion cancelada."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

      *> Llamar al subprograma DF0000 para registrar el diferido
           CALL WS-PGM-DF0000 USING REG-TARJ,
                                    REG-DIFD,
                                    LK-DATOS-SESION,
                                    LK-DATOS-TRANSACCION,
                                    WS-MONTO-TX,
                                    WS-CUOTAS-TX,
                                    WS-RESULTADO-DIF.

           IF WS-RESULTADO-DIF = 'S'
               COMPUTE WS-TOTAL-PAGAR =
                   DIFD-VALOR-CUOTA * WS-CUOTAS-TX
               COMPUTE WS-INTERES-DISP =
                   WS-TOTAL-PAGAR - WS-MONTO-TX
               MOVE DIFD-VALOR-CUOTA TO WS-CUOTA-EDIT
               DISPLAY ">>> Consumo diferido registrado <<<"
               DISPLAY "Cuotas       : "
               DISPLAY WS-CUOTAS-TX
               DISPLAY "Valor cuota  : "
               DISPLAY WS-CUOTA-EDIT
               MOVE WS-MONTO-TX TO WS-SALDO-EDIT
               DISPLAY "Monto compra : "
               DISPLAY WS-SALDO-EDIT
               MOVE WS-INTERES-DISP TO WS-SALDO-EDIT
               DISPLAY "Interes total: "
               DISPLAY WS-SALDO-EDIT
               MOVE WS-TOTAL-PAGAR TO WS-SALDO-EDIT
               DISPLAY "TOTAL A PAGAR: "
               DISPLAY WS-SALDO-EDIT
           ELSE
               DISPLAY "ERROR en diferido: "
               DISPLAY LK-MENSAJE
           END-IF.

           DISPLAY "Presione ENTER para continuar."
           ACCEPT WS-PAUSA.

      ************************************************************
      * OPCION 4 - CANCELAR TARJETA (SALDO DEBE SER CERO)
      ************************************************************
       5000-CANCELAR-TARJETA.
           PERFORM 9050-MARCO-TARJ.
           DISPLAY "=== CANCELACION DE TARJETA ===".

           PERFORM 9100-BUSCAR-CLIENTE.
           IF WS-DOC-OK = 'N' EXIT PARAGRAPH END-IF.

           MOVE CUSM-ID-CLIENTE TO TARJ-ID-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                                      REG-DIFD,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-ERROR-NODATA
               DISPLAY "El cliente no tiene tarjeta registrada."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           IF NOT LK-EXITO
               DISPLAY LK-MENSAJE
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           MOVE TARJ-SALDO-UTILIZADO TO WS-SALDO-EDIT.
           DISPLAY "------------------------------------------".
           DISPLAY "Tarjeta: "
           DISPLAY TARJ-NRO-TARJETA.
           DISPLAY "Estado : "
           DISPLAY TARJ-ESTADO.
           DISPLAY "Saldo  : "
           DISPLAY WS-SALDO-EDIT.
           DISPLAY "------------------------------------------".

           IF TARJ-SALDO-UTILIZADO > ZEROS
               DISPLAY "ERROR: Prohibido cerrar tarjetas con"
               DISPLAY "saldos pendientes de pago."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Confirmar cancelacion definitiva (S/N): "
           ACCEPT WS-CONFIRMAR.

           IF WS-CONFIRMAR NOT = 'S' AND WS-CONFIRMAR NOT = 's'
               DISPLAY "Operacion cancelada."
               DISPLAY "Presione ENTER para continuar."
               ACCEPT WS-PAUSA
               EXIT PARAGRAPH
           END-IF.

           MOVE 'B' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOTARJ USING REG-TARJ,
                                      REG-DIFD,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-EXITO
               MOVE 'C' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY ">>> Tarjeta cancelada correctamente <<<"
           ELSE
               MOVE 'R' TO WS-ACCION-TRANS
               CALL WS-PGM-DBIOTRAN USING WS-ACCION-TRANS
               DISPLAY LK-MENSAJE
           END-IF.

           DISPLAY "Presione ENTER para continuar."
           ACCEPT WS-PAUSA.

      ************************************************************
      * 9100 - Buscar cliente por documento
      ************************************************************
       9100-BUSCAR-CLIENTE.
           MOVE 'N' TO WS-DOC-OK.
           DISPLAY "Documento (cedula/pasaporte): " LINE 09 COL 05.
           ACCEPT WS-DOC-ENTRADA LINE 09 COL 36.

           MOVE 12 TO WS-I.
           PERFORM UNTIL WS-I = 0
                      OR WS-DOC-ENTRADA(WS-I:1) NOT = SPACE
               SUBTRACT 1 FROM WS-I
           END-PERFORM.
           MOVE WS-I TO WS-DOC-LEN.

           IF WS-DOC-LEN < 6 OR WS-DOC-LEN > 12
               DISPLAY "Documento invalido (6-12 caracteres)."
                  LINE 11 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           IF WS-DOC-LEN = 10
               IF WS-DOC-ENTRADA(1:10) IS NUMERIC
                   PERFORM 9800-VALIDAR-MODULO-10
                   IF WS-MOD10-OK = 'N'
                       DISPLAY "Cedula invalida (Modulo 10)."
                          LINE 11 COL 05
                       DISPLAY "Presione ENTER para continuar."
                          LINE 23 COL 05
                       ACCEPT WS-PAUSA LINE 23 COL 36
                       EXIT PARAGRAPH
                   END-IF
                   MOVE 'CED' TO CUSM-TIPO-DOC
               ELSE
                   MOVE 'PAS' TO CUSM-TIPO-DOC
               END-IF
           ELSE
               MOVE 'PAS' TO CUSM-TIPO-DOC
           END-IF.

           MOVE WS-DOC-ENTRADA TO CUSM-DOC-CLIENTE.

           MOVE 'C' TO LK-ACCION-DB.
           CALL WS-PGM-DBIOCUSM USING REG-CUSM,
                                      LK-DATOS-SESION,
                                      LK-DATOS-TRANSACCION.

           IF LK-ERROR-NODATA
               DISPLAY "Cliente no encontrado." LINE 11 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           IF NOT LK-EXITO
               DISPLAY "Error tecnico al buscar cliente."
                  LINE 11 COL 05
               DISPLAY "Presione ENTER para continuar."
                  LINE 23 COL 05
               ACCEPT WS-PAUSA LINE 23 COL 36
               EXIT PARAGRAPH
           END-IF.

           DISPLAY "Cliente: " LINE 11 COL 05.
           DISPLAY CUSM-NOMBRE-CLIENTE LINE 11 COL 15.
           DISPLAY CUSM-APELLIDOS-CLIENTE LINE 11 COL 42.
           MOVE 'S' TO WS-DOC-OK.

      ************************************************************
      * 9200 - Generar numero de tarjeta de 16 digitos
      ************************************************************
       9200-GENERAR-NRO-TARJETA.
           ACCEPT WS-SEMILLA FROM TIME.
           COMPUTE WS-RANDOM = FUNCTION MOD(WS-SEMILLA, 10000).
           STRING "4555" CUSM-ID-CLIENTE WS-RANDOM
               DELIMITED BY SIZE
               INTO TARJ-NRO-TARJETA.

      ************************************************************
      * 9800 - Validar cedula ecuatoriana por Modulo 10
      ************************************************************
       9800-VALIDAR-MODULO-10.
           MOVE 'N' TO WS-MOD10-OK.
           MOVE 0   TO WS-SUMA-MOD10.

           MOVE WS-DOC-ENTRADA(1:2) TO WS-PROV-DOC.
           IF WS-PROV-DOC < 1 OR WS-PROV-DOC > 24
               EXIT PARAGRAPH
           END-IF.

           MOVE WS-DOC-ENTRADA(10:1) TO WS-DIGITO-VERIF.

           PERFORM VARYING WS-K FROM 1 BY 1
                   UNTIL WS-K > 9
               MOVE WS-DOC-ENTRADA(WS-K:1) TO WS-DIGITO
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

           IF WS-DIGITO-CALC = WS-DIGITO-VERIF
               MOVE 'S' TO WS-MOD10-OK
           END-IF.

      ************************************************************
      * 9050 - Cabecera del modulo (limpia pantalla)
      ************************************************************
       9050-MARCO-TARJ.
           DISPLAY SCR-MARCO-TARJ.

      ************************************************************
      * 9051 - Menu de opciones
      ************************************************************
       9051-MENU-TARJ.
           DISPLAY SCR-MENU-TARJ.
           ACCEPT SCR-TARJ-OPC.

       END PROGRAM TC0000.

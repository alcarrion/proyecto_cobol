      ******************************************************************
      * PROGRAMA: BANCSLOG (LOGIN CENTRAL - BANCO LAF)                 *
      * OBJETIVO: IDENTIFICACION UNICA Y ACTIVACION DE SESION          *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCSLOG.

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PANTALLA.
           05 FILLER PIC X(65) VALUE 
           "+---------------------------------------------------------------+".
           05 FILLER PIC X(65) VALUE 
           "|                                                               |".
           05 FILLER PIC X(65) VALUE 
           "|    ██████╗  █████╗ ███╗    ██╗ ██████╗ ██████╗  ██████╗       |".
           05 FILLER PIC X(65) VALUE 
           "|    ██╔══██╗██╔══██╗████╗  ██║██╔════╝██╔═══██╗██╔═══██╗       |".
           05 FILLER PIC X(65) VALUE 
           "|    ██████╔╝███████║██╔██╗ ██║██║     ██║   ██║██║   ██║       |".
           05 FILLER PIC X(65) VALUE 
           "|    SISTEMA DE CORE BANCARIO - BANCO LAF - V 2.0               |".
           05 FILLER PIC X(65) VALUE 
           "+---------------------------------------------------------------+".
           05 FILLER PIC X(65) VALUE 
           "|  ESTABLECIENDO CONEXION CON BASE DE DATOS...        [ OK ]    |".
           05 FILLER PIC X(65) VALUE 
           "|  CARGANDO PARAMETROS DEL SISTEMA...                 [ OK ]    |".
           05 FILLER PIC X(65) VALUE 
           "+---------------------------------------------------------------+".

       01  WS-CAPTURA.
           05 WS-CEDULA-USER           PIC X(12).

       LINKAGE SECTION.
       COPY LKCIF.

       PROCEDURE DIVISION USING LK-DATOS-SESION LK-DATOS-TRANSACCION.

       0000-MAIN.
           PERFORM 1000-DIBUJAR-SPLASH
           PERFORM 2000-PROCESAR-LOGIN
           GOBACK.

       1000-DIBUJAR-SPLASH.
           DISPLAY SPACES UPON CRT
           DISPLAY WS-PANTALLA.

       2000-PROCESAR-LOGIN.
           DISPLAY " "
           DISPLAY "  POR FAVOR, INGRESE IDENTIFICACION PARA COMENZAR:"
           DISPLAY "  > " WITH NO ADVANCING
           ACCEPT WS-CEDULA-USER
           
           * AQUI SIMULAMOS UNA BUSQUEDA EN DBIOCUSM (LA HAREMOS REAL LUEGO)
           IF WS-CEDULA-USER NOT = SPACES
              MOVE 'S'            TO LK-SESION-ACTIVA
              MOVE WS-CEDULA-USER TO LK-DOC-CLIENTE-SES
              MOVE "ALISON CARRION" TO LK-NOMBRE-CLIENTE
              MOVE 850            TO LK-SCORE-CLIENTE
              MOVE 'ACARRION'     TO LK-USUARIO-ID
              MOVE 'TQ01'         TO LK-TERMINAL-ID
              DISPLAY " "
              DISPLAY "  IDENTIFICACION EXITOSA. CARGANDO PERFIL... [ OK ]"
           ELSE
              MOVE 'N'            TO LK-SESION-ACTIVA
              DISPLAY "  ERROR: DEBE INGRESAR UNA IDENTIFICACION VALIDA."
           END-IF.
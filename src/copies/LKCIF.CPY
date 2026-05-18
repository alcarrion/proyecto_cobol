      *================================================================*
      * COPYBOOK : LKCIF.CPY                                           *
      * FUNCION  : Area de Sesion Unica + Comunicacion DBIO            *
      * BANCO LAF v3.0 - TCS Ecuador Nivel 1000                        *
      *----------------------------------------------------------------*
      * AREA 1: LK-DATOS-SESION                                        *
      * Cargada UNA SOLA VEZ en BANCSMENU al autenticar al cajero.   *
      * Viaja por LINKAGE a CI0000, IN0000, TC0000 y BR0000.         *
      *----------------------------------------------------------------*
           COPY TIPOSCOM.
       01  LK-DATOS-SESION.
           05 LKCIF-CLIENTE-ID         PIC 9(08).
           05 LKCIF-CEDULA             PIC X(12).
           05 LKCIF-NOMBRE-COMPLETO    PIC X(50).
           05 LKCIF-USUARIO            PIC X(08).
           05 LKCIF-TERMINAL           PIC X(04).
           05 LKCIF-SCORE-VIGENTE      PIC 9(03).
           05 LKCIF-ESTADO-MORA        PIC X(01).
              88 CLIENTE-EN-MORA       VALUE 'S'.
              88 CLIENTE-SIN-MORA      VALUE 'N'.

      *----------------------------------------------------------------*
      * AREA 2: LK-DATOS-TRANSACCION                                   *
      * Canal de comunicacion entre Capa Negocio y Capa DBIO.        *
      *================================================================*
       01  LK-DATOS-TRANSACCION.
           05 LK-ACCION-DB             PIC X(01).
              88 ACCION-INSERT         VALUE 'A'.
              88 ACCION-SELECT         VALUE 'C'.
              88 ACCION-UPDATE         VALUE 'M'.
              88 ACCION-BAJA           VALUE 'B'.
              88 ACCION-LOCK           VALUE 'L'.
              88 ACCION-SECUENCIA      VALUE 'S'.
              88 ACCION-CURSOR-FETCH   VALUE 'F'.
              88 ACCION-CURSOR-CLOSE   VALUE 'Z'.
           05 LK-TX-CLIENTE-ID         PIC 9(08).
           05 LK-COD-RETORNO           PIC X(04).
              88 LK-EXITO              VALUE '00  '.
              88 LK-ERROR-FONDOS       VALUE 'E001'.
              88 LK-ERROR-NODATA       VALUE 'E404'.
              88 LK-ERROR-SCORE        VALUE 'E400'.
              88 LK-ERROR-DUPLICADO    VALUE 'E409'.
              88 LK-ERROR-TECNICO      VALUE 'E999'.
           05 LK-MENSAJE               PIC X(50).

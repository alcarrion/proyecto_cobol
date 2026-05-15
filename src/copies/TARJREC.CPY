      *================================================================*
      * COPYBOOK: TARJREC - REGISTRO TARJETAS DE CREDITO              *
      * BANCO LAF v2.0 - Con Cuenta Principal Vinculada               *
      *================================================================*
       01  REG-TARJ.
           05 TARJ-NRO-TARJETA         PIC X(16).
           05 TARJ-ID-CLIENTE          PIC 9(09).
           05 TARJ-CUENTA-PRINCIPAL    PIC 9(09).
           05 TARJ-FECHA-EMISION       PIC X(10).
           05 TARJ-FECHA-VENCTO        PIC X(10).
           05 TARJ-CUPO-APROBADO       PIC S9(12)V99.
           05 TARJ-SALDO-UTILIZADO     PIC S9(12)V99.
           05 TARJ-ESTADO-TARJETA      PIC X(01).
              88 TARJETA-ACTIVA        VALUE 'A'.
              88 TARJETA-BLOQUEADA     VALUE 'B'.
              88 TARJETA-VENCIDA       VALUE 'V'.
           05 TARJ-IMPORTE-MOV         PIC S9(12)V99.

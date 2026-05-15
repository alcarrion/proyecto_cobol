      *================================================================*
      * COPYBOOK: INVMREC - REGISTRO CUENTAS A LA VISTA               *
      * BANCO LAF v2.0 - Soporta Multi-Cuenta (1:N)                   *
      *================================================================*
       01  REG-INVM.
           05 INVM-ID-CUENTA           PIC 9(09).
           05 INVM-ID-CLIENTE          PIC 9(09).
           05 INVM-TIPO-CUENTA         PIC X(01).
              88 CUENTA-AHORROS        VALUE 'A'.
              88 CUENTA-CORRIENTE      VALUE 'C'.
              88 CUENTA-CHEQUES        VALUE 'H'.
           05 INVM-SALDO-ACTUAL        PIC S9(15)V99.
           05 INVM-FECHA-APERTURA      PIC X(10).
           05 INVM-ESTADO-CUENTA       PIC X(01).
              88 CUENTA-ACTIVA         VALUE 'A'.
              88 CUENTA-BLOQUEADA      VALUE 'B'.
              88 CUENTA-CERRADA        VALUE 'C'.

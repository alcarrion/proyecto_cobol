      *================================================================*
      * COPYBOOK: INVMREC                                              *
      * DESCRIPCION: REGISTRO DE CUENTAS BANCARIAS ADAPTADO            *
      * ADAPTACIÓN: ESPEJO EXACTO DE LA TABLA 'CTACTES' DEL EQUIPO     *
      *================================================================*
           COPY TIPOSCOM.
       01  REG-INVM.
           05 INVM-ID-CUENTA           PIC 9(08).
           05 INVM-ID-CLIENTE          PIC 9(08).
           05 INVM-TIPO-CUENTA         PIC X(01).
              88 CTA-AHORROS           VALUE 'A'.
              88 CTA-CORRIENTE         VALUE 'C'.
              88 CTA-HIPOTECARIA       VALUE 'H'.  *> Cuenta vinculada para débito de Hipoteca
           05 INVM-SALDO-ACTUAL        PIC S9(13)V99 COMP-3.
           05 INVM-FECHA-APERTURA      PIC X(10).
           05 INVM-ESTADO-CUENTA       PIC X(01).
              88 CTA-ACTIVA            VALUE 'A'.
              88 CTA-BLOQUEADA         VALUE 'B'.
              88 CTA-CERRADA           VALUE 'C'.

      *================================================================*
      * COPYBOOK : TARJREC.CPY                                        *
      * FUNCION  : Registro de Tarjetas de Credito + Diferidos        *
      *================================================================*
           COPY TIPOSCOM.
       01  REG-TARJ.
           05 TARJ-NRO-TARJETA        PIC X(16).
           05 TARJ-ID-CLIENTE         PIC 9(08).
           05 TARJ-CUENTA-PRINCIPAL   PIC 9(08).
           05 TARJ-FECHA-EMISION      PIC X(10).
           05 TARJ-FECHA-VENCTO       PIC X(10).
           05 TARJ-CUPO-APROBADO      PIC S9(10)V99 COMP-3.
           05 TARJ-SALDO-UTILIZADO    PIC S9(10)V99 COMP-3.
           05 TARJ-ESTADO             PIC X(01).
              88 TARJETA-ACTIVA       VALUE 'A'.
              88 TARJETA-BLOQUEADA    VALUE 'B'.
              88 TARJETA-VENCIDA      VALUE 'V'.

       01  REG-DIFD.
           05 DIFD-ID-DIFERIDO        PIC 9(08).
           05 DIFD-NRO-TARJETA        PIC X(16).
           05 DIFD-MONTO-COMPRA       PIC S9(10)V99 COMP-3.
           05 DIFD-CUOTAS-TOTALES     PIC 9(03).
           05 DIFD-CUOTAS-PENDIENTES  PIC 9(03).
           05 DIFD-VALOR-CUOTA        PIC S9(10)V99 COMP-3.
           05 DIFD-TASA-INTERES       PIC S9(03)V99 COMP-3.

      *================================================================*
      * COPYBOOK : TARJREC.CPY                                        *
      * FUNCION  : Registro de Tarjetas de Credito + Diferidos        *
      * TABLAS   : tarjetas, tarjetas_diferidos                        *
      * BANCO LAF v3.0 - TCS Ecuador Nivel 1000                       *
      *================================================================*
      * TABLA tarjetas:                                                *
      *   NRO_TARJETA(PK VARCHAR16), ID_CLIENTE(FK INT),              *
      *   CUENTA_PRINCIPAL(FK INT a ctactes),                         *
      *   FECHA_EMISION(DATE), FECHA_VENCTO(DATE),                    *
      *   CUPO_APROBADO(DEC12,2), SALDO_UTILIZADO(DEC12,2 DEF 0),     *
      *   ESTADO_TARJETA(CHAR1: A=Activa B=Bloqueada V=Vencida)       *
      *----------------------------------------------------------------*
      * TABLA tarjetas_diferidos:                                      *
      *   ID_DIFERIDO(INT AI PK), NRO_TARJETA(FK VARCHAR16),          *
      *   MONTO_COMPRA(DEC12,2), CUOTAS_TOTALES(INT),                 *
      *   CUOTAS_PENDIENTES(INT), VALOR_CUOTA(DEC12,2),               *
      *   TASA_INTERES(DEC5,2)                                         *
      *================================================================*
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

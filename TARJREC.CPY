      *================================================================*
      * COPYBOOK: TARJREC                                              *
      * DESCRIPCION: ESTRUCTURA DE LA TABLA MAESTRA DE TARJETAS        *
      *================================================================*
       01  REG-TARJ.
           05 TARJ-ID-CLIENTE          PIC 9(08).
           05 TARJ-NRO-TARJETA         PIC X(16).
           05 TARJ-FECHA-EMISION       PIC X(10).
           05 TARJ-FECHA-VENCIM        PIC X(10).
           05 TARJ-LIMITE-TARJETA      PIC S9(10)V99 COMP-3.
      *    Acumulado de consumos del mes en curso
           05 TARJ-ACUM-MES            PIC S9(10)V99 COMP-3.
      *    Monto liquidado (a pagar) del cierre anterior
           05 TARJ-LIQUIDACION-MES     PIC S9(10)V99 COMP-3.
      *    ESTADO: 'A' = ACTIVA, 'B' = BLOQUEADA, 'I' = INACTIVA
           05 TARJ-ESTADO              PIC X(01).

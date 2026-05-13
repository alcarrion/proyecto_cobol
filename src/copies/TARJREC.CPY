      *================================================================*
      * COPYBOOK: TARJREC                                              *
      * DESCRIPCION: ESTRUCTURA DE LA TABLA MAESTRA DE TARJETAS        *
      *================================================================*
       01  REG-TARJ.
           05 TARJ-ID-CLIENTE          PIC 9(08).
           05 TARJ-NRO-TARJETA         PIC X(16).
           05 TARJ-FECHA-EMISION       PIC X(10).
           05 TARJ-FECHA-VENCIM        PIC X(10).
           05 TARJ-LIMITE-TARJETA      PIC S9(10)V99.
           05 TARJ-ACUM-MES            PIC S9(10)V99.
           05 TARJ-LIQUIDACION-MES     PIC S9(10)V99.
           05 TARJ-ESTADO              PIC X(01).
           05 TARJ-IMPORTE-MOV         PIC S9(10)V99.

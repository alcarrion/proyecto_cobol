      *================================================================*
      * COPYBOOK: CUSMREC                                              *
      * DESCRIPCION: ESTRUCTURA DE LA TABLA MAESTRA DE CLIENTES        *
      *================================================================*
       01  REG-CUSM.
           05 CUSM-ID-CLIENTE          PIC 9(08).
           05 CUSM-TIPO-DOC            PIC X(03).
           05 CUSM-DOC-CLIENTE         PIC X(12).
           05 CUSM-FECHA-ALTA          PIC X(10).
           05 CUSM-NOMBRE              PIC X(25).
           05 CUSM-APELLIDOS           PIC X(25).
           05 CUSM-DIRECCION           PIC X(45).
           05 CUSM-TELEFONO            PIC X(12).
           05 CUSM-EMAIL               PIC X(40).
      *    BANDERAS DE PRODUCTOS (0=NO TIENE, 1=TIENE)
           05 CUSM-TARJETA             PIC 9(01).
           05 CUSM-CREDITO             PIC 9(01).
           05 CUSM-HIPOTECA            PIC 9(01).
           05 CUSM-CTA-ACTIVA          PIC 9(01).
      *    CAMPOS FINANCIEROS Y DE CIERRE
           05 CUSM-FECHA-CIERRE        PIC X(10).
           05 CUSM-SALDO-CLIENTE       PIC S9(10)V99.

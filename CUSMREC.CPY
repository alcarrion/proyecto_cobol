      *================================================================*
      * COPYBOOK: CUSMREC                                              *
      * DESCRIPCION: ESTRUCTURA DE LA TABLA MAESTRA DE CLIENTES (CUSM) *
      *================================================================*
       01  REG-CUSM.
           05 CUSM-ID-CLIENTE          PIC 9(08).
           05 CUSM-TIPO-DOC            PIC X(02).
           05 CUSM-DOC-CLIENTE         PIC X(15).
           05 CUSM-FECHA-ALTA          PIC 9(10).
           05 CUSM-NOMBRE              PIC X(30).
           05 CUSM-APELLIDOS           PIC X(30).
           05 CUSM-DIRECCION           PIC X(50).
           05 CUSM-TELEFONO            PIC X(15).
           05 CUSM-EMAIL               PIC X(50).
      *    BANDERAS DE PRODUCTOS (0=NO TIENE, 1=TIENE)[cite: 1, 2]
           05 CUSM-FLG-TARJETA         PIC 9(01).
           05 CUSM-FLG-CREDITO         PIC 9(01).
           05 CUSM-FLG-HIPOTECA        PIC 9(01).
           05 CUSM-FLG-CTA-ACTIVA      PIC 9(01).

      *    CAMPOS FINANCIEROS Y DE CIERRE
           05 CUSM-FECHA-CIERRE        PIC 9(08).
           05 CUS-SALDO-TOTAL          PIC S9(13)V99  USAGE COMP-3.
      *    ESTADO: 'A' = ACTIVO, 'I' = INACTIVO
           05 CUSM-ESTADO              PIC X(01).

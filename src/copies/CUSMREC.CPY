      *================================================================*
      * COPYBOOK: CUSMREC - REGISTRO MAESTRO DE CLIENTES              *
      * BANCO LAF v2.0 - Incluye Scoring y Mayoria de Edad            *
      *================================================================*
       01  REG-CUSM.
           05 CUSM-ID-CLIENTE          PIC 9(09).
           05 CUSM-TIPO-DOC            PIC X(03).
           05 CUSM-DOC-CLIENTE         PIC X(12).
           05 CUSM-FECHA-ALTA          PIC X(10).
           05 CUSM-NOMBRE              PIC X(25).
           05 CUSM-APELLIDOS           PIC X(25).
           05 CUSM-FECHA-NACIMIENTO    PIC X(10).
           05 CUSM-DIRECCION           PIC X(45).
           05 CUSM-TELEFONO            PIC X(12).
           05 CUSM-EMAIL               PIC X(40).
           05 CUSM-SCORE-CREDITICIO    PIC 9(03).
           05 CUSM-INGRESOS            PIC S9(12)V99.
           05 CUSM-TIENE-TARJETA       PIC 9(01).
           05 CUSM-TIENE-HIPOTECA      PIC 9(01).
           05 CUSM-ESTADO-CLIENTE      PIC X(01).
              88 CLIENTE-ACTIVO        VALUE 'A'.
              88 CLIENTE-INACTIVO      VALUE 'I'.
           05 CUSM-SALDO-TOTAL-VISTA   PIC S9(12)V99.

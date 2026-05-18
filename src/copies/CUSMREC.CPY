      *================================================================*
      * COPYBOOK: CUSMREC                                              *
      * DESCRIPCION: REGISTRO MAESTRO DE CLIENTES REFACTORIZADO v3.0   *
      * ADAPTACIÓN: ESPEJO EXACTO DE LA TABLA 'CLIENTES' DEL EQUIPO    *
      *================================================================*
       01  REG-CUSM.
           05 CUSM-ID-CLIENTE          PIC 9(08).
           05 CUSM-TIPO-DOC            PIC X(03).
           05 CUSM-DOC-CLIENTE         PIC X(12).
           05 CUSM-FECHA-ALTA          PIC X(10).
           05 CUSM-NOMBRE-CLIENTE      PIC X(25).
           05 CUSM-APELLIDOS-CLIENTE   PIC X(25).
           05 CUSM-FECHA-NACIMIENTO    PIC X(10).
           05 CUSM-DIRECCION-CLIENTE   PIC X(45).
           05 CUSM-TELEF-CLIENTE       PIC X(12).
           05 CUSM-EMAIL-CLIENTE       PIC X(40).
           05 CUSM-SCORE-CREDITICIO    PIC 9(03).
           05 CUSM-INGRESOS-MENSUALES  PIC S9(10)V99 COMP-3.
           05 CUSM-TIENE-TARJETA       PIC 9(01).
           05 CUSM-TIENE-HIPOTECA      PIC 9(01).
           05 CUSM-ESTADO-CLIENTE      PIC X(01).
              88 CLIENTE-ACTIVO        VALUE 'A'.
              88 CLIENTE-INACTIVO      VALUE 'I'.
           05 CUSM-SALDO-TOTAL-VISTA   PIC S9(10)V99 COMP-3.

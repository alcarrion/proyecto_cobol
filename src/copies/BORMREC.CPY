      *================================================================*
      * COPYBOOK: BORMREC - REGISTRO HIPOTECAS                        *
      * BANCO LAF v2.0 - Con Cuenta Debito Vinculada                  *
      * ESTADO: ACTIVO MOROSO CASTIGADO PAGADO                        *
      *================================================================*
       01 BORM-REGISTRO.
           05 BORM-ID-HIPOTECA         PIC 9(09).
           05 BORM-ID-CLIENTE          PIC 9(09).
           05 BORM-CUENTA-DEBITO       PIC 9(09).
           05 BORM-MONTO-PRESTAMO      PIC S9(15)V99.
           05 BORM-SALDO-DEUDA         PIC S9(15)V99.
           05 BORM-TASA-ANUAL          PIC S9(07)V9999.
           05 BORM-CUOTA-MENSUAL       PIC S9(15)V99.
           05 BORM-MESES-MORA          PIC 9(03).
           05 BORM-ESTADO-PRESTAMO     PIC X(15).
              88 HIPOTECA-ACTIVA       VALUE 'ACTIVO'.
              88 HIPOTECA-MOROSA       VALUE 'MOROSO'.
              88 HIPOTECA-CASTIGADA    VALUE 'CASTIGADO'.
              88 HIPOTECA-PAGADA       VALUE 'PAGADO'.
           05 BORM-FECHA-VENCIMIENTO   PIC X(10).

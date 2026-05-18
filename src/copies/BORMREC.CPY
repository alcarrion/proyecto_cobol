      *================================================================*
      * COPYBOOK: BORMREC                                              *
      * DESCRIPCION: REGISTRO DE HIPOTECAS REFACTORIZADO v3.0          *
      * ADAPTACIÓN: ESPEJO EXACTO DE LA TABLA 'HIPOTECAS' DEL EQUIPO   *
      * BANCO LAF v3.0 - TCS Ecuador Nivel 1000                        *
      *================================================================*
       01  REG-BORM.
           05 BORM-ID-HIPOTECA         PIC 9(08).
           05 BORM-ID-CLIENTE          PIC 9(08).
           05 BORM-CUENTA-DEBITO       PIC 9(08).
           05 BORM-MONTO-PRESTAMO      PIC S9(13)V99 COMP-3.
           05 BORM-SALDO-DEUDA         PIC S9(13)V99 COMP-3.
           05 BORM-TASA-ANUAL          PIC S9(03)V9999 COMP-3.
           05 BORM-CUOTA-MENSUAL       PIC S9(13)V99 COMP-3.
           05 BORM-MESES-MORA          PIC 9(03).
           05 BORM-ESTADO-PRESTAMO     PIC X(15).
              88 HIPO-ACTIVO           VALUE 'ACTIVO'.
              88 HIPO-MOROSO           VALUE 'MOROSO'.
              88 HIPO-PAGADO           VALUE 'PAGADO'.
           05 BORM-FECHA-VENCIMIENTO   PIC X(10).

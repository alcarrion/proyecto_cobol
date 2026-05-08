      *================================================================*
      * BORMREC.CPY - REGISTRO DE LA TABLA HIPOTECAS                  *
      *   ESTADO es X(10) para coincidir con varchar(10) en la DB     *
      *   Los estados posibles son:                                    *
      *     ACTIVO    (6 chars)                                        *
      *     MOROSO    (6 chars)                                        *
      *     CASTIGADO (9 chars)                                        *
      *     CANCELADO (9 chars)                                        *
      *================================================================*
       01 BORM-REGISTRO.
           05 BORM-ID-HIPOTECA        PIC 9(09).
           05 BORM-ID-CLIENTE         PIC 9(08).
           05 BORM-FECHA-INICIO       PIC X(10).
           05 BORM-MONTO-ORIGINAL     PIC S9(13)V99.
           05 BORM-TASA-INTERES       PIC S9(03)V9999.
           05 BORM-SALDO-ACTUAL       PIC S9(13)V99.
           05 BORM-FECHA-VENCTO       PIC X(10).
           05 BORM-DIA-PAGO           PIC 9(02).
           05 BORM-ESTADO             PIC X(20).
           05 BORM-CUOTA-MENSUAL      PIC S9(13)V99.
           05 BORM-MESES-MORA         PIC 9(03).
           05 BORM-FECHA-ULT-PAGO     PIC X(10).

      *================================================================*
      * COPYBOOK: SECREC                                               *
      * DESCRIPCION: MAESTRO CONTROL DE SECUENCIAS Y NUMERADORES       *
      * ADAPTACIÓN: ESPEJO EXACTO DE 'CONTROL_SECUENCIAS' DEL EQUIPO   *
      * BANCO LAF v3.0 - TCS Ecuador Nivel 1000                        *
      *================================================================*
       01  REG-SEC.
           05 SEC-PRODUCTO             PIC X(20).
              88 SEC-PROD-CLIENTE      VALUE 'CLIENTES'.
              88 SEC-PROD-CUENTA       VALUE 'CTACTES'.
              88 SEC-PROD-MOVIMIENTO   VALUE 'MOVIMIENTOS'.
              88 SEC-PROD-HIPOTECA     VALUE 'HIPOTECA'.
              88 SEC-PROD-DIFERIDO     VALUE 'TARJETAS_DIFERIDOS'.
           05 SEC-ULTIMO-VALOR         PIC 9(08).
           05 SEC-FECHA-REFRESCO       PIC X(10).
           
      *================================================================*
      * COPYBOOK: LKTF.cpy                                             *
      * DESCRIPCION: AREA DE COMUNICACION EXCLUSIVA PARA TRICKLE FEED  *
      * USO: LINKAGE SECTION EN TFFILE, TFMX, TFBATFIN, XXXREP Y CORE  *
      *================================================================*
       01  LK-TRICKLE-FEED-INTERFACE.
      * ACCONES BATCH DEFINIDAS PARA EL MOTOR CONTABLE
      * 'D' = DEBITO / RETIRO
      * 'C' = CREDITO / DEPOSITO
      * 'P' = PAGO DE CREDITO (CARTERA)
      * 'R' = CONSULTA BATCH
           05 LK-TF-ACCION             PIC X(01).
              88 LK-TF-OP-DEBITO       VALUE 'D'.
              88 LK-TF-OP-CREDITO      VALUE 'C'.
              88 LK-TF-OP-PAGO-CRED    VALUE 'P'.
              88 LK-TF-OP-CONSULTA     VALUE 'R'.

      * CAMPOS DE TRANSFERENCIA DE DATOS FINANCIEROS
           05 LK-TF-CUENTA             PIC 9(12).
           05 LK-TF-NUM-CREDITO        PIC X(20).
           05 LK-TF-MONTO              PIC S9(13)V99.

      * MONITOREO Y CAPTURA DE ERRORES (AUDITORÍA SATÉLITE)
      * 00 = OK / 07 = INSFONDOS / 01 = NO EXISTE / 99 = ERR-DB
           05 LK-TF-COD-RETORNO        PIC 9(02).
           05 LK-TF-MENSAJE            PIC X(50).

      * CAMPOS DE TRAZABILIDAD BATCH
           05 LK-TF-LOTE-ID            PIC 9(09).
           05 LK-TF-REPLICA-ID         PIC X(04).
           05 LK-TF-ARCHIVO-PADRE      PIC X(120).
           05 LK-TF-TIMESTAMP-PROCESO  PIC 9(14).
           05 LK-TF-ESTADO-FINALIZACION PIC X(01).
              88 LK-TF-ESTADO-OK       VALUE 'S'.
              88 LK-TF-ESTADO-ERROR    VALUE 'N'.

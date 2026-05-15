      *================================================================*
      * COPYBOOK: MOVREC - REGISTRO DE MOVIMIENTOS                    *
      * BANCO LAF v2.0 - Historial Auditado                           *
      * TIPO_MOV: 2=Deposito 3=Retiro 4=Cobro Cuota                   *
      *================================================================*
       01  REG-MOVS.
           05 MOVS-ID-MOVIMIENTO       PIC 9(09).
           05 MOVS-ID-CUENTA           PIC 9(09).
           05 MOVS-TIPO-MOV            PIC 9(02).
              88 MOV-DEPOSITO          VALUE 2.
              88 MOV-RETIRO            VALUE 3.
              88 MOV-COBRO-CUOTA       VALUE 4.
           05 MOVS-IMPORTE             PIC S9(15)V99.
           05 MOVS-SALDO-RESULTANTE    PIC S9(15)V99.
           05 MOVS-TERMINAL-ID         PIC X(04).
           05 MOVS-USUARIO-ID          PIC X(08).

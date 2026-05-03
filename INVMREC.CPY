      * TABLA INVM - MAESTRO DE CUENTAS CORRIENTES Y DEPOSITOS
       01  REG-INVM.
           05 INVM-ID-CLIENTE      PIC 9(08).
           05 INVM-COD-ULT-MOV     PIC 9(02).
           05 INVM-FECHA-ULT-MOV   PIC X(08).
           05 INVM-IMPORTE-MOV     PIC S9(10)V99 COMP-3.
      *    El signo (S) permite saldos negativos si hay sobregiro
           05 INVM-SALDO-ACTUAL    PIC S9(10)V99 COMP-3.

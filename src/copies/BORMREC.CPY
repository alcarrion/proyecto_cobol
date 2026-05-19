      *================================================================*
      * COPYBOOK: MOVREC                                               *
      * DESCRIPCION: REGISTRO DEL LIBRO MAYOR DE MOVIMIENTOS           *
      * ADAPTACIÓN: ESPEJO EXACTO DE LA TABLA 'MOVIMIENTOS' DEL EQUIPO *
      *================================================================*
       01  REG-MOV.
           05 MOV-ID-MOVIMIENTO        PIC 9(08).
           05 MOV-ID-CUENTA            PIC 9(08).
           05 MOV-FECHA-HORA           PIC X(19).
           05 MOV-TIPO-MOV             PIC 9(01).
              88 MOV-DEPOSITO          VALUE 2.
              88 MOV-RETIRO            VALUE 3.
              88 MOV-COBRO-CUOTA       VALUE 4.
           05 MOV-IMPORTE              PIC S9(13)V99 COMP-3.
           05 MOV-SALDO-RESULTANTE     PIC S9(13)V99 COMP-3.
           05 MOV-TERMINAL-ID          PIC X(04).
           05 MOV-USUARIO-ID           PIC X(08).
           
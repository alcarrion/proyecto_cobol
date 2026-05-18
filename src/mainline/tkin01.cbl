       IDENTIFICATION DIVISION.
       PROGRAM-ID. tkin01.
      *================================================================*
      * PROGRAMA: tkin01.cob                                           *
      * RESPONSABILIDAD: Router General de Negocio Contable            *
      * FUNCIÓN: Evalúa el tipo de producto y deriva al sub-programa    *
      * especialista para aislar los entornos de memoria.      *
      *================================================================*
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Aquí definiremos variables de control internas si se requieren

       LINKAGE SECTION.
      * Estructura unificada que viene desde TFBATFIN
       01  REG-CTA.
           05 CTA-NRO-CUENTA       PIC X(12).
           05 CTA-NUM-CREDITO      PIC X(20).
           05 CTA-SALDO-ACTUAL     PIC S9(13)V99.
           05 CTA-MONTO-MOV        PIC 9(13)V99.

           COPY LKTF. 
       PROCEDURE DIVISION USING REG-CTA, LK-TRICKLE-FEED-INTERFACE.
       0000-PRINCIPAL.
           MOVE 0 TO LK-TF-COD-RETORNO
           MOVE "OK" TO LK-TF-MENSAJE

      *----------------------------------------------------------------*
      * RUTEO DRÁSTICO Y MODULAR DE PRODUCTOS BANCARIOS
      *----------------------------------------------------------------*
           EVALUATE LK-TF-ACCION
      * Caso Cuentas Corrientes / Ahorros (Depósitos "C" y Retiros "D")
               WHEN "C"
               WHEN "D"
                   CALL "tkin_dda" USING REG-CTA, 
                                         LK-TRICKLE-FEED-INTERFACE
                   
      * Caso Amortización de Préstamos / Hipotecas ("P")
               WHEN "P"
                   CALL "tkin_hip" USING REG-CTA, 
                                         LK-TRICKLE-FEED-INTERFACE
                   
      * Caso Consumos / Diferidos de Tarjetas de Crédito ("T")
               WHEN "T"
                   CALL "tkin_tar" USING REG-CTA, 
                                         LK-TRICKLE-FEED-INTERFACE

               WHEN OTHER
                   MOVE 99 TO LK-TF-COD-RETORNO
                   MOVE "ERR: ACCION CONTABLE NO IMPLEMENTADA" 
                     TO LK-TF-MENSAJE
           END-EVALUATE.

           GOBACK.
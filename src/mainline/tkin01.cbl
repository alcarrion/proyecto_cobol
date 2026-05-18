       IDENTIFICATION DIVISION.
       PROGRAM-ID. tkin01.
      *================================================================*
      * PROGRAMA: tkin01.cob                                           *
      * RESPONSABILIDAD: Router General de Negocio Contable            *
      * CORRECCIÓN: Alineación estricta de PIC 9(09) con TFBATFIN y    *
      * desempaquetado dinámico para tkin_hip y tkin_tarj.             *
      *================================================================*
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Variables de aislamiento para adaptar firmas de subprogramas
       01  WS-LOCAL-MAPPING.
           05 WS-HIP-ID           PIC 9(10).
           05 WS-HIP-MORA         PIC 9(02).
           05 WS-TARJ-CTA         PIC 9(10).
           05 WS-TARJ-PAN         PIC X(16).

       LINKAGE SECTION.
      * CORRECCIÓN: Estructura unificada alineada numéricamente al 100%
       01  REG-CTA.
           05 CTA-NRO-CUENTA       PIC 9(09).
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
                   
                   MOVE CTA-NRO-CUENTA TO WS-HIP-ID
                   IF CTA-NUM-CREDITO IS NUMERIC
                       MOVE FUNCTION NUMVAL(CTA-NUM-CREDITO) TO
                        WS-HIP-MORA
                   ELSE
                       MOVE 0 TO WS-HIP-MORA
                   END-IF
                   
                   CALL "tkin_hip" USING WS-HIP-ID, 
                                         WS-HIP-MORA,
                                         CTA-MONTO-MOV, 
                                         LK-TRICKLE-FEED-INTERFACE
                   
      * Caso Consumos / Diferidos de Tarjetas de Crédito ("T")
               WHEN "T"
                  
                   MOVE CTA-NRO-CUENTA TO WS-TARJ-CTA
                   MOVE CTA-NUM-CREDITO(1:16) TO WS-TARJ-PAN
                   
                   CALL "tkin_tarj" USING WS-TARJ-CTA, 
                                          WS-TARJ-PAN,
                                          CTA-MONTO-MOV, 
                                          LK-TRICKLE-FEED-INTERFACE

               WHEN OTHER
                   MOVE 99 TO LK-TF-COD-RETORNO
                   MOVE "ERR: ACCION CONTABLE NO IMPLEMENTADA" 
                     TO LK-TF-MENSAJE
           END-EVALUATE.

           GOBACK.
           
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYSLOG.
      *================================================================*
      * MODULO: SYSLOG - Registro de auditoria en archivo de log       *
      * USO: CALL 'SYSLOG' USING WS-LOG-MSG (PIC X(80))               *
      * Genera: audit.log en el directorio de ejecucion                *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUDIT-FILE ASSIGN TO "..\docs\logs\audit.log"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-LOG-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  AUDIT-FILE.
       01  AUDIT-RECORD     PIC X(120).
       WORKING-STORAGE SECTION.
       01  WS-LOG-STATUS    PIC X(02).
       01  WS-DATE-RAW      PIC X(08).
       01  WS-TIME-RAW      PIC X(08).
       01  WS-DATE-FMT      PIC X(10).
       01  WS-TIME-FMT      PIC X(08).
       LINKAGE SECTION.
       01  LK-LOG-MSG        PIC X(80).
       PROCEDURE DIVISION USING LK-LOG-MSG.
           ACCEPT WS-DATE-RAW FROM DATE YYYYMMDD
           ACCEPT WS-TIME-RAW FROM TIME
           STRING WS-DATE-RAW(1:4) "-" WS-DATE-RAW(5:2)
                  "-" WS-DATE-RAW(7:2)
                  DELIMITED BY SIZE INTO WS-DATE-FMT
           STRING WS-TIME-RAW(1:2) ":" WS-TIME-RAW(3:2)
                  ":" WS-TIME-RAW(5:2)
                  DELIMITED BY SIZE INTO WS-TIME-FMT
           OPEN EXTEND AUDIT-FILE
           IF WS-LOG-STATUS NOT = "00"
               OPEN OUTPUT AUDIT-FILE
           END-IF
           IF WS-LOG-STATUS = "00"
               MOVE SPACES TO AUDIT-RECORD
               STRING WS-DATE-FMT " " WS-TIME-FMT " " LK-LOG-MSG
                      DELIMITED BY SIZE INTO AUDIT-RECORD
               WRITE AUDIT-RECORD
               CLOSE AUDIT-FILE
           END-IF
           GOBACK.

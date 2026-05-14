       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYSLOG.
      *================================================================*
      * MODULO: SYSLOG - Registro de auditoria en archivo de log       *
      * USO: CALL 'SYSLOG' USING WS-LOG-MSG (PIC X(200))              *
      * Genera: audit.log en docs/logs desde bin/                      *
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
       01  AUDIT-RECORD     PIC X(220).
       WORKING-STORAGE SECTION.
       01  WS-LOG-STATUS    PIC X(02).
       01  WS-DATE-STRUCT.
           05 WS-ANIO       PIC X(4).
           05 WS-MES        PIC X(2).
           05 WS-DIA        PIC X(2).
       01  WS-TIME-STRUCT.
           05 WS-HORA       PIC X(2).
           05 WS-MIN        PIC X(2).
           05 WS-SEG        PIC X(2).
           05 FILLER        PIC X(2).
       01  WS-DATE-FMT      PIC X(10).
       01  WS-TIME-FMT      PIC X(08).
       LINKAGE SECTION.
       01  LK-LOG-MSG        PIC X(200).
       PROCEDURE DIVISION USING LK-LOG-MSG.
           ACCEPT WS-DATE-STRUCT FROM DATE YYYYMMDD
           ACCEPT WS-TIME-STRUCT FROM TIME
           MOVE SPACES TO WS-DATE-FMT
           STRING WS-ANIO "-" WS-MES "-" WS-DIA
                  DELIMITED BY SIZE INTO WS-DATE-FMT
           END-STRING
           MOVE SPACES TO WS-TIME-FMT
           STRING WS-HORA ":" WS-MIN ":" WS-SEG
                  DELIMITED BY SIZE INTO WS-TIME-FMT
           END-STRING
           OPEN EXTEND AUDIT-FILE
           IF WS-LOG-STATUS NOT = "00"
               OPEN OUTPUT AUDIT-FILE
           END-IF
           IF WS-LOG-STATUS = "00"
               MOVE SPACES TO AUDIT-RECORD
               STRING WS-DATE-FMT " " WS-TIME-FMT " " LK-LOG-MSG
                      DELIMITED BY SIZE INTO AUDIT-RECORD
               END-STRING
               WRITE AUDIT-RECORD
               CLOSE AUDIT-FILE
           END-IF
           GOBACK.

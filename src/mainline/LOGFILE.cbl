       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGFILE.
      *================================================================*
      * MODULO  : LOGFILE                                              *
      * FUNCION : Registro de auditoria por modulo y periodo            *
      * USO     : CALL 'LOGFILE' USING LK-MODULO LK-PERIODO LK-MSG     *
      *           LK-MODULO  PIC X(03)  ej: 'BAT', 'RPT'                *
      *           LK-PERIODO PIC X(06)  ej: '202605'                    *
      *           LK-MSG     PIC X(200) mensaje a registrar              *
      * SALIDA  : ..\docs\logs\<MODULO>_LOGS_<PERIODO>.txt              *
      *           Formato: YYYY-MM-DD HH:MM:SS <mensaje>                *
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-FILE ASSIGN TO WS-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LOG-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOG-FILE.
       01  LOG-RECORD       PIC X(250).

       WORKING-STORAGE SECTION.
       01  WS-LOG-STATUS    PIC X(02).
       01  WS-FILENAME      PIC X(80).

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
       01  LK-MODULO        PIC X(03).
       01  LK-PERIODO       PIC X(06).
       01  LK-MSG           PIC X(200).

       PROCEDURE DIVISION USING LK-MODULO, LK-PERIODO, LK-MSG.

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

      *    Construir nombre de archivo: ..\docs\logs\<MOD>_LOGS_<PER>.txt
           MOVE SPACES TO WS-FILENAME
           STRING "..\docs\logs\" DELIMITED BY SIZE
                  LK-MODULO       DELIMITED BY SIZE
                  "_LOGS_"        DELIMITED BY SIZE
                  LK-PERIODO      DELIMITED BY SIZE
                  ".txt"          DELIMITED BY SIZE
                  INTO WS-FILENAME
           END-STRING

      *    Abrir en modo append. Si no existe, crear.
           OPEN EXTEND LOG-FILE
           IF WS-LOG-STATUS NOT = "00"
               OPEN OUTPUT LOG-FILE
           END-IF

           IF WS-LOG-STATUS = "00"
               MOVE SPACES TO LOG-RECORD
               STRING WS-DATE-FMT " " WS-TIME-FMT " " LK-MSG
                      DELIMITED BY SIZE INTO LOG-RECORD
               END-STRING
               WRITE LOG-RECORD
               CLOSE LOG-FILE
           END-IF

           GOBACK.

       END PROGRAM LOGFILE.

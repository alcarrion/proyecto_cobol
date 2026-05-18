      *================================================================*
      * ARCHIVO: PATHS.CPY
      * PROPOSITO: Centralizar todas las rutas del proyecto
      * VERSION: 1.0
      * FECHA: 2026-05-13
      * USO: COPY PATHS-FILE FROM "..\copies\PATHS.CPY"
      *      O simplemente: COPY PATHS-FILE
      *================================================================*
      
       01  PATHS-CONFIG.
           05 PATH-PROYECTO     PIC X(100) 
              VALUE ".".
           05 PATH-SRC          PIC X(100)
              VALUE ".\src".
           05 PATH-MAINLINE     PIC X(100)
              VALUE ".\src\mainline".
           05 PATH-DBIO         PIC X(100)
              VALUE ".\src\dbio".
           05 PATH-COPIES       PIC X(100)
              VALUE ".\src\copies".
           05 PATH-UTILS        PIC X(100)
              VALUE ".\src\utils".
           05 PATH-SQL          PIC X(100)
              VALUE ".\sql".
           05 PATH-CONFIG       PIC X(100)
              VALUE ".\config".
           05 PATH-DOCS         PIC X(100)
              VALUE ".\docs".
           05 PATH-BIN          PIC X(100)
              VALUE ".\bin".
           05 PATH-BUILD        PIC X(100)
              VALUE ".\build".
           05 PATH-BUILD-LOGS   PIC X(100)
              VALUE ".\build\logs".
           05 PATH-TEST         PIC X(100)
              VALUE ".\test".
           05 PATH-SCHEMA       PIC X(100)
              VALUE ".\sql\schema".
       
       01  ARCHIVOS-CONFIG.
           05 FIL-DB-CONFIG     PIC X(30)
              VALUE "db_config.cfg".
           05 FIL-BDD-TXT       PIC X(30)
              VALUE "bdd.txt".
           05 FIL-ENV-PROP      PIC X(30)
              VALUE "environment.properties".
           05 FIL-PATHS-CFG     PIC X(30)
              VALUE "paths.cfg".
       
       01  ARCHIVOS-SQLSCRIPTS.
           05 FIL-DBIOCUSM-SQL  PIC X(30)
              VALUE "DBIOCUSM.sqb".
           05 FIL-DBIOBORM-SQL  PIC X(30)
              VALUE "DBIOBORM.sqb".
           05 FIL-DBIOINVM-SQL  PIC X(30)
              VALUE "DBIOINVM.sqb".
           05 FIL-DBIOTARJ-SQL  PIC X(30)
              VALUE "DBIOTARJ.sqb".
           05 FIL-DBIOTRAN-SQL  PIC X(30)
              VALUE "DBIOTRAN.sqb".
           05 FIL-TESTCONN-SQL  PIC X(30)
              VALUE "testconn.sqb".
       
       01  ARCHIVOS-PROGRAMAS.
           05 FIL-BANCSMENU     PIC X(30)
              VALUE "BANCSMENU.exe".
           05 FIL-CI0000        PIC X(30)
              VALUE "CI0000.exe".
           05 FIL-BR0000        PIC X(30)
              VALUE "BR0000.exe".
           05 FIL-IN0000        PIC X(30)
              VALUE "IN0000.exe".
           05 FIL-TC0000        PIC X(30)
              VALUE "TC0000.exe".
           05 FIL-DBIOCUSM      PIC X(30)
              VALUE "DBIOCUSM.exe".
           05 FIL-DBIOBORM      PIC X(30)
              VALUE "DBIOBORM.exe".
           05 FIL-DBIOINVM      PIC X(30)
              VALUE "DBIOINVM.exe".
           05 FIL-DBIOTARJ      PIC X(30)
              VALUE "DBIOTARJ.exe".
           05 FIL-DBIOTRAN      PIC X(30)
              VALUE "DBIOTRAN.exe".

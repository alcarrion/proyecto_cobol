@echo off
:: Cargar las librerías necesarias para SQL antes de lanzar el ejecutable
set "COBOL_MAIN=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL"
set "COBOL_LIBS_ESQL=C:\Program Files (x86)\OpenCobolIDE\binaries\win32\release"
set "PATH=%COBOL_MAIN%\bin;%COBOL_LIBS_ESQL%;%PATH%"
set "COB_PRELOAD=ocsql"

:: Ejecutar
echo Lanzando TFSUMM...
TFSUMM.exe
pause
@echo off
setlocal
title Sistema Bancario COBOL - Menu Principal
color 0A

:: Configurar las rutas necesarias para GnuCOBOL y OpenCobolIDE
set "COBOL_MAIN=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL"
set "COBOL_BIN=%COBOL_MAIN%\bin"
set "COBOL_LIBS_ESQL=%COBOL_MAIN%\bin\win32\release"

:: Agregar al PATH para que Windows encuentre las DLLs (libcob-4.dll, etc)
set "PATH=%COBOL_BIN%;%COBOL_LIBS_ESQL%;%PATH%"

:: Configurar librerias de COBOL para que encuentre modulos como OCSQL
set "COB_LIBRARY_PATH=%COBOL_LIBS_ESQL%"

:: Activar trazas de OCSQL para que vayan al log
set "OCSQL_TRACE=1"
set "OCESQL_TRACE=1"

:: Ejecutar el programa principal (redirigiendo los logs de SQL/stderr a un archivo)
echo Iniciando BANCSMENU.exe...
echo.
"%~dp0BANCSMENU.exe" 2>> "..\docs\logs\ocsql_debug.log"

echo.
pause

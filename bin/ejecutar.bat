@echo off
setlocal
set "COBOL_MAIN=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL"
set "COBOL_BIN=%COBOL_MAIN%\bin"
set "COBOL_LIBS_ESQL=%COBOL_MAIN%\binaries3\win32\release"
set "BIN_DIR=%~dp0"

:: Copiar DLLs necesarias (silencioso)
copy "%COBOL_BIN%\libcob-4.dll"       "%BIN_DIR%" >nul 2>&1
copy "%COBOL_BIN%\libgcc_s_dw2-1.dll" "%BIN_DIR%" >nul 2>&1
copy "%COBOL_BIN%\libgmp-10.dll"       "%BIN_DIR%" >nul 2>&1
copy "%COBOL_BIN%\libiconv-2.dll"      "%BIN_DIR%" >nul 2>&1
copy "%COBOL_BIN%\libintl-8.dll"       "%BIN_DIR%" >nul 2>&1
copy "%COBOL_BIN%\pdcurses.dll"        "%BIN_DIR%" >nul 2>&1
copy "%COBOL_BIN%\pthreadGC-3.dll"     "%BIN_DIR%" >nul 2>&1
copy "%COBOL_BIN%\libdb-6.2.dll"       "%BIN_DIR%" >nul 2>&1
copy "%COBOL_LIBS_ESQL%\ocsql.dll"     "%BIN_DIR%" >nul 2>&1

:: Suprimir logs internos del motor SQL (OCSQL_LOGLEVEL=0 silencia todo)
set "OCSQL_LOGLEVEL=0"

:: Configurar variables de entorno
set "COB_LIBRARY_PATH=%BIN_DIR%"
set "PATH=%BIN_DIR%;%COBOL_BIN%;%COBOL_LIBS_ESQL%;%PATH%"

:: Ejecutar el programa
cd "%BIN_DIR%"
cls
BANCSMENU.exe
pause

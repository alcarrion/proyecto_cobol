@echo off
color 0A
title Sistema Bancario COBOL

set ROOT=%~dp0..
set PATH=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin;C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\binaries3\win32\release;%PATH%
set COB_CONFIG_DIR=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\config
set COB_LIBRARY_PATH=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin;%ROOT%\bin
set COB_COPY_DIR=%ROOT%\src\copies

cls
echo Iniciando Sistema Bancario...
if not exist "%ROOT%\bin\logs" mkdir "%ROOT%\bin\logs"
echo Mensajes de OCSQL se guardan en "%ROOT%\bin\logs\ocsql.log"
"%ROOT%\bin\BANCSMENU.exe" 2> "%ROOT%\bin\logs\ocsql.log"
pause

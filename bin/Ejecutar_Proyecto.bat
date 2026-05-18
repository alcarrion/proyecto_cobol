@echo off
color 0A
title Sistema Bancario COBOL

set ROOT=%~dp0..
set PATH=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin;C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\binaries3\win32\release;%PATH%
set COB_CONFIG_DIR=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\config
set COB_LIBRARY_PATH=%ROOT%\bin
set COB_COPY_DIR=%ROOT%\src\copies

cls
echo Iniciando Sistema Bancario...
"%ROOT%\bin\BANCSMENU.exe"
pause

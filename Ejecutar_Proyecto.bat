@echo off
color 0A
title Sistema Bancario COBOL
set PATH=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin;C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\binaries3\win32\release;%PATH%
set COB_CONFIG_DIR=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\config
set COB_LIBRARY_PATH=C:\Users\dell\Documents\proyecto_cobol
cls
echo Iniciando Sistema Bancario...
BANCSMENU.exe
pause

@echo off
color 0B
title Compilador - Sistema Bancario COBOL

set ROOT=%~dp0..
set PATH=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin;%PATH%
set COBC="C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin\cobc.exe"
set COB_CONFIG_DIR=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\config
set COB_COPY_DIR=%ROOT%\src\copies

echo ============================================
echo  Compilando Sistema Bancario COBOL
echo ============================================
echo.

%COBC% -x -I "%ROOT%\src\copies" ^
    "%ROOT%\src\mainline\BANCSMENU.cob" ^
    "%ROOT%\src\mainline\CI0000.cbl" ^
    "%ROOT%\src\mainline\IN0000.cbl" ^
    "%ROOT%\src\mainline\TC0000.cbl" ^
    "%ROOT%\src\mainline\BR0000.cbl" ^
    "%ROOT%\src\mainline\DBIOCUSM.cob" ^
    "%ROOT%\src\mainline\DBIOINVM.cob" ^
    "%ROOT%\src\mainline\DBIOTARJ.cob" ^
    "%ROOT%\src\mainline\DBIOBORM.cob" ^
    "%ROOT%\src\mainline\DBIOTRAN.cob" ^
    -o "%ROOT%\bin\BANCSMENU.exe"

if %ERRORLEVEL% == 0 (
    echo.
    echo Compilacion exitosa: %ROOT%\bin\BANCSMENU.exe
) else (
    echo.
    echo ERROR: La compilacion fallo. Revisa los mensajes anteriores.
)

pause

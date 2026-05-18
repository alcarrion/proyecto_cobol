@echo off
color 0B
title Compilador - Sistema Bancario COBOL

set ROOT=%~dp0..
set COBC=cobc

echo ============================================
echo  Compilando Sistema Bancario COBOL
echo ============================================
echo.

%COBC% -x -free -I "%ROOT%\src\copies" ^
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

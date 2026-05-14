@echo off
setlocal
color 0B
title Compilador - Sistema Bancario COBOL

:: 1. Configurar las rutas
set "COBOL_MAIN=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL"
set "COBOL_BIN=%COBOL_MAIN%\bin"
set "COBOL_LIBS_ESQL=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\binaries\win32\release"

:: Variables de entorno
set "COB_MAIN_DIR=%COBOL_MAIN%"
set "COB_CONFIG_DIR=%COBOL_MAIN%\config"
set "PATH=%COBOL_BIN%;%COBOL_LIBS_ESQL%;%PATH%"

:: 2. Definir rutas del proyecto
set "ROOT=%~dp0.."
set "SQL_DIR=%ROOT%\sql"
set "MAINLINE_DIR=%ROOT%\src\mainline"
set "BIN_DIR=%ROOT%\bin"
set "COPIES_DIR=%ROOT%\src\copies"
set "COBCPY=%COPIES_DIR%"

set "COBC=%COBOL_BIN%\cobc.exe"
:: Nota: Según la arquitectura de 3 capas, el precompilador suele ser esq10C.exe [cite: 172, 179]
set "PRECOMPILADOR=%COBOL_LIBS_ESQL%\esqlOC.exe"

echo ============================================
echo  1. Pre-compilando archivos SQL (.sqb)
echo ============================================

for %%f in ("%SQL_DIR%\*.sqb") do (
    echo Procesando: %%~nxf ...
    "%PRECOMPILADOR%" -I "%COPIES_DIR%" -static -o "%MAINLINE_DIR%\%%~nf.cob" "%%f"
)

echo.
echo ============================================
echo  2. Compilando Sistema Bancario COBOL
echo ============================================

:: Compilación y enlace de todos los módulos 
:: IMPORTANTE: No dejar líneas en blanco entre los archivos con '^'
"%COBC%" -x -v -I "%COPIES_DIR%" ^
    -L "%COBOL_LIBS_ESQL%" ^
    -locsql ^
    "%MAINLINE_DIR%\BANCSMENU.cob" ^
    "%MAINLINE_DIR%\CI0000.cbl" ^
    "%MAINLINE_DIR%\IN0000.cbl" ^
    "%MAINLINE_DIR%\TC0000.cbl" ^
    "%MAINLINE_DIR%\BR0000.cbl" ^
    "%MAINLINE_DIR%\DBIOCUSM.cob" ^
    "%MAINLINE_DIR%\DBIOINVM.cob" ^
    "%MAINLINE_DIR%\DBIOTARJ.cob" ^
    "%MAINLINE_DIR%\DBIOBORM.cob" ^
    "%MAINLINE_DIR%\DBIOTRAN.cob" ^
    "%MAINLINE_DIR%\BNCR004.cob" ^
    "%MAINLINE_DIR%\TFMX.cob" ^
    "%MAINLINE_DIR%\RRD000.cob" ^
    "%MAINLINE_DIR%\XXXREP.cob" ^
    "%MAINLINE_DIR%\TFTRCT.cob" ^
    -o "%BIN_DIR%\BANCSMENU.exe"

echo.
echo ============================================
echo  3. Compilando TFDRMAIN (Motor Batch/Demonio)
echo ============================================

"%COBC%" -x -v -I "%COPIES_DIR%" ^
    -L "%COBOL_LIBS_ESQL%" ^
    -locsql ^
    "%MAINLINE_DIR%\TFDRMAIN.cob" ^
    "%MAINLINE_DIR%\BNCR004.cob" ^
    "%MAINLINE_DIR%\TFMX.cob" ^
    "%MAINLINE_DIR%\RRD000.cob" ^
    "%MAINLINE_DIR%\XXXREP.cob" ^
    "%MAINLINE_DIR%\TFTRCT.cob" ^
    "%MAINLINE_DIR%\DBIOCUSM.cob" ^
    "%MAINLINE_DIR%\DBIOINVM.cob" ^
    "%MAINLINE_DIR%\DBIOTARJ.cob" ^
    "%MAINLINE_DIR%\DBIOBORM.cob" ^
    "%MAINLINE_DIR%\DBIOTRAN.cob" ^
    "%MAINLINE_DIR%\IN0000.cbl" ^
    "%MAINLINE_DIR%\BR0000.cbl" ^
    -o "%BIN_DIR%\TFDRMAIN.exe"

if %ERRORLEVEL% == 0 (
    echo.
    echo EXITO: Ambos ejecutables generados en /bin
) else (
    echo.
    echo ERROR: Fallo la compilacion de los modulos.
)
pause
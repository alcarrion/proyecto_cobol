@echo off
setlocal
color 0B
title Compilador - Sistema Bancario COBOL

:: 1. Configurar las rutas
set "COBOL_MAIN=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL"
set "COBOL_BIN=%COBOL_MAIN%\bin"
set "COBOL_LIBS_ESQL=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\binaries\win32\release"

:: Variables de entorno vitales
set "COB_MAIN_DIR=%COBOL_MAIN%"
set "COB_CONFIG_DIR=%COBOL_MAIN%\config"
:: MUY IMPORTANTE: Agregar BIN y la carpeta del precompilador al PATH
set "PATH=%COBOL_BIN%;%COBOL_LIBS_ESQL%;%PATH%"

:: 2. Definir rutas del proyecto
set "ROOT=%~dp0.."
set "SQL_DIR=%ROOT%\sql"
set "MAINLINE_DIR=%ROOT%\src\mainline"
set "BIN_DIR=%ROOT%\bin"
set "COPIES_DIR=%ROOT%\src\copies"
set "COBCPY=%COPIES_DIR%"

set "COBC=%COBOL_BIN%\cobc.exe"
set "PRECOMPILADOR=%COBOL_LIBS_ESQL%\esqlOC.exe"

echo ============================================
echo  1. Pre-compilando archivos SQL (.sqb)
echo ============================================

for %%f in ("%SQL_DIR%\*.sqb") do (
    echo Procesando: %%~nxf ...
    :: Agregamos -static y verificamos la salida
    "%PRECOMPILADOR%" -I "%COPIES_DIR%" -static -o "%MAINLINE_DIR%\%%~nf.cob" "%%f"
)

echo.
echo ============================================
echo  2. Compilando Sistema Bancario COBOL
echo ============================================

:: Agregamos -L para las librerías y -l para enlazar el motor SQL
:: Nota: Reemplaza 'ocsql' por el nombre real de la lib si es diferente (ej. cobsql)
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
    -o "%BIN_DIR%\BANCSMENU.exe"

if %ERRORLEVEL% == 0 (
    echo.
    echo EXITO: Compilacion finalizada.
) else (
    echo.
    echo ERROR: El compilador dio un error critico.
)
pause
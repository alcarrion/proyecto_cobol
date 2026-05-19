@echo off
setlocal
color 0B
title Compilador - Sistema Bancario COBOL (EPN Core 2026)

:: 1. Configurar las rutas del compilador GnuCOBOL
set "COBOL_MAIN=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL"
set "COBOL_BIN=%COBOL_MAIN%\bin"
set "COBOL_LIBS_ESQL=%COBOL_MAIN%\binaries\win32\release"

:: Variables de entorno del sistema operativo
set "COB_MAIN_DIR=%COBOL_MAIN%"
set "COB_CONFIG_DIR=%COBOL_MAIN%\config"
set "PATH=%COBOL_BIN%;%COBOL_LIBS_ESQL%;%PATH%"

:: 2. Definir rutas del proyecto (Estructura EPN Core)
set "ROOT=%~dp0.."
set "SQL_DIR=%ROOT%\sql"
set "MAINLINE_DIR=%ROOT%\src\mainline"
set "BIN_DIR=%ROOT%\bin"
set "COPIES_DIR=%ROOT%\src\copies"
set "COBCPY=%COPIES_DIR%"

set "COBC=%COBOL_BIN%\cobc.exe"
set "PRECOMPILADOR=%COBOL_LIBS_ESQL%\esqlOC.exe"

:: Variable de control para detectar fallos intermedios
set "COMPILATION_FAILED=0"

echo ============================================
echo  1. Pre-compilando archivos SQL (.sqb)
echo ============================================

for %%f in ("%SQL_DIR%\*.sqb") do (
    echo Procesando precompilacion: %%~nxf ...
    "%PRECOMPILADOR%" -I "%COPIES_DIR%" -static -o "%MAINLINE_DIR%\%%~nf.cob" "%%f"
    if errorlevel 1 set "COMPILATION_FAILED=1"
)

if %COMPILATION_FAILED% == 1 goto finalizar

echo.
echo ============================================
echo  2. Generando Modulos Objeto Individuales (.o)
echo ============================================
cd /d "%MAINLINE_DIR%"

for %%g in (*.cob *.cbl) do (
    echo Compilando modulo aislado: %%g ...
    "%COBC%" -c -I "%COPIES_DIR%" "%%g"
    if errorlevel 1 set "COMPILATION_FAILED=1"
)

if %COMPILATION_FAILED% == 1 goto finalizar

echo.
echo ============================================
echo  3. Enlazando Componentes Contables Masivos
echo ============================================

echo Enlazando TFDRMAIN.exe (Stage 2: Orchestrator Engine)...
:: CORRECCIÓN CRÍTICA: Se añade tkin_tarj.o para soportar transacciones 004
"%COBC%" -x -v -o "%BIN_DIR%\TFDRMAIN.exe" ^
    TFDRMAIN.cob ^
    TFFILE.o BNCR004.o TFMX.o RRD000.o XXXREP.o ^
    TFTRCT.o TFBATFIN.o tkin01.o tkin_dda.o tkin_hip.o tkin_tarj.o ^
    DBIOCUSM.o DBIOINVM.o DBIOTARJ.o ^
    IN0000.o BR0000.o DBIOBORM.o DBIOTRAN.o DF0000.o ^
    -L "%COBOL_LIBS_ESQL%" -locsql
if errorlevel 1 set "COMPILATION_FAILED=1"

echo.
echo Enlazando TFDRFILE.exe (Stage 1: Ingestion Engine)...
"%COBC%" -x -v -o "%BIN_DIR%\TFDRFILE.exe" ^
    TFDRFILE.cob ^
    TFFILE.o BNCR004.o ^
    -L "%COBOL_LIBS_ESQL%" -locsql
if errorlevel 1 set "COMPILATION_FAILED=1"

echo.
echo Enlazando Menu de Interfaz Online BANCSMENU.exe...
"%COBC%" -x -v -o "%BIN_DIR%\BANCSMENU.exe" ^
    BANCSMENU.cob ^
    CI0000.o IN0000.o TC0000.o BR0000.o ^
    BAT000.o RP0000.o LOGFILE.o ^
    DBIOCUSM.o DBIOINVM.o DBIOTARJ.o DBIOBORM.o DBIOTRAN.o ^
    -L "%COBOL_LIBS_ESQL%" -locsql
if errorlevel 1 set "COMPILATION_FAILED=1"

echo.
echo Limpiando archivos objeto temporales de la compilacion...
del *.o > nul 2>&1

:finalizar
echo.
echo ============================================
echo  VERIFICACION FINAL DEL ENTRAMADO BATCH
echo ============================================

if %COMPILATION_FAILED% == 0 (
    echo.
    echo [OK] EXITO: Todos los ejecutables financieros generados en /bin
    echo       - BANCSMENU.exe (Online Menu Core)
    echo       - TFDRMAIN.exe  (Stage 2: Orchestrator Engine)
    echo       - TFDRFILE.exe  (Stage 1: Ingestion Engine)
    echo.
) else (
    echo.
    echo [X] ERROR CRITICO: Fallo la compilacion de uno o mas modulos bancarios.
    echo     Por favor, revise las lineas de error impresas en la consola.
    echo.
)

pause
exit /b %COMPILATION_FAILED%

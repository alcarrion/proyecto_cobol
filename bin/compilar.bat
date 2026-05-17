@echo off
setlocal
color 0B
title Compilador - Sistema Bancario COBOL

:: 1. Configurar las rutas del compilador GnuCOBOL
set "COBOL_MAIN=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL"
set "COBOL_BIN=%COBOL_MAIN%\bin"
set "COBOL_LIBS_ESQL=C:\Program Files (x86)\OpenCobolIDE\binaries\win32\release"

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
echo  2. Generando Módulos Objeto Individuales (.o)
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
echo  3. Enlazando Binarios Core (Linkage Phase)
echo ============================================

:: CORRECCIÓN: El archivo principal se pasa como .cob para inyectar el Main Entry Point de consola
echo Enlazando BANCSMENU.exe (Interfaz Core Online)...
"%COBC%" -x -v -o "%BIN_DIR%\BANCSMENU.exe" ^
    BANCSMENU.cob ^
    TFFILE.o CI0000.o IN0000.o TC0000.o BR0000.o ^
    DBIOCUSM.o DBIOINVM.o DBIOTARJ.o DBIOBORM.o DBIOTRAN.o ^
    BNCR004.o TFMX.o RRD000.o XXXREP.o TFTRCT.o TFBATFIN.o tkin01.o ^
    -L "%COBOL_LIBS_ESQL%" -locsql
if errorlevel 1 set "COMPILATION_FAILED=1"

echo Enlazando TFDRMAIN.exe (Orquestador Batch)...
"%COBC%" -x -v -o "%BIN_DIR%\TFDRMAIN.exe" ^
    TFDRMAIN.cob ^
    TFFILE.o BNCR004.o TFMX.o RRD000.o XXXREP.o ^
    TFTRCT.o TFBATFIN.o tkin01.o DBIOCUSM.o DBIOINVM.o DBIOTARJ.o ^
    DBIOBORM.o DBIOTRAN.o IN0000.o BR0000.o ^
    -L "%COBOL_LIBS_ESQL%" -locsql
if errorlevel 1 set "COMPILATION_FAILED=1"

echo Enlazando TFDRFILE.exe (Lanzador Driver + Ingesta)...
"%COBC%" -x -v -o "%BIN_DIR%\TFDRFILE.exe" ^
    TFDRFILE.cob ^
    TFFILE.o BNCR004.o ^
    -L "%COBOL_LIBS_ESQL%" -locsql
if errorlevel 1 set "COMPILATION_FAILED=1"

echo.
echo Limpiando archivos objeto temporales...
del *.o > nul 2>&1

:finalizar
echo.
echo ============================================
echo  VERIFICACION FINAL DEL ENTRAMADO BATCH
echo ============================================

if %COMPILATION_FAILED% == 0 (
    echo.
    echo [OK] EXITO: Todos los ejecutables financieros generados en /bin
    echo      - BANCSMENU.exe (Online Menu Core)
    echo      - TFDRMAIN.exe  (Stage 2: Orchestrator Engine)
    echo      - TFDRFILE.exe  (Stage 1: Ingestion Engine)
) else (
    echo.
    echo [X] ERROR CRITICO: Fallo la compilacion de uno o mas modulos.
    echo     Por favor, revise las lineas de error del compilador mas arriba.
)

pause
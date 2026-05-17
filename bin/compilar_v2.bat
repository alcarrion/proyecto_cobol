@echo off
REM ================================================================
REM  SISTEMA DE COMPILACION PROFESIONAL - CORE BANCARIO
REM  GnuCOBOL + MySQL / OpenCOBOL IDE
REM  Autor: Arquitecto COBOL Senior
REM  Fecha: 2026-05-16
REM ================================================================

setlocal enabledelayedexpansion
color 0B
title [COMPILER] - Sistema Bancario COBOL - Build Engine

REM ================================================================
REM  1. CONFIGURACION DE RUTAS
REM ================================================================

set "COBOL_MAIN=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL"
set "COBOL_BIN=%COBOL_MAIN%\bin"
set "COBOL_LIBS_ESQL=C:\Program Files (x86)\OpenCobolIDE\binaries\win32\release"

set "COB_MAIN_DIR=%COBOL_MAIN%"
set "COB_CONFIG_DIR=%COBOL_MAIN%\config"
set "PATH=%COBOL_BIN%;%COBOL_LIBS_ESQL%;%PATH%"

set "ROOT=%~dp0.."
set "SQL_DIR=%ROOT%\sql"
set "MAINLINE_DIR=%ROOT%\src\mainline"
set "BIN_DIR=%ROOT%\bin"
set "COPIES_DIR=%ROOT%\src\copies"
set "LOGS_DIR=%BIN_DIR%\logs"

REM Crear directorio de logs si no existe
if not exist "%LOGS_DIR%" mkdir "%LOGS_DIR%"

set "COBCPY=%COPIES_DIR%"
set "COBC=%COBOL_BIN%\cobc.exe"
set "PRECOMPILADOR=%COBOL_LIBS_ESQL%\esqlOC.exe"

REM ================================================================
REM  2. VARIABLES DE CONTROL
REM ================================================================

set "COMPILATION_FAILED=0"
set "MODULES_COMPILED=0"
set "MODULES_FAILED=0"
set "BUILD_TIMESTAMP=%date:~6,4%%date:~3,2%%date:~0,2%_%time:~0,2%%time:~3,2%%time:~6,2%"
set "LOG_FILE=%LOGS_DIR%\build_%BUILD_TIMESTAMP%.log"

REM ================================================================
REM  3. INICIO DEL LOG
REM ================================================================

echo [BUILD START] %date% %time% >> "%LOG_FILE%"
echo ================================================================ >> "%LOG_FILE%"

cls
echo.
echo ================================================================
echo  [*] COMPILADOR COBOL - SISTEMA CORE BANCARIO
echo ================================================================
echo  Timestamp: %date% %time%
echo  Proyecto:  PROYECTO CORE TATA
echo  Target:    GnuCOBOL + MySQL
echo ================================================================
echo.

REM ================================================================
REM  FASE 1: PRE-COMPILACION SQL
REM ================================================================

echo [INFO] Iniciando pre-compilacion SQL (.sqb files)...
set "SQL_COUNT=0"
for %%f in ("%SQL_DIR%\*.sqb") do (
    set /a SQL_COUNT+=1
)

if %SQL_COUNT% equ 0 (
    echo [WARN] No hay archivos .sqb para pre-compilar
) else (
    echo [INFO] Se encontraron %SQL_COUNT% archivos SQL
    for %%f in ("%SQL_DIR%\*.sqb") do (
        echo [.] Pre-compilando: %%~nxf
        "%PRECOMPILADOR%" -I "%COPIES_DIR%" -static -o "%MAINLINE_DIR%\%%~nf.cob" "%%f" >nul 2>&1
        if !ERRORLEVEL! neq 0 (
            echo [ERROR] Fallo pre-compilacion: %%~nxf
            set "COMPILATION_FAILED=1"
            set /a MODULES_FAILED+=1
            >> "%LOG_FILE%" echo [ERROR-ESQL] %%~nxf - ERRORLEVEL: !ERRORLEVEL!
        ) else (
            echo [OK] %%~nxf
            set /a MODULES_COMPILED+=1
            >> "%LOG_FILE%" echo [OK-ESQL] %%~nxf
        )
    )
)
echo.

REM ================================================================
REM  FASE 2: COMPILACION CORE ONLINE (BANCSMENU)
REM ================================================================

echo [INFO] Compilando: BANCSMENU (Interfaz Core Online)
set "BINARY_NAME=BANCSMENU.exe"
set "LOG_MODULE=%LOGS_DIR%\build_BANCSMENU_%BUILD_TIMESTAMP%.log"

(
    "%COBC%" -x -fno-remove-unreachable-code -I "%COPIES_DIR%" ^
        -L "%COBOL_LIBS_ESQL%" -locsql ^
        "%MAINLINE_DIR%\BANCSMENU.cob" ^
        "%MAINLINE_DIR%\TFFILE.cob" ^
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
        "%MAINLINE_DIR%\TFBATFIN.cob" ^
        "%MAINLINE_DIR%\tkin01.cob" ^
        -o "%BIN_DIR%\BANCSMENU.exe"
) >"%LOG_MODULE%" 2>&1

if !ERRORLEVEL! neq 0 (
    echo [ERROR] Compilacion fallida: BANCSMENU
    set "COMPILATION_FAILED=1"
    set /a MODULES_FAILED+=1
    >> "%LOG_FILE%" echo [ERROR] BANCSMENU ERRORLEVEL: !ERRORLEVEL!
    type "%LOG_MODULE%" >> "%LOG_FILE%"
) else (
    echo [OK] BANCSMENU.exe generado exitosamente
    set /a MODULES_COMPILED+=1
    >> "%LOG_FILE%" echo [OK] BANCSMENU
)
echo.

REM ================================================================
REM  FASE 3: COMPILACION BATCH ORCHESTRATOR (TFDRMAIN)
REM ================================================================

echo [INFO] Compilando: TFDRMAIN (Orquestador Batch)
set "BINARY_NAME=TFDRMAIN.exe"
set "LOG_MODULE=%LOGS_DIR%\build_TFDRMAIN_%BUILD_TIMESTAMP%.log"

(
    "%COBC%" -x -fno-remove-unreachable-code -I "%COPIES_DIR%" ^
        -L "%COBOL_LIBS_ESQL%" -locsql ^
        "%MAINLINE_DIR%\TFDRMAIN.cob" ^
        "%MAINLINE_DIR%\TFFILE.cob" ^
        "%MAINLINE_DIR%\BNCR004.cob" ^
        "%MAINLINE_DIR%\TFMX.cob" ^
        "%MAINLINE_DIR%\RRD000.cob" ^
        "%MAINLINE_DIR%\XXXREP.cob" ^
        "%MAINLINE_DIR%\TFTRCT.cob" ^
        "%MAINLINE_DIR%\TFBATFIN.cob" ^
        "%MAINLINE_DIR%\tkin01.cob" ^
        "%MAINLINE_DIR%\DBIOCUSM.cob" ^
        "%MAINLINE_DIR%\DBIOINVM.cob" ^
        "%MAINLINE_DIR%\DBIOTARJ.cob" ^
        "%MAINLINE_DIR%\DBIOBORM.cob" ^
        "%MAINLINE_DIR%\DBIOTRAN.cob" ^
        "%MAINLINE_DIR%\IN0000.cbl" ^
        "%MAINLINE_DIR%\BR0000.cbl" ^
        -o "%BIN_DIR%\TFDRMAIN.exe"
) >"%LOG_MODULE%" 2>&1

if !ERRORLEVEL! neq 0 (
    echo [ERROR] Compilacion fallida: TFDRMAIN
    set "COMPILATION_FAILED=1"
    set /a MODULES_FAILED+=1
    >> "%LOG_FILE%" echo [ERROR] TFDRMAIN ERRORLEVEL: !ERRORLEVEL!
    type "%LOG_MODULE%" >> "%LOG_FILE%"
) else (
    echo [OK] TFDRMAIN.exe generado exitosamente
    set /a MODULES_COMPILED+=1
    >> "%LOG_FILE%" echo [OK] TFDRMAIN
)
echo.

REM ================================================================
REM  FASE 4: COMPILACION DRIVER + INGESTA (TFDRFILE)
REM ================================================================

echo [INFO] Compilando: TFDRFILE (Lanzador Driver + Ingesta)
set "BINARY_NAME=TFDRFILE.exe"
set "LOG_MODULE=%LOGS_DIR%\build_TFDRFILE_%BUILD_TIMESTAMP%.log"

(
    "%COBC%" -x -fno-remove-unreachable-code -I "%COPIES_DIR%" ^
        -L "%COBOL_LIBS_ESQL%" -locsql ^
        "%MAINLINE_DIR%\TFDRFILE.cob" ^
        "%MAINLINE_DIR%\TFFILE.cob" ^
        "%MAINLINE_DIR%\BNCR004.cob" ^
        -o "%BIN_DIR%\TFDRFILE.exe"
) >"%LOG_MODULE%" 2>&1

if !ERRORLEVEL! neq 0 (
    echo [ERROR] Compilacion fallida: TFDRFILE
    set "COMPILATION_FAILED=1"
    set /a MODULES_FAILED+=1
    >> "%LOG_FILE%" echo [ERROR] TFDRFILE ERRORLEVEL: !ERRORLEVEL!
    type "%LOG_MODULE%" >> "%LOG_FILE%"
) else (
    echo [OK] TFDRFILE.exe generado exitosamente
    set /a MODULES_COMPILED+=1
    >> "%LOG_FILE%" echo [OK] TFDRFILE
)
echo.

REM ================================================================
REM  FASE FINAL: RESUMEN Y VERIFICACION
REM ================================================================

echo ================================================================
echo  [*] RESUMEN FINAL DE COMPILACION
echo ================================================================
echo.
echo  Total Modulos Compilados: %MODULES_COMPILED%
echo  Total Modulos Fallidos:   %MODULES_FAILED%
echo  Archivo de Log:           %LOG_FILE%
echo.

>> "%LOG_FILE%" echo.
>> "%LOG_FILE%" echo [BUILD END] %date% %time%
>> "%LOG_FILE%" echo [SUMMARY] Compiled: %MODULES_COMPILED% | Failed: %MODULES_FAILED%

if %COMPILATION_FAILED% == 0 (
    echo [OK] =====================================================
    echo [OK]  EXITO: Compilacion finalizada sin errores
    echo [OK]  Ejecutables generados en: %BIN_DIR%
    echo [OK]  - BANCSMENU.exe    [Core Online]
    echo [OK]  - TFDRMAIN.exe     [Batch Orchestrator]
    echo [OK]  - TFDRFILE.exe     [Driver + Ingesta]
    echo [OK] =====================================================
    >> "%LOG_FILE%" echo [STATUS] BUILD SUCCESS
) else (
    echo [FATAL] =================================================
    echo [FATAL]  ERROR CRITICO: Compilacion con fallos
    echo [FATAL]  Revise los logs en: %LOGS_DIR%
    echo [FATAL] =================================================
    >> "%LOG_FILE%" echo [STATUS] BUILD FAILED
)

echo.
pause
endlocal

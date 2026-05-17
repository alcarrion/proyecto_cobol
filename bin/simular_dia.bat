@echo off
setlocal enabledelayedexpansion
color 0A
title Orquestador End-to-End - Core Bancario

set "INPUT_DIR=C:\banco\spool\Interfaces\BATCH-INPUT"
set "MYSQL_PATH=C:\Program Files\MySQL\MySQL Server 8.0\bin\mysql.exe"

echo ========================================================
echo  1. RESETEANDO AMBIENTE CONTABLE (MYSQL)
echo ========================================================
:: Retornamos las fases de control a 00 y vaciamos réplicas para una corrida limpia
"%MYSQL_PATH%" -u root -p -e "USE proyecto_cobol; UPDATE tffm SET FASE = '00', REPLICA_NO = NULL; TRUNCATE TABLE tf01; TRUNCATE TABLE tf02; UPDATE tf_replicas SET STATUS = 'L';"

echo.
echo ========================================================
echo  2. INYECTANDO NUEVOS ARCHIVOS AL SPOOL BATCH
echo ========================================================
if not exist "%INPUT_DIR%" mkdir "%INPUT_DIR%"

(echo CRE^|22345679^|000000000050000 && echo CRE^|22345680^|000000000125025) > "%INPUT_DIR%\CRE-999-160526-190000-160526-002.TXT"
(echo DEB^|22345681^|000000000020000 && echo DEB^|22345682^|000000000150000) > "%INPUT_DIR%\DEB-999-160526-191500-160526-002.TXT"
(echo PAG^|22345679^|CR-2026-X99^|000000000025000 && echo PAG^|22345682^|CR-2026-Z12^|000000000100000) > "%INPUT_DIR%\PAG-999-160526-193000-160526-002.TXT"

echo [OK] Archivos sembrados exitosamente en BATCH-INPUT.
pause

echo.
echo ========================================================
echo  3. EJECUTANDO STAGE 1: INGESTA Y LOTEO (TFDRFILE)
echo ========================================================
call .\TFDRFILE.exe
if %ERRORLEVEL% neq 0 (
    echo [X] Error crítico en la etapa de ingesta.
    pause
    exit /b
)
echo [OK] Ingesta culminada. Archivos fragmentados y listos en tablas.
pause

echo.
echo ========================================================
echo  4. EJECUTANDO STAGE 2: MAQUINA DE ESTADOS (TFDRMAIN)
echo ========================================================
call .\TFDRMAIN.exe
if %ERRORLEVEL% neq 0 (
    echo [X] Error crítico en la ejecución contable.
    pause
    exit /b
)

echo.
echo ========================================================
echo  [CIERRE] SIMULACION FINALIZADA CON EXITO TOTAL
echo ========================================================
pause
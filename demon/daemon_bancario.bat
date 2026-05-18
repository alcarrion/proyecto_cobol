@echo off
setlocal
color 0B
title Demonio de Control - Sistema Bancario COBOL

:: 1. Configurar las mismas rutas de librerias del compilador para tiempo de ejecucion
set "COBOL_MAIN=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL"
set "COBOL_BIN=%COBOL_MAIN%\bin"
set "COBOL_LIBS_ESQL=C:\Program Files (x86)\OpenCobolIDE\binaries\win32\release"

:: Inyectamos las rutas en el PATH de ESTA sesion para que libcob localice ocsql.dll
set "PATH=%COBOL_BIN%;%COBOL_LIBS_ESQL%;%PATH%"
set "COB_PRELOAD=ocsql"

:: 2. Cambiar al directorio de binarios relativo al demonio
cd /d "%~dp0"
cd ..\bin

:inicio
cls
echo =======================================================
echo           DEMONIO DE CONTROL 2.0 - CORE BANCARIO
echo =======================================================
echo MODO: ALTA CONCURRENCIA (6 REPLICAS)
echo ESTADO: VIGILANDO BATCH-INPUT...
echo -------------------------------------------------------
echo [%date% %time%]

:: ETAPA 1: Ingesta, Validación y Loteo
echo [STEP 1] Buscando archivos nuevos y fragmentando...
TFDRFILE.exe

:: ETAPA 2: Orquestación y Procesamiento de Réplicas
echo [STEP 2] Procesando lotes en cola y balanceando carga...
TFDRMAIN.exe

echo.
echo [%date% %time%] Ciclo de procesamiento completado.
echo Esperando 30 segundos para proximo escaneo...
echo =======================================================
timeout /t 30 /nobreak > nul
goto inicio
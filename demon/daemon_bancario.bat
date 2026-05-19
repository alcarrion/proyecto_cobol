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

:: Desactivar verbose logging de OCSQL para eliminar mensajes SQL en terminal
set "OCSQL_LOG_OFF=1"
set "COB_VERBOSE=0"

:: Directorios y logging
set "SCRIPT_DIR=%~dp0"
set "LOG_DIR=%SCRIPT_DIR%..\logs"
if not exist "%LOG_DIR%" (
	mkdir "%LOG_DIR%"
)
set "DAEMON_LOG=%LOG_DIR%\daemonio.log"
set "DAEMON_ERR=%LOG_DIR%\daemonio_err.log"

:: Modo verboso: exportar VERBOSE=1 antes de ejecutar el .bat para ver salidas en consola

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
if /I "%VERBOSE%"=="1" (
	echo [STEP 1] Buscando archivos nuevos y fragmentando...
	TFDRFILE.exe 2>> "%DAEMON_ERR%"
) else (
	echo [%date% %time%] [STEP 1] TFDRFILE start >> "%DAEMON_LOG%"
	TFDRFILE.exe >> "%DAEMON_LOG%" 2>> "%DAEMON_ERR%"
	echo [%date% %time%] [STEP 1] TFDRFILE end >> "%DAEMON_LOG%"
)

:: ETAPA 2: Orquestación y Procesamiento de Réplicas
if /I "%VERBOSE%"=="1" (
	echo [STEP 2] Procesando lotes en cola y balanceando carga...
	TFDRMAIN.exe 2>> "%DAEMON_ERR%"
) else (
	echo [%date% %time%] [STEP 2] TFDRMAIN start >> "%DAEMON_LOG%"
	TFDRMAIN.exe >> "%DAEMON_LOG%" 2>> "%DAEMON_ERR%"
	echo [%date% %time%] [STEP 2] TFDRMAIN end >> "%DAEMON_LOG%"
)

echo.
echo [%date% %time%] Ciclo de procesamiento completado.
echo Esperando 30 segundos para proximo escaneo...
echo =======================================================
timeout /t 30 /nobreak > nul
goto inicio
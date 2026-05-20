@echo off
setlocal
color 0B
title Demonio de Control - Sistema Bancario COBOL

:: --- CONFIGURACIÓN DE VISIBILIDAD ---
:: Forzamos el modo verboso a 1 para ver los DISPLAYs en consola.
:: Cambia a 0 si en producción quieres que sea totalmente silencioso.
set "VERBOSE=1"

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

:: 2. Cambiar al directorio de binarios relativo al demonio
cd /d "%~dp0"
cd ..\bin

:: Limpiamos la pantalla UNA SOLA VEZ al iniciar el demonio
cls
echo =======================================================
echo           DEMONIO DE CONTROL 2.0 - CORE BANCARIO
echo =======================================================
echo MODO: ALTA CONCURRENCIA (POOL DE WORKERS)
echo ESTADO: INICIANDO MONITOREO CONTINUO...
echo =======================================================

:inicio
echo.
echo -------------------------------------------------------
echo [%date% %time%] INICIANDO NUEVO CICLO BATCH
echo -------------------------------------------------------

:: ETAPA 1: Ingesta, Validación y Loteo
if /I "%VERBOSE%"=="1" (
    echo [*] [STEP 1] Buscando archivos nuevos y fragmentando...
    TFDRFILE.exe 2>> "%DAEMON_ERR%"
) else (
    echo [%date% %time%] [STEP 1] TFDRFILE start >> "%DAEMON_LOG%"
    TFDRFILE.exe >> "%DAEMON_LOG%" 2>> "%DAEMON_ERR%"
    echo [%date% %time%] [STEP 1] TFDRFILE end >> "%DAEMON_LOG%"
)

:: ETAPA 2: Orquestación y Procesamiento de Réplicas (MULTIHILO)
if /I "%VERBOSE%"=="1" (
    echo [*] [STEP 2] Lanzando POOL DE WORKERS para balanceo real...
    start /B cmd /c "TFDRMAIN.exe 2>> "%DAEMON_ERR%""
    start /B cmd /c "TFDRMAIN.exe 2>> "%DAEMON_ERR%""
    start /B cmd /c "TFDRMAIN.exe 2>> "%DAEMON_ERR%""
    start /B cmd /c "TFDRMAIN.exe 2>> "%DAEMON_ERR%""
    
    :: Damos tiempo al OS para que los hilos asignen las réplicas
    timeout /t 5 /nobreak > nul
) else (
    echo [%date% %time%] [STEP 2] TFDRMAIN Pool start >> "%DAEMON_LOG%"
    start /B cmd /c "TFDRMAIN.exe >> "%DAEMON_LOG%" 2>> "%DAEMON_ERR%""
    start /B cmd /c "TFDRMAIN.exe >> "%DAEMON_LOG%" 2>> "%DAEMON_ERR%""
    start /B cmd /c "TFDRMAIN.exe >> "%DAEMON_LOG%" 2>> "%DAEMON_ERR%""
    start /B cmd /c "TFDRMAIN.exe >> "%DAEMON_LOG%" 2>> "%DAEMON_ERR%""
    timeout /t 5 /nobreak > nul
)

:: ETAPA 3: Cuadratura y Acta de Cierre
:: Verificamos si la carpeta de input está vacía. Si lo está, el batch del día terminó.
dir /b "C:\banco\spool\Interfaces\BATCH-INPUT\*.TXT" > nul 2>&1
if errorlevel 1 (
    if /I "%VERBOSE%"=="1" (
        echo [*] [STEP 3] Bandeja de entrada limpia. Generando Acta de Cuadratura EOB...
        TFSUMM.exe 2>> "%DAEMON_ERR%"
    ) else (
        echo [%date% %time%] [STEP 3] TFSUMM start >> "%DAEMON_LOG%"
        TFSUMM.exe >> "%DAEMON_LOG%" 2>> "%DAEMON_ERR%"
        echo [%date% %time%] [STEP 3] TFSUMM end >> "%DAEMON_LOG%"
    )
) else (
    echo [*] [INFO] Aun hay archivos en la bandeja de entrada. Postergando acta de cierre...
)

echo.
echo [%date% %time%] Ciclo de procesamiento completado.
echo Esperando 30 segundos para proximo escaneo... (Presiona CTRL+C para detener)
timeout /t 30 /nobreak > nul
goto inicio
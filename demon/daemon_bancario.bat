@echo off
setlocal
:: %~dp0 es la ruta donde esta este .bat
cd /d "%~dp0"
cd ..\bin

:inicio
cls
color 0B
echo =======================================================
echo      DEMONIO DE CONTROL 2.0 - EPN CORE BANCARIO
echo =======================================================
echo  MODO: ALTA CONCURRENCIA (6 REPLICAS)
echo  ESTADO: VIGILANDO BATCH-INPUT...
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
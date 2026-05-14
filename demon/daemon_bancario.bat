@echo off
:: %~dp0 es la ruta donde esta este .bat (carpeta demon)
cd /d "%~dp0"
:: Subimos un nivel y entramos a bin
cd ..\bin

:inicio
cls
echo =======================================================
echo     DEMONIO DE CONTROL - EPN CORE BANCARIO
echo =======================================================
echo [%date% %time%] Escaneando base de datos por lotes...

:: Ejecutamos el motor independiente
TFDRMAIN.exe

echo.
echo [%date% %time%] Ciclo de procesamiento completado.
echo Esperando 30 segundos...
echo =======================================================
timeout /t 30 /nobreak > nul

goto inicio
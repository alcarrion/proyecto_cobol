@echo off
REM ================================================================
REM SCRIPT: Ejecutar_Proyecto.bat
REM PROPOSITO: Ejecutar aplicacion COBOL con rutas configuradas
REM VERSION: 1.0
REM FECHA: 2026-05-13
REM ================================================================

setlocal enabledelayedexpansion

REM Cargar variables de entorno
if exist config\environment.properties (
    for /f "delims== tokens=1,2" %%a in (config\environment.properties) do set %%a=%%b
)

REM Agregar bin/ al PATH para que encuentre los .exe
set PATH=%CD%\bin;%PATH%

REM Verificar que ejecutable existe
if not exist bin\BANCSMENU.exe (
    echo.
    echo ================================================================
    echo  ERROR: BANCSMENU.exe no existe
    echo ================================================================
    echo.
    echo Por favor, primero ejecuta: build.bat
    echo.
    pause
    exit /b 1
)

echo.
echo ================================================================
echo  INICIANDO APLICACION BANCARIA
echo ================================================================
echo.
echo Ruta del proyecto: %CD%
echo Ruta config:      %CONFIG_PATH%
echo Ruta bin:         %CD%\bin
echo.
echo ================================================================
echo.

REM Ejecutar programa principal
cd /d "%CD%\bin"
BANCSMENU.exe

cd /d "%CD%\.."
pause

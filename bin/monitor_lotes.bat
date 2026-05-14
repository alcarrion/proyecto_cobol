@echo off
setlocal enabledelayedexpansion
cd /d "%~dp0"
set "ROOT=%CD%\.."
for %%I in ("%ROOT%") do set "ROOT=%%~fI"
set "FOLDER_TEMP=%ROOT%\banco\spool\Interfaces\BATCH-UPLOADS-TEMP"
set "FOLDER_S=%ROOT%\banco\spool\Interfaces\BATCH-UPLOAD-S"
set "FOLDER_REP=%FOLDER_S%\TRICKLE-FEED-REPORT"
set "DB_USER=root"
set "DB_PASS=12345"
set "DB_NAME=proyecto_cobol"
set "MYSQL=C:\Program Files\MySQL\MySQL Server 8.0\bin\mysql.exe"

if not exist "%FOLDER_TEMP%" mkdir "%FOLDER_TEMP%"
if not exist "%FOLDER_S%"    mkdir "%FOLDER_S%"
if not exist "%FOLDER_REP%"  mkdir "%FOLDER_REP%"

echo Carpeta TEMP: %FOLDER_TEMP%
echo Carpeta S:    %FOLDER_S%
echo Carpeta REP:  %FOLDER_REP%
echo.

goto start_monitor

:start_monitor
cls
echo [MONITOR TRICKLE FEED] Escaneando archivos en TEMP...
if exist "%FOLDER_TEMP%\*.txt" (
    for %%f in ("%FOLDER_TEMP%\*.txt") do (
        echo Archivo detectado: %%~nxf

        :: Determinar TIPO_PROG segun prefijo del nombre (TRF_ = hipotecas, EPG = cuentas)
        set "TIPO_PROG=EPG"
        echo %%~nxf | findstr /r /c:"^TRF_" > nul
        if not errorlevel 1 set "TIPO_PROG=TRF"

        :: 1. Registrar en TFFM (Fase 00)
        echo Registrando en TFFM como tipo !TIPO_PROG!...
        "%MYSQL%" -u %DB_USER% -p%DB_PASS% %DB_NAME% -e "INSERT INTO TFFM (NOMBRE_ARCHIVO, FASE, TIPO_PROG, ESTADO_REPLICA) VALUES ('%%~nxf', '00', '!TIPO_PROG!', 'R')"
        if errorlevel 1 (
            echo [ERROR] No se pudo registrar en TFFM.
            echo         Verifique que mysql.exe este en el PATH y que la BD este corriendo.
        ) else (
            echo [OK] Lote registrado en TFFM.

            :: 2. Mover archivo a carpeta de procesamiento
            move "%%f" "%FOLDER_S%\"
            echo [OK] Archivo movido a BATCH-UPLOAD-S.
            echo.
            echo ================================================================
            echo  LOTE LISTO: %%~nxf
            echo  Para procesar: abra BANCSMENU.exe y elija la opcion 7
            echo ================================================================
        )
        echo.
    )
) else (
    echo No hay archivos pendientes.
)
timeout /t 5 > nul
goto start_monitor

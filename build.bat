@echo off
REM ================================================================
REM SCRIPT: build.bat
REM PROPOSITO: Compilar todos los programas COBOL con rutas relativas
REM VERSION: 1.0
REM FECHA: 2026-05-13
REM INSTRUCCIONES: Ejecutar desde la carpeta raiz del proyecto
REM ================================================================

setlocal enabledelayedexpansion

echo.
echo ================================================================
echo  COMPILACION DE PROGRAMAS COBOL - PROYECTO BANCARIO v1.0
echo ================================================================
echo.

REM Limpiar directorio bin (opcional)
REM del bin\*.exe >nul 2>&1

REM Cargar variables de entorno
if exist config\environment.properties (
    for /f "delims== tokens=1,2" %%a in (config\environment.properties) do set %%a=%%b
)

REM Variables locales
setlocal enabledelayedexpansion
set COBOL_FLAGS=-x -free -Wall
set COPY_PATH=-I.\src\copies -I.\src\dbio -I.\src\utils
set OUTPUT_DIR=.\bin
set ERROR_COUNT=0

echo [INFO] Copybookpath: %COPY_PATH%
echo [INFO] Output dir: %OUTPUT_DIR%
echo.

REM ===== COMPILAR MAINLINE PROGRAMS =====
echo ============ MAINLINE PROGRAMS ============
echo.

echo [1/11] Compilando BANCSMENU.cob...
cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\BANCSMENU.exe src\mainline\BANCSMENU.cob >nul 2>&1
if errorlevel 1 (
    echo   ERROR: Fallo compilacion BANCSMENU.cob
    set /a ERROR_COUNT+=1
) else (
    echo   OK: %OUTPUT_DIR%\BANCSMENU.exe
)

echo [2/11] Compilando CI0000.cbl...
cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\CI0000.exe src\mainline\CI0000.cbl >nul 2>&1
if errorlevel 1 (
    echo   ERROR: Fallo compilacion CI0000.cbl
    set /a ERROR_COUNT+=1
) else (
    echo   OK: %OUTPUT_DIR%\CI0000.exe
)

echo [3/11] Compilando BR0000.cbl...
cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\BR0000.exe src\mainline\BR0000.cbl >nul 2>&1
if errorlevel 1 (
    echo   ERROR: Fallo compilacion BR0000.cbl
    set /a ERROR_COUNT+=1
) else (
    echo   OK: %OUTPUT_DIR%\BR0000.exe
)

echo [4/11] Compilando IN0000.cbl...
cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\IN0000.exe src\mainline\IN0000.cbl >nul 2>&1
if errorlevel 1 (
    echo   ERROR: Fallo compilacion IN0000.cbl
    set /a ERROR_COUNT+=1
) else (
    echo   OK: %OUTPUT_DIR%\IN0000.exe
)

echo [5/11] Compilando TC0000.cbl...
cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\TC0000.exe src\mainline\TC0000.cbl >nul 2>&1
if errorlevel 1 (
    echo   ERROR: Fallo compilacion TC0000.cbl
    set /a ERROR_COUNT+=1
) else (
    echo   OK: %OUTPUT_DIR%\TC0000.exe
)

echo.
echo ============ DBIO PROGRAMS (DATABASE IO) ============
echo.

echo [6/11] Compilando DBIOCUSM.cob...
cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\DBIOCUSM.exe src\dbio\DBIOCUSM.cob >nul 2>&1
if errorlevel 1 (
    echo   ERROR: Fallo compilacion DBIOCUSM.cob
    set /a ERROR_COUNT+=1
) else (
    echo   OK: %OUTPUT_DIR%\DBIOCUSM.exe
)

echo [7/11] Compilando DBIOBORM.cob...
cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\DBIOBORM.exe src\dbio\DBIOBORM.cob >nul 2>&1
if errorlevel 1 (
    echo   ERROR: Fallo compilacion DBIOBORM.cob
    set /a ERROR_COUNT+=1
) else (
    echo   OK: %OUTPUT_DIR%\DBIOBORM.exe
)

echo [8/11] Compilando DBIOINVM.cob...
cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\DBIOINVM.exe src\dbio\DBIOINVM.cob >nul 2>&1
if errorlevel 1 (
    echo   ERROR: Fallo compilacion DBIOINVM.cob
    set /a ERROR_COUNT+=1
) else (
    echo   OK: %OUTPUT_DIR%\DBIOINVM.exe
)

echo [9/11] Compilando DBIOTARJ.cob...
cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\DBIOTARJ.exe src\dbio\DBIOTARJ.cob >nul 2>&1
if errorlevel 1 (
    echo   ERROR: Fallo compilacion DBIOTARJ.cob
    set /a ERROR_COUNT+=1
) else (
    echo   OK: %OUTPUT_DIR%\DBIOTARJ.exe
)

echo [10/11] Compilando DBIOTRAN.cob...
cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\DBIOTRAN.exe src\dbio\DBIOTRAN.cob >nul 2>&1
if errorlevel 1 (
    echo   ERROR: Fallo compilacion DBIOTRAN.cob
    set /a ERROR_COUNT+=1
) else (
    echo   OK: %OUTPUT_DIR%\DBIOTRAN.exe
)

echo.
echo ============ UTILS PROGRAMS ============
echo.

echo [11/11] Compilando RP0000.cob...
cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\RP0000.exe src\utils\RP0000.cob >nul 2>&1
if errorlevel 1 (
    echo   ERROR: Fallo compilacion RP0000.cob
    set /a ERROR_COUNT+=1
) else (
    echo   OK: %OUTPUT_DIR%\RP0000.exe
)

REM BAT000 no se compila directamente (llamado desde menu)
REM echo [12/12] Compilando BAT000.cob...
REM cobc -x -free -Wall %COPY_PATH% -o %OUTPUT_DIR%\BAT000.exe src\utils\BAT000.cob >nul 2>&1
REM if errorlevel 1 (
REM     echo   ERROR: Fallo compilacion BAT000.cob
REM     set /a ERROR_COUNT+=1
REM ) else (
REM     echo   OK: %OUTPUT_DIR%\BAT000.exe
REM )

echo.
echo ================================================================
if %ERROR_COUNT% EQU 0 (
    echo  COMPILACION EXITOSA - Todos los programas compilados
) else (
    echo  COMPILACION CON ERRORES - %ERROR_COUNT% programa(s) fallaron
)
echo ================================================================
echo.
echo Ejecutables generados en: %OUTPUT_DIR%
echo.
echo Para ejecutar el programa principal:
echo   %OUTPUT_DIR%\BANCSMENU.exe
echo.
pause

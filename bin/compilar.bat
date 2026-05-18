@echo off
setlocal
color 0B
title Compilador - BANCO LAF v3.0 - Clientes, Cuentas y Tarjetas

:: ============================================================
:: 1. Configurar rutas de GnuCOBOL y ESQL
:: ============================================================
set "COBOL_MAIN=C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL"
set "COBOL_BIN=%COBOL_MAIN%\bin"
set "COBOL_LIBS_ESQL=%COBOL_MAIN%\binaries3\win32\release"

:: Variables de entorno necesarias
set "COB_MAIN_DIR=%COBOL_MAIN%"
set "COB_CONFIG_DIR=%COBOL_MAIN%\config"
set "PATH=%COBOL_BIN%;%COBOL_LIBS_ESQL%;%PATH%"

:: ============================================================
:: 2. Rutas del proyecto
:: ============================================================
set "ROOT=%~dp0.."
set "SQL_DIR=%ROOT%\sql"
set "MAINLINE_DIR=%ROOT%\src\mainline"
set "BIN_DIR=%ROOT%\bin"
set "COPIES_DIR=%ROOT%\src\copies"
set "COBCPY=%COPIES_DIR%"

set "COBC=%COBOL_BIN%\cobc.exe"
set "PRECOMPILADOR=%COBOL_LIBS_ESQL%\esqlOC.exe"

echo ============================================
echo  BANCO LAF v3.0 - COMPILACION FASE CLIENTES + CUENTAS
echo ============================================
echo ROOT       : %ROOT%
echo SQL_DIR    : %SQL_DIR%
echo MAINLINE   : %MAINLINE_DIR%
echo COPIES     : %COPIES_DIR%
echo BIN        : %BIN_DIR%
echo ============================================

echo.
echo ============================================
echo  0. Limpiando COB generados de esta fase
echo ============================================

if exist "%MAINLINE_DIR%\BANCSMENU.cob" del /f /q "%MAINLINE_DIR%\BANCSMENU.cob"
if exist "%MAINLINE_DIR%\DBIOCUSM.cob" del /f /q "%MAINLINE_DIR%\DBIOCUSM.cob"
if exist "%MAINLINE_DIR%\DBIOTRAN.cob" del /f /q "%MAINLINE_DIR%\DBIOTRAN.cob"
rem DBIOINVM.cob se preserva (esqlOC tiene SIGSEGV en su SQB con cursor DECLARE)
if exist "%MAINLINE_DIR%\DBIOTARJ.cob" del /f /q "%MAINLINE_DIR%\DBIOTARJ.cob"
if exist "%MAINLINE_DIR%\DBIOBORM.cob" del /f /q "%MAINLINE_DIR%\DBIOBORM.cob"

echo Archivos generados anteriores eliminados.
echo Tus .SQB y .CBL editables estan a salvo.

echo.
echo ============================================
echo  1. Pre-compilando SQL embebido
echo ============================================

echo [1/2] BANCSMENU.sqb  -> BANCSMENU.cob
"%PRECOMPILADOR%" -I "%COPIES_DIR%" -static -o "%MAINLINE_DIR%\BANCSMENU.cob" "%SQL_DIR%\BANCSMENU.sqb"

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ERROR: Fallo al precompilar BANCSMENU.sqb
    pause
    exit /b 1
)

echo [2/2] DBIOCUSM.sqb   -> DBIOCUSM.cob
"%PRECOMPILADOR%" -I "%COPIES_DIR%" -static -o "%MAINLINE_DIR%\DBIOCUSM.cob" "%SQL_DIR%\DBIOCUSM.sqb"

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ERROR: Fallo al precompilar DBIOCUSM.sqb
    pause
    exit /b 1
)

echo [3/3] DBIOTRAN.sqb   -> DBIOTRAN.cob
"%PRECOMPILADOR%" -I "%COPIES_DIR%" -static -o "%MAINLINE_DIR%\DBIOTRAN.cob" "%SQL_DIR%\DBIOTRAN.sqb"

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ERROR: Fallo al precompilar DBIOTRAN.sqb
    pause
    exit /b 1
)

echo [4/4] DBIOINVM.cob   -> se usa version preservada (sin re-precompilar)
if not exist "%MAINLINE_DIR%\DBIOINVM.cob" (
    echo ERROR: DBIOINVM.cob no existe. Ejecuta una vez el precompilador manualmente.
    pause
    exit /b 1
)

echo [5/5] DBIOTARJ.sqb   -> DBIOTARJ.cob
"%PRECOMPILADOR%" -I "%COPIES_DIR%" -static -o "%MAINLINE_DIR%\DBIOTARJ.cob" "%SQL_DIR%\DBIOTARJ.sqb"

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ERROR: Fallo al precompilar DBIOTARJ.sqb
    pause
    exit /b 1
)

echo [6/6] DBIOBORM.sqb  -> DBIOBORM.cob
"%PRECOMPILADOR%" -I "%COPIES_DIR%" -static -o "%MAINLINE_DIR%\DBIOBORM.cob" "%SQL_DIR%\DBIOBORM.sqb"

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo ERROR: Fallo al precompilar DBIOBORM.sqb
    pause
    exit /b 1
)

echo.
echo Precompilacion SQL finalizada.

echo.
echo ============================================
echo  2. Compilando todos los modulos
echo ============================================

:: Workaround bug cobc 3.x: compilar subprogramas como DLL y solo el main como EXE
:: (al combinarlos en un unico -x el traductor de cobc hace SIGSEGV en TC0000)

echo [DLL 1/8] DBIOCUSM
"%COBC%" -m -fno-static-call -I "%COPIES_DIR%" -L "%COBOL_LIBS_ESQL%" -locsql -o "%BIN_DIR%\DBIOCUSM.dll" "%MAINLINE_DIR%\DBIOCUSM.cob"
if %ERRORLEVEL% NEQ 0 goto :ERROR_COBOL

echo [DLL 2/8] DBIOTRAN
"%COBC%" -m -fno-static-call -I "%COPIES_DIR%" -L "%COBOL_LIBS_ESQL%" -locsql -o "%BIN_DIR%\DBIOTRAN.dll" "%MAINLINE_DIR%\DBIOTRAN.cob"
if %ERRORLEVEL% NEQ 0 goto :ERROR_COBOL

echo [DLL 3/8] DBIOINVM
"%COBC%" -m -fno-static-call -I "%COPIES_DIR%" -L "%COBOL_LIBS_ESQL%" -locsql -o "%BIN_DIR%\DBIOINVM.dll" "%MAINLINE_DIR%\DBIOINVM.cob"
if %ERRORLEVEL% NEQ 0 goto :ERROR_COBOL

echo [DLL 4/8] DBIOTARJ
"%COBC%" -m -fno-static-call -I "%COPIES_DIR%" -L "%COBOL_LIBS_ESQL%" -locsql -o "%BIN_DIR%\DBIOTARJ.dll" "%MAINLINE_DIR%\DBIOTARJ.cob"
if %ERRORLEVEL% NEQ 0 goto :ERROR_COBOL

echo [DLL 5/8] CI0000
"%COBC%" -m -fno-static-call -I "%COPIES_DIR%" -L "%COBOL_LIBS_ESQL%" -locsql -o "%BIN_DIR%\CI0000.dll" "%MAINLINE_DIR%\CI0000.cbl"
if %ERRORLEVEL% NEQ 0 goto :ERROR_COBOL

echo [DLL 6/8] IN0000
"%COBC%" -m -fno-static-call -I "%COPIES_DIR%" -L "%COBOL_LIBS_ESQL%" -locsql -o "%BIN_DIR%\IN0000.dll" "%MAINLINE_DIR%\IN0000.cbl"
if %ERRORLEVEL% NEQ 0 goto :ERROR_COBOL

echo [DLL 7/8] DF0000
"%COBC%" -m -fno-static-call -I "%COPIES_DIR%" -L "%COBOL_LIBS_ESQL%" -locsql -o "%BIN_DIR%\DF0000.dll" "%MAINLINE_DIR%\DF0000.cbl"
if %ERRORLEVEL% NEQ 0 goto :ERROR_COBOL

echo [DLL 8/8] TC0000
"%COBC%" -m -fno-static-call -I "%COPIES_DIR%" -L "%COBOL_LIBS_ESQL%" -locsql -o "%BIN_DIR%\TC0000.dll" "%MAINLINE_DIR%\TC0000.cbl"
if %ERRORLEVEL% NEQ 0 goto :ERROR_COBOL

echo [DLL 9/10] DBIOBORM
"%COBC%" -m -fno-static-call -I "%COPIES_DIR%" -L "%COBOL_LIBS_ESQL%" -locsql -o "%BIN_DIR%\DBIOBORM.dll" "%MAINLINE_DIR%\DBIOBORM.cob"
if %ERRORLEVEL% NEQ 0 goto :ERROR_COBOL

echo [DLL 10/10] BR0000
"%COBC%" -m -fno-static-call -I "%COPIES_DIR%" -L "%COBOL_LIBS_ESQL%" -locsql -o "%BIN_DIR%\BR0000.dll" "%MAINLINE_DIR%\BR0000.cbl"
if %ERRORLEVEL% NEQ 0 goto :ERROR_COBOL

echo [EXE] BANCSMENU
"%COBC%" -x -fno-static-call -I "%COPIES_DIR%" -L "%COBOL_LIBS_ESQL%" -locsql -o "%BIN_DIR%\BANCSMENU.exe" "%MAINLINE_DIR%\BANCSMENU.cob"
if %ERRORLEVEL% NEQ 0 goto :ERROR_COBOL

echo.
echo ============================================
echo  EXITO: COMPILACION COMPLETA
echo ============================================
echo Ejecutable: %BIN_DIR%\BANCSMENU.exe
echo DLLs      : %BIN_DIR%\*.dll
echo ============================================
goto :FIN_COBOL

:ERROR_COBOL
echo.
echo ============================================
echo  ERROR: FALLO LA COMPILACION COBOL
echo ============================================
echo Revisa los errores mostrados arriba.
echo ============================================

:FIN_COBOL

pause

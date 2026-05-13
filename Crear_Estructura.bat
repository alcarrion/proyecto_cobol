@echo off
REM ================================================================
REM SCRIPT: Crear_Estructura.bat
REM PROPOSITO: Crear la estructura de carpetas del proyecto
REM VERSION: 1.0
REM FECHA: 2026-05-13
REM INSTRUCCIONES: Ejecutar primero, una sola vez
REM ================================================================

echo.
echo ================================================================
echo  CREANDO ESTRUCTURA DE CARPETAS DEL PROYECTO
echo ================================================================
echo.

REM Crear carpetas principales
echo Creando carpetas en src/...
mkdir src\mainline >nul 2>&1
mkdir src\dbio >nul 2>&1
mkdir src\copies >nul 2>&1
mkdir src\utils >nul 2>&1
mkdir src\lib >nul 2>&1
echo   ✓ Carpetas src/ creadas

echo Creando carpetas en sql/...
mkdir sql\schema >nul 2>&1
echo   ✓ Carpetas sql/ creadas

echo Creando carpetas en config/...
mkdir config >nul 2>&1
echo   ✓ Carpetas config/ creadas

echo Creando carpetas en docs/...
mkdir docs >nul 2>&1
echo   ✓ Carpetas docs/ creadas

echo Creando carpetas en build/...
mkdir build\logs >nul 2>&1
mkdir build\temp >nul 2>&1
echo   ✓ Carpetas build/ creadas

echo Creando carpetas en bin/...
mkdir bin >nul 2>&1
echo   ✓ Carpetas bin/ creadas

echo Creando carpetas en test/...
mkdir test >nul 2>&1
echo   ✓ Carpetas test/ creadas

echo.
echo ================================================================
echo  ✓ ESTRUCTURA DE CARPETAS CREADA EXITOSAMENTE
echo ================================================================
echo.
echo Siguiente paso:
echo   Ejecutar "Reorganizar_Proyecto.bat" para mover los archivos
echo.
pause

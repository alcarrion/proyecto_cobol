@echo off
REM ================================================================
REM SCRIPT: Reorganizar_Proyecto.bat
REM PROPOSITO: Mover archivos a su nueva ubicacion ordenada
REM VERSION: 1.0
REM FECHA: 2026-05-13
REM INSTRUCCIONES: Ejecutar desde la carpeta raiz del proyecto
REM ================================================================

setlocal enabledelayedexpansion

echo.
echo ================================================================
echo  REORGANIZACION DE ARCHIVOS DEL PROYECTO COBOL BANCARIO v1.0
echo ================================================================
echo.

REM Verificar que estamos en el directorio correcto
if not exist "src\mainline" (
    echo ERROR: Las carpetas de destino no existen.
    echo Por favor, primero ejecuta "Crear_Estructura.bat"
    pause
    exit /b 1
)

echo [1/8] Moviendo MAINLINE programs...
move CI0000.cbl src\mainline\ >nul 2>&1 && echo   ✓ CI0000.cbl
move BR0000.cbl src\mainline\ >nul 2>&1 && echo   ✓ BR0000.cbl
move IN0000.cbl src\mainline\ >nul 2>&1 && echo   ✓ IN0000.cbl
move TC0000.cbl src\mainline\ >nul 2>&1 && echo   ✓ TC0000.cbl
move BANCSMENU.cob src\mainline\ >nul 2>&1 && echo   ✓ BANCSMENU.cob
move BANCSMENU.sqb src\mainline\ >nul 2>&1 && echo   ✓ BANCSMENU.sqb

echo.
echo [2/8] Moviendo DBIO programs...
move DBIOCUSM.cob src\dbio\ >nul 2>&1 && echo   ✓ DBIOCUSM.cob
move DBIOBORM.cob src\dbio\ >nul 2>&1 && echo   ✓ DBIOBORM.cob
move DBIOINVM.cob src\dbio\ >nul 2>&1 && echo   ✓ DBIOINVM.cob
move DBIOTARJ.cob src\dbio\ >nul 2>&1 && echo   ✓ DBIOTARJ.cob
move DBIOTRAN.cob src\dbio\ >nul 2>&1 && echo   ✓ DBIOTRAN.cob

echo.
echo [3/8] Moviendo COPYBOOKS...
move CUSMREC.CPY src\copies\ >nul 2>&1 && echo   ✓ CUSMREC.CPY
move BORMREC.CPY src\copies\ >nul 2>&1 && echo   ✓ BORMREC.CPY
move INVMREC.CPY src\copies\ >nul 2>&1 && echo   ✓ INVMREC.CPY
move TARJREC.CPY src\copies\ >nul 2>&1 && echo   ✓ TARJREC.CPY
move LKCIF.CPY src\copies\ >nul 2>&1 && echo   ✓ LKCIF.CPY

echo.
echo [4/8] Moviendo UTILS programs...
move RP0000.cob src\utils\ >nul 2>&1 && echo   ✓ RP0000.cob
move RP0000.sqb src\utils\ >nul 2>&1 && echo   ✓ RP0000.sqb
move BAT000.cob src\utils\ >nul 2>&1 && echo   ✓ BAT000.cob
move BAT000.sqb src\utils\ >nul 2>&1 && echo   ✓ BAT000.sqb

echo.
echo [5/8] Moviendo SQL SCRIPTS...
move DBIOCUSM.sqb sql\ >nul 2>&1 && echo   ✓ DBIOCUSM.sqb
move DBIOBORM.sqb sql\ >nul 2>&1 && echo   ✓ DBIOBORM.sqb
move DBIOINVM.sqb sql\ >nul 2>&1 && echo   ✓ DBIOINVM.sqb
move DBIOTARJ.sqb sql\ >nul 2>&1 && echo   ✓ DBIOTARJ.sqb
move DBIOTRAN.sqb sql\ >nul 2>&1 && echo   ✓ DBIOTRAN.sqb
move testconn.sqb sql\ >nul 2>&1 && echo   ✓ testconn.sqb
move "DBIOBORM - copia.sqb" sql\ >nul 2>&1 && echo   ✓ DBIOBORM - copia.sqb

echo.
echo [6/8] Moviendo CONFIG files...
if exist db_config.cfg (
    move db_config.cfg config\ >nul 2>&1 && echo   ✓ db_config.cfg
)
if exist bdd.txt (
    move bdd.txt config\ >nul 2>&1 && echo   ✓ bdd.txt
)

echo.
echo [7/8] Moviendo DOCS...
move DOCUMENTACION_COMPLETA.md docs\ >nul 2>&1 && echo   ✓ DOCUMENTACION_COMPLETA.md
move INDICE_DOCUMENTACION.md docs\ >nul 2>&1 && echo   ✓ INDICE_DOCUMENTACION.md
if exist DOCUMENTACION_COMPLETA.pdf (
    move DOCUMENTACION_COMPLETA.pdf docs\ >nul 2>&1 && echo   ✓ DOCUMENTACION_COMPLETA.pdf
)

echo.
echo [8/8] Moviendo TEST files...
if exist "BR0000 - copia.cbl" (
    move "BR0000 - copia.cbl" test\ >nul 2>&1 && echo   ✓ BR0000 - copia.cbl
)

echo.
echo ================================================================
echo  ✓ REORGANIZACION COMPLETADA EXITOSAMENTE
echo ================================================================
echo.
echo Nueva estructura creada:
echo   src/
echo   ├── mainline/     (programas principales)
echo   ├── dbio/         (acceso a datos)
echo   ├── copies/       (includes compartidos + PATHS.CPY)
echo   └── utils/        (utilidades)
echo   sql/              (scripts SQL)
echo   config/           (configuracion + environment.properties)
echo   docs/             (documentacion)
echo   bin/              (ejecutables compilados)
echo   build/            (artefactos compilacion)
echo   test/             (pruebas)
echo.
echo Archivos de configuracion creados:
echo   ✓ src/copies/PATHS.CPY        (rutas centralizadas en COBOL)
echo   ✓ config/environment.properties (rutas en formato texto)
echo   ✓ config/paths.cfg            (rutas en formato INI)
echo.
echo PROXIMO PASO:
echo   1. Revisar PLAN_REORGANIZACION.md
echo   2. Actualizar referencias COPY en tus programas COBOL
echo   3. Ejecutar Crear_Compilacion.bat para build automatico
echo.
pause

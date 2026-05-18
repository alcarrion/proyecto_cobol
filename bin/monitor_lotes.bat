@echo off
set "FOLDER_TEMP=C:\Users\rocha\OneDrive\Documentos\PROYECTO CORE TATA\proyecto_cobol\src\proyecto_cobol\banco\spool\Interfaces\BATCH-UPLOADS-TEMP"
set "FOLDER_S=C:\Users\rocha\OneDrive\Documentos\PROYECTO CORE TATA\proyecto_cobol\src\proyecto_cobol\banco\spool\Interfaces\BATCH-UPLOAD-S"

:monitor
cls
echo [MONITOR TRICKLE FEED] Escaneando archivos en TEMP...
if exist "%FOLDER_TEMP%\*.txt" (
    for %%f in ("%FOLDER_TEMP%\*.txt") do (
        echo Archivo detectado: %%~nxf
        
        :: 1. Insertar en DB (Fase 00) - Aquí llamarías a un pequeño ejecutable o SQL
        echo Registrando en TFFM...
        
        :: 2. Mover a carpeta de procesamiento (Fase 10)
        move "%%f" "%FOLDER_S%\"
        
        :: 3. Disparar Orquestador
        echo Iniciando Orquestador TFTRCT...
        start /b TFTRCT.exe
    )
)
timeout /t 5 > nul
goto monitor
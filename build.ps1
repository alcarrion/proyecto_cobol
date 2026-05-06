$ErrorActionPreference = "Stop"

$env:PATH = "C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin;C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\binaries3\win32\release;$env:PATH"
$env:COB_CONFIG_DIR = "C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\config"
$env:COB_COPY_DIR = "c:\Users\dell\Documents\proyecto_cobol"
$env:COB_LIBRARY_PATH = "c:\Users\dell\Documents\proyecto_cobol"

$esql = "C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\binaries3\win32\release\esqlOC.exe"
$cobc = "C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin\cobc.exe"
$libPath = "-LC:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\binaries3\win32\release"

Set-Location "c:\Users\dell\Documents\proyecto_cobol"

Write-Host "1. PRECOMPILANDO ARCHIVOS SQL (SQB -> COB)..."
& $esql -static -o DBIOCUSM.cob DBIOCUSM.sqb
& $esql -static -o DBIOTARJ.cob DBIOTARJ.sqb
& $esql -static -o DBIOINVM.cob DBIOINVM.sqb
& $esql -static -o DBIOBORM.cob DBIOBORM.sqb
& $esql -static -o BAT000.cob BAT000.sqb
& $esql -static -o RP0000.cob RP0000.sqb
& $esql -static -o BANCSMENU.cob BANCSMENU.sqb

Write-Host "2. COMPILANDO MODULOS DBIO A DLL (GENERADOS POR ESQLOC - SIN TEXT-COLUMN)..."
& $cobc -m -Wall -debug $libPath -locsql DBIOCUSM.cob
& $cobc -m -Wall -debug $libPath -locsql DBIOTARJ.cob
& $cobc -m -Wall -debug $libPath -locsql DBIOINVM.cob
& $cobc -m -Wall -debug $libPath -locsql DBIOBORM.cob

Write-Host "3. COMPILANDO MODULOS BATCH Y REPORTES A DLL (GENERADOS POR ESQLOC)..."
& $cobc -m -Wall -debug $libPath -locsql BAT000.cob
& $cobc -m -Wall -debug $libPath -locsql RP0000.cob

Write-Host "4. COMPILANDO MAINLINES A DLL (MANUALES - CON TEXT-COLUMN 255)..."
& $cobc -m -ftext-column=255 -Wall -debug CI0000.cbl
& $cobc -m -ftext-column=255 -Wall -debug IN0000.cbl
& $cobc -m -ftext-column=255 -Wall -debug TC0000.cbl
& $cobc -m -ftext-column=255 -Wall -debug BR0000.cob

Write-Host "5. COMPILANDO MENU PRINCIPAL A EXE (GENERADO POR ESQLOC)..."
& $cobc -x -Wall -debug $libPath -locsql BANCSMENU.cob

Write-Host "COMPILACION FINALIZADA CON EXITO!"

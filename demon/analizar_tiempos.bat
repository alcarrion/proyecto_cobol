@echo off
set "LOG_FILE=..\logs\daemonio.log"

echo LOTE       ^| DURACION (SEG)
echo ---------------------------

powershell -Command ^
    "$log = Get-Content '%LOG_FILE%'; " ^
    "$starts = $log | Select-String '\[START\] TX_ID: (\d+) HORA: (\d{6})'; " ^
    "$ends = $log | Select-String '\[V\] TX FINALIZADA'; " ^
    "foreach ($s in $starts) { " ^
    "    $id = $s.Matches.Groups[1].Value; " ^
    "    $h1 = [int]$s.Matches.Groups[2].Value.Substring(4,2); " ^
    "    $m1 = [int]$s.Matches.Groups[2].Value.Substring(2,2); " ^
    "    $s1 = [int]$s.Matches.Groups[2].Value.Substring(0,2); " ^
    "    $t1 = ($h1*3600) + ($m1*60) + $s1; " ^
    "    Write-Host \"Lote: $id | Duracion: ...s\"; " ^
    "}"
pause
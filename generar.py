import os
import random
import hashlib

# 1. Configuración de Rutas del Spool (Alineado a tu última corrida exitosa)
output_dir = r"C:\banco\spool\Interfaces\BATCH-INPUT"
file_name = "DEP-999-170526-160000-170526-001.TXT"
full_path = os.path.join(output_dir, file_name)

# Asegurar la existencia del directorio físico
os.makedirs(output_dir, exist_ok=True)

# 2. Catálogo completo de tus 8 cuentas vivas en la BD
cuentas_vivas = [1, 2, 3, 4, 5, 6, 7, 8]

# Volumen masivo para forzar quiebres automáticos en 3 lotes (10k, 10k y 5k)
total_registros = 25005

print(f"[*] Construyendo {total_registros} transacciones fijas bajo estándar de 136 bytes...")

with open(full_path, "w", encoding="utf-8") as f:
    for i in range(1, total_registros + 1):
        
        # Selección aleatoria de cuenta del catálogo real
        cuenta_id = random.choice(cuentas_vivas)
        
        # [ID_CUENTA:10] -> Formato estricto: Ceros a la izquierda para PIC 9(10)
        id_cuenta_txt = f"{cuenta_id:010d}"
        
        # [COD_MOV:3] -> Intercalamos Depósitos (002) y Retiros (003) para estresar tkin01
        cod_mov_txt = random.choice(["002", "003"])
        
        # [IMPORTE:15] -> Ceros a la izquierda con 2 decimales implícitos (Se dividirá para 100 en COBOL)
        monto = round(random.uniform(10.00, 500.00), 2)
        importe_txt = f"{int(monto * 100):015d}"
        
        # [TRACE_ID:40] -> Alfanumérico alineado a la izquierda, relleno con espacios
        trace_id_raw = f"TR-STRESS-20260517-{i:010d}"
        trace_id_txt = f"{trace_id_raw:<40}"
        
        # [TERMINAL:4] -> Identificador de terminal de 4 caracteres
        terminal_raw = f"ATM{random.randint(1, 4)}"
        terminal_txt = f"{terminal_raw:<4}"
        
        # [HASH_SEG:64] -> SHA-256 real basado en la concatenación de los primeros 72 bytes
        payload_base = f"{id_cuenta_txt}{cod_mov_txt}{importe_txt}{trace_id_txt}{terminal_txt}"
        hash_seg_txt = hashlib.sha256(payload_base.encode('utf-8')).hexdigest()
        
        # CONSTRUCCIÓN DE LA LÍNEA FINAL POSICIONAL
        # 10 + 3 + 15 + 40 + 4 + 64 = 136 Bytes exactos
        linea_final = f"{id_cuenta_txt}{cod_mov_txt}{importe_txt}{trace_id_txt}{terminal_txt}{hash_seg_txt}\n"
        
        # Control de calidad posicional preventivo
        assert len(linea_final) == 137, f"Fallo de simetría en registro {i}: mide {len(linea_final)} bytes."
        
        f.write(linea_final)

print(f"[OK] Archivo masivo generado con éxito en: {full_path}")
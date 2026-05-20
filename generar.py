import random
import hashlib
from datetime import datetime
from pathlib import Path

# ============================================================
# CONFIGURACIÓN
# ============================================================

TOTAL_TRANSACCIONES = 100
PORCENTAJE_ERRORES = 0.25   # 25% inconsistentes

OUTPUT_DIR = "transacciones_test"
Path(OUTPUT_DIR).mkdir(exist_ok=True)

# ============================================================
# DATA BASE VÁLIDA
# ============================================================

clientes = []

for i in range(1, 101):

    tarjeta = str(4555223400000000 + i)

    id_cliente = 22345700 + i

    id_cuenta = str(100 + i).zfill(10)

    clientes.append({
        "tarjeta": tarjeta,
        "id_cliente": id_cliente,
        "id_cuenta": id_cuenta
    })

cuentas_validas = [c["id_cuenta"] for c in clientes]
tarjetas_validas = [c["tarjeta"] for c in clientes]

# ============================================================
# HELPERS
# ============================================================

def generar_hash():
    return hashlib.sha256(
        str(random.random()).encode()
    ).hexdigest()

def generar_trace(prefijo):
    fecha = datetime.now().strftime("%Y%m%d%H%M%S")
    rnd = random.randint(10000, 99999)

    trace = f"{prefijo}-{fecha}-{rnd}"

    return trace[:40].ljust(40, "X")

def generar_monto():
    return str(
        random.randint(1000, 999999)
    ).zfill(15)

def generar_terminal():
    return random.choice([
        "ATM1",
        "ATM2",
        "WEB1",
        "POS1",
        "APP1"
    ])

# ============================================================
# ERRORES POSIBLES
# ============================================================

def cuenta_inexistente():
    return str(random.randint(900000000, 999999999)).zfill(10)

def tarjeta_inexistente():
    return str(random.randint(
        4999999999999999,
        5999999999999999
    ))

def cod_mov_invalido():
    return random.choice([
        "999",
        "ABC",
        "777",
        "000",
        "XXX"
    ])

def hash_corrupto():
    return "HASH_INVALIDO"

# ============================================================
# TRANSACCIONES TARJETAS
# FORMATO:
# [COD_MOV:3][ID_CUENTA:10][DATOS_TX:16]
# [MONTO:15][TRACE_ID:40][HASH_SEG:64]
# ============================================================

transacciones_tarjetas = []

for i in range(TOTAL_TRANSACCIONES):

    cliente = random.choice(clientes)

    inconsistente = (
        random.random() < PORCENTAJE_ERRORES
    )

    cod_mov = "004"
    id_cuenta = cliente["id_cuenta"]
    tarjeta = cliente["tarjeta"]
    monto = generar_monto()
    trace = generar_trace("TR-PAG-TARJETA")
    hash_seg = generar_hash()

    errores_aplicados = []

    if inconsistente:

        tipo_error = random.choice([
            "CUENTA_INVALIDA",
            "TARJETA_INVALIDA",
            "CODIGO_INVALIDO",
            "HASH_INVALIDO",
            "MULTIPLE"
        ])

        if tipo_error == "CUENTA_INVALIDA":
            id_cuenta = cuenta_inexistente()
            errores_aplicados.append(tipo_error)

        elif tipo_error == "TARJETA_INVALIDA":
            tarjeta = tarjeta_inexistente()
            errores_aplicados.append(tipo_error)

        elif tipo_error == "CODIGO_INVALIDO":
            cod_mov = cod_mov_invalido()
            errores_aplicados.append(tipo_error)

        elif tipo_error == "HASH_INVALIDO":
            hash_seg = hash_corrupto()
            errores_aplicados.append(tipo_error)

        elif tipo_error == "MULTIPLE":

            id_cuenta = cuenta_inexistente()
            tarjeta = tarjeta_inexistente()
            cod_mov = cod_mov_invalido()

            errores_aplicados.extend([
                "CUENTA_INVALIDA",
                "TARJETA_INVALIDA",
                "CODIGO_INVALIDO"
            ])

    linea = (
        f"{cod_mov}"
        f"{id_cuenta}"
        f"{tarjeta}"
        f"{monto}"
        f"{trace}"
        f"{hash_seg}"
    )

    transacciones_tarjetas.append({
        "linea": linea,
        "errores": errores_aplicados
    })

# ============================================================
# TRANSACCIONES DEPÓSITOS / RETIROS
# FORMATO:
# [ID_CUENTA:10][COD_MOV:3][IMPORTE:15]
# [TRACE_ID:40][TERMINAL:4][HASH_SEG:64]
# ============================================================

transacciones_dep = []

for i in range(TOTAL_TRANSACCIONES):

    cliente = random.choice(clientes)

    inconsistente = (
        random.random() < PORCENTAJE_ERRORES
    )

    id_cuenta = cliente["id_cuenta"]

    cod_mov = random.choice([
        "002",
        "003"
    ])

    importe = generar_monto()

    trace = generar_trace("UUID-CTR")

    terminal = generar_terminal()

    hash_seg = generar_hash()

    errores_aplicados = []

    if inconsistente:

        tipo_error = random.choice([
            "CUENTA_INVALIDA",
            "CODIGO_INVALIDO",
            "HASH_INVALIDO",
            "TERMINAL_INVALIDA",
            "MULTIPLE"
        ])

        if tipo_error == "CUENTA_INVALIDA":
            id_cuenta = cuenta_inexistente()
            errores_aplicados.append(tipo_error)

        elif tipo_error == "CODIGO_INVALIDO":
            cod_mov = cod_mov_invalido()
            errores_aplicados.append(tipo_error)

        elif tipo_error == "HASH_INVALIDO":
            hash_seg = hash_corrupto()
            errores_aplicados.append(tipo_error)

        elif tipo_error == "TERMINAL_INVALIDA":
            terminal = "ZZZZ"
            errores_aplicados.append(tipo_error)

        elif tipo_error == "MULTIPLE":

            id_cuenta = cuenta_inexistente()
            cod_mov = cod_mov_invalido()
            terminal = "####"

            errores_aplicados.extend([
                "CUENTA_INVALIDA",
                "CODIGO_INVALIDO",
                "TERMINAL_INVALIDA"
            ])

    linea = (
        f"{id_cuenta}"
        f"{cod_mov}"
        f"{importe}"
        f"{trace}"
        f"{terminal}"
        f"{hash_seg}"
    )

    transacciones_dep.append({
        "linea": linea,
        "errores": errores_aplicados
    })

# ============================================================
# NOMBRES ARCHIVOS
# ============================================================

fecha = datetime.now().strftime("%d%m%y")
hora = datetime.now().strftime("%H%M%S")

archivo_dep = (
    f"DEP-999-{fecha}-{hora}-{fecha}-001.TXT"
)

archivo_pag = (
    f"PAG--999-{fecha}-{hora}-{fecha}-001.TXT"
)

archivo_log = (
    f"LOG-ERRORES-{fecha}-{hora}.TXT"
)

# ============================================================
# ESCRIBIR ARCHIVOS
# ============================================================

with open(f"{OUTPUT_DIR}/{archivo_pag}", "w") as f:

    for tx in transacciones_tarjetas:
        f.write(tx["linea"] + "\n")

with open(f"{OUTPUT_DIR}/{archivo_dep}", "w") as f:

    for tx in transacciones_dep:
        f.write(tx["linea"] + "\n")

# ============================================================
# LOG DE ERRORES
# ============================================================

with open(f"{OUTPUT_DIR}/{archivo_log}", "w") as f:

    f.write("========== TRANSACCIONES INCONSISTENTES ==========\n\n")

    for i, tx in enumerate(transacciones_tarjetas):

        if tx["errores"]:

            f.write(
                f"[PAGOS TARJETA #{i+1}] "
                f"{','.join(tx['errores'])}\n"
            )

            f.write(tx["linea"] + "\n\n")

    for i, tx in enumerate(transacciones_dep):

        if tx["errores"]:

            f.write(
                f"[DEPOSITO/RETIRO #{i+1}] "
                f"{','.join(tx['errores'])}\n"
            )

            f.write(tx["linea"] + "\n\n")

# ============================================================
# RESULTADO
# ============================================================

print("======================================")
print("ARCHIVOS GENERADOS")
print("======================================")
print(f"DEP/RET : {archivo_dep}")
print(f"PAGOS   : {archivo_pag}")
print(f"LOG     : {archivo_log}")
print("======================================")
print(f"TX TOTALES: {TOTAL_TRANSACCIONES * 2}")
print(f"% ERROR   : {int(PORCENTAJE_ERRORES * 100)}%")
print("======================================")
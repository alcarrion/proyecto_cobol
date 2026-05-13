# 🏗️ ARQUITECTURA VISUAL DEL PROYECTO REORGANIZADO

## Diagrama General

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROYECTO COBOL BANCARIO v1.0                  │
│                      Estructura Profesional                       │
└─────────────────────────────────────────────────────────────────┘

                        ┌──────────────┐
                        │   USUARIO    │
                        └──────┬───────┘
                               │
                   ┌───────────┴───────────┐
                   │                       │
          ┌────────▼─────────┐  ┌─────────▼────────┐
          │ SCRIPTS DE INICIO │  │  DOCUMENTACIÓN   │
          ├──────────────────┤  ├──────────────────┤
          │ Ejecutar_*.bat   │  │ README.md        │
          │ Crear_*.bat      │  │ GUIA_*.md        │
          │ build.bat (TBD)  │  │ PLAN_*.md        │
          └────────┬─────────┘  └──────────────────┘
                   │
                   └───────────────┬────────────────────────┐
                                   │                        │
                    ┌──────────────▼────────────┐    ┌──────▼──────────┐
                    │   CODIGO FUENTE (SRC/)    │    │  CONFIGURACIÓN  │
                    ├───────────────────────────┤    ├─────────────────┤
                    │ mainline/                 │    │ config/         │
                    │  ├─ CI0000.cbl            │    │  ├─ PATHS.CPY   │◄── ⭐ CLAVE
                    │  ├─ BR0000.cbl            │    │  ├─ environment │
                    │  ├─ IN0000.cbl            │    │  │   .properties │
                    │  ├─ TC0000.cbl            │    │  └─ paths.cfg   │
                    │  └─ BANCSMENU.cob         │    └────┬────────────┘
                    │                           │         │
                    │ dbio/                     │         │
                    │  ├─ DBIOCUSM.cob          │    ┌────▼─────────────┐
                    │  ├─ DBIOBORM.cob          │    │ Rutas            │
                    │  ├─ DBIOINVM.cob          │    │ Centralizadas:   │
                    │  ├─ DBIOTARJ.cob          │    │ PATH-CONFIG      │
                    │  └─ DBIOTRAN.cob          │    │ PATH-SRC         │
                    │                           │    │ FIL-DB-CONFIG    │
                    │ copies/ ◄─────────────────┼────┤ FIL-BDD-TXT      │
                    │  ├─ PATHS.CPY ⭐ ◄────────┘    │ etc.             │
                    │  ├─ CUSMREC.CPY           │    └──────────────────┘
                    │  ├─ BORMREC.CPY           │
                    │  ├─ INVMREC.CPY           │
                    │  ├─ TARJREC.CPY           │
                    │  └─ LKCIF.CPY             │
                    │                           │
                    │ utils/                    │
                    │  ├─ RP0000.cob            │
                    │  └─ BAT000.cob            │
                    └───────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                    ARCHIVOS COMPILADOS (BIN/)                    │
│  CI0000.exe │ BR0000.exe │ IN0000.exe │ TC0000.exe │ ... etc  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Flujo de Rutas Dinámicas

```
┌──────────────────────────────────────────────────────────────┐
│                    PROGRAMA COBOL INICIA                      │
└────────────────────┬─────────────────────────────────────────┘
                     │
        ┌────────────▼────────────┐
        │   COPY PATHS-FILE       │
        │  (src/copies/PATHS.CPY) │
        └────────────┬────────────┘
                     │
         ┌───────────▼────────────┐
         │ Variables en Memoria:  │
         │ PATH-CONFIG = ".\config"
         │ FIL-DB-CONFIG = "db_config.cfg"
         │ ...                    │
         └───────────┬────────────┘
                     │
    ┌────────────────▼──────────────────┐
    │ CONSTRUIR RUTA DINÁMICAMENTE:     │
    │                                    │
    │ MOVE FUNCTION CONCATENATE(        │
    │   PATH-CONFIG "\"                 │
    │   FIL-DB-CONFIG                   │
    │ ) TO WS-RUTA-CONFIG              │
    │                                    │
    │ Resultado:                        │
    │ WS-RUTA-CONFIG = ".\config\db_config.cfg"
    └────────────┬───────────────────────┘
                 │
    ┌────────────▼──────────────────┐
    │ OPEN INPUT ARCH-CONFIG        │
    │ ASSIGN TO WS-RUTA-CONFIG      │
    │                                │
    │ → Busca y abre archivo        │
    │   en ruta correcta ✓           │
    └────────────────────────────────┘
```

---

## Matriz de Dependencias

```
┌────────────────────────────────────────────────────────────┐
│                  MAINLINE PROGRAMS                         │
│           (Punto de entrada de usuarios)                   │
├────────────────────────────────────────────────────────────┤
│                          │                                 │
│  CI0000 ────────┐        │        ┌──────────── BR0000    │
│                 ├────────┼────────┤                        │
│  TC0000 ────────┼─ ┐      │      ┌ ┼──────────── IN0000    │
│                 │  │      │      │  │                      │
│  BANCSMENU ─────┼──┼──────┼──────┼──┼────────── RP0000     │
│                 │  │      │      │  │                      │
└────────────────┬┼──┼──────┼──────┼──┼──────────────────────┘
                 ││  │      │      │  │
     ┌───────────┘│  │      │      │  └────────────────┐
     │            │  │      │      │                   │
     │   ┌────────┴──┼──────┼──────┼─────────────┐    │
     │   │           │      │      │             │    │
     │   ▼           ▼      ▼      ▼             ▼    ▼
  ┌──────────────────────────────────────────────────────────┐
  │         COPYBOOKS (src/copies/)                          │
  ├──────────────────────────────────────────────────────────┤
  │  PATHS.CPY ◄────────────────────────────────── CLAVE    │
  │  CUSMREC.CPY (Estructura Cliente)                       │
  │  BORMREC.CPY (Estructura Hipoteca)                      │
  │  INVMREC.CPY (Estructura Cuenta)                        │
  │  TARJREC.CPY (Estructura Tarjeta)                       │
  │  LKCIF.CPY (Layout CIF)                                 │
  └──────────────────────────────────────────────────────────┘
            │
            └──────────────────────────────────┐
                                               │
                    ┌──────────────────────────▼─────────┐
                    │   DBIO PROGRAMS (Acceso a Datos)   │
                    ├────────────────────────────────────┤
                    │  DBIOCUSM.exe (CRUD Clientes)     │
                    │  DBIOBORM.exe (CRUD Hipotecas)    │
                    │  DBIOINVM.exe (CRUD Cuentas)      │
                    │  DBIOTARJ.exe (CRUD Tarjetas)     │
                    │  DBIOTRAN.exe (CRUD Transacciones)│
                    └────────────┬─────────────────────┬─┘
                                 │                     │
                        ┌────────▼───────┐    ┌───────▼──────────┐
                        │   BASE DE DATOS │    │   SQL SCRIPTS    │
                        │                 │    │   (sql/)         │
                        │ banco_system    │    │                  │
                        │  ├─ clientes    │    │ DBIOCUSM.sqb     │
                        │  ├─ hipotecas   │    │ DBIOBORM.sqb     │
                        │  ├─ cuentas     │    │ DBIOINVM.sqb     │
                        │  ├─ tarjetas    │    │ DBIOTARJ.sqb     │
                        │  └─ transacciones
                        └─────────────────┘    └──────────────────┘
```

---

## Capas de la Arquitectura

```
╔════════════════════════════════════════════════════════╗
║                    CAPA DE PRESENTACIÓN               ║
║   (MAINLINE PROGRAMS: CI0000, BR0000, IN0000, TC0000) ║
║   - Interacción con usuario                           ║
║   - Menús y validación inicial                        ║
║   - Orquestación de procesos                          ║
╚══════════════════════════════════════════════════════╬╝
                                                        │
                          ┌─────────────────────────────┘
                          │
╔═════════════════════════▼════════════════════════════╗
║                  CAPA DE LÓGICA DE NEGOCIO          ║
║           (CALLS A DBIO PROGRAMS)                    ║
║   - Validaciones complejas                           ║
║   - Cálculos y transformaciones                      ║
║   - Coordinación entre módulos                       ║
╚══════════════════════════════════════════════════════╬╝
                                                        │
                          ┌─────────────────────────────┘
                          │
╔═════════════════════════▼════════════════════════════╗
║              CAPA DE ACCESO A DATOS (DBIO)          ║
║    (DBIOCUSM, DBIOBORM, DBIOINVM, DBIOTARJ, ...)   ║
║   - CRUD operations (Create, Read, Update, Delete)  ║
║   - SQL embedding (EXEC SQL)                        ║
║   - Manejo de transacciones                         ║
╚══════════════════════════════════════════════════════╬╝
                                                        │
                          ┌─────────────────────────────┘
                          │
╔═════════════════════════▼════════════════════════════╗
║              CAPA DE DATOS (BD)                      ║
║              PostgreSQL / Oracle                     ║
║   - Tablas: clientes, hipotecas, cuentas, tarjetas ║
║   - Transacciones ACID                              ║
╚════════════════════════════════════════════════════╝


╔════════════════════════════════════════════════════════╗
║        CAPA TRANSVERSAL: CONFIGURACIÓN               ║
║    (PATHS.CPY + environment.properties)              ║
║    - Rutas centralizadas (PATHS.CPY) ⭐              ║
║    - Variables de entorno                           ║
║    - Credenciales y configuración                   ║
╚════════════════════════════════════════════════════════╝
```

---

## Flujo de una Operación Completa

```
┌─────────────────────────────────────────────────────────────┐
│ USUARIO selecciona opción "Alta de Cliente"en BANCSMENU    │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
        ┌────────────────────────────────┐
        │ CI0000.exe inicia              │
        │                                │
        │ 1. COPY PATHS-FILE             │
        │ 2. Construye rutas dinámicas   │
        │ 3. Lee db_config.cfg           │
        │ 4. Obtiene datos del cliente   │
        │ 5. Valida estructuralmente    │
        └────────────────────┬───────────┘
                             │
                             ▼
        ┌────────────────────────────────┐
        │ Llama a DBIO Program:          │
        │ CALL "DBIOCUSM" USING REG-CUSM│
        └────────────────────┬───────────┘
                             │
                             ▼
        ┌────────────────────────────────┐
        │ DBIOCUSM.exe:                  │
        │                                │
        │ 1. COPY PATHS-FILE             │
        │ 2. Conecta a BD                │
        │ 3. Valida datos nuevamente     │
        │ 4. INSERT en tabla CLIENTES    │
        │ 5. COMMIT transacción          │
        └────────────────────┬───────────┘
                             │
                             ▼
        ┌────────────────────────────────┐
        │ Base de Datos:                 │
        │                                │
        │ INSERT INTO clientes VALUES(...)
        │                                │
        │ Registro creado exitosamente   │
        └────────────────────┬───────────┘
                             │
                             ▼
        ┌────────────────────────────────┐
        │ Retorna a CI0000               │
        │ Muestra confirmación al usuario│
        └────────────────────────────────┘
```

---

## Estructura de Rutas (Relative Paths)

```
proyecto_cobol/
│
├── .git/                           (Control de versiones)
├── .gitignore
├── README.md                       (★ Leer primero)
├── QUICK_START.md                  (★ Comienza aquí)
├── GUIA_ACTUALIZAR_CODIGO.md       (★ Cómo actualizar COBOL)
├── PLAN_REORGANIZACION.md
├── RESUMEN_EJECUCION.md
│
├── Crear_Estructura.bat            (→ Ejecutar primero)
├── Reorganizar_Proyecto.bat        (→ Ejecutar segundo)
├── Ejecutar_Proyecto.bat
│
├── src/
│   ├── mainline/
│   │   ├── BANCSMENU.cob
│   │   ├── CI0000.cbl ◄─────────┐
│   │   ├── BR0000.cbl           │
│   │   ├── IN0000.cbl           │
│   │   └── TC0000.cbl           │
│   │                             │
│   ├── dbio/                      │
│   │   ├── DBIOCUSM.cob          │
│   │   ├── DBIOBORM.cob          │
│   │   ├── DBIOINVM.cob          │
│   │   ├── DBIOTARJ.cob          │
│   │   └── DBIOTRAN.cob          │
│   │                             │
│   ├── copies/                    │
│   │   ├── PATHS.CPY ◄────────────┼─────── ⭐ IMPORTANTÍSIMO
│   │   │   └─ PATH-CONFIG = ".\config"
│   │   │   └─ FIL-DB-CONFIG = "db_config.cfg"
│   │   │   └─ etc...
│   │   ├── CUSMREC.CPY ◄──────────┼─────── Cada programa
│   │   ├── BORMREC.CPY            │       hace: COPY PATHS-FILE
│   │   ├── INVMREC.CPY            │
│   │   ├── TARJREC.CPY            │
│   │   └── LKCIF.CPY              │
│   │                             │
│   └── utils/                     │
│       ├── RP0000.cob             │
│       └── BAT000.cob             │
│                                 │
├── sql/                          │
│   ├── DBIOCUSM.sqb              │
│   ├── DBIOBORM.sqb              │
│   ├── DBIOINVM.sqb              │
│   ├── DBIOTARJ.sqb              │
│   ├── DBIOTRAN.sqb              │
│   ├── testconn.sqb              │
│   └── schema/                   │
│       └── (scripts creación)    │
│                                 │
├── config/                       │
│   ├── environment.properties ◄──┼─────── (Descarga de valores
│   ├── paths.cfg                │        desde environment.properties
│   ├── db_config.cfg             │
│   └── bdd.txt                  │
│                                 │
├── docs/                         │
│   ├── DOCUMENTACION_COMPLETA.md
│   ├── INDICE_DOCUMENTACION.md   │
│   └── DOCUMENTACION_COMPLETA.pdf│
│                                 │
├── bin/                          │
│   ├── BANCSMENU.exe ◄───────────┼──── Ejecutables compilados
│   ├── CI0000.exe                │
│   ├── BR0000.exe                │
│   ├── IN0000.exe                │
│   ├── TC0000.exe                │
│   └── ocsql.exp                 │
│                                 │
├── build/                        │
│   ├── logs/                     │
│   └── temp/                     │
│                                 │
└── test/
    ├── test_cases.cbl
    └── (archivos de prueba)
```

---

## Resumen Visual

```
┌──────────────────────────────────────────────────────┐
│                                                      │
│    USUARIO EJECUTA PROGRAMA → CARGA PATHS.CPY      │
│                         ↓                            │
│        PROGRAMA SABE DÓNDE BUSCAR ARCHIVOS         │
│                         ↓                            │
│    TODO FUNCIONA SIN CAMBIAR CÓDIGO HARDCODEADO    │
│                         ↓                            │
│  CAMBIO FUTURO: SOLO EDITAR PATHS.CPY             │
│                         ↓                            │
│  TODO EL PROYECTO USA LAS NUEVAS RUTAS ✓          │
│                                                      │
└──────────────────────────────────────────────────────┘
```

---

**Versión**: 1.0  
**Fecha**: 2026-05-13  
**Estado**: Implementado ✅

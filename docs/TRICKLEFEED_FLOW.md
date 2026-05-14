# Flujo Trickle Feed — Mapeo y Recomendaciones

Fecha: 2026-05-14

Resumen
- Este documento mapea cómo el proyecto procesa lotes de transacciones tipo "trickle feed" desde archivos .txt hasta la afectación de saldos y la generación de reportes. Incluye recomendaciones operativas y técnicas para robustecer el flujo y el demonio.

1. Archivo de entrada
- Formato esperado: delimitado por pipes `|`.
- Estructura: `OPERACION|CUENTA|MONTO_SIN_DECIMALES`
  - `OPERACION`: `DEP` o `RET`.
  - `CUENTA`: identificador del cliente (ej. 22345679).
  - `MONTO`: 15 dígitos; últimos 2 = decimales (ej. `000000000150002` -> 1500.02).
- Ejemplo de línea: `DEP|22345679|000000000150002`

2. Demonio / Orquestador
- El demonio es un bucle (archivo `.bat` o similar) que despierta al motor (`TFDRMAIN.exe` o al COBOL correspondiente) periódicamente (ej. 30s). Su propósito es disponibilidad y re-arranque automático.
- Ficheros y scripts relevantes:
  - [bin/monitor_lotes.bat](bin/monitor_lotes.bat)
  - [demon/daemon_bancario.bat](demon/daemon_bancario.bat)
  - [src/mainline/TFDRMAIN.cob](src/mainline/TFDRMAIN.cob) (o [sql/TFDRMAIN.sqb](sql/TFDRMAIN.sqb) como referencia)

3. Máquina de estados y fases (mapeo actual)
| Fase | Programa | Acción | Tablas involucradas |
|------|----------|--------|--------------------|
| 00 | TFTRCT | Validación inicial: existencia de archivo y fecha vs `control_secuencias` | `TFFM`, `control_secuencias` |
| 10 | TFMX | Ingesta / Staging: leer .txt y escribir en réplica (`TF06`) | `TF06`, `TFFM` |
| 20 | RRD000 | Procesamiento de negocio: afecta saldos, aplica reglas | `TF06`, `clientes` (y otras tablas core) |
| 30 | XXXREP | Generación de output físico `.out` con resultados del lote | `TF06` |
| 40 | TFTRCT | Cierre: marcar lote finalizado en `TFFM` | `TFFM` |

Notas:
- `TF06` actúa como réplica / cola: cada registro tiene `ESTADO` (2 Pendiente, 3 Procesando, 4 OK, 7 Error).
- El demonio procesa solo lotes con `FASE < 40` y `ESTADO_REPLICA = 'R'` según la descripción.

4. Control de fechas contables
- `control_secuencias` provee la fecha contable. Si hay desajuste entre la fecha en el archivo y la fecha contable, el lote se detiene para evitar inconsistencias.

5. Logging y trazabilidad
- Mensajes observables en terminal: `CONEXION EXITOSA`, `LOTE ENCONTRADO`, `EJECUTANDO TFMX`, `EJECUTANDO RRD000`, `EJECUTANDO XXXREP`.
- Recomendación: dejar trazas correlacionadas por `ID_LOTE` y `TRACE_ID` para facilitar búsqueda en logs.

6. Requisitos operativos críticos (ya listados y confirmados)
- Modo silencioso para programas invocados desde batch: evitar `ACCEPT`/pausas interactivas.
- Uso de rutas relativas: `..\\banco\\spool\\...` para portabilidad.
- La carpeta `TRICKLE-FEED-REPORT` debe existir y tener permisos de escritura.
- `COMMIT` estratégico tras finalizar cada fase para visibilidad entre programas.

7. Riesgos detectados y mejoras recomendadas
- Idempotencia y reintentos:
  - Asegurar que la re-ejecución de un lote incompleto no produzca doble aplicación de movimientos. Implementar marcas de idempotencia por `ID_TRANSACCION` y `ID_LOTE`.
- Bloqueos y concurrencia:
  - Evitar que dos instancias procesen el mismo lote. Añadir lock lógico en `TFFM` (p.ej. `locked_by`, `locked_at`) o un lock DB (SELECT ... FOR UPDATE) durante la fase 10->20.
- Manejo de errores transaccionales:
  - Ejecutar `COMMIT` solo cuando la fase se completa; en errores, `ROLLBACK` y marcar `TF06` con `ESTADO=7` y `ERROR_CODE`/`ERROR_MSG`.
- Observabilidad y alertas:
  - Añadir métricas: filas ingestadas por minuto, tasa de errores, tiempo medio por fase.
  - Alertas por lotes con errores > umbral o por no completados en X horas.
- Validación de entrada y control de formatos:
  - Validar longitud y formato del campo `MONTO` antes de insertar en `TF06`.
- Consistencia contable:
  - Verificar timezones y reglas de cierre contable entre `control_secuencias` y timestamps de archivo.
- Pruebas y sandbox:
  - Añadir suite de pruebas de integración que simule archivos con casos límite (montos 0, negativos, errores de formato, duplicados).

8. Cambios técnicos sugeridos (prioritarios)
- Implementar locking en `TFFM` para asignar lotes a worker fijo.
- Añadir campo `TRACE_ID` y `ORIGEN` en `TF06` para trazabilidad completa.
- Mejorar logs con nivel (INFO/WARN/ERROR) y persistencia central (archivo rotado o syslog/ELK).
- Crear scripts de comprobación previa (pre-flight) que validen la estructura del spool y la fecha contable antes de INSERT.

9. Checklist de despliegue y operación
- Confirmar que `TRICKLE-FEED-REPORT` existe y es accesible por el usuario del proceso.
- Verificar que los COBOL llamados no usan `ACCEPT` en modo lote.
- Revisar permisos y rutas relativas en `demon/daemon_bancario.bat` y `bin/monitor_lotes.bat`.
- Probar ciclo end-to-end en entorno de pruebas que imite `control_secuencias` real.

10. Próximos pasos (acciónable)
- Implementar locking lógico en `TFFM` y prueba de concurrencia.
- Añadir validaciones en `TFMX` antes del INSERT a `TF06`.
- Instrumentar métricas y alertas básicas.

Referencias a código en el repo
- [src/mainline/TFTRCT.cob](src/mainline/TFTRCT.cob) (orquestador)
- [src/mainline/TFMX.cob](src/mainline/TFMX.cob) (ingesta)
- [src/mainline/RRD000.cob](src/mainline/RRD000.cob) (procesamiento negocio)
- [src/mainline/XXXREP.cob](src/mainline/XXXREP.cob) (generación reportes)
- [sql/TFDRMAIN.sqb](sql/TFDRMAIN.sqb)

Si quieres, puedo:
- Implementar bloqueo lógico y añadir los campos propuestos en `TFFM` y `TF06`.
- Crear pruebas de integración que simulen lotes y errores.

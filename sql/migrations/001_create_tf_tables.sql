-- Migration: 001_create_tf_tables.sql
-- Created: 2026-05-15
-- Purpose: Crear tablas de parametrización/replcias/estados/reintentos y extender tffm/tf06

SET FOREIGN_KEY_CHECKS = 0;

-- 1) Crear tablas de parametrización y control
CREATE TABLE IF NOT EXISTS tf_parametros (
    param_key VARCHAR(100) PRIMARY KEY,
    param_value VARCHAR(200),
    descripcion VARCHAR(255)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS tf_replicas (
    replica_no INT PRIMARY KEY,
    replica_name VARCHAR(50),
    max_parallel INT DEFAULT 1,
    status VARCHAR(20) DEFAULT 'IDLE',
    last_heartbeat TIMESTAMP NULL,
    current_load INT DEFAULT 0
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS tf_estados (
    estado_code VARCHAR(20) PRIMARY KEY,
    descripcion VARCHAR(100)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS tf_reintentos (
    id INT AUTO_INCREMENT PRIMARY KEY,
    id_lote INT NOT NULL,
    intentos_realizados INT DEFAULT 0,
    max_intentos INT DEFAULT 3,
    ultimo_error VARCHAR(255),
    last_try TIMESTAMP NULL,
    FOREIGN KEY (id_lote) REFERENCES tffm(ID_LOTE) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Poblar estados basicos
INSERT IGNORE INTO tf_estados (estado_code, descripcion) VALUES
('PENDIENTE','En espera'),
('PROCESANDO','En ejecución'),
('EXITOSO','Finalizado'),
('FALLIDO','Error'),
('REINTENTO','Reprocesando');

-- 2) Extender tffm con columnas nuevas (creadas como NULLABLE / sin constraints fuertes)
ALTER TABLE tffm
    ADD COLUMN IF NOT EXISTS FILE_NAME VARCHAR(200) NULL,
    ADD COLUMN IF NOT EXISTS TYPE_UPDATE VARCHAR(50) NULL,
    ADD COLUMN IF NOT EXISTS REPLICA_NO INT NULL,
    ADD COLUMN IF NOT EXISTS STATUS VARCHAR(20) DEFAULT 'PENDIENTE',
    ADD COLUMN IF NOT EXISTS PRIORITY INT DEFAULT 0,
    ADD COLUMN IF NOT EXISTS TIME_PROCESS TIME NULL,
    ADD COLUMN IF NOT EXISTS NO_TRANSACCION INT DEFAULT 0;

-- Índices adicionales
ALTER TABLE tffm
    ADD INDEX IF NOT EXISTS IDX_TFFM_REPLICA (REPLICA_NO),
    ADD INDEX IF NOT EXISTS IDX_TFFM_STATUS (STATUS),
    ADD INDEX IF NOT EXISTS IDX_TFFM_FILENAME (FILE_NAME(100));

-- 3) Extender tf06 con columnas estructuradas (se mantienen DATOS_TX para raw)
ALTER TABLE tf06
    ADD COLUMN IF NOT EXISTS OPERATION CHAR(3) NULL,
    ADD COLUMN IF NOT EXISTS ACCOUNT VARCHAR(50) NULL,
    ADD COLUMN IF NOT EXISTS AMOUNT_DECIMAL DECIMAL(15,2) NULL,
    ADD COLUMN IF NOT EXISTS TRACE_ID VARCHAR(36) NULL,
    ADD COLUMN IF NOT EXISTS ORIGEN VARCHAR(50) NULL,
    ADD COLUMN IF NOT EXISTS INSERTED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP NULL;

ALTER TABLE tf06
    ADD INDEX IF NOT EXISTS IDX_TF06_OP (OPERATION),
    ADD INDEX IF NOT EXISTS IDX_TF06_ACCOUNT (ACCOUNT(30)),
    ADD INDEX IF NOT EXISTS IDX_TF06_TRACE (TRACE_ID),
    ADD INDEX IF NOT EXISTS IDX_TF06_LOTE_OP (ID_LOTE, OPERATION);

-- 4) Migración parcial de datos existentes: mapear NOMBRE_ARCHIVO -> FILE_NAME y TIPO_PROG -> TYPE_UPDATE
UPDATE tffm SET FILE_NAME = NOMBRE_ARCHIVO WHERE FILE_NAME IS NULL OR FILE_NAME = '';
UPDATE tffm SET TYPE_UPDATE = TIPO_PROG WHERE TYPE_UPDATE IS NULL OR TYPE_UPDATE = '';
UPDATE tffm SET NO_TRANSACCION = TOTAL_REGISTROS WHERE NO_TRANSACCION = 0 OR NO_TRANSACCION IS NULL;

-- 5) Intento de parseo de DATOS_TX para poblar columnas estructuradas (solo filas con formato campo1|campo2|campo3)
-- Se asume formato: OP|ACCOUNT|AMOUNTCENTS (ej. DEP|22345679|0000000050000)
UPDATE tf06
SET
    OPERATION = SUBSTRING_INDEX(DATOS_TX,'|',1),
    ACCOUNT = SUBSTRING_INDEX(SUBSTRING_INDEX(DATOS_TX,'|',2),'|',-1),
    AMOUNT_DECIMAL = CAST(CONVERT(SUBSTRING_INDEX(DATOS_TX,'|',-1),UNSIGNED) AS DECIMAL) / 100
WHERE OPERATION IS NULL
  AND DATOS_TX LIKE '%|%|%';

-- 6) Crear tabla auxiliar para sub-lotes (opcional / ayudará a mantener relación entre lote padre y sublotes)
CREATE TABLE IF NOT EXISTS tffm_sublotes (
    sub_id INT AUTO_INCREMENT PRIMARY KEY,
    parent_lote INT NOT NULL,
    sub_nombre VARCHAR(200),
    registros INT DEFAULT 0,
    replica_no INT NULL,
    estado VARCHAR(20) DEFAULT 'PENDIENTE',
    fecha_creacion TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (parent_lote) REFERENCES tffm(ID_LOTE) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

SET FOREIGN_KEY_CHECKS = 1;

-- Notes: Ejecutar este script primero en entorno de pruebas. Las columnas se crean NULLABLE para minimizar impacto.
-- Recomendación: crear dump previo con `mysqldump --routines --triggers proyecto_cobol > backup_pre_migration.sql`

-- FIN DE MIGRACION

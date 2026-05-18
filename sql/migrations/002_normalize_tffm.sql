-- =========================================================
-- MIGRACIÓN: Normalizar TFFM y añadir réplicas + logs
-- Archivo: sql/migrations/002_normalize_tffm.sql
-- Requisitos: hacer backup de la BD antes de ejecutar
-- =========================================================

USE proyecto_cobol;

SET FOREIGN_KEY_CHECKS = 0;

-- 1) Crear nueva tabla de lotes normalizada
CREATE TABLE IF NOT EXISTS tffm_lotes (
    id_lote INT AUTO_INCREMENT PRIMARY KEY,
    file_name VARCHAR(255) NOT NULL,
    type_update VARCHAR(20),
    replica_no VARCHAR(20),
    status VARCHAR(30) NOT NULL DEFAULT 'PENDING',
    priority INT DEFAULT 0,
    time_ref TIME,
    no_transaccion INT DEFAULT 0,
    estado_replica VARCHAR(10),
    fecha_sistema DATE,
    fecha_hora_alt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    meta JSON,
    KEY idx_tffm_lotes_status (status),
    KEY idx_tffm_lotes_replica (replica_no)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 2) Tabla de réplicas
CREATE TABLE IF NOT EXISTS replicas (
    replica_id VARCHAR(10) PRIMARY KEY,
    host VARCHAR(100),
    capacity INT DEFAULT 10000,
    current_load INT DEFAULT 0,
    status ENUM('UP','DOWN','MAINT') DEFAULT 'UP',
    last_heartbeat DATETIME,
    meta JSON
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 3) Tabla de logs centralizados
CREATE TABLE IF NOT EXISTS tf_logs (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    ts DATETIME DEFAULT CURRENT_TIMESTAMP,
    program VARCHAR(64),
    replica VARCHAR(20),
    file_name VARCHAR(255),
    lote_id INT,
    nivel VARCHAR(10),
    mensaje TEXT,
    error_code VARCHAR(50),
    retry_no INT DEFAULT 0,
    old_status VARCHAR(30),
    new_status VARCHAR(30),
    extra JSON,
    KEY idx_tf_logs_ts (ts),
    KEY idx_tf_logs_lote (lote_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 4) Tabla para hashes/duplicados
CREATE TABLE IF NOT EXISTS tffm_hashes (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    file_name VARCHAR(255),
    record_hash VARCHAR(64),
    lote_id INT,
    attempt_id VARCHAR(50),
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    UNIQUE KEY ux_file_hash (file_name, record_hash, attempt_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 5) Migrar datos existentes desde la tabla tffm
-- Mapear campos: FASE -> status (mapeo simple), TIPO_PROG -> type_update, ESTADO_REPLICA -> estado_replica

INSERT INTO tffm_lotes (
    id_lote,
    file_name,
    type_update,
    status,
    no_transaccion,
    estado_replica,
    fecha_sistema,
    fecha_hora_alt,
    meta
)
SELECT
    ID_LOTE,
    NOMBRE_ARCHIVO,
    TIPO_PROG,
    CASE FASE
        WHEN '00' THEN 'PENDING'
        WHEN '10' THEN 'VALIDATING'
        WHEN '20' THEN 'ASSIGNED'
        WHEN '30' THEN 'PROCESSING'
        WHEN '40' THEN 'COMPLETED'
        ELSE 'UNKNOWN'
    END AS status,
    TOTAL_REGISTROS,
    ESTADO_REPLICA,
    FECHA_SISTEMA,
    FECHA_HORA_ALT,
    JSON_OBJECT('total_ok', TOTAL_OK, 'total_error', TOTAL_ERROR, 'legacy_fase', FASE)
FROM tffm;

-- Ajustar AUTO_INCREMENT para preservar continuidad
SET @maxid = (SELECT IFNULL(MAX(id_lote),0) FROM tffm_lotes);
SET @sql = CONCAT('ALTER TABLE tffm_lotes AUTO_INCREMENT = ', @maxid + 1);
PREPARE stmt FROM @sql; EXECUTE stmt; DEALLOCATE PREPARE stmt;

-- 6) Crear vista de compatibilidad si algún proceso aún consulta la tabla antigua structure
CREATE OR REPLACE VIEW v_tffm AS
SELECT
    id_lote AS ID_LOTE,
    file_name AS NOMBRE_ARCHIVO,
    -- reconstruir FASE desde status si es posible
    CASE
        WHEN status = 'PENDING' THEN '00'
        WHEN status = 'VALIDATING' THEN '10'
        WHEN status = 'ASSIGNED' THEN '20'
        WHEN status = 'PROCESSING' THEN '30'
        WHEN status = 'COMPLETED' THEN '40'
        ELSE '99'
    END AS FASE,
    estado_replica AS ESTADO_REPLICA,
    type_update AS TIPO_PROG,
    no_transaccion AS TOTAL_REGISTROS,
    JSON_UNQUOTE(JSON_EXTRACT(meta, '$.total_ok')) AS TOTAL_OK,
    JSON_UNQUOTE(JSON_EXTRACT(meta, '$.total_error')) AS TOTAL_ERROR,
    fecha_sistema AS FECHA_SISTEMA,
    fecha_hora_alt AS FECHA_HORA_ALT
FROM tffm_lotes;

-- 7) Recomendaciones: backup y validación manual antes de DROP
-- NOTA: No eliminar la tabla `tffm` automáticamente: revisar aplicaciones que la usan.

SET FOREIGN_KEY_CHECKS = 1;

-- FIN MIGRACIÓN

-- ================================================================
-- BANCO LAF - CORE BANCARIO - NIVEL 1000 TCS ECUADOR
-- Schema v2.0 - Multi-cuenta, Scoring, Auditoria Completa
-- ================================================================

CREATE DATABASE IF NOT EXISTS proyecto_cobol
    CHARACTER SET utf8mb4
    COLLATE utf8mb4_spanish_ci;

USE proyecto_cobol;

-- ================================================================
-- DROP ORDER (respetar FK)
-- ================================================================
DROP TABLE IF EXISTS movimientos;
DROP TABLE IF EXISTS tarjetas_diferidos;
DROP TABLE IF EXISTS tarjetas;
DROP TABLE IF EXISTS hipotecas;
DROP TABLE IF EXISTS ctactes;
DROP TABLE IF EXISTS clientes;
DROP TABLE IF EXISTS control_secuencias;
DROP TABLE IF EXISTS TF06;
DROP TABLE IF EXISTS TFFM;

-- ================================================================
-- 1. CLIENTES - Master de identidad y scoring
-- ================================================================
CREATE TABLE clientes (
    ID_CLIENTE         INT NOT NULL AUTO_INCREMENT,
    TIPO_DOC           VARCHAR(3) NOT NULL,
    DOC_CLIENTE        VARCHAR(12) NOT NULL,
    FECHA_ALTA         DATE NOT NULL,
    NOMBRE_CLIENTE     VARCHAR(25) NOT NULL,
    APELLIDOS_CLIENTE  VARCHAR(25) NOT NULL,
    FECHA_NACIMIENTO   DATE NOT NULL,
    DIRECCION_CLIENTE  VARCHAR(45),
    TELEF_CLIENTE      VARCHAR(12),
    EMAIL_CLIENTE      VARCHAR(40),
    SCORE_CREDITICIO   INT DEFAULT 500,
    INGRESOS_MENSUALES DECIMAL(12,2) DEFAULT 0,
    TIENE_TARJETA      TINYINT(1) DEFAULT 0,
    TIENE_HIPOTECA     TINYINT(1) DEFAULT 0,
    ESTADO_CLIENTE     CHAR(1) DEFAULT 'A',
    SALDO_TOTAL_VISTA  DECIMAL(12,2) DEFAULT 0,
    PRIMARY KEY (ID_CLIENTE),
    UNIQUE KEY (TIPO_DOC, DOC_CLIENTE)
) ENGINE=InnoDB AUTO_INCREMENT=22345682 DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- 2. CUENTAS A LA VISTA (Multi-cuenta 1:N)
-- ================================================================
CREATE TABLE ctactes (
    ID_CUENTA          INT NOT NULL AUTO_INCREMENT,
    ID_CLIENTE         INT NOT NULL,
    TIPO_CUENTA        CHAR(1) NOT NULL,
    SALDO_ACTUAL       DECIMAL(15,2) DEFAULT 0,
    FECHA_APERTURA     DATE NOT NULL,
    ESTADO_CUENTA      CHAR(1) DEFAULT 'A',
    PRIMARY KEY (ID_CUENTA),
    CONSTRAINT FK_CTACTES_CLI FOREIGN KEY (ID_CLIENTE) REFERENCES clientes(ID_CLIENTE)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- 3. MOVIMIENTOS (Historial auditado)
-- ================================================================
CREATE TABLE movimientos (
    ID_MOVIMIENTO      INT NOT NULL AUTO_INCREMENT,
    ID_CUENTA          INT NOT NULL,
    FECHA_HORA         TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    TIPO_MOV           INT NOT NULL,
    IMPORTE            DECIMAL(15,2) NOT NULL,
    SALDO_RESULTANTE   DECIMAL(15,2) NOT NULL,
    TERMINAL_ID        VARCHAR(4),
    USUARIO_ID         VARCHAR(8),
    PRIMARY KEY (ID_MOVIMIENTO),
    CONSTRAINT FK_MOV_CTA FOREIGN KEY (ID_CUENTA) REFERENCES ctactes(ID_CUENTA)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- 4. TARJETAS DE CREDITO
-- ================================================================
CREATE TABLE tarjetas (
    NRO_TARJETA        VARCHAR(16) NOT NULL,
    ID_CLIENTE         INT NOT NULL,
    CUENTA_PRINCIPAL   INT NOT NULL,
    FECHA_EMISION      DATE NOT NULL,
    FECHA_VENCTO       DATE NOT NULL,
    CUPO_APROBADO      DECIMAL(12,2) NOT NULL,
    SALDO_UTILIZADO    DECIMAL(12,2) DEFAULT 0,
    ESTADO_TARJETA     CHAR(1) DEFAULT 'A',
    PRIMARY KEY (NRO_TARJETA),
    CONSTRAINT FK_TARJ_CLI FOREIGN KEY (ID_CLIENTE) REFERENCES clientes(ID_CLIENTE),
    CONSTRAINT FK_TARJ_CTA FOREIGN KEY (CUENTA_PRINCIPAL) REFERENCES ctactes(ID_CUENTA)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- 5. DIFERIDOS (Cuotas de tarjeta)
-- ================================================================
CREATE TABLE tarjetas_diferidos (
    ID_DIFERIDO        INT NOT NULL AUTO_INCREMENT,
    NRO_TARJETA        VARCHAR(16) NOT NULL,
    MONTO_COMPRA       DECIMAL(12,2) NOT NULL,
    CUOTAS_TOTALES     INT NOT NULL,
    CUOTAS_PENDIENTES  INT NOT NULL,
    VALOR_CUOTA        DECIMAL(12,2) NOT NULL,
    TASA_INTERES       DECIMAL(5,2) NOT NULL,
    PRIMARY KEY (ID_DIFERIDO),
    CONSTRAINT FK_DIF_TARJ FOREIGN KEY (NRO_TARJETA) REFERENCES tarjetas(NRO_TARJETA)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- 6. HIPOTECAS
-- ================================================================
CREATE TABLE hipotecas (
    ID_HIPOTECA        INT NOT NULL,
    ID_CLIENTE         INT NOT NULL,
    CUENTA_DEBITO      INT NOT NULL,
    MONTO_PRESTAMO     DECIMAL(15,2) NOT NULL,
    SALDO_DEUDA        DECIMAL(15,2) NOT NULL,
    TASA_ANUAL         DECIMAL(7,4) NOT NULL,
    CUOTA_MENSUAL      DECIMAL(15,2) NOT NULL,
    MESES_MORA         INT DEFAULT 0,
    ESTADO_PRESTAMO    VARCHAR(15) DEFAULT 'ACTIVO',
    FECHA_VENCIMIENTO  DATE NOT NULL,
    PRIMARY KEY (ID_HIPOTECA),
    CONSTRAINT FK_HIP_CLI FOREIGN KEY (ID_CLIENTE) REFERENCES clientes(ID_CLIENTE),
    CONSTRAINT FK_HIP_CTA FOREIGN KEY (CUENTA_DEBITO) REFERENCES ctactes(ID_CUENTA)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- 7. CONTROL DE SECUENCIAS
-- ================================================================
CREATE TABLE control_secuencias (
    PRODUCTO           VARCHAR(20) NOT NULL,
    ULTIMO_VALOR       INT NOT NULL DEFAULT 0,
    FECHA_REFRESCO     DATE,
    PRIMARY KEY (PRODUCTO)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

INSERT INTO control_secuencias (PRODUCTO, ULTIMO_VALOR, FECHA_REFRESCO)
VALUES ('HIPOTECA', 0, CURDATE());

-- ================================================================
-- 8. TFFM (Trickle Feed - NO TOCAR)
-- ================================================================
CREATE TABLE TFFM (
    ID_LOTE             INT NOT NULL AUTO_INCREMENT,
    NOMBRE_ARCHIVO      VARCHAR(50) NOT NULL,
    FASE                CHAR(2) NOT NULL DEFAULT '00',
    TIPO_PROG           CHAR(3) NOT NULL DEFAULT 'EPG',
    ESTADO_REPLICA      CHAR(1) NOT NULL DEFAULT 'R',
    PRIMARY KEY (ID_LOTE)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- 9. TF06 (Trickle Feed registros - NO TOCAR)
-- ================================================================
CREATE TABLE TF06 (
    ID_REGISTRO         INT NOT NULL AUTO_INCREMENT,
    ID_LOTE             INT NOT NULL,
    ESTADO              TINYINT NOT NULL DEFAULT 2,
    DATOS_TX            VARCHAR(500) NOT NULL DEFAULT '',
    COD_ERROR           VARCHAR(4) NULL DEFAULT NULL,
    PRIMARY KEY (ID_REGISTRO),
    KEY idx_tf06_lote_estado (ID_LOTE, ESTADO),
    CONSTRAINT fk_tf06_lote FOREIGN KEY (ID_LOTE) REFERENCES TFFM(ID_LOTE)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- VISTA GERENCIAL KPI
-- ================================================================
CREATE VIEW vista_gerencial AS
SELECT
    COUNT(ID_CLIENTE)    AS total_clientes,
    SUM(SALDO_TOTAL_VISTA) AS liquidez_total,
    (SELECT SUM(SALDO_DEUDA) FROM hipotecas) AS cartera_hipotecaria
FROM clientes WHERE ESTADO_CLIENTE = 'A';

-- ================================================================
-- NOTA PARA BAT000 (Cierre Mensual):
-- BAT000 debe actualizar sus queries para usar:
-- clientes: ESTADO_CLIENTE en lugar de CTA_ACTIVA
--           TIENE_HIPOTECA en lugar de HIPOTECA
-- hipotecas: SALDO_DEUDA en lugar de SALDO_ACTUAL
--            ESTADO_PRESTAMO en lugar de ESTADO
--            TASA_ANUAL en lugar de TASA_INTERES
--            MONTO_PRESTAMO en lugar de MONTO_ORIGINAL
--            CUENTA_DEBITO (nuevo campo, FK a ctactes)
--            FECHA_VENCIMIENTO en lugar de FECHA_VENCTO
-- ctactes: PK ahora es ID_CUENTA (no ID_CLIENTE)
--          usar WHERE ID_CLIENTE = ? con JOIN
-- Las tablas AUDIT_MAESTRA, AUDIT_HIPOTECAS, AUDIT_TARJETAS
-- son creadas por BAT000 y no cambian.
-- ================================================================

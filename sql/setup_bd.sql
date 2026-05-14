-- ================================================================
-- SCRIPT DE CREACION DE BASE DE DATOS
-- SISTEMA BANCARIO COBOL
-- Generado desde: DBIOCUSM.sqb, DBIOINVM.sqb, DBIOTARJ.sqb,
--                 DBIOBORM.sqb, TFTRCT.sqb, TFMX.sqb, RRD000.sqb
-- ================================================================

-- Crear y seleccionar la base de datos
CREATE DATABASE IF NOT EXISTS proyecto_cobol
    CHARACTER SET utf8mb4
    COLLATE utf8mb4_spanish_ci;

USE proyecto_cobol;

-- ================================================================
-- TABLA: clientes
-- Fuente: DBIOCUSM.sqb
-- ================================================================
CREATE TABLE IF NOT EXISTS clientes (
    ID_CLIENTE          INT              NOT NULL AUTO_INCREMENT,
    TIPO_DOC            VARCHAR(3)       NOT NULL DEFAULT 'CC',
    DOC_CLIENTE         VARCHAR(12)      NOT NULL,
    FECHA_ALTA          VARCHAR(10)      NOT NULL DEFAULT '0000-00-00',
    NOMBRE_CLIENTE      VARCHAR(25)      NOT NULL DEFAULT '',
    APELLIDOS_CLIENTE   VARCHAR(25)      NOT NULL DEFAULT '',
    DIRECCION_CLIENTE   VARCHAR(45)      NOT NULL DEFAULT '',
    TELEF_CLIENTE       VARCHAR(12)      NOT NULL DEFAULT '',
    EMAIL_CLIENTE       VARCHAR(40)      NOT NULL DEFAULT '',
    TARJETA             TINYINT(1)       NOT NULL DEFAULT 0,
    CREDITO             TINYINT(1)       NOT NULL DEFAULT 0,
    HIPOTECA            TINYINT(1)       NOT NULL DEFAULT 0,
    CTA_ACTIVA          TINYINT(1)       NOT NULL DEFAULT 1,
    FECHA_CIERRE        VARCHAR(10)      NULL     DEFAULT NULL,
    SALDO_CLIENTE       DECIMAL(12,2)    NOT NULL DEFAULT 0.00,
    PRIMARY KEY (ID_CLIENTE),
    UNIQUE KEY uk_doc_cliente (DOC_CLIENTE)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- TABLA: ctactes  (Cuentas Corrientes)
-- Fuente: DBIOINVM.sqb
-- ================================================================
CREATE TABLE IF NOT EXISTS ctactes (
    ID_CLIENTE          INT              NOT NULL,
    COD_ULT_MOV         SMALLINT         NOT NULL DEFAULT 0,
    FECHA_ULT_MOV       VARCHAR(10)      NOT NULL DEFAULT '0000-00-00',
    IMPORTE_MOV         DECIMAL(12,2)    NOT NULL DEFAULT 0.00,
    SALDO_ACTUAL        DECIMAL(12,2)    NOT NULL DEFAULT 0.00,
    PRIMARY KEY (ID_CLIENTE),
    CONSTRAINT fk_ctactes_cliente
        FOREIGN KEY (ID_CLIENTE) REFERENCES clientes(ID_CLIENTE)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- TABLA: TARJETAS
-- Fuente: DBIOTARJ.sqb
-- ESTADO: 'A'=Activa  'B'=Bloqueada  'I'=Inactiva/Cancelada
-- ================================================================
CREATE TABLE IF NOT EXISTS TARJETAS (
    NRO_TARJETA         VARCHAR(16)      NOT NULL,
    ID_CLIENTE          INT              NOT NULL,
    FECHA_EMISION       VARCHAR(10)      NOT NULL DEFAULT '0000-00-00',
    FECHA_VENCIMIENTO   VARCHAR(10)      NOT NULL DEFAULT '0000-00-00',
    LIMITE_TARJETA      DECIMAL(12,2)    NOT NULL DEFAULT 0.00,
    ACUM_MES            DECIMAL(12,2)    NOT NULL DEFAULT 0.00,
    LIQUIDACION_MES     DECIMAL(12,2)    NOT NULL DEFAULT 0.00,
    ESTADO              CHAR(1)          NOT NULL DEFAULT 'A',
    PRIMARY KEY (NRO_TARJETA),
    CONSTRAINT fk_tarjetas_cliente
        FOREIGN KEY (ID_CLIENTE) REFERENCES clientes(ID_CLIENTE)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- TABLA: hipotecas
-- Fuente: DBIOBORM.sqb
-- ESTADO: 'ACTIVO'  'MOROSO'  'CASTIGADO'  'CANCELADO'
-- ================================================================
CREATE TABLE IF NOT EXISTS hipotecas (
    ID_HIPOTECA         INT              NOT NULL,
    ID_CLIENTE          INT              NOT NULL,
    FECHA_INICIO        VARCHAR(10)      NOT NULL DEFAULT '0000-00-00',
    MONTO_ORIGINAL      DECIMAL(15,2)    NOT NULL DEFAULT 0.00,
    TASA_INTERES        DECIMAL(7,4)     NOT NULL DEFAULT 0.0000,
    SALDO_ACTUAL        DECIMAL(15,2)    NOT NULL DEFAULT 0.00,
    FECHA_VENCTO        VARCHAR(10)      NOT NULL DEFAULT '0000-00-00',
    DIA_PAGO            TINYINT          NOT NULL DEFAULT 1,
    ESTADO              VARCHAR(20)      NOT NULL DEFAULT 'ACTIVO',
    CUOTA_MENSUAL       DECIMAL(15,2)    NOT NULL DEFAULT 0.00,
    MESES_MORA          SMALLINT         NOT NULL DEFAULT 0,
    FECHA_ULT_PAGO      VARCHAR(10)      NULL     DEFAULT NULL,
    PRIMARY KEY (ID_HIPOTECA),
    CONSTRAINT fk_hipotecas_cliente
        FOREIGN KEY (ID_CLIENTE) REFERENCES clientes(ID_CLIENTE)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- TABLA: control_secuencias
-- Fuente: DBIOBORM.sqb (secuencia de hipotecas)
--         BNCR004.sqb  (fecha contable del sistema)
-- ================================================================
CREATE TABLE IF NOT EXISTS control_secuencias (
    TIPO_PRODUCTO       VARCHAR(20)      NOT NULL,
    ULTIMO_NUMERO       INT              NOT NULL DEFAULT 0,
    FECHA_PROCESO       VARCHAR(10)      NOT NULL DEFAULT '0000-00-00',
    PRIMARY KEY (TIPO_PRODUCTO)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- Fila inicial requerida por DBIOBORM y BNCR004
INSERT IGNORE INTO control_secuencias (TIPO_PRODUCTO, ULTIMO_NUMERO, FECHA_PROCESO)
VALUES ('HIPOTECA', 0, DATE_FORMAT(CURDATE(), '%Y-%m-%d'));

-- ================================================================
-- TABLA: TFFM  (Control de Lotes - Trickle Feed)
-- Fuente: TFTRCT.sqb, monitor_lotes.bat
-- FASE:           '00'=Registrado  '10'=Cargado  '20'=Procesado
--                 '30'=Reportado   '40'=Completo
-- ESTADO_REPLICA: 'R'=Listo para procesar
-- TIPO_PROG:      'EPG'=Cuentas (IN0000)  'TRF'=Hipotecas (BR0000)
-- ================================================================
CREATE TABLE IF NOT EXISTS TFFM (
    ID_LOTE             INT              NOT NULL AUTO_INCREMENT,
    NOMBRE_ARCHIVO      VARCHAR(50)      NOT NULL,
    FASE                CHAR(2)          NOT NULL DEFAULT '00',
    TIPO_PROG           CHAR(3)          NOT NULL DEFAULT 'EPG',
    ESTADO_REPLICA      CHAR(1)          NOT NULL DEFAULT 'R',
    PRIMARY KEY (ID_LOTE)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- ================================================================
-- TABLA: TF06  (Registros individuales del lote)
-- Fuente: TFMX.sqb, RRD000.sqb, XXXREP.sqb
-- ESTADO: 2=Pendiente  3=Procesando  4=Exitoso  7=Error  8=Critico
-- ================================================================
CREATE TABLE IF NOT EXISTS TF06 (
    ID_REGISTRO         INT              NOT NULL AUTO_INCREMENT,
    ID_LOTE             INT              NOT NULL,
    ESTADO              TINYINT          NOT NULL DEFAULT 2,
    DATOS_TX            VARCHAR(500)     NOT NULL DEFAULT '',
    COD_ERROR           VARCHAR(4)       NULL     DEFAULT NULL,
    PRIMARY KEY (ID_REGISTRO),
    KEY idx_tf06_lote_estado (ID_LOTE, ESTADO),
    CONSTRAINT fk_tf06_lote
        FOREIGN KEY (ID_LOTE) REFERENCES TFFM(ID_LOTE)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

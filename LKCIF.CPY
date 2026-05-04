      *================================================================*
      * COPYBOOK: LKCIF                                                *
      * DESCRIPCION: AREA DE COMUNICACION PARA EL MODULO CIF (CLIENTES)*
      * USO: LINKAGE SECTION EN PROGRAMAS MAINLINE Y MODULOS DBIO      *
      *================================================================*
       01  LK-DATOS-TRANSACCION.
      *    CODIGO DE ACCION PARA BASE DE DATOS (FLAG DE OPERACION)
      *    'A' = ALTA (INSERT)
      *    'B' = BAJA LOGICA (UPDATE ESTADO)
      *    'M' = MODIFICACION (UPDATE)
      *    'C' = CONSULTA (SELECT)
           05 LK-ACCION-DB             PIC X(01).
           05 LK-COD-RETORNO           PIC 9(02).
           05 LK-MENSAJE               PIC X(50).
           05 LK-ID-CLIENTE            PIC 9(09).
           05 LK-IMPORTE-TRANSACCION   PIC S9(13)V99 COMP-3.
           05 LK-MODO-OPERACION        PIC X(01).
              88 LK-CONSULTA           VALUE 'C'.
              88 LK-ALTA               VALUE 'A'.

      *    CODIGO DE RETORNO (RESULTADO DE LA OPERACION)
      *    00 = EXITO / OPERACION REALIZADA
      *    01-98 = ERRORES DE NEGOCIO O VALIDACION
      *    99 = ERROR CRITICO DE BASE DE DATOS (SQLCODE != 0)
           05 LK-COD-RETORNO           PIC 9(02).

      *    MENSAJE DESCRIPTIVO PARA EL USUARIO O LOG
           05 LK-MENSAJE               PIC X(50).

      *    CAMPOS DE CONTROL DE AUDITORIA (OPCIONALES PARA EL CORE)[cite: 1, 2]
      *    ID DEL USUARIO QUE REALIZA LA TRANSACCION
           05 LK-USUARIO-ID            PIC X(08).
      *    ID DE LA TERMINAL O AGENCIA
           05 LK-TERMINAL-ID           PIC X(04).
      *    FECHA CONTABLE DEL SISTEMA (YYYYMMDD)
           05 LK-FECHA-PROCESO         PIC 9(08).

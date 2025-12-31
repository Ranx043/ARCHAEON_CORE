# COBOL_04: Manejo de Archivos

> "En el mainframe, los archivos son la sangre del sistema. COBOL es el corazón que los bombea."

---

## Tipos de Archivos

| Tipo | Organización | Acceso | VSAM Equiv |
|------|--------------|--------|------------|
| SEQUENTIAL | Secuencial | Secuencial | ESDS |
| INDEXED | Por clave | Secuencial/Random | KSDS |
| RELATIVE | Por número de registro | Secuencial/Random | RRDS |
| LINE SEQUENTIAL | Por líneas (texto) | Secuencial | N/A |

---

## ENVIRONMENT DIVISION: SELECT

### SELECT Básico

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-------------------------------------------------
      * ARCHIVO SECUENCIAL
      *-------------------------------------------------
           SELECT ARCHIVO-ENTRADA
               ASSIGN TO "ENTRADA.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-STATUS-ENT.

      *-------------------------------------------------
      * ARCHIVO INDEXADO (VSAM KSDS)
      *-------------------------------------------------
           SELECT ARCHIVO-CLIENTES
               ASSIGN TO CLIENTES
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-NUMERO
               ALTERNATE RECORD KEY IS CLI-RFC
                   WITH DUPLICATES
               FILE STATUS IS WS-STATUS-CLI.

      *-------------------------------------------------
      * ARCHIVO RELATIVO (VSAM RRDS)
      *-------------------------------------------------
           SELECT ARCHIVO-HASH
               ASSIGN TO HASHFILE
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS RANDOM
               RELATIVE KEY IS WS-NUMERO-REG
               FILE STATUS IS WS-STATUS-HASH.
```

### SELECT para Mainframe (JCL)

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * En mainframe, el nombre se asocia vía JCL DD
           SELECT ARCHIVO-MOVIMIENTOS
               ASSIGN TO MOVTOS
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS-MOVTOS.

      * El JCL tendrá:
      * //MOVTOS   DD DSN=PROD.MOVIMIENTOS.DATA,DISP=SHR
```

---

## DATA DIVISION: FILE SECTION

### Archivo Secuencial de Longitud Fija

```cobol
       DATA DIVISION.
       FILE SECTION.

       FD  ARCHIVO-ENTRADA
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 150 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS REG-ENTRADA.

       01  REG-ENTRADA.
           05  ENT-TIPO          PIC X(1).
           05  ENT-NUMERO        PIC 9(10).
           05  ENT-NOMBRE        PIC X(40).
           05  ENT-DIRECCION     PIC X(60).
           05  ENT-MONTO         PIC S9(9)V99 COMP-3.
           05  ENT-FECHA         PIC 9(8).
           05  FILLER            PIC X(25).
```

### Archivo Secuencial de Longitud Variable

```cobol
       FD  ARCHIVO-VARIABLE
           RECORDING MODE IS V
           BLOCK CONTAINS 0 RECORDS
           RECORD IS VARYING IN SIZE
               FROM 50 TO 500 CHARACTERS
               DEPENDING ON WS-LONG-REG
           LABEL RECORDS ARE STANDARD.

       01  REG-VARIABLE.
           05  VAR-HEADER        PIC X(50).
           05  VAR-DETALLE       PIC X(450).
```

### Archivo Indexado (VSAM KSDS)

```cobol
       FD  ARCHIVO-CLIENTES
           RECORD CONTAINS 200 CHARACTERS.

       01  REG-CLIENTE.
           05  CLI-NUMERO        PIC 9(10).
           05  CLI-RFC           PIC X(13).
           05  CLI-NOMBRE        PIC X(40).
           05  CLI-DIRECCION.
               10  CLI-CALLE     PIC X(40).
               10  CLI-CIUDAD    PIC X(20).
               10  CLI-CP        PIC X(5).
           05  CLI-SALDO         PIC S9(11)V99 COMP-3.
           05  CLI-FECHA-ALTA    PIC 9(8).
           05  CLI-ESTADO        PIC X(1).
           05  FILLER            PIC X(60).
```

### Archivo Relativo (VSAM RRDS)

```cobol
       FD  ARCHIVO-HASH
           RECORD CONTAINS 100 CHARACTERS.

       01  REG-HASH.
           05  HASH-KEY          PIC 9(10).
           05  HASH-DATOS        PIC X(90).
```

---

## FILE STATUS

### Definición

```cobol
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-STATUS-1       PIC X(1).
           05  WS-STATUS-2       PIC X(1).

      * O más detallado:
       01  WS-FILE-STATUS.
           05  WS-FS-BYTE1       PIC X(1).
               88  FS-SUCCESSFUL VALUE "0".
               88  FS-EOF        VALUE "1".
               88  FS-INVALID-KEY VALUE "2".
               88  FS-PERM-ERROR VALUE "3".
               88  FS-LOGIC-ERROR VALUE "4".
               88  FS-VSAM-ERROR VALUE "9".
           05  WS-FS-BYTE2       PIC X(1).
```

### Códigos Comunes

| Status | Significado |
|--------|-------------|
| 00 | Operación exitosa |
| 02 | Duplicado (índice alterno con DUPLICATES) |
| 10 | End of file |
| 21 | Clave fuera de secuencia |
| 22 | Clave duplicada (PRIMARY KEY) |
| 23 | Registro no encontrado |
| 24 | Disco lleno / fuera de límites |
| 30 | Error permanente |
| 34 | Registro fuera de límites |
| 35 | Archivo no encontrado |
| 37 | Error al abrir, modo incorrecto |
| 39 | Atributos de archivo no coinciden |
| 41 | Archivo ya está abierto |
| 42 | Archivo no está abierto |
| 43 | DELETE sin READ previo |
| 44 | REWRITE sin READ previo |
| 46 | READ sin posición válida |
| 47 | READ en archivo no abierto para input |
| 48 | WRITE en archivo no abierto para output |
| 49 | DELETE/REWRITE en archivo no abierto I-O |

### Rutina de Validación

```cobol
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS        PIC X(2).
       01  WS-FILE-NAME          PIC X(30).

       PROCEDURE DIVISION.

       CHECK-FILE-STATUS.
           IF WS-FILE-STATUS NOT = "00" AND "10"
               DISPLAY "ERROR EN ARCHIVO: " WS-FILE-NAME
               DISPLAY "FILE STATUS: " WS-FILE-STATUS
               PERFORM DECODE-FILE-STATUS
               PERFORM ABORT-PROGRAMA
           END-IF.

       DECODE-FILE-STATUS.
           EVALUATE WS-FILE-STATUS
               WHEN "00"
                   DISPLAY "OPERACION EXITOSA"
               WHEN "10"
                   DISPLAY "FIN DE ARCHIVO"
               WHEN "22"
                   DISPLAY "CLAVE DUPLICADA"
               WHEN "23"
                   DISPLAY "REGISTRO NO ENCONTRADO"
               WHEN "35"
                   DISPLAY "ARCHIVO NO EXISTE"
               WHEN "39"
                   DISPLAY "ATRIBUTOS INCOMPATIBLES"
               WHEN "41"
                   DISPLAY "ARCHIVO YA ABIERTO"
               WHEN "42"
                   DISPLAY "ARCHIVO NO ABIERTO"
               WHEN OTHER
                   DISPLAY "ERROR DESCONOCIDO: " WS-FILE-STATUS
           END-EVALUATE.
```

---

## OPEN / CLOSE

### Modos de Apertura

```cobol
      * INPUT - Solo lectura
           OPEN INPUT ARCHIVO-ENTRADA

      * OUTPUT - Solo escritura (crea/reemplaza)
           OPEN OUTPUT ARCHIVO-SALIDA

      * I-O - Lectura y escritura (actualización)
           OPEN I-O ARCHIVO-MAESTRO

      * EXTEND - Agregar al final
           OPEN EXTEND ARCHIVO-LOG

      * Múltiples archivos
           OPEN INPUT  ARCHIVO-1
                       ARCHIVO-2
                OUTPUT ARCHIVO-3
                I-O    ARCHIVO-4
```

### CLOSE

```cobol
      * CLOSE simple
           CLOSE ARCHIVO-ENTRADA

      * CLOSE múltiples
           CLOSE ARCHIVO-1
                 ARCHIVO-2
                 ARCHIVO-3

      * CLOSE con opciones (cintas)
           CLOSE ARCHIVO-CINTA WITH NO REWIND
           CLOSE ARCHIVO-CINTA WITH LOCK
```

### Patrón: Apertura con Validación

```cobol
       ABRIR-ARCHIVOS.
           MOVE "ARCHIVO-ENTRADA" TO WS-FILE-NAME
           OPEN INPUT ARCHIVO-ENTRADA
           IF WS-FILE-STATUS NOT = "00"
               PERFORM CHECK-FILE-STATUS
           END-IF

           MOVE "ARCHIVO-SALIDA" TO WS-FILE-NAME
           OPEN OUTPUT ARCHIVO-SALIDA
           IF WS-FILE-STATUS NOT = "00"
               PERFORM CHECK-FILE-STATUS
           END-IF

           MOVE "ARCHIVO-MAESTRO" TO WS-FILE-NAME
           OPEN I-O ARCHIVO-MAESTRO
           IF WS-FILE-STATUS NOT = "00"
               PERFORM CHECK-FILE-STATUS
           END-IF.

       CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-ENTRADA
                 ARCHIVO-SALIDA
                 ARCHIVO-MAESTRO.
```

---

## READ

### READ Secuencial

```cobol
      * READ simple
           READ ARCHIVO-ENTRADA
               AT END
                   SET FIN-ARCHIVO TO TRUE
           END-READ

      * READ INTO (mueve a working-storage)
           READ ARCHIVO-ENTRADA INTO WS-REGISTRO
               AT END
                   SET FIN-ARCHIVO TO TRUE
               NOT AT END
                   ADD 1 TO WS-CONTADOR
           END-READ

      * Patrón estándar de lectura
       LEER-SIGUIENTE.
           READ ARCHIVO-ENTRADA INTO WS-REG
               AT END
                   SET FIN-ARCHIVO TO TRUE
                   MOVE HIGH-VALUES TO WS-REG-CLAVE
           END-READ.
```

### READ Aleatorio (Indexed/Relative)

```cobol
      * READ por clave primaria
           MOVE "12345" TO CLI-NUMERO
           READ ARCHIVO-CLIENTES
               INVALID KEY
                   DISPLAY "CLIENTE NO ENCONTRADO"
               NOT INVALID KEY
                   PERFORM MOSTRAR-CLIENTE
           END-READ

      * READ por clave alterna
           MOVE "RFC1234567890" TO CLI-RFC
           READ ARCHIVO-CLIENTES
               KEY IS CLI-RFC
               INVALID KEY
                   DISPLAY "RFC NO EXISTE"
           END-READ

      * READ con INTO
           READ ARCHIVO-CLIENTES INTO WS-CLIENTE
               INVALID KEY
                   SET CLIENTE-NO-EXISTE TO TRUE
           END-READ
```

### READ NEXT (Secuencial en archivo indexado)

```cobol
      * Posicionar y leer secuencialmente
           MOVE "A" TO CLI-NUMERO
           START ARCHIVO-CLIENTES
               KEY IS GREATER THAN OR EQUAL TO CLI-NUMERO
               INVALID KEY
                   SET NO-HAY-REGISTROS TO TRUE
           END-START

           PERFORM UNTIL FIN-ARCHIVO
               READ ARCHIVO-CLIENTES NEXT
                   AT END SET FIN-ARCHIVO TO TRUE
                   NOT AT END PERFORM PROCESAR-CLIENTE
               END-READ
           END-PERFORM
```

---

## WRITE

### WRITE Secuencial

```cobol
      * WRITE simple
           WRITE REG-SALIDA

      * WRITE FROM (desde working-storage)
           WRITE REG-SALIDA FROM WS-REGISTRO

      * WRITE con INVALID KEY (archivos indexados)
           WRITE REG-CLIENTE
               INVALID KEY
                   DISPLAY "ERROR: CLAVE DUPLICADA"
                   ADD 1 TO WS-ERRORES
           END-WRITE
```

### WRITE con Opciones de Reporte

```cobol
      * Para archivos de impresión
       FD  ARCHIVO-REPORTE
           LINAGE IS 60 LINES
           WITH FOOTING AT 55
           LINES AT TOP 3
           LINES AT BOTTOM 2.

       01  LINEA-REPORTE          PIC X(132).

       PROCEDURE DIVISION.
      * WRITE BEFORE ADVANCING (avanza antes de escribir)
           WRITE LINEA-REPORTE FROM WS-ENCABEZADO
               BEFORE ADVANCING 2 LINES

      * WRITE AFTER ADVANCING (avanza después de escribir)
           WRITE LINEA-REPORTE FROM WS-DETALLE
               AFTER ADVANCING 1 LINE

      * Salto de página
           WRITE LINEA-REPORTE FROM WS-ENCABEZADO
               AFTER ADVANCING PAGE

      * End of page (LINAGE)
           WRITE LINEA-REPORTE FROM WS-DETALLE
               AT END-OF-PAGE
                   PERFORM NUEVA-PAGINA
           END-WRITE
```

### WRITE Archivo Indexado

```cobol
      * INSERT nuevo registro
           MOVE WS-CLIENTE TO REG-CLIENTE
           WRITE REG-CLIENTE
               INVALID KEY
                   EVALUATE WS-FILE-STATUS
                       WHEN "22"
                           DISPLAY "CLIENTE YA EXISTE"
                       WHEN "24"
                           DISPLAY "ARCHIVO LLENO"
                       WHEN OTHER
                           DISPLAY "ERROR: " WS-FILE-STATUS
                   END-EVALUATE
           END-WRITE
```

---

## REWRITE

```cobol
      * REWRITE requiere READ previo en modo I-O
           MOVE "12345" TO CLI-NUMERO
           READ ARCHIVO-CLIENTES
               INVALID KEY
                   DISPLAY "CLIENTE NO EXISTE"
                   GO TO REWRITE-EXIT
           END-READ

      * Modificar datos
           ADD 1000 TO CLI-SALDO
           MOVE FUNCTION CURRENT-DATE TO CLI-FECHA-ULT-MOV

      * Actualizar registro
           REWRITE REG-CLIENTE
               INVALID KEY
                   DISPLAY "ERROR EN ACTUALIZACION"
           END-REWRITE.

       REWRITE-EXIT.
           EXIT.
```

---

## DELETE

```cobol
      * DELETE en archivo indexado
      * Requiere READ previo o posicionar con clave

      * Método 1: Después de READ
           MOVE "12345" TO CLI-NUMERO
           READ ARCHIVO-CLIENTES
               INVALID KEY
                   DISPLAY "NO EXISTE PARA BORRAR"
                   GO TO DELETE-EXIT
           END-READ

           DELETE ARCHIVO-CLIENTES
               INVALID KEY
                   DISPLAY "ERROR EN DELETE"
           END-DELETE.

      * Método 2: Por clave directamente (algunos compiladores)
           MOVE "12345" TO CLI-NUMERO
           DELETE ARCHIVO-CLIENTES
               INVALID KEY
                   DISPLAY "REGISTRO NO ENCONTRADO"
           END-DELETE.
```

---

## START (Posicionamiento)

```cobol
      * START posiciona para READ NEXT
      * No lee el registro, solo posiciona

      * Posicionar al inicio de registros >= clave
           MOVE "M" TO CLI-NUMERO
           START ARCHIVO-CLIENTES
               KEY IS >= CLI-NUMERO
               INVALID KEY
                   DISPLAY "NO HAY REGISTROS DESDE M"
           END-START

      * Opciones de KEY
           START ARCHIVO KEY IS EQUAL TO CLI-NUMERO
           START ARCHIVO KEY IS GREATER THAN CLI-NUMERO
           START ARCHIVO KEY IS NOT LESS THAN CLI-NUMERO
           START ARCHIVO KEY IS GREATER THAN OR EQUAL CLI-NUMERO

      * START con clave alterna
           MOVE "PEREZ" TO CLI-APELLIDO
           START ARCHIVO-CLIENTES
               KEY IS >= CLI-APELLIDO
           END-START

      * Patrón: Lectura por rango
       LEER-RANGO.
           MOVE WS-INICIO-RANGO TO CLI-NUMERO
           START ARCHIVO-CLIENTES
               KEY IS >= CLI-NUMERO
               INVALID KEY
                   SET NO-HAY-DATOS TO TRUE
                   GO TO LEER-RANGO-EXIT
           END-START

           PERFORM UNTIL FIN-ARCHIVO
                      OR CLI-NUMERO > WS-FIN-RANGO
               READ ARCHIVO-CLIENTES NEXT
                   AT END SET FIN-ARCHIVO TO TRUE
                   NOT AT END
                       IF CLI-NUMERO <= WS-FIN-RANGO
                           PERFORM PROCESAR-CLIENTE
                       END-IF
               END-READ
           END-PERFORM.

       LEER-RANGO-EXIT.
           EXIT.
```

---

## SORT

### SORT con INPUT/OUTPUT PROCEDURE

```cobol
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-ENTRADA
               ASSIGN TO ENTRADA.

           SELECT SORT-FILE
               ASSIGN TO SORTWORK.

           SELECT ARCHIVO-SALIDA
               ASSIGN TO SALIDA.

       DATA DIVISION.
       FILE SECTION.

       FD  ARCHIVO-ENTRADA.
       01  REG-ENTRADA           PIC X(100).

       SD  SORT-FILE.
       01  SORT-REG.
           05  SORT-KEY1         PIC X(10).
           05  SORT-KEY2         PIC 9(5).
           05  SORT-DATOS        PIC X(85).

       FD  ARCHIVO-SALIDA.
       01  REG-SALIDA            PIC X(100).

       PROCEDURE DIVISION.
           SORT SORT-FILE
               ON ASCENDING KEY SORT-KEY1
               ON DESCENDING KEY SORT-KEY2
               INPUT PROCEDURE IS 1000-INPUT-PROC
               OUTPUT PROCEDURE IS 2000-OUTPUT-PROC
           STOP RUN.

       1000-INPUT-PROC SECTION.
           OPEN INPUT ARCHIVO-ENTRADA
           PERFORM UNTIL FIN-ENTRADA
               READ ARCHIVO-ENTRADA INTO WS-REG
                   AT END SET FIN-ENTRADA TO TRUE
                   NOT AT END
                       IF WS-REG-VALIDO
                           MOVE WS-REG TO SORT-REG
                           RELEASE SORT-REG
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ARCHIVO-ENTRADA.

       2000-OUTPUT-PROC SECTION.
           OPEN OUTPUT ARCHIVO-SALIDA
           PERFORM UNTIL FIN-SORT
               RETURN SORT-FILE INTO WS-REG
                   AT END SET FIN-SORT TO TRUE
                   NOT AT END
                       WRITE REG-SALIDA FROM WS-REG
               END-RETURN
           END-PERFORM
           CLOSE ARCHIVO-SALIDA.
```

### SORT con USING/GIVING

```cobol
      * SORT simple sin procedimientos
           SORT SORT-FILE
               ON ASCENDING KEY SORT-KEY1
               USING ARCHIVO-ENTRADA
               GIVING ARCHIVO-SALIDA

      * SORT con múltiples claves
           SORT SORT-FILE
               ON ASCENDING KEY SORT-REGION
               ON ASCENDING KEY SORT-SUCURSAL
               ON DESCENDING KEY SORT-MONTO
               USING ARCHIVO-ENTRADA
               GIVING ARCHIVO-SALIDA

      * SORT con DUPLICATES IN ORDER
           SORT SORT-FILE
               ON ASCENDING KEY SORT-CLAVE
               WITH DUPLICATES IN ORDER
               USING ARCHIVO-ENTRADA
               GIVING ARCHIVO-SALIDA
```

---

## MERGE

```cobol
       ENVIRONMENT DIVISION.
       FILE-CONTROL.
           SELECT ARCHIVO-1 ASSIGN TO FILE1.
           SELECT ARCHIVO-2 ASSIGN TO FILE2.
           SELECT ARCHIVO-3 ASSIGN TO FILE3.
           SELECT MERGE-FILE ASSIGN TO MERGEWORK.
           SELECT ARCHIVO-SALIDA ASSIGN TO SALIDA.

       DATA DIVISION.
       FILE SECTION.

       FD  ARCHIVO-1.
       01  REG-1                 PIC X(100).

       FD  ARCHIVO-2.
       01  REG-2                 PIC X(100).

       FD  ARCHIVO-3.
       01  REG-3                 PIC X(100).

       SD  MERGE-FILE.
       01  MERGE-REG.
           05  MERGE-KEY         PIC X(10).
           05  MERGE-DATOS       PIC X(90).

       FD  ARCHIVO-SALIDA.
       01  REG-SALIDA            PIC X(100).

       PROCEDURE DIVISION.
      * MERGE asume que los archivos de entrada ya están ordenados
           MERGE MERGE-FILE
               ON ASCENDING KEY MERGE-KEY
               USING ARCHIVO-1
                     ARCHIVO-2
                     ARCHIVO-3
               GIVING ARCHIVO-SALIDA

           STOP RUN.
```

---

## Programa Completo: Mantenimiento de Archivo Indexado

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MANT-CLIENTES.
       AUTHOR. ARCHAEON.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-CLIENTES
               ASSIGN TO CLIENTES
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLI-NUMERO
               ALTERNATE RECORD KEY IS CLI-RFC
                   WITH DUPLICATES
               FILE STATUS IS WS-FS-CLI.

           SELECT ARCHIVO-TRANSACC
               ASSIGN TO TRANSACC
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS-TRX.

           SELECT ARCHIVO-ERRORES
               ASSIGN TO ERRORES
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS-ERR.

       DATA DIVISION.
       FILE SECTION.

       FD  ARCHIVO-CLIENTES.
       01  REG-CLIENTE.
           05  CLI-NUMERO        PIC 9(10).
           05  CLI-RFC           PIC X(13).
           05  CLI-NOMBRE        PIC X(40).
           05  CLI-SALDO         PIC S9(11)V99 COMP-3.
           05  CLI-ESTADO        PIC X(1).
               88  CLI-ACTIVO    VALUE "A".
               88  CLI-INACTIVO  VALUE "I".
           05  CLI-FECHA-ULT     PIC 9(8).
           05  FILLER            PIC X(50).

       FD  ARCHIVO-TRANSACC.
       01  REG-TRANSACC.
           05  TRX-TIPO          PIC X(1).
               88  TRX-ALTA      VALUE "A".
               88  TRX-BAJA      VALUE "B".
               88  TRX-MODIF     VALUE "M".
           05  TRX-CLIENTE       PIC 9(10).
           05  TRX-DATOS         PIC X(100).

       FD  ARCHIVO-ERRORES.
       01  REG-ERROR             PIC X(150).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-FS-CLI         PIC X(2).
           05  WS-FS-TRX         PIC X(2).
           05  WS-FS-ERR         PIC X(2).

       01  WS-FLAGS.
           05  WS-FIN-TRX        PIC X VALUE "N".
               88  FIN-TRANSACC  VALUE "S".

       01  WS-CONTADORES.
           05  WS-ALTAS          PIC 9(7) VALUE 0.
           05  WS-BAJAS          PIC 9(7) VALUE 0.
           05  WS-MODIF          PIC 9(7) VALUE 0.
           05  WS-ERRORES        PIC 9(7) VALUE 0.

       01  WS-FECHA-HOY          PIC 9(8).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO UNTIL FIN-TRANSACC
           PERFORM 3000-FIN
           STOP RUN.

       1000-INICIO.
           ACCEPT WS-FECHA-HOY FROM DATE YYYYMMDD
           OPEN I-O ARCHIVO-CLIENTES
           IF WS-FS-CLI NOT = "00"
               DISPLAY "ERROR ABRIENDO CLIENTES: " WS-FS-CLI
               STOP RUN
           END-IF

           OPEN INPUT ARCHIVO-TRANSACC
           OPEN OUTPUT ARCHIVO-ERRORES
           PERFORM 2100-LEER-TRANSACC.

       2000-PROCESO.
           EVALUATE TRUE
               WHEN TRX-ALTA
                   PERFORM 2200-PROCESAR-ALTA
               WHEN TRX-BAJA
                   PERFORM 2300-PROCESAR-BAJA
               WHEN TRX-MODIF
                   PERFORM 2400-PROCESAR-MODIF
               WHEN OTHER
                   PERFORM 2500-ERROR-TIPO
           END-EVALUATE
           PERFORM 2100-LEER-TRANSACC.

       2100-LEER-TRANSACC.
           READ ARCHIVO-TRANSACC
               AT END SET FIN-TRANSACC TO TRUE
           END-READ.

       2200-PROCESAR-ALTA.
           MOVE TRX-CLIENTE TO CLI-NUMERO
           READ ARCHIVO-CLIENTES
               INVALID KEY
                   PERFORM 2210-INSERTAR-CLIENTE
               NOT INVALID KEY
                   MOVE "CLIENTE YA EXISTE" TO REG-ERROR
                   WRITE REG-ERROR
                   ADD 1 TO WS-ERRORES
           END-READ.

       2210-INSERTAR-CLIENTE.
           INITIALIZE REG-CLIENTE
           MOVE TRX-CLIENTE TO CLI-NUMERO
           MOVE TRX-DATOS(1:13) TO CLI-RFC
           MOVE TRX-DATOS(14:40) TO CLI-NOMBRE
           SET CLI-ACTIVO TO TRUE
           MOVE WS-FECHA-HOY TO CLI-FECHA-ULT
           WRITE REG-CLIENTE
               INVALID KEY
                   MOVE "ERROR EN ALTA" TO REG-ERROR
                   WRITE REG-ERROR
                   ADD 1 TO WS-ERRORES
               NOT INVALID KEY
                   ADD 1 TO WS-ALTAS
           END-WRITE.

       2300-PROCESAR-BAJA.
           MOVE TRX-CLIENTE TO CLI-NUMERO
           READ ARCHIVO-CLIENTES
               INVALID KEY
                   MOVE "CLIENTE NO EXISTE PARA BAJA"
                       TO REG-ERROR
                   WRITE REG-ERROR
                   ADD 1 TO WS-ERRORES
               NOT INVALID KEY
                   SET CLI-INACTIVO TO TRUE
                   MOVE WS-FECHA-HOY TO CLI-FECHA-ULT
                   REWRITE REG-CLIENTE
                   ADD 1 TO WS-BAJAS
           END-READ.

       2400-PROCESAR-MODIF.
           MOVE TRX-CLIENTE TO CLI-NUMERO
           READ ARCHIVO-CLIENTES
               INVALID KEY
                   MOVE "CLIENTE NO EXISTE PARA MODIF"
                       TO REG-ERROR
                   WRITE REG-ERROR
                   ADD 1 TO WS-ERRORES
               NOT INVALID KEY
                   MOVE TRX-DATOS(14:40) TO CLI-NOMBRE
                   MOVE WS-FECHA-HOY TO CLI-FECHA-ULT
                   REWRITE REG-CLIENTE
                   ADD 1 TO WS-MODIF
           END-READ.

       2500-ERROR-TIPO.
           STRING "TIPO TRANSACCION INVALIDO: " DELIMITED SIZE
                  TRX-TIPO DELIMITED SIZE
               INTO REG-ERROR
           END-STRING
           WRITE REG-ERROR
           ADD 1 TO WS-ERRORES.

       3000-FIN.
           DISPLAY "=== RESUMEN DE PROCESO ==="
           DISPLAY "ALTAS:         " WS-ALTAS
           DISPLAY "BAJAS:         " WS-BAJAS
           DISPLAY "MODIFICACIONES:" WS-MODIF
           DISPLAY "ERRORES:       " WS-ERRORES

           CLOSE ARCHIVO-CLIENTES
                 ARCHIVO-TRANSACC
                 ARCHIVO-ERRORES.
```

---

## Próximo Documento

- **COBOL_05_MAINFRAME.md**: JCL, CICS, DB2, y entorno mainframe completo

---

*"Los archivos en COBOL son la memoria permanente del negocio. Cada registro cuenta una historia de décadas."*


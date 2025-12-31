# COBOL_01: Fundamentos de COBOL

> "Un programa COBOL de 1970 puede seguir procesando millones de transacciones hoy. Eso es ingeniería de verdad."

---

## Historia

```
1959: COBOL creado por comité (CODASYL)
      Grace Hopper fue figura clave
1968: COBOL-68 (primer estándar ANSI)
1974: COBOL-74
1985: COBOL-85 (programación estructurada)
2002: COBOL-2002 (OOP, XML)
2014: COBOL-2014 (último estándar)
```

### ¿Por Qué COBOL Sigue Vivo?

| Razón | Explicación |
|-------|-------------|
| **Estabilidad** | Código probado por décadas |
| **Crítico** | Banca, seguros, gobierno |
| **Costo** | Reescribir es más caro que mantener |
| **Rendimiento** | Optimizado para batch processing |
| **Volumen** | ~220 mil millones de líneas en uso |

### Dónde Vive COBOL

- **95%** de transacciones ATM
- **80%** de transacciones en persona
- Mainframes IBM z/Series
- Sistemas de seguridad social
- Compañías de seguros
- Aerolíneas (reservas)

---

## Estructura de un Programa COBOL

Todo programa COBOL tiene 4 divisiones obligatorias:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MI-PROGRAMA.
      *-----------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
      *-----------------------------------------
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *-----------------------------------------
       PROCEDURE DIVISION.
           PERFORM PROCESO-PRINCIPAL
           STOP RUN.
```

### Visualización de Estructura

```
┌────────────────────────────────────────────────────────────┐
│                    PROGRAMA COBOL                          │
├────────────────────────────────────────────────────────────┤
│  IDENTIFICATION DIVISION                                   │
│  ├── PROGRAM-ID                                           │
│  ├── AUTHOR (opcional)                                    │
│  └── DATE-WRITTEN (opcional)                              │
├────────────────────────────────────────────────────────────┤
│  ENVIRONMENT DIVISION                                      │
│  ├── CONFIGURATION SECTION                                │
│  │   ├── SOURCE-COMPUTER                                  │
│  │   └── OBJECT-COMPUTER                                  │
│  └── INPUT-OUTPUT SECTION                                 │
│      └── FILE-CONTROL (definición de archivos)            │
├────────────────────────────────────────────────────────────┤
│  DATA DIVISION                                             │
│  ├── FILE SECTION (estructura de archivos)                │
│  ├── WORKING-STORAGE SECTION (variables)                  │
│  ├── LOCAL-STORAGE SECTION (variables locales)            │
│  └── LINKAGE SECTION (parámetros)                         │
├────────────────────────────────────────────────────────────┤
│  PROCEDURE DIVISION                                        │
│  ├── SECTION-1                                            │
│  │   ├── PARAGRAPH-1                                      │
│  │   └── PARAGRAPH-2                                      │
│  └── SECTION-2                                            │
│      └── ...                                              │
└────────────────────────────────────────────────────────────┘
```

---

## Formato de Línea (Columnas)

COBOL tradicional usa formato fijo de 80 columnas:

```
Columnas   Propósito
────────   ─────────────────────────────
1-6        Número de secuencia (opcional)
7          Indicador:
           * = Comentario
           - = Continuación
           / = Salto de página
           D = Depuración
8-11       Área A (DIVISION, SECTION, 01 levels)
12-72      Área B (Código)
73-80      Identificación (ignorado)

       |  |    |                                                    |
       1  7   12                                                   72
       |  |    |                                                    |
      *ESTE ES UN COMENTARIO
       01  WS-VARIABLE    PIC X(10).
           MOVE "HOLA" TO WS-VARIABLE.
```

### Formato Libre (COBOL-2002+)

```cobol
>>SOURCE FORMAT IS FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MODERNO.
* Ya no importan las columnas
```

---

## Tipos de Datos: PICTURE Clause

PICTURE (PIC) define el formato de los datos:

### Caracteres de PICTURE

| Símbolo | Significado | Ejemplo |
|---------|-------------|---------|
| 9 | Dígito numérico | PIC 9(5) = 00000-99999 |
| X | Cualquier carácter | PIC X(10) = alfanumérico |
| A | Solo alfabético | PIC A(5) = letras |
| V | Punto decimal implícito | PIC 9(3)V99 = 123.45 |
| S | Signo (positivo/negativo) | PIC S9(5) = -99999 a +99999 |
| P | Posición decimal asumida | PIC 9(3)PP = miles |
| Z | Cero suprimido | PIC Z(5) = espacios si cero |
| , | Coma de edición | PIC Z,ZZZ |
| . | Punto decimal de edición | PIC ZZZ.99 |
| $ | Símbolo moneda | PIC $ZZZ.99 |
| - | Signo negativo | PIC -(5)9 |
| + | Signo siempre | PIC +(5)9 |
| CR | Crédito | PIC 9(5)CR |
| DB | Débito | PIC 9(5)DB |

### Ejemplos de PICTURE

```cobol
      * Numéricos
       01  WS-CANTIDAD        PIC 9(5).          *> 00000-99999
       01  WS-MONTO           PIC 9(7)V99.       *> 9999999.99
       01  WS-SALDO           PIC S9(9)V99.      *> ±999999999.99

      * Alfanuméricos
       01  WS-NOMBRE          PIC X(30).         *> 30 caracteres
       01  WS-CODIGO          PIC X(10).         *> código alfanum

      * Editados (para display)
       01  WS-MONTO-EDIT      PIC $ZZZ,ZZZ.99.   *> $  1,234.56
       01  WS-FECHA-EDIT      PIC 99/99/9999.    *> 31/12/2024
       01  WS-TELEFONO        PIC (999) 999-9999. *> (555) 123-4567
```

### USAGE Clause

Define cómo se almacena el dato:

```cobol
       01  WS-DISPLAY-NUM     PIC 9(5) USAGE DISPLAY.
      *    Un byte por dígito (default)

       01  WS-BINARY-NUM      PIC 9(5) USAGE BINARY.
      *    Binario nativo (2 o 4 bytes)

       01  WS-COMP-NUM        PIC 9(5) USAGE COMP.
      *    = BINARY (sinónimo)

       01  WS-COMP-3-NUM      PIC 9(5) USAGE COMP-3.
      *    Packed decimal (BCD, 3 bytes para 5 dígitos)

       01  WS-POINTER         USAGE POINTER.
      *    Puntero de memoria
```

### Comparación de USAGE

| USAGE | PIC 9(5) ocupa | Rango | Uso |
|-------|----------------|-------|-----|
| DISPLAY | 5 bytes | 0-99999 | I/O, archivos |
| BINARY | 4 bytes | 0-99999 | Cálculos |
| COMP-3 | 3 bytes | 0-99999 | Mainframe, DB2 |

---

## Niveles de Datos

COBOL usa números de nivel para estructurar datos:

| Nivel | Propósito |
|-------|-----------|
| 01 | Registro principal (record) |
| 02-49 | Campos subordinados |
| 66 | RENAMES (alias) |
| 77 | Variable independiente |
| 88 | Condición (valor booleano) |

### Ejemplo de Estructura

```cobol
       01  WS-CLIENTE.
           05  WS-CLI-ID           PIC 9(10).
           05  WS-CLI-NOMBRE.
               10  WS-CLI-NOMBRE1  PIC X(20).
               10  WS-CLI-NOMBRE2  PIC X(20).
           05  WS-CLI-DIRECCION.
               10  WS-CLI-CALLE    PIC X(30).
               10  WS-CLI-CIUDAD   PIC X(20).
               10  WS-CLI-CP       PIC 9(5).
           05  WS-CLI-SALDO        PIC S9(9)V99.

       77  WS-CONTADOR             PIC 9(5) VALUE 0.

       01  WS-ESTADO               PIC X(1).
           88  WS-ACTIVO           VALUE 'A'.
           88  WS-INACTIVO         VALUE 'I'.
           88  WS-SUSPENDIDO       VALUE 'S'.
```

### Visualización de Estructura

```
WS-CLIENTE (01)
├── WS-CLI-ID (05)
├── WS-CLI-NOMBRE (05)
│   ├── WS-CLI-NOMBRE1 (10)
│   └── WS-CLI-NOMBRE2 (10)
├── WS-CLI-DIRECCION (05)
│   ├── WS-CLI-CALLE (10)
│   ├── WS-CLI-CIUDAD (10)
│   └── WS-CLI-CP (10)
└── WS-CLI-SALDO (05)
```

---

## Nivel 88: Condiciones

El nivel 88 define valores con nombre (enumeraciones):

```cobol
       01  WS-MES                  PIC 99.
           88  ENERO               VALUE 01.
           88  FEBRERO             VALUE 02.
           88  MARZO               VALUE 03.
           88  PRIMER-TRIMESTRE    VALUE 01 THRU 03.
           88  MES-VALIDO          VALUE 01 THRU 12.

      * Uso:
           IF ENERO
               DISPLAY "Es enero"
           END-IF

           IF PRIMER-TRIMESTRE
               PERFORM CALCULAR-Q1
           END-IF

           SET MARZO TO TRUE    *> WS-MES = 03
```

---

## Instrucciones Básicas

### MOVE (Asignación)

```cobol
      * Mover literal
       MOVE "HOLA" TO WS-TEXTO.
       MOVE 12345 TO WS-NUMERO.
       MOVE ZEROS TO WS-CONTADOR.
       MOVE SPACES TO WS-NOMBRE.

      * Mover variable
       MOVE WS-ORIGEN TO WS-DESTINO.

      * MOVE CORRESPONDING (campos con mismo nombre)
       MOVE CORRESPONDING WS-REGISTRO1 TO WS-REGISTRO2.
```

### Aritmética

```cobol
      * ADD
       ADD 1 TO WS-CONTADOR.
       ADD WS-A WS-B TO WS-C.
       ADD WS-A TO WS-B GIVING WS-C.

      * SUBTRACT
       SUBTRACT 1 FROM WS-CONTADOR.
       SUBTRACT WS-A FROM WS-B GIVING WS-C.

      * MULTIPLY
       MULTIPLY WS-A BY WS-B.
       MULTIPLY WS-A BY WS-B GIVING WS-C.

      * DIVIDE
       DIVIDE WS-A INTO WS-B.
       DIVIDE WS-A INTO WS-B GIVING WS-C REMAINDER WS-R.

      * COMPUTE (expresiones)
       COMPUTE WS-RESULTADO = WS-A + WS-B * WS-C.
       COMPUTE WS-INTERES = WS-CAPITAL * WS-TASA / 100.
       COMPUTE WS-TOTAL ROUNDED = WS-SUBTOTAL * 1.16.
```

### Control de Flujo

```cobol
      * IF-ELSE
       IF WS-EDAD >= 18
           DISPLAY "MAYOR DE EDAD"
       ELSE
           DISPLAY "MENOR DE EDAD"
       END-IF.

      * IF anidado
       IF WS-TIPO = 'A'
           IF WS-MONTO > 1000
               PERFORM PROCESO-GRANDE
           ELSE
               PERFORM PROCESO-NORMAL
           END-IF
       END-IF.

      * EVALUATE (switch/case)
       EVALUATE TRUE
           WHEN WS-OPCION = 1
               PERFORM OPCION-UNO
           WHEN WS-OPCION = 2
               PERFORM OPCION-DOS
           WHEN WS-OPCION = 3 THRU 5
               PERFORM OPCIONES-TRES-A-CINCO
           WHEN OTHER
               PERFORM OPCION-DEFAULT
       END-EVALUATE.

      * EVALUATE con múltiples condiciones
       EVALUATE WS-TIPO ALSO WS-ESTADO
           WHEN 'A' ALSO 'V'
               PERFORM TIPO-A-VIGENTE
           WHEN 'B' ALSO ANY
               PERFORM TIPO-B-CUALQUIERA
           WHEN OTHER
               PERFORM OTRO-CASO
       END-EVALUATE.
```

### PERFORM (Loops y Llamadas)

```cobol
      * Llamar párrafo
       PERFORM INICIALIZAR.
       PERFORM PROCESAR.
       PERFORM FINALIZAR.

      * Loop TIMES
       PERFORM IMPRIMIR-LINEA 10 TIMES.

      * Loop UNTIL
       PERFORM LEER-REGISTRO
           UNTIL WS-FIN-ARCHIVO = 'S'.

      * Loop VARYING
       PERFORM PROCESAR-ELEMENTO
           VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > 100.

      * Loop con TEST AFTER
       PERFORM LEER-DATO
           WITH TEST AFTER
           UNTIL WS-DATO = SPACES.

      * PERFORM THRU
       PERFORM 1000-INICIO THRU 1000-FIN.
```

### STRING y UNSTRING

```cobol
      * STRING (concatenar)
       STRING WS-NOMBRE DELIMITED BY SPACE
              ", " DELIMITED BY SIZE
              WS-APELLIDO DELIMITED BY SPACE
              INTO WS-NOMBRE-COMPLETO.

      * UNSTRING (separar)
       UNSTRING WS-LINEA DELIMITED BY ","
           INTO WS-CAMPO1
                WS-CAMPO2
                WS-CAMPO3.
```

### INSPECT (buscar/reemplazar)

```cobol
      * Contar ocurrencias
       INSPECT WS-TEXTO TALLYING WS-CONT
           FOR ALL "A".

      * Reemplazar
       INSPECT WS-TEXTO REPLACING
           ALL "OLD" BY "NEW".

      * Convertir a mayúsculas
       INSPECT WS-TEXTO CONVERTING
           "abcdefghijklmnopqrstuvwxyz"
           TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
```

---

## Ejemplo: Programa Completo

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULA-TOTAL.
       AUTHOR. ARCHAEON.
       DATE-WRITTEN. 2024-12-31.
      *================================================
      * Programa: Calcular total con IVA
      *================================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-SUBTOTAL          PIC 9(7)V99 VALUE 0.
       01  WS-IVA               PIC 9(7)V99 VALUE 0.
       01  WS-TOTAL             PIC 9(7)V99 VALUE 0.
       01  WS-TASA-IVA          PIC V99     VALUE 0.16.

       01  WS-SUBTOTAL-EDIT     PIC $ZZZ,ZZZ.99.
       01  WS-IVA-EDIT          PIC $ZZZ,ZZZ.99.
       01  WS-TOTAL-EDIT        PIC $ZZZ,ZZZ.99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-CALCULAR
           PERFORM 3000-MOSTRAR
           STOP RUN.

       1000-INICIALIZAR.
           MOVE 1500.50 TO WS-SUBTOTAL.

       2000-CALCULAR.
           COMPUTE WS-IVA ROUNDED =
               WS-SUBTOTAL * WS-TASA-IVA.
           COMPUTE WS-TOTAL =
               WS-SUBTOTAL + WS-IVA.

       3000-MOSTRAR.
           MOVE WS-SUBTOTAL TO WS-SUBTOTAL-EDIT
           MOVE WS-IVA TO WS-IVA-EDIT
           MOVE WS-TOTAL TO WS-TOTAL-EDIT

           DISPLAY "SUBTOTAL: " WS-SUBTOTAL-EDIT
           DISPLAY "IVA:      " WS-IVA-EDIT
           DISPLAY "TOTAL:    " WS-TOTAL-EDIT.
```

### Salida:
```
SUBTOTAL: $  1,500.50
IVA:      $    240.08
TOTAL:    $  1,740.58
```

---

## Compilación y Ejecución

### GnuCOBOL (Open Source)

```bash
# Instalar
sudo apt install gnucobol

# Compilar
cobc -x -free programa.cbl -o programa

# Ejecutar
./programa
```

### IBM Enterprise COBOL (Mainframe)

```jcl
//COMPILE  EXEC PGM=IGYCRCTL
//SYSLIB   DD DSN=SYS1.COPYLIB,DISP=SHR
//SYSIN    DD DSN=USER.SOURCE(PROGRAMA),DISP=SHR
//SYSLIN   DD DSN=USER.OBJ(PROGRAMA),DISP=SHR
```

---

## Próximos Documentos

- **COBOL_02_DATA_DIVISION.md**: DATA DIVISION en detalle
- **COBOL_03_PROCEDURE.md**: PROCEDURE DIVISION completa
- **COBOL_04_FILES.md**: Manejo de archivos

---

*"COBOL fue diseñado para ser legible por gerentes. 60 años después, los programadores aún lo entienden."*


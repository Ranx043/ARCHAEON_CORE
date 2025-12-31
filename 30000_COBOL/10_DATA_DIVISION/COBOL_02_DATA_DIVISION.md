# COBOL_02: DATA DIVISION

> "En COBOL, los datos son ciudadanos de primera clase. La DATA DIVISION es donde vive la lógica de negocio."

---

## Estructura de DATA DIVISION

```cobol
       DATA DIVISION.
      *-----------------------------------------
       FILE SECTION.
      *    Definición de archivos y sus registros
      *-----------------------------------------
       WORKING-STORAGE SECTION.
      *    Variables del programa (persisten)
      *-----------------------------------------
       LOCAL-STORAGE SECTION.
      *    Variables locales (se reinician)
      *-----------------------------------------
       LINKAGE SECTION.
      *    Parámetros recibidos de CALL
      *-----------------------------------------
       REPORT SECTION.
      *    Definición de reportes (Report Writer)
      *-----------------------------------------
       SCREEN SECTION.
      *    Definición de pantallas (CICS alternativo)
```

---

## FILE SECTION

Define la estructura de archivos externos:

```cobol
       FILE SECTION.

       FD  ARCHIVO-CLIENTES
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 150 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS REG-CLIENTE.

       01  REG-CLIENTE.
           05  CLI-ID              PIC 9(10).
           05  CLI-NOMBRE          PIC X(40).
           05  CLI-DIRECCION       PIC X(60).
           05  CLI-SALDO           PIC S9(9)V99.
           05  CLI-FECHA-ALTA      PIC 9(8).
           05  FILLER              PIC X(21).
```

### Tipos de Archivos

| Recording Mode | Descripción |
|----------------|-------------|
| F (Fixed) | Registros de longitud fija |
| V (Variable) | Registros de longitud variable |
| U (Undefined) | Sin estructura definida |
| S (Spanned) | Registros que cruzan bloques |

---

## WORKING-STORAGE SECTION

Variables que persisten durante toda la ejecución:

### Tipos Numéricos

```cobol
       WORKING-STORAGE SECTION.

      *-----------------------------------------
      * NUMÉRICOS - DISPLAY (un byte por dígito)
      *-----------------------------------------
       01  WS-NUM-DISPLAY          PIC 9(5).
      *    Ocupa 5 bytes: "12345"
      *    Rango: 0 a 99999

       01  WS-NUM-SIGNED           PIC S9(5).
      *    Ocupa 5 bytes, último tiene signo
      *    Rango: -99999 a +99999

       01  WS-NUM-DECIMAL          PIC 9(5)V99.
      *    Ocupa 7 bytes: "1234567" = 12345.67
      *    V es punto decimal implícito

      *-----------------------------------------
      * NUMÉRICOS - COMP/BINARY
      *-----------------------------------------
       01  WS-COMP-SMALL           PIC S9(4) COMP.
      *    2 bytes (halfword)
      *    Rango: -32768 a +32767

       01  WS-COMP-MEDIUM          PIC S9(9) COMP.
      *    4 bytes (fullword)
      *    Rango: -2,147,483,648 a +2,147,483,647

       01  WS-COMP-LARGE           PIC S9(18) COMP.
      *    8 bytes (doubleword)

      *-----------------------------------------
      * NUMÉRICOS - COMP-3 (Packed Decimal/BCD)
      *-----------------------------------------
       01  WS-PACKED               PIC S9(7) COMP-3.
      *    4 bytes: cada byte = 2 dígitos
      *    Último nibble = signo (C=+, D=-)
      *    +1234567 = x'1234567C'

       01  WS-PACKED-DEC           PIC S9(7)V99 COMP-3.
      *    5 bytes: 1234567.99
```

### Tabla de COMP Types (IBM)

| USAGE | PIC 9(n) | Bytes | Rango |
|-------|----------|-------|-------|
| COMP | 1-4 | 2 | 0-9999 |
| COMP | 5-9 | 4 | 0-999999999 |
| COMP | 10-18 | 8 | 0-10^18 |
| COMP-3 | n | (n+2)/2 | Packed decimal |

### Tipos Alfanuméricos

```cobol
      *-----------------------------------------
      * ALFANUMÉRICOS
      *-----------------------------------------
       01  WS-TEXTO                PIC X(30).
      *    30 caracteres cualquiera

       01  WS-TEXTO-UPPER          PIC A(30).
      *    30 caracteres solo alfabéticos

       01  WS-MIXTO                PIC X(10)9(5)X(5).
      *    10 chars + 5 dígitos + 5 chars

       01  WS-JUSTIFIED            PIC X(20) JUSTIFIED RIGHT.
      *    Alineado a la derecha
```

### Valores Iniciales

```cobol
       01  WS-CONTADOR             PIC 9(5) VALUE 0.
       01  WS-NOMBRE               PIC X(20) VALUE "DESCONOCIDO".
       01  WS-FECHA                PIC 9(8) VALUE 20241231.
       01  WS-TASA                 PIC V99 VALUE 0.16.

      * Valores especiales
       01  WS-CEROS                PIC 9(10) VALUE ZEROS.
       01  WS-ESPACIOS             PIC X(10) VALUE SPACES.
       01  WS-HIGH                 PIC X(10) VALUE HIGH-VALUES.
       01  WS-LOW                  PIC X(10) VALUE LOW-VALUES.
       01  WS-QUOTE                PIC X(10) VALUE QUOTES.
       01  WS-ALL-X                PIC X(10) VALUE ALL "X".
```

---

## Estructuras y Grupos

### Registro con Subniveles

```cobol
       01  WS-EMPLEADO.
           05  EMP-ID              PIC 9(8).
           05  EMP-NOMBRE.
               10  EMP-NOMBRE1     PIC X(15).
               10  EMP-NOMBRE2     PIC X(15).
               10  EMP-APELLIDO1   PIC X(20).
               10  EMP-APELLIDO2   PIC X(20).
           05  EMP-FECHA-NAC.
               10  EMP-NAC-ANIO    PIC 9(4).
               10  EMP-NAC-MES     PIC 9(2).
               10  EMP-NAC-DIA     PIC 9(2).
           05  EMP-SALARIO         PIC S9(9)V99 COMP-3.
           05  EMP-DEPARTAMENTO    PIC X(4).
           05  EMP-ESTADO          PIC X(1).
               88  EMP-ACTIVO      VALUE 'A'.
               88  EMP-INACTIVO    VALUE 'I'.
               88  EMP-VACACIONES  VALUE 'V'.
               88  EMP-LICENCIA    VALUE 'L'.
```

### REDEFINES

Permite ver los mismos bytes de diferentes formas:

```cobol
       01  WS-FECHA-NUM            PIC 9(8).
      *    20241231

       01  WS-FECHA-PARTS REDEFINES WS-FECHA-NUM.
           05  WS-ANIO             PIC 9(4).
           05  WS-MES              PIC 9(2).
           05  WS-DIA              PIC 9(2).

      * Uso:
      *    MOVE 20241231 TO WS-FECHA-NUM
      *    Ahora WS-ANIO = 2024, WS-MES = 12, WS-DIA = 31
```

### REDEFINES para Conversión

```cobol
       01  WS-NUMERO-TEXTO         PIC X(10).
       01  WS-NUMERO-NUM REDEFINES WS-NUMERO-TEXTO
                                   PIC 9(10).

      * De texto a número:
           MOVE "0000012345" TO WS-NUMERO-TEXTO
           COMPUTE WS-RESULTADO = WS-NUMERO-NUM + 1

      * De número a texto:
           MOVE 12345 TO WS-NUMERO-NUM
           DISPLAY WS-NUMERO-TEXTO
```

---

## Tablas (OCCURS)

### Tabla Simple

```cobol
       01  WS-TABLA-MESES.
           05  WS-MES-NOMBRE       PIC X(10) OCCURS 12 TIMES.

      * Acceso:
           MOVE "ENERO" TO WS-MES-NOMBRE(1)
           MOVE "DICIEMBRE" TO WS-MES-NOMBRE(12)
```

### Tabla con Estructura

```cobol
       01  WS-TABLA-PRODUCTOS.
           05  WS-PRODUCTO OCCURS 100 TIMES
               INDEXED BY WS-IDX-PROD.
               10  WS-PROD-CODIGO   PIC X(10).
               10  WS-PROD-NOMBRE   PIC X(30).
               10  WS-PROD-PRECIO   PIC 9(7)V99.
               10  WS-PROD-STOCK    PIC 9(5).

      * Acceso con índice:
           SET WS-IDX-PROD TO 1
           MOVE "A001" TO WS-PROD-CODIGO(WS-IDX-PROD)
```

### Tabla con DEPENDING ON (Variable)

```cobol
       01  WS-CANT-ITEMS           PIC 9(3).

       01  WS-TABLA-VARIABLE.
           05  WS-ITEM OCCURS 1 TO 999 TIMES
               DEPENDING ON WS-CANT-ITEMS.
               10  WS-ITEM-ID      PIC 9(5).
               10  WS-ITEM-VALOR   PIC 9(7)V99.
```

### Tabla Multidimensional

```cobol
       01  WS-MATRIZ.
           05  WS-FILA OCCURS 10 TIMES.
               10  WS-COLUMNA OCCURS 20 TIMES.
                   15  WS-CELDA    PIC 9(5)V99.

      * Acceso:
           MOVE 123.45 TO WS-CELDA(3, 5)
      *    Fila 3, Columna 5
```

### Búsqueda en Tabla

```cobol
      * SEARCH lineal
       01  WS-TABLA-BUSQUEDA.
           05  WS-ELEMENTO OCCURS 100 TIMES
               INDEXED BY WS-IDX.
               10  WS-CLAVE        PIC X(10).
               10  WS-VALOR        PIC 9(5).

       PROCEDURE DIVISION.
           SET WS-IDX TO 1
           SEARCH WS-ELEMENTO
               AT END
                   DISPLAY "NO ENCONTRADO"
               WHEN WS-CLAVE(WS-IDX) = WS-BUSCAR
                   DISPLAY "ENCONTRADO: " WS-VALOR(WS-IDX)
           END-SEARCH.

      * SEARCH ALL (binaria, tabla ordenada)
       01  WS-TABLA-ORDENADA.
           05  WS-ELEM OCCURS 1000 TIMES
               ASCENDING KEY IS WS-KEY
               INDEXED BY WS-IX.
               10  WS-KEY          PIC X(10).
               10  WS-DATA         PIC X(50).

           SEARCH ALL WS-ELEM
               AT END
                   DISPLAY "NO EXISTE"
               WHEN WS-KEY(WS-IX) = WS-BUSCAR
                   MOVE WS-DATA(WS-IX) TO WS-RESULTADO
           END-SEARCH.
```

---

## COPY Statement

Incluir código desde biblioteca:

```cobol
      * En WORKING-STORAGE:
       COPY CLIENTE.

      * Equivale a incluir el contenido de CLIENTE.cpy:
      * 01  WS-CLIENTE.
      *     05  CLI-ID     PIC 9(10).
      *     05  CLI-NOMBRE PIC X(40).
      *     ...

      * Con REPLACING:
       COPY CLIENTE REPLACING
           ==CLI-== BY ==WSC-==.
      * Cambia todos los prefijos CLI- por WSC-
```

### Copybook Típico

Archivo: `CLIENTE.cpy`
```cobol
      * COPYBOOK: CLIENTE
      * Estructura de registro de cliente
       01  WS-CLIENTE.
           05  CLI-ID              PIC 9(10).
           05  CLI-TIPO            PIC X(1).
               88  CLI-PERSONA     VALUE 'P'.
               88  CLI-EMPRESA     VALUE 'E'.
           05  CLI-NOMBRE          PIC X(40).
           05  CLI-RFC             PIC X(13).
           05  CLI-FECHA-ALTA      PIC 9(8).
           05  CLI-SALDO           PIC S9(11)V99 COMP-3.
           05  CLI-LIMITE          PIC S9(11)V99 COMP-3.
           05  CLI-ESTADO          PIC X(1).
               88  CLI-ACTIVO      VALUE 'A'.
               88  CLI-BLOQUEADO   VALUE 'B'.
               88  CLI-CERRADO     VALUE 'C'.
```

---

## LINKAGE SECTION

Para recibir datos de CALL:

### Programa Llamador

```cobol
       WORKING-STORAGE SECTION.
       01  WS-PARAMETRO1           PIC X(20).
       01  WS-PARAMETRO2           PIC 9(5).
       01  WS-RESULTADO            PIC 9(10).

       PROCEDURE DIVISION.
           MOVE "DATO" TO WS-PARAMETRO1
           MOVE 100 TO WS-PARAMETRO2

           CALL "SUBPROGRAMA" USING
               WS-PARAMETRO1
               WS-PARAMETRO2
               WS-RESULTADO
```

### Programa Llamado

```cobol
       LINKAGE SECTION.
       01  LS-PARAM1               PIC X(20).
       01  LS-PARAM2               PIC 9(5).
       01  LS-RESULT               PIC 9(10).

       PROCEDURE DIVISION USING
           LS-PARAM1
           LS-PARAM2
           LS-RESULT.

           COMPUTE LS-RESULT = LS-PARAM2 * 100
           GOBACK.
```

---

## LOCAL-STORAGE SECTION

Variables que se reinician en cada invocación:

```cobol
       LOCAL-STORAGE SECTION.
       01  LS-CONTADOR             PIC 9(5) VALUE 0.
       01  LS-ACUMULADOR           PIC 9(10) VALUE 0.

      * Cada vez que se llama al programa,
      * LS-CONTADOR y LS-ACUMULADOR vuelven a 0
```

---

## Campos Editados

Para presentación de datos:

```cobol
       01  WS-DATOS.
           05  WS-MONTO            PIC 9(7)V99 VALUE 1234567.89.
           05  WS-NEGATIVO         PIC S9(7)V99 VALUE -12345.67.
           05  WS-FECHA            PIC 9(8) VALUE 20241231.

       01  WS-EDITADOS.
           05  WS-MONTO-EDIT       PIC $Z,ZZZ,ZZ9.99.
           05  WS-NEG-EDIT         PIC -Z,ZZZ,ZZ9.99.
           05  WS-FECHA-EDIT       PIC 99/99/9999.

       PROCEDURE DIVISION.
           MOVE WS-MONTO TO WS-MONTO-EDIT
      *    Resultado: $1,234,567.89

           MOVE WS-NEGATIVO TO WS-NEG-EDIT
      *    Resultado: -   12,345.67

           MOVE WS-FECHA TO WS-FECHA-EDIT
      *    Resultado: 31/12/2024
```

### Símbolos de Edición

| Símbolo | Función | Ejemplo |
|---------|---------|---------|
| Z | Suprime ceros | ZZZ99 → "  123" |
| 9 | Siempre dígito | 99999 → "00123" |
| $ | Signo moneda | $ZZZ.99 → "$123.45" |
| , | Coma | Z,ZZZ → "1,234" |
| . | Punto decimal | ZZZ.99 → "123.45" |
| - | Signo si negativo | -ZZZ → "-123" |
| + | Signo siempre | +ZZZ → "+123" |
| CR | Crédito | ZZZ.99CR → "123.45CR" |
| DB | Débito | ZZZ.99DB → "123.45DB" |
| B | Inserta espacio | 99B99 → "12 34" |
| 0 | Inserta cero | 990099 → "120034" |
| / | Inserta slash | 99/99/99 → "31/12/24" |

---

## Próximos Documentos

- **COBOL_03_PROCEDURE.md**: PROCEDURE DIVISION
- **COBOL_04_FILES.md**: Manejo de archivos
- **COBOL_05_MAINFRAME.md**: JCL y entorno mainframe

---

*"La DATA DIVISION es el contrato con la realidad. Define qué datos existen y cómo se ven."*


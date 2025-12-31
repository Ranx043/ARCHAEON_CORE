# PROTO_04_MAPPINGS - Mapeos Semanticos

> ARCHAEON - Guardian del Codigo Ancestral
> Dominio: Mapeos entre Lenguajes Legacy y Modernos
> Nivel: Avanzado

## Indice

1. [Fundamentos de Mapeo](#1-fundamentos-de-mapeo)
2. [Mapeo de Tipos de Datos](#2-mapeo-de-tipos-de-datos)
3. [Mapeo de Control de Flujo](#3-mapeo-de-control-de-flujo)
4. [Mapeo de Modelos de Memoria](#4-mapeo-de-modelos-de-memoria)
5. [Mapeo de I/O](#5-mapeo-de-io)
6. [Mapeo de Errores](#6-mapeo-de-errores)
7. [Mapeo de Concurrencia](#7-mapeo-de-concurrencia)
8. [Tablas de Referencia](#8-tablas-de-referencia)

---

## 1. Fundamentos de Mapeo

### 1.1 Principios de Mapeo Semantico

```yaml
MAPEO_SEMANTICO:
  objetivo:
    - Preservar significado y comportamiento
    - Traducir idioms a equivalentes naturales
    - Mantener rendimiento comparable

  niveles:
    sintactico:
      descripcion: "Traduccion directa de sintaxis"
      precision: "Alta"
      idiomatico: "Bajo"

    semantico:
      descripcion: "Preservar significado"
      precision: "Alta"
      idiomatico: "Medio"

    idiomatico:
      descripcion: "Usar patrones naturales del destino"
      precision: "Media"
      idiomatico: "Alto"

  consideraciones:
    - Precision numerica
    - Manejo de null/undefined
    - Orden de evaluacion
    - Efectos secundarios
    - Limites de overflow
```

### 1.2 Framework de Mapeo

```python
# === Framework de Mapeo Semantico ===

from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, Type
from dataclasses import dataclass
from enum import Enum

class LenguajeOrigen(Enum):
    COBOL = "cobol"
    C = "c"
    FORTRAN = "fortran"
    ASSEMBLY = "assembly"

class LenguajeDestino(Enum):
    PYTHON = "python"
    JAVA = "java"
    RUST = "rust"
    GO = "go"

@dataclass
class TipoOrigen:
    """Representa un tipo en lenguaje origen"""
    nombre: str
    lenguaje: LenguajeOrigen
    atributos: Dict[str, Any]

@dataclass
class TipoDestino:
    """Representa un tipo en lenguaje destino"""
    nombre: str
    lenguaje: LenguajeDestino
    codigo_ejemplo: str
    notas: str = ""

class Mapeador(ABC):
    """Clase base para mapeadores"""

    @abstractmethod
    def mapear_tipo(self, tipo: TipoOrigen) -> TipoDestino:
        pass

    @abstractmethod
    def mapear_expresion(self, expr: str) -> str:
        pass

class MapeadorCOBOLPython(Mapeador):
    """Mapea COBOL a Python"""

    MAPEO_TIPOS = {
        'PIC 9': 'int',
        'PIC S9': 'int',
        'PIC X': 'str',
        'PIC A': 'str',
        'COMP': 'int',
        'COMP-3': 'Decimal',
        'COMP-1': 'float',
        'COMP-2': 'float',
    }

    def mapear_tipo(self, tipo: TipoOrigen) -> TipoDestino:
        pic = tipo.atributos.get('picture', '')
        decimales = tipo.atributos.get('decimales', 0)

        # Determinar tipo base
        tipo_python = self._pic_a_python(pic, decimales)

        return TipoDestino(
            nombre=tipo_python,
            lenguaje=LenguajeDestino.PYTHON,
            codigo_ejemplo=self._generar_ejemplo(tipo, tipo_python)
        )

    def _pic_a_python(self, pic: str, decimales: int) -> str:
        if 'V' in pic or decimales > 0:
            return 'Decimal'
        if pic.startswith('9') or pic.startswith('S9'):
            return 'int'
        if pic.startswith('X') or pic.startswith('A'):
            return 'str'
        return 'Any'

    def mapear_expresion(self, expr: str) -> str:
        # Transformar expresion COBOL a Python
        transformaciones = [
            ('COMPUTE ', ''),
            (' = ', ' = '),
            (' + ', ' + '),
            (' - ', ' - '),
            (' * ', ' * '),
            (' / ', ' / '),
            (' ** ', ' ** '),
        ]

        resultado = expr
        for cobol, python in transformaciones:
            resultado = resultado.replace(cobol, python)

        return resultado

    def _generar_ejemplo(self, tipo: TipoOrigen,
                         tipo_python: str) -> str:
        nombre = tipo.nombre.lower().replace('-', '_')

        if tipo_python == 'Decimal':
            return f"{nombre}: Decimal = Decimal('0')"
        elif tipo_python == 'int':
            return f"{nombre}: int = 0"
        elif tipo_python == 'str':
            longitud = tipo.atributos.get('longitud', 1)
            return f"{nombre}: str = ' ' * {longitud}"
        else:
            return f"{nombre}: {tipo_python}"
```

---

## 2. Mapeo de Tipos de Datos

### 2.1 COBOL a Python/Java

| COBOL PIC | Python | Java | Notas |
|-----------|--------|------|-------|
| `PIC 9(n)` | `int` | `int/long` | Entero sin signo |
| `PIC S9(n)` | `int` | `int/long` | Entero con signo |
| `PIC 9(n)V9(m)` | `Decimal` | `BigDecimal` | Decimal fijo |
| `PIC S9(n)V9(m)` | `Decimal` | `BigDecimal` | Decimal con signo |
| `PIC X(n)` | `str` | `String` | Alfanumerico |
| `PIC A(n)` | `str` | `String` | Solo alfabetico |
| `COMP/BINARY` | `int` | `int/long` | Binario nativo |
| `COMP-1` | `float` | `float` | Punto flotante simple |
| `COMP-2` | `float` | `double` | Punto flotante doble |
| `COMP-3/PACKED` | `Decimal` | `BigDecimal` | Packed decimal |

```python
# === Ejemplo de mapeo COBOL PIC a Python ===

from decimal import Decimal, ROUND_HALF_UP
from dataclasses import dataclass
from typing import Optional
import re

@dataclass
class COBOLField:
    """Campo COBOL traducido a Python"""
    nombre: str
    tipo_python: type
    longitud: int
    decimales: int = 0
    signed: bool = False
    valor_inicial: any = None

def parsear_pic(pic: str) -> dict:
    """Parsea PICTURE COBOL y retorna atributos"""

    resultado = {
        'tipo': 'str',
        'longitud': 0,
        'decimales': 0,
        'signed': False
    }

    # Detectar signo
    if pic.startswith('S'):
        resultado['signed'] = True
        pic = pic[1:]

    # Expandir notacion comprimida: 9(5) -> 99999
    def expandir(match):
        char = match.group(1)
        count = int(match.group(2))
        return char * count

    pic_expandido = re.sub(r'([9XAV])\((\d+)\)', expandir, pic)

    # Contar caracteres
    enteros = pic_expandido.count('9')
    alfanum = pic_expandido.count('X') + pic_expandido.count('A')

    # Detectar decimales (V marca posicion decimal)
    if 'V' in pic_expandido:
        partes = pic_expandido.split('V')
        enteros = partes[0].count('9')
        resultado['decimales'] = partes[1].count('9')
        resultado['tipo'] = 'decimal'
        resultado['longitud'] = enteros + resultado['decimales']
    elif alfanum > 0:
        resultado['tipo'] = 'str'
        resultado['longitud'] = alfanum
    else:
        resultado['tipo'] = 'int'
        resultado['longitud'] = enteros

    return resultado

# Convertir a clase Python
def cobol_pic_to_python(nombre: str, pic: str) -> str:
    """Genera codigo Python para un campo COBOL"""

    info = parsear_pic(pic)
    nombre_py = nombre.lower().replace('-', '_')

    if info['tipo'] == 'decimal':
        precision = info['longitud']
        escala = info['decimales']
        return f"{nombre_py}: Decimal  # PIC {pic} -> {precision}.{escala}"

    elif info['tipo'] == 'int':
        max_val = 10 ** info['longitud'] - 1
        return f"{nombre_py}: int  # PIC {pic} -> max {max_val}"

    else:  # str
        return f"{nombre_py}: str  # PIC {pic} -> len {info['longitud']}"

# Ejemplo
print(cobol_pic_to_python('CLI-CODIGO', '9(8)'))
# cli_codigo: int  # PIC 9(8) -> max 99999999

print(cobol_pic_to_python('CLI-SALDO', 'S9(7)V99'))
# cli_saldo: Decimal  # PIC S9(7)V99 -> 9.2
```

### 2.2 C a Rust/Go

| C | Rust | Go | Notas |
|---|------|-----|-------|
| `char` | `i8/u8` | `int8/byte` | 8 bits |
| `short` | `i16` | `int16` | 16 bits |
| `int` | `i32` | `int32` | 32 bits |
| `long` | `i64` | `int64` | 64 bits |
| `unsigned int` | `u32` | `uint32` | Sin signo |
| `float` | `f32` | `float32` | Simple precision |
| `double` | `f64` | `float64` | Doble precision |
| `char*` | `String/*str` | `string` | Cadena |
| `void*` | `*mut c_void` | `unsafe.Pointer` | Puntero generico |
| `int[]` | `Vec<i32>/[i32]` | `[]int32` | Array |
| `struct` | `struct` | `struct` | Estructura |
| `union` | `enum` | N/A | Union -> enum Rust |
| `NULL` | `None/null` | `nil` | Valor nulo |

```rust
// === Ejemplo de mapeo C a Rust ===

// C original
// typedef struct {
//     int id;
//     char nombre[50];
//     double saldo;
//     struct tm* fecha;
// } Cliente;

// Rust equivalente
use chrono::{DateTime, Utc};

#[derive(Debug, Clone)]
pub struct Cliente {
    pub id: i32,
    pub nombre: String,        // char[50] -> String
    pub saldo: f64,           // double -> f64
    pub fecha: Option<DateTime<Utc>>,  // struct tm* -> Option<DateTime>
}

impl Cliente {
    pub fn new(id: i32, nombre: &str, saldo: f64) -> Self {
        Self {
            id,
            nombre: nombre.to_string(),
            saldo,
            fecha: Some(Utc::now()),
        }
    }

    // C: void set_nombre(Cliente* c, const char* n)
    pub fn set_nombre(&mut self, nombre: &str) {
        // Limitar a 49 caracteres como en C
        self.nombre = nombre.chars().take(49).collect();
    }
}

// Manejo de punteros nulos
// C: if (cliente != NULL) { ... }
// Rust:
fn procesar_cliente(cliente: Option<&Cliente>) {
    if let Some(c) = cliente {
        println!("ID: {}", c.id);
    }
}
```

### 2.3 Fortran a Python/NumPy

| Fortran | Python/NumPy | Notas |
|---------|--------------|-------|
| `INTEGER` | `int/np.int32` | 32 bits default |
| `INTEGER*8` | `np.int64` | 64 bits |
| `REAL` | `float/np.float32` | Simple precision |
| `REAL*8/DOUBLE` | `np.float64` | Doble precision |
| `COMPLEX` | `complex/np.complex64` | Complejo |
| `CHARACTER*n` | `str` | Cadena |
| `LOGICAL` | `bool/np.bool_` | Booleano |
| `REAL A(N,M)` | `np.ndarray` | Array 2D |
| `DIMENSION` | `np.zeros/ones` | Declaracion array |

```python
# === Ejemplo de mapeo Fortran a NumPy ===
import numpy as np

# Fortran original:
# REAL*8 MATRIZ(100, 100)
# INTEGER*4 VECTOR(1000)
# COMPLEX*16 EIGENVALUES(50)

# Python equivalente
matriz: np.ndarray = np.zeros((100, 100), dtype=np.float64)
vector: np.ndarray = np.zeros(1000, dtype=np.int32)
eigenvalues: np.ndarray = np.zeros(50, dtype=np.complex128)

# Fortran COMMON block
# COMMON /PARAMS/ TOL, MAXITER, N
# equivalente Python con dataclass

from dataclasses import dataclass

@dataclass
class FortranParams:
    tol: float = 1e-6
    maxiter: int = 100
    n: int = 0

# Singleton para emular COMMON
_params = FortranParams()

def get_params() -> FortranParams:
    return _params

# Fortran array operations
# DO I = 1, N
#    A(I) = B(I) + C(I)
# END DO

# NumPy vectorizado
def array_add(b: np.ndarray, c: np.ndarray) -> np.ndarray:
    return b + c  # Operacion vectorizada

# Fortran array slicing (1-indexed, column-major)
# A(1:50, 1:50) = B(51:100, 51:100)

# NumPy (0-indexed, row-major by default)
# Nota: Fortran es column-major, NumPy es row-major
def slice_example():
    A = np.zeros((100, 100), order='F')  # 'F' para column-major
    B = np.zeros((100, 100), order='F')

    # Ajustar indices (Fortran 1-based -> Python 0-based)
    A[0:50, 0:50] = B[50:100, 50:100]
```

---

## 3. Mapeo de Control de Flujo

### 3.1 PERFORM (COBOL) a Funciones

| COBOL | Python | Java |
|-------|--------|------|
| `PERFORM parrafo` | `parrafo()` | `parrafo();` |
| `PERFORM parrafo THRU parrafo-exit` | `parrafo()` | `parrafo();` |
| `PERFORM parrafo N TIMES` | `for _ in range(n): parrafo()` | `for(int i=0;i<n;i++) parrafo();` |
| `PERFORM parrafo UNTIL cond` | `while not cond: parrafo()` | `while(!cond) parrafo();` |
| `PERFORM parrafo VARYING i` | `for i in range(...): parrafo()` | `for(int i=...;...) parrafo();` |

```python
# === Mapeo de PERFORM COBOL a Python ===

# COBOL:
# PERFORM PROCESAR-REGISTRO
#     UNTIL FIN-ARCHIVO
# END-PERFORM

# Python directo:
while not fin_archivo:
    procesar_registro()

# Python idiomatico (iterador):
for registro in leer_registros(archivo):
    procesar_registro(registro)

# ---

# COBOL:
# PERFORM CALCULAR
#     VARYING I FROM 1 BY 1 UNTIL I > 100
# END-PERFORM

# Python:
for i in range(1, 101):
    calcular(i)

# ---

# COBOL:
# PERFORM PROCESAR-LOTE 10 TIMES

# Python:
for _ in range(10):
    procesar_lote()

# ---

# COBOL anidado:
# PERFORM PROCESAR-FILA
#     VARYING I FROM 1 BY 1 UNTIL I > FILAS
#     AFTER J FROM 1 BY 1 UNTIL J > COLUMNAS
# END-PERFORM

# Python:
for i in range(1, filas + 1):
    for j in range(1, columnas + 1):
        procesar_fila(i, j)
```

### 3.2 DO (Fortran) a for/while

| Fortran | Python | Notas |
|---------|--------|-------|
| `DO I = 1, N` | `for i in range(1, n+1)` | Ajustar indices |
| `DO I = 1, N, 2` | `for i in range(1, n+1, 2)` | Con paso |
| `DO WHILE (cond)` | `while cond:` | While |
| `DO` (infinito) | `while True:` | Loop infinito |
| `EXIT` | `break` | Salir del loop |
| `CYCLE` | `continue` | Siguiente iteracion |
| `DO CONCURRENT` | `multiprocessing/numba` | Paralelo |

```python
# === Mapeo DO Fortran a Python ===
import numpy as np
from numba import prange, njit

# Fortran:
# DO I = 1, N
#    A(I) = B(I) * C(I)
# END DO

# Python literal
for i in range(n):
    a[i] = b[i] * c[i]

# Python vectorizado (preferido)
a = b * c

# ---

# Fortran DO WHILE:
# DO WHILE (ERROR .GT. TOL)
#    CALL ITERAR(ERROR)
# END DO

# Python:
while error > tol:
    error = iterar()

# ---

# Fortran DO CONCURRENT (paralelo):
# DO CONCURRENT (I = 1:N)
#    A(I) = SQRT(B(I))
# END DO

# Python con NumPy (vectorizado, usa SIMD)
a = np.sqrt(b)

# Python con Numba (paralelo explícito)
@njit(parallel=True)
def sqrt_parallel(b):
    n = len(b)
    a = np.empty(n)
    for i in prange(n):
        a[i] = np.sqrt(b[i])
    return a
```

### 3.3 Eliminacion de GOTO

| Patron GOTO | Estructura Equivalente |
|-------------|------------------------|
| `GOTO error_handler` | `try/except` o `Result` |
| `GOTO cleanup` | `try/finally` o `defer` |
| `GOTO loop_exit` | `break` |
| `GOTO next_iteration` | `continue` |
| `GOTO case_n` | `match/switch` |

```python
# === Eliminacion de GOTO ===

# Fortran con GOTO:
# 10   READ(*,*) X
#      IF (X .LT. 0) GOTO 100
#      IF (X .GT. 1000) GOTO 200
#      RESULT = SQRT(X)
#      GOTO 999
# 100  WRITE(*,*) 'Negativo'
#      GOTO 10
# 200  WRITE(*,*) 'Muy grande'
# 999  CONTINUE

# Python estructurado:
def procesar_entrada():
    while True:
        x = float(input())

        if x < 0:
            print('Negativo')
            continue  # GOTO 10

        if x > 1000:
            print('Muy grande')
            return None  # GOTO 999

        return x ** 0.5  # GOTO 999

# Python con excepciones:
class ValorNegativoError(Exception):
    pass

class ValorMuyGrandeError(Exception):
    pass

def procesar_con_excepciones():
    while True:
        try:
            x = float(input())

            if x < 0:
                raise ValorNegativoError()
            if x > 1000:
                raise ValorMuyGrandeError()

            return x ** 0.5

        except ValorNegativoError:
            print('Negativo')
        except ValorMuyGrandeError:
            print('Muy grande')
            return None
```

---

## 4. Mapeo de Modelos de Memoria

### 4.1 WORKING-STORAGE a Clases

```cobol
       * COBOL WORKING-STORAGE
       WORKING-STORAGE SECTION.
       01  WS-CONTADORES.
           05  WS-TOTAL-LEIDOS    PIC 9(6) VALUE 0.
           05  WS-TOTAL-ERRORES   PIC 9(6) VALUE 0.
           05  WS-TOTAL-PROCESADOS PIC 9(6) VALUE 0.

       01  WS-CLIENTE.
           05  WS-CLI-ID          PIC 9(8).
           05  WS-CLI-NOMBRE      PIC X(30).
           05  WS-CLI-SALDO       PIC S9(7)V99.
```

```python
# === Python equivalente ===
from dataclasses import dataclass, field
from decimal import Decimal

@dataclass
class Contadores:
    """WS-CONTADORES"""
    total_leidos: int = 0
    total_errores: int = 0
    total_procesados: int = 0

    def incrementar_leidos(self):
        self.total_leidos += 1

    def incrementar_errores(self):
        self.total_errores += 1

    def incrementar_procesados(self):
        self.total_procesados += 1

@dataclass
class Cliente:
    """WS-CLIENTE"""
    id: int = 0                           # PIC 9(8)
    nombre: str = ""                      # PIC X(30)
    saldo: Decimal = field(default_factory=lambda: Decimal('0'))  # PIC S9(7)V99

    def __post_init__(self):
        # Validar limites como COBOL
        if self.id < 0 or self.id > 99999999:
            raise ValueError("ID fuera de rango")
        if len(self.nombre) > 30:
            self.nombre = self.nombre[:30]

class ProgramaEstado:
    """Emula WORKING-STORAGE completo"""

    def __init__(self):
        self.contadores = Contadores()
        self.cliente = Cliente()

    def reiniciar(self):
        self.contadores = Contadores()
        self.cliente = Cliente()
```

### 4.2 malloc/free a Ownership (Rust)

```c
// C con malloc/free
typedef struct {
    int* data;
    size_t size;
    size_t capacity;
} Vector;

Vector* vector_create(size_t capacity) {
    Vector* v = malloc(sizeof(Vector));
    if (!v) return NULL;

    v->data = malloc(capacity * sizeof(int));
    if (!v->data) {
        free(v);
        return NULL;
    }

    v->size = 0;
    v->capacity = capacity;
    return v;
}

void vector_destroy(Vector* v) {
    if (v) {
        free(v->data);
        free(v);
    }
}

void vector_push(Vector* v, int value) {
    if (v->size >= v->capacity) {
        // Realloc...
    }
    v->data[v->size++] = value;
}
```

```rust
// Rust con ownership
pub struct Vector {
    data: Vec<i32>,  // Vec maneja memoria automaticamente
}

impl Vector {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self { data: Vec::with_capacity(capacity) }
    }

    pub fn push(&mut self, value: i32) {
        self.data.push(value);  // Crece automaticamente
    }

    pub fn get(&self, index: usize) -> Option<i32> {
        self.data.get(index).copied()
    }
}

// Drop se implementa automaticamente
// No se necesita vector_destroy()

// Uso seguro
fn ejemplo() {
    let mut v = Vector::new();
    v.push(1);
    v.push(2);
    // v se libera automaticamente al salir del scope
}
```

### 4.3 COMMON Blocks a Modulos

```fortran
! Fortran COMMON
      COMMON /GLOBALS/ A, B, C
      COMMON /CONFIG/ TOL, MAXITER

      SUBROUTINE PROCESAR
      COMMON /GLOBALS/ A, B, C
      ! Usa A, B, C
      END
```

```python
# Python - Modulo como singleton
# archivo: globals.py
from dataclasses import dataclass

@dataclass
class GlobalState:
    a: float = 0.0
    b: float = 0.0
    c: float = 0.0

@dataclass
class Config:
    tol: float = 1e-6
    maxiter: int = 100

# Instancias singleton
_globals = GlobalState()
_config = Config()

def get_globals() -> GlobalState:
    return _globals

def get_config() -> Config:
    return _config

# archivo: processor.py
from globals import get_globals, get_config

def procesar():
    g = get_globals()
    cfg = get_config()

    # Usa g.a, g.b, g.c
    # Usa cfg.tol, cfg.maxiter
```

---

## 5. Mapeo de I/O

### 5.1 Archivos COBOL a Python

| COBOL | Python | Notas |
|-------|--------|-------|
| `OPEN INPUT` | `open(f, 'r')` | Lectura |
| `OPEN OUTPUT` | `open(f, 'w')` | Escritura |
| `OPEN I-O` | `open(f, 'r+')` | Lectura/Escritura |
| `OPEN EXTEND` | `open(f, 'a')` | Append |
| `CLOSE` | `f.close()` | Cerrar |
| `READ ... AT END` | `for line in f` | Leer con EOF |
| `WRITE` | `f.write()` | Escribir |
| `REWRITE` | Seek + write | Actualizar |
| `DELETE` | Requiere reescribir | Eliminar registro |
| `START` | `f.seek()` | Posicionar |

```python
# === Mapeo I/O COBOL a Python ===

# COBOL archivo secuencial
class ArchivoSecuencialCOBOL:
    """Emula archivo secuencial COBOL"""

    def __init__(self, nombre: str, longitud_registro: int):
        self.nombre = nombre
        self.longitud = longitud_registro
        self.archivo = None
        self.file_status = "00"

    def open_input(self):
        try:
            self.archivo = open(self.nombre, 'r')
            self.file_status = "00"
        except FileNotFoundError:
            self.file_status = "35"

    def open_output(self):
        self.archivo = open(self.nombre, 'w')
        self.file_status = "00"

    def read(self) -> str:
        """READ ... AT END"""
        linea = self.archivo.readline()
        if not linea:
            self.file_status = "10"  # EOF
            return None
        self.file_status = "00"
        return linea.rstrip('\n')

    def write(self, registro: str):
        """WRITE"""
        # Padding a longitud fija como COBOL
        registro_fijo = registro.ljust(self.longitud)[:self.longitud]
        self.archivo.write(registro_fijo + '\n')
        self.file_status = "00"

    def close(self):
        if self.archivo:
            self.archivo.close()
            self.archivo = None
        self.file_status = "00"


# Uso equivalente al COBOL
archivo = ArchivoSecuencialCOBOL('datos.txt', 80)
archivo.open_input()

while True:
    registro = archivo.read()
    if archivo.file_status == "10":  # AT END
        break
    procesar(registro)

archivo.close()
```

### 5.2 Formatos Fortran a Python

| Fortran | Python | Ejemplo |
|---------|--------|---------|
| `I5` | `{:5d}` | Entero 5 digitos |
| `F10.4` | `{:10.4f}` | Float 10 ancho, 4 dec |
| `E15.6` | `{:15.6e}` | Exponencial |
| `A20` | `{:20s}` | String 20 chars |
| `X` | ` ` | Espacio |
| `/` | `\n` | Nueva linea |

```python
# === Mapeo formatos Fortran a Python ===

def fortran_format_to_python(fortran_fmt: str) -> str:
    """Convierte formato Fortran a format string Python"""
    import re

    # Mapeos basicos
    conversiones = []

    # Patron para descriptores
    patron = r'(\d*)([IFEAD])(\d+)(?:\.(\d+))?'

    for match in re.finditer(patron, fortran_fmt, re.IGNORECASE):
        repetir = int(match.group(1)) if match.group(1) else 1
        tipo = match.group(2).upper()
        ancho = int(match.group(3))
        decimales = int(match.group(4)) if match.group(4) else 0

        for _ in range(repetir):
            if tipo == 'I':
                conversiones.append(f'{{:{ancho}d}}')
            elif tipo == 'F':
                conversiones.append(f'{{:{ancho}.{decimales}f}}')
            elif tipo == 'E':
                conversiones.append(f'{{:{ancho}.{decimales}e}}')
            elif tipo == 'A':
                conversiones.append(f'{{:{ancho}s}}')
            elif tipo == 'D':
                conversiones.append(f'{{:{ancho}.{decimales}e}}')

    return ''.join(conversiones)

# Ejemplo
# Fortran: WRITE(*,'(I5,F10.4,A20)') N, X, NOMBRE
# Python:
n = 42
x = 3.14159
nombre = "Test"

fmt = fortran_format_to_python('I5,F10.4,A20')
print(fmt.format(n, x, nombre))
# Salida: "   42    3.1416Test                "
```

---

## 6. Mapeo de Errores

### 6.1 Codigos de Error a Excepciones

| Patron Legacy | Python | Rust |
|--------------|--------|------|
| `return -1` | `raise Exception` | `Err(...)` |
| `errno` | `OSError` | `std::io::Error` |
| `FILE-STATUS` | `try/except` | `Result<T, E>` |
| `INVALID KEY` | `KeyError` | `Option::None` |

```python
# === Mapeo errores COBOL FILE-STATUS a Python ===

from enum import Enum
from typing import Optional

class COBOLFileStatus(Enum):
    """FILE STATUS codes COBOL"""
    SUCCESS = "00"
    EOF = "10"
    DUPLICATE_KEY = "22"
    RECORD_NOT_FOUND = "23"
    DISK_FULL = "34"
    FILE_NOT_FOUND = "35"
    PERMISSION_DENIED = "37"

class COBOLFileError(Exception):
    """Excepcion base para errores de archivo COBOL"""
    def __init__(self, status: COBOLFileStatus, mensaje: str = ""):
        self.status = status
        self.mensaje = mensaje or self._mensaje_default()
        super().__init__(self.mensaje)

    def _mensaje_default(self) -> str:
        mensajes = {
            COBOLFileStatus.EOF: "Fin de archivo",
            COBOLFileStatus.DUPLICATE_KEY: "Clave duplicada",
            COBOLFileStatus.RECORD_NOT_FOUND: "Registro no encontrado",
            COBOLFileStatus.DISK_FULL: "Disco lleno",
            COBOLFileStatus.FILE_NOT_FOUND: "Archivo no encontrado",
            COBOLFileStatus.PERMISSION_DENIED: "Permiso denegado",
        }
        return mensajes.get(self.status, f"Error {self.status.value}")

# Excepciones especificas
class EOFError(COBOLFileError):
    def __init__(self):
        super().__init__(COBOLFileStatus.EOF)

class DuplicateKeyError(COBOLFileError):
    def __init__(self, key):
        super().__init__(COBOLFileStatus.DUPLICATE_KEY, f"Clave duplicada: {key}")

class RecordNotFoundError(COBOLFileError):
    def __init__(self, key):
        super().__init__(COBOLFileStatus.RECORD_NOT_FOUND, f"No encontrado: {key}")

# Uso
def leer_registro(archivo, clave):
    """
    COBOL:
    READ ARCHIVO-CLIENTES
        INVALID KEY
            DISPLAY 'NO ENCONTRADO'
        NOT INVALID KEY
            DISPLAY CLI-NOMBRE
    END-READ
    """
    try:
        registro = archivo.buscar(clave)
        return registro
    except RecordNotFoundError:
        print('NO ENCONTRADO')
        return None
```

```rust
// === Rust: Result en lugar de excepciones ===

use thiserror::Error;

#[derive(Error, Debug)]
pub enum FileError {
    #[error("Fin de archivo")]
    EndOfFile,

    #[error("Clave duplicada: {0}")]
    DuplicateKey(String),

    #[error("Registro no encontrado: {0}")]
    RecordNotFound(String),

    #[error("Error de IO: {0}")]
    IoError(#[from] std::io::Error),
}

type Result<T> = std::result::Result<T, FileError>;

fn leer_registro(archivo: &Archivo, clave: &str) -> Result<Registro> {
    archivo.buscar(clave)
        .ok_or_else(|| FileError::RecordNotFound(clave.to_string()))
}

// Uso
fn main() {
    match leer_registro(&archivo, "12345") {
        Ok(registro) => println!("{}", registro.nombre),
        Err(FileError::RecordNotFound(_)) => println!("NO ENCONTRADO"),
        Err(e) => eprintln!("Error: {}", e),
    }
}
```

---

## 7. Mapeo de Concurrencia

### 7.1 Threads C a Rust/Go

| C (pthreads) | Rust | Go |
|--------------|------|-----|
| `pthread_create` | `thread::spawn` | `go func()` |
| `pthread_join` | `handle.join()` | `wg.Wait()` |
| `pthread_mutex_t` | `Mutex<T>` | `sync.Mutex` |
| `pthread_cond_t` | `Condvar` | `sync.Cond` |
| `pthread_rwlock_t` | `RwLock<T>` | `sync.RWMutex` |

```c
// C con pthreads
#include <pthread.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int contador = 0;

void* incrementar(void* arg) {
    for (int i = 0; i < 1000; i++) {
        pthread_mutex_lock(&mutex);
        contador++;
        pthread_mutex_unlock(&mutex);
    }
    return NULL;
}

int main() {
    pthread_t threads[10];

    for (int i = 0; i < 10; i++) {
        pthread_create(&threads[i], NULL, incrementar, NULL);
    }

    for (int i = 0; i < 10; i++) {
        pthread_join(threads[i], NULL);
    }

    printf("Contador: %d\n", contador);
    return 0;
}
```

```rust
// Rust equivalente
use std::sync::{Arc, Mutex};
use std::thread;

fn main() {
    let contador = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let contador_clone = Arc::clone(&contador);
        let handle = thread::spawn(move || {
            for _ in 0..1000 {
                let mut num = contador_clone.lock().unwrap();
                *num += 1;
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Contador: {}", *contador.lock().unwrap());
}
```

```go
// Go equivalente
package main

import (
    "fmt"
    "sync"
)

func main() {
    var mutex sync.Mutex
    var wg sync.WaitGroup
    contador := 0

    for i := 0; i < 10; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            for j := 0; j < 1000; j++ {
                mutex.Lock()
                contador++
                mutex.Unlock()
            }
        }()
    }

    wg.Wait()
    fmt.Printf("Contador: %d\n", contador)
}
```

---

## 8. Tablas de Referencia

### 8.1 Tabla Maestra: COBOL a Python

| Concepto COBOL | Python Equivalente |
|----------------|-------------------|
| `IDENTIFICATION DIVISION` | Docstring del modulo |
| `ENVIRONMENT DIVISION` | Imports y configuracion |
| `DATA DIVISION` | Definicion de clases |
| `PROCEDURE DIVISION` | Metodos y funciones |
| `WORKING-STORAGE` | Atributos de instancia |
| `LINKAGE SECTION` | Parametros de funcion |
| `FILE SECTION` | Clase de archivo |
| `COPY` | `import` / `from X import` |
| `PERFORM` | Llamada a funcion |
| `MOVE` | Asignacion `=` |
| `COMPUTE` | Expresion aritmetica |
| `IF/ELSE/END-IF` | `if/elif/else` |
| `EVALUATE` | `match` (3.10+) / `if/elif` |
| `PERFORM UNTIL` | `while not` |
| `PERFORM VARYING` | `for i in range()` |
| `STRING/UNSTRING` | f-strings / `.split()` |
| `INSPECT` | `.replace()` / `.count()` |
| `ACCEPT` | `input()` |
| `DISPLAY` | `print()` |
| `STOP RUN` | `sys.exit()` |

### 8.2 Tabla Maestra: C a Rust

| Concepto C | Rust Equivalente |
|------------|-----------------|
| `int` | `i32` |
| `char*` | `String` / `&str` |
| `struct` | `struct` |
| `union` | `enum` con variantes |
| `typedef` | `type` alias |
| `#define CONST` | `const` |
| `#define MACRO()` | `macro_rules!` |
| `malloc/free` | `Box::new` / auto drop |
| `NULL` | `None` / `null` |
| `if (ptr)` | `if let Some(x)` |
| `switch` | `match` |
| `for` | `for` / iteradores |
| `while` | `while` / `loop` |
| `goto cleanup` | `?` operator / RAII |
| `printf` | `println!` macro |
| `FILE*` | `File` |
| `errno` | `Result<T, E>` |
| `assert()` | `assert!` / `debug_assert!` |

### 8.3 Tabla Maestra: Fortran a Python/NumPy

| Concepto Fortran | Python/NumPy Equivalente |
|------------------|-------------------------|
| `PROGRAM` | `if __name__ == '__main__'` |
| `SUBROUTINE` | `def func()` |
| `FUNCTION` | `def func() -> tipo` |
| `MODULE` | Modulo Python / clase |
| `IMPLICIT NONE` | Type hints |
| `INTEGER` | `int` / `np.int32` |
| `REAL` | `float` / `np.float64` |
| `CHARACTER` | `str` |
| `LOGICAL` | `bool` |
| `COMPLEX` | `complex` / `np.complex128` |
| `DIMENSION` | `np.zeros()` / `np.empty()` |
| `ALLOCATE` | Lista / `np.zeros()` |
| `DEALLOCATE` | `del` / garbage collection |
| `COMMON` | Modulo con variables globales |
| `DO` | `for` |
| `DO WHILE` | `while` |
| `IF/THEN/ELSE` | `if/elif/else` |
| `SELECT CASE` | `match` (3.10+) |
| `CALL` | Llamada a funcion |
| `RETURN` | `return` |
| `WRITE` | `print()` |
| `READ` | `input()` |
| `OPEN/CLOSE` | `open()` context manager |
| `MATMUL` | `@` / `np.matmul` |
| `DOT_PRODUCT` | `np.dot` |
| `SUM/PRODUCT` | `np.sum` / `np.prod` |
| `MAXVAL/MINVAL` | `np.max` / `np.min` |
| `TRANSPOSE` | `.T` / `np.transpose` |

---

## Referencias

1. "COBOL Programmer's Reference" - IBM
2. "The C Programming Language" - K&R
3. "Modern Fortran Explained" - Metcalf et al.
4. "The Rust Programming Language" - Klabnik & Nichols
5. "Effective Go" - golang.org
6. NumPy Documentation - numpy.org

---

*ARCHAEON - Guardian del Codigo Ancestral*
*Mappings - El Diccionario Entre Mundos*

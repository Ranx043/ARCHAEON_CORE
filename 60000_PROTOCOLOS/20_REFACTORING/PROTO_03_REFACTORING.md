# PROTO_03_REFACTORING - Patrones de Refactorizacion

> ARCHAEON - Guardian del Codigo Ancestral
> Dominio: Refactorizacion y Modernizacion de Codigo Legacy
> Nivel: Avanzado

## Indice

1. [Fundamentos de Refactoring](#1-fundamentos-de-refactoring)
2. [Code Smells en Legacy](#2-code-smells-en-legacy)
3. [Refactoring COBOL](#3-refactoring-cobol)
4. [Refactoring C](#4-refactoring-c)
5. [Refactoring Fortran](#5-refactoring-fortran)
6. [Refactoring Assembly](#6-refactoring-assembly)
7. [Testing Durante Refactoring](#7-testing-durante-refactoring)
8. [Modernizacion Incremental](#8-modernizacion-incremental)

---

## 1. Fundamentos de Refactoring

### 1.1 Principios Basicos

```yaml
REFACTORING_PRINCIPLES:
  definicion:
    - Cambiar estructura interna sin alterar comportamiento externo
    - Mejorar legibilidad y mantenibilidad
    - Reducir complejidad tecnica

  reglas_oro:
    - Nunca refactorizar sin tests
    - Cambios pequenos e incrementales
    - Commit frecuente
    - Un solo objetivo por refactoring

  cuando_refactorizar:
    - Antes de agregar nueva funcionalidad
    - Al corregir bugs
    - Durante code review
    - Cuando codigo es dificil de entender

  cuando_NO_refactorizar:
    - Cerca de deadline critico
    - Sin tests automatizados
    - Sin entender completamente el codigo
    - Si funciona y no se tocara mas
```

### 1.2 Proceso de Refactoring Seguro

```python
# === Framework de Refactoring Seguro ===

from abc import ABC, abstractmethod
from typing import List, Callable, Any
from dataclasses import dataclass
import subprocess
import hashlib

@dataclass
class RefactoringStep:
    """Un paso de refactoring"""
    nombre: str
    descripcion: str
    transformacion: Callable
    rollback: Callable

class RefactoringSession:
    """Sesion de refactoring con safety net"""

    def __init__(self, proyecto_path: str):
        self.proyecto = proyecto_path
        self.pasos: List[RefactoringStep] = []
        self.completados: List[RefactoringStep] = []
        self.checkpoint_hash = None

    def crear_checkpoint(self):
        """Crear punto de restauracion"""
        # Git stash o commit temporal
        subprocess.run(['git', 'stash', 'push', '-m', 'refactoring-checkpoint'],
                      cwd=self.proyecto)
        self.checkpoint_hash = self._get_current_hash()

    def restaurar_checkpoint(self):
        """Restaurar si algo sale mal"""
        subprocess.run(['git', 'stash', 'pop'], cwd=self.proyecto)

    def agregar_paso(self, paso: RefactoringStep):
        """Agregar paso al plan"""
        self.pasos.append(paso)

    def ejecutar(self, con_tests: bool = True) -> bool:
        """Ejecutar todos los pasos"""
        self.crear_checkpoint()

        for paso in self.pasos:
            print(f"Ejecutando: {paso.nombre}")

            try:
                paso.transformacion()

                if con_tests and not self._ejecutar_tests():
                    print(f"Tests fallaron en: {paso.nombre}")
                    self._rollback_todos()
                    return False

                self.completados.append(paso)
                self._commit(f"refactor: {paso.nombre}")

            except Exception as e:
                print(f"Error en {paso.nombre}: {e}")
                self._rollback_todos()
                return False

        return True

    def _ejecutar_tests(self) -> bool:
        """Ejecutar suite de tests"""
        result = subprocess.run(
            ['python', '-m', 'pytest', '-q'],
            cwd=self.proyecto,
            capture_output=True
        )
        return result.returncode == 0

    def _rollback_todos(self):
        """Deshacer todos los cambios"""
        for paso in reversed(self.completados):
            paso.rollback()
        self.restaurar_checkpoint()
        self.completados.clear()

    def _commit(self, mensaje: str):
        """Commit incremental"""
        subprocess.run(['git', 'add', '-A'], cwd=self.proyecto)
        subprocess.run(['git', 'commit', '-m', mensaje], cwd=self.proyecto)

    def _get_current_hash(self) -> str:
        result = subprocess.run(
            ['git', 'rev-parse', 'HEAD'],
            cwd=self.proyecto,
            capture_output=True,
            text=True
        )
        return result.stdout.strip()
```

---

## 2. Code Smells en Legacy

### 2.1 Smells Comunes en COBOL

```cobol
       * === SMELL: Parrafo Monolitico ===
       * Un solo parrafo hace demasiado

       PROCESAR-TODO.
           OPEN INPUT ARCHIVO-ENTRADA
           OPEN OUTPUT ARCHIVO-SALIDA
           READ ARCHIVO-ENTRADA
           PERFORM UNTIL FIN-ARCHIVO
               IF TIPO-REGISTRO = 'A'
                   ADD MONTO TO TOTAL-A
                   MOVE MONTO TO WS-MONTO-EDIT
                   STRING CODIGO DELIMITED SIZE
                          WS-MONTO-EDIT DELIMITED SIZE
                          INTO LINEA-SALIDA
                   WRITE LINEA-SALIDA
               END-IF
               IF TIPO-REGISTRO = 'B'
                   SUBTRACT MONTO FROM TOTAL-B
                   COMPUTE DESCUENTO = MONTO * 0.1
                   MOVE DESCUENTO TO WS-DESC-EDIT
                   STRING CODIGO DELIMITED SIZE
                          WS-DESC-EDIT DELIMITED SIZE
                          INTO LINEA-SALIDA
                   WRITE LINEA-SALIDA
               END-IF
               READ ARCHIVO-ENTRADA
                   AT END SET FIN-ARCHIVO TO TRUE
               END-READ
           END-PERFORM
           CLOSE ARCHIVO-ENTRADA ARCHIVO-SALIDA.

       * === REFACTORIZADO: Parrafos Pequenos ===

       PROCESAR-ARCHIVO.
           PERFORM INICIALIZAR-ARCHIVOS
           PERFORM PROCESAR-REGISTROS
           PERFORM CERRAR-ARCHIVOS.

       INICIALIZAR-ARCHIVOS.
           OPEN INPUT ARCHIVO-ENTRADA
           OPEN OUTPUT ARCHIVO-SALIDA.

       PROCESAR-REGISTROS.
           PERFORM LEER-REGISTRO
           PERFORM UNTIL FIN-ARCHIVO
               PERFORM PROCESAR-SEGUN-TIPO
               PERFORM LEER-REGISTRO
           END-PERFORM.

       PROCESAR-SEGUN-TIPO.
           EVALUATE TIPO-REGISTRO
               WHEN 'A' PERFORM PROCESAR-TIPO-A
               WHEN 'B' PERFORM PROCESAR-TIPO-B
               WHEN OTHER CONTINUE
           END-EVALUATE.

       PROCESAR-TIPO-A.
           ADD MONTO TO TOTAL-A
           PERFORM FORMATEAR-MONTO
           PERFORM ESCRIBIR-LINEA.

       PROCESAR-TIPO-B.
           SUBTRACT MONTO FROM TOTAL-B
           COMPUTE DESCUENTO = MONTO * 0.1
           PERFORM FORMATEAR-DESCUENTO
           PERFORM ESCRIBIR-LINEA.
```

### 2.2 Smells Comunes en C

```c
// === SMELL: Funcion Demasiado Larga ===
int procesar_datos(char* archivo, int modo, Config* cfg) {
    FILE* f = fopen(archivo, "r");
    if (!f) return -1;

    char buffer[1024];
    int total = 0;
    int errores = 0;
    int linea = 0;

    // 200+ lineas de codigo mezclado...
    while (fgets(buffer, sizeof(buffer), f)) {
        linea++;
        // parsing
        char* token = strtok(buffer, ",");
        if (!token) { errores++; continue; }

        int valor = atoi(token);
        if (modo == 1) {
            // logica modo 1
            if (cfg->filtro && valor < cfg->minimo) continue;
            total += valor;
        } else if (modo == 2) {
            // logica modo 2
            total += valor * 2;
        }
        // ... mas logica mezclada
    }

    fclose(f);
    return total;
}

// === REFACTORIZADO: Funciones Pequenas ===

typedef struct {
    int total;
    int errores;
    int lineas_procesadas;
} ResultadoProcesamiento;

typedef int (*ProcesadorLinea)(const char*, Config*);

// Funcion principal limpia
ResultadoProcesamiento procesar_datos_v2(const char* archivo,
                                          ProcesadorLinea procesador,
                                          Config* cfg) {
    ResultadoProcesamiento resultado = {0, 0, 0};

    FILE* f = abrir_archivo_seguro(archivo, "r");
    if (!f) {
        resultado.errores = -1;
        return resultado;
    }

    procesar_lineas(f, procesador, cfg, &resultado);

    fclose(f);
    return resultado;
}

// Funciones auxiliares
static FILE* abrir_archivo_seguro(const char* archivo, const char* modo) {
    if (!archivo || !modo) return NULL;
    return fopen(archivo, modo);
}

static void procesar_lineas(FILE* f, ProcesadorLinea proc,
                            Config* cfg, ResultadoProcesamiento* res) {
    char buffer[1024];

    while (fgets(buffer, sizeof(buffer), f)) {
        res->lineas_procesadas++;

        int valor = proc(buffer, cfg);
        if (valor < 0) {
            res->errores++;
        } else {
            res->total += valor;
        }
    }
}

// Procesadores especificos
static int procesador_modo1(const char* linea, Config* cfg) {
    int valor = parsear_valor(linea);
    if (valor < 0) return -1;

    if (cfg->filtro && valor < cfg->minimo) return 0;
    return valor;
}

static int procesador_modo2(const char* linea, Config* cfg) {
    int valor = parsear_valor(linea);
    if (valor < 0) return -1;
    return valor * 2;
}
```

### 2.3 Smells Comunes en Fortran

```fortran
! === SMELL: COMMON Blocks Excesivos ===

      common /globals/ a, b, c, d, e, f, g, h
      common /moreglob/ i, j, k, l, m, n, o, p
      common /config/ tol, maxiter, debug
      common /arrays/ matriz(1000,1000), vector(1000)

      subroutine calcular()
      common /globals/ a, b, c, d, e, f, g, h
      common /arrays/ matriz(1000,1000), vector(1000)
      ! ... usa variables globales
      end

! === REFACTORIZADO: Modulos y Tipos ===

      module configuracion
        implicit none

        type :: config_t
          real(8) :: tolerancia = 1.0d-6
          integer :: max_iter = 100
          logical :: debug = .false.
        end type

        type(config_t), save :: config_global

      contains
        subroutine inicializar_config(tol, maxiter, dbg)
          real(8), intent(in) :: tol
          integer, intent(in) :: maxiter
          logical, intent(in) :: dbg

          config_global%tolerancia = tol
          config_global%max_iter = maxiter
          config_global%debug = dbg
        end subroutine
      end module

      module datos_trabajo
        implicit none

        type :: workspace_t
          real(8), allocatable :: matriz(:,:)
          real(8), allocatable :: vector(:)
        end type

      contains
        subroutine crear_workspace(ws, n)
          type(workspace_t), intent(out) :: ws
          integer, intent(in) :: n

          allocate(ws%matriz(n,n))
          allocate(ws%vector(n))
        end subroutine

        subroutine destruir_workspace(ws)
          type(workspace_t), intent(inout) :: ws
          if (allocated(ws%matriz)) deallocate(ws%matriz)
          if (allocated(ws%vector)) deallocate(ws%vector)
        end subroutine
      end module

      module calculadora
        use configuracion
        use datos_trabajo
        implicit none

      contains
        subroutine calcular(ws, resultado)
          type(workspace_t), intent(inout) :: ws
          real(8), intent(out) :: resultado

          ! Usa ws y config_global
          ! ...
        end subroutine
      end module
```

---

## 3. Refactoring COBOL

### 3.1 Extract Paragraph

```cobol
       * === ANTES: Codigo duplicado ===
       CALCULAR-IMPUESTO-A.
           IF MONTO-A > 1000
               COMPUTE IMPUESTO-A = MONTO-A * 0.21
           ELSE
               COMPUTE IMPUESTO-A = MONTO-A * 0.10
           END-IF
           ADD IMPUESTO-A TO TOTAL-IMPUESTOS.

       CALCULAR-IMPUESTO-B.
           IF MONTO-B > 1000
               COMPUTE IMPUESTO-B = MONTO-B * 0.21
           ELSE
               COMPUTE IMPUESTO-B = MONTO-B * 0.10
           END-IF
           ADD IMPUESTO-B TO TOTAL-IMPUESTOS.

       * === DESPUES: Parrafo extraido ===
       CALCULAR-IMPUESTO-A.
           MOVE MONTO-A TO WS-MONTO-CALC
           PERFORM CALCULAR-IMPUESTO-GENERICO
           MOVE WS-IMPUESTO-CALC TO IMPUESTO-A.

       CALCULAR-IMPUESTO-B.
           MOVE MONTO-B TO WS-MONTO-CALC
           PERFORM CALCULAR-IMPUESTO-GENERICO
           MOVE WS-IMPUESTO-CALC TO IMPUESTO-B.

       CALCULAR-IMPUESTO-GENERICO.
           IF WS-MONTO-CALC > 1000
               COMPUTE WS-IMPUESTO-CALC = WS-MONTO-CALC * 0.21
           ELSE
               COMPUTE WS-IMPUESTO-CALC = WS-MONTO-CALC * 0.10
           END-IF
           ADD WS-IMPUESTO-CALC TO TOTAL-IMPUESTOS.
```

### 3.2 Replace Nested IF with EVALUATE

```cobol
       * === ANTES: IFs anidados ===
       DETERMINAR-DESCUENTO.
           IF CLIENTE-TIPO = 'P'
               IF COMPRA-TOTAL > 1000
                   MOVE 0.15 TO WS-DESCUENTO
               ELSE
                   IF COMPRA-TOTAL > 500
                       MOVE 0.10 TO WS-DESCUENTO
                   ELSE
                       MOVE 0.05 TO WS-DESCUENTO
                   END-IF
               END-IF
           ELSE
               IF CLIENTE-TIPO = 'E'
                   IF COMPRA-TOTAL > 500
                       MOVE 0.20 TO WS-DESCUENTO
                   ELSE
                       MOVE 0.10 TO WS-DESCUENTO
                   END-IF
               ELSE
                   MOVE 0 TO WS-DESCUENTO
               END-IF
           END-IF.

       * === DESPUES: EVALUATE claro ===
       DETERMINAR-DESCUENTO.
           EVALUATE TRUE
               WHEN CLIENTE-TIPO = 'P' AND COMPRA-TOTAL > 1000
                   MOVE 0.15 TO WS-DESCUENTO
               WHEN CLIENTE-TIPO = 'P' AND COMPRA-TOTAL > 500
                   MOVE 0.10 TO WS-DESCUENTO
               WHEN CLIENTE-TIPO = 'P'
                   MOVE 0.05 TO WS-DESCUENTO
               WHEN CLIENTE-TIPO = 'E' AND COMPRA-TOTAL > 500
                   MOVE 0.20 TO WS-DESCUENTO
               WHEN CLIENTE-TIPO = 'E'
                   MOVE 0.10 TO WS-DESCUENTO
               WHEN OTHER
                   MOVE 0 TO WS-DESCUENTO
           END-EVALUATE.

       * === ALTERNATIVA: Tabla de descuentos ===
       01  TABLA-DESCUENTOS.
           05  FILA-DESC OCCURS 6 TIMES.
               10  TD-TIPO       PIC X.
               10  TD-MIN-COMPRA PIC 9(6).
               10  TD-DESCUENTO  PIC 9V99.

       INICIALIZAR-TABLA-DESCUENTOS.
           MOVE 'P100000015' TO FILA-DESC(1)
           MOVE 'P050000010' TO FILA-DESC(2)
           MOVE 'P000000005' TO FILA-DESC(3)
           MOVE 'E050000020' TO FILA-DESC(4)
           MOVE 'E000000010' TO FILA-DESC(5)
           MOVE ' 000000000' TO FILA-DESC(6).

       BUSCAR-DESCUENTO.
           MOVE 0 TO WS-DESCUENTO
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 6
               IF CLIENTE-TIPO = TD-TIPO(WS-IDX) AND
                  COMPRA-TOTAL >= TD-MIN-COMPRA(WS-IDX)
                   MOVE TD-DESCUENTO(WS-IDX) TO WS-DESCUENTO
                   EXIT PERFORM
               END-IF
           END-PERFORM.
```

### 3.3 Consolidar COPYs Redundantes

```cobol
       * === ANTES: COPYs duplicados y desordenados ===
       * En programa A:
       COPY CLIENTE-REC.
       COPY VALIDA-CLIENTE.
       COPY FORMATO-FECHA.

       * En programa B:
       COPY CLIENTE-REGISTRO.    *> Diferente nombre, mismo contenido
       COPY VALIDA-CLIENTE.
       COPY FORMATO-FECHA-V2.    *> Version diferente

       * === DESPUES: Biblioteca COPY unificada ===
       * copylib/CLIENTE.cpy
       01  CLIENTE-REGISTRO.
           05  CLI-ID            PIC 9(8).
           05  CLI-NOMBRE        PIC X(30).
           05  CLI-TIPO          PIC X.
               88 CLI-PREMIUM    VALUE 'P'.
               88 CLI-ESTANDAR   VALUE 'E'.
           05  CLI-SALDO         PIC S9(9)V99 COMP-3.
           05  CLI-FECHA-ALTA    PIC 9(8).

       * copylib/VALIDACIONES.cpy
       01  WS-VALIDACION.
           05  WS-VALID-OK       PIC 9 VALUE 0.
               88 VALIDACION-OK  VALUE 0.
               88 VALIDACION-ERROR VALUE 1.
           05  WS-VALID-MSG      PIC X(50).

       * copylib/FUNCIONES-FECHA.cpy
           COPY FUNCIONES-FECHA
               REPLACING ==:PREFIJO:== BY ==WS-==.
```

---

## 4. Refactoring C

### 4.1 Extract Function

```c
// === ANTES: Funcion monolitica ===
int procesar_pedido(Pedido* p) {
    // Validar pedido
    if (p == NULL) return -1;
    if (p->items == NULL || p->num_items == 0) return -2;
    for (int i = 0; i < p->num_items; i++) {
        if (p->items[i].cantidad <= 0) return -3;
        if (p->items[i].precio < 0) return -3;
    }

    // Calcular totales
    double subtotal = 0;
    for (int i = 0; i < p->num_items; i++) {
        subtotal += p->items[i].cantidad * p->items[i].precio;
    }

    // Aplicar descuentos
    double descuento = 0;
    if (p->cliente->tipo == PREMIUM) {
        if (subtotal > 1000) descuento = 0.15;
        else if (subtotal > 500) descuento = 0.10;
        else descuento = 0.05;
    }
    double total_descuento = subtotal * descuento;

    // Calcular impuestos
    double base_imponible = subtotal - total_descuento;
    double impuesto = base_imponible * 0.21;

    // Actualizar pedido
    p->subtotal = subtotal;
    p->descuento = total_descuento;
    p->impuesto = impuesto;
    p->total = base_imponible + impuesto;

    return 0;
}

// === DESPUES: Funciones extraidas ===

typedef enum {
    PEDIDO_OK = 0,
    PEDIDO_NULL = -1,
    PEDIDO_SIN_ITEMS = -2,
    PEDIDO_ITEM_INVALIDO = -3
} PedidoError;

// Validacion separada
static PedidoError validar_pedido(const Pedido* p) {
    if (p == NULL) return PEDIDO_NULL;
    if (p->items == NULL || p->num_items == 0) return PEDIDO_SIN_ITEMS;

    for (int i = 0; i < p->num_items; i++) {
        if (!item_valido(&p->items[i])) return PEDIDO_ITEM_INVALIDO;
    }
    return PEDIDO_OK;
}

static bool item_valido(const Item* item) {
    return item->cantidad > 0 && item->precio >= 0;
}

// Calculos separados
static double calcular_subtotal(const Pedido* p) {
    double subtotal = 0;
    for (int i = 0; i < p->num_items; i++) {
        subtotal += calcular_total_item(&p->items[i]);
    }
    return subtotal;
}

static double calcular_total_item(const Item* item) {
    return item->cantidad * item->precio;
}

static double obtener_porcentaje_descuento(TipoCliente tipo, double subtotal) {
    if (tipo != PREMIUM) return 0;

    if (subtotal > 1000) return 0.15;
    if (subtotal > 500) return 0.10;
    return 0.05;
}

static double calcular_impuesto(double base_imponible, double tasa) {
    return base_imponible * tasa;
}

// Funcion principal limpia
PedidoError procesar_pedido_v2(Pedido* p) {
    PedidoError error = validar_pedido(p);
    if (error != PEDIDO_OK) return error;

    p->subtotal = calcular_subtotal(p);

    double pct_descuento = obtener_porcentaje_descuento(
        p->cliente->tipo, p->subtotal);
    p->descuento = p->subtotal * pct_descuento;

    double base = p->subtotal - p->descuento;
    p->impuesto = calcular_impuesto(base, TASA_IVA);
    p->total = base + p->impuesto;

    return PEDIDO_OK;
}
```

### 4.2 Replace Error Code with Struct

```c
// === ANTES: Codigos de error y parametros out ===
int buscar_usuario(const char* nombre, Usuario* resultado) {
    if (!nombre) return -1;  // Error: nombre null
    if (!resultado) return -2;  // Error: resultado null

    // buscar...
    if (!encontrado) return -3;  // Error: no encontrado

    *resultado = usuario_encontrado;
    return 0;  // OK
}

// Uso confuso
Usuario u;
int err = buscar_usuario("juan", &u);
if (err == 0) {
    // usar u
} else if (err == -1) {
    // nombre null
} else if (err == -2) {
    // resultado null
} else {
    // no encontrado
}

// === DESPUES: Resultado estructurado ===
typedef enum {
    BUSQUEDA_OK,
    BUSQUEDA_PARAM_NULL,
    BUSQUEDA_NO_ENCONTRADO,
    BUSQUEDA_ERROR_DB
} BusquedaStatus;

typedef struct {
    BusquedaStatus status;
    Usuario* usuario;       // NULL si error
    const char* mensaje;    // Descripcion del error
} ResultadoBusqueda;

ResultadoBusqueda buscar_usuario_v2(const char* nombre) {
    ResultadoBusqueda r = {0};

    if (!nombre) {
        r.status = BUSQUEDA_PARAM_NULL;
        r.mensaje = "Nombre no puede ser NULL";
        return r;
    }

    Usuario* u = db_buscar_por_nombre(nombre);
    if (!u) {
        r.status = BUSQUEDA_NO_ENCONTRADO;
        r.mensaje = "Usuario no encontrado";
        return r;
    }

    r.status = BUSQUEDA_OK;
    r.usuario = u;
    return r;
}

// Uso claro
ResultadoBusqueda r = buscar_usuario_v2("juan");
if (r.status == BUSQUEDA_OK) {
    usar_usuario(r.usuario);
} else {
    printf("Error: %s\n", r.mensaje);
}
```

### 4.3 Replace Preprocessor Macros with Functions

```c
// === ANTES: Macros peligrosas ===
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define SQUARE(x) ((x) * (x))
#define ABS(x) ((x) < 0 ? -(x) : (x))

// Problema: efectos secundarios
int x = 5;
int y = MAX(x++, 3);  // x se incrementa dos veces!

// === DESPUES: Funciones inline ===
static inline int max_int(int a, int b) {
    return a > b ? a : b;
}

static inline int min_int(int a, int b) {
    return a < b ? a : b;
}

static inline int square_int(int x) {
    return x * x;
}

static inline int abs_int(int x) {
    return x < 0 ? -x : x;
}

// Para tipos genericos en C11:
#define max(a, b) _Generic((a), \
    int: max_int, \
    double: max_double, \
    float: max_float \
)(a, b)

// O usar funciones con sufijo de tipo
static inline double max_double(double a, double b) {
    return a > b ? a : b;
}
```

---

## 5. Refactoring Fortran

### 5.1 COMMON a Modulos

```fortran
! === ANTES: COMMON blocks ===
      program main
      common /params/ tol, maxiter
      common /data/ x(1000), y(1000), n

      real tol
      integer maxiter, n
      real x, y

      call initialize()
      call compute()
      end

      subroutine initialize()
      common /params/ tol, maxiter
      real tol
      integer maxiter

      tol = 1.0e-6
      maxiter = 100
      end

! === DESPUES: Modulos ===
module parameters
    implicit none
    real :: tolerance = 1.0e-6
    integer :: max_iterations = 100

contains
    subroutine set_parameters(tol, maxiter)
        real, intent(in) :: tol
        integer, intent(in) :: maxiter

        tolerance = tol
        max_iterations = maxiter
    end subroutine
end module

module work_data
    implicit none
    real, allocatable :: x(:), y(:)
    integer :: n = 0

contains
    subroutine allocate_data(size)
        integer, intent(in) :: size

        if (allocated(x)) deallocate(x)
        if (allocated(y)) deallocate(y)

        allocate(x(size), y(size))
        n = size
    end subroutine

    subroutine deallocate_data()
        if (allocated(x)) deallocate(x)
        if (allocated(y)) deallocate(y)
        n = 0
    end subroutine
end module

program main
    use parameters
    use work_data
    implicit none

    call set_parameters(1.0e-8, 200)
    call allocate_data(1000)

    ! ... computaciones ...

    call deallocate_data()
end program
```

### 5.2 IMPLICIT a IMPLICIT NONE

```fortran
! === ANTES: IMPLICIT permite errores ===
      subroutine calculate(x, y, result)
      real x, y, result
      real temp

      tmep = x + y        ! Error de typo: tmep en lugar de temp
      result = tmep * 2   ! Usa variable no inicializada

      return
      end

! === DESPUES: IMPLICIT NONE detecta errores ===
subroutine calculate(x, y, result)
    implicit none

    ! Declaraciones explicitas
    real, intent(in) :: x, y
    real, intent(out) :: result
    real :: temp

    temp = x + y
    result = temp * 2.0

end subroutine

! Patron moderno completo
module calculator
    implicit none
    private

    public :: calculate, calculate_array

contains

    pure function calculate(x, y) result(res)
        real, intent(in) :: x, y
        real :: res

        res = (x + y) * 2.0
    end function

    pure function calculate_array(arr) result(res)
        real, intent(in) :: arr(:)
        real :: res

        res = sum(arr) * 2.0
    end function

end module
```

### 5.3 Fixed Format a Free Format

```fortran
! === ANTES: Formato fijo (columnas 7-72) ===
      PROGRAM EJEMPLO
      IMPLICIT NONE
      INTEGER I, J, K
      REAL A(100,100), B(100,100)
C     Este es un comentario en formato fijo
C     Las lineas continuan con caracter en columna 6
      DO 10 I = 1, 100
         DO 20 J = 1, 100
            A(I,J) = REAL(I+J)
   20    CONTINUE
   10 CONTINUE
      CALL PROCESAR(A, B, 100)
      END

! === DESPUES: Formato libre moderno ===
program ejemplo
    implicit none

    integer :: i, j
    integer, parameter :: N = 100
    real :: a(N, N), b(N, N)

    ! Este es un comentario en formato libre
    ! Las lineas continuan con & al final

    do i = 1, N
        do j = 1, N
            a(i, j) = real(i + j)
        end do
    end do

    call procesar(a, b, N)

end program ejemplo

! Subrutina moderna
subroutine procesar(input, output, size)
    implicit none

    integer, intent(in) :: size
    real, intent(in) :: input(size, size)
    real, intent(out) :: output(size, size)

    ! Operacion vectorizada en lugar de loops
    output = input * 2.0 + 1.0

end subroutine procesar
```

---

## 6. Refactoring Assembly

### 6.1 Macro Expansion

```asm
; === ANTES: Codigo repetido ===
section .text

guardar_registro_1:
    push eax
    push ebx
    push ecx
    push edx
    ; ... codigo ...
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

guardar_registro_2:
    push eax
    push ebx
    push ecx
    push edx
    ; ... codigo diferente ...
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

; === DESPUES: Macros ===
%macro PUSH_REGS 0
    push eax
    push ebx
    push ecx
    push edx
%endmacro

%macro POP_REGS 0
    pop edx
    pop ecx
    pop ebx
    pop eax
%endmacro

%macro PROC_START 0
    push ebp
    mov ebp, esp
    PUSH_REGS
%endmacro

%macro PROC_END 0
    POP_REGS
    mov esp, ebp
    pop ebp
    ret
%endmacro

; Uso simplificado
guardar_registro_1:
    PROC_START
    ; ... codigo ...
    PROC_END

guardar_registro_2:
    PROC_START
    ; ... codigo diferente ...
    PROC_END
```

### 6.2 Estructurar Saltos

```asm
; === ANTES: Spaghetti de JMPs ===
procesar:
    cmp eax, 0
    jl negativo
    je cero
    cmp eax, 100
    jg grande
    ; caso normal
    jmp fin

negativo:
    ; manejar negativo
    jmp fin

cero:
    ; manejar cero
    jmp fin

grande:
    ; manejar grande
    jmp fin

fin:
    ret

; === DESPUES: Estructura clara con macros ===
%macro IF_LESS 2     ; IF_LESS valor, etiqueta
    cmp eax, %1
    jl %2
%endmacro

%macro IF_EQUAL 2
    cmp eax, %1
    je %2
%endmacro

%macro IF_GREATER 2
    cmp eax, %1
    jg %2
%endmacro

procesar:
    ; Validaciones primero, caso comun al final
    IF_LESS 0, .handle_negative
    IF_EQUAL 0, .handle_zero
    IF_GREATER 100, .handle_large

    ; Caso normal (mas comun)
    call procesar_normal
    jmp .done

.handle_negative:
    call procesar_negativo
    jmp .done

.handle_zero:
    call procesar_cero
    jmp .done

.handle_large:
    call procesar_grande

.done:
    ret

; Funciones auxiliares separadas
procesar_normal:
    ; ...
    ret

procesar_negativo:
    ; ...
    ret
```

---

## 7. Testing Durante Refactoring

### 7.1 Framework de Tests para Legacy

```python
# === Framework de testing para codigo legacy ===

import subprocess
import tempfile
import os
from typing import List, Dict, Any, Callable
from dataclasses import dataclass

@dataclass
class TestCase:
    nombre: str
    input_data: str
    expected_output: str
    descripcion: str = ""

@dataclass
class TestResult:
    nombre: str
    passed: bool
    expected: str
    actual: str
    error: str = ""

class LegacyCodeTester:
    """Tester para codigo legacy (COBOL, Fortran, etc.)"""

    def __init__(self, compilador: str, flags: List[str] = None):
        self.compilador = compilador
        self.flags = flags or []
        self.casos: List[TestCase] = []

    def agregar_caso(self, caso: TestCase):
        self.casos.append(caso)

    def ejecutar_todos(self, codigo_fuente: str) -> List[TestResult]:
        """Compilar y ejecutar todos los tests"""
        resultados = []

        # Compilar
        ejecutable = self._compilar(codigo_fuente)
        if not ejecutable:
            return [TestResult(
                "compilacion", False, "", "",
                "Error de compilacion"
            )]

        # Ejecutar cada caso
        for caso in self.casos:
            resultado = self._ejecutar_caso(ejecutable, caso)
            resultados.append(resultado)

        # Limpiar
        os.unlink(ejecutable)

        return resultados

    def _compilar(self, fuente: str) -> str:
        """Compilar codigo fuente"""
        with tempfile.NamedTemporaryFile(
            mode='w', suffix=self._get_extension(), delete=False
        ) as f:
            f.write(fuente)
            archivo_fuente = f.name

        ejecutable = archivo_fuente + '.exe'

        try:
            cmd = [self.compilador] + self.flags + [
                archivo_fuente, '-o', ejecutable
            ]
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True
            )

            if result.returncode != 0:
                print(f"Error compilacion: {result.stderr}")
                return None

            return ejecutable

        finally:
            os.unlink(archivo_fuente)

    def _ejecutar_caso(self, ejecutable: str, caso: TestCase) -> TestResult:
        """Ejecutar un caso de prueba"""
        try:
            result = subprocess.run(
                [ejecutable],
                input=caso.input_data,
                capture_output=True,
                text=True,
                timeout=30
            )

            actual = result.stdout.strip()
            expected = caso.expected_output.strip()
            passed = actual == expected

            return TestResult(
                caso.nombre,
                passed,
                expected,
                actual
            )

        except subprocess.TimeoutExpired:
            return TestResult(
                caso.nombre, False,
                caso.expected_output, "",
                "Timeout"
            )
        except Exception as e:
            return TestResult(
                caso.nombre, False,
                caso.expected_output, "",
                str(e)
            )

    def _get_extension(self) -> str:
        extensiones = {
            'gfortran': '.f90',
            'cobc': '.cob',
            'gcc': '.c'
        }
        for comp, ext in extensiones.items():
            if comp in self.compilador:
                return ext
        return '.txt'


# Ejemplo de uso para Fortran
def test_fortran_refactoring():
    tester = LegacyCodeTester('gfortran', ['-std=f2008'])

    tester.agregar_caso(TestCase(
        nombre="suma_positivos",
        input_data="5 10",
        expected_output="15",
        descripcion="Suma de dos numeros positivos"
    ))

    tester.agregar_caso(TestCase(
        nombre="suma_negativos",
        input_data="-5 -10",
        expected_output="-15",
        descripcion="Suma de dos numeros negativos"
    ))

    codigo_original = '''
    program suma
        integer :: a, b
        read(*,*) a, b
        print *, a + b
    end program
    '''

    codigo_refactorizado = '''
    module operaciones
        implicit none
    contains
        pure integer function sumar(a, b)
            integer, intent(in) :: a, b
            sumar = a + b
        end function
    end module

    program suma
        use operaciones
        implicit none
        integer :: a, b
        read(*,*) a, b
        print *, sumar(a, b)
    end program
    '''

    print("=== Probando codigo original ===")
    for r in tester.ejecutar_todos(codigo_original):
        status = "PASS" if r.passed else "FAIL"
        print(f"{status}: {r.nombre}")

    print("\n=== Probando codigo refactorizado ===")
    for r in tester.ejecutar_todos(codigo_refactorizado):
        status = "PASS" if r.passed else "FAIL"
        print(f"{status}: {r.nombre}")
```

### 7.2 Golden Master Testing

```python
# === Golden Master: Capturar comportamiento existente ===

import hashlib
import json
from pathlib import Path
from typing import Dict, List, Any

class GoldenMasterTester:
    """
    Captura output del sistema legacy como 'golden master'
    y verifica que refactoring produce mismos resultados
    """

    def __init__(self, golden_dir: str = ".golden"):
        self.golden_dir = Path(golden_dir)
        self.golden_dir.mkdir(exist_ok=True)

    def capturar(self, nombre: str, funcion, *args, **kwargs) -> Any:
        """Capturar resultado como golden master"""
        resultado = funcion(*args, **kwargs)

        golden_file = self.golden_dir / f"{nombre}.json"
        with open(golden_file, 'w') as f:
            json.dump({
                'args': str(args),
                'kwargs': str(kwargs),
                'resultado': self._serializar(resultado),
                'hash': self._calcular_hash(resultado)
            }, f, indent=2)

        return resultado

    def verificar(self, nombre: str, funcion, *args, **kwargs) -> bool:
        """Verificar que funcion produce mismo resultado que golden"""
        golden_file = self.golden_dir / f"{nombre}.json"

        if not golden_file.exists():
            raise FileNotFoundError(
                f"No existe golden master: {nombre}. "
                "Ejecute capturar() primero."
            )

        with open(golden_file, 'r') as f:
            golden = json.load(f)

        resultado = funcion(*args, **kwargs)
        hash_actual = self._calcular_hash(resultado)

        if hash_actual != golden['hash']:
            print(f"FALLO: {nombre}")
            print(f"  Esperado: {golden['resultado']}")
            print(f"  Actual: {self._serializar(resultado)}")
            return False

        return True

    def _serializar(self, obj: Any) -> Any:
        """Convertir a formato serializable"""
        if hasattr(obj, '__dict__'):
            return obj.__dict__
        elif isinstance(obj, (list, tuple)):
            return [self._serializar(x) for x in obj]
        elif isinstance(obj, dict):
            return {k: self._serializar(v) for k, v in obj.items()}
        else:
            return obj

    def _calcular_hash(self, obj: Any) -> str:
        """Calcular hash del resultado"""
        serializado = json.dumps(
            self._serializar(obj),
            sort_keys=True
        ).encode()
        return hashlib.sha256(serializado).hexdigest()


# Uso
def sistema_legacy(datos):
    """Funcion legacy que queremos refactorizar"""
    resultado = []
    for d in datos:
        if d > 0:
            resultado.append(d * 2)
        else:
            resultado.append(d)
    return resultado

def sistema_refactorizado(datos):
    """Version refactorizada"""
    return [d * 2 if d > 0 else d for d in datos]

# Capturar golden master del sistema legacy
gm = GoldenMasterTester()

casos_prueba = [
    [1, 2, 3, 4, 5],
    [-1, 0, 1],
    [],
    [100, -100, 0]
]

# Fase 1: Capturar comportamiento original
for i, caso in enumerate(casos_prueba):
    gm.capturar(f"caso_{i}", sistema_legacy, caso)

# Fase 2: Verificar refactoring
print("Verificando refactoring...")
for i, caso in enumerate(casos_prueba):
    passed = gm.verificar(f"caso_{i}", sistema_refactorizado, caso)
    print(f"Caso {i}: {'PASS' if passed else 'FAIL'}")
```

---

## 8. Modernizacion Incremental

### 8.1 Strangler Fig Pattern

```python
# === Patron Strangler Fig para migracion gradual ===

from abc import ABC, abstractmethod
from typing import Dict, Any, Optional
import logging

class SistemaLegacy(ABC):
    """Interface para sistema legacy"""

    @abstractmethod
    def procesar(self, datos: Dict[str, Any]) -> Dict[str, Any]:
        pass

class SistemaModerno(ABC):
    """Interface para sistema moderno"""

    @abstractmethod
    def procesar(self, datos: Dict[str, Any]) -> Dict[str, Any]:
        pass

class StranglerFacade:
    """
    Fachada que gradualmente redirige trafico
    del sistema legacy al moderno
    """

    def __init__(self,
                 legacy: SistemaLegacy,
                 moderno: SistemaModerno):
        self.legacy = legacy
        self.moderno = moderno
        self.funciones_migradas: Dict[str, bool] = {}
        self.logger = logging.getLogger(__name__)

    def migrar_funcion(self, nombre: str):
        """Marcar funcion como migrada"""
        self.funciones_migradas[nombre] = True
        self.logger.info(f"Funcion '{nombre}' migrada al sistema moderno")

    def revertir_funcion(self, nombre: str):
        """Revertir migracion si hay problemas"""
        self.funciones_migradas[nombre] = False
        self.logger.warning(f"Funcion '{nombre}' revertida al sistema legacy")

    def procesar(self, funcion: str, datos: Dict[str, Any]) -> Dict[str, Any]:
        """Procesar usando sistema apropiado"""

        if self.funciones_migradas.get(funcion, False):
            try:
                resultado = self.moderno.procesar(datos)
                self.logger.debug(f"'{funcion}' procesado por sistema moderno")
                return resultado
            except Exception as e:
                self.logger.error(
                    f"Error en moderno para '{funcion}': {e}. "
                    "Fallback a legacy."
                )
                # Fallback automatico
                return self.legacy.procesar(datos)
        else:
            return self.legacy.procesar(datos)


# Ejemplo de implementacion
class SistemaCobolLegacy(SistemaLegacy):
    def procesar(self, datos: Dict[str, Any]) -> Dict[str, Any]:
        # Llamar a programa COBOL via subprocess o similar
        pass

class SistemaPythonModerno(SistemaModerno):
    def procesar(self, datos: Dict[str, Any]) -> Dict[str, Any]:
        # Implementacion Python moderna
        pass

# Uso
facade = StranglerFacade(
    legacy=SistemaCobolLegacy(),
    moderno=SistemaPythonModerno()
)

# Migrar gradualmente
facade.migrar_funcion("calcular_interes")
facade.migrar_funcion("validar_cliente")

# Todo el codigo cliente usa la fachada
resultado = facade.procesar("calcular_interes", datos)
```

### 8.2 Feature Toggles para Migracion

```python
# === Feature Toggles para migracion segura ===

from enum import Enum
from typing import Dict, Callable, Any
import json
from pathlib import Path

class EstadoFeature(Enum):
    LEGACY = "legacy"
    MODERNO = "moderno"
    CANARY = "canary"  # % de trafico al moderno
    SHADOW = "shadow"  # Ambos, comparar resultados

class FeatureToggleManager:
    """Gestiona feature toggles para migracion"""

    def __init__(self, config_path: str):
        self.config_path = Path(config_path)
        self.config = self._cargar_config()
        self.metricas: Dict[str, Dict[str, int]] = {}

    def _cargar_config(self) -> Dict:
        if self.config_path.exists():
            with open(self.config_path) as f:
                return json.load(f)
        return {}

    def guardar_config(self):
        with open(self.config_path, 'w') as f:
            json.dump(self.config, f, indent=2)

    def configurar_feature(self,
                           nombre: str,
                           estado: EstadoFeature,
                           porcentaje_canary: int = 10):
        """Configurar estado de una feature"""
        self.config[nombre] = {
            'estado': estado.value,
            'porcentaje_canary': porcentaje_canary
        }
        self.guardar_config()

    def ejecutar(self,
                 nombre: str,
                 impl_legacy: Callable,
                 impl_moderno: Callable,
                 *args, **kwargs) -> Any:
        """Ejecutar con toggle"""

        feature_config = self.config.get(nombre, {})
        estado = EstadoFeature(feature_config.get('estado', 'legacy'))

        self._inicializar_metricas(nombre)

        if estado == EstadoFeature.LEGACY:
            self.metricas[nombre]['legacy'] += 1
            return impl_legacy(*args, **kwargs)

        elif estado == EstadoFeature.MODERNO:
            self.metricas[nombre]['moderno'] += 1
            return impl_moderno(*args, **kwargs)

        elif estado == EstadoFeature.CANARY:
            porcentaje = feature_config.get('porcentaje_canary', 10)
            if self._es_canary(porcentaje):
                self.metricas[nombre]['moderno'] += 1
                return impl_moderno(*args, **kwargs)
            else:
                self.metricas[nombre]['legacy'] += 1
                return impl_legacy(*args, **kwargs)

        elif estado == EstadoFeature.SHADOW:
            # Ejecutar ambos y comparar
            resultado_legacy = impl_legacy(*args, **kwargs)
            resultado_moderno = impl_moderno(*args, **kwargs)

            self._comparar_resultados(
                nombre, resultado_legacy, resultado_moderno
            )

            # Siempre retornar legacy en modo shadow
            return resultado_legacy

    def _es_canary(self, porcentaje: int) -> bool:
        import random
        return random.randint(1, 100) <= porcentaje

    def _inicializar_metricas(self, nombre: str):
        if nombre not in self.metricas:
            self.metricas[nombre] = {
                'legacy': 0,
                'moderno': 0,
                'discrepancias': 0
            }

    def _comparar_resultados(self, nombre: str, legacy: Any, moderno: Any):
        if legacy != moderno:
            self.metricas[nombre]['discrepancias'] += 1
            # Log para investigacion
            import logging
            logging.warning(
                f"Discrepancia en {nombre}: "
                f"legacy={legacy}, moderno={moderno}"
            )

    def obtener_metricas(self) -> Dict:
        return self.metricas


# Uso
toggles = FeatureToggleManager("feature_toggles.json")

# Configurar features
toggles.configurar_feature(
    "calcular_descuento",
    EstadoFeature.CANARY,
    porcentaje_canary=20  # 20% al sistema moderno
)

toggles.configurar_feature(
    "generar_reporte",
    EstadoFeature.SHADOW  # Comparar resultados
)

# En el codigo
def calcular_descuento_legacy(monto): return monto * 0.1
def calcular_descuento_moderno(monto): return monto * 0.10

resultado = toggles.ejecutar(
    "calcular_descuento",
    calcular_descuento_legacy,
    calcular_descuento_moderno,
    1000
)
```

---

## Referencias

1. Fowler, Martin - "Refactoring" (2nd Edition)
2. Feathers, Michael - "Working Effectively with Legacy Code"
3. Kerievsky, Joshua - "Refactoring to Patterns"
4. COBOL Modernization Guide - IBM Redbooks
5. Fortran Best Practices - fortran-lang.org

---

*ARCHAEON - Guardian del Codigo Ancestral*
*Refactoring - El Arte de Mejorar Sin Romper*

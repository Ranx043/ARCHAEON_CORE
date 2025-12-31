---
title: "PYTHON 04 - Puente C-Python"
version: "1.0.0"
date: "2025-12-31"
author: "ARCHAEON CORE"
classification: "ARCHAEON/80000_MODERNOS/PYTHON"
purpose: "Integracion de codigo C con Python mediante ctypes, cffi y Cython"
language_bridge: "C Legacy -> Python Modern"
keywords:
  - ctypes
  - cffi
  - cython
  - c extension
  - ffi
  - wrapper
  - memory management
  - system libraries
dependencies:
  - python >= 3.10
  - cffi >= 1.15
  - cython >= 3.0
status: "ACTIVE"
---

# PYTHON 04 - PUENTE C-PYTHON

## INDICE

1. [Introduccion al Puente C-Python](#1-introduccion-al-puente-c-python)
2. [ctypes: Llamando Bibliotecas C](#2-ctypes-llamando-bibliotecas-c)
3. [cffi: Interfaz C Moderna](#3-cffi-interfaz-c-moderna)
4. [Cython: Escribiendo Extensiones C](#4-cython-escribiendo-extensiones-c)
5. [Gestion de Memoria entre Python y C](#5-gestion-de-memoria-entre-python-y-c)
6. [Envolviendo Bibliotecas del Sistema](#6-envolviendo-bibliotecas-del-sistema)
7. [Depurando Codigo Mixto Python/C](#7-depurando-codigo-mixto-pythonc)
8. [Mejores Practicas y Patrones](#8-mejores-practicas-y-patrones)

---

## 1. INTRODUCCION AL PUENTE C-PYTHON

### 1.1 Por Que Integrar C con Python

La integracion C-Python es esencial cuando:
- Necesitas rendimiento critico en secciones especificas
- Debes usar bibliotecas existentes en C
- Interactuas con APIs del sistema operativo
- Migras codigo legacy manteniendo compatibilidad

```python
"""
ARCHAEON C-Python Bridge Overview

This module covers strategies for integrating C code
with Python applications.

Available approaches:
1. ctypes: FFI built into Python standard library
2. cffi: Modern, cleaner FFI
3. Cython: Python-like syntax compiling to C
4. CPython API: Direct Python C extension (most complex)

Each has different trade-offs in terms of:
- Ease of use
- Performance overhead
- Flexibility
- Maintenance burden
"""

from dataclasses import dataclass
from enum import Enum, auto
from typing import Optional

class CBridgeMethod(Enum):
    """Methods for bridging C and Python."""
    CTYPES = auto()      # Standard library FFI
    CFFI = auto()        # Modern FFI
    CYTHON = auto()      # Cython compiler
    CPYTHON_API = auto() # Direct C extension

@dataclass
class BridgeRecommendation:
    """Recommendation for C-Python bridging."""
    method: CBridgeMethod
    rationale: str
    complexity: str  # low, medium, high
    performance: str # excellent, good, moderate
    example_use_cases: list[str]

def recommend_bridge_method(
    has_c_source: bool,
    existing_library: bool,
    performance_critical: bool,
    python_integration_depth: str  # shallow, medium, deep
) -> BridgeRecommendation:
    """
    Recommend C-Python bridging method.

    Args:
        has_c_source: Whether C source is available
        existing_library: Whether wrapping existing compiled library
        performance_critical: Whether performance is critical
        python_integration_depth: How deeply C integrates with Python

    Returns:
        BridgeRecommendation with suggested approach
    """
    if existing_library and not has_c_source:
        # Binary library - must use FFI
        return BridgeRecommendation(
            method=CBridgeMethod.CFFI,
            rationale="Binary library requires FFI approach",
            complexity="medium",
            performance="excellent",
            example_use_cases=[
                "System libraries (libc, libm)",
                "Third-party libraries without Python bindings",
                "Hardware driver interfaces"
            ]
        )

    if has_c_source and performance_critical:
        # Source available, need performance
        return BridgeRecommendation(
            method=CBridgeMethod.CYTHON,
            rationale="Cython provides best performance with source access",
            complexity="medium",
            performance="excellent",
            example_use_cases=[
                "Numerical algorithms",
                "Data processing loops",
                "Performance hotspots"
            ]
        )

    if python_integration_depth == "shallow":
        # Simple function calls
        return BridgeRecommendation(
            method=CBridgeMethod.CTYPES,
            rationale="ctypes is simplest for basic function calls",
            complexity="low",
            performance="good",
            example_use_cases=[
                "Simple library wrappers",
                "Quick prototyping",
                "One-off integrations"
            ]
        )

    # Default to CFFI for most cases
    return BridgeRecommendation(
        method=CBridgeMethod.CFFI,
        rationale="CFFI provides good balance of features and simplicity",
        complexity="medium",
        performance="excellent",
        example_use_cases=[
            "Complex library wrappers",
            "Production applications",
            "Cross-platform code"
        ]
    )
```

### 1.2 Comparacion de Metodos

```python
"""
Comparison of C-Python bridging methods.
"""

BRIDGE_COMPARISON = """
Method       | Complexity | Performance | Dependencies | Best For
-------------|------------|-------------|--------------|----------
ctypes       | Low        | Good        | None         | Quick wrapping
cffi         | Medium     | Excellent   | cffi package | Production libs
Cython       | Medium     | Excellent   | Cython       | Performance code
CPython API  | High       | Excellent   | None         | Complex extensions

Detailed Comparison:
-------------------

ctypes:
  Pros:
    - Part of standard library (no dependencies)
    - Simple for basic function calls
    - Works with any compiled library
  Cons:
    - Verbose type declarations
    - Can be slow for many small calls
    - Error handling can be tricky

cffi:
  Pros:
    - Clean, Pythonic API
    - Can parse C header files
    - Excellent performance
    - Good error messages
  Cons:
    - Requires external package
    - Learning curve for advanced features

Cython:
  Pros:
    - Python-like syntax
    - Can optimize pure Python code
    - Excellent performance
    - Type checking at compile time
  Cons:
    - Requires compilation step
    - Debugging can be challenging
    - Build system complexity

CPython API:
  Pros:
    - Full control over Python internals
    - Best possible performance
    - Access to all Python features
  Cons:
    - Very complex
    - Must manage reference counting
    - Version-specific code
"""

print(BRIDGE_COMPARISON)
```

---

## 2. CTYPES: LLAMANDO BIBLIOTECAS C

### 2.1 Fundamentos de ctypes

```python
"""
ctypes - Python's built-in FFI for calling C libraries.

ctypes allows:
- Loading shared libraries (.dll, .so, .dylib)
- Defining C data types
- Calling C functions with proper argument/return types
- Working with C structures and arrays
"""

import ctypes
from ctypes import (
    c_int, c_long, c_float, c_double,
    c_char, c_char_p, c_void_p,
    c_uint, c_ulong, c_size_t,
    POINTER, Structure, Union,
    byref, pointer, cast,
    CDLL, CFUNCTYPE
)
import platform
import numpy as np

def load_c_library(name: str) -> ctypes.CDLL:
    """
    Load a C library in a cross-platform way.

    Args:
        name: Library name without extension

    Returns:
        Loaded CDLL object
    """
    system = platform.system()

    if system == "Windows":
        lib_name = f"{name}.dll"
    elif system == "Darwin":  # macOS
        lib_name = f"lib{name}.dylib"
    else:  # Linux and others
        lib_name = f"lib{name}.so"

    try:
        return ctypes.CDLL(lib_name)
    except OSError as e:
        # Try common paths
        for path in [f"./{lib_name}", f"/usr/lib/{lib_name}", f"/usr/local/lib/{lib_name}"]:
            try:
                return ctypes.CDLL(path)
            except OSError:
                continue
        raise e


def demonstrate_basic_ctypes():
    """Demonstrate basic ctypes usage with standard C library."""

    # Load C standard library
    if platform.system() == "Windows":
        libc = ctypes.CDLL("msvcrt")
    else:
        libc = ctypes.CDLL(None)  # Default C library

    # Simple function: strlen
    libc.strlen.argtypes = [c_char_p]
    libc.strlen.restype = c_size_t

    test_string = b"Hello, ARCHAEON!"
    length = libc.strlen(test_string)
    print(f"strlen('{test_string.decode()}'): {length}")

    # Math function: sqrt (from libm)
    libm = ctypes.CDLL("libm.so.6" if platform.system() == "Linux" else None)
    if libm:
        libm.sqrt.argtypes = [c_double]
        libm.sqrt.restype = c_double

        result = libm.sqrt(2.0)
        print(f"sqrt(2.0): {result}")


def demonstrate_ctypes_types():
    """Demonstrate ctypes type system."""

    print("\nctypes Type System")
    print("=" * 50)

    # C to ctypes type mapping
    TYPE_MAPPING = """
    C Type          | ctypes Type    | Python Type
    ----------------|----------------|------------
    char            | c_char         | bytes (1)
    char*           | c_char_p       | bytes/str
    int             | c_int          | int
    unsigned int    | c_uint         | int
    long            | c_long         | int
    unsigned long   | c_ulong        | int
    long long       | c_longlong     | int
    float           | c_float        | float
    double          | c_double       | float
    void*           | c_void_p       | int/None
    size_t          | c_size_t       | int
    """
    print(TYPE_MAPPING)

    # Creating values
    i = c_int(42)
    print(f"c_int(42).value = {i.value}")

    d = c_double(3.14159)
    print(f"c_double(3.14159).value = {d.value}")

    # Pointers
    ptr = pointer(i)
    print(f"pointer(c_int(42)).contents.value = {ptr.contents.value}")

    # Arrays
    IntArray5 = c_int * 5
    arr = IntArray5(1, 2, 3, 4, 5)
    print(f"Array elements: {[arr[i] for i in range(5)]}")
```

### 2.2 Estructuras y Tipos Complejos

```c
// File: example_lib.c
// Example C library for ctypes demonstration

#include <stdlib.h>
#include <string.h>
#include <math.h>

// Simple structure
typedef struct {
    double x;
    double y;
    double z;
} Vector3D;

// Nested structure
typedef struct {
    Vector3D position;
    Vector3D velocity;
    double mass;
    int id;
} Particle;

// Function prototypes
double vector_magnitude(Vector3D* v);
void vector_add(Vector3D* a, Vector3D* b, Vector3D* result);
double vector_dot(Vector3D* a, Vector3D* b);

Particle* create_particle(double x, double y, double z, double mass, int id);
void free_particle(Particle* p);
double particle_kinetic_energy(Particle* p);

// Array processing
void scale_array(double* arr, int n, double factor);
double sum_array(double* arr, int n);

// Implementations
double vector_magnitude(Vector3D* v) {
    return sqrt(v->x * v->x + v->y * v->y + v->z * v->z);
}

void vector_add(Vector3D* a, Vector3D* b, Vector3D* result) {
    result->x = a->x + b->x;
    result->y = a->y + b->y;
    result->z = a->z + b->z;
}

double vector_dot(Vector3D* a, Vector3D* b) {
    return a->x * b->x + a->y * b->y + a->z * b->z;
}

Particle* create_particle(double x, double y, double z, double mass, int id) {
    Particle* p = (Particle*)malloc(sizeof(Particle));
    p->position.x = x;
    p->position.y = y;
    p->position.z = z;
    p->velocity.x = 0;
    p->velocity.y = 0;
    p->velocity.z = 0;
    p->mass = mass;
    p->id = id;
    return p;
}

void free_particle(Particle* p) {
    free(p);
}

double particle_kinetic_energy(Particle* p) {
    double v_sq = p->velocity.x * p->velocity.x +
                  p->velocity.y * p->velocity.y +
                  p->velocity.z * p->velocity.z;
    return 0.5 * p->mass * v_sq;
}

void scale_array(double* arr, int n, double factor) {
    for (int i = 0; i < n; i++) {
        arr[i] *= factor;
    }
}

double sum_array(double* arr, int n) {
    double sum = 0.0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}
```

```python
"""
ctypes wrapper for example C library.
"""

import ctypes
from ctypes import Structure, POINTER, c_double, c_int
import numpy as np
from numpy.typing import NDArray

# Define C structures in Python
class Vector3D(Structure):
    """
    Python representation of C Vector3D structure.

    C equivalent:
        typedef struct {
            double x;
            double y;
            double z;
        } Vector3D;
    """
    _fields_ = [
        ("x", c_double),
        ("y", c_double),
        ("z", c_double)
    ]

    def __repr__(self) -> str:
        return f"Vector3D({self.x}, {self.y}, {self.z})"

    @classmethod
    def from_tuple(cls, t: tuple[float, float, float]) -> "Vector3D":
        """Create Vector3D from tuple."""
        return cls(t[0], t[1], t[2])

    def to_numpy(self) -> np.ndarray:
        """Convert to NumPy array."""
        return np.array([self.x, self.y, self.z], dtype=np.float64)


class Particle(Structure):
    """
    Python representation of C Particle structure.

    C equivalent:
        typedef struct {
            Vector3D position;
            Vector3D velocity;
            double mass;
            int id;
        } Particle;
    """
    _fields_ = [
        ("position", Vector3D),
        ("velocity", Vector3D),
        ("mass", c_double),
        ("id", c_int)
    ]

    def __repr__(self) -> str:
        return (f"Particle(id={self.id}, mass={self.mass}, "
                f"pos={self.position}, vel={self.velocity})")


class ExampleLibWrapper:
    """
    Python wrapper for example C library.

    Demonstrates proper ctypes patterns for library wrapping.
    """

    def __init__(self, lib_path: str = "./libexample.so"):
        """Load library and set up function signatures."""
        self._lib = ctypes.CDLL(lib_path)
        self._setup_functions()

    def _setup_functions(self) -> None:
        """Set up function argument and return types."""

        # vector_magnitude
        self._lib.vector_magnitude.argtypes = [POINTER(Vector3D)]
        self._lib.vector_magnitude.restype = c_double

        # vector_add
        self._lib.vector_add.argtypes = [
            POINTER(Vector3D),
            POINTER(Vector3D),
            POINTER(Vector3D)
        ]
        self._lib.vector_add.restype = None

        # vector_dot
        self._lib.vector_dot.argtypes = [POINTER(Vector3D), POINTER(Vector3D)]
        self._lib.vector_dot.restype = c_double

        # create_particle
        self._lib.create_particle.argtypes = [
            c_double, c_double, c_double, c_double, c_int
        ]
        self._lib.create_particle.restype = POINTER(Particle)

        # free_particle
        self._lib.free_particle.argtypes = [POINTER(Particle)]
        self._lib.free_particle.restype = None

        # particle_kinetic_energy
        self._lib.particle_kinetic_energy.argtypes = [POINTER(Particle)]
        self._lib.particle_kinetic_energy.restype = c_double

        # scale_array
        self._lib.scale_array.argtypes = [POINTER(c_double), c_int, c_double]
        self._lib.scale_array.restype = None

        # sum_array
        self._lib.sum_array.argtypes = [POINTER(c_double), c_int]
        self._lib.sum_array.restype = c_double

    def vector_magnitude(self, v: Vector3D) -> float:
        """Calculate vector magnitude."""
        return self._lib.vector_magnitude(ctypes.byref(v))

    def vector_add(self, a: Vector3D, b: Vector3D) -> Vector3D:
        """Add two vectors."""
        result = Vector3D()
        self._lib.vector_add(
            ctypes.byref(a),
            ctypes.byref(b),
            ctypes.byref(result)
        )
        return result

    def vector_dot(self, a: Vector3D, b: Vector3D) -> float:
        """Calculate dot product."""
        return self._lib.vector_dot(ctypes.byref(a), ctypes.byref(b))

    def create_particle(
        self,
        x: float, y: float, z: float,
        mass: float,
        particle_id: int
    ) -> POINTER(Particle):
        """Create a particle (returns pointer)."""
        return self._lib.create_particle(x, y, z, mass, particle_id)

    def free_particle(self, p: POINTER(Particle)) -> None:
        """Free a particle."""
        self._lib.free_particle(p)

    def scale_array(self, arr: NDArray[np.float64], factor: float) -> None:
        """Scale array in-place."""
        if not arr.flags['C_CONTIGUOUS']:
            raise ValueError("Array must be C-contiguous")

        ptr = arr.ctypes.data_as(POINTER(c_double))
        self._lib.scale_array(ptr, len(arr), factor)

    def sum_array(self, arr: NDArray[np.float64]) -> float:
        """Sum array elements."""
        if not arr.flags['C_CONTIGUOUS']:
            arr = np.ascontiguousarray(arr)

        ptr = arr.ctypes.data_as(POINTER(c_double))
        return self._lib.sum_array(ptr, len(arr))


def demonstrate_structure_usage():
    """Demonstrate using C structures with ctypes."""

    print("\nUsing C Structures with ctypes")
    print("=" * 50)

    # Create vector directly
    v1 = Vector3D(1.0, 2.0, 3.0)
    print(f"Vector v1: {v1}")

    # Access fields
    print(f"  v1.x = {v1.x}")
    print(f"  v1.y = {v1.y}")
    print(f"  v1.z = {v1.z}")

    # Create from tuple
    v2 = Vector3D.from_tuple((4.0, 5.0, 6.0))
    print(f"Vector v2: {v2}")

    # Convert to NumPy
    arr = v1.to_numpy()
    print(f"As NumPy array: {arr}")

    # Create particle
    p = Particle()
    p.position = Vector3D(1.0, 2.0, 3.0)
    p.velocity = Vector3D(0.1, 0.2, 0.3)
    p.mass = 1.5
    p.id = 42
    print(f"\nParticle: {p}")
```

### 2.3 Callbacks y Punteros a Funciones

```python
"""
ctypes Callbacks - Passing Python functions to C.

Essential for libraries that use callback patterns
(event handlers, numerical integration, etc.)
"""

import ctypes
from ctypes import CFUNCTYPE, c_double, c_int, POINTER

# Define callback type
# C: double (*callback)(double x, void* user_data)
CallbackType = CFUNCTYPE(c_double, c_double, ctypes.c_void_p)


def create_python_callback(func):
    """
    Create a ctypes callback from a Python function.

    Args:
        func: Python function with signature f(x: float) -> float

    Returns:
        ctypes callback object
    """
    def wrapper(x: float, user_data) -> float:
        return func(x)

    return CallbackType(wrapper)


# Example: Simpson's rule with callback
"""
// C code expecting callback:
double integrate(double (*f)(double, void*),
                 double a, double b,
                 int n, void* user_data);
"""

class NumericalIntegrationWrapper:
    """
    Wrapper for C numerical integration library using callbacks.
    """

    def __init__(self, lib_path: str):
        self._lib = ctypes.CDLL(lib_path)

        # Define integrate function
        self._lib.integrate.argtypes = [
            CallbackType,  # Callback function
            c_double,      # a
            c_double,      # b
            c_int,         # n
            ctypes.c_void_p  # user_data
        ]
        self._lib.integrate.restype = c_double

    def integrate(
        self,
        func,
        a: float,
        b: float,
        n: int = 100
    ) -> float:
        """
        Integrate function from a to b.

        Args:
            func: Python function f(x) -> float
            a: Lower bound
            b: Upper bound
            n: Number of intervals

        Returns:
            Integral value
        """
        # Create callback
        callback = create_python_callback(func)

        # Keep reference to prevent garbage collection
        self._current_callback = callback

        return self._lib.integrate(callback, a, b, n, None)


def demonstrate_callbacks():
    """Demonstrate callback usage."""

    print("\nCallbacks with ctypes")
    print("=" * 50)

    # Example: Using qsort from libc with Python comparison function
    libc = ctypes.CDLL(None)

    # Comparison function type: int (*)(const void*, const void*)
    CMPFUNC = CFUNCTYPE(c_int, ctypes.c_void_p, ctypes.c_void_p)

    def py_compare(a, b):
        """Python comparison function for integers."""
        a_val = ctypes.cast(a, POINTER(c_int)).contents.value
        b_val = ctypes.cast(b, POINTER(c_int)).contents.value

        if a_val < b_val:
            return -1
        elif a_val > b_val:
            return 1
        return 0

    # Create C-compatible comparison function
    cmp_func = CMPFUNC(py_compare)

    # Create array to sort
    IntArray = c_int * 10
    arr = IntArray(9, 3, 7, 1, 5, 8, 2, 6, 4, 0)
    print(f"Before qsort: {list(arr)}")

    # Call qsort
    libc.qsort(
        arr,                    # Array to sort
        len(arr),               # Number of elements
        ctypes.sizeof(c_int),   # Size of each element
        cmp_func                # Comparison function
    )

    print(f"After qsort:  {list(arr)}")
```

---

## 3. CFFI: INTERFAZ C MODERNA

### 3.1 Fundamentos de CFFI

```python
"""
CFFI - C Foreign Function Interface for Python.

CFFI provides a cleaner, more Pythonic interface compared to ctypes.
It can parse C header files directly and offers better performance.

Two modes:
1. ABI mode: Load compiled libraries (like ctypes)
2. API mode: Compile C code inline (better performance)
"""

# ABI Mode Example
def demonstrate_cffi_abi():
    """Demonstrate CFFI ABI mode (runtime library loading)."""
    from cffi import FFI

    ffi = FFI()

    # Declare C interface
    ffi.cdef("""
        double sqrt(double x);
        double sin(double x);
        double cos(double x);
        double exp(double x);
        double log(double x);
    """)

    # Load library
    libm = ffi.dlopen("m")  # libm.so on Linux

    # Use functions
    print("\nCFFI ABI Mode - Math Functions")
    print("=" * 50)
    print(f"sqrt(2) = {libm.sqrt(2.0)}")
    print(f"sin(pi/2) = {libm.sin(3.14159265/2)}")
    print(f"exp(1) = {libm.exp(1.0)}")
    print(f"log(e) = {libm.log(2.71828)}")


def demonstrate_cffi_api():
    """Demonstrate CFFI API mode (inline compilation)."""
    from cffi import FFI

    ffi = FFI()

    # Define C declarations
    ffi.cdef("""
        typedef struct {
            double x;
            double y;
            double z;
        } Point3D;

        double distance(Point3D* p1, Point3D* p2);
        void midpoint(Point3D* p1, Point3D* p2, Point3D* result);
    """)

    # Define C source code
    ffi.set_source("_geometry_cffi", """
        #include <math.h>

        typedef struct {
            double x;
            double y;
            double z;
        } Point3D;

        double distance(Point3D* p1, Point3D* p2) {
            double dx = p2->x - p1->x;
            double dy = p2->y - p1->y;
            double dz = p2->z - p1->z;
            return sqrt(dx*dx + dy*dy + dz*dz);
        }

        void midpoint(Point3D* p1, Point3D* p2, Point3D* result) {
            result->x = (p1->x + p2->x) / 2.0;
            result->y = (p1->y + p2->y) / 2.0;
            result->z = (p1->z + p2->z) / 2.0;
        }
    """, libraries=["m"])

    # Compile (in practice, do this once in setup.py)
    ffi.compile()

    # Now import and use
    from _geometry_cffi import ffi, lib

    # Create points
    p1 = ffi.new("Point3D*")
    p1.x, p1.y, p1.z = 0.0, 0.0, 0.0

    p2 = ffi.new("Point3D*")
    p2.x, p2.y, p2.z = 3.0, 4.0, 0.0

    # Calculate distance
    d = lib.distance(p1, p2)
    print(f"\nCFFI API Mode - Geometry")
    print("=" * 50)
    print(f"Distance: {d}")

    # Calculate midpoint
    mid = ffi.new("Point3D*")
    lib.midpoint(p1, p2, mid)
    print(f"Midpoint: ({mid.x}, {mid.y}, {mid.z})")
```

### 3.2 Trabajando con Arrays

```python
"""
CFFI with NumPy arrays.

CFFI provides efficient ways to pass NumPy arrays to C code.
"""

from cffi import FFI
import numpy as np
from numpy.typing import NDArray

ffi = FFI()

# Define C interface for array operations
ffi.cdef("""
    void vector_scale(double* arr, int n, double factor);
    double vector_norm(double* arr, int n);
    void matrix_multiply(double* A, double* B, double* C,
                         int m, int n, int k);
""")


class CFFIArrayWrapper:
    """
    Wrapper for C array operations using CFFI.
    """

    def __init__(self, lib_path: str):
        self._lib = ffi.dlopen(lib_path)

    def vector_scale(
        self,
        arr: NDArray[np.float64],
        factor: float
    ) -> None:
        """
        Scale array in-place: arr *= factor

        Args:
            arr: Input array (modified in-place)
            factor: Scale factor
        """
        # Ensure contiguous
        if not arr.flags['C_CONTIGUOUS']:
            raise ValueError("Array must be C-contiguous")

        # Get pointer to array data
        ptr = ffi.cast("double*", arr.ctypes.data)

        # Call C function
        self._lib.vector_scale(ptr, len(arr), factor)

    def vector_norm(self, arr: NDArray[np.float64]) -> float:
        """
        Calculate Euclidean norm of vector.

        Args:
            arr: Input vector

        Returns:
            Euclidean norm
        """
        arr = np.ascontiguousarray(arr, dtype=np.float64)
        ptr = ffi.cast("double*", arr.ctypes.data)
        return self._lib.vector_norm(ptr, len(arr))

    def matrix_multiply(
        self,
        A: NDArray[np.float64],
        B: NDArray[np.float64]
    ) -> NDArray[np.float64]:
        """
        Matrix multiplication: C = A @ B

        Args:
            A: Matrix (m x k)
            B: Matrix (k x n)

        Returns:
            Result matrix (m x n)
        """
        m, k = A.shape
        k2, n = B.shape

        if k != k2:
            raise ValueError(f"Dimension mismatch: {k} vs {k2}")

        # Ensure contiguous, row-major order
        A = np.ascontiguousarray(A, dtype=np.float64)
        B = np.ascontiguousarray(B, dtype=np.float64)
        C = np.zeros((m, n), dtype=np.float64, order='C')

        # Get pointers
        A_ptr = ffi.cast("double*", A.ctypes.data)
        B_ptr = ffi.cast("double*", B.ctypes.data)
        C_ptr = ffi.cast("double*", C.ctypes.data)

        # Call C function
        self._lib.matrix_multiply(A_ptr, B_ptr, C_ptr, m, n, k)

        return C


def demonstrate_cffi_numpy():
    """Demonstrate CFFI with NumPy arrays."""

    print("\nCFFI with NumPy Arrays")
    print("=" * 50)

    # Create test arrays
    arr = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
    print(f"Original array: {arr}")

    # Using ffi.from_buffer for direct access
    ptr = ffi.from_buffer("double[]", arr)
    print(f"Accessing via CFFI: {[ptr[i] for i in range(len(arr))]}")

    # Create array from C memory
    c_array = ffi.new("double[10]")
    for i in range(10):
        c_array[i] = i * 0.5

    # Convert to NumPy (zero-copy)
    np_from_c = np.frombuffer(ffi.buffer(c_array), dtype=np.float64)
    print(f"NumPy from C array: {np_from_c}")
```

### 3.3 CFFI Builder Pattern

```python
"""
CFFI Builder Pattern - For proper package distribution.

This pattern separates build-time and runtime code.
"""

# File: geometry_build.py
# Run this to build the extension

def build_geometry_extension():
    """Build geometry CFFI extension."""
    from cffi import FFI

    ffibuilder = FFI()

    ffibuilder.cdef("""
        // Point structure
        typedef struct {
            double x;
            double y;
            double z;
        } Point3D;

        // Triangle structure
        typedef struct {
            Point3D vertices[3];
            Point3D normal;
            double area;
        } Triangle;

        // Functions
        double point_distance(Point3D* p1, Point3D* p2);
        void point_add(Point3D* p1, Point3D* p2, Point3D* result);
        double point_dot(Point3D* p1, Point3D* p2);
        void point_cross(Point3D* p1, Point3D* p2, Point3D* result);

        Triangle* create_triangle(Point3D* p1, Point3D* p2, Point3D* p3);
        void free_triangle(Triangle* t);
        double triangle_area(Triangle* t);
        int point_in_triangle(Point3D* p, Triangle* t);
    """)

    ffibuilder.set_source(
        "_geometry",  # Module name
        """
        #include <math.h>
        #include <stdlib.h>

        typedef struct {
            double x;
            double y;
            double z;
        } Point3D;

        typedef struct {
            Point3D vertices[3];
            Point3D normal;
            double area;
        } Triangle;

        double point_distance(Point3D* p1, Point3D* p2) {
            double dx = p2->x - p1->x;
            double dy = p2->y - p1->y;
            double dz = p2->z - p1->z;
            return sqrt(dx*dx + dy*dy + dz*dz);
        }

        void point_add(Point3D* p1, Point3D* p2, Point3D* result) {
            result->x = p1->x + p2->x;
            result->y = p1->y + p2->y;
            result->z = p1->z + p2->z;
        }

        double point_dot(Point3D* p1, Point3D* p2) {
            return p1->x * p2->x + p1->y * p2->y + p1->z * p2->z;
        }

        void point_cross(Point3D* p1, Point3D* p2, Point3D* result) {
            result->x = p1->y * p2->z - p1->z * p2->y;
            result->y = p1->z * p2->x - p1->x * p2->z;
            result->z = p1->x * p2->y - p1->y * p2->x;
        }

        Triangle* create_triangle(Point3D* p1, Point3D* p2, Point3D* p3) {
            Triangle* t = (Triangle*)malloc(sizeof(Triangle));
            t->vertices[0] = *p1;
            t->vertices[1] = *p2;
            t->vertices[2] = *p3;

            // Calculate edge vectors
            Point3D e1, e2;
            e1.x = p2->x - p1->x; e1.y = p2->y - p1->y; e1.z = p2->z - p1->z;
            e2.x = p3->x - p1->x; e2.y = p3->y - p1->y; e2.z = p3->z - p1->z;

            // Normal = e1 x e2
            point_cross(&e1, &e2, &t->normal);

            // Area = 0.5 * |normal|
            t->area = 0.5 * sqrt(point_dot(&t->normal, &t->normal));

            // Normalize
            double len = sqrt(point_dot(&t->normal, &t->normal));
            if (len > 1e-10) {
                t->normal.x /= len;
                t->normal.y /= len;
                t->normal.z /= len;
            }

            return t;
        }

        void free_triangle(Triangle* t) {
            free(t);
        }

        double triangle_area(Triangle* t) {
            return t->area;
        }

        int point_in_triangle(Point3D* p, Triangle* t) {
            // Barycentric coordinate test (simplified)
            // Returns 1 if inside, 0 otherwise
            return 0; // Simplified implementation
        }
        """,
        libraries=["m"]
    )

    return ffibuilder


if __name__ == "__main__":
    ffibuilder = build_geometry_extension()
    ffibuilder.compile(verbose=True)


# File: geometry_wrapper.py
# Runtime wrapper class

class GeometryWrapper:
    """
    High-level Python wrapper for geometry library.
    """

    def __init__(self):
        from _geometry import ffi, lib
        self._ffi = ffi
        self._lib = lib

    def distance(self, p1: tuple, p2: tuple) -> float:
        """Calculate distance between two points."""
        point1 = self._ffi.new("Point3D*")
        point1.x, point1.y, point1.z = p1

        point2 = self._ffi.new("Point3D*")
        point2.x, point2.y, point2.z = p2

        return self._lib.point_distance(point1, point2)

    def create_triangle(self, p1: tuple, p2: tuple, p3: tuple):
        """Create triangle from three points."""
        points = []
        for p in [p1, p2, p3]:
            point = self._ffi.new("Point3D*")
            point.x, point.y, point.z = p
            points.append(point)

        return self._lib.create_triangle(*points)

    def triangle_area(self, triangle) -> float:
        """Get area of triangle."""
        return self._lib.triangle_area(triangle)

    def free_triangle(self, triangle) -> None:
        """Free triangle memory."""
        self._lib.free_triangle(triangle)
```

---

## 4. CYTHON: ESCRIBIENDO EXTENSIONES C

### 4.1 Fundamentos de Cython

```cython
# File: vector_ops.pyx
# Cython implementation of vector operations

"""
Cython - Write C extensions using Python-like syntax.

Cython provides:
- Static typing for C-level performance
- Easy integration with NumPy
- Direct C library access
- Gradual optimization (start Python, add types)
"""

# cython: language_level=3
# cython: boundscheck=False
# cython: wraparound=False
# cython: cdivision=True

import numpy as np
cimport numpy as np
from libc.math cimport sqrt, sin, cos, exp
from libc.stdlib cimport malloc, free

# Type declarations
ctypedef np.float64_t DTYPE_t

# Simple function with type declarations
def python_dot_product(a, b):
    """Pure Python dot product (slow)."""
    return sum(x * y for x, y in zip(a, b))


cpdef double cython_dot_product(double[:] a, double[:] b):
    """
    Cython dot product with typed memoryviews.

    Much faster than Python version due to:
    - No Python overhead in loop
    - Direct memory access
    - C-level multiplication
    """
    cdef:
        Py_ssize_t i
        Py_ssize_t n = a.shape[0]
        double result = 0.0

    for i in range(n):
        result += a[i] * b[i]

    return result


cpdef void vector_add(double[:] a, double[:] b, double[:] result) nogil:
    """
    Vector addition: result = a + b

    'nogil' allows releasing Python GIL for parallelism.
    """
    cdef Py_ssize_t i
    cdef Py_ssize_t n = a.shape[0]

    for i in range(n):
        result[i] = a[i] + b[i]


cpdef double vector_norm(double[:] a) nogil:
    """Calculate Euclidean norm."""
    cdef:
        Py_ssize_t i
        Py_ssize_t n = a.shape[0]
        double sum_sq = 0.0

    for i in range(n):
        sum_sq += a[i] * a[i]

    return sqrt(sum_sq)


# NumPy-aware function
def matrix_multiply_cython(
    np.ndarray[DTYPE_t, ndim=2] A,
    np.ndarray[DTYPE_t, ndim=2] B
):
    """
    Matrix multiplication using Cython with NumPy arrays.

    Args:
        A: Matrix (m x k)
        B: Matrix (k x n)

    Returns:
        Result matrix (m x n)
    """
    cdef:
        Py_ssize_t m = A.shape[0]
        Py_ssize_t k = A.shape[1]
        Py_ssize_t n = B.shape[1]
        Py_ssize_t i, j, l
        double temp

    # Allocate result
    cdef np.ndarray[DTYPE_t, ndim=2] C = np.zeros((m, n), dtype=np.float64)

    # Triple loop (optimized with Cython)
    for i in range(m):
        for j in range(n):
            temp = 0.0
            for l in range(k):
                temp += A[i, l] * B[l, j]
            C[i, j] = temp

    return C


# C structure in Cython
cdef struct Particle:
    double x
    double y
    double z
    double vx
    double vy
    double vz
    double mass


cdef class ParticleSystem:
    """
    Cython extension type for particle simulation.

    Extension types are like Python classes but with C-level
    performance for attribute access.
    """
    cdef:
        Particle* particles
        int n_particles
        double dt

    def __cinit__(self, int n, double dt=0.001):
        """Allocate memory (called before __init__)."""
        self.particles = <Particle*>malloc(n * sizeof(Particle))
        if self.particles == NULL:
            raise MemoryError("Failed to allocate particles")
        self.n_particles = n
        self.dt = dt

        # Initialize to zero
        cdef int i
        for i in range(n):
            self.particles[i].x = 0
            self.particles[i].y = 0
            self.particles[i].z = 0
            self.particles[i].vx = 0
            self.particles[i].vy = 0
            self.particles[i].vz = 0
            self.particles[i].mass = 1.0

    def __dealloc__(self):
        """Free memory (called during garbage collection)."""
        if self.particles != NULL:
            free(self.particles)

    cpdef void set_particle(self, int i, double x, double y, double z,
                            double vx, double vy, double vz, double mass):
        """Set particle state."""
        if i < 0 or i >= self.n_particles:
            raise IndexError(f"Particle index {i} out of range")

        self.particles[i].x = x
        self.particles[i].y = y
        self.particles[i].z = z
        self.particles[i].vx = vx
        self.particles[i].vy = vy
        self.particles[i].vz = vz
        self.particles[i].mass = mass

    cpdef void step(self) nogil:
        """Advance simulation by one time step."""
        cdef:
            int i
            Particle* p

        for i in range(self.n_particles):
            p = &self.particles[i]
            p.x += p.vx * self.dt
            p.y += p.vy * self.dt
            p.z += p.vz * self.dt

    cpdef double total_kinetic_energy(self):
        """Calculate total kinetic energy."""
        cdef:
            int i
            double ke = 0.0
            Particle* p

        for i in range(self.n_particles):
            p = &self.particles[i]
            ke += 0.5 * p.mass * (p.vx**2 + p.vy**2 + p.vz**2)

        return ke

    def get_positions(self):
        """Return positions as NumPy array."""
        cdef np.ndarray[DTYPE_t, ndim=2] pos = np.zeros(
            (self.n_particles, 3), dtype=np.float64
        )
        cdef int i

        for i in range(self.n_particles):
            pos[i, 0] = self.particles[i].x
            pos[i, 1] = self.particles[i].y
            pos[i, 2] = self.particles[i].z

        return pos
```

### 4.2 Setup y Compilacion de Cython

```python
# File: setup.py
# Build script for Cython extension

from setuptools import setup, Extension
from Cython.Build import cythonize
import numpy as np

# Define extensions
extensions = [
    Extension(
        "vector_ops",
        sources=["vector_ops.pyx"],
        include_dirs=[np.get_include()],
        extra_compile_args=["-O3", "-ffast-math"],
        extra_link_args=[],
    )
]

setup(
    name="vector_ops",
    ext_modules=cythonize(
        extensions,
        compiler_directives={
            "language_level": "3",
            "boundscheck": False,
            "wraparound": False,
            "cdivision": True,
        }
    ),
)

# Build: python setup.py build_ext --inplace
```

```python
# File: pyproject.toml alternative (modern approach)
"""
[build-system]
requires = ["setuptools>=45", "cython>=3.0", "numpy>=1.24"]
build-backend = "setuptools.build_meta"

[project]
name = "vector_ops"
version = "1.0.0"
requires-python = ">=3.10"
dependencies = ["numpy>=1.24"]

[tool.setuptools]
py-modules = []

[tool.cython]
language_level = 3
boundscheck = false
wraparound = false
"""
```

### 4.3 Parallel Cython con OpenMP

```cython
# File: parallel_ops.pyx
# Parallel Cython operations using OpenMP

# cython: language_level=3
# cython: boundscheck=False
# cython: wraparound=False

from cython.parallel import prange, parallel
import numpy as np
cimport numpy as np

ctypedef np.float64_t DTYPE_t


cpdef void parallel_vector_add(
    double[:] a,
    double[:] b,
    double[:] result
) nogil:
    """
    Parallel vector addition using OpenMP.

    Compile with: -fopenmp
    """
    cdef Py_ssize_t i
    cdef Py_ssize_t n = a.shape[0]

    # prange enables OpenMP parallelism
    for i in prange(n, nogil=True):
        result[i] = a[i] + b[i]


cpdef double parallel_dot_product(double[:] a, double[:] b):
    """
    Parallel dot product with reduction.
    """
    cdef:
        Py_ssize_t i
        Py_ssize_t n = a.shape[0]
        double result = 0.0

    # Parallel reduction
    for i in prange(n, nogil=True):
        result += a[i] * b[i]

    return result


def parallel_matrix_multiply(
    np.ndarray[DTYPE_t, ndim=2] A,
    np.ndarray[DTYPE_t, ndim=2] B
):
    """
    Parallel matrix multiplication.
    """
    cdef:
        Py_ssize_t m = A.shape[0]
        Py_ssize_t k = A.shape[1]
        Py_ssize_t n = B.shape[1]
        Py_ssize_t i, j, l
        double temp

    cdef np.ndarray[DTYPE_t, ndim=2] C = np.zeros((m, n), dtype=np.float64)

    # Parallelize outer loop
    for i in prange(m, nogil=True):
        for j in range(n):
            temp = 0.0
            for l in range(k):
                temp = temp + A[i, l] * B[l, j]
            C[i, j] = temp

    return C
```

---

## 5. GESTION DE MEMORIA ENTRE PYTHON Y C

### 5.1 Principios de Gestion de Memoria

```python
"""
Memory Management between Python and C.

Critical concepts:
1. Python uses reference counting + garbage collection
2. C uses manual malloc/free
3. Data passed to C must remain valid
4. Avoid memory leaks when bridging
"""

import ctypes
from ctypes import c_double, POINTER
import numpy as np
from typing import Any
from contextlib import contextmanager

class MemoryManager:
    """
    Helper class for managing memory in C-Python bridge.

    Tracks allocations to prevent leaks.
    """

    def __init__(self):
        self._allocations: list[Any] = []
        self._c_allocations: list[tuple] = []  # (ptr, free_func)

    def track_python_object(self, obj: Any) -> Any:
        """
        Keep reference to Python object to prevent GC.

        Use when passing Python objects to C that might be
        garbage collected before C is done with them.
        """
        self._allocations.append(obj)
        return obj

    def track_c_allocation(
        self,
        ptr: ctypes.c_void_p,
        free_func
    ) -> ctypes.c_void_p:
        """
        Track C allocation for later cleanup.

        Args:
            ptr: Pointer to allocated memory
            free_func: Function to call to free memory

        Returns:
            The pointer (for chaining)
        """
        self._c_allocations.append((ptr, free_func))
        return ptr

    def cleanup(self) -> None:
        """Free all tracked allocations."""
        # Free C allocations
        for ptr, free_func in self._c_allocations:
            if ptr:
                free_func(ptr)
        self._c_allocations.clear()

        # Release Python references
        self._allocations.clear()

    def __enter__(self) -> "MemoryManager":
        return self

    def __exit__(self, *args) -> None:
        self.cleanup()


@contextmanager
def numpy_to_c_array(arr: np.ndarray, dtype=c_double):
    """
    Context manager for safely passing NumPy array to C.

    Ensures array remains valid and contiguous during C call.

    Usage:
        with numpy_to_c_array(my_array) as ptr:
            c_function(ptr, len(my_array))
    """
    # Ensure contiguous
    arr_contig = np.ascontiguousarray(arr, dtype=np.float64)

    # Get pointer
    ptr = arr_contig.ctypes.data_as(POINTER(dtype))

    try:
        yield ptr
    finally:
        # Array goes out of scope after with block
        pass


def demonstrate_memory_safety():
    """Demonstrate safe memory handling patterns."""

    print("\nMemory Safety Patterns")
    print("=" * 50)

    # Pattern 1: Keep references alive
    libc = ctypes.CDLL(None)

    # Dangerous: string might be garbage collected
    # ptr = ctypes.c_char_p(b"test")  # Don't do this!

    # Safe: keep explicit reference
    my_string = b"Hello, World!"
    ptr = ctypes.c_char_p(my_string)  # my_string keeps data alive

    print(f"Safe string access: {ptr.value}")

    # Pattern 2: Use context manager for NumPy arrays
    arr = np.array([1.0, 2.0, 3.0, 4.0, 5.0])

    with numpy_to_c_array(arr) as c_ptr:
        # Safe to use c_ptr within this block
        print(f"Array pointer: {c_ptr}")
        print(f"First element via C: {c_ptr[0]}")

    # Pattern 3: Use MemoryManager for complex allocations
    with MemoryManager() as mm:
        # Allocate C memory (example)
        # c_buffer = libc.malloc(100)
        # mm.track_c_allocation(c_buffer, libc.free)

        # Keep Python object alive
        data = np.random.randn(1000)
        mm.track_python_object(data)

        # Use data and c_buffer...
        print(f"Data tracked, shape: {data.shape}")

    # Memory automatically cleaned up after with block
```

### 5.2 Zero-Copy Transfers

```python
"""
Zero-Copy Data Transfer between Python and C.

Avoiding data copies is crucial for performance.
"""

import numpy as np
import ctypes
from ctypes import c_double, POINTER
from typing import Tuple

def numpy_array_info(arr: np.ndarray) -> dict:
    """Get detailed array information."""
    return {
        "shape": arr.shape,
        "dtype": arr.dtype,
        "strides": arr.strides,
        "data_ptr": arr.ctypes.data,
        "c_contiguous": arr.flags['C_CONTIGUOUS'],
        "f_contiguous": arr.flags['F_CONTIGUOUS'],
        "owndata": arr.flags['OWNDATA'],
        "writeable": arr.flags['WRITEABLE'],
    }


def zero_copy_to_c(arr: np.ndarray) -> Tuple[POINTER(c_double), int]:
    """
    Get C pointer to NumPy array data without copying.

    CRITICAL: The NumPy array must stay alive while C uses the pointer!

    Returns:
        Tuple of (pointer, length)
    """
    if arr.dtype != np.float64:
        raise ValueError(f"Expected float64, got {arr.dtype}")

    if not arr.flags['C_CONTIGUOUS']:
        raise ValueError("Array must be C-contiguous for zero-copy")

    ptr = arr.ctypes.data_as(POINTER(c_double))
    return ptr, arr.size


def numpy_from_c_pointer(
    ptr: ctypes.c_void_p,
    shape: Tuple[int, ...],
    dtype=np.float64,
    copy: bool = False
) -> np.ndarray:
    """
    Create NumPy array from C pointer.

    Args:
        ptr: Pointer to C data
        shape: Array shape
        dtype: NumPy dtype
        copy: If True, copy data (safer but slower)

    Returns:
        NumPy array

    WARNING: If copy=False, the C memory must remain valid!
    """
    # Calculate total size
    size = 1
    for dim in shape:
        size *= dim

    # Create ctypes array type
    ArrayType = ctypes.c_double * size

    # Cast pointer to array
    c_array = ctypes.cast(ptr, ctypes.POINTER(ArrayType)).contents

    # Create NumPy array (view of C memory)
    np_array = np.ctypeslib.as_array(c_array).reshape(shape)

    if copy:
        return np_array.copy()
    return np_array


class SharedBuffer:
    """
    Shared buffer between Python and C.

    Allocates memory that both can access efficiently.
    """

    def __init__(self, shape: Tuple[int, ...], dtype=np.float64):
        self.shape = shape
        self.dtype = dtype

        # Allocate NumPy array
        self._array = np.zeros(shape, dtype=dtype, order='C')

        # Get C pointer
        self._c_ptr = self._array.ctypes.data_as(POINTER(c_double))

    @property
    def numpy_array(self) -> np.ndarray:
        """Get NumPy view of buffer."""
        return self._array

    @property
    def c_pointer(self) -> POINTER(c_double):
        """Get C pointer to buffer."""
        return self._c_ptr

    @property
    def size(self) -> int:
        """Total number of elements."""
        return self._array.size

    def fill(self, value: float) -> None:
        """Fill buffer with value."""
        self._array.fill(value)

    def __repr__(self) -> str:
        return f"SharedBuffer(shape={self.shape}, dtype={self.dtype})"


def demonstrate_zero_copy():
    """Demonstrate zero-copy data transfer."""

    print("\nZero-Copy Data Transfer")
    print("=" * 50)

    # Create shared buffer
    buffer = SharedBuffer((100, 100))
    print(f"Created: {buffer}")

    # Python writes to buffer
    buffer.numpy_array[:, :] = np.random.randn(100, 100)
    print(f"Filled with random data, sum: {buffer.numpy_array.sum():.4f}")

    # Get info
    info = numpy_array_info(buffer.numpy_array)
    print(f"Array info: {info}")

    # C pointer is ready to use
    print(f"C pointer: {buffer.c_pointer}")
    print(f"First element via C: {buffer.c_pointer[0]:.6f}")
    print(f"First element via NumPy: {buffer.numpy_array.flat[0]:.6f}")
```

---

## 6. ENVOLVIENDO BIBLIOTECAS DEL SISTEMA

### 6.1 Ejemplo: libc y llamadas al sistema

```python
"""
Wrapping System Libraries.

Examples of wrapping common system functionality.
"""

import ctypes
from ctypes import (
    c_int, c_long, c_char_p, c_void_p, c_size_t,
    POINTER, Structure, create_string_buffer
)
import platform

def load_libc():
    """Load C standard library."""
    system = platform.system()
    if system == "Windows":
        return ctypes.CDLL("msvcrt")
    elif system == "Darwin":
        return ctypes.CDLL("libc.dylib")
    else:
        return ctypes.CDLL("libc.so.6")


class LibCWrapper:
    """Wrapper for common libc functions."""

    def __init__(self):
        self._libc = load_libc()
        self._setup_functions()

    def _setup_functions(self):
        """Set up function signatures."""

        # Memory functions
        self._libc.malloc.argtypes = [c_size_t]
        self._libc.malloc.restype = c_void_p

        self._libc.free.argtypes = [c_void_p]
        self._libc.free.restype = None

        self._libc.memcpy.argtypes = [c_void_p, c_void_p, c_size_t]
        self._libc.memcpy.restype = c_void_p

        self._libc.memset.argtypes = [c_void_p, c_int, c_size_t]
        self._libc.memset.restype = c_void_p

        # String functions
        self._libc.strlen.argtypes = [c_char_p]
        self._libc.strlen.restype = c_size_t

        self._libc.strcpy.argtypes = [c_char_p, c_char_p]
        self._libc.strcpy.restype = c_char_p

    def malloc(self, size: int) -> c_void_p:
        """Allocate memory."""
        ptr = self._libc.malloc(size)
        if not ptr:
            raise MemoryError(f"Failed to allocate {size} bytes")
        return ptr

    def free(self, ptr: c_void_p) -> None:
        """Free memory."""
        self._libc.free(ptr)

    def memcpy(self, dest: c_void_p, src: c_void_p, n: int) -> c_void_p:
        """Copy memory."""
        return self._libc.memcpy(dest, src, n)

    def strlen(self, s: bytes) -> int:
        """Get string length."""
        return self._libc.strlen(s)


# POSIX time structure
class Timespec(Structure):
    """struct timespec from time.h"""
    _fields_ = [
        ("tv_sec", c_long),   # Seconds
        ("tv_nsec", c_long),  # Nanoseconds
    ]


class POSIXTimeWrapper:
    """Wrapper for POSIX time functions."""

    def __init__(self):
        if platform.system() == "Windows":
            raise OSError("POSIX time not available on Windows")

        self._libc = ctypes.CDLL("libc.so.6")
        self._librt = ctypes.CDLL("librt.so.1")

        # clock_gettime
        self._librt.clock_gettime.argtypes = [c_int, POINTER(Timespec)]
        self._librt.clock_gettime.restype = c_int

    def clock_gettime(self, clock_id: int = 0) -> float:
        """
        Get high-resolution time.

        Args:
            clock_id: 0=CLOCK_REALTIME, 1=CLOCK_MONOTONIC

        Returns:
            Time in seconds with nanosecond precision
        """
        ts = Timespec()
        result = self._librt.clock_gettime(clock_id, ctypes.byref(ts))
        if result != 0:
            raise OSError("clock_gettime failed")
        return ts.tv_sec + ts.tv_nsec / 1e9


def demonstrate_system_wrappers():
    """Demonstrate system library wrappers."""

    print("\nSystem Library Wrappers")
    print("=" * 50)

    libc = LibCWrapper()

    # String operations
    test_str = b"Hello, ARCHAEON!"
    length = libc.strlen(test_str)
    print(f"strlen('{test_str.decode()}'): {length}")

    # Memory operations
    size = 1024
    ptr = libc.malloc(size)
    print(f"Allocated {size} bytes at {ptr}")

    libc.free(ptr)
    print("Memory freed")

    # POSIX time (Linux only)
    if platform.system() == "Linux":
        try:
            timer = POSIXTimeWrapper()
            t1 = timer.clock_gettime(1)  # CLOCK_MONOTONIC
            import time
            time.sleep(0.1)
            t2 = timer.clock_gettime(1)
            print(f"Elapsed time (POSIX): {t2 - t1:.6f} seconds")
        except Exception as e:
            print(f"POSIX time error: {e}")
```

---

## 7. DEPURANDO CODIGO MIXTO PYTHON/C

### 7.1 Herramientas y Tecnicas de Depuracion

```python
"""
Debugging Mixed Python/C Code.

Challenges:
- Python debugger can't see into C code
- C debugger can't see Python objects
- Crashes in C can be hard to diagnose
- Memory issues may manifest far from cause
"""

import ctypes
from ctypes import c_int, c_double, POINTER, Structure
import traceback
import sys
from typing import Callable, Any
from functools import wraps

class CDebugHelper:
    """
    Helper for debugging C library calls.
    """

    def __init__(self, verbose: bool = True):
        self.verbose = verbose
        self.call_log: list[dict] = []

    def wrap_function(
        self,
        func: Callable,
        name: str,
        arg_names: list[str] = None
    ) -> Callable:
        """
        Wrap a C function with debugging.

        Args:
            func: C function to wrap
            name: Function name for logging
            arg_names: Names of arguments for logging

        Returns:
            Wrapped function
        """
        @wraps(func)
        def wrapper(*args, **kwargs):
            call_info = {
                "name": name,
                "args": args,
                "kwargs": kwargs,
                "result": None,
                "error": None,
            }

            if self.verbose:
                arg_str = ", ".join(str(a) for a in args)
                print(f"CALL: {name}({arg_str})")

            try:
                result = func(*args, **kwargs)
                call_info["result"] = result

                if self.verbose:
                    print(f"  -> {result}")

                return result

            except Exception as e:
                call_info["error"] = str(e)

                if self.verbose:
                    print(f"  ERROR: {e}")
                    traceback.print_exc()

                raise

            finally:
                self.call_log.append(call_info)

        return wrapper

    def get_call_history(self) -> list[dict]:
        """Get history of C function calls."""
        return self.call_log.copy()

    def clear_history(self) -> None:
        """Clear call history."""
        self.call_log.clear()


def catch_c_errors(func: Callable) -> Callable:
    """
    Decorator to catch and report C-level errors.

    Provides better error messages for common C issues.
    """
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)

        except ctypes.ArgumentError as e:
            # Wrong argument type passed to C function
            print(f"\nC ARGUMENT ERROR in {func.__name__}:")
            print(f"  {e}")
            print(f"  Args: {args}")
            print(f"  Check that Python types match C function signature")
            raise

        except OSError as e:
            # System error from C library
            print(f"\nC SYSTEM ERROR in {func.__name__}:")
            print(f"  {e}")
            print(f"  errno might provide more information")
            raise

        except Exception as e:
            # General error
            print(f"\nERROR in {func.__name__}: {e}")
            raise

    return wrapper


def check_array_for_c(arr, name: str = "array") -> None:
    """
    Validate array is suitable for C.

    Raises informative errors for common issues.
    """
    import numpy as np

    print(f"\nValidating {name} for C:")
    print(f"  dtype: {arr.dtype}")
    print(f"  shape: {arr.shape}")
    print(f"  C-contiguous: {arr.flags['C_CONTIGUOUS']}")
    print(f"  F-contiguous: {arr.flags['F_CONTIGUOUS']}")
    print(f"  Writeable: {arr.flags['WRITEABLE']}")

    if not arr.flags['C_CONTIGUOUS']:
        print(f"  WARNING: Array is not C-contiguous!")
        print(f"  Use: arr = np.ascontiguousarray(arr)")

    if arr.dtype != np.float64:
        print(f"  WARNING: dtype is not float64!")
        print(f"  Many C functions expect double (float64)")


def memory_check():
    """
    Check for potential memory issues.

    On Linux, can use tools like:
    - valgrind: valgrind python script.py
    - address sanitizer: compile with -fsanitize=address
    """
    import gc

    # Force garbage collection
    gc.collect()

    # Get GC statistics
    stats = gc.get_stats()
    print("\nGarbage Collector Statistics:")
    for i, gen_stats in enumerate(stats):
        print(f"  Generation {i}: {gen_stats}")

    # Check for uncollectable objects
    garbage = gc.garbage
    if garbage:
        print(f"\nWARNING: {len(garbage)} uncollectable objects!")
        for obj in garbage[:5]:  # Show first 5
            print(f"  {type(obj)}: {obj}")
```

### 7.2 Patrones de Pruebas

```python
"""
Testing Patterns for C-Python Bridge Code.
"""

import numpy as np
from typing import Callable
import pytest

def reference_implementation(
    c_func: Callable,
    py_func: Callable,
    *args,
    rtol: float = 1e-10,
    atol: float = 1e-14
) -> bool:
    """
    Test C function against Python reference.

    Args:
        c_func: C implementation
        py_func: Python reference implementation
        *args: Arguments to pass to both functions
        rtol: Relative tolerance
        atol: Absolute tolerance

    Returns:
        True if results match
    """
    c_result = c_func(*args)
    py_result = py_func(*args)

    if isinstance(c_result, np.ndarray):
        match = np.allclose(c_result, py_result, rtol=rtol, atol=atol)
        if not match:
            diff = np.abs(c_result - py_result)
            print(f"Max difference: {np.max(diff)}")
            print(f"At index: {np.unravel_index(np.argmax(diff), diff.shape)}")
        return match
    else:
        return abs(c_result - py_result) < atol + rtol * abs(py_result)


class CBridgeTestSuite:
    """
    Test suite template for C-Python bridge code.
    """

    @staticmethod
    def test_array_passing():
        """Test array can be passed to C correctly."""
        arr = np.array([1.0, 2.0, 3.0, 4.0, 5.0])

        # Ensure contiguous
        assert arr.flags['C_CONTIGUOUS']

        # Ensure correct dtype
        assert arr.dtype == np.float64

        # Get pointer
        ptr = arr.ctypes.data
        assert ptr != 0

    @staticmethod
    def test_memory_not_corrupted():
        """Test that C operations don't corrupt memory."""
        original = np.array([1.0, 2.0, 3.0])
        backup = original.copy()

        # ... perform C operations ...

        # Verify unmodified data is intact
        # (Modify this based on which data should/shouldn't change)

    @staticmethod
    def test_error_handling():
        """Test that C errors are properly caught."""
        # Test with invalid inputs
        # C function should either:
        # 1. Return error code
        # 2. Raise exception via ctypes
        # 3. Set errno
        pass

    @staticmethod
    def test_boundary_conditions():
        """Test edge cases that might cause C issues."""
        test_cases = [
            np.array([], dtype=np.float64),  # Empty array
            np.array([0.0]),                  # Single element
            np.array([np.nan]),               # NaN
            np.array([np.inf]),               # Infinity
            np.zeros(10000000),               # Large array
        ]

        for arr in test_cases:
            # Test each case
            pass
```

---

## 8. MEJORES PRACTICAS Y PATRONES

### 8.1 Guia de Mejores Practicas

```python
"""
Best Practices for C-Python Bridge Development.
"""

BEST_PRACTICES = """
ARCHAEON C-Python Bridge Best Practices
========================================

1. ALWAYS DEFINE ARGTYPES AND RESTYPE
   - Never call C functions without type information
   - Prevents crashes from type mismatches
   - Documents the interface

   # BAD
   result = lib.my_function(x, y)

   # GOOD
   lib.my_function.argtypes = [c_double, c_double]
   lib.my_function.restype = c_double
   result = lib.my_function(x, y)

2. HANDLE MEMORY CAREFULLY
   - Keep Python objects alive while C uses them
   - Free C allocations when done
   - Use context managers for cleanup

3. VALIDATE ARRAYS BEFORE PASSING
   - Check contiguity (C vs Fortran order)
   - Check dtype matches C expectation
   - Check for NULL after allocation

4. USE ZERO-COPY WHEN POSSIBLE
   - Avoid np.ascontiguousarray() if already contiguous
   - Use buffer protocol (ffi.from_buffer)
   - Share memory via SharedBuffer pattern

5. TEST THOROUGHLY
   - Test with empty arrays
   - Test with very large arrays
   - Test with special values (NaN, Inf)
   - Compare against Python reference

6. DOCUMENT OWNERSHIP
   - Who allocates? Who frees?
   - What memory order is expected?
   - What happens on error?

7. HANDLE ERRORS GRACEFULLY
   - Check return codes
   - Check errno for system calls
   - Provide meaningful error messages

8. PREFER CFFI FOR NEW CODE
   - Cleaner API than ctypes
   - Better error messages
   - Can parse headers directly

9. USE CYTHON FOR PERFORMANCE
   - When you have source access
   - For tight numerical loops
   - When you need OpenMP

10. KEEP IT SIMPLE
    - Minimize interface complexity
    - Batch operations to reduce call overhead
    - Consider if NumPy/SciPy can do it instead
"""

print(BEST_PRACTICES)
```

### 8.2 Tabla de Referencia Rapida

```python
"""
Quick Reference for C-Python Bridging.
"""

TYPE_REFERENCE = """
C Type to Python/ctypes Mapping
===============================

Integers:
  char         -> c_char      -> bytes (1)
  short        -> c_short     -> int
  int          -> c_int       -> int
  long         -> c_long      -> int
  long long    -> c_longlong  -> int
  unsigned int -> c_uint      -> int
  size_t       -> c_size_t    -> int

Floating Point:
  float        -> c_float     -> float
  double       -> c_double    -> float

Pointers:
  void*        -> c_void_p    -> int/None
  char*        -> c_char_p    -> bytes
  int*         -> POINTER(c_int)
  double*      -> POINTER(c_double)

Arrays:
  int[10]      -> c_int * 10
  double[N]    -> c_double * N
  2D array     -> (c_double * cols) * rows

NumPy Mappings:
  c_float      -> np.float32
  c_double     -> np.float64
  c_int        -> np.int32
  c_long       -> np.int64 (usually)
  c_longlong   -> np.int64
"""

COMMON_PATTERNS = """
Common Patterns
===============

1. Load Library:
   lib = ctypes.CDLL("./libmylib.so")

2. Define Function:
   lib.func.argtypes = [c_double, POINTER(c_double)]
   lib.func.restype = c_int

3. Pass NumPy Array:
   arr = np.array([1.0, 2.0, 3.0])
   ptr = arr.ctypes.data_as(POINTER(c_double))
   lib.func(ptr, len(arr))

4. Define Structure:
   class MyStruct(Structure):
       _fields_ = [("x", c_double), ("y", c_double)]

5. Pass Structure:
   s = MyStruct(1.0, 2.0)
   lib.func(ctypes.byref(s))

6. Callback:
   CALLBACK = CFUNCTYPE(c_double, c_double)
   def py_func(x): return x * 2
   lib.register_callback(CALLBACK(py_func))

7. Error Handling:
   result = lib.func(args)
   if result < 0:
       raise RuntimeError(f"C error: {result}")
"""

print(TYPE_REFERENCE)
print(COMMON_PATTERNS)
```

---

## RESUMEN

Este modulo cubre la integracion de codigo C con Python:

1. **ctypes**: Interfaz FFI estandar de Python
2. **cffi**: Alternativa moderna y mas limpia
3. **Cython**: Extensiones C con sintaxis Python
4. **Memoria**: Gestion segura entre lenguajes
5. **Sistema**: Envolviendo bibliotecas del sistema
6. **Depuracion**: Tecnicas para codigo mixto

Puntos clave para ARCHAEON:
- ctypes para prototipos rapidos y bibliotecas simples
- cffi para produccion y bibliotecas complejas
- Cython cuando se necesita maximo rendimiento
- Siempre definir tipos para funciones C
- Validar arrays antes de pasarlos a C
- Documentar propiedad de memoria claramente

---

*ARCHAEON CORE - Bridging Legacy to Modern*
*C-Python Bridge v1.0.0*

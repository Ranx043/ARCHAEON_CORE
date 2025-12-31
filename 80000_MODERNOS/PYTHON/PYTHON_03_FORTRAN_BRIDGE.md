---
title: "PYTHON 03 - Puente Fortran-Python"
version: "1.0.0"
date: "2025-12-31"
author: "ARCHAEON CORE"
classification: "ARCHAEON/80000_MODERNOS/PYTHON"
purpose: "Integracion de codigo Fortran con Python mediante f2py y wrappers"
language_bridge: "Fortran Legacy -> Python Modern"
keywords:
  - f2py
  - fortran
  - numpy
  - blas
  - lapack
  - legacy code
  - wrapper
  - interoperability
dependencies:
  - python >= 3.10
  - numpy >= 1.24
  - scipy >= 1.10
  - fortran compiler (gfortran)
status: "ACTIVE"
---

# PYTHON 03 - PUENTE FORTRAN-PYTHON

## INDICE

1. [Introduccion al Puente Fortran-Python](#1-introduccion-al-puente-fortran-python)
2. [f2py: Envolviendo Codigo Fortran](#2-f2py-envolviendo-codigo-fortran)
3. [Arrays NumPy y Memoria Fortran](#3-arrays-numpy-y-memoria-fortran)
4. [Llamando BLAS y LAPACK desde Python](#4-llamando-blas-y-lapack-desde-python)
5. [Convirtiendo Algoritmos Fortran a NumPy](#5-convirtiendo-algoritmos-fortran-a-numpy)
6. [Caso de Estudio: Migrando Codigo Numerico](#6-caso-de-estudio-migrando-codigo-numerico)
7. [Preservando Precision Numerica](#7-preservando-precision-numerica)
8. [Mejores Practicas y Depuracion](#8-mejores-practicas-y-depuracion)

---

## 1. INTRODUCCION AL PUENTE FORTRAN-PYTHON

### 1.1 Por Que Mantener Codigo Fortran

Fortran sigue siendo relevante en computacion cientifica por:
- Decadas de codigo legacy optimizado y validado
- Rendimiento excepcional en operaciones numericas
- Bibliotecas cientificas maduras (BLAS, LAPACK, FFTW)
- Optimizaciones del compilador para computacion numerica

El puente Fortran-Python permite:
- Reutilizar codigo legacy probado
- Agregar interfaces modernas a bibliotecas existentes
- Migrar gradualmente a Python manteniendo rendimiento
- Combinar lo mejor de ambos mundos

```python
"""
ARCHAEON Fortran-Python Bridge Overview

This module demonstrates strategies for integrating Fortran
code with modern Python applications.

The bridge can work in two directions:
1. Fortran -> Python: Wrap Fortran for Python access
2. Python -> Fortran: Call Fortran from Python

Key tools:
- f2py: NumPy's Fortran-to-Python interface generator
- ctypes: Python's foreign function interface
- cffi: C Foreign Function Interface (via C interface)
- iso_c_binding: Fortran 2003 C interoperability
"""

from dataclasses import dataclass
from enum import Enum, auto
from typing import Callable, Any
import numpy as np
from numpy.typing import NDArray

class BridgeStrategy(Enum):
    """Available strategies for Fortran-Python bridging."""
    F2PY = auto()          # NumPy's f2py
    CTYPES = auto()        # Python ctypes with ISO_C_BINDING
    CFFI = auto()          # CFFI with C interface
    CYTHON = auto()        # Cython wrapper
    PURE_NUMPY = auto()    # Rewrite in NumPy

@dataclass
class BridgeDecision:
    """Decision framework for choosing bridge strategy."""
    strategy: BridgeStrategy
    rationale: str
    effort: str  # low, medium, high
    performance_impact: str  # minimal, moderate, significant

def recommend_bridge_strategy(
    has_fortran_source: bool,
    uses_fortran_features: list[str],
    performance_critical: bool,
    team_fortran_expertise: bool
) -> BridgeDecision:
    """
    Recommend a bridging strategy based on project characteristics.

    Args:
        has_fortran_source: Whether source code is available
        uses_fortran_features: List of Fortran features used
        performance_critical: Whether performance is critical
        team_fortran_expertise: Whether team knows Fortran

    Returns:
        BridgeDecision with recommended strategy
    """
    # Check for complex Fortran features
    complex_features = {'common_blocks', 'equivalence', 'entry', 'alternate_returns'}
    has_complex = bool(set(uses_fortran_features) & complex_features)

    if not has_fortran_source:
        # Binary-only: must use ctypes or cffi
        return BridgeDecision(
            strategy=BridgeStrategy.CTYPES,
            rationale="Source unavailable, need binary interface",
            effort="medium",
            performance_impact="minimal"
        )

    if performance_critical and not has_complex:
        return BridgeDecision(
            strategy=BridgeStrategy.F2PY,
            rationale="Best performance with direct Fortran calls",
            effort="low",
            performance_impact="minimal"
        )

    if has_complex and team_fortran_expertise:
        return BridgeDecision(
            strategy=BridgeStrategy.CTYPES,
            rationale="Complex features require ISO_C_BINDING modernization",
            effort="high",
            performance_impact="minimal"
        )

    if not performance_critical:
        return BridgeDecision(
            strategy=BridgeStrategy.PURE_NUMPY,
            rationale="NumPy rewrite provides cleaner, maintainable code",
            effort="medium",
            performance_impact="moderate"
        )

    return BridgeDecision(
        strategy=BridgeStrategy.F2PY,
        rationale="Default recommendation for most Fortran code",
        effort="low",
        performance_impact="minimal"
    )
```

### 1.2 Comparacion de Estrategias de Puente

```python
"""
Comparison table of bridging strategies.
"""

BRIDGE_COMPARISON = """
Strategy    | Effort | Performance | Flexibility | Maintainability
------------|--------|-------------|-------------|----------------
f2py        | Low    | Excellent   | Good        | Good
ctypes      | Medium | Excellent   | High        | Medium
cffi        | Medium | Excellent   | High        | Good
Cython      | High   | Excellent   | Very High   | Medium
Pure NumPy  | Medium | Good        | Very High   | Excellent

Recommendations:
- f2py: First choice for most Fortran wrappers
- ctypes: When ISO_C_BINDING is used or binary-only
- cffi: Alternative to ctypes with cleaner API
- Cython: Complex wrappers needing custom logic
- Pure NumPy: When code is simple and performance acceptable
"""

# Feature support comparison
FEATURE_SUPPORT = """
Feature               | f2py | ctypes | cffi | Cython
----------------------|------|--------|------|-------
Simple subroutines    | Yes  | Yes    | Yes  | Yes
Functions             | Yes  | Yes    | Yes  | Yes
Assumed-shape arrays  | Yes  | No     | No   | No
Allocatable arrays    | Partial | No  | No   | Yes
Derived types         | No   | Yes*   | Yes* | Yes
Common blocks         | Yes  | No     | No   | No
Modules               | Yes  | Yes*   | Yes* | Yes
Optional arguments    | Yes  | Manual | Manual | Yes
Intent declarations   | Yes  | N/A    | N/A  | N/A

* Requires ISO_C_BINDING in Fortran source
"""

print(BRIDGE_COMPARISON)
print(FEATURE_SUPPORT)
```

---

## 2. F2PY: ENVOLVIENDO CODIGO FORTRAN

### 2.1 Basicos de f2py

f2py es la herramienta incluida con NumPy para generar wrappers
de Python para codigo Fortran.

```fortran
! File: basic_routines.f90
! Basic Fortran routines for f2py demonstration

subroutine vector_add(a, b, c, n)
    ! Add two vectors element-wise
    ! C(i) = A(i) + B(i)
    implicit none

    ! Input/output declarations with intent
    integer, intent(in) :: n
    real(8), intent(in) :: a(n), b(n)
    real(8), intent(out) :: c(n)

    ! Local variables
    integer :: i

    ! Main computation
    do i = 1, n
        c(i) = a(i) + b(i)
    end do

end subroutine vector_add


subroutine matrix_multiply(a, b, c, m, n, k)
    ! Matrix multiplication: C = A * B
    ! A is m x k, B is k x n, C is m x n
    implicit none

    integer, intent(in) :: m, n, k
    real(8), intent(in) :: a(m, k), b(k, n)
    real(8), intent(out) :: c(m, n)

    integer :: i, j, l

    c = 0.0d0

    do j = 1, n
        do l = 1, k
            do i = 1, m
                c(i, j) = c(i, j) + a(i, l) * b(l, j)
            end do
        end do
    end do

end subroutine matrix_multiply


real(8) function dot_product_custom(a, b, n)
    ! Custom dot product implementation
    implicit none

    integer, intent(in) :: n
    real(8), intent(in) :: a(n), b(n)

    integer :: i

    dot_product_custom = 0.0d0
    do i = 1, n
        dot_product_custom = dot_product_custom + a(i) * b(i)
    end do

end function dot_product_custom
```

### 2.2 Compilacion con f2py

```bash
# Compile Fortran to Python extension module

# Basic compilation
f2py -c basic_routines.f90 -m fortran_routines

# With optimization flags
f2py -c basic_routines.f90 -m fortran_routines \
    --f90flags="-O3 -march=native"

# With BLAS/LAPACK linking
f2py -c basic_routines.f90 -m fortran_routines \
    -llapack -lblas

# Generate signature file first (for customization)
f2py basic_routines.f90 -m fortran_routines -h signature.pyf

# Compile from signature file
f2py -c signature.pyf basic_routines.f90
```

```python
"""
Using f2py-generated modules in Python.
"""

import numpy as np

# Import the compiled Fortran module
# import fortran_routines as fr

def demonstrate_f2py_usage():
    """Demonstrate calling f2py-wrapped functions."""

    # Create test data
    n = 1000
    a = np.random.randn(n)
    b = np.random.randn(n)

    # Call Fortran vector addition
    # Note: f2py automatically handles intent(out) arrays
    # c = fr.vector_add(a, b, n)

    # Alternative: pre-allocate output
    # c = np.empty(n, dtype=np.float64, order='F')
    # fr.vector_add(a, b, c, n)

    # Matrix multiplication
    m, k, n = 100, 50, 75
    A = np.random.randn(m, k).astype(np.float64, order='F')
    B = np.random.randn(k, n).astype(np.float64, order='F')

    # Call Fortran - note Fortran array ordering
    # C = fr.matrix_multiply(A, B, m, n, k)

    # Dot product
    # result = fr.dot_product_custom(a, b, len(a))

    print("f2py usage demonstration")
    print("  To run: compile Fortran code first with f2py")
```

### 2.3 Signature Files y Customizacion

```python
"""
f2py Signature File Customization.

Signature files (.pyf) allow fine control over the interface.
"""

# Example signature file content
SIGNATURE_FILE = '''
!    -*- f90 -*-
! Signature file for f2py customization

python module fortran_routines
    interface
        subroutine vector_add(a, b, c, n)
            ! Customized interface for vector_add
            real(8), dimension(n), intent(in) :: a
            real(8), dimension(n), intent(in) :: b
            real(8), dimension(n), intent(out) :: c
            integer, intent(in), optional :: n = len(a)
            ! n is now optional - inferred from array size
        end subroutine vector_add

        subroutine matrix_multiply(a, b, c, m, n, k)
            ! Matrix dimensions inferred from array shapes
            real(8), dimension(m, k), intent(in) :: a
            real(8), dimension(k, n), intent(in) :: b
            real(8), dimension(m, n), intent(out) :: c
            integer, intent(in), optional :: m = shape(a, 0)
            integer, intent(in), optional :: n = shape(b, 1)
            integer, intent(in), optional :: k = shape(a, 1)
        end subroutine matrix_multiply

        function dot_product_custom(a, b, n) result(r)
            real(8), dimension(n), intent(in) :: a
            real(8), dimension(n), intent(in) :: b
            integer, intent(in), optional :: n = len(a)
            real(8) :: r
        end function dot_product_custom
    end interface
end python module fortran_routines
'''

def demonstrate_signature_customization():
    """
    Show signature file customizations.

    Common customizations:
    1. Make dimension arguments optional (inferred from arrays)
    2. Change intent declarations
    3. Add documentation strings
    4. Rename functions/subroutines
    5. Hide internal arguments
    """

    customizations = """
    Common .pyf Customizations:
    --------------------------

    1. Optional dimension arguments:
       integer, intent(in), optional :: n = len(a)

    2. Hide arguments from Python interface:
       integer, intent(hide) :: internal_flag

    3. Add callback functions:
       external :: callback_func
       real(8) :: callback_func

    4. Threadsafe declaration:
       !f2py threadsafe

    5. Add docstrings:
       !f2py note: This function computes...

    6. Rename in Python:
       subroutine old_fortran_name
       !f2py python_name = new_python_name
    """
    print(customizations)
```

### 2.4 Manejo de Tipos de Datos

```fortran
! File: data_types.f90
! Demonstration of various Fortran data types for f2py

module data_types_mod
    implicit none

    ! Kind parameters for portability
    integer, parameter :: sp = selected_real_kind(6, 37)   ! Single precision
    integer, parameter :: dp = selected_real_kind(15, 307) ! Double precision
    integer, parameter :: qp = selected_real_kind(33, 4931) ! Quad precision (if available)

    integer, parameter :: i4 = selected_int_kind(9)  ! 32-bit integer
    integer, parameter :: i8 = selected_int_kind(18) ! 64-bit integer

contains

    subroutine process_single(arr, n)
        ! Single precision array processing
        integer, intent(in) :: n
        real(sp), intent(inout) :: arr(n)

        arr = arr * 2.0_sp
    end subroutine process_single

    subroutine process_double(arr, n)
        ! Double precision array processing
        integer, intent(in) :: n
        real(dp), intent(inout) :: arr(n)

        arr = arr * 2.0_dp
    end subroutine process_double

    subroutine process_complex(arr, n)
        ! Complex array processing
        integer, intent(in) :: n
        complex(dp), intent(inout) :: arr(n)

        arr = arr * (2.0_dp, 0.0_dp)
    end subroutine process_complex

    subroutine process_integer(arr, n)
        ! Integer array processing
        integer, intent(in) :: n
        integer(i4), intent(inout) :: arr(n)

        arr = arr * 2
    end subroutine process_integer

    subroutine process_logical(arr, n, result)
        ! Logical array - count true values
        integer, intent(in) :: n
        logical, intent(in) :: arr(n)
        integer, intent(out) :: result

        result = count(arr)
    end subroutine process_logical

end module data_types_mod
```

```python
"""
Python side: handling Fortran data types.
"""

import numpy as np

# Fortran to NumPy type mapping
FORTRAN_NUMPY_TYPES = {
    # Fortran KIND -> NumPy dtype
    'REAL(4)': np.float32,
    'REAL(8)': np.float64,
    'REAL(16)': np.longdouble,  # Platform dependent
    'INTEGER(4)': np.int32,
    'INTEGER(8)': np.int64,
    'COMPLEX(4)': np.complex64,
    'COMPLEX(8)': np.complex128,
    'LOGICAL': np.bool_,
    'CHARACTER(n)': 'S{n}',  # Fixed-length string
}

def prepare_array_for_fortran(
    arr: np.ndarray,
    dtype: type = np.float64,
    copy: bool = False
) -> np.ndarray:
    """
    Prepare a NumPy array for passing to Fortran.

    Ensures:
    1. Correct data type
    2. Fortran (column-major) memory order
    3. Contiguous memory layout

    Args:
        arr: Input NumPy array
        dtype: Required data type
        copy: Force copy even if array is compatible

    Returns:
        Fortran-compatible array
    """
    # Check if already compatible
    is_compatible = (
        arr.dtype == dtype and
        arr.flags['F_CONTIGUOUS'] and
        not copy
    )

    if is_compatible:
        return arr

    # Convert to Fortran-order array with correct dtype
    return np.asfortranarray(arr, dtype=dtype)


def verify_fortran_compatibility(arr: np.ndarray) -> dict:
    """
    Check array compatibility with Fortran.

    Returns dictionary with compatibility information.
    """
    return {
        'dtype': str(arr.dtype),
        'fortran_contiguous': arr.flags['F_CONTIGUOUS'],
        'c_contiguous': arr.flags['C_CONTIGUOUS'],
        'writeable': arr.flags['WRITEABLE'],
        'aligned': arr.flags['ALIGNED'],
        'shape': arr.shape,
        'strides': arr.strides,
        'itemsize': arr.itemsize,
    }


def demonstrate_type_handling():
    """Demonstrate proper type handling for Fortran calls."""

    print("Type Handling for Fortran Interop")
    print("=" * 50)

    # Single precision
    arr_sp = np.array([1.0, 2.0, 3.0], dtype=np.float32)
    arr_sp_f = prepare_array_for_fortran(arr_sp, np.float32)
    print(f"Single precision: {verify_fortran_compatibility(arr_sp_f)}")

    # Double precision (default in most Fortran)
    arr_dp = np.array([1.0, 2.0, 3.0], dtype=np.float64)
    arr_dp_f = prepare_array_for_fortran(arr_dp, np.float64)
    print(f"Double precision: {verify_fortran_compatibility(arr_dp_f)}")

    # Complex
    arr_complex = np.array([1+2j, 3+4j], dtype=np.complex128)
    arr_complex_f = prepare_array_for_fortran(arr_complex, np.complex128)
    print(f"Complex: {verify_fortran_compatibility(arr_complex_f)}")

    # 2D array - watch memory order!
    arr_2d = np.array([[1, 2, 3], [4, 5, 6]], dtype=np.float64)
    print(f"\n2D C-order: F_CONTIGUOUS = {arr_2d.flags['F_CONTIGUOUS']}")

    arr_2d_f = np.asfortranarray(arr_2d)
    print(f"2D Fortran-order: F_CONTIGUOUS = {arr_2d_f.flags['F_CONTIGUOUS']}")
```

---

## 3. ARRAYS NUMPY Y MEMORIA FORTRAN

### 3.1 Orden de Almacenamiento

```python
"""
Memory Layout: C-order vs Fortran-order.

Understanding memory layout is CRITICAL for correct Fortran interop.

C-order (Row-major):     Fortran-order (Column-major):
arr[0,0] arr[0,1]        arr[0,0] arr[1,0]
arr[1,0] arr[1,1]        arr[0,1] arr[1,1]

In memory:               In memory:
[a00, a01, a10, a11]     [a00, a10, a01, a11]
"""

import numpy as np

def demonstrate_memory_order():
    """Show difference between C and Fortran memory ordering."""

    # Create 2D array
    arr = np.array([[1, 2, 3],
                    [4, 5, 6]], dtype=np.float64)

    print("Memory Order Demonstration")
    print("=" * 50)
    print(f"\nOriginal array:\n{arr}")
    print(f"Shape: {arr.shape} (2 rows, 3 columns)")

    # C-order (default in NumPy)
    arr_c = np.ascontiguousarray(arr)
    print(f"\nC-order (row-major):")
    print(f"  Flat view: {arr_c.ravel('C')}")
    print(f"  Strides: {arr_c.strides} bytes")
    print(f"  To get arr[i,j]: move {arr_c.strides[0]} bytes per row, {arr_c.strides[1]} per col")

    # Fortran-order
    arr_f = np.asfortranarray(arr)
    print(f"\nFortran-order (column-major):")
    print(f"  Flat view: {arr_f.ravel('F')}")
    print(f"  Strides: {arr_f.strides} bytes")
    print(f"  To get arr[i,j]: move {arr_f.strides[0]} bytes per row, {arr_f.strides[1]} per col")

    # Index correspondence
    print("\n" + "=" * 50)
    print("INDEX CORRESPONDENCE:")
    print("  NumPy arr[i, j] == Fortran arr(i+1, j+1)")
    print("  But memory layout is REVERSED!")
    print("")
    print("  NumPy C-order:       fastest index is LAST (j)")
    print("  Fortran:             fastest index is FIRST (i)")
    print("")
    print("  When passing to Fortran, the array appears TRANSPOSED")
    print("  unless using Fortran-order arrays!")


def memory_pitfalls():
    """Common pitfalls with memory ordering."""

    print("\nCommon Memory Pitfalls")
    print("=" * 50)

    # Pitfall 1: Slicing can break contiguity
    arr = np.arange(20).reshape(4, 5)
    print(f"\nPitfall 1: Slicing breaks contiguity")
    print(f"  Original F_CONTIGUOUS: {arr.flags['F_CONTIGUOUS']}")

    slice_arr = arr[::2, ::2]  # Non-contiguous!
    print(f"  After slicing [::2, ::2]: F_CONTIGUOUS = {slice_arr.flags['F_CONTIGUOUS']}")
    print(f"  C_CONTIGUOUS = {slice_arr.flags['C_CONTIGUOUS']}")

    # Fix: make copy
    slice_copy = np.asfortranarray(slice_arr)
    print(f"  After asfortranarray: F_CONTIGUOUS = {slice_copy.flags['F_CONTIGUOUS']}")

    # Pitfall 2: Operations may not preserve order
    print(f"\nPitfall 2: Operations may change order")
    a = np.asfortranarray(np.random.randn(3, 3))
    b = np.asfortranarray(np.random.randn(3, 3))
    print(f"  a is F_CONTIGUOUS: {a.flags['F_CONTIGUOUS']}")
    print(f"  b is F_CONTIGUOUS: {b.flags['F_CONTIGUOUS']}")

    c = a + b
    print(f"  a + b is F_CONTIGUOUS: {c.flags['F_CONTIGUOUS']}")
    # Note: result may or may not preserve Fortran order!

    # Pitfall 3: reshape can break order
    print(f"\nPitfall 3: reshape can break order")
    arr_f = np.asfortranarray(np.arange(12).reshape(3, 4))
    print(f"  Original: shape={arr_f.shape}, F_CONTIGUOUS={arr_f.flags['F_CONTIGUOUS']}")

    reshaped = arr_f.reshape(4, 3)  # May change order
    print(f"  Reshaped: shape={reshaped.shape}, F_CONTIGUOUS={reshaped.flags['F_CONTIGUOUS']}")

    # Fix: specify order in reshape
    reshaped_f = arr_f.reshape(4, 3, order='F')
    print(f"  Reshaped with order='F': F_CONTIGUOUS={reshaped_f.flags['F_CONTIGUOUS']}")
```

### 3.2 Pasando Arrays a Fortran

```python
"""
Correct patterns for passing arrays to Fortran.
"""

import numpy as np
from typing import Any
from numpy.typing import NDArray

class FortranArrayWrapper:
    """
    Utility class for preparing arrays for Fortran calls.

    Handles:
    - Memory order conversion
    - Type checking
    - Dimension validation
    """

    def __init__(self, dtype: type = np.float64):
        self.dtype = dtype

    def prepare(
        self,
        arr: np.ndarray,
        intent: str = 'in',
        copy: bool = False
    ) -> np.ndarray:
        """
        Prepare array for Fortran.

        Args:
            arr: Input array
            intent: 'in', 'out', or 'inout'
            copy: Force copy

        Returns:
            Fortran-compatible array
        """
        # Check dtype
        if arr.dtype != self.dtype:
            arr = arr.astype(self.dtype, order='F')

        # Ensure Fortran order
        if not arr.flags['F_CONTIGUOUS']:
            arr = np.asfortranarray(arr)
        elif copy:
            arr = arr.copy(order='F')

        # For intent(out), ensure writeable
        if intent in ('out', 'inout') and not arr.flags['WRITEABLE']:
            raise ValueError("Output array must be writeable")

        return arr

    def create_output(self, shape: tuple, fill: float = 0.0) -> np.ndarray:
        """Create properly formatted output array."""
        arr = np.empty(shape, dtype=self.dtype, order='F')
        if fill != 0.0:
            arr.fill(fill)
        return arr


def demonstrate_array_passing():
    """Demonstrate correct array passing patterns."""

    print("Array Passing Patterns")
    print("=" * 50)

    wrapper = FortranArrayWrapper(np.float64)

    # Pattern 1: Input array (intent(in))
    print("\nPattern 1: Input array (read-only)")
    input_arr = np.array([[1, 2], [3, 4]], dtype=np.float64)
    fortran_input = wrapper.prepare(input_arr, intent='in')
    print(f"  Input prepared: F_CONTIGUOUS={fortran_input.flags['F_CONTIGUOUS']}")

    # Pattern 2: Output array (intent(out))
    print("\nPattern 2: Output array (write-only)")
    output_arr = wrapper.create_output((3, 3))
    print(f"  Output created: shape={output_arr.shape}, F_CONTIGUOUS={output_arr.flags['F_CONTIGUOUS']}")

    # Pattern 3: Input/Output array (intent(inout))
    print("\nPattern 3: In/Out array (read-write)")
    inout_arr = np.array([1.0, 2.0, 3.0])
    fortran_inout = wrapper.prepare(inout_arr, intent='inout', copy=True)
    print(f"  In/Out prepared: copy of original to avoid unintended modification")

    # Pattern 4: 2D matrix for BLAS/LAPACK
    print("\nPattern 4: Matrix for BLAS/LAPACK")
    matrix = np.random.randn(100, 100)
    # BLAS/LAPACK expect column-major, double precision
    blas_matrix = wrapper.prepare(matrix)
    print(f"  Matrix prepared: dtype={blas_matrix.dtype}, F_CONTIGUOUS={blas_matrix.flags['F_CONTIGUOUS']}")
```

### 3.3 Arrays Multidimensionales

```fortran
! File: multidim_arrays.f90
! Handling multidimensional arrays

subroutine process_3d_array(arr, nx, ny, nz, result)
    ! Process a 3D array
    ! Fortran stores in column-major order:
    ! arr(1,1,1), arr(2,1,1), ... arr(nx,1,1),
    ! arr(1,2,1), arr(2,2,1), ... arr(nx,2,1),
    ! ...
    ! arr(1,1,2), arr(2,1,2), ...

    implicit none

    integer, intent(in) :: nx, ny, nz
    real(8), intent(in) :: arr(nx, ny, nz)
    real(8), intent(out) :: result

    integer :: i, j, k

    result = 0.0d0

    ! Optimal loop order for Fortran: innermost index first
    do k = 1, nz
        do j = 1, ny
            do i = 1, nx
                result = result + arr(i, j, k)
            end do
        end do
    end do

end subroutine process_3d_array


subroutine laplacian_3d(u, lap, nx, ny, nz, dx, dy, dz)
    ! Compute 3D Laplacian using finite differences
    ! lap = d2u/dx2 + d2u/dy2 + d2u/dz2

    implicit none

    integer, intent(in) :: nx, ny, nz
    real(8), intent(in) :: u(nx, ny, nz)
    real(8), intent(out) :: lap(nx, ny, nz)
    real(8), intent(in) :: dx, dy, dz

    real(8) :: dx2_inv, dy2_inv, dz2_inv
    integer :: i, j, k

    dx2_inv = 1.0d0 / (dx * dx)
    dy2_inv = 1.0d0 / (dy * dy)
    dz2_inv = 1.0d0 / (dz * dz)

    ! Initialize boundaries to zero
    lap = 0.0d0

    ! Interior points
    do k = 2, nz - 1
        do j = 2, ny - 1
            do i = 2, nx - 1
                lap(i, j, k) = &
                    (u(i+1, j, k) - 2*u(i, j, k) + u(i-1, j, k)) * dx2_inv + &
                    (u(i, j+1, k) - 2*u(i, j, k) + u(i, j-1, k)) * dy2_inv + &
                    (u(i, j, k+1) - 2*u(i, j, k) + u(i, j, k-1)) * dz2_inv
            end do
        end do
    end do

end subroutine laplacian_3d
```

```python
"""
Calling multidimensional Fortran routines from Python.
"""

import numpy as np

def prepare_3d_array_for_fortran(arr: np.ndarray) -> np.ndarray:
    """
    Prepare 3D array for Fortran.

    NumPy 3D array indexing: arr[k, j, i]
    Fortran 3D array indexing: arr(i, j, k)

    When passing to Fortran:
    - If array is C-order, Fortran sees it transposed
    - Use Fortran-order or transpose explicitly
    """
    # Ensure double precision, Fortran order
    return np.asfortranarray(arr, dtype=np.float64)


def numpy_laplacian_3d(
    u: np.ndarray,
    dx: float,
    dy: float,
    dz: float
) -> np.ndarray:
    """
    NumPy equivalent of Fortran laplacian_3d.

    This shows how to write the equivalent NumPy code
    for validation of Fortran routines.
    """
    lap = np.zeros_like(u)

    dx2_inv = 1.0 / (dx * dx)
    dy2_inv = 1.0 / (dy * dy)
    dz2_inv = 1.0 / (dz * dz)

    # Interior points using slicing
    # Note: NumPy index order is [z, y, x] if matching Fortran (i, j, k)
    lap[1:-1, 1:-1, 1:-1] = (
        (u[1:-1, 1:-1, 2:] - 2*u[1:-1, 1:-1, 1:-1] + u[1:-1, 1:-1, :-2]) * dx2_inv +
        (u[1:-1, 2:, 1:-1] - 2*u[1:-1, 1:-1, 1:-1] + u[1:-1, :-2, 1:-1]) * dy2_inv +
        (u[2:, 1:-1, 1:-1] - 2*u[1:-1, 1:-1, 1:-1] + u[:-2, 1:-1, 1:-1]) * dz2_inv
    )

    return lap


def demonstrate_3d_array_handling():
    """Demonstrate 3D array handling."""

    print("3D Array Handling")
    print("=" * 50)

    # Create test array
    nx, ny, nz = 10, 10, 10
    u = np.random.randn(nx, ny, nz)

    print(f"\nOriginal array shape: {u.shape}")
    print(f"C-contiguous: {u.flags['C_CONTIGUOUS']}")

    # Prepare for Fortran
    u_fortran = prepare_3d_array_for_fortran(u)
    print(f"\nFortran-prepared shape: {u_fortran.shape}")
    print(f"F-contiguous: {u_fortran.flags['F_CONTIGUOUS']}")

    # Compute Laplacian (NumPy version)
    dx = dy = dz = 0.1
    lap = numpy_laplacian_3d(u, dx, dy, dz)
    print(f"\nLaplacian computed: max abs value = {np.max(np.abs(lap)):.6f}")
```

---

## 4. LLAMANDO BLAS Y LAPACK DESDE PYTHON

### 4.1 Acceso Directo a BLAS

```python
"""
Direct BLAS access from Python.

BLAS (Basic Linear Algebra Subprograms) provides:
- Level 1: Vector-vector operations (DOT, AXPY, NRM2)
- Level 2: Matrix-vector operations (GEMV, TRSV)
- Level 3: Matrix-matrix operations (GEMM, TRSM)

SciPy provides low-level BLAS wrappers in scipy.linalg.blas
"""

import numpy as np
from scipy.linalg import blas

def demonstrate_blas_level1():
    """BLAS Level 1: Vector-vector operations."""

    print("BLAS Level 1: Vector-Vector Operations")
    print("=" * 50)

    n = 1000
    x = np.random.randn(n)
    y = np.random.randn(n)

    # DDOT: dot product
    # Fortran: DDOT(N, X, INCX, Y, INCY)
    dot_result = blas.ddot(x, y)
    print(f"DDOT result: {dot_result:.6f}")
    print(f"NumPy equivalent: {np.dot(x, y):.6f}")

    # DNRM2: Euclidean norm
    # Fortran: DNRM2(N, X, INCX)
    norm_result = blas.dnrm2(x)
    print(f"\nDNRM2 result: {norm_result:.6f}")
    print(f"NumPy equivalent: {np.linalg.norm(x):.6f}")

    # DAXPY: y = alpha*x + y
    # Fortran: DAXPY(N, ALPHA, X, INCX, Y, INCY)
    alpha = 2.5
    y_copy = y.copy()
    blas.daxpy(x, y_copy, a=alpha)  # y_copy is modified in-place
    print(f"\nDAXPY (y = {alpha}*x + y)")
    print(f"BLAS result[0:3]: {y_copy[:3]}")
    print(f"NumPy result[0:3]: {(alpha * x + y)[:3]}")

    # DSCAL: x = alpha * x
    x_copy = x.copy()
    blas.dscal(alpha, x_copy)  # x_copy is modified in-place
    print(f"\nDSCAL (x = {alpha}*x)")
    print(f"BLAS result[0:3]: {x_copy[:3]}")
    print(f"NumPy result[0:3]: {(alpha * x)[:3]}")


def demonstrate_blas_level2():
    """BLAS Level 2: Matrix-vector operations."""

    print("\nBLAS Level 2: Matrix-Vector Operations")
    print("=" * 50)

    m, n = 100, 50
    A = np.asfortranarray(np.random.randn(m, n))
    x = np.random.randn(n)
    y = np.random.randn(m)

    # DGEMV: y = alpha*A*x + beta*y
    # Fortran: DGEMV(TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y, INCY)
    alpha = 1.0
    beta = 0.0

    y_result = blas.dgemv(alpha, A, x, beta=beta, y=y.copy())
    print(f"DGEMV (y = A*x)")
    print(f"BLAS result[0:3]: {y_result[:3]}")
    print(f"NumPy result[0:3]: {(A @ x)[:3]}")

    # Transposed: y = alpha*A'*x + beta*y
    x_m = np.random.randn(m)
    y_n = np.random.randn(n)

    y_result_t = blas.dgemv(alpha, A, x_m, beta=beta, y=y_n.copy(), trans=1)
    print(f"\nDGEMV transposed (y = A'*x)")
    print(f"BLAS result[0:3]: {y_result_t[:3]}")
    print(f"NumPy result[0:3]: {(A.T @ x_m)[:3]}")


def demonstrate_blas_level3():
    """BLAS Level 3: Matrix-matrix operations."""

    print("\nBLAS Level 3: Matrix-Matrix Operations")
    print("=" * 50)

    m, n, k = 100, 80, 60
    A = np.asfortranarray(np.random.randn(m, k))
    B = np.asfortranarray(np.random.randn(k, n))
    C = np.asfortranarray(np.zeros((m, n)))

    # DGEMM: C = alpha*A*B + beta*C
    # Fortran: DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
    alpha = 1.0
    beta = 0.0

    C_result = blas.dgemm(alpha, A, B, beta=beta, c=C)
    print(f"DGEMM (C = A*B)")
    print(f"  Matrix dimensions: A({m}x{k}) * B({k}x{n}) = C({m}x{n})")
    print(f"  Max difference from NumPy: {np.max(np.abs(C_result - A @ B)):.2e}")

    # With accumulation: C = alpha*A*B + beta*C
    C_init = np.asfortranarray(np.random.randn(m, n))
    alpha, beta = 2.0, 0.5

    C_accum = blas.dgemm(alpha, A, B, beta=beta, c=C_init.copy())
    expected = alpha * (A @ B) + beta * C_init
    print(f"\nDGEMM with accumulation (C = {alpha}*A*B + {beta}*C)")
    print(f"  Max difference from NumPy: {np.max(np.abs(C_accum - expected)):.2e}")
```

### 4.2 Acceso Directo a LAPACK

```python
"""
Direct LAPACK access from Python.

LAPACK provides:
- Linear system solvers
- Eigenvalue/eigenvector computation
- Singular value decomposition
- Matrix factorizations (LU, Cholesky, QR)

SciPy provides low-level LAPACK wrappers in scipy.linalg.lapack
"""

import numpy as np
from scipy.linalg import lapack

def demonstrate_lapack_solvers():
    """LAPACK linear system solvers."""

    print("LAPACK Linear System Solvers")
    print("=" * 50)

    n = 100
    A = np.asfortranarray(np.random.randn(n, n) + n * np.eye(n))
    b = np.random.randn(n)

    # DGESV: General linear solve (LU factorization)
    # Fortran: DGESV(N, NRHS, A, LDA, IPIV, B, LDB, INFO)
    lu, piv, x, info = lapack.dgesv(A, b.reshape(-1, 1))
    print(f"DGESV (general solver):")
    print(f"  INFO = {info} (0 = success)")
    print(f"  Residual: {np.linalg.norm(A @ x.flatten() - b):.2e}")

    # DPOSV: Positive definite solve (Cholesky)
    # Create SPD matrix
    A_spd = A @ A.T + n * np.eye(n)
    A_spd = np.asfortranarray(A_spd)

    c, x_spd, info = lapack.dposv(A_spd, b.reshape(-1, 1))
    print(f"\nDPOSV (SPD solver via Cholesky):")
    print(f"  INFO = {info} (0 = success)")
    print(f"  Residual: {np.linalg.norm(A_spd @ x_spd.flatten() - b):.2e}")


def demonstrate_lapack_factorizations():
    """LAPACK matrix factorizations."""

    print("\nLAPACK Matrix Factorizations")
    print("=" * 50)

    n = 50
    A = np.asfortranarray(np.random.randn(n, n))

    # DGETRF: LU factorization
    # Fortran: DGETRF(M, N, A, LDA, IPIV, INFO)
    lu, piv, info = lapack.dgetrf(A)
    print(f"DGETRF (LU factorization):")
    print(f"  INFO = {info} (0 = success)")
    print(f"  Pivot indices shape: {piv.shape}")

    # DPOTRF: Cholesky factorization (SPD)
    A_spd = A @ A.T + n * np.eye(n)
    A_spd = np.asfortranarray(A_spd)

    c, info = lapack.dpotrf(A_spd)
    print(f"\nDPOTRF (Cholesky factorization):")
    print(f"  INFO = {info} (0 = success)")
    # Lower triangle contains L such that A = L*L'

    # DGEQRF: QR factorization
    A_qr = np.asfortranarray(np.random.randn(100, 50))
    qr, tau, work, info = lapack.dgeqrf(A_qr)
    print(f"\nDGEQRF (QR factorization):")
    print(f"  INFO = {info} (0 = success)")
    print(f"  Tau (elementary reflectors) shape: {tau.shape}")


def demonstrate_lapack_eigenvalue():
    """LAPACK eigenvalue computations."""

    print("\nLAPACK Eigenvalue Computations")
    print("=" * 50)

    n = 50

    # Symmetric eigenvalue problem
    A_sym = np.random.randn(n, n)
    A_sym = (A_sym + A_sym.T) / 2  # Symmetrize
    A_sym = np.asfortranarray(A_sym)

    # DSYEV: Symmetric eigenvalue (all eigenvalues and eigenvectors)
    # Fortran: DSYEV(JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO)
    w, v, info = lapack.dsyev(A_sym)
    print(f"DSYEV (symmetric eigenvalue):")
    print(f"  INFO = {info} (0 = success)")
    print(f"  Eigenvalues range: [{w.min():.4f}, {w.max():.4f}]")

    # Verify: A*V = V*Lambda
    Lambda = np.diag(w)
    residual = np.linalg.norm(A_sym @ v - v @ Lambda)
    print(f"  Reconstruction residual: {residual:.2e}")

    # General eigenvalue problem
    A_gen = np.asfortranarray(np.random.randn(n, n))

    # DGEEV: General eigenvalue
    wr, wi, vl, vr, info = lapack.dgeev(A_gen)
    print(f"\nDGEEV (general eigenvalue):")
    print(f"  INFO = {info} (0 = success)")
    print(f"  Number of complex eigenvalues: {np.sum(wi != 0)}")
    eigenvalues = wr + 1j * wi
    print(f"  Largest eigenvalue magnitude: {np.max(np.abs(eigenvalues)):.4f}")
```

---

## 5. CONVIRTIENDO ALGORITMOS FORTRAN A NUMPY

### 5.1 Patrones de Conversion Comunes

```python
"""
Common Patterns for Converting Fortran to NumPy.

Key principles:
1. Replace DO loops with vectorized operations
2. Use broadcasting instead of nested loops
3. Use NumPy/SciPy functions for standard operations
4. Maintain numerical precision
"""

import numpy as np
from typing import Tuple

# Pattern 1: Simple DO loop -> Vectorized operation
def fortran_loop_example():
    """
    Fortran:
        DO i = 1, N
            C(i) = A(i) + B(i)
        END DO

    NumPy:
        c = a + b
    """
    n = 1000
    a = np.random.randn(n)
    b = np.random.randn(n)
    c = a + b  # Vectorized - no loop needed!
    return c


# Pattern 2: Nested loops -> Broadcasting
def fortran_nested_to_broadcast():
    """
    Fortran (outer product):
        DO i = 1, M
            DO j = 1, N
                C(i, j) = A(i) * B(j)
            END DO
        END DO

    NumPy:
        C = A[:, np.newaxis] * B[np.newaxis, :]
        # or simply:
        C = np.outer(A, B)
    """
    m, n = 100, 80
    a = np.random.randn(m)
    b = np.random.randn(n)

    # Using broadcasting
    c = a[:, np.newaxis] * b[np.newaxis, :]

    # Or using np.outer
    c_outer = np.outer(a, b)

    assert np.allclose(c, c_outer)
    return c


# Pattern 3: Conditional operations -> np.where
def fortran_where_to_numpy():
    """
    Fortran:
        WHERE (A > 0)
            B = A
        ELSEWHERE
            B = 0
        END WHERE

    NumPy:
        B = np.where(A > 0, A, 0)
    """
    a = np.random.randn(100)
    b = np.where(a > 0, a, 0)
    return b


# Pattern 4: Reduction operations
def fortran_reduction_to_numpy():
    """
    Fortran:
        total = 0.0D0
        DO i = 1, N
            total = total + A(i)
        END DO

    NumPy:
        total = np.sum(A)
    """
    a = np.random.randn(1000)
    total = np.sum(a)  # Much faster!
    return total


# Pattern 5: Matrix operations
def fortran_matmul_to_numpy():
    """
    Fortran:
        DO i = 1, M
            DO j = 1, N
                C(i, j) = 0.0D0
                DO k = 1, K
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                END DO
            END DO
        END DO

    NumPy:
        C = A @ B  # or np.matmul(A, B)
    """
    m, k, n = 100, 80, 60
    a = np.random.randn(m, k)
    b = np.random.randn(k, n)
    c = a @ b
    return c


# Pattern 6: Tridiagonal solver
def fortran_tridiag_to_scipy():
    """
    Fortran Thomas algorithm (tridiagonal solver):
        ! Forward elimination
        DO i = 2, N
            w = a(i) / b(i-1)
            b(i) = b(i) - w * c(i-1)
            d(i) = d(i) - w * d(i-1)
        END DO

        ! Back substitution
        x(N) = d(N) / b(N)
        DO i = N-1, 1, -1
            x(i) = (d(i) - c(i) * x(i+1)) / b(i)
        END DO

    NumPy/SciPy:
        from scipy.linalg import solve_banded
        # Or use scipy.sparse.linalg.spsolve
    """
    from scipy.linalg import solve_banded

    n = 100
    # Tridiagonal matrix: a (lower), b (main), c (upper)
    a = -1 * np.ones(n - 1)  # Lower diagonal
    b = 4 * np.ones(n)       # Main diagonal
    c = -1 * np.ones(n - 1)  # Upper diagonal
    d = np.ones(n)           # RHS

    # Pack into banded form
    ab = np.zeros((3, n))
    ab[0, 1:] = c    # Upper diagonal
    ab[1, :] = b     # Main diagonal
    ab[2, :-1] = a   # Lower diagonal

    x = solve_banded((1, 1), ab, d)
    return x
```

### 5.2 Ejemplo: Conversion de Integracion Numerica

```fortran
! File: integration.f90
! Fortran numerical integration routines

subroutine simpson(f, a, b, n, result)
    ! Simpson's rule integration
    ! Integrates f from a to b using n intervals (n must be even)

    implicit none

    external :: f
    real(8), intent(in) :: a, b
    integer, intent(in) :: n
    real(8), intent(out) :: result

    real(8) :: f  ! Function type
    real(8) :: h, x
    integer :: i

    if (mod(n, 2) /= 0) then
        result = 0.0d0
        return
    end if

    h = (b - a) / n

    result = f(a) + f(b)

    ! Odd terms (coefficient 4)
    do i = 1, n - 1, 2
        x = a + i * h
        result = result + 4.0d0 * f(x)
    end do

    ! Even terms (coefficient 2)
    do i = 2, n - 2, 2
        x = a + i * h
        result = result + 2.0d0 * f(x)
    end do

    result = result * h / 3.0d0

end subroutine simpson
```

```python
"""
NumPy conversion of Simpson's rule integration.
"""

import numpy as np
from typing import Callable
from dataclasses import dataclass

@dataclass
class IntegrationResult:
    """Result of numerical integration."""
    value: float
    error_estimate: float
    n_intervals: int

def simpson_fortran_style(
    f: Callable[[float], float],
    a: float,
    b: float,
    n: int
) -> float:
    """
    Direct translation of Fortran Simpson's rule.
    Uses explicit loops (slow but clear mapping).
    """
    if n % 2 != 0:
        raise ValueError("n must be even")

    h = (b - a) / n
    result = f(a) + f(b)

    # Odd terms (coefficient 4)
    for i in range(1, n, 2):
        x = a + i * h
        result += 4.0 * f(x)

    # Even terms (coefficient 2)
    for i in range(2, n - 1, 2):
        x = a + i * h
        result += 2.0 * f(x)

    return result * h / 3.0


def simpson_vectorized(
    f: Callable[[np.ndarray], np.ndarray],
    a: float,
    b: float,
    n: int
) -> float:
    """
    Vectorized Simpson's rule - Pythonic version.
    Function must accept and return arrays.
    """
    if n % 2 != 0:
        raise ValueError("n must be even")

    x = np.linspace(a, b, n + 1)
    y = f(x)
    h = (b - a) / n

    # Simpson's coefficients: 1, 4, 2, 4, 2, ..., 4, 1
    coeffs = np.ones(n + 1)
    coeffs[1:-1:2] = 4  # Odd indices
    coeffs[2:-1:2] = 2  # Even indices

    return np.sum(coeffs * y) * h / 3.0


def simpson_scipy(
    f: Callable[[np.ndarray], np.ndarray],
    a: float,
    b: float,
    n: int
) -> float:
    """Use SciPy's Simpson integration."""
    from scipy.integrate import simpson

    x = np.linspace(a, b, n + 1)
    y = f(x)
    return simpson(y, x=x)


def compare_implementations():
    """Compare different Simpson implementations."""

    print("Simpson's Rule Implementation Comparison")
    print("=" * 50)

    # Test function: sin(x)
    def f_scalar(x: float) -> float:
        return np.sin(x)

    def f_vector(x: np.ndarray) -> np.ndarray:
        return np.sin(x)

    a, b = 0, np.pi
    n = 100
    exact = 2.0  # Integral of sin from 0 to pi

    # Fortran-style (loop)
    result_loop = simpson_fortran_style(f_scalar, a, b, n)
    print(f"Fortran-style (loop): {result_loop:.10f}, error: {abs(result_loop - exact):.2e}")

    # Vectorized
    result_vec = simpson_vectorized(f_vector, a, b, n)
    print(f"Vectorized:           {result_vec:.10f}, error: {abs(result_vec - exact):.2e}")

    # SciPy
    result_scipy = simpson_scipy(f_vector, a, b, n)
    print(f"SciPy:                {result_scipy:.10f}, error: {abs(result_scipy - exact):.2e}")

    # Benchmark
    import time

    n_large = 10000
    iterations = 100

    start = time.perf_counter()
    for _ in range(iterations):
        simpson_fortran_style(f_scalar, a, b, n_large)
    time_loop = (time.perf_counter() - start) / iterations

    start = time.perf_counter()
    for _ in range(iterations):
        simpson_vectorized(f_vector, a, b, n_large)
    time_vec = (time.perf_counter() - start) / iterations

    print(f"\nPerformance (n={n_large}, {iterations} iterations):")
    print(f"  Loop:       {time_loop*1000:.3f} ms")
    print(f"  Vectorized: {time_vec*1000:.3f} ms")
    print(f"  Speedup:    {time_loop/time_vec:.1f}x")
```

---

## 6. CASO DE ESTUDIO: MIGRANDO CODIGO NUMERICO

### 6.1 Codigo Legacy: Solucion de EDO

```fortran
! File: ode_solver.f90
! Legacy Fortran ODE solver (RK4)

module ode_module
    implicit none

    integer, parameter :: dp = selected_real_kind(15, 307)

contains

    subroutine rk4_step(f, t, y, h, y_new, neq)
        ! Single RK4 step
        ! dy/dt = f(t, y)

        implicit none

        external :: f
        integer, intent(in) :: neq
        real(dp), intent(in) :: t, h
        real(dp), intent(in) :: y(neq)
        real(dp), intent(out) :: y_new(neq)

        real(dp) :: k1(neq), k2(neq), k3(neq), k4(neq)

        call f(t, y, k1, neq)
        call f(t + h/2, y + h*k1/2, k2, neq)
        call f(t + h/2, y + h*k2/2, k3, neq)
        call f(t + h, y + h*k3, k4, neq)

        y_new = y + h * (k1 + 2*k2 + 2*k3 + k4) / 6

    end subroutine rk4_step


    subroutine solve_ode(f, t0, tf, y0, n_steps, t_out, y_out, neq)
        ! Solve ODE from t0 to tf using RK4

        implicit none

        external :: f
        integer, intent(in) :: neq, n_steps
        real(dp), intent(in) :: t0, tf
        real(dp), intent(in) :: y0(neq)
        real(dp), intent(out) :: t_out(n_steps + 1)
        real(dp), intent(out) :: y_out(neq, n_steps + 1)

        real(dp) :: t, h, y(neq), y_new(neq)
        integer :: i

        h = (tf - t0) / n_steps
        t = t0
        y = y0

        t_out(1) = t
        y_out(:, 1) = y

        do i = 1, n_steps
            call rk4_step(f, t, y, h, y_new, neq)
            t = t + h
            y = y_new

            t_out(i + 1) = t
            y_out(:, i + 1) = y
        end do

    end subroutine solve_ode

end module ode_module
```

### 6.2 Version Python Migrada

```python
"""
Python migration of Fortran ODE solver.

This demonstrates a complete migration workflow:
1. Direct translation (functional equivalence)
2. Pythonic refactoring (idiomatic Python)
3. Performance optimization (NumPy/SciPy)
"""

import numpy as np
from numpy.typing import NDArray
from dataclasses import dataclass
from typing import Callable, Protocol
from abc import ABC, abstractmethod

# Type aliases
State = NDArray[np.float64]  # ODE state vector
Time = float
DerivativeFunc = Callable[[Time, State], State]


@dataclass
class ODESolution:
    """Solution of an ODE initial value problem."""
    t: NDArray[np.float64]
    y: NDArray[np.float64]
    n_steps: int
    method: str

    @property
    def final_state(self) -> State:
        """Final state of the system."""
        return self.y[:, -1]

    @property
    def final_time(self) -> Time:
        """Final time."""
        return self.t[-1]


class ODESolver(ABC):
    """Abstract base class for ODE solvers."""

    @abstractmethod
    def step(
        self,
        f: DerivativeFunc,
        t: Time,
        y: State,
        h: float
    ) -> State:
        """Perform single integration step."""
        pass

    def solve(
        self,
        f: DerivativeFunc,
        t_span: tuple[float, float],
        y0: State,
        n_steps: int
    ) -> ODESolution:
        """Solve ODE from t0 to tf."""
        t0, tf = t_span
        h = (tf - t0) / n_steps
        neq = len(y0)

        # Allocate output arrays
        t_out = np.zeros(n_steps + 1)
        y_out = np.zeros((neq, n_steps + 1))

        # Initial conditions
        t = t0
        y = y0.copy()
        t_out[0] = t
        y_out[:, 0] = y

        # Integration loop
        for i in range(n_steps):
            y = self.step(f, t, y, h)
            t = t + h
            t_out[i + 1] = t
            y_out[:, i + 1] = y

        return ODESolution(
            t=t_out,
            y=y_out,
            n_steps=n_steps,
            method=self.__class__.__name__
        )


class RK4Solver(ODESolver):
    """Classical 4th-order Runge-Kutta solver."""

    def step(
        self,
        f: DerivativeFunc,
        t: Time,
        y: State,
        h: float
    ) -> State:
        """
        Single RK4 step.

        This is functionally equivalent to the Fortran rk4_step subroutine.
        """
        k1 = f(t, y)
        k2 = f(t + h/2, y + h*k1/2)
        k3 = f(t + h/2, y + h*k2/2)
        k4 = f(t + h, y + h*k3)

        return y + h * (k1 + 2*k2 + 2*k3 + k4) / 6


class AdaptiveRK45Solver(ODESolver):
    """
    Adaptive RK45 solver (Dormand-Prince).

    Improvement over fixed-step RK4 with error control.
    """

    def __init__(
        self,
        atol: float = 1e-6,
        rtol: float = 1e-3,
        max_step: float = np.inf,
        min_step: float = 1e-10
    ):
        self.atol = atol
        self.rtol = rtol
        self.max_step = max_step
        self.min_step = min_step

    def step(
        self,
        f: DerivativeFunc,
        t: Time,
        y: State,
        h: float
    ) -> State:
        """Non-adaptive step (for interface compatibility)."""
        # Use embedded RK45 coefficients (Dormand-Prince)
        # Simplified: just do RK4 step
        k1 = f(t, y)
        k2 = f(t + h/2, y + h*k1/2)
        k3 = f(t + h/2, y + h*k2/2)
        k4 = f(t + h, y + h*k3)
        return y + h * (k1 + 2*k2 + 2*k3 + k4) / 6

    def solve_adaptive(
        self,
        f: DerivativeFunc,
        t_span: tuple[float, float],
        y0: State
    ) -> ODESolution:
        """
        Solve with adaptive stepping.

        For production use, prefer scipy.integrate.solve_ivp
        """
        from scipy.integrate import solve_ivp

        result = solve_ivp(
            f, t_span, y0,
            method='RK45',
            atol=self.atol,
            rtol=self.rtol,
            max_step=self.max_step
        )

        return ODESolution(
            t=result.t,
            y=result.y,
            n_steps=len(result.t) - 1,
            method='RK45-adaptive'
        )


def demonstrate_ode_migration():
    """Demonstrate ODE solver migration."""

    print("ODE Solver Migration Example")
    print("=" * 50)

    # Define ODE: harmonic oscillator
    # y'' + omega^2 * y = 0
    # State: [y, y']
    omega = 2.0

    def harmonic_oscillator(t: float, y: State) -> State:
        """Harmonic oscillator ODE."""
        return np.array([y[1], -omega**2 * y[0]])

    # Initial conditions
    y0 = np.array([1.0, 0.0])  # y(0) = 1, y'(0) = 0
    t_span = (0, 10)
    n_steps = 1000

    # Solve with our RK4
    solver = RK4Solver()
    solution = solver.solve(harmonic_oscillator, t_span, y0, n_steps)

    print(f"RK4 Solution:")
    print(f"  Final time: {solution.final_time:.2f}")
    print(f"  Final state: y={solution.final_state[0]:.6f}, y'={solution.final_state[1]:.6f}")

    # Analytical solution
    t_final = solution.final_time
    y_exact = np.cos(omega * t_final)
    print(f"  Analytical y: {y_exact:.6f}")
    print(f"  Error: {abs(solution.final_state[0] - y_exact):.2e}")

    # Compare with SciPy
    from scipy.integrate import solve_ivp

    scipy_result = solve_ivp(
        harmonic_oscillator,
        t_span,
        y0,
        method='RK45',
        dense_output=True
    )

    y_scipy = scipy_result.sol(t_final)[0]
    print(f"\nSciPy RK45 Solution:")
    print(f"  Final y: {y_scipy:.6f}")
    print(f"  Error: {abs(y_scipy - y_exact):.2e}")
    print(f"  Number of steps: {len(scipy_result.t)}")


if __name__ == "__main__":
    demonstrate_ode_migration()
```

---

## 7. PRESERVANDO PRECISION NUMERICA

### 7.1 Precision de Punto Flotante

```python
"""
Preserving Numerical Precision in Fortran-Python Bridge.

Critical considerations:
1. Match Fortran KIND with NumPy dtype
2. Handle IEEE special values (NaN, Inf)
3. Manage floating-point exceptions
4. Preserve algorithm stability
"""

import numpy as np
from typing import NamedTuple

class PrecisionInfo(NamedTuple):
    """Information about floating-point precision."""
    fortran_kind: str
    numpy_dtype: type
    bits: int
    eps: float
    tiny: float
    huge: float

# Precision mapping table
PRECISION_MAP = {
    'single': PrecisionInfo(
        fortran_kind='REAL(4)',
        numpy_dtype=np.float32,
        bits=32,
        eps=np.finfo(np.float32).eps,
        tiny=np.finfo(np.float32).tiny,
        huge=np.finfo(np.float32).max
    ),
    'double': PrecisionInfo(
        fortran_kind='REAL(8)',
        numpy_dtype=np.float64,
        bits=64,
        eps=np.finfo(np.float64).eps,
        tiny=np.finfo(np.float64).tiny,
        huge=np.finfo(np.float64).max
    ),
}


def compare_precision():
    """Compare precision between Fortran and NumPy."""

    print("Floating-Point Precision Comparison")
    print("=" * 50)

    for name, info in PRECISION_MAP.items():
        print(f"\n{name.upper()} Precision:")
        print(f"  Fortran: {info.fortran_kind}")
        print(f"  NumPy:   {info.numpy_dtype}")
        print(f"  Bits:    {info.bits}")
        print(f"  Epsilon: {info.eps:.2e}")
        print(f"  Tiny:    {info.tiny:.2e}")
        print(f"  Huge:    {info.huge:.2e}")


def demonstrate_precision_issues():
    """Demonstrate common precision issues."""

    print("\nCommon Precision Issues")
    print("=" * 50)

    # Issue 1: Cancellation error
    print("\n1. Cancellation Error:")
    a = 1.0
    b = 1e-16
    c = a + b - a  # Should be b

    print(f"   a = 1.0, b = 1e-16")
    print(f"   a + b - a = {c} (should be {b})")
    print(f"   Relative error: {abs(c - b) / b if b != 0 else 0:.2e}")

    # Issue 2: Summation order
    print("\n2. Summation Order (Kahan summation):")
    n = 10000000
    arr = np.ones(n, dtype=np.float32) * 1e-7

    # Naive sum
    naive_sum = np.sum(arr)

    # More accurate with higher precision
    accurate_sum = np.sum(arr.astype(np.float64))

    print(f"   Sum of {n} values of 1e-7:")
    print(f"   Naive (float32):    {naive_sum:.10f}")
    print(f"   Accurate (float64): {accurate_sum:.10f}")
    print(f"   Expected:           {1e-7 * n:.10f}")

    # Issue 3: Comparison of floats
    print("\n3. Float Comparison:")
    x = 0.1 + 0.2
    y = 0.3
    print(f"   0.1 + 0.2 == 0.3: {x == y}")
    print(f"   np.isclose(0.1 + 0.2, 0.3): {np.isclose(x, y)}")


def ensure_precision_compatibility(
    arr: np.ndarray,
    target: str = 'double'
) -> np.ndarray:
    """
    Ensure array has precision matching Fortran target.

    Args:
        arr: Input array
        target: 'single' or 'double'

    Returns:
        Array with correct precision
    """
    info = PRECISION_MAP[target]

    if arr.dtype == info.numpy_dtype:
        return arr

    # Convert with warning
    print(f"Warning: Converting from {arr.dtype} to {info.numpy_dtype}")
    return arr.astype(info.numpy_dtype)


def validate_numerical_equivalence(
    result_python: np.ndarray,
    result_fortran: np.ndarray,
    rtol: float = 1e-10,
    atol: float = 1e-14
) -> tuple[bool, dict]:
    """
    Validate that Python and Fortran results are equivalent.

    Returns:
        Tuple of (is_equivalent, diagnostics_dict)
    """
    if result_python.shape != result_fortran.shape:
        return False, {"error": "Shape mismatch"}

    max_abs_diff = np.max(np.abs(result_python - result_fortran))
    max_rel_diff = np.max(
        np.abs(result_python - result_fortran) /
        np.maximum(np.abs(result_fortran), atol)
    )

    is_close = np.allclose(result_python, result_fortran, rtol=rtol, atol=atol)

    diagnostics = {
        "is_equivalent": is_close,
        "max_absolute_difference": max_abs_diff,
        "max_relative_difference": max_rel_diff,
        "rtol_used": rtol,
        "atol_used": atol,
        "n_elements": result_python.size,
        "n_different": np.sum(~np.isclose(result_python, result_fortran, rtol=rtol, atol=atol))
    }

    return is_close, diagnostics
```

---

## 8. MEJORES PRACTICAS Y DEPURACION

### 8.1 Lista de Verificacion de Migracion

```python
"""
Migration Checklist and Best Practices.
"""

MIGRATION_CHECKLIST = """
ARCHAEON Fortran-to-Python Migration Checklist
==============================================

PRE-MIGRATION:
[ ] Document original Fortran code behavior
[ ] Create test cases with known inputs/outputs
[ ] Profile original code to identify hotspots
[ ] Identify data types and precision requirements
[ ] Map Fortran modules to Python modules

DURING MIGRATION:
[ ] Match array indexing (0-based vs 1-based)
[ ] Handle memory order (column-major vs row-major)
[ ] Preserve intent (in/out/inout) semantics
[ ] Convert intrinsic functions to NumPy equivalents
[ ] Replace DO loops with vectorized operations
[ ] Handle COMMON blocks (if present)
[ ] Convert I/O operations

POST-MIGRATION:
[ ] Validate numerical results against Fortran
[ ] Compare performance (identify regressions)
[ ] Add type hints and documentation
[ ] Write unit tests for all functions
[ ] Profile and optimize Python code
[ ] Document any behavioral differences

COMMON ISSUES:
- Index off-by-one errors
- Memory order mismatch (transpose issues)
- Precision loss (single vs double)
- Missing intent handling
- Loop order optimization loss
"""

print(MIGRATION_CHECKLIST)
```

### 8.2 Herramientas de Depuracion

```python
"""
Debugging Tools for Fortran-Python Bridge.
"""

import numpy as np
from functools import wraps
from typing import Callable, Any

def debug_array_info(name: str, arr: np.ndarray) -> None:
    """Print detailed array information for debugging."""
    print(f"\nArray '{name}':")
    print(f"  Shape: {arr.shape}")
    print(f"  Dtype: {arr.dtype}")
    print(f"  F-contiguous: {arr.flags['F_CONTIGUOUS']}")
    print(f"  C-contiguous: {arr.flags['C_CONTIGUOUS']}")
    print(f"  Strides: {arr.strides}")
    print(f"  Min: {np.min(arr):.6e}, Max: {np.max(arr):.6e}")
    print(f"  Contains NaN: {np.any(np.isnan(arr))}")
    print(f"  Contains Inf: {np.any(np.isinf(arr))}")


def fortran_call_debugger(func: Callable) -> Callable:
    """
    Decorator to debug Fortran function calls.

    Prints input/output array info and validates results.
    """
    @wraps(func)
    def wrapper(*args, **kwargs):
        print(f"\n{'='*50}")
        print(f"Calling: {func.__name__}")

        # Log inputs
        for i, arg in enumerate(args):
            if isinstance(arg, np.ndarray):
                debug_array_info(f"arg[{i}]", arg)

        # Call function
        result = func(*args, **kwargs)

        # Log output
        if isinstance(result, np.ndarray):
            debug_array_info("result", result)
        elif isinstance(result, tuple):
            for i, r in enumerate(result):
                if isinstance(r, np.ndarray):
                    debug_array_info(f"result[{i}]", r)

        print(f"{'='*50}\n")
        return result

    return wrapper


class NumericalValidator:
    """Validate numerical results between implementations."""

    def __init__(
        self,
        rtol: float = 1e-10,
        atol: float = 1e-14,
        verbose: bool = True
    ):
        self.rtol = rtol
        self.atol = atol
        self.verbose = verbose
        self.failures = []

    def compare(
        self,
        name: str,
        result: np.ndarray,
        reference: np.ndarray
    ) -> bool:
        """Compare result against reference."""
        is_close = np.allclose(result, reference, rtol=self.rtol, atol=self.atol)

        if not is_close:
            diff = np.abs(result - reference)
            failure_info = {
                "name": name,
                "max_diff": np.max(diff),
                "mean_diff": np.mean(diff),
                "n_failures": np.sum(~np.isclose(result, reference, self.rtol, self.atol)),
                "shape": result.shape
            }
            self.failures.append(failure_info)

            if self.verbose:
                print(f"VALIDATION FAILED: {name}")
                print(f"  Max difference: {failure_info['max_diff']:.2e}")
                print(f"  Mean difference: {failure_info['mean_diff']:.2e}")
                print(f"  Failed elements: {failure_info['n_failures']}/{result.size}")

        elif self.verbose:
            print(f"VALIDATION PASSED: {name}")

        return is_close

    def summary(self) -> str:
        """Return validation summary."""
        total = len(self.failures)
        if total == 0:
            return "All validations passed!"

        summary = f"\n{total} validation(s) failed:\n"
        for f in self.failures:
            summary += f"  - {f['name']}: max_diff={f['max_diff']:.2e}\n"
        return summary
```

---

## RESUMEN

Este modulo cubre la integracion de codigo Fortran con Python:

1. **f2py**: Herramienta principal para envolver codigo Fortran
2. **Memoria**: Diferencias criticas entre C-order y Fortran-order
3. **BLAS/LAPACK**: Acceso directo a rutinas de alto rendimiento
4. **Conversion**: Patrones comunes para migrar codigo
5. **Precision**: Preservar exactitud numerica en la migracion

Puntos clave para ARCHAEON:
- f2py es la opcion predeterminada para la mayoria de casos
- Siempre verificar el orden de memoria de arrays
- Validar resultados contra la implementacion original
- La vectorizacion NumPy puede reemplazar muchos patrones Fortran
- BLAS/LAPACK proporcionan rendimiento comparable a Fortran

---

*ARCHAEON CORE - Bridging Legacy to Modern*
*Fortran-Python Bridge v1.0.0*

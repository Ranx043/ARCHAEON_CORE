---
title: "PYTHON 02 - Computacion Cientifica con Python"
version: "1.0.0"
date: "2025-12-31"
author: "ARCHAEON CORE"
classification: "ARCHAEON/80000_MODERNOS/PYTHON"
purpose: "NumPy, SciPy, Pandas y visualizacion para computacion cientifica"
language_bridge: "Fortran/C Arrays -> NumPy/SciPy"
keywords:
  - numpy
  - scipy
  - pandas
  - matplotlib
  - scientific computing
  - numerical methods
  - vectorization
dependencies:
  - python >= 3.10
  - numpy >= 1.24
  - scipy >= 1.10
  - pandas >= 2.0
  - matplotlib >= 3.7
status: "ACTIVE"
---

# PYTHON 02 - COMPUTACION CIENTIFICA CON PYTHON

## INDICE

1. [NumPy: Fundamentos de Arrays](#1-numpy-fundamentos-de-arrays)
2. [NumPy: Broadcasting y Vectorizacion](#2-numpy-broadcasting-y-vectorizacion)
3. [SciPy: Algebra Lineal](#3-scipy-algebra-lineal)
4. [SciPy: Optimizacion e Integracion](#4-scipy-optimizacion-e-integracion)
5. [Pandas: DataFrames para Procesamiento de Datos](#5-pandas-dataframes-para-procesamiento-de-datos)
6. [Matplotlib y Visualizacion](#6-matplotlib-y-visualizacion)
7. [Comparacion con Arrays Fortran](#7-comparacion-con-arrays-fortran)
8. [Consideraciones de Rendimiento](#8-consideraciones-de-rendimiento)
9. [Patrones Comunes y Mejores Practicas](#9-patrones-comunes-y-mejores-practicas)

---

## 1. NUMPY: FUNDAMENTOS DE ARRAYS

### 1.1 Introduccion a NumPy

NumPy es la biblioteca fundamental para computacion cientifica en Python.
Proporciona arrays N-dimensionales eficientes que reemplazan los arrays
de Fortran y C para la mayoria de aplicaciones cientificas.

```python
"""
NumPy Fundamentals - Array creation and basic operations.

NumPy arrays are the Python equivalent of:
- Fortran REAL(8), DIMENSION(:,:) arrays
- C double arrays with pointer arithmetic
- MATLAB matrices

Key advantages over Fortran/C:
- Dynamic sizing without manual memory management
- Broadcasting for element-wise operations
- Extensive mathematical functions
- Integration with Python ecosystem
"""

import numpy as np
from numpy.typing import NDArray
from typing import Any

# Array creation - basic methods
def demonstrate_array_creation() -> None:
    """Demonstrate various array creation methods."""

    # From Python list - equivalent to Fortran array initialization
    # Fortran: REAL(8) :: arr(5) = [1.0, 2.0, 3.0, 4.0, 5.0]
    arr1d = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
    print(f"1D array: {arr1d}")
    print(f"  dtype: {arr1d.dtype}, shape: {arr1d.shape}")

    # 2D array - equivalent to Fortran matrix
    # Fortran: REAL(8) :: mat(2,3)
    arr2d = np.array([
        [1.0, 2.0, 3.0],
        [4.0, 5.0, 6.0]
    ])
    print(f"\n2D array:\n{arr2d}")
    print(f"  shape: {arr2d.shape} (rows, cols)")

    # Zeros array - Fortran equivalent: arr = 0.0D0
    zeros = np.zeros((3, 4), dtype=np.float64)
    print(f"\nZeros (3x4):\n{zeros}")

    # Ones array
    ones = np.ones((2, 3), dtype=np.float64)
    print(f"\nOnes (2x3):\n{ones}")

    # Identity matrix - Fortran: unit matrix subroutine
    identity = np.eye(4, dtype=np.float64)
    print(f"\nIdentity (4x4):\n{identity}")

    # Diagonal matrix
    diag = np.diag([1.0, 2.0, 3.0, 4.0])
    print(f"\nDiagonal:\n{diag}")

    # Range arrays - Fortran: DO i = 1, 10
    arange = np.arange(0, 10, 2)  # start, stop, step
    print(f"\nArange(0,10,2): {arange}")

    # Linspace - Fortran: evenly spaced grid
    linspace = np.linspace(0, 1, 11)  # start, stop, num_points
    print(f"\nLinspace(0,1,11): {linspace}")

    # Logspace - logarithmically spaced
    logspace = np.logspace(0, 3, 4)  # 10^0 to 10^3, 4 points
    print(f"\nLogspace(0,3,4): {logspace}")

    # Random arrays - for Monte Carlo, testing
    np.random.seed(42)  # For reproducibility
    uniform = np.random.random((2, 3))
    normal = np.random.randn(2, 3)  # Standard normal
    print(f"\nUniform random:\n{uniform}")
    print(f"\nNormal random:\n{normal}")


# Data types - equivalent to Fortran KIND
def demonstrate_dtypes() -> None:
    """Demonstrate NumPy data types."""

    # Integer types
    # Fortran: INTEGER(1), INTEGER(2), INTEGER(4), INTEGER(8)
    int8_arr = np.array([1, 2, 3], dtype=np.int8)    # 1 byte
    int16_arr = np.array([1, 2, 3], dtype=np.int16)  # 2 bytes
    int32_arr = np.array([1, 2, 3], dtype=np.int32)  # 4 bytes
    int64_arr = np.array([1, 2, 3], dtype=np.int64)  # 8 bytes

    # Floating point types
    # Fortran: REAL(4), REAL(8), REAL(16)
    float32_arr = np.array([1.0, 2.0], dtype=np.float32)  # Single precision
    float64_arr = np.array([1.0, 2.0], dtype=np.float64)  # Double precision
    # float128_arr = np.array([1.0, 2.0], dtype=np.float128)  # Quad (platform dependent)

    # Complex types
    # Fortran: COMPLEX(4), COMPLEX(8)
    complex64_arr = np.array([1+2j, 3+4j], dtype=np.complex64)
    complex128_arr = np.array([1+2j, 3+4j], dtype=np.complex128)

    # Boolean type
    # Fortran: LOGICAL
    bool_arr = np.array([True, False, True], dtype=np.bool_)

    # Print information
    dtypes = [
        ("int8", int8_arr),
        ("int64", int64_arr),
        ("float32", float32_arr),
        ("float64", float64_arr),
        ("complex128", complex128_arr),
        ("bool", bool_arr),
    ]

    print("NumPy Data Types:")
    for name, arr in dtypes:
        print(f"  {name:12s}: itemsize={arr.itemsize} bytes")


# Type annotations for NumPy arrays
def typed_array_operations(
    matrix: NDArray[np.float64],
    vector: NDArray[np.float64]
) -> NDArray[np.float64]:
    """
    Example of type-annotated NumPy operations.

    Type hints improve code clarity and enable static analysis.
    """
    # Matrix-vector multiplication
    result = matrix @ vector
    return result
```

### 1.2 Indexacion y Slicing

```python
"""
Array Indexing and Slicing - Accessing array elements.

NumPy indexing is more powerful than Fortran/C:
- Negative indices
- Boolean indexing
- Fancy indexing
- Views vs copies
"""

import numpy as np
from numpy.typing import NDArray

def demonstrate_indexing() -> None:
    """Demonstrate NumPy indexing patterns."""

    # Create sample array
    arr = np.arange(20).reshape(4, 5)
    print(f"Original array (4x5):\n{arr}\n")

    # Basic indexing - like Fortran/C but 0-based
    # Fortran: arr(1,1) is first element
    # NumPy: arr[0,0] is first element
    print(f"arr[0,0] = {arr[0, 0]}")  # First element
    print(f"arr[3,4] = {arr[3, 4]}")  # Last element
    print(f"arr[-1,-1] = {arr[-1, -1]}")  # Last element (negative index)

    # Row slicing - Fortran: arr(2,:)
    print(f"\nRow 1 (arr[1,:]): {arr[1, :]}")

    # Column slicing - Fortran: arr(:,3)
    print(f"Column 2 (arr[:,2]): {arr[:, 2]}")

    # Subarray slicing - Fortran: arr(1:3, 2:4)
    print(f"\nSubarray arr[0:2, 1:4]:\n{arr[0:2, 1:4]}")

    # Step slicing - every other element
    print(f"\nEvery other row: arr[::2, :]:\n{arr[::2, :]}")
    print(f"Reversed rows: arr[::-1, :]:\n{arr[::-1, :]}")


def demonstrate_advanced_indexing() -> None:
    """Demonstrate advanced indexing techniques."""

    arr = np.arange(25).reshape(5, 5)
    print(f"Original array:\n{arr}\n")

    # Boolean indexing - select elements meeting condition
    # This is much more elegant than Fortran WHERE
    mask = arr > 12
    print(f"Boolean mask (arr > 12):\n{mask}\n")
    print(f"Elements > 12: {arr[mask]}")

    # Modify elements meeting condition
    arr_copy = arr.copy()
    arr_copy[arr_copy > 20] = -1
    print(f"\nAfter setting >20 to -1:\n{arr_copy}")

    # Fancy indexing - select specific indices
    rows = np.array([0, 2, 4])
    cols = np.array([1, 3, 0])
    print(f"\nFancy indexing arr[{rows}, {cols}]: {arr[rows, cols]}")

    # Select specific rows
    print(f"\nRows 0, 2, 4:\n{arr[[0, 2, 4], :]}")

    # np.where - conditional selection
    # Fortran: WHERE/ELSEWHERE
    result = np.where(arr > 12, arr, 0)
    print(f"\nnp.where(arr > 12, arr, 0):\n{result}")


def views_vs_copies() -> None:
    """
    Demonstrate difference between views and copies.

    Critical for understanding memory behavior,
    especially when optimizing legacy code.
    """
    original = np.arange(10)

    # Slicing creates a VIEW (shared memory)
    view = original[2:7]
    view[0] = 999  # Modifies original!
    print(f"After modifying view: original = {original}")

    # Explicit copy creates independent array
    original = np.arange(10)
    copy = original[2:7].copy()
    copy[0] = 999  # Does NOT modify original
    print(f"After modifying copy: original = {original}")

    # Check if array owns its data
    print(f"\noriginal.flags.owndata: {original.flags.owndata}")
    print(f"view.flags.owndata: {view.flags.owndata}")
    print(f"copy.flags.owndata: {copy.flags.owndata}")
```

### 1.3 Operaciones con Arrays

```python
"""
Array Operations - Mathematical operations on arrays.

NumPy operations are element-wise by default,
similar to Fortran's elemental functions.
"""

import numpy as np
from numpy.typing import NDArray

def demonstrate_arithmetic() -> None:
    """Demonstrate arithmetic operations on arrays."""

    a = np.array([1.0, 2.0, 3.0, 4.0, 5.0])
    b = np.array([2.0, 2.0, 2.0, 2.0, 2.0])

    print("Array arithmetic (element-wise):")
    print(f"a = {a}")
    print(f"b = {b}")
    print(f"a + b = {a + b}")
    print(f"a - b = {a - b}")
    print(f"a * b = {a * b}")  # Element-wise, NOT dot product
    print(f"a / b = {a / b}")
    print(f"a ** b = {a ** b}")  # Element-wise power
    print(f"a // b = {a // b}")  # Floor division
    print(f"a % b = {a % b}")    # Modulo

    # Scalar operations broadcast to all elements
    print(f"\na + 10 = {a + 10}")
    print(f"a * 2 = {a * 2}")
    print(f"1 / a = {1 / a}")


def demonstrate_mathematical_functions() -> None:
    """Demonstrate mathematical functions on arrays."""

    x = np.linspace(0, 2*np.pi, 9)

    print("Universal functions (ufuncs):")
    print(f"x = {x}")
    print(f"sin(x) = {np.sin(x)}")
    print(f"cos(x) = {np.cos(x)}")
    print(f"exp(x[:4]) = {np.exp(x[:4])}")  # First 4 elements
    print(f"log(x[1:5]) = {np.log(x[1:5])}")  # Avoid log(0)
    print(f"sqrt(x) = {np.sqrt(x)}")

    # Fortran equivalent functions
    print("\nFortran equivalents:")
    print(f"  np.abs(-5) = {np.abs(-5)} (ABS)")
    print(f"  np.sign(-3) = {np.sign(-3)} (SIGN)")
    print(f"  np.floor(3.7) = {np.floor(3.7)} (FLOOR)")
    print(f"  np.ceil(3.2) = {np.ceil(3.2)} (CEILING)")
    print(f"  np.round(3.5) = {np.round(3.5)} (NINT)")


def demonstrate_aggregation() -> None:
    """Demonstrate aggregation functions."""

    arr = np.array([[1, 2, 3],
                    [4, 5, 6],
                    [7, 8, 9]], dtype=np.float64)

    print(f"Array:\n{arr}\n")

    # Global aggregations - Fortran intrinsics
    print("Global aggregations (Fortran equivalents):")
    print(f"  np.sum(arr) = {np.sum(arr)} (SUM)")
    print(f"  np.prod(arr) = {np.prod(arr)} (PRODUCT)")
    print(f"  np.min(arr) = {np.min(arr)} (MINVAL)")
    print(f"  np.max(arr) = {np.max(arr)} (MAXVAL)")
    print(f"  np.mean(arr) = {np.mean(arr)}")
    print(f"  np.std(arr) = {np.std(arr):.4f}")
    print(f"  np.var(arr) = {np.var(arr):.4f}")

    # Axis-specific aggregations
    # Fortran: SUM(arr, DIM=1)
    print("\nAxis-specific aggregations:")
    print(f"  Sum along axis=0 (columns): {np.sum(arr, axis=0)}")
    print(f"  Sum along axis=1 (rows): {np.sum(arr, axis=1)}")
    print(f"  Min along axis=0: {np.min(arr, axis=0)}")
    print(f"  Max along axis=1: {np.max(arr, axis=1)}")

    # Cumulative operations
    print("\nCumulative operations:")
    print(f"  Cumsum (flat): {np.cumsum(arr)}")
    print(f"  Cumsum axis=1:\n{np.cumsum(arr, axis=1)}")
    print(f"  Cumprod axis=0:\n{np.cumprod(arr, axis=0)}")
```

---

## 2. NUMPY: BROADCASTING Y VECTORIZACION

### 2.1 Broadcasting Rules

```python
"""
Broadcasting - NumPy's powerful array operation mechanism.

Broadcasting allows operations between arrays of different shapes,
eliminating the need for explicit loops (common in Fortran).

Rules:
1. If arrays have different ndim, pad smaller with 1s on the left
2. Arrays are compatible if sizes match or one is 1
3. In any dimension where one array has size 1, it is "stretched"
"""

import numpy as np

def demonstrate_broadcasting_basics() -> None:
    """Demonstrate basic broadcasting patterns."""

    # Scalar broadcasting - most common case
    # Equivalent to Fortran: arr = arr + 10 (elemental)
    arr = np.array([[1, 2, 3],
                    [4, 5, 6]])
    print(f"Array + scalar:\n{arr} + 10 =\n{arr + 10}\n")

    # 1D array with 2D array
    # Fortran would require explicit loop or temporary array
    row_vector = np.array([1, 2, 3])
    print(f"2D array + row vector:")
    print(f"{arr}\n+\n{row_vector}\n=\n{arr + row_vector}\n")

    # Column vector broadcasting
    col_vector = np.array([[10],
                           [20]])
    print(f"2D array + column vector:")
    print(f"{arr}\n+\n{col_vector}\n=\n{arr + col_vector}\n")


def demonstrate_broadcasting_advanced() -> None:
    """Demonstrate advanced broadcasting patterns."""

    # Outer product using broadcasting
    # Fortran: SPREAD function or explicit loops
    x = np.array([1, 2, 3, 4])
    y = np.array([10, 20, 30])

    # Create outer product matrix
    # x[:, np.newaxis] creates column vector (4,1)
    # y has shape (3,)
    # Result is (4, 3)
    outer = x[:, np.newaxis] * y
    print(f"Outer product of {x} and {y}:")
    print(f"{outer}\n")

    # Distance matrix calculation
    # Common in computational geometry, physics
    points = np.array([[0, 0],
                       [1, 0],
                       [0, 1],
                       [1, 1]])

    # Calculate pairwise distances using broadcasting
    # points[:, np.newaxis, :] has shape (4, 1, 2)
    # points[np.newaxis, :, :] has shape (1, 4, 2)
    # Subtraction broadcasts to (4, 4, 2)
    diff = points[:, np.newaxis, :] - points[np.newaxis, :, :]
    distances = np.sqrt(np.sum(diff ** 2, axis=2))
    print("Pairwise distance matrix:")
    print(f"{distances}\n")


def broadcasting_vs_fortran_loops() -> None:
    """
    Compare broadcasting with Fortran-style loops.

    Broadcasting eliminates loops, improving:
    - Readability
    - Performance (C-level loops in NumPy)
    - Memory efficiency
    """

    # Problem: Add row means to each element
    matrix = np.array([[1, 2, 3],
                       [4, 5, 6],
                       [7, 8, 9]], dtype=np.float64)

    # Fortran-style approach (AVOID):
    # DO i = 1, 3
    #     row_mean = SUM(matrix(i,:)) / 3
    #     DO j = 1, 3
    #         result(i,j) = matrix(i,j) + row_mean
    #     END DO
    # END DO

    # Pythonic approach with loops (slow):
    result_loop = np.zeros_like(matrix)
    for i in range(3):
        row_mean = np.mean(matrix[i, :])
        for j in range(3):
            result_loop[i, j] = matrix[i, j] + row_mean

    # Broadcasting approach (fast, elegant):
    row_means = np.mean(matrix, axis=1, keepdims=True)  # Shape (3, 1)
    result_broadcast = matrix + row_means

    print("Original matrix:")
    print(matrix)
    print(f"\nRow means (with keepdims): {row_means.T}")
    print(f"\nResult with broadcasting:\n{result_broadcast}")

    # Verify both give same result
    assert np.allclose(result_loop, result_broadcast)
```

### 2.2 Vectorizacion

```python
"""
Vectorization - Replacing loops with array operations.

Vectorization is the key to high-performance NumPy code.
It replaces Python loops with optimized C loops.
"""

import numpy as np
import time
from typing import Callable

def benchmark(
    func: Callable,
    *args,
    repeats: int = 100
) -> tuple[float, any]:
    """Benchmark a function."""
    start = time.perf_counter()
    for _ in range(repeats):
        result = func(*args)
    elapsed = (time.perf_counter() - start) / repeats
    return elapsed, result


def compare_loop_vs_vectorized() -> None:
    """Compare loop-based vs vectorized implementations."""

    n = 10000
    x = np.random.randn(n)
    y = np.random.randn(n)

    # Problem: Compute element-wise Euclidean distance
    # Fortran-style loop implementation
    def loop_distance(x: np.ndarray, y: np.ndarray) -> np.ndarray:
        result = np.zeros(len(x))
        for i in range(len(x)):
            result[i] = np.sqrt(x[i]**2 + y[i]**2)
        return result

    # Vectorized implementation
    def vectorized_distance(x: np.ndarray, y: np.ndarray) -> np.ndarray:
        return np.sqrt(x**2 + y**2)

    # Using numpy function directly
    def numpy_hypot(x: np.ndarray, y: np.ndarray) -> np.ndarray:
        return np.hypot(x, y)

    # Benchmark
    time_loop, _ = benchmark(loop_distance, x, y, repeats=10)
    time_vec, _ = benchmark(vectorized_distance, x, y, repeats=100)
    time_hypot, _ = benchmark(numpy_hypot, x, y, repeats=100)

    print("Distance calculation benchmark:")
    print(f"  Loop:       {time_loop*1000:.4f} ms")
    print(f"  Vectorized: {time_vec*1000:.4f} ms ({time_loop/time_vec:.1f}x faster)")
    print(f"  np.hypot:   {time_hypot*1000:.4f} ms ({time_loop/time_hypot:.1f}x faster)")


def vectorize_custom_function() -> None:
    """Demonstrate np.vectorize for custom functions."""

    # Sometimes we have a scalar function
    def scalar_step_function(x: float) -> float:
        """Step function - works only on scalars."""
        if x < -1:
            return -1.0
        elif x > 1:
            return 1.0
        else:
            return x

    # Vectorize it
    vectorized_step = np.vectorize(scalar_step_function)

    x = np.linspace(-2, 2, 9)
    print(f"Input: {x}")
    print(f"Step function output: {vectorized_step(x)}")

    # Better: use np.where for truly vectorized operation
    def truly_vectorized_step(x: np.ndarray) -> np.ndarray:
        """Properly vectorized step function."""
        return np.where(x < -1, -1.0,
               np.where(x > 1, 1.0, x))

    print(f"np.where version: {truly_vectorized_step(x)}")

    # Or use np.clip for this specific case
    print(f"np.clip version: {np.clip(x, -1, 1)}")


def vectorized_numerical_integration() -> None:
    """
    Example: Vectorized numerical integration.

    Demonstrates replacing Fortran DO loops with vectorization.
    """

    def f(x: np.ndarray) -> np.ndarray:
        """Function to integrate: f(x) = sin(x) * exp(-x/5)"""
        return np.sin(x) * np.exp(-x / 5)

    # Integration bounds
    a, b = 0, 10
    n_points = 1000

    # Fortran-style trapezoidal rule:
    # h = (b - a) / n
    # integral = 0.5 * (f(a) + f(b))
    # DO i = 1, n-1
    #     integral = integral + f(a + i*h)
    # END DO
    # integral = integral * h

    # Vectorized trapezoidal rule
    x = np.linspace(a, b, n_points + 1)
    y = f(x)
    h = (b - a) / n_points

    # Method 1: Explicit vectorization
    integral_1 = h * (0.5 * (y[0] + y[-1]) + np.sum(y[1:-1]))

    # Method 2: Using np.trapz
    integral_2 = np.trapz(y, x)

    print("Numerical Integration of sin(x)*exp(-x/5) from 0 to 10:")
    print(f"  Vectorized trapezoidal: {integral_1:.10f}")
    print(f"  np.trapz:               {integral_2:.10f}")
```

---

## 3. SCIPY: ALGEBRA LINEAL

### 3.1 Operaciones Basicas de Algebra Lineal

```python
"""
Linear Algebra with SciPy and NumPy.

SciPy provides optimized BLAS/LAPACK wrappers,
giving Python access to the same routines used by Fortran.
"""

import numpy as np
from numpy.typing import NDArray
import scipy.linalg as la

def demonstrate_basic_linalg() -> None:
    """Demonstrate basic linear algebra operations."""

    # Create test matrices
    A = np.array([[4, 2, 1],
                  [2, 5, 2],
                  [1, 2, 4]], dtype=np.float64)

    b = np.array([1, 2, 3], dtype=np.float64)

    print(f"Matrix A:\n{A}\n")
    print(f"Vector b: {b}\n")

    # Matrix-vector multiplication
    # Fortran: DGEMV
    result = A @ b
    print(f"A @ b = {result}")

    # Matrix-matrix multiplication
    # Fortran: DGEMM
    B = np.array([[1, 0],
                  [0, 1],
                  [1, 1]], dtype=np.float64)
    C = A @ B
    print(f"\nA @ B:\n{C}")

    # Transpose
    # Fortran: TRANSPOSE(A)
    print(f"\nTranspose of A:\n{A.T}")

    # Determinant
    # Fortran: Typically from LAPACK
    det = la.det(A)
    print(f"\nDeterminant of A: {det:.6f}")

    # Trace
    # Fortran: sum of diagonal
    trace = np.trace(A)
    print(f"Trace of A: {trace}")

    # Rank
    rank = np.linalg.matrix_rank(A)
    print(f"Rank of A: {rank}")


def demonstrate_matrix_norms() -> None:
    """Demonstrate matrix and vector norms."""

    A = np.array([[1, 2, 3],
                  [4, 5, 6],
                  [7, 8, 9]], dtype=np.float64)

    v = np.array([3, 4], dtype=np.float64)

    print("Vector norms:")
    print(f"  ||v||_1 (L1):     {la.norm(v, 1)}")      # Sum of absolute values
    print(f"  ||v||_2 (L2):     {la.norm(v, 2)}")      # Euclidean norm
    print(f"  ||v||_inf (Linf): {la.norm(v, np.inf)}") # Max absolute value

    print("\nMatrix norms:")
    print(f"  ||A||_1:          {la.norm(A, 1)}")      # Max column sum
    print(f"  ||A||_2:          {la.norm(A, 2)}")      # Spectral norm
    print(f"  ||A||_inf:        {la.norm(A, np.inf)}") # Max row sum
    print(f"  ||A||_F:          {la.norm(A, 'fro')}")  # Frobenius norm


def solve_linear_system() -> None:
    """
    Solve linear systems Ax = b.

    Demonstrates various methods equivalent to:
    - Fortran DGESV (LU factorization)
    - Fortran DPOTRS (Cholesky for SPD)
    """

    # Create a well-conditioned SPD matrix
    A = np.array([[10, 2, 1],
                  [2, 8, 2],
                  [1, 2, 6]], dtype=np.float64)

    b = np.array([12, 10, 8], dtype=np.float64)

    print(f"Solving Ax = b where:")
    print(f"A =\n{A}")
    print(f"b = {b}\n")

    # Method 1: General solver (LU decomposition)
    # Fortran: DGESV
    x_general = la.solve(A, b)
    print(f"General solve: x = {x_general}")
    print(f"  Residual: {la.norm(A @ x_general - b):.2e}")

    # Method 2: Assuming symmetric positive definite
    # Fortran: DPOTRS (uses Cholesky)
    x_spd = la.solve(A, b, assume_a='pos')
    print(f"SPD solve:     x = {x_spd}")

    # Method 3: LU decomposition (for multiple RHS)
    # Fortran: DGETRF + DGETRS
    lu, piv = la.lu_factor(A)
    x_lu = la.lu_solve((lu, piv), b)
    print(f"LU solve:      x = {x_lu}")

    # Method 4: Cholesky decomposition (for SPD)
    # Fortran: DPOTRF + DPOTRS
    L = la.cholesky(A, lower=True)
    # Solve L y = b, then L^T x = y
    y = la.solve_triangular(L, b, lower=True)
    x_chol = la.solve_triangular(L.T, y, lower=False)
    print(f"Cholesky:      x = {x_chol}")


def matrix_decompositions() -> None:
    """
    Demonstrate matrix decompositions.

    These are fundamental in numerical linear algebra,
    with direct equivalents in LAPACK.
    """

    A = np.array([[4, 2, 1],
                  [2, 5, 2],
                  [1, 2, 4]], dtype=np.float64)

    print("Matrix Decompositions")
    print("=" * 50)
    print(f"\nOriginal matrix A:\n{A}\n")

    # LU Decomposition
    # Fortran: DGETRF
    P, L, U = la.lu(A)
    print("LU Decomposition (PA = LU):")
    print(f"P =\n{P}")
    print(f"L =\n{L}")
    print(f"U =\n{U}")
    print(f"Reconstruction error: {la.norm(P @ A - L @ U):.2e}\n")

    # Cholesky Decomposition (for SPD)
    # Fortran: DPOTRF
    L_chol = la.cholesky(A, lower=True)
    print("Cholesky Decomposition (A = L L^T):")
    print(f"L =\n{L_chol}")
    print(f"Reconstruction error: {la.norm(A - L_chol @ L_chol.T):.2e}\n")

    # QR Decomposition
    # Fortran: DGEQRF
    Q, R = la.qr(A)
    print("QR Decomposition (A = QR):")
    print(f"Q =\n{Q}")
    print(f"R =\n{R}")
    print(f"Reconstruction error: {la.norm(A - Q @ R):.2e}\n")

    # Eigenvalue Decomposition
    # Fortran: DSYEV (symmetric), DGEEV (general)
    eigenvalues, eigenvectors = la.eigh(A)  # For symmetric
    print("Eigenvalue Decomposition:")
    print(f"Eigenvalues: {eigenvalues}")
    print(f"Eigenvectors:\n{eigenvectors}")
    # Verify: A V = V Lambda
    Lambda = np.diag(eigenvalues)
    print(f"Reconstruction error: {la.norm(A @ eigenvectors - eigenvectors @ Lambda):.2e}\n")

    # SVD - Singular Value Decomposition
    # Fortran: DGESVD
    U_svd, s, Vt = la.svd(A)
    print("SVD (A = U S V^T):")
    print(f"Singular values: {s}")
    print(f"U =\n{U_svd}")
    print(f"V^T =\n{Vt}")
    S = np.diag(s)
    print(f"Reconstruction error: {la.norm(A - U_svd @ S @ Vt):.2e}")
```

### 3.2 Sistemas Especiales y Matrices Sparse

```python
"""
Special Systems and Sparse Matrices.

Efficient handling of structured matrices,
common in scientific computing (PDEs, etc.)
"""

import numpy as np
import scipy.linalg as la
import scipy.sparse as sp
from scipy.sparse.linalg import spsolve, cg, gmres

def solve_tridiagonal_system() -> None:
    """
    Solve tridiagonal system efficiently.

    Common in finite difference methods.
    Fortran: DGTTRF + DGTTRS
    """

    n = 100

    # Create tridiagonal system
    # Main diagonal
    d = 4.0 * np.ones(n)
    # Off-diagonals
    dl = -1.0 * np.ones(n - 1)  # Lower
    du = -1.0 * np.ones(n - 1)  # Upper

    # RHS
    b = np.ones(n)
    b[0] = 3
    b[-1] = 3

    # Form full matrix for comparison
    A_full = np.diag(d) + np.diag(dl, -1) + np.diag(du, 1)

    # Method 1: Solve as banded matrix
    # Much more efficient than full LU for banded systems
    ab = np.zeros((3, n))
    ab[0, 1:] = du    # Upper diagonal
    ab[1, :] = d      # Main diagonal
    ab[2, :-1] = dl   # Lower diagonal

    x_banded = la.solve_banded((1, 1), ab, b)

    # Method 2: Full solve (for comparison)
    x_full = la.solve(A_full, b)

    print("Tridiagonal System Solution:")
    print(f"  Banded solve: x[0:5] = {x_banded[:5]}")
    print(f"  Full solve:   x[0:5] = {x_full[:5]}")
    print(f"  Difference:   {la.norm(x_banded - x_full):.2e}")


def sparse_matrix_operations() -> None:
    """
    Demonstrate sparse matrix operations.

    Essential for large-scale problems (FEM, FDM, networks).
    """

    # Create sparse matrix in COO format
    # Coordinate format - easy to construct
    n = 1000
    row = np.array([0, 0, 1, 1, 2, 2, 3])
    col = np.array([0, 1, 0, 1, 1, 2, 3])
    data = np.array([4, -1, -1, 4, -1, 4, 4], dtype=np.float64)

    A_coo = sp.coo_matrix((data, (row, col)), shape=(4, 4))
    print(f"COO matrix:\n{A_coo.toarray()}")

    # Convert to CSR for efficient arithmetic
    # CSR = Compressed Sparse Row
    A_csr = A_coo.tocsr()

    # CSC for efficient column slicing
    # CSC = Compressed Sparse Column
    A_csc = A_coo.tocsc()

    # Create larger sparse matrix - typical PDE discretization
    n = 100
    diagonals = [
        -1 * np.ones(n - 1),  # Lower diagonal
        4 * np.ones(n),        # Main diagonal
        -1 * np.ones(n - 1)   # Upper diagonal
    ]
    A_sparse = sp.diags(diagonals, [-1, 0, 1], format='csr')

    print(f"\nSparse matrix info:")
    print(f"  Shape: {A_sparse.shape}")
    print(f"  Non-zeros: {A_sparse.nnz}")
    print(f"  Density: {A_sparse.nnz / (n*n):.4%}")

    # Solve sparse system
    b = np.ones(n)
    x = spsolve(A_sparse, b)
    print(f"\nSparse solve:")
    print(f"  x[0:5] = {x[:5]}")
    print(f"  Residual: {la.norm(A_sparse @ x - b):.2e}")


def iterative_solvers() -> None:
    """
    Demonstrate iterative solvers for large sparse systems.

    More efficient than direct methods for very large problems.
    """

    n = 1000

    # Create SPD sparse matrix (Laplacian-like)
    diagonals = [
        -1 * np.ones(n - 1),
        4 * np.ones(n),
        -1 * np.ones(n - 1)
    ]
    A = sp.diags(diagonals, [-1, 0, 1], format='csr')

    b = np.ones(n)

    # Conjugate Gradient (for SPD systems)
    x_cg, info_cg = cg(A, b, tol=1e-10)
    print("Conjugate Gradient:")
    print(f"  Converged: {info_cg == 0}")
    print(f"  Residual: {la.norm(A @ x_cg - b):.2e}")

    # GMRES (for general non-symmetric)
    x_gmres, info_gmres = gmres(A, b, tol=1e-10)
    print("\nGMRES:")
    print(f"  Converged: {info_gmres == 0}")
    print(f"  Residual: {la.norm(A @ x_gmres - b):.2e}")
```

---

## 4. SCIPY: OPTIMIZACION E INTEGRACION

### 4.1 Optimizacion

```python
"""
Optimization with SciPy.

Covers:
- Root finding
- Minimization
- Curve fitting
- Constrained optimization
"""

import numpy as np
from scipy import optimize
from typing import Callable

def root_finding_examples() -> None:
    """Demonstrate root finding methods."""

    # Scalar root finding
    def f(x: float) -> float:
        """Function: x^3 - x - 2 = 0"""
        return x**3 - x - 2

    def f_prime(x: float) -> float:
        """Derivative: 3x^2 - 1"""
        return 3*x**2 - 1

    print("Root Finding for x^3 - x - 2 = 0")
    print("=" * 50)

    # Brent's method (bracketing)
    root_brent = optimize.brentq(f, 1, 2)
    print(f"Brent's method:    x = {root_brent:.10f}")

    # Newton-Raphson
    root_newton = optimize.newton(f, x0=1.5, fprime=f_prime)
    print(f"Newton-Raphson:    x = {root_newton:.10f}")

    # Secant method (no derivative needed)
    root_secant = optimize.newton(f, x0=1.5)
    print(f"Secant method:     x = {root_secant:.10f}")

    # Verify
    print(f"\nVerification: f(root) = {f(root_brent):.2e}")

    # Multivariate root finding
    def system(x: np.ndarray) -> np.ndarray:
        """System of equations:
        x^2 + y^2 = 4
        x*y = 1
        """
        return np.array([
            x[0]**2 + x[1]**2 - 4,
            x[0] * x[1] - 1
        ])

    def jacobian(x: np.ndarray) -> np.ndarray:
        """Jacobian of the system."""
        return np.array([
            [2*x[0], 2*x[1]],
            [x[1], x[0]]
        ])

    print("\nMultivariate Root Finding")
    print("x^2 + y^2 = 4, x*y = 1")

    # Solve using Newton's method (default)
    solution = optimize.root(system, x0=[1, 1], jac=jacobian)
    print(f"Solution: x = {solution.x[0]:.6f}, y = {solution.x[1]:.6f}")
    print(f"Converged: {solution.success}")
    print(f"Residual: {np.linalg.norm(system(solution.x)):.2e}")


def minimization_examples() -> None:
    """Demonstrate function minimization."""

    print("\nUnconstrained Minimization")
    print("=" * 50)

    # Rosenbrock function - classic test case
    def rosenbrock(x: np.ndarray) -> float:
        """Rosenbrock function: f(x,y) = (1-x)^2 + 100(y-x^2)^2"""
        return (1 - x[0])**2 + 100*(x[1] - x[0]**2)**2

    def rosenbrock_grad(x: np.ndarray) -> np.ndarray:
        """Gradient of Rosenbrock function."""
        return np.array([
            -2*(1 - x[0]) - 400*x[0]*(x[1] - x[0]**2),
            200*(x[1] - x[0]**2)
        ])

    x0 = np.array([-1.0, 1.0])

    # Nelder-Mead (derivative-free)
    result_nm = optimize.minimize(rosenbrock, x0, method='Nelder-Mead')
    print(f"Nelder-Mead: x = {result_nm.x}, f(x) = {result_nm.fun:.2e}")

    # BFGS (quasi-Newton)
    result_bfgs = optimize.minimize(
        rosenbrock, x0, method='BFGS', jac=rosenbrock_grad
    )
    print(f"BFGS:        x = {result_bfgs.x}, f(x) = {result_bfgs.fun:.2e}")

    # L-BFGS-B (bounded)
    bounds = [(-5, 5), (-5, 5)]
    result_lbfgsb = optimize.minimize(
        rosenbrock, x0, method='L-BFGS-B',
        jac=rosenbrock_grad, bounds=bounds
    )
    print(f"L-BFGS-B:    x = {result_lbfgsb.x}, f(x) = {result_lbfgsb.fun:.2e}")


def curve_fitting_examples() -> None:
    """Demonstrate curve fitting."""

    print("\nCurve Fitting")
    print("=" * 50)

    # Generate noisy data
    np.random.seed(42)
    x_data = np.linspace(0, 10, 50)
    y_true = 2.5 * np.sin(1.5 * x_data) * np.exp(-0.1 * x_data)
    y_data = y_true + 0.3 * np.random.randn(len(x_data))

    # Define model function
    def model(x: np.ndarray, a: float, b: float, c: float) -> np.ndarray:
        """Model: y = a * sin(b*x) * exp(-c*x)"""
        return a * np.sin(b * x) * np.exp(-c * x)

    # Fit using least squares
    popt, pcov = optimize.curve_fit(model, x_data, y_data, p0=[1, 1, 0.1])

    print(f"True parameters:    a=2.5, b=1.5, c=0.1")
    print(f"Fitted parameters:  a={popt[0]:.3f}, b={popt[1]:.3f}, c={popt[2]:.3f}")
    print(f"Parameter uncertainties: {np.sqrt(np.diag(pcov))}")

    # Calculate R-squared
    y_fit = model(x_data, *popt)
    ss_res = np.sum((y_data - y_fit)**2)
    ss_tot = np.sum((y_data - np.mean(y_data))**2)
    r_squared = 1 - ss_res / ss_tot
    print(f"R-squared: {r_squared:.4f}")


def constrained_optimization() -> None:
    """Demonstrate constrained optimization."""

    print("\nConstrained Optimization")
    print("=" * 50)

    # Minimize f(x,y) = x^2 + y^2
    # Subject to: x + y >= 1
    #            x >= 0, y >= 0

    def objective(x: np.ndarray) -> float:
        return x[0]**2 + x[1]**2

    def constraint(x: np.ndarray) -> float:
        return x[0] + x[1] - 1  # >= 0

    # Define constraints
    constraints = {'type': 'ineq', 'fun': constraint}
    bounds = [(0, None), (0, None)]

    result = optimize.minimize(
        objective,
        x0=[0.5, 0.5],
        method='SLSQP',
        bounds=bounds,
        constraints=constraints
    )

    print(f"Problem: min x^2 + y^2 s.t. x+y >= 1, x,y >= 0")
    print(f"Solution: x = {result.x[0]:.6f}, y = {result.x[1]:.6f}")
    print(f"Objective: {result.fun:.6f}")
    print(f"Constraint satisfied: x+y = {result.x[0] + result.x[1]:.6f}")
```

### 4.2 Integracion Numerica

```python
"""
Numerical Integration with SciPy.

Equivalent to Fortran quadrature routines from QUADPACK.
"""

import numpy as np
from scipy import integrate
from typing import Callable

def scalar_integration() -> None:
    """Demonstrate scalar function integration."""

    print("Scalar Integration")
    print("=" * 50)

    # Function to integrate
    def f(x: float) -> float:
        """f(x) = sin(x) * exp(-x/5)"""
        return np.sin(x) * np.exp(-x / 5)

    # Definite integral
    result, error = integrate.quad(f, 0, 10)
    print(f"Integral of sin(x)*exp(-x/5) from 0 to 10:")
    print(f"  Result: {result:.10f}")
    print(f"  Error estimate: {error:.2e}")

    # Improper integral (infinite bounds)
    def gaussian(x: float) -> float:
        return np.exp(-x**2)

    result_inf, _ = integrate.quad(gaussian, -np.inf, np.inf)
    print(f"\nGaussian integral (-inf to inf): {result_inf:.10f}")
    print(f"  Analytical value: sqrt(pi) = {np.sqrt(np.pi):.10f}")

    # Integral with parameters
    def exponential(x: float, rate: float) -> float:
        return rate * np.exp(-rate * x)

    result_param, _ = integrate.quad(exponential, 0, np.inf, args=(2.0,))
    print(f"\nExponential integral (rate=2): {result_param:.10f}")


def double_integration() -> None:
    """Demonstrate double integration."""

    print("\nDouble Integration")
    print("=" * 50)

    # Integrate f(x,y) = x*y over unit square
    def f(y: float, x: float) -> float:  # Note: y first for dblquad
        return x * y

    result, error = integrate.dblquad(f, 0, 1, 0, 1)
    print(f"Integral of x*y over [0,1]x[0,1]: {result:.10f}")
    print(f"  Analytical value: 0.25")

    # Integration over non-rectangular region
    # Integrate over triangle: 0 <= x <= 1, 0 <= y <= x
    def lower_limit(x: float) -> float:
        return 0

    def upper_limit(x: float) -> float:
        return x

    result_tri, _ = integrate.dblquad(f, 0, 1, lower_limit, upper_limit)
    print(f"\nIntegral over triangle (y<=x): {result_tri:.10f}")


def ode_integration() -> None:
    """
    Demonstrate ODE integration.

    Replaces Fortran ODE solvers (e.g., ODEPACK, RKF45).
    """

    print("\nODE Integration")
    print("=" * 50)

    # Simple harmonic oscillator: y'' + y = 0
    # Convert to first-order system:
    # y0' = y1
    # y1' = -y0

    def harmonic_oscillator(t: float, y: np.ndarray) -> np.ndarray:
        """Harmonic oscillator ODE system."""
        return np.array([y[1], -y[0]])

    # Initial conditions: y(0) = 1, y'(0) = 0
    y0 = np.array([1.0, 0.0])
    t_span = (0, 10 * np.pi)
    t_eval = np.linspace(*t_span, 1000)

    # Solve using default RK45
    solution = integrate.solve_ivp(
        harmonic_oscillator,
        t_span,
        y0,
        t_eval=t_eval,
        method='RK45'
    )

    print(f"Harmonic oscillator: y'' + y = 0, y(0)=1, y'(0)=0")
    print(f"  Solution at t=pi: y = {solution.y[0, 100]:.6f} (analytical: -1)")
    print(f"  Solution at t=2pi: y = {solution.y[0, 200]:.6f} (analytical: 1)")

    # Stiff system example - Van der Pol oscillator
    mu = 1000  # Stiffness parameter

    def van_der_pol(t: float, y: np.ndarray) -> np.ndarray:
        """Van der Pol oscillator (stiff for large mu)."""
        return np.array([
            y[1],
            mu * (1 - y[0]**2) * y[1] - y[0]
        ])

    # Use stiff solver (BDF)
    solution_stiff = integrate.solve_ivp(
        van_der_pol,
        (0, 3000),
        [2.0, 0.0],
        method='BDF',
        dense_output=True
    )

    print(f"\nVan der Pol oscillator (mu={mu}, stiff):")
    print(f"  Integration successful: {solution_stiff.success}")
    print(f"  Number of evaluations: {solution_stiff.nfev}")
```

---

## 5. PANDAS: DATAFRAMES PARA PROCESAMIENTO DE DATOS

### 5.1 Fundamentos de DataFrames

```python
"""
Pandas DataFrames - Tabular data processing.

DataFrames provide:
- Labeled rows and columns
- Missing data handling
- Rich I/O (CSV, Excel, HDF5, SQL, etc.)
- Grouping and aggregation
- Time series functionality

Replaces custom Fortran record-reading routines.
"""

import pandas as pd
import numpy as np
from typing import Any

def create_dataframes() -> None:
    """Demonstrate DataFrame creation."""

    # From dictionary
    data = {
        'time': [0.0, 0.1, 0.2, 0.3, 0.4],
        'position': [0.0, 0.98, 1.92, 2.82, 3.68],
        'velocity': [10.0, 9.8, 9.6, 9.4, 9.2],
        'acceleration': [-9.8, -9.8, -9.8, -9.8, -9.8]
    }
    df = pd.DataFrame(data)
    print("DataFrame from dict:")
    print(df)
    print(f"\nData types:\n{df.dtypes}\n")

    # From NumPy array
    arr = np.random.randn(5, 3)
    df_numpy = pd.DataFrame(
        arr,
        columns=['x', 'y', 'z'],
        index=['p1', 'p2', 'p3', 'p4', 'p5']
    )
    print("DataFrame from NumPy array:")
    print(df_numpy)

    # From CSV file (example structure)
    # df = pd.read_csv('data.csv')

    # From Fortran-style fixed-width format
    # df = pd.read_fwf('fortran_output.dat', colspecs=[(0,10), (10,20)])


def dataframe_operations() -> None:
    """Demonstrate DataFrame operations."""

    # Create sample DataFrame
    np.random.seed(42)
    n = 100
    df = pd.DataFrame({
        'experiment_id': range(1, n + 1),
        'temperature': np.random.uniform(20, 30, n),
        'pressure': np.random.uniform(1, 2, n),
        'concentration': np.random.exponential(0.1, n),
        'result': np.random.choice(['success', 'failure'], n, p=[0.8, 0.2])
    })

    print("Sample DataFrame:")
    print(df.head())
    print(f"\nShape: {df.shape}")
    print(f"\nStatistics:\n{df.describe()}")

    # Selection
    print("\nSelection examples:")
    print(f"First 3 rows:\n{df.iloc[:3]}")
    print(f"\nColumn 'temperature':\n{df['temperature'].head()}")
    print(f"\nMultiple columns:\n{df[['temperature', 'pressure']].head()}")

    # Boolean indexing
    high_temp = df[df['temperature'] > 25]
    print(f"\nRows with temperature > 25: {len(high_temp)}")

    # Compound conditions
    filtered = df[(df['temperature'] > 25) & (df['pressure'] < 1.5)]
    print(f"Rows with temp>25 AND pressure<1.5: {len(filtered)}")


def grouping_and_aggregation() -> None:
    """Demonstrate grouping and aggregation."""

    # Create sample data
    df = pd.DataFrame({
        'material': ['steel', 'aluminum', 'steel', 'copper', 'aluminum'] * 20,
        'test_type': ['tensile', 'compression'] * 50,
        'strength': np.random.uniform(100, 500, 100),
        'elongation': np.random.uniform(0.01, 0.3, 100)
    })

    print("Grouping and Aggregation")
    print("=" * 50)

    # Group by single column
    by_material = df.groupby('material')['strength'].agg(['mean', 'std', 'count'])
    print(f"Strength by material:\n{by_material}\n")

    # Group by multiple columns
    by_both = df.groupby(['material', 'test_type']).agg({
        'strength': ['mean', 'std'],
        'elongation': ['mean', 'max']
    })
    print(f"By material and test type:\n{by_both}")

    # Pivot table
    pivot = df.pivot_table(
        values='strength',
        index='material',
        columns='test_type',
        aggfunc=['mean', 'count']
    )
    print(f"\nPivot table:\n{pivot}")


def time_series_operations() -> None:
    """Demonstrate time series functionality."""

    print("\nTime Series Operations")
    print("=" * 50)

    # Create time series
    dates = pd.date_range('2024-01-01', periods=100, freq='h')
    ts = pd.DataFrame({
        'timestamp': dates,
        'temperature': 20 + 5*np.sin(np.arange(100) * 2*np.pi/24) + np.random.randn(100)*0.5,
        'humidity': 60 + 10*np.cos(np.arange(100) * 2*np.pi/24) + np.random.randn(100)*2
    })
    ts.set_index('timestamp', inplace=True)

    print("Time series sample:")
    print(ts.head(10))

    # Resampling
    daily = ts.resample('D').agg({
        'temperature': ['mean', 'min', 'max'],
        'humidity': 'mean'
    })
    print(f"\nDaily resampled:\n{daily}")

    # Rolling statistics
    ts['temp_rolling_mean'] = ts['temperature'].rolling(window=6).mean()
    ts['temp_rolling_std'] = ts['temperature'].rolling(window=6).std()
    print(f"\nWith rolling statistics:\n{ts.head(10)}")
```

---

## 6. MATPLOTLIB Y VISUALIZACION

### 6.1 Graficas Basicas

```python
"""
Matplotlib - Scientific Visualization.

Provides publication-quality plots, replacing:
- GNUPLOT
- Fortran graphics libraries
- Custom PostScript generation
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.figure import Figure
from matplotlib.axes import Axes

def basic_line_plots() -> None:
    """Demonstrate basic line plots."""

    # Generate data
    x = np.linspace(0, 2*np.pi, 100)
    y1 = np.sin(x)
    y2 = np.cos(x)
    y3 = np.sin(2*x)

    # Create figure and axes
    fig, ax = plt.subplots(figsize=(10, 6))

    # Plot multiple lines
    ax.plot(x, y1, 'b-', label='sin(x)', linewidth=2)
    ax.plot(x, y2, 'r--', label='cos(x)', linewidth=2)
    ax.plot(x, y3, 'g:', label='sin(2x)', linewidth=2)

    # Formatting
    ax.set_xlabel('x (radians)', fontsize=12)
    ax.set_ylabel('y', fontsize=12)
    ax.set_title('Trigonometric Functions', fontsize=14)
    ax.legend(loc='upper right')
    ax.grid(True, alpha=0.3)
    ax.set_xlim(0, 2*np.pi)
    ax.set_ylim(-1.5, 1.5)

    plt.tight_layout()
    plt.savefig('trig_functions.png', dpi=150)
    plt.close()


def scientific_subplots() -> None:
    """Demonstrate subplot layouts for scientific data."""

    # Generate simulation-like data
    t = np.linspace(0, 10, 500)
    position = np.sin(t) * np.exp(-0.1*t)
    velocity = np.cos(t) * np.exp(-0.1*t) - 0.1 * np.sin(t) * np.exp(-0.1*t)
    energy = 0.5 * (position**2 + velocity**2)

    # Create subplot grid
    fig, axes = plt.subplots(3, 1, figsize=(10, 8), sharex=True)

    # Position plot
    axes[0].plot(t, position, 'b-', linewidth=1.5)
    axes[0].set_ylabel('Position')
    axes[0].set_title('Damped Oscillator Simulation')
    axes[0].grid(True, alpha=0.3)

    # Velocity plot
    axes[1].plot(t, velocity, 'r-', linewidth=1.5)
    axes[1].set_ylabel('Velocity')
    axes[1].grid(True, alpha=0.3)

    # Energy plot
    axes[2].plot(t, energy, 'g-', linewidth=1.5)
    axes[2].set_xlabel('Time')
    axes[2].set_ylabel('Energy')
    axes[2].grid(True, alpha=0.3)

    plt.tight_layout()
    plt.savefig('oscillator_simulation.png', dpi=150)
    plt.close()


def contour_and_surface_plots() -> None:
    """Demonstrate 2D and 3D plots for field data."""

    # Generate 2D field data
    x = np.linspace(-2, 2, 100)
    y = np.linspace(-2, 2, 100)
    X, Y = np.meshgrid(x, y)
    Z = np.sin(X**2 + Y**2) * np.exp(-(X**2 + Y**2)/3)

    # Contour plot
    fig, axes = plt.subplots(1, 2, figsize=(12, 5))

    # Filled contours
    cf = axes[0].contourf(X, Y, Z, levels=20, cmap='viridis')
    axes[0].contour(X, Y, Z, levels=10, colors='black', linewidths=0.5)
    axes[0].set_xlabel('x')
    axes[0].set_ylabel('y')
    axes[0].set_title('Contour Plot')
    axes[0].set_aspect('equal')
    plt.colorbar(cf, ax=axes[0], label='f(x,y)')

    # Pseudocolor plot
    pc = axes[1].pcolormesh(X, Y, Z, shading='auto', cmap='RdBu')
    axes[1].set_xlabel('x')
    axes[1].set_ylabel('y')
    axes[1].set_title('Pseudocolor Plot')
    axes[1].set_aspect('equal')
    plt.colorbar(pc, ax=axes[1], label='f(x,y)')

    plt.tight_layout()
    plt.savefig('contour_plots.png', dpi=150)
    plt.close()

    # 3D surface plot
    from mpl_toolkits.mplot3d import Axes3D

    fig = plt.figure(figsize=(10, 7))
    ax = fig.add_subplot(111, projection='3d')

    surf = ax.plot_surface(X, Y, Z, cmap='viridis', edgecolor='none', alpha=0.8)
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('f(x,y)')
    ax.set_title('3D Surface Plot')
    fig.colorbar(surf, shrink=0.5, aspect=5)

    plt.tight_layout()
    plt.savefig('surface_plot.png', dpi=150)
    plt.close()
```

---

## 7. COMPARACION CON ARRAYS FORTRAN

### 7.1 Orden de Memoria

```python
"""
Memory Layout: Fortran vs C vs NumPy.

Understanding memory layout is crucial when:
- Bridging to Fortran code
- Optimizing cache performance
- Working with external libraries
"""

import numpy as np

def demonstrate_memory_order() -> None:
    """Demonstrate memory ordering differences."""

    print("Memory Order Comparison")
    print("=" * 50)

    # Create 2D array
    arr_c = np.array([[1, 2, 3],
                      [4, 5, 6]], dtype=np.float64, order='C')

    arr_f = np.array([[1, 2, 3],
                      [4, 5, 6]], dtype=np.float64, order='F')

    print(f"Array shape: {arr_c.shape}")
    print(f"\nC-order (row-major) - memory layout:")
    print(f"  arr_c.flags['C_CONTIGUOUS']: {arr_c.flags['C_CONTIGUOUS']}")
    print(f"  Memory: {arr_c.flatten('K')}")
    print(f"  Strides: {arr_c.strides}")

    print(f"\nFortran-order (column-major) - memory layout:")
    print(f"  arr_f.flags['F_CONTIGUOUS']: {arr_f.flags['F_CONTIGUOUS']}")
    print(f"  Memory: {arr_f.flatten('K')}")
    print(f"  Strides: {arr_f.strides}")

    # Index correspondence
    print("\nIndex Correspondence:")
    print("NumPy arr[i,j] = Fortran arr(j+1, i+1)")
    print("  C-order: arr[row, col] - fastest varying is last index")
    print("  F-order: arr(col, row) - fastest varying is first index")


def efficient_array_access() -> None:
    """Demonstrate efficient array traversal patterns."""

    import time

    n = 1000
    arr = np.random.randn(n, n)

    # Row-wise iteration (efficient for C-order)
    start = time.perf_counter()
    total_row = 0.0
    for i in range(n):
        total_row += np.sum(arr[i, :])  # Access full row
    time_row = time.perf_counter() - start

    # Column-wise iteration (efficient for F-order)
    start = time.perf_counter()
    total_col = 0.0
    for j in range(n):
        total_col += np.sum(arr[:, j])  # Access full column
    time_col = time.perf_counter() - start

    print(f"Array traversal ({n}x{n}):")
    print(f"  Row-wise (C-order optimal): {time_row*1000:.2f} ms")
    print(f"  Column-wise:                {time_col*1000:.2f} ms")
    print(f"  Ratio: {time_col/time_row:.2f}x")
```

### 7.2 Tabla de Equivalencias Fortran-NumPy

```python
"""
Fortran to NumPy Translation Reference.

This serves as a quick reference for migrating Fortran code.
"""

FORTRAN_NUMPY_EQUIVALENTS = """
Array Creation:
--------------
Fortran: REAL(8), DIMENSION(100)           -> NumPy: np.zeros(100, dtype=np.float64)
Fortran: REAL(8), DIMENSION(10,20)         -> NumPy: np.zeros((10, 20), dtype=np.float64)
Fortran: INTEGER, DIMENSION(50)            -> NumPy: np.zeros(50, dtype=np.int32)
Fortran: COMPLEX(8), DIMENSION(30)         -> NumPy: np.zeros(30, dtype=np.complex128)
Fortran: arr = 0.0D0                       -> NumPy: arr = np.zeros_like(arr)
Fortran: arr = 1.0D0                       -> NumPy: arr = np.ones_like(arr)

Array Indexing:
--------------
Fortran: arr(i)                            -> NumPy: arr[i-1]  # 0-based indexing!
Fortran: arr(i,j)                          -> NumPy: arr[i-1, j-1]
Fortran: arr(1:10)                         -> NumPy: arr[0:10] or arr[:10]
Fortran: arr(1:10:2)                       -> NumPy: arr[0:10:2]
Fortran: arr(:,j)                          -> NumPy: arr[:, j-1]
Fortran: arr(i,:)                          -> NumPy: arr[i-1, :]

Intrinsic Functions:
-------------------
Fortran: SUM(arr)                          -> NumPy: np.sum(arr)
Fortran: SUM(arr, DIM=1)                   -> NumPy: np.sum(arr, axis=0)  # Different convention!
Fortran: PRODUCT(arr)                      -> NumPy: np.prod(arr)
Fortran: MINVAL(arr)                       -> NumPy: np.min(arr)
Fortran: MAXVAL(arr)                       -> NumPy: np.max(arr)
Fortran: MINLOC(arr)                       -> NumPy: np.argmin(arr) + 1
Fortran: MAXLOC(arr)                       -> NumPy: np.argmax(arr) + 1
Fortran: SIZE(arr)                         -> NumPy: arr.size
Fortran: SIZE(arr, DIM=1)                  -> NumPy: arr.shape[0]
Fortran: SHAPE(arr)                        -> NumPy: arr.shape
Fortran: RESHAPE(arr, [m, n])              -> NumPy: arr.reshape((m, n), order='F')
Fortran: TRANSPOSE(arr)                    -> NumPy: arr.T

Array Operations:
----------------
Fortran: arr1 + arr2                       -> NumPy: arr1 + arr2
Fortran: arr1 * arr2                       -> NumPy: arr1 * arr2 (element-wise)
Fortran: MATMUL(A, B)                      -> NumPy: A @ B or np.matmul(A, B)
Fortran: DOT_PRODUCT(a, b)                 -> NumPy: np.dot(a, b)

Special Constructs:
------------------
Fortran: WHERE (arr > 0) arr = 1           -> NumPy: arr[arr > 0] = 1
Fortran: PACK(arr, mask)                   -> NumPy: arr[mask]
Fortran: MERGE(arr1, arr2, mask)           -> NumPy: np.where(mask, arr1, arr2)
Fortran: SPREAD(arr, DIM, NCOPIES)         -> NumPy: np.broadcast_to() or np.tile()
Fortran: CSHIFT(arr, SHIFT)                -> NumPy: np.roll(arr, shift)
Fortran: EOSHIFT(arr, SHIFT)               -> NumPy: Custom (no direct equivalent)

LAPACK/BLAS:
-----------
Fortran: DGEMM                             -> NumPy: np.matmul() or @ operator
Fortran: DGEMV                             -> NumPy: np.matmul() or @ operator
Fortran: DGESV                             -> SciPy: scipy.linalg.solve()
Fortran: DGELS                             -> SciPy: scipy.linalg.lstsq()
Fortran: DSYEV                             -> SciPy: scipy.linalg.eigh()
Fortran: DGESVD                            -> SciPy: scipy.linalg.svd()
"""

print(FORTRAN_NUMPY_EQUIVALENTS)
```

---

## 8. CONSIDERACIONES DE RENDIMIENTO

### 8.1 Optimizacion de Codigo NumPy

```python
"""
Performance Optimization for NumPy Code.

Key principles:
1. Avoid Python loops - use vectorized operations
2. Preallocate arrays
3. Use appropriate data types
4. Understand memory access patterns
5. Use specialized functions when available
"""

import numpy as np
import time
from typing import Callable

def benchmark(func: Callable, *args, repeats: int = 10) -> float:
    """Benchmark function execution time."""
    times = []
    for _ in range(repeats):
        start = time.perf_counter()
        func(*args)
        times.append(time.perf_counter() - start)
    return np.median(times)


def optimization_examples() -> None:
    """Demonstrate optimization techniques."""

    print("Optimization Examples")
    print("=" * 50)

    n = 10000

    # Example 1: Avoid growing lists
    def slow_list_growth():
        result = []
        for i in range(n):
            result.append(i ** 2)
        return np.array(result)

    def preallocated_array():
        result = np.empty(n, dtype=np.float64)
        for i in range(n):
            result[i] = i ** 2
        return result

    def vectorized():
        return np.arange(n, dtype=np.float64) ** 2

    t_slow = benchmark(slow_list_growth)
    t_prealloc = benchmark(preallocated_array)
    t_vec = benchmark(vectorized)

    print(f"\nSquaring numbers 0 to {n-1}:")
    print(f"  List growth:   {t_slow*1000:.3f} ms")
    print(f"  Preallocated:  {t_prealloc*1000:.3f} ms ({t_slow/t_prealloc:.1f}x faster)")
    print(f"  Vectorized:    {t_vec*1000:.3f} ms ({t_slow/t_vec:.1f}x faster)")

    # Example 2: Use specialized functions
    x = np.random.randn(n)

    def manual_norm():
        return np.sqrt(np.sum(x ** 2))

    def builtin_norm():
        return np.linalg.norm(x)

    t_manual = benchmark(manual_norm, repeats=100)
    t_builtin = benchmark(builtin_norm, repeats=100)

    print(f"\nVector norm calculation:")
    print(f"  Manual:  {t_manual*1000:.4f} ms")
    print(f"  Built-in: {t_builtin*1000:.4f} ms ({t_manual/t_builtin:.1f}x faster)")

    # Example 3: Avoid temporary arrays
    a = np.random.randn(n)
    b = np.random.randn(n)
    c = np.random.randn(n)

    def with_temps():
        return a + b + c + a*b + b*c  # Creates many temporaries

    def in_place():
        result = a.copy()
        result += b
        result += c
        temp = a * b
        result += temp
        temp = b * c
        result += temp
        return result

    def numexpr_style():
        # Using numexpr would be: ne.evaluate('a + b + c + a*b + b*c')
        # Without numexpr, at least minimize operations
        return a + b + c + a*b + b*c

    t_temps = benchmark(with_temps, repeats=100)
    t_inplace = benchmark(in_place, repeats=100)

    print(f"\nCompound expression (a + b + c + a*b + b*c):")
    print(f"  With temporaries: {t_temps*1000:.4f} ms")
    print(f"  In-place ops:     {t_inplace*1000:.4f} ms")


def memory_efficiency() -> None:
    """Demonstrate memory-efficient practices."""

    print("\nMemory Efficiency")
    print("=" * 50)

    # Use appropriate dtypes
    n = 1_000_000

    arr_float64 = np.zeros(n, dtype=np.float64)
    arr_float32 = np.zeros(n, dtype=np.float32)
    arr_int64 = np.zeros(n, dtype=np.int64)
    arr_int32 = np.zeros(n, dtype=np.int32)
    arr_int8 = np.zeros(n, dtype=np.int8)

    print(f"Memory for {n:,} elements:")
    print(f"  float64: {arr_float64.nbytes / 1024 / 1024:.2f} MB")
    print(f"  float32: {arr_float32.nbytes / 1024 / 1024:.2f} MB")
    print(f"  int64:   {arr_int64.nbytes / 1024 / 1024:.2f} MB")
    print(f"  int32:   {arr_int32.nbytes / 1024 / 1024:.2f} MB")
    print(f"  int8:    {arr_int8.nbytes / 1024 / 1024:.2f} MB")

    # Views vs copies
    original = np.random.randn(1000, 1000)

    view = original[:100, :100]  # No memory copy
    copy = original[:100, :100].copy()  # Memory copy

    print(f"\nViews vs Copies (1000x1000 float64):")
    print(f"  Original size: {original.nbytes / 1024 / 1024:.2f} MB")
    print(f"  View is same memory: {np.shares_memory(original, view)}")
    print(f"  Copy shares memory: {np.shares_memory(original, copy)}")
```

---

## 9. PATRONES COMUNES Y MEJORES PRACTICAS

### 9.1 Patrones de Codigo Cientifico

```python
"""
Common Patterns in Scientific Python Code.
"""

import numpy as np
from numpy.typing import NDArray
from dataclasses import dataclass
from typing import Protocol, Callable

# Pattern 1: Numerical Method as Class
@dataclass
class IntegrationResult:
    """Result of numerical integration."""
    value: float
    error: float
    n_evaluations: int
    converged: bool

class NumericalIntegrator:
    """
    Numerical integrator using adaptive quadrature.

    Encapsulates integration algorithm with consistent interface.
    """

    def __init__(
        self,
        tolerance: float = 1e-10,
        max_evaluations: int = 10000
    ) -> None:
        self.tolerance = tolerance
        self.max_evaluations = max_evaluations

    def integrate(
        self,
        func: Callable[[float], float],
        a: float,
        b: float
    ) -> IntegrationResult:
        """Integrate function from a to b."""
        from scipy import integrate

        result, error, info = integrate.quad(
            func, a, b,
            full_output=True,
            limit=self.max_evaluations
        )

        n_evals = info.get('neval', 0)
        converged = error < self.tolerance * abs(result)

        return IntegrationResult(
            value=result,
            error=error,
            n_evaluations=n_evals,
            converged=converged
        )


# Pattern 2: Factory Function for Matrices
def create_test_matrix(
    size: int,
    matrix_type: str = "symmetric_positive_definite"
) -> NDArray[np.float64]:
    """
    Factory function for creating test matrices.

    Args:
        size: Matrix dimension
        matrix_type: Type of matrix to create

    Returns:
        Requested matrix type

    Available types:
        - symmetric_positive_definite
        - hilbert
        - tridiagonal
        - random_orthogonal
    """
    if matrix_type == "symmetric_positive_definite":
        A = np.random.randn(size, size)
        return A @ A.T + size * np.eye(size)

    elif matrix_type == "hilbert":
        # Hilbert matrix - classic ill-conditioned example
        i, j = np.ogrid[1:size+1, 1:size+1]
        return 1.0 / (i + j - 1)

    elif matrix_type == "tridiagonal":
        return (np.diag(np.full(size, 4.0)) +
                np.diag(np.full(size-1, -1.0), 1) +
                np.diag(np.full(size-1, -1.0), -1))

    elif matrix_type == "random_orthogonal":
        A = np.random.randn(size, size)
        Q, _ = np.linalg.qr(A)
        return Q

    else:
        raise ValueError(f"Unknown matrix type: {matrix_type}")


# Pattern 3: Context Manager for Timing
from contextlib import contextmanager
from typing import Iterator

@contextmanager
def timer(name: str = "Operation") -> Iterator[dict]:
    """
    Context manager for timing code blocks.

    Usage:
        with timer("Matrix multiply") as t:
            result = A @ B
        print(f"Took {t['elapsed']:.3f} seconds")
    """
    import time
    result = {"name": name, "elapsed": 0.0}
    start = time.perf_counter()
    try:
        yield result
    finally:
        result["elapsed"] = time.perf_counter() - start
        print(f"{name}: {result['elapsed']:.4f} seconds")


# Pattern 4: Validation Decorators
from functools import wraps

def validate_positive_definite(func):
    """Decorator to validate matrix is positive definite."""
    @wraps(func)
    def wrapper(matrix: NDArray, *args, **kwargs):
        # Quick check using eigenvalues
        eigenvalues = np.linalg.eigvalsh(matrix)
        if not np.all(eigenvalues > 0):
            raise ValueError("Matrix is not positive definite")
        return func(matrix, *args, **kwargs)
    return wrapper

@validate_positive_definite
def cholesky_decomposition(matrix: NDArray) -> NDArray:
    """Compute Cholesky decomposition."""
    return np.linalg.cholesky(matrix)
```

### 9.2 Estructura Recomendada para Proyectos Cientificos

```python
"""
Recommended Project Structure for Scientific Python.

project/
├── pyproject.toml           # Project configuration
├── README.md                 # Documentation
├── src/
│   └── project_name/
│       ├── __init__.py
│       ├── core/            # Core algorithms
│       │   ├── __init__.py
│       │   ├── solvers.py
│       │   └── integrators.py
│       ├── models/          # Data structures
│       │   ├── __init__.py
│       │   └── simulation.py
│       ├── io/              # I/O utilities
│       │   ├── __init__.py
│       │   ├── readers.py
│       │   └── writers.py
│       └── utils/           # Helper functions
│           ├── __init__.py
│           └── validation.py
├── tests/
│   ├── __init__.py
│   ├── conftest.py          # Pytest fixtures
│   ├── test_solvers.py
│   └── test_integrators.py
├── examples/
│   └── basic_usage.py
└── docs/
    └── api/
"""

# Example conftest.py with fixtures
import pytest
import numpy as np

@pytest.fixture
def random_spd_matrix():
    """Fixture providing random symmetric positive definite matrix."""
    def _make_spd(n: int, seed: int = 42) -> np.ndarray:
        np.random.seed(seed)
        A = np.random.randn(n, n)
        return A @ A.T + n * np.eye(n)
    return _make_spd

@pytest.fixture
def test_tolerance() -> float:
    """Fixture providing standard test tolerance."""
    return 1e-10


# Example test file
def test_solver_convergence(random_spd_matrix, test_tolerance):
    """Test that solver converges for SPD matrix."""
    A = random_spd_matrix(100)
    b = np.ones(100)

    # ... solver code ...
    # x = solve(A, b)

    # assert np.linalg.norm(A @ x - b) < test_tolerance
```

---

## RESUMEN

Este modulo cubre las herramientas fundamentales para computacion cientifica
en Python:

1. **NumPy**: Arrays eficientes, broadcasting, vectorizacion
2. **SciPy**: Algebra lineal, optimizacion, integracion
3. **Pandas**: Procesamiento de datos tabulares
4. **Matplotlib**: Visualizacion cientifica

Puntos clave para migracion desde Fortran/C:
- NumPy usa indexacion 0-based (vs 1-based en Fortran)
- NumPy es row-major por defecto (vs column-major en Fortran)
- Vectorizacion reemplaza bucles explicitos
- SciPy proporciona acceso a las mismas rutinas BLAS/LAPACK
- Type hints mejoran la legibilidad y deteccion de errores

---

*ARCHAEON CORE - Bridging Legacy to Modern*
*Scientific Computing with Python v1.0.0*

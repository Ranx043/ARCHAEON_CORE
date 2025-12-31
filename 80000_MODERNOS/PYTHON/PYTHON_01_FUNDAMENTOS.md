---
title: "PYTHON 01 - Fundamentos de Python Moderno"
version: "1.0.0"
date: "2025-12-31"
author: "ARCHAEON CORE"
classification: "ARCHAEON/80000_MODERNOS/PYTHON"
purpose: "Fundamentos de Python 3.10+ para transicion desde lenguajes legacy"
language_bridge: "Fortran/C -> Python"
keywords:
  - python
  - fundamentos
  - type hints
  - dataclasses
  - package management
  - virtual environments
  - scientific computing
dependencies:
  - python >= 3.10
  - typing
  - dataclasses
status: "ACTIVE"
---

# PYTHON 01 - FUNDAMENTOS DE PYTHON MODERNO

## INDICE

1. [Introduccion a Python Moderno](#1-introduccion-a-python-moderno)
2. [Core Features Python 3.10+](#2-core-features-python-310)
3. [Sistema de Tipos y Type Hints](#3-sistema-de-tipos-y-type-hints)
4. [Dataclasses y Estructuras de Datos](#4-dataclasses-y-estructuras-de-datos)
5. [Protocolos y Duck Typing Estructural](#5-protocolos-y-duck-typing-estructural)
6. [Gestion de Paquetes](#6-gestion-de-paquetes)
7. [Entornos Virtuales](#7-entornos-virtuales)
8. [Mejores Practicas para Codigo Cientifico](#8-mejores-practicas-para-codigo-cientifico)
9. [Tablas de Referencia](#9-tablas-de-referencia)

---

## 1. INTRODUCCION A PYTHON MODERNO

### 1.1 Por Que Python para Computacion Cientifica

Python ha emergido como el lenguaje dominante en computacion cientifica, reemplazando
progresivamente a Fortran y C en muchas aplicaciones. ARCHAEON documenta esta transicion
para facilitar la migracion de codigo legacy.

```python
#!/usr/bin/env python3
"""
ARCHAEON - Python Moderno para Ciencia
Este modulo demuestra las capacidades de Python 3.10+
para reemplazar codigo legacy Fortran/C.
"""

from __future__ import annotations

import sys
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from numpy.typing import NDArray

# Verify Python version
def check_python_version() -> None:
    """Ensure we're running Python 3.10 or higher."""
    if sys.version_info < (3, 10):
        raise RuntimeError(
            f"Python 3.10+ required. Current: {sys.version_info.major}.{sys.version_info.minor}"
        )
    print(f"Python {sys.version_info.major}.{sys.version_info.minor} - Ready for modern science")

if __name__ == "__main__":
    check_python_version()
```

### 1.2 Comparacion con Lenguajes Legacy

| Aspecto | Fortran | C | Python Moderno |
|---------|---------|---|----------------|
| Tipado | Estatico | Estatico | Dinamico + Type Hints |
| Compilacion | Compilado | Compilado | Interpretado + JIT |
| Arrays | Nativo | Manual | NumPy |
| Gestion Memoria | Semi-automatica | Manual | Automatica (GC) |
| Ecosistema | Limitado | Amplio | Muy amplio |
| Curva Aprendizaje | Media | Alta | Baja |
| Rendimiento Crudo | Muy alto | Muy alto | Medio (optimizable) |

### 1.3 Filosofia de ARCHAEON

```python
"""
ARCHAEON Philosophy for Legacy Bridge:

1. PRESERVE: Mantener la precision numerica del codigo legacy
2. MODERNIZE: Usar caracteristicas modernas de Python
3. OPTIMIZE: Aprovechar NumPy/SciPy para rendimiento
4. DOCUMENT: Documentar equivalencias y diferencias
5. TEST: Validar resultados contra implementacion original
"""

class ArchaeonPhilosophy:
    """Core principles for legacy code migration."""

    PRINCIPLES = {
        "preserve": "Maintain numerical precision from legacy code",
        "modernize": "Use modern Python features and idioms",
        "optimize": "Leverage NumPy/SciPy for performance",
        "document": "Document equivalences and differences",
        "test": "Validate results against original implementation"
    }

    @classmethod
    def describe(cls) -> None:
        """Print all ARCHAEON principles."""
        for key, value in cls.PRINCIPLES.items():
            print(f"  {key.upper()}: {value}")
```

---

## 2. CORE FEATURES PYTHON 3.10+

### 2.1 Structural Pattern Matching (match/case)

Introducido en Python 3.10, pattern matching proporciona una alternativa
elegante a cadenas if/elif que eran comunes en codigo legacy.

```python
"""
Pattern Matching - Replacement for complex if/elif chains
Equivalent to Fortran SELECT CASE or C switch statements
"""

from dataclasses import dataclass
from typing import Any

@dataclass
class Vector3D:
    """A 3D vector, similar to Fortran REAL(3) array."""
    x: float
    y: float
    z: float

@dataclass
class Matrix:
    """A matrix with dimensions, similar to Fortran REAL(m,n)."""
    rows: int
    cols: int
    data: list[list[float]]

def process_mathematical_object(obj: Any) -> str:
    """
    Process different mathematical objects using pattern matching.

    Legacy Fortran equivalent:
        SELECT CASE (obj_type)
        CASE (1)
            ! Handle scalar
        CASE (2)
            ! Handle vector
        CASE DEFAULT
            ! Handle unknown
        END SELECT
    """
    match obj:
        case int() | float() as scalar:
            return f"Scalar value: {scalar}"

        case Vector3D(x=0, y=0, z=0):
            return "Zero vector detected"

        case Vector3D(x=x, y=y, z=z) if x == y == z:
            return f"Uniform vector: all components = {x}"

        case Vector3D(x=x, y=y, z=z):
            magnitude = (x**2 + y**2 + z**2) ** 0.5
            return f"Vector with magnitude: {magnitude:.6f}"

        case Matrix(rows=r, cols=c) if r == c:
            return f"Square matrix: {r}x{c}"

        case Matrix(rows=r, cols=c):
            return f"Rectangular matrix: {r}x{c}"

        case [*elements] if all(isinstance(e, (int, float)) for e in elements):
            return f"Numeric list with {len(elements)} elements"

        case {"type": "sparse", "nnz": nnz}:
            return f"Sparse matrix with {nnz} non-zero elements"

        case _:
            return "Unknown mathematical object"

# Usage examples
def demonstrate_pattern_matching() -> None:
    """Demonstrate pattern matching with various objects."""
    objects = [
        42,
        3.14159,
        Vector3D(0, 0, 0),
        Vector3D(1, 1, 1),
        Vector3D(3, 4, 0),
        Matrix(3, 3, [[1, 0, 0], [0, 1, 0], [0, 0, 1]]),
        Matrix(2, 4, [[1, 2, 3, 4], [5, 6, 7, 8]]),
        [1, 2, 3, 4, 5],
        {"type": "sparse", "nnz": 1000},
    ]

    for obj in objects:
        result = process_mathematical_object(obj)
        print(f"  {obj!r:40} -> {result}")
```

### 2.2 Improved Error Messages

Python 3.10+ proporciona mensajes de error mas descriptivos:

```python
"""
Python 3.10+ provides enhanced error messages.
This is crucial when debugging migrated legacy code.
"""

def demonstrate_improved_errors() -> None:
    """
    Examples of improved error messages in Python 3.10+.

    Old Python (< 3.10):
        SyntaxError: invalid syntax

    New Python (>= 3.10):
        SyntaxError: '{' was never closed

    This helps developers migrating from Fortran/C
    who may not be familiar with Python syntax.
    """

    # Example 1: Dictionary not closed
    # data = {"x": 1, "y": 2   # Python 3.10+ shows: '{' was never closed

    # Example 2: Missing comma in list
    # values = [1, 2 3, 4]     # Python 3.10+ shows: perhaps you forgot a comma?

    # Example 3: Missing colon
    # if condition             # Python 3.10+ shows: expected ':'
    #     pass

    print("Python 3.10+ provides context-aware error messages")
    print("This significantly helps when migrating legacy code")
```

### 2.3 Parenthesized Context Managers

```python
"""
Parenthesized Context Managers (Python 3.10+)
Useful for managing multiple resources, similar to Fortran
OPEN statements for multiple files.
"""

from contextlib import contextmanager
from pathlib import Path
from typing import TextIO, Iterator

@contextmanager
def open_data_file(path: Path, mode: str = "r") -> Iterator[TextIO]:
    """
    Context manager for data files with legacy format support.

    Fortran equivalent:
        OPEN(UNIT=10, FILE='input.dat', STATUS='OLD')
        OPEN(UNIT=20, FILE='output.dat', STATUS='REPLACE')
        ! ... operations ...
        CLOSE(10)
        CLOSE(20)
    """
    file_handle = open(path, mode, encoding="utf-8")
    try:
        yield file_handle
    finally:
        file_handle.close()

def process_multiple_files(
    input_path: Path,
    output_path: Path,
    log_path: Path
) -> None:
    """
    Process multiple files simultaneously using parenthesized context managers.
    Python 3.10+ allows cleaner multi-line context managers.
    """
    # Python 3.10+ syntax - parenthesized context managers
    with (
        open_data_file(input_path, "r") as input_file,
        open_data_file(output_path, "w") as output_file,
        open_data_file(log_path, "a") as log_file,
    ):
        for line_num, line in enumerate(input_file, 1):
            processed = line.strip().upper()
            output_file.write(f"{processed}\n")
            log_file.write(f"Processed line {line_num}\n")
```

### 2.4 Union Types with Pipe Operator

```python
"""
Union Types with | operator (Python 3.10+)
Cleaner syntax for type hints, replacing Union[X, Y].
"""

from typing import Union  # Still available but | is preferred

# Old style (Python 3.9 and earlier)
def old_style_function(value: Union[int, float, str]) -> Union[int, float]:
    """Legacy union type syntax."""
    if isinstance(value, str):
        return float(value)
    return value

# New style (Python 3.10+)
def new_style_function(value: int | float | str) -> int | float:
    """
    Modern union type syntax using pipe operator.

    This is equivalent to:
        Fortran: REAL or INTEGER (no direct equivalent)
        C: union { int i; float f; } (different semantics)
    """
    if isinstance(value, str):
        return float(value)
    return value

# Type alias using | operator
Number = int | float | complex
ScientificValue = Number | str | None

def parse_scientific_value(raw: str) -> ScientificValue:
    """
    Parse a value that could be numeric or special string.
    Handles Fortran-style special values like 'NaN', 'Inf'.
    """
    raw = raw.strip().upper()

    match raw:
        case "NAN" | ".NAN" | "NAN()" :
            return float("nan")
        case "INF" | ".INF" | "INFINITY":
            return float("inf")
        case "-INF" | "-.INF" | "-INFINITY":
            return float("-inf")
        case "" | "NULL" | "MISSING":
            return None
        case _:
            try:
                # Try integer first
                if "." not in raw and "E" not in raw:
                    return int(raw)
                return float(raw)
            except ValueError:
                return raw  # Return as string if unparseable
```

### 2.5 Better Type Parameter Syntax

```python
"""
Type Parameter Syntax improvements for generic types.
Essential for scientific libraries that work with various numeric types.
"""

from typing import TypeVar, Generic, Protocol
from collections.abc import Sequence, Callable

# TypeVar for numeric types - constrained to numeric types
T = TypeVar("T", int, float, complex)
NumericT = TypeVar("NumericT", bound="Numeric")

class Numeric(Protocol):
    """Protocol for numeric types - duck typing for numbers."""
    def __add__(self, other: "Numeric") -> "Numeric": ...
    def __sub__(self, other: "Numeric") -> "Numeric": ...
    def __mul__(self, other: "Numeric") -> "Numeric": ...
    def __truediv__(self, other: "Numeric") -> "Numeric": ...

class ScientificArray(Generic[T]):
    """
    A generic scientific array class.

    Fortran equivalent:
        TYPE :: SCIENTIFIC_ARRAY
            REAL(KIND=SELECTED_REAL_KIND(15)), ALLOCATABLE :: data(:)
        END TYPE

    With Python generics, we can parameterize the element type.
    """

    def __init__(self, data: Sequence[T]) -> None:
        self._data: list[T] = list(data)

    def __getitem__(self, index: int) -> T:
        return self._data[index]

    def __len__(self) -> int:
        return len(self._data)

    def map(self, func: Callable[[T], T]) -> "ScientificArray[T]":
        """Apply a function to each element."""
        return ScientificArray([func(x) for x in self._data])

    def reduce(self, func: Callable[[T, T], T], initial: T) -> T:
        """Reduce array to single value using binary function."""
        result = initial
        for item in self._data:
            result = func(result, item)
        return result

    def sum(self) -> T:
        """
        Sum all elements.

        Fortran equivalent: SUM(array)
        """
        if not self._data:
            raise ValueError("Cannot sum empty array")
        return self.reduce(lambda a, b: a + b, self._data[0].__class__(0))
```

---

## 3. SISTEMA DE TIPOS Y TYPE HINTS

### 3.1 Fundamentos de Type Hints

```python
"""
Type Hints - Static type information for dynamic Python.

This bridges the gap between:
- Fortran's strict static typing
- C's static but sometimes lax typing
- Python's dynamic typing

Type hints enable static analysis tools like mypy to catch errors
before runtime, similar to Fortran/C compilation checks.
"""

from typing import (
    Any,
    Optional,
    Final,
    Literal,
    TypedDict,
    NamedTuple,
    Annotated,
    get_type_hints,
    cast,
    overload,
)
from collections.abc import Sequence, Mapping, Iterator, Callable

# Basic type hints
def compute_distance(x1: float, y1: float, x2: float, y2: float) -> float:
    """
    Compute Euclidean distance between two points.

    Fortran equivalent:
        REAL FUNCTION DISTANCE(X1, Y1, X2, Y2)
            REAL, INTENT(IN) :: X1, Y1, X2, Y2
            DISTANCE = SQRT((X2-X1)**2 + (Y2-Y1)**2)
        END FUNCTION

    Args:
        x1: X coordinate of first point
        y1: Y coordinate of first point
        x2: X coordinate of second point
        y2: Y coordinate of second point

    Returns:
        Euclidean distance between points
    """
    return ((x2 - x1) ** 2 + (y2 - y1) ** 2) ** 0.5


# Optional types - None handling
def find_root(coefficients: Sequence[float]) -> float | None:
    """
    Find a root of polynomial, returning None if not found.

    The Optional pattern replaces Fortran's approach of:
        - Using special values (e.g., -9999.0)
        - Using output status flags
        - Using separate logical return values
    """
    # Simplified - real implementation would use numerical methods
    if len(coefficients) < 2:
        return None
    # Linear case: ax + b = 0
    if len(coefficients) == 2:
        a, b = coefficients
        if abs(a) < 1e-15:
            return None
        return -b / a
    return None  # Would implement Newton-Raphson, etc.


# Final - constants that shouldn't be reassigned
# Equivalent to Fortran PARAMETER or C const
PI: Final[float] = 3.141592653589793
E: Final[float] = 2.718281828459045
GOLDEN_RATIO: Final[float] = 1.618033988749895

# Fortran equivalent:
#   REAL, PARAMETER :: PI = 3.141592653589793D0
# C equivalent:
#   const double PI = 3.141592653589793;


# Literal types - restrict to specific values
def set_precision(mode: Literal["single", "double", "quad"]) -> int:
    """
    Set numerical precision mode.

    Literal types ensure only valid values are passed,
    similar to Fortran's SELECT CASE validation.
    """
    precision_bits = {
        "single": 32,
        "double": 64,
        "quad": 128,
    }
    return precision_bits[mode]
```

### 3.2 TypedDict y NamedTuple

```python
"""
TypedDict and NamedTuple - Structured data with type information.

These replace:
- Fortran's TYPE definitions
- C's struct definitions
- Plain Python dicts (which lack structure information)
"""

from typing import TypedDict, NamedTuple, NotRequired
from dataclasses import dataclass

# TypedDict - typed dictionary
class ParticleState(TypedDict):
    """
    State of a particle in simulation.

    Fortran equivalent:
        TYPE :: PARTICLE_STATE
            REAL(8) :: position(3)
            REAL(8) :: velocity(3)
            REAL(8) :: mass
            REAL(8) :: charge
            INTEGER :: particle_id
        END TYPE
    """
    position: tuple[float, float, float]
    velocity: tuple[float, float, float]
    mass: float
    charge: float
    particle_id: int


class ExtendedParticleState(ParticleState, total=False):
    """Extended particle state with optional fields."""
    spin: tuple[float, float, float]
    creation_time: float
    label: str


# Python 3.11+ NotRequired syntax
class ConfigDict(TypedDict):
    """Configuration dictionary with optional fields."""
    name: str
    precision: Literal["single", "double", "quad"]
    max_iterations: int
    tolerance: NotRequired[float]  # Optional field
    debug_mode: NotRequired[bool]


# NamedTuple - immutable structured data
class Point3D(NamedTuple):
    """
    Immutable 3D point.

    Similar to Fortran's derived type but immutable.
    More memory-efficient than dataclass for simple data.
    """
    x: float
    y: float
    z: float

    def distance_to(self, other: "Point3D") -> float:
        """Compute distance to another point."""
        return (
            (self.x - other.x) ** 2 +
            (self.y - other.y) ** 2 +
            (self.z - other.z) ** 2
        ) ** 0.5

    def __add__(self, other: "Point3D") -> "Point3D":
        """Vector addition."""
        return Point3D(
            self.x + other.x,
            self.y + other.y,
            self.z + other.z
        )


class SimulationResult(NamedTuple):
    """
    Result container for numerical simulations.

    Using NamedTuple provides:
    - Type hints for each field
    - Immutability (prevents accidental modification)
    - Tuple unpacking compatibility
    - Memory efficiency
    """
    converged: bool
    iterations: int
    final_value: float
    residual: float
    message: str

    def is_success(self) -> bool:
        """Check if simulation was successful."""
        return self.converged and self.residual < 1e-10


# Usage demonstration
def run_simulation() -> SimulationResult:
    """Run a mock simulation and return structured result."""
    # ... simulation logic ...
    return SimulationResult(
        converged=True,
        iterations=42,
        final_value=3.14159,
        residual=1e-12,
        message="Convergence achieved"
    )
```

### 3.3 Annotated Types

```python
"""
Annotated Types - Adding metadata to type hints.

Useful for:
- Input validation
- Physical units
- Value constraints
- Documentation
"""

from typing import Annotated
from dataclasses import dataclass

# Metadata classes for annotations
@dataclass(frozen=True)
class Unit:
    """Physical unit annotation."""
    name: str
    symbol: str

@dataclass(frozen=True)
class Range:
    """Valid range annotation."""
    min_value: float
    max_value: float

@dataclass(frozen=True)
class Precision:
    """Precision requirement annotation."""
    decimal_places: int

# Type aliases with annotations
Meters = Annotated[float, Unit("meters", "m")]
Seconds = Annotated[float, Unit("seconds", "s")]
Kilograms = Annotated[float, Unit("kilograms", "kg")]
Temperature = Annotated[float, Unit("kelvin", "K"), Range(0, float("inf"))]
Percentage = Annotated[float, Range(0, 100)]
HighPrecision = Annotated[float, Precision(15)]

def compute_kinetic_energy(
    mass: Kilograms,
    velocity: Annotated[float, Unit("meters per second", "m/s")]
) -> Annotated[float, Unit("joules", "J")]:
    """
    Compute kinetic energy: E = 0.5 * m * v^2

    The Annotated types document the expected units,
    similar to Fortran's practice of commenting units:

        ! FUNCTION KINETIC_ENERGY(MASS, VELOCITY)
        ! MASS in kg, VELOCITY in m/s
        ! Returns energy in Joules
    """
    return 0.5 * mass * velocity ** 2


def validate_temperature(temp: Temperature) -> bool:
    """
    Validate temperature is physically meaningful.

    Annotated metadata can be extracted at runtime for validation:
    """
    hints = get_type_hints(validate_temperature, include_extras=True)
    temp_hint = hints["temp"]

    # Extract Range metadata
    for metadata in temp_hint.__metadata__:
        if isinstance(metadata, Range):
            return metadata.min_value <= temp <= metadata.max_value

    return True
```

### 3.4 Function Overloading

```python
"""
Function Overloading with @overload decorator.

Provides multiple type signatures for a single function,
similar to C++ function overloading.
"""

from typing import overload, Sequence
import numpy as np
from numpy.typing import NDArray

@overload
def normalize(data: float) -> float: ...

@overload
def normalize(data: list[float]) -> list[float]: ...

@overload
def normalize(data: NDArray[np.float64]) -> NDArray[np.float64]: ...

def normalize(
    data: float | list[float] | NDArray[np.float64]
) -> float | list[float] | NDArray[np.float64]:
    """
    Normalize data to range [0, 1].

    Overloaded to handle:
    - Single float values
    - Lists of floats
    - NumPy arrays

    This pattern is common when bridging from Fortran,
    where elemental functions can operate on scalars or arrays.

    Fortran equivalent:
        ELEMENTAL REAL FUNCTION NORMALIZE(X)
            REAL, INTENT(IN) :: X
            NORMALIZE = (X - X_MIN) / (X_MAX - X_MIN)
        END FUNCTION
    """
    if isinstance(data, (int, float)):
        # Scalar normalization
        return float(data) / (abs(data) if data != 0 else 1.0)

    elif isinstance(data, list):
        min_val = min(data)
        max_val = max(data)
        range_val = max_val - min_val
        if range_val == 0:
            return [0.5] * len(data)
        return [(x - min_val) / range_val for x in data]

    else:  # NumPy array
        min_val = np.min(data)
        max_val = np.max(data)
        range_val = max_val - min_val
        if range_val == 0:
            return np.full_like(data, 0.5)
        return (data - min_val) / range_val
```

---

## 4. DATACLASSES Y ESTRUCTURAS DE DATOS

### 4.1 Dataclasses Fundamentales

```python
"""
Dataclasses - Modern Python's answer to C structs and Fortran derived types.

Introduced in Python 3.7, enhanced in 3.10+.
Provides automatic generation of:
- __init__
- __repr__
- __eq__
- __hash__ (optional)
- Comparison methods (optional)
"""

from dataclasses import dataclass, field, asdict, astuple, replace
from typing import ClassVar
import json

@dataclass
class Atom:
    """
    Represents an atom in a molecular simulation.

    Fortran equivalent:
        TYPE :: ATOM
            CHARACTER(LEN=2) :: symbol
            REAL(8) :: mass
            REAL(8) :: position(3)
            REAL(8) :: velocity(3)
            INTEGER :: atomic_number
        END TYPE ATOM
    """
    symbol: str
    atomic_number: int
    mass: float
    position: tuple[float, float, float] = (0.0, 0.0, 0.0)
    velocity: tuple[float, float, float] = (0.0, 0.0, 0.0)

    def kinetic_energy(self) -> float:
        """Calculate kinetic energy of atom."""
        v_squared = sum(v ** 2 for v in self.velocity)
        return 0.5 * self.mass * v_squared


@dataclass(frozen=True)
class ImmutablePoint:
    """
    Immutable 3D point - cannot be modified after creation.

    frozen=True makes the dataclass hashable and immutable,
    similar to Fortran's INTENT(IN) for all fields.
    """
    x: float
    y: float
    z: float

    def distance_from_origin(self) -> float:
        """Calculate distance from origin."""
        return (self.x ** 2 + self.y ** 2 + self.z ** 2) ** 0.5


@dataclass(order=True)
class PrioritizedTask:
    """
    Task with automatic comparison based on priority.

    order=True generates __lt__, __le__, __gt__, __ge__ methods.
    Useful for priority queues in simulation scheduling.
    """
    sort_index: float = field(init=False, repr=False)
    priority: int
    name: str
    execution_time: float

    def __post_init__(self) -> None:
        """Set sort index for ordering."""
        # Lower priority number = higher priority
        self.sort_index = self.priority


@dataclass
class SimulationConfig:
    """
    Configuration for numerical simulation with defaults.

    Demonstrates:
    - Default values
    - Factory defaults for mutable types
    - Class variables
    - Post-initialization processing
    """
    # Class variable - shared across all instances
    DEFAULT_PRECISION: ClassVar[str] = "double"

    # Required fields (no defaults)
    name: str
    num_particles: int

    # Optional fields with defaults
    time_step: float = 1e-3
    max_iterations: int = 10000
    tolerance: float = 1e-10
    precision: str = "double"

    # Mutable defaults must use field(default_factory=...)
    boundary_conditions: list[str] = field(
        default_factory=lambda: ["periodic", "periodic", "periodic"]
    )
    output_fields: set[str] = field(
        default_factory=lambda: {"position", "velocity", "energy"}
    )
    metadata: dict[str, str] = field(default_factory=dict)

    # Computed field - excluded from __init__
    total_time: float = field(init=False)

    def __post_init__(self) -> None:
        """Validate and compute derived values."""
        if self.num_particles <= 0:
            raise ValueError(f"num_particles must be positive: {self.num_particles}")
        if self.time_step <= 0:
            raise ValueError(f"time_step must be positive: {self.time_step}")

        self.total_time = self.time_step * self.max_iterations

        # Validate precision
        valid_precisions = {"single", "double", "quad"}
        if self.precision not in valid_precisions:
            raise ValueError(f"precision must be one of {valid_precisions}")

    def to_json(self) -> str:
        """Serialize configuration to JSON."""
        data = asdict(self)
        # Convert set to list for JSON serialization
        data["output_fields"] = list(data["output_fields"])
        return json.dumps(data, indent=2)

    @classmethod
    def from_json(cls, json_str: str) -> "SimulationConfig":
        """Deserialize configuration from JSON."""
        data = json.loads(json_str)
        # Convert list back to set
        data["output_fields"] = set(data["output_fields"])
        # Remove computed fields
        data.pop("total_time", None)
        return cls(**data)


# Usage demonstration
def demonstrate_dataclasses() -> None:
    """Demonstrate dataclass usage patterns."""

    # Create atom
    hydrogen = Atom(
        symbol="H",
        atomic_number=1,
        mass=1.008,
        position=(1.0, 2.0, 3.0),
        velocity=(0.1, 0.2, 0.3)
    )
    print(f"Atom: {hydrogen}")
    print(f"Kinetic energy: {hydrogen.kinetic_energy():.6f}")

    # Immutable point
    origin = ImmutablePoint(0, 0, 0)
    point = ImmutablePoint(3, 4, 0)
    print(f"Distance from origin: {point.distance_from_origin()}")

    # Create modified copy (since frozen, can't modify in place)
    moved_point = replace(point, z=5.0)
    print(f"Moved point: {moved_point}")

    # Configuration with defaults
    config = SimulationConfig(
        name="particle_dynamics",
        num_particles=1000
    )
    print(f"Config: {config.name}, total_time: {config.total_time}")
```

### 4.2 Dataclasses Avanzados

```python
"""
Advanced Dataclass Patterns for Scientific Computing.
"""

from dataclasses import dataclass, field, InitVar
from typing import Any
import numpy as np
from numpy.typing import NDArray

@dataclass
class NumericalGrid:
    """
    Numerical grid for finite difference methods.

    Demonstrates:
    - InitVar for initialization-only variables
    - Factory functions for array creation
    - Computed properties
    """
    # InitVar - used only during initialization, not stored
    x_min: InitVar[float]
    x_max: InitVar[float]
    n_points: InitVar[int]

    # Stored fields
    grid_points: NDArray[np.float64] = field(init=False)
    spacing: float = field(init=False)

    def __post_init__(
        self,
        x_min: float,
        x_max: float,
        n_points: int
    ) -> None:
        """Initialize grid from parameters."""
        self.grid_points = np.linspace(x_min, x_max, n_points)
        self.spacing = (x_max - x_min) / (n_points - 1)

    @property
    def n_points(self) -> int:
        """Number of grid points."""
        return len(self.grid_points)

    def interpolate(self, x: float) -> tuple[int, float]:
        """Find interpolation index and weight for point x."""
        idx = int((x - self.grid_points[0]) / self.spacing)
        idx = max(0, min(idx, self.n_points - 2))
        weight = (x - self.grid_points[idx]) / self.spacing
        return idx, weight


@dataclass
class ComputationResult:
    """
    Result of a numerical computation with metadata.

    Demonstrates slot optimization for memory efficiency.
    """
    __slots__ = ("value", "error", "iterations", "converged", "timing")

    value: float
    error: float
    iterations: int
    converged: bool
    timing: float

    def __repr__(self) -> str:
        status = "CONVERGED" if self.converged else "FAILED"
        return (
            f"ComputationResult({status}, "
            f"value={self.value:.10e}, "
            f"error={self.error:.2e}, "
            f"iterations={self.iterations})"
        )


@dataclass
class Matrix:
    """
    Matrix class demonstrating array handling in dataclasses.

    Fortran equivalent structure:
        TYPE :: MATRIX
            INTEGER :: rows, cols
            REAL(8), ALLOCATABLE :: data(:,:)
        END TYPE
    """
    rows: int
    cols: int
    data: NDArray[np.float64] = field(repr=False)

    @classmethod
    def zeros(cls, rows: int, cols: int) -> "Matrix":
        """Create zero matrix."""
        return cls(rows, cols, np.zeros((rows, cols)))

    @classmethod
    def identity(cls, n: int) -> "Matrix":
        """Create identity matrix."""
        return cls(n, n, np.eye(n))

    @classmethod
    def from_nested_list(cls, data: list[list[float]]) -> "Matrix":
        """Create matrix from nested list."""
        arr = np.array(data, dtype=np.float64)
        return cls(arr.shape[0], arr.shape[1], arr)

    def __matmul__(self, other: "Matrix") -> "Matrix":
        """Matrix multiplication using @ operator."""
        if self.cols != other.rows:
            raise ValueError(
                f"Incompatible dimensions: {self.cols} vs {other.rows}"
            )
        result = self.data @ other.data
        return Matrix(self.rows, other.cols, result)

    def transpose(self) -> "Matrix":
        """Return transposed matrix."""
        return Matrix(self.cols, self.rows, self.data.T.copy())

    def trace(self) -> float:
        """Return matrix trace (sum of diagonal elements)."""
        return float(np.trace(self.data))

    def frobenius_norm(self) -> float:
        """Return Frobenius norm."""
        return float(np.linalg.norm(self.data, "fro"))
```

---

## 5. PROTOCOLOS Y DUCK TYPING ESTRUCTURAL

### 5.1 Protocolos Basicos

```python
"""
Protocols - Structural subtyping (duck typing with type hints).

Protocols define interfaces without requiring inheritance.
This is Python's answer to:
- C++ concepts (C++20)
- Go interfaces
- Haskell type classes

Protocols are particularly useful when wrapping legacy code
that has implicit interface contracts.
"""

from typing import Protocol, runtime_checkable
from abc import abstractmethod
import numpy as np
from numpy.typing import NDArray

class Integrable(Protocol):
    """
    Protocol for objects that can be integrated.

    Any class implementing __call__(x: float) -> float
    automatically satisfies this protocol.
    """

    def __call__(self, x: float) -> float:
        """Evaluate function at point x."""
        ...


class Differentiable(Protocol):
    """Protocol for objects that can be differentiated."""

    def __call__(self, x: float) -> float:
        """Evaluate function at point x."""
        ...

    def derivative(self, x: float) -> float:
        """Evaluate derivative at point x."""
        ...


@runtime_checkable
class ArrayLike(Protocol):
    """
    Protocol for array-like objects.

    @runtime_checkable allows isinstance() checks.
    Matches NumPy arrays, lists, and custom types.
    """

    def __len__(self) -> int: ...
    def __getitem__(self, index: int) -> float: ...


class Solver(Protocol):
    """
    Protocol for numerical solvers.

    This abstracts the interface for various solvers,
    allowing easy substitution (e.g., replace legacy Fortran
    solver with Python implementation).
    """

    def solve(
        self,
        matrix: NDArray[np.float64],
        rhs: NDArray[np.float64]
    ) -> NDArray[np.float64]:
        """Solve linear system Ax = b."""
        ...

    @property
    def name(self) -> str:
        """Solver name for logging."""
        ...

    @property
    def iterations(self) -> int:
        """Number of iterations used."""
        ...


# Concrete implementations
class PolynomialFunction:
    """
    Polynomial function - implements Integrable and Differentiable protocols.

    Note: No explicit inheritance from protocols is needed.
    """

    def __init__(self, coefficients: list[float]) -> None:
        """
        Initialize polynomial with coefficients.

        coefficients[i] is the coefficient of x^i.
        """
        self.coefficients = coefficients

    def __call__(self, x: float) -> float:
        """Evaluate polynomial at x using Horner's method."""
        result = 0.0
        for coef in reversed(self.coefficients):
            result = result * x + coef
        return result

    def derivative(self, x: float) -> float:
        """Evaluate derivative at x."""
        if len(self.coefficients) < 2:
            return 0.0

        # Derivative coefficients
        deriv_coefs = [
            i * c for i, c in enumerate(self.coefficients)
        ][1:]

        # Evaluate using Horner's method
        result = 0.0
        for coef in reversed(deriv_coefs):
            result = result * x + coef
        return result


class LUSolver:
    """LU decomposition solver implementing Solver protocol."""

    def __init__(self) -> None:
        self._iterations = 0

    @property
    def name(self) -> str:
        return "LU Decomposition"

    @property
    def iterations(self) -> int:
        return self._iterations

    def solve(
        self,
        matrix: NDArray[np.float64],
        rhs: NDArray[np.float64]
    ) -> NDArray[np.float64]:
        """Solve using LU decomposition."""
        from scipy.linalg import lu_factor, lu_solve

        lu, piv = lu_factor(matrix)
        self._iterations = 1  # Direct method
        return lu_solve((lu, piv), rhs)


class ConjugateGradientSolver:
    """Conjugate gradient solver implementing Solver protocol."""

    def __init__(self, tolerance: float = 1e-10, max_iter: int = 1000) -> None:
        self.tolerance = tolerance
        self.max_iter = max_iter
        self._iterations = 0

    @property
    def name(self) -> str:
        return "Conjugate Gradient"

    @property
    def iterations(self) -> int:
        return self._iterations

    def solve(
        self,
        matrix: NDArray[np.float64],
        rhs: NDArray[np.float64]
    ) -> NDArray[np.float64]:
        """Solve using conjugate gradient method."""
        from scipy.sparse.linalg import cg

        solution, info = cg(
            matrix, rhs,
            tol=self.tolerance,
            maxiter=self.max_iter
        )
        self._iterations = info if info > 0 else self.max_iter
        return solution


def integrate_function(
    func: Integrable,
    a: float,
    b: float,
    n_points: int = 100
) -> float:
    """
    Integrate function using trapezoidal rule.

    Works with any object implementing Integrable protocol.
    """
    h = (b - a) / n_points
    result = 0.5 * (func(a) + func(b))
    for i in range(1, n_points):
        result += func(a + i * h)
    return result * h


def find_root_newton(
    func: Differentiable,
    x0: float,
    tolerance: float = 1e-10,
    max_iterations: int = 100
) -> float | None:
    """
    Find root using Newton-Raphson method.

    Works with any object implementing Differentiable protocol.
    """
    x = x0
    for _ in range(max_iterations):
        f_val = func(x)
        if abs(f_val) < tolerance:
            return x

        df_val = func.derivative(x)
        if abs(df_val) < 1e-15:
            return None  # Derivative too small

        x = x - f_val / df_val

    return None  # Did not converge
```

### 5.2 Protocolos Genericos

```python
"""
Generic Protocols - Protocols with type parameters.
"""

from typing import Protocol, TypeVar, Generic

T = TypeVar("T")
T_co = TypeVar("T_co", covariant=True)
T_contra = TypeVar("T_contra", contravariant=True)

class Container(Protocol[T_co]):
    """
    Protocol for container types.
    Covariant in T - Container[Derived] is subtype of Container[Base].
    """

    def __len__(self) -> int: ...
    def __contains__(self, item: object) -> bool: ...
    def __iter__(self) -> Iterator[T_co]: ...


class Comparable(Protocol):
    """Protocol for comparable objects."""

    def __lt__(self, other: "Comparable") -> bool: ...
    def __le__(self, other: "Comparable") -> bool: ...
    def __gt__(self, other: "Comparable") -> bool: ...
    def __ge__(self, other: "Comparable") -> bool: ...


CT = TypeVar("CT", bound=Comparable)

def find_minimum(items: Container[CT]) -> CT | None:
    """
    Find minimum item in container.
    Works with any Container of Comparable items.
    """
    min_item = None
    for item in items:
        if min_item is None or item < min_item:
            min_item = item
    return min_item


class Processor(Protocol[T_contra, T_co]):
    """
    Processor protocol - contravariant in input, covariant in output.

    This pattern is useful for data processing pipelines,
    common in scientific workflows.
    """

    def process(self, data: T_contra) -> T_co: ...


class DataNormalizer:
    """Normalizer implementing Processor[list[float], NDArray]."""

    def process(self, data: list[float]) -> NDArray[np.float64]:
        """Normalize data to [0, 1] range."""
        arr = np.array(data)
        min_val, max_val = arr.min(), arr.max()
        if max_val - min_val < 1e-15:
            return np.zeros_like(arr)
        return (arr - min_val) / (max_val - min_val)
```

---

## 6. GESTION DE PAQUETES

### 6.1 pip - El Gestor Estandar

```python
"""
Package Management with pip.

pip is Python's standard package manager, similar to:
- apt/yum for Linux system packages
- npm for Node.js

For scientific computing, managing dependencies is crucial
to ensure reproducibility of legacy code migrations.
"""

# Common pip commands (run in terminal, not Python):

# Install package
# $ pip install numpy

# Install specific version (important for legacy code compatibility)
# $ pip install numpy==1.24.0

# Install from requirements file
# $ pip install -r requirements.txt

# Generate requirements file
# $ pip freeze > requirements.txt

# Install in development mode
# $ pip install -e .

# Show package info
# $ pip show numpy

# List installed packages
# $ pip list

# Check for outdated packages
# $ pip list --outdated

# Upgrade package
# $ pip install --upgrade numpy
```

### 6.2 Poetry - Gestion Moderna de Dependencias

```toml
# pyproject.toml - Poetry configuration
# Modern project configuration replacing setup.py

[tool.poetry]
name = "archaeon-scientific"
version = "1.0.0"
description = "ARCHAEON scientific computing toolkit"
authors = ["ARCHAEON CORE <archaeon@soul-core.ai>"]
readme = "README.md"
license = "MIT"
packages = [{include = "archaeon_scientific", from = "src"}]

[tool.poetry.dependencies]
python = "^3.10"
numpy = "^1.24.0"
scipy = "^1.10.0"
pandas = "^2.0.0"
matplotlib = "^3.7.0"

[tool.poetry.group.dev.dependencies]
pytest = "^7.3.0"
mypy = "^1.3.0"
black = "^23.3.0"
ruff = "^0.0.270"

[tool.poetry.group.docs.dependencies]
sphinx = "^6.0.0"
sphinx-rtd-theme = "^1.2.0"

[build-system]
requires = ["poetry-core>=1.0.0"]
build-backend = "poetry.core.masonry.api"

[tool.poetry.scripts]
archaeon-run = "archaeon_scientific.cli:main"
```

```bash
# Poetry commands

# Create new project
# $ poetry new archaeon-scientific

# Initialize in existing directory
# $ poetry init

# Install dependencies
# $ poetry install

# Add dependency
# $ poetry add numpy

# Add dev dependency
# $ poetry add --group dev pytest

# Update dependencies
# $ poetry update

# Show dependency tree
# $ poetry show --tree

# Build package
# $ poetry build

# Publish to PyPI
# $ poetry publish
```

### 6.3 uv - El Nuevo Gestor Ultra-Rapido

```bash
# uv - Ultra-fast Python package installer
# Written in Rust, 10-100x faster than pip

# Install uv
# $ pip install uv

# Install package with uv
# $ uv pip install numpy

# Install from requirements
# $ uv pip install -r requirements.txt

# Sync environment from requirements
# $ uv pip sync requirements.txt

# Create virtual environment
# $ uv venv

# uv project commands (similar to poetry)
# $ uv init
# $ uv add numpy
# $ uv sync
```

### 6.4 Comparacion de Gestores de Paquetes

```python
"""
Package Manager Comparison Table
"""

PACKAGE_MANAGERS = {
    "pip": {
        "pros": [
            "Standard tool, always available",
            "Simple and well-documented",
            "Works with any requirements.txt",
        ],
        "cons": [
            "No lock file by default",
            "No dependency resolution before install",
            "Can be slow for large projects",
        ],
        "best_for": "Simple projects, quick installs"
    },
    "poetry": {
        "pros": [
            "Deterministic builds with lock file",
            "Dependency resolution upfront",
            "Combined build and publish tool",
            "Virtual environment management",
        ],
        "cons": [
            "Slower than pip for simple installs",
            "Learning curve for configuration",
            "Not part of standard library",
        ],
        "best_for": "Serious projects requiring reproducibility"
    },
    "uv": {
        "pros": [
            "Extremely fast (Rust-based)",
            "pip-compatible",
            "Modern dependency resolution",
            "Low memory usage",
        ],
        "cons": [
            "Relatively new (less battle-tested)",
            "Some edge cases may differ from pip",
        ],
        "best_for": "Large projects, CI/CD pipelines"
    },
    "conda": {
        "pros": [
            "Handles non-Python dependencies",
            "Pre-compiled scientific packages",
            "Environment management included",
        ],
        "cons": [
            "Larger disk footprint",
            "Can conflict with pip packages",
            "Slower environment creation",
        ],
        "best_for": "Scientific computing with complex dependencies"
    }
}
```

---

## 7. ENTORNOS VIRTUALES

### 7.1 venv - Entornos Virtuales Nativos

```bash
# Create virtual environment
python -m venv .venv

# Activate (Linux/macOS)
source .venv/bin/activate

# Activate (Windows)
.venv\Scripts\activate

# Deactivate
deactivate

# Install packages in environment
pip install numpy scipy matplotlib

# Save environment
pip freeze > requirements.txt
```

```python
"""
Virtual Environment Management Utilities
"""

import os
import sys
import subprocess
from pathlib import Path
from dataclasses import dataclass

@dataclass
class VirtualEnv:
    """
    Represents a Python virtual environment.

    Provides utilities for managing environments,
    crucial for isolating legacy code migrations.
    """
    path: Path

    @property
    def python_path(self) -> Path:
        """Path to Python interpreter."""
        if sys.platform == "win32":
            return self.path / "Scripts" / "python.exe"
        return self.path / "bin" / "python"

    @property
    def pip_path(self) -> Path:
        """Path to pip."""
        if sys.platform == "win32":
            return self.path / "Scripts" / "pip.exe"
        return self.path / "bin" / "pip"

    @property
    def is_active(self) -> bool:
        """Check if this environment is currently active."""
        return str(self.path) in os.environ.get("VIRTUAL_ENV", "")

    def exists(self) -> bool:
        """Check if environment exists."""
        return self.python_path.exists()

    @classmethod
    def create(
        cls,
        path: Path,
        system_site_packages: bool = False,
        with_pip: bool = True
    ) -> "VirtualEnv":
        """Create a new virtual environment."""
        import venv

        venv.create(
            path,
            system_site_packages=system_site_packages,
            with_pip=with_pip
        )
        return cls(path)

    def install(self, *packages: str) -> None:
        """Install packages in this environment."""
        subprocess.run(
            [str(self.pip_path), "install", *packages],
            check=True
        )

    def run_python(self, *args: str) -> subprocess.CompletedProcess:
        """Run Python with arguments in this environment."""
        return subprocess.run(
            [str(self.python_path), *args],
            capture_output=True,
            text=True
        )

    def get_installed_packages(self) -> dict[str, str]:
        """Get dictionary of installed packages and versions."""
        result = self.run_python("-m", "pip", "freeze")
        packages = {}
        for line in result.stdout.strip().split("\n"):
            if "==" in line:
                name, version = line.split("==")
                packages[name] = version
        return packages


def setup_legacy_migration_env(
    project_path: Path,
    legacy_requirements: list[str]
) -> VirtualEnv:
    """
    Set up environment for legacy code migration.

    Creates isolated environment with specific package versions
    to match legacy code requirements.

    Example:
        # Create env matching Fortran wrapper requirements
        env = setup_legacy_migration_env(
            Path("./my_project"),
            ["numpy==1.21.0", "scipy==1.7.0", "f90wrap==0.2.3"]
        )
    """
    venv_path = project_path / ".venv-legacy"

    if not venv_path.exists():
        env = VirtualEnv.create(venv_path)
    else:
        env = VirtualEnv(venv_path)

    env.install(*legacy_requirements)
    return env
```

### 7.2 Gestion de Multiples Versiones de Python

```python
"""
Managing Multiple Python Versions with pyenv.

Essential for testing legacy code compatibility.
"""

# pyenv commands (shell, not Python)

# List available Python versions
# $ pyenv install --list

# Install specific version
# $ pyenv install 3.10.12
# $ pyenv install 3.11.4

# Set global Python version
# $ pyenv global 3.11.4

# Set local Python version (per-project)
# $ pyenv local 3.10.12

# Create virtualenv with specific Python
# $ pyenv virtualenv 3.10.12 legacy-project

# List virtualenvs
# $ pyenv virtualenvs

# Activate virtualenv
# $ pyenv activate legacy-project
```

---

## 8. MEJORES PRACTICAS PARA CODIGO CIENTIFICO

### 8.1 Estructura de Proyecto Cientifico

```
scientific_project/
├── pyproject.toml          # Project configuration
├── README.md               # Project documentation
├── LICENSE                 # License file
├── .gitignore              # Git ignore patterns
├── .python-version         # Python version (pyenv)
├── src/
│   └── scientific_project/
│       ├── __init__.py
│       ├── py.typed        # PEP 561 marker
│       ├── core/
│       │   ├── __init__.py
│       │   ├── algorithms.py
│       │   ├── solvers.py
│       │   └── types.py
│       ├── io/
│       │   ├── __init__.py
│       │   ├── readers.py
│       │   └── writers.py
│       ├── utils/
│       │   ├── __init__.py
│       │   ├── validation.py
│       │   └── timing.py
│       └── legacy/
│           ├── __init__.py
│           ├── fortran_wrapper.py
│           └── c_bridge.py
├── tests/
│   ├── __init__.py
│   ├── conftest.py
│   ├── test_algorithms.py
│   └── test_legacy_compatibility.py
├── benchmarks/
│   ├── bench_algorithms.py
│   └── compare_with_legacy.py
├── examples/
│   ├── basic_usage.py
│   └── migration_example.py
└── docs/
    ├── conf.py
    ├── index.rst
    └── api/
```

### 8.2 Codigo Cientifico con Type Hints

```python
"""
Scientific Code Best Practices with Type Hints.
"""

from __future__ import annotations

from typing import Callable, TypeAlias
from dataclasses import dataclass
from enum import Enum, auto
import numpy as np
from numpy.typing import NDArray

# Type aliases for scientific computing
Vector = NDArray[np.float64]
Matrix = NDArray[np.float64]
ScalarFunction = Callable[[float], float]
VectorFunction = Callable[[Vector], Vector]

class ConvergenceStatus(Enum):
    """Enumeration for solver convergence status."""
    CONVERGED = auto()
    MAX_ITERATIONS = auto()
    DIVERGED = auto()
    STAGNATED = auto()

@dataclass(frozen=True)
class SolverResult:
    """
    Immutable result from a numerical solver.

    Demonstrates best practices:
    - Immutable for safety
    - Clear type hints
    - Comprehensive information
    """
    solution: Vector
    status: ConvergenceStatus
    iterations: int
    residual_norm: float
    elapsed_time: float

    def is_success(self) -> bool:
        """Check if solver succeeded."""
        return self.status == ConvergenceStatus.CONVERGED


def solve_nonlinear_system(
    func: VectorFunction,
    jacobian: Callable[[Vector], Matrix],
    x0: Vector,
    *,  # Force keyword-only arguments
    tolerance: float = 1e-10,
    max_iterations: int = 100,
    damping: float = 1.0,
) -> SolverResult:
    """
    Solve nonlinear system using Newton's method.

    This function demonstrates:
    - Clear type hints for all parameters
    - Keyword-only arguments for optional parameters
    - Comprehensive docstring
    - Proper return type

    Args:
        func: Vector function F(x) to find root of
        jacobian: Function returning Jacobian matrix J(x)
        x0: Initial guess vector
        tolerance: Convergence tolerance (default: 1e-10)
        max_iterations: Maximum iterations (default: 100)
        damping: Damping factor for updates (default: 1.0)

    Returns:
        SolverResult containing solution and convergence info

    Raises:
        ValueError: If dimensions are inconsistent
        numpy.linalg.LinAlgError: If Jacobian is singular

    Example:
        >>> def f(x): return np.array([x[0]**2 - 1, x[1]**2 - 4])
        >>> def j(x): return np.diag([2*x[0], 2*x[1]])
        >>> result = solve_nonlinear_system(f, j, np.array([0.5, 1.0]))
        >>> result.status
        ConvergenceStatus.CONVERGED
    """
    import time

    start_time = time.perf_counter()
    x = x0.copy()

    for iteration in range(max_iterations):
        f_val = func(x)
        residual = np.linalg.norm(f_val)

        if residual < tolerance:
            elapsed = time.perf_counter() - start_time
            return SolverResult(
                solution=x,
                status=ConvergenceStatus.CONVERGED,
                iterations=iteration + 1,
                residual_norm=residual,
                elapsed_time=elapsed
            )

        j_val = jacobian(x)
        delta = np.linalg.solve(j_val, -f_val)
        x = x + damping * delta

    elapsed = time.perf_counter() - start_time
    return SolverResult(
        solution=x,
        status=ConvergenceStatus.MAX_ITERATIONS,
        iterations=max_iterations,
        residual_norm=np.linalg.norm(func(x)),
        elapsed_time=elapsed
    )
```

### 8.3 Validacion y Contratos

```python
"""
Input Validation and Design by Contract for Scientific Code.
"""

from typing import Any
from functools import wraps
import numpy as np
from numpy.typing import NDArray

def validate_positive(name: str, value: float) -> None:
    """Validate that a value is positive."""
    if value <= 0:
        raise ValueError(f"{name} must be positive, got {value}")

def validate_array_shape(
    name: str,
    array: NDArray,
    expected_shape: tuple[int, ...]
) -> None:
    """Validate array has expected shape."""
    if array.shape != expected_shape:
        raise ValueError(
            f"{name} has shape {array.shape}, expected {expected_shape}"
        )

def validate_square_matrix(name: str, matrix: NDArray) -> None:
    """Validate that matrix is square."""
    if matrix.ndim != 2 or matrix.shape[0] != matrix.shape[1]:
        raise ValueError(
            f"{name} must be square matrix, got shape {matrix.shape}"
        )

def requires(condition: bool, message: str) -> None:
    """Precondition check (design by contract)."""
    if not condition:
        raise ValueError(f"Precondition failed: {message}")

def ensures(condition: bool, message: str) -> None:
    """Postcondition check (design by contract)."""
    if not condition:
        raise AssertionError(f"Postcondition failed: {message}")

def validated_input(func):
    """
    Decorator for functions with validated inputs.

    Wraps function to provide clear error messages.
    """
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except (ValueError, TypeError) as e:
            raise type(e)(f"In {func.__name__}: {e}") from e
    return wrapper


@validated_input
def compute_matrix_exponential(
    A: NDArray[np.float64],
    terms: int = 20
) -> NDArray[np.float64]:
    """
    Compute matrix exponential using Taylor series.

    Demonstrates input validation for scientific functions.

    Args:
        A: Square matrix
        terms: Number of Taylor series terms

    Returns:
        Matrix exponential exp(A)
    """
    # Preconditions
    validate_square_matrix("A", A)
    validate_positive("terms", terms)

    n = A.shape[0]
    result = np.eye(n, dtype=np.float64)
    term = np.eye(n, dtype=np.float64)

    for k in range(1, terms + 1):
        term = term @ A / k
        result = result + term

    # Postconditions
    ensures(result.shape == A.shape, "Result shape matches input")
    ensures(np.all(np.isfinite(result)), "Result contains finite values")

    return result
```

---

## 9. TABLAS DE REFERENCIA

### 9.1 Tipos Python vs Fortran/C

| Python Type | Fortran Equivalent | C Equivalent | Notes |
|-------------|-------------------|--------------|-------|
| `int` | `INTEGER` | `int`, `long` | Arbitrary precision in Python |
| `float` | `REAL(8)` | `double` | IEEE 754 double |
| `complex` | `COMPLEX(8)` | `double complex` | Complex double |
| `bool` | `LOGICAL` | `_Bool` | True/False |
| `str` | `CHARACTER(*)` | `char*` | Unicode in Python |
| `bytes` | `CHARACTER(*)` | `unsigned char*` | Raw bytes |
| `list` | `ALLOCATABLE` array | Dynamic array | Mutable sequence |
| `tuple` | Fixed array | Fixed array | Immutable sequence |
| `dict` | - | `struct` (partial) | Key-value mapping |
| `numpy.ndarray` | Array | Array pointer | Numeric arrays |

### 9.2 Operadores Comparativos

| Operacion | Python | Fortran | C |
|-----------|--------|---------|---|
| Igualdad | `==` | `.EQ.` or `==` | `==` |
| Diferente | `!=` | `.NE.` or `/=` | `!=` |
| Menor | `<` | `.LT.` or `<` | `<` |
| Mayor | `>` | `.GT.` or `>` | `>` |
| Menor o igual | `<=` | `.LE.` or `<=` | `<=` |
| Mayor o igual | `>=` | `.GE.` or `>=` | `>=` |
| AND logico | `and` | `.AND.` | `&&` |
| OR logico | `or` | `.OR.` | `\|\|` |
| NOT logico | `not` | `.NOT.` | `!` |

### 9.3 Funciones Matematicas Comunes

| Operacion | Python/NumPy | Fortran | C (math.h) |
|-----------|--------------|---------|------------|
| Valor absoluto | `abs()`, `np.abs()` | `ABS()` | `fabs()` |
| Raiz cuadrada | `np.sqrt()` | `SQRT()` | `sqrt()` |
| Potencia | `**`, `np.power()` | `**` | `pow()` |
| Exponencial | `np.exp()` | `EXP()` | `exp()` |
| Logaritmo natural | `np.log()` | `LOG()` | `log()` |
| Logaritmo base 10 | `np.log10()` | `LOG10()` | `log10()` |
| Seno | `np.sin()` | `SIN()` | `sin()` |
| Coseno | `np.cos()` | `COS()` | `cos()` |
| Tangente | `np.tan()` | `TAN()` | `tan()` |
| Arco seno | `np.arcsin()` | `ASIN()` | `asin()` |
| Arco coseno | `np.arccos()` | `ACOS()` | `acos()` |
| Arco tangente | `np.arctan()` | `ATAN()` | `atan()` |
| Minimo | `min()`, `np.min()` | `MIN()` | fmin() |
| Maximo | `max()`, `np.max()` | `MAX()` | fmax() |
| Suelo | `np.floor()` | `FLOOR()` | `floor()` |
| Techo | `np.ceil()` | `CEILING()` | `ceil()` |
| Redondeo | `round()`, `np.round()` | `NINT()` | `round()` |

### 9.4 Palabras Clave de Python 3.10+

| Keyword | Uso | Ejemplo |
|---------|-----|---------|
| `match` | Pattern matching | `match value:` |
| `case` | Pattern case | `case 1:` |
| `type` | Type alias (3.12+) | `type Vector = list[float]` |
| `async` | Async function | `async def fetch():` |
| `await` | Await coroutine | `await response` |
| `walrus` (`:=`) | Assignment expression | `if (n := len(x)) > 0:` |

---

## RECURSOS ADICIONALES

### Documentacion Oficial
- [Python Documentation](https://docs.python.org/3/)
- [Python Type Hints PEP 484](https://peps.python.org/pep-0484/)
- [Dataclasses PEP 557](https://peps.python.org/pep-0557/)
- [Protocols PEP 544](https://peps.python.org/pep-0544/)
- [Pattern Matching PEP 634](https://peps.python.org/pep-0634/)

### Herramientas
- [mypy](https://mypy.readthedocs.io/) - Static type checker
- [ruff](https://github.com/astral-sh/ruff) - Fast Python linter
- [black](https://black.readthedocs.io/) - Code formatter
- [pytest](https://pytest.org/) - Testing framework

---

*ARCHAEON CORE - Bridging Legacy to Modern*
*Python Fundamentals v1.0.0*

---
title: "PYTHON 05 - Python Avanzado para Ciencia"
version: "1.0.0"
date: "2025-12-31"
author: "ARCHAEON CORE"
classification: "ARCHAEON/80000_MODERNOS/PYTHON"
purpose: "Optimizacion, concurrencia, packaging y testing para codigo cientifico"
language_bridge: "Legacy Optimization -> Modern Python Performance"
keywords:
  - performance
  - profiling
  - asyncio
  - multiprocessing
  - packaging
  - mypy
  - testing
  - optimization
dependencies:
  - python >= 3.10
  - numpy >= 1.24
  - pytest >= 7.0
  - mypy >= 1.0
status: "ACTIVE"
---

# PYTHON 05 - PYTHON AVANZADO PARA CIENCIA

## INDICE

1. [Optimizacion de Rendimiento](#1-optimizacion-de-rendimiento)
2. [Profiling y Deteccion de Cuellos de Botella](#2-profiling-y-deteccion-de-cuellos-de-botella)
3. [Asyncio para Codigo I/O-Bound](#3-asyncio-para-codigo-io-bound)
4. [Multiprocessing para Codigo CPU-Bound](#4-multiprocessing-para-codigo-cpu-bound)
5. [Packaging y Distribucion](#5-packaging-y-distribucion)
6. [Type Checking con mypy](#6-type-checking-con-mypy)
7. [Testing de Codigo Cientifico](#7-testing-de-codigo-cientifico)
8. [Patrones Avanzados y Mejores Practicas](#8-patrones-avanzados-y-mejores-practicas)

---

## 1. OPTIMIZACION DE RENDIMIENTO

### 1.1 Principios de Optimizacion

```python
"""
Performance Optimization Principles for Scientific Python.

Key principles:
1. Measure first, optimize second
2. Optimize the bottleneck, not everything
3. Prefer algorithmic improvements over micro-optimizations
4. Use appropriate data structures
5. Leverage NumPy/SciPy before custom solutions
"""

from dataclasses import dataclass
from typing import Callable, Any
import time
import numpy as np

@dataclass
class OptimizationResult:
    """Result of optimization comparison."""
    original_time: float
    optimized_time: float
    speedup: float
    results_match: bool

def compare_implementations(
    original: Callable,
    optimized: Callable,
    args: tuple = (),
    kwargs: dict = None,
    n_runs: int = 100,
    tolerance: float = 1e-10
) -> OptimizationResult:
    """
    Compare original and optimized implementations.

    Args:
        original: Original function
        optimized: Optimized function
        args: Function arguments
        kwargs: Function keyword arguments
        n_runs: Number of timing runs
        tolerance: Tolerance for result comparison

    Returns:
        OptimizationResult with timing and verification
    """
    kwargs = kwargs or {}

    # Get results
    original_result = original(*args, **kwargs)
    optimized_result = optimized(*args, **kwargs)

    # Check results match
    if isinstance(original_result, np.ndarray):
        results_match = np.allclose(
            original_result, optimized_result,
            rtol=tolerance, atol=tolerance
        )
    else:
        results_match = abs(original_result - optimized_result) < tolerance

    # Time original
    start = time.perf_counter()
    for _ in range(n_runs):
        original(*args, **kwargs)
    original_time = (time.perf_counter() - start) / n_runs

    # Time optimized
    start = time.perf_counter()
    for _ in range(n_runs):
        optimized(*args, **kwargs)
    optimized_time = (time.perf_counter() - start) / n_runs

    return OptimizationResult(
        original_time=original_time,
        optimized_time=optimized_time,
        speedup=original_time / optimized_time,
        results_match=results_match
    )


# Example: Loop optimization
def demonstrate_loop_optimization():
    """Demonstrate common loop optimizations."""

    print("Loop Optimization Examples")
    print("=" * 50)

    n = 10000

    # Pattern 1: List comprehension vs loop
    def loop_version():
        result = []
        for i in range(n):
            result.append(i ** 2)
        return result

    def comprehension_version():
        return [i ** 2 for i in range(n)]

    def numpy_version():
        return np.arange(n) ** 2

    result1 = compare_implementations(loop_version, comprehension_version)
    print(f"\n1. List comprehension vs loop:")
    print(f"   Speedup: {result1.speedup:.2f}x")

    result2 = compare_implementations(loop_version, numpy_version)
    print(f"\n2. NumPy vs loop:")
    print(f"   Speedup: {result2.speedup:.2f}x")

    # Pattern 2: Avoiding repeated calculations
    def repeated_calc():
        result = 0
        for i in range(n):
            result += np.sqrt(2) * i  # sqrt(2) computed n times
        return result

    def cached_calc():
        sqrt_2 = np.sqrt(2)
        result = 0
        for i in range(n):
            result += sqrt_2 * i
        return result

    def vectorized_calc():
        return np.sqrt(2) * np.sum(np.arange(n))

    result3 = compare_implementations(repeated_calc, cached_calc, n_runs=100)
    print(f"\n3. Cached constant vs repeated calculation:")
    print(f"   Speedup: {result3.speedup:.2f}x")

    result4 = compare_implementations(repeated_calc, vectorized_calc, n_runs=100)
    print(f"\n4. Vectorized vs repeated calculation:")
    print(f"   Speedup: {result4.speedup:.2f}x")


# Pattern 3: Data structure selection
def demonstrate_data_structure_optimization():
    """Demonstrate impact of data structure choice."""

    print("\nData Structure Optimization")
    print("=" * 50)

    n = 10000
    search_values = list(range(0, n, 10))

    # Search in list vs set
    data_list = list(range(n))
    data_set = set(range(n))

    def search_list():
        return sum(1 for v in search_values if v in data_list)

    def search_set():
        return sum(1 for v in search_values if v in data_set)

    result = compare_implementations(search_list, search_set, n_runs=100)
    print(f"\n1. Set lookup vs list lookup:")
    print(f"   Speedup: {result.speedup:.2f}x")

    # Dictionary vs list of tuples
    list_pairs = [(i, i*2) for i in range(n)]
    dict_pairs = {i: i*2 for i in range(n)}

    def lookup_list():
        results = []
        for key in search_values:
            for k, v in list_pairs:
                if k == key:
                    results.append(v)
                    break
        return results

    def lookup_dict():
        return [dict_pairs[key] for key in search_values]

    result2 = compare_implementations(lookup_list, lookup_dict, n_runs=10)
    print(f"\n2. Dictionary vs list of tuples lookup:")
    print(f"   Speedup: {result2.speedup:.2f}x")
```

### 1.2 Optimizacion de Memoria

```python
"""
Memory Optimization Techniques.

Memory optimization is crucial for:
- Large scientific datasets
- Long-running simulations
- Resource-constrained environments
"""

import sys
import numpy as np
from typing import Iterator, Generator
from dataclasses import dataclass

def get_size(obj: object) -> int:
    """Get approximate memory size of object in bytes."""
    size = sys.getsizeof(obj)

    if isinstance(obj, dict):
        size += sum(get_size(k) + get_size(v) for k, v in obj.items())
    elif isinstance(obj, (list, tuple, set, frozenset)):
        size += sum(get_size(item) for item in obj)
    elif isinstance(obj, np.ndarray):
        size = obj.nbytes

    return size


def demonstrate_memory_optimization():
    """Demonstrate memory optimization techniques."""

    print("Memory Optimization Techniques")
    print("=" * 50)

    # Technique 1: Use appropriate dtypes
    n = 1_000_000

    float64_arr = np.zeros(n, dtype=np.float64)
    float32_arr = np.zeros(n, dtype=np.float32)
    int64_arr = np.zeros(n, dtype=np.int64)
    int32_arr = np.zeros(n, dtype=np.int32)
    int8_arr = np.zeros(n, dtype=np.int8)

    print(f"\n1. Array dtype impact ({n:,} elements):")
    print(f"   float64: {float64_arr.nbytes / 1024 / 1024:.2f} MB")
    print(f"   float32: {float32_arr.nbytes / 1024 / 1024:.2f} MB")
    print(f"   int64:   {int64_arr.nbytes / 1024 / 1024:.2f} MB")
    print(f"   int32:   {int32_arr.nbytes / 1024 / 1024:.2f} MB")
    print(f"   int8:    {int8_arr.nbytes / 1024 / 1024:.2f} MB")

    # Technique 2: Generators vs lists
    def list_squares(n: int) -> list:
        return [i ** 2 for i in range(n)]

    def gen_squares(n: int) -> Generator[int, None, None]:
        for i in range(n):
            yield i ** 2

    list_result = list_squares(100000)
    gen_result = gen_squares(100000)

    print(f"\n2. Generator vs list (100,000 elements):")
    print(f"   List memory: {get_size(list_result) / 1024:.2f} KB")
    print(f"   Generator memory: {get_size(gen_result)} bytes")

    # Technique 3: __slots__ for classes
    class RegularPoint:
        """Regular class - uses dict for attributes."""
        def __init__(self, x: float, y: float, z: float):
            self.x = x
            self.y = y
            self.z = z

    class SlottedPoint:
        """Slotted class - fixed attributes, less memory."""
        __slots__ = ('x', 'y', 'z')

        def __init__(self, x: float, y: float, z: float):
            self.x = x
            self.y = y
            self.z = z

    regular_points = [RegularPoint(1.0, 2.0, 3.0) for _ in range(10000)]
    slotted_points = [SlottedPoint(1.0, 2.0, 3.0) for _ in range(10000)]

    print(f"\n3. __slots__ optimization (10,000 objects):")
    print(f"   Regular class: ~{sys.getsizeof(regular_points[0])} bytes/object")
    print(f"   Slotted class: ~{sys.getsizeof(slotted_points[0])} bytes/object")


# Memory-efficient iterator for large datasets
class ChunkedArrayIterator:
    """
    Memory-efficient iterator for processing large arrays in chunks.

    Useful when array is too large to fit in memory.
    """

    def __init__(
        self,
        array: np.ndarray,
        chunk_size: int = 10000
    ):
        self.array = array
        self.chunk_size = chunk_size
        self.n_chunks = (len(array) + chunk_size - 1) // chunk_size

    def __iter__(self) -> Iterator[np.ndarray]:
        for i in range(self.n_chunks):
            start = i * self.chunk_size
            end = min(start + self.chunk_size, len(self.array))
            yield self.array[start:end]

    def __len__(self) -> int:
        return self.n_chunks


def process_large_array(
    array: np.ndarray,
    chunk_size: int = 10000
) -> float:
    """
    Process large array in memory-efficient chunks.

    Example: calculate mean without loading full array.
    """
    total = 0.0
    count = 0

    for chunk in ChunkedArrayIterator(array, chunk_size):
        total += np.sum(chunk)
        count += len(chunk)

    return total / count
```

---

## 2. PROFILING Y DETECCION DE CUELLOS DE BOTELLA

### 2.1 Herramientas de Profiling

```python
"""
Profiling Tools for Python Performance Analysis.

Tools covered:
1. timeit - Quick timing
2. cProfile - Detailed function profiling
3. line_profiler - Line-by-line profiling
4. memory_profiler - Memory usage profiling
"""

import cProfile
import pstats
import io
from typing import Callable, Any
import time
from contextlib import contextmanager
from functools import wraps

# Simple timing decorator
def timed(func: Callable) -> Callable:
    """Decorator to time function execution."""
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time.perf_counter()
        result = func(*args, **kwargs)
        elapsed = time.perf_counter() - start
        print(f"{func.__name__}: {elapsed:.4f} seconds")
        return result
    return wrapper


@contextmanager
def timer(name: str = "Block"):
    """Context manager for timing code blocks."""
    start = time.perf_counter()
    yield
    elapsed = time.perf_counter() - start
    print(f"{name}: {elapsed:.4f} seconds")


def profile_function(
    func: Callable,
    *args,
    sort_by: str = "cumulative",
    top_n: int = 20,
    **kwargs
) -> str:
    """
    Profile a function using cProfile.

    Args:
        func: Function to profile
        *args: Arguments to pass to function
        sort_by: Sort key (cumulative, time, calls)
        top_n: Number of top entries to show
        **kwargs: Keyword arguments to pass to function

    Returns:
        Profiling report as string
    """
    profiler = cProfile.Profile()

    # Run function with profiling
    profiler.enable()
    result = func(*args, **kwargs)
    profiler.disable()

    # Generate report
    stream = io.StringIO()
    stats = pstats.Stats(profiler, stream=stream)
    stats.sort_stats(sort_by)
    stats.print_stats(top_n)

    return stream.getvalue()


# Example: Profiling a scientific computation
def matrix_operations_example(n: int = 500):
    """Example function to profile."""
    import numpy as np

    # Create matrices
    A = np.random.randn(n, n)
    B = np.random.randn(n, n)

    # Various operations
    C = A @ B  # Matrix multiplication
    D = np.linalg.inv(A @ A.T + np.eye(n))  # Inverse
    E = np.linalg.svd(A)  # SVD

    return C, D, E


def demonstrate_profiling():
    """Demonstrate profiling tools."""

    print("Profiling Demonstration")
    print("=" * 50)

    # Using timer context manager
    print("\n1. Timer context manager:")
    with timer("Matrix operations"):
        matrix_operations_example(200)

    # Using timed decorator
    @timed
    def timed_matrix_ops():
        return matrix_operations_example(200)

    print("\n2. Timed decorator:")
    timed_matrix_ops()

    # Using cProfile
    print("\n3. cProfile output:")
    report = profile_function(matrix_operations_example, 200, top_n=10)
    print(report)


# Custom profiler for scientific code
class ScientificProfiler:
    """
    Profiler designed for scientific computations.

    Tracks:
    - Execution time
    - Memory usage
    - FLOP estimates
    """

    def __init__(self):
        self.measurements: list[dict] = []

    def profile(self, name: str = None):
        """Decorator for profiling functions."""
        def decorator(func: Callable) -> Callable:
            @wraps(func)
            def wrapper(*args, **kwargs):
                import tracemalloc

                # Start memory tracking
                tracemalloc.start()

                # Time execution
                start = time.perf_counter()
                result = func(*args, **kwargs)
                elapsed = time.perf_counter() - start

                # Get memory usage
                current, peak = tracemalloc.get_traced_memory()
                tracemalloc.stop()

                # Record measurement
                measurement = {
                    "name": name or func.__name__,
                    "time": elapsed,
                    "memory_current": current,
                    "memory_peak": peak,
                }
                self.measurements.append(measurement)

                return result
            return wrapper
        return decorator

    def report(self) -> str:
        """Generate profiling report."""
        lines = ["Scientific Profiling Report", "=" * 50]

        for m in self.measurements:
            lines.append(f"\n{m['name']}:")
            lines.append(f"  Time: {m['time']:.4f} s")
            lines.append(f"  Memory (current): {m['memory_current'] / 1024:.2f} KB")
            lines.append(f"  Memory (peak): {m['memory_peak'] / 1024:.2f} KB")

        return "\n".join(lines)

    def clear(self):
        """Clear measurements."""
        self.measurements.clear()
```

### 2.2 Identificando Cuellos de Botella

```python
"""
Bottleneck Identification Strategies.
"""

import numpy as np
from typing import Callable, List, Tuple
import time

class BottleneckAnalyzer:
    """
    Analyze code to identify performance bottlenecks.
    """

    def __init__(self):
        self.timing_data: dict[str, list[float]] = {}

    def instrument(self, name: str):
        """Decorator to instrument a function for timing."""
        def decorator(func: Callable) -> Callable:
            @wraps(func)
            def wrapper(*args, **kwargs):
                start = time.perf_counter()
                result = func(*args, **kwargs)
                elapsed = time.perf_counter() - start

                if name not in self.timing_data:
                    self.timing_data[name] = []
                self.timing_data[name].append(elapsed)

                return result
            return wrapper
        return decorator

    def analyze(self) -> List[Tuple[str, float, float]]:
        """
        Analyze timing data to identify bottlenecks.

        Returns:
            List of (name, total_time, percentage) sorted by time
        """
        totals = {
            name: sum(times)
            for name, times in self.timing_data.items()
        }

        grand_total = sum(totals.values())

        results = [
            (name, total, total / grand_total * 100)
            for name, total in sorted(
                totals.items(),
                key=lambda x: x[1],
                reverse=True
            )
        ]

        return results

    def report(self) -> str:
        """Generate bottleneck report."""
        analysis = self.analyze()

        lines = [
            "Bottleneck Analysis",
            "=" * 50,
            f"{'Function':<30} {'Total Time':>12} {'Percentage':>10}",
            "-" * 52
        ]

        for name, total, percentage in analysis:
            lines.append(f"{name:<30} {total:>10.4f}s {percentage:>9.1f}%")

        return "\n".join(lines)


def demonstrate_bottleneck_analysis():
    """Demonstrate bottleneck identification."""

    analyzer = BottleneckAnalyzer()

    @analyzer.instrument("data_loading")
    def load_data(n: int) -> np.ndarray:
        # Simulated data loading
        time.sleep(0.01)  # I/O simulation
        return np.random.randn(n, n)

    @analyzer.instrument("preprocessing")
    def preprocess(data: np.ndarray) -> np.ndarray:
        return (data - data.mean()) / data.std()

    @analyzer.instrument("matrix_multiply")
    def compute_product(A: np.ndarray, B: np.ndarray) -> np.ndarray:
        return A @ B

    @analyzer.instrument("eigenvalue_decomposition")
    def compute_eigenvalues(A: np.ndarray) -> np.ndarray:
        return np.linalg.eigvalsh(A @ A.T)

    @analyzer.instrument("postprocessing")
    def postprocess(result: np.ndarray) -> np.ndarray:
        return np.sort(result)[::-1]

    # Run pipeline multiple times
    print("Running analysis pipeline...")
    for _ in range(10):
        data1 = load_data(200)
        data2 = load_data(200)

        proc1 = preprocess(data1)
        proc2 = preprocess(data2)

        product = compute_product(proc1, proc2)
        eigenvalues = compute_eigenvalues(product)
        result = postprocess(eigenvalues)

    # Show bottleneck analysis
    print("\n" + analyzer.report())
```

---

## 3. ASYNCIO PARA CODIGO I/O-BOUND

### 3.1 Fundamentos de Asyncio

```python
"""
Asyncio for I/O-Bound Scientific Computing.

Asyncio is ideal for:
- Fetching data from multiple sources
- Concurrent file operations
- Network-based data acquisition
- Parallel API calls to external services
"""

import asyncio
from typing import List, Any
import time
from dataclasses import dataclass

@dataclass
class AsyncResult:
    """Result from async operation."""
    value: Any
    elapsed: float
    source: str

async def simulate_data_fetch(source: str, delay: float) -> AsyncResult:
    """
    Simulate fetching data from a remote source.

    In real applications, this would be:
    - HTTP requests (aiohttp)
    - Database queries (asyncpg, aiomysql)
    - File I/O (aiofiles)
    """
    start = time.perf_counter()
    await asyncio.sleep(delay)  # Simulate network delay

    # Simulated data
    import numpy as np
    data = np.random.randn(100)

    return AsyncResult(
        value=data,
        elapsed=time.perf_counter() - start,
        source=source
    )


async def fetch_all_data(sources: List[str], delays: List[float]) -> List[AsyncResult]:
    """
    Fetch data from all sources concurrently.

    This demonstrates the power of async: all fetches
    happen in parallel, total time is max(delays) not sum(delays).
    """
    tasks = [
        simulate_data_fetch(source, delay)
        for source, delay in zip(sources, delays)
    ]
    return await asyncio.gather(*tasks)


def compare_sync_vs_async():
    """Compare synchronous vs asynchronous data fetching."""

    print("Sync vs Async Comparison")
    print("=" * 50)

    sources = ["server1", "server2", "server3", "server4", "server5"]
    delays = [0.5, 0.3, 0.4, 0.2, 0.6]

    # Synchronous version
    print("\nSynchronous fetching:")
    start = time.perf_counter()
    sync_results = []
    for source, delay in zip(sources, delays):
        # In sync code, each fetch blocks
        time.sleep(delay)
        sync_results.append(f"Data from {source}")
    sync_time = time.perf_counter() - start
    print(f"  Total time: {sync_time:.2f}s (sum of delays)")

    # Asynchronous version
    print("\nAsynchronous fetching:")
    start = time.perf_counter()
    async_results = asyncio.run(fetch_all_data(sources, delays))
    async_time = time.perf_counter() - start
    print(f"  Total time: {async_time:.2f}s (max of delays)")
    print(f"  Speedup: {sync_time / async_time:.2f}x")


# Scientific async patterns
class AsyncDataPipeline:
    """
    Async pipeline for scientific data processing.

    Demonstrates producer-consumer pattern with asyncio.
    """

    def __init__(self, max_queue_size: int = 100):
        self.queue: asyncio.Queue = asyncio.Queue(maxsize=max_queue_size)
        self.results: List[Any] = []
        self._stop = False

    async def producer(self, data_sources: List[str]):
        """Produce data items asynchronously."""
        import numpy as np

        for source in data_sources:
            # Simulate async data acquisition
            await asyncio.sleep(0.1)
            data = np.random.randn(100)
            await self.queue.put((source, data))
            print(f"Produced data from {source}")

        # Signal end of production
        await self.queue.put(None)

    async def consumer(self, processor):
        """Consume and process data items."""
        while True:
            item = await self.queue.get()

            if item is None:
                self.queue.task_done()
                break

            source, data = item
            result = processor(data)
            self.results.append((source, result))
            print(f"Processed data from {source}, result: {result:.4f}")
            self.queue.task_done()

    async def run(self, data_sources: List[str], processor):
        """Run the pipeline."""
        # Start producer and consumer
        producer_task = asyncio.create_task(self.producer(data_sources))
        consumer_task = asyncio.create_task(self.consumer(processor))

        # Wait for completion
        await producer_task
        await self.queue.join()
        await consumer_task

        return self.results


def demonstrate_async_pipeline():
    """Demonstrate async data pipeline."""

    print("\nAsync Data Pipeline")
    print("=" * 50)

    pipeline = AsyncDataPipeline()

    sources = [f"sensor_{i}" for i in range(5)]

    def processor(data):
        """Simple processor: calculate mean."""
        import numpy as np
        return np.mean(data)

    results = asyncio.run(pipeline.run(sources, processor))
    print(f"\nProcessed {len(results)} data sources")
```

### 3.2 Async con Bibliotecas Cientificas

```python
"""
Async patterns for scientific computing libraries.

Note: NumPy/SciPy operations are CPU-bound and don't benefit
from asyncio directly. Use async for I/O around them.
"""

import asyncio
import numpy as np
from typing import List, Tuple
from concurrent.futures import ThreadPoolExecutor
from functools import partial

# Async file I/O for scientific data
async def read_scientific_data_async(filepath: str) -> np.ndarray:
    """
    Read scientific data file asynchronously.

    Uses thread pool for actual I/O since file operations are blocking.
    """
    loop = asyncio.get_event_loop()

    def read_file():
        # This could be np.load, np.loadtxt, h5py, etc.
        return np.load(filepath)

    with ThreadPoolExecutor() as pool:
        data = await loop.run_in_executor(pool, read_file)

    return data


async def process_multiple_files(filepaths: List[str]) -> List[np.ndarray]:
    """
    Process multiple data files concurrently.

    Files are read in parallel using thread pool.
    """
    tasks = [read_scientific_data_async(fp) for fp in filepaths]
    return await asyncio.gather(*tasks)


# Async for remote data sources
class AsyncRemoteDataLoader:
    """
    Async loader for remote scientific data.

    Supports:
    - Multiple data sources
    - Retry logic
    - Progress reporting
    """

    def __init__(self, timeout: float = 30.0, max_retries: int = 3):
        self.timeout = timeout
        self.max_retries = max_retries

    async def fetch_with_retry(
        self,
        url: str,
        retry_count: int = 0
    ) -> np.ndarray:
        """Fetch data with retry logic."""
        try:
            # In real code, use aiohttp:
            # async with aiohttp.ClientSession() as session:
            #     async with session.get(url) as response:
            #         data = await response.read()

            # Simulated fetch
            await asyncio.sleep(0.1)
            return np.random.randn(100)

        except Exception as e:
            if retry_count < self.max_retries:
                wait_time = 2 ** retry_count  # Exponential backoff
                await asyncio.sleep(wait_time)
                return await self.fetch_with_retry(url, retry_count + 1)
            raise

    async def fetch_all(
        self,
        urls: List[str],
        progress_callback=None
    ) -> List[np.ndarray]:
        """Fetch from all URLs with progress reporting."""
        results = []
        total = len(urls)

        for i, url in enumerate(urls):
            result = await self.fetch_with_retry(url)
            results.append(result)

            if progress_callback:
                progress_callback(i + 1, total)

        return results


async def demonstrate_async_data_loading():
    """Demonstrate async scientific data loading."""

    print("\nAsync Scientific Data Loading")
    print("=" * 50)

    loader = AsyncRemoteDataLoader()

    urls = [f"https://data.example.com/dataset_{i}.npy" for i in range(5)]

    def progress(current, total):
        print(f"  Progress: {current}/{total}")

    print("Fetching data from remote sources...")
    results = await loader.fetch_all(urls, progress_callback=progress)
    print(f"Loaded {len(results)} datasets")
```

---

## 4. MULTIPROCESSING PARA CODIGO CPU-BOUND

### 4.1 Fundamentos de Multiprocessing

```python
"""
Multiprocessing for CPU-Bound Scientific Computing.

Python's GIL (Global Interpreter Lock) prevents true
parallelism in threads. Use multiprocessing for:
- Heavy numerical computations
- Parallel simulations
- Embarrassingly parallel problems
"""

import multiprocessing as mp
from multiprocessing import Pool, Process, Queue, Manager
import numpy as np
from typing import List, Callable, Any, Tuple
import time

def cpu_intensive_task(data: np.ndarray) -> float:
    """
    Example CPU-intensive task.

    Computes something expensive that benefits from parallelization.
    """
    # Simulate heavy computation
    result = 0.0
    for _ in range(100):
        result += np.sum(np.linalg.eigvalsh(data @ data.T))
    return result


def demonstrate_multiprocessing_basics():
    """Demonstrate basic multiprocessing patterns."""

    print("Multiprocessing Basics")
    print("=" * 50)

    n_cores = mp.cpu_count()
    print(f"Available CPU cores: {n_cores}")

    # Create test data
    n_tasks = 8
    task_data = [np.random.randn(100, 100) for _ in range(n_tasks)]

    # Sequential execution
    print("\nSequential execution:")
    start = time.perf_counter()
    sequential_results = [cpu_intensive_task(data) for data in task_data]
    sequential_time = time.perf_counter() - start
    print(f"  Time: {sequential_time:.2f}s")

    # Parallel execution with Pool
    print("\nParallel execution (Pool.map):")
    start = time.perf_counter()
    with Pool(processes=n_cores) as pool:
        parallel_results = pool.map(cpu_intensive_task, task_data)
    parallel_time = time.perf_counter() - start
    print(f"  Time: {parallel_time:.2f}s")
    print(f"  Speedup: {sequential_time / parallel_time:.2f}x")

    # Verify results match
    match = all(
        abs(s - p) < 1e-10
        for s, p in zip(sequential_results, parallel_results)
    )
    print(f"  Results match: {match}")


# Parallel Monte Carlo simulation
def monte_carlo_worker(args: Tuple[int, int]) -> float:
    """
    Worker function for parallel Monte Carlo.

    Estimates pi using random sampling.
    """
    n_samples, seed = args
    np.random.seed(seed)

    x = np.random.random(n_samples)
    y = np.random.random(n_samples)

    inside = np.sum(x**2 + y**2 <= 1)
    return inside / n_samples * 4


def parallel_monte_carlo_pi(n_samples: int, n_workers: int) -> Tuple[float, float]:
    """
    Estimate pi using parallel Monte Carlo.

    Args:
        n_samples: Total number of samples
        n_workers: Number of parallel workers

    Returns:
        Tuple of (pi_estimate, standard_error)
    """
    samples_per_worker = n_samples // n_workers

    # Create work items with different seeds
    work_items = [
        (samples_per_worker, seed)
        for seed in range(n_workers)
    ]

    with Pool(processes=n_workers) as pool:
        results = pool.map(monte_carlo_worker, work_items)

    pi_estimate = np.mean(results)
    std_error = np.std(results) / np.sqrt(n_workers)

    return pi_estimate, std_error


def demonstrate_parallel_monte_carlo():
    """Demonstrate parallel Monte Carlo simulation."""

    print("\nParallel Monte Carlo (Pi Estimation)")
    print("=" * 50)

    n_samples = 10_000_000
    n_workers = mp.cpu_count()

    print(f"Samples: {n_samples:,}")
    print(f"Workers: {n_workers}")

    start = time.perf_counter()
    pi_est, std_err = parallel_monte_carlo_pi(n_samples, n_workers)
    elapsed = time.perf_counter() - start

    print(f"\nResults:")
    print(f"  Pi estimate: {pi_est:.6f}")
    print(f"  Std error: {std_err:.6f}")
    print(f"  True pi: {np.pi:.6f}")
    print(f"  Error: {abs(pi_est - np.pi):.6f}")
    print(f"  Time: {elapsed:.2f}s")
```

### 4.2 Patrones Avanzados de Paralelizacion

```python
"""
Advanced Parallelization Patterns.
"""

import multiprocessing as mp
from multiprocessing import Pool, shared_memory
import numpy as np
from typing import Callable, List, Tuple, Any
from dataclasses import dataclass
from functools import partial

@dataclass
class ParallelResult:
    """Result from parallel computation."""
    results: List[Any]
    elapsed_time: float
    n_workers: int

class ParallelComputer:
    """
    High-level interface for parallel computing.

    Handles:
    - Work distribution
    - Progress tracking
    - Error handling
    - Result aggregation
    """

    def __init__(self, n_workers: int = None):
        self.n_workers = n_workers or mp.cpu_count()

    def map(
        self,
        func: Callable,
        items: List[Any],
        chunksize: int = None
    ) -> ParallelResult:
        """
        Map function over items in parallel.

        Args:
            func: Function to apply
            items: Items to process
            chunksize: Items per worker batch

        Returns:
            ParallelResult with results and metadata
        """
        import time

        if chunksize is None:
            chunksize = max(1, len(items) // (self.n_workers * 4))

        start = time.perf_counter()

        with Pool(processes=self.n_workers) as pool:
            results = pool.map(func, items, chunksize=chunksize)

        elapsed = time.perf_counter() - start

        return ParallelResult(
            results=results,
            elapsed_time=elapsed,
            n_workers=self.n_workers
        )

    def starmap(
        self,
        func: Callable,
        args_list: List[Tuple]
    ) -> ParallelResult:
        """
        Map function with multiple arguments per item.

        Args:
            func: Function to apply
            args_list: List of argument tuples

        Returns:
            ParallelResult with results and metadata
        """
        import time

        start = time.perf_counter()

        with Pool(processes=self.n_workers) as pool:
            results = pool.starmap(func, args_list)

        elapsed = time.perf_counter() - start

        return ParallelResult(
            results=results,
            elapsed_time=elapsed,
            n_workers=self.n_workers
        )


# Shared memory for large arrays
class SharedArrayProcessor:
    """
    Process large arrays using shared memory.

    Avoids copying large arrays to each worker process.
    """

    def __init__(self, array: np.ndarray):
        self.shape = array.shape
        self.dtype = array.dtype

        # Create shared memory
        self.shm = shared_memory.SharedMemory(
            create=True,
            size=array.nbytes
        )

        # Create numpy array backed by shared memory
        self.shared_array = np.ndarray(
            self.shape,
            dtype=self.dtype,
            buffer=self.shm.buf
        )

        # Copy data to shared memory
        self.shared_array[:] = array[:]

    def process_chunk(self, chunk_indices: Tuple[int, int]) -> float:
        """Process a chunk of the shared array."""
        start, end = chunk_indices
        chunk = self.shared_array[start:end]
        return np.sum(chunk ** 2)

    def parallel_sum_squares(self, n_workers: int = None) -> float:
        """
        Compute sum of squares using parallel workers.

        Workers access shared memory directly - no copying.
        """
        n_workers = n_workers or mp.cpu_count()
        n = len(self.shared_array)
        chunk_size = n // n_workers

        chunks = [
            (i * chunk_size, min((i + 1) * chunk_size, n))
            for i in range(n_workers)
        ]

        with Pool(processes=n_workers) as pool:
            results = pool.map(self.process_chunk, chunks)

        return sum(results)

    def cleanup(self):
        """Release shared memory."""
        self.shm.close()
        self.shm.unlink()

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.cleanup()


def demonstrate_shared_memory():
    """Demonstrate shared memory parallel processing."""

    print("\nShared Memory Processing")
    print("=" * 50)

    # Create large array
    n = 10_000_000
    data = np.random.randn(n)
    print(f"Array size: {data.nbytes / 1024 / 1024:.1f} MB")

    # Process with shared memory
    with SharedArrayProcessor(data) as processor:
        start = time.perf_counter()
        result = processor.parallel_sum_squares()
        elapsed = time.perf_counter() - start

        print(f"Sum of squares: {result:.6e}")
        print(f"Time: {elapsed:.4f}s")

    # Compare with regular (non-shared) parallel
    print("\nComparison with NumPy:")
    start = time.perf_counter()
    numpy_result = np.sum(data ** 2)
    numpy_elapsed = time.perf_counter() - start
    print(f"NumPy result: {numpy_result:.6e}")
    print(f"NumPy time: {numpy_elapsed:.4f}s")
```

---

## 5. PACKAGING Y DISTRIBUCION

### 5.1 Estructura Moderna de Paquetes

```python
"""
Modern Python Package Structure.

This section covers creating distributable Python packages
for scientific computing.
"""

# Recommended project structure
PROJECT_STRUCTURE = """
my_scientific_package/
├── pyproject.toml          # Build configuration (PEP 518/621)
├── README.md               # Documentation
├── LICENSE                 # License file
├── CHANGELOG.md            # Version history
├── .gitignore              # Git ignore
├── src/
│   └── my_package/         # Main package (src layout)
│       ├── __init__.py     # Package initialization
│       ├── py.typed        # PEP 561 marker file
│       ├── core/
│       │   ├── __init__.py
│       │   ├── algorithms.py
│       │   └── types.py
│       ├── io/
│       │   ├── __init__.py
│       │   └── formats.py
│       └── _version.py     # Version info
├── tests/
│   ├── __init__.py
│   ├── conftest.py         # Pytest fixtures
│   ├── test_algorithms.py
│   └── test_io.py
├── docs/
│   ├── conf.py             # Sphinx configuration
│   └── index.rst
├── examples/
│   └── basic_usage.py
└── benchmarks/
    └── bench_algorithms.py
"""

print(PROJECT_STRUCTURE)
```

### 5.2 pyproject.toml Moderno

```toml
# pyproject.toml - Modern Python project configuration

[build-system]
requires = ["setuptools>=61.0", "wheel", "cython>=3.0", "numpy>=1.24"]
build-backend = "setuptools.build_meta"

[project]
name = "archaeon-scientific"
version = "1.0.0"
description = "ARCHAEON Scientific Computing Toolkit"
readme = "README.md"
license = {text = "MIT"}
authors = [
    {name = "ARCHAEON CORE", email = "archaeon@soul-core.ai"}
]
maintainers = [
    {name = "ARCHAEON CORE", email = "archaeon@soul-core.ai"}
]
keywords = ["scientific-computing", "numerical-methods", "fortran", "legacy"]
classifiers = [
    "Development Status :: 4 - Beta",
    "Intended Audience :: Science/Research",
    "License :: OSI Approved :: MIT License",
    "Programming Language :: Python :: 3",
    "Programming Language :: Python :: 3.10",
    "Programming Language :: Python :: 3.11",
    "Programming Language :: Python :: 3.12",
    "Topic :: Scientific/Engineering",
    "Typing :: Typed",
]
requires-python = ">=3.10"
dependencies = [
    "numpy>=1.24",
    "scipy>=1.10",
    "pandas>=2.0",
]

[project.optional-dependencies]
dev = [
    "pytest>=7.3",
    "pytest-cov>=4.0",
    "mypy>=1.3",
    "ruff>=0.0.270",
    "black>=23.3",
]
docs = [
    "sphinx>=6.0",
    "sphinx-rtd-theme>=1.2",
    "myst-parser>=1.0",
]
all = ["archaeon-scientific[dev,docs]"]

[project.urls]
Homepage = "https://github.com/soul-core/archaeon-scientific"
Documentation = "https://archaeon-scientific.readthedocs.io"
Repository = "https://github.com/soul-core/archaeon-scientific"

[project.scripts]
archaeon-run = "archaeon_scientific.cli:main"

[project.entry-points."archaeon.plugins"]
fortran-bridge = "archaeon_scientific.bridges:fortran_plugin"

[tool.setuptools.packages.find]
where = ["src"]

[tool.setuptools.package-data]
"*" = ["py.typed", "*.pyx", "*.pxd"]

# Ruff configuration
[tool.ruff]
target-version = "py310"
line-length = 88
select = [
    "E",   # pycodestyle errors
    "W",   # pycodestyle warnings
    "F",   # Pyflakes
    "I",   # isort
    "B",   # flake8-bugbear
    "C4",  # flake8-comprehensions
    "UP",  # pyupgrade
]
ignore = [
    "E501",  # line too long (handled by black)
]

[tool.ruff.isort]
known-first-party = ["archaeon_scientific"]

# Black configuration
[tool.black]
line-length = 88
target-version = ["py310"]

# Mypy configuration
[tool.mypy]
python_version = "3.10"
warn_return_any = true
warn_unused_ignores = true
disallow_untyped_defs = true
plugins = ["numpy.typing.mypy_plugin"]

[[tool.mypy.overrides]]
module = ["scipy.*", "pandas.*"]
ignore_missing_imports = true

# Pytest configuration
[tool.pytest.ini_options]
testpaths = ["tests"]
python_files = ["test_*.py"]
addopts = "-v --cov=archaeon_scientific --cov-report=term-missing"
```

### 5.3 Publicacion y Distribucion

```python
"""
Package Publishing Workflow.
"""

PUBLISHING_WORKFLOW = """
Package Publishing Workflow
===========================

1. PREPARATION
--------------
# Update version in pyproject.toml or _version.py
# Update CHANGELOG.md

# Ensure clean state
git status
git stash  # if needed

# Install build tools
pip install build twine

2. BUILD PACKAGE
----------------
# Build source distribution and wheel
python -m build

# This creates:
#   dist/
#       my_package-1.0.0.tar.gz      (sdist)
#       my_package-1.0.0-py3-none-any.whl  (wheel)

3. VERIFY PACKAGE
-----------------
# Check package contents
tar tzf dist/my_package-1.0.0.tar.gz
unzip -l dist/my_package-1.0.0-py3-none-any.whl

# Check package metadata
twine check dist/*

# Test installation in clean environment
pip install virtualenv
virtualenv test_env
source test_env/bin/activate  # or test_env\\Scripts\\activate on Windows
pip install dist/my_package-1.0.0-py3-none-any.whl
python -c "import my_package; print(my_package.__version__)"
deactivate

4. PUBLISH TO TESTPYPI (OPTIONAL)
---------------------------------
# Upload to TestPyPI first
twine upload --repository testpypi dist/*

# Install from TestPyPI to verify
pip install --index-url https://test.pypi.org/simple/ my_package

5. PUBLISH TO PYPI
------------------
# Upload to real PyPI
twine upload dist/*

# Or use trusted publishing with GitHub Actions

6. TAG RELEASE
--------------
git tag -a v1.0.0 -m "Release version 1.0.0"
git push origin v1.0.0
"""

print(PUBLISHING_WORKFLOW)
```

---

## 6. TYPE CHECKING CON MYPY

### 6.1 Configuracion de mypy

```python
"""
Type Checking with mypy for Scientific Python.

mypy provides static type checking, catching errors before runtime.
Especially valuable for scientific code where type errors
can lead to subtle numerical bugs.
"""

from typing import (
    TypeVar, Generic, Protocol, Callable,
    overload, Final, Literal, TypeAlias,
    TypedDict, NamedTuple, Annotated
)
from typing_extensions import Self
import numpy as np
from numpy.typing import NDArray, ArrayLike
from dataclasses import dataclass

# Type variables for generic types
T = TypeVar("T")
NumericT = TypeVar("NumericT", int, float, complex)
ArrayT = TypeVar("ArrayT", bound=np.ndarray)

# Type aliases for scientific computing
Vector = NDArray[np.float64]
Matrix = NDArray[np.float64]
ComplexVector = NDArray[np.complex128]
ScalarFunc = Callable[[float], float]
VectorFunc = Callable[[Vector], Vector]

# Annotated types for documentation and validation
PositiveFloat = Annotated[float, "must be positive"]
Probability = Annotated[float, "must be between 0 and 1"]
UnitVector = Annotated[Vector, "must have unit length"]


# Protocol for duck typing
class Integrable(Protocol):
    """Protocol for objects that can be integrated."""

    def __call__(self, x: float) -> float: ...


class Differentiable(Protocol):
    """Protocol for objects that can be differentiated."""

    def __call__(self, x: float) -> float: ...
    def derivative(self, x: float) -> float: ...


# Generic class with type constraints
class NumericalResult(Generic[NumericT]):
    """
    Generic numerical result container.

    Type parameter ensures consistent types throughout.
    """

    def __init__(self, value: NumericT, error: NumericT) -> None:
        self.value = value
        self.error = error

    def relative_error(self) -> float:
        """Calculate relative error."""
        if abs(self.value) < 1e-15:
            return float("inf")
        return abs(self.error) / abs(self.value)


# Overloaded functions
@overload
def normalize(data: float) -> float: ...

@overload
def normalize(data: list[float]) -> list[float]: ...

@overload
def normalize(data: Vector) -> Vector: ...

def normalize(data: float | list[float] | Vector) -> float | list[float] | Vector:
    """
    Normalize data to unit range.

    Overloading provides precise return types based on input.
    """
    if isinstance(data, (int, float)):
        return 1.0 if data != 0 else 0.0
    elif isinstance(data, list):
        max_val = max(abs(x) for x in data)
        if max_val == 0:
            return [0.0] * len(data)
        return [x / max_val for x in data]
    else:
        max_val = np.max(np.abs(data))
        if max_val == 0:
            return np.zeros_like(data)
        return data / max_val


# TypedDict for configuration
class SolverConfig(TypedDict):
    """Configuration for numerical solver."""
    method: Literal["newton", "bisection", "brent"]
    tolerance: float
    max_iterations: int
    verbose: bool


class SolverConfigPartial(TypedDict, total=False):
    """Solver config with all optional fields."""
    method: Literal["newton", "bisection", "brent"]
    tolerance: float
    max_iterations: int
    verbose: bool


def create_solver_config(
    method: Literal["newton", "bisection", "brent"] = "newton",
    tolerance: float = 1e-10,
    max_iterations: int = 100,
    verbose: bool = False
) -> SolverConfig:
    """Create solver configuration with defaults."""
    return {
        "method": method,
        "tolerance": tolerance,
        "max_iterations": max_iterations,
        "verbose": verbose,
    }


# Self type for method chaining
@dataclass
class MatrixBuilder:
    """Builder for constructing matrices with method chaining."""

    _rows: int = 0
    _cols: int = 0
    _data: list[list[float]] | None = None

    def rows(self, n: int) -> Self:
        """Set number of rows."""
        self._rows = n
        return self

    def cols(self, n: int) -> Self:
        """Set number of columns."""
        self._cols = n
        return self

    def zeros(self) -> Self:
        """Initialize with zeros."""
        self._data = [[0.0] * self._cols for _ in range(self._rows)]
        return self

    def identity(self) -> Self:
        """Initialize as identity matrix."""
        if self._rows != self._cols:
            raise ValueError("Identity requires square matrix")
        self._data = [
            [1.0 if i == j else 0.0 for j in range(self._cols)]
            for i in range(self._rows)
        ]
        return self

    def build(self) -> Matrix:
        """Build the matrix."""
        if self._data is None:
            raise ValueError("Matrix not initialized")
        return np.array(self._data, dtype=np.float64)
```

### 6.2 Patrones de Tipado Cientifico

```python
"""
Scientific Typing Patterns.

Demonstrate type annotations for common scientific patterns.
"""

from typing import TypeVar, Generic, Protocol, Callable, Iterator
from dataclasses import dataclass, field
import numpy as np
from numpy.typing import NDArray

# Type-safe scientific functions
def integrate_simpson(
    f: Callable[[float], float],
    a: float,
    b: float,
    n: int = 100
) -> float:
    """
    Integrate function using Simpson's rule.

    Type annotations ensure:
    - f is callable with float and returns float
    - a, b are floats
    - n is int
    - returns float
    """
    if n % 2 != 0:
        n += 1

    h = (b - a) / n
    result = f(a) + f(b)

    for i in range(1, n):
        coefficient = 4 if i % 2 == 1 else 2
        result += coefficient * f(a + i * h)

    return result * h / 3


# Type-safe matrix operations
def solve_linear_system(
    A: Matrix,
    b: Vector
) -> Vector:
    """
    Solve linear system Ax = b.

    Type annotations ensure correct array types.
    """
    return np.linalg.solve(A, b)


def eigenvalue_decomposition(
    A: Matrix
) -> tuple[Vector, Matrix]:
    """
    Compute eigenvalue decomposition.

    Returns typed tuple of eigenvalues and eigenvectors.
    """
    eigenvalues, eigenvectors = np.linalg.eig(A)
    return eigenvalues.real, eigenvectors.real


# Generic iterator for numerical sequences
class NumericalSequence(Generic[T], Iterator[T]):
    """
    Generic numerical sequence iterator.

    T can be float, complex, or Vector.
    """

    def __init__(
        self,
        initial: T,
        step_func: Callable[[T], T],
        max_iterations: int = 1000
    ) -> None:
        self.current = initial
        self.step_func = step_func
        self.max_iterations = max_iterations
        self.iteration = 0

    def __iter__(self) -> Iterator[T]:
        return self

    def __next__(self) -> T:
        if self.iteration >= self.max_iterations:
            raise StopIteration

        result = self.current
        self.current = self.step_func(self.current)
        self.iteration += 1
        return result


# Protocol for numerical methods
class NumericalMethod(Protocol):
    """Protocol for numerical methods."""

    name: str

    def solve(
        self,
        func: Callable[[float], float],
        x0: float
    ) -> float:
        """Solve the problem."""
        ...

    def set_tolerance(self, tol: float) -> None:
        """Set convergence tolerance."""
        ...


@dataclass
class NewtonMethod:
    """Newton-Raphson method implementing NumericalMethod protocol."""

    name: str = "Newton-Raphson"
    tolerance: float = 1e-10
    max_iterations: int = 100

    def solve(
        self,
        func: Callable[[float], float],
        x0: float
    ) -> float:
        """Find root using Newton-Raphson."""
        x = x0
        h = 1e-8

        for _ in range(self.max_iterations):
            f_val = func(x)
            if abs(f_val) < self.tolerance:
                return x

            # Numerical derivative
            df_val = (func(x + h) - func(x - h)) / (2 * h)

            if abs(df_val) < 1e-15:
                raise ValueError("Derivative too small")

            x = x - f_val / df_val

        raise ValueError("Did not converge")

    def set_tolerance(self, tol: float) -> None:
        """Set convergence tolerance."""
        self.tolerance = tol


def run_method(
    method: NumericalMethod,
    func: Callable[[float], float],
    x0: float
) -> float:
    """
    Run any numerical method that satisfies the protocol.

    This function accepts any object that implements NumericalMethod.
    """
    print(f"Running {method.name}")
    return method.solve(func, x0)
```

---

## 7. TESTING DE CODIGO CIENTIFICO

### 7.1 Fundamentos de Testing Cientifico

```python
"""
Testing Scientific Python Code.

Scientific testing requires special considerations:
- Floating-point comparison with tolerance
- Reference solutions and analytical checks
- Numerical convergence verification
- Edge cases (singularities, numerical limits)
"""

import pytest
import numpy as np
from numpy.testing import assert_allclose, assert_array_equal
from typing import Callable
from dataclasses import dataclass

# Pytest fixtures for scientific testing
@pytest.fixture
def random_seed():
    """Set random seed for reproducibility."""
    np.random.seed(42)
    return 42


@pytest.fixture
def sample_matrices():
    """Provide sample matrices for testing."""
    np.random.seed(42)

    return {
        "small": np.random.randn(5, 5),
        "medium": np.random.randn(50, 50),
        "large": np.random.randn(200, 200),
        "spd": None,  # Will be set below
        "singular": None,
        "ill_conditioned": None,
    }


@pytest.fixture
def spd_matrix():
    """Symmetric Positive Definite matrix."""
    np.random.seed(42)
    A = np.random.randn(50, 50)
    return A @ A.T + 50 * np.eye(50)


@pytest.fixture
def test_functions():
    """Test functions with known integrals."""
    return {
        "sin": (np.sin, 0, np.pi, 2.0),  # func, a, b, exact
        "exp": (np.exp, 0, 1, np.e - 1),
        "poly": (lambda x: x**2, 0, 1, 1/3),
    }


# Test class for numerical algorithms
class TestNumericalIntegration:
    """Tests for numerical integration routines."""

    def test_simpson_exact_for_polynomials(self):
        """Simpson's rule is exact for polynomials up to degree 3."""
        # Integrate x^3 from 0 to 1
        def f(x):
            return x ** 3

        result = integrate_simpson(f, 0, 1, n=2)  # Minimum n
        expected = 0.25

        assert_allclose(result, expected, rtol=1e-10)

    def test_simpson_convergence(self):
        """Test that error decreases as n increases."""
        def f(x):
            return np.sin(x)

        a, b = 0, np.pi
        exact = 2.0

        errors = []
        for n in [10, 20, 40, 80, 160]:
            result = integrate_simpson(f, a, b, n)
            errors.append(abs(result - exact))

        # Simpson's rule has O(h^4) convergence
        # Error should decrease by factor of ~16 when n doubles
        for i in range(1, len(errors)):
            ratio = errors[i-1] / errors[i]
            assert ratio > 14, f"Convergence ratio {ratio} too low"

    @pytest.mark.parametrize("func_name", ["sin", "exp", "poly"])
    def test_multiple_functions(self, test_functions, func_name):
        """Test integration against known exact values."""
        func, a, b, exact = test_functions[func_name]
        result = integrate_simpson(func, a, b, n=100)
        assert_allclose(result, exact, rtol=1e-6)


class TestLinearAlgebra:
    """Tests for linear algebra routines."""

    def test_solve_random_system(self, random_seed):
        """Test solving random linear system."""
        n = 50
        A = np.random.randn(n, n) + n * np.eye(n)  # Well-conditioned
        x_true = np.random.randn(n)
        b = A @ x_true

        x_computed = np.linalg.solve(A, b)

        assert_allclose(x_computed, x_true, rtol=1e-10)

    def test_solve_residual(self, spd_matrix):
        """Test that solution satisfies Ax = b."""
        A = spd_matrix
        b = np.random.randn(A.shape[0])

        x = np.linalg.solve(A, b)
        residual = A @ x - b

        assert np.linalg.norm(residual) < 1e-10

    def test_eigenvalue_decomposition(self, spd_matrix):
        """Test eigenvalue decomposition of SPD matrix."""
        A = spd_matrix

        eigenvalues, eigenvectors = np.linalg.eigh(A)

        # All eigenvalues should be positive for SPD
        assert np.all(eigenvalues > 0)

        # Reconstruction: A = V @ Lambda @ V^T
        Lambda = np.diag(eigenvalues)
        A_reconstructed = eigenvectors @ Lambda @ eigenvectors.T

        assert_allclose(A_reconstructed, A, rtol=1e-10)

    @pytest.mark.parametrize("n", [10, 50, 100])
    def test_various_sizes(self, n):
        """Test with various matrix sizes."""
        np.random.seed(42)
        A = np.random.randn(n, n) + n * np.eye(n)
        b = np.random.randn(n)

        x = np.linalg.solve(A, b)

        assert_allclose(A @ x, b, rtol=1e-10)


# Property-based testing for numerical code
class TestNumericalProperties:
    """Property-based tests for numerical operations."""

    def test_norm_positivity(self):
        """Norm should always be non-negative."""
        for _ in range(100):
            v = np.random.randn(100)
            norm = np.linalg.norm(v)
            assert norm >= 0

    def test_norm_zero_iff_zero_vector(self):
        """Norm is zero if and only if vector is zero."""
        # Non-zero vector
        v = np.array([1.0, 2.0, 3.0])
        assert np.linalg.norm(v) > 0

        # Zero vector
        v_zero = np.zeros(10)
        assert np.linalg.norm(v_zero) == 0

    def test_matrix_inverse_identity(self):
        """A @ A^(-1) should be identity."""
        np.random.seed(42)
        for n in [5, 10, 20]:
            A = np.random.randn(n, n) + n * np.eye(n)
            A_inv = np.linalg.inv(A)
            product = A @ A_inv

            assert_allclose(product, np.eye(n), atol=1e-10)

    def test_dot_product_commutativity(self):
        """Dot product should be commutative."""
        for _ in range(100):
            a = np.random.randn(50)
            b = np.random.randn(50)

            assert_allclose(np.dot(a, b), np.dot(b, a))
```

### 7.2 Testing Avanzado

```python
"""
Advanced Testing Patterns for Scientific Code.
"""

import pytest
import numpy as np
from hypothesis import given, strategies as st, settings
from hypothesis.extra.numpy import arrays
from typing import Callable

# Hypothesis for property-based testing
class TestWithHypothesis:
    """Property-based tests using Hypothesis."""

    @given(arrays(
        dtype=np.float64,
        shape=st.integers(1, 100),
        elements=st.floats(-1e6, 1e6, allow_nan=False, allow_infinity=False)
    ))
    @settings(max_examples=100)
    def test_sum_invariants(self, arr):
        """Test sum invariants with random arrays."""
        # Sum should be between n*min and n*max
        s = np.sum(arr)
        n = len(arr)

        if n > 0:
            assert s >= n * np.min(arr) - 1e-10
            assert s <= n * np.max(arr) + 1e-10

    @given(
        st.floats(-100, 100, allow_nan=False),
        st.floats(-100, 100, allow_nan=False)
    )
    def test_distance_symmetry(self, a, b):
        """Distance should be symmetric."""
        dist_ab = abs(a - b)
        dist_ba = abs(b - a)
        assert np.isclose(dist_ab, dist_ba)


# Benchmark tests
class TestPerformance:
    """Performance regression tests."""

    def test_matrix_multiply_time(self, benchmark):
        """Matrix multiplication should complete in reasonable time."""
        n = 500
        A = np.random.randn(n, n)
        B = np.random.randn(n, n)

        result = benchmark(lambda: A @ B)

        assert result is not None

    def test_solve_time(self, benchmark):
        """Linear solve should complete in reasonable time."""
        n = 500
        A = np.random.randn(n, n) + n * np.eye(n)
        b = np.random.randn(n)

        result = benchmark(lambda: np.linalg.solve(A, b))

        assert len(result) == n


# Test fixtures for numerical edge cases
@pytest.fixture
def edge_case_matrices():
    """Matrices that test edge cases."""
    return {
        "nearly_singular": np.array([
            [1, 1],
            [1, 1 + 1e-10]
        ]),
        "hilbert_3": 1.0 / (np.arange(3)[:, None] + np.arange(3) + 1),
        "zero": np.zeros((3, 3)),
        "identity": np.eye(5),
    }


class TestEdgeCases:
    """Test numerical edge cases."""

    def test_nearly_singular_detection(self, edge_case_matrices):
        """Detect nearly singular matrices."""
        A = edge_case_matrices["nearly_singular"]
        cond = np.linalg.cond(A)

        assert cond > 1e10  # High condition number

    def test_hilbert_ill_conditioning(self, edge_case_matrices):
        """Hilbert matrices are ill-conditioned."""
        H = edge_case_matrices["hilbert_3"]
        cond = np.linalg.cond(H)

        assert cond > 100

    def test_zero_matrix_handling(self, edge_case_matrices):
        """Handle zero matrix appropriately."""
        Z = edge_case_matrices["zero"]

        # Rank should be 0
        assert np.linalg.matrix_rank(Z) == 0

        # Determinant should be 0
        assert np.linalg.det(Z) == 0

    def test_identity_properties(self, edge_case_matrices):
        """Identity matrix should have unit eigenvalues."""
        I = edge_case_matrices["identity"]

        eigenvalues = np.linalg.eigvalsh(I)
        assert_allclose(eigenvalues, np.ones(5))
```

---

## 8. PATRONES AVANZADOS Y MEJORES PRACTICAS

### 8.1 Patrones de Diseno para Ciencia

```python
"""
Design Patterns for Scientific Python.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Protocol, Callable, TypeVar, Generic
import numpy as np
from contextlib import contextmanager

# Strategy Pattern for algorithms
class IntegrationStrategy(Protocol):
    """Protocol for integration strategies."""

    def integrate(
        self,
        f: Callable[[float], float],
        a: float,
        b: float
    ) -> float:
        """Perform integration."""
        ...


@dataclass
class SimpsonIntegration:
    """Simpson's rule integration strategy."""
    n_points: int = 100

    def integrate(
        self,
        f: Callable[[float], float],
        a: float,
        b: float
    ) -> float:
        """Integrate using Simpson's rule."""
        n = self.n_points
        h = (b - a) / n
        result = f(a) + f(b)

        for i in range(1, n):
            coef = 4 if i % 2 == 1 else 2
            result += coef * f(a + i * h)

        return result * h / 3


@dataclass
class GaussQuadrature:
    """Gauss quadrature integration strategy."""
    n_points: int = 10

    def integrate(
        self,
        f: Callable[[float], float],
        a: float,
        b: float
    ) -> float:
        """Integrate using Gauss-Legendre quadrature."""
        from scipy.special import roots_legendre

        nodes, weights = roots_legendre(self.n_points)

        # Transform from [-1, 1] to [a, b]
        x = 0.5 * (b - a) * nodes + 0.5 * (a + b)
        w = 0.5 * (b - a) * weights

        return np.sum(w * f(x))


class Integrator:
    """Integrator using strategy pattern."""

    def __init__(self, strategy: IntegrationStrategy):
        self.strategy = strategy

    def integrate(
        self,
        f: Callable[[float], float],
        a: float,
        b: float
    ) -> float:
        """Delegate to strategy."""
        return self.strategy.integrate(f, a, b)


# Factory Pattern for solvers
class SolverFactory:
    """Factory for creating numerical solvers."""

    _solvers: dict[str, type] = {}

    @classmethod
    def register(cls, name: str, solver_class: type) -> None:
        """Register a solver class."""
        cls._solvers[name] = solver_class

    @classmethod
    def create(cls, name: str, **kwargs) -> "NumericalSolver":
        """Create a solver instance."""
        if name not in cls._solvers:
            raise ValueError(f"Unknown solver: {name}")
        return cls._solvers[name](**kwargs)


@dataclass
class NumericalSolver(ABC):
    """Abstract base for numerical solvers."""
    tolerance: float = 1e-10
    max_iterations: int = 100

    @abstractmethod
    def solve(self, *args, **kwargs):
        """Solve the problem."""
        pass


# Observer Pattern for convergence monitoring
@dataclass
class ConvergenceObserver:
    """Observer for monitoring convergence."""
    history: list[float] = field(default_factory=list)
    verbose: bool = True

    def update(self, iteration: int, residual: float) -> None:
        """Update with new iteration data."""
        self.history.append(residual)
        if self.verbose:
            print(f"Iteration {iteration}: residual = {residual:.2e}")

    def is_converged(self, tolerance: float) -> bool:
        """Check if converged."""
        if not self.history:
            return False
        return self.history[-1] < tolerance


# Builder Pattern for simulations
@dataclass
class SimulationBuilder:
    """Builder for simulation setup."""
    _domain: tuple[float, float] | None = None
    _initial_condition: Callable | None = None
    _boundary_conditions: dict = field(default_factory=dict)
    _time_span: tuple[float, float] = (0.0, 1.0)
    _dt: float = 0.01

    def domain(self, a: float, b: float) -> "SimulationBuilder":
        """Set spatial domain."""
        self._domain = (a, b)
        return self

    def initial_condition(self, func: Callable) -> "SimulationBuilder":
        """Set initial condition."""
        self._initial_condition = func
        return self

    def boundary_condition(
        self,
        side: str,
        value: float | Callable
    ) -> "SimulationBuilder":
        """Add boundary condition."""
        self._boundary_conditions[side] = value
        return self

    def time_span(self, t0: float, tf: float) -> "SimulationBuilder":
        """Set time span."""
        self._time_span = (t0, tf)
        return self

    def time_step(self, dt: float) -> "SimulationBuilder":
        """Set time step."""
        self._dt = dt
        return self

    def build(self) -> "Simulation":
        """Build the simulation."""
        if self._domain is None:
            raise ValueError("Domain not set")
        if self._initial_condition is None:
            raise ValueError("Initial condition not set")

        return Simulation(
            domain=self._domain,
            initial_condition=self._initial_condition,
            boundary_conditions=self._boundary_conditions,
            time_span=self._time_span,
            dt=self._dt
        )


@dataclass
class Simulation:
    """Simulation configuration."""
    domain: tuple[float, float]
    initial_condition: Callable
    boundary_conditions: dict
    time_span: tuple[float, float]
    dt: float
```

### 8.2 Mejores Practicas Resumen

```python
"""
Best Practices Summary for Scientific Python.
"""

BEST_PRACTICES = """
ARCHAEON Scientific Python Best Practices
==========================================

1. PERFORMANCE
--------------
- Profile before optimizing
- Use NumPy vectorization instead of loops
- Choose appropriate data types (float32 vs float64)
- Consider memory layout (C vs Fortran order)
- Use specialized libraries (SciPy, numba) for hot paths

2. CORRECTNESS
--------------
- Use type hints throughout
- Run mypy for static type checking
- Write comprehensive tests with pytest
- Include property-based tests (Hypothesis)
- Test edge cases and numerical limits

3. REPRODUCIBILITY
------------------
- Set random seeds for testing
- Document dependencies with exact versions
- Use virtual environments
- Include requirements.txt or pyproject.toml

4. CODE QUALITY
---------------
- Follow PEP 8 (use black for formatting)
- Use meaningful variable names
- Write docstrings for all public functions
- Keep functions focused and small
- Avoid global state

5. DOCUMENTATION
----------------
- Document units and conventions
- Explain algorithm choices
- Include usage examples
- Document numerical precision requirements

6. TESTING
----------
- Test against analytical solutions when possible
- Verify convergence rates
- Test with various problem sizes
- Include regression tests for performance

7. PARALLELIZATION
------------------
- Use multiprocessing for CPU-bound tasks
- Use asyncio for I/O-bound tasks
- Consider joblib for simple parallelism
- Be aware of GIL limitations

8. ERROR HANDLING
-----------------
- Check input validity early
- Provide informative error messages
- Handle numerical exceptions (NaN, overflow)
- Use logging for debugging

9. LEGACY INTEGRATION
---------------------
- Wrap Fortran with f2py
- Wrap C with ctypes or cffi
- Validate against legacy implementations
- Document any behavioral differences

10. DISTRIBUTION
----------------
- Use pyproject.toml for configuration
- Include type stubs (py.typed)
- Publish to PyPI for easy installation
- Provide conda packages for complex dependencies
"""

print(BEST_PRACTICES)
```

---

## RESUMEN

Este modulo cubre tecnicas avanzadas para Python cientifico:

1. **Optimizacion**: Perfilado, vectorizacion, gestion de memoria
2. **Profiling**: Identificacion sistematica de cuellos de botella
3. **Asyncio**: Paralelismo para operaciones I/O
4. **Multiprocessing**: Paralelismo real para CPU
5. **Packaging**: Distribucion moderna de paquetes
6. **mypy**: Type checking estatico
7. **Testing**: Testing especializado para codigo cientifico
8. **Patrones**: Patrones de diseno para ciencia

Puntos clave para ARCHAEON:
- Siempre perfilar antes de optimizar
- NumPy vectorizado supera bucles Python
- multiprocessing para CPU-bound, asyncio para I/O-bound
- Type hints mejoran calidad y documentacion
- Tests deben verificar propiedades numericas
- Estructura de paquetes moderna facilita distribucion

---

*ARCHAEON CORE - Bridging Legacy to Modern*
*Advanced Python v1.0.0*

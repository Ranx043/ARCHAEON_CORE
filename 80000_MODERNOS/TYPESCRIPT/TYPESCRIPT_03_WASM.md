---
título: "TypeScript 03 - WebAssembly y Integración con Código Legacy"
módulo: ARCHAEON_CORE
sección: 80000_MODERNOS/TYPESCRIPT
versión: 1.0.0
fecha_creación: 2025-12-31
autor: ARCHAEON Sistema de Documentación
descripción: >
  Integración de WebAssembly con TypeScript, compilación desde C/C++ y Rust,
  WASI, compartición de memoria y optimización de rendimiento.
tags:
  - typescript
  - webassembly
  - wasm
  - rust
  - c
  - wasi
  - performance
requisitos:
  - Node.js 18+
  - TypeScript 5.0+
  - Rust toolchain (para Rust->WASM)
  - Emscripten (para C/C++->WASM)
---

# TYPESCRIPT 03 - WEBASSEMBLY Y INTEGRACIÓN CON CÓDIGO LEGACY

## Tabla de Contenidos

1. [Fundamentos de WebAssembly](#fundamentos-de-webassembly)
2. [WebAssembly desde TypeScript](#webassembly-desde-typescript)
3. [Compilando C/C++ a WASM](#compilando-cc-a-wasm)
4. [Rust y wasm-bindgen](#rust-y-wasm-bindgen)
5. [WASI - WebAssembly System Interface](#wasi---webassembly-system-interface)
6. [Compartición de Memoria](#compartición-de-memoria)
7. [Optimización de Rendimiento](#optimización-de-rendimiento)
8. [Casos de Uso para Código Legacy](#casos-de-uso-para-código-legacy)

---

## Fundamentos de WebAssembly

### ¿Qué es WebAssembly?

WebAssembly (WASM) es un formato de instrucciones binarias para una máquina
virtual basada en stack. Está diseñado como un objetivo de compilación portable
para lenguajes de alto nivel como C, C++, Rust, y otros.

```
┌─────────────────────────────────────────────────────────────┐
│                    Flujo de Compilación                      │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌─────────┐     ┌──────────┐     ┌─────────┐              │
│  │  C/C++  │────▶│ Emscripten│────▶│  .wasm  │              │
│  └─────────┘     └──────────┘     └─────────┘              │
│                                        │                    │
│  ┌─────────┐     ┌──────────┐          │                    │
│  │  Rust   │────▶│wasm-pack │────▶     │                    │
│  └─────────┘     └──────────┘          │                    │
│                                        ▼                    │
│                              ┌─────────────────┐            │
│                              │  JavaScript/TS  │            │
│                              │    Runtime      │            │
│                              └─────────────────┘            │
│                                        │                    │
│                              ┌─────────┴─────────┐          │
│                              ▼                   ▼          │
│                        ┌──────────┐       ┌──────────┐      │
│                        │ Browser  │       │ Node.js  │      │
│                        └──────────┘       └──────────┘      │
└─────────────────────────────────────────────────────────────┘
```

### Características Principales

| Característica   | Descripción                                      |
| ---------------- | ------------------------------------------------ |
| Rendimiento      | Near-native speed (1.1-1.5x native code)         |
| Seguridad        | Sandboxed execution environment                  |
| Portabilidad     | Runs on any platform with WASM runtime           |
| Interoperabilidad| Direct integration with JavaScript/TypeScript    |
| Tamaño           | Compact binary format                            |
| Determinismo     | Consistent behavior across platforms             |

### Conceptos Clave

```typescript
// WebAssembly module structure
interface WasmModule {
  // Imports: Functions/memory provided by host (JS/TS)
  imports: {
    env: {
      memory: WebAssembly.Memory;
      log: (ptr: number, len: number) => void;
    };
  };

  // Exports: Functions/memory exposed to host
  exports: {
    add: (a: number, b: number) => number;
    memory: WebAssembly.Memory;
    allocate: (size: number) => number;
    deallocate: (ptr: number) => void;
  };
}

// WebAssembly value types
type WasmValueType = "i32" | "i64" | "f32" | "f64" | "v128" | "funcref" | "externref";

// Linear memory model
// WASM memory is a resizable ArrayBuffer
const memory = new WebAssembly.Memory({
  initial: 256, // 256 pages (16MB)
  maximum: 512, // 512 pages (32MB)
  shared: true, // For threading (optional)
});
```

---

## WebAssembly desde TypeScript

### Carga Básica de Módulos WASM

```typescript
// src/wasm/loader.ts
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

const __dirname = dirname(fileURLToPath(import.meta.url));

// Generic WASM module loader
async function loadWasmModule<T extends WebAssembly.Exports>(
  wasmPath: string,
  imports: WebAssembly.Imports = {}
): Promise<{ instance: WebAssembly.Instance; exports: T }> {
  const wasmBuffer = await readFile(wasmPath);
  const wasmModule = await WebAssembly.compile(wasmBuffer);
  const instance = await WebAssembly.instantiate(wasmModule, imports);

  return {
    instance,
    exports: instance.exports as T,
  };
}

// Streaming instantiation for browser
async function loadWasmStreaming<T extends WebAssembly.Exports>(
  wasmUrl: string,
  imports: WebAssembly.Imports = {}
): Promise<{ instance: WebAssembly.Instance; exports: T }> {
  const response = await fetch(wasmUrl);
  const { instance } = await WebAssembly.instantiateStreaming(response, imports);

  return {
    instance,
    exports: instance.exports as T,
  };
}

// Type-safe WASM module interface
interface MathWasmExports extends WebAssembly.Exports {
  add: (a: number, b: number) => number;
  multiply: (a: number, b: number) => number;
  factorial: (n: number) => number;
  fibonacci: (n: number) => number;
  memory: WebAssembly.Memory;
}

// Load and use WASM module
async function useMathWasm() {
  const wasmPath = join(__dirname, "../wasm/math.wasm");
  const { exports } = await loadWasmModule<MathWasmExports>(wasmPath);

  console.log("add(5, 3):", exports.add(5, 3));
  console.log("multiply(4, 7):", exports.multiply(4, 7));
  console.log("factorial(10):", exports.factorial(10));
  console.log("fibonacci(40):", exports.fibonacci(40));
}
```

### Wrapper Pattern Tipado

```typescript
// src/wasm/math-wrapper.ts
interface MathModuleExports {
  add: (a: number, b: number) => number;
  subtract: (a: number, b: number) => number;
  multiply: (a: number, b: number) => number;
  divide: (a: number, b: number) => number;
  power: (base: number, exp: number) => number;
  sqrt: (n: number) => number;
  memory: WebAssembly.Memory;
  __heap_base: WebAssembly.Global;
}

export class WasmMath {
  private exports: MathModuleExports;
  private memoryView: Float64Array;

  private constructor(exports: MathModuleExports) {
    this.exports = exports;
    this.memoryView = new Float64Array(exports.memory.buffer);
  }

  static async create(wasmPath: string): Promise<WasmMath> {
    const wasmBuffer = await import("node:fs/promises").then((fs) =>
      fs.readFile(wasmPath)
    );
    const module = await WebAssembly.compile(wasmBuffer);
    const instance = await WebAssembly.instantiate(module, {});

    return new WasmMath(instance.exports as MathModuleExports);
  }

  add(a: number, b: number): number {
    return this.exports.add(a, b);
  }

  subtract(a: number, b: number): number {
    return this.exports.subtract(a, b);
  }

  multiply(a: number, b: number): number {
    return this.exports.multiply(a, b);
  }

  divide(a: number, b: number): number {
    if (b === 0) {
      throw new Error("Division by zero");
    }
    return this.exports.divide(a, b);
  }

  power(base: number, exp: number): number {
    return this.exports.power(base, exp);
  }

  sqrt(n: number): number {
    if (n < 0) {
      throw new Error("Cannot compute square root of negative number");
    }
    return this.exports.sqrt(n);
  }

  // Batch operations using shared memory
  batchAdd(values: number[]): number {
    const offset = this.exports.__heap_base.value / 8; // Float64 offset

    // Write values to WASM memory
    for (let i = 0; i < values.length; i++) {
      this.memoryView[offset + i] = values[i];
    }

    // Sum all values
    let sum = 0;
    for (let i = 0; i < values.length; i++) {
      sum = this.exports.add(sum, this.memoryView[offset + i]);
    }

    return sum;
  }
}

// Usage
async function main() {
  const math = await WasmMath.create("./math.wasm");

  console.log("10 + 5 =", math.add(10, 5));
  console.log("2^10 =", math.power(2, 10));
  console.log("sqrt(144) =", math.sqrt(144));

  const values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  console.log("Sum of 1-10:", math.batchAdd(values));
}
```

---

## Compilando C/C++ a WASM

### Configuración de Emscripten

```bash
# Install Emscripten
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
./emsdk install latest
./emsdk activate latest
source ./emsdk_env.sh

# Verify installation
emcc --version
```

### Código C de Ejemplo

```c
// src/native/math.c
#include <emscripten.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

// Export macro for WebAssembly
#define WASM_EXPORT EMSCRIPTEN_KEEPALIVE

// Basic arithmetic
WASM_EXPORT
int add(int a, int b) {
    return a + b;
}

WASM_EXPORT
int multiply(int a, int b) {
    return a * b;
}

WASM_EXPORT
double power(double base, int exp) {
    return pow(base, exp);
}

// String processing
WASM_EXPORT
int string_length(const char* str) {
    return strlen(str);
}

WASM_EXPORT
void string_uppercase(char* str) {
    while (*str) {
        if (*str >= 'a' && *str <= 'z') {
            *str = *str - 32;
        }
        str++;
    }
}

// Array operations
WASM_EXPORT
int array_sum(int* arr, int length) {
    int sum = 0;
    for (int i = 0; i < length; i++) {
        sum += arr[i];
    }
    return sum;
}

WASM_EXPORT
void array_sort(int* arr, int length) {
    for (int i = 0; i < length - 1; i++) {
        for (int j = 0; j < length - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                int temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
}

// Memory management
WASM_EXPORT
void* allocate(size_t size) {
    return malloc(size);
}

WASM_EXPORT
void deallocate(void* ptr) {
    free(ptr);
}

// Image processing example
typedef struct {
    unsigned char r, g, b, a;
} Pixel;

WASM_EXPORT
void grayscale(Pixel* pixels, int width, int height) {
    int total = width * height;
    for (int i = 0; i < total; i++) {
        unsigned char gray = (pixels[i].r * 0.299 +
                             pixels[i].g * 0.587 +
                             pixels[i].b * 0.114);
        pixels[i].r = gray;
        pixels[i].g = gray;
        pixels[i].b = gray;
    }
}

WASM_EXPORT
void invert_colors(Pixel* pixels, int width, int height) {
    int total = width * height;
    for (int i = 0; i < total; i++) {
        pixels[i].r = 255 - pixels[i].r;
        pixels[i].g = 255 - pixels[i].g;
        pixels[i].b = 255 - pixels[i].b;
    }
}
```

### Compilación

```bash
# Compile to WebAssembly
emcc src/native/math.c \
  -o dist/math.wasm \
  -s WASM=1 \
  -s EXPORTED_RUNTIME_METHODS='["ccall", "cwrap", "getValue", "setValue"]' \
  -s ALLOW_MEMORY_GROWTH=1 \
  -s INITIAL_MEMORY=16777216 \
  -s MAXIMUM_MEMORY=1073741824 \
  -s MODULARIZE=1 \
  -s EXPORT_ES6=1 \
  -s ENVIRONMENT='web,node' \
  -O3

# Generate TypeScript declarations
emcc src/native/math.c \
  -o dist/math.js \
  --emit-tsd math.d.ts
```

### TypeScript Integration

```typescript
// src/wasm/c-module.ts
import createModule from "../dist/math.js";

interface EmscriptenModule {
  _add: (a: number, b: number) => number;
  _multiply: (a: number, b: number) => number;
  _power: (base: number, exp: number) => number;
  _string_length: (ptr: number) => number;
  _string_uppercase: (ptr: number) => void;
  _array_sum: (ptr: number, length: number) => number;
  _array_sort: (ptr: number, length: number) => void;
  _allocate: (size: number) => number;
  _deallocate: (ptr: number) => void;
  _grayscale: (ptr: number, width: number, height: number) => void;
  _invert_colors: (ptr: number, width: number, height: number) => void;

  ccall: <T>(
    name: string,
    returnType: string,
    argTypes: string[],
    args: unknown[]
  ) => T;
  cwrap: <T extends (...args: any[]) => any>(
    name: string,
    returnType: string,
    argTypes: string[]
  ) => T;
  getValue: (ptr: number, type: string) => number;
  setValue: (ptr: number, value: number, type: string) => void;
  HEAPU8: Uint8Array;
  HEAPU32: Uint32Array;
  HEAPF64: Float64Array;
}

export class CWasmModule {
  private module: EmscriptenModule;

  private constructor(module: EmscriptenModule) {
    this.module = module;
  }

  static async create(): Promise<CWasmModule> {
    const module = await createModule();
    return new CWasmModule(module);
  }

  // Direct function calls
  add(a: number, b: number): number {
    return this.module._add(a, b);
  }

  multiply(a: number, b: number): number {
    return this.module._multiply(a, b);
  }

  power(base: number, exp: number): number {
    return this.module._power(base, exp);
  }

  // String operations with memory management
  processString(str: string): string {
    // Allocate memory for string
    const length = str.length + 1;
    const ptr = this.module._allocate(length);

    // Copy string to WASM memory
    for (let i = 0; i < str.length; i++) {
      this.module.HEAPU8[ptr + i] = str.charCodeAt(i);
    }
    this.module.HEAPU8[ptr + str.length] = 0; // Null terminator

    // Process string
    this.module._string_uppercase(ptr);

    // Read result
    let result = "";
    let i = 0;
    while (this.module.HEAPU8[ptr + i] !== 0) {
      result += String.fromCharCode(this.module.HEAPU8[ptr + i]);
      i++;
    }

    // Free memory
    this.module._deallocate(ptr);

    return result;
  }

  // Array operations
  sortArray(arr: number[]): number[] {
    const length = arr.length;
    const ptr = this.module._allocate(length * 4); // int32 = 4 bytes

    // Copy array to WASM memory
    for (let i = 0; i < length; i++) {
      this.module.HEAPU32[(ptr / 4) + i] = arr[i];
    }

    // Sort in place
    this.module._array_sort(ptr, length);

    // Read result
    const result: number[] = [];
    for (let i = 0; i < length; i++) {
      result.push(this.module.HEAPU32[(ptr / 4) + i]);
    }

    // Free memory
    this.module._deallocate(ptr);

    return result;
  }

  sumArray(arr: number[]): number {
    const length = arr.length;
    const ptr = this.module._allocate(length * 4);

    for (let i = 0; i < length; i++) {
      this.module.HEAPU32[(ptr / 4) + i] = arr[i];
    }

    const sum = this.module._array_sum(ptr, length);
    this.module._deallocate(ptr);

    return sum;
  }

  // Image processing
  processImageGrayscale(imageData: ImageData): ImageData {
    const { width, height, data } = imageData;
    const ptr = this.module._allocate(data.length);

    // Copy image data to WASM memory
    this.module.HEAPU8.set(data, ptr);

    // Process
    this.module._grayscale(ptr, width, height);

    // Read result
    const resultData = new Uint8ClampedArray(
      this.module.HEAPU8.slice(ptr, ptr + data.length)
    );

    this.module._deallocate(ptr);

    return new ImageData(resultData, width, height);
  }
}

// Usage example
async function main() {
  const wasm = await CWasmModule.create();

  console.log("add(10, 20):", wasm.add(10, 20));
  console.log("power(2, 8):", wasm.power(2, 8));
  console.log("uppercase:", wasm.processString("hello world"));
  console.log("sorted:", wasm.sortArray([5, 2, 8, 1, 9, 3]));
  console.log("sum:", wasm.sumArray([1, 2, 3, 4, 5]));
}
```

---

## Rust y wasm-bindgen

### Configuración del Proyecto Rust

```bash
# Install Rust and wasm-pack
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
cargo install wasm-pack

# Create new Rust WASM project
cargo new --lib my-wasm-lib
cd my-wasm-lib
```

### Cargo.toml

```toml
[package]
name = "my-wasm-lib"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib", "rlib"]

[dependencies]
wasm-bindgen = "0.2"
js-sys = "0.3"
web-sys = { version = "0.3", features = ["console", "Window", "Document"] }
serde = { version = "1.0", features = ["derive"] }
serde-wasm-bindgen = "0.6"
console_error_panic_hook = "0.1"

[profile.release]
lto = true
opt-level = 3

[package.metadata.wasm-pack.profile.release]
wasm-opt = ["-O4"]
```

### Código Rust

```rust
// src/lib.rs
use wasm_bindgen::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// Initialize panic hook for better error messages
#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

// Basic arithmetic functions
#[wasm_bindgen]
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[wasm_bindgen]
pub fn multiply(a: f64, b: f64) -> f64 {
    a * b
}

#[wasm_bindgen]
pub fn fibonacci(n: u32) -> u64 {
    if n <= 1 {
        return n as u64;
    }

    let mut a = 0u64;
    let mut b = 1u64;

    for _ in 2..=n {
        let c = a + b;
        a = b;
        b = c;
    }

    b
}

// String processing
#[wasm_bindgen]
pub fn reverse_string(s: &str) -> String {
    s.chars().rev().collect()
}

#[wasm_bindgen]
pub fn to_uppercase(s: &str) -> String {
    s.to_uppercase()
}

#[wasm_bindgen]
pub fn word_count(s: &str) -> usize {
    s.split_whitespace().count()
}

// Struct with wasm-bindgen
#[wasm_bindgen]
#[derive(Clone)]
pub struct Point {
    x: f64,
    y: f64,
}

#[wasm_bindgen]
impl Point {
    #[wasm_bindgen(constructor)]
    pub fn new(x: f64, y: f64) -> Point {
        Point { x, y }
    }

    #[wasm_bindgen(getter)]
    pub fn x(&self) -> f64 {
        self.x
    }

    #[wasm_bindgen(setter)]
    pub fn set_x(&mut self, x: f64) {
        self.x = x;
    }

    #[wasm_bindgen(getter)]
    pub fn y(&self) -> f64 {
        self.y
    }

    #[wasm_bindgen(setter)]
    pub fn set_y(&mut self, y: f64) {
        self.y = y;
    }

    pub fn distance(&self, other: &Point) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx * dx + dy * dy).sqrt()
    }

    pub fn translate(&mut self, dx: f64, dy: f64) {
        self.x += dx;
        self.y += dy;
    }
}

// Complex data structures with serde
#[derive(Serialize, Deserialize)]
pub struct User {
    id: u32,
    name: String,
    email: String,
    tags: Vec<String>,
}

#[wasm_bindgen]
pub fn process_user(val: JsValue) -> Result<JsValue, JsValue> {
    let user: User = serde_wasm_bindgen::from_value(val)?;

    let processed = User {
        id: user.id,
        name: user.name.to_uppercase(),
        email: user.email.to_lowercase(),
        tags: user.tags.iter().map(|t| t.trim().to_string()).collect(),
    };

    Ok(serde_wasm_bindgen::to_value(&processed)?)
}

// Array operations
#[wasm_bindgen]
pub fn sort_numbers(arr: &[i32]) -> Vec<i32> {
    let mut sorted = arr.to_vec();
    sorted.sort();
    sorted
}

#[wasm_bindgen]
pub fn sum_array(arr: &[f64]) -> f64 {
    arr.iter().sum()
}

#[wasm_bindgen]
pub fn filter_even(arr: &[i32]) -> Vec<i32> {
    arr.iter().filter(|&&x| x % 2 == 0).cloned().collect()
}

// Image processing with raw memory
#[wasm_bindgen]
pub fn grayscale(data: &mut [u8]) {
    for chunk in data.chunks_mut(4) {
        if chunk.len() == 4 {
            let gray = (0.299 * chunk[0] as f64 +
                       0.587 * chunk[1] as f64 +
                       0.114 * chunk[2] as f64) as u8;
            chunk[0] = gray;
            chunk[1] = gray;
            chunk[2] = gray;
            // chunk[3] (alpha) remains unchanged
        }
    }
}

#[wasm_bindgen]
pub fn blur(data: &mut [u8], width: usize, height: usize) {
    let mut output = data.to_vec();

    for y in 1..height-1 {
        for x in 1..width-1 {
            for c in 0..3 { // RGB channels
                let mut sum = 0u32;
                for dy in -1i32..=1 {
                    for dx in -1i32..=1 {
                        let idx = ((y as i32 + dy) as usize * width +
                                  (x as i32 + dx) as usize) * 4 + c;
                        sum += data[idx] as u32;
                    }
                }
                let idx = (y * width + x) * 4 + c;
                output[idx] = (sum / 9) as u8;
            }
        }
    }

    data.copy_from_slice(&output);
}

// Callbacks from Rust to JavaScript
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    #[wasm_bindgen(js_namespace = console)]
    fn error(s: &str);
}

#[wasm_bindgen]
pub fn log_message(msg: &str) {
    log(&format!("[WASM] {}", msg));
}

// Async operations
#[wasm_bindgen]
pub async fn async_compute(n: u32) -> u64 {
    // Simulate async work
    let result = fibonacci(n);
    result
}

// Error handling
#[wasm_bindgen]
pub fn safe_divide(a: f64, b: f64) -> Result<f64, JsValue> {
    if b == 0.0 {
        Err(JsValue::from_str("Division by zero"))
    } else {
        Ok(a / b)
    }
}
```

### Compilación y TypeScript Bindings

```bash
# Build with wasm-pack
wasm-pack build --target web --out-dir pkg

# For Node.js
wasm-pack build --target nodejs --out-dir pkg-node

# For bundler (webpack, vite, etc.)
wasm-pack build --target bundler --out-dir pkg-bundler
```

### TypeScript Usage

```typescript
// src/rust-wasm.ts
import init, {
  add,
  multiply,
  fibonacci,
  reverse_string,
  Point,
  process_user,
  grayscale,
  safe_divide,
} from "../pkg/my_wasm_lib.js";

async function main() {
  // Initialize WASM module
  await init();

  // Basic operations
  console.log("add(5, 3):", add(5, 3));
  console.log("multiply(2.5, 4.0):", multiply(2.5, 4.0));
  console.log("fibonacci(40):", fibonacci(40n));

  // String operations
  console.log("reverse:", reverse_string("Hello, WASM!"));

  // Class usage
  const p1 = new Point(0, 0);
  const p2 = new Point(3, 4);
  console.log("distance:", p1.distance(p2)); // 5

  p1.translate(1, 1);
  console.log("translated:", p1.x, p1.y);

  // Complex data
  const user = {
    id: 1,
    name: "alice",
    email: "ALICE@Example.COM",
    tags: ["  admin  ", "user"],
  };
  const processed = process_user(user);
  console.log("processed user:", processed);

  // Error handling
  try {
    const result = safe_divide(10, 0);
    console.log("result:", result);
  } catch (e) {
    console.error("Error:", e);
  }
}

main().catch(console.error);
```

---

## WASI - WebAssembly System Interface

### ¿Qué es WASI?

WASI (WebAssembly System Interface) proporciona un conjunto de APIs estándar
para que los módulos WASM interactúen con el sistema operativo de manera
portable y segura.

```typescript
// src/wasi/wasi-runner.ts
import { WASI } from "wasi";
import { readFile } from "node:fs/promises";
import { argv, env } from "node:process";

interface WasiOptions {
  args?: string[];
  env?: Record<string, string>;
  preopens?: Record<string, string>;
  stdin?: number;
  stdout?: number;
  stderr?: number;
}

async function runWasiModule(
  wasmPath: string,
  options: WasiOptions = {}
): Promise<number> {
  const wasi = new WASI({
    version: "preview1",
    args: options.args ?? [wasmPath],
    env: options.env ?? {},
    preopens: options.preopens ?? {},
    stdin: options.stdin,
    stdout: options.stdout,
    stderr: options.stderr,
  });

  const wasmBuffer = await readFile(wasmPath);
  const module = await WebAssembly.compile(wasmBuffer);

  const instance = await WebAssembly.instantiate(module, {
    wasi_snapshot_preview1: wasi.wasiImport,
  });

  // Start the WASI module
  const exitCode = wasi.start(instance);
  return exitCode;
}

// Example usage
async function main() {
  // Run a WASI program with file system access
  const exitCode = await runWasiModule("./hello.wasm", {
    args: ["hello", "--name", "World"],
    env: {
      HOME: "/home/user",
      PATH: "/usr/bin:/bin",
    },
    preopens: {
      "/sandbox": "./data", // Map /sandbox to local ./data
    },
  });

  console.log("Exit code:", exitCode);
}
```

### Rust WASI Application

```rust
// src/main.rs (for WASI)
use std::env;
use std::fs;
use std::io::{self, Read, Write};

fn main() -> io::Result<()> {
    // Read command line arguments
    let args: Vec<String> = env::args().collect();
    println!("Arguments: {:?}", args);

    // Read environment variables
    for (key, value) in env::vars() {
        println!("{}: {}", key, value);
    }

    // File operations (requires preopens)
    let contents = fs::read_to_string("/sandbox/input.txt")?;
    println!("File contents: {}", contents);

    // Write to file
    fs::write("/sandbox/output.txt", "Hello from WASI!")?;

    // Standard I/O
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    io::stdout().write_all(b"Received input\n")?;

    Ok(())
}
```

---

## Compartición de Memoria

### Memoria Compartida entre JS y WASM

```typescript
// src/wasm/shared-memory.ts
interface SharedMemoryExports {
  memory: WebAssembly.Memory;
  process_buffer: (ptr: number, len: number) => void;
  get_result_ptr: () => number;
  get_result_len: () => number;
}

class SharedMemoryManager {
  private memory: WebAssembly.Memory;
  private exports: SharedMemoryExports;
  private heapStart: number;
  private heapEnd: number;
  private allocations: Map<number, number> = new Map();

  constructor(exports: SharedMemoryExports, heapStart: number) {
    this.exports = exports;
    this.memory = exports.memory;
    this.heapStart = heapStart;
    this.heapEnd = heapStart;
  }

  // Allocate memory in WASM heap
  allocate(size: number): number {
    const ptr = this.heapEnd;
    this.heapEnd += size;

    // Grow memory if needed
    const pagesNeeded = Math.ceil(this.heapEnd / 65536);
    const currentPages = this.memory.buffer.byteLength / 65536;

    if (pagesNeeded > currentPages) {
      this.memory.grow(pagesNeeded - currentPages);
    }

    this.allocations.set(ptr, size);
    return ptr;
  }

  // Free memory (simple bump allocator doesn't truly free)
  free(ptr: number): void {
    this.allocations.delete(ptr);
  }

  // Get view into memory
  getUint8Array(ptr: number, len: number): Uint8Array {
    return new Uint8Array(this.memory.buffer, ptr, len);
  }

  getInt32Array(ptr: number, len: number): Int32Array {
    return new Int32Array(this.memory.buffer, ptr, len);
  }

  getFloat64Array(ptr: number, len: number): Float64Array {
    return new Float64Array(this.memory.buffer, ptr, len);
  }

  // Copy data to WASM memory
  writeBytes(data: Uint8Array): number {
    const ptr = this.allocate(data.length);
    const view = this.getUint8Array(ptr, data.length);
    view.set(data);
    return ptr;
  }

  writeString(str: string): { ptr: number; len: number } {
    const encoder = new TextEncoder();
    const data = encoder.encode(str);
    const ptr = this.writeBytes(data);
    return { ptr, len: data.length };
  }

  readString(ptr: number, len: number): string {
    const view = this.getUint8Array(ptr, len);
    const decoder = new TextDecoder();
    return decoder.decode(view);
  }

  // Process data in WASM
  processData(data: Uint8Array): Uint8Array {
    const inputPtr = this.writeBytes(data);
    this.exports.process_buffer(inputPtr, data.length);

    const resultPtr = this.exports.get_result_ptr();
    const resultLen = this.exports.get_result_len();

    const result = new Uint8Array(resultLen);
    result.set(this.getUint8Array(resultPtr, resultLen));

    this.free(inputPtr);
    return result;
  }
}

// Example: Image processing with shared memory
async function processImage(imageData: ImageData): Promise<ImageData> {
  const { exports } = await loadWasmModule<SharedMemoryExports>("./image.wasm");

  const manager = new SharedMemoryManager(exports, 1024); // Heap starts at 1024

  // Copy image data to WASM
  const inputPtr = manager.writeBytes(new Uint8Array(imageData.data));

  // Process in WASM
  exports.process_buffer(inputPtr, imageData.data.length);

  // Get result
  const resultPtr = exports.get_result_ptr();
  const resultData = manager.getUint8Array(resultPtr, imageData.data.length);

  // Create new ImageData
  const result = new ImageData(
    new Uint8ClampedArray(resultData),
    imageData.width,
    imageData.height
  );

  manager.free(inputPtr);
  return result;
}
```

### Web Workers con WASM

```typescript
// src/workers/wasm-worker.ts
import init, { heavy_computation } from "../pkg/my_wasm_lib.js";

// Worker message types
interface WorkerMessage {
  type: "init" | "compute";
  id: string;
  data?: unknown;
}

interface WorkerResponse {
  type: "ready" | "result" | "error";
  id: string;
  data?: unknown;
  error?: string;
}

let initialized = false;

self.onmessage = async (event: MessageEvent<WorkerMessage>) => {
  const { type, id, data } = event.data;

  try {
    if (type === "init") {
      await init();
      initialized = true;
      self.postMessage({ type: "ready", id } as WorkerResponse);
      return;
    }

    if (type === "compute") {
      if (!initialized) {
        throw new Error("WASM not initialized");
      }

      const result = heavy_computation(data as number);
      self.postMessage({ type: "result", id, data: result } as WorkerResponse);
    }
  } catch (error) {
    self.postMessage({
      type: "error",
      id,
      error: (error as Error).message,
    } as WorkerResponse);
  }
};

// Main thread usage
// src/wasm-pool.ts
class WasmWorkerPool {
  private workers: Worker[] = [];
  private pendingTasks: Map<string, (result: unknown) => void> = new Map();
  private taskQueue: Array<{ id: string; data: unknown }> = [];
  private idleWorkers: Worker[] = [];

  constructor(workerPath: string, poolSize: number = navigator.hardwareConcurrency) {
    for (let i = 0; i < poolSize; i++) {
      const worker = new Worker(workerPath, { type: "module" });
      worker.onmessage = this.handleMessage.bind(this, worker);
      this.workers.push(worker);
    }
  }

  async initialize(): Promise<void> {
    const initPromises = this.workers.map((worker) => {
      return new Promise<void>((resolve) => {
        const id = crypto.randomUUID();
        this.pendingTasks.set(id, () => {
          this.idleWorkers.push(worker);
          resolve();
        });
        worker.postMessage({ type: "init", id });
      });
    });

    await Promise.all(initPromises);
  }

  private handleMessage(worker: Worker, event: MessageEvent<WorkerResponse>) {
    const { type, id, data, error } = event.data;

    const resolve = this.pendingTasks.get(id);
    if (resolve) {
      this.pendingTasks.delete(id);
      resolve(data);
    }

    // Return worker to pool
    this.idleWorkers.push(worker);
    this.processQueue();
  }

  private processQueue() {
    while (this.idleWorkers.length > 0 && this.taskQueue.length > 0) {
      const worker = this.idleWorkers.pop()!;
      const task = this.taskQueue.shift()!;
      worker.postMessage({ type: "compute", ...task });
    }
  }

  async compute<T>(data: unknown): Promise<T> {
    return new Promise((resolve) => {
      const id = crypto.randomUUID();
      this.pendingTasks.set(id, resolve as (result: unknown) => void);

      if (this.idleWorkers.length > 0) {
        const worker = this.idleWorkers.pop()!;
        worker.postMessage({ type: "compute", id, data });
      } else {
        this.taskQueue.push({ id, data });
      }
    });
  }

  terminate(): void {
    this.workers.forEach((worker) => worker.terminate());
    this.workers = [];
    this.idleWorkers = [];
  }
}
```

---

## Optimización de Rendimiento

### Tabla de Mejores Prácticas

| Práctica                          | Impacto | Descripción                           |
| --------------------------------- | ------- | ------------------------------------- |
| Minimizar cruces JS-WASM          | Alto    | Cada llamada tiene overhead           |
| Usar memoria compartida           | Alto    | Evita copiar datos                    |
| Operaciones batch                 | Alto    | Procesar arrays completos             |
| Compilar con optimizaciones       | Medio   | -O3, LTO, wasm-opt                    |
| Evitar allocaciones frecuentes    | Medio   | Pool de memoria                       |
| SIMD cuando disponible            | Alto    | Procesamiento vectorizado             |
| Streaming compilation             | Bajo    | Carga más rápida de módulos           |

### Benchmarking

```typescript
// src/wasm/benchmark.ts
interface BenchmarkResult {
  name: string;
  iterations: number;
  totalMs: number;
  avgMs: number;
  opsPerSecond: number;
}

async function benchmark(
  name: string,
  fn: () => void | Promise<void>,
  iterations: number = 1000
): Promise<BenchmarkResult> {
  // Warm up
  for (let i = 0; i < 10; i++) {
    await fn();
  }

  const start = performance.now();

  for (let i = 0; i < iterations; i++) {
    await fn();
  }

  const end = performance.now();
  const totalMs = end - start;
  const avgMs = totalMs / iterations;
  const opsPerSecond = 1000 / avgMs;

  return {
    name,
    iterations,
    totalMs,
    avgMs,
    opsPerSecond,
  };
}

// Compare JS vs WASM performance
async function runBenchmarks() {
  const wasmModule = await loadWasmModule<{ fibonacci: (n: number) => bigint }>(
    "./fib.wasm"
  );

  // JavaScript implementation
  function jsFibonacci(n: number): bigint {
    if (n <= 1) return BigInt(n);
    let a = 0n, b = 1n;
    for (let i = 2; i <= n; i++) {
      const c = a + b;
      a = b;
      b = c;
    }
    return b;
  }

  const n = 40;

  const jsResult = await benchmark("JS Fibonacci", () => jsFibonacci(n), 10000);

  const wasmResult = await benchmark(
    "WASM Fibonacci",
    () => wasmModule.exports.fibonacci(n),
    10000
  );

  console.table([jsResult, wasmResult]);
  console.log(`WASM is ${(jsResult.avgMs / wasmResult.avgMs).toFixed(2)}x faster`);
}
```

---

## Casos de Uso para Código Legacy

### Migración de Bibliotecas C

```
┌─────────────────────────────────────────────────────────────┐
│              Estrategia de Migración Legacy                  │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Fase 1: Análisis                                           │
│  ├── Identificar código crítico de rendimiento              │
│  ├── Evaluar complejidad de la API                          │
│  └── Estimar esfuerzo de migración                          │
│                                                             │
│  Fase 2: Preparación                                        │
│  ├── Crear wrapper C/C++ limpio                             │
│  ├── Definir interfaz TypeScript                            │
│  └── Escribir tests de compatibilidad                       │
│                                                             │
│  Fase 3: Compilación                                        │
│  ├── Configurar Emscripten/wasm-pack                        │
│  ├── Compilar a WASM                                        │
│  └── Generar bindings TypeScript                            │
│                                                             │
│  Fase 4: Integración                                        │
│  ├── Implementar wrapper TypeScript                         │
│  ├── Manejar memoria y tipos                                │
│  └── Optimizar rendimiento                                  │
│                                                             │
│  Fase 5: Testing y Deploy                                   │
│  ├── Validar equivalencia funcional                         │
│  ├── Benchmark comparativo                                  │
│  └── Deploy gradual                                         │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Ejemplo: Biblioteca de Criptografía

```typescript
// src/wasm/crypto-legacy.ts
// Wrapping a legacy C cryptography library

interface CryptoWasmExports {
  memory: WebAssembly.Memory;
  sha256_init: () => number;
  sha256_update: (ctx: number, data: number, len: number) => void;
  sha256_final: (ctx: number, out: number) => void;
  aes_encrypt: (
    key: number,
    keyLen: number,
    iv: number,
    data: number,
    dataLen: number,
    out: number
  ) => number;
  aes_decrypt: (
    key: number,
    keyLen: number,
    iv: number,
    data: number,
    dataLen: number,
    out: number
  ) => number;
  malloc: (size: number) => number;
  free: (ptr: number) => void;
}

class LegacyCrypto {
  private exports: CryptoWasmExports;
  private heap: Uint8Array;

  private constructor(exports: CryptoWasmExports) {
    this.exports = exports;
    this.heap = new Uint8Array(exports.memory.buffer);
  }

  static async create(): Promise<LegacyCrypto> {
    const { exports } = await loadWasmModule<CryptoWasmExports>(
      "./crypto.wasm"
    );
    return new LegacyCrypto(exports);
  }

  sha256(data: Uint8Array): Uint8Array {
    const ctx = this.exports.sha256_init();
    const dataPtr = this.exports.malloc(data.length);
    const outPtr = this.exports.malloc(32);

    try {
      // Copy input data
      this.heap.set(data, dataPtr);

      // Hash
      this.exports.sha256_update(ctx, dataPtr, data.length);
      this.exports.sha256_final(ctx, outPtr);

      // Read result
      const result = new Uint8Array(32);
      result.set(this.heap.slice(outPtr, outPtr + 32));
      return result;
    } finally {
      this.exports.free(dataPtr);
      this.exports.free(outPtr);
      this.exports.free(ctx);
    }
  }

  aesEncrypt(key: Uint8Array, iv: Uint8Array, data: Uint8Array): Uint8Array {
    const keyPtr = this.exports.malloc(key.length);
    const ivPtr = this.exports.malloc(iv.length);
    const dataPtr = this.exports.malloc(data.length);
    const outPtr = this.exports.malloc(data.length + 16); // Padding

    try {
      this.heap.set(key, keyPtr);
      this.heap.set(iv, ivPtr);
      this.heap.set(data, dataPtr);

      const outLen = this.exports.aes_encrypt(
        keyPtr,
        key.length,
        ivPtr,
        dataPtr,
        data.length,
        outPtr
      );

      const result = new Uint8Array(outLen);
      result.set(this.heap.slice(outPtr, outPtr + outLen));
      return result;
    } finally {
      this.exports.free(keyPtr);
      this.exports.free(ivPtr);
      this.exports.free(dataPtr);
      this.exports.free(outPtr);
    }
  }
}

// Usage
async function useLegacyCrypto() {
  const crypto = await LegacyCrypto.create();

  const data = new TextEncoder().encode("Hello, World!");
  const hash = crypto.sha256(data);

  console.log(
    "SHA-256:",
    Array.from(hash)
      .map((b) => b.toString(16).padStart(2, "0"))
      .join("")
  );
}
```

---

## Referencias y Recursos

### Documentación Oficial

- [WebAssembly.org](https://webassembly.org/)
- [MDN WebAssembly Guide](https://developer.mozilla.org/en-US/docs/WebAssembly)
- [wasm-bindgen Book](https://rustwasm.github.io/docs/wasm-bindgen/)
- [Emscripten Documentation](https://emscripten.org/docs/)
- [WASI Specification](https://wasi.dev/)

### Herramientas

| Herramienta    | Propósito                           |
| -------------- | ----------------------------------- |
| `wasm-pack`    | Build Rust to WASM                  |
| `emscripten`   | Compile C/C++ to WASM               |
| `wabt`         | WebAssembly Binary Toolkit          |
| `wasm-opt`     | WASM optimizer                      |
| `wasmtime`     | WASI runtime                        |
| `wasmer`       | Universal WASM runtime              |

---

## Notas de ARCHAEON

> WebAssembly representa una revolución en la portabilidad del código,
> permitiendo que bibliotecas legacy escritas en C, C++, Rust y otros
> lenguajes se ejecuten de forma segura en navegadores y entornos Node.js.
> ARCHAEON documenta estos patrones para facilitar la modernización
> de sistemas heredados sin perder el valor del código existente.

**Próximo documento:** [TYPESCRIPT_04_FULLSTACK.md](./TYPESCRIPT_04_FULLSTACK.md) - Patrones Full Stack

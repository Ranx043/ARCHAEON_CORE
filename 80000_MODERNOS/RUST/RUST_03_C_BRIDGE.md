---
título: "RUST 03 - Puente C-Rust (FFI)"
versión: "1.0.0"
fecha_creación: "2025-12-31"
última_actualización: "2025-12-31"
autor: "ARCHAEON_CORE"
dominio: "Lenguajes Modernos / Rust"
especialización: "Foreign Function Interface (FFI) con C"
contexto_soul_core: "ARCHAEON - Interoperabilidad C ↔ Rust"
tags:
  - rust
  - ffi
  - c-interop
  - bindgen
  - cbindgen
  - c2rust
  - linking
  - abi
  - memory-safety
dependencias:
  - rust_fundamentos: "RUST_01_FUNDAMENTOS.md"
  - rust_sistemas: "RUST_02_SISTEMAS.md"
  - conocimiento_c: "avanzado"
---

# RUST 03: PUENTE C-RUST (FFI)

## Índice de Contenidos

1. [Introducción a FFI](#1-introducción-a-ffi)
2. [Fundamentos de FFI](#2-fundamentos-de-ffi)
3. [Bindgen: Generando Bindings a C](#3-bindgen-generando-bindings-a-c)
4. [Cbindgen: Exponiendo Rust a C](#4-cbindgen-exponiendo-rust-a-c)
5. [C2Rust: Traducción Automática](#5-c2rust-traducción-automática)
6. [Linking con Bibliotecas C](#6-linking-con-bibliotecas-c)
7. [Seguridad de Memoria en FFI](#7-seguridad-de-memoria-en-ffi)
8. [Patrones Avanzados de FFI](#8-patrones-avanzados-de-ffi)
9. [Tablas de Referencia](#9-tablas-de-referencia)

---

## 1. Introducción a FFI

### 1.1 ¿Por Qué FFI?

FFI (Foreign Function Interface) permite la interoperabilidad entre Rust y C,
habilitando:
- Uso de bibliotecas C existentes en Rust
- Exposición de código Rust para uso desde C
- Migración gradual de proyectos C a Rust
- Acceso a APIs del sistema operativo

```rust
// Example: Calling C's printf from Rust
use std::ffi::CString;
use std::os::raw::c_int;

extern "C" {
    fn printf(format: *const i8, ...) -> c_int;
}

fn call_printf() {
    let format = CString::new("Hello from Rust via C printf: %d\n").unwrap();

    unsafe {
        printf(format.as_ptr(), 42 as c_int);
    }
}
```

### 1.2 El ABI de C

El ABI (Application Binary Interface) de C es el estándar de facto para
interoperabilidad entre lenguajes a nivel binario.

```rust
// Specifying calling convention
extern "C" fn c_abi_function() {}       // C calling convention
extern "system" fn system_function() {} // Platform-specific (stdcall on Windows)
extern "Rust" fn rust_function() {}     // Rust default (unstable ABI)

// C ABI is stable and well-defined
#[repr(C)]
struct CCompatibleStruct {
    field1: i32,
    field2: f64,
}
```

---

## 2. Fundamentos de FFI

### 2.1 Tipos Primitivos C en Rust

```rust
use std::os::raw::{
    c_char,   // i8 or u8 depending on platform
    c_schar,  // signed char (i8)
    c_uchar,  // unsigned char (u8)
    c_short,  // short (i16)
    c_ushort, // unsigned short (u16)
    c_int,    // int (i32)
    c_uint,   // unsigned int (u32)
    c_long,   // long (i32 or i64)
    c_ulong,  // unsigned long (u32 or u64)
    c_longlong,  // long long (i64)
    c_ulonglong, // unsigned long long (u64)
    c_float,  // float (f32)
    c_double, // double (f64)
    c_void,   // void
};

// libc crate provides more types
// use libc::{size_t, ssize_t, off_t, pid_t, ...};

// Fixed-size types from core (preferred for portability)
type int8_t = i8;
type uint8_t = u8;
type int16_t = i16;
type uint16_t = u16;
type int32_t = i32;
type uint32_t = u32;
type int64_t = i64;
type uint64_t = u64;
type intptr_t = isize;
type uintptr_t = usize;
```

### 2.2 Punteros y Arrays

```rust
use std::os::raw::{c_char, c_int, c_void};

extern "C" {
    // Pointer to int
    fn takes_int_ptr(ptr: *mut c_int);

    // Pointer to const char (C string)
    fn takes_string(s: *const c_char);

    // Void pointer (generic data)
    fn takes_void_ptr(data: *mut c_void, len: usize);

    // Array parameter (decays to pointer in C)
    fn takes_array(arr: *const c_int, len: usize);

    // Double pointer (pointer to pointer)
    fn output_string(out: *mut *mut c_char) -> c_int;
}

// Safe wrappers
fn safe_takes_array(arr: &[i32]) {
    unsafe {
        takes_array(arr.as_ptr(), arr.len());
    }
}

fn safe_output_string() -> Option<String> {
    use std::ffi::CStr;

    let mut ptr: *mut c_char = std::ptr::null_mut();

    unsafe {
        if output_string(&mut ptr) != 0 {
            return None;
        }

        if ptr.is_null() {
            return None;
        }

        let c_str = CStr::from_ptr(ptr);
        let result = c_str.to_string_lossy().into_owned();

        // Remember to free the C-allocated string!
        libc::free(ptr as *mut c_void);

        Some(result)
    }
}
```

### 2.3 Strings: CString y CStr

```rust
use std::ffi::{CString, CStr};
use std::os::raw::c_char;

extern "C" {
    fn c_function_takes_string(s: *const c_char);
    fn c_function_returns_string() -> *const c_char;
}

// Rust -> C: Use CString
fn pass_string_to_c(rust_string: &str) {
    // CString adds null terminator
    let c_string = CString::new(rust_string).expect("CString::new failed");

    unsafe {
        c_function_takes_string(c_string.as_ptr());
    }

    // c_string is valid until dropped
}

// C -> Rust: Use CStr
fn receive_string_from_c() -> String {
    unsafe {
        let c_ptr = c_function_returns_string();

        if c_ptr.is_null() {
            return String::new();
        }

        // CStr borrows the C string (doesn't copy)
        let c_str = CStr::from_ptr(c_ptr);

        // Convert to Rust String (copies data)
        c_str.to_string_lossy().into_owned()
    }
}

// Handling interior null bytes
fn string_with_nulls() {
    let data = "Hello\0World";  // Contains null byte

    // This will fail!
    let result = CString::new(data);
    assert!(result.is_err());  // NulError

    // Must handle the error
    match CString::new(data) {
        Ok(c_string) => println!("Created: {:?}", c_string),
        Err(e) => println!("Error at position: {}", e.nul_position()),
    }
}
```

### 2.4 Structs Compatibles con C

```rust
use std::os::raw::{c_int, c_char, c_double};

// Must use repr(C) for C-compatible layout
#[repr(C)]
pub struct Point {
    pub x: c_double,
    pub y: c_double,
}

// Nested structs
#[repr(C)]
pub struct Rectangle {
    pub origin: Point,
    pub size: Point,
}

// Struct with pointer fields
#[repr(C)]
pub struct Buffer {
    pub data: *mut u8,
    pub len: usize,
    pub capacity: usize,
}

// Opaque types (incomplete types in C)
#[repr(C)]
pub struct OpaqueHandle {
    _private: [u8; 0],
    _marker: std::marker::PhantomData<(*mut u8, std::marker::PhantomPinned)>,
}

extern "C" {
    fn create_handle() -> *mut OpaqueHandle;
    fn destroy_handle(handle: *mut OpaqueHandle);
    fn use_handle(handle: *mut OpaqueHandle) -> c_int;
}

// Safe wrapper for opaque type
pub struct Handle {
    ptr: *mut OpaqueHandle,
}

impl Handle {
    pub fn new() -> Option<Self> {
        let ptr = unsafe { create_handle() };

        if ptr.is_null() {
            None
        } else {
            Some(Handle { ptr })
        }
    }

    pub fn use_it(&self) -> i32 {
        unsafe { use_handle(self.ptr) as i32 }
    }
}

impl Drop for Handle {
    fn drop(&mut self) {
        unsafe {
            destroy_handle(self.ptr);
        }
    }
}
```

### 2.5 Enums y Unions

```rust
use std::os::raw::c_int;

// C-compatible enum (with explicit discriminant size)
#[repr(C)]
pub enum Color {
    Red = 0,
    Green = 1,
    Blue = 2,
}

// Enum with specific integer type
#[repr(i32)]
pub enum Status {
    Ok = 0,
    Error = -1,
    Pending = 1,
}

// C union
#[repr(C)]
pub union FloatOrInt {
    pub f: f32,
    pub i: i32,
}

fn union_usage() {
    let mut u = FloatOrInt { f: 1.0 };

    unsafe {
        println!("As float: {}", u.f);
        println!("As int: {:#x}", u.i);

        u.i = 0x40000000;
        println!("After setting int, as float: {}", u.f);  // 2.0
    }
}

// Tagged union pattern (discriminated union)
#[repr(C)]
pub struct TaggedValue {
    pub tag: ValueTag,
    pub value: Value,
}

#[repr(C)]
pub enum ValueTag {
    Integer = 0,
    Float = 1,
    String = 2,
}

#[repr(C)]
pub union Value {
    pub integer: i64,
    pub float: f64,
    pub string: *const c_char,
}

use std::os::raw::c_char;

fn tagged_union_usage() {
    let tv = TaggedValue {
        tag: ValueTag::Integer,
        value: Value { integer: 42 },
    };

    unsafe {
        match tv.tag {
            ValueTag::Integer => println!("Integer: {}", tv.value.integer),
            ValueTag::Float => println!("Float: {}", tv.value.float),
            ValueTag::String => {
                if !tv.value.string.is_null() {
                    let s = std::ffi::CStr::from_ptr(tv.value.string);
                    println!("String: {:?}", s);
                }
            }
        }
    }
}
```

---

## 3. Bindgen: Generando Bindings a C

### 3.1 Configuración de Bindgen

```toml
# Cargo.toml
[package]
name = "my-c-bindings"
version = "0.1.0"
edition = "2021"

[build-dependencies]
bindgen = "0.69"

[dependencies]
libc = "0.2"
```

```rust
// build.rs
use std::env;
use std::path::PathBuf;

fn main() {
    // Tell cargo to tell rustc to link the library
    println!("cargo:rustc-link-lib=mylib");

    // Tell cargo to rerun if wrapper changes
    println!("cargo:rerun-if-changed=wrapper.h");

    // Generate bindings
    let bindings = bindgen::Builder::default()
        // The input header
        .header("wrapper.h")
        // Include paths
        .clang_arg("-I/usr/include")
        // Whitelist specific items
        .allowlist_function("my_.*")
        .allowlist_type("My.*")
        .allowlist_var("MY_.*")
        // Blocklist items
        .blocklist_function("internal_.*")
        // Generate Debug impl
        .derive_debug(true)
        // Generate Default impl
        .derive_default(true)
        // Generate Copy/Clone
        .derive_copy(true)
        // Use core instead of std
        .use_core()
        // Generate comments
        .generate_comments(true)
        // Parse as C++
        // .clang_arg("-x").clang_arg("c++")
        // Generate bindings
        .generate()
        .expect("Unable to generate bindings");

    // Write bindings to output directory
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
```

```c
// wrapper.h - The C header to generate bindings for
#ifndef WRAPPER_H
#define WRAPPER_H

#include <stdint.h>
#include <stddef.h>

#define MY_CONSTANT 42
#define MY_VERSION "1.0.0"

typedef struct {
    int32_t x;
    int32_t y;
} MyPoint;

typedef enum {
    MY_STATUS_OK = 0,
    MY_STATUS_ERROR = -1,
} MyStatus;

MyStatus my_init(void);
MyStatus my_create_point(MyPoint* out, int32_t x, int32_t y);
void my_cleanup(void);

#endif
```

```rust
// src/lib.rs - Using the generated bindings
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

// Safe wrapper
pub fn init() -> Result<(), &'static str> {
    unsafe {
        match my_init() {
            MyStatus::MY_STATUS_OK => Ok(()),
            MyStatus::MY_STATUS_ERROR => Err("Initialization failed"),
        }
    }
}

pub fn create_point(x: i32, y: i32) -> Result<MyPoint, &'static str> {
    let mut point = MyPoint::default();

    unsafe {
        match my_create_point(&mut point, x, y) {
            MyStatus::MY_STATUS_OK => Ok(point),
            MyStatus::MY_STATUS_ERROR => Err("Failed to create point"),
        }
    }
}

pub fn cleanup() {
    unsafe {
        my_cleanup();
    }
}
```

### 3.2 Bindgen Avanzado

```rust
// build.rs - Advanced bindgen configuration
fn main() {
    let bindings = bindgen::Builder::default()
        .header("complex_header.h")

        // Custom type mappings
        .raw_line("pub type FILE = libc::FILE;")

        // Opaque types (don't generate definition)
        .opaque_type("InternalStruct")

        // Blocklist specific items
        .blocklist_item("PRIVATE_.*")

        // Custom derives for specific types
        .no_copy("NoCopyStruct")
        .no_debug("NoDebugStruct")

        // Make fields public
        .default_visibility(bindgen::FieldVisibilityKind::Public)

        // Rustfmt the output
        .rustfmt_bindings(true)

        // Layout tests
        .layout_tests(true)

        // Constified enum variants
        .constified_enum_module("MyEnum")

        // Bitfield enums
        .bitfield_enum("MyFlags")

        // Generate inline functions
        .generate_inline_functions(true)

        // Parse system headers
        .clang_arg("-isystem")
        .clang_arg("/usr/include")

        // C++ options
        // .clang_arg("-x")
        // .clang_arg("c++")
        // .clang_arg("-std=c++17")

        .generate()
        .expect("Unable to generate bindings");

    // ... write to file
}
```

### 3.3 Callbacks y Function Pointers

```c
// callback_header.h
typedef void (*callback_fn)(int value, void* user_data);

void register_callback(callback_fn cb, void* user_data);
void trigger_callbacks(void);
```

```rust
// Using callbacks from Rust
use std::os::raw::{c_int, c_void};

// Type alias matching C typedef
type CallbackFn = Option<unsafe extern "C" fn(value: c_int, user_data: *mut c_void)>;

extern "C" {
    fn register_callback(cb: CallbackFn, user_data: *mut c_void);
    fn trigger_callbacks();
}

// Rust callback function
unsafe extern "C" fn my_callback(value: c_int, user_data: *mut c_void) {
    println!("Callback received: {}", value);

    // Cast user_data back to Rust type
    if !user_data.is_null() {
        let data = &*(user_data as *const String);
        println!("User data: {}", data);
    }
}

fn setup_callback() {
    let user_data = Box::new(String::from("Hello from Rust"));
    let user_data_ptr = Box::into_raw(user_data) as *mut c_void;

    unsafe {
        register_callback(Some(my_callback), user_data_ptr);
        trigger_callbacks();

        // Clean up
        let _ = Box::from_raw(user_data_ptr as *mut String);
    }
}

// Using closures with callbacks (requires trampoline)
struct CallbackContext<F: FnMut(i32)> {
    closure: F,
}

unsafe extern "C" fn closure_trampoline<F: FnMut(i32)>(
    value: c_int,
    user_data: *mut c_void,
) {
    let context = &mut *(user_data as *mut CallbackContext<F>);
    (context.closure)(value as i32);
}

fn setup_closure_callback<F: FnMut(i32)>(mut closure: F) {
    let mut context = CallbackContext { closure };
    let context_ptr = &mut context as *mut CallbackContext<F> as *mut c_void;

    unsafe {
        register_callback(Some(closure_trampoline::<F>), context_ptr);
        trigger_callbacks();
    }

    // context is dropped here, callback must not be called after this!
}
```

---

## 4. Cbindgen: Exponiendo Rust a C

### 4.1 Configuración de Cbindgen

```toml
# Cargo.toml
[package]
name = "rust-lib-for-c"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib", "staticlib"]

[build-dependencies]
cbindgen = "0.26"
```

```rust
// build.rs
use std::env;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_language(cbindgen::Language::C)
        .with_include_guard("RUST_LIB_H")
        .with_documentation(true)
        .with_style(cbindgen::Style::Both)  // Both tag and typedef
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file("rust_lib.h");
}
```

```toml
# cbindgen.toml - Alternative configuration
language = "C"

# Header settings
header = "/* Generated by cbindgen - Do not edit */"
include_guard = "RUST_LIB_H"
autogen_warning = "/* Warning: this file is autogenerated */"
include_version = true

# Style settings
style = "both"  # Generate both tag and typedef
usize_is_size_t = true
documentation = true
documentation_style = "doxy"

# Enum settings
[enum]
rename_variants = "ScreamingSnakeCase"

# Struct settings
[struct]
rename_fields = "SnakeCase"

# Function settings
[fn]
rename_args = "SnakeCase"

# Export settings
[export]
include = ["my_*", "My*"]
exclude = ["internal_*"]

# Parse settings
[parse]
parse_deps = true
include = ["my_dependency"]
```

### 4.2 Exponiendo Funciones y Tipos

```rust
// src/lib.rs
use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int, c_void};
use std::ptr;

/// A point in 2D space
#[repr(C)]
pub struct Point {
    pub x: f64,
    pub y: f64,
}

/// Status codes for library operations
#[repr(C)]
pub enum Status {
    Ok = 0,
    Error = -1,
    InvalidArgument = -2,
}

/// Create a new point
#[no_mangle]
pub extern "C" fn point_new(x: f64, y: f64) -> Point {
    Point { x, y }
}

/// Calculate distance between two points
#[no_mangle]
pub extern "C" fn point_distance(a: &Point, b: &Point) -> f64 {
    let dx = b.x - a.x;
    let dy = b.y - a.y;
    (dx * dx + dy * dy).sqrt()
}

/// Opaque handle for internal state
pub struct Handle {
    data: Vec<u8>,
    name: String,
}

/// Create a new handle
#[no_mangle]
pub extern "C" fn handle_create(name: *const c_char) -> *mut Handle {
    if name.is_null() {
        return ptr::null_mut();
    }

    let c_str = unsafe { CStr::from_ptr(name) };
    let name_str = match c_str.to_str() {
        Ok(s) => s.to_owned(),
        Err(_) => return ptr::null_mut(),
    };

    let handle = Box::new(Handle {
        data: Vec::new(),
        name: name_str,
    });

    Box::into_raw(handle)
}

/// Destroy a handle
#[no_mangle]
pub extern "C" fn handle_destroy(handle: *mut Handle) {
    if !handle.is_null() {
        unsafe {
            let _ = Box::from_raw(handle);
        }
    }
}

/// Get handle name (caller must NOT free the returned string)
#[no_mangle]
pub extern "C" fn handle_get_name(handle: *const Handle) -> *const c_char {
    if handle.is_null() {
        return ptr::null();
    }

    unsafe {
        // Return pointer to internal string data
        // Valid only while handle exists
        (*handle).name.as_ptr() as *const c_char
    }
}

/// Write data to handle
#[no_mangle]
pub extern "C" fn handle_write(
    handle: *mut Handle,
    data: *const u8,
    len: usize,
) -> Status {
    if handle.is_null() || data.is_null() {
        return Status::InvalidArgument;
    }

    unsafe {
        let slice = std::slice::from_raw_parts(data, len);
        (*handle).data.extend_from_slice(slice);
    }

    Status::Ok
}

/// Read data from handle
/// Returns number of bytes copied, or -1 on error
#[no_mangle]
pub extern "C" fn handle_read(
    handle: *const Handle,
    buffer: *mut u8,
    buffer_len: usize,
) -> c_int {
    if handle.is_null() || buffer.is_null() {
        return -1;
    }

    unsafe {
        let data = &(*handle).data;
        let copy_len = buffer_len.min(data.len());

        ptr::copy_nonoverlapping(data.as_ptr(), buffer, copy_len);

        copy_len as c_int
    }
}
```

### 4.3 Generated Header

```c
/* rust_lib.h - Generated by cbindgen */
#ifndef RUST_LIB_H
#define RUST_LIB_H

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

/**
 * A point in 2D space
 */
typedef struct Point {
  double x;
  double y;
} Point;

/**
 * Status codes for library operations
 */
typedef enum Status {
  Status_Ok = 0,
  Status_Error = -1,
  Status_InvalidArgument = -2,
} Status;

/**
 * Opaque handle for internal state
 */
typedef struct Handle Handle;

/**
 * Create a new point
 */
struct Point point_new(double x, double y);

/**
 * Calculate distance between two points
 */
double point_distance(const struct Point *a, const struct Point *b);

/**
 * Create a new handle
 */
struct Handle *handle_create(const char *name);

/**
 * Destroy a handle
 */
void handle_destroy(struct Handle *handle);

/**
 * Get handle name (caller must NOT free the returned string)
 */
const char *handle_get_name(const struct Handle *handle);

/**
 * Write data to handle
 */
enum Status handle_write(struct Handle *handle, const uint8_t *data, size_t len);

/**
 * Read data from handle
 * Returns number of bytes copied, or -1 on error
 */
int handle_read(const struct Handle *handle, uint8_t *buffer, size_t buffer_len);

#endif /* RUST_LIB_H */
```

---

## 5. C2Rust: Traducción Automática

### 5.1 Instalación y Uso Básico

```bash
# Install c2rust
cargo install c2rust

# Translate a C project
c2rust transpile compile_commands.json --emit-build-files

# Or for single files
c2rust transpile path/to/file.c
```

### 5.2 Ejemplo de Traducción

```c
// Original C code: linked_list.c
#include <stdlib.h>
#include <stdio.h>

struct node {
    int data;
    struct node* next;
};

struct node* create_node(int data) {
    struct node* n = malloc(sizeof(struct node));
    if (n == NULL) return NULL;

    n->data = data;
    n->next = NULL;
    return n;
}

void push(struct node** head, int data) {
    struct node* new_node = create_node(data);
    if (new_node == NULL) return;

    new_node->next = *head;
    *head = new_node;
}

void print_list(struct node* head) {
    while (head != NULL) {
        printf("%d -> ", head->data);
        head = head->next;
    }
    printf("NULL\n");
}

void free_list(struct node* head) {
    while (head != NULL) {
        struct node* temp = head;
        head = head->next;
        free(temp);
    }
}
```

```rust
// Translated Rust code (c2rust output, slightly cleaned up)
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(unused_assignments)]
#![allow(unused_mut)]

use std::os::raw::{c_int, c_void};

extern "C" {
    fn malloc(_: usize) -> *mut c_void;
    fn free(_: *mut c_void);
    fn printf(_: *const i8, _: ...) -> c_int;
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct node {
    pub data: c_int,
    pub next: *mut node,
}

#[no_mangle]
pub unsafe extern "C" fn create_node(data: c_int) -> *mut node {
    let n: *mut node = malloc(::std::mem::size_of::<node>()) as *mut node;

    if n.is_null() {
        return std::ptr::null_mut();
    }

    (*n).data = data;
    (*n).next = std::ptr::null_mut();

    n
}

#[no_mangle]
pub unsafe extern "C" fn push(head: *mut *mut node, data: c_int) {
    let new_node: *mut node = create_node(data);

    if new_node.is_null() {
        return;
    }

    (*new_node).next = *head;
    *head = new_node;
}

#[no_mangle]
pub unsafe extern "C" fn print_list(mut head: *mut node) {
    while !head.is_null() {
        printf(b"%d -> \0".as_ptr() as *const i8, (*head).data);
        head = (*head).next;
    }
    printf(b"NULL\n\0".as_ptr() as *const i8);
}

#[no_mangle]
pub unsafe extern "C" fn free_list(mut head: *mut node) {
    while !head.is_null() {
        let temp: *mut node = head;
        head = (*head).next;
        free(temp as *mut c_void);
    }
}
```

### 5.3 Refactoring a Safe Rust

```rust
// Refactored safe Rust version
pub struct Node {
    data: i32,
    next: Option<Box<Node>>,
}

pub struct LinkedList {
    head: Option<Box<Node>>,
}

impl LinkedList {
    pub fn new() -> Self {
        LinkedList { head: None }
    }

    pub fn push(&mut self, data: i32) {
        let new_node = Box::new(Node {
            data,
            next: self.head.take(),
        });
        self.head = Some(new_node);
    }

    pub fn pop(&mut self) -> Option<i32> {
        self.head.take().map(|node| {
            self.head = node.next;
            node.data
        })
    }

    pub fn print(&self) {
        let mut current = &self.head;

        while let Some(node) = current {
            print!("{} -> ", node.data);
            current = &node.next;
        }

        println!("None");
    }
}

// Automatic cleanup via Drop - no free_list needed!
impl Drop for LinkedList {
    fn drop(&mut self) {
        // Iterative drop to prevent stack overflow
        while self.pop().is_some() {}
    }
}

// FFI wrapper to maintain C compatibility
#[no_mangle]
pub extern "C" fn linked_list_new() -> *mut LinkedList {
    Box::into_raw(Box::new(LinkedList::new()))
}

#[no_mangle]
pub extern "C" fn linked_list_push(list: *mut LinkedList, data: i32) {
    if let Some(list) = unsafe { list.as_mut() } {
        list.push(data);
    }
}

#[no_mangle]
pub extern "C" fn linked_list_pop(list: *mut LinkedList, out: *mut i32) -> bool {
    if let Some(list) = unsafe { list.as_mut() } {
        if let Some(value) = list.pop() {
            unsafe { *out = value };
            return true;
        }
    }
    false
}

#[no_mangle]
pub extern "C" fn linked_list_free(list: *mut LinkedList) {
    if !list.is_null() {
        unsafe {
            let _ = Box::from_raw(list);
        }
    }
}
```

---

## 6. Linking con Bibliotecas C

### 6.1 Static vs Dynamic Linking

```rust
// build.rs
fn main() {
    // Dynamic linking (default)
    println!("cargo:rustc-link-lib=mylib");

    // Static linking
    println!("cargo:rustc-link-lib=static=mylib");

    // Framework (macOS)
    println!("cargo:rustc-link-lib=framework=CoreFoundation");

    // Library search path
    println!("cargo:rustc-link-search=native=/usr/local/lib");
    println!("cargo:rustc-link-search=native=./lib");

    // Conditional linking
    #[cfg(target_os = "linux")]
    println!("cargo:rustc-link-lib=dl");

    #[cfg(target_os = "windows")]
    println!("cargo:rustc-link-lib=ws2_32");
}
```

### 6.2 Building C Code from Rust

```toml
# Cargo.toml
[build-dependencies]
cc = "1.0"
```

```rust
// build.rs - Compile C code with cc crate
fn main() {
    cc::Build::new()
        // Add C source files
        .file("src/c/helper.c")
        .file("src/c/utils.c")
        // Include directories
        .include("src/c/include")
        // Compiler flags
        .flag("-Wall")
        .flag("-Wextra")
        // Optimization
        .opt_level(2)
        // Define macros
        .define("DEBUG", "1")
        .define("VERSION", "\"1.0\"")
        // Target-specific settings
        .pic(true)  // Position Independent Code
        // Compile
        .compile("myhelper");

    // For C++ code
    cc::Build::new()
        .cpp(true)
        .file("src/cpp/module.cpp")
        .flag("-std=c++17")
        .compile("mycppmodule");
}
```

### 6.3 pkg-config Integration

```toml
# Cargo.toml
[build-dependencies]
pkg-config = "0.3"
```

```rust
// build.rs
fn main() {
    // Find and link library using pkg-config
    pkg_config::Config::new()
        .atleast_version("2.0")
        .probe("openssl")
        .expect("OpenSSL not found");

    // Or with more control
    let lib = pkg_config::Config::new()
        .statik(true)
        .probe("zlib")
        .expect("zlib not found");

    println!("zlib version: {:?}", lib.version);
    println!("Include paths: {:?}", lib.include_paths);
    println!("Library paths: {:?}", lib.link_paths);
}
```

### 6.4 CMake Integration

```toml
# Cargo.toml
[build-dependencies]
cmake = "0.1"
```

```rust
// build.rs
fn main() {
    let dst = cmake::Config::new("external/mylib")
        .define("BUILD_SHARED_LIBS", "OFF")
        .define("CMAKE_BUILD_TYPE", "Release")
        .build();

    println!("cargo:rustc-link-search=native={}/lib", dst.display());
    println!("cargo:rustc-link-lib=static=mylib");
}
```

---

## 7. Seguridad de Memoria en FFI

### 7.1 Ownership Across FFI Boundary

```rust
use std::os::raw::c_char;
use std::ffi::{CStr, CString};

// Pattern 1: Rust allocates, Rust frees
#[no_mangle]
pub extern "C" fn create_string() -> *mut c_char {
    let s = CString::new("Hello from Rust").unwrap();
    s.into_raw()  // Rust gives up ownership
}

#[no_mangle]
pub extern "C" fn free_string(s: *mut c_char) {
    if !s.is_null() {
        unsafe {
            let _ = CString::from_raw(s);  // Rust takes back ownership
        }
    }
}

// Pattern 2: C allocates, C frees
extern "C" {
    fn c_create_string() -> *mut c_char;
    fn c_free_string(s: *mut c_char);
}

fn use_c_string() {
    unsafe {
        let s = c_create_string();
        if !s.is_null() {
            let rust_str = CStr::from_ptr(s).to_string_lossy();
            println!("Got: {}", rust_str);
            c_free_string(s);  // C must free its own memory
        }
    }
}

// Pattern 3: Caller-provided buffer (safest)
#[no_mangle]
pub extern "C" fn get_string(
    buffer: *mut c_char,
    buffer_len: usize,
) -> usize {
    let message = "Hello from Rust";

    if buffer.is_null() {
        // Return required buffer size
        return message.len() + 1;  // +1 for null terminator
    }

    let copy_len = buffer_len.min(message.len() + 1);

    unsafe {
        std::ptr::copy_nonoverlapping(
            message.as_ptr(),
            buffer as *mut u8,
            copy_len - 1,
        );
        *buffer.add(copy_len - 1) = 0;  // Null terminator
    }

    copy_len - 1  // Return bytes written (excluding null)
}
```

### 7.2 Lifetime Management

```rust
use std::collections::HashMap;
use std::sync::Mutex;

// Global registry for objects with FFI handles
static REGISTRY: Mutex<Option<HashMap<usize, Box<dyn std::any::Any + Send>>>> =
    Mutex::new(None);

fn init_registry() {
    let mut reg = REGISTRY.lock().unwrap();
    if reg.is_none() {
        *reg = Some(HashMap::new());
    }
}

fn register_object<T: 'static + Send>(obj: T) -> usize {
    init_registry();

    let mut reg = REGISTRY.lock().unwrap();
    let map = reg.as_mut().unwrap();

    let handle = Box::as_ref(&Box::new(obj)) as *const T as usize;
    map.insert(handle, Box::new(obj));

    handle
}

fn get_object<T: 'static>(handle: usize) -> Option<&'static T> {
    let reg = REGISTRY.lock().unwrap();
    let map = reg.as_ref()?;

    map.get(&handle).and_then(|boxed| {
        boxed.downcast_ref::<T>()
    })
}

fn unregister_object(handle: usize) -> bool {
    let mut reg = REGISTRY.lock().unwrap();

    if let Some(map) = reg.as_mut() {
        map.remove(&handle).is_some()
    } else {
        false
    }
}

// FFI interface
pub struct MyObject {
    data: String,
}

#[no_mangle]
pub extern "C" fn my_object_create(data: *const std::os::raw::c_char) -> usize {
    if data.is_null() {
        return 0;
    }

    let c_str = unsafe { std::ffi::CStr::from_ptr(data) };
    let data_str = c_str.to_string_lossy().into_owned();

    register_object(MyObject { data: data_str })
}

#[no_mangle]
pub extern "C" fn my_object_destroy(handle: usize) {
    unregister_object(handle);
}
```

### 7.3 Error Handling Patterns

```rust
use std::os::raw::{c_char, c_int};
use std::ffi::CString;

// Thread-local error storage
thread_local! {
    static LAST_ERROR: std::cell::RefCell<Option<CString>> = std::cell::RefCell::new(None);
}

fn set_last_error(msg: &str) {
    LAST_ERROR.with(|e| {
        *e.borrow_mut() = CString::new(msg).ok();
    });
}

fn clear_last_error() {
    LAST_ERROR.with(|e| {
        *e.borrow_mut() = None;
    });
}

/// Get the last error message. Returns NULL if no error.
/// The returned string is valid until the next API call on this thread.
#[no_mangle]
pub extern "C" fn get_last_error() -> *const c_char {
    LAST_ERROR.with(|e| {
        match &*e.borrow() {
            Some(cstring) => cstring.as_ptr(),
            None => std::ptr::null(),
        }
    })
}

/// Returns 0 on success, negative on error.
/// Call get_last_error() for error details.
#[no_mangle]
pub extern "C" fn do_operation(param: c_int) -> c_int {
    clear_last_error();

    if param < 0 {
        set_last_error("Parameter must be non-negative");
        return -1;
    }

    if param > 100 {
        set_last_error("Parameter too large (max 100)");
        return -2;
    }

    // Operation succeeded
    param * 2
}

// Result struct pattern
#[repr(C)]
pub struct Result {
    pub success: bool,
    pub value: c_int,
    pub error_code: c_int,
}

#[no_mangle]
pub extern "C" fn do_operation_result(param: c_int) -> Result {
    if param < 0 {
        return Result {
            success: false,
            value: 0,
            error_code: -1,
        };
    }

    Result {
        success: true,
        value: param * 2,
        error_code: 0,
    }
}
```

---

## 8. Patrones Avanzados de FFI

### 8.1 Vtable Pattern (Object-Oriented FFI)

```rust
use std::os::raw::{c_int, c_void};

// Virtual method table for polymorphism
#[repr(C)]
pub struct AnimalVTable {
    pub speak: extern "C" fn(*const c_void),
    pub get_legs: extern "C" fn(*const c_void) -> c_int,
    pub destroy: extern "C" fn(*mut c_void),
}

#[repr(C)]
pub struct Animal {
    pub vtable: *const AnimalVTable,
    pub data: *mut c_void,
}

// Dog implementation
struct Dog {
    name: String,
}

extern "C" fn dog_speak(data: *const c_void) {
    let dog = unsafe { &*(data as *const Dog) };
    println!("{} says: Woof!", dog.name);
}

extern "C" fn dog_get_legs(_data: *const c_void) -> c_int {
    4
}

extern "C" fn dog_destroy(data: *mut c_void) {
    unsafe {
        let _ = Box::from_raw(data as *mut Dog);
    }
}

static DOG_VTABLE: AnimalVTable = AnimalVTable {
    speak: dog_speak,
    get_legs: dog_get_legs,
    destroy: dog_destroy,
};

#[no_mangle]
pub extern "C" fn create_dog(name: *const std::os::raw::c_char) -> Animal {
    let name_str = unsafe {
        std::ffi::CStr::from_ptr(name).to_string_lossy().into_owned()
    };

    let dog = Box::new(Dog { name: name_str });

    Animal {
        vtable: &DOG_VTABLE,
        data: Box::into_raw(dog) as *mut c_void,
    }
}

// Generic animal operations
#[no_mangle]
pub extern "C" fn animal_speak(animal: *const Animal) {
    unsafe {
        let vtable = &*(*animal).vtable;
        (vtable.speak)((*animal).data);
    }
}

#[no_mangle]
pub extern "C" fn animal_get_legs(animal: *const Animal) -> c_int {
    unsafe {
        let vtable = &*(*animal).vtable;
        (vtable.get_legs)((*animal).data)
    }
}

#[no_mangle]
pub extern "C" fn animal_destroy(animal: *mut Animal) {
    unsafe {
        let vtable = &*(*animal).vtable;
        (vtable.destroy)((*animal).data);
    }
}
```

### 8.2 Async FFI

```rust
use std::ffi::c_void;
use std::os::raw::c_int;

// Callback-based async pattern
type AsyncCallback = extern "C" fn(result: c_int, user_data: *mut c_void);

#[no_mangle]
pub extern "C" fn async_operation(
    input: c_int,
    callback: AsyncCallback,
    user_data: *mut c_void,
) {
    // Spawn async task
    std::thread::spawn(move || {
        // Simulate work
        std::thread::sleep(std::time::Duration::from_millis(100));

        let result = input * 2;

        // Call back to C
        callback(result, user_data);
    });
}

// Future-based async with polling
#[repr(C)]
pub enum AsyncStatus {
    Pending,
    Ready,
    Error,
}

#[repr(C)]
pub struct AsyncHandle {
    status: AsyncStatus,
    result: c_int,
    // Internal state
    _handle: *mut c_void,
}

use std::sync::mpsc;

#[no_mangle]
pub extern "C" fn async_start(input: c_int) -> *mut AsyncHandle {
    let (tx, rx) = mpsc::channel();

    std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_millis(100));
        let _ = tx.send(input * 2);
    });

    let handle = Box::new(AsyncHandle {
        status: AsyncStatus::Pending,
        result: 0,
        _handle: Box::into_raw(Box::new(rx)) as *mut c_void,
    });

    Box::into_raw(handle)
}

#[no_mangle]
pub extern "C" fn async_poll(handle: *mut AsyncHandle) -> AsyncStatus {
    if handle.is_null() {
        return AsyncStatus::Error;
    }

    unsafe {
        let h = &mut *handle;

        if matches!(h.status, AsyncStatus::Pending) {
            let rx = &*(h._handle as *const mpsc::Receiver<c_int>);

            match rx.try_recv() {
                Ok(result) => {
                    h.result = result;
                    h.status = AsyncStatus::Ready;
                }
                Err(mpsc::TryRecvError::Empty) => {}
                Err(mpsc::TryRecvError::Disconnected) => {
                    h.status = AsyncStatus::Error;
                }
            }
        }

        h.status
    }
}

#[no_mangle]
pub extern "C" fn async_get_result(handle: *const AsyncHandle) -> c_int {
    if handle.is_null() {
        return 0;
    }

    unsafe { (*handle).result }
}

#[no_mangle]
pub extern "C" fn async_destroy(handle: *mut AsyncHandle) {
    if !handle.is_null() {
        unsafe {
            let h = Box::from_raw(handle);
            let _ = Box::from_raw(h._handle as *mut mpsc::Receiver<c_int>);
        }
    }
}
```

---

## 9. Tablas de Referencia

### 9.1 Type Mapping C ↔ Rust

| C Type | Rust Type | Notes |
|--------|-----------|-------|
| `char` | `c_char` (i8/u8) | Platform-dependent sign |
| `signed char` | `i8` | Always signed |
| `unsigned char` | `u8` | Always unsigned |
| `short` | `c_short` (i16) | |
| `int` | `c_int` (i32) | |
| `long` | `c_long` (i32/i64) | Platform-dependent size |
| `long long` | `c_longlong` (i64) | |
| `float` | `f32` | |
| `double` | `f64` | |
| `size_t` | `usize` | |
| `ssize_t` | `isize` | |
| `void*` | `*mut c_void` | |
| `const void*` | `*const c_void` | |
| `char*` | `*mut c_char` | |
| `const char*` | `*const c_char` | |

### 9.2 FFI Attributes

| Attribute | Usage |
|-----------|-------|
| `#[no_mangle]` | Prevent name mangling |
| `#[repr(C)]` | C-compatible layout |
| `#[repr(transparent)]` | Same layout as inner type |
| `#[repr(packed)]` | No padding |
| `#[repr(align(N))]` | Minimum alignment |
| `extern "C"` | C calling convention |
| `extern "system"` | System calling convention |

### 9.3 Common FFI Patterns

| Pattern | Use Case |
|---------|----------|
| Opaque pointer | Hide implementation details |
| Callback + user_data | Closures across FFI |
| Caller-provided buffer | Avoid ownership transfer |
| Error code + get_last_error | Error handling |
| Vtable | Polymorphism |
| Handle registry | Lifetime management |

### 9.4 Safety Checklist

- [ ] All pointer parameters checked for null
- [ ] Buffer lengths validated before use
- [ ] Ownership clearly documented
- [ ] Proper cleanup functions provided
- [ ] Thread safety documented
- [ ] Error handling implemented
- [ ] Panic = catch_unwind at boundary
- [ ] repr(C) on all exposed types
- [ ] No mutable static access without sync

---

## Referencias y Recursos Adicionales

1. **Rust FFI Omnibus**: https://jakegoulding.com/rust-ffi-omnibus/
2. **Bindgen User Guide**: https://rust-lang.github.io/rust-bindgen/
3. **Cbindgen Documentation**: https://github.com/mozilla/cbindgen
4. **C2Rust**: https://c2rust.com/

---

*Documento generado por ARCHAEON_CORE - Sistema de Documentación de Lenguajes*
*Especialización: Interoperabilidad C ↔ Rust*
*Última actualización: 2025-12-31*

---
título: "RUST 02 - Programación de Sistemas"
versión: "1.0.0"
fecha_creación: "2025-12-31"
última_actualización: "2025-12-31"
autor: "ARCHAEON_CORE"
dominio: "Lenguajes Modernos / Rust"
especialización: "Programación de Sistemas y Bajo Nivel"
contexto_soul_core: "ARCHAEON - Evolución C → Rust"
tags:
  - rust
  - systems-programming
  - no_std
  - unsafe
  - memory-layout
  - hardware
  - low-level
  - kernel
dependencias:
  - rust_fundamentos: "RUST_01_FUNDAMENTOS.md"
  - conocimiento_c: "avanzado"
  - arquitectura: "básico"
---

# RUST 02: PROGRAMACIÓN DE SISTEMAS

## Índice de Contenidos

1. [Introducción a la Programación de Sistemas](#1-introducción-a-la-programación-de-sistemas)
2. [Patrones de Programación de Sistemas](#2-patrones-de-programación-de-sistemas)
3. [Programación No_std](#3-programación-no_std)
4. [Unsafe Rust](#4-unsafe-rust)
5. [Memory Layout y Alignment](#5-memory-layout-y-alignment)
6. [Interfacing con Hardware](#6-interfacing-con-hardware)
7. [Reemplazando Código C de Sistema](#7-reemplazando-código-c-de-sistema)
8. [Tablas de Referencia](#8-tablas-de-referencia)

---

## 1. Introducción a la Programación de Sistemas

### 1.1 Rust como Lenguaje de Sistemas

Rust está diseñado para reemplazar C en programación de sistemas, ofreciendo:
- Zero-cost abstractions
- Memory safety sin garbage collector
- Control de bajo nivel con seguridad de alto nivel
- Interoperabilidad directa con C

```rust
// System-level Rust: Direct memory control with safety
#![no_std]
#![no_main]

use core::panic::PanicInfo;

// Entry point for bare-metal
#[no_mangle]
pub extern "C" fn _start() -> ! {
    // Direct hardware interaction
    let vga_buffer = 0xb8000 as *mut u8;

    unsafe {
        // Write "OK" to VGA buffer
        *vga_buffer.offset(0) = b'O';
        *vga_buffer.offset(1) = 0x0f; // White on black
        *vga_buffer.offset(2) = b'K';
        *vga_buffer.offset(3) = 0x0f;
    }

    loop {}
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}
```

### 1.2 Comparación Filosófica: C vs Rust

| Aspecto | C | Rust |
|---------|---|------|
| Modelo de memoria | Manual, confianza total | Ownership, verificado |
| Undefined Behavior | Común, silencioso | Solo en `unsafe` |
| Null pointers | Ubicuos | No existen |
| Buffer overflows | Fáciles | Imposibles (safe) |
| Data races | Comunes | Imposibles |
| Abstraction cost | Zero (manual) | Zero (compiler) |

---

## 2. Patrones de Programación de Sistemas

### 2.1 RAII (Resource Acquisition Is Initialization)

```rust
use std::fs::File;
use std::io::{self, Read, Write};

// RAII pattern: resources tied to object lifetime
struct TempFile {
    path: String,
    file: File,
}

impl TempFile {
    fn new(path: &str) -> io::Result<Self> {
        let file = File::create(path)?;
        Ok(TempFile {
            path: path.to_string(),
            file,
        })
    }

    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        self.file.write(data)
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        // Automatic cleanup when TempFile goes out of scope
        let _ = std::fs::remove_file(&self.path);
        println!("Cleaned up temp file: {}", self.path);
    }
}

fn raii_example() -> io::Result<()> {
    {
        let mut temp = TempFile::new("temp.txt")?;
        temp.write(b"temporary data")?;
        // File is automatically deleted when temp goes out of scope
    }
    Ok(())
}
```

```c
// C: Manual resource management
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char* path;
    FILE* file;
} TempFile;

TempFile* temp_file_new(const char* path) {
    TempFile* tf = malloc(sizeof(TempFile));
    if (!tf) return NULL;

    tf->path = strdup(path);
    tf->file = fopen(path, "w");

    if (!tf->file) {
        free(tf->path);
        free(tf);
        return NULL;
    }

    return tf;
}

void temp_file_free(TempFile* tf) {
    if (tf) {
        if (tf->file) fclose(tf->file);
        if (tf->path) {
            remove(tf->path);  // Easy to forget!
            free(tf->path);
        }
        free(tf);
    }
}

// Must remember to call temp_file_free!
```

### 2.2 Zero-Copy Patterns

```rust
// Zero-copy string parsing
fn parse_header(data: &[u8]) -> Option<(&str, &str)> {
    let data_str = std::str::from_utf8(data).ok()?;

    let colon_pos = data_str.find(':')?;
    let key = data_str[..colon_pos].trim();
    let value = data_str[colon_pos + 1..].trim();

    Some((key, value))
}

// Zero-copy slice operations
struct PacketParser<'a> {
    data: &'a [u8],
    position: usize,
}

impl<'a> PacketParser<'a> {
    fn new(data: &'a [u8]) -> Self {
        PacketParser { data, position: 0 }
    }

    fn read_bytes(&mut self, len: usize) -> Option<&'a [u8]> {
        if self.position + len > self.data.len() {
            return None;
        }

        let slice = &self.data[self.position..self.position + len];
        self.position += len;
        Some(slice)
    }

    fn read_u32(&mut self) -> Option<u32> {
        let bytes = self.read_bytes(4)?;
        Some(u32::from_be_bytes(bytes.try_into().ok()?))
    }

    fn read_string(&mut self, len: usize) -> Option<&'a str> {
        let bytes = self.read_bytes(len)?;
        std::str::from_utf8(bytes).ok()
    }
}

fn zero_copy_example() {
    let packet = [
        0x00, 0x00, 0x00, 0x0D,  // Length: 13
        b'H', b'e', b'l', b'l', b'o', b',', b' ',
        b'W', b'o', b'r', b'l', b'd', b'!',
    ];

    let mut parser = PacketParser::new(&packet);

    if let Some(len) = parser.read_u32() {
        if let Some(message) = parser.read_string(len as usize) {
            println!("Message: {}", message);  // No allocation!
        }
    }
}
```

### 2.3 Buffer Pool Pattern

```rust
use std::sync::Mutex;
use std::collections::VecDeque;

// Buffer pool for efficient memory reuse
struct BufferPool {
    buffers: Mutex<VecDeque<Vec<u8>>>,
    buffer_size: usize,
    max_buffers: usize,
}

impl BufferPool {
    fn new(buffer_size: usize, max_buffers: usize) -> Self {
        BufferPool {
            buffers: Mutex::new(VecDeque::with_capacity(max_buffers)),
            buffer_size,
            max_buffers,
        }
    }

    fn acquire(&self) -> PooledBuffer {
        let buffer = {
            let mut pool = self.buffers.lock().unwrap();
            pool.pop_front()
        };

        let vec = buffer.unwrap_or_else(|| vec![0u8; self.buffer_size]);
        PooledBuffer {
            buffer: Some(vec),
            pool: self,
        }
    }

    fn release(&self, mut buffer: Vec<u8>) {
        let mut pool = self.buffers.lock().unwrap();

        if pool.len() < self.max_buffers {
            buffer.clear();
            pool.push_back(buffer);
        }
        // Otherwise, buffer is dropped
    }
}

struct PooledBuffer<'a> {
    buffer: Option<Vec<u8>>,
    pool: &'a BufferPool,
}

impl<'a> std::ops::Deref for PooledBuffer<'a> {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        self.buffer.as_ref().unwrap()
    }
}

impl<'a> std::ops::DerefMut for PooledBuffer<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.buffer.as_mut().unwrap()
    }
}

impl<'a> Drop for PooledBuffer<'a> {
    fn drop(&mut self) {
        if let Some(buffer) = self.buffer.take() {
            self.pool.release(buffer);
        }
    }
}

fn buffer_pool_example() {
    let pool = BufferPool::new(4096, 16);

    {
        let mut buf = pool.acquire();
        buf.extend_from_slice(b"Hello, World!");
        // Buffer automatically returned to pool on drop
    }

    // Reuse the same buffer
    let buf = pool.acquire();
    assert!(buf.is_empty());  // Cleared but reused
}
```

### 2.4 Type-State Pattern

```rust
// Type-state pattern for compile-time state machine validation
struct Uninitialized;
struct Connected;
struct Authenticated;

struct Connection<State> {
    socket: String,  // Simplified
    _state: std::marker::PhantomData<State>,
}

impl Connection<Uninitialized> {
    fn new() -> Self {
        Connection {
            socket: String::new(),
            _state: std::marker::PhantomData,
        }
    }

    fn connect(self, address: &str) -> Result<Connection<Connected>, &'static str> {
        // Perform connection...
        Ok(Connection {
            socket: address.to_string(),
            _state: std::marker::PhantomData,
        })
    }
}

impl Connection<Connected> {
    fn authenticate(self, credentials: &str) -> Result<Connection<Authenticated>, &'static str> {
        // Perform authentication...
        Ok(Connection {
            socket: self.socket,
            _state: std::marker::PhantomData,
        })
    }

    fn disconnect(self) -> Connection<Uninitialized> {
        Connection::new()
    }
}

impl Connection<Authenticated> {
    fn send(&self, data: &[u8]) -> Result<(), &'static str> {
        println!("Sending {} bytes to {}", data.len(), self.socket);
        Ok(())
    }

    fn receive(&self) -> Result<Vec<u8>, &'static str> {
        Ok(vec![])
    }

    fn logout(self) -> Connection<Connected> {
        Connection {
            socket: self.socket,
            _state: std::marker::PhantomData,
        }
    }
}

fn typestate_example() -> Result<(), &'static str> {
    let conn = Connection::new();

    // Can't send on uninitialized connection - compile error!
    // conn.send(b"data");

    let conn = conn.connect("localhost:8080")?;

    // Can't send on connected but not authenticated - compile error!
    // conn.send(b"data");

    let conn = conn.authenticate("user:pass")?;

    // Now we can send!
    conn.send(b"Hello!")?;

    Ok(())
}
```

---

## 3. Programación No_std

### 3.1 Fundamentos de no_std

```rust
// Disable standard library
#![no_std]

// Core library is always available
use core::ptr;
use core::mem;
use core::slice;

// alloc crate available if allocator is provided
// extern crate alloc;
// use alloc::vec::Vec;
// use alloc::string::String;

// Minimal implementation
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

// Working with raw memory
pub unsafe fn copy_memory(src: *const u8, dst: *mut u8, len: usize) {
    ptr::copy_nonoverlapping(src, dst, len);
}

// Fixed-size arrays instead of Vec
pub struct FixedBuffer {
    data: [u8; 1024],
    len: usize,
}

impl FixedBuffer {
    pub const fn new() -> Self {
        FixedBuffer {
            data: [0; 1024],
            len: 0,
        }
    }

    pub fn push(&mut self, byte: u8) -> bool {
        if self.len < self.data.len() {
            self.data[self.len] = byte;
            self.len += 1;
            true
        } else {
            false
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.data[..self.len]
    }
}
```

### 3.2 Custom Allocator

```rust
#![no_std]
#![feature(allocator_api)]

extern crate alloc;

use core::alloc::{GlobalAlloc, Layout};
use core::ptr::null_mut;

// Simple bump allocator for no_std environments
struct BumpAllocator {
    heap_start: usize,
    heap_end: usize,
    next: usize,
}

impl BumpAllocator {
    const fn new() -> Self {
        BumpAllocator {
            heap_start: 0,
            heap_end: 0,
            next: 0,
        }
    }

    unsafe fn init(&mut self, heap_start: usize, heap_size: usize) {
        self.heap_start = heap_start;
        self.heap_end = heap_start + heap_size;
        self.next = heap_start;
    }
}

unsafe impl GlobalAlloc for BumpAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let alloc_start = align_up(self.next, layout.align());
        let alloc_end = match alloc_start.checked_add(layout.size()) {
            Some(end) => end,
            None => return null_mut(),
        };

        if alloc_end > self.heap_end {
            null_mut()  // Out of memory
        } else {
            // Note: This is a simplified example - real implementation needs atomic operations
            // self.next = alloc_end;
            alloc_start as *mut u8
        }
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
        // Bump allocator doesn't support deallocation
    }
}

fn align_up(addr: usize, align: usize) -> usize {
    (addr + align - 1) & !(align - 1)
}

// For actual use:
// #[global_allocator]
// static ALLOCATOR: BumpAllocator = BumpAllocator::new();
```

### 3.3 No_std Collections

```rust
#![no_std]

use core::mem::MaybeUninit;

// Fixed-capacity stack-allocated vector
pub struct ArrayVec<T, const N: usize> {
    data: [MaybeUninit<T>; N],
    len: usize,
}

impl<T, const N: usize> ArrayVec<T, N> {
    pub const fn new() -> Self {
        ArrayVec {
            // SAFETY: MaybeUninit doesn't require initialization
            data: unsafe { MaybeUninit::uninit().assume_init() },
            len: 0,
        }
    }

    pub fn push(&mut self, value: T) -> Result<(), T> {
        if self.len < N {
            self.data[self.len].write(value);
            self.len += 1;
            Ok(())
        } else {
            Err(value)
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len > 0 {
            self.len -= 1;
            // SAFETY: We know this index was initialized
            Some(unsafe { self.data[self.len].assume_init_read() })
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn as_slice(&self) -> &[T] {
        // SAFETY: First len elements are initialized
        unsafe { core::slice::from_raw_parts(self.data.as_ptr() as *const T, self.len) }
    }
}

impl<T, const N: usize> Drop for ArrayVec<T, N> {
    fn drop(&mut self) {
        // Drop all initialized elements
        while self.pop().is_some() {}
    }
}

// Heapless ring buffer
pub struct RingBuffer<T: Copy, const N: usize> {
    data: [T; N],
    read_idx: usize,
    write_idx: usize,
}

impl<T: Copy + Default, const N: usize> RingBuffer<T, N> {
    pub fn new() -> Self {
        RingBuffer {
            data: [T::default(); N],
            read_idx: 0,
            write_idx: 0,
        }
    }

    pub fn push(&mut self, value: T) -> bool {
        let next_write = (self.write_idx + 1) % N;

        if next_write == self.read_idx {
            false  // Buffer full
        } else {
            self.data[self.write_idx] = value;
            self.write_idx = next_write;
            true
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.read_idx == self.write_idx {
            None  // Buffer empty
        } else {
            let value = self.data[self.read_idx];
            self.read_idx = (self.read_idx + 1) % N;
            Some(value)
        }
    }
}
```

### 3.4 Panic Handler en no_std

```rust
#![no_std]
#![no_main]

use core::panic::PanicInfo;

// Custom panic handler
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    // In embedded: maybe blink an LED or halt

    // Try to extract location
    if let Some(location) = info.location() {
        // Log to debug output if available
        // debug_print!("Panic at {}:{}", location.file(), location.line());
    }

    // Halt the system
    loop {
        // Architecture-specific halt instruction
        #[cfg(target_arch = "x86_64")]
        unsafe {
            core::arch::asm!("hlt");
        }

        #[cfg(target_arch = "arm")]
        unsafe {
            core::arch::asm!("wfi");  // Wait for interrupt
        }
    }
}

// Alternative: abort on panic
#[cfg(not(feature = "panic_halt"))]
#[panic_handler]
fn panic_abort(_info: &PanicInfo) -> ! {
    // Trigger immediate abort
    unsafe {
        core::intrinsics::abort();
    }
}
```

---

## 4. Unsafe Rust

### 4.1 Los Cinco Superpoderes de Unsafe

```rust
fn unsafe_superpowers() {
    // 1. Dereference raw pointers
    let mut num = 5;
    let r1 = &num as *const i32;
    let r2 = &mut num as *mut i32;

    unsafe {
        println!("r1 is: {}", *r1);
        *r2 = 10;
        println!("r2 is: {}", *r2);
    }

    // 2. Call unsafe functions or methods
    unsafe fn dangerous() {
        println!("This is dangerous!");
    }

    unsafe {
        dangerous();
    }

    // 3. Access or modify mutable static variables
    static mut COUNTER: u32 = 0;

    unsafe {
        COUNTER += 1;
        println!("COUNTER: {}", COUNTER);
    }

    // 4. Implement unsafe traits
    unsafe trait UnsafeTrait {
        fn method(&self);
    }

    // 5. Access fields of unions
    #[repr(C)]
    union IntOrFloat {
        i: i32,
        f: f32,
    }

    let u = IntOrFloat { i: 42 };
    unsafe {
        println!("As int: {}", u.i);
        println!("As float: {}", u.f);  // Reinterprets bits
    }
}
```

### 4.2 Safe Abstractions over Unsafe Code

```rust
use std::slice;

// Safe function wrapping unsafe code
fn split_at_mut(slice: &mut [i32], mid: usize) -> (&mut [i32], &mut [i32]) {
    let len = slice.len();
    let ptr = slice.as_mut_ptr();

    assert!(mid <= len);

    unsafe {
        (
            slice::from_raw_parts_mut(ptr, mid),
            slice::from_raw_parts_mut(ptr.add(mid), len - mid),
        )
    }
}

// Safe wrapper for memory-mapped I/O
pub struct MmioRegister<T: Copy> {
    ptr: *mut T,
}

impl<T: Copy> MmioRegister<T> {
    /// # Safety
    /// The caller must ensure ptr points to valid memory-mapped I/O register
    pub const unsafe fn new(ptr: *mut T) -> Self {
        MmioRegister { ptr }
    }

    pub fn read(&self) -> T {
        // Volatile read to prevent optimization
        unsafe { core::ptr::read_volatile(self.ptr) }
    }

    pub fn write(&self, value: T) {
        // Volatile write to ensure side effects
        unsafe { core::ptr::write_volatile(self.ptr, value) }
    }
}

// Example: GPIO register
struct GpioRegisters {
    data: MmioRegister<u32>,
    direction: MmioRegister<u32>,
}

fn gpio_example() {
    // Assume base address 0x4000_0000
    let gpio = unsafe {
        GpioRegisters {
            data: MmioRegister::new(0x4000_0000 as *mut u32),
            direction: MmioRegister::new(0x4000_0004 as *mut u32),
        }
    };

    // Now safe to use!
    gpio.direction.write(0xFF);  // All outputs
    gpio.data.write(0x55);       // Pattern
}
```

### 4.3 Inline Assembly

```rust
use std::arch::asm;

// x86_64 assembly examples
#[cfg(target_arch = "x86_64")]
fn inline_assembly_examples() {
    // Basic assembly
    let result: u64;
    unsafe {
        asm!(
            "mov {}, 42",
            out(reg) result,
        );
    }
    println!("Result: {}", result);

    // Add two numbers
    let a: u64 = 10;
    let b: u64 = 20;
    let sum: u64;
    unsafe {
        asm!(
            "add {0}, {1}",
            inout(reg) a => sum,
            in(reg) b,
        );
    }
    println!("Sum: {}", sum);

    // CPUID instruction
    let mut eax: u32 = 0;
    let mut ebx: u32;
    let mut ecx: u32;
    let mut edx: u32;

    unsafe {
        asm!(
            "cpuid",
            inout("eax") eax,
            out("ebx") ebx,
            out("ecx") ecx,
            out("edx") edx,
        );
    }

    // Read processor vendor string
    let vendor = [
        ebx.to_le_bytes(),
        edx.to_le_bytes(),
        ecx.to_le_bytes(),
    ].concat();

    if let Ok(s) = std::str::from_utf8(&vendor) {
        println!("CPU Vendor: {}", s);
    }
}

// ARM assembly example
#[cfg(target_arch = "arm")]
fn arm_assembly() {
    // Read CPSR (Current Program Status Register)
    let cpsr: u32;
    unsafe {
        asm!(
            "mrs {}, cpsr",
            out(reg) cpsr,
        );
    }
    println!("CPSR: {:#010x}", cpsr);
}
```

### 4.4 Transmute y Reinterpretación de Bits

```rust
use std::mem;

fn transmute_examples() {
    // Reinterpret bytes as different type
    let bytes: [u8; 4] = [0x00, 0x00, 0x80, 0x3f];
    let float: f32 = unsafe { mem::transmute(bytes) };
    println!("Float: {}", float);  // 1.0

    // Function pointer casting
    fn add(a: i32, b: i32) -> i32 { a + b }

    let fn_ptr: fn(i32, i32) -> i32 = add;
    let address: usize = fn_ptr as usize;
    println!("Function address: {:#x}", address);

    // Safer alternative: use from_ne_bytes
    let bytes: [u8; 4] = [0x00, 0x00, 0x80, 0x3f];
    let float = f32::from_le_bytes(bytes);  // No unsafe needed!

    // Transmuting references (dangerous!)
    let num = 42i32;
    let bytes: &[u8; 4] = unsafe { mem::transmute(&num) };
    println!("Bytes: {:?}", bytes);

    // Better: use to_ne_bytes
    let bytes = num.to_ne_bytes();
}

// Type punning with unions (C-compatible)
#[repr(C)]
union Reinterpret {
    u: u32,
    f: f32,
    bytes: [u8; 4],
}

fn union_reinterpret() {
    let u = Reinterpret { f: 1.0 };

    unsafe {
        println!("As u32: {:#010x}", u.u);
        println!("As bytes: {:?}", u.bytes);
    }
}
```

---

## 5. Memory Layout y Alignment

### 5.1 Representación de Structs

```rust
use std::mem;

// Default Rust representation - may reorder fields
struct DefaultRepr {
    a: u8,
    b: u32,
    c: u16,
}

// C-compatible representation
#[repr(C)]
struct CRepr {
    a: u8,
    b: u32,
    c: u16,
}

// Packed representation (no padding)
#[repr(C, packed)]
struct PackedRepr {
    a: u8,
    b: u32,
    c: u16,
}

// Aligned representation
#[repr(C, align(16))]
struct AlignedRepr {
    data: [u8; 8],
}

fn layout_examples() {
    println!("DefaultRepr:");
    println!("  Size: {}", mem::size_of::<DefaultRepr>());
    println!("  Align: {}", mem::align_of::<DefaultRepr>());

    println!("CRepr:");
    println!("  Size: {}", mem::size_of::<CRepr>());      // 12
    println!("  Align: {}", mem::align_of::<CRepr>());    // 4

    println!("PackedRepr:");
    println!("  Size: {}", mem::size_of::<PackedRepr>()); // 7
    println!("  Align: {}", mem::align_of::<PackedRepr>());// 1

    println!("AlignedRepr:");
    println!("  Size: {}", mem::size_of::<AlignedRepr>()); // 16
    println!("  Align: {}", mem::align_of::<AlignedRepr>());// 16
}
```

### 5.2 Field Offsets y Padding

```rust
use std::mem;

#[repr(C)]
struct Example {
    a: u8,      // offset 0, size 1
                // padding: 3 bytes
    b: u32,     // offset 4, size 4
    c: u16,     // offset 8, size 2
                // padding: 2 bytes
}               // total size: 12, alignment: 4

macro_rules! offset_of {
    ($type:ty, $field:ident) => {{
        let uninit = std::mem::MaybeUninit::<$type>::uninit();
        let base_ptr = uninit.as_ptr();
        let field_ptr = unsafe { std::ptr::addr_of!((*base_ptr).$field) };
        (field_ptr as usize) - (base_ptr as usize)
    }};
}

fn field_offset_example() {
    println!("Field offsets in Example:");
    println!("  a: {}", offset_of!(Example, a));  // 0
    println!("  b: {}", offset_of!(Example, b));  // 4
    println!("  c: {}", offset_of!(Example, c));  // 8

    // Visualize memory layout
    let example = Example { a: 0xAA, b: 0xBBBBBBBB, c: 0xCCCC };
    let bytes: &[u8; 12] = unsafe { mem::transmute(&example) };

    println!("Memory layout:");
    for (i, byte) in bytes.iter().enumerate() {
        println!("  [{:2}]: 0x{:02x}", i, byte);
    }
}
```

### 5.3 Enums y Discriminants

```rust
use std::mem;

// Simple enum
#[repr(u8)]
enum SimpleEnum {
    A = 0,
    B = 1,
    C = 2,
}

// Enum with data - tagged union
#[repr(C)]
enum Tagged {
    Empty,
    Small(u8),
    Large([u8; 32]),
}

// Nullable pointer optimization
enum NullablePtr<T> {
    Null,
    Valid(Box<T>),
}

fn enum_layout() {
    println!("SimpleEnum size: {}", mem::size_of::<SimpleEnum>());  // 1

    println!("Tagged size: {}", mem::size_of::<Tagged>());

    // Nullable pointer optimization in action
    println!("Option<Box<i32>> size: {}", mem::size_of::<Option<Box<i32>>>());  // 8
    println!("Box<i32> size: {}", mem::size_of::<Box<i32>>());  // 8
    // Same size! No tag needed because Box is never null

    // Option<&T> also gets this optimization
    println!("Option<&i32> size: {}", mem::size_of::<Option<&i32>>());  // 8
    println!("&i32 size: {}", mem::size_of::<&i32>());  // 8
}
```

### 5.4 DST (Dynamically Sized Types)

```rust
use std::mem;

// Slice is a DST - [T] has unknown size
fn slice_dst() {
    let arr = [1, 2, 3, 4, 5];
    let slice: &[i32] = &arr;

    // Slice reference is a fat pointer: (ptr, len)
    println!("&[i32] size: {}", mem::size_of::<&[i32]>());  // 16 (8 + 8)
    println!("&i32 size: {}", mem::size_of::<&i32>());      // 8

    // Access underlying fat pointer data
    let fat_ptr: (*const i32, usize) = unsafe { mem::transmute(slice) };
    println!("Pointer: {:?}, Length: {}", fat_ptr.0, fat_ptr.1);
}

// Trait objects are DSTs
trait Animal {
    fn speak(&self);
}

fn trait_object_dst() {
    // Trait object reference is a fat pointer: (ptr, vtable_ptr)
    println!("&dyn Animal size: {}", mem::size_of::<&dyn Animal>());  // 16

    // Box<dyn Trait> is also a fat pointer
    println!("Box<dyn Animal> size: {}", mem::size_of::<Box<dyn Animal>>());  // 16
}

// Custom DST with last field as slice
struct CustomDst {
    header: u32,
    data: [u8],  // Unsized last field
}

// Cannot create CustomDst directly - must use:
// - Reference from existing data
// - Box::new_uninit_slice and similar
```

---

## 6. Interfacing con Hardware

### 6.1 Memory-Mapped I/O

```rust
use core::ptr::{read_volatile, write_volatile};

// Memory-mapped register access
#[repr(C)]
struct UartRegisters {
    data: u32,
    status: u32,
    control: u32,
    baud: u32,
}

struct Uart {
    regs: *mut UartRegisters,
}

impl Uart {
    pub unsafe fn new(base_address: usize) -> Self {
        Uart {
            regs: base_address as *mut UartRegisters,
        }
    }

    pub fn write_byte(&self, byte: u8) {
        unsafe {
            // Wait until transmit buffer is empty
            while read_volatile(&(*self.regs).status) & 0x20 == 0 {}

            // Write byte
            write_volatile(&mut (*self.regs).data, byte as u32);
        }
    }

    pub fn read_byte(&self) -> Option<u8> {
        unsafe {
            // Check if data available
            if read_volatile(&(*self.regs).status) & 0x01 != 0 {
                Some(read_volatile(&(*self.regs).data) as u8)
            } else {
                None
            }
        }
    }

    pub fn set_baud_rate(&self, rate: u32) {
        unsafe {
            write_volatile(&mut (*self.regs).baud, rate);
        }
    }
}

// Using volatile wrapper type
use core::cell::UnsafeCell;

#[repr(transparent)]
pub struct Volatile<T: Copy> {
    value: UnsafeCell<T>,
}

impl<T: Copy> Volatile<T> {
    pub fn read(&self) -> T {
        unsafe { read_volatile(self.value.get()) }
    }

    pub fn write(&self, value: T) {
        unsafe { write_volatile(self.value.get(), value) }
    }
}

// Better UART with Volatile wrapper
#[repr(C)]
struct SafeUartRegisters {
    data: Volatile<u32>,
    status: Volatile<u32>,
    control: Volatile<u32>,
    baud: Volatile<u32>,
}
```

### 6.2 DMA (Direct Memory Access)

```rust
use core::sync::atomic::{AtomicBool, Ordering};

// DMA buffer must be properly aligned and non-cacheable
#[repr(C, align(32))]
struct DmaBuffer {
    data: [u8; 4096],
}

impl DmaBuffer {
    pub fn new() -> Self {
        DmaBuffer { data: [0; 4096] }
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.data.as_ptr()
    }

    pub fn as_mut_ptr(&mut self) -> *mut u8 {
        self.data.as_mut_ptr()
    }
}

struct DmaController {
    base: usize,
    transfer_complete: AtomicBool,
}

impl DmaController {
    pub unsafe fn new(base: usize) -> Self {
        DmaController {
            base,
            transfer_complete: AtomicBool::new(true),
        }
    }

    pub fn start_transfer(&self, src: *const u8, dst: *mut u8, len: usize) {
        // Ensure previous transfer is complete
        while !self.transfer_complete.load(Ordering::Acquire) {}

        self.transfer_complete.store(false, Ordering::Release);

        unsafe {
            // Write DMA registers
            let regs = self.base as *mut u32;
            write_volatile(regs.offset(0), src as u32);      // Source address
            write_volatile(regs.offset(1), dst as u32);      // Destination address
            write_volatile(regs.offset(2), len as u32);      // Transfer length
            write_volatile(regs.offset(3), 0x01);            // Start transfer
        }
    }

    pub fn is_complete(&self) -> bool {
        self.transfer_complete.load(Ordering::Acquire)
    }

    pub fn wait_complete(&self) {
        while !self.is_complete() {
            core::hint::spin_loop();
        }
    }

    // Called from interrupt handler
    pub fn handle_interrupt(&self) {
        self.transfer_complete.store(true, Ordering::Release);
    }
}

use core::ptr::write_volatile;
```

### 6.3 Interrupt Handling

```rust
use core::sync::atomic::{AtomicUsize, Ordering};

// Interrupt vector table
#[repr(C)]
pub struct InterruptVector {
    handler: extern "C" fn(),
}

// Static interrupt handlers
static TIMER_COUNT: AtomicUsize = AtomicUsize::new(0);

#[no_mangle]
extern "C" fn timer_interrupt_handler() {
    TIMER_COUNT.fetch_add(1, Ordering::Relaxed);

    // Acknowledge interrupt
    unsafe {
        let timer_status = 0x4000_0000 as *mut u32;
        core::ptr::write_volatile(timer_status, 0x01);
    }
}

// Critical section (disable interrupts)
#[inline]
pub fn critical_section<F, R>(f: F) -> R
where
    F: FnOnce() -> R,
{
    // Architecture-specific interrupt disable
    #[cfg(target_arch = "arm")]
    unsafe {
        let primask: u32;
        core::arch::asm!("mrs {}, PRIMASK", out(reg) primask);
        core::arch::asm!("cpsid i");  // Disable interrupts

        let result = f();

        // Restore previous interrupt state
        if primask == 0 {
            core::arch::asm!("cpsie i");  // Enable interrupts
        }

        result
    }

    #[cfg(not(target_arch = "arm"))]
    f()  // No-op on other architectures
}

// Usage
fn critical_section_example() {
    let shared_data = AtomicUsize::new(0);

    critical_section(|| {
        // Safe to access shared state here
        let value = shared_data.load(Ordering::Relaxed);
        shared_data.store(value + 1, Ordering::Relaxed);
    });
}
```

---

## 7. Reemplazando Código C de Sistema

### 7.1 Migración de malloc/free a Rust

```c
// C: Manual memory management
#include <stdlib.h>

typedef struct {
    int* data;
    size_t len;
    size_t capacity;
} IntArray;

IntArray* int_array_new(size_t capacity) {
    IntArray* arr = malloc(sizeof(IntArray));
    if (!arr) return NULL;

    arr->data = malloc(capacity * sizeof(int));
    if (!arr->data) {
        free(arr);
        return NULL;
    }

    arr->len = 0;
    arr->capacity = capacity;
    return arr;
}

void int_array_push(IntArray* arr, int value) {
    if (arr->len >= arr->capacity) {
        size_t new_cap = arr->capacity * 2;
        int* new_data = realloc(arr->data, new_cap * sizeof(int));
        if (!new_data) return;  // Handle error?
        arr->data = new_data;
        arr->capacity = new_cap;
    }
    arr->data[arr->len++] = value;
}

void int_array_free(IntArray* arr) {
    if (arr) {
        free(arr->data);
        free(arr);
    }
}
```

```rust
// Rust: Automatic memory management
struct IntArray {
    data: Vec<i32>,
}

impl IntArray {
    fn new(capacity: usize) -> Self {
        IntArray {
            data: Vec::with_capacity(capacity),
        }
    }

    fn push(&mut self, value: i32) {
        self.data.push(value);  // Automatic reallocation
    }

    fn get(&self, index: usize) -> Option<i32> {
        self.data.get(index).copied()
    }

    fn len(&self) -> usize {
        self.data.len()
    }
}

// No free function needed! Memory freed when IntArray is dropped

fn rust_example() {
    let mut arr = IntArray::new(10);
    arr.push(1);
    arr.push(2);
    arr.push(3);

    // arr is automatically freed when it goes out of scope
}
```

### 7.2 Migración de Linked List

```c
// C: Linked list with pointers
struct Node {
    int data;
    struct Node* next;
};

struct Node* list_new(int data) {
    struct Node* node = malloc(sizeof(struct Node));
    if (node) {
        node->data = data;
        node->next = NULL;
    }
    return node;
}

void list_append(struct Node** head, int data) {
    struct Node* new_node = list_new(data);
    if (!new_node) return;

    if (*head == NULL) {
        *head = new_node;
        return;
    }

    struct Node* current = *head;
    while (current->next != NULL) {
        current = current->next;
    }
    current->next = new_node;
}

void list_free(struct Node* head) {
    while (head) {
        struct Node* temp = head;
        head = head->next;
        free(temp);
    }
}
```

```rust
// Rust: Safe linked list
type Link<T> = Option<Box<Node<T>>>;

struct Node<T> {
    data: T,
    next: Link<T>,
}

pub struct LinkedList<T> {
    head: Link<T>,
    len: usize,
}

impl<T> LinkedList<T> {
    pub fn new() -> Self {
        LinkedList { head: None, len: 0 }
    }

    pub fn push_front(&mut self, data: T) {
        let new_node = Box::new(Node {
            data,
            next: self.head.take(),
        });
        self.head = Some(new_node);
        self.len += 1;
    }

    pub fn pop_front(&mut self) -> Option<T> {
        self.head.take().map(|node| {
            self.head = node.next;
            self.len -= 1;
            node.data
        })
    }

    pub fn push_back(&mut self, data: T) {
        let new_node = Box::new(Node { data, next: None });

        match &mut self.head {
            None => self.head = Some(new_node),
            Some(mut current) => {
                while current.next.is_some() {
                    current = current.next.as_mut().unwrap();
                }
                current.next = Some(new_node);
            }
        }
        self.len += 1;
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn iter(&self) -> Iter<T> {
        Iter {
            next: self.head.as_deref(),
        }
    }
}

// Iterator implementation
pub struct Iter<'a, T> {
    next: Option<&'a Node<T>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            self.next = node.next.as_deref();
            &node.data
        })
    }
}

// Automatic cleanup via Drop
impl<T> Drop for LinkedList<T> {
    fn drop(&mut self) {
        // Iteratively drop to avoid stack overflow on long lists
        let mut current = self.head.take();
        while let Some(mut node) = current {
            current = node.next.take();
        }
    }
}
```

### 7.3 Migración de File I/O

```c
// C: File I/O with error handling
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* read_file(const char* path, size_t* out_len) {
    FILE* f = fopen(path, "rb");
    if (!f) {
        return NULL;
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char* buffer = malloc(size + 1);
    if (!buffer) {
        fclose(f);
        return NULL;
    }

    size_t read = fread(buffer, 1, size, f);
    fclose(f);

    if (read != (size_t)size) {
        free(buffer);
        return NULL;
    }

    buffer[size] = '\0';
    *out_len = size;
    return buffer;
}

int write_file(const char* path, const char* data, size_t len) {
    FILE* f = fopen(path, "wb");
    if (!f) return -1;

    size_t written = fwrite(data, 1, len, f);
    fclose(f);

    return (written == len) ? 0 : -1;
}
```

```rust
// Rust: File I/O with Result
use std::fs::{self, File};
use std::io::{self, Read, Write, BufReader, BufWriter};
use std::path::Path;

fn read_file(path: impl AsRef<Path>) -> io::Result<String> {
    fs::read_to_string(path)
}

fn read_file_bytes(path: impl AsRef<Path>) -> io::Result<Vec<u8>> {
    fs::read(path)
}

fn read_file_buffered(path: impl AsRef<Path>) -> io::Result<String> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    Ok(contents)
}

fn write_file(path: impl AsRef<Path>, data: &str) -> io::Result<()> {
    fs::write(path, data)
}

fn write_file_buffered(path: impl AsRef<Path>, data: &[u8]) -> io::Result<()> {
    let file = File::create(path)?;
    let mut writer = BufWriter::new(file);
    writer.write_all(data)?;
    writer.flush()?;
    Ok(())
}

// With proper error handling
fn process_file(input: &str, output: &str) -> io::Result<()> {
    let contents = read_file(input)?;

    let processed = contents.to_uppercase();

    write_file(output, &processed)?;

    Ok(())
}

fn main() {
    match process_file("input.txt", "output.txt") {
        Ok(()) => println!("Success!"),
        Err(e) => eprintln!("Error: {}", e),
    }
}
```

---

## 8. Tablas de Referencia

### 8.1 Unsafe Operations Reference

| Operación | Requisito | Peligros |
|-----------|-----------|----------|
| `*ptr` (deref raw) | `unsafe` block | Null, dangling, alignment |
| `ptr.offset(n)` | `unsafe` block | Out of bounds |
| `ptr.read()` | `unsafe` block | Uninitialized memory |
| `ptr.write()` | `unsafe` block | Overwriting valid data |
| `transmute` | `unsafe` block | Wrong type interpretation |
| Static mut access | `unsafe` block | Data races |
| `asm!` | `unsafe` block | Architecture-specific issues |
| FFI calls | `unsafe` block | ABI mismatch |

### 8.2 Memory Layout Attributes

| Atributo | Efecto |
|----------|--------|
| `#[repr(C)]` | C-compatible layout |
| `#[repr(packed)]` | No padding |
| `#[repr(align(N))]` | Minimum alignment N |
| `#[repr(transparent)]` | Same as inner type |
| `#[repr(u8)]` | Enum discriminant as u8 |

### 8.3 Volatile Operations

| Función | Uso |
|---------|-----|
| `read_volatile(ptr)` | MMIO read |
| `write_volatile(ptr, val)` | MMIO write |
| `copy_nonoverlapping(src, dst, n)` | memcpy |
| `copy(src, dst, n)` | memmove |
| `write_bytes(ptr, val, n)` | memset |

### 8.4 no_std Feature Comparison

| Feature | std | core | alloc |
|---------|-----|------|-------|
| `println!` | Yes | No | No |
| `Vec<T>` | Yes | No | Yes |
| `String` | Yes | No | Yes |
| `Box<T>` | Yes | No | Yes |
| `HashMap` | Yes | No | Yes |
| Threads | Yes | No | No |
| File I/O | Yes | No | No |
| Networking | Yes | No | No |
| `Option<T>` | Yes | Yes | Yes |
| `Result<T,E>` | Yes | Yes | Yes |
| `mem::*` | Yes | Yes | Yes |
| `ptr::*` | Yes | Yes | Yes |

---

## Referencias y Recursos Adicionales

1. **The Rustonomicon**: https://doc.rust-lang.org/nomicon/
2. **Embedded Rust Book**: https://docs.rust-embedded.org/book/
3. **Writing an OS in Rust**: https://os.phil-opp.com/
4. **Rust Atomics and Locks**: https://marabos.nl/atomics/

---

*Documento generado por ARCHAEON_CORE - Sistema de Documentación de Lenguajes*
*Especialización: Programación de Sistemas C → Rust*
*Última actualización: 2025-12-31*

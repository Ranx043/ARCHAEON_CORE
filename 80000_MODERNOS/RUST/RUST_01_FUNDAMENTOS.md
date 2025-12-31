---
título: "RUST 01 - Fundamentos del Lenguaje"
versión: "1.0.0"
fecha_creación: "2025-12-31"
última_actualización: "2025-12-31"
autor: "ARCHAEON_CORE"
dominio: "Lenguajes Modernos / Rust"
especialización: "Fundamentos y Sistema de Ownership"
contexto_soul_core: "ARCHAEON - Evolución C → Rust"
tags:
  - rust
  - ownership
  - borrowing
  - lifetimes
  - pattern-matching
  - error-handling
  - traits
  - generics
  - smart-pointers
  - memory-safety
dependencias:
  - conocimiento_c: "intermedio"
  - sistemas: "básico"
---

# RUST 01: FUNDAMENTOS DEL LENGUAJE

## Índice de Contenidos

1. [Introducción: De C a Rust](#1-introducción-de-c-a-rust)
2. [Sistema de Ownership](#2-sistema-de-ownership)
3. [Borrowing y Referencias](#3-borrowing-y-referencias)
4. [Lifetimes](#4-lifetimes)
5. [Pattern Matching y Enums](#5-pattern-matching-y-enums)
6. [Error Handling](#6-error-handling)
7. [Traits y Generics](#7-traits-y-generics)
8. [Smart Pointers](#8-smart-pointers)
9. [Comparación con Punteros C](#9-comparación-con-punteros-c)
10. [Tablas de Referencia](#10-tablas-de-referencia)

---

## 1. Introducción: De C a Rust

### 1.1 Por Qué Rust Reemplaza a C

Rust es un lenguaje de sistemas diseñado para proporcionar la velocidad y control
de bajo nivel de C, pero con garantías de seguridad de memoria en tiempo de compilación.

```rust
// Rust: Memory safety guaranteed at compile time
fn main() {
    let data = vec![1, 2, 3, 4, 5];

    // This would be a compile error - can't use after move
    // let moved = data;
    // println!("{:?}", data); // ERROR: value borrowed after move

    // Safe access with borrowing
    let borrowed = &data;
    println!("Data: {:?}", borrowed);
    println!("Original still valid: {:?}", data);
}
```

```c
// C: Manual memory management, potential for errors
#include <stdlib.h>
#include <stdio.h>

int main() {
    int* data = malloc(5 * sizeof(int));

    // Potential issues:
    // - Forgot to check if malloc returned NULL
    // - Use after free
    // - Double free
    // - Memory leaks

    free(data);
    // data is now dangling pointer - undefined behavior if used
    printf("%d\n", data[0]); // UB: use after free

    return 0;
}
```

### 1.2 Filosofía de Diseño de Rust

| Principio | C | Rust |
|-----------|---|------|
| Seguridad de memoria | Manual | Compile-time |
| Gestión de recursos | Manual (malloc/free) | RAII automático |
| Null pointers | Común | No existen (Option<T>) |
| Data races | Posibles | Imposibles (compilador) |
| Undefined behavior | Frecuente | Solo en `unsafe` |

---

## 2. Sistema de Ownership

### 2.1 Las Tres Reglas del Ownership

```rust
// Rule 1: Each value has exactly one owner
fn ownership_rule_one() {
    let s1 = String::from("hello"); // s1 owns the String
    let s2 = s1;                     // Ownership moved to s2

    // println!("{}", s1); // ERROR: s1 no longer owns the value
    println!("{}", s2);    // OK: s2 is the owner
}

// Rule 2: There can only be one owner at a time
fn ownership_rule_two() {
    let s = String::from("world");
    takes_ownership(s);  // s's value moves into the function

    // println!("{}", s); // ERROR: s no longer valid
}

fn takes_ownership(some_string: String) {
    println!("{}", some_string);
} // some_string goes out of scope, memory is freed

// Rule 3: When the owner goes out of scope, the value is dropped
fn ownership_rule_three() {
    {
        let s = String::from("scoped");
        println!("{}", s);
    } // s goes out of scope, Drop is called automatically

    // s is not accessible here
}
```

### 2.2 Move Semantics vs Copy

```rust
// Types that implement Copy trait are copied, not moved
fn copy_types() {
    // Primitive types implement Copy
    let x: i32 = 5;
    let y = x;  // x is copied, not moved

    println!("x = {}, y = {}", x, y); // Both valid!

    // Stack-only data is cheap to copy
    let a: f64 = 3.14;
    let b = a;  // Copied

    let c: bool = true;
    let d = c;  // Copied

    let e: char = 'R';
    let f = e;  // Copied
}

// Types that allocate heap memory are moved
fn move_types() {
    let s1 = String::from("heap allocated");
    let s2 = s1;  // Moved, not copied

    // s1 is now invalid

    let v1 = vec![1, 2, 3];
    let v2 = v1;  // Moved

    // v1 is now invalid
}

// Implementing Copy for your types
#[derive(Copy, Clone)]
struct Point {
    x: f64,
    y: f64,
}

fn custom_copy() {
    let p1 = Point { x: 1.0, y: 2.0 };
    let p2 = p1;  // Copied because Point implements Copy

    println!("p1: ({}, {})", p1.x, p1.y); // Valid!
    println!("p2: ({}, {})", p2.x, p2.y);
}
```

### 2.3 Clone para Copias Explícitas

```rust
fn explicit_cloning() {
    let s1 = String::from("original");
    let s2 = s1.clone();  // Explicit deep copy

    println!("s1 = {}", s1); // Valid - s1 still owns original
    println!("s2 = {}", s2); // s2 owns the clone

    // Clone can be expensive for large data
    let big_vec: Vec<i32> = (0..1_000_000).collect();
    let cloned_vec = big_vec.clone();  // Copies 1 million integers
}

// Implementing Clone for custom types
#[derive(Clone)]
struct Buffer {
    data: Vec<u8>,
    position: usize,
}

impl Buffer {
    fn new(size: usize) -> Self {
        Buffer {
            data: vec![0; size],
            position: 0,
        }
    }
}

fn clone_buffer() {
    let buf1 = Buffer::new(1024);
    let buf2 = buf1.clone();  // Deep copy of data vector
}
```

### 2.4 Ownership en Funciones

```rust
// Passing ownership to functions
fn take_string(s: String) {
    println!("Received: {}", s);
} // s is dropped here

// Returning ownership from functions
fn give_string() -> String {
    let s = String::from("new string");
    s  // Ownership transferred to caller
}

// Taking and returning ownership
fn process_and_return(mut s: String) -> String {
    s.push_str(" - processed");
    s  // Return ownership
}

fn ownership_in_practice() {
    let s1 = String::from("hello");
    take_string(s1);  // s1 moved
    // s1 no longer valid

    let s2 = give_string();  // s2 now owns the string
    println!("Received: {}", s2);

    let s3 = String::from("data");
    let s4 = process_and_return(s3);  // s3 moved, s4 owns result
    println!("Processed: {}", s4);
}
```

---

## 3. Borrowing y Referencias

### 3.1 Referencias Inmutables

```rust
fn immutable_references() {
    let s = String::from("hello");

    // Create immutable reference
    let r1 = &s;
    let r2 = &s;  // Multiple immutable refs allowed

    println!("r1: {}, r2: {}", r1, r2);

    // Original still valid
    println!("s: {}", s);
}

// Passing references to functions
fn calculate_length(s: &String) -> usize {
    s.len()
} // s goes out of scope but doesn't drop the String

fn borrowing_in_functions() {
    let s = String::from("hello");
    let len = calculate_length(&s);  // Borrow s

    println!("Length of '{}' is {}", s, len);  // s still valid
}
```

### 3.2 Referencias Mutables

```rust
fn mutable_references() {
    let mut s = String::from("hello");

    // Create mutable reference
    let r = &mut s;
    r.push_str(", world!");

    println!("{}", r);
}

// Only one mutable reference at a time
fn mutable_reference_rules() {
    let mut s = String::from("hello");

    let r1 = &mut s;
    // let r2 = &mut s;  // ERROR: cannot borrow as mutable more than once

    r1.push_str("!");
    println!("{}", r1);

    // After r1's last use, we can create new mutable ref
    let r2 = &mut s;
    r2.push_str("!");
}

// Cannot mix mutable and immutable references
fn no_mixing_references() {
    let mut s = String::from("hello");

    let r1 = &s;     // OK - immutable borrow
    let r2 = &s;     // OK - multiple immutable borrows

    // let r3 = &mut s;  // ERROR: cannot borrow as mutable while immutable borrow exists

    println!("{} and {}", r1, r2);
    // r1 and r2 no longer used after this point

    let r3 = &mut s;  // OK - immutable refs are done
    r3.push_str("!");
}
```

### 3.3 Dangling References (Prevención)

```rust
// Rust prevents dangling references at compile time
// fn dangle() -> &String {  // ERROR: missing lifetime specifier
//     let s = String::from("hello");
//     &s  // s is dropped here, reference would be dangling
// }

// Correct approach: return owned value
fn no_dangle() -> String {
    let s = String::from("hello");
    s  // Ownership is moved out
}

// Or use references with proper lifetimes
fn with_lifetime<'a>(s: &'a String) -> &'a str {
    &s[..]
}
```

### 3.4 Slices

```rust
fn string_slices() {
    let s = String::from("hello world");

    // String slices
    let hello: &str = &s[0..5];
    let world: &str = &s[6..11];

    println!("{} {}", hello, world);

    // Shorthand syntax
    let hello = &s[..5];   // From start
    let world = &s[6..];   // To end
    let full = &s[..];     // Full slice
}

fn array_slices() {
    let a = [1, 2, 3, 4, 5];

    let slice: &[i32] = &a[1..3];

    assert_eq!(slice, &[2, 3]);

    // Mutable slices
    let mut arr = [1, 2, 3, 4, 5];
    let slice_mut: &mut [i32] = &mut arr[..3];
    slice_mut[0] = 10;

    assert_eq!(arr, [10, 2, 3, 4, 5]);
}

// Function that works with slices (more flexible)
fn first_word(s: &str) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[..i];
        }
    }

    &s[..]
}

fn slice_flexibility() {
    let my_string = String::from("hello world");

    // Works with String slices
    let word = first_word(&my_string[..]);

    // Works with string literals too
    let literal = "hello world";
    let word = first_word(literal);

    // Deref coercion: &String -> &str
    let word = first_word(&my_string);
}
```

---

## 4. Lifetimes

### 4.1 Anotaciones de Lifetime

```rust
// Explicit lifetime annotations
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

fn lifetime_example() {
    let string1 = String::from("long string is long");

    {
        let string2 = String::from("xyz");
        let result = longest(string1.as_str(), string2.as_str());
        println!("The longest string is '{}'", result);
    }

    // This would fail - string2 doesn't live long enough
    // let result;
    // {
    //     let string2 = String::from("xyz");
    //     result = longest(string1.as_str(), string2.as_str());
    // }
    // println!("Result: {}", result);
}
```

### 4.2 Lifetimes en Estructuras

```rust
// Struct holding references must have lifetime annotations
struct ImportantExcerpt<'a> {
    part: &'a str,
}

impl<'a> ImportantExcerpt<'a> {
    fn level(&self) -> i32 {
        3
    }

    // Lifetime elision rules apply here
    fn announce_and_return_part(&self, announcement: &str) -> &str {
        println!("Attention please: {}", announcement);
        self.part
    }
}

fn struct_lifetimes() {
    let novel = String::from("Call me Ishmael. Some years ago...");
    let first_sentence = novel.split('.').next().expect("Could not find '.'");

    let excerpt = ImportantExcerpt {
        part: first_sentence,
    };

    println!("Excerpt: {}", excerpt.part);
}
```

### 4.3 Lifetime Elision Rules

```rust
// Rule 1: Each reference parameter gets its own lifetime
// fn foo(x: &i32) becomes fn foo<'a>(x: &'a i32)

// Rule 2: If exactly one input lifetime, it's assigned to all outputs
// fn foo(x: &i32) -> &i32 becomes fn foo<'a>(x: &'a i32) -> &'a i32

// Rule 3: If &self or &mut self, self's lifetime is assigned to outputs
impl<'a> ImportantExcerpt<'a> {
    // No need to annotate - self's lifetime is used for return
    fn get_part(&self) -> &str {
        self.part
    }
}

// Examples where elision works
fn first_word_elided(s: &str) -> &str {
    // Compiler infers: fn first_word<'a>(s: &'a str) -> &'a str
    let bytes = s.as_bytes();
    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[..i];
        }
    }
    &s[..]
}
```

### 4.4 Static Lifetime

```rust
// 'static lifetime: lives for entire program duration
fn static_lifetime() {
    // String literals have 'static lifetime
    let s: &'static str = "I live forever!";

    // Constants are 'static
    const GREETING: &str = "Hello, world!";

    // Static variables
    static LANGUAGE: &str = "Rust";

    println!("{} {} {}", s, GREETING, LANGUAGE);
}

// Be careful with 'static in generics
fn print_static<T: std::fmt::Display + 'static>(t: T) {
    println!("{}", t);
}

// This requires T to be owned or have 'static references
fn static_bound_example() {
    let owned = String::from("owned");
    print_static(owned);  // OK - owned types satisfy 'static

    let static_ref: &'static str = "static";
    print_static(static_ref);  // OK - 'static reference

    // let local = String::from("local");
    // print_static(&local);  // ERROR - &local is not 'static
}
```

---

## 5. Pattern Matching y Enums

### 5.1 Enums Básicos

```rust
// Simple enum
enum IpAddrKind {
    V4,
    V6,
}

// Enum with data
enum IpAddr {
    V4(u8, u8, u8, u8),
    V6(String),
}

// Enum with different variant types
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

impl Message {
    fn call(&self) {
        match self {
            Message::Quit => println!("Quit"),
            Message::Move { x, y } => println!("Move to ({}, {})", x, y),
            Message::Write(text) => println!("Write: {}", text),
            Message::ChangeColor(r, g, b) => println!("Color: ({}, {}, {})", r, g, b),
        }
    }
}

fn enum_usage() {
    let home = IpAddr::V4(127, 0, 0, 1);
    let loopback = IpAddr::V6(String::from("::1"));

    let msg = Message::Write(String::from("hello"));
    msg.call();
}
```

### 5.2 Pattern Matching con match

```rust
enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter(UsState),
}

#[derive(Debug)]
enum UsState {
    Alabama,
    Alaska,
    // ... etc
}

fn value_in_cents(coin: Coin) -> u8 {
    match coin {
        Coin::Penny => {
            println!("Lucky penny!");
            1
        }
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter(state) => {
            println!("State quarter from {:?}!", state);
            25
        }
    }
}

// Matching with patterns
fn match_patterns() {
    let x = 5;

    match x {
        1 => println!("one"),
        2 | 3 => println!("two or three"),  // Or pattern
        4..=6 => println!("four through six"),  // Range pattern
        _ => println!("something else"),  // Catch-all
    }

    // Destructuring tuples
    let pair = (0, -2);
    match pair {
        (0, y) => println!("First is zero, y = {}", y),
        (x, 0) => println!("x = {}, second is zero", x),
        _ => println!("No zeros"),
    }

    // Destructuring structs
    struct Point { x: i32, y: i32 }
    let p = Point { x: 0, y: 7 };

    match p {
        Point { x: 0, y } => println!("On y axis at {}", y),
        Point { x, y: 0 } => println!("On x axis at {}", x),
        Point { x, y } => println!("At ({}, {})", x, y),
    }
}
```

### 5.3 if let y while let

```rust
fn if_let_example() {
    let config_max = Some(3u8);

    // Verbose match
    match config_max {
        Some(max) => println!("Maximum is {}", max),
        _ => (),
    }

    // Concise if let
    if let Some(max) = config_max {
        println!("Maximum is {}", max);
    }

    // if let with else
    let coin = Coin::Penny;
    if let Coin::Quarter(state) = coin {
        println!("State: {:?}", state);
    } else {
        println!("Not a quarter");
    }
}

fn while_let_example() {
    let mut stack = Vec::new();
    stack.push(1);
    stack.push(2);
    stack.push(3);

    // Pop until empty
    while let Some(top) = stack.pop() {
        println!("{}", top);
    }
}
```

### 5.4 Guards y Bindings

```rust
fn match_guards() {
    let num = Some(4);

    match num {
        Some(x) if x < 5 => println!("less than five: {}", x),
        Some(x) => println!("{}", x),
        None => (),
    }

    let x = 4;
    let y = false;

    match x {
        4 | 5 | 6 if y => println!("yes"),
        _ => println!("no"),
    }
}

fn at_bindings() {
    enum Color {
        Rgb(i32, i32, i32),
        Hsv(i32, i32, i32),
    }

    let color = Color::Rgb(122, 17, 40);

    match color {
        Color::Rgb(r @ 0..=100, g, b) => {
            println!("Red {} in range, g={}, b={}", r, g, b);
        }
        Color::Rgb(r, g, b) => {
            println!("RGB: {}, {}, {}", r, g, b);
        }
        Color::Hsv(h, s, v) => {
            println!("HSV: {}, {}, {}", h, s, v);
        }
    }
}
```

---

## 6. Error Handling

### 6.1 Option<T>

```rust
// Option replaces null pointers
fn find_element(arr: &[i32], target: i32) -> Option<usize> {
    for (index, &item) in arr.iter().enumerate() {
        if item == target {
            return Some(index);
        }
    }
    None
}

fn option_usage() {
    let numbers = [1, 2, 3, 4, 5];

    // Using match
    match find_element(&numbers, 3) {
        Some(index) => println!("Found at index {}", index),
        None => println!("Not found"),
    }

    // Using if let
    if let Some(index) = find_element(&numbers, 3) {
        println!("Found at index {}", index);
    }

    // Using unwrap (panics if None)
    let index = find_element(&numbers, 3).unwrap();

    // Using expect (panics with message)
    let index = find_element(&numbers, 3).expect("Element not found");

    // Using unwrap_or (default value)
    let index = find_element(&numbers, 99).unwrap_or(0);

    // Using unwrap_or_else (lazy default)
    let index = find_element(&numbers, 99).unwrap_or_else(|| {
        println!("Computing default...");
        0
    });
}
```

### 6.2 Result<T, E>

```rust
use std::fs::File;
use std::io::{self, Read};

// Functions that can fail return Result
fn read_file(path: &str) -> Result<String, io::Error> {
    let mut file = File::open(path)?;  // ? operator propagates error
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn result_usage() {
    // Using match
    match read_file("hello.txt") {
        Ok(contents) => println!("File contents: {}", contents),
        Err(e) => println!("Error reading file: {}", e),
    }

    // Using if let
    if let Ok(contents) = read_file("hello.txt") {
        println!("Contents: {}", contents);
    }

    // Using unwrap/expect
    let contents = read_file("hello.txt").unwrap();
    let contents = read_file("hello.txt").expect("Failed to read file");

    // Using unwrap_or_else
    let contents = read_file("hello.txt").unwrap_or_else(|e| {
        println!("Error: {}, using default", e);
        String::from("default content")
    });
}
```

### 6.3 Propagación de Errores con ?

```rust
use std::fs::File;
use std::io::{self, Read};

// Manual error propagation
fn read_username_verbose() -> Result<String, io::Error> {
    let f = File::open("username.txt");

    let mut f = match f {
        Ok(file) => file,
        Err(e) => return Err(e),
    };

    let mut s = String::new();

    match f.read_to_string(&mut s) {
        Ok(_) => Ok(s),
        Err(e) => Err(e),
    }
}

// Using ? operator
fn read_username_short() -> Result<String, io::Error> {
    let mut f = File::open("username.txt")?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    Ok(s)
}

// Chaining with ?
fn read_username_chain() -> Result<String, io::Error> {
    let mut s = String::new();
    File::open("username.txt")?.read_to_string(&mut s)?;
    Ok(s)
}

// Using fs::read_to_string
fn read_username_simple() -> Result<String, io::Error> {
    std::fs::read_to_string("username.txt")
}
```

### 6.4 Custom Error Types

```rust
use std::fmt;
use std::error::Error;

// Define custom error
#[derive(Debug)]
enum AppError {
    IoError(std::io::Error),
    ParseError(std::num::ParseIntError),
    ValidationError(String),
}

impl fmt::Display for AppError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AppError::IoError(e) => write!(f, "IO error: {}", e),
            AppError::ParseError(e) => write!(f, "Parse error: {}", e),
            AppError::ValidationError(msg) => write!(f, "Validation error: {}", msg),
        }
    }
}

impl Error for AppError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            AppError::IoError(e) => Some(e),
            AppError::ParseError(e) => Some(e),
            AppError::ValidationError(_) => None,
        }
    }
}

// Implement From for automatic conversion
impl From<std::io::Error> for AppError {
    fn from(err: std::io::Error) -> AppError {
        AppError::IoError(err)
    }
}

impl From<std::num::ParseIntError> for AppError {
    fn from(err: std::num::ParseIntError) -> AppError {
        AppError::ParseError(err)
    }
}

// Now ? works automatically
fn process_file(path: &str) -> Result<i32, AppError> {
    let contents = std::fs::read_to_string(path)?;  // IoError -> AppError
    let number: i32 = contents.trim().parse()?;     // ParseError -> AppError

    if number < 0 {
        return Err(AppError::ValidationError("Number must be positive".to_string()));
    }

    Ok(number)
}
```

### 6.5 Comparación con C

```c
// C: Error handling with return codes
#include <stdio.h>
#include <errno.h>

int read_number(const char* path, int* result) {
    FILE* f = fopen(path, "r");
    if (f == NULL) {
        return -1;  // Error code
    }

    if (fscanf(f, "%d", result) != 1) {
        fclose(f);
        return -2;  // Different error code
    }

    fclose(f);
    return 0;  // Success
}

int main() {
    int number;
    int err = read_number("number.txt", &number);

    if (err == -1) {
        perror("Failed to open file");
    } else if (err == -2) {
        fprintf(stderr, "Failed to parse number\n");
    } else {
        printf("Number: %d\n", number);
    }

    return 0;
}
```

```rust
// Rust: Type-safe error handling
use std::fs;
use std::num::ParseIntError;

fn read_number(path: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let contents = fs::read_to_string(path)?;
    let number: i32 = contents.trim().parse()?;
    Ok(number)
}

fn main() {
    match read_number("number.txt") {
        Ok(number) => println!("Number: {}", number),
        Err(e) => eprintln!("Error: {}", e),
    }
}
```

---

## 7. Traits y Generics

### 7.1 Definiendo Traits

```rust
// Define a trait (like interfaces in other languages)
trait Summary {
    fn summarize(&self) -> String;

    // Default implementation
    fn summarize_author(&self) -> String {
        String::from("(Anonymous)")
    }
}

struct NewsArticle {
    headline: String,
    location: String,
    author: String,
    content: String,
}

impl Summary for NewsArticle {
    fn summarize(&self) -> String {
        format!("{}, by {} ({})", self.headline, self.author, self.location)
    }
}

struct Tweet {
    username: String,
    content: String,
}

impl Summary for Tweet {
    fn summarize(&self) -> String {
        format!("{}: {}", self.username, self.content)
    }

    fn summarize_author(&self) -> String {
        format!("@{}", self.username)
    }
}

fn trait_usage() {
    let tweet = Tweet {
        username: String::from("rust_lang"),
        content: String::from("Hello, Rustaceans!"),
    };

    println!("{}", tweet.summarize());
    println!("Author: {}", tweet.summarize_author());
}
```

### 7.2 Trait Bounds

```rust
// Using trait as parameter
fn notify(item: &impl Summary) {
    println!("Breaking news! {}", item.summarize());
}

// Trait bound syntax (equivalent)
fn notify_verbose<T: Summary>(item: &T) {
    println!("Breaking news! {}", item.summarize());
}

// Multiple trait bounds
fn notify_multiple<T: Summary + std::fmt::Display>(item: &T) {
    println!("{}", item.summarize());
    println!("{}", item);  // Uses Display
}

// Where clause for cleaner syntax
fn some_function<T, U>(t: &T, u: &U) -> i32
where
    T: Summary + Clone,
    U: Clone + std::fmt::Debug,
{
    // Implementation
    0
}

// Returning types that implement traits
fn returns_summarizable() -> impl Summary {
    Tweet {
        username: String::from("horse_ebooks"),
        content: String::from("of course"),
    }
}
```

### 7.3 Generics

```rust
// Generic function
fn largest<T: PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];

    for item in list {
        if item > largest {
            largest = item;
        }
    }

    largest
}

// Generic struct
struct Point<T> {
    x: T,
    y: T,
}

// Generic struct with different types
struct MixedPoint<T, U> {
    x: T,
    y: U,
}

impl<T> Point<T> {
    fn x(&self) -> &T {
        &self.x
    }
}

// Implementation for specific type
impl Point<f32> {
    fn distance_from_origin(&self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}

// Generic enum
enum Result<T, E> {
    Ok(T),
    Err(E),
}

enum Option<T> {
    Some(T),
    None,
}

fn generics_usage() {
    let integer_point = Point { x: 5, y: 10 };
    let float_point = Point { x: 1.0, y: 4.0 };
    let mixed = MixedPoint { x: 5, y: 4.0 };

    println!("x = {}", integer_point.x());
    println!("Distance: {}", float_point.distance_from_origin());
}
```

### 7.4 Common Standard Library Traits

```rust
// Debug: formatted with {:?}
#[derive(Debug)]
struct DebugStruct {
    value: i32,
}

// Clone: explicit copy
#[derive(Clone)]
struct Cloneable {
    data: Vec<i32>,
}

// Copy: implicit copy (requires Clone)
#[derive(Copy, Clone)]
struct Copyable {
    x: i32,
    y: i32,
}

// PartialEq, Eq: equality comparison
#[derive(PartialEq, Eq)]
struct Comparable {
    id: i32,
}

// PartialOrd, Ord: ordering
#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Orderable {
    priority: i32,
}

// Default: default value
#[derive(Default)]
struct WithDefault {
    count: i32,     // Default: 0
    name: String,   // Default: ""
}

// Hash: can be used in HashMap keys
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Hash, PartialEq, Eq)]
struct Hashable {
    id: i32,
}

fn standard_traits() {
    let d = DebugStruct { value: 42 };
    println!("{:?}", d);

    let c = Cloneable { data: vec![1, 2, 3] };
    let c2 = c.clone();

    let default_struct = WithDefault::default();

    let mut map = HashMap::new();
    map.insert(Hashable { id: 1 }, "one");
}
```

---

## 8. Smart Pointers

### 8.1 Box<T>

```rust
// Box: heap allocation
fn box_basics() {
    // Allocate on heap
    let b = Box::new(5);
    println!("b = {}", b);

    // Useful for large data
    let large_array = Box::new([0u8; 1_000_000]);

    // Recursive types need Box
    #[derive(Debug)]
    enum List {
        Cons(i32, Box<List>),
        Nil,
    }

    use List::{Cons, Nil};

    let list = Cons(1, Box::new(Cons(2, Box::new(Cons(3, Box::new(Nil))))));
    println!("{:?}", list);
}

// Box for trait objects
trait Animal {
    fn speak(&self);
}

struct Dog;
struct Cat;

impl Animal for Dog {
    fn speak(&self) {
        println!("Woof!");
    }
}

impl Animal for Cat {
    fn speak(&self) {
        println!("Meow!");
    }
}

fn animals_example() {
    let animals: Vec<Box<dyn Animal>> = vec![
        Box::new(Dog),
        Box::new(Cat),
    ];

    for animal in animals {
        animal.speak();
    }
}
```

### 8.2 Rc<T> (Reference Counted)

```rust
use std::rc::Rc;

fn rc_basics() {
    // Rc allows multiple owners
    let a = Rc::new(5);
    println!("Reference count: {}", Rc::strong_count(&a));  // 1

    let b = Rc::clone(&a);  // Increment count
    println!("Reference count: {}", Rc::strong_count(&a));  // 2

    {
        let c = Rc::clone(&a);
        println!("Reference count: {}", Rc::strong_count(&a));  // 3
    }

    println!("Reference count: {}", Rc::strong_count(&a));  // 2
}

// Shared ownership example
#[derive(Debug)]
enum List {
    Cons(i32, Rc<List>),
    Nil,
}

fn shared_list() {
    use List::{Cons, Nil};

    let a = Rc::new(Cons(5, Rc::new(Cons(10, Rc::new(Nil)))));

    // b and c share ownership of a
    let b = Cons(3, Rc::clone(&a));
    let c = Cons(4, Rc::clone(&a));

    println!("a: {:?}", a);
    println!("b: {:?}", b);
    println!("c: {:?}", c);
}
```

### 8.3 Arc<T> (Atomic Reference Counted)

```rust
use std::sync::Arc;
use std::thread;

fn arc_threads() {
    let data = Arc::new(vec![1, 2, 3, 4, 5]);

    let mut handles = vec![];

    for i in 0..3 {
        let data_clone = Arc::clone(&data);

        let handle = thread::spawn(move || {
            println!("Thread {} sees: {:?}", i, data_clone);
        });

        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }
}
```

### 8.4 RefCell<T> y Interior Mutability

```rust
use std::cell::RefCell;

fn refcell_basics() {
    // RefCell allows mutable borrows checked at runtime
    let data = RefCell::new(5);

    // Borrow immutably
    let borrowed = data.borrow();
    println!("Value: {}", *borrowed);
    drop(borrowed);  // Must drop before mutable borrow

    // Borrow mutably
    *data.borrow_mut() += 1;
    println!("After mutation: {}", *data.borrow());
}

// Common pattern: Rc<RefCell<T>>
use std::rc::Rc;

#[derive(Debug)]
struct Node {
    value: i32,
    children: RefCell<Vec<Rc<Node>>>,
}

fn tree_example() {
    let leaf = Rc::new(Node {
        value: 3,
        children: RefCell::new(vec![]),
    });

    let branch = Rc::new(Node {
        value: 5,
        children: RefCell::new(vec![Rc::clone(&leaf)]),
    });

    // Modify children after creation
    branch.children.borrow_mut().push(Rc::new(Node {
        value: 7,
        children: RefCell::new(vec![]),
    }));

    println!("{:?}", branch);
}
```

### 8.5 Weak<T>

```rust
use std::rc::{Rc, Weak};
use std::cell::RefCell;

#[derive(Debug)]
struct TreeNode {
    value: i32,
    parent: RefCell<Weak<TreeNode>>,
    children: RefCell<Vec<Rc<TreeNode>>>,
}

fn weak_references() {
    let leaf = Rc::new(TreeNode {
        value: 3,
        parent: RefCell::new(Weak::new()),
        children: RefCell::new(vec![]),
    });

    println!("leaf parent = {:?}", leaf.parent.borrow().upgrade());

    let branch = Rc::new(TreeNode {
        value: 5,
        parent: RefCell::new(Weak::new()),
        children: RefCell::new(vec![Rc::clone(&leaf)]),
    });

    // Set leaf's parent to branch using Weak
    *leaf.parent.borrow_mut() = Rc::downgrade(&branch);

    println!("leaf parent = {:?}", leaf.parent.borrow().upgrade());
}
```

---

## 9. Comparación con Punteros C

### 9.1 Raw Pointers vs Safe References

```c
// C: Raw pointers
int main() {
    int x = 10;
    int* ptr = &x;          // Pointer to x
    int** ptr_ptr = &ptr;   // Pointer to pointer

    *ptr = 20;              // Dereference
    **ptr_ptr = 30;         // Double dereference

    int* null_ptr = NULL;   // Null pointer
    // *null_ptr = 5;       // UB: null pointer dereference

    int* dangling;          // Uninitialized pointer
    // *dangling = 5;       // UB: use of uninitialized pointer

    int* heap = malloc(sizeof(int));
    free(heap);
    // *heap = 5;           // UB: use after free

    return 0;
}
```

```rust
// Rust: Safe references with raw pointer escape hatch
fn pointer_comparison() {
    // Safe references (default)
    let x = 10;
    let r: &i32 = &x;           // Reference to x
    let rr: &&i32 = &r;         // Reference to reference

    println!("*r = {}", *r);
    println!("**rr = {}", **rr);

    // No null references - use Option<&T>
    let maybe_ref: Option<&i32> = Some(&x);

    // Raw pointers (require unsafe to dereference)
    let raw_ptr: *const i32 = &x;
    let raw_mut_ptr: *mut i32 = &x as *const i32 as *mut i32;

    unsafe {
        println!("Raw pointer value: {}", *raw_ptr);
    }
}
```

### 9.2 Tabla de Equivalencias

| Concepto C | Concepto Rust | Notas |
|------------|---------------|-------|
| `int* ptr` | `&i32` / `&mut i32` | Referencias seguras |
| `int* ptr = NULL` | `Option<&i32> = None` | Null explícito y seguro |
| `malloc(size)` | `Box::new()` / `Vec::with_capacity()` | Heap allocation |
| `free(ptr)` | Automático (Drop) | RAII pattern |
| `int** ptr` | `&&i32` / `Box<Box<i32>>` | Referencias anidadas |
| `const int*` | `&i32` | Inmutable por defecto |
| `int* const` | `let ptr = &x` | Variable inmutable |
| `void*` | `*const ()` o generics | Type-erased pointer |

### 9.3 Patrones de Migración

```c
// C: Linked list with raw pointers
struct Node {
    int data;
    struct Node* next;
};

struct Node* create_node(int data) {
    struct Node* node = malloc(sizeof(struct Node));
    if (node) {
        node->data = data;
        node->next = NULL;
    }
    return node;
}

void free_list(struct Node* head) {
    while (head) {
        struct Node* temp = head;
        head = head->next;
        free(temp);
    }
}
```

```rust
// Rust: Safe linked list
#[derive(Debug)]
struct Node {
    data: i32,
    next: Option<Box<Node>>,
}

impl Node {
    fn new(data: i32) -> Self {
        Node { data, next: None }
    }

    fn push(&mut self, data: i32) {
        let new_node = Box::new(Node {
            data,
            next: self.next.take(),
        });
        self.next = Some(new_node);
    }
}

fn linked_list_example() {
    let mut head = Node::new(1);
    head.push(2);
    head.push(3);

    println!("{:?}", head);
    // Automatic cleanup when head goes out of scope
}
```

---

## 10. Tablas de Referencia

### 10.1 Ownership Quick Reference

| Operación | Tipo | Ownership | Ejemplo |
|-----------|------|-----------|---------|
| Asignación | `Copy` | Copied | `let y = x;` (x still valid) |
| Asignación | No `Copy` | Moved | `let y = x;` (x invalid) |
| Referencia | `&T` | Borrowed | `let r = &x;` (x valid) |
| Referencia mut | `&mut T` | Mut borrowed | `let r = &mut x;` |
| Clone | `Clone` | Cloned | `let y = x.clone();` |

### 10.2 Smart Pointer Selection

| Tipo | Use Case | Thread Safe | Mutability |
|------|----------|-------------|------------|
| `Box<T>` | Single owner, heap | No | Normal |
| `Rc<T>` | Multiple owners, single thread | No | Inmutable |
| `Arc<T>` | Multiple owners, multi thread | Yes | Inmutable |
| `RefCell<T>` | Interior mutability | No | Runtime checked |
| `Mutex<T>` | Thread-safe mutability | Yes | Guarded |
| `RwLock<T>` | Read-write thread safe | Yes | Guarded |

### 10.3 Error Handling Cheat Sheet

| Método | Uso | Panics |
|--------|-----|--------|
| `unwrap()` | Get value or panic | Yes (on None/Err) |
| `expect(msg)` | Get value or panic with message | Yes |
| `unwrap_or(default)` | Get value or default | No |
| `unwrap_or_else(f)` | Get value or compute default | No |
| `?` | Propagate error | No (returns Err) |
| `ok()` | Result → Option | No |
| `err()` | Result → Option (error) | No |
| `map(f)` | Transform inner value | No |
| `and_then(f)` | Chain operations | No |

### 10.4 Lifetime Annotations Quick Reference

```rust
// Function signatures
fn f<'a>(x: &'a str) -> &'a str { x }
fn f<'a, 'b>(x: &'a str, y: &'b str) -> &'a str { x }
fn f<'a>(x: &'a str, y: &str) -> &'a str { x }

// Struct definitions
struct S<'a> { r: &'a str }
struct S<'a, 'b> { x: &'a str, y: &'b str }

// Impl blocks
impl<'a> S<'a> {
    fn method(&self) -> &str { self.r }
}

// Static lifetime
let s: &'static str = "lives forever";
static CONSTANT: &str = "also static";
```

---

## Referencias y Recursos Adicionales

1. **The Rust Programming Language Book**: https://doc.rust-lang.org/book/
2. **Rust by Example**: https://doc.rust-lang.org/rust-by-example/
3. **Rustonomicon** (unsafe Rust): https://doc.rust-lang.org/nomicon/
4. **Rust Reference**: https://doc.rust-lang.org/reference/

---

*Documento generado por ARCHAEON_CORE - Sistema de Documentación de Lenguajes*
*Especialización: Evolución C → Rust*
*Última actualización: 2025-12-31*

---
title: "GO_03_C_BRIDGE"
version: "1.0.0"
date: "2025-12-31"
author: "ARCHAEON_CORE"
domain: "SOUL_CORE/ARCHAEON/MODERNOS/GO"
type: "technical_documentation"
classification: "interoperability"
language: "Go"
paradigm: "systems_programming"
legacy_bridge: "C"
keywords:
  - cgo
  - ffi
  - c_interop
  - memory_management
  - linking
  - performance
dependencies:
  - go_1.21+
  - gcc
  - clang
related_docs:
  - GO_01_FUNDAMENTOS.md
  - GO_02_CONCURRENCIA.md
  - GO_04_SISTEMAS.md
---

# GO_03_C_BRIDGE

## Tabla de Contenidos

1. [Introducción a cgo](#introducción-a-cgo)
2. [Fundamentos de cgo](#fundamentos-de-cgo)
3. [Llamando a C desde Go](#llamando-a-c-desde-go)
4. [Pasando Datos entre Go y C](#pasando-datos-entre-go-y-c)
5. [Gestión de Memoria](#gestión-de-memoria)
6. [Enlazando Bibliotecas C](#enlazando-bibliotecas-c)
7. [Callbacks y Punteros a Funciones](#callbacks-y-punteros-a-funciones)
8. [Consideraciones de Rendimiento](#consideraciones-de-rendimiento)
9. [Mejores Prácticas](#mejores-prácticas)

---

## Introducción a cgo

cgo es la herramienta que permite a Go llamar código C y viceversa. Es esencial
para interoperar con bibliotecas del sistema, código legacy y optimizaciones
de bajo nivel.

### Cuándo Usar cgo

| Caso de Uso | Recomendación |
|-------------|---------------|
| Biblioteca C sin equivalente Go | Usar cgo |
| Código de rendimiento crítico | Evaluar primero Go puro |
| Drivers de hardware | Usar cgo |
| Código legacy a migrar | cgo como puente temporal |
| Bindings de bibliotecas | Usar cgo |

### Arquitectura de cgo

```
┌──────────────────────────────────────────────────────────────┐
│                      Go Application                          │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────────┐        ┌─────────────────────────────┐  │
│  │    Go Code      │        │        cgo Wrapper          │  │
│  │                 │───────>│  (auto-generated bindings)  │  │
│  └─────────────────┘        └─────────────┬───────────────┘  │
│                                           │                   │
│  ┌────────────────────────────────────────┼───────────────┐  │
│  │                    C Runtime           │               │  │
│  │  ┌─────────────────┐  ┌────────────────▼────────────┐  │  │
│  │  │   C Headers     │  │        C Code               │  │  │
│  │  │  (#include)     │  │    (implementation)         │  │  │
│  │  └─────────────────┘  └─────────────────────────────┘  │  │
│  └────────────────────────────────────────────────────────┘  │
│                                                               │
└──────────────────────────────────────────────────────────────┘
```

---

## Fundamentos de cgo

### Primer Programa cgo

```go
package main

/*
#include <stdio.h>
#include <stdlib.h>

// Inline C function
void sayHello(const char* name) {
    printf("Hello from C, %s!\n", name);
}

// C macro
#define MAX_SIZE 1024
*/
import "C"

import (
    "fmt"
    "unsafe"
)

func main() {
    // Call C function
    name := C.CString("Go Developer")
    defer C.free(unsafe.Pointer(name))

    C.sayHello(name)

    // Use C constant
    fmt.Printf("MAX_SIZE from C: %d\n", C.MAX_SIZE)
}
```

### Reglas de Sintaxis cgo

```go
package main

/*
IMPORTANT: There must be NO blank line between the closing comment
and the import "C" statement!

#cgo CFLAGS: -I/usr/local/include
#cgo LDFLAGS: -L/usr/local/lib -lmylib
#cgo linux LDFLAGS: -lrt
#cgo darwin LDFLAGS: -framework CoreFoundation
#cgo windows LDFLAGS: -lws2_32

#include <stdlib.h>
#include <string.h>
*/
import "C"

// cgo directives:
// #cgo CFLAGS:    C compiler flags
// #cgo CPPFLAGS:  C preprocessor flags
// #cgo CXXFLAGS:  C++ compiler flags
// #cgo LDFLAGS:   Linker flags
// #cgo pkg-config: package  (uses pkg-config)

// Build constraints can be combined:
// #cgo linux,amd64 LDFLAGS: -L/special/path

// Environment variables:
// CGO_ENABLED=1    Enable cgo (default when C compiler available)
// CC=gcc           C compiler
// CXX=g++          C++ compiler
// CGO_CFLAGS       Additional C flags
// CGO_LDFLAGS      Additional linker flags
```

### Tipos de Datos cgo

```go
package main

/*
#include <stdint.h>
#include <stdbool.h>

// C struct
typedef struct {
    int id;
    char name[64];
    double value;
    bool active;
} Record;

// C enum
typedef enum {
    STATUS_OK = 0,
    STATUS_ERROR = 1,
    STATUS_PENDING = 2
} Status;

// C union (access first member only in Go)
typedef union {
    int i;
    float f;
    char s[16];
} Data;
*/
import "C"

import (
    "fmt"
    "unsafe"
)

func main() {
    // Numeric type mappings
    var cInt C.int = 42
    var cLong C.long = 1000000
    var cFloat C.float = 3.14
    var cDouble C.double = 2.71828

    // Fixed-width integers
    var cInt32 C.int32_t = 100
    var cUint64 C.uint64_t = 18446744073709551615

    // Size types
    var cSize C.size_t = 1024
    var cSsize C.ssize_t = -1

    // Pointer types
    var cVoidPtr unsafe.Pointer
    var cCharPtr *C.char

    // Go to C type conversions
    goInt := int(cInt)
    goFloat := float64(cDouble)
    goInt32 := int32(cInt32)

    fmt.Printf("Go int: %d, float: %f, int32: %d\n", goInt, goFloat, goInt32)

    // Using C struct
    var record C.Record
    record.id = 1
    record.value = 99.99
    record.active = C.bool(true)

    // Copy string to C char array
    name := "TestRecord"
    cName := C.CString(name)
    defer C.free(unsafe.Pointer(cName))
    C.strncpy(&record.name[0], cName, 63)

    fmt.Printf("Record ID: %d, Name: %s\n", record.id, C.GoString(&record.name[0]))

    // Using C enum
    var status C.Status = C.STATUS_OK
    fmt.Printf("Status: %d\n", status)
}
```

---

## Llamando a C desde Go

### Funciones C Inline

```go
package main

/*
#include <stdio.h>
#include <math.h>

// Simple function
int add(int a, int b) {
    return a + b;
}

// Function with pointer parameter
void increment(int* value) {
    (*value)++;
}

// Function returning pointer (caller must free)
char* createMessage(const char* prefix, int num) {
    char* buffer = malloc(256);
    if (buffer != NULL) {
        snprintf(buffer, 256, "%s: %d", prefix, num);
    }
    return buffer;
}

// Variadic function wrapper (cgo doesn't support variadic directly)
void logMessage(const char* format, const char* arg1) {
    printf(format, arg1);
}

// Using math functions
double computeSqrt(double x) {
    return sqrt(x);
}
*/
import "C"

import (
    "fmt"
    "unsafe"
)

func main() {
    // Call simple function
    result := C.add(10, 20)
    fmt.Printf("add(10, 20) = %d\n", result)

    // Call function with pointer
    value := C.int(5)
    C.increment(&value)
    fmt.Printf("After increment: %d\n", value)

    // Call function returning pointer
    prefix := C.CString("Message")
    defer C.free(unsafe.Pointer(prefix))

    msg := C.createMessage(prefix, 42)
    if msg != nil {
        defer C.free(unsafe.Pointer(msg))
        fmt.Printf("Created message: %s\n", C.GoString(msg))
    }

    // Math function
    sqrt := C.computeSqrt(16.0)
    fmt.Printf("sqrt(16) = %f\n", sqrt)
}
```

### Funciones de Biblioteca Estándar C

```go
package main

/*
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>

// Wrapper for errno (thread-local in many implementations)
int getErrno() {
    return errno;
}

void setErrno(int e) {
    errno = e;
}
*/
import "C"

import (
    "fmt"
    "unsafe"
)

func main() {
    // String functions
    str1 := C.CString("Hello")
    str2 := C.CString("World")
    defer C.free(unsafe.Pointer(str1))
    defer C.free(unsafe.Pointer(str2))

    // strlen
    len := C.strlen(str1)
    fmt.Printf("strlen(\"Hello\") = %d\n", len)

    // strcmp
    cmp := C.strcmp(str1, str2)
    fmt.Printf("strcmp(\"Hello\", \"World\") = %d\n", cmp)

    // strcat (be careful with buffer sizes!)
    buffer := (*C.char)(C.malloc(256))
    defer C.free(unsafe.Pointer(buffer))
    C.strcpy(buffer, str1)
    C.strcat(buffer, C.CString(" "))
    C.strcat(buffer, str2)
    fmt.Printf("Concatenated: %s\n", C.GoString(buffer))

    // Memory functions
    data := C.malloc(100)
    defer C.free(data)

    C.memset(data, 0, 100)

    src := C.CString("Copy this")
    defer C.free(unsafe.Pointer(src))
    C.memcpy(data, unsafe.Pointer(src), C.strlen(src)+1)
    fmt.Printf("Copied: %s\n", C.GoString((*C.char)(data)))

    // Time functions
    var t C.time_t
    C.time(&t)
    fmt.Printf("Current time (Unix): %d\n", t)

    // Random numbers
    C.srand(C.uint(t))
    for i := 0; i < 5; i++ {
        r := C.rand()
        fmt.Printf("Random: %d\n", r)
    }

    // Environment variables
    home := C.getenv(C.CString("HOME"))
    if home != nil {
        fmt.Printf("HOME = %s\n", C.GoString(home))
    }

    // errno handling
    C.setErrno(0)
    // Simulate error...
    errno := C.getErrno()
    fmt.Printf("errno = %d\n", errno)
}
```

---

## Pasando Datos entre Go y C

### Strings

```go
package main

/*
#include <stdlib.h>
#include <string.h>

void processString(const char* input, char* output, size_t outSize) {
    // Convert to uppercase
    size_t len = strlen(input);
    for (size_t i = 0; i < len && i < outSize - 1; i++) {
        char c = input[i];
        if (c >= 'a' && c <= 'z') {
            output[i] = c - 32;
        } else {
            output[i] = c;
        }
    }
    output[len < outSize - 1 ? len : outSize - 1] = '\0';
}

// Return allocated string (caller frees)
char* duplicateString(const char* s) {
    size_t len = strlen(s);
    char* dup = malloc(len + 1);
    if (dup) {
        strcpy(dup, s);
    }
    return dup;
}
*/
import "C"

import (
    "fmt"
    "unsafe"
)

func main() {
    // Go string to C string
    goStr := "Hello, cgo!"
    cStr := C.CString(goStr)
    defer C.free(unsafe.Pointer(cStr))

    fmt.Printf("Go string: %s\n", goStr)
    fmt.Printf("C string: %s\n", C.GoString(cStr))

    // Process string in C
    output := (*C.char)(C.malloc(256))
    defer C.free(unsafe.Pointer(output))

    C.processString(cStr, output, 256)
    fmt.Printf("Processed: %s\n", C.GoString(output))

    // C.GoString vs C.GoStringN
    cStr2 := C.CString("Hello\x00World") // Contains null byte
    defer C.free(unsafe.Pointer(cStr2))

    // GoString stops at null byte
    fmt.Printf("GoString: %s (len=%d)\n", C.GoString(cStr2), len(C.GoString(cStr2)))

    // GoStringN reads exactly n bytes
    full := C.GoStringN(cStr2, 11)
    fmt.Printf("GoStringN: %q (len=%d)\n", full, len(full))

    // C.GoBytes for binary data
    binData := []byte{0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x00, 0x57, 0x6f, 0x72, 0x6c, 0x64}
    cBin := C.CBytes(binData)
    defer C.free(cBin)

    goBin := C.GoBytes(cBin, C.int(len(binData)))
    fmt.Printf("Binary: %v\n", goBin)
}
```

### Arrays y Slices

```go
package main

/*
#include <stdlib.h>
#include <string.h>

// Process array of integers
void doubleArray(int* arr, size_t len) {
    for (size_t i = 0; i < len; i++) {
        arr[i] *= 2;
    }
}

// Sum array
long sumArray(const int* arr, size_t len) {
    long sum = 0;
    for (size_t i = 0; i < len; i++) {
        sum += arr[i];
    }
    return sum;
}

// Create and fill array
int* createArray(size_t len) {
    int* arr = malloc(len * sizeof(int));
    if (arr) {
        for (size_t i = 0; i < len; i++) {
            arr[i] = (int)i;
        }
    }
    return arr;
}

// Multi-dimensional array
void transpose(const double* in, double* out, int rows, int cols) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            out[j * rows + i] = in[i * cols + j];
        }
    }
}
*/
import "C"

import (
    "fmt"
    "unsafe"
)

func main() {
    // Go slice to C array
    goSlice := []C.int{1, 2, 3, 4, 5}

    // Get pointer to first element
    ptr := &goSlice[0]

    // Sum array
    sum := C.sumArray(ptr, C.size_t(len(goSlice)))
    fmt.Printf("Sum: %d\n", sum)

    // Modify in place
    C.doubleArray(ptr, C.size_t(len(goSlice)))

    // Convert back to Go slice (view same memory)
    fmt.Printf("Doubled: %v\n", goSlice)

    // C array to Go slice
    cArray := C.createArray(10)
    if cArray != nil {
        defer C.free(unsafe.Pointer(cArray))

        // Create Go slice backed by C array (UNSAFE: must not outlive C memory)
        // Option 1: Copy to Go slice (SAFE)
        goFromC := make([]int32, 10)
        for i := 0; i < 10; i++ {
            goFromC[i] = int32(*(*C.int)(unsafe.Pointer(uintptr(unsafe.Pointer(cArray)) + uintptr(i)*unsafe.Sizeof(C.int(0)))))
        }
        fmt.Printf("From C (copied): %v\n", goFromC)

        // Option 2: Slice header pointing to C memory (UNSAFE)
        // Only valid while C memory is valid!
        sliceHeader := struct {
            Data uintptr
            Len  int
            Cap  int
        }{uintptr(unsafe.Pointer(cArray)), 10, 10}

        unsafeSlice := *(*[]C.int)(unsafe.Pointer(&sliceHeader))
        fmt.Printf("From C (unsafe view): %v\n", unsafeSlice)
    }

    // 2D array / matrix
    matrix := []C.double{
        1, 2, 3,
        4, 5, 6,
    }
    transposed := make([]C.double, 6)

    C.transpose(&matrix[0], &transposed[0], 2, 3)
    fmt.Printf("Transposed: %v\n", transposed)
}
```

### Structs

```go
package main

/*
#include <stdlib.h>
#include <string.h>

typedef struct {
    int id;
    char name[64];
    double values[10];
    int count;
} DataRecord;

typedef struct {
    DataRecord* records;
    size_t length;
    size_t capacity;
} DataList;

// Initialize record
void initRecord(DataRecord* r, int id, const char* name) {
    r->id = id;
    strncpy(r->name, name, 63);
    r->name[63] = '\0';
    r->count = 0;
}

// Add value to record
void addValue(DataRecord* r, double val) {
    if (r->count < 10) {
        r->values[r->count++] = val;
    }
}

// Get sum of values
double sumValues(const DataRecord* r) {
    double sum = 0;
    for (int i = 0; i < r->count; i++) {
        sum += r->values[i];
    }
    return sum;
}

// Create data list
DataList* createList(size_t capacity) {
    DataList* list = malloc(sizeof(DataList));
    if (list) {
        list->records = malloc(capacity * sizeof(DataRecord));
        list->length = 0;
        list->capacity = capacity;
    }
    return list;
}

// Free data list
void freeList(DataList* list) {
    if (list) {
        free(list->records);
        free(list);
    }
}
*/
import "C"

import (
    "fmt"
    "unsafe"
)

// Go wrapper type
type DataRecord struct {
    cRecord *C.DataRecord
}

func NewDataRecord(id int, name string) *DataRecord {
    cRecord := (*C.DataRecord)(C.malloc(C.sizeof_DataRecord))
    cName := C.CString(name)
    defer C.free(unsafe.Pointer(cName))

    C.initRecord(cRecord, C.int(id), cName)

    return &DataRecord{cRecord: cRecord}
}

func (r *DataRecord) Free() {
    if r.cRecord != nil {
        C.free(unsafe.Pointer(r.cRecord))
        r.cRecord = nil
    }
}

func (r *DataRecord) AddValue(val float64) {
    C.addValue(r.cRecord, C.double(val))
}

func (r *DataRecord) Sum() float64 {
    return float64(C.sumValues(r.cRecord))
}

func (r *DataRecord) ID() int {
    return int(r.cRecord.id)
}

func (r *DataRecord) Name() string {
    return C.GoString(&r.cRecord.name[0])
}

func main() {
    // Using wrapped struct
    record := NewDataRecord(1, "TestRecord")
    defer record.Free()

    record.AddValue(10.5)
    record.AddValue(20.3)
    record.AddValue(30.2)

    fmt.Printf("Record %d (%s): sum = %.2f\n",
        record.ID(), record.Name(), record.Sum())

    // Direct struct manipulation
    var directRecord C.DataRecord
    directRecord.id = 2
    cName := C.CString("DirectRecord")
    C.strncpy(&directRecord.name[0], cName, 63)
    C.free(unsafe.Pointer(cName))

    directRecord.values[0] = 1.1
    directRecord.values[1] = 2.2
    directRecord.count = 2

    fmt.Printf("Direct Record %d: sum = %.2f\n",
        directRecord.id, C.sumValues(&directRecord))

    // Using data list
    list := C.createList(10)
    if list != nil {
        defer C.freeList(list)

        // Access records array
        // recordsSlice would point to C memory
        fmt.Printf("List capacity: %d\n", list.capacity)
    }
}
```

---

## Gestión de Memoria

### Reglas Fundamentales

```go
package main

/*
#include <stdlib.h>
#include <string.h>

// Returns pointer to heap memory (caller must free)
char* allocString(const char* input) {
    size_t len = strlen(input);
    char* buf = malloc(len + 1);
    if (buf) {
        strcpy(buf, input);
    }
    return buf;
}

// Fills caller-provided buffer (no allocation)
void fillBuffer(char* buf, size_t size, const char* content) {
    strncpy(buf, content, size - 1);
    buf[size - 1] = '\0';
}

// Structure with internal allocation
typedef struct {
    char* data;
    size_t length;
} Buffer;

Buffer* createBuffer(size_t size) {
    Buffer* buf = malloc(sizeof(Buffer));
    if (buf) {
        buf->data = malloc(size);
        buf->length = size;
    }
    return buf;
}

void freeBuffer(Buffer* buf) {
    if (buf) {
        free(buf->data);
        free(buf);
    }
}
*/
import "C"

import (
    "fmt"
    "runtime"
    "unsafe"
)

// Rule 1: C memory allocated with malloc must be freed with C.free
func rule1_AllocatedMemoryMustBeFreed() {
    cStr := C.CString("Hello") // Allocates C memory
    defer C.free(unsafe.Pointer(cStr)) // MUST free

    // Use the string...
    fmt.Println(C.GoString(cStr))
}

// Rule 2: Go pointers can be passed to C only during call
func rule2_GoPointersTemporary() {
    goSlice := make([]byte, 100)

    // OK: Go pointer valid during C call
    buf := (*C.char)(unsafe.Pointer(&goSlice[0]))
    C.fillBuffer(buf, 100, C.CString("Hello"))
    // WARNING: C.CString above leaks! Should be:
    cContent := C.CString("Hello")
    defer C.free(unsafe.Pointer(cContent))
    C.fillBuffer(buf, 100, cContent)

    fmt.Println(string(goSlice[:5]))
}

// Rule 3: Don't store Go pointers in C memory
func rule3_DontStoreGoPointers() {
    // BAD: Storing Go pointer in C struct
    // The Go GC may move the data, invalidating the pointer

    // GOOD: Copy data to C memory
    goData := []byte("Important data")
    cData := C.CBytes(goData)
    defer C.free(cData)

    // Use cData in C structures...
}

// Rule 4: Use finalizers for cleanup (with caution)
type ManagedBuffer struct {
    cBuffer *C.Buffer
}

func NewManagedBuffer(size int) *ManagedBuffer {
    mb := &ManagedBuffer{
        cBuffer: C.createBuffer(C.size_t(size)),
    }

    // Set finalizer to clean up if object is GC'd
    runtime.SetFinalizer(mb, func(m *ManagedBuffer) {
        m.Free()
    })

    return mb
}

func (m *ManagedBuffer) Free() {
    if m.cBuffer != nil {
        C.freeBuffer(m.cBuffer)
        m.cBuffer = nil
    }
}

// Rule 5: Be careful with C.CString in loops
func rule5_CStringInLoops() {
    words := []string{"hello", "world", "test"}

    // BAD: Memory leak in loop
    // for _, word := range words {
    //     cWord := C.CString(word)
    //     // Do something with cWord
    //     // LEAK: cWord never freed
    // }

    // GOOD: Free inside loop
    for _, word := range words {
        cWord := C.CString(word)
        // Do something with cWord
        C.free(unsafe.Pointer(cWord))
    }

    // ALTERNATIVE: Pre-allocate and reuse
    buf := (*C.char)(C.malloc(256))
    defer C.free(unsafe.Pointer(buf))

    for _, word := range words {
        cWord := C.CString(word)
        C.strncpy(buf, cWord, 255)
        C.free(unsafe.Pointer(cWord))
        // Use buf...
    }
}

func main() {
    rule1_AllocatedMemoryMustBeFreed()
    rule2_GoPointersTemporary()
    rule3_DontStoreGoPointers()

    // Managed buffer with automatic cleanup
    buf := NewManagedBuffer(1024)
    fmt.Printf("Buffer created with size: %d\n", buf.cBuffer.length)
    buf.Free() // Explicit cleanup is better than relying on GC

    rule5_CStringInLoops()
}
```

### Patrón de Wrapper Seguro

```go
package main

/*
#include <stdlib.h>
#include <string.h>

typedef struct {
    void* data;
    size_t size;
    int type;
} Resource;

Resource* createResource(int type, size_t size) {
    Resource* r = malloc(sizeof(Resource));
    if (r) {
        r->data = malloc(size);
        r->size = size;
        r->type = type;
        memset(r->data, 0, size);
    }
    return r;
}

void freeResource(Resource* r) {
    if (r) {
        if (r->data) {
            free(r->data);
        }
        free(r);
    }
}

int writeToResource(Resource* r, const void* data, size_t len) {
    if (!r || !r->data || len > r->size) {
        return -1;
    }
    memcpy(r->data, data, len);
    return 0;
}

int readFromResource(const Resource* r, void* buf, size_t len) {
    if (!r || !r->data || len > r->size) {
        return -1;
    }
    memcpy(buf, r->data, len);
    return 0;
}
*/
import "C"

import (
    "errors"
    "fmt"
    "sync"
    "unsafe"
)

// Thread-safe resource wrapper
type SafeResource struct {
    mu       sync.RWMutex
    cRes     *C.Resource
    closed   bool
}

var ErrClosed = errors.New("resource is closed")
var ErrNilResource = errors.New("nil resource")

func NewSafeResource(resourceType int, size int) (*SafeResource, error) {
    cRes := C.createResource(C.int(resourceType), C.size_t(size))
    if cRes == nil {
        return nil, errors.New("failed to create resource")
    }

    return &SafeResource{
        cRes: cRes,
    }, nil
}

func (r *SafeResource) Write(data []byte) error {
    r.mu.Lock()
    defer r.mu.Unlock()

    if r.closed {
        return ErrClosed
    }
    if r.cRes == nil {
        return ErrNilResource
    }

    result := C.writeToResource(r.cRes, unsafe.Pointer(&data[0]), C.size_t(len(data)))
    if result != 0 {
        return errors.New("write failed")
    }

    return nil
}

func (r *SafeResource) Read(size int) ([]byte, error) {
    r.mu.RLock()
    defer r.mu.RUnlock()

    if r.closed {
        return nil, ErrClosed
    }
    if r.cRes == nil {
        return nil, ErrNilResource
    }

    buf := make([]byte, size)
    result := C.readFromResource(r.cRes, unsafe.Pointer(&buf[0]), C.size_t(size))
    if result != 0 {
        return nil, errors.New("read failed")
    }

    return buf, nil
}

func (r *SafeResource) Close() error {
    r.mu.Lock()
    defer r.mu.Unlock()

    if r.closed {
        return nil
    }

    if r.cRes != nil {
        C.freeResource(r.cRes)
        r.cRes = nil
    }
    r.closed = true

    return nil
}

func (r *SafeResource) Size() int {
    r.mu.RLock()
    defer r.mu.RUnlock()

    if r.cRes == nil {
        return 0
    }
    return int(r.cRes.size)
}

func main() {
    res, err := NewSafeResource(1, 1024)
    if err != nil {
        fmt.Println("Error:", err)
        return
    }
    defer res.Close()

    // Write data
    err = res.Write([]byte("Hello, Safe Resource!"))
    if err != nil {
        fmt.Println("Write error:", err)
        return
    }

    // Read data
    data, err := res.Read(21)
    if err != nil {
        fmt.Println("Read error:", err)
        return
    }

    fmt.Printf("Read: %s\n", string(data))
    fmt.Printf("Resource size: %d\n", res.Size())
}
```

---

## Enlazando Bibliotecas C

### Biblioteca Estática

```go
// mathlib.h
#ifndef MATHLIB_H
#define MATHLIB_H

double power(double base, int exp);
double factorial(int n);
int fibonacci(int n);

#endif

// mathlib.c
#include "mathlib.h"

double power(double base, int exp) {
    double result = 1.0;
    int absExp = exp < 0 ? -exp : exp;
    for (int i = 0; i < absExp; i++) {
        result *= base;
    }
    return exp < 0 ? 1.0 / result : result;
}

double factorial(int n) {
    if (n <= 1) return 1.0;
    double result = 1.0;
    for (int i = 2; i <= n; i++) {
        result *= i;
    }
    return result;
}

int fibonacci(int n) {
    if (n <= 1) return n;
    int a = 0, b = 1;
    for (int i = 2; i <= n; i++) {
        int temp = a + b;
        a = b;
        b = temp;
    }
    return b;
}
```

```bash
# Compile static library
gcc -c mathlib.c -o mathlib.o
ar rcs libmathlib.a mathlib.o
```

```go
package main

/*
#cgo CFLAGS: -I${SRCDIR}/include
#cgo LDFLAGS: -L${SRCDIR}/lib -lmathlib

#include "mathlib.h"
*/
import "C"

import "fmt"

func main() {
    // Use static library functions
    result := C.power(2.0, 10)
    fmt.Printf("2^10 = %.0f\n", result)

    fact := C.factorial(10)
    fmt.Printf("10! = %.0f\n", fact)

    fib := C.fibonacci(20)
    fmt.Printf("fib(20) = %d\n", fib)
}
```

### Biblioteca Dinámica

```go
package main

/*
#cgo LDFLAGS: -ldl

#include <dlfcn.h>
#include <stdlib.h>

// Function pointer types
typedef double (*power_func)(double, int);
typedef int (*fibonacci_func)(int);

// Load library dynamically
void* loadLibrary(const char* path) {
    return dlopen(path, RTLD_LAZY);
}

void closeLibrary(void* handle) {
    if (handle) dlclose(handle);
}

const char* getError() {
    return dlerror();
}

// Get function from library
void* getSymbol(void* handle, const char* name) {
    return dlsym(handle, name);
}

// Call power function
double callPower(void* func, double base, int exp) {
    power_func pf = (power_func)func;
    return pf(base, exp);
}

// Call fibonacci function
int callFibonacci(void* func, int n) {
    fibonacci_func ff = (fibonacci_func)func;
    return ff(n);
}
*/
import "C"

import (
    "fmt"
    "unsafe"
)

type Library struct {
    handle unsafe.Pointer
}

func LoadLibrary(path string) (*Library, error) {
    cPath := C.CString(path)
    defer C.free(unsafe.Pointer(cPath))

    handle := C.loadLibrary(cPath)
    if handle == nil {
        errMsg := C.getError()
        return nil, fmt.Errorf("failed to load library: %s", C.GoString(errMsg))
    }

    return &Library{handle: handle}, nil
}

func (l *Library) Close() {
    if l.handle != nil {
        C.closeLibrary(l.handle)
        l.handle = nil
    }
}

func (l *Library) GetSymbol(name string) (unsafe.Pointer, error) {
    cName := C.CString(name)
    defer C.free(unsafe.Pointer(cName))

    sym := C.getSymbol(l.handle, cName)
    if sym == nil {
        errMsg := C.getError()
        return nil, fmt.Errorf("symbol not found: %s", C.GoString(errMsg))
    }

    return sym, nil
}

func (l *Library) CallPower(base float64, exp int) (float64, error) {
    sym, err := l.GetSymbol("power")
    if err != nil {
        return 0, err
    }

    result := C.callPower(sym, C.double(base), C.int(exp))
    return float64(result), nil
}

func (l *Library) CallFibonacci(n int) (int, error) {
    sym, err := l.GetSymbol("fibonacci")
    if err != nil {
        return 0, err
    }

    result := C.callFibonacci(sym, C.int(n))
    return int(result), nil
}

func main() {
    lib, err := LoadLibrary("./libmathlib.so")
    if err != nil {
        fmt.Println("Error:", err)
        return
    }
    defer lib.Close()

    result, err := lib.CallPower(2.0, 10)
    if err != nil {
        fmt.Println("Power error:", err)
    } else {
        fmt.Printf("2^10 = %.0f\n", result)
    }

    fib, err := lib.CallFibonacci(20)
    if err != nil {
        fmt.Println("Fibonacci error:", err)
    } else {
        fmt.Printf("fib(20) = %d\n", fib)
    }
}
```

### Usando pkg-config

```go
package main

/*
#cgo pkg-config: openssl libcurl

#include <openssl/sha.h>
#include <curl/curl.h>
#include <stdlib.h>
#include <string.h>

// SHA256 hash
void computeSHA256(const unsigned char* data, size_t len, unsigned char* hash) {
    SHA256(data, len, hash);
}
*/
import "C"

import (
    "encoding/hex"
    "fmt"
    "unsafe"
)

func SHA256(data []byte) string {
    hash := make([]byte, 32)

    C.computeSHA256(
        (*C.uchar)(unsafe.Pointer(&data[0])),
        C.size_t(len(data)),
        (*C.uchar)(unsafe.Pointer(&hash[0])),
    )

    return hex.EncodeToString(hash)
}

func main() {
    data := []byte("Hello, World!")
    hash := SHA256(data)
    fmt.Printf("SHA256: %s\n", hash)
}
```

---

## Callbacks y Punteros a Funciones

### Callbacks de Go desde C

```go
package main

/*
#include <stdlib.h>

// Callback type
typedef int (*callback_t)(int, int);
typedef void (*event_callback_t)(const char*, void*);

// Store callback globally (simplified example)
static callback_t storedCallback = NULL;
static event_callback_t storedEventCallback = NULL;
static void* storedUserData = NULL;

// Register callbacks
void registerCallback(callback_t cb) {
    storedCallback = cb;
}

void registerEventCallback(event_callback_t cb, void* userData) {
    storedEventCallback = cb;
    storedUserData = userData;
}

// Invoke callbacks
int invokeCallback(int a, int b) {
    if (storedCallback) {
        return storedCallback(a, b);
    }
    return -1;
}

void invokeEventCallback(const char* event) {
    if (storedEventCallback) {
        storedEventCallback(event, storedUserData);
    }
}

// External callback declarations (implemented in Go)
extern int goCallback(int a, int b);
extern void goEventCallback(const char* event, void* userData);
*/
import "C"

import (
    "fmt"
    "unsafe"
)

// Go function that will be called from C
//export goCallback
func goCallback(a, b C.int) C.int {
    fmt.Printf("goCallback called with: %d, %d\n", a, b)
    return a + b
}

// Event callback with user data
//export goEventCallback
func goEventCallback(event *C.char, userData unsafe.Pointer) {
    eventStr := C.GoString(event)
    // Recover Go data from pointer
    if userData != nil {
        id := *(*int)(userData)
        fmt.Printf("Event: %s (handler ID: %d)\n", eventStr, id)
    } else {
        fmt.Printf("Event: %s\n", eventStr)
    }
}

func main() {
    // Register simple callback
    C.registerCallback(C.callback_t(C.goCallback))

    // Invoke through C
    result := C.invokeCallback(10, 20)
    fmt.Printf("Result: %d\n", result)

    // Register event callback with user data
    handlerID := 42
    C.registerEventCallback(
        C.event_callback_t(C.goEventCallback),
        unsafe.Pointer(&handlerID),
    )

    // Trigger events
    event1 := C.CString("button_click")
    event2 := C.CString("data_loaded")
    defer C.free(unsafe.Pointer(event1))
    defer C.free(unsafe.Pointer(event2))

    C.invokeEventCallback(event1)
    C.invokeEventCallback(event2)
}
```

### Sistema de Eventos con Callbacks

```go
package main

/*
#include <stdlib.h>
#include <string.h>

#define MAX_HANDLERS 10

typedef struct {
    int id;
    void (*handler)(int eventType, const char* data, void* userData);
    void* userData;
    int active;
} EventHandler;

static EventHandler handlers[MAX_HANDLERS];
static int nextHandlerId = 1;

// Register handler
int registerHandler(void (*handler)(int, const char*, void*), void* userData) {
    for (int i = 0; i < MAX_HANDLERS; i++) {
        if (!handlers[i].active) {
            handlers[i].id = nextHandlerId++;
            handlers[i].handler = handler;
            handlers[i].userData = userData;
            handlers[i].active = 1;
            return handlers[i].id;
        }
    }
    return -1; // No space
}

// Unregister handler
void unregisterHandler(int id) {
    for (int i = 0; i < MAX_HANDLERS; i++) {
        if (handlers[i].active && handlers[i].id == id) {
            handlers[i].active = 0;
            handlers[i].handler = NULL;
            handlers[i].userData = NULL;
            break;
        }
    }
}

// Emit event to all handlers
void emitEvent(int eventType, const char* data) {
    for (int i = 0; i < MAX_HANDLERS; i++) {
        if (handlers[i].active && handlers[i].handler) {
            handlers[i].handler(eventType, data, handlers[i].userData);
        }
    }
}

// External Go handler
extern void goEventHandler(int eventType, const char* data, void* userData);
*/
import "C"

import (
    "fmt"
    "sync"
    "unsafe"
)

// Event types
const (
    EventTypeData   = 1
    EventTypeError  = 2
    EventTypeStatus = 3
)

// Handler context stored on Go side
type HandlerContext struct {
    Name     string
    Callback func(eventType int, data string)
}

var (
    contextMu   sync.RWMutex
    contextMap  = make(map[int]*HandlerContext)
    nextContext = 1
)

//export goEventHandler
func goEventHandler(eventType C.int, data *C.char, userData unsafe.Pointer) {
    ctxID := *(*int)(userData)

    contextMu.RLock()
    ctx, ok := contextMap[ctxID]
    contextMu.RUnlock()

    if ok && ctx.Callback != nil {
        ctx.Callback(int(eventType), C.GoString(data))
    }
}

// Register a Go handler
func RegisterHandler(name string, callback func(int, string)) int {
    // Create context
    contextMu.Lock()
    ctxID := nextContext
    nextContext++
    contextMap[ctxID] = &HandlerContext{
        Name:     name,
        Callback: callback,
    }
    contextMu.Unlock()

    // Store context ID pointer
    idPtr := new(int)
    *idPtr = ctxID

    // Register with C
    handlerID := C.registerHandler(
        C.goEventHandler,
        unsafe.Pointer(idPtr),
    )

    if handlerID == -1 {
        contextMu.Lock()
        delete(contextMap, ctxID)
        contextMu.Unlock()
        return -1
    }

    return int(handlerID)
}

func UnregisterHandler(id int) {
    C.unregisterHandler(C.int(id))
}

func EmitEvent(eventType int, data string) {
    cData := C.CString(data)
    defer C.free(unsafe.Pointer(cData))

    C.emitEvent(C.int(eventType), cData)
}

func main() {
    // Register handlers
    id1 := RegisterHandler("Logger", func(et int, data string) {
        fmt.Printf("[Logger] Event %d: %s\n", et, data)
    })

    id2 := RegisterHandler("Metrics", func(et int, data string) {
        fmt.Printf("[Metrics] Recording event type %d\n", et)
    })

    // Emit events
    EmitEvent(EventTypeData, "User logged in")
    EmitEvent(EventTypeStatus, "System healthy")
    EmitEvent(EventTypeError, "Connection failed")

    // Unregister one handler
    UnregisterHandler(id1)

    fmt.Println("\nAfter unregistering Logger:")
    EmitEvent(EventTypeData, "Another event")

    UnregisterHandler(id2)
}
```

---

## Consideraciones de Rendimiento

### Overhead de cgo

```go
package main

/*
#include <stdlib.h>

int simpleAdd(int a, int b) {
    return a + b;
}

// More complex operation
long sumArray(const int* arr, size_t len) {
    long sum = 0;
    for (size_t i = 0; i < len; i++) {
        sum += arr[i];
    }
    return sum;
}
*/
import "C"

import (
    "fmt"
    "testing"
    "unsafe"
)

// Pure Go implementations
func goAdd(a, b int) int {
    return a + b
}

func goSumArray(arr []int) int64 {
    var sum int64
    for _, v := range arr {
        sum += int64(v)
    }
    return sum
}

// Benchmark simple operation
func BenchmarkCgoAdd(b *testing.B) {
    for i := 0; i < b.N; i++ {
        C.simpleAdd(10, 20)
    }
}

func BenchmarkGoAdd(b *testing.B) {
    for i := 0; i < b.N; i++ {
        goAdd(10, 20)
    }
}

// Benchmark array sum
func BenchmarkCgoSumArray(b *testing.B) {
    arr := make([]C.int, 1000)
    for i := range arr {
        arr[i] = C.int(i)
    }

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        C.sumArray(&arr[0], C.size_t(len(arr)))
    }
}

func BenchmarkGoSumArray(b *testing.B) {
    arr := make([]int, 1000)
    for i := range arr {
        arr[i] = i
    }

    b.ResetTimer()
    for i := 0; i < b.N; i++ {
        goSumArray(arr)
    }
}

/*
Typical results:
BenchmarkCgoAdd-8           50000000    25 ns/op
BenchmarkGoAdd-8          2000000000     0.3 ns/op
BenchmarkCgoSumArray-8       500000   2500 ns/op
BenchmarkGoSumArray-8       1000000   1000 ns/op

Key observations:
- cgo call overhead is ~50-100ns per call
- For simple operations, Go is much faster
- For complex operations, C may be faster but cgo overhead matters
- Minimize cgo calls, batch operations when possible
*/

func main() {
    // Demonstrate the overhead
    fmt.Println("cgo overhead demonstration")
    fmt.Println("Run benchmarks with: go test -bench=.")
}
```

### Optimización de Llamadas cgo

```go
package main

/*
#include <stdlib.h>
#include <string.h>

// Process multiple items in one call (batching)
typedef struct {
    int id;
    double value;
} Item;

void processItemsBatch(Item* items, size_t count, double* results) {
    for (size_t i = 0; i < count; i++) {
        results[i] = items[i].value * 2.0 + items[i].id;
    }
}

// Single item version (for comparison)
double processItem(int id, double value) {
    return value * 2.0 + id;
}
*/
import "C"

import (
    "fmt"
    "unsafe"
)

type Item struct {
    ID    int
    Value float64
}

// Inefficient: one cgo call per item
func processItemsOneByOne(items []Item) []float64 {
    results := make([]float64, len(items))
    for i, item := range items {
        results[i] = float64(C.processItem(C.int(item.ID), C.double(item.Value)))
    }
    return results
}

// Efficient: batch processing in one cgo call
func processItemsBatch(items []Item) []float64 {
    if len(items) == 0 {
        return nil
    }

    // Convert to C struct array
    cItems := make([]C.Item, len(items))
    for i, item := range items {
        cItems[i].id = C.int(item.ID)
        cItems[i].value = C.double(item.Value)
    }

    // Allocate results
    results := make([]C.double, len(items))

    // Single cgo call
    C.processItemsBatch(&cItems[0], C.size_t(len(items)), &results[0])

    // Convert results
    goResults := make([]float64, len(items))
    for i, r := range results {
        goResults[i] = float64(r)
    }

    return goResults
}

// Strategy: Pre-allocate reusable buffers
type ItemProcessor struct {
    cItems  []C.Item
    results []C.double
    cap     int
}

func NewItemProcessor(capacity int) *ItemProcessor {
    return &ItemProcessor{
        cItems:  make([]C.Item, capacity),
        results: make([]C.double, capacity),
        cap:     capacity,
    }
}

func (p *ItemProcessor) Process(items []Item) []float64 {
    if len(items) > p.cap {
        // Grow buffers if needed
        p.cItems = make([]C.Item, len(items))
        p.results = make([]C.double, len(items))
        p.cap = len(items)
    }

    for i, item := range items {
        p.cItems[i].id = C.int(item.ID)
        p.cItems[i].value = C.double(item.Value)
    }

    C.processItemsBatch(&p.cItems[0], C.size_t(len(items)), &p.results[0])

    goResults := make([]float64, len(items))
    for i := range items {
        goResults[i] = float64(p.results[i])
    }

    return goResults
}

func main() {
    // Create test data
    items := make([]Item, 1000)
    for i := range items {
        items[i] = Item{ID: i, Value: float64(i) * 1.5}
    }

    // Compare approaches
    results1 := processItemsOneByOne(items)
    results2 := processItemsBatch(items)

    fmt.Printf("One-by-one first 5: %v\n", results1[:5])
    fmt.Printf("Batch first 5: %v\n", results2[:5])

    // With reusable processor
    processor := NewItemProcessor(1000)
    results3 := processor.Process(items)
    fmt.Printf("Reusable first 5: %v\n", results3[:5])
}
```

---

## Mejores Prácticas

### Lista de Verificación

```go
package main

/*
BEST PRACTICES CHECKLIST:

1. MEMORY MANAGEMENT
   [x] Always free C.CString with C.free
   [x] Always free C.CBytes with C.free
   [x] Always free C-allocated memory before Go objects go out of scope
   [x] Use defer for cleanup when possible
   [x] Consider using runtime.SetFinalizer for long-lived objects

2. THREAD SAFETY
   [x] Lock goroutine to OS thread if C code uses thread-local storage
       runtime.LockOSThread() / runtime.UnlockOSThread()
   [x] Don't hold Go mutexes while calling C code
   [x] Be aware that C code may create threads

3. POINTER RULES
   [x] Don't store Go pointers in C memory
   [x] Go pointers passed to C are only valid during the call
   [x] Pass data by copying to C-allocated memory for persistence
   [x] Use cgo.Handle for passing Go objects to C (Go 1.17+)

4. PERFORMANCE
   [x] Minimize cgo calls - batch operations when possible
   [x] Prefer pure Go for simple operations
   [x] Pre-allocate buffers for repeated operations
   [x] Profile with go test -bench and pprof

5. BUILD & DEPLOYMENT
   [x] Document C dependencies clearly
   [x] Use pkg-config when available
   [x] Consider static linking for deployment
   [x] Test on all target platforms

6. ERROR HANDLING
   [x] Check C function return values
   [x] Handle errno properly
   [x] Convert C error codes to Go errors

7. TESTING
   [x] Test with -race flag
   [x] Test memory leaks with valgrind or AddressSanitizer
   [x] Mock C dependencies when possible
*/

// Example: Using cgo.Handle (Go 1.17+)
import "runtime/cgo"

type MyObject struct {
    Data string
}

//export processObject
func processObject(h C.uintptr_t) {
    handle := cgo.Handle(h)
    obj := handle.Value().(*MyObject)
    fmt.Println("Processing:", obj.Data)
}

func main() {
    obj := &MyObject{Data: "Hello"}
    handle := cgo.NewHandle(obj)
    defer handle.Delete()

    // Pass handle to C
    C.callWithHandle(C.uintptr_t(handle))
}
```

### Estructura de Proyecto Recomendada

```
myproject/
├── go.mod
├── main.go
├── cgo/
│   ├── wrapper.go          # cgo wrapper code
│   ├── wrapper_test.go     # tests
│   └── include/            # C headers
│       └── mylib.h
├── lib/
│   ├── libmylib.a         # static library
│   └── libmylib.so        # shared library
├── internal/
│   └── native/            # Pure Go fallback implementations
│       └── fallback.go
└── Makefile               # Build C dependencies
```

```makefile
# Makefile for C dependencies
CC = gcc
CFLAGS = -Wall -O2 -fPIC
LDFLAGS = -shared

LIB_SRC = lib/mylib.c
LIB_OBJ = lib/mylib.o
STATIC_LIB = lib/libmylib.a
SHARED_LIB = lib/libmylib.so

all: $(STATIC_LIB) $(SHARED_LIB)

$(STATIC_LIB): $(LIB_OBJ)
	ar rcs $@ $^

$(SHARED_LIB): $(LIB_OBJ)
	$(CC) $(LDFLAGS) -o $@ $^

$(LIB_OBJ): $(LIB_SRC)
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(LIB_OBJ) $(STATIC_LIB) $(SHARED_LIB)

.PHONY: all clean
```

---

## Referencias

- [cgo Documentation](https://golang.org/cmd/cgo/)
- [cgo Wiki](https://github.com/golang/go/wiki/cgo)
- [Go C API Guidelines](https://github.com/golang/go/wiki/cgo#turning-c-arrays-into-go-slices)
- [CGO is not Go](https://dave.cheney.net/2016/01/18/cgo-is-not-go)
- [Rules for passing pointers between Go and C](https://pkg.go.dev/cmd/cgo#hdr-Passing_pointers)

---

*ARCHAEON_CORE - Preservando la evolución del código*
*El puente entre C y Go: Interoperabilidad moderna*

---
title: "GO_01_FUNDAMENTOS"
version: "1.0.0"
date: "2025-12-31"
author: "ARCHAEON_CORE"
domain: "SOUL_CORE/ARCHAEON/MODERNOS/GO"
type: "technical_documentation"
classification: "foundational"
language: "Go"
paradigm: "concurrent_imperative"
legacy_bridge: "C"
keywords:
  - go_basics
  - types
  - functions
  - structs
  - interfaces
  - error_handling
  - packages
  - testing
  - c_comparison
dependencies:
  - go_1.21+
  - gcc_for_cgo
related_docs:
  - GO_02_CONCURRENCIA.md
  - GO_03_C_BRIDGE.md
---

# GO_01_FUNDAMENTOS

## Tabla de Contenidos

1. [Introducción a Go](#introducción-a-go)
2. [Sistema de Tipos](#sistema-de-tipos)
3. [Funciones y Métodos](#funciones-y-métodos)
4. [Estructuras y Composición](#estructuras-y-composición)
5. [Interfaces](#interfaces)
6. [Manejo de Errores](#manejo-de-errores)
7. [Paquetes y Módulos](#paquetes-y-módulos)
8. [Testing con go test](#testing-con-go-test)
9. [Comparación con C](#comparación-con-c)
10. [Mejores Prácticas](#mejores-prácticas)

---

## Introducción a Go

Go (también conocido como Golang) fue diseñado en Google por Robert Griesemer,
Rob Pike y Ken Thompson. Es un lenguaje compilado, estáticamente tipado, con
garbage collection y soporte nativo para concurrencia.

### Filosofía del Lenguaje

```go
// Go emphasizes simplicity, readability, and efficiency
// The language has only 25 keywords

package main

import "fmt"

func main() {
    // Simple, clear, and direct
    message := "Hello from Go"
    fmt.Println(message)
}
```

### Características Principales

| Característica | Descripción | Comparación con C |
|---------------|-------------|-------------------|
| Compilación | Compilado a código nativo | Similar a C |
| Tipado | Estático con inferencia | Más flexible que C |
| Memoria | Garbage Collection | Manual en C |
| Concurrencia | Goroutines nativos | Threads POSIX en C |
| Punteros | Sin aritmética de punteros | Aritmética completa en C |

### Instalación y Configuración

```bash
# Download from golang.org
# Linux/macOS
wget https://go.dev/dl/go1.21.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.21.linux-amd64.tar.gz

# Add to PATH
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

# Verify installation
go version
go env
```

### Primer Programa

```go
// main.go
package main

import (
    "fmt"
    "os"
    "runtime"
)

func main() {
    // Print system information
    fmt.Printf("Go version: %s\n", runtime.Version())
    fmt.Printf("OS: %s\n", runtime.GOOS)
    fmt.Printf("Architecture: %s\n", runtime.GOARCH)
    fmt.Printf("CPUs: %d\n", runtime.NumCPU())

    // Command line arguments
    if len(os.Args) > 1 {
        fmt.Printf("Arguments: %v\n", os.Args[1:])
    }
}
```

---

## Sistema de Tipos

### Tipos Básicos

```go
package main

import (
    "fmt"
    "math"
    "unsafe"
)

func main() {
    // Boolean
    var isActive bool = true

    // Numeric types - Integers
    var i int = 42           // Platform dependent (32 or 64 bit)
    var i8 int8 = 127        // -128 to 127
    var i16 int16 = 32767    // -32768 to 32767
    var i32 int32 = 2147483647
    var i64 int64 = 9223372036854775807

    // Unsigned integers
    var u uint = 42
    var u8 uint8 = 255       // byte is alias for uint8
    var u16 uint16 = 65535
    var u32 uint32 = 4294967295
    var u64 uint64 = 18446744073709551615

    // Special integer types
    var ptr uintptr = uintptr(unsafe.Pointer(&i))
    var b byte = 'A'         // alias for uint8
    var r rune = '世'        // alias for int32, represents Unicode code point

    // Floating point
    var f32 float32 = math.MaxFloat32
    var f64 float64 = math.MaxFloat64

    // Complex numbers
    var c64 complex64 = complex(1.0, 2.0)
    var c128 complex128 = complex(3.0, 4.0)

    // String
    var s string = "Hello, 世界"

    // Print sizes
    fmt.Printf("int size: %d bytes\n", unsafe.Sizeof(i))
    fmt.Printf("int64 size: %d bytes\n", unsafe.Sizeof(i64))
    fmt.Printf("float64 size: %d bytes\n", unsafe.Sizeof(f64))
    fmt.Printf("string '%s' length: %d bytes\n", s, len(s))

    // Rune count vs byte length
    fmt.Printf("Rune count: %d\n", len([]rune(s)))
}
```

### Declaración de Variables

```go
package main

import "fmt"

func main() {
    // Full declaration
    var name string = "Go"

    // Type inference
    var version = "1.21"

    // Short declaration (inside functions only)
    year := 2009

    // Multiple declarations
    var (
        a int = 1
        b int = 2
        c int = 3
    )

    // Multiple assignment
    x, y, z := 10, 20, 30

    // Swap without temp variable
    x, y = y, x

    // Constants
    const Pi = 3.14159
    const (
        StatusOK    = 200
        StatusError = 500
    )

    // iota for enumerated constants
    const (
        Sunday    = iota // 0
        Monday           // 1
        Tuesday          // 2
        Wednesday        // 3
        Thursday         // 4
        Friday           // 5
        Saturday         // 6
    )

    // Bit flags with iota
    const (
        Read   = 1 << iota // 1
        Write              // 2
        Execute            // 4
    )

    fmt.Printf("Permissions: Read=%d, Write=%d, Execute=%d\n", Read, Write, Execute)
}
```

### Arrays y Slices

```go
package main

import "fmt"

func main() {
    // Arrays - fixed size, value type
    var arr [5]int = [5]int{1, 2, 3, 4, 5}
    arr2 := [...]int{1, 2, 3} // Size inferred from initializer

    // Multi-dimensional arrays
    matrix := [3][3]int{
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9},
    }

    // Slices - dynamic, reference type
    slice := []int{1, 2, 3, 4, 5}

    // Slice from array
    sliceFromArr := arr[1:4] // Elements at index 1, 2, 3

    // make() for slices
    s1 := make([]int, 5)      // length 5, capacity 5
    s2 := make([]int, 5, 10)  // length 5, capacity 10

    fmt.Printf("s1: len=%d, cap=%d\n", len(s1), cap(s1))
    fmt.Printf("s2: len=%d, cap=%d\n", len(s2), cap(s2))

    // Append to slice
    slice = append(slice, 6, 7, 8)

    // Append another slice
    more := []int{9, 10}
    slice = append(slice, more...)

    // Copy slices
    dest := make([]int, len(slice))
    copied := copy(dest, slice)
    fmt.Printf("Copied %d elements\n", copied)

    // Slice internals
    // slice = {pointer, length, capacity}

    // Removing element from slice
    removeIndex := 2
    slice = append(slice[:removeIndex], slice[removeIndex+1:]...)

    // Slice tricks
    // Filter in place
    numbers := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    evens := numbers[:0] // Reuse underlying array
    for _, n := range numbers {
        if n%2 == 0 {
            evens = append(evens, n)
        }
    }
    fmt.Printf("Evens: %v\n", evens)
}
```

### Maps

```go
package main

import "fmt"

func main() {
    // Map declaration and initialization
    var m1 map[string]int                    // nil map
    m2 := map[string]int{}                   // empty map
    m3 := make(map[string]int)               // empty map with make
    m4 := make(map[string]int, 100)          // initial capacity hint

    // Map literal
    ages := map[string]int{
        "Alice": 30,
        "Bob":   25,
        "Carol": 35,
    }

    // Access elements
    aliceAge := ages["Alice"]
    fmt.Printf("Alice is %d years old\n", aliceAge)

    // Check if key exists
    age, exists := ages["Dave"]
    if exists {
        fmt.Printf("Dave is %d\n", age)
    } else {
        fmt.Println("Dave not found")
    }

    // Add or update
    ages["Dave"] = 28
    ages["Alice"] = 31

    // Delete
    delete(ages, "Bob")

    // Iterate over map
    for name, age := range ages {
        fmt.Printf("%s: %d\n", name, age)
    }

    // Map with struct values
    type Person struct {
        Name string
        Age  int
    }

    people := map[string]Person{
        "employee1": {Name: "John", Age: 30},
        "employee2": {Name: "Jane", Age: 25},
    }

    // Nested maps
    nested := map[string]map[string]int{
        "group1": {"a": 1, "b": 2},
        "group2": {"c": 3, "d": 4},
    }

    // Initialize nested map before use
    nested["group3"] = make(map[string]int)
    nested["group3"]["e"] = 5

    // Map as set
    set := make(map[string]struct{})
    set["item1"] = struct{}{}
    set["item2"] = struct{}{}

    if _, ok := set["item1"]; ok {
        fmt.Println("item1 exists in set")
    }
}
```

---

## Funciones y Métodos

### Declaración de Funciones

```go
package main

import (
    "errors"
    "fmt"
)

// Basic function
func add(a, b int) int {
    return a + b
}

// Multiple parameters of same type
func multiply(a, b, c int) int {
    return a * b * c
}

// Multiple return values
func divide(a, b float64) (float64, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}

// Named return values
func rectangle(width, height float64) (area, perimeter float64) {
    area = width * height
    perimeter = 2 * (width + height)
    return // naked return
}

// Variadic function
func sum(numbers ...int) int {
    total := 0
    for _, n := range numbers {
        total += n
    }
    return total
}

// Function as parameter
func apply(fn func(int, int) int, a, b int) int {
    return fn(a, b)
}

// Function returning function (closure)
func counter() func() int {
    count := 0
    return func() int {
        count++
        return count
    }
}

// Defer example
func readFile(filename string) error {
    // Deferred functions are executed in LIFO order
    defer fmt.Println("Cleanup 1")
    defer fmt.Println("Cleanup 2")

    fmt.Println("Reading file:", filename)
    return nil
}

func main() {
    // Call basic function
    fmt.Println("Add:", add(3, 5))

    // Multiple return values
    result, err := divide(10, 2)
    if err != nil {
        fmt.Println("Error:", err)
    } else {
        fmt.Println("Result:", result)
    }

    // Named returns
    area, perimeter := rectangle(5, 3)
    fmt.Printf("Area: %.2f, Perimeter: %.2f\n", area, perimeter)

    // Variadic function
    fmt.Println("Sum:", sum(1, 2, 3, 4, 5))

    // Pass slice to variadic function
    nums := []int{10, 20, 30}
    fmt.Println("Sum of slice:", sum(nums...))

    // Function as value
    fn := add
    fmt.Println("Function variable:", fn(10, 20))

    // Anonymous function
    square := func(x int) int {
        return x * x
    }
    fmt.Println("Square:", square(5))

    // Immediately invoked function
    result2 := func(x int) int {
        return x * 2
    }(10)
    fmt.Println("IIFE result:", result2)

    // Closure
    next := counter()
    fmt.Println("Counter:", next()) // 1
    fmt.Println("Counter:", next()) // 2
    fmt.Println("Counter:", next()) // 3

    // Apply function
    fmt.Println("Apply add:", apply(add, 5, 3))
    fmt.Println("Apply multiply:", apply(func(a, b int) int { return a * b }, 5, 3))

    // Defer
    readFile("test.txt")
}
```

### Métodos

```go
package main

import (
    "fmt"
    "math"
)

// Type definition
type Point struct {
    X, Y float64
}

// Method with value receiver
// The receiver is a copy of the value
func (p Point) Distance(q Point) float64 {
    dx := p.X - q.X
    dy := p.Y - q.Y
    return math.Sqrt(dx*dx + dy*dy)
}

// Method with pointer receiver
// The receiver is a pointer to the value
func (p *Point) Scale(factor float64) {
    p.X *= factor
    p.Y *= factor
}

// Method that can be called on nil receiver
type List struct {
    Value int
    Next  *List
}

func (l *List) Sum() int {
    if l == nil {
        return 0
    }
    return l.Value + l.Next.Sum()
}

// Type alias with methods
type Celsius float64
type Fahrenheit float64

func (c Celsius) ToFahrenheit() Fahrenheit {
    return Fahrenheit(c*9/5 + 32)
}

func (f Fahrenheit) ToCelsius() Celsius {
    return Celsius((f - 32) * 5 / 9)
}

func (c Celsius) String() string {
    return fmt.Sprintf("%.2f°C", c)
}

// Function type with methods
type Handler func(string) string

func (h Handler) Execute(input string) string {
    return h(input)
}

func main() {
    // Using methods
    p1 := Point{0, 0}
    p2 := Point{3, 4}

    fmt.Printf("Distance: %.2f\n", p1.Distance(p2))

    // Pointer receiver method
    p1.Scale(2)  // Go automatically takes address: (&p1).Scale(2)
    fmt.Printf("Scaled point: %+v\n", p1)

    // Nil receiver
    var list *List
    fmt.Printf("Nil list sum: %d\n", list.Sum())

    list = &List{1, &List{2, &List{3, nil}}}
    fmt.Printf("List sum: %d\n", list.Sum())

    // Type alias methods
    temp := Celsius(100)
    fmt.Printf("%s = %.2f°F\n", temp, temp.ToFahrenheit())

    // Function type with method
    upper := Handler(func(s string) string {
        return "Processed: " + s
    })
    fmt.Println(upper.Execute("hello"))
}
```

---

## Estructuras y Composición

### Definición de Estructuras

```go
package main

import (
    "encoding/json"
    "fmt"
    "time"
)

// Basic struct
type Person struct {
    FirstName string
    LastName  string
    Age       int
    Email     string
}

// Struct with tags
type User struct {
    ID        int       `json:"id" db:"user_id"`
    Username  string    `json:"username" db:"username"`
    Email     string    `json:"email,omitempty" db:"email"`
    CreatedAt time.Time `json:"created_at" db:"created_at"`
    password  string    // unexported field (lowercase)
}

// Nested structs
type Address struct {
    Street  string
    City    string
    Country string
    ZipCode string
}

type Employee struct {
    Person
    Address   Address
    Title     string
    Salary    float64
    StartDate time.Time
}

// Anonymous struct
func processConfig() {
    config := struct {
        Host string
        Port int
        SSL  bool
    }{
        Host: "localhost",
        Port: 8080,
        SSL:  true,
    }
    fmt.Printf("Config: %+v\n", config)
}

func main() {
    // Creating structs
    p1 := Person{"John", "Doe", 30, "john@example.com"}

    p2 := Person{
        FirstName: "Jane",
        LastName:  "Smith",
        Age:       25,
    }

    var p3 Person // zero value initialization
    p3.FirstName = "Bob"

    // Pointer to struct
    p4 := &Person{
        FirstName: "Alice",
        LastName:  "Johnson",
        Age:       35,
        Email:     "alice@example.com",
    }

    // Access fields (same syntax for pointer and value)
    fmt.Println(p4.FirstName) // No need for (*p4).FirstName

    // Struct comparison
    p5 := Person{"John", "Doe", 30, "john@example.com"}
    fmt.Printf("p1 == p5: %v\n", p1 == p5)

    // Nested struct
    emp := Employee{
        Person: Person{
            FirstName: "Charlie",
            LastName:  "Brown",
            Age:       40,
        },
        Address: Address{
            Street:  "123 Main St",
            City:    "New York",
            Country: "USA",
        },
        Title:     "Engineer",
        Salary:    100000,
        StartDate: time.Now(),
    }

    // Accessing embedded struct fields
    fmt.Println(emp.FirstName) // Promoted from Person
    fmt.Println(emp.Address.City)

    // JSON marshaling
    user := User{
        ID:        1,
        Username:  "john_doe",
        Email:     "john@example.com",
        CreatedAt: time.Now(),
    }

    jsonData, err := json.MarshalIndent(user, "", "  ")
    if err != nil {
        fmt.Println("Error:", err)
        return
    }
    fmt.Println("JSON:", string(jsonData))

    // JSON unmarshaling
    jsonStr := `{"id": 2, "username": "jane_doe", "created_at": "2024-01-01T00:00:00Z"}`
    var user2 User
    err = json.Unmarshal([]byte(jsonStr), &user2)
    if err != nil {
        fmt.Println("Error:", err)
        return
    }
    fmt.Printf("Parsed: %+v\n", user2)

    processConfig()
}
```

### Composición vs Herencia

```go
package main

import "fmt"

// Go uses composition over inheritance

// Base "class" equivalent
type Engine struct {
    Horsepower int
    Type       string
}

func (e *Engine) Start() string {
    return fmt.Sprintf("Engine started: %dHP %s", e.Horsepower, e.Type)
}

func (e *Engine) Stop() string {
    return "Engine stopped"
}

// Composed struct - Car HAS an Engine
type Car struct {
    Engine // Embedded (anonymous) field - promotes methods
    Brand  string
    Model  string
    Year   int
}

// Override Engine's method
func (c *Car) Start() string {
    engineStart := c.Engine.Start() // Call embedded method
    return fmt.Sprintf("%s starting... %s", c.Brand, engineStart)
}

// Another composed struct
type Motorcycle struct {
    Engine
    Brand     string
    HasSideCar bool
}

// Interface for polymorphism
type Vehicle interface {
    Start() string
    Stop() string
}

func startVehicle(v Vehicle) {
    fmt.Println(v.Start())
}

// Multiple embedding
type Logger struct {
    Prefix string
}

func (l *Logger) Log(msg string) {
    fmt.Printf("[%s] %s\n", l.Prefix, msg)
}

type Config struct {
    Debug bool
}

func (c *Config) IsDebug() bool {
    return c.Debug
}

// Service with multiple embedded types
type Service struct {
    Logger
    Config
    Name string
}

func main() {
    // Create a car
    car := &Car{
        Engine: Engine{Horsepower: 200, Type: "V6"},
        Brand:  "Toyota",
        Model:  "Camry",
        Year:   2024,
    }

    // Promoted method from Engine
    fmt.Println(car.Stop())

    // Overridden method
    fmt.Println(car.Start())

    // Access embedded field directly
    fmt.Printf("Horsepower: %d\n", car.Horsepower)
    fmt.Printf("Engine Type: %s\n", car.Engine.Type)

    // Motorcycle
    moto := &Motorcycle{
        Engine:    Engine{Horsepower: 100, Type: "Inline-4"},
        Brand:     "Honda",
        HasSideCar: false,
    }

    // Polymorphism through interface
    vehicles := []Vehicle{car, moto}
    for _, v := range vehicles {
        startVehicle(v)
    }

    // Multiple embedding
    svc := &Service{
        Logger: Logger{Prefix: "SVC"},
        Config: Config{Debug: true},
        Name:   "MyService",
    }

    svc.Log("Service initialized")
    if svc.IsDebug() {
        svc.Log("Debug mode enabled")
    }
}
```

---

## Interfaces

### Definición e Implementación

```go
package main

import (
    "fmt"
    "io"
    "math"
    "strings"
)

// Interface definition
type Shape interface {
    Area() float64
    Perimeter() float64
}

// Interface with Stringer
type Stringer interface {
    String() string
}

// Multiple interfaces
type Describable interface {
    Shape
    Stringer
    Describe() string
}

// Implementation: Circle
type Circle struct {
    Radius float64
}

func (c Circle) Area() float64 {
    return math.Pi * c.Radius * c.Radius
}

func (c Circle) Perimeter() float64 {
    return 2 * math.Pi * c.Radius
}

func (c Circle) String() string {
    return fmt.Sprintf("Circle(r=%.2f)", c.Radius)
}

func (c Circle) Describe() string {
    return fmt.Sprintf("A circle with radius %.2f", c.Radius)
}

// Implementation: Rectangle
type Rectangle struct {
    Width, Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
    return 2 * (r.Width + r.Height)
}

func (r Rectangle) String() string {
    return fmt.Sprintf("Rectangle(w=%.2f, h=%.2f)", r.Width, r.Height)
}

func (r Rectangle) Describe() string {
    return fmt.Sprintf("A rectangle with dimensions %.2fx%.2f", r.Width, r.Height)
}

// Empty interface - any type
func printAnything(v interface{}) {
    fmt.Printf("Type: %T, Value: %v\n", v, v)
}

// Type assertion
func processValue(v interface{}) {
    // Type assertion with ok check
    if str, ok := v.(string); ok {
        fmt.Printf("String value: %s\n", strings.ToUpper(str))
        return
    }

    if num, ok := v.(int); ok {
        fmt.Printf("Int value squared: %d\n", num*num)
        return
    }

    fmt.Printf("Unknown type: %T\n", v)
}

// Type switch
func describe(v interface{}) string {
    switch val := v.(type) {
    case nil:
        return "nil value"
    case int:
        return fmt.Sprintf("int: %d", val)
    case float64:
        return fmt.Sprintf("float64: %.2f", val)
    case string:
        return fmt.Sprintf("string: %q", val)
    case bool:
        return fmt.Sprintf("bool: %t", val)
    case Shape:
        return fmt.Sprintf("shape with area: %.2f", val.Area())
    default:
        return fmt.Sprintf("unknown type: %T", val)
    }
}

// Interface composition
type Reader interface {
    Read(p []byte) (n int, err error)
}

type Writer interface {
    Write(p []byte) (n int, err error)
}

type ReadWriter interface {
    Reader
    Writer
}

// Custom implementation of io.Reader
type StringReader struct {
    data string
    pos  int
}

func (sr *StringReader) Read(p []byte) (n int, err error) {
    if sr.pos >= len(sr.data) {
        return 0, io.EOF
    }
    n = copy(p, sr.data[sr.pos:])
    sr.pos += n
    return n, nil
}

func main() {
    // Create shapes
    circle := Circle{Radius: 5}
    rect := Rectangle{Width: 4, Height: 6}

    // Use as interface
    shapes := []Shape{circle, rect}

    fmt.Println("=== Shapes ===")
    for _, s := range shapes {
        fmt.Printf("Area: %.2f, Perimeter: %.2f\n", s.Area(), s.Perimeter())
    }

    // Describable interface
    describables := []Describable{circle, rect}
    fmt.Println("\n=== Describables ===")
    for _, d := range describables {
        fmt.Println(d.Describe())
    }

    // Empty interface
    fmt.Println("\n=== Empty Interface ===")
    printAnything(42)
    printAnything("hello")
    printAnything(true)
    printAnything(circle)

    // Type assertion
    fmt.Println("\n=== Type Assertion ===")
    processValue("hello")
    processValue(42)
    processValue(3.14)

    // Type switch
    fmt.Println("\n=== Type Switch ===")
    values := []interface{}{42, 3.14, "hello", true, circle, nil}
    for _, v := range values {
        fmt.Println(describe(v))
    }

    // Custom Reader
    fmt.Println("\n=== Custom Reader ===")
    sr := &StringReader{data: "Hello, Go interfaces!"}
    buf := make([]byte, 8)
    for {
        n, err := sr.Read(buf)
        if err == io.EOF {
            break
        }
        fmt.Printf("Read %d bytes: %s\n", n, string(buf[:n]))
    }
}
```

### Patrones de Interface

```go
package main

import (
    "fmt"
    "sort"
)

// Interface segregation - small, focused interfaces
type Opener interface {
    Open() error
}

type Closer interface {
    Close() error
}

type OpenCloser interface {
    Opener
    Closer
}

// Accept interfaces, return structs
type Database interface {
    Connect() error
    Query(sql string) ([]map[string]interface{}, error)
    Close() error
}

type MySQLDB struct {
    host     string
    port     int
    database string
}

func NewMySQLDB(host string, port int, db string) *MySQLDB {
    return &MySQLDB{host: host, port: port, database: db}
}

func (m *MySQLDB) Connect() error {
    fmt.Printf("Connecting to MySQL at %s:%d/%s\n", m.host, m.port, m.database)
    return nil
}

func (m *MySQLDB) Query(sql string) ([]map[string]interface{}, error) {
    fmt.Printf("Executing: %s\n", sql)
    return []map[string]interface{}{{"id": 1, "name": "test"}}, nil
}

func (m *MySQLDB) Close() error {
    fmt.Println("Closing MySQL connection")
    return nil
}

// Function accepting interface
func RunQuery(db Database, sql string) error {
    if err := db.Connect(); err != nil {
        return err
    }
    defer db.Close()

    results, err := db.Query(sql)
    if err != nil {
        return err
    }

    for _, row := range results {
        fmt.Printf("Row: %v\n", row)
    }
    return nil
}

// Implementing sort.Interface
type Person struct {
    Name string
    Age  int
}

type ByAge []Person

func (a ByAge) Len() int           { return len(a) }
func (a ByAge) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a ByAge) Less(i, j int) bool { return a[i].Age < a[j].Age }

type ByName []Person

func (a ByName) Len() int           { return len(a) }
func (a ByName) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a ByName) Less(i, j int) bool { return a[i].Name < a[j].Name }

func main() {
    // Use interface for dependency injection
    db := NewMySQLDB("localhost", 3306, "testdb")
    RunQuery(db, "SELECT * FROM users")

    // Sorting with interfaces
    people := []Person{
        {"Alice", 30},
        {"Bob", 25},
        {"Charlie", 35},
        {"Diana", 28},
    }

    fmt.Println("\n=== Sorting ===")
    fmt.Println("Original:", people)

    sort.Sort(ByAge(people))
    fmt.Println("By Age:", people)

    sort.Sort(ByName(people))
    fmt.Println("By Name:", people)

    // Sort slice with custom function (Go 1.8+)
    sort.Slice(people, func(i, j int) bool {
        return people[i].Age > people[j].Age // Descending
    })
    fmt.Println("By Age (desc):", people)
}
```

---

## Manejo de Errores

### Patrones de Error

```go
package main

import (
    "errors"
    "fmt"
    "io"
    "os"
)

// Simple error
func divide(a, b float64) (float64, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}

// Error with formatting
func openFile(name string) (*os.File, error) {
    f, err := os.Open(name)
    if err != nil {
        return nil, fmt.Errorf("failed to open %s: %w", name, err)
    }
    return f, nil
}

// Custom error type
type ValidationError struct {
    Field   string
    Message string
}

func (e *ValidationError) Error() string {
    return fmt.Sprintf("validation error on %s: %s", e.Field, e.Message)
}

func validateAge(age int) error {
    if age < 0 {
        return &ValidationError{Field: "age", Message: "cannot be negative"}
    }
    if age > 150 {
        return &ValidationError{Field: "age", Message: "unrealistic value"}
    }
    return nil
}

// Sentinel errors
var (
    ErrNotFound     = errors.New("not found")
    ErrUnauthorized = errors.New("unauthorized")
    ErrInvalidInput = errors.New("invalid input")
)

func findUser(id int) (string, error) {
    users := map[int]string{1: "Alice", 2: "Bob"}
    if user, ok := users[id]; ok {
        return user, nil
    }
    return "", ErrNotFound
}

// Error wrapping (Go 1.13+)
func processFile(name string) error {
    f, err := os.Open(name)
    if err != nil {
        return fmt.Errorf("processFile: %w", err)
    }
    defer f.Close()

    // Process file...
    return nil
}

// Error unwrapping and checking
func handleFileError(err error) {
    if err == nil {
        return
    }

    // Check for specific error
    if errors.Is(err, os.ErrNotExist) {
        fmt.Println("File does not exist")
        return
    }

    if errors.Is(err, os.ErrPermission) {
        fmt.Println("Permission denied")
        return
    }

    // Check for error type
    var pathErr *os.PathError
    if errors.As(err, &pathErr) {
        fmt.Printf("Path error: Op=%s, Path=%s\n", pathErr.Op, pathErr.Path)
        return
    }

    fmt.Printf("Unknown error: %v\n", err)
}

// Multiple errors
type MultiError struct {
    Errors []error
}

func (m *MultiError) Error() string {
    if len(m.Errors) == 0 {
        return "no errors"
    }
    if len(m.Errors) == 1 {
        return m.Errors[0].Error()
    }
    return fmt.Sprintf("%d errors occurred", len(m.Errors))
}

func (m *MultiError) Add(err error) {
    if err != nil {
        m.Errors = append(m.Errors, err)
    }
}

func (m *MultiError) HasErrors() bool {
    return len(m.Errors) > 0
}

// Panic and recover
func riskyOperation() {
    defer func() {
        if r := recover(); r != nil {
            fmt.Printf("Recovered from panic: %v\n", r)
        }
    }()

    panic("something went wrong")
}

// Must pattern for initialization
func mustParse(data string) map[string]string {
    result := make(map[string]string)
    // Parse logic...
    if data == "" {
        panic("empty data")
    }
    return result
}

func main() {
    // Simple error
    result, err := divide(10, 0)
    if err != nil {
        fmt.Println("Error:", err)
    } else {
        fmt.Println("Result:", result)
    }

    // Validation error
    if err := validateAge(-5); err != nil {
        if ve, ok := err.(*ValidationError); ok {
            fmt.Printf("Validation failed: field=%s, msg=%s\n", ve.Field, ve.Message)
        }
    }

    // Sentinel error check
    _, err = findUser(999)
    if errors.Is(err, ErrNotFound) {
        fmt.Println("User not found")
    }

    // Error wrapping
    err = processFile("nonexistent.txt")
    handleFileError(err)

    // Multiple errors
    multi := &MultiError{}
    multi.Add(errors.New("error 1"))
    multi.Add(errors.New("error 2"))
    if multi.HasErrors() {
        fmt.Println("Multiple errors:", multi.Error())
    }

    // Panic and recover
    riskyOperation()
    fmt.Println("Continued after panic")

    // Defer with error handling
    func() {
        f, err := os.Open("test.txt")
        if err != nil {
            fmt.Println("Open error:", err)
            return
        }
        defer func() {
            if cerr := f.Close(); cerr != nil {
                fmt.Println("Close error:", cerr)
            }
        }()
        // Use file...
    }()
}
```

---

## Paquetes y Módulos

### Estructura de Proyecto

```
myproject/
├── go.mod
├── go.sum
├── main.go
├── cmd/
│   └── myapp/
│       └── main.go
├── internal/
│   ├── config/
│   │   └── config.go
│   └── service/
│       └── service.go
├── pkg/
│   └── utils/
│       └── utils.go
└── api/
    └── handlers/
        └── handlers.go
```

### Módulos Go

```go
// go.mod
module github.com/user/myproject

go 1.21

require (
    github.com/gin-gonic/gin v1.9.1
    github.com/spf13/viper v1.16.0
)

require (
    // Indirect dependencies
    github.com/pelletier/go-toml/v2 v2.0.8 // indirect
)
```

```bash
# Initialize module
go mod init github.com/user/myproject

# Add dependencies
go get github.com/gin-gonic/gin@latest

# Update dependencies
go get -u ./...

# Tidy dependencies
go mod tidy

# Download dependencies
go mod download

# Vendor dependencies
go mod vendor

# Show module graph
go mod graph

# Verify dependencies
go mod verify
```

### Organización de Paquetes

```go
// internal/config/config.go
package config

import (
    "os"
    "strconv"
)

type Config struct {
    Host     string
    Port     int
    Debug    bool
    Database DatabaseConfig
}

type DatabaseConfig struct {
    Host     string
    Port     int
    Name     string
    User     string
    Password string
}

func Load() (*Config, error) {
    port, _ := strconv.Atoi(getEnv("PORT", "8080"))
    dbPort, _ := strconv.Atoi(getEnv("DB_PORT", "5432"))

    return &Config{
        Host:  getEnv("HOST", "localhost"),
        Port:  port,
        Debug: getEnv("DEBUG", "false") == "true",
        Database: DatabaseConfig{
            Host:     getEnv("DB_HOST", "localhost"),
            Port:     dbPort,
            Name:     getEnv("DB_NAME", "mydb"),
            User:     getEnv("DB_USER", "user"),
            Password: getEnv("DB_PASSWORD", ""),
        },
    }, nil
}

func getEnv(key, defaultValue string) string {
    if value := os.Getenv(key); value != "" {
        return value
    }
    return defaultValue
}

// pkg/utils/utils.go
package utils

import (
    "crypto/rand"
    "encoding/hex"
)

// GenerateID generates a random hex ID
func GenerateID(length int) (string, error) {
    bytes := make([]byte, length)
    if _, err := rand.Read(bytes); err != nil {
        return "", err
    }
    return hex.EncodeToString(bytes), nil
}

// internal/service/service.go
package service

import (
    "github.com/user/myproject/internal/config"
    "github.com/user/myproject/pkg/utils"
)

type Service struct {
    config *config.Config
}

func New(cfg *config.Config) *Service {
    return &Service{config: cfg}
}

func (s *Service) GenerateToken() (string, error) {
    return utils.GenerateID(16)
}

// main.go
package main

import (
    "fmt"
    "log"

    "github.com/user/myproject/internal/config"
    "github.com/user/myproject/internal/service"
)

func main() {
    cfg, err := config.Load()
    if err != nil {
        log.Fatal(err)
    }

    svc := service.New(cfg)
    token, err := svc.GenerateToken()
    if err != nil {
        log.Fatal(err)
    }

    fmt.Printf("Generated token: %s\n", token)
}
```

---

## Testing con go test

### Tests Básicos

```go
// math_test.go
package math

import (
    "testing"
)

// Function to test
func Add(a, b int) int {
    return a + b
}

func Multiply(a, b int) int {
    return a * b
}

// Basic test
func TestAdd(t *testing.T) {
    result := Add(2, 3)
    if result != 5 {
        t.Errorf("Add(2, 3) = %d; want 5", result)
    }
}

// Table-driven test
func TestAddTableDriven(t *testing.T) {
    tests := []struct {
        name     string
        a, b     int
        expected int
    }{
        {"positive numbers", 2, 3, 5},
        {"negative numbers", -2, -3, -5},
        {"mixed numbers", -2, 3, 1},
        {"zeros", 0, 0, 0},
        {"with zero", 5, 0, 5},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            result := Add(tt.a, tt.b)
            if result != tt.expected {
                t.Errorf("Add(%d, %d) = %d; want %d", tt.a, tt.b, result, tt.expected)
            }
        })
    }
}

// Test with setup and teardown
func TestWithSetup(t *testing.T) {
    // Setup
    t.Log("Setting up test")

    // Teardown
    t.Cleanup(func() {
        t.Log("Cleaning up after test")
    })

    // Test logic
    result := Multiply(4, 5)
    if result != 20 {
        t.Errorf("Multiply(4, 5) = %d; want 20", result)
    }
}

// Skip test conditionally
func TestSkipExample(t *testing.T) {
    if testing.Short() {
        t.Skip("Skipping in short mode")
    }
    // Long-running test...
}

// Parallel tests
func TestParallel1(t *testing.T) {
    t.Parallel()
    // Test runs in parallel
}

func TestParallel2(t *testing.T) {
    t.Parallel()
    // This test also runs in parallel
}
```

### Benchmarks

```go
// benchmark_test.go
package math

import (
    "testing"
)

func Fibonacci(n int) int {
    if n < 2 {
        return n
    }
    return Fibonacci(n-1) + Fibonacci(n-2)
}

func FibonacciIterative(n int) int {
    if n < 2 {
        return n
    }
    a, b := 0, 1
    for i := 2; i <= n; i++ {
        a, b = b, a+b
    }
    return b
}

// Basic benchmark
func BenchmarkFibonacci(b *testing.B) {
    for i := 0; i < b.N; i++ {
        Fibonacci(20)
    }
}

func BenchmarkFibonacciIterative(b *testing.B) {
    for i := 0; i < b.N; i++ {
        FibonacciIterative(20)
    }
}

// Benchmark with different inputs
func BenchmarkFibonacciInputs(b *testing.B) {
    inputs := []int{5, 10, 15, 20}

    for _, n := range inputs {
        b.Run(fmt.Sprintf("n=%d", n), func(b *testing.B) {
            for i := 0; i < b.N; i++ {
                FibonacciIterative(n)
            }
        })
    }
}

// Benchmark with memory allocation tracking
func BenchmarkWithAllocs(b *testing.B) {
    b.ReportAllocs()
    for i := 0; i < b.N; i++ {
        _ = make([]int, 1000)
    }
}

// Reset timer for setup
func BenchmarkWithSetup(b *testing.B) {
    // Setup that shouldn't be measured
    data := make([]int, 1000000)
    for i := range data {
        data[i] = i
    }

    b.ResetTimer() // Reset timer after setup

    for i := 0; i < b.N; i++ {
        sum := 0
        for _, v := range data {
            sum += v
        }
    }
}
```

### Mocking e Interfaces

```go
// service_test.go
package service

import (
    "errors"
    "testing"
)

// Interface for dependency
type Repository interface {
    GetUser(id int) (*User, error)
    SaveUser(user *User) error
}

type User struct {
    ID   int
    Name string
}

// Service that depends on Repository
type UserService struct {
    repo Repository
}

func NewUserService(repo Repository) *UserService {
    return &UserService{repo: repo}
}

func (s *UserService) GetUserName(id int) (string, error) {
    user, err := s.repo.GetUser(id)
    if err != nil {
        return "", err
    }
    return user.Name, nil
}

// Mock implementation
type MockRepository struct {
    users map[int]*User
    err   error
}

func NewMockRepository() *MockRepository {
    return &MockRepository{
        users: make(map[int]*User),
    }
}

func (m *MockRepository) GetUser(id int) (*User, error) {
    if m.err != nil {
        return nil, m.err
    }
    if user, ok := m.users[id]; ok {
        return user, nil
    }
    return nil, errors.New("user not found")
}

func (m *MockRepository) SaveUser(user *User) error {
    if m.err != nil {
        return m.err
    }
    m.users[user.ID] = user
    return nil
}

func (m *MockRepository) SetError(err error) {
    m.err = err
}

// Tests using mock
func TestGetUserName(t *testing.T) {
    // Setup mock
    mock := NewMockRepository()
    mock.users[1] = &User{ID: 1, Name: "Alice"}

    // Create service with mock
    svc := NewUserService(mock)

    // Test success case
    name, err := svc.GetUserName(1)
    if err != nil {
        t.Errorf("unexpected error: %v", err)
    }
    if name != "Alice" {
        t.Errorf("got %s; want Alice", name)
    }

    // Test not found case
    _, err = svc.GetUserName(999)
    if err == nil {
        t.Error("expected error for non-existent user")
    }
}

func TestGetUserNameWithError(t *testing.T) {
    mock := NewMockRepository()
    mock.SetError(errors.New("database error"))

    svc := NewUserService(mock)

    _, err := svc.GetUserName(1)
    if err == nil {
        t.Error("expected error")
    }
}
```

```bash
# Run tests
go test ./...

# Run with verbose output
go test -v ./...

# Run specific test
go test -run TestAdd ./...

# Run with coverage
go test -cover ./...

# Generate coverage report
go test -coverprofile=coverage.out ./...
go tool cover -html=coverage.out

# Run benchmarks
go test -bench=. ./...

# Run benchmarks with memory stats
go test -bench=. -benchmem ./...

# Run in short mode
go test -short ./...

# Race detection
go test -race ./...
```

---

## Comparación con C

### Tabla Comparativa de Estructuras

| Concepto | C | Go |
|----------|---|-----|
| Tipos básicos | `int, char, float` | `int, byte, float64` |
| Strings | `char*` o `char[]` | `string` (inmutable) |
| Arrays | `int arr[10]` | `[10]int` |
| Slices | No nativo | `[]int` |
| Structs | `struct` | `type ... struct` |
| Punteros | `*ptr, &var` | `*ptr, &var` |
| Aritmética ptr | Permitida | No permitida |
| Memoria | `malloc/free` | Garbage Collection |
| Errores | Return codes, errno | Multiple return values |
| Genéricos | Macros/void* | Generics (Go 1.18+) |

### Ejemplos Comparativos

```c
/* C: Dynamic array */
#include <stdlib.h>
#include <string.h>

typedef struct {
    int *data;
    size_t len;
    size_t cap;
} IntSlice;

IntSlice* new_slice(size_t cap) {
    IntSlice *s = malloc(sizeof(IntSlice));
    s->data = malloc(cap * sizeof(int));
    s->len = 0;
    s->cap = cap;
    return s;
}

void append(IntSlice *s, int val) {
    if (s->len >= s->cap) {
        s->cap *= 2;
        s->data = realloc(s->data, s->cap * sizeof(int));
    }
    s->data[s->len++] = val;
}

void free_slice(IntSlice *s) {
    free(s->data);
    free(s);
}
```

```go
// Go: Built-in slice
package main

func main() {
    // Slice with automatic growth
    s := make([]int, 0, 10)

    // Append automatically grows capacity
    s = append(s, 1, 2, 3)

    // No manual memory management needed
    // GC handles cleanup
}
```

```c
/* C: Error handling */
#include <stdio.h>
#include <errno.h>

int read_file(const char *path, char **content) {
    FILE *f = fopen(path, "r");
    if (f == NULL) {
        return -1;  // errno is set
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    *content = malloc(size + 1);
    if (*content == NULL) {
        fclose(f);
        return -1;
    }

    fread(*content, 1, size, f);
    (*content)[size] = '\0';

    fclose(f);
    return 0;
}
```

```go
// Go: Error handling
package main

import (
    "fmt"
    "os"
)

func readFile(path string) ([]byte, error) {
    content, err := os.ReadFile(path)
    if err != nil {
        return nil, fmt.Errorf("reading %s: %w", path, err)
    }
    return content, nil
}

func main() {
    content, err := readFile("test.txt")
    if err != nil {
        fmt.Println("Error:", err)
        return
    }
    fmt.Println(string(content))
}
```

```c
/* C: Struct with methods (function pointers) */
typedef struct {
    double x, y;
    double (*distance)(void*, void*);
} Point;

double point_distance(void *p1, void *p2) {
    Point *a = (Point*)p1;
    Point *b = (Point*)p2;
    double dx = a->x - b->x;
    double dy = a->y - b->y;
    return sqrt(dx*dx + dy*dy);
}

Point new_point(double x, double y) {
    Point p = {x, y, point_distance};
    return p;
}
```

```go
// Go: Struct with methods
package main

import "math"

type Point struct {
    X, Y float64
}

func (p Point) Distance(q Point) float64 {
    dx := p.X - q.X
    dy := p.Y - q.Y
    return math.Sqrt(dx*dx + dy*dy)
}

func main() {
    p1 := Point{0, 0}
    p2 := Point{3, 4}

    dist := p1.Distance(p2)
    fmt.Printf("Distance: %.2f\n", dist)
}
```

---

## Mejores Prácticas

### Guía de Estilo

```go
// 1. Use gofmt for formatting
// All Go code should be formatted with gofmt

// 2. Naming conventions
// - MixedCaps or mixedCaps, not underscores
// - Exported names start with uppercase
// - Package names are lowercase, single word
// - Interface names: single method + "er" (Reader, Writer)

// Good
type UserService struct{}
func (u *UserService) GetUserByID(id int) (*User, error)

// Bad
type user_service struct{}
func (u *user_service) get_user_by_id(id int) (*User, error)

// 3. Error handling
// Always check errors, don't ignore them

// Good
f, err := os.Open(path)
if err != nil {
    return fmt.Errorf("opening %s: %w", path, err)
}
defer f.Close()

// Bad
f, _ := os.Open(path)

// 4. Keep functions small and focused
// Each function should do one thing well

// 5. Prefer composition over inheritance
type Logger struct{}
type Config struct{}

type Service struct {
    Logger
    Config
    // ...
}

// 6. Accept interfaces, return structs
func ProcessData(r io.Reader) (*Result, error) {
    // ...
}

// 7. Use defer for cleanup
func processFile(path string) error {
    f, err := os.Open(path)
    if err != nil {
        return err
    }
    defer f.Close()

    // Process file...
    return nil
}

// 8. Avoid global state when possible
// Use dependency injection instead

// 9. Document exported functions and types
// Package doc: general purpose utilities
package utils

// GenerateID creates a unique identifier with the specified length.
// It uses cryptographically secure random bytes.
func GenerateID(length int) (string, error) {
    // ...
}

// 10. Use context for cancellation and timeouts
func fetchData(ctx context.Context, url string) ([]byte, error) {
    req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
    if err != nil {
        return nil, err
    }
    // ...
}
```

### Herramientas Esenciales

```bash
# Format code
gofmt -w .

# Static analysis
go vet ./...

# Find common mistakes
staticcheck ./...

# Security scanning
gosec ./...

# Lint code
golangci-lint run

# Generate documentation
godoc -http=:6060

# View module dependencies
go list -m all

# Find why a dependency is included
go mod why github.com/some/package
```

---

## Referencias

- [Go Documentation](https://go.dev/doc/)
- [Effective Go](https://go.dev/doc/effective_go)
- [Go Code Review Comments](https://github.com/golang/go/wiki/CodeReviewComments)
- [Go Proverbs](https://go-proverbs.github.io/)
- [The Go Programming Language Specification](https://go.dev/ref/spec)

---

*ARCHAEON_CORE - Preservando la evolución del código*
*De C a Go: La modernización del software de sistemas*

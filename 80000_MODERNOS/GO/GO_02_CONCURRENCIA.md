---
title: "GO_02_CONCURRENCIA"
version: "1.0.0"
date: "2025-12-31"
author: "ARCHAEON_CORE"
domain: "SOUL_CORE/ARCHAEON/MODERNOS/GO"
type: "technical_documentation"
classification: "advanced"
language: "Go"
paradigm: "concurrent_imperative"
legacy_bridge: "C/POSIX_threads"
keywords:
  - goroutines
  - channels
  - select
  - mutex
  - waitgroup
  - concurrency_patterns
  - race_detection
  - scheduler
dependencies:
  - go_1.21+
related_docs:
  - GO_01_FUNDAMENTOS.md
  - GO_03_C_BRIDGE.md
  - GO_04_SISTEMAS.md
---

# GO_02_CONCURRENCIA

## Tabla de Contenidos

1. [Modelo de Concurrencia de Go](#modelo-de-concurrencia-de-go)
2. [Goroutines](#goroutines)
3. [Channels](#channels)
4. [Select Statement](#select-statement)
5. [Paquete sync](#paquete-sync)
6. [Patrones de Concurrencia](#patrones-de-concurrencia)
7. [Race Condition Detection](#race-condition-detection)
8. [Comparación con C Threads](#comparación-con-c-threads)
9. [Mejores Prácticas](#mejores-prácticas)

---

## Modelo de Concurrencia de Go

Go implementa el modelo CSP (Communicating Sequential Processes) de Tony Hoare.
La filosofía es: "No comunicar compartiendo memoria; compartir memoria comunicando."

### El Go Scheduler (GMP Model)

```
┌─────────────────────────────────────────────────────────────┐
│                     Go Scheduler (GMP)                       │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│   G (Goroutine)  - Lightweight user-space thread            │
│   M (Machine)    - OS thread                                 │
│   P (Processor)  - Logical processor (context)              │
│                                                              │
│   ┌───┐ ┌───┐ ┌───┐ ┌───┐ ┌───┐                            │
│   │ G │ │ G │ │ G │ │ G │ │ G │  Goroutines               │
│   └─┬─┘ └─┬─┘ └─┬─┘ └─┬─┘ └─┬─┘                            │
│     │     │     │     │     │                               │
│   ┌─┴─────┴─────┴─────┴─────┴─┐                             │
│   │         P (Local Queue)    │                             │
│   └────────────┬───────────────┘                             │
│                │                                              │
│   ┌────────────┴───────────────┐                             │
│   │            M (OS Thread)    │                             │
│   └────────────┬───────────────┘                             │
│                │                                              │
│   ┌────────────┴───────────────┐                             │
│   │         OS Kernel           │                             │
│   └─────────────────────────────┘                             │
└─────────────────────────────────────────────────────────────┘
```

### GOMAXPROCS y Configuración

```go
package main

import (
    "fmt"
    "runtime"
    "sync"
)

func main() {
    // Get number of logical CPUs
    numCPU := runtime.NumCPU()
    fmt.Printf("Number of CPUs: %d\n", numCPU)

    // Get current GOMAXPROCS
    current := runtime.GOMAXPROCS(0)
    fmt.Printf("Current GOMAXPROCS: %d\n", current)

    // Set GOMAXPROCS (returns previous value)
    previous := runtime.GOMAXPROCS(4)
    fmt.Printf("Previous GOMAXPROCS: %d\n", previous)

    // Get number of goroutines
    fmt.Printf("Number of goroutines: %d\n", runtime.NumGoroutine())

    // Force garbage collection
    runtime.GC()

    // Yield processor to other goroutines
    runtime.Gosched()

    // Get Go version
    fmt.Printf("Go version: %s\n", runtime.Version())

    // Memory statistics
    var memStats runtime.MemStats
    runtime.ReadMemStats(&memStats)
    fmt.Printf("Allocated memory: %d bytes\n", memStats.Alloc)
    fmt.Printf("Total allocations: %d bytes\n", memStats.TotalAlloc)
    fmt.Printf("System memory: %d bytes\n", memStats.Sys)
    fmt.Printf("GC cycles: %d\n", memStats.NumGC)
}
```

---

## Goroutines

### Fundamentos de Goroutines

```go
package main

import (
    "fmt"
    "sync"
    "time"
)

// Simple goroutine
func sayHello(name string) {
    fmt.Printf("Hello, %s!\n", name)
}

// Goroutine with return value via channel
func compute(x int, result chan<- int) {
    time.Sleep(100 * time.Millisecond)
    result <- x * x
}

func main() {
    // Start a goroutine
    go sayHello("World")

    // Anonymous goroutine
    go func() {
        fmt.Println("Anonymous goroutine")
    }()

    // Goroutine with parameter
    message := "Hello from closure"
    go func(msg string) {
        fmt.Println(msg)
    }(message)

    // Wait for goroutines (simple approach)
    time.Sleep(100 * time.Millisecond)

    // Better approach: WaitGroup
    var wg sync.WaitGroup

    for i := 0; i < 5; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            fmt.Printf("Goroutine %d running\n", id)
            time.Sleep(50 * time.Millisecond)
        }(i) // Pass i as parameter to avoid closure issue
    }

    wg.Wait()
    fmt.Println("All goroutines completed")

    // Collect results from goroutines
    results := make(chan int, 5)

    for i := 1; i <= 5; i++ {
        go compute(i, results)
    }

    // Collect all results
    for i := 0; i < 5; i++ {
        result := <-results
        fmt.Printf("Result: %d\n", result)
    }
}
```

### Ciclo de Vida de Goroutines

```go
package main

import (
    "context"
    "fmt"
    "sync"
    "time"
)

// Long-running goroutine with cancellation
func worker(ctx context.Context, id int, wg *sync.WaitGroup) {
    defer wg.Done()

    for {
        select {
        case <-ctx.Done():
            fmt.Printf("Worker %d: shutting down (reason: %v)\n", id, ctx.Err())
            return
        default:
            fmt.Printf("Worker %d: working...\n", id)
            time.Sleep(500 * time.Millisecond)
        }
    }
}

// Goroutine with timeout
func fetchWithTimeout(url string, timeout time.Duration) (string, error) {
    ctx, cancel := context.WithTimeout(context.Background(), timeout)
    defer cancel()

    resultCh := make(chan string, 1)
    errCh := make(chan error, 1)

    go func() {
        // Simulate fetch
        time.Sleep(200 * time.Millisecond)
        resultCh <- "data from " + url
    }()

    select {
    case <-ctx.Done():
        return "", ctx.Err()
    case err := <-errCh:
        return "", err
    case result := <-resultCh:
        return result, nil
    }
}

// Goroutine leak prevention
func processWithLeak() {
    ch := make(chan int)

    go func() {
        // This goroutine will leak if channel is never read
        ch <- 42
        fmt.Println("Sent value") // Never printed if leaked
    }()

    // If we return early without reading, goroutine leaks
    // return

    val := <-ch
    fmt.Println("Received:", val)
}

// Proper cleanup with buffered channel
func processWithoutLeak() {
    ch := make(chan int, 1) // Buffered channel

    go func() {
        ch <- 42 // Non-blocking because of buffer
        fmt.Println("Sent value")
    }()

    // Even if we return early, goroutine won't block forever
    time.Sleep(10 * time.Millisecond)

    select {
    case val := <-ch:
        fmt.Println("Received:", val)
    default:
        fmt.Println("No value ready")
    }
}

func main() {
    // Context with cancellation
    ctx, cancel := context.WithCancel(context.Background())
    var wg sync.WaitGroup

    // Start multiple workers
    for i := 1; i <= 3; i++ {
        wg.Add(1)
        go worker(ctx, i, &wg)
    }

    // Let workers run for a bit
    time.Sleep(2 * time.Second)

    // Signal all workers to stop
    cancel()

    // Wait for all workers to finish
    wg.Wait()
    fmt.Println("All workers stopped")

    // Fetch with timeout
    result, err := fetchWithTimeout("https://example.com", 1*time.Second)
    if err != nil {
        fmt.Println("Fetch error:", err)
    } else {
        fmt.Println("Fetch result:", result)
    }

    processWithLeak()
    processWithoutLeak()
}
```

---

## Channels

### Tipos de Channels

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Unbuffered channel (synchronous)
    unbuffered := make(chan int)

    go func() {
        unbuffered <- 42 // Blocks until receiver is ready
    }()

    value := <-unbuffered // Blocks until sender sends
    fmt.Println("Unbuffered:", value)

    // Buffered channel (asynchronous up to capacity)
    buffered := make(chan int, 3)

    buffered <- 1 // Non-blocking (buffer has space)
    buffered <- 2
    buffered <- 3
    // buffered <- 4 // Would block (buffer full)

    fmt.Println("Buffered:", <-buffered, <-buffered, <-buffered)

    // Directional channels
    sendOnly := make(chan<- int, 1) // Can only send
    recvOnly := make(<-chan int, 1) // Can only receive

    // Using directional channels in functions
    go producer(sendOnly)

    // Channel length and capacity
    ch := make(chan int, 5)
    ch <- 1
    ch <- 2

    fmt.Printf("Length: %d, Capacity: %d\n", len(ch), cap(ch))

    // Close a channel
    close(buffered)

    // Reading from closed channel
    val, ok := <-buffered
    fmt.Printf("Value: %d, OK: %v\n", val, ok) // 0, false

    // Range over channel
    numbers := make(chan int, 5)
    go func() {
        for i := 1; i <= 5; i++ {
            numbers <- i
        }
        close(numbers)
    }()

    for num := range numbers {
        fmt.Printf("Number: %d\n", num)
    }
}

func producer(ch chan<- int) {
    ch <- 42
}

func consumer(ch <-chan int) int {
    return <-ch
}
```

### Patrones con Channels

```go
package main

import (
    "fmt"
    "math/rand"
    "sync"
    "time"
)

// Generator pattern
func generator(nums ...int) <-chan int {
    out := make(chan int)
    go func() {
        defer close(out)
        for _, n := range nums {
            out <- n
        }
    }()
    return out
}

// Pipeline pattern - square
func square(in <-chan int) <-chan int {
    out := make(chan int)
    go func() {
        defer close(out)
        for n := range in {
            out <- n * n
        }
    }()
    return out
}

// Pipeline pattern - double
func double(in <-chan int) <-chan int {
    out := make(chan int)
    go func() {
        defer close(out)
        for n := range in {
            out <- n * 2
        }
    }()
    return out
}

// Fan-out: multiple goroutines read from same channel
// Fan-in: multiple channels merge into one
func fanIn(channels ...<-chan int) <-chan int {
    var wg sync.WaitGroup
    merged := make(chan int)

    // Start a goroutine for each input channel
    output := func(ch <-chan int) {
        defer wg.Done()
        for n := range ch {
            merged <- n
        }
    }

    wg.Add(len(channels))
    for _, ch := range channels {
        go output(ch)
    }

    // Close merged channel when all inputs are done
    go func() {
        wg.Wait()
        close(merged)
    }()

    return merged
}

// Worker pool pattern
func workerPool(numWorkers int, jobs <-chan int, results chan<- int) {
    var wg sync.WaitGroup

    worker := func(id int) {
        defer wg.Done()
        for job := range jobs {
            fmt.Printf("Worker %d processing job %d\n", id, job)
            time.Sleep(100 * time.Millisecond)
            results <- job * 2
        }
    }

    wg.Add(numWorkers)
    for i := 1; i <= numWorkers; i++ {
        go worker(i)
    }

    // Close results when all workers done
    go func() {
        wg.Wait()
        close(results)
    }()
}

// Semaphore pattern using buffered channel
func semaphore() {
    const maxConcurrent = 3
    sem := make(chan struct{}, maxConcurrent)

    var wg sync.WaitGroup

    for i := 1; i <= 10; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()

            sem <- struct{}{} // Acquire
            defer func() {
                <-sem // Release
            }()

            fmt.Printf("Task %d running (max %d concurrent)\n", id, maxConcurrent)
            time.Sleep(100 * time.Millisecond)
        }(i)
    }

    wg.Wait()
}

// Or-done pattern
func orDone(done <-chan struct{}, c <-chan int) <-chan int {
    out := make(chan int)
    go func() {
        defer close(out)
        for {
            select {
            case <-done:
                return
            case v, ok := <-c:
                if !ok {
                    return
                }
                select {
                case out <- v:
                case <-done:
                    return
                }
            }
        }
    }()
    return out
}

// Tee pattern - split channel into two
func tee(done <-chan struct{}, in <-chan int) (<-chan int, <-chan int) {
    out1 := make(chan int)
    out2 := make(chan int)

    go func() {
        defer close(out1)
        defer close(out2)

        for val := range orDone(done, in) {
            // Create local copies for closure
            o1, o2 := out1, out2

            for i := 0; i < 2; i++ {
                select {
                case <-done:
                    return
                case o1 <- val:
                    o1 = nil // Disable this case after sending
                case o2 <- val:
                    o2 = nil
                }
            }
        }
    }()

    return out1, out2
}

func main() {
    // Pipeline
    fmt.Println("=== Pipeline ===")
    nums := generator(1, 2, 3, 4, 5)
    squared := square(nums)
    doubled := double(squared)

    for n := range doubled {
        fmt.Println(n)
    }

    // Fan-in
    fmt.Println("\n=== Fan-in ===")
    ch1 := generator(1, 2, 3)
    ch2 := generator(4, 5, 6)
    ch3 := generator(7, 8, 9)

    for n := range fanIn(ch1, ch2, ch3) {
        fmt.Println(n)
    }

    // Worker pool
    fmt.Println("\n=== Worker Pool ===")
    jobs := make(chan int, 10)
    results := make(chan int, 10)

    go workerPool(3, jobs, results)

    // Send jobs
    for i := 1; i <= 10; i++ {
        jobs <- i
    }
    close(jobs)

    // Collect results
    for result := range results {
        fmt.Println("Result:", result)
    }

    // Semaphore
    fmt.Println("\n=== Semaphore ===")
    semaphore()
}
```

---

## Select Statement

### Uso Básico de Select

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    ch1 := make(chan string)
    ch2 := make(chan string)

    // Producer 1
    go func() {
        time.Sleep(100 * time.Millisecond)
        ch1 <- "from channel 1"
    }()

    // Producer 2
    go func() {
        time.Sleep(200 * time.Millisecond)
        ch2 <- "from channel 2"
    }()

    // Basic select - waits for first available
    for i := 0; i < 2; i++ {
        select {
        case msg1 := <-ch1:
            fmt.Println("Received:", msg1)
        case msg2 := <-ch2:
            fmt.Println("Received:", msg2)
        }
    }

    // Select with timeout
    ch3 := make(chan string)
    go func() {
        time.Sleep(2 * time.Second)
        ch3 <- "delayed message"
    }()

    select {
    case msg := <-ch3:
        fmt.Println("Received:", msg)
    case <-time.After(1 * time.Second):
        fmt.Println("Timeout waiting for message")
    }

    // Non-blocking select with default
    ch4 := make(chan int, 1)

    select {
    case val := <-ch4:
        fmt.Println("Received:", val)
    default:
        fmt.Println("No value available")
    }

    // Non-blocking send
    ch4 <- 42
    ch4 <- 43 // Would block, but default prevents it

    select {
    case ch4 <- 100:
        fmt.Println("Sent value")
    default:
        fmt.Println("Channel full, couldn't send")
    }

    // Select with nil channel (disabled)
    var nilCh chan int
    ch5 := make(chan int, 1)
    ch5 <- 1

    select {
    case <-nilCh:
        fmt.Println("This will never happen")
    case val := <-ch5:
        fmt.Println("From ch5:", val)
    }
}
```

### Patrones Avanzados con Select

```go
package main

import (
    "context"
    "fmt"
    "math/rand"
    "sync"
    "time"
)

// Heartbeat pattern
func heartbeat(ctx context.Context, interval time.Duration) <-chan struct{} {
    heartbeat := make(chan struct{})

    go func() {
        defer close(heartbeat)
        ticker := time.NewTicker(interval)
        defer ticker.Stop()

        for {
            select {
            case <-ctx.Done():
                return
            case <-ticker.C:
                select {
                case heartbeat <- struct{}{}:
                default:
                    // Skip if no one is listening
                }
            }
        }
    }()

    return heartbeat
}

// Rate limiting
func rateLimiter(ratePerSecond int) <-chan time.Time {
    interval := time.Second / time.Duration(ratePerSecond)
    return time.Tick(interval)
}

// Debounce pattern
func debounce(input <-chan string, delay time.Duration) <-chan string {
    output := make(chan string)

    go func() {
        defer close(output)
        var timer *time.Timer
        var lastValue string

        for {
            select {
            case val, ok := <-input:
                if !ok {
                    if timer != nil {
                        timer.Stop()
                    }
                    return
                }
                lastValue = val
                if timer == nil {
                    timer = time.NewTimer(delay)
                } else {
                    timer.Reset(delay)
                }
            case <-timer.C:
                output <- lastValue
                timer = nil
            }
        }
    }()

    return output
}

// Priority select pattern
func prioritySelect(high, low <-chan int) {
    for {
        select {
        case val := <-high:
            fmt.Println("High priority:", val)
        default:
            select {
            case val := <-high:
                fmt.Println("High priority:", val)
            case val := <-low:
                fmt.Println("Low priority:", val)
            }
        }
    }
}

// Broadcast pattern
type Broadcaster struct {
    mu        sync.RWMutex
    listeners []chan string
}

func NewBroadcaster() *Broadcaster {
    return &Broadcaster{}
}

func (b *Broadcaster) Subscribe() <-chan string {
    b.mu.Lock()
    defer b.mu.Unlock()

    ch := make(chan string, 10)
    b.listeners = append(b.listeners, ch)
    return ch
}

func (b *Broadcaster) Broadcast(msg string) {
    b.mu.RLock()
    defer b.mu.RUnlock()

    for _, ch := range b.listeners {
        select {
        case ch <- msg:
        default:
            // Skip slow listeners
        }
    }
}

func (b *Broadcaster) Close() {
    b.mu.Lock()
    defer b.mu.Unlock()

    for _, ch := range b.listeners {
        close(ch)
    }
    b.listeners = nil
}

// Dynamic select with reflect
import "reflect"

func dynamicSelect(channels []<-chan int) int {
    cases := make([]reflect.SelectCase, len(channels))

    for i, ch := range channels {
        cases[i] = reflect.SelectCase{
            Dir:  reflect.SelectRecv,
            Chan: reflect.ValueOf(ch),
        }
    }

    _, value, _ := reflect.Select(cases)
    return int(value.Int())
}

func main() {
    // Heartbeat
    ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
    defer cancel()

    hb := heartbeat(ctx, 500*time.Millisecond)

    for {
        select {
        case _, ok := <-hb:
            if !ok {
                fmt.Println("Heartbeat stopped")
                break
            }
            fmt.Println("Heartbeat received")
        case <-ctx.Done():
            fmt.Println("Context done")
            return
        }
    }

    // Rate limiter
    limiter := rateLimiter(5) // 5 requests per second

    for i := 0; i < 10; i++ {
        <-limiter
        fmt.Printf("Request %d at %s\n", i, time.Now().Format("15:04:05.000"))
    }

    // Broadcaster
    bc := NewBroadcaster()

    // Subscribers
    sub1 := bc.Subscribe()
    sub2 := bc.Subscribe()

    var wg sync.WaitGroup
    wg.Add(2)

    go func() {
        defer wg.Done()
        for msg := range sub1 {
            fmt.Println("Subscriber 1:", msg)
        }
    }()

    go func() {
        defer wg.Done()
        for msg := range sub2 {
            fmt.Println("Subscriber 2:", msg)
        }
    }()

    // Send broadcasts
    bc.Broadcast("Hello")
    bc.Broadcast("World")
    time.Sleep(100 * time.Millisecond)

    bc.Close()
    wg.Wait()
}
```

---

## Paquete sync

### Mutex y RWMutex

```go
package main

import (
    "fmt"
    "sync"
    "time"
)

// Counter with Mutex
type SafeCounter struct {
    mu    sync.Mutex
    value int
}

func (c *SafeCounter) Inc() {
    c.mu.Lock()
    defer c.mu.Unlock()
    c.value++
}

func (c *SafeCounter) Value() int {
    c.mu.Lock()
    defer c.mu.Unlock()
    return c.value
}

// Cache with RWMutex (multiple readers, single writer)
type Cache struct {
    mu   sync.RWMutex
    data map[string]string
}

func NewCache() *Cache {
    return &Cache{data: make(map[string]string)}
}

func (c *Cache) Get(key string) (string, bool) {
    c.mu.RLock() // Multiple readers allowed
    defer c.mu.RUnlock()
    val, ok := c.data[key]
    return val, ok
}

func (c *Cache) Set(key, value string) {
    c.mu.Lock() // Exclusive lock for writing
    defer c.mu.Unlock()
    c.data[key] = value
}

func (c *Cache) Delete(key string) {
    c.mu.Lock()
    defer c.mu.Unlock()
    delete(c.data, key)
}

func main() {
    // SafeCounter
    counter := &SafeCounter{}
    var wg sync.WaitGroup

    for i := 0; i < 1000; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            counter.Inc()
        }()
    }

    wg.Wait()
    fmt.Printf("Counter value: %d\n", counter.Value())

    // Cache with RWMutex
    cache := NewCache()

    // Writer goroutine
    go func() {
        for i := 0; i < 100; i++ {
            cache.Set(fmt.Sprintf("key%d", i), fmt.Sprintf("value%d", i))
            time.Sleep(10 * time.Millisecond)
        }
    }()

    // Multiple reader goroutines
    for i := 0; i < 5; i++ {
        go func(id int) {
            for j := 0; j < 100; j++ {
                key := fmt.Sprintf("key%d", j%100)
                if val, ok := cache.Get(key); ok {
                    fmt.Printf("Reader %d: %s=%s\n", id, key, val)
                }
                time.Sleep(5 * time.Millisecond)
            }
        }(i)
    }

    time.Sleep(2 * time.Second)
}
```

### WaitGroup y Once

```go
package main

import (
    "fmt"
    "sync"
    "time"
)

// WaitGroup for coordinating goroutines
func downloadFiles(urls []string) {
    var wg sync.WaitGroup

    for _, url := range urls {
        wg.Add(1)
        go func(url string) {
            defer wg.Done()
            fmt.Printf("Downloading: %s\n", url)
            time.Sleep(100 * time.Millisecond) // Simulate download
            fmt.Printf("Completed: %s\n", url)
        }(url)
    }

    wg.Wait()
    fmt.Println("All downloads completed")
}

// sync.Once for one-time initialization
type Singleton struct {
    value string
}

var (
    instance *Singleton
    once     sync.Once
)

func GetInstance() *Singleton {
    once.Do(func() {
        fmt.Println("Creating singleton instance...")
        instance = &Singleton{value: "initialized"}
    })
    return instance
}

// Lazy initialization with sync.Once
type ExpensiveResource struct {
    data []byte
}

type ResourceManager struct {
    resource *ExpensiveResource
    once     sync.Once
}

func (rm *ResourceManager) GetResource() *ExpensiveResource {
    rm.once.Do(func() {
        fmt.Println("Loading expensive resource...")
        rm.resource = &ExpensiveResource{
            data: make([]byte, 1024*1024), // 1MB
        }
    })
    return rm.resource
}

func main() {
    // WaitGroup example
    urls := []string{
        "https://example.com/file1",
        "https://example.com/file2",
        "https://example.com/file3",
    }
    downloadFiles(urls)

    // sync.Once example
    var wg sync.WaitGroup

    for i := 0; i < 5; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            singleton := GetInstance()
            fmt.Printf("Goroutine %d: %s\n", id, singleton.value)
        }(i)
    }

    wg.Wait()

    // Resource manager
    rm := &ResourceManager{}

    for i := 0; i < 3; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            resource := rm.GetResource()
            fmt.Printf("Goroutine %d: resource size = %d bytes\n", id, len(resource.data))
        }(i)
    }

    wg.Wait()
}
```

### Cond y Pool

```go
package main

import (
    "bytes"
    "fmt"
    "sync"
    "time"
)

// sync.Cond for conditional waiting
type Queue struct {
    mu    sync.Mutex
    cond  *sync.Cond
    items []int
}

func NewQueue() *Queue {
    q := &Queue{}
    q.cond = sync.NewCond(&q.mu)
    return q
}

func (q *Queue) Enqueue(item int) {
    q.mu.Lock()
    defer q.mu.Unlock()

    q.items = append(q.items, item)
    q.cond.Signal() // Wake one waiter
}

func (q *Queue) Dequeue() int {
    q.mu.Lock()
    defer q.mu.Unlock()

    for len(q.items) == 0 {
        q.cond.Wait() // Wait for signal
    }

    item := q.items[0]
    q.items = q.items[1:]
    return item
}

// Broadcast example
type Barrier struct {
    mu      sync.Mutex
    cond    *sync.Cond
    count   int
    parties int
}

func NewBarrier(parties int) *Barrier {
    b := &Barrier{parties: parties}
    b.cond = sync.NewCond(&b.mu)
    return b
}

func (b *Barrier) Wait() {
    b.mu.Lock()
    defer b.mu.Unlock()

    b.count++
    if b.count == b.parties {
        b.count = 0
        b.cond.Broadcast() // Wake all waiters
    } else {
        b.cond.Wait()
    }
}

// sync.Pool for object reuse
var bufferPool = sync.Pool{
    New: func() interface{} {
        fmt.Println("Creating new buffer")
        return new(bytes.Buffer)
    },
}

func processData(data string) string {
    // Get buffer from pool
    buf := bufferPool.Get().(*bytes.Buffer)
    buf.Reset() // Clear previous content

    // Use buffer
    buf.WriteString("Processed: ")
    buf.WriteString(data)
    result := buf.String()

    // Return buffer to pool
    bufferPool.Put(buf)

    return result
}

// sync.Map for concurrent map access
func syncMapExample() {
    var m sync.Map

    // Store
    m.Store("key1", "value1")
    m.Store("key2", "value2")
    m.Store("key3", 42)

    // Load
    if val, ok := m.Load("key1"); ok {
        fmt.Println("key1:", val)
    }

    // LoadOrStore
    actual, loaded := m.LoadOrStore("key4", "value4")
    fmt.Printf("key4: %v, was loaded: %v\n", actual, loaded)

    // Delete
    m.Delete("key2")

    // Range
    m.Range(func(key, value interface{}) bool {
        fmt.Printf("%v: %v\n", key, value)
        return true // Continue iteration
    })

    // LoadAndDelete (Go 1.15+)
    val, loaded := m.LoadAndDelete("key1")
    fmt.Printf("Deleted key1: %v, existed: %v\n", val, loaded)
}

func main() {
    // Queue with Cond
    q := NewQueue()

    // Producer
    go func() {
        for i := 1; i <= 5; i++ {
            time.Sleep(100 * time.Millisecond)
            fmt.Printf("Enqueuing: %d\n", i)
            q.Enqueue(i)
        }
    }()

    // Consumer
    go func() {
        for i := 0; i < 5; i++ {
            item := q.Dequeue()
            fmt.Printf("Dequeued: %d\n", item)
        }
    }()

    time.Sleep(1 * time.Second)

    // Barrier example
    barrier := NewBarrier(3)
    var wg sync.WaitGroup

    for i := 1; i <= 3; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            fmt.Printf("Goroutine %d: doing work\n", id)
            time.Sleep(time.Duration(id) * 100 * time.Millisecond)
            fmt.Printf("Goroutine %d: waiting at barrier\n", id)
            barrier.Wait()
            fmt.Printf("Goroutine %d: passed barrier\n", id)
        }(i)
    }

    wg.Wait()

    // Pool example
    fmt.Println("\n=== Pool Example ===")
    for i := 0; i < 5; i++ {
        result := processData(fmt.Sprintf("data%d", i))
        fmt.Println(result)
    }

    // sync.Map example
    fmt.Println("\n=== sync.Map Example ===")
    syncMapExample()
}
```

---

## Patrones de Concurrencia

### Context para Cancelación y Timeouts

```go
package main

import (
    "context"
    "errors"
    "fmt"
    "math/rand"
    "time"
)

// Service that respects context
func fetchFromDB(ctx context.Context, query string) (string, error) {
    // Simulate variable latency
    delay := time.Duration(rand.Intn(500)) * time.Millisecond

    select {
    case <-time.After(delay):
        return fmt.Sprintf("Result for: %s", query), nil
    case <-ctx.Done():
        return "", ctx.Err()
    }
}

// Chain of context-aware functions
func processRequest(ctx context.Context, userID string) error {
    // Add value to context
    ctx = context.WithValue(ctx, "requestID", "req-12345")

    // Step 1: Fetch user
    user, err := fetchUser(ctx, userID)
    if err != nil {
        return fmt.Errorf("fetch user: %w", err)
    }
    fmt.Println("Fetched user:", user)

    // Step 2: Fetch orders
    orders, err := fetchOrders(ctx, userID)
    if err != nil {
        return fmt.Errorf("fetch orders: %w", err)
    }
    fmt.Println("Fetched orders:", orders)

    return nil
}

func fetchUser(ctx context.Context, id string) (string, error) {
    requestID := ctx.Value("requestID")
    fmt.Printf("[%s] Fetching user %s\n", requestID, id)

    select {
    case <-time.After(100 * time.Millisecond):
        return fmt.Sprintf("User-%s", id), nil
    case <-ctx.Done():
        return "", ctx.Err()
    }
}

func fetchOrders(ctx context.Context, userID string) ([]string, error) {
    requestID := ctx.Value("requestID")
    fmt.Printf("[%s] Fetching orders for %s\n", requestID, userID)

    select {
    case <-time.After(150 * time.Millisecond):
        return []string{"order-1", "order-2"}, nil
    case <-ctx.Done():
        return nil, ctx.Err()
    }
}

// Parallel operations with context
func parallelFetch(ctx context.Context) ([]string, error) {
    type result struct {
        data string
        err  error
    }

    results := make(chan result, 3)

    // Launch parallel fetches
    for _, source := range []string{"source1", "source2", "source3"} {
        go func(src string) {
            data, err := fetchFromDB(ctx, src)
            results <- result{data, err}
        }(source)
    }

    // Collect results
    var allData []string
    for i := 0; i < 3; i++ {
        select {
        case r := <-results:
            if r.err != nil {
                return nil, r.err
            }
            allData = append(allData, r.data)
        case <-ctx.Done():
            return nil, ctx.Err()
        }
    }

    return allData, nil
}

func main() {
    // Context with timeout
    ctx, cancel := context.WithTimeout(context.Background(), 300*time.Millisecond)
    defer cancel()

    err := processRequest(ctx, "user-123")
    if err != nil {
        if errors.Is(err, context.DeadlineExceeded) {
            fmt.Println("Request timed out")
        } else if errors.Is(err, context.Canceled) {
            fmt.Println("Request was canceled")
        } else {
            fmt.Println("Error:", err)
        }
    } else {
        fmt.Println("Request completed successfully")
    }

    // Context with deadline
    deadline := time.Now().Add(500 * time.Millisecond)
    ctx2, cancel2 := context.WithDeadline(context.Background(), deadline)
    defer cancel2()

    results, err := parallelFetch(ctx2)
    if err != nil {
        fmt.Println("Parallel fetch error:", err)
    } else {
        fmt.Println("Parallel fetch results:", results)
    }

    // Manual cancellation
    ctx3, cancel3 := context.WithCancel(context.Background())

    go func() {
        time.Sleep(100 * time.Millisecond)
        cancel3() // Cancel after 100ms
    }()

    _, err = fetchFromDB(ctx3, "test")
    fmt.Println("Fetch with manual cancel:", err)
}
```

### Error Group Pattern

```go
package main

import (
    "context"
    "errors"
    "fmt"
    "sync"
)

// Simple errgroup implementation
type ErrGroup struct {
    wg   sync.WaitGroup
    err  error
    once sync.Once
    mu   sync.Mutex
}

func (g *ErrGroup) Go(f func() error) {
    g.wg.Add(1)
    go func() {
        defer g.wg.Done()
        if err := f(); err != nil {
            g.once.Do(func() {
                g.mu.Lock()
                g.err = err
                g.mu.Unlock()
            })
        }
    }()
}

func (g *ErrGroup) Wait() error {
    g.wg.Wait()
    return g.err
}

// ErrGroup with context
type ErrGroupWithContext struct {
    ctx    context.Context
    cancel context.CancelFunc
    wg     sync.WaitGroup
    err    error
    once   sync.Once
}

func WithContext(ctx context.Context) (*ErrGroupWithContext, context.Context) {
    ctx, cancel := context.WithCancel(ctx)
    return &ErrGroupWithContext{ctx: ctx, cancel: cancel}, ctx
}

func (g *ErrGroupWithContext) Go(f func() error) {
    g.wg.Add(1)
    go func() {
        defer g.wg.Done()
        if err := f(); err != nil {
            g.once.Do(func() {
                g.err = err
                g.cancel() // Cancel context on first error
            })
        }
    }()
}

func (g *ErrGroupWithContext) Wait() error {
    g.wg.Wait()
    if g.cancel != nil {
        g.cancel()
    }
    return g.err
}

// Example services
func fetchService1(ctx context.Context) error {
    select {
    case <-ctx.Done():
        fmt.Println("Service1 cancelled")
        return ctx.Err()
    default:
        fmt.Println("Service1 completed")
        return nil
    }
}

func fetchService2(ctx context.Context) error {
    return errors.New("service2 failed")
}

func fetchService3(ctx context.Context) error {
    select {
    case <-ctx.Done():
        fmt.Println("Service3 cancelled")
        return ctx.Err()
    default:
        fmt.Println("Service3 completed")
        return nil
    }
}

func main() {
    // Simple ErrGroup
    fmt.Println("=== Simple ErrGroup ===")
    g := &ErrGroup{}

    g.Go(func() error {
        fmt.Println("Task 1")
        return nil
    })

    g.Go(func() error {
        fmt.Println("Task 2")
        return errors.New("task 2 failed")
    })

    g.Go(func() error {
        fmt.Println("Task 3")
        return nil
    })

    if err := g.Wait(); err != nil {
        fmt.Println("Error:", err)
    }

    // ErrGroup with context
    fmt.Println("\n=== ErrGroup with Context ===")
    eg, ctx := WithContext(context.Background())

    eg.Go(func() error { return fetchService1(ctx) })
    eg.Go(func() error { return fetchService2(ctx) })
    eg.Go(func() error { return fetchService3(ctx) })

    if err := eg.Wait(); err != nil {
        fmt.Println("Error:", err)
    }
}
```

---

## Race Condition Detection

### Detectando Data Races

```go
package main

import (
    "fmt"
    "sync"
)

// RACE CONDITION: Unsynchronized counter
func unsafeCounter() {
    counter := 0
    var wg sync.WaitGroup

    for i := 0; i < 1000; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            counter++ // DATA RACE: multiple goroutines accessing without sync
        }()
    }

    wg.Wait()
    fmt.Println("Unsafe counter (may be wrong):", counter)
}

// SAFE: Counter with mutex
func safeCounterMutex() {
    var mu sync.Mutex
    counter := 0
    var wg sync.WaitGroup

    for i := 0; i < 1000; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            mu.Lock()
            counter++
            mu.Unlock()
        }()
    }

    wg.Wait()
    fmt.Println("Safe counter (mutex):", counter)
}

// SAFE: Counter with atomic
import "sync/atomic"

func safeCounterAtomic() {
    var counter int64 = 0
    var wg sync.WaitGroup

    for i := 0; i < 1000; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            atomic.AddInt64(&counter, 1)
        }()
    }

    wg.Wait()
    fmt.Println("Safe counter (atomic):", counter)
}

// SAFE: Counter with channel
func safeCounterChannel() {
    counter := 0
    incrementCh := make(chan struct{}, 1000)
    done := make(chan struct{})

    // Single goroutine owns the counter
    go func() {
        for range incrementCh {
            counter++
        }
        done <- struct{}{}
    }()

    var wg sync.WaitGroup
    for i := 0; i < 1000; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            incrementCh <- struct{}{}
        }()
    }

    wg.Wait()
    close(incrementCh)
    <-done

    fmt.Println("Safe counter (channel):", counter)
}

// RACE CONDITION: Concurrent map access
func unsafeMap() {
    m := make(map[int]int)
    var wg sync.WaitGroup

    for i := 0; i < 100; i++ {
        wg.Add(1)
        go func(i int) {
            defer wg.Done()
            m[i] = i * 2 // DATA RACE: concurrent map writes
        }(i)
    }

    wg.Wait()
}

// SAFE: sync.Map
func safeMapSync() {
    var m sync.Map
    var wg sync.WaitGroup

    for i := 0; i < 100; i++ {
        wg.Add(1)
        go func(i int) {
            defer wg.Done()
            m.Store(i, i*2)
        }(i)
    }

    wg.Wait()

    count := 0
    m.Range(func(key, value interface{}) bool {
        count++
        return true
    })
    fmt.Println("sync.Map entries:", count)
}

// Check-then-act race
func checkThenActRace() {
    var balance int64 = 100

    // RACE: check and update not atomic
    withdraw := func(amount int64) bool {
        if atomic.LoadInt64(&balance) >= amount {
            // Race window here!
            atomic.AddInt64(&balance, -amount)
            return true
        }
        return false
    }

    var wg sync.WaitGroup
    for i := 0; i < 10; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            if withdraw(20) {
                fmt.Println("Withdrew 20")
            }
        }()
    }

    wg.Wait()
    fmt.Println("Final balance:", balance)
}

// SAFE: Atomic compare-and-swap
func checkThenActSafe() {
    var balance int64 = 100

    withdraw := func(amount int64) bool {
        for {
            current := atomic.LoadInt64(&balance)
            if current < amount {
                return false
            }
            if atomic.CompareAndSwapInt64(&balance, current, current-amount) {
                return true
            }
            // CAS failed, retry
        }
    }

    var wg sync.WaitGroup
    for i := 0; i < 10; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            if withdraw(20) {
                fmt.Println("Withdrew 20")
            }
        }()
    }

    wg.Wait()
    fmt.Println("Final balance:", balance)
}

func main() {
    fmt.Println("Run with: go run -race main.go")
    fmt.Println()

    unsafeCounter()
    safeCounterMutex()
    safeCounterAtomic()
    safeCounterChannel()

    fmt.Println()
    // unsafeMap() // This will panic!
    safeMapSync()

    fmt.Println()
    checkThenActRace()
    checkThenActSafe()
}
```

```bash
# Run with race detector
go run -race main.go

# Test with race detector
go test -race ./...

# Build with race detector (for testing)
go build -race -o myapp ./...

# Race detector flags
GORACE="log_path=/tmp/race_log" go run -race main.go
```

---

## Comparación con C Threads

### Tabla Comparativa

| Aspecto | POSIX Threads (C) | Goroutines (Go) |
|---------|-------------------|-----------------|
| Creación | `pthread_create()` | `go func()` |
| Peso | ~1MB stack mínimo | ~2KB stack inicial |
| Scheduling | OS kernel | Go runtime (M:N) |
| Sincronización | Mutex, Semáforos | Channels, Mutex |
| Comunicación | Memoria compartida | Channels (CSP) |
| Escalabilidad | Miles de threads | Millones de goroutines |
| Complejidad | Alta | Baja |

### Ejemplos Comparativos

```c
/* C: POSIX Threads example */
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define NUM_THREADS 5

typedef struct {
    int thread_id;
    int value;
} ThreadData;

pthread_mutex_t counter_mutex = PTHREAD_MUTEX_INITIALIZER;
int shared_counter = 0;

void* worker(void* arg) {
    ThreadData* data = (ThreadData*)arg;

    printf("Thread %d starting with value %d\n",
           data->thread_id, data->value);

    // Critical section
    pthread_mutex_lock(&counter_mutex);
    shared_counter += data->value;
    pthread_mutex_unlock(&counter_mutex);

    printf("Thread %d done, counter = %d\n",
           data->thread_id, shared_counter);

    pthread_exit(NULL);
}

int main() {
    pthread_t threads[NUM_THREADS];
    ThreadData thread_data[NUM_THREADS];

    for (int i = 0; i < NUM_THREADS; i++) {
        thread_data[i].thread_id = i;
        thread_data[i].value = i * 10;

        int rc = pthread_create(&threads[i], NULL,
                                worker, &thread_data[i]);
        if (rc) {
            printf("Error creating thread %d\n", i);
            exit(1);
        }
    }

    // Wait for all threads
    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_join(threads[i], NULL);
    }

    printf("Final counter: %d\n", shared_counter);
    pthread_mutex_destroy(&counter_mutex);

    return 0;
}
```

```go
// Go: Equivalent with goroutines
package main

import (
    "fmt"
    "sync"
)

func main() {
    const numGoroutines = 5

    var mu sync.Mutex
    var wg sync.WaitGroup
    sharedCounter := 0

    for i := 0; i < numGoroutines; i++ {
        wg.Add(1)
        go func(id, value int) {
            defer wg.Done()

            fmt.Printf("Goroutine %d starting with value %d\n", id, value)

            // Critical section
            mu.Lock()
            sharedCounter += value
            mu.Unlock()

            fmt.Printf("Goroutine %d done\n", id)
        }(i, i*10)
    }

    wg.Wait()
    fmt.Printf("Final counter: %d\n", sharedCounter)
}
```

```c
/* C: Producer-Consumer with condition variable */
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define BUFFER_SIZE 10

int buffer[BUFFER_SIZE];
int count = 0;
int in = 0, out = 0;

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t not_full = PTHREAD_COND_INITIALIZER;
pthread_cond_t not_empty = PTHREAD_COND_INITIALIZER;

void* producer(void* arg) {
    for (int i = 0; i < 20; i++) {
        pthread_mutex_lock(&mutex);

        while (count == BUFFER_SIZE) {
            pthread_cond_wait(&not_full, &mutex);
        }

        buffer[in] = i;
        in = (in + 1) % BUFFER_SIZE;
        count++;
        printf("Produced: %d\n", i);

        pthread_cond_signal(&not_empty);
        pthread_mutex_unlock(&mutex);
    }
    return NULL;
}

void* consumer(void* arg) {
    for (int i = 0; i < 20; i++) {
        pthread_mutex_lock(&mutex);

        while (count == 0) {
            pthread_cond_wait(&not_empty, &mutex);
        }

        int item = buffer[out];
        out = (out + 1) % BUFFER_SIZE;
        count--;
        printf("Consumed: %d\n", item);

        pthread_cond_signal(&not_full);
        pthread_mutex_unlock(&mutex);
    }
    return NULL;
}

int main() {
    pthread_t prod, cons;

    pthread_create(&prod, NULL, producer, NULL);
    pthread_create(&cons, NULL, consumer, NULL);

    pthread_join(prod, NULL);
    pthread_join(cons, NULL);

    return 0;
}
```

```go
// Go: Producer-Consumer with channels
package main

import (
    "fmt"
    "sync"
)

func producer(ch chan<- int, wg *sync.WaitGroup) {
    defer wg.Done()
    defer close(ch)

    for i := 0; i < 20; i++ {
        ch <- i
        fmt.Printf("Produced: %d\n", i)
    }
}

func consumer(ch <-chan int, wg *sync.WaitGroup) {
    defer wg.Done()

    for item := range ch {
        fmt.Printf("Consumed: %d\n", item)
    }
}

func main() {
    ch := make(chan int, 10) // Buffered channel
    var wg sync.WaitGroup

    wg.Add(2)
    go producer(ch, &wg)
    go consumer(ch, &wg)

    wg.Wait()
}
```

---

## Mejores Prácticas

### Guía de Concurrencia

```go
package main

// 1. Prefer channels for communication between goroutines
// "Don't communicate by sharing memory; share memory by communicating"

// 2. Keep goroutines simple and focused
func worker(jobs <-chan Job, results chan<- Result) {
    for job := range jobs {
        results <- process(job)
    }
}

// 3. Always handle goroutine lifecycle
func startWorker(ctx context.Context) {
    go func() {
        for {
            select {
            case <-ctx.Done():
                return // Clean exit
            default:
                doWork()
            }
        }
    }()
}

// 4. Use context for cancellation and timeouts
func fetchWithContext(ctx context.Context, url string) ([]byte, error) {
    req, _ := http.NewRequestWithContext(ctx, "GET", url, nil)
    // ...
}

// 5. Close channels from sender, not receiver
func producer(ch chan<- int) {
    defer close(ch) // Sender closes
    for i := 0; i < 10; i++ {
        ch <- i
    }
}

// 6. Use sync.WaitGroup for coordinating goroutines
func processAll(items []Item) {
    var wg sync.WaitGroup
    for _, item := range items {
        wg.Add(1)
        go func(it Item) {
            defer wg.Done()
            process(it)
        }(item)
    }
    wg.Wait()
}

// 7. Prefer sync.RWMutex for read-heavy workloads
type Cache struct {
    mu   sync.RWMutex
    data map[string]string
}

func (c *Cache) Get(key string) string {
    c.mu.RLock()
    defer c.mu.RUnlock()
    return c.data[key]
}

// 8. Use atomic operations for simple counters
import "sync/atomic"

var counter int64

func increment() {
    atomic.AddInt64(&counter, 1)
}

// 9. Avoid goroutine leaks
func fetchWithTimeout(url string) (string, error) {
    ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel() // Always call cancel

    resultCh := make(chan string, 1) // Buffered to prevent leak

    go func() {
        result := fetch(url)
        select {
        case resultCh <- result:
        case <-ctx.Done():
            // Context cancelled, result discarded
        }
    }()

    select {
    case result := <-resultCh:
        return result, nil
    case <-ctx.Done():
        return "", ctx.Err()
    }
}

// 10. Use -race flag during development
// go test -race ./...
// go run -race main.go

// 11. Limit concurrency with semaphores
func limitedConcurrency(tasks []Task, maxConcurrent int) {
    sem := make(chan struct{}, maxConcurrent)
    var wg sync.WaitGroup

    for _, task := range tasks {
        wg.Add(1)
        sem <- struct{}{} // Acquire semaphore

        go func(t Task) {
            defer wg.Done()
            defer func() { <-sem }() // Release semaphore

            process(t)
        }(task)
    }

    wg.Wait()
}

// 12. Document goroutine ownership
// Package-level: document which goroutines own which resources
// Function-level: document if function spawns goroutines
```

### Herramientas de Análisis

```bash
# Race detector
go build -race
go test -race ./...

# Deadlock detection (external tools)
go get -u github.com/sasha-s/go-deadlock

# Profile goroutines
go tool pprof http://localhost:6060/debug/pprof/goroutine

# Trace execution
go test -trace=trace.out ./...
go tool trace trace.out
```

---

## Referencias

- [Go Concurrency Patterns](https://go.dev/blog/pipelines)
- [Advanced Go Concurrency Patterns](https://go.dev/blog/io2013-talk-concurrency)
- [The Go Memory Model](https://go.dev/ref/mem)
- [Go Data Race Detector](https://go.dev/doc/articles/race_detector)
- [Concurrency is not Parallelism](https://go.dev/blog/waza-talk)

---

*ARCHAEON_CORE - Preservando la evolución del código*
*De C threads a Goroutines: Concurrencia moderna*

---
título: "RUST 04 - Concurrencia"
versión: "1.0.0"
fecha_creación: "2025-12-31"
última_actualización: "2025-12-31"
autor: "ARCHAEON_CORE"
dominio: "Lenguajes Modernos / Rust"
especialización: "Concurrencia y Paralelismo"
contexto_soul_core: "ARCHAEON - Evolución C → Rust"
tags:
  - rust
  - concurrency
  - threads
  - async
  - tokio
  - channels
  - synchronization
  - parallelism
dependencias:
  - rust_fundamentos: "RUST_01_FUNDAMENTOS.md"
  - rust_sistemas: "RUST_02_SISTEMAS.md"
  - conocimiento_c: "avanzado"
---

# RUST 04: CONCURRENCIA

## Índice de Contenidos

1. [Modelo de Concurrencia Segura](#1-modelo-de-concurrencia-segura)
2. [Threads y Thread Safety](#2-threads-y-thread-safety)
3. [Channels (mpsc)](#3-channels-mpsc)
4. [Async/Await y Tokio](#4-asyncawait-y-tokio)
5. [Primitivas de Sincronización](#5-primitivas-de-sincronización)
6. [Comparación con C pthreads](#6-comparación-con-c-pthreads)
7. [Patrones Avanzados](#7-patrones-avanzados)
8. [Tablas de Referencia](#8-tablas-de-referencia)

---

## 1. Modelo de Concurrencia Segura

### 1.1 Fearless Concurrency

Rust garantiza seguridad en concurrencia en tiempo de compilación a través
de su sistema de ownership y traits Send/Sync.

```rust
use std::thread;

fn fearless_concurrency() {
    let data = vec![1, 2, 3, 4, 5];

    // This would fail to compile - data moved into closure
    // let handle = thread::spawn(|| {
    //     println!("{:?}", data);
    // });
    // println!("{:?}", data);  // ERROR: data was moved

    // Solution 1: Move ownership
    let data = vec![1, 2, 3, 4, 5];
    let handle = thread::spawn(move || {
        println!("In thread: {:?}", data);
    });
    handle.join().unwrap();
    // data is no longer accessible here

    // Solution 2: Clone the data
    let data = vec![1, 2, 3, 4, 5];
    let data_clone = data.clone();
    let handle = thread::spawn(move || {
        println!("In thread: {:?}", data_clone);
    });
    println!("In main: {:?}", data);
    handle.join().unwrap();
}
```

### 1.2 Send y Sync Traits

```rust
use std::rc::Rc;
use std::sync::Arc;
use std::cell::RefCell;
use std::sync::Mutex;
use std::thread;

// Send: Safe to transfer ownership between threads
// Sync: Safe to share references between threads

fn send_sync_examples() {
    // i32 is Send + Sync
    let x: i32 = 42;
    thread::spawn(move || {
        println!("{}", x);
    }).join().unwrap();

    // Rc is NOT Send (not thread-safe reference counting)
    let rc = Rc::new(42);
    // thread::spawn(move || {
    //     println!("{}", rc);  // ERROR: Rc is not Send
    // });

    // Arc IS Send + Sync (atomic reference counting)
    let arc = Arc::new(42);
    let arc_clone = Arc::clone(&arc);
    thread::spawn(move || {
        println!("{}", arc_clone);
    }).join().unwrap();

    // RefCell is NOT Sync (interior mutability not thread-safe)
    let cell = RefCell::new(42);
    // Cannot share &RefCell between threads

    // Mutex IS Sync (provides safe interior mutability)
    let mutex = Arc::new(Mutex::new(42));
    let mutex_clone = Arc::clone(&mutex);
    thread::spawn(move || {
        let mut guard = mutex_clone.lock().unwrap();
        *guard += 1;
    }).join().unwrap();
}

// Custom types inherit Send/Sync from their fields
struct MySendType {
    data: Vec<i32>,  // Vec is Send + Sync
}
// MySendType is automatically Send + Sync

struct MyNonSendType {
    data: Rc<i32>,  // Rc is not Send
}
// MyNonSendType is NOT Send

// Opt-out of automatic Send/Sync (rare)
struct NotThreadSafe {
    data: *mut i32,  // Raw pointer is not Send
}

// Force Send/Sync (UNSAFE - must guarantee safety)
unsafe impl Send for NotThreadSafe {}
unsafe impl Sync for NotThreadSafe {}
```

### 1.3 Data Race Prevention

```rust
use std::sync::{Arc, Mutex};
use std::thread;

fn data_race_prevention() {
    // This would be a data race in C - compiler prevents in Rust
    let mut counter = 0;

    // ERROR: cannot borrow as mutable across threads
    // let handles: Vec<_> = (0..10).map(|_| {
    //     thread::spawn(|| {
    //         counter += 1;  // Not allowed!
    //     })
    // }).collect();

    // Correct: Use Mutex for thread-safe mutation
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            let mut num = counter.lock().unwrap();
            *num += 1;
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Counter: {}", *counter.lock().unwrap());
}
```

---

## 2. Threads y Thread Safety

### 2.1 Creación y Manejo de Threads

```rust
use std::thread;
use std::time::Duration;

fn basic_threads() {
    // Spawn a thread
    let handle = thread::spawn(|| {
        for i in 1..10 {
            println!("Thread: count {}", i);
            thread::sleep(Duration::from_millis(100));
        }
    });

    // Main thread continues
    for i in 1..5 {
        println!("Main: count {}", i);
        thread::sleep(Duration::from_millis(150));
    }

    // Wait for thread to finish
    handle.join().unwrap();
}

// Named threads
fn named_threads() {
    let builder = thread::Builder::new()
        .name("worker".into())
        .stack_size(8 * 1024 * 1024);  // 8MB stack

    let handle = builder.spawn(|| {
        println!("Thread name: {:?}", thread::current().name());
    }).unwrap();

    handle.join().unwrap();
}

// Getting thread info
fn thread_info() {
    println!("Main thread ID: {:?}", thread::current().id());
    println!("Number of CPUs: {}", thread::available_parallelism().unwrap());

    let handle = thread::spawn(|| {
        println!("Child thread ID: {:?}", thread::current().id());
    });

    handle.join().unwrap();
}
```

### 2.2 Scoped Threads

```rust
use std::thread;

fn scoped_threads() {
    let mut data = vec![1, 2, 3, 4, 5];

    thread::scope(|s| {
        // Scoped threads can borrow from outer scope
        s.spawn(|| {
            println!("First: {:?}", &data[..2]);
        });

        s.spawn(|| {
            println!("Last: {:?}", &data[3..]);
        });

        // Even mutable borrows with proper splitting
        let (first, second) = data.split_at_mut(2);

        s.spawn(|| {
            first[0] = 10;
        });

        s.spawn(|| {
            second[0] = 30;
        });

        // All threads joined automatically when scope ends
    });

    // Data is accessible again
    println!("Final: {:?}", data);
}

// Parallel iteration with scoped threads
fn parallel_map() {
    let numbers: Vec<i32> = (1..=100).collect();
    let mut results = vec![0; numbers.len()];

    let chunk_size = numbers.len() / 4;

    thread::scope(|s| {
        for (chunk_idx, (nums, res)) in numbers
            .chunks(chunk_size)
            .zip(results.chunks_mut(chunk_size))
            .enumerate()
        {
            s.spawn(move || {
                for (i, &num) in nums.iter().enumerate() {
                    res[i] = num * num;
                }
                println!("Chunk {} processed", chunk_idx);
            });
        }
    });

    println!("First 10 results: {:?}", &results[..10]);
}
```

### 2.3 Thread Local Storage

```rust
use std::cell::RefCell;
use std::thread;

// Thread-local data
thread_local! {
    static THREAD_DATA: RefCell<Vec<i32>> = RefCell::new(Vec::new());
    static THREAD_ID: RefCell<u32> = RefCell::new(0);
}

fn thread_local_storage() {
    // Initialize in main thread
    THREAD_ID.with(|id| {
        *id.borrow_mut() = 1;
    });

    let handle = thread::spawn(|| {
        // Each thread has its own copy
        THREAD_ID.with(|id| {
            *id.borrow_mut() = 2;
        });

        THREAD_DATA.with(|data| {
            data.borrow_mut().push(100);
            data.borrow_mut().push(200);
            println!("Thread 2 data: {:?}", data.borrow());
        });
    });

    THREAD_DATA.with(|data| {
        data.borrow_mut().push(1);
        data.borrow_mut().push(2);
        println!("Thread 1 data: {:?}", data.borrow());
    });

    handle.join().unwrap();
}

// Thread-local with initialization
thread_local! {
    static BUFFER: RefCell<Vec<u8>> = RefCell::new(vec![0; 1024]);
}

fn use_thread_local_buffer() {
    BUFFER.with(|buf| {
        let mut buffer = buf.borrow_mut();
        // Use the buffer...
        buffer[0] = 42;
    });
}
```

---

## 3. Channels (mpsc)

### 3.1 Basic Channel Usage

```rust
use std::sync::mpsc;
use std::thread;

fn basic_channel() {
    // Create a channel
    let (tx, rx) = mpsc::channel();

    // Spawn producer thread
    thread::spawn(move || {
        let msg = String::from("Hello from thread");
        tx.send(msg).unwrap();
        // msg is moved into channel, can't use it here
    });

    // Receive in main thread
    let received = rx.recv().unwrap();
    println!("Got: {}", received);
}

fn multiple_messages() {
    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        let messages = vec![
            String::from("one"),
            String::from("two"),
            String::from("three"),
        ];

        for msg in messages {
            tx.send(msg).unwrap();
            thread::sleep(std::time::Duration::from_millis(100));
        }
    });

    // Receive using iterator
    for received in rx {
        println!("Got: {}", received);
    }
}
```

### 3.2 Multiple Producers

```rust
use std::sync::mpsc;
use std::thread;

fn multiple_producers() {
    let (tx, rx) = mpsc::channel();

    // Clone the sender for each producer
    for i in 0..4 {
        let tx_clone = tx.clone();

        thread::spawn(move || {
            let msg = format!("Message from thread {}", i);
            tx_clone.send(msg).unwrap();
        });
    }

    // Drop original sender so receiver knows when all done
    drop(tx);

    // Collect all messages
    for msg in rx {
        println!("{}", msg);
    }
}

// Structured messages with enum
enum WorkerMessage {
    Task(i32),
    Result(i32),
    Done,
}

fn structured_channel() {
    let (tx, rx) = mpsc::channel();

    let worker = thread::spawn(move || {
        loop {
            match rx.recv() {
                Ok(WorkerMessage::Task(n)) => {
                    println!("Processing task: {}", n);
                }
                Ok(WorkerMessage::Done) => {
                    println!("Worker shutting down");
                    break;
                }
                Ok(WorkerMessage::Result(_)) => {}
                Err(_) => break,
            }
        }
    });

    tx.send(WorkerMessage::Task(1)).unwrap();
    tx.send(WorkerMessage::Task(2)).unwrap();
    tx.send(WorkerMessage::Done).unwrap();

    worker.join().unwrap();
}
```

### 3.3 Sync Channel (Bounded)

```rust
use std::sync::mpsc;
use std::thread;

fn sync_channel() {
    // Bounded channel with capacity 2
    let (tx, rx) = mpsc::sync_channel(2);

    thread::spawn(move || {
        for i in 0..5 {
            println!("Sending: {}", i);
            tx.send(i).unwrap();  // Blocks if channel full
            println!("Sent: {}", i);
        }
    });

    thread::sleep(std::time::Duration::from_millis(500));

    for msg in rx {
        println!("Received: {}", msg);
        thread::sleep(std::time::Duration::from_millis(100));
    }
}

// Backpressure handling
fn with_backpressure() {
    let (tx, rx) = mpsc::sync_channel(10);

    let producer = thread::spawn(move || {
        for i in 0..100 {
            match tx.try_send(i) {
                Ok(()) => println!("Sent {}", i),
                Err(mpsc::TrySendError::Full(val)) => {
                    println!("Channel full, dropping {}", val);
                }
                Err(mpsc::TrySendError::Disconnected(_)) => break,
            }
            thread::sleep(std::time::Duration::from_millis(10));
        }
    });

    let consumer = thread::spawn(move || {
        loop {
            match rx.recv_timeout(std::time::Duration::from_millis(500)) {
                Ok(val) => println!("Received {}", val),
                Err(mpsc::RecvTimeoutError::Timeout) => {
                    println!("Timeout, continuing...");
                }
                Err(mpsc::RecvTimeoutError::Disconnected) => break,
            }
        }
    });

    producer.join().unwrap();
    consumer.join().unwrap();
}
```

### 3.4 Crossbeam Channels

```rust
use crossbeam_channel::{bounded, select, unbounded, Receiver, Sender};
use std::thread;
use std::time::Duration;

fn crossbeam_basics() {
    // Unbounded channel (like mpsc::channel)
    let (tx, rx): (Sender<i32>, Receiver<i32>) = unbounded();

    tx.send(1).unwrap();
    tx.send(2).unwrap();

    assert_eq!(rx.recv().unwrap(), 1);
    assert_eq!(rx.recv().unwrap(), 2);

    // Bounded channel with capacity
    let (tx, rx) = bounded(5);

    // Non-blocking operations
    tx.try_send(1).unwrap();
    assert_eq!(rx.try_recv().unwrap(), 1);
}

fn crossbeam_select() {
    let (tx1, rx1) = bounded(1);
    let (tx2, rx2) = bounded(1);

    thread::spawn(move || {
        thread::sleep(Duration::from_millis(100));
        tx1.send("from 1").unwrap();
    });

    thread::spawn(move || {
        thread::sleep(Duration::from_millis(50));
        tx2.send("from 2").unwrap();
    });

    // Select from multiple channels
    select! {
        recv(rx1) -> msg => println!("rx1: {:?}", msg),
        recv(rx2) -> msg => println!("rx2: {:?}", msg),
        default(Duration::from_millis(200)) => println!("timeout"),
    }
}

// Fan-out / Fan-in pattern
fn fan_out_fan_in() {
    let (task_tx, task_rx) = bounded(100);
    let (result_tx, result_rx) = bounded(100);

    // Spawn workers
    for worker_id in 0..4 {
        let task_rx = task_rx.clone();
        let result_tx = result_tx.clone();

        thread::spawn(move || {
            while let Ok(task) = task_rx.recv() {
                let result = format!("Worker {} processed {}", worker_id, task);
                result_tx.send(result).unwrap();
            }
        });
    }

    // Send tasks
    for i in 0..20 {
        task_tx.send(i).unwrap();
    }
    drop(task_tx);

    // Collect results
    drop(result_tx);
    for result in result_rx {
        println!("{}", result);
    }
}
```

---

## 4. Async/Await y Tokio

### 4.1 Fundamentos de Async

```rust
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll};

// Async functions return impl Future
async fn hello_async() -> String {
    "Hello, async world!".to_string()
}

// Equivalent manual implementation
fn hello_manual() -> impl Future<Output = String> {
    async {
        "Hello, manual async!".to_string()
    }
}

// Custom future
struct Delay {
    when: std::time::Instant,
}

impl Future for Delay {
    type Output = &'static str;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if std::time::Instant::now() >= self.when {
            Poll::Ready("done!")
        } else {
            // Schedule wake-up
            let waker = cx.waker().clone();
            let when = self.when;

            std::thread::spawn(move || {
                let now = std::time::Instant::now();
                if now < when {
                    std::thread::sleep(when - now);
                }
                waker.wake();
            });

            Poll::Pending
        }
    }
}

async fn use_delay() {
    let delay = Delay {
        when: std::time::Instant::now() + std::time::Duration::from_secs(1),
    };

    println!("Waiting...");
    let result = delay.await;
    println!("Delay complete: {}", result);
}
```

### 4.2 Tokio Runtime

```rust
use tokio::time::{sleep, Duration};

// Basic tokio main
#[tokio::main]
async fn main() {
    println!("Hello from tokio!");

    // Spawn tasks
    let handle = tokio::spawn(async {
        sleep(Duration::from_millis(100)).await;
        "Task completed"
    });

    let result = handle.await.unwrap();
    println!("{}", result);
}

// Manual runtime creation
fn manual_runtime() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    rt.block_on(async {
        println!("Running on runtime");
    });
}

// Multi-threaded runtime
fn multi_threaded() {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(4)
        .enable_all()
        .build()
        .unwrap();

    rt.block_on(async {
        let mut handles = vec![];

        for i in 0..10 {
            handles.push(tokio::spawn(async move {
                sleep(Duration::from_millis(100)).await;
                format!("Task {} done", i)
            }));
        }

        for handle in handles {
            println!("{}", handle.await.unwrap());
        }
    });
}
```

### 4.3 Async I/O

```rust
use tokio::fs::File;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt};

async fn async_file_io() -> io::Result<()> {
    // Write file
    let mut file = File::create("hello.txt").await?;
    file.write_all(b"Hello, async I/O!").await?;

    // Read file
    let mut file = File::open("hello.txt").await?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).await?;
    println!("File contents: {}", contents);

    Ok(())
}

use tokio::net::{TcpListener, TcpStream};

async fn tcp_server() -> io::Result<()> {
    let listener = TcpListener::bind("127.0.0.1:8080").await?;

    loop {
        let (socket, addr) = listener.accept().await?;
        println!("Connection from: {}", addr);

        tokio::spawn(async move {
            handle_connection(socket).await;
        });
    }
}

async fn handle_connection(mut socket: TcpStream) {
    let mut buffer = [0u8; 1024];

    loop {
        match socket.read(&mut buffer).await {
            Ok(0) => break,  // Connection closed
            Ok(n) => {
                // Echo back
                if socket.write_all(&buffer[..n]).await.is_err() {
                    break;
                }
            }
            Err(_) => break,
        }
    }
}
```

### 4.4 Async Channels

```rust
use tokio::sync::mpsc;

async fn tokio_channels() {
    // Bounded channel
    let (tx, mut rx) = mpsc::channel(100);

    tokio::spawn(async move {
        for i in 0..10 {
            tx.send(i).await.unwrap();
        }
    });

    while let Some(value) = rx.recv().await {
        println!("Received: {}", value);
    }
}

use tokio::sync::broadcast;

async fn broadcast_channel() {
    let (tx, _) = broadcast::channel(16);

    let mut rx1 = tx.subscribe();
    let mut rx2 = tx.subscribe();

    tokio::spawn(async move {
        while let Ok(msg) = rx1.recv().await {
            println!("Receiver 1: {}", msg);
        }
    });

    tokio::spawn(async move {
        while let Ok(msg) = rx2.recv().await {
            println!("Receiver 2: {}", msg);
        }
    });

    tx.send("Hello to all!").unwrap();
    tx.send("Goodbye!").unwrap();

    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
}

use tokio::sync::watch;

async fn watch_channel() {
    // Single value that can be watched for changes
    let (tx, mut rx) = watch::channel("initial");

    tokio::spawn(async move {
        loop {
            if rx.changed().await.is_err() {
                break;
            }
            println!("Value changed: {}", *rx.borrow());
        }
    });

    tx.send("first update").unwrap();
    tx.send("second update").unwrap();
}
```

### 4.5 Async Patterns

```rust
use tokio::time::{timeout, Duration};
use futures::future::{join_all, select_all};

async fn concurrent_patterns() {
    // Run multiple futures concurrently
    let (r1, r2, r3) = tokio::join!(
        async { 1 },
        async { 2 },
        async { 3 },
    );
    println!("Results: {}, {}, {}", r1, r2, r3);

    // Race: first to complete wins
    tokio::select! {
        _ = tokio::time::sleep(Duration::from_millis(100)) => {
            println!("Timer completed first");
        }
        result = async { 42 } => {
            println!("Computation completed first: {}", result);
        }
    }

    // Timeout
    match timeout(Duration::from_millis(100), async {
        tokio::time::sleep(Duration::from_millis(200)).await;
        "completed"
    }).await {
        Ok(result) => println!("Got result: {}", result),
        Err(_) => println!("Timed out"),
    }
}

// Parallel iteration
async fn parallel_fetch(urls: Vec<&str>) -> Vec<String> {
    let futures: Vec<_> = urls.into_iter()
        .map(|url| async move {
            // Simulated fetch
            tokio::time::sleep(Duration::from_millis(100)).await;
            format!("Response from {}", url)
        })
        .collect();

    join_all(futures).await
}
```

---

## 5. Primitivas de Sincronización

### 5.1 Mutex

```rust
use std::sync::{Arc, Mutex};
use std::thread;

fn mutex_basic() {
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            // Lock returns MutexGuard (RAII)
            let mut num = counter.lock().unwrap();
            *num += 1;
            // Lock released when guard goes out of scope
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Result: {}", *counter.lock().unwrap());
}

// Handling poisoned mutex
fn handle_poison() {
    let mutex = Arc::new(Mutex::new(0));
    let mutex_clone = Arc::clone(&mutex);

    // This thread will panic while holding the lock
    let handle = thread::spawn(move || {
        let _guard = mutex_clone.lock().unwrap();
        panic!("Oh no!");
    });

    let _ = handle.join();  // Ignore panic

    // Mutex is now poisoned
    match mutex.lock() {
        Ok(guard) => println!("Got value: {}", *guard),
        Err(poisoned) => {
            // Can still access data
            let guard = poisoned.into_inner();
            println!("Recovered value: {}", *guard);
        }
    }
}

// Try lock (non-blocking)
fn try_lock_example() {
    let mutex = Arc::new(Mutex::new(0));

    match mutex.try_lock() {
        Ok(mut guard) => {
            *guard += 1;
        }
        Err(_) => {
            println!("Couldn't acquire lock");
        }
    }
}
```

### 5.2 RwLock

```rust
use std::sync::{Arc, RwLock};
use std::thread;

fn rwlock_example() {
    let data = Arc::new(RwLock::new(vec![1, 2, 3]));
    let mut handles = vec![];

    // Multiple readers
    for i in 0..3 {
        let data = Arc::clone(&data);
        handles.push(thread::spawn(move || {
            let read_guard = data.read().unwrap();
            println!("Reader {}: {:?}", i, *read_guard);
        }));
    }

    // Single writer
    {
        let data = Arc::clone(&data);
        handles.push(thread::spawn(move || {
            let mut write_guard = data.write().unwrap();
            write_guard.push(4);
            println!("Writer: added element");
        }));
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Final: {:?}", *data.read().unwrap());
}
```

### 5.3 Condition Variables

```rust
use std::sync::{Arc, Condvar, Mutex};
use std::thread;

fn condvar_example() {
    let pair = Arc::new((Mutex::new(false), Condvar::new()));
    let pair_clone = Arc::clone(&pair);

    // Waiting thread
    let handle = thread::spawn(move || {
        let (lock, cvar) = &*pair_clone;
        let mut ready = lock.lock().unwrap();

        while !*ready {
            ready = cvar.wait(ready).unwrap();
        }

        println!("Worker: received signal, proceeding");
    });

    // Signaling thread
    thread::sleep(std::time::Duration::from_millis(100));
    {
        let (lock, cvar) = &*pair;
        let mut ready = lock.lock().unwrap();
        *ready = true;
        println!("Main: signaling worker");
        cvar.notify_one();
    }

    handle.join().unwrap();
}

// Producer-consumer with condvar
fn producer_consumer() {
    let queue = Arc::new((Mutex::new(Vec::new()), Condvar::new()));

    let queue_producer = Arc::clone(&queue);
    let producer = thread::spawn(move || {
        for i in 0..5 {
            let (lock, cvar) = &*queue_producer;
            let mut q = lock.lock().unwrap();
            q.push(i);
            println!("Produced: {}", i);
            cvar.notify_one();
            drop(q);
            thread::sleep(std::time::Duration::from_millis(50));
        }
    });

    let queue_consumer = Arc::clone(&queue);
    let consumer = thread::spawn(move || {
        for _ in 0..5 {
            let (lock, cvar) = &*queue_consumer;
            let mut q = lock.lock().unwrap();

            while q.is_empty() {
                q = cvar.wait(q).unwrap();
            }

            let item = q.remove(0);
            println!("Consumed: {}", item);
        }
    });

    producer.join().unwrap();
    consumer.join().unwrap();
}
```

### 5.4 Atomics

```rust
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;

fn atomic_counter() {
    let counter = Arc::new(AtomicUsize::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        handles.push(thread::spawn(move || {
            for _ in 0..1000 {
                counter.fetch_add(1, Ordering::SeqCst);
            }
        }));
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Counter: {}", counter.load(Ordering::SeqCst));
}

// Spinlock implementation
struct SpinLock {
    locked: AtomicBool,
}

impl SpinLock {
    fn new() -> Self {
        SpinLock {
            locked: AtomicBool::new(false),
        }
    }

    fn lock(&self) {
        while self.locked.compare_exchange_weak(
            false,
            true,
            Ordering::Acquire,
            Ordering::Relaxed,
        ).is_err() {
            std::hint::spin_loop();
        }
    }

    fn unlock(&self) {
        self.locked.store(false, Ordering::Release);
    }
}

// Memory ordering explained
fn ordering_examples() {
    let flag = AtomicBool::new(false);
    let data = AtomicUsize::new(0);

    // Relaxed: No synchronization, fastest
    data.store(42, Ordering::Relaxed);

    // Release: All prior writes visible after this store
    data.store(42, Ordering::Release);

    // Acquire: All subsequent reads see writes before Release
    let _ = data.load(Ordering::Acquire);

    // SeqCst: Total ordering, strictest
    data.store(42, Ordering::SeqCst);
}
```

### 5.5 Barrier

```rust
use std::sync::{Arc, Barrier};
use std::thread;

fn barrier_example() {
    let barrier = Arc::new(Barrier::new(4));
    let mut handles = vec![];

    for i in 0..4 {
        let barrier = Arc::clone(&barrier);
        handles.push(thread::spawn(move || {
            println!("Thread {} before barrier", i);

            // All threads wait here until all arrive
            barrier.wait();

            println!("Thread {} after barrier", i);
        }));
    }

    for handle in handles {
        handle.join().unwrap();
    }
}

// Phased computation
fn phased_computation() {
    let barrier = Arc::new(Barrier::new(3));
    let data = Arc::new(Mutex::new(vec![0; 9]));

    use std::sync::Mutex;

    for phase in 0..3 {
        let mut handles = vec![];

        for worker in 0..3 {
            let barrier = Arc::clone(&barrier);
            let data = Arc::clone(&data);

            handles.push(thread::spawn(move || {
                // Do work
                {
                    let mut d = data.lock().unwrap();
                    d[phase * 3 + worker] = phase * 10 + worker;
                }

                println!("Worker {} finished phase {}", worker, phase);
                barrier.wait();
            }));
        }

        for handle in handles {
            handle.join().unwrap();
        }

        println!("Phase {} complete", phase);
    }
}
```

---

## 6. Comparación con C pthreads

### 6.1 Thread Creation

```c
// C: pthread creation
#include <pthread.h>
#include <stdio.h>

void* thread_func(void* arg) {
    int* num = (int*)arg;
    printf("Thread received: %d\n", *num);
    return NULL;
}

int main() {
    pthread_t thread;
    int arg = 42;

    // Create thread
    if (pthread_create(&thread, NULL, thread_func, &arg) != 0) {
        perror("pthread_create failed");
        return 1;
    }

    // Join thread
    pthread_join(thread, NULL);

    return 0;
}
```

```rust
// Rust: thread creation
use std::thread;

fn main() {
    let arg = 42;

    // Create thread - ownership is clear
    let handle = thread::spawn(move || {
        println!("Thread received: {}", arg);
    });

    // Join thread - Result handles errors
    handle.join().unwrap();
}
```

### 6.2 Mutex Comparison

```c
// C: pthread mutex
#include <pthread.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int shared_data = 0;

void* increment(void* arg) {
    for (int i = 0; i < 10000; i++) {
        pthread_mutex_lock(&mutex);
        shared_data++;
        pthread_mutex_unlock(&mutex);
        // Easy to forget unlock!
    }
    return NULL;
}

int main() {
    pthread_t t1, t2;
    pthread_create(&t1, NULL, increment, NULL);
    pthread_create(&t2, NULL, increment, NULL);
    pthread_join(t1, NULL);
    pthread_join(t2, NULL);
    printf("Result: %d\n", shared_data);
    pthread_mutex_destroy(&mutex);
    return 0;
}
```

```rust
// Rust: mutex with automatic unlock
use std::sync::{Arc, Mutex};
use std::thread;

fn main() {
    let shared_data = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..2 {
        let data = Arc::clone(&shared_data);
        handles.push(thread::spawn(move || {
            for _ in 0..10000 {
                let mut guard = data.lock().unwrap();
                *guard += 1;
                // Automatic unlock when guard goes out of scope
            }
        }));
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Result: {}", *shared_data.lock().unwrap());
    // No manual cleanup needed
}
```

### 6.3 Condition Variables

```c
// C: pthread condition variable
#include <pthread.h>
#include <stdbool.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t cond = PTHREAD_COND_INITIALIZER;
bool ready = false;

void* waiter(void* arg) {
    pthread_mutex_lock(&mutex);
    while (!ready) {
        pthread_cond_wait(&cond, &mutex);
    }
    printf("Received signal\n");
    pthread_mutex_unlock(&mutex);
    return NULL;
}

void* signaler(void* arg) {
    pthread_mutex_lock(&mutex);
    ready = true;
    pthread_cond_signal(&cond);
    pthread_mutex_unlock(&mutex);
    return NULL;
}
```

```rust
// Rust: condition variable
use std::sync::{Arc, Condvar, Mutex};
use std::thread;

fn main() {
    let pair = Arc::new((Mutex::new(false), Condvar::new()));

    let pair2 = Arc::clone(&pair);
    let waiter = thread::spawn(move || {
        let (lock, cvar) = &*pair2;
        let mut ready = lock.lock().unwrap();
        while !*ready {
            ready = cvar.wait(ready).unwrap();
        }
        println!("Received signal");
    });

    let (lock, cvar) = &*pair;
    {
        let mut ready = lock.lock().unwrap();
        *ready = true;
    }
    cvar.notify_one();

    waiter.join().unwrap();
}
```

---

## 7. Patrones Avanzados

### 7.1 Thread Pool

```rust
use std::sync::{mpsc, Arc, Mutex};
use std::thread;

type Job = Box<dyn FnOnce() + Send + 'static>;

struct ThreadPool {
    workers: Vec<Worker>,
    sender: Option<mpsc::Sender<Job>>,
}

struct Worker {
    id: usize,
    thread: Option<thread::JoinHandle<()>>,
}

impl ThreadPool {
    fn new(size: usize) -> ThreadPool {
        let (sender, receiver) = mpsc::channel();
        let receiver = Arc::new(Mutex::new(receiver));

        let mut workers = Vec::with_capacity(size);

        for id in 0..size {
            workers.push(Worker::new(id, Arc::clone(&receiver)));
        }

        ThreadPool {
            workers,
            sender: Some(sender),
        }
    }

    fn execute<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        let job = Box::new(f);
        self.sender.as_ref().unwrap().send(job).unwrap();
    }
}

impl Worker {
    fn new(id: usize, receiver: Arc<Mutex<mpsc::Receiver<Job>>>) -> Worker {
        let thread = thread::spawn(move || loop {
            let message = receiver.lock().unwrap().recv();

            match message {
                Ok(job) => {
                    println!("Worker {} executing job", id);
                    job();
                }
                Err(_) => {
                    println!("Worker {} disconnected", id);
                    break;
                }
            }
        });

        Worker {
            id,
            thread: Some(thread),
        }
    }
}

impl Drop for ThreadPool {
    fn drop(&mut self) {
        drop(self.sender.take());

        for worker in &mut self.workers {
            if let Some(thread) = worker.thread.take() {
                thread.join().unwrap();
            }
        }
    }
}

fn use_thread_pool() {
    let pool = ThreadPool::new(4);

    for i in 0..8 {
        pool.execute(move || {
            println!("Task {} running", i);
            thread::sleep(std::time::Duration::from_millis(100));
        });
    }
}
```

### 7.2 Actor Pattern

```rust
use std::sync::mpsc;
use std::thread;

// Actor message types
enum CounterMessage {
    Increment,
    Decrement,
    Get(mpsc::Sender<i32>),
    Stop,
}

struct Counter {
    count: i32,
    receiver: mpsc::Receiver<CounterMessage>,
}

impl Counter {
    fn new() -> (Self, mpsc::Sender<CounterMessage>) {
        let (sender, receiver) = mpsc::channel();
        (Counter { count: 0, receiver }, sender)
    }

    fn run(mut self) {
        loop {
            match self.receiver.recv() {
                Ok(CounterMessage::Increment) => self.count += 1,
                Ok(CounterMessage::Decrement) => self.count -= 1,
                Ok(CounterMessage::Get(reply)) => {
                    let _ = reply.send(self.count);
                }
                Ok(CounterMessage::Stop) => break,
                Err(_) => break,
            }
        }
    }
}

fn actor_example() {
    let (counter, sender) = Counter::new();

    let handle = thread::spawn(move || {
        counter.run();
    });

    sender.send(CounterMessage::Increment).unwrap();
    sender.send(CounterMessage::Increment).unwrap();
    sender.send(CounterMessage::Increment).unwrap();
    sender.send(CounterMessage::Decrement).unwrap();

    let (reply_tx, reply_rx) = mpsc::channel();
    sender.send(CounterMessage::Get(reply_tx)).unwrap();
    println!("Count: {}", reply_rx.recv().unwrap());

    sender.send(CounterMessage::Stop).unwrap();
    handle.join().unwrap();
}
```

---

## 8. Tablas de Referencia

### 8.1 Sync Primitives Comparison

| Primitive | Use Case | Blocks | Readers | Writers |
|-----------|----------|--------|---------|---------|
| `Mutex<T>` | Single access | Yes | 1 | 1 |
| `RwLock<T>` | Read-heavy | Yes | N | 1 |
| `Atomic*` | Simple values | No | N | N |
| `Condvar` | Signaling | Yes | - | - |
| `Barrier` | Synchronize | Yes | - | - |
| `Once` | Init once | Yes | - | 1 |

### 8.2 Channel Comparison

| Channel | Bounded | Multi-Producer | Multi-Consumer |
|---------|---------|----------------|----------------|
| `mpsc::channel` | No | Yes | No |
| `mpsc::sync_channel` | Yes | Yes | No |
| `crossbeam::unbounded` | No | Yes | Yes |
| `crossbeam::bounded` | Yes | Yes | Yes |
| `tokio::mpsc` | Yes | Yes | No |
| `tokio::broadcast` | Yes | Yes | Yes |
| `tokio::watch` | No (1 val) | No | Yes |

### 8.3 Send and Sync

| Type | Send | Sync | Notes |
|------|------|------|-------|
| Primitives | Yes | Yes | i32, f64, bool, etc. |
| `&T` (T: Sync) | Yes | Yes | Shared reference |
| `&mut T` (T: Send) | Yes | No | Exclusive reference |
| `Box<T>` (T: Send) | Yes | if T: Sync | Owned heap |
| `Vec<T>` (T: Send) | Yes | if T: Sync | |
| `Rc<T>` | No | No | Not thread-safe |
| `Arc<T>` | Yes | Yes | Atomic ref count |
| `Mutex<T>` | Yes | Yes | Interior mutability |
| `RwLock<T>` | Yes | Yes | Interior mutability |
| `Cell<T>` | if T: Send | No | Not thread-safe |
| `RefCell<T>` | if T: Send | No | Not thread-safe |
| `*const T` | No | No | Raw pointer |
| `*mut T` | No | No | Raw pointer |

### 8.4 Ordering Summary

| Ordering | Guarantees |
|----------|-----------|
| `Relaxed` | No sync, just atomic |
| `Acquire` | See writes before Release |
| `Release` | Make writes visible |
| `AcqRel` | Both Acquire and Release |
| `SeqCst` | Total ordering |

---

## Referencias y Recursos Adicionales

1. **Rust Book - Concurrency**: https://doc.rust-lang.org/book/ch16-00-concurrency.html
2. **Tokio Tutorial**: https://tokio.rs/tokio/tutorial
3. **Rust Atomics and Locks**: https://marabos.nl/atomics/
4. **Crossbeam**: https://docs.rs/crossbeam

---

*Documento generado por ARCHAEON_CORE - Sistema de Documentación de Lenguajes*
*Especialización: Concurrencia C → Rust*
*Última actualización: 2025-12-31*

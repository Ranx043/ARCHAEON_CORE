---
título: "RUST 05 - Programación Embedded"
versión: "1.0.0"
fecha_creación: "2025-12-31"
última_actualización: "2025-12-31"
autor: "ARCHAEON_CORE"
dominio: "Lenguajes Modernos / Rust"
especialización: "Programación Embedded y Bare Metal"
contexto_soul_core: "ARCHAEON - Evolución C Embedded → Rust Embedded"
tags:
  - rust
  - embedded
  - no_std
  - bare-metal
  - hal
  - rtos
  - interrupts
  - mmio
  - cortex-m
  - arm
dependencias:
  - rust_fundamentos: "RUST_01_FUNDAMENTOS.md"
  - rust_sistemas: "RUST_02_SISTEMAS.md"
  - conocimiento_c: "avanzado"
  - arquitectura_embedded: "intermedio"
---

# RUST 05: PROGRAMACIÓN EMBEDDED

## Índice de Contenidos

1. [Introducción a Embedded Rust](#1-introducción-a-embedded-rust)
2. [Embedded Rust (no_std)](#2-embedded-rust-no_std)
3. [Bare Metal Programming](#3-bare-metal-programming)
4. [HAL y Peripheral Access](#4-hal-y-peripheral-access)
5. [Interrupt Handling](#5-interrupt-handling)
6. [Memory-Mapped I/O](#6-memory-mapped-io)
7. [Reemplazando C Embedded Code](#7-reemplazando-c-embedded-code)
8. [Patrones Avanzados](#8-patrones-avanzados)
9. [Tablas de Referencia](#9-tablas-de-referencia)

---

## 1. Introducción a Embedded Rust

### 1.1 Por Qué Rust para Embedded

Rust ofrece ventajas significativas sobre C para desarrollo embedded:
- Memory safety sin garbage collector
- Zero-cost abstractions
- Modern tooling (cargo, rustfmt, clippy)
- Type-safe peripheral access
- Fearless concurrency

```rust
#![no_std]
#![no_main]

use cortex_m_rt::entry;
use panic_halt as _;

// Safe embedded Rust - LED blink
#[entry]
fn main() -> ! {
    // Get peripherals safely
    let dp = stm32f4xx_hal::pac::Peripherals::take().unwrap();

    // Configure GPIO with type-safe API
    let gpioa = dp.GPIOA.split();
    let mut led = gpioa.pa5.into_push_pull_output();

    loop {
        led.set_high();
        cortex_m::asm::delay(8_000_000);
        led.set_low();
        cortex_m::asm::delay(8_000_000);
    }
}
```

### 1.2 Ecosistema Embedded Rust

```text
+------------------+
|   Application    |  Your code
+------------------+
|       HAL        |  Hardware Abstraction Layer (stm32f4xx-hal)
+------------------+
|       PAC        |  Peripheral Access Crate (stm32f4)
+------------------+
|    cortex-m      |  CPU support (cortex-m, riscv)
+------------------+
|    cortex-m-rt   |  Runtime (startup, vector table)
+------------------+
|     Hardware     |  Physical MCU
+------------------+
```

### 1.3 Configuración del Proyecto

```toml
# Cargo.toml for STM32F4
[package]
name = "embedded-app"
version = "0.1.0"
edition = "2021"

[dependencies]
cortex-m = "0.7"
cortex-m-rt = "0.7"
panic-halt = "0.2"

# HAL crate for specific MCU
stm32f4xx-hal = { version = "0.17", features = ["stm32f411"] }

# Optional: RTIC for real-time
# cortex-m-rtic = "1.1"

[profile.release]
opt-level = "s"     # Optimize for size
lto = true          # Link-time optimization
codegen-units = 1   # Better optimization
debug = true        # Keep debug symbols

[profile.dev]
opt-level = 1       # Some optimization for faster builds
```

```toml
# .cargo/config.toml
[target.thumbv7em-none-eabihf]
runner = "probe-rs run --chip STM32F411CEUx"

[build]
target = "thumbv7em-none-eabihf"

[env]
DEFMT_LOG = "debug"
```

```ld
/* memory.x - Memory layout */
MEMORY
{
    FLASH : ORIGIN = 0x08000000, LENGTH = 512K
    RAM   : ORIGIN = 0x20000000, LENGTH = 128K
}

/* Optional: place critical code in RAM */
SECTIONS
{
    .fast_code :
    {
        *(.fast_code)
    } > RAM AT > FLASH
}
```

---

## 2. Embedded Rust (no_std)

### 2.1 Core Library Features

```rust
#![no_std]

// Core provides essential types without heap allocation
use core::ptr;
use core::mem;
use core::slice;
use core::str;
use core::cell::{Cell, RefCell, UnsafeCell};
use core::sync::atomic::{AtomicBool, AtomicU32, Ordering};

// Option and Result are in core
fn find_max(data: &[u32]) -> Option<u32> {
    if data.is_empty() {
        return None;
    }

    let mut max = data[0];
    for &val in &data[1..] {
        if val > max {
            max = val;
        }
    }
    Some(max)
}

// Iterators work in no_std
fn sum_values(data: &[u32]) -> u32 {
    data.iter().sum()
}

// Fixed-size arrays instead of Vec
struct Buffer<const N: usize> {
    data: [u8; N],
    len: usize,
}

impl<const N: usize> Buffer<N> {
    pub const fn new() -> Self {
        Buffer {
            data: [0; N],
            len: 0,
        }
    }

    pub fn push(&mut self, byte: u8) -> bool {
        if self.len < N {
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

### 2.2 Heapless Collections

```rust
#![no_std]

use heapless::{Vec, String, FnvIndexMap};

// Fixed-capacity vector
fn heapless_vec() {
    let mut vec: Vec<i32, 16> = Vec::new();

    vec.push(1).unwrap();
    vec.push(2).unwrap();
    vec.push(3).unwrap();

    for item in &vec {
        // Process item
    }
}

// Fixed-capacity string
fn heapless_string() {
    let mut s: String<64> = String::new();

    core::write!(&mut s, "Value: {}", 42).unwrap();
}

// Fixed-capacity hashmap
fn heapless_map() {
    let mut map: FnvIndexMap<&str, i32, 8> = FnvIndexMap::new();

    map.insert("one", 1).unwrap();
    map.insert("two", 2).unwrap();

    if let Some(val) = map.get("one") {
        // Use val
    }
}

// Queue for interrupt communication
use heapless::spsc::Queue;

static mut QUEUE: Queue<u8, 16> = Queue::new();

fn producer() {
    let mut producer = unsafe { QUEUE.split().0 };
    let _ = producer.enqueue(42);
}

fn consumer() {
    let mut consumer = unsafe { QUEUE.split().1 };
    if let Some(val) = consumer.dequeue() {
        // Process val
    }
}
```

### 2.3 Panic Handling

```rust
#![no_std]
#![no_main]

use core::panic::PanicInfo;

// Option 1: Halt on panic
#[panic_handler]
fn panic_halt(_info: &PanicInfo) -> ! {
    loop {
        cortex_m::asm::wfi();
    }
}

// Option 2: Blink LED on panic
#[panic_handler]
fn panic_blink(_info: &PanicInfo) -> ! {
    // Get GPIO for LED (simplified)
    let gpioa = unsafe { &*stm32f4::stm32f411::GPIOA::ptr() };

    loop {
        // Toggle LED
        gpioa.odr.modify(|r, w| unsafe {
            w.odr5().bit(!r.odr5().bit())
        });

        // Delay
        cortex_m::asm::delay(2_000_000);
    }
}

// Option 3: Log panic info via defmt
use defmt_rtt as _;

#[panic_handler]
fn panic_defmt(info: &PanicInfo) -> ! {
    defmt::error!("Panic: {:?}", defmt::Debug2Format(info));

    loop {
        cortex_m::asm::bkpt();
    }
}

// Option 4: Reset on panic
#[panic_handler]
fn panic_reset(_info: &PanicInfo) -> ! {
    cortex_m::peripheral::SCB::sys_reset();
}
```

### 2.4 Logging con defmt

```rust
#![no_std]
#![no_main]

use defmt::{info, warn, error, debug, trace};
use defmt_rtt as _;
use panic_probe as _;

#[cortex_m_rt::entry]
fn main() -> ! {
    info!("Application started");

    let value = 42u32;
    debug!("Debug value: {}", value);

    // Efficient binary logging
    let buffer = [1u8, 2, 3, 4, 5];
    trace!("Buffer contents: {:?}", buffer);

    // Conditional compilation
    #[cfg(debug_assertions)]
    warn!("Running in debug mode");

    loop {
        // Main loop
    }
}

// Custom defmt formatting
#[derive(defmt::Format)]
struct SensorReading {
    temperature: i16,
    humidity: u8,
}

fn log_sensor(reading: &SensorReading) {
    info!("Sensor: {:?}", reading);
}
```

---

## 3. Bare Metal Programming

### 3.1 Entry Point y Startup

```rust
#![no_std]
#![no_main]

use cortex_m_rt::{entry, pre_init, exception};
use core::ptr;

// Pre-initialization (before RAM initialization)
#[pre_init]
unsafe fn before_main() {
    // Initialize external memory, etc.
}

// Main entry point
#[entry]
fn main() -> ! {
    // Initialize hardware
    init_system();

    // Main loop
    loop {
        // Application code
    }
}

// Exception handlers
#[exception]
fn HardFault(ef: &cortex_m_rt::ExceptionFrame) -> ! {
    // Log fault information
    loop {}
}

#[exception]
fn DefaultHandler(irqn: i16) {
    // Handle unexpected interrupts
}

fn init_system() {
    // Clock configuration, peripheral init, etc.
}
```

### 3.2 Linker Script Personalizado

```rust
// Placing code in specific sections
#[link_section = ".fast_code"]
fn time_critical_function() {
    // This function will be placed in RAM for faster execution
}

// Placing data in specific sections
#[link_section = ".ccmram"]
static mut CRITICAL_BUFFER: [u8; 1024] = [0; 1024];

// Boot configuration for STM32
#[link_section = ".boot_config"]
#[used]
static BOOT_CONFIG: [u32; 4] = [
    0x0000_0000,  // Option bytes
    0x0000_0000,
    0x0000_0000,
    0x0000_0000,
];
```

### 3.3 Clock Configuration

```rust
use stm32f4xx_hal::{
    pac,
    prelude::*,
    rcc::{RccExt, Clocks},
};

fn configure_clocks(dp: pac::Peripherals) -> Clocks {
    // Configure for 100 MHz using external 8 MHz crystal
    let rcc = dp.RCC.constrain();

    let clocks = rcc
        .cfgr
        .use_hse(8.MHz())       // External 8 MHz crystal
        .sysclk(100.MHz())      // Target system clock
        .hclk(100.MHz())        // AHB bus clock
        .pclk1(50.MHz())        // APB1 clock (max 50 MHz)
        .pclk2(100.MHz())       // APB2 clock
        .freeze();

    clocks
}

// Manual clock configuration (register level)
unsafe fn configure_clocks_manual() {
    let rcc = &*pac::RCC::ptr();
    let flash = &*pac::FLASH::ptr();

    // Enable HSE
    rcc.cr.modify(|_, w| w.hseon().set_bit());
    while rcc.cr.read().hserdy().bit_is_clear() {}

    // Configure PLL: 8 MHz / 8 * 400 / 4 = 100 MHz
    rcc.pllcfgr.modify(|_, w| {
        w.pllsrc().hse()
         .pllm().bits(8)
         .plln().bits(400)
         .pllp().div4()
    });

    // Enable PLL
    rcc.cr.modify(|_, w| w.pllon().set_bit());
    while rcc.cr.read().pllrdy().bit_is_clear() {}

    // Configure flash wait states for 100 MHz
    flash.acr.modify(|_, w| w.latency().bits(3));

    // Switch to PLL
    rcc.cfgr.modify(|_, w| w.sw().pll());
    while rcc.cfgr.read().sws().bits() != 0b10 {}
}
```

### 3.4 Startup Sequence

```rust
#![no_std]
#![no_main]

use cortex_m::peripheral::Peripherals as CorePeripherals;
use stm32f4xx_hal::pac::Peripherals as DevicePeripherals;

#[cortex_m_rt::entry]
fn main() -> ! {
    // 1. Get core peripherals (NVIC, SysTick, etc.)
    let cp = CorePeripherals::take().unwrap();

    // 2. Get device peripherals
    let dp = DevicePeripherals::take().unwrap();

    // 3. Configure clocks
    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.sysclk(100.MHz()).freeze();

    // 4. Configure GPIO
    let gpioa = dp.GPIOA.split();
    let gpiob = dp.GPIOB.split();

    // 5. Configure peripherals (UART, SPI, I2C, etc.)
    let serial = dp.USART2.serial(
        (gpioa.pa2, gpioa.pa3),
        115_200.bps(),
        &clocks,
    ).unwrap();

    // 6. Configure interrupts
    let mut nvic = cp.NVIC;
    unsafe {
        nvic.set_priority(stm32f4xx_hal::pac::Interrupt::USART2, 1);
        cortex_m::peripheral::NVIC::unmask(stm32f4xx_hal::pac::Interrupt::USART2);
    }

    // 7. Enter main loop
    loop {
        // Application logic
        cortex_m::asm::wfi();  // Wait for interrupt
    }
}
```

---

## 4. HAL y Peripheral Access

### 4.1 GPIO

```rust
use stm32f4xx_hal::{
    pac,
    prelude::*,
    gpio::{Input, Output, PushPull, PullUp, Alternate, Edge, ExtiPin},
};

fn gpio_examples(dp: pac::Peripherals, cp: cortex_m::Peripherals) {
    let gpioa = dp.GPIOA.split();
    let gpiob = dp.GPIOB.split();
    let mut syscfg = dp.SYSCFG.constrain();

    // Output pin
    let mut led = gpioa.pa5.into_push_pull_output();
    led.set_high();
    led.set_low();
    led.toggle();

    // Input pin with pull-up
    let button = gpioa.pa0.into_pull_up_input();
    if button.is_high() {
        led.set_high();
    }

    // Alternate function (for peripherals)
    let _tx: Alternate<_, 7> = gpioa.pa2.into_alternate();
    let _rx: Alternate<_, 7> = gpioa.pa3.into_alternate();

    // External interrupt
    let mut button_int = gpiob.pb0.into_pull_up_input();
    button_int.make_interrupt_source(&mut syscfg);
    button_int.trigger_on_edge(&mut dp.EXTI, Edge::Falling);
    button_int.enable_interrupt(&mut dp.EXTI);
}

// Type-state GPIO
fn type_state_gpio() {
    // Compile-time enforcement of pin configuration
    // let pin: PA5<Input<Floating>>;  // Unconfigured
    // let pin: PA5<Output<PushPull>>; // Configured as output
    // let pin: PA5<Alternate<7>>;     // Configured for UART
}
```

### 4.2 UART/Serial

```rust
use stm32f4xx_hal::{
    pac,
    prelude::*,
    serial::{Serial, Config, Event},
};
use core::fmt::Write;

fn uart_example(dp: pac::Peripherals) {
    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();

    let gpioa = dp.GPIOA.split();

    // Configure UART
    let tx = gpioa.pa2.into_alternate();
    let rx = gpioa.pa3.into_alternate();

    let mut serial = Serial::new(
        dp.USART2,
        (tx, rx),
        Config::default().baudrate(115_200.bps()),
        &clocks,
    ).unwrap();

    // Blocking write
    writeln!(serial, "Hello, UART!").unwrap();

    // Blocking read
    let byte = nb::block!(serial.read()).unwrap();

    // Non-blocking operations
    match serial.read() {
        Ok(byte) => { /* Got byte */ }
        Err(nb::Error::WouldBlock) => { /* No data available */ }
        Err(nb::Error::Other(e)) => { /* Error */ }
    }

    // Split into tx/rx for separate use
    let (mut tx, mut rx) = serial.split();

    // Enable interrupts
    tx.listen(Event::Txe);
    rx.listen(Event::Rxne);
}
```

### 4.3 SPI

```rust
use stm32f4xx_hal::{
    pac,
    prelude::*,
    spi::{Spi, Mode, Phase, Polarity},
};

fn spi_example(dp: pac::Peripherals) {
    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();

    let gpioa = dp.GPIOA.split();

    // Configure SPI pins
    let sck = gpioa.pa5.into_alternate();
    let miso = gpioa.pa6.into_alternate();
    let mosi = gpioa.pa7.into_alternate();

    // Configure CS pin
    let mut cs = gpioa.pa4.into_push_pull_output();
    cs.set_high();  // Deselect

    // Configure SPI
    let mut spi = Spi::new(
        dp.SPI1,
        (sck, miso, mosi),
        Mode {
            polarity: Polarity::IdleLow,
            phase: Phase::CaptureOnFirstTransition,
        },
        1.MHz(),
        &clocks,
    );

    // Transfer data
    cs.set_low();  // Select device

    let tx_data = [0x01, 0x02, 0x03];
    let mut rx_data = [0u8; 3];

    spi.transfer(&mut rx_data, &tx_data).unwrap();

    cs.set_high();  // Deselect

    // Write only
    cs.set_low();
    spi.write(&[0xFF, 0x00]).unwrap();
    cs.set_high();
}
```

### 4.4 I2C

```rust
use stm32f4xx_hal::{
    pac,
    prelude::*,
    i2c::{I2c, Mode as I2cMode},
};

fn i2c_example(dp: pac::Peripherals) {
    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();

    let gpiob = dp.GPIOB.split();

    // Configure I2C pins
    let scl = gpiob.pb8.into_alternate_open_drain();
    let sda = gpiob.pb9.into_alternate_open_drain();

    // Configure I2C
    let mut i2c = I2c::new(
        dp.I2C1,
        (scl, sda),
        I2cMode::Standard { frequency: 100.kHz() },
        &clocks,
    );

    // Write to device
    let device_addr = 0x50u8;
    let data = [0x00, 0x01, 0x02];
    i2c.write(device_addr, &data).unwrap();

    // Read from device
    let mut buffer = [0u8; 4];
    i2c.read(device_addr, &mut buffer).unwrap();

    // Write-then-read (common pattern)
    let register = [0x00u8];  // Register address
    let mut value = [0u8; 2];
    i2c.write_read(device_addr, &register, &mut value).unwrap();
}

// Using embedded-hal traits for driver portability
use embedded_hal::i2c::I2c as I2cTrait;

struct Sensor<I2C> {
    i2c: I2C,
    address: u8,
}

impl<I2C: I2cTrait> Sensor<I2C> {
    pub fn new(i2c: I2C, address: u8) -> Self {
        Sensor { i2c, address }
    }

    pub fn read_value(&mut self) -> Result<u16, I2C::Error> {
        let mut buffer = [0u8; 2];
        self.i2c.read(self.address, &mut buffer)?;
        Ok(u16::from_be_bytes(buffer))
    }
}
```

### 4.5 Timers y PWM

```rust
use stm32f4xx_hal::{
    pac,
    prelude::*,
    timer::{Timer, Event, CounterUs},
    pwm::PwmExt,
};

fn timer_example(dp: pac::Peripherals, cp: cortex_m::Peripherals) {
    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();

    // Basic timer
    let mut timer = dp.TIM2.counter_us(&clocks);
    timer.start(1.secs()).unwrap();

    // Wait for timer
    nb::block!(timer.wait()).unwrap();

    // Periodic timer with interrupt
    let mut timer = dp.TIM3.counter_us(&clocks);
    timer.start(100.millis()).unwrap();
    timer.listen(Event::Update);

    // PWM output
    let gpioa = dp.GPIOA.split();
    let channels = dp.TIM1.pwm_hz(
        gpioa.pa8.into_alternate(),
        1.kHz(),
        &clocks,
    );

    let mut pwm = channels.split();
    pwm.set_duty(pwm.get_max_duty() / 2);  // 50% duty cycle
    pwm.enable();
}

// Delay provider
fn delay_example(cp: cortex_m::Peripherals, clocks: &stm32f4xx_hal::rcc::Clocks) {
    let mut delay = cp.SYST.delay(clocks);

    delay.delay_ms(1000u32);
    delay.delay_us(500u32);
}
```

### 4.6 ADC

```rust
use stm32f4xx_hal::{
    pac,
    prelude::*,
    adc::{Adc, config::AdcConfig},
};

fn adc_example(dp: pac::Peripherals) {
    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();

    let gpioa = dp.GPIOA.split();

    // Configure ADC pin
    let adc_pin = gpioa.pa0.into_analog();

    // Configure ADC
    let mut adc = Adc::adc1(dp.ADC1, true, AdcConfig::default());

    // Read value
    let sample: u16 = adc.read(&adc_pin).unwrap();

    // Convert to voltage (assuming 3.3V reference, 12-bit ADC)
    let voltage = (sample as f32 / 4095.0) * 3.3;

    // Internal temperature sensor
    let mut temp_sensor = adc.enable_temperature();
    let temp_sample: u16 = adc.read(&mut temp_sensor).unwrap();
}
```

---

## 5. Interrupt Handling

### 5.1 Basic Interrupt Setup

```rust
#![no_std]
#![no_main]

use cortex_m::peripheral::NVIC;
use stm32f4xx_hal::{
    pac::{self, interrupt, Interrupt},
    prelude::*,
    gpio::{Edge, ExtiPin},
};
use core::cell::RefCell;
use cortex_m::interrupt::Mutex;

// Shared state protected by Mutex
static BUTTON: Mutex<RefCell<Option<stm32f4xx_hal::gpio::PB0<stm32f4xx_hal::gpio::Input>>>> =
    Mutex::new(RefCell::new(None));

static BUTTON_PRESSED: Mutex<RefCell<bool>> =
    Mutex::new(RefCell::new(false));

#[cortex_m_rt::entry]
fn main() -> ! {
    let dp = pac::Peripherals::take().unwrap();

    let gpiob = dp.GPIOB.split();
    let mut syscfg = dp.SYSCFG.constrain();

    // Configure button with interrupt
    let mut button = gpiob.pb0.into_pull_up_input();
    button.make_interrupt_source(&mut syscfg);
    button.trigger_on_edge(&mut dp.EXTI, Edge::Falling);
    button.enable_interrupt(&mut dp.EXTI);

    // Store button in global state
    cortex_m::interrupt::free(|cs| {
        BUTTON.borrow(cs).replace(Some(button));
    });

    // Enable interrupt in NVIC
    unsafe {
        NVIC::unmask(Interrupt::EXTI0);
    }

    loop {
        cortex_m::interrupt::free(|cs| {
            if *BUTTON_PRESSED.borrow(cs).borrow() {
                // Handle button press
                *BUTTON_PRESSED.borrow(cs).borrow_mut() = false;
            }
        });

        cortex_m::asm::wfi();
    }
}

#[interrupt]
fn EXTI0() {
    cortex_m::interrupt::free(|cs| {
        if let Some(ref mut button) = *BUTTON.borrow(cs).borrow_mut() {
            button.clear_interrupt_pending_bit();
            *BUTTON_PRESSED.borrow(cs).borrow_mut() = true;
        }
    });
}
```

### 5.2 RTIC (Real-Time Interrupt-driven Concurrency)

```rust
#![no_std]
#![no_main]

use panic_halt as _;
use rtic::app;
use stm32f4xx_hal::{
    pac,
    prelude::*,
    gpio::{Output, PushPull, PA5},
    timer::{Event, CounterUs},
};

#[app(device = stm32f4xx_hal::pac, peripherals = true)]
mod app {
    use super::*;

    // Shared resources
    #[shared]
    struct Shared {
        led: PA5<Output<PushPull>>,
    }

    // Local resources (task-specific)
    #[local]
    struct Local {
        timer: CounterUs<pac::TIM2>,
    }

    #[init]
    fn init(cx: init::Context) -> (Shared, Local) {
        let dp = cx.device;

        let rcc = dp.RCC.constrain();
        let clocks = rcc.cfgr.sysclk(100.MHz()).freeze();

        let gpioa = dp.GPIOA.split();
        let led = gpioa.pa5.into_push_pull_output();

        let mut timer = dp.TIM2.counter_us(&clocks);
        timer.start(1.secs()).unwrap();
        timer.listen(Event::Update);

        (
            Shared { led },
            Local { timer },
        )
    }

    // Idle task (lowest priority)
    #[idle]
    fn idle(_: idle::Context) -> ! {
        loop {
            cortex_m::asm::wfi();
        }
    }

    // Timer interrupt handler
    #[task(binds = TIM2, local = [timer], shared = [led])]
    fn timer_tick(mut cx: timer_tick::Context) {
        cx.local.timer.clear_interrupt(Event::Update);

        cx.shared.led.lock(|led| {
            led.toggle();
        });
    }
}
```

### 5.3 Priority y Preemption

```rust
#![no_std]
#![no_main]

use panic_halt as _;
use rtic::app;

#[app(device = stm32f4xx_hal::pac, peripherals = true, dispatchers = [SPI1, SPI2])]
mod app {
    use stm32f4xx_hal::{pac, prelude::*};

    #[shared]
    struct Shared {
        counter: u32,
    }

    #[local]
    struct Local {}

    #[init]
    fn init(_: init::Context) -> (Shared, Local) {
        // Schedule software task
        high_priority::spawn().ok();

        (Shared { counter: 0 }, Local {})
    }

    // Low priority task
    #[task(priority = 1, shared = [counter])]
    async fn low_priority(mut cx: low_priority::Context) {
        cx.shared.counter.lock(|c| {
            *c += 1;
        });
    }

    // High priority task - can preempt low_priority
    #[task(priority = 2, shared = [counter])]
    async fn high_priority(mut cx: high_priority::Context) {
        cx.shared.counter.lock(|c| {
            *c += 10;
        });

        // Spawn lower priority task
        low_priority::spawn().ok();
    }

    // Hardware interrupt - highest priority
    #[task(binds = USART2, priority = 3, shared = [counter])]
    fn usart2(mut cx: usart2::Context) {
        cx.shared.counter.lock(|c| {
            *c += 100;
        });
    }
}
```

### 5.4 DMA con Interrupts

```rust
use stm32f4xx_hal::{
    pac,
    dma::{StreamsTuple, Transfer, config::DmaConfig},
    serial::Serial,
};

static mut TX_BUFFER: [u8; 64] = [0; 64];
static mut RX_BUFFER: [u8; 64] = [0; 64];

fn dma_uart_example(dp: pac::Peripherals) {
    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();

    let gpioa = dp.GPIOA.split();

    // Configure UART
    let serial = Serial::new(
        dp.USART2,
        (gpioa.pa2.into_alternate(), gpioa.pa3.into_alternate()),
        115_200.bps(),
        &clocks,
    ).unwrap();

    let (tx, rx) = serial.split();

    // Get DMA streams
    let streams = StreamsTuple::new(dp.DMA1);

    // Configure DMA for TX
    let tx_config = DmaConfig::default()
        .memory_increment(true)
        .transfer_complete_interrupt(true);

    // Start DMA transfer
    let tx_buffer = unsafe { &TX_BUFFER };
    let transfer = Transfer::init_memory_to_peripheral(
        streams.6,
        tx,
        tx_buffer,
        None,
        tx_config,
    );

    // In interrupt handler:
    // transfer.clear_transfer_complete_interrupt();
}
```

---

## 6. Memory-Mapped I/O

### 6.1 Direct Register Access

```rust
use core::ptr::{read_volatile, write_volatile};

// Base addresses
const GPIOA_BASE: usize = 0x4002_0000;
const RCC_BASE: usize = 0x4002_3800;

// Register offsets for GPIO
const GPIO_MODER: usize = 0x00;
const GPIO_OTYPER: usize = 0x04;
const GPIO_OSPEEDR: usize = 0x08;
const GPIO_PUPDR: usize = 0x0C;
const GPIO_IDR: usize = 0x10;
const GPIO_ODR: usize = 0x14;
const GPIO_BSRR: usize = 0x18;

// Safe wrappers for MMIO
#[repr(transparent)]
struct Register<T: Copy> {
    addr: *mut T,
}

impl<T: Copy> Register<T> {
    const fn new(addr: usize) -> Self {
        Register { addr: addr as *mut T }
    }

    fn read(&self) -> T {
        unsafe { read_volatile(self.addr) }
    }

    fn write(&self, value: T) {
        unsafe { write_volatile(self.addr, value) }
    }

    fn modify<F>(&self, f: F)
    where
        F: FnOnce(T) -> T,
    {
        let value = self.read();
        self.write(f(value));
    }
}

// GPIO peripheral struct
struct Gpio {
    moder: Register<u32>,
    otyper: Register<u32>,
    ospeedr: Register<u32>,
    pupdr: Register<u32>,
    idr: Register<u32>,
    odr: Register<u32>,
    bsrr: Register<u32>,
}

impl Gpio {
    const fn new(base: usize) -> Self {
        Gpio {
            moder: Register::new(base + GPIO_MODER),
            otyper: Register::new(base + GPIO_OTYPER),
            ospeedr: Register::new(base + GPIO_OSPEEDR),
            pupdr: Register::new(base + GPIO_PUPDR),
            idr: Register::new(base + GPIO_IDR),
            odr: Register::new(base + GPIO_ODR),
            bsrr: Register::new(base + GPIO_BSRR),
        }
    }

    fn set_pin_output(&self, pin: u8) {
        self.moder.modify(|r| {
            let mask = !(0b11 << (pin * 2));
            let value = 0b01 << (pin * 2);  // Output mode
            (r & mask) | value
        });
    }

    fn set_pin_high(&self, pin: u8) {
        self.bsrr.write(1 << pin);
    }

    fn set_pin_low(&self, pin: u8) {
        self.bsrr.write(1 << (pin + 16));
    }

    fn read_pin(&self, pin: u8) -> bool {
        (self.idr.read() & (1 << pin)) != 0
    }
}

static GPIOA: Gpio = Gpio::new(GPIOA_BASE);

fn manual_gpio_example() {
    // Enable GPIOA clock (in RCC)
    let rcc_ahb1enr = Register::<u32>::new(RCC_BASE + 0x30);
    rcc_ahb1enr.modify(|r| r | (1 << 0));

    // Configure PA5 as output
    GPIOA.set_pin_output(5);

    // Toggle LED
    GPIOA.set_pin_high(5);
    cortex_m::asm::delay(1_000_000);
    GPIOA.set_pin_low(5);
}
```

### 6.2 SVD-Generated PAC

```rust
// Using SVD2Rust generated PAC
use stm32f4::stm32f411;

fn pac_example() {
    let dp = stm32f411::Peripherals::take().unwrap();

    // Enable GPIOA clock
    dp.RCC.ahb1enr.modify(|_, w| w.gpioaen().set_bit());

    // Configure PA5 as output
    dp.GPIOA.moder.modify(|_, w| w.moder5().output());
    dp.GPIOA.otyper.modify(|_, w| w.ot5().push_pull());
    dp.GPIOA.ospeedr.modify(|_, w| w.ospeedr5().low_speed());

    // Set PA5 high
    dp.GPIOA.bsrr.write(|w| w.bs5().set_bit());

    // Set PA5 low
    dp.GPIOA.bsrr.write(|w| w.br5().set_bit());

    // Read PA5
    let is_high = dp.GPIOA.idr.read().idr5().bit_is_set();
}
```

### 6.3 Bitbanding (Cortex-M3/M4)

```rust
// Bit-banding allows atomic bit manipulation
const SRAM_BASE: usize = 0x2000_0000;
const SRAM_BB_BASE: usize = 0x2200_0000;
const PERIPH_BASE: usize = 0x4000_0000;
const PERIPH_BB_BASE: usize = 0x4200_0000;

fn bitband_sram_addr(byte_addr: usize, bit: u8) -> usize {
    SRAM_BB_BASE + ((byte_addr - SRAM_BASE) * 32) + ((bit as usize) * 4)
}

fn bitband_periph_addr(byte_addr: usize, bit: u8) -> usize {
    PERIPH_BB_BASE + ((byte_addr - PERIPH_BASE) * 32) + ((bit as usize) * 4)
}

// Atomic bit set/clear using bitbanding
struct BitBandBit {
    addr: *mut u32,
}

impl BitBandBit {
    fn set(&self) {
        unsafe { core::ptr::write_volatile(self.addr, 1) };
    }

    fn clear(&self) {
        unsafe { core::ptr::write_volatile(self.addr, 0) };
    }

    fn read(&self) -> bool {
        unsafe { core::ptr::read_volatile(self.addr) != 0 }
    }
}

fn bitband_example() {
    // GPIO ODR bit for PA5
    const GPIOA_ODR: usize = 0x4002_0014;
    const PA5_BIT: u8 = 5;

    let pa5_bb = BitBandBit {
        addr: bitband_periph_addr(GPIOA_ODR, PA5_BIT) as *mut u32,
    };

    pa5_bb.set();    // Atomic set
    pa5_bb.clear();  // Atomic clear
}
```

---

## 7. Reemplazando C Embedded Code

### 7.1 Migración de GPIO C a Rust

```c
// C: STM32 GPIO configuration
#include "stm32f4xx.h"

void gpio_init(void) {
    // Enable GPIOA clock
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN;

    // Configure PA5 as output
    GPIOA->MODER &= ~GPIO_MODER_MODE5;
    GPIOA->MODER |= GPIO_MODER_MODE5_0;  // Output mode

    // Push-pull
    GPIOA->OTYPER &= ~GPIO_OTYPER_OT5;

    // Low speed
    GPIOA->OSPEEDR &= ~GPIO_OSPEEDR_OSPEED5;
}

void gpio_set_high(void) {
    GPIOA->BSRR = GPIO_BSRR_BS5;
}

void gpio_set_low(void) {
    GPIOA->BSRR = GPIO_BSRR_BR5;
}

void gpio_toggle(void) {
    GPIOA->ODR ^= GPIO_ODR_OD5;  // Not atomic!
}
```

```rust
// Rust: Type-safe GPIO
use stm32f4xx_hal::{pac, prelude::*, gpio::{Output, PushPull, PA5}};

struct Led {
    pin: PA5<Output<PushPull>>,
}

impl Led {
    fn new(gpioa: stm32f4xx_hal::gpio::gpioa::Parts) -> Self {
        Led {
            pin: gpioa.pa5.into_push_pull_output(),
        }
    }

    fn on(&mut self) {
        self.pin.set_high();
    }

    fn off(&mut self) {
        self.pin.set_low();
    }

    fn toggle(&mut self) {
        self.pin.toggle();  // Atomic using BSRR
    }
}

fn main_rust() {
    let dp = pac::Peripherals::take().unwrap();

    let gpioa = dp.GPIOA.split();  // Automatically enables clock
    let mut led = Led::new(gpioa);

    led.on();
    led.off();
    led.toggle();
}
```

### 7.2 Migración de UART C a Rust

```c
// C: UART driver
#include "stm32f4xx.h"
#include <string.h>

void uart_init(uint32_t baudrate) {
    // Enable clocks
    RCC->APB1ENR |= RCC_APB1ENR_USART2EN;
    RCC->AHB1ENR |= RCC_AHB1ENR_GPIOAEN;

    // Configure GPIO
    GPIOA->MODER |= GPIO_MODER_MODE2_1 | GPIO_MODER_MODE3_1;  // AF
    GPIOA->AFR[0] |= (7 << 8) | (7 << 12);  // AF7

    // Configure UART
    USART2->BRR = SystemCoreClock / baudrate;
    USART2->CR1 = USART_CR1_TE | USART_CR1_RE | USART_CR1_UE;
}

void uart_write(uint8_t byte) {
    while (!(USART2->SR & USART_SR_TXE));
    USART2->DR = byte;
}

uint8_t uart_read(void) {
    while (!(USART2->SR & USART_SR_RXNE));
    return USART2->DR;
}

void uart_print(const char* str) {
    while (*str) {
        uart_write(*str++);
    }
}
```

```rust
// Rust: Safe UART driver
use stm32f4xx_hal::{
    pac,
    prelude::*,
    serial::{Serial, config::Config},
};
use core::fmt::Write;
use nb::block;

struct Uart {
    serial: Serial<pac::USART2>,
}

impl Uart {
    fn new(
        usart: pac::USART2,
        pins: (impl Into<stm32f4xx_hal::gpio::PA2<stm32f4xx_hal::gpio::Alternate<7>>>,
               impl Into<stm32f4xx_hal::gpio::PA3<stm32f4xx_hal::gpio::Alternate<7>>>),
        baudrate: u32,
        clocks: &stm32f4xx_hal::rcc::Clocks,
    ) -> Self {
        let serial = Serial::new(
            usart,
            pins,
            Config::default().baudrate(baudrate.bps()),
            clocks,
        ).unwrap();

        Uart { serial }
    }

    fn write_byte(&mut self, byte: u8) {
        block!(self.serial.write(byte)).unwrap();
    }

    fn read_byte(&mut self) -> u8 {
        block!(self.serial.read()).unwrap()
    }

    fn print(&mut self, s: &str) {
        for byte in s.bytes() {
            self.write_byte(byte);
        }
    }
}

// Implement Write trait for formatting
impl Write for Uart {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        self.print(s);
        Ok(())
    }
}

fn uart_example() {
    let dp = pac::Peripherals::take().unwrap();
    let rcc = dp.RCC.constrain();
    let clocks = rcc.cfgr.freeze();
    let gpioa = dp.GPIOA.split();

    let mut uart = Uart::new(
        dp.USART2,
        (gpioa.pa2.into_alternate(), gpioa.pa3.into_alternate()),
        115_200,
        &clocks,
    );

    writeln!(uart, "Hello from Rust!").unwrap();
}
```

### 7.3 Migración de Ring Buffer

```c
// C: Ring buffer for interrupt-driven UART
#define BUFFER_SIZE 64

typedef struct {
    uint8_t data[BUFFER_SIZE];
    volatile uint8_t head;
    volatile uint8_t tail;
} RingBuffer;

static RingBuffer rx_buffer = {0};
static RingBuffer tx_buffer = {0};

int ring_buffer_put(RingBuffer* rb, uint8_t byte) {
    uint8_t next = (rb->head + 1) % BUFFER_SIZE;
    if (next == rb->tail) return -1;  // Full
    rb->data[rb->head] = byte;
    rb->head = next;
    return 0;
}

int ring_buffer_get(RingBuffer* rb, uint8_t* byte) {
    if (rb->head == rb->tail) return -1;  // Empty
    *byte = rb->data[rb->tail];
    rb->tail = (rb->tail + 1) % BUFFER_SIZE;
    return 0;
}
```

```rust
// Rust: Type-safe ring buffer
use core::sync::atomic::{AtomicUsize, Ordering};

pub struct RingBuffer<const N: usize> {
    data: [u8; N],
    head: AtomicUsize,
    tail: AtomicUsize,
}

impl<const N: usize> RingBuffer<N> {
    pub const fn new() -> Self {
        RingBuffer {
            data: [0; N],
            head: AtomicUsize::new(0),
            tail: AtomicUsize::new(0),
        }
    }

    pub fn push(&mut self, byte: u8) -> Result<(), u8> {
        let head = self.head.load(Ordering::Relaxed);
        let next = (head + 1) % N;

        if next == self.tail.load(Ordering::Acquire) {
            return Err(byte);  // Full
        }

        self.data[head] = byte;
        self.head.store(next, Ordering::Release);
        Ok(())
    }

    pub fn pop(&mut self) -> Option<u8> {
        let tail = self.tail.load(Ordering::Relaxed);

        if tail == self.head.load(Ordering::Acquire) {
            return None;  // Empty
        }

        let byte = self.data[tail];
        self.tail.store((tail + 1) % N, Ordering::Release);
        Some(byte)
    }

    pub fn is_empty(&self) -> bool {
        self.head.load(Ordering::Relaxed) == self.tail.load(Ordering::Relaxed)
    }

    pub fn is_full(&self) -> bool {
        let head = self.head.load(Ordering::Relaxed);
        let next = (head + 1) % N;
        next == self.tail.load(Ordering::Relaxed)
    }
}

// Or use heapless::spsc::Queue for SPSC scenarios
use heapless::spsc::Queue;

static mut RX_QUEUE: Queue<u8, 64> = Queue::new();

fn interrupt_safe_queue() {
    // Producer (interrupt context)
    let (mut producer, _) = unsafe { RX_QUEUE.split() };
    let _ = producer.enqueue(42);

    // Consumer (main context)
    let (_, mut consumer) = unsafe { RX_QUEUE.split() };
    if let Some(byte) = consumer.dequeue() {
        // Process byte
    }
}
```

---

## 8. Patrones Avanzados

### 8.1 State Machine Driver

```rust
// Type-state pattern for peripheral driver
use core::marker::PhantomData;

// States
struct Disabled;
struct Enabled;
struct Transmitting;

struct UartDriver<STATE> {
    // Peripheral registers
    regs: *mut u32,
    _state: PhantomData<STATE>,
}

impl UartDriver<Disabled> {
    pub fn new(base: usize) -> Self {
        UartDriver {
            regs: base as *mut u32,
            _state: PhantomData,
        }
    }

    pub fn enable(self, baudrate: u32) -> UartDriver<Enabled> {
        // Configure and enable UART
        UartDriver {
            regs: self.regs,
            _state: PhantomData,
        }
    }
}

impl UartDriver<Enabled> {
    pub fn write(&mut self, byte: u8) {
        // Blocking write
    }

    pub fn read(&mut self) -> u8 {
        // Blocking read
        0
    }

    pub fn start_transmit(self, buffer: &[u8]) -> UartDriver<Transmitting> {
        // Start DMA transmit
        UartDriver {
            regs: self.regs,
            _state: PhantomData,
        }
    }

    pub fn disable(self) -> UartDriver<Disabled> {
        // Disable UART
        UartDriver {
            regs: self.regs,
            _state: PhantomData,
        }
    }
}

impl UartDriver<Transmitting> {
    pub fn is_complete(&self) -> bool {
        // Check DMA complete
        true
    }

    pub fn wait_complete(self) -> UartDriver<Enabled> {
        while !self.is_complete() {}
        UartDriver {
            regs: self.regs,
            _state: PhantomData,
        }
    }
}
```

### 8.2 Singleton Peripherals

```rust
use core::sync::atomic::{AtomicBool, Ordering};
use core::cell::UnsafeCell;

// Safe singleton pattern
pub struct Peripheral<T> {
    taken: AtomicBool,
    inner: UnsafeCell<Option<T>>,
}

unsafe impl<T: Send> Sync for Peripheral<T> {}

impl<T> Peripheral<T> {
    pub const fn new() -> Self {
        Peripheral {
            taken: AtomicBool::new(false),
            inner: UnsafeCell::new(None),
        }
    }

    pub fn take(&self) -> Option<T> {
        if self.taken
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Relaxed)
            .is_ok()
        {
            unsafe { (*self.inner.get()).take() }
        } else {
            None
        }
    }

    pub fn replace(&self, value: T) {
        unsafe {
            *self.inner.get() = Some(value);
        }
        self.taken.store(false, Ordering::Release);
    }
}

static UART: Peripheral<UartDriver<Disabled>> = Peripheral::new();

fn singleton_example() {
    if let Some(uart) = UART.take() {
        let uart = uart.enable(115200);
        // Use uart...
        // Return when done
    }
}
```

### 8.3 Async Embedded

```rust
#![no_std]
#![no_main]
#![feature(type_alias_impl_trait)]

use embassy_executor::Spawner;
use embassy_stm32::gpio::{Level, Output, Speed};
use embassy_time::Timer;

#[embassy_executor::main]
async fn main(_spawner: Spawner) {
    let p = embassy_stm32::init(Default::default());

    let mut led = Output::new(p.PA5, Level::High, Speed::Low);

    loop {
        led.set_high();
        Timer::after_millis(500).await;
        led.set_low();
        Timer::after_millis(500).await;
    }
}

// Async UART
use embassy_stm32::usart::{Uart, Config};

#[embassy_executor::task]
async fn uart_task(mut uart: Uart<'static, embassy_stm32::peripherals::USART2>) {
    let mut buf = [0u8; 64];

    loop {
        match uart.read(&mut buf).await {
            Ok(n) => {
                // Echo back
                uart.write(&buf[..n]).await.ok();
            }
            Err(_) => {}
        }
    }
}
```

---

## 9. Tablas de Referencia

### 9.1 Target Triples

| Architecture | Target | FPU |
|--------------|--------|-----|
| Cortex-M0/M0+ | thumbv6m-none-eabi | No |
| Cortex-M3 | thumbv7m-none-eabi | No |
| Cortex-M4/M7 | thumbv7em-none-eabi | No |
| Cortex-M4F/M7F | thumbv7em-none-eabihf | Yes |
| Cortex-M33 | thumbv8m.main-none-eabi | No |
| Cortex-M33 | thumbv8m.main-none-eabihf | Yes |
| RISC-V 32 | riscv32imac-unknown-none-elf | No |
| RISC-V 32 | riscv32imc-unknown-none-elf | No |

### 9.2 Essential Crates

| Crate | Purpose |
|-------|---------|
| cortex-m | Cortex-M CPU support |
| cortex-m-rt | Cortex-M runtime |
| panic-halt | Minimal panic handler |
| panic-probe | Panic via probe |
| defmt | Efficient logging |
| heapless | Heap-free data structures |
| embedded-hal | Hardware abstraction traits |
| nb | Non-blocking API |

### 9.3 HAL Crates por MCU

| Manufacturer | Crate |
|--------------|-------|
| STM32 | stm32f4xx-hal, stm32h7xx-hal, etc. |
| Nordic | nrf52840-hal, nrf-hal |
| Raspberry Pi | rp2040-hal |
| ESP32 | esp-hal, esp32-hal |
| Atmel/Microchip | atsamd-hal |
| NXP | lpc-hal, imxrt-hal |

### 9.4 Memory Sections

| Section | Purpose |
|---------|---------|
| .text | Code |
| .rodata | Read-only data |
| .data | Initialized data |
| .bss | Zero-initialized data |
| .vector_table | Interrupt vectors |
| .ccmram | Core-coupled memory |
| .backup | Backup SRAM |

---

## Referencias y Recursos Adicionales

1. **Embedded Rust Book**: https://docs.rust-embedded.org/book/
2. **Discovery Book**: https://docs.rust-embedded.org/discovery/
3. **Embedonomicon**: https://docs.rust-embedded.org/embedonomicon/
4. **RTIC**: https://rtic.rs/
5. **Embassy**: https://embassy.dev/

---

*Documento generado por ARCHAEON_CORE - Sistema de Documentación de Lenguajes*
*Especialización: Embedded C → Rust Embedded*
*Última actualización: 2025-12-31*

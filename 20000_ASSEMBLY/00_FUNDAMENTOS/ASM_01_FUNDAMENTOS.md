# ASM_01: Fundamentos del Lenguaje Assembly

> "Assembly no es difícil. Es honesto con el hardware."

---

## Qué es Assembly

Assembly es el lenguaje de programación de más bajo nivel que sigue siendo legible por humanos. Cada instrucción corresponde directamente a una operación del procesador.

```
Código fuente → Compilador → Assembly → Ensamblador → Código máquina
     (C)           (gcc)      (.s)       (as)         (binario)
```

### Características Fundamentales

| Característica | Descripción |
|----------------|-------------|
| **1:1 con máquina** | Cada instrucción = una operación del CPU |
| **Específico de arquitectura** | x86 ≠ ARM ≠ MIPS |
| **Control total** | Acceso directo a registros y memoria |
| **Sin abstracciones** | No hay tipos, no hay objetos, no hay GC |
| **Máximo rendimiento** | El programador controla cada ciclo |

---

## Conceptos Universales

### 1. Registros

Los registros son la memoria más rápida del procesador. Cada arquitectura tiene su conjunto:

```
┌─────────────────────────────────────────────────────────────┐
│                         CPU                                  │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐           │
│  │   R0    │ │   R1    │ │   R2    │ │   ...   │ Registros │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘           │
│                      ↕                                       │
│              ┌─────────────┐                                │
│              │     ALU     │  Unidad Aritmético-Lógica      │
│              └─────────────┘                                │
│                      ↕                                       │
│              ┌─────────────┐                                │
│              │   Cache     │                                │
│              └─────────────┘                                │
└──────────────────────↕──────────────────────────────────────┘
                       ↕
               ┌─────────────┐
               │    RAM      │  Memoria Principal
               └─────────────┘
```

**Tipos de Registros:**

| Tipo | Propósito | Ejemplo x86 | Ejemplo ARM |
|------|-----------|-------------|-------------|
| General | Datos y direcciones | EAX, EBX, ECX, EDX | R0-R12 |
| Puntero de pila | Tope de la pila | ESP/RSP | SP |
| Puntero base | Base del frame | EBP/RBP | FP (R11) |
| Contador programa | Siguiente instrucción | EIP/RIP | PC (R15) |
| Flags | Estado del CPU | EFLAGS | CPSR |

### 2. Memoria

La memoria se organiza en direcciones lineales. Cada dirección apunta a un byte.

```
Dirección    Contenido
0x00000000   [ byte 0 ]
0x00000001   [ byte 1 ]
0x00000002   [ byte 2 ]
...
0xFFFFFFFF   [ byte max ]
```

**Endianness:**

```
Valor: 0x12345678

Little Endian (x86):        Big Endian (MIPS, 68000):
Addr+0: 0x78                Addr+0: 0x12
Addr+1: 0x56                Addr+1: 0x34
Addr+2: 0x34                Addr+2: 0x56
Addr+3: 0x12                Addr+3: 0x78
```

### 3. La Pila (Stack)

La pila es una estructura LIFO (Last In, First Out) usada para:
- Guardar direcciones de retorno
- Pasar parámetros
- Variables locales
- Guardar registros

```
Memoria Alta
┌────────────────┐
│   ...          │
├────────────────┤
│  Var local 2   │ ← ESP/SP apunta aquí
├────────────────┤
│  Var local 1   │
├────────────────┤
│  EBP guardado  │ ← Frame pointer
├────────────────┤
│  Dir. retorno  │
├────────────────┤
│  Parámetro 1   │
├────────────────┤
│  Parámetro 2   │
├────────────────┤
│   ...          │
└────────────────┘
Memoria Baja
```

### 4. Instrucciones Básicas

Todas las arquitecturas comparten estas categorías:

| Categoría | Operación | x86 | ARM | MIPS |
|-----------|-----------|-----|-----|------|
| **Mover datos** | Copiar valor | MOV | MOV | LW/SW |
| **Aritmética** | Sumar | ADD | ADD | ADD |
| | Restar | SUB | SUB | SUB |
| | Multiplicar | MUL/IMUL | MUL | MULT |
| | Dividir | DIV/IDIV | (soft) | DIV |
| **Lógica** | AND | AND | AND | AND |
| | OR | OR | ORR | OR |
| | XOR | XOR | EOR | XOR |
| | NOT | NOT | MVN | NOR |
| **Shifts** | Izquierda | SHL | LSL | SLL |
| | Derecha lógico | SHR | LSR | SRL |
| | Derecha aritmético | SAR | ASR | SRA |
| **Comparar** | Comparar | CMP | CMP | (SUB) |
| **Saltos** | Incondicional | JMP | B | J |
| | Condicional | JE, JNE, JG... | BEQ, BNE... | BEQ, BNE... |
| **Pila** | Push | PUSH | PUSH/STM | (manual) |
| | Pop | POP | POP/LDM | (manual) |
| **Llamadas** | Llamar función | CALL | BL | JAL |
| | Retornar | RET | BX LR | JR $ra |

---

## Modos de Direccionamiento

Los modos de direccionamiento determinan cómo se obtiene un operando.

### Tabla de Modos

| Modo | Descripción | Ejemplo x86 | Ejemplo ARM |
|------|-------------|-------------|-------------|
| **Inmediato** | Valor en la instrucción | MOV EAX, 42 | MOV R0, #42 |
| **Registro** | Valor en registro | MOV EAX, EBX | MOV R0, R1 |
| **Directo** | Valor en dirección fija | MOV EAX, [0x1000] | LDR R0, =addr |
| **Indirecto** | Dirección en registro | MOV EAX, [EBX] | LDR R0, [R1] |
| **Base+Offset** | Base + desplazamiento | MOV EAX, [EBX+8] | LDR R0, [R1, #8] |
| **Base+Index** | Base + registro índice | MOV EAX, [EBX+ECX] | LDR R0, [R1, R2] |
| **Escalado** | Base + Index*escala | MOV EAX, [EBX+ECX*4] | (LSL en offset) |

### Visualización de Modos

```
Inmediato:
  MOV EAX, 42
  ┌─────┐
  │ 42  │ → EAX
  └─────┘

Registro:
  MOV EAX, EBX
  ┌─────┐    ┌─────┐
  │ EBX │ ─→ │ EAX │
  └─────┘    └─────┘

Indirecto (puntero):
  MOV EAX, [EBX]
  ┌─────┐     ┌─────────┐     ┌─────┐
  │ EBX │ ──→ │ Memoria │ ──→ │ EAX │
  │0x100│     │ [0x100] │     │     │
  └─────┘     └─────────┘     └─────┘

Base+Offset (struct/array):
  MOV EAX, [EBX+8]
  ┌─────┐           ┌─────────┐     ┌─────┐
  │ EBX │ + 8  ──→  │ Memoria │ ──→ │ EAX │
  │0x100│           │ [0x108] │     │     │
  └─────┘           └─────────┘     └─────┘

Escalado (array de structs):
  MOV EAX, [EBX+ECX*4]
  ┌─────┐ ┌─────┐           ┌─────────┐     ┌─────┐
  │ EBX │+│ ECX │×4  ───→   │ Memoria │ ──→ │ EAX │
  │0x100│ │  3  │           │ [0x10C] │     │     │
  └─────┘ └─────┘           └─────────┘     └─────┘
```

---

## Flags y Condiciones

El registro de flags guarda el resultado de la última operación.

### Flags Comunes

| Flag | Nombre | Significado |
|------|--------|-------------|
| ZF | Zero Flag | Resultado fue cero |
| SF | Sign Flag | Resultado negativo (bit más significativo = 1) |
| CF | Carry Flag | Hubo acarreo/préstamo (unsigned overflow) |
| OF | Overflow Flag | Overflow con signo |
| PF | Parity Flag | Número par de bits en 1 |

### Ejemplo de Flags

```asm
; Después de SUB EAX, EBX:
;
; Si EAX = 5, EBX = 5:
;   Resultado = 0
;   ZF = 1 (resultado cero)
;   SF = 0 (no negativo)
;
; Si EAX = 3, EBX = 5:
;   Resultado = -2 (0xFFFFFFFE)
;   ZF = 0 (no cero)
;   SF = 1 (negativo)
;   CF = 1 (borrow en unsigned)
```

### Saltos Condicionales

| Condición | x86 | ARM | Significado |
|-----------|-----|-----|-------------|
| Igual | JE/JZ | BEQ | ZF = 1 |
| No igual | JNE/JNZ | BNE | ZF = 0 |
| Mayor (signed) | JG | BGT | ZF=0 AND SF=OF |
| Mayor o igual | JGE | BGE | SF = OF |
| Menor (signed) | JL | BLT | SF ≠ OF |
| Menor o igual | JLE | BLE | ZF=1 OR SF≠OF |
| Above (unsigned) | JA | BHI | CF=0 AND ZF=0 |
| Below (unsigned) | JB | BLO | CF = 1 |

---

## Convenciones de Llamada

Las convenciones de llamada definen cómo las funciones reciben parámetros y devuelven valores.

### Comparación de Convenciones

| Aspecto | x86 cdecl | x86-64 SysV | ARM AAPCS |
|---------|-----------|-------------|-----------|
| **Parámetros** | Stack | RDI,RSI,RDX,RCX,R8,R9 | R0-R3 |
| **Retorno** | EAX | RAX | R0 |
| **Caller-saved** | EAX,ECX,EDX | RAX,RCX,RDX,RSI,RDI,R8-R11 | R0-R3,R12 |
| **Callee-saved** | EBX,ESI,EDI,EBP | RBX,RBP,R12-R15 | R4-R11 |
| **Limpieza stack** | Caller | Caller | Caller |

### Stack Frame Típico

```
┌────────────────────────┐ ← Dirección alta
│     Parámetro N        │
│     ...                │
│     Parámetro 1        │
├────────────────────────┤
│   Dirección retorno    │ ← Guardada por CALL
├────────────────────────┤
│   EBP/RBP anterior     │ ← Frame pointer guardado
├────────────────────────┤ ← EBP/RBP actual
│   Variable local 1     │
│   Variable local 2     │
│   ...                  │
│   Registros guardados  │
├────────────────────────┤ ← ESP/RSP
│   (espacio libre)      │
└────────────────────────┘ ← Dirección baja
```

---

## Sintaxis: Intel vs AT&T

Existen dos sintaxis principales para x86:

| Aspecto | Intel | AT&T |
|---------|-------|------|
| **Orden operandos** | dest, src | src, dest |
| **Prefijo registro** | ninguno | % |
| **Prefijo inmediato** | ninguno | $ |
| **Tamaño** | DWORD PTR | sufijo (l, w, b) |
| **Memoria** | [base+index*scale+disp] | disp(base,index,scale) |

### Ejemplo Comparativo

```asm
; Intel syntax (NASM, MASM):
mov eax, [ebx+ecx*4+8]
add eax, 42
push eax
call my_function

; AT&T syntax (GAS):
movl 8(%ebx,%ecx,4), %eax
addl $42, %eax
pushl %eax
call my_function
```

---

## Secciones del Programa

Un programa en assembly tiene secciones definidas:

```asm
section .data       ; Datos inicializados
    msg db "Hello", 0
    count dd 42

section .bss        ; Datos no inicializados
    buffer resb 256
    number resd 1

section .text       ; Código ejecutable
    global _start

_start:
    ; código aquí
```

### Secciones Comunes

| Sección | Propósito | Permisos |
|---------|-----------|----------|
| .text | Código ejecutable | R-X |
| .data | Datos inicializados | RW- |
| .bss | Datos no inicializados | RW- |
| .rodata | Constantes | R-- |

---

## Herramientas del Ecosistema

### Ensambladores

| Herramienta | Plataforma | Sintaxis | Uso |
|-------------|------------|----------|-----|
| NASM | Multi | Intel | Popular, portable |
| MASM | Windows | Intel | Microsoft |
| GAS | Multi | AT&T | GNU toolchain |
| FASM | Multi | Intel | Flat assembler |
| YASM | Multi | Intel/AT&T | Modular |

### Depuradores

| Herramienta | Plataforma | Características |
|-------------|------------|-----------------|
| GDB | Unix/Linux | Estándar, scriptable |
| LLDB | Multi | LLVM project |
| WinDbg | Windows | Kernel debugging |
| x64dbg | Windows | GUI, plugins |
| Radare2 | Multi | Reversing framework |
| IDA Pro | Multi | Industria estándar |

### Desensambladores

| Herramienta | Tipo | Uso |
|-------------|------|-----|
| objdump | Línea de comandos | Quick analysis |
| Ghidra | GUI + decompiler | NSA, gratuito |
| IDA Pro | GUI + decompiler | Comercial, estándar |
| Binary Ninja | GUI + API | Moderno, API Python |

---

## Ejemplo: Hello World Multi-arquitectura

### x86-64 Linux (NASM)

```asm
section .data
    msg db "Hello, World!", 10
    len equ $ - msg

section .text
    global _start

_start:
    ; write(1, msg, len)
    mov rax, 1          ; syscall: write
    mov rdi, 1          ; fd: stdout
    mov rsi, msg        ; buffer
    mov rdx, len        ; count
    syscall

    ; exit(0)
    mov rax, 60         ; syscall: exit
    xor rdi, rdi        ; status: 0
    syscall
```

### ARM Linux (GAS)

```asm
.data
    msg: .asciz "Hello, World!\n"
    len = . - msg

.text
.global _start

_start:
    @ write(1, msg, len)
    mov r7, #4          @ syscall: write
    mov r0, #1          @ fd: stdout
    ldr r1, =msg        @ buffer
    mov r2, #len        @ count
    swi 0

    @ exit(0)
    mov r7, #1          @ syscall: exit
    mov r0, #0          @ status
    swi 0
```

### MIPS Linux (GAS)

```asm
.data
    msg: .asciiz "Hello, World!\n"

.text
.globl main

main:
    # print string
    li $v0, 4           # syscall: print_string
    la $a0, msg         # string address
    syscall

    # exit
    li $v0, 10          # syscall: exit
    syscall
```

---

## Por Qué Aprender Assembly

### Razones Técnicas

1. **Entender el hardware**: Saber qué hace realmente el CPU
2. **Optimización**: Escribir código crítico de rendimiento
3. **Debugging**: Entender crashes y core dumps
4. **Seguridad**: Análisis de malware, exploits, reversing
5. **Sistemas embebidos**: Bare metal, bootloaders, drivers
6. **Compiladores**: Entender el output del compilador

### Razones Filosóficas

> "Los que no entienden assembly están condenados a reinventar GCC... pobremente."
> — Henry Spencer (adaptado)

Assembly te enseña:
- No hay magia, solo instrucciones
- Todo tiene un costo
- La memoria no es infinita
- Los bugs de C tienen explicación

---

## Recursos de Aprendizaje

### Libros Clásicos

| Título | Autor | Enfoque |
|--------|-------|---------|
| "Programming from the Ground Up" | Bartlett | x86 Linux, gratis |
| "The Art of Assembly Language" | Hyde | x86, completo |
| "ARM System Developer's Guide" | Sloss et al | ARM embedded |
| "See MIPS Run" | Sweetman | MIPS architecture |

### Prácticas Recomendadas

1. **Escribir funciones pequeñas**: Empezar con suma, factorial
2. **Leer output del compilador**: `gcc -S` es tu amigo
3. **Usar un debugger**: Paso a paso con GDB
4. **Resolver CTFs**: Práctica de reversing
5. **Escribir un bootloader**: Entender bare metal

---

## Próximos Documentos

- **ASM_02_x86.md**: Arquitectura x86/x64 en detalle
- **ASM_03_ARM.md**: ARM y AArch64
- **ASM_04_MIPS.md**: MIPS clásico y moderno
- **ASM_05_RETRO.md**: 6502, Z80, 68000

---

*"En assembly, no hay abstracciones que te protejan. Pero tampoco hay abstracciones que te limiten."*


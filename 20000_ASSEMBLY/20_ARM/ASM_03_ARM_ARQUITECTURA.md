# ASM_03: Arquitectura ARM

> "ARM demostró que la eficiencia supera la fuerza bruta. Domina móviles, IoT, y ahora servidores."

---

## Historia y Evolución

```
1985: ARM1     (Acorn)           ← 32-bit RISC
1991: ARM6     (Apple Newton)
1997: ARM7TDMI (Game Boy Advance)
2004: ARM11    (Raspberry Pi 1)
2011: Cortex-A15 (smartphones modernos)
2016: ARMv8/AArch64 (64-bit)
2020: Apple M1  (ARM en desktop)
```

### Familias ARM

| Familia | Uso | Características |
|---------|-----|-----------------|
| Cortex-M | Microcontroladores | Ultra bajo consumo, simple |
| Cortex-R | Real-time | Determinismo, safety-critical |
| Cortex-A | Application | Alto rendimiento, OS completo |
| Neoverse | Servidores | Cloud, datacenter |

### Modos de Ejecución

| Modo | Bits | Set de Instrucciones |
|------|------|---------------------|
| ARM | 32 | Instrucciones de 32 bits |
| Thumb | 16/32 | Instrucciones compactas |
| AArch64 | 64 | ARMv8, instrucciones de 32 bits |

---

## Registros ARM32

### Registros de Propósito General

```
┌─────────┬─────────────────────────────────────────────┐
│ Registro│ Alias/Uso                                   │
├─────────┼─────────────────────────────────────────────┤
│   R0    │ Argumento 1 / Retorno                       │
│   R1    │ Argumento 2                                 │
│   R2    │ Argumento 3                                 │
│   R3    │ Argumento 4                                 │
├─────────┼─────────────────────────────────────────────┤
│   R4    │ Variable (callee-saved)                     │
│   R5    │ Variable (callee-saved)                     │
│   R6    │ Variable (callee-saved)                     │
│   R7    │ Variable (callee-saved) / Syscall number    │
│   R8    │ Variable (callee-saved)                     │
│   R9    │ Platform register (callee-saved)            │
│   R10   │ Variable (callee-saved)                     │
├─────────┼─────────────────────────────────────────────┤
│   R11   │ FP - Frame Pointer (callee-saved)           │
│   R12   │ IP - Intra-Procedure scratch                │
│   R13   │ SP - Stack Pointer                          │
│   R14   │ LR - Link Register (return address)         │
│   R15   │ PC - Program Counter                        │
├─────────┼─────────────────────────────────────────────┤
│  CPSR   │ Current Program Status Register             │
└─────────┴─────────────────────────────────────────────┘
```

### Registro CPSR

```
┌────┬────┬────┬────┬───────────────────┬────┬────┬────┬────┬────┐
│ N  │ Z  │ C  │ V  │      ...          │ I  │ F  │ T  │ M4-M0  │
└────┴────┴────┴────┴───────────────────┴────┴────┴────┴────────┘
 31   30   29   28                        7    6    5    4-0

N: Negative     - Resultado negativo
Z: Zero         - Resultado cero
C: Carry        - Acarreo/borrow
V: Overflow     - Overflow con signo
I: IRQ disable  - Deshabilitar IRQ
F: FIQ disable  - Deshabilitar FIQ
T: Thumb mode   - Modo Thumb activo
M: Mode bits    - Modo de ejecución (User, FIQ, IRQ, etc.)
```

---

## Registros AArch64 (ARM64)

### Registros de Propósito General

```
┌─────────┬─────────┬──────────────────────────────────────┐
│ 64-bit  │ 32-bit  │ Uso                                  │
├─────────┼─────────┼──────────────────────────────────────┤
│   X0    │   W0    │ Argumento 1 / Retorno                │
│   X1    │   W1    │ Argumento 2                          │
│   X2    │   W2    │ Argumento 3                          │
│   X3    │   W3    │ Argumento 4                          │
│   X4    │   W4    │ Argumento 5                          │
│   X5    │   W5    │ Argumento 6                          │
│   X6    │   W6    │ Argumento 7                          │
│   X7    │   W7    │ Argumento 8                          │
├─────────┼─────────┼──────────────────────────────────────┤
│  X8     │   W8    │ Indirect result / Syscall number     │
│  X9-X15 │  W9-W15 │ Temporales (caller-saved)            │
├─────────┼─────────┼──────────────────────────────────────┤
│ X16     │   W16   │ IP0 - Intra-Procedure call scratch   │
│ X17     │   W17   │ IP1 - Intra-Procedure call scratch   │
│ X18     │   W18   │ Platform register                    │
├─────────┼─────────┼──────────────────────────────────────┤
│ X19-X28 │ W19-W28 │ Callee-saved registers               │
├─────────┼─────────┼──────────────────────────────────────┤
│ X29     │   W29   │ FP - Frame Pointer                   │
│ X30     │   W30   │ LR - Link Register                   │
│  SP     │   WSP   │ Stack Pointer                        │
│  PC     │    -    │ Program Counter (no accesible)       │
├─────────┼─────────┼──────────────────────────────────────┤
│  XZR    │   WZR   │ Zero Register (siempre 0)            │
└─────────┴─────────┴──────────────────────────────────────┘
```

---

## Instrucciones ARM32

### Característica Única: Ejecución Condicional

Casi TODAS las instrucciones pueden ejecutarse condicionalmente:

```asm
; Sufijos de condición
EQ  - Equal (Z=1)
NE  - Not Equal (Z=0)
CS  - Carry Set (C=1)
CC  - Carry Clear (C=0)
MI  - Minus/Negative (N=1)
PL  - Plus/Positive (N=0)
VS  - Overflow Set (V=1)
VC  - Overflow Clear (V=0)
HI  - Higher (C=1 AND Z=0)
LS  - Lower or Same (C=0 OR Z=1)
GE  - Greater or Equal (N=V)
LT  - Less Than (N≠V)
GT  - Greater Than (Z=0 AND N=V)
LE  - Less or Equal (Z=1 OR N≠V)
AL  - Always (default)

; Ejemplos
CMP   R0, #10
ADDGT R1, R1, #1    ; if (R0 > 10) R1++
SUBLE R2, R2, #1    ; if (R0 <= 10) R2--
```

### Movimiento de Datos

```asm
; MOV - Mover inmediato o registro
MOV   R0, #42          ; R0 = 42
MOV   R0, R1           ; R0 = R1
MOV   R0, R1, LSL #2   ; R0 = R1 << 2

; MVN - Move NOT
MVN   R0, #0           ; R0 = ~0 = -1

; MOVW/MOVT - Cargar constantes de 32 bits
MOVW  R0, #0x1234      ; Low 16 bits
MOVT  R0, #0x5678      ; High 16 bits
                       ; R0 = 0x56781234

; LDR - Load Register
LDR   R0, [R1]         ; R0 = *R1
LDR   R0, [R1, #4]     ; R0 = *(R1 + 4)
LDR   R0, [R1, R2]     ; R0 = *(R1 + R2)
LDR   R0, [R1, R2, LSL #2]  ; R0 = *(R1 + R2*4)

; Modos de pre/post incremento
LDR   R0, [R1, #4]!    ; Pre-increment: R1 += 4; R0 = *R1
LDR   R0, [R1], #4     ; Post-increment: R0 = *R1; R1 += 4

; STR - Store Register
STR   R0, [R1]         ; *R1 = R0
STR   R0, [R1, #4]     ; *(R1 + 4) = R0

; Load/Store múltiple
LDMIA R13!, {R0-R3}    ; Pop R0,R1,R2,R3 (Increment After)
STMDB R13!, {R0-R3}    ; Push R0,R1,R2,R3 (Decrement Before)

; Aliases para stack
PUSH  {R4-R6, LR}      ; = STMDB SP!, {R4-R6, LR}
POP   {R4-R6, PC}      ; = LDMIA SP!, {R4-R6, PC}
```

### Aritmética

```asm
; Suma
ADD   R0, R1, R2       ; R0 = R1 + R2
ADD   R0, R1, #10      ; R0 = R1 + 10
ADDS  R0, R1, R2       ; R0 = R1 + R2, set flags
ADC   R0, R1, R2       ; R0 = R1 + R2 + Carry

; Resta
SUB   R0, R1, R2       ; R0 = R1 - R2
RSB   R0, R1, R2       ; R0 = R2 - R1 (Reverse Subtract)
SBC   R0, R1, R2       ; R0 = R1 - R2 - !Carry

; Multiplicación
MUL   R0, R1, R2       ; R0 = R1 * R2 (32-bit result)
MLA   R0, R1, R2, R3   ; R0 = R1 * R2 + R3 (multiply-accumulate)
UMULL R0, R1, R2, R3   ; R1:R0 = R2 * R3 (64-bit unsigned)
SMULL R0, R1, R2, R3   ; R1:R0 = R2 * R3 (64-bit signed)

; División (ARMv7+)
SDIV  R0, R1, R2       ; R0 = R1 / R2 (signed)
UDIV  R0, R1, R2       ; R0 = R1 / R2 (unsigned)
```

### Barrel Shifter

ARM tiene un barrel shifter integrado que permite shifts en el segundo operando:

```asm
; Tipos de shift
LSL #n   - Logical Shift Left
LSR #n   - Logical Shift Right
ASR #n   - Arithmetic Shift Right (preserva signo)
ROR #n   - Rotate Right
RRX      - Rotate Right Extended (through carry)

; Ejemplos
ADD   R0, R1, R2, LSL #2   ; R0 = R1 + (R2 << 2)
MOV   R0, R1, ASR #3       ; R0 = R1 >> 3 (signed)
RSB   R0, R1, R1, LSL #4   ; R0 = R1*16 - R1 = R1*15

; Multiplicar por constantes
; x * 5 = x + x*4
ADD   R0, R1, R1, LSL #2

; x * 10 = x*8 + x*2
ADD   R0, R1, R1, LSL #2   ; R0 = x*5
ADD   R0, R0, R0           ; R0 = x*10
```

### Lógica

```asm
AND   R0, R1, R2       ; R0 = R1 & R2
ORR   R0, R1, R2       ; R0 = R1 | R2
EOR   R0, R1, R2       ; R0 = R1 ^ R2
BIC   R0, R1, R2       ; R0 = R1 & ~R2 (bit clear)
MVN   R0, R1           ; R0 = ~R1

; Bit manipulation (ARMv6T2+)
BFI   R0, R1, #8, #4   ; Insert 4 bits from R1 at position 8
BFC   R0, #8, #4       ; Clear 4 bits at position 8
UBFX  R0, R1, #8, #4   ; Extract 4 unsigned bits from position 8
SBFX  R0, R1, #8, #4   ; Extract 4 signed bits from position 8
```

### Comparación y Saltos

```asm
; Comparar
CMP   R0, R1           ; Set flags for R0 - R1
CMP   R0, #10          ; Set flags for R0 - 10
CMN   R0, R1           ; Set flags for R0 + R1
TST   R0, R1           ; Set flags for R0 & R1
TEQ   R0, R1           ; Set flags for R0 ^ R1

; Saltos
B     label            ; Branch (incondicional)
BEQ   label            ; Branch if Equal
BNE   label            ; Branch if Not Equal
BGT   label            ; Branch if Greater Than
BLT   label            ; Branch if Less Than
BL    function         ; Branch and Link (call)

; Retorno
BX    LR               ; Branch and eXchange (return)
BLX   R0               ; Branch Link and eXchange (call via register)
```

---

## Instrucciones AArch64

### Movimiento de Datos

```asm
; MOV
MOV   X0, #42          ; X0 = 42
MOV   X0, X1           ; X0 = X1
MOV   X0, XZR          ; X0 = 0

; Cargar constantes grandes
MOVZ  X0, #0x1234, LSL #16   ; X0 = 0x12340000
MOVK  X0, #0x5678            ; X0 = 0x12345678 (keep other bits)

; Load/Store
LDR   X0, [X1]         ; Load 64-bit
LDR   W0, [X1]         ; Load 32-bit
LDRH  W0, [X1]         ; Load 16-bit halfword
LDRB  W0, [X1]         ; Load 8-bit byte
LDRSW X0, [X1]         ; Load 32-bit, sign-extend to 64
LDRSH X0, [X1]         ; Load 16-bit, sign-extend to 64

; Load pair
LDP   X0, X1, [SP]     ; Load pair of registers
STP   X0, X1, [SP]     ; Store pair of registers

; Direccionamiento
LDR   X0, [X1, #8]         ; Base + offset
LDR   X0, [X1, X2]         ; Base + register
LDR   X0, [X1, X2, LSL #3] ; Base + (register << 3)
LDR   X0, [X1, #8]!        ; Pre-index
LDR   X0, [X1], #8         ; Post-index
```

### Aritmética AArch64

```asm
; Suma/Resta
ADD   X0, X1, X2       ; X0 = X1 + X2
ADD   X0, X1, #100     ; X0 = X1 + 100
ADDS  X0, X1, X2       ; ADD + set flags
SUB   X0, X1, X2       ; X0 = X1 - X2
SUBS  X0, X1, X2       ; SUB + set flags

; Con extensión
ADD   X0, X1, W2, SXTW ; X0 = X1 + sign_extend(W2)
ADD   X0, X1, W2, UXTW ; X0 = X1 + zero_extend(W2)

; Multiplicación
MUL   X0, X1, X2       ; X0 = X1 * X2
MADD  X0, X1, X2, X3   ; X0 = X1 * X2 + X3
MSUB  X0, X1, X2, X3   ; X0 = X3 - X1 * X2
SMULL X0, W1, W2       ; X0 = W1 * W2 (signed)
UMULL X0, W1, W2       ; X0 = W1 * W2 (unsigned)

; División
SDIV  X0, X1, X2       ; X0 = X1 / X2 (signed)
UDIV  X0, X1, X2       ; X0 = X1 / X2 (unsigned)
```

### Saltos y Control

```asm
; Comparar
CMP   X0, X1           ; Compare
CMP   X0, #10          ; Compare with immediate
TST   X0, X1           ; Test bits

; Conditional select
CSEL  X0, X1, X2, EQ   ; X0 = (EQ) ? X1 : X2
CSINC X0, X1, X2, NE   ; X0 = (NE) ? X1 : X2+1
CSET  X0, EQ           ; X0 = (EQ) ? 1 : 0

; Saltos
B     label            ; Branch
B.EQ  label            ; Branch if equal
B.NE  label            ; Branch if not equal
B.GT  label            ; Branch if greater than
BL    function         ; Branch and Link (call)
RET                    ; Return (uses X30/LR)
BR    X0               ; Branch to register
BLR   X0               ; Branch and Link to register
```

---

## Convención de Llamada ARM (AAPCS)

### ARM32

```asm
; Parámetros: R0-R3 (resto en stack)
; Retorno: R0 (y R1 para 64-bit)
; Caller-saved: R0-R3, R12
; Callee-saved: R4-R11, LR

; Llamar función
MOV   R0, #1           ; arg1
MOV   R1, #2           ; arg2
BL    my_function
; resultado en R0

; Implementar función
my_function:
    PUSH  {R4-R6, LR}  ; guardar callee-saved + LR
    ; ... código ...
    POP   {R4-R6, PC}  ; restaurar y retornar (PC = LR)
```

### AArch64 (AAPCS64)

```asm
; Parámetros: X0-X7 (resto en stack)
; Retorno: X0 (y X1 para 128-bit)
; Caller-saved: X0-X18
; Callee-saved: X19-X28, X29(FP), X30(LR)

; Llamar función
MOV   X0, #1
MOV   X1, #2
BL    my_function
; resultado en X0

; Implementar función
my_function:
    STP   X29, X30, [SP, #-16]!  ; push FP, LR
    MOV   X29, SP                ; set frame pointer
    ; ... código ...
    LDP   X29, X30, [SP], #16    ; pop FP, LR
    RET
```

---

## System Calls (Linux ARM64)

```asm
; Syscall: X8 = número, args en X0-X5
; Retorno en X0

.global _start
_start:
    ; write(1, msg, len)
    MOV   X0, #1           ; fd = stdout
    LDR   X1, =msg         ; buffer
    MOV   X2, #13          ; length
    MOV   X8, #64          ; syscall write
    SVC   #0               ; supervisor call

    ; exit(0)
    MOV   X0, #0           ; status
    MOV   X8, #93          ; syscall exit
    SVC   #0

.data
msg: .asciz "Hello, ARM64!\n"
```

### Syscalls Comunes ARM64

| Número | Nombre | Argumentos |
|--------|--------|------------|
| 63 | read | fd, buf, count |
| 64 | write | fd, buf, count |
| 56 | openat | dirfd, path, flags, mode |
| 57 | close | fd |
| 222 | mmap | addr, len, prot, flags, fd, offset |
| 93 | exit | status |
| 220 | clone | flags, stack, ... |
| 221 | execve | path, argv, envp |

---

## NEON/SIMD

### Registros NEON

```
ARM32:  D0-D31 (64-bit), Q0-Q15 (128-bit)
ARM64:  V0-V31 (128-bit)

Interpretaciones:
V0.16B = 16 x 8-bit
V0.8H  = 8 x 16-bit
V0.4S  = 4 x 32-bit
V0.2D  = 2 x 64-bit
```

### Operaciones NEON

```asm
; Cargar
LD1   {V0.4S}, [X0]      ; Load 4 x 32-bit
LD2   {V0.4S, V1.4S}, [X0]  ; Load interleaved

; Aritmética
ADD   V0.4S, V1.4S, V2.4S  ; 4 sumas paralelas
MUL   V0.4S, V1.4S, V2.4S  ; 4 multiplicaciones
FMUL  V0.4S, V1.4S, V2.4S  ; 4 float multiplications

; Reduce
ADDV  S0, V0.4S          ; Suma horizontal de V0

; Almacenar
ST1   {V0.4S}, [X0]      ; Store 4 x 32-bit
```

---

## Patrones Comunes ARM

### Loop

```asm
; ARM32
    MOV   R4, #10        ; i = 10
loop:
    ; ... cuerpo ...
    SUBS  R4, R4, #1     ; i-- y set flags
    BNE   loop           ; while (i != 0)

; ARM64
    MOV   X4, #10
loop:
    ; ... cuerpo ...
    SUBS  X4, X4, #1
    B.NE  loop
```

### If-Else (con ejecución condicional)

```asm
; ARM32: if (R0 > 10) R1 = 1; else R1 = 0;
CMP   R0, #10
MOVGT R1, #1
MOVLE R1, #0

; ARM64: if (X0 > 10) X1 = 1; else X1 = 0;
CMP   X0, #10
CSET  X1, GT
```

### Máximo de dos valores

```asm
; ARM32
CMP   R0, R1
MOVLT R0, R1           ; if R0 < R1, R0 = R1

; ARM64
CMP   X0, X1
CSEL  X0, X1, X0, GT   ; X0 = (X1 > X0) ? X1 : X0
```

---

## Herramientas

### Cross-compilation

```bash
# Instalar toolchain
sudo apt install gcc-arm-linux-gnueabihf     # ARM32
sudo apt install gcc-aarch64-linux-gnu       # ARM64

# Compilar
arm-linux-gnueabihf-gcc -o prog prog.c       # ARM32
aarch64-linux-gnu-gcc -o prog prog.c         # ARM64

# Ensamblar
arm-linux-gnueabihf-as -o prog.o prog.s
aarch64-linux-gnu-as -o prog.o prog.s
```

### Emulación con QEMU

```bash
# Instalar
sudo apt install qemu-user

# Ejecutar binario ARM en x86
qemu-arm ./prog_arm32
qemu-aarch64 ./prog_arm64

# Con GDB
qemu-aarch64 -g 1234 ./prog &
gdb-multiarch ./prog
(gdb) target remote :1234
```

---

## Próximos Documentos

- **ASM_03_ARM_THUMB.md**: Modo Thumb en detalle
- **ASM_03_ARM_CORTEX_M.md**: Programación de microcontroladores
- **ASM_03_ARM_NEON.md**: SIMD en profundidad

---

*"ARM probó que la eficiencia energética gana la carrera. Tu teléfono es más poderoso que los mainframes de 1990."*


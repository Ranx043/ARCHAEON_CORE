# ASM_04: Arquitectura MIPS

> "MIPS es el ejemplo perfecto de RISC: simple, elegante, y didáctico. Si entiendes MIPS, entiendes RISC."

---

## Historia y Contexto

```
1985: MIPS R2000    ← Primera implementación comercial
1988: MIPS R3000    (PlayStation 1)
1991: MIPS R4000    (64-bit, SGI workstations)
1996: MIPS R10000   (superscalar)
2000: MIPS32/MIPS64 (estandarización)
2012: MIPS aptiv    (IoT, networking)
2021: RISC-V...     (MIPS declina)
```

### Dónde encontrar MIPS

| Dispositivo | Uso |
|-------------|-----|
| PlayStation 1, 2, PSP | Gaming |
| Nintendo 64 | Gaming |
| Routers (Cisco, Linksys) | Networking |
| Cámaras IP | Embedded |
| Educación | Casi todas las universidades |

### ¿Por qué estudiar MIPS?

1. **Didáctico**: Diseño limpio, fácil de entender
2. **RISC puro**: Sin las complejidades de x86
3. **Libros clásicos**: Patterson & Hennessy
4. **Todavía existe**: Embedded, routers, legacy
5. **Base para RISC-V**: Herencia directa

---

## Registros MIPS32

### Los 32 Registros

```
┌─────────┬─────────┬────────────────────────────────────────┐
│ Número  │ Nombre  │ Uso / Convención                       │
├─────────┼─────────┼────────────────────────────────────────┤
│   $0    │  $zero  │ Constante 0 (hardwired)                │
│   $1    │  $at    │ Assembler Temporary (reservado)        │
├─────────┼─────────┼────────────────────────────────────────┤
│   $2    │  $v0    │ Valor de retorno / syscall number      │
│   $3    │  $v1    │ Valor de retorno (segundo)             │
├─────────┼─────────┼────────────────────────────────────────┤
│   $4    │  $a0    │ Argumento 1                            │
│   $5    │  $a1    │ Argumento 2                            │
│   $6    │  $a2    │ Argumento 3                            │
│   $7    │  $a3    │ Argumento 4                            │
├─────────┼─────────┼────────────────────────────────────────┤
│  $8-$15 │ $t0-$t7 │ Temporales (caller-saved)              │
├─────────┼─────────┼────────────────────────────────────────┤
│ $16-$23 │ $s0-$s7 │ Saved (callee-saved)                   │
├─────────┼─────────┼────────────────────────────────────────┤
│ $24-$25 │ $t8-$t9 │ Más temporales                         │
├─────────┼─────────┼────────────────────────────────────────┤
│ $26-$27 │ $k0-$k1 │ Reservados para kernel                 │
├─────────┼─────────┼────────────────────────────────────────┤
│   $28   │  $gp    │ Global Pointer                         │
│   $29   │  $sp    │ Stack Pointer                          │
│   $30   │  $fp    │ Frame Pointer ($s8)                    │
│   $31   │  $ra    │ Return Address                         │
├─────────┼─────────┼────────────────────────────────────────┤
│   HI    │   -     │ High 32 bits de mult/div               │
│   LO    │   -     │ Low 32 bits de mult/div                │
│   PC    │   -     │ Program Counter                        │
└─────────┴─────────┴────────────────────────────────────────┘
```

### Registro $zero

El registro $0 siempre contiene cero. Escribir en él no tiene efecto.

```asm
add $t0, $zero, $zero   ; $t0 = 0
add $t0, $t1, $zero     ; $t0 = $t1 (copy)
beq $t0, $zero, label   ; if ($t0 == 0) goto label
```

---

## Formato de Instrucciones

MIPS tiene solo 3 formatos de instrucción, todos de 32 bits:

### Tipo R (Register)

```
┌────────┬───────┬───────┬───────┬───────┬────────┐
│ opcode │  rs   │  rt   │  rd   │ shamt │ funct  │
│  6 bit │ 5 bit │ 5 bit │ 5 bit │ 5 bit │ 6 bit  │
└────────┴───────┴───────┴───────┴───────┴────────┘

rs:     registro source 1
rt:     registro source 2
rd:     registro destino
shamt:  shift amount
funct:  función (opcode = 0, funct distingue operación)

Ejemplo: add $t0, $t1, $t2
opcode = 0, rs = $t1, rt = $t2, rd = $t0, shamt = 0, funct = 32
```

### Tipo I (Immediate)

```
┌────────┬───────┬───────┬──────────────────────┐
│ opcode │  rs   │  rt   │      immediate       │
│  6 bit │ 5 bit │ 5 bit │        16 bit        │
└────────┴───────┴───────┴──────────────────────┘

immediate: valor constante o offset

Ejemplo: addi $t0, $t1, 100
opcode = 8, rs = $t1, rt = $t0, immediate = 100

Ejemplo: lw $t0, 8($t1)
opcode = 35, rs = $t1, rt = $t0, immediate = 8
```

### Tipo J (Jump)

```
┌────────┬────────────────────────────────────┐
│ opcode │            address                 │
│  6 bit │             26 bit                 │
└────────┴────────────────────────────────────┘

address: dirección de salto (×4, 256MB de rango)

Ejemplo: j label
opcode = 2, address = label/4
```

---

## Instrucciones Aritméticas

### Suma y Resta

```asm
; Tipo R (registro-registro)
add  $t0, $t1, $t2     ; $t0 = $t1 + $t2 (con overflow trap)
addu $t0, $t1, $t2     ; $t0 = $t1 + $t2 (sin overflow trap)
sub  $t0, $t1, $t2     ; $t0 = $t1 - $t2 (con overflow trap)
subu $t0, $t1, $t2     ; $t0 = $t1 - $t2 (sin overflow trap)

; Tipo I (con inmediato)
addi  $t0, $t1, 100    ; $t0 = $t1 + 100 (signed, overflow trap)
addiu $t0, $t1, 100    ; $t0 = $t1 + 100 (sin overflow trap)

; Nota: No hay subi, usar addiu con negativo
addiu $t0, $t1, -50    ; $t0 = $t1 - 50
```

### Multiplicación y División

```asm
; Multiplicación produce resultado de 64 bits en HI:LO
mult  $t0, $t1         ; HI:LO = $t0 × $t1 (signed)
multu $t0, $t1         ; HI:LO = $t0 × $t1 (unsigned)

; Acceder a HI y LO
mfhi  $t2              ; $t2 = HI
mflo  $t2              ; $t2 = LO
mthi  $t2              ; HI = $t2
mtlo  $t2              ; LO = $t2

; División
div   $t0, $t1         ; LO = $t0 / $t1, HI = $t0 % $t1 (signed)
divu  $t0, $t1         ; LO = $t0 / $t1, HI = $t0 % $t1 (unsigned)

; MIPS32 añade:
mul   $t0, $t1, $t2    ; $t0 = $t1 × $t2 (solo 32 bits bajos)
```

---

## Instrucciones Lógicas

```asm
; Lógica con registros
and  $t0, $t1, $t2     ; $t0 = $t1 & $t2
or   $t0, $t1, $t2     ; $t0 = $t1 | $t2
xor  $t0, $t1, $t2     ; $t0 = $t1 ^ $t2
nor  $t0, $t1, $t2     ; $t0 = ~($t1 | $t2)

; NOT usando NOR
nor  $t0, $t1, $zero   ; $t0 = ~$t1

; Lógica con inmediato (zero-extended)
andi $t0, $t1, 0xFF    ; $t0 = $t1 & 0x000000FF
ori  $t0, $t1, 0xFF    ; $t0 = $t1 | 0x000000FF
xori $t0, $t1, 0xFF    ; $t0 = $t1 ^ 0x000000FF
```

---

## Instrucciones de Shift

```asm
; Shift por constante (shamt en instrucción)
sll  $t0, $t1, 4       ; $t0 = $t1 << 4 (shift left logical)
srl  $t0, $t1, 4       ; $t0 = $t1 >> 4 (shift right logical)
sra  $t0, $t1, 4       ; $t0 = $t1 >> 4 (shift right arithmetic)

; Shift por registro
sllv $t0, $t1, $t2     ; $t0 = $t1 << $t2
srlv $t0, $t1, $t2     ; $t0 = $t1 >> $t2
srav $t0, $t1, $t2     ; $t0 = $t1 >> $t2 (arith)

; Multiplicar/dividir por potencias de 2
sll  $t0, $t1, 2       ; × 4
sll  $t0, $t1, 3       ; × 8
srl  $t0, $t1, 2       ; ÷ 4 (unsigned)
sra  $t0, $t1, 2       ; ÷ 4 (signed)
```

---

## Load y Store

MIPS es arquitectura **load-store**: solo LW/SW acceden a memoria.

### Instrucciones de Carga

```asm
; Load Word (32 bits)
lw   $t0, 0($t1)       ; $t0 = Mem[$t1 + 0]
lw   $t0, 4($t1)       ; $t0 = Mem[$t1 + 4]
lw   $t0, -8($t1)      ; $t0 = Mem[$t1 - 8]

; Load Halfword (16 bits)
lh   $t0, 0($t1)       ; $t0 = signext(Mem[$t1]) (signed)
lhu  $t0, 0($t1)       ; $t0 = zeroext(Mem[$t1]) (unsigned)

; Load Byte (8 bits)
lb   $t0, 0($t1)       ; $t0 = signext(Mem[$t1]) (signed)
lbu  $t0, 0($t1)       ; $t0 = zeroext(Mem[$t1]) (unsigned)
```

### Instrucciones de Almacenamiento

```asm
; Store Word (32 bits)
sw   $t0, 0($t1)       ; Mem[$t1 + 0] = $t0

; Store Halfword (16 bits)
sh   $t0, 0($t1)       ; Mem[$t1] = $t0[15:0]

; Store Byte (8 bits)
sb   $t0, 0($t1)       ; Mem[$t1] = $t0[7:0]
```

### Cargar Direcciones

```asm
; Load Upper Immediate
lui  $t0, 0x1234       ; $t0 = 0x12340000

; Cargar constante de 32 bits
lui  $t0, 0x1234       ; $t0 = 0x12340000
ori  $t0, $t0, 0x5678  ; $t0 = 0x12345678

; Load Address (pseudo-instrucción)
la   $t0, label        ; $t0 = dirección de label

; Load Immediate (pseudo-instrucción)
li   $t0, 0x12345678   ; Expandido a lui + ori
```

---

## Comparaciones y Branches

### Set on Less Than

```asm
; Comparar y setear
slt  $t0, $t1, $t2     ; $t0 = ($t1 < $t2) ? 1 : 0 (signed)
sltu $t0, $t1, $t2     ; $t0 = ($t1 < $t2) ? 1 : 0 (unsigned)
slti $t0, $t1, 100     ; $t0 = ($t1 < 100) ? 1 : 0 (signed)
sltiu $t0, $t1, 100    ; $t0 = ($t1 < 100) ? 1 : 0 (unsigned)
```

### Branches

```asm
; Branch on Equal/Not Equal
beq  $t0, $t1, label   ; if ($t0 == $t1) goto label
bne  $t0, $t1, label   ; if ($t0 != $t1) goto label

; Branch con $zero
beq  $t0, $zero, label ; if ($t0 == 0) goto label
bne  $t0, $zero, label ; if ($t0 != 0) goto label

; Pseudo-instrucciones (ensamblador las expande)
beqz $t0, label        ; = beq $t0, $zero, label
bnez $t0, label        ; = bne $t0, $zero, label
bgt  $t0, $t1, label   ; if ($t0 > $t1)
bge  $t0, $t1, label   ; if ($t0 >= $t1)
blt  $t0, $t1, label   ; if ($t0 < $t1)
ble  $t0, $t1, label   ; if ($t0 <= $t1)

; Branch on Less/Greater Than Zero
bgtz $t0, label        ; if ($t0 > 0)
blez $t0, label        ; if ($t0 <= 0)
bltz $t0, label        ; if ($t0 < 0)
bgez $t0, label        ; if ($t0 >= 0)
```

### Delay Slot

**IMPORTANTE**: MIPS tiene **branch delay slot**. La instrucción DESPUÉS del branch siempre se ejecuta.

```asm
; El siguiente add SE EJECUTA antes del branch
beq  $t0, $t1, target
add  $t2, $t3, $t4     ; ← DELAY SLOT: siempre ejecutado

; Si no necesitas el delay slot:
beq  $t0, $t1, target
nop                    ; No operation (delay slot vacío)
```

---

## Jumps y Llamadas

```asm
; Jump incondicional
j    label             ; PC = label (26 bits << 2)

; Jump Register
jr   $ra               ; PC = $ra (return)
jr   $t0               ; PC = $t0 (jump table)

; Jump and Link (call)
jal  function          ; $ra = PC + 4; PC = function

; Jump and Link Register
jalr $t0               ; $ra = PC + 4; PC = $t0
jalr $ra, $t0          ; $ra = PC + 4; PC = $t0
```

---

## Convención de Llamada MIPS

### Resumen

| Registros | Uso | Preservados |
|-----------|-----|-------------|
| $a0-$a3 | Argumentos 1-4 | No (caller-saved) |
| $v0-$v1 | Retorno | No |
| $t0-$t9 | Temporales | No (caller-saved) |
| $s0-$s7 | Saved | Sí (callee-saved) |
| $sp | Stack Pointer | Sí |
| $fp | Frame Pointer | Sí |
| $ra | Return Address | Sí |

### Ejemplo de Función

```asm
# int sum(int a, int b) { return a + b; }
sum:
    add  $v0, $a0, $a1   ; return a + b
    jr   $ra             ; return

# Llamar:
    li   $a0, 5          ; a = 5
    li   $a1, 3          ; b = 3
    jal  sum             ; call sum
    nop                  ; delay slot
    # resultado en $v0
```

### Función con Stack Frame

```asm
# int factorial(int n)
factorial:
    # Crear stack frame
    addiu $sp, $sp, -8   ; reservar 8 bytes
    sw    $ra, 4($sp)    ; guardar $ra
    sw    $s0, 0($sp)    ; guardar $s0

    # Caso base
    slti  $t0, $a0, 2    ; if n < 2
    beq   $t0, $zero, recurse
    li    $v0, 1         ; return 1
    j     done

recurse:
    move  $s0, $a0       ; $s0 = n
    addiu $a0, $a0, -1   ; n - 1
    jal   factorial      ; factorial(n-1)
    nop

    mul   $v0, $s0, $v0  ; n * factorial(n-1)

done:
    # Destruir stack frame
    lw    $s0, 0($sp)    ; restaurar $s0
    lw    $ra, 4($sp)    ; restaurar $ra
    addiu $sp, $sp, 8    ; liberar stack
    jr    $ra            ; return
    nop
```

### Stack Frame

```
┌────────────────┐ ← $sp antes de llamar
│   Argumento 5+ │ (si hay más de 4 args)
├────────────────┤
│   Argumento 4  │ (espacio para $a0-$a3)
├────────────────┤
│    ...         │
├────────────────┤ ← $sp al entrar
│   $ra saved    │
├────────────────┤
│   $fp saved    │
├────────────────┤
│   $s0-$s7      │ (los que se usen)
├────────────────┤
│  locals        │
├────────────────┤ ← $sp actual
```

---

## System Calls (SPIM/MARS)

En simuladores educativos:

```asm
# Print integer
li   $v0, 1            ; syscall print_int
move $a0, $t0          ; integer to print
syscall

# Print string
li   $v0, 4            ; syscall print_string
la   $a0, msg          ; address of string
syscall

# Read integer
li   $v0, 5            ; syscall read_int
syscall
move $t0, $v0          ; result in $v0

# Exit
li   $v0, 10           ; syscall exit
syscall

# Allocate memory (sbrk)
li   $v0, 9            ; syscall sbrk
li   $a0, 100          ; bytes to allocate
syscall                ; address in $v0
```

### Syscalls Comunes

| $v0 | Nombre | Argumentos | Retorno |
|-----|--------|------------|---------|
| 1 | print_int | $a0 = int | - |
| 4 | print_string | $a0 = addr | - |
| 5 | read_int | - | $v0 = int |
| 8 | read_string | $a0 = buf, $a1 = len | - |
| 9 | sbrk | $a0 = bytes | $v0 = addr |
| 10 | exit | - | - |
| 11 | print_char | $a0 = char | - |
| 12 | read_char | - | $v0 = char |

---

## Pseudo-instrucciones

El ensamblador expande estas a instrucciones reales:

| Pseudo | Expansión |
|--------|-----------|
| `li $t0, 0x12345678` | `lui $t0, 0x1234; ori $t0, 0x5678` |
| `la $t0, label` | `lui $t0, upper(label); ori $t0, lower(label)` |
| `move $t0, $t1` | `add $t0, $t1, $zero` |
| `blt $t0, $t1, L` | `slt $at, $t0, $t1; bne $at, $zero, L` |
| `bgt $t0, $t1, L` | `slt $at, $t1, $t0; bne $at, $zero, L` |
| `ble $t0, $t1, L` | `slt $at, $t1, $t0; beq $at, $zero, L` |
| `bge $t0, $t1, L` | `slt $at, $t0, $t1; beq $at, $zero, L` |
| `not $t0, $t1` | `nor $t0, $t1, $zero` |
| `nop` | `sll $zero, $zero, 0` |

---

## Ejemplo: Hello World

```asm
.data
msg:    .asciiz "Hello, MIPS!\n"

.text
.globl main

main:
    # Print string
    li   $v0, 4          ; syscall print_string
    la   $a0, msg        ; address of string
    syscall

    # Exit
    li   $v0, 10         ; syscall exit
    syscall
```

---

## MIPS64

Extensión de 64 bits:

```asm
; Registros de 64 bits
; Instrucciones con 'd' suffix

dadd  $t0, $t1, $t2    ; 64-bit add
daddi $t0, $t1, 100    ; 64-bit add immediate
dsub  $t0, $t1, $t2    ; 64-bit subtract
dmult $t0, $t1         ; 64-bit multiply
ddiv  $t0, $t1         ; 64-bit divide

ld    $t0, 0($t1)      ; Load doubleword (64 bits)
sd    $t0, 0($t1)      ; Store doubleword

dsll  $t0, $t1, 4      ; 64-bit shift left
dsrl  $t0, $t1, 4      ; 64-bit shift right logical
dsra  $t0, $t1, 4      ; 64-bit shift right arithmetic
```

---

## Herramientas

### Simuladores

| Herramienta | Descripción |
|-------------|-------------|
| **MARS** | Java, GUI, educativo, syscalls |
| **SPIM** | Clásico, QtSPIM tiene GUI |
| **QEMU** | Emulador completo de sistema |

### Compilación

```bash
# Cross-compiler
mips-linux-gnu-gcc -o prog prog.c
mips-linux-gnu-as -o prog.o prog.s
mips-linux-gnu-ld -o prog prog.o

# Generar assembly desde C
mips-linux-gnu-gcc -S prog.c
```

---

## Patrones Comunes

### Loop

```asm
# for (i = 0; i < 10; i++)
    li   $t0, 0          ; i = 0
loop:
    slti $t1, $t0, 10    ; i < 10?
    beq  $t1, $zero, done
    nop

    # cuerpo del loop

    addiu $t0, $t0, 1    ; i++
    j    loop
    nop
done:
```

### If-Else

```asm
# if (a > b) { ... } else { ... }
    slt  $t0, $a1, $a0   ; a > b?
    beq  $t0, $zero, else_branch
    nop
then_branch:
    # código if
    j    endif
    nop
else_branch:
    # código else
endif:
```

### Array Access

```asm
# array[i] donde array es de words
    sll  $t0, $t1, 2     ; i * 4 (cada word = 4 bytes)
    add  $t0, $t0, $s0   ; base + offset
    lw   $t2, 0($t0)     ; cargar array[i]
```

---

## Próximos Documentos

- **ASM_04_MIPS_PIPELINE.md**: Pipeline de 5 etapas
- **ASM_04_MIPS_EXCEPTIONS.md**: Manejo de excepciones
- **ASM_04_MIPS_FPU.md**: Coprocesador de punto flotante

---

*"MIPS demostró que menos es más. 32 instrucciones bien diseñadas superan a 1000 mal pensadas."*


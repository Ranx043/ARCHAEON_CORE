# ASM_02: Arquitectura x86/x64

> "x86 es el testamento de la compatibilidad hacia atrás. Código de 1978 sigue ejecutándose hoy."

---

## Historia y Evolución

```
1978: 8086    (16-bit)     ← El origen
1985: 80386   (32-bit)     ← Modo protegido, paginación
1989: 80486   (FPU integrada)
1993: Pentium (superscalar)
2003: AMD64   (64-bit)     ← x86-64
2006: Intel 64 (EM64T)
```

### Modos de Operación

| Modo | Bits | Registros | Uso |
|------|------|-----------|-----|
| Real | 16 | AX, BX... | Boot, BIOS, DOS |
| Protected | 32 | EAX, EBX... | Windows 32-bit, Linux i386 |
| Long | 64 | RAX, RBX... | Sistemas modernos |
| Compatibility | 32 en 64 | EAX... | Apps 32-bit en OS 64-bit |

---

## Registros

### Registros de Propósito General

```
64-bit    32-bit   16-bit   8-bit high   8-bit low
┌─────────┬────────┬────────┬────────────┬──────────┐
│   RAX   │  EAX   │   AX   │     AH     │    AL    │  Accumulator
│   RBX   │  EBX   │   BX   │     BH     │    BL    │  Base
│   RCX   │  ECX   │   CX   │     CH     │    CL    │  Counter
│   RDX   │  EDX   │   DX   │     DH     │    DL    │  Data
│   RSI   │  ESI   │   SI   │     -      │   SIL    │  Source Index
│   RDI   │  EDI   │   DI   │     -      │   DIL    │  Dest Index
│   RBP   │  EBP   │   BP   │     -      │   BPL    │  Base Pointer
│   RSP   │  ESP   │   SP   │     -      │   SPL    │  Stack Pointer
│   R8    │  R8D   │  R8W   │     -      │   R8B    │  (x64 only)
│   R9    │  R9D   │  R9W   │     -      │   R9B    │
│   R10   │  R10D  │  R10W  │     -      │   R10B   │
│   R11   │  R11D  │  R11W  │     -      │   R11B   │
│   R12   │  R12D  │  R12W  │     -      │   R12B   │
│   R13   │  R13D  │  R13W  │     -      │   R13B   │
│   R14   │  R14D  │  R14W  │     -      │   R14B   │
│   R15   │  R15D  │  R15W  │     -      │   R15B   │
└─────────┴────────┴────────┴────────────┴──────────┘
```

### Visualización de Subregistros

```
RAX (64 bits):
┌───────────────────────────────────────────────────────────────┐
│                              RAX                               │
├───────────────────────────────┬───────────────────────────────┤
│           (unused)            │              EAX              │
├───────────────────────────────┼───────────────┬───────────────┤
│                               │    (unused)   │       AX      │
├───────────────────────────────┼───────────────┼───────┬───────┤
│                               │               │  AH   │  AL   │
└───────────────────────────────┴───────────────┴───────┴───────┘
 63                            32              16       8       0
```

### Registros Especiales

| Registro | 32-bit | 64-bit | Propósito |
|----------|--------|--------|-----------|
| Instruction Pointer | EIP | RIP | Próxima instrucción |
| Stack Pointer | ESP | RSP | Tope de la pila |
| Base Pointer | EBP | RBP | Base del stack frame |
| Flags | EFLAGS | RFLAGS | Estado del procesador |

### Registro EFLAGS/RFLAGS

```
┌────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┐
│ OF │ DF │ IF │ TF │ SF │ ZF │  0 │ AF │  0 │ PF │  1 │ CF │
└────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┘
  11   10    9    8    7    6    5    4    3    2    1    0

CF (bit 0):  Carry Flag      - Acarreo/préstamo unsigned
PF (bit 2):  Parity Flag     - Paridad de bits
AF (bit 4):  Auxiliary Flag  - Acarreo BCD
ZF (bit 6):  Zero Flag       - Resultado cero
SF (bit 7):  Sign Flag       - Resultado negativo
TF (bit 8):  Trap Flag       - Single step debugging
IF (bit 9):  Interrupt Flag  - Habilitar interrupciones
DF (bit 10): Direction Flag  - Dirección de strings
OF (bit 11): Overflow Flag   - Overflow con signo
```

---

## Instrucciones Fundamentales

### Movimiento de Datos

```asm
; MOV - Copiar datos
mov eax, 42           ; Inmediato a registro
mov eax, ebx          ; Registro a registro
mov eax, [ebx]        ; Memoria a registro
mov [ebx], eax        ; Registro a memoria
mov eax, [ebx+8]      ; Memoria con offset

; MOVZX - Mover con extensión cero
movzx eax, byte [ebx] ; Byte → Dword (unsigned)
movzx eax, word [ebx] ; Word → Dword (unsigned)

; MOVSX - Mover con extensión de signo
movsx eax, byte [ebx] ; Byte → Dword (signed)
movsx eax, word [ebx] ; Word → Dword (signed)

; LEA - Load Effective Address (calcula dirección)
lea eax, [ebx+ecx*4+8] ; EAX = EBX + ECX*4 + 8
                       ; NO accede a memoria!

; XCHG - Intercambiar
xchg eax, ebx         ; Swap valores
```

### Aritmética

```asm
; Suma
add eax, ebx          ; EAX = EAX + EBX
add eax, 10           ; EAX = EAX + 10
adc eax, ebx          ; EAX = EAX + EBX + CF (con carry)
inc eax               ; EAX++

; Resta
sub eax, ebx          ; EAX = EAX - EBX
sbb eax, ebx          ; EAX = EAX - EBX - CF (con borrow)
dec eax               ; EAX--
neg eax               ; EAX = -EAX

; Multiplicación
mul ebx               ; EDX:EAX = EAX * EBX (unsigned)
imul ebx              ; EDX:EAX = EAX * EBX (signed)
imul eax, ebx         ; EAX = EAX * EBX
imul eax, ebx, 10     ; EAX = EBX * 10

; División
; Dividendo en EDX:EAX, divisor en operando
div ebx               ; EAX = EDX:EAX / EBX, EDX = remainder (unsigned)
idiv ebx              ; EAX = EDX:EAX / EBX, EDX = remainder (signed)

; Preparar para división signed
cdq                   ; EDX:EAX = sign-extend(EAX)
                      ; Necesario antes de IDIV
```

### Lógica y Bits

```asm
; Operaciones lógicas
and eax, ebx          ; EAX = EAX & EBX
or  eax, ebx          ; EAX = EAX | EBX
xor eax, ebx          ; EAX = EAX ^ EBX
not eax               ; EAX = ~EAX

; Shifts (desplazamientos)
shl eax, 4            ; Shift left 4 bits (× 16)
shr eax, 4            ; Shift right 4 bits (÷ 16, unsigned)
sar eax, 4            ; Shift right 4 bits (÷ 16, signed)
rol eax, 4            ; Rotate left
ror eax, 4            ; Rotate right

; Test (AND sin guardar resultado)
test eax, eax         ; Setea flags, común para check zero
test eax, 1           ; Check bit 0

; Bit manipulation
bt  eax, 5            ; CF = bit 5 of EAX
bts eax, 5            ; CF = bit 5, then set bit 5
btr eax, 5            ; CF = bit 5, then reset bit 5
btc eax, 5            ; CF = bit 5, then complement bit 5
bsf eax, ebx          ; Find first set bit (from LSB)
bsr eax, ebx          ; Find first set bit (from MSB)
```

### Comparación y Saltos

```asm
; Comparar (resta sin guardar)
cmp eax, ebx          ; Setea flags basado en EAX - EBX

; Saltos condicionales (signed)
je  label             ; Jump if Equal (ZF=1)
jne label             ; Jump if Not Equal (ZF=0)
jg  label             ; Jump if Greater (ZF=0 AND SF=OF)
jge label             ; Jump if Greater or Equal (SF=OF)
jl  label             ; Jump if Less (SF≠OF)
jle label             ; Jump if Less or Equal (ZF=1 OR SF≠OF)

; Saltos condicionales (unsigned)
ja  label             ; Jump if Above (CF=0 AND ZF=0)
jae label             ; Jump if Above or Equal (CF=0)
jb  label             ; Jump if Below (CF=1)
jbe label             ; Jump if Below or Equal (CF=1 OR ZF=1)

; Saltos por flags
jz  label             ; Jump if Zero (ZF=1) = je
jnz label             ; Jump if Not Zero (ZF=0) = jne
js  label             ; Jump if Sign (SF=1)
jns label             ; Jump if Not Sign (SF=0)
jo  label             ; Jump if Overflow (OF=1)
jno label             ; Jump if Not Overflow (OF=0)
jc  label             ; Jump if Carry (CF=1) = jb
jnc label             ; Jump if Not Carry (CF=0) = jae

; Salto incondicional
jmp label             ; Salto directo
jmp eax               ; Salto indirecto (a dirección en EAX)
jmp [eax]             ; Salto a dirección en memoria
```

### Stack y Llamadas

```asm
; Stack
push eax              ; RSP -= 8; [RSP] = EAX
pop  eax              ; EAX = [RSP]; RSP += 8
pushf                 ; Push flags
popf                  ; Pop flags

; Llamadas
call function         ; Push return addr, jump to function
ret                   ; Pop return addr, jump to it
ret 8                 ; Pop ret addr, add 8 to ESP (cleanup args)

; Enter/Leave (crear/destruir stack frame)
enter 16, 0           ; push ebp; mov ebp,esp; sub esp,16
leave                 ; mov esp,ebp; pop ebp
```

---

## Modos de Direccionamiento x86

### Sintaxis General

```
[base + index*scale + displacement]

base:         registro (EBX, ESI, etc.)
index:        registro (no ESP)
scale:        1, 2, 4, u 8
displacement: constante (8, 16, o 32 bits)
```

### Ejemplos Prácticos

```asm
; Array de bytes
mov al, [array + ecx]           ; array[i]

; Array de ints (4 bytes)
mov eax, [array + ecx*4]        ; array[i]

; Array de structs (12 bytes cada uno)
lea eax, [ecx + ecx*2]          ; EAX = i * 3
mov edx, [array + eax*4]        ; array[i].first_field (offset 0)
mov edx, [array + eax*4 + 4]    ; array[i].second_field (offset 4)

; Acceso a struct via puntero
mov eax, [ebx]                  ; ptr->first (offset 0)
mov eax, [ebx + 4]              ; ptr->second (offset 4)
mov eax, [ebx + 8]              ; ptr->third (offset 8)

; Stack local variables
mov eax, [ebp - 4]              ; primera var local
mov eax, [ebp - 8]              ; segunda var local
mov eax, [ebp + 8]              ; primer parámetro (32-bit)
mov eax, [ebp + 12]             ; segundo parámetro
```

---

## Convención de Llamada x86 (32-bit)

### cdecl (C Declaration)

```asm
; Llamar: int sum(int a, int b)
push 5                ; b = 5
push 3                ; a = 3
call sum              ; llamar
add esp, 8            ; limpiar stack (caller)
; resultado en EAX

; Implementar sum:
sum:
    push ebp          ; guardar frame pointer
    mov ebp, esp      ; nuevo frame

    mov eax, [ebp+8]  ; a
    add eax, [ebp+12] ; + b

    pop ebp           ; restaurar frame
    ret               ; retornar (EAX tiene resultado)
```

### Stack Frame 32-bit

```
┌────────────────┐ ← Direcciones altas
│      b (5)     │  [EBP+12]
├────────────────┤
│      a (3)     │  [EBP+8]
├────────────────┤
│  Return addr   │  [EBP+4]
├────────────────┤
│   Saved EBP    │  [EBP] ← EBP apunta aquí
├────────────────┤
│   Local var 1  │  [EBP-4]
├────────────────┤
│   Local var 2  │  [EBP-8]
├────────────────┤ ← ESP
│    (libre)     │
└────────────────┘ ← Direcciones bajas
```

---

## Convención de Llamada x64 (System V AMD64 ABI)

### Pasaje de Parámetros

| Parámetro | Entero/Puntero | Float/Double |
|-----------|----------------|--------------|
| 1 | RDI | XMM0 |
| 2 | RSI | XMM1 |
| 3 | RDX | XMM2 |
| 4 | RCX | XMM3 |
| 5 | R8 | XMM4 |
| 6 | R9 | XMM5 |
| 7+ | Stack | XMM6-7, stack |

### Registros Preservados

| Caller-saved | Callee-saved |
|--------------|--------------|
| RAX, RCX, RDX | RBX, RBP |
| RSI, RDI | R12, R13, R14, R15 |
| R8, R9, R10, R11 | |

### Ejemplo x64

```asm
; Llamar: long func(long a, long b, long c)
mov rdi, 1            ; a = 1
mov rsi, 2            ; b = 2
mov rdx, 3            ; c = 3
call func             ; resultado en RAX

; Implementar func:
func:
    push rbp
    mov rbp, rsp

    ; RDI = a, RSI = b, RDX = c
    mov rax, rdi
    add rax, rsi
    add rax, rdx

    pop rbp
    ret
```

### Red Zone (x64)

En x64 System V, los 128 bytes debajo de RSP son la "red zone":
- Pueden usarse sin ajustar RSP
- NO se garantizan después de CALL
- Útil para funciones leaf (sin llamadas)

```asm
; Función leaf usando red zone
leaf_func:
    mov [rsp-8], rdi    ; usar red zone
    mov [rsp-16], rsi   ; sin push!
    ; ... trabajo ...
    ret                 ; sin pop!
```

---

## System Calls (Linux x64)

### Mecanismo

```asm
; Syscall: RAX = número, args en RDI, RSI, RDX, R10, R8, R9
; Retorno en RAX

; write(1, msg, len)
mov rax, 1            ; syscall write
mov rdi, 1            ; fd = stdout
lea rsi, [rel msg]    ; buffer
mov rdx, 13           ; length
syscall

; exit(0)
mov rax, 60           ; syscall exit
xor rdi, rdi          ; status = 0
syscall
```

### Syscalls Comunes

| Número | Nombre | Argumentos |
|--------|--------|------------|
| 0 | read | fd, buf, count |
| 1 | write | fd, buf, count |
| 2 | open | filename, flags, mode |
| 3 | close | fd |
| 9 | mmap | addr, len, prot, flags, fd, offset |
| 60 | exit | status |
| 57 | fork | - |
| 59 | execve | filename, argv, envp |

---

## Instrucciones SIMD Básicas

### SSE/AVX Registros

```
XMM0-XMM15:  128 bits (SSE)
YMM0-YMM15:  256 bits (AVX)
ZMM0-ZMM31:  512 bits (AVX-512)
```

### Operaciones Vectoriales

```asm
; Cargar/Almacenar
movaps xmm0, [array]      ; Mover 4 floats alineados
movups xmm0, [array]      ; Mover 4 floats no alineados
movapd xmm0, [array]      ; Mover 2 doubles alineados

; Aritmética paralela
addps xmm0, xmm1          ; 4 sumas float en paralelo
mulps xmm0, xmm1          ; 4 multiplicaciones
addpd xmm0, xmm1          ; 2 sumas double

; Ejemplo: sumar arrays
; float result = a[0]+b[0], a[1]+b[1], a[2]+b[2], a[3]+b[3]
movaps xmm0, [array_a]
addps  xmm0, [array_b]
movaps [result], xmm0
```

---

## Patrones Comunes

### Loop Básico

```asm
; for (int i = 0; i < n; i++)
    xor ecx, ecx          ; i = 0
.loop:
    cmp ecx, [n]          ; i < n?
    jge .done

    ; cuerpo del loop

    inc ecx               ; i++
    jmp .loop
.done:
```

### Loop Optimizado

```asm
; Cuenta hacia atrás (evita comparación)
    mov ecx, [n]          ; i = n
.loop:
    ; cuerpo del loop

    dec ecx               ; i--
    jnz .loop             ; si no zero, continuar
```

### If-Else

```asm
; if (eax > 10) { ... } else { ... }
    cmp eax, 10
    jle .else
.if:
    ; código if
    jmp .endif
.else:
    ; código else
.endif:
```

### Switch Básico

```asm
; switch(eax)
    cmp eax, 0
    je .case0
    cmp eax, 1
    je .case1
    cmp eax, 2
    je .case2
    jmp .default

.case0:
    ; ...
    jmp .endswitch
.case1:
    ; ...
    jmp .endswitch
.case2:
    ; ...
    jmp .endswitch
.default:
    ; ...
.endswitch:
```

### Switch con Jump Table

```asm
; Switch optimizado para casos consecutivos
    cmp eax, 3
    ja .default           ; fuera de rango

    jmp [.table + eax*8]  ; salto indirecto

section .rodata
.table:
    dq .case0
    dq .case1
    dq .case2
    dq .case3
```

---

## Optimización x86

### Reglas Generales

1. **LEA para aritmética**: Más rápido que ADD+MUL
2. **XOR para zero**: `xor eax, eax` mejor que `mov eax, 0`
3. **TEST para comparar con cero**: `test eax, eax` mejor que `cmp eax, 0`
4. **Evitar divisiones**: Usar shifts cuando sea posible
5. **Alinear código**: Loops en límites de 16 bytes

### Ejemplos de Optimización

```asm
; Malo
mov eax, 0            ; 5 bytes
cmp eax, 0            ; 2 bytes

; Bueno
xor eax, eax          ; 2 bytes
test eax, eax         ; 2 bytes

; Multiplicar por constante
; x * 5 = x + x*4
lea eax, [eax + eax*4]

; x * 10 = (x + x*4) * 2
lea eax, [eax + eax*4]
add eax, eax

; División por potencia de 2 (unsigned)
shr eax, 3            ; / 8
```

---

## Debugging x86

### Con GDB

```bash
# Compilar con debug info
nasm -f elf64 -g prog.asm
ld -o prog prog.o

# Debugging
gdb ./prog
(gdb) break _start
(gdb) run
(gdb) info registers
(gdb) x/10i $rip        # ver instrucciones
(gdb) x/4xw $rsp        # ver stack
(gdb) stepi             # una instrucción
(gdb) nexti             # una instrucción (sin entrar en calls)
```

### Layout de GDB

```
(gdb) layout asm        # ver assembly
(gdb) layout regs       # ver registros
(gdb) layout split      # ver ambos
```

---

## Próximos Documentos

- **ASM_02_X86_INSTRUCCIONES.md**: Set completo de instrucciones
- **ASM_02_X86_SIMD.md**: SSE, AVX en detalle
- **ASM_02_X86_SEGURIDAD.md**: Exploits, mitigaciones

---

*"x86 carga 40 años de historia en cada instrucción. Respétalo."*


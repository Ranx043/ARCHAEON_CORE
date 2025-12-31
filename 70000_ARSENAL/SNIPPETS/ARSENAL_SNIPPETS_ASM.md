---
title: "ARCHAEON Arsenal - Assembly Language Snippets"
version: "1.0.0"
category: "ARSENAL/SNIPPETS"
language: "Assembly (x86/x64/ARM)"
purpose: "Practical assembly code patterns for low-level programming"
created: "2025-12-31"
tags: ["assembly", "x86", "x64", "arm", "low-level", "bootloader", "syscalls"]
complexity: "Advanced"
---

# ARCHAEON ARSENAL: Assembly Language Snippets

> **Mission**: Provide battle-tested assembly patterns for system programming,
> embedded development, and low-level optimization.

---

## Table of Contents

1. [x86/x64 Function Patterns](#x86x64-function-patterns)
2. [System Calls](#system-calls)
3. [String Operations](#string-operations)
4. [Memory Operations](#memory-operations)
5. [Bit Manipulation](#bit-manipulation)
6. [ARM Patterns](#arm-patterns)
7. [Bootloader Patterns](#bootloader-patterns)
8. [Interrupt Handling](#interrupt-handling)

---

## x86/x64 Function Patterns

### Standard Function Prologue/Epilogue (32-bit)

```asm
; ==============================================================================
; x86 Standard Function Prologue/Epilogue
; Convention: cdecl (caller cleans stack)
; ==============================================================================

my_function:
    ; === PROLOGUE ===
    push    ebp                 ; Save old base pointer
    mov     ebp, esp            ; Establish new stack frame
    sub     esp, 16             ; Reserve 16 bytes for local variables
    push    ebx                 ; Save callee-saved registers
    push    esi
    push    edi

    ; === FUNCTION BODY ===
    ; Access parameters:
    ;   [ebp+8]  = first parameter
    ;   [ebp+12] = second parameter
    ;   [ebp+16] = third parameter
    ; Local variables:
    ;   [ebp-4]  = first local
    ;   [ebp-8]  = second local

    mov     eax, [ebp+8]        ; Load first parameter
    mov     [ebp-4], eax        ; Store to local variable

    ; === EPILOGUE ===
    pop     edi                 ; Restore callee-saved registers
    pop     esi
    pop     ebx
    mov     esp, ebp            ; Restore stack pointer
    pop     ebp                 ; Restore base pointer
    ret                         ; Return to caller
```

### x64 Function Prologue/Epilogue (Windows ABI)

```asm
; ==============================================================================
; x64 Windows Function Prologue/Epilogue
; Calling convention: Microsoft x64 (RCX, RDX, R8, R9, then stack)
; Shadow space: 32 bytes required
; ==============================================================================

my_function_x64:
    ; === PROLOGUE ===
    push    rbp
    mov     rbp, rsp
    sub     rsp, 64             ; 32 shadow + 32 locals (must be 16-byte aligned)

    ; Save non-volatile registers if used
    mov     [rbp-8], rbx
    mov     [rbp-16], rsi
    mov     [rbp-24], rdi

    ; === FUNCTION BODY ===
    ; Parameters (Windows x64):
    ;   RCX = 1st parameter
    ;   RDX = 2nd parameter
    ;   R8  = 3rd parameter
    ;   R9  = 4th parameter
    ;   [rbp+48] = 5th parameter (after shadow space)

    mov     [rbp+16], rcx       ; Save to shadow space
    mov     [rbp+24], rdx

    ; === EPILOGUE ===
    mov     rbx, [rbp-8]        ; Restore non-volatile registers
    mov     rsi, [rbp-16]
    mov     rdi, [rbp-24]
    add     rsp, 64
    pop     rbp
    ret
```

### x64 Function (System V ABI - Linux/macOS)

```asm
; ==============================================================================
; x64 System V Function Prologue/Epilogue
; Calling convention: RDI, RSI, RDX, RCX, R8, R9, then stack
; ==============================================================================

my_function_sysv:
    ; === PROLOGUE ===
    push    rbp
    mov     rbp, rsp
    sub     rsp, 32             ; Local variables (16-byte aligned)

    ; === FUNCTION BODY ===
    ; Parameters (System V):
    ;   RDI = 1st parameter
    ;   RSI = 2nd parameter
    ;   RDX = 3rd parameter
    ;   RCX = 4th parameter
    ;   R8  = 5th parameter
    ;   R9  = 6th parameter

    mov     [rbp-8], rdi        ; Save first param to local

    ; === EPILOGUE ===
    leave                       ; Equivalent to: mov rsp, rbp; pop rbp
    ret
```

### Leaf Function (No Stack Frame)

```asm
; ==============================================================================
; Leaf Function - Optimized for simple operations
; No calls to other functions, minimal register usage
; ==============================================================================

; Calculate: (a + b) * c
; Parameters: RDI=a, RSI=b, RDX=c
leaf_multiply_add:
    mov     rax, rdi            ; rax = a
    add     rax, rsi            ; rax = a + b
    imul    rax, rdx            ; rax = (a + b) * c
    ret                         ; Return value in RAX
```

---

## System Calls

### Linux x64 System Calls

```asm
; ==============================================================================
; Linux x64 System Call Convention
; syscall number in RAX
; Parameters: RDI, RSI, RDX, R10, R8, R9
; Return value in RAX (-errno on error)
; ==============================================================================

section .data
    msg:        db "Hello, World!", 10
    msg_len:    equ $ - msg
    filename:   db "/tmp/test.txt", 0

section .bss
    buffer:     resb 256
    fd:         resq 1

section .text

; --- Write to stdout ---
sys_write_stdout:
    mov     rax, 1              ; sys_write
    mov     rdi, 1              ; fd = stdout
    lea     rsi, [rel msg]      ; buffer address
    mov     rdx, msg_len        ; count
    syscall
    ret

; --- Read from file ---
sys_read_file:
    ; Open file
    mov     rax, 2              ; sys_open
    lea     rdi, [rel filename] ; pathname
    mov     rsi, 0              ; O_RDONLY
    mov     rdx, 0              ; mode (ignored for O_RDONLY)
    syscall

    test    rax, rax            ; Check for error
    js      .error
    mov     [rel fd], rax       ; Save file descriptor

    ; Read from file
    mov     rdi, rax            ; fd
    mov     rax, 0              ; sys_read
    lea     rsi, [rel buffer]   ; buffer
    mov     rdx, 256            ; count
    syscall

    ; Close file
    push    rax                 ; Save bytes read
    mov     rax, 3              ; sys_close
    mov     rdi, [rel fd]
    syscall
    pop     rax
    ret

.error:
    mov     rax, -1
    ret

; --- Exit program ---
sys_exit:
    mov     rax, 60             ; sys_exit
    xor     rdi, rdi            ; status = 0
    syscall
```

### Linux x86 System Calls (32-bit)

```asm
; ==============================================================================
; Linux x86 System Call Convention
; syscall number in EAX
; Parameters: EBX, ECX, EDX, ESI, EDI, EBP
; Use INT 0x80
; ==============================================================================

section .data
    msg:    db "Hello from 32-bit!", 10
    len:    equ $ - msg

section .text

sys_write_32:
    mov     eax, 4              ; sys_write
    mov     ebx, 1              ; fd = stdout
    mov     ecx, msg            ; buffer
    mov     edx, len            ; count
    int     0x80
    ret

sys_exit_32:
    mov     eax, 1              ; sys_exit
    xor     ebx, ebx            ; status = 0
    int     0x80
```

### Windows x64 System Calls (via ntdll)

```asm
; ==============================================================================
; Windows Native API Calls
; Uses shadow space, specific syscall numbers vary by Windows version
; ==============================================================================

extern NtWriteFile
extern NtClose

section .text

; Write to file handle (using ntdll import)
win_write_file:
    sub     rsp, 88             ; Shadow space + parameters

    mov     rcx, [file_handle]  ; FileHandle
    xor     rdx, rdx            ; Event (NULL)
    xor     r8, r8              ; ApcRoutine (NULL)
    xor     r9, r9              ; ApcContext (NULL)
    lea     rax, [io_status]
    mov     [rsp+32], rax       ; IoStatusBlock
    lea     rax, [buffer]
    mov     [rsp+40], rax       ; Buffer
    mov     qword [rsp+48], 256 ; Length
    mov     qword [rsp+56], 0   ; ByteOffset (NULL)
    mov     qword [rsp+64], 0   ; Key (NULL)

    call    NtWriteFile

    add     rsp, 88
    ret
```

---

## String Operations

### String Length (x64)

```asm
; ==============================================================================
; String Length - Multiple implementations
; Input: RDI = pointer to null-terminated string
; Output: RAX = length (not including null terminator)
; ==============================================================================

; --- Simple byte-by-byte scan ---
strlen_simple:
    xor     rax, rax            ; Counter = 0
.loop:
    cmp     byte [rdi+rax], 0   ; Check for null
    je      .done
    inc     rax
    jmp     .loop
.done:
    ret

; --- Using REPNE SCASB ---
strlen_repne:
    push    rdi
    xor     rcx, rcx
    dec     rcx                 ; RCX = -1 (max count)
    xor     al, al              ; Search for 0
    repne   scasb               ; Scan string
    not     rcx                 ; Invert count
    dec     rcx                 ; Adjust (exclude null)
    mov     rax, rcx
    pop     rdi
    ret

; --- SSE4.2 optimized (16 bytes at a time) ---
strlen_sse42:
    mov         rax, rdi
    pxor        xmm0, xmm0          ; Zero register for comparison
    and         rdi, -16            ; Align to 16 bytes

    pcmpeqb     xmm1, [rdi]         ; Compare 16 bytes
    pmovmskb    ecx, xmm1           ; Get mask of matches

    ; Handle unaligned start
    mov         rdx, rax
    sub         rdx, rdi            ; Offset from aligned address
    shr         ecx, cl             ; Shift out pre-string bytes
    shl         ecx, cl

    test        ecx, ecx
    jnz         .found

.scan_loop:
    add         rdi, 16
    pcmpeqb     xmm1, [rdi]
    pmovmskb    ecx, xmm1
    test        ecx, ecx
    jz          .scan_loop

.found:
    bsf         ecx, ecx            ; Find first set bit
    mov         rax, rdi
    add         rax, rcx
    sub         rax, [rsp-8]        ; Calculate length
    ret
```

### String Copy

```asm
; ==============================================================================
; String Copy
; Input: RDI = destination, RSI = source
; Output: RAX = destination pointer
; ==============================================================================

strcpy_asm:
    push    rdi                 ; Save destination for return
.loop:
    lodsb                       ; Load byte from [RSI] into AL, inc RSI
    stosb                       ; Store AL to [RDI], inc RDI
    test    al, al              ; Check for null terminator
    jnz     .loop
    pop     rax                 ; Return destination
    ret

; --- With length limit (strncpy equivalent) ---
strncpy_asm:
    ; RDI = dest, RSI = src, RDX = max length
    push    rdi
    mov     rcx, rdx            ; Counter
.loop:
    test    rcx, rcx
    jz      .done
    lodsb
    stosb
    dec     rcx
    test    al, al
    jnz     .loop
    ; Pad with zeros if needed
.pad:
    test    rcx, rcx
    jz      .done
    xor     al, al
    stosb
    dec     rcx
    jmp     .pad
.done:
    pop     rax
    ret
```

### String Compare

```asm
; ==============================================================================
; String Compare
; Input: RDI = string1, RSI = string2
; Output: EAX = 0 if equal, <0 if s1<s2, >0 if s1>s2
; ==============================================================================

strcmp_asm:
.loop:
    mov     al, [rdi]
    mov     cl, [rsi]
    cmp     al, cl
    jne     .differ
    test    al, al              ; Check for end of string
    jz      .equal
    inc     rdi
    inc     rsi
    jmp     .loop
.differ:
    movzx   eax, al
    movzx   ecx, cl
    sub     eax, ecx
    ret
.equal:
    xor     eax, eax
    ret

; --- Using REPE CMPSB ---
strcmp_repe:
    push    rdi
    push    rsi

    ; First find length of first string
    mov     rcx, -1
    xor     al, al
    repne   scasb
    not     rcx                 ; Length including null

    ; Compare strings
    pop     rsi
    pop     rdi
    repe    cmpsb

    movzx   eax, byte [rdi-1]
    movzx   ecx, byte [rsi-1]
    sub     eax, ecx
    ret
```

---

## Memory Operations

### Memory Copy (memcpy)

```asm
; ==============================================================================
; Memory Copy - Various implementations
; Input: RDI = dest, RSI = src, RDX = count
; Output: RAX = dest
; ==============================================================================

memcpy_simple:
    push    rdi
    mov     rcx, rdx
    rep     movsb               ; Copy RCX bytes from [RSI] to [RDI]
    pop     rax
    ret

; --- Optimized with QWORD moves ---
memcpy_fast:
    push    rdi
    mov     rcx, rdx
    shr     rcx, 3              ; Divide by 8 for QWORD count
    rep     movsq               ; Copy 8 bytes at a time

    mov     rcx, rdx
    and     rcx, 7              ; Remaining bytes
    rep     movsb

    pop     rax
    ret

; --- Non-temporal (streaming) for large buffers ---
memcpy_nt:
    ; Best for buffers > L3 cache size
    push    rdi

    mov     rcx, rdx
    shr     rcx, 6              ; Divide by 64 (cache line)

.loop:
    movdqa  xmm0, [rsi]
    movdqa  xmm1, [rsi+16]
    movdqa  xmm2, [rsi+32]
    movdqa  xmm3, [rsi+48]

    movntdq [rdi], xmm0         ; Non-temporal store
    movntdq [rdi+16], xmm1
    movntdq [rdi+32], xmm2
    movntdq [rdi+48], xmm3

    add     rsi, 64
    add     rdi, 64
    dec     rcx
    jnz     .loop

    sfence                      ; Ensure stores complete

    ; Handle remainder
    mov     rcx, rdx
    and     rcx, 63
    rep     movsb

    pop     rax
    ret
```

### Memory Set (memset)

```asm
; ==============================================================================
; Memory Set
; Input: RDI = dest, RSI = value (byte), RDX = count
; Output: RAX = dest
; ==============================================================================

memset_asm:
    push    rdi
    mov     al, sil             ; Value to set
    mov     rcx, rdx            ; Count
    rep     stosb
    pop     rax
    ret

; --- Optimized version ---
memset_fast:
    push    rdi

    ; Broadcast byte to all positions in RAX
    movzx   eax, sil            ; Zero-extend byte
    mov     rcx, 0x0101010101010101
    imul    rax, rcx            ; Replicate byte 8 times

    mov     rcx, rdx
    shr     rcx, 3              ; QWORD count
    rep     stosq

    mov     rcx, rdx
    and     rcx, 7              ; Remaining bytes
    rep     stosb

    pop     rax
    ret
```

### Memory Compare

```asm
; ==============================================================================
; Memory Compare
; Input: RDI = ptr1, RSI = ptr2, RDX = count
; Output: EAX = 0 if equal, <0 if m1<m2, >0 if m1>m2
; ==============================================================================

memcmp_asm:
    mov     rcx, rdx
    repe    cmpsb               ; Compare while equal
    je      .equal

    movzx   eax, byte [rdi-1]
    movzx   ecx, byte [rsi-1]
    sub     eax, ecx
    ret

.equal:
    xor     eax, eax
    ret
```

---

## Bit Manipulation

### Common Bit Operations

```asm
; ==============================================================================
; Bit Manipulation Patterns
; ==============================================================================

section .text

; --- Count set bits (population count) ---
; Input: RDI = value
; Output: EAX = number of 1 bits
popcnt_asm:
    xor     eax, eax
.loop:
    test    rdi, rdi
    jz      .done
    mov     rsi, rdi
    dec     rsi                 ; x - 1
    and     rdi, rsi            ; x & (x-1) clears lowest set bit
    inc     eax
    jmp     .loop
.done:
    ret

; --- Using POPCNT instruction (SSE4.2+) ---
popcnt_hw:
    popcnt  rax, rdi
    ret

; --- Find lowest set bit position ---
; Input: RDI = value
; Output: EAX = bit position (0-63), or -1 if zero
bsf_asm:
    bsf     rax, rdi            ; Bit scan forward
    jnz     .found
    mov     eax, -1             ; Return -1 if no bits set
    ret
.found:
    ret

; --- Find highest set bit position ---
bsr_asm:
    bsr     rax, rdi            ; Bit scan reverse
    jnz     .found
    mov     eax, -1
    ret
.found:
    ret

; --- Isolate lowest set bit ---
; x & (-x) isolates the lowest set bit
isolate_lowest:
    mov     rax, rdi
    neg     rax
    and     rax, rdi
    ret

; --- Clear lowest set bit ---
; x & (x-1) clears the lowest set bit
clear_lowest:
    mov     rax, rdi
    dec     rax
    and     rax, rdi
    ret

; --- Set bit at position ---
; RDI = value, RSI = bit position
set_bit:
    mov     rax, 1
    mov     rcx, rsi
    shl     rax, cl
    or      rax, rdi
    ret

; --- Clear bit at position ---
clear_bit:
    mov     rax, 1
    mov     rcx, rsi
    shl     rax, cl
    not     rax
    and     rax, rdi
    ret

; --- Toggle bit at position ---
toggle_bit:
    mov     rax, 1
    mov     rcx, rsi
    shl     rax, cl
    xor     rax, rdi
    ret

; --- Test bit at position ---
; Returns non-zero if bit is set
test_bit:
    mov     rax, 1
    mov     rcx, rsi
    shl     rax, cl
    and     rax, rdi
    ret

; --- Rotate left ---
; RDI = value, RSI = count
rol_asm:
    mov     rax, rdi
    mov     rcx, rsi
    rol     rax, cl
    ret

; --- Rotate right ---
ror_asm:
    mov     rax, rdi
    mov     rcx, rsi
    ror     rax, cl
    ret

; --- Byte swap (endianness conversion) ---
bswap_32:
    mov     eax, edi
    bswap   eax
    ret

bswap_64:
    mov     rax, rdi
    bswap   rax
    ret

; --- Extract bit field ---
; RDI = value, RSI = start bit, RDX = length
extract_bits:
    mov     rax, rdi
    mov     rcx, rsi
    shr     rax, cl             ; Shift field to position 0

    mov     rcx, rdx
    mov     rdi, 1
    shl     rdi, cl
    dec     rdi                 ; Create mask of 'length' bits

    and     rax, rdi
    ret
```

---

## ARM Patterns

### ARM Cortex-M GPIO Control

```asm
; ==============================================================================
; ARM Cortex-M GPIO Patterns (Thumb-2)
; Target: STM32F4xx series
; ==============================================================================

.syntax unified
.cpu cortex-m4
.thumb

; GPIO Base addresses for STM32F4
.equ GPIOA_BASE,    0x40020000
.equ GPIOB_BASE,    0x40020400
.equ GPIOC_BASE,    0x40020800

; GPIO Register offsets
.equ GPIO_MODER,    0x00    @ Mode register
.equ GPIO_OTYPER,   0x04    @ Output type register
.equ GPIO_OSPEEDR,  0x08    @ Output speed register
.equ GPIO_PUPDR,    0x0C    @ Pull-up/pull-down register
.equ GPIO_IDR,      0x10    @ Input data register
.equ GPIO_ODR,      0x14    @ Output data register
.equ GPIO_BSRR,     0x18    @ Bit set/reset register

.section .text

; --- Set GPIO pin as output ---
; R0 = GPIO base address
; R1 = pin number (0-15)
gpio_set_output:
    push    {r4, lr}

    ldr     r2, [r0, #GPIO_MODER]
    mov     r3, #3
    lsl     r4, r1, #1          @ Pin * 2 for 2-bit field
    lsl     r3, r3, r4          @ Create mask
    bic     r2, r2, r3          @ Clear field

    mov     r3, #1              @ Output mode = 01
    lsl     r3, r3, r4
    orr     r2, r2, r3          @ Set output mode

    str     r2, [r0, #GPIO_MODER]

    pop     {r4, pc}

; --- Set GPIO pin high ---
; R0 = GPIO base address
; R1 = pin number
gpio_set_high:
    mov     r2, #1
    lsl     r2, r2, r1          @ Create bit mask
    str     r2, [r0, #GPIO_BSRR]  @ Write to BSRR sets pin
    bx      lr

; --- Set GPIO pin low ---
; R0 = GPIO base address
; R1 = pin number
gpio_set_low:
    mov     r2, #1
    lsl     r2, r2, r1
    lsl     r2, r2, #16         @ Upper 16 bits of BSRR reset pins
    str     r2, [r0, #GPIO_BSRR]
    bx      lr

; --- Toggle GPIO pin ---
; R0 = GPIO base address
; R1 = pin number
gpio_toggle:
    ldr     r2, [r0, #GPIO_ODR]
    mov     r3, #1
    lsl     r3, r3, r1
    eor     r2, r2, r3
    str     r2, [r0, #GPIO_ODR]
    bx      lr

; --- Read GPIO pin ---
; R0 = GPIO base address
; R1 = pin number
; Returns: R0 = 0 or 1
gpio_read:
    ldr     r2, [r0, #GPIO_IDR]
    mov     r0, #1
    lsl     r0, r0, r1
    and     r0, r0, r2
    lsr     r0, r0, r1
    bx      lr
```

### ARM Interrupt Handler

```asm
; ==============================================================================
; ARM Cortex-M Interrupt Handling
; ==============================================================================

.syntax unified
.cpu cortex-m4
.thumb

; NVIC registers
.equ NVIC_BASE,     0xE000E100
.equ NVIC_ISER0,    0x000       @ Interrupt Set Enable
.equ NVIC_ICER0,    0x080       @ Interrupt Clear Enable
.equ NVIC_ISPR0,    0x100       @ Interrupt Set Pending
.equ NVIC_ICPR0,    0x180       @ Interrupt Clear Pending
.equ NVIC_IPR0,     0x300       @ Interrupt Priority

.section .text

; --- Enable interrupt ---
; R0 = IRQ number (0-239)
nvic_enable_irq:
    ldr     r1, =NVIC_BASE
    mov     r2, r0
    lsr     r2, r2, #5          @ Divide by 32 for register index
    lsl     r2, r2, #2          @ Multiply by 4 for byte offset
    add     r1, r1, r2
    add     r1, r1, #NVIC_ISER0

    and     r0, r0, #31         @ Bit position within register
    mov     r2, #1
    lsl     r2, r2, r0
    str     r2, [r1]
    bx      lr

; --- Disable interrupt ---
nvic_disable_irq:
    ldr     r1, =NVIC_BASE
    mov     r2, r0
    lsr     r2, r2, #5
    lsl     r2, r2, #2
    add     r1, r1, r2
    add     r1, r1, #NVIC_ICER0

    and     r0, r0, #31
    mov     r2, #1
    lsl     r2, r2, r0
    str     r2, [r1]
    bx      lr

; --- Set interrupt priority ---
; R0 = IRQ number
; R1 = priority (0-255, lower = higher priority)
nvic_set_priority:
    ldr     r2, =NVIC_BASE
    add     r2, r2, #NVIC_IPR0
    strb    r1, [r2, r0]
    bx      lr

; --- Example interrupt handler ---
.global TIM2_IRQHandler
.type TIM2_IRQHandler, %function
TIM2_IRQHandler:
    push    {r4-r7, lr}         @ Save registers

    ; Clear interrupt flag
    ldr     r0, =TIM2_BASE
    ldr     r1, [r0, #TIM_SR]
    bic     r1, r1, #1          @ Clear UIF flag
    str     r1, [r0, #TIM_SR]

    ; Your interrupt handling code here

    pop     {r4-r7, pc}         @ Restore and return
```

---

## Bootloader Patterns

### x86 Real Mode Bootloader

```asm
; ==============================================================================
; x86 Real Mode Bootloader (MBR)
; Loaded at 0x7C00, 512 bytes max
; ==============================================================================

[BITS 16]
[ORG 0x7C00]

boot_start:
    ; Setup segments
    cli                         ; Disable interrupts
    xor     ax, ax
    mov     ds, ax
    mov     es, ax
    mov     ss, ax
    mov     sp, 0x7C00          ; Stack below bootloader
    sti                         ; Re-enable interrupts

    ; Save boot drive
    mov     [boot_drive], dl

    ; Print loading message
    mov     si, msg_loading
    call    print_string

    ; Load kernel from disk
    call    load_kernel

    ; Switch to protected mode
    call    enable_a20

    lgdt    [gdt_descriptor]    ; Load GDT

    mov     eax, cr0
    or      eax, 1              ; Set PE bit
    mov     cr0, eax

    jmp     CODE_SEG:protected_mode

; --- Print null-terminated string ---
; SI = string pointer
print_string:
    pusha
.loop:
    lodsb
    test    al, al
    jz      .done
    mov     ah, 0x0E            ; BIOS teletype
    mov     bh, 0
    int     0x10
    jmp     .loop
.done:
    popa
    ret

; --- Load kernel using BIOS INT 13h ---
load_kernel:
    pusha

    mov     ah, 0x02            ; BIOS read sectors
    mov     al, 15              ; Number of sectors
    mov     ch, 0               ; Cylinder
    mov     cl, 2               ; Start sector (1-indexed)
    mov     dh, 0               ; Head
    mov     dl, [boot_drive]    ; Drive
    mov     bx, 0x1000          ; Load address (ES:BX)
    mov     es, bx
    xor     bx, bx

    int     0x13
    jc      disk_error

    popa
    ret

disk_error:
    mov     si, msg_error
    call    print_string
    jmp     $

; --- Enable A20 line ---
enable_a20:
    ; Fast A20 via port 0x92
    in      al, 0x92
    test    al, 2
    jnz     .done
    or      al, 2
    and     al, 0xFE
    out     0x92, al
.done:
    ret

; --- GDT ---
gdt_start:
    dq      0                   ; Null descriptor

gdt_code:
    dw      0xFFFF              ; Limit (low)
    dw      0                   ; Base (low)
    db      0                   ; Base (middle)
    db      10011010b           ; Access: present, ring 0, code, readable
    db      11001111b           ; Flags + limit (high)
    db      0                   ; Base (high)

gdt_data:
    dw      0xFFFF
    dw      0
    db      0
    db      10010010b           ; Access: present, ring 0, data, writable
    db      11001111b
    db      0

gdt_end:

gdt_descriptor:
    dw      gdt_end - gdt_start - 1
    dd      gdt_start

CODE_SEG    equ     gdt_code - gdt_start
DATA_SEG    equ     gdt_data - gdt_start

; --- Data ---
boot_drive:     db 0
msg_loading:    db "Loading...", 13, 10, 0
msg_error:      db "Disk error!", 0

; --- Protected mode entry ---
[BITS 32]
protected_mode:
    mov     ax, DATA_SEG
    mov     ds, ax
    mov     es, ax
    mov     fs, ax
    mov     gs, ax
    mov     ss, ax
    mov     esp, 0x90000

    ; Jump to loaded kernel
    jmp     0x10000

; --- Boot signature ---
times 510 - ($ - $$) db 0
dw      0xAA55
```

### UEFI Application Entry

```asm
; ==============================================================================
; UEFI Application Entry Point (x64)
; ==============================================================================

[BITS 64]

section .text

global _start
extern EfiMain

; UEFI entry point - receives ImageHandle and SystemTable
_start:
    ; Save parameters (Microsoft x64 ABI)
    sub     rsp, 40             ; Shadow space + alignment

    ; RCX = ImageHandle
    ; RDX = SystemTable

    ; Call EfiMain(ImageHandle, SystemTable)
    call    EfiMain

    add     rsp, 40
    ret
```

---

## Interrupt Handling

### x86 IDT Setup

```asm
; ==============================================================================
; x86 Protected Mode IDT Setup
; ==============================================================================

[BITS 32]

section .data

; IDT entries (256 * 8 bytes = 2048 bytes)
align 8
idt_start:
    times 256 dq 0
idt_end:

idt_descriptor:
    dw      idt_end - idt_start - 1
    dd      idt_start

section .text

; --- Setup IDT entry ---
; EAX = interrupt number
; EBX = handler address
; ECX = selector
; EDX = type/attributes
setup_idt_entry:
    push    edi

    lea     edi, [idt_start]
    shl     eax, 3              ; Multiply by 8 (entry size)
    add     edi, eax

    mov     [edi], bx           ; Offset low
    mov     [edi+2], cx         ; Selector
    mov     byte [edi+4], 0     ; Reserved
    mov     [edi+5], dl         ; Type/attributes
    shr     ebx, 16
    mov     [edi+6], bx         ; Offset high

    pop     edi
    ret

; --- Load IDT ---
load_idt:
    lidt    [idt_descriptor]
    ret

; --- Common interrupt handler wrapper ---
%macro ISR_NOERRCODE 1
global isr%1
isr%1:
    push    dword 0             ; Dummy error code
    push    dword %1            ; Interrupt number
    jmp     isr_common
%endmacro

%macro ISR_ERRCODE 1
global isr%1
isr%1:
    push    dword %1            ; Interrupt number (error code already pushed)
    jmp     isr_common
%endmacro

; Define ISRs
ISR_NOERRCODE 0                 ; Division by zero
ISR_NOERRCODE 1                 ; Debug
ISR_NOERRCODE 2                 ; NMI
ISR_NOERRCODE 3                 ; Breakpoint
ISR_NOERRCODE 4                 ; Overflow
ISR_NOERRCODE 5                 ; Bound range exceeded
ISR_NOERRCODE 6                 ; Invalid opcode
ISR_NOERRCODE 7                 ; Device not available
ISR_ERRCODE   8                 ; Double fault
ISR_NOERRCODE 9                 ; Coprocessor segment overrun
ISR_ERRCODE   10                ; Invalid TSS
ISR_ERRCODE   11                ; Segment not present
ISR_ERRCODE   12                ; Stack fault
ISR_ERRCODE   13                ; General protection fault
ISR_ERRCODE   14                ; Page fault
ISR_NOERRCODE 15                ; Reserved

extern interrupt_handler

isr_common:
    ; Save all registers
    pushad

    ; Save segment registers
    push    ds
    push    es
    push    fs
    push    gs

    ; Load kernel data segment
    mov     ax, 0x10
    mov     ds, ax
    mov     es, ax
    mov     fs, ax
    mov     gs, ax

    ; Call C handler
    push    esp                 ; Pass pointer to registers
    call    interrupt_handler
    add     esp, 4

    ; Restore segment registers
    pop     gs
    pop     fs
    pop     es
    pop     ds

    ; Restore general registers
    popad

    ; Remove error code and interrupt number
    add     esp, 8

    iret
```

---

## Usage Notes

### Assembler Syntax Differences

| Feature | NASM | GAS (AT&T) |
|---------|------|------------|
| Operand order | dest, src | src, dest |
| Register prefix | none | % |
| Immediate prefix | none | $ |
| Memory reference | [eax] | (%eax) |
| Size suffix | none | b/w/l/q |

### Register Conventions Summary

| ABI | Args | Return | Callee-saved | Caller-saved |
|-----|------|--------|--------------|--------------|
| x86 cdecl | stack | EAX/EDX | EBX,ESI,EDI,EBP | EAX,ECX,EDX |
| x64 Windows | RCX,RDX,R8,R9 | RAX | RBX,RBP,RDI,RSI,R12-R15 | RAX,RCX,RDX,R8-R11 |
| x64 System V | RDI,RSI,RDX,RCX,R8,R9 | RAX | RBX,RBP,R12-R15 | RAX,RCX,RDX,RSI,RDI,R8-R11 |
| ARM | R0-R3 | R0 | R4-R11,LR | R0-R3,R12 |

---

*ARCHAEON Arsenal - Assembly Division*
*"In the beginning was the Word, and the Word was machine code"*

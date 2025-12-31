---
title: "ARSENAL Reference: Assembly Instructions"
category: reference
domain: low-level
architectures: [x86, x64, ARM, 6502, Z80, 68000]
version: 1.0.0
last_updated: 2025-12-31
tags: [assembly, instructions, syscalls, encoding]
---

# ARSENAL_REF_INSTRUCCIONES
## Complete Assembly Instruction Reference

---

# TABLE OF CONTENTS

1. [x86/x64 Instructions](#x86x64-instructions)
2. [ARM Instructions](#arm-instructions)
3. [6502 Instructions](#6502-instructions)
4. [Z80 Instructions](#z80-instructions)
5. [68000 Instructions](#68000-instructions)
6. [System Call Tables](#system-call-tables)

---

# x86/x64 INSTRUCTIONS

## Data Movement Instructions

| Instruction | Syntax | Description | Flags | Encoding |
|-------------|--------|-------------|-------|----------|
| `MOV` | `MOV dest, src` | Copy data | None | 88-8B, B0-BF, C6-C7 |
| `MOVZX` | `MOVZX dest, src` | Move with zero-extend | None | 0F B6/B7 |
| `MOVSX` | `MOVSX dest, src` | Move with sign-extend | None | 0F BE/BF |
| `MOVSXD` | `MOVSXD r64, r/m32` | Sign-extend dword to qword | None | 63 (x64) |
| `LEA` | `LEA dest, [addr]` | Load effective address | None | 8D |
| `XCHG` | `XCHG op1, op2` | Exchange operands | None | 86-87, 90-97 |
| `PUSH` | `PUSH src` | Push onto stack | None | 50-57, 68, 6A, FF/6 |
| `POP` | `POP dest` | Pop from stack | None | 58-5F, 8F/0 |
| `PUSHF` | `PUSHF` | Push flags | None | 9C |
| `POPF` | `POPF` | Pop flags | All | 9D |
| `CBW` | `CBW` | AL -> AX (sign-extend) | None | 98 |
| `CWD` | `CWD` | AX -> DX:AX (sign-extend) | None | 99 |
| `CDQ` | `CDQ` | EAX -> EDX:EAX | None | 99 |
| `CQO` | `CQO` | RAX -> RDX:RAX (x64) | None | 48 99 |
| `BSWAP` | `BSWAP reg` | Byte swap | None | 0F C8+r |

### MOV Examples
```asm
mov eax, 42           ; Immediate to register
mov [ebx], eax        ; Register to memory
mov ecx, [edx+4]      ; Memory to register
mov byte ptr [eax], 0 ; Immediate to memory
mov rax, qword ptr [rsp+8] ; x64 memory access
```

## Arithmetic Instructions

| Instruction | Syntax | Description | Flags Affected | Encoding |
|-------------|--------|-------------|----------------|----------|
| `ADD` | `ADD dest, src` | Addition | OF, SF, ZF, AF, CF, PF | 00-05, 80-83/0 |
| `ADC` | `ADC dest, src` | Add with carry | OF, SF, ZF, AF, CF, PF | 10-15, 80-83/2 |
| `SUB` | `SUB dest, src` | Subtraction | OF, SF, ZF, AF, CF, PF | 28-2D, 80-83/5 |
| `SBB` | `SBB dest, src` | Subtract with borrow | OF, SF, ZF, AF, CF, PF | 18-1D, 80-83/3 |
| `INC` | `INC dest` | Increment | OF, SF, ZF, AF, PF | 40-47, FE-FF/0 |
| `DEC` | `DEC dest` | Decrement | OF, SF, ZF, AF, PF | 48-4F, FE-FF/1 |
| `NEG` | `NEG dest` | Two's complement | OF, SF, ZF, AF, CF, PF | F6-F7/3 |
| `MUL` | `MUL src` | Unsigned multiply | OF, CF | F6-F7/4 |
| `IMUL` | `IMUL src` | Signed multiply | OF, CF, SF, ZF, AF, PF | F6-F7/5, 0F AF |
| `DIV` | `DIV src` | Unsigned divide | Undefined | F6-F7/6 |
| `IDIV` | `IDIV src` | Signed divide | Undefined | F6-F7/7 |

### Multiplication Details
```asm
; MUL - unsigned, result in AX/DX:AX/EDX:EAX/RDX:RAX
mul bl      ; AX = AL * BL
mul bx      ; DX:AX = AX * BX
mul ebx     ; EDX:EAX = EAX * EBX
mul rbx     ; RDX:RAX = RAX * RBX (x64)

; IMUL - signed, multiple forms
imul eax, ebx        ; EAX = EAX * EBX
imul eax, ebx, 10    ; EAX = EBX * 10
imul eax, [mem], 5   ; EAX = [mem] * 5
```

## Logical Instructions

| Instruction | Syntax | Description | Flags Affected | Encoding |
|-------------|--------|-------------|----------------|----------|
| `AND` | `AND dest, src` | Bitwise AND | OF=0, CF=0, SF, ZF, PF | 20-25, 80-83/4 |
| `OR` | `OR dest, src` | Bitwise OR | OF=0, CF=0, SF, ZF, PF | 08-0D, 80-83/1 |
| `XOR` | `XOR dest, src` | Bitwise XOR | OF=0, CF=0, SF, ZF, PF | 30-35, 80-83/6 |
| `NOT` | `NOT dest` | Bitwise NOT | None | F6-F7/2 |
| `TEST` | `TEST op1, op2` | Bitwise AND (no store) | OF=0, CF=0, SF, ZF, PF | 84-85, A8-A9, F6-F7/0 |

### Logical Examples
```asm
and eax, 0xFF        ; Mask lower byte
or  eax, 0x80000000  ; Set high bit
xor eax, eax         ; Fast zero (preferred)
test eax, eax        ; Check if zero
not eax              ; Invert all bits
```

## Shift and Rotate Instructions

| Instruction | Syntax | Description | Flags | Encoding |
|-------------|--------|-------------|-------|----------|
| `SHL/SAL` | `SHL dest, count` | Shift left | CF, OF, SF, ZF, PF | C0-C1/4, D0-D3/4 |
| `SHR` | `SHR dest, count` | Logical shift right | CF, OF, SF, ZF, PF | C0-C1/5, D0-D3/5 |
| `SAR` | `SAR dest, count` | Arithmetic shift right | CF, OF, SF, ZF, PF | C0-C1/7, D0-D3/7 |
| `ROL` | `ROL dest, count` | Rotate left | CF, OF | C0-C1/0, D0-D3/0 |
| `ROR` | `ROR dest, count` | Rotate right | CF, OF | C0-C1/1, D0-D3/1 |
| `RCL` | `RCL dest, count` | Rotate left through carry | CF, OF | C0-C1/2, D0-D3/2 |
| `RCR` | `RCR dest, count` | Rotate right through carry | CF, OF | C0-C1/3, D0-D3/3 |
| `SHLD` | `SHLD dest, src, cnt` | Double shift left | CF, OF, SF, ZF, PF | 0F A4-A5 |
| `SHRD` | `SHRD dest, src, cnt` | Double shift right | CF, OF, SF, ZF, PF | 0F AC-AD |

### Shift Examples
```asm
shl eax, 1      ; Multiply by 2
shl eax, 4      ; Multiply by 16
shr eax, 1      ; Unsigned divide by 2
sar eax, 1      ; Signed divide by 2
rol eax, 8      ; Rotate byte positions
```

## Comparison and Jump Instructions

### Comparison
| Instruction | Syntax | Description | Flags | Encoding |
|-------------|--------|-------------|-------|----------|
| `CMP` | `CMP op1, op2` | Compare (SUB without store) | OF, SF, ZF, AF, CF, PF | 38-3D, 80-83/7 |
| `TEST` | `TEST op1, op2` | Test bits (AND without store) | OF=0, CF=0, SF, ZF, PF | 84-85, A8-A9, F6-F7/0 |

### Conditional Jumps
| Instruction | Condition | Description | Flags Tested |
|-------------|-----------|-------------|--------------|
| `JE/JZ` | Equal/Zero | Jump if ZF=1 | ZF |
| `JNE/JNZ` | Not equal/Not zero | Jump if ZF=0 | ZF |
| `JA/JNBE` | Above (unsigned) | Jump if CF=0 and ZF=0 | CF, ZF |
| `JAE/JNB/JNC` | Above or equal | Jump if CF=0 | CF |
| `JB/JNAE/JC` | Below (unsigned) | Jump if CF=1 | CF |
| `JBE/JNA` | Below or equal | Jump if CF=1 or ZF=1 | CF, ZF |
| `JG/JNLE` | Greater (signed) | Jump if ZF=0 and SF=OF | ZF, SF, OF |
| `JGE/JNL` | Greater or equal | Jump if SF=OF | SF, OF |
| `JL/JNGE` | Less (signed) | Jump if SF!=OF | SF, OF |
| `JLE/JNG` | Less or equal | Jump if ZF=1 or SF!=OF | ZF, SF, OF |
| `JS` | Sign | Jump if SF=1 | SF |
| `JNS` | Not sign | Jump if SF=0 | SF |
| `JO` | Overflow | Jump if OF=1 | OF |
| `JNO` | Not overflow | Jump if OF=0 | OF |
| `JP/JPE` | Parity even | Jump if PF=1 | PF |
| `JNP/JPO` | Parity odd | Jump if PF=0 | PF |
| `JCXZ/JECXZ/JRCXZ` | CX/ECX/RCX = 0 | Jump if counter zero | None |

### Unconditional Jumps
| Instruction | Syntax | Description | Encoding |
|-------------|--------|-------------|----------|
| `JMP` | `JMP label` | Unconditional jump | E9, EB, FF/4 |
| `CALL` | `CALL label` | Call procedure | E8, FF/2 |
| `RET` | `RET [imm16]` | Return from procedure | C2, C3 |
| `LOOP` | `LOOP label` | Dec counter, jump if != 0 | E2 |
| `LOOPE/LOOPZ` | `LOOPE label` | Loop while equal/zero | E1 |
| `LOOPNE/LOOPNZ` | `LOOPNE label` | Loop while not equal | E0 |

## String Instructions

| Instruction | Syntax | Description | Direction |
|-------------|--------|-------------|-----------|
| `MOVSB/W/D/Q` | `MOVSB` | Move string | SI->DI |
| `CMPSB/W/D/Q` | `CMPSB` | Compare strings | SI vs DI |
| `SCASB/W/D/Q` | `SCASB` | Scan string | AL/AX/EAX vs DI |
| `LODSB/W/D/Q` | `LODSB` | Load string | SI -> AL/AX/EAX |
| `STOSB/W/D/Q` | `STOSB` | Store string | AL/AX/EAX -> DI |
| `REP` | `REP inst` | Repeat CX times | - |
| `REPE/REPZ` | `REPE inst` | Repeat while equal | - |
| `REPNE/REPNZ` | `REPNE inst` | Repeat while not equal | - |

### String Examples
```asm
; Copy string (DF=0 for forward)
cld
mov esi, source
mov edi, dest
mov ecx, length
rep movsb

; Find byte in string
mov edi, buffer
mov ecx, length
mov al, target
repne scasb
; EDI points to byte after match, ECX has remaining count
```

## SIMD Instructions (SSE/AVX)

### Data Movement (SSE)
| Instruction | Syntax | Description |
|-------------|--------|-------------|
| `MOVAPS` | `MOVAPS xmm, xmm/m128` | Move aligned packed single |
| `MOVUPS` | `MOVUPS xmm, xmm/m128` | Move unaligned packed single |
| `MOVAPD` | `MOVAPD xmm, xmm/m128` | Move aligned packed double |
| `MOVUPD` | `MOVUPD xmm, xmm/m128` | Move unaligned packed double |
| `MOVDQA` | `MOVDQA xmm, xmm/m128` | Move aligned packed integer |
| `MOVDQU` | `MOVDQU xmm, xmm/m128` | Move unaligned packed integer |
| `MOVSS` | `MOVSS xmm, xmm/m32` | Move scalar single |
| `MOVSD` | `MOVSD xmm, xmm/m64` | Move scalar double |

### Arithmetic (SSE/SSE2)
| Instruction | Syntax | Description |
|-------------|--------|-------------|
| `ADDPS` | `ADDPS xmm, xmm/m128` | Add packed single |
| `ADDPD` | `ADDPD xmm, xmm/m128` | Add packed double |
| `ADDSS` | `ADDSS xmm, xmm/m32` | Add scalar single |
| `ADDSD` | `ADDSD xmm, xmm/m64` | Add scalar double |
| `SUBPS` | `SUBPS xmm, xmm/m128` | Subtract packed single |
| `MULPS` | `MULPS xmm, xmm/m128` | Multiply packed single |
| `DIVPS` | `DIVPS xmm, xmm/m128` | Divide packed single |
| `SQRTPS` | `SQRTPS xmm, xmm/m128` | Square root packed single |
| `MINPS` | `MINPS xmm, xmm/m128` | Minimum packed single |
| `MAXPS` | `MAXPS xmm, xmm/m128` | Maximum packed single |

### Comparison (SSE)
| Instruction | Syntax | Description |
|-------------|--------|-------------|
| `CMPPS` | `CMPPS xmm, xmm/m128, imm8` | Compare packed single |
| `CMPEQPS` | `CMPEQPS xmm, xmm/m128` | Compare equal |
| `CMPLTPS` | `CMPLTPS xmm, xmm/m128` | Compare less than |
| `CMPLEPS` | `CMPLEPS xmm, xmm/m128` | Compare less or equal |
| `CMPUNORDPS` | `CMPUNORDPS xmm, xmm/m128` | Compare unordered (NaN) |

### AVX Extensions
| Instruction | Syntax | Description |
|-------------|--------|-------------|
| `VADDPS` | `VADDPS ymm, ymm, ymm/m256` | 256-bit add packed single |
| `VFMADD132PS` | `VFMADD132PS xmm, xmm, xmm/m128` | Fused multiply-add |
| `VBROADCASTSS` | `VBROADCASTSS ymm, m32` | Broadcast single |
| `VPERM2F128` | `VPERM2F128 ymm, ymm, ymm/m256, imm8` | Permute 128-bit lanes |

## Bit Manipulation Instructions

| Instruction | Syntax | Description | Flags |
|-------------|--------|-------------|-------|
| `BT` | `BT dest, bit` | Bit test | CF = bit |
| `BTS` | `BTS dest, bit` | Bit test and set | CF = bit |
| `BTR` | `BTR dest, bit` | Bit test and reset | CF = bit |
| `BTC` | `BTC dest, bit` | Bit test and complement | CF = bit |
| `BSF` | `BSF dest, src` | Bit scan forward | ZF |
| `BSR` | `BSR dest, src` | Bit scan reverse | ZF |
| `POPCNT` | `POPCNT dest, src` | Population count | ZF |
| `LZCNT` | `LZCNT dest, src` | Leading zero count | CF, ZF |
| `TZCNT` | `TZCNT dest, src` | Trailing zero count | CF, ZF |

---

# ARM INSTRUCTIONS

## Data Processing Instructions (ARM32/AArch64)

| Instruction | Syntax | Description | Flags |
|-------------|--------|-------------|-------|
| `MOV` | `MOV Rd, Op2` | Move | NZCV (if S) |
| `MVN` | `MVN Rd, Op2` | Move NOT | NZCV (if S) |
| `ADD` | `ADD Rd, Rn, Op2` | Add | NZCV (if S) |
| `ADC` | `ADC Rd, Rn, Op2` | Add with carry | NZCV (if S) |
| `SUB` | `SUB Rd, Rn, Op2` | Subtract | NZCV (if S) |
| `SBC` | `SBC Rd, Rn, Op2` | Subtract with carry | NZCV (if S) |
| `RSB` | `RSB Rd, Rn, Op2` | Reverse subtract | NZCV (if S) |
| `MUL` | `MUL Rd, Rm, Rs` | Multiply | NZ (if S) |
| `MLA` | `MLA Rd, Rm, Rs, Rn` | Multiply-accumulate | NZ (if S) |
| `AND` | `AND Rd, Rn, Op2` | Bitwise AND | NZCV (if S) |
| `ORR` | `ORR Rd, Rn, Op2` | Bitwise OR | NZCV (if S) |
| `EOR` | `EOR Rd, Rn, Op2` | Bitwise XOR | NZCV (if S) |
| `BIC` | `BIC Rd, Rn, Op2` | Bit clear | NZCV (if S) |
| `CMP` | `CMP Rn, Op2` | Compare | NZCV |
| `CMN` | `CMN Rn, Op2` | Compare negative | NZCV |
| `TST` | `TST Rn, Op2` | Test bits | NZC |
| `TEQ` | `TEQ Rn, Op2` | Test equivalence | NZC |

### ARM Condition Codes
| Code | Suffix | Condition | Flags |
|------|--------|-----------|-------|
| 0000 | EQ | Equal | Z=1 |
| 0001 | NE | Not equal | Z=0 |
| 0010 | CS/HS | Carry set / unsigned higher or same | C=1 |
| 0011 | CC/LO | Carry clear / unsigned lower | C=0 |
| 0100 | MI | Minus / negative | N=1 |
| 0101 | PL | Plus / positive or zero | N=0 |
| 0110 | VS | Overflow | V=1 |
| 0111 | VC | No overflow | V=0 |
| 1000 | HI | Unsigned higher | C=1 and Z=0 |
| 1001 | LS | Unsigned lower or same | C=0 or Z=1 |
| 1010 | GE | Signed greater or equal | N=V |
| 1011 | LT | Signed less than | N!=V |
| 1100 | GT | Signed greater than | Z=0 and N=V |
| 1101 | LE | Signed less than or equal | Z=1 or N!=V |
| 1110 | AL | Always | - |

## Load/Store Instructions (ARM)

| Instruction | Syntax | Description |
|-------------|--------|-------------|
| `LDR` | `LDR Rd, [Rn, #offset]` | Load register |
| `LDRB` | `LDRB Rd, [Rn]` | Load byte |
| `LDRH` | `LDRH Rd, [Rn]` | Load halfword |
| `LDRSB` | `LDRSB Rd, [Rn]` | Load signed byte |
| `LDRSH` | `LDRSH Rd, [Rn]` | Load signed halfword |
| `STR` | `STR Rd, [Rn, #offset]` | Store register |
| `STRB` | `STRB Rd, [Rn]` | Store byte |
| `STRH` | `STRH Rd, [Rn]` | Store halfword |
| `LDM` | `LDM Rn{!}, {reglist}` | Load multiple |
| `STM` | `STM Rn{!}, {reglist}` | Store multiple |
| `PUSH` | `PUSH {reglist}` | Push registers |
| `POP` | `POP {reglist}` | Pop registers |

### Addressing Modes
```asm
LDR R0, [R1]           ; [R1]
LDR R0, [R1, #4]       ; [R1 + 4]
LDR R0, [R1, #4]!      ; R1 = R1 + 4, then [R1]
LDR R0, [R1], #4       ; [R1], then R1 = R1 + 4
LDR R0, [R1, R2]       ; [R1 + R2]
LDR R0, [R1, R2, LSL #2] ; [R1 + R2*4]
```

## Branch Instructions (ARM)

| Instruction | Syntax | Description |
|-------------|--------|-------------|
| `B` | `B label` | Branch |
| `BL` | `BL label` | Branch with link |
| `BX` | `BX Rm` | Branch and exchange |
| `BLX` | `BLX Rm` | Branch with link and exchange |
| `CBZ` | `CBZ Rn, label` | Compare and branch if zero (Thumb-2) |
| `CBNZ` | `CBNZ Rn, label` | Compare and branch if not zero |

---

# 6502 INSTRUCTIONS

## Complete 6502 Instruction Set

| Instruction | Bytes | Cycles | Description | Flags |
|-------------|-------|--------|-------------|-------|
| `ADC` | 2-3 | 2-6 | Add with carry | NVZC |
| `AND` | 2-3 | 2-6 | Logical AND | NZ |
| `ASL` | 1-3 | 2-7 | Arithmetic shift left | NZC |
| `BCC` | 2 | 2-4 | Branch if carry clear | - |
| `BCS` | 2 | 2-4 | Branch if carry set | - |
| `BEQ` | 2 | 2-4 | Branch if equal | - |
| `BIT` | 2-3 | 3-4 | Bit test | NVZ |
| `BMI` | 2 | 2-4 | Branch if minus | - |
| `BNE` | 2 | 2-4 | Branch if not equal | - |
| `BPL` | 2 | 2-4 | Branch if plus | - |
| `BRK` | 1 | 7 | Force interrupt | B |
| `BVC` | 2 | 2-4 | Branch if overflow clear | - |
| `BVS` | 2 | 2-4 | Branch if overflow set | - |
| `CLC` | 1 | 2 | Clear carry | C=0 |
| `CLD` | 1 | 2 | Clear decimal | D=0 |
| `CLI` | 1 | 2 | Clear interrupt | I=0 |
| `CLV` | 1 | 2 | Clear overflow | V=0 |
| `CMP` | 2-3 | 2-6 | Compare accumulator | NZC |
| `CPX` | 2-3 | 2-4 | Compare X | NZC |
| `CPY` | 2-3 | 2-4 | Compare Y | NZC |
| `DEC` | 2-3 | 5-7 | Decrement memory | NZ |
| `DEX` | 1 | 2 | Decrement X | NZ |
| `DEY` | 1 | 2 | Decrement Y | NZ |
| `EOR` | 2-3 | 2-6 | Exclusive OR | NZ |
| `INC` | 2-3 | 5-7 | Increment memory | NZ |
| `INX` | 1 | 2 | Increment X | NZ |
| `INY` | 1 | 2 | Increment Y | NZ |
| `JMP` | 3 | 3-5 | Jump | - |
| `JSR` | 3 | 6 | Jump to subroutine | - |
| `LDA` | 2-3 | 2-6 | Load accumulator | NZ |
| `LDX` | 2-3 | 2-5 | Load X | NZ |
| `LDY` | 2-3 | 2-5 | Load Y | NZ |
| `LSR` | 1-3 | 2-7 | Logical shift right | NZC |
| `NOP` | 1 | 2 | No operation | - |
| `ORA` | 2-3 | 2-6 | Logical OR | NZ |
| `PHA` | 1 | 3 | Push accumulator | - |
| `PHP` | 1 | 3 | Push processor status | - |
| `PLA` | 1 | 4 | Pull accumulator | NZ |
| `PLP` | 1 | 4 | Pull processor status | All |
| `ROL` | 1-3 | 2-7 | Rotate left | NZC |
| `ROR` | 1-3 | 2-7 | Rotate right | NZC |
| `RTI` | 1 | 6 | Return from interrupt | All |
| `RTS` | 1 | 6 | Return from subroutine | - |
| `SBC` | 2-3 | 2-6 | Subtract with carry | NVZC |
| `SEC` | 1 | 2 | Set carry | C=1 |
| `SED` | 1 | 2 | Set decimal | D=1 |
| `SEI` | 1 | 2 | Set interrupt disable | I=1 |
| `STA` | 2-3 | 3-6 | Store accumulator | - |
| `STX` | 2-3 | 3-4 | Store X | - |
| `STY` | 2-3 | 3-4 | Store Y | - |
| `TAX` | 1 | 2 | Transfer A to X | NZ |
| `TAY` | 1 | 2 | Transfer A to Y | NZ |
| `TSX` | 1 | 2 | Transfer SP to X | NZ |
| `TXA` | 1 | 2 | Transfer X to A | NZ |
| `TXS` | 1 | 2 | Transfer X to SP | - |
| `TYA` | 1 | 2 | Transfer Y to A | NZ |

### 6502 Addressing Modes
| Mode | Syntax | Example | Bytes |
|------|--------|---------|-------|
| Immediate | #$nn | LDA #$42 | 2 |
| Zero Page | $nn | LDA $80 | 2 |
| Zero Page,X | $nn,X | LDA $80,X | 2 |
| Zero Page,Y | $nn,Y | LDX $80,Y | 2 |
| Absolute | $nnnn | LDA $1234 | 3 |
| Absolute,X | $nnnn,X | LDA $1234,X | 3 |
| Absolute,Y | $nnnn,Y | LDA $1234,Y | 3 |
| Indirect | ($nnnn) | JMP ($1234) | 3 |
| Indexed Indirect | ($nn,X) | LDA ($80,X) | 2 |
| Indirect Indexed | ($nn),Y | LDA ($80),Y | 2 |
| Accumulator | A | ROL A | 1 |
| Implied | - | INX | 1 |
| Relative | $nn | BEQ label | 2 |

---

# Z80 INSTRUCTIONS

## Z80 Instruction Set (Selected)

### Load Instructions
| Instruction | Syntax | Description | Cycles |
|-------------|--------|-------------|--------|
| `LD` | `LD r, r'` | Load register | 4 |
| `LD` | `LD r, n` | Load immediate | 7 |
| `LD` | `LD r, (HL)` | Load from memory | 7 |
| `LD` | `LD (HL), r` | Store to memory | 7 |
| `LD` | `LD A, (nn)` | Load A from address | 13 |
| `LD` | `LD (nn), A` | Store A to address | 13 |
| `LD` | `LD dd, nn` | Load 16-bit immediate | 10 |
| `LD` | `LD dd, (nn)` | Load 16-bit from memory | 20 |
| `PUSH` | `PUSH qq` | Push register pair | 11 |
| `POP` | `POP qq` | Pop register pair | 10 |
| `EX` | `EX DE, HL` | Exchange DE and HL | 4 |
| `EXX` | `EXX` | Exchange all alt registers | 4 |

### Arithmetic Instructions
| Instruction | Syntax | Flags | Cycles |
|-------------|--------|-------|--------|
| `ADD` | `ADD A, r` | SZYHPC | 4 |
| `ADC` | `ADC A, r` | SZYHPC | 4 |
| `SUB` | `SUB r` | SZYHPC | 4 |
| `SBC` | `SBC A, r` | SZYHPC | 4 |
| `AND` | `AND r` | SZP, H=1, NC | 4 |
| `OR` | `OR r` | SZP, H=0, NC | 4 |
| `XOR` | `XOR r` | SZP, H=0, NC | 4 |
| `CP` | `CP r` | SZYHPC | 4 |
| `INC` | `INC r` | SZYH, P=V | 4 |
| `DEC` | `DEC r` | SZYH, P=V | 4 |
| `NEG` | `NEG` | SZYHPC | 8 |

### Bit Instructions
| Instruction | Syntax | Description | Cycles |
|-------------|--------|-------------|--------|
| `BIT` | `BIT b, r` | Test bit | 8 |
| `SET` | `SET b, r` | Set bit | 8 |
| `RES` | `RES b, r` | Reset bit | 8 |
| `RLC` | `RLC r` | Rotate left circular | 8 |
| `RRC` | `RRC r` | Rotate right circular | 8 |
| `RL` | `RL r` | Rotate left through carry | 8 |
| `RR` | `RR r` | Rotate right through carry | 8 |
| `SLA` | `SLA r` | Shift left arithmetic | 8 |
| `SRA` | `SRA r` | Shift right arithmetic | 8 |
| `SRL` | `SRL r` | Shift right logical | 8 |

### Jump and Call
| Instruction | Syntax | Description | Cycles |
|-------------|--------|-------------|--------|
| `JP` | `JP nn` | Jump | 10 |
| `JP` | `JP cc, nn` | Conditional jump | 10 |
| `JR` | `JR e` | Relative jump | 12 |
| `JR` | `JR cc, e` | Conditional relative | 12/7 |
| `CALL` | `CALL nn` | Call subroutine | 17 |
| `CALL` | `CALL cc, nn` | Conditional call | 17/10 |
| `RET` | `RET` | Return | 10 |
| `RET` | `RET cc` | Conditional return | 11/5 |
| `DJNZ` | `DJNZ e` | Dec B, jump if not zero | 13/8 |

---

# 68000 INSTRUCTIONS

## Motorola 68000 Instruction Set

### Data Movement
| Instruction | Syntax | Description | Flags |
|-------------|--------|-------------|-------|
| `MOVE` | `MOVE.s src, dst` | Move data | NZVC=0 |
| `MOVEA` | `MOVEA.s src, An` | Move to address reg | - |
| `MOVEQ` | `MOVEQ #data, Dn` | Quick move (-128..127) | NZV=0,C=0 |
| `MOVEM` | `MOVEM.s regs, dst` | Move multiple | - |
| `LEA` | `LEA ea, An` | Load effective address | - |
| `PEA` | `PEA ea` | Push effective address | - |
| `EXG` | `EXG Rx, Ry` | Exchange registers | - |
| `SWAP` | `SWAP Dn` | Swap register halves | NZV=0,C=0 |

### Arithmetic
| Instruction | Syntax | Description | Flags |
|-------------|--------|-------------|-------|
| `ADD` | `ADD.s src, dst` | Add | XNZVC |
| `ADDA` | `ADDA.s src, An` | Add to address | - |
| `ADDI` | `ADDI.s #data, dst` | Add immediate | XNZVC |
| `ADDQ` | `ADDQ.s #1-8, dst` | Add quick | XNZVC |
| `SUB` | `SUB.s src, dst` | Subtract | XNZVC |
| `SUBA` | `SUBA.s src, An` | Subtract from address | - |
| `MULS` | `MULS.W src, Dn` | Signed multiply | NZV=0,C=0 |
| `MULU` | `MULU.W src, Dn` | Unsigned multiply | NZV=0,C=0 |
| `DIVS` | `DIVS.W src, Dn` | Signed divide | NZVC |
| `DIVU` | `DIVU.W src, Dn` | Unsigned divide | NZVC |
| `NEG` | `NEG.s dst` | Negate | XNZVC |
| `CLR` | `CLR.s dst` | Clear | N=0,Z=1,V=0,C=0 |

### Logical
| Instruction | Syntax | Description | Flags |
|-------------|--------|-------------|-------|
| `AND` | `AND.s src, dst` | Logical AND | NZV=0,C=0 |
| `OR` | `OR.s src, dst` | Logical OR | NZV=0,C=0 |
| `EOR` | `EOR.s Dn, dst` | Exclusive OR | NZV=0,C=0 |
| `NOT` | `NOT.s dst` | Logical NOT | NZV=0,C=0 |
| `TST` | `TST.s dst` | Test operand | NZV=0,C=0 |

### Shift and Rotate
| Instruction | Syntax | Description | Flags |
|-------------|--------|-------------|-------|
| `ASL` | `ASL.s #cnt/Dn, dst` | Arithmetic shift left | XNZVC |
| `ASR` | `ASR.s #cnt/Dn, dst` | Arithmetic shift right | XNZVC |
| `LSL` | `LSL.s #cnt/Dn, dst` | Logical shift left | XNZVC |
| `LSR` | `LSR.s #cnt/Dn, dst` | Logical shift right | XNZVC |
| `ROL` | `ROL.s #cnt/Dn, dst` | Rotate left | NZV=0,C |
| `ROR` | `ROR.s #cnt/Dn, dst` | Rotate right | NZV=0,C |
| `ROXL` | `ROXL.s #cnt/Dn, dst` | Rotate left through X | XNZV=0,C |
| `ROXR` | `ROXR.s #cnt/Dn, dst` | Rotate right through X | XNZV=0,C |

### Branch and Jump
| Instruction | Syntax | Description |
|-------------|--------|-------------|
| `BRA` | `BRA label` | Branch always |
| `BSR` | `BSR label` | Branch to subroutine |
| `Bcc` | `Bcc label` | Branch on condition |
| `JMP` | `JMP ea` | Jump |
| `JSR` | `JSR ea` | Jump to subroutine |
| `RTS` | `RTS` | Return from subroutine |
| `DBcc` | `DBcc Dn, label` | Test, decrement, branch |

---

# SYSTEM CALL TABLES

## Linux x86 (32-bit) System Calls

| # | Name | EAX | EBX | ECX | EDX | ESI | EDI |
|---|------|-----|-----|-----|-----|-----|-----|
| 1 | exit | 1 | status | - | - | - | - |
| 2 | fork | 2 | - | - | - | - | - |
| 3 | read | 3 | fd | buf | count | - | - |
| 4 | write | 4 | fd | buf | count | - | - |
| 5 | open | 5 | path | flags | mode | - | - |
| 6 | close | 6 | fd | - | - | - | - |
| 7 | waitpid | 7 | pid | status | options | - | - |
| 8 | creat | 8 | path | mode | - | - | - |
| 9 | link | 9 | oldpath | newpath | - | - | - |
| 10 | unlink | 10 | path | - | - | - | - |
| 11 | execve | 11 | path | argv | envp | - | - |
| 12 | chdir | 12 | path | - | - | - | - |
| 13 | time | 13 | tloc | - | - | - | - |
| 19 | lseek | 19 | fd | offset | whence | - | - |
| 20 | getpid | 20 | - | - | - | - | - |
| 33 | access | 33 | path | mode | - | - | - |
| 37 | kill | 37 | pid | sig | - | - | - |
| 39 | mkdir | 39 | path | mode | - | - | - |
| 40 | rmdir | 40 | path | - | - | - | - |
| 45 | brk | 45 | addr | - | - | - | - |
| 54 | ioctl | 54 | fd | cmd | arg | - | - |
| 63 | dup2 | 63 | oldfd | newfd | - | - | - |
| 90 | mmap | 90 | - | - | - | - | - |
| 91 | munmap | 91 | addr | length | - | - | - |
| 102 | socketcall | 102 | call | args | - | - | - |
| 120 | clone | 120 | flags | stack | ptid | tls | ctid |
| 162 | nanosleep | 162 | req | rem | - | - | - |

### x86 Syscall Example
```asm
; write(1, msg, len)
mov eax, 4          ; sys_write
mov ebx, 1          ; stdout
mov ecx, msg        ; buffer
mov edx, len        ; length
int 0x80            ; call kernel
```

## Linux x64 (64-bit) System Calls

| # | Name | RAX | RDI | RSI | RDX | R10 | R8 | R9 |
|---|------|-----|-----|-----|-----|-----|----|----|
| 0 | read | 0 | fd | buf | count | - | - | - |
| 1 | write | 1 | fd | buf | count | - | - | - |
| 2 | open | 2 | path | flags | mode | - | - | - |
| 3 | close | 3 | fd | - | - | - | - | - |
| 4 | stat | 4 | path | statbuf | - | - | - | - |
| 5 | fstat | 5 | fd | statbuf | - | - | - | - |
| 6 | lstat | 6 | path | statbuf | - | - | - | - |
| 7 | poll | 7 | fds | nfds | timeout | - | - | - |
| 8 | lseek | 8 | fd | offset | whence | - | - | - |
| 9 | mmap | 9 | addr | length | prot | flags | fd | offset |
| 10 | mprotect | 10 | addr | length | prot | - | - | - |
| 11 | munmap | 11 | addr | length | - | - | - | - |
| 12 | brk | 12 | addr | - | - | - | - | - |
| 21 | access | 21 | path | mode | - | - | - | - |
| 22 | pipe | 22 | pipefd | - | - | - | - | - |
| 32 | dup | 32 | oldfd | - | - | - | - | - |
| 33 | dup2 | 33 | oldfd | newfd | - | - | - | - |
| 35 | nanosleep | 35 | req | rem | - | - | - | - |
| 39 | getpid | 39 | - | - | - | - | - | - |
| 41 | socket | 41 | domain | type | protocol | - | - | - |
| 42 | connect | 42 | sockfd | addr | addrlen | - | - | - |
| 43 | accept | 43 | sockfd | addr | addrlen | - | - | - |
| 44 | sendto | 44 | sockfd | buf | len | flags | dest | addrlen |
| 45 | recvfrom | 45 | sockfd | buf | len | flags | src | addrlen |
| 49 | bind | 49 | sockfd | addr | addrlen | - | - | - |
| 50 | listen | 50 | sockfd | backlog | - | - | - | - |
| 56 | clone | 56 | flags | stack | ptid | ctid | tls | - |
| 57 | fork | 57 | - | - | - | - | - | - |
| 58 | vfork | 58 | - | - | - | - | - | - |
| 59 | execve | 59 | path | argv | envp | - | - | - |
| 60 | exit | 60 | status | - | - | - | - | - |
| 61 | wait4 | 61 | pid | status | options | rusage | - | - |
| 62 | kill | 62 | pid | sig | - | - | - | - |

### x64 Syscall Example
```asm
; write(1, msg, len)
mov rax, 1          ; sys_write
mov rdi, 1          ; stdout
mov rsi, msg        ; buffer
mov rdx, len        ; length
syscall             ; call kernel
```

## Linux ARM (32-bit) System Calls

| # | Name | R7 | R0 | R1 | R2 | R3 |
|---|------|----|----|----|----|-----|
| 1 | exit | 1 | status | - | - | - |
| 2 | fork | 2 | - | - | - | - |
| 3 | read | 3 | fd | buf | count | - |
| 4 | write | 4 | fd | buf | count | - |
| 5 | open | 5 | path | flags | mode | - |
| 6 | close | 6 | fd | - | - | - |
| 11 | execve | 11 | path | argv | envp | - |
| 19 | lseek | 19 | fd | offset | whence | - |
| 20 | getpid | 20 | - | - | - | - |
| 45 | brk | 45 | addr | - | - | - |
| 54 | ioctl | 54 | fd | cmd | arg | - |
| 90 | mmap | 90 | addr | len | prot | flags |
| 91 | munmap | 91 | addr | length | - | - |
| 120 | clone | 120 | flags | stack | ptid | tls |
| 162 | nanosleep | 162 | req | rem | - | - |

### ARM Syscall Example
```asm
@ write(1, msg, len)
mov r7, #4          @ sys_write
mov r0, #1          @ stdout
ldr r1, =msg        @ buffer
mov r2, #len        @ length
swi #0              @ call kernel
```

## Linux AArch64 (ARM 64-bit) System Calls

| # | Name | X8 | X0 | X1 | X2 | X3 | X4 | X5 |
|---|------|----|----|----|----|----|----|-----|
| 56 | openat | 56 | dirfd | path | flags | mode | - | - |
| 57 | close | 57 | fd | - | - | - | - | - |
| 63 | read | 63 | fd | buf | count | - | - | - |
| 64 | write | 64 | fd | buf | count | - | - | - |
| 93 | exit | 93 | status | - | - | - | - | - |
| 172 | getpid | 172 | - | - | - | - | - | - |
| 220 | clone | 220 | flags | stack | ptid | tls | ctid | - |
| 221 | execve | 221 | path | argv | envp | - | - | - |
| 222 | mmap | 222 | addr | len | prot | flags | fd | offset |
| 226 | mprotect | 226 | addr | len | prot | - | - | - |
| 215 | munmap | 215 | addr | len | - | - | - | - |

### AArch64 Syscall Example
```asm
// write(1, msg, len)
mov x8, #64         // sys_write
mov x0, #1          // stdout
ldr x1, =msg        // buffer
mov x2, len         // length
svc #0              // call kernel
```

---

# REGISTER REFERENCE

## x86/x64 Registers

### General Purpose (64-bit)
| 64-bit | 32-bit | 16-bit | 8-bit High | 8-bit Low | Purpose |
|--------|--------|--------|------------|-----------|---------|
| RAX | EAX | AX | AH | AL | Accumulator |
| RBX | EBX | BX | BH | BL | Base |
| RCX | ECX | CX | CH | CL | Counter |
| RDX | EDX | DX | DH | DL | Data |
| RSI | ESI | SI | - | SIL | Source Index |
| RDI | EDI | DI | - | DIL | Destination Index |
| RBP | EBP | BP | - | BPL | Base Pointer |
| RSP | ESP | SP | - | SPL | Stack Pointer |
| R8-R15 | R8D-R15D | R8W-R15W | - | R8B-R15B | Extended (x64) |

### Segment Registers
| Register | Purpose |
|----------|---------|
| CS | Code Segment |
| DS | Data Segment |
| SS | Stack Segment |
| ES | Extra Segment |
| FS | Extra (TLS in x64) |
| GS | Extra (kernel data in x64) |

### FLAGS Register
| Bit | Flag | Name | Description |
|-----|------|------|-------------|
| 0 | CF | Carry | Unsigned overflow |
| 2 | PF | Parity | Even parity |
| 4 | AF | Auxiliary | BCD carry |
| 6 | ZF | Zero | Result is zero |
| 7 | SF | Sign | Result is negative |
| 8 | TF | Trap | Single step |
| 9 | IF | Interrupt | Enable interrupts |
| 10 | DF | Direction | String direction |
| 11 | OF | Overflow | Signed overflow |

## ARM Registers

### ARM32 Registers
| Register | Alias | Purpose |
|----------|-------|---------|
| R0-R3 | a1-a4 | Arguments/Results |
| R4-R11 | v1-v8 | Callee-saved |
| R12 | IP | Intra-procedure scratch |
| R13 | SP | Stack Pointer |
| R14 | LR | Link Register |
| R15 | PC | Program Counter |
| CPSR | - | Status Register |

### AArch64 Registers
| Register | Purpose |
|----------|---------|
| X0-X7 | Arguments/Results |
| X8 | Indirect result |
| X9-X15 | Temporary |
| X16-X17 | Intra-procedure call |
| X18 | Platform register |
| X19-X28 | Callee-saved |
| X29 | Frame Pointer |
| X30 | Link Register |
| SP | Stack Pointer |
| PC | Program Counter |

---

# ENCODING REFERENCE

## x86 Instruction Encoding

### ModR/M Byte
```
7  6  5  4  3  2  1  0
+--+--+--+--+--+--+--+--+
| Mod  |  Reg  |  R/M  |
+--+--+--+--+--+--+--+--+
```

| Mod | Description |
|-----|-------------|
| 00 | [R/M] or disp32 if R/M=101 |
| 01 | [R/M + disp8] |
| 10 | [R/M + disp32] |
| 11 | Register direct |

### SIB Byte (Scale-Index-Base)
```
7  6  5  4  3  2  1  0
+--+--+--+--+--+--+--+--+
|Scale|  Index | Base  |
+--+--+--+--+--+--+--+--+
```

| Scale | Multiplier |
|-------|------------|
| 00 | 1 |
| 01 | 2 |
| 10 | 4 |
| 11 | 8 |

### REX Prefix (x64)
```
0100 W R X B
     | | | |
     | | | +-- Base extension
     | | +---- Index extension
     | +------ Reg extension
     +-------- 64-bit operand
```

---

*ARCHAEON Arsenal - Assembly Reference v1.0*
*"Knowledge of the machine is power over the machine"*

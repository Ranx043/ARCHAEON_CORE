---
title: "ARSENAL COMPILADORES - Herramientas de Compilación Legacy"
category: "ARCHAEON_ARSENAL"
type: "tool_documentation"
version: "1.0.0"
created: "2025-12-31"
languages: ["Assembly", "COBOL", "C", "Fortran"]
purpose: "Documentación completa de compiladores para lenguajes legacy"
tags: ["compilers", "assembly", "cobol", "c", "fortran", "build-systems"]
---

# ARSENAL COMPILADORES

## Guía Completa de Compiladores para Lenguajes Legacy

Este documento cubre la instalación, configuración y uso de compiladores
para lenguajes de programación legacy incluyendo Assembly, COBOL, C y Fortran.

---

## 1. COMPILADORES ASSEMBLY

### 1.1 NASM (Netwide Assembler)

**Descripción**: Ensamblador portable para x86/x86-64, sintaxis Intel.

#### Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install nasm

# Linux (Fedora/RHEL)
sudo dnf install nasm

# Windows (Chocolatey)
choco install nasm

# macOS
brew install nasm
```

#### Sintaxis Básica

```nasm
; Ejemplo NASM - Hello World (Linux x86-64)
section .data
    msg db "Hello, World!", 0xa
    len equ $ - msg

section .text
    global _start

_start:
    mov rax, 1          ; syscall: write
    mov rdi, 1          ; fd: stdout
    mov rsi, msg        ; buffer
    mov rdx, len        ; length
    syscall

    mov rax, 60         ; syscall: exit
    xor rdi, rdi        ; status: 0
    syscall
```

#### Flags Comunes

```bash
# Formato de salida
nasm -f elf64 program.asm -o program.o    # Linux 64-bit
nasm -f elf32 program.asm -o program.o    # Linux 32-bit
nasm -f win64 program.asm -o program.obj  # Windows 64-bit
nasm -f win32 program.asm -o program.obj  # Windows 32-bit
nasm -f macho64 program.asm -o program.o  # macOS 64-bit

# Debugging symbols
nasm -f elf64 -g -F dwarf program.asm     # DWARF debug info

# Listado
nasm -l listing.lst program.asm           # Generar listado

# Preprocesador
nasm -E program.asm                       # Solo preprocesar
nasm -D SYMBOL=value program.asm          # Definir símbolo
```

### 1.2 MASM (Microsoft Macro Assembler)

**Descripción**: Ensamblador oficial de Microsoft para Windows.

#### Instalación

```powershell
# Incluido con Visual Studio Build Tools
# O descargar independiente de Microsoft

# Verificar instalación
ml64 /?    # MASM 64-bit
ml /?      # MASM 32-bit
```

#### Sintaxis Básica

```asm
; Ejemplo MASM - Windows x64
.code
main PROC
    sub rsp, 28h          ; Shadow space + alignment

    lea rcx, msg          ; lpText
    xor rdx, rdx          ; lpCaption (NULL)
    xor r8d, r8d          ; uType (MB_OK)
    xor r9d, r9d          ; hWnd (NULL)
    call MessageBoxA

    add rsp, 28h
    ret
main ENDP

.data
msg BYTE "Hello from MASM!", 0
END
```

#### Flags Comunes

```powershell
# Compilación básica
ml64 /c program.asm              # Solo compilar
ml64 /Fo output.obj program.asm  # Especificar salida
ml64 /Zi program.asm             # Debug info
ml64 /W3 program.asm             # Nivel de warnings
ml64 /EP program.asm             # Solo preprocesar
```

### 1.3 GAS (GNU Assembler)

**Descripción**: Ensamblador del proyecto GNU, sintaxis AT&T por defecto.

#### Sintaxis AT&T vs Intel

```gas
# AT&T Syntax (default)
movq $1, %rax           # mov rax, 1
movq %rsi, %rdi         # mov rdi, rsi
addq $4, (%rax)         # add qword [rax], 4

# Intel Syntax (con directiva)
.intel_syntax noprefix
mov rax, 1
mov rdi, rsi
add qword ptr [rax], 4
```

#### Flags Comunes

```bash
# Ensamblado
as -o output.o input.s           # Básico
as --64 -o output.o input.s      # 64-bit explícito
as -g -o output.o input.s        # Con debug info
as -a=listing.lst input.s        # Generar listado

# Usar sintaxis Intel
as --msyntax=intel input.s
```

### 1.4 FASM (Flat Assembler)

**Descripción**: Ensamblador auto-hospedado, muy rápido.

#### Instalación

```bash
# Descargar de flatassembler.net
# O en Linux:
sudo apt install fasm
```

#### Sintaxis Única

```fasm
; FASM tiene sintaxis propia
format ELF64 executable
entry _start

segment readable executable
_start:
    mov eax, 1
    mov edi, 1
    mov rsi, msg
    mov edx, msg_len
    syscall

    mov eax, 60
    xor edi, edi
    syscall

segment readable writeable
msg db 'Hello from FASM!', 0xa
msg_len = $ - msg
```

---

## 2. COMPILADORES COBOL

### 2.1 GnuCOBOL

**Descripción**: Compilador COBOL libre que traduce a C.

#### Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install gnucobol

# Linux (Fedora/RHEL)
sudo dnf install gnucobol

# Windows (via MSYS2)
pacman -S mingw-w64-x86_64-gnucobol

# macOS
brew install gnucobol

# Verificar
cobc --version
```

#### Compilación Básica

```bash
# Compilar a ejecutable
cobc -x program.cob -o program

# Compilar a módulo (para llamadas)
cobc -m module.cob

# Compilar múltiples fuentes
cobc -x main.cob sub1.cob sub2.cob -o program

# Solo verificar sintaxis
cobc -fsyntax-only program.cob
```

#### Flags Importantes

```bash
# Estándares COBOL
cobc -std=cobol85 program.cob    # COBOL-85
cobc -std=cobol2002 program.cob  # COBOL-2002
cobc -std=cobol2014 program.cob  # COBOL-2014
cobc -std=ibm program.cob        # Compatibilidad IBM
cobc -std=mf program.cob         # Compatibilidad Micro Focus

# Debugging
cobc -g program.cob              # Símbolos de debug
cobc -ftraceall program.cob      # Trace todas las sentencias
cobc -debug program.cob          # Habilitar debug runtime

# Warnings
cobc -Wall program.cob           # Todos los warnings
cobc -Wextra program.cob         # Warnings extra

# Optimización
cobc -O program.cob              # Optimización básica
cobc -O2 program.cob             # Optimización agresiva
```

#### Ejemplo COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(30) VALUE "Hello from GnuCOBOL!".

       PROCEDURE DIVISION.
           DISPLAY WS-MESSAGE.
           STOP RUN.
```

### 2.2 Micro Focus COBOL

**Descripción**: Compilador COBOL comercial líder.

#### Características

- Visual COBOL para Visual Studio
- COBOL para .NET y JVM
- Soporte completo COBOL-2014
- Herramientas de modernización

#### Comandos Básicos

```bash
# Compilar (línea de comandos)
cob -x program.cbl

# Con opciones
cob -x -C "DIALECT(MF)" program.cbl
cob -x -C "WARNINGS" program.cbl

# Generar listado
cob -x -L program.cbl
```

### 2.3 IBM Enterprise COBOL

**Descripción**: Compilador COBOL para mainframes z/OS.

#### JCL de Compilación

```jcl
//COMPILE  JOB (ACCT),'COMPILE COBOL',CLASS=A
//STEP1    EXEC IGYWCL
//COBOL.SYSIN DD *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MYPROG.
       ...
/*
//LKED.SYSLMOD DD DSN=MY.LOADLIB(MYPROG),DISP=SHR
```

#### Opciones Comunes

```
RENT          - Código reentrante
OPT(2)        - Optimización nivel 2
LIST          - Generar listado
XREF          - Cross-reference
TEST          - Información de debug
TRUNC(OPT)    - Truncamiento optimizado
```

---

## 3. COMPILADORES C

### 3.1 GCC (GNU Compiler Collection)

#### Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install gcc build-essential

# Linux (Fedora/RHEL)
sudo dnf install gcc

# Windows (MinGW-w64)
# Descargar de sourceforge.net/projects/mingw-w64/

# macOS
xcode-select --install
# o
brew install gcc
```

#### Flags de Estándar

```bash
# Estándares C
gcc -std=c89 program.c      # ANSI C / C89
gcc -std=c99 program.c      # C99
gcc -std=c11 program.c      # C11
gcc -std=c17 program.c      # C17
gcc -std=c2x program.c      # C2x (draft)

gcc -std=gnu99 program.c    # C99 + extensiones GNU
```

#### Flags de Optimización

```bash
# Niveles de optimización
gcc -O0 program.c           # Sin optimización (debug)
gcc -O1 program.c           # Optimización básica
gcc -O2 program.c           # Optimización recomendada
gcc -O3 program.c           # Optimización agresiva
gcc -Os program.c           # Optimizar tamaño
gcc -Ofast program.c        # Más rápido (puede no ser estándar)

# Flags específicos
gcc -funroll-loops program.c
gcc -finline-functions program.c
gcc -fomit-frame-pointer program.c
```

#### Flags de Debugging

```bash
gcc -g program.c            # Símbolos de debug
gcc -g3 program.c           # Máximo nivel de debug
gcc -ggdb program.c         # Formato GDB específico
gcc -gdwarf-4 program.c     # DWARF versión 4
```

#### Flags de Warnings

```bash
gcc -Wall program.c         # Warnings comunes
gcc -Wextra program.c       # Warnings adicionales
gcc -Werror program.c       # Warnings como errores
gcc -Wpedantic program.c    # Estricto con el estándar
gcc -Wconversion program.c  # Conversiones implícitas
gcc -Wshadow program.c      # Variable shadowing

# Combinación recomendada
gcc -Wall -Wextra -Wpedantic -Werror program.c
```

### 3.2 Clang

#### Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install clang

# Linux (Fedora/RHEL)
sudo dnf install clang

# Windows
# Descargar de releases.llvm.org

# macOS (incluido con Xcode)
xcode-select --install
```

#### Flags Específicos de Clang

```bash
# Sanitizers
clang -fsanitize=address program.c      # AddressSanitizer
clang -fsanitize=memory program.c       # MemorySanitizer
clang -fsanitize=undefined program.c    # UndefinedBehaviorSan
clang -fsanitize=thread program.c       # ThreadSanitizer

# Análisis estático integrado
clang --analyze program.c

# Compatibilidad GCC
clang -fgnu89-inline program.c
```

### 3.3 MSVC (Microsoft Visual C++)

#### Instalación

```powershell
# Instalar Visual Studio Build Tools
# O Visual Studio Community/Professional

# Usar desde Developer Command Prompt
cl /?
```

#### Flags Comunes

```powershell
# Compilación básica
cl program.c

# Estándares
cl /std:c11 program.c
cl /std:c17 program.c

# Optimización
cl /Od program.c        # Sin optimización
cl /O1 program.c        # Tamaño mínimo
cl /O2 program.c        # Velocidad máxima
cl /Ox program.c        # Full optimization

# Debugging
cl /Zi program.c        # Debug info para CodeView
cl /Z7 program.c        # Debug info en .obj

# Warnings
cl /W4 program.c        # Nivel 4 (máximo)
cl /WX program.c        # Warnings como errores
cl /Wall program.c      # Todos los warnings
```

---

## 4. COMPILADORES FORTRAN

### 4.1 gfortran (GNU Fortran)

#### Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install gfortran

# Linux (Fedora/RHEL)
sudo dnf install gcc-gfortran

# macOS
brew install gcc
```

#### Flags de Estándar

```bash
gfortran -std=f95 program.f90     # Fortran 95
gfortran -std=f2003 program.f90   # Fortran 2003
gfortran -std=f2008 program.f90   # Fortran 2008
gfortran -std=f2018 program.f90   # Fortran 2018
gfortran -std=legacy program.f    # Código legacy
```

#### Flags Útiles

```bash
# Extensiones de archivo
gfortran program.f      # Fixed-form (columnas)
gfortran program.f90    # Free-form

# Forzar forma
gfortran -ffixed-form program.txt
gfortran -ffree-form program.txt

# Debugging
gfortran -g -fbacktrace program.f90
gfortran -fcheck=all program.f90    # Runtime checks

# Warnings
gfortran -Wall -Wextra program.f90
```

### 4.2 Intel Fortran (ifort/ifx)

#### Características

- Altamente optimizado para CPU Intel
- Excelente para HPC
- Soporte completo de Fortran 2018

```bash
# Compilación básica
ifort program.f90 -o program
ifx program.f90 -o program     # oneAPI (nuevo)

# Optimización
ifort -O3 program.f90
ifort -xHost program.f90       # Optimizar para CPU actual

# Parallel
ifort -qopenmp program.f90     # OpenMP
```

### 4.3 Flang (LLVM Fortran)

```bash
# Instalación
sudo apt install flang

# Uso básico
flang program.f90 -o program
```

---

## 5. CROSS-COMPILATION

### 5.1 Configuración de Cross-Compilers

```bash
# Instalar toolchains
sudo apt install gcc-arm-linux-gnueabihf      # ARM 32-bit
sudo apt install gcc-aarch64-linux-gnu        # ARM 64-bit
sudo apt install mingw-w64                    # Windows desde Linux

# Cross-compilar para ARM
arm-linux-gnueabihf-gcc -o program program.c

# Cross-compilar para Windows
x86_64-w64-mingw32-gcc -o program.exe program.c
i686-w64-mingw32-gcc -o program.exe program.c     # 32-bit
```

### 5.2 Sysroots y Libraries

```bash
# Especificar sysroot
gcc --sysroot=/path/to/sysroot program.c

# Incluir paths
gcc -I/cross/include -L/cross/lib program.c
```

---

## 6. BUILD SYSTEMS

### 6.1 Make

#### Makefile Básico

```makefile
CC = gcc
CFLAGS = -Wall -Wextra -O2
LDFLAGS =
SOURCES = main.c utils.c
OBJECTS = $(SOURCES:.c=.o)
TARGET = program

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJECTS) $(TARGET)

.PHONY: all clean
```

#### Comandos Make

```bash
make                    # Build default target
make -j4               # Parallel build (4 jobs)
make clean             # Clean target
make -n                # Dry run
make -B                # Force rebuild
make CFLAGS="-g"       # Override variable
```

### 6.2 CMake

#### CMakeLists.txt Básico

```cmake
cmake_minimum_required(VERSION 3.10)
project(MyProject VERSION 1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

add_executable(program main.c utils.c)

target_compile_options(program PRIVATE -Wall -Wextra)
target_include_directories(program PRIVATE ${CMAKE_SOURCE_DIR}/include)
```

#### Comandos CMake

```bash
# Configurar
mkdir build && cd build
cmake ..
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake -DCMAKE_C_COMPILER=clang ..

# Construir
cmake --build .
cmake --build . --parallel 4

# Instalar
cmake --install . --prefix /usr/local
```

---

## 7. TIPS Y MEJORES PRÁCTICAS

### 7.1 Flags de Producción vs Desarrollo

```bash
# Desarrollo
gcc -g -O0 -Wall -Wextra -fsanitize=address,undefined program.c

# Producción
gcc -O2 -DNDEBUG -march=native program.c
```

### 7.2 Link-Time Optimization (LTO)

```bash
gcc -flto -O2 file1.c file2.c -o program
```

### 7.3 Profile-Guided Optimization (PGO)

```bash
# Paso 1: Instrumentar
gcc -fprofile-generate program.c -o program

# Paso 2: Ejecutar con datos representativos
./program < typical_input.txt

# Paso 3: Recompilar con perfil
gcc -fprofile-use program.c -o program_optimized
```

### 7.4 Verificar Compilador Instalado

```bash
gcc --version
clang --version
gfortran --version
cobc --version
nasm --version
```

---

## 8. REFERENCIAS

- GCC Manual: https://gcc.gnu.org/onlinedocs/
- LLVM/Clang: https://clang.llvm.org/docs/
- NASM: https://www.nasm.us/docs.php
- GnuCOBOL: https://gnucobol.sourceforge.io/
- gfortran: https://gcc.gnu.org/fortran/

---

*Última actualización: 2025-12-31*
*ARCHAEON Arsenal - Compiladores Legacy*

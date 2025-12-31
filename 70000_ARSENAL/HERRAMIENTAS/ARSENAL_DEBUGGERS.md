---
title: "ARSENAL DEBUGGERS - Herramientas de Depuración Legacy"
category: "ARCHAEON_ARSENAL"
type: "tool_documentation"
version: "1.0.0"
created: "2025-12-31"
tools: ["GDB", "LLDB", "WinDbg", "x64dbg", "OllyDbg"]
purpose: "Documentación completa de debuggers para desarrollo legacy"
tags: ["debuggers", "gdb", "reverse-engineering", "debugging", "cobol"]
---

# ARSENAL DEBUGGERS

## Guía Completa de Depuradores para Lenguajes Legacy

Este documento cubre el uso de depuradores para Assembly, C, Fortran,
COBOL y técnicas de ingeniería inversa.

---

## 1. GDB (GNU Debugger)

### 1.1 Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install gdb

# Linux (Fedora/RHEL)
sudo dnf install gdb

# macOS
brew install gdb

# Windows (via MSYS2/MinGW)
pacman -S mingw-w64-x86_64-gdb
```

### 1.2 Compilación para Debug

```bash
# Incluir símbolos de debug
gcc -g -O0 program.c -o program

# Máximo nivel de debug
gcc -g3 -O0 program.c -o program

# Para assembly (NASM)
nasm -f elf64 -g -F dwarf program.asm
ld -o program program.o
```

### 1.3 Comandos Básicos

#### Iniciar GDB

```bash
gdb program                  # Iniciar con programa
gdb -q program               # Modo silencioso
gdb -tui program             # Modo TUI (interfaz texto)
gdb --args program arg1 arg2 # Con argumentos
gdb -p PID                   # Attach a proceso
gdb -c core program          # Analizar core dump
```

#### Navegación y Control

```gdb
# Ejecutar
(gdb) run                    # Ejecutar programa
(gdb) run arg1 arg2          # Con argumentos
(gdb) start                  # Ejecutar hasta main
(gdb) continue               # Continuar ejecución
(gdb) c                      # Alias de continue

# Stepping
(gdb) next                   # Siguiente línea (step over)
(gdb) n                      # Alias
(gdb) step                   # Entrar en función (step into)
(gdb) s                      # Alias
(gdb) nexti                  # Siguiente instrucción
(gdb) ni                     # Alias
(gdb) stepi                  # Step into instrucción
(gdb) si                     # Alias
(gdb) finish                 # Ejecutar hasta return
(gdb) until LINENO           # Ejecutar hasta línea

# Control
(gdb) kill                   # Terminar programa
(gdb) quit                   # Salir de GDB
(gdb) q                      # Alias
```

### 1.4 Breakpoints

```gdb
# Establecer breakpoints
(gdb) break main             # En función main
(gdb) b main                 # Alias
(gdb) break file.c:25        # En línea específica
(gdb) break *0x400550        # En dirección
(gdb) break func if x > 10   # Breakpoint condicional

# Listar y gestionar
(gdb) info breakpoints       # Listar todos
(gdb) info b                 # Alias
(gdb) delete 1               # Eliminar breakpoint 1
(gdb) delete                 # Eliminar todos
(gdb) disable 1              # Deshabilitar breakpoint
(gdb) enable 1               # Habilitar breakpoint
(gdb) clear main             # Eliminar en función
(gdb) clear file.c:25        # Eliminar en línea

# Breakpoints temporales
(gdb) tbreak main            # Se elimina después de hit

# Comandos en breakpoint
(gdb) commands 1
> print x
> continue
> end
```

### 1.5 Watchpoints

```gdb
# Watchpoints (data breakpoints)
(gdb) watch variable         # Cuando variable cambia
(gdb) watch *0x7fff5fbff8    # Dirección de memoria
(gdb) rwatch variable        # Cuando se lee
(gdb) awatch variable        # Lectura o escritura

# Información
(gdb) info watchpoints
```

### 1.6 Examinar Datos

```gdb
# Print valores
(gdb) print variable         # Valor de variable
(gdb) p variable             # Alias
(gdb) print /x variable      # En hexadecimal
(gdb) print /d variable      # En decimal
(gdb) print /t variable      # En binario
(gdb) print /c variable      # Como caracter
(gdb) print array[0]@10      # Array, 10 elementos
(gdb) print *ptr             # Dereferenciar
(gdb) print $rax             # Registro

# Examinar memoria (x)
(gdb) x/10x $rsp             # 10 words hex desde RSP
(gdb) x/s 0x400600           # String
(gdb) x/10i $rip             # 10 instrucciones desde RIP
(gdb) x/20b address          # 20 bytes

# Formatos de x:
# x - hex, d - decimal, u - unsigned, o - octal
# t - binary, a - address, c - char, s - string
# i - instruction, f - float

# Tamaños:
# b - byte (8-bit), h - halfword (16-bit)
# w - word (32-bit), g - giant (64-bit)

# Display automático
(gdb) display variable       # Mostrar en cada paso
(gdb) display /x $rax
(gdb) undisplay 1            # Eliminar display
(gdb) info display           # Listar displays
```

### 1.7 Stack y Backtrace

```gdb
# Backtrace
(gdb) backtrace              # Ver stack completo
(gdb) bt                     # Alias
(gdb) bt full                # Con variables locales
(gdb) bt 5                   # Solo 5 frames

# Frames
(gdb) frame 2                # Ir a frame 2
(gdb) f 2                    # Alias
(gdb) up                     # Subir un frame
(gdb) down                   # Bajar un frame
(gdb) info frame             # Info del frame actual
(gdb) info locals            # Variables locales
(gdb) info args              # Argumentos
```

### 1.8 Registros

```gdb
# Ver registros
(gdb) info registers         # Todos los registros
(gdb) info reg               # Alias
(gdb) info registers rax rbx # Específicos
(gdb) info all-registers     # Incluyendo FPU/SSE

# Modificar registros
(gdb) set $rax = 0x100
(gdb) set $rip = 0x400550
```

### 1.9 Modo TUI (Text User Interface)

```bash
# Iniciar con TUI
gdb -tui program

# Comandos TUI
(gdb) tui enable             # Activar TUI
(gdb) tui disable            # Desactivar TUI
(gdb) layout src             # Vista código fuente
(gdb) layout asm             # Vista assembly
(gdb) layout split           # Código + assembly
(gdb) layout regs            # Registros + código
(gdb) focus cmd              # Foco en comandos
(gdb) focus src              # Foco en código
(gdb) refresh                # Refrescar pantalla

# Atajos en TUI
# Ctrl+x a    - Toggle TUI
# Ctrl+x 1    - Una ventana
# Ctrl+x 2    - Dos ventanas
# Ctrl+L      - Refrescar
```

### 1.10 Scripts y Automatización

```gdb
# Archivo .gdbinit
set disassembly-flavor intel
set print pretty on
set pagination off

# Definir comandos
define pstack
    bt
    info locals
end

# Ejecutar script
(gdb) source script.gdb

# Desde línea de comandos
gdb -x script.gdb program
```

---

## 2. LLDB (LLVM Debugger)

### 2.1 Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install lldb

# macOS (incluido con Xcode)
xcode-select --install

# Verificar
lldb --version
```

### 2.2 Equivalencias GDB -> LLDB

| GDB | LLDB |
|-----|------|
| `run` | `run` o `r` |
| `break main` | `breakpoint set -n main` o `b main` |
| `break file.c:10` | `b file.c:10` |
| `next` | `next` o `n` |
| `step` | `step` o `s` |
| `continue` | `continue` o `c` |
| `print var` | `print var` o `p var` |
| `info registers` | `register read` |
| `backtrace` | `bt` |
| `x/10x addr` | `memory read -c 10 -f x addr` |
| `disassemble` | `disassemble` o `di` |

### 2.3 Comandos Específicos LLDB

```lldb
# Breakpoints
(lldb) breakpoint set --name main
(lldb) b main
(lldb) breakpoint set --file main.c --line 25
(lldb) b main.c:25
(lldb) breakpoint set --address 0x400550
(lldb) breakpoint list
(lldb) breakpoint delete 1
(lldb) breakpoint modify -c "x > 10" 1

# Watchpoints
(lldb) watchpoint set variable var
(lldb) watchpoint set expression -- &var

# Examinar memoria
(lldb) memory read 0x7fff5fbff8
(lldb) memory read -c 20 -f x 0x7fff5fbff8
(lldb) memory read -s 4 -c 10 -f x $rsp

# Registros
(lldb) register read
(lldb) register read rax rbx
(lldb) register write rax 0x100

# Expression evaluation
(lldb) expr variable
(lldb) expr (int)variable + 5
(lldb) expr -f x -- variable

# Disassembly
(lldb) disassemble -f            # Función actual
(lldb) disassemble -n main       # Función específica
(lldb) disassemble -s 0x400550   # Desde dirección
```

### 2.4 GUI con LLDB

```bash
# lldb-mi para IDEs
lldb-mi program

# VS Code integration
# Instalar CodeLLDB extension
```

---

## 3. WinDbg (Windows Debugger)

### 3.1 Instalación

```powershell
# Parte de Windows SDK o
# Descargar WinDbg Preview de Microsoft Store
```

### 3.2 Comandos Básicos

```windbg
# Control de ejecución
g                           ; Go (continuar)
t                           ; Trace (step into)
p                           ; Step over
gu                          ; Go up (hasta return)
.restart                    ; Reiniciar

# Breakpoints
bp address                  ; Breakpoint en dirección
bp module!function          ; Breakpoint en función
bu symbol                   ; Unresolved breakpoint
ba w4 address               ; Hardware breakpoint (write 4 bytes)
bl                          ; Listar breakpoints
bc *                        ; Clear all breakpoints
bd 0                        ; Disable breakpoint 0
be 0                        ; Enable breakpoint 0

# Examinar memoria
db address                  ; Display bytes
dw address                  ; Display words
dd address                  ; Display dwords
dq address                  ; Display qwords
da address                  ; Display ASCII
du address                  ; Display Unicode
dps address                 ; Display pointers with symbols

# Registros
r                           ; Todos los registros
r rax                       ; Registro específico
r rax=0x100                 ; Modificar registro

# Stack
k                           ; Stack trace
kb                          ; Con primeros 3 args
kp                          ; Con todos los parámetros
kn                          ; Con frame numbers
.frame N                    ; Cambiar a frame N

# Symbols
.sympath                    ; Ver symbol path
.sympath+ srv*              ; Agregar Microsoft symbol server
.reload                     ; Recargar símbolos
x module!*                  ; Listar símbolos
ln address                  ; List nearest symbols
```

### 3.3 Kernel Debugging

```windbg
# Configurar target (en máquina objetivo)
bcdedit /debug on
bcdedit /dbgsettings serial debugport:1 baudrate:115200

# Conectar desde host
# File -> Kernel Debug -> COM

# Comandos kernel
!process 0 0                ; Listar procesos
!thread                     ; Thread actual
!analyze -v                 ; Análisis de crash
!pool address               ; Info de pool
!irql                       ; IRQL actual
!devobj address             ; Device object
!drvobj DriverName          ; Driver object
```

### 3.4 Debugging Scripts

```windbg
# Ejecutar script
$<script.txt

# Alias
as ShortName LongCommand

# Conditional breakpoints
bp address ".if (condition) {commands} .else {gc}"

# Logging
.logopen logfile.txt
.logclose
```

---

## 4. x64dbg / OllyDbg

### 4.1 x64dbg

**Descripción**: Debugger open-source para Windows x64/x86.

#### Instalación

```
Descargar de: https://x64dbg.com/
Extraer y ejecutar x64dbg.exe o x32dbg.exe
```

#### Atajos de Teclado

```
F2          - Toggle breakpoint
F7          - Step into
F8          - Step over
F9          - Run
Ctrl+F9     - Execute till return
F12         - Pause
Space       - Assemble at address
Ctrl+G      - Go to address
Ctrl+E      - Edit memory
Ctrl+B      - Binary search
Ctrl+F      - Pattern search
```

#### Comandos

```
# En command bar
bp address              ; Breakpoint
bc address              ; Clear breakpoint
bph address, rw, size   ; Hardware breakpoint
bpm address, rw         ; Memory breakpoint
run                     ; Ejecutar
sti                     ; Step into
sto                     ; Step over
rtr                     ; Run to return
rtu                     ; Run to user code

# Logging
log "mensaje"
logclear

# Variables
$result = eax
```

### 4.2 OllyDbg

**Descripción**: Debugger clásico para Windows 32-bit (legacy pero útil).

#### Atajos Principales

```
F2          - Breakpoint
F7          - Step into
F8          - Step over
F9          - Run
Ctrl+F9     - Execute till return
Alt+F9      - Execute till user code
F12         - Pause
Space       - Assemble
Ctrl+G      - Go to expression
```

#### Análisis

```
Right-click -> Analyze -> Analyze code
Right-click -> Search for -> All referenced strings
Right-click -> Search for -> All intermodular calls
```

---

## 5. DEBUGGING FORTRAN CON GDB

### 5.1 Compilación

```bash
# Con símbolos de debug
gfortran -g -O0 -fbacktrace program.f90 -o program

# Con checks de runtime
gfortran -g -fcheck=all -fbacktrace program.f90
```

### 5.2 Tips Específicos Fortran

```gdb
# Ver arrays
(gdb) print array
(gdb) print array(1:10)        # Slice

# Variables de módulos
(gdb) print modulename::variable

# COMMON blocks
(gdb) print 'commonname'::variable

# Llamar subrutinas
(gdb) call mysub(arg1, arg2)

# Ver tipo derivado
(gdb) print mystruct
(gdb) print mystruct%field
```

---

## 6. DEBUGGING COBOL

### 6.1 GnuCOBOL Debugging

```bash
# Compilar con debug
cobc -g -x program.cob

# Debug con GDB
gdb program
```

```gdb
# En GDB
(gdb) break program.cob:100    # Línea específica
(gdb) print WS-VARIABLE        # Variable COBOL
(gdb) print WS-RECORD.FIELD    # Campo de estructura
```

### 6.2 Trace en GnuCOBOL

```bash
# Compilar con trace
cobc -x -ftraceall program.cob

# Ejecutar genera trace automático
./program
```

### 6.3 Micro Focus Animator

**Descripción**: Debugger visual para Micro Focus COBOL.

#### Características

- Debugging visual paso a paso
- Inspección de variables COBOL
- Breakpoints en párrafos
- Watch expressions
- Call stack COBOL

#### Comandos

```
F7          - Step into
F8          - Step over
F10         - Run
F9          - Toggle breakpoint
Ctrl+B      - Set breakpoint
```

### 6.4 IBM XPEDITER

**Descripción**: Debugger para COBOL en mainframes z/OS.

#### Características

- Debugging interactivo en TSO/ISPF
- Breakpoints en párrafos y líneas
- Keep/drop de variables
- Análisis de archivos

#### Comandos XPEDITER

```
AT LINE nnn           ; Breakpoint en línea
AT PARA paragraph     ; Breakpoint en párrafo
GO                    ; Continuar
STEP                  ; Paso a paso
PEEK variable         ; Ver variable
KEEP variable         ; Monitorear variable
LIST BREAKS           ; Listar breakpoints
DELETE BREAK ALL      ; Eliminar breakpoints
```

---

## 7. REMOTE DEBUGGING

### 7.1 GDB Remote

#### En Target (máquina remota)

```bash
# Usar gdbserver
gdbserver :1234 ./program
gdbserver :1234 --attach PID
```

#### En Host (máquina local)

```bash
gdb ./program
(gdb) target remote hostname:1234
(gdb) continue
```

### 7.2 LLDB Remote

```bash
# Target
lldb-server platform --server --listen *:1234

# Host
(lldb) platform select remote-linux
(lldb) platform connect connect://hostname:1234
(lldb) target create program
(lldb) run
```

### 7.3 SSH Tunneling

```bash
# Crear túnel
ssh -L 1234:localhost:1234 user@remote

# Conectar GDB localmente
gdb -ex "target remote localhost:1234"
```

---

## 8. TIPS AVANZADOS

### 8.1 Core Dumps

```bash
# Habilitar core dumps
ulimit -c unlimited

# Analizar core dump
gdb program core

# Comandos en core
(gdb) bt                     # Stack trace
(gdb) info threads           # Ver threads
(gdb) thread apply all bt    # BT de todos los threads
```

### 8.2 Reverse Debugging (GDB)

```gdb
# Habilitar recording
(gdb) target record-full
(gdb) run

# Comandos reverse
(gdb) reverse-next          # rn
(gdb) reverse-step          # rs
(gdb) reverse-continue      # rc
(gdb) reverse-finish

# Record info
(gdb) info record
```

### 8.3 Python en GDB

```gdb
(gdb) python print(gdb.parse_and_eval("variable"))

# Pretty printers
(gdb) python
import gdb.printing
class MyPrinter(gdb.printing.PrettyPrinter):
    # ...
end
```

### 8.4 Configuración .gdbinit

```gdb
# ~/.gdbinit
set disassembly-flavor intel
set print pretty on
set print array on
set print array-indexes on
set pagination off
set confirm off

# Historial
set history save on
set history size 10000
set history filename ~/.gdb_history

# Python support
python
import os
sys.path.insert(0, os.path.expanduser('~/.gdb'))
end

# Alias útiles
define xxd
    dump binary memory /tmp/dump.bin $arg0 $arg0+$arg1
    shell xxd /tmp/dump.bin
end
```

---

## 9. TROUBLESHOOTING

### 9.1 Problemas Comunes

```bash
# "No debugging symbols found"
# Recompilar con -g

# "Cannot access memory at address"
# Verificar permisos, ASLR, PIE

# Deshabilitar ASLR temporalmente
echo 0 | sudo tee /proc/sys/kernel/randomize_va_space

# Compilar sin PIE
gcc -no-pie -g program.c
```

### 9.2 ptrace y Permisos

```bash
# Error: "Operation not permitted"
# Modificar ptrace_scope
echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope

# O con capabilities
sudo setcap cap_sys_ptrace+ep /usr/bin/gdb
```

---

## 10. REFERENCIAS

- GDB Manual: https://sourceware.org/gdb/documentation/
- LLDB Tutorial: https://lldb.llvm.org/use/tutorial.html
- WinDbg Docs: https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/
- x64dbg: https://help.x64dbg.com/

---

*Última actualización: 2025-12-31*
*ARCHAEON Arsenal - Debuggers Legacy*

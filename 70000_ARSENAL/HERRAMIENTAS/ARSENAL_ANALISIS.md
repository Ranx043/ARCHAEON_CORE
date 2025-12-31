---
title: "ARSENAL ANALISIS - Herramientas de Analisis Estatico"
category: "ARCHAEON_ARSENAL"
type: "tool_documentation"
version: "1.0.0"
created: "2025-12-31"
tools: ["IDA Pro", "Ghidra", "radare2", "cppcheck", "Valgrind"]
purpose: "Documentación de herramientas de análisis estático y binario"
tags: ["static-analysis", "reverse-engineering", "binary-analysis", "security"]
---

# ARSENAL ANALISIS

## Guía Completa de Herramientas de Análisis Estático

Este documento cubre herramientas de análisis estático, ingeniería inversa,
análisis de binarios y verificación de código para lenguajes legacy.

---

## 1. ANÁLISIS DE ASSEMBLY / BINARIOS

### 1.1 IDA Pro

**Descripción**: El estándar de la industria para ingeniería inversa y análisis de binarios.

#### Características Principales

- Desensamblador interactivo
- Decompilador (Hex-Rays)
- Soporte multi-arquitectura
- Scripting Python/IDC
- Plugins extensivos

#### Interfaz y Navegación

```
# Atajos principales
G               - Go to address
N               - Rename symbol
X               - Cross-references to
Ctrl+X          - Cross-references from
Space           - Toggle graph/text view
Tab             - Toggle assembly/pseudocode
H               - Toggle hex/decimal
D               - Data definition
C               - Code definition
U               - Undefine
P               - Create function
; (semicolon)   - Add comment
```

#### Python Scripting (IDAPython)

```python
# Obtener dirección actual
import idaapi
import idc

ea = idc.here()
print(f"Dirección actual: {hex(ea)}")

# Iterar sobre funciones
for func_ea in idautils.Functions():
    func_name = idc.get_func_name(func_ea)
    print(f"Función: {func_name} en {hex(func_ea)}")

# Buscar string
pattern = "password"
for s in idautils.Strings():
    if pattern in str(s):
        print(f"Encontrado en {hex(s.ea)}: {s}")

# Buscar patrón de bytes
pattern = "48 8B 05 ?? ?? ?? ??"
addr = idc.find_binary(0, idc.SEARCH_DOWN, pattern)
if addr != idc.BADADDR:
    print(f"Patrón encontrado en {hex(addr)}")

# Cross-references
for xref in idautils.XrefsTo(ea):
    print(f"Referencia desde {hex(xref.frm)}")
```

### 1.2 Ghidra

**Descripción**: Framework de ingeniería inversa de NSA, gratuito y open-source.

#### Instalación

```bash
# Descargar de: https://ghidra-sre.org/
# Requiere JDK 11+

# Linux/macOS
unzip ghidra_*.zip
cd ghidra_*
./ghidraRun

# Windows
# Extraer y ejecutar ghidraRun.bat
```

#### Características

- Desensamblador/Decompilador gratuito
- Soporte multi-plataforma y multi-arquitectura
- Scripting en Java y Python (Jython)
- Colaboración en equipo
- Extensible con plugins

#### Atajos Principales

```
G               - Go to address
L               - Rename/Label
Ctrl+Shift+E    - Edit function signature
D               - Disassemble
C               - Clear code
T               - Set data type
;               - Add comment
Ctrl+/          - Add plate comment
Tab             - Switch listing/decompiler
F               - Create function
X               - References to
Ctrl+Shift+F    - Find references to field
```

#### Scripting (Python/Jython)

```python
# ghidra_script.py
# @category: Analysis
# @author: ARCHAEON

from ghidra.program.model.symbol import SymbolType

# Obtener programa actual
program = getCurrentProgram()
listing = program.getListing()

# Iterar funciones
fm = program.getFunctionManager()
for func in fm.getFunctions(True):
    print(f"Función: {func.getName()} @ {func.getEntryPoint()}")

# Buscar strings
for data in listing.getDefinedData(True):
    if data.hasStringValue():
        print(f"String @ {data.getAddress()}: {data.getValue()}")

# Buscar llamadas a función específica
from ghidra.app.decompiler import DecompInterface
decompiler = DecompInterface()
decompiler.openProgram(program)

target_func = getFunction("malloc")
if target_func:
    refs = getReferencesTo(target_func.getEntryPoint())
    for ref in refs:
        print(f"malloc llamado desde: {ref.getFromAddress()}")
```

#### Headless Analysis

```bash
# Análisis sin GUI
./analyzeHeadless /path/to/project ProjectName \
    -import binary.exe \
    -postScript script.py \
    -scriptPath /scripts
```

### 1.3 radare2 / rizin

**Descripción**: Framework de ingeniería inversa en línea de comandos.

#### Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install radare2

# Desde git (última versión)
git clone https://github.com/radareorg/radare2
cd radare2
sys/install.sh

# rizin (fork moderno)
sudo apt install rizin
```

#### Comandos Básicos

```bash
# Abrir binario
r2 binary
r2 -d binary              # Con debugger
r2 -A binary              # Análisis automático

# En shell de r2:
[0x00401000]> aaa         # Analizar todo
[0x00401000]> afl         # Listar funciones
[0x00401000]> pdf         # Disassemble función actual
[0x00401000]> pdf @ main  # Disassemble main
[0x00401000]> s main      # Seek a main
[0x00401000]> VV          # Vista gráfica
[0x00401000]> V           # Vista visual
[0x00401000]> q           # Salir
```

#### Comandos de Análisis

```bash
# Información
[0x00401000]> i           # Info general
[0x00401000]> ie          # Entrypoint
[0x00401000]> iS          # Secciones
[0x00401000]> ii          # Imports
[0x00401000]> iE          # Exports
[0x00401000]> iz          # Strings en data
[0x00401000]> izz         # Todas las strings

# Funciones
[0x00401000]> afl         # Listar funciones
[0x00401000]> afn newname # Renombrar función
[0x00401000]> afi         # Info de función actual
[0x00401000]> afv         # Variables locales

# Cross-references
[0x00401000]> axt         # Xrefs to current
[0x00401000]> axf         # Xrefs from current
[0x00401000]> axt @ sym.main

# Búsqueda
[0x00401000]> / password  # Buscar string
[0x00401000]> /x 9090     # Buscar bytes
[0x00401000]> /c jmp      # Buscar instrucción
```

#### Debugging con r2

```bash
# Iniciar debug
r2 -d binary

[0x7f...]> db main        # Breakpoint en main
[0x7f...]> dc             # Continue
[0x7f...]> ds             # Step
[0x7f...]> dso            # Step over
[0x7f...]> dr             # Ver registros
[0x7f...]> drr            # Ver registros referenciados
[0x7f...]> px 64          # Print hex 64 bytes
[0x7f...]> dm             # Memory maps
```

#### r2pipe (Scripting)

```python
import r2pipe

r2 = r2pipe.open("binary")
r2.cmd("aaa")                    # Analizar

# Obtener funciones
funcs = r2.cmdj("aflj")          # JSON output
for f in funcs:
    print(f"{f['name']} @ {hex(f['offset'])}")

# Buscar strings
strings = r2.cmdj("izj")
for s in strings:
    print(f"{s['string']}")

r2.quit()
```

---

## 2. ANÁLISIS ESTÁTICO DE C

### 2.1 cppcheck

**Descripción**: Analizador estático para C/C++, detecta bugs sin compilar.

#### Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install cppcheck

# macOS
brew install cppcheck

# Windows
choco install cppcheck
```

#### Uso Básico

```bash
# Analizar archivo
cppcheck file.c

# Analizar directorio
cppcheck src/

# Habilitar todos los checks
cppcheck --enable=all file.c

# Checks específicos
cppcheck --enable=warning file.c
cppcheck --enable=style file.c
cppcheck --enable=performance file.c
cppcheck --enable=portability file.c
cppcheck --enable=information file.c
cppcheck --enable=unusedFunction file.c

# Con estándar específico
cppcheck --std=c11 file.c
cppcheck --std=c89 file.c

# Verbose
cppcheck -v file.c

# Output XML
cppcheck --xml --xml-version=2 file.c 2> results.xml

# Suprimir warnings
cppcheck --suppress=unusedFunction file.c

# Con includes
cppcheck -I /usr/include -I ./include file.c
```

#### Configuración

```bash
# Archivo de supresiones
# suppressions.txt:
# unusedFunction:file.c
# uninitvar

cppcheck --suppressions-list=suppressions.txt file.c

# Reglas personalizadas (XML)
cppcheck --rule-file=rules.xml file.c
```

### 2.2 clang-tidy

**Descripción**: Linter basado en Clang con fixes automáticos.

#### Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install clang-tidy

# macOS
brew install llvm
```

#### Uso Básico

```bash
# Analizar archivo
clang-tidy file.c

# Con compile_commands.json
clang-tidy -p build/ file.c

# Checks específicos
clang-tidy -checks='*' file.c
clang-tidy -checks='-*,bugprone-*' file.c
clang-tidy -checks='-*,modernize-*,readability-*' file.cpp

# Fix automático
clang-tidy -fix file.c

# Listar checks disponibles
clang-tidy -list-checks

# Con header filter
clang-tidy -header-filter='.*' file.c
```

#### Archivo .clang-tidy

```yaml
---
Checks: >
  -*,
  bugprone-*,
  cert-*,
  clang-analyzer-*,
  cppcoreguidelines-*,
  misc-*,
  modernize-*,
  performance-*,
  readability-*,
  -modernize-use-trailing-return-type

WarningsAsErrors: ''
HeaderFilterRegex: '.*'
FormatStyle: file
CheckOptions:
  - key: readability-identifier-naming.VariableCase
    value: lower_case
  - key: readability-identifier-naming.FunctionCase
    value: lower_case
```

### 2.3 Coverity (Conceptos)

**Descripción**: Herramienta comercial de análisis estático avanzado.

#### Características

- Detección de defectos complejos
- Análisis inter-procedimental
- Base de datos de defectos
- Integración CI/CD
- Gestión de defectos

#### Tipos de Defectos Detectados

```
# Categorías principales:
- NULL_RETURNS: Uso de valor NULL
- RESOURCE_LEAK: Fugas de recursos
- USE_AFTER_FREE: Uso después de liberar
- BUFFER_SIZE: Problemas de buffer
- UNINIT: Variables no inicializadas
- DEADCODE: Código muerto
- TAINTED_SCALAR: Input no sanitizado
```

---

## 3. ANÁLISIS DE COBOL

### 3.1 SonarQube COBOL Plugin

**Descripción**: Análisis de calidad de código COBOL.

#### Configuración Básica

```properties
# sonar-project.properties
sonar.projectKey=my-cobol-project
sonar.projectName=My COBOL Project
sonar.sources=src/cobol
sonar.cobol.file.suffixes=.cbl,.cob,.cpy
sonar.cobol.copy.directories=src/copybooks
sonar.cobol.sql.catalog.defaultSchema=SCHEMA1
```

#### Reglas Comunes

```
# Reglas de código:
- Paragraphs should not be too complex
- PERFORM statements should not be nested too deeply
- Sections and paragraphs should be reachable
- Dead code should be removed
- GOTO statements should be avoided
- Variables should be initialized before use

# Métricas:
- Cognitive Complexity
- Cyclomatic Complexity
- Lines of Code
- Comment density
```

### 3.2 Micro Focus Enterprise Analyzer

**Descripción**: Herramienta de análisis para modernización COBOL.

#### Características

- Análisis de dependencias
- Documentación automática
- Impact analysis
- Diagrama de flujo automático
- Métricas de código

### 3.3 IBM Application Discovery

**Descripción**: Análisis de aplicaciones mainframe.

```
# Características:
- Análisis de COBOL, PL/I, Assembler
- Mapeo de dependencias
- Análisis de datos
- Documentación técnica
- Visualización de arquitectura
```

---

## 4. HERRAMIENTAS DE BINARIOS (GNU)

### 4.1 objdump

**Descripción**: Muestra información de archivos objeto.

```bash
# Disassembly
objdump -d binary              # Disassemble secciones ejecutables
objdump -D binary              # Disassemble todas las secciones
objdump -d -M intel binary     # Sintaxis Intel
objdump -d -S binary           # Con código fuente

# Headers
objdump -f binary              # File headers
objdump -h binary              # Section headers
objdump -p binary              # Private headers
objdump -x binary              # All headers

# Secciones específicas
objdump -d -j .text binary     # Solo .text
objdump -s -j .rodata binary   # Contenido de .rodata

# Símbolos
objdump -t binary              # Symbol table
objdump -T binary              # Dynamic symbols

# Relocaciones
objdump -r binary              # Relocations
objdump -R binary              # Dynamic relocations
```

### 4.2 nm

**Descripción**: Lista símbolos de archivos objeto.

```bash
# Uso básico
nm binary                      # Listar símbolos
nm -n binary                   # Ordenar por dirección
nm -g binary                   # Solo externos
nm -u binary                   # Solo indefinidos
nm -C binary                   # Demangle C++
nm -D binary                   # Dynamic symbols
nm -S binary                   # Con tamaños

# Filtrar por tipo
nm -t x binary                 # Direcciones en hex
nm --defined-only binary       # Solo definidos

# Tipos de símbolos comunes:
# T/t - Código (text)
# D/d - Datos inicializados
# B/b - BSS (no inicializados)
# U   - Undefined
# R/r - Read-only data
```

### 4.3 readelf

**Descripción**: Muestra información de archivos ELF.

```bash
# Headers
readelf -h binary              # ELF header
readelf -l binary              # Program headers
readelf -S binary              # Section headers
readelf -e binary              # All headers

# Símbolos
readelf -s binary              # Symbol table
readelf --dyn-syms binary      # Dynamic symbols

# Dependencias
readelf -d binary              # Dynamic section
readelf -n binary              # Notes

# Detalle
readelf -a binary              # Todo
readelf -W binary              # Wide output

# Secciones específicas
readelf -p .rodata binary      # Strings en .rodata
readelf -x .data binary        # Hex dump de .data
```

### 4.4 dumpbin (Windows)

**Descripción**: Equivalente de objdump para Windows PE.

```powershell
# En Developer Command Prompt
dumpbin /headers binary.exe
dumpbin /exports binary.dll
dumpbin /imports binary.exe
dumpbin /disasm binary.exe
dumpbin /symbols binary.obj
dumpbin /dependents binary.exe
dumpbin /all binary.exe
```

### 4.5 strings

**Descripción**: Extrae strings imprimibles de binarios.

```bash
# Uso básico
strings binary                 # Strings ASCII
strings -a binary              # Todas las secciones
strings -n 10 binary           # Mínimo 10 caracteres
strings -t x binary            # Con offset hex
strings -e l binary            # Little-endian Unicode
strings -e b binary            # Big-endian Unicode
```

---

## 5. MEMORY CHECKERS

### 5.1 Valgrind

**Descripción**: Framework de instrumentación para debugging de memoria.

#### Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install valgrind

# macOS (limitado)
brew install valgrind
```

#### Memcheck (Detector de errores de memoria)

```bash
# Uso básico
valgrind ./program

# Con más detalle
valgrind --leak-check=full ./program
valgrind --leak-check=full --show-leak-kinds=all ./program
valgrind --track-origins=yes ./program

# Completo
valgrind --leak-check=full \
         --show-leak-kinds=all \
         --track-origins=yes \
         --verbose \
         --log-file=valgrind.log \
         ./program

# Suprimir errores conocidos
valgrind --suppressions=mysupp.supp ./program
```

#### Tipos de Errores

```
# Errores detectados por Memcheck:
- Invalid read/write: Acceso fuera de bounds
- Use of uninitialised values: Valores no inicializados
- Invalid free: Free de memoria no allocada
- Mismatched free: new/free, malloc/delete mismatch
- Memory leaks: Memoria no liberada
  - Definitely lost: Perdida definitiva
  - Indirectly lost: Perdida por puntero perdido
  - Possibly lost: Posiblemente perdida
  - Still reachable: Alcanzable pero no liberada
```

#### Otras Herramientas Valgrind

```bash
# Cachegrind - Profiler de caché
valgrind --tool=cachegrind ./program
cg_annotate cachegrind.out.*

# Callgrind - Profiler de llamadas
valgrind --tool=callgrind ./program
callgrind_annotate callgrind.out.*
kcachegrind callgrind.out.*        # GUI

# Helgrind - Detector de race conditions
valgrind --tool=helgrind ./program

# DRD - Otro detector de threading
valgrind --tool=drd ./program

# Massif - Profiler de heap
valgrind --tool=massif ./program
ms_print massif.out.*
```

### 5.2 AddressSanitizer (ASan)

**Descripción**: Detector de errores de memoria en tiempo de compilación.

#### Uso

```bash
# Compilar con ASan
gcc -fsanitize=address -g program.c -o program
clang -fsanitize=address -g program.c -o program

# Ejecutar normalmente
./program

# Variables de entorno
ASAN_OPTIONS=detect_leaks=1 ./program
ASAN_OPTIONS=halt_on_error=0 ./program
ASAN_OPTIONS=symbolize=1 ./program
```

#### Errores Detectados

```
- Heap buffer overflow
- Stack buffer overflow
- Global buffer overflow
- Use after free
- Use after return
- Use after scope
- Double free
- Memory leaks (con LeakSanitizer)
```

### 5.3 Otros Sanitizers

```bash
# MemorySanitizer (detecta uso de memoria no inicializada)
clang -fsanitize=memory -g program.c

# UndefinedBehaviorSanitizer
gcc -fsanitize=undefined -g program.c
clang -fsanitize=undefined -g program.c

# ThreadSanitizer (data races)
gcc -fsanitize=thread -g program.c

# Combinaciones
clang -fsanitize=address,undefined -g program.c
```

---

## 6. TIPS Y MEJORES PRÁCTICAS

### 6.1 Workflow de Análisis de Binarios

```bash
# 1. Identificación inicial
file binary
strings binary | head -50
readelf -h binary  # o dumpbin /headers

# 2. Análisis de estructura
readelf -S binary
objdump -h binary

# 3. Búsqueda de funciones interesantes
nm binary | grep -i main
objdump -t binary | grep -i crypt

# 4. Análisis detallado en herramienta
# Abrir en Ghidra/IDA/r2
```

### 6.2 Integración en CI/CD

```yaml
# GitHub Actions ejemplo
- name: Static Analysis
  run: |
    cppcheck --enable=all --error-exitcode=1 src/

- name: Memory Check
  run: |
    gcc -fsanitize=address -g tests.c -o tests
    ./tests
```

### 6.3 Reglas de Análisis Personalizadas

```xml
<!-- cppcheck rule -->
<rule>
  <pattern>strcpy\s*\(</pattern>
  <message>
    <severity>warning</severity>
    <summary>Consider using strncpy instead of strcpy</summary>
  </message>
</rule>
```

---

## 7. REFERENCIAS

- IDA Pro: https://hex-rays.com/ida-pro/
- Ghidra: https://ghidra-sre.org/
- radare2: https://rada.re/
- cppcheck: http://cppcheck.net/
- Valgrind: https://valgrind.org/
- LLVM Sanitizers: https://clang.llvm.org/docs/

---

*Última actualización: 2025-12-31*
*ARCHAEON Arsenal - Análisis Estático*

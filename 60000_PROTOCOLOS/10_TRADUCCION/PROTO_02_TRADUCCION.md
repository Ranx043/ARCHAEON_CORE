# PROTO_02_TRADUCCION - Patrones de Traduccion

> ARCHAEON - Guardian del Codigo Ancestral
> Dominio: Traduccion entre Lenguajes Legacy y Modernos
> Nivel: Avanzado

## Indice

1. [Fundamentos de Traduccion](#1-fundamentos-de-traduccion)
2. [Assembly a C/Rust](#2-assembly-a-crust)
3. [COBOL a Python/Java](#3-cobol-a-pythonjava)
4. [C a Rust/Go](#4-c-a-rustgo)
5. [Fortran a Python](#5-fortran-a-python)
6. [Preservacion Semantica](#6-preservacion-semantica)
7. [Casos Especiales](#7-casos-especiales)
8. [Estrategias Automatizadas](#8-estrategias-automatizadas)

---

## 1. Fundamentos de Traduccion

### 1.1 Principios de Traduccion de Codigo

```yaml
PRINCIPIOS_FUNDAMENTALES:
  fidelidad_semantica:
    - Preservar comportamiento exacto
    - Mantener efectos secundarios
    - Respetar orden de operaciones

  claridad:
    - Codigo idiomatico en destino
    - Nombres significativos
    - Estructura logica

  eficiencia:
    - No degradar rendimiento
    - Aprovechar caracteristicas modernas
    - Optimizar donde sea seguro

  mantenibilidad:
    - Codigo legible
    - Documentacion de decisiones
    - Trazabilidad al original
```

### 1.2 Proceso de Traduccion

```
FUENTE          ANALISIS          TRANSFORMACION       DESTINO
+-------+      +----------+       +-------------+      +--------+
| Legacy| ---> | AST/IR   | --->  | Patrones    | ---> | Modern |
| Code  |      | Analysis |       | Application |      | Code   |
+-------+      +----------+       +-------------+      +--------+
                    |                   |
                    v                   v
              Symbol Tables      Mapping Rules
              Type Info          Idiom Transform
              Control Flow       Optimization
```

### 1.3 Niveles de Traduccion

```python
class TraduccionNivel:
    """Niveles de fidelidad en traduccion"""

    LITERAL = 1      # Traduccion linea por linea
    ESTRUCTURAL = 2  # Reorganiza estructuras
    IDIOMATICO = 3   # Usa patrones del lenguaje destino
    OPTIMIZADO = 4   # Mejora rendimiento y claridad

    @staticmethod
    def seleccionar_nivel(contexto):
        if contexto.critico_seguridad:
            return TraduccionNivel.LITERAL
        elif contexto.requiere_auditoria:
            return TraduccionNivel.ESTRUCTURAL
        elif contexto.modernizacion_completa:
            return TraduccionNivel.IDIOMATICO
        else:
            return TraduccionNivel.OPTIMIZADO
```

---

## 2. Assembly a C/Rust

### 2.1 Registros a Variables

```asm
; === ASSEMBLY ORIGINAL (x86) ===
section .data
    resultado dd 0

section .text
global calcular
calcular:
    push ebp
    mov ebp, esp

    mov eax, [ebp+8]    ; primer parametro
    mov ebx, [ebp+12]   ; segundo parametro
    add eax, ebx        ; suma
    imul eax, 2         ; multiplicar por 2
    mov [resultado], eax

    pop ebp
    ret
```

```c
// === TRADUCCION A C ===
static int resultado = 0;

int calcular(int param1, int param2) {
    int eax = param1;      // mov eax, [ebp+8]
    int ebx = param2;      // mov ebx, [ebp+12]
    eax = eax + ebx;       // add eax, ebx
    eax = eax * 2;         // imul eax, 2
    resultado = eax;       // mov [resultado], eax
    return eax;
}

// === VERSION IDIOMATICA ===
static int resultado = 0;

int calcular(int a, int b) {
    resultado = (a + b) * 2;
    return resultado;
}
```

```rust
// === TRADUCCION A RUST ===
use std::sync::atomic::{AtomicI32, Ordering};

static RESULTADO: AtomicI32 = AtomicI32::new(0);

fn calcular(a: i32, b: i32) -> i32 {
    let result = (a + b) * 2;
    RESULTADO.store(result, Ordering::SeqCst);
    result
}

// Version sin estado global (preferida)
fn calcular_puro(a: i32, b: i32) -> i32 {
    (a + b) * 2
}
```

### 2.2 Direccionamiento de Memoria

```asm
; === ASSEMBLY - Acceso a array ===
section .data
    array dd 10, 20, 30, 40, 50

section .text
suma_array:
    xor eax, eax          ; suma = 0
    xor ecx, ecx          ; i = 0
.loop:
    cmp ecx, 5
    jge .done
    add eax, [array + ecx*4]  ; suma += array[i]
    inc ecx
    jmp .loop
.done:
    ret
```

```c
// === TRADUCCION A C ===
int array[] = {10, 20, 30, 40, 50};

int suma_array(void) {
    int suma = 0;
    for (int i = 0; i < 5; i++) {
        suma += array[i];
    }
    return suma;
}
```

```rust
// === TRADUCCION A RUST ===
const ARRAY: [i32; 5] = [10, 20, 30, 40, 50];

fn suma_array() -> i32 {
    ARRAY.iter().sum()
}

// O version explicita
fn suma_array_explicita() -> i32 {
    let mut suma = 0;
    for &valor in ARRAY.iter() {
        suma += valor;
    }
    suma
}
```

### 2.3 Flags y Condicionales

```asm
; === ASSEMBLY - Comparaciones con flags ===
comparar:
    cmp eax, ebx
    jl  menor          ; jump if less (SF != OF)
    je  igual          ; jump if equal (ZF = 1)
    jg  mayor          ; jump if greater (ZF=0 and SF=OF)
menor:
    mov ecx, -1
    ret
igual:
    mov ecx, 0
    ret
mayor:
    mov ecx, 1
    ret
```

```c
// === TRADUCCION A C ===
int comparar(int a, int b) {
    if (a < b) {
        return -1;
    } else if (a == b) {
        return 0;
    } else {
        return 1;
    }
}

// Version compacta
int comparar_compacto(int a, int b) {
    return (a > b) - (a < b);
}
```

```rust
// === TRADUCCION A RUST ===
use std::cmp::Ordering;

fn comparar(a: i32, b: i32) -> i32 {
    match a.cmp(&b) {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

// Usando signum
fn comparar_signum(a: i32, b: i32) -> i32 {
    (a - b).signum()
}
```

### 2.4 Llamadas al Sistema

```asm
; === ASSEMBLY - Syscall Linux x86-64 ===
; write(1, mensaje, len)
section .data
    mensaje db "Hola", 10
    len equ $ - mensaje

section .text
escribir:
    mov rax, 1          ; syscall write
    mov rdi, 1          ; stdout
    lea rsi, [mensaje]  ; buffer
    mov rdx, len        ; longitud
    syscall
    ret
```

```c
// === TRADUCCION A C ===
#include <unistd.h>

void escribir(void) {
    const char mensaje[] = "Hola\n";
    write(STDOUT_FILENO, mensaje, sizeof(mensaje) - 1);
}

// Version con stdio
#include <stdio.h>
void escribir_stdio(void) {
    printf("Hola\n");
}
```

```rust
// === TRADUCCION A RUST ===
fn escribir() {
    println!("Hola");
}

// Version con syscall directo (unsafe)
#[cfg(target_os = "linux")]
fn escribir_syscall() {
    use std::io::{self, Write};
    io::stdout().write_all(b"Hola\n").unwrap();
}
```

---

## 3. COBOL a Python/Java

### 3.1 PICTURE a Tipos de Datos

```cobol
       * === COBOL DATA DIVISION ===
       01  CLIENTE-REGISTRO.
           05  CLI-CODIGO        PIC 9(8).
           05  CLI-NOMBRE        PIC X(30).
           05  CLI-SALDO         PIC S9(7)V99.
           05  CLI-FECHA-ALTA.
               10  CLI-ANNO      PIC 9(4).
               10  CLI-MES       PIC 9(2).
               10  CLI-DIA       PIC 9(2).
           05  CLI-ACTIVO        PIC 9 VALUE 1.
               88  ACTIVO        VALUE 1.
               88  INACTIVO      VALUE 0.
```

```python
# === TRADUCCION A PYTHON ===
from dataclasses import dataclass
from decimal import Decimal
from datetime import date
from typing import Optional

@dataclass
class ClienteRegistro:
    codigo: int                    # PIC 9(8)
    nombre: str                    # PIC X(30)
    saldo: Decimal                 # PIC S9(7)V99
    fecha_alta: date               # Grupo de fecha
    activo: bool = True            # PIC 9 con 88s

    def __post_init__(self):
        # Validaciones tipo COBOL
        if self.codigo < 0 or self.codigo > 99999999:
            raise ValueError("Codigo fuera de rango")
        if len(self.nombre) > 30:
            self.nombre = self.nombre[:30]
        # Saldo con 2 decimales
        self.saldo = Decimal(str(self.saldo)).quantize(Decimal('0.01'))

    @classmethod
    def from_cobol_string(cls, data: str) -> 'ClienteRegistro':
        """Parse desde formato COBOL de longitud fija"""
        return cls(
            codigo=int(data[0:8]),
            nombre=data[8:38].strip(),
            saldo=Decimal(data[38:47]) / 100,  # V99 = 2 decimales implicitos
            fecha_alta=date(
                int(data[47:51]),  # Año
                int(data[51:53]),  # Mes
                int(data[53:55])   # Dia
            ),
            activo=data[55] == '1'
        )
```

```java
// === TRADUCCION A JAVA ===
import java.math.BigDecimal;
import java.time.LocalDate;

public class ClienteRegistro {
    private int codigo;           // PIC 9(8)
    private String nombre;        // PIC X(30)
    private BigDecimal saldo;     // PIC S9(7)V99
    private LocalDate fechaAlta;  // Grupo de fecha
    private boolean activo;       // PIC 9 con 88s

    public ClienteRegistro() {
        this.activo = true;  // VALUE 1
    }

    // Getters y setters con validacion COBOL
    public void setCodigo(int codigo) {
        if (codigo < 0 || codigo > 99999999) {
            throw new IllegalArgumentException("Codigo fuera de rango");
        }
        this.codigo = codigo;
    }

    public void setNombre(String nombre) {
        this.nombre = nombre.length() > 30
            ? nombre.substring(0, 30)
            : String.format("%-30s", nombre);
    }

    public void setSaldo(BigDecimal saldo) {
        this.saldo = saldo.setScale(2, BigDecimal.ROUND_HALF_UP);
    }

    // Parser desde formato COBOL
    public static ClienteRegistro fromCobolString(String data) {
        ClienteRegistro cliente = new ClienteRegistro();
        cliente.setCodigo(Integer.parseInt(data.substring(0, 8)));
        cliente.setNombre(data.substring(8, 38).trim());
        cliente.setSaldo(new BigDecimal(data.substring(38, 47))
            .divide(new BigDecimal(100)));
        cliente.setFechaAlta(LocalDate.of(
            Integer.parseInt(data.substring(47, 51)),
            Integer.parseInt(data.substring(51, 53)),
            Integer.parseInt(data.substring(53, 55))
        ));
        cliente.setActivo(data.charAt(55) == '1');
        return cliente;
    }
}
```

### 3.2 PERFORM a Estructuras de Control

```cobol
       * === COBOL PROCEDURE DIVISION ===
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INICIALIZAR
           PERFORM PROCESAR-CLIENTES
               UNTIL FIN-ARCHIVO
           PERFORM GENERAR-REPORTE
           STOP RUN.

       INICIALIZAR.
           OPEN INPUT ARCHIVO-CLIENTES
           OPEN OUTPUT ARCHIVO-REPORTE
           MOVE 0 TO TOTAL-PROCESADOS
           MOVE 0 TO TOTAL-ERRORES.

       PROCESAR-CLIENTES.
           READ ARCHIVO-CLIENTES
               AT END SET FIN-ARCHIVO TO TRUE
               NOT AT END PERFORM VALIDAR-CLIENTE
           END-READ.

       VALIDAR-CLIENTE.
           IF CLI-SALDO < 0
               ADD 1 TO TOTAL-ERRORES
               PERFORM REGISTRAR-ERROR
           ELSE
               ADD 1 TO TOTAL-PROCESADOS
               PERFORM ACTUALIZAR-CLIENTE
           END-IF.
```

```python
# === TRADUCCION A PYTHON ===
class ProcesadorClientes:
    def __init__(self):
        self.archivo_clientes = None
        self.archivo_reporte = None
        self.total_procesados = 0
        self.total_errores = 0
        self.fin_archivo = False

    def main(self):
        """MAIN-PARA"""
        self.inicializar()
        while not self.fin_archivo:
            self.procesar_clientes()
        self.generar_reporte()

    def inicializar(self):
        """INICIALIZAR"""
        self.archivo_clientes = open('clientes.dat', 'r')
        self.archivo_reporte = open('reporte.txt', 'w')
        self.total_procesados = 0
        self.total_errores = 0

    def procesar_clientes(self):
        """PROCESAR-CLIENTES"""
        linea = self.archivo_clientes.readline()
        if not linea:
            self.fin_archivo = True
        else:
            cliente = ClienteRegistro.from_cobol_string(linea)
            self.validar_cliente(cliente)

    def validar_cliente(self, cliente):
        """VALIDAR-CLIENTE"""
        if cliente.saldo < 0:
            self.total_errores += 1
            self.registrar_error(cliente)
        else:
            self.total_procesados += 1
            self.actualizar_cliente(cliente)

    def registrar_error(self, cliente):
        """REGISTRAR-ERROR"""
        self.archivo_reporte.write(
            f"ERROR: Cliente {cliente.codigo} con saldo negativo\n"
        )

    def actualizar_cliente(self, cliente):
        """ACTUALIZAR-CLIENTE"""
        # Logica de actualizacion
        pass

    def generar_reporte(self):
        """GENERAR-REPORTE"""
        self.archivo_reporte.write(
            f"Procesados: {self.total_procesados}\n"
            f"Errores: {self.total_errores}\n"
        )
        self.archivo_clientes.close()
        self.archivo_reporte.close()

# Version mas Pythonica
def procesar_clientes_pythonic(ruta_entrada, ruta_salida):
    """Version idiomatica Python"""
    errores = []
    procesados = []

    with open(ruta_entrada, 'r') as f:
        for linea in f:
            cliente = ClienteRegistro.from_cobol_string(linea)
            if cliente.saldo < 0:
                errores.append(cliente)
            else:
                procesados.append(cliente)

    with open(ruta_salida, 'w') as f:
        f.write(f"Procesados: {len(procesados)}\n")
        f.write(f"Errores: {len(errores)}\n")
        for cliente in errores:
            f.write(f"ERROR: Cliente {cliente.codigo}\n")

    return procesados, errores
```

### 3.3 Archivos Secuenciales e Indexados

```cobol
       * === COBOL - Archivo Indexado ===
       SELECT ARCHIVO-CLIENTES
           ASSIGN TO "CLIENTES.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CLI-CODIGO
           ALTERNATE KEY IS CLI-NOMBRE WITH DUPLICATES
           FILE STATUS IS WS-FILE-STATUS.

       PROCEDURE DIVISION.
       BUSCAR-CLIENTE.
           MOVE 12345678 TO CLI-CODIGO
           READ ARCHIVO-CLIENTES
               INVALID KEY
                   DISPLAY "Cliente no encontrado"
               NOT INVALID KEY
                   DISPLAY CLI-NOMBRE
           END-READ.

       AGREGAR-CLIENTE.
           MOVE 99999999 TO CLI-CODIGO
           MOVE "NUEVO CLIENTE" TO CLI-NOMBRE
           WRITE CLIENTE-REGISTRO
               INVALID KEY
                   DISPLAY "Error al escribir"
           END-WRITE.
```

```python
# === TRADUCCION A PYTHON con SQLite ===
import sqlite3
from contextlib import contextmanager

class ArchivoIndexadoClientes:
    """Emula archivo indexado COBOL con SQLite"""

    def __init__(self, ruta_db):
        self.ruta = ruta_db
        self.file_status = "00"
        self._crear_tabla()

    def _crear_tabla(self):
        with self._conexion() as conn:
            conn.execute('''
                CREATE TABLE IF NOT EXISTS clientes (
                    codigo INTEGER PRIMARY KEY,
                    nombre TEXT,
                    saldo REAL,
                    fecha_alta TEXT,
                    activo INTEGER
                )
            ''')
            conn.execute('''
                CREATE INDEX IF NOT EXISTS idx_nombre
                ON clientes(nombre)
            ''')

    @contextmanager
    def _conexion(self):
        conn = sqlite3.connect(self.ruta)
        try:
            yield conn
            conn.commit()
        finally:
            conn.close()

    def read_by_key(self, codigo: int) -> ClienteRegistro:
        """READ con clave primaria"""
        with self._conexion() as conn:
            cursor = conn.execute(
                'SELECT * FROM clientes WHERE codigo = ?',
                (codigo,)
            )
            row = cursor.fetchone()
            if row is None:
                self.file_status = "23"  # INVALID KEY
                raise KeyError(f"Cliente {codigo} no encontrado")
            self.file_status = "00"
            return self._row_to_cliente(row)

    def read_by_nombre(self, nombre: str) -> list:
        """READ con clave alternativa (con duplicados)"""
        with self._conexion() as conn:
            cursor = conn.execute(
                'SELECT * FROM clientes WHERE nombre LIKE ?',
                (f'%{nombre}%',)
            )
            return [self._row_to_cliente(row) for row in cursor]

    def write(self, cliente: ClienteRegistro):
        """WRITE - Agregar registro"""
        try:
            with self._conexion() as conn:
                conn.execute('''
                    INSERT INTO clientes VALUES (?, ?, ?, ?, ?)
                ''', (
                    cliente.codigo,
                    cliente.nombre,
                    float(cliente.saldo),
                    cliente.fecha_alta.isoformat(),
                    1 if cliente.activo else 0
                ))
            self.file_status = "00"
        except sqlite3.IntegrityError:
            self.file_status = "22"  # INVALID KEY (duplicado)
            raise

    def rewrite(self, cliente: ClienteRegistro):
        """REWRITE - Actualizar registro"""
        with self._conexion() as conn:
            result = conn.execute('''
                UPDATE clientes
                SET nombre=?, saldo=?, fecha_alta=?, activo=?
                WHERE codigo=?
            ''', (
                cliente.nombre,
                float(cliente.saldo),
                cliente.fecha_alta.isoformat(),
                1 if cliente.activo else 0,
                cliente.codigo
            ))
            if result.rowcount == 0:
                self.file_status = "23"
                raise KeyError("Registro no existe")
            self.file_status = "00"

    def delete(self, codigo: int):
        """DELETE - Eliminar registro"""
        with self._conexion() as conn:
            result = conn.execute(
                'DELETE FROM clientes WHERE codigo = ?',
                (codigo,)
            )
            if result.rowcount == 0:
                self.file_status = "23"
                raise KeyError("Registro no existe")
            self.file_status = "00"

    def start(self, codigo: int):
        """START - Posicionar para lectura secuencial"""
        self._cursor_position = codigo
        self.file_status = "00"

    def read_next(self) -> ClienteRegistro:
        """READ NEXT - Lectura secuencial"""
        with self._conexion() as conn:
            cursor = conn.execute(
                'SELECT * FROM clientes WHERE codigo >= ? LIMIT 1',
                (self._cursor_position,)
            )
            row = cursor.fetchone()
            if row is None:
                self.file_status = "10"  # EOF
                raise StopIteration()
            self._cursor_position = row[0] + 1
            self.file_status = "00"
            return self._row_to_cliente(row)

    def _row_to_cliente(self, row):
        from datetime import date
        return ClienteRegistro(
            codigo=row[0],
            nombre=row[1],
            saldo=Decimal(str(row[2])),
            fecha_alta=date.fromisoformat(row[3]),
            activo=bool(row[4])
        )


# Uso
archivo = ArchivoIndexadoClientes('clientes.db')

# Buscar cliente (READ)
try:
    cliente = archivo.read_by_key(12345678)
    print(cliente.nombre)
except KeyError:
    print("Cliente no encontrado")

# Agregar cliente (WRITE)
nuevo = ClienteRegistro(99999999, "NUEVO CLIENTE", Decimal('0'), date.today())
try:
    archivo.write(nuevo)
except sqlite3.IntegrityError:
    print("Error al escribir")
```

---

## 4. C a Rust/Go

### 4.1 Punteros a Referencias y Ownership

```c
// === C ORIGINAL ===
#include <stdlib.h>
#include <string.h>

typedef struct {
    char* nombre;
    int edad;
} Persona;

Persona* crear_persona(const char* nombre, int edad) {
    Persona* p = malloc(sizeof(Persona));
    if (p == NULL) return NULL;

    p->nombre = malloc(strlen(nombre) + 1);
    if (p->nombre == NULL) {
        free(p);
        return NULL;
    }
    strcpy(p->nombre, nombre);
    p->edad = edad;
    return p;
}

void liberar_persona(Persona* p) {
    if (p != NULL) {
        free(p->nombre);
        free(p);
    }
}

void modificar_edad(Persona* p, int nueva_edad) {
    if (p != NULL) {
        p->edad = nueva_edad;
    }
}
```

```rust
// === TRADUCCION A RUST ===

// Estructura con ownership claro
struct Persona {
    nombre: String,  // String es owned
    edad: i32,
}

impl Persona {
    // Constructor - transfiere ownership del String creado
    fn new(nombre: &str, edad: i32) -> Persona {
        Persona {
            nombre: nombre.to_string(),
            edad,
        }
    }

    // Metodo que toma &mut self - referencia mutable
    fn modificar_edad(&mut self, nueva_edad: i32) {
        self.edad = nueva_edad;
    }

    // Getter que retorna referencia
    fn nombre(&self) -> &str {
        &self.nombre
    }
}

// Drop se implementa automaticamente
// No necesitamos liberar_persona()

fn main() {
    // Crear persona - ownership en 'persona'
    let mut persona = Persona::new("Juan", 30);

    // Modificar - prestamo mutable
    persona.modificar_edad(31);

    // Leer - prestamo inmutable
    println!("Nombre: {}", persona.nombre());

    // persona se libera automaticamente al salir del scope
}

// Version con Option para manejar nulos
fn crear_persona_option(nombre: &str, edad: i32) -> Option<Persona> {
    if nombre.is_empty() {
        None
    } else {
        Some(Persona::new(nombre, edad))
    }
}
```

```go
// === TRADUCCION A GO ===
package main

type Persona struct {
    Nombre string
    Edad   int
}

// Constructor - Go no tiene constructores, usamos funcion
func NuevaPersona(nombre string, edad int) *Persona {
    return &Persona{
        Nombre: nombre,
        Edad:   edad,
    }
}

// Metodo con receiver puntero para modificar
func (p *Persona) ModificarEdad(nuevaEdad int) {
    if p != nil {
        p.Edad = nuevaEdad
    }
}

// Metodo con receiver valor para solo lectura
func (p Persona) GetNombre() string {
    return p.Nombre
}

func main() {
    // Crear persona
    persona := NuevaPersona("Juan", 30)

    // Modificar
    persona.ModificarEdad(31)

    // GC se encarga de liberar memoria
}
```

### 4.2 malloc/free a Ownership y Box

```c
// === C - Gestion manual de memoria ===
typedef struct Node {
    int data;
    struct Node* next;
} Node;

Node* crear_lista(int* valores, int n) {
    if (n == 0) return NULL;

    Node* head = malloc(sizeof(Node));
    head->data = valores[0];
    head->next = NULL;

    Node* current = head;
    for (int i = 1; i < n; i++) {
        current->next = malloc(sizeof(Node));
        current = current->next;
        current->data = valores[i];
        current->next = NULL;
    }
    return head;
}

void liberar_lista(Node* head) {
    while (head != NULL) {
        Node* temp = head;
        head = head->next;
        free(temp);
    }
}

int sumar_lista(Node* head) {
    int suma = 0;
    while (head != NULL) {
        suma += head->data;
        head = head->next;
    }
    return suma;
}
```

```rust
// === TRADUCCION A RUST ===

// Usando Box para heap allocation
struct Node {
    data: i32,
    next: Option<Box<Node>>,
}

impl Node {
    fn new(data: i32) -> Node {
        Node { data, next: None }
    }
}

fn crear_lista(valores: &[i32]) -> Option<Box<Node>> {
    if valores.is_empty() {
        return None;
    }

    let mut head = Box::new(Node::new(valores[0]));
    let mut current = &mut head;

    for &valor in &valores[1..] {
        current.next = Some(Box::new(Node::new(valor)));
        current = current.next.as_mut().unwrap();
    }

    Some(head)
}

// No necesitamos liberar_lista - Drop automatico

fn sumar_lista(head: &Option<Box<Node>>) -> i32 {
    let mut suma = 0;
    let mut current = head;

    while let Some(node) = current {
        suma += node.data;
        current = &node.next;
    }

    suma
}

// Version idiomatica con iterador
impl Node {
    fn iter(&self) -> NodeIter {
        NodeIter { current: Some(self) }
    }
}

struct NodeIter<'a> {
    current: Option<&'a Node>,
}

impl<'a> Iterator for NodeIter<'a> {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        self.current.map(|node| {
            self.current = node.next.as_deref();
            node.data
        })
    }
}

fn sumar_idiomatico(head: &Option<Box<Node>>) -> i32 {
    head.as_ref()
        .map(|n| n.iter().sum())
        .unwrap_or(0)
}
```

### 4.3 Manejo de Errores

```c
// === C - Codigos de error ===
#include <errno.h>

typedef enum {
    OK = 0,
    ERROR_ARCHIVO = -1,
    ERROR_MEMORIA = -2,
    ERROR_FORMATO = -3
} ResultCode;

ResultCode procesar_archivo(const char* ruta, int* resultado) {
    FILE* f = fopen(ruta, "r");
    if (f == NULL) {
        return ERROR_ARCHIVO;
    }

    int* buffer = malloc(1000 * sizeof(int));
    if (buffer == NULL) {
        fclose(f);
        return ERROR_MEMORIA;
    }

    int valor;
    int count = 0;
    while (fscanf(f, "%d", &valor) == 1 && count < 1000) {
        buffer[count++] = valor;
    }

    if (count == 0) {
        free(buffer);
        fclose(f);
        return ERROR_FORMATO;
    }

    *resultado = 0;
    for (int i = 0; i < count; i++) {
        *resultado += buffer[i];
    }

    free(buffer);
    fclose(f);
    return OK;
}
```

```rust
// === TRADUCCION A RUST con Result ===
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug)]
enum ProcesarError {
    ArchivoNoEncontrado(std::io::Error),
    FormatoInvalido(String),
    ArchivoVacio,
}

fn procesar_archivo(ruta: &str) -> Result<i32, ProcesarError> {
    // ? propaga errores automaticamente
    let file = File::open(ruta)
        .map_err(ProcesarError::ArchivoNoEncontrado)?;

    let reader = BufReader::new(file);
    let mut valores = Vec::new();

    for (num_linea, linea) in reader.lines().enumerate() {
        let linea = linea.map_err(ProcesarError::ArchivoNoEncontrado)?;

        for palabra in linea.split_whitespace() {
            let valor: i32 = palabra.parse()
                .map_err(|_| ProcesarError::FormatoInvalido(
                    format!("Linea {}: '{}' no es numero", num_linea + 1, palabra)
                ))?;
            valores.push(valor);
        }
    }

    if valores.is_empty() {
        return Err(ProcesarError::ArchivoVacio);
    }

    Ok(valores.iter().sum())
}

// Uso
fn main() {
    match procesar_archivo("datos.txt") {
        Ok(suma) => println!("Suma: {}", suma),
        Err(ProcesarError::ArchivoNoEncontrado(e)) => {
            eprintln!("No se pudo abrir archivo: {}", e)
        }
        Err(ProcesarError::FormatoInvalido(msg)) => {
            eprintln!("Error de formato: {}", msg)
        }
        Err(ProcesarError::ArchivoVacio) => {
            eprintln!("El archivo esta vacio")
        }
    }
}
```

```go
// === TRADUCCION A GO ===
package main

import (
    "bufio"
    "errors"
    "fmt"
    "os"
    "strconv"
    "strings"
)

var (
    ErrArchivoVacio   = errors.New("archivo vacio")
    ErrFormatoInvalido = errors.New("formato invalido")
)

func procesarArchivo(ruta string) (int, error) {
    file, err := os.Open(ruta)
    if err != nil {
        return 0, fmt.Errorf("no se pudo abrir archivo: %w", err)
    }
    defer file.Close()

    var valores []int
    scanner := bufio.NewScanner(file)
    numLinea := 0

    for scanner.Scan() {
        numLinea++
        linea := scanner.Text()

        for _, palabra := range strings.Fields(linea) {
            valor, err := strconv.Atoi(palabra)
            if err != nil {
                return 0, fmt.Errorf("linea %d: '%s' %w",
                    numLinea, palabra, ErrFormatoInvalido)
            }
            valores = append(valores, valor)
        }
    }

    if err := scanner.Err(); err != nil {
        return 0, err
    }

    if len(valores) == 0 {
        return 0, ErrArchivoVacio
    }

    suma := 0
    for _, v := range valores {
        suma += v
    }

    return suma, nil
}

func main() {
    suma, err := procesarArchivo("datos.txt")
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        os.Exit(1)
    }
    fmt.Printf("Suma: %d\n", suma)
}
```

---

## 5. Fortran a Python

### 5.1 Arrays a NumPy

```fortran
! === FORTRAN - Operaciones con matrices ===
program matriz_ops
    implicit none

    integer, parameter :: N = 100
    real(8) :: A(N, N), B(N, N), C(N, N)
    real(8) :: suma, producto_punto
    integer :: i, j

    ! Inicializar matrices
    do j = 1, N
        do i = 1, N
            A(i, j) = real(i + j, 8)
            B(i, j) = real(i * j, 8)
        end do
    end do

    ! Suma de matrices
    C = A + B

    ! Multiplicacion elemento a elemento
    C = A * B

    ! Multiplicacion matricial
    C = matmul(A, B)

    ! Suma de todos los elementos
    suma = sum(A)

    ! Producto punto de columnas
    producto_punto = dot_product(A(:, 1), B(:, 1))

    ! Operaciones con slices
    C(1:50, 1:50) = A(51:100, 51:100)

    print *, "Suma total:", suma
    print *, "Producto punto:", producto_punto

end program matriz_ops
```

```python
# === TRADUCCION A PYTHON con NumPy ===
import numpy as np

def matriz_ops():
    N = 100

    # Inicializar matrices
    # Fortran es column-major, NumPy es row-major por default
    i, j = np.meshgrid(np.arange(1, N+1), np.arange(1, N+1), indexing='ij')
    A = (i + j).astype(np.float64)
    B = (i * j).astype(np.float64)

    # Suma de matrices
    C = A + B

    # Multiplicacion elemento a elemento
    C = A * B

    # Multiplicacion matricial
    C = A @ B  # o np.matmul(A, B)

    # Suma de todos los elementos
    suma = np.sum(A)

    # Producto punto de columnas
    # Fortran: A(:, 1) es primera columna
    # NumPy: A[:, 0] es primera columna (0-indexed)
    producto_punto = np.dot(A[:, 0], B[:, 0])

    # Operaciones con slices
    # Fortran 1:50 = indices 1 a 50
    # Python 0:50 = indices 0 a 49 (equivalente)
    # Fortran 51:100 = Python 50:100
    C[0:50, 0:50] = A[50:100, 50:100]

    print(f"Suma total: {suma}")
    print(f"Producto punto: {producto_punto}")

    return C

# Version mas Pythonica
def matriz_ops_pythonic():
    N = 100

    # Inicializacion vectorizada
    indices = np.arange(1, N+1)
    A = np.add.outer(indices, indices).astype(np.float64)
    B = np.multiply.outer(indices, indices).astype(np.float64)

    # Operaciones
    C = A @ B

    print(f"Suma total: {A.sum()}")
    print(f"Producto punto primera columna: {A[:, 0] @ B[:, 0]}")

    return C
```

### 5.2 DO Loops a Comprensiones y Vectorizacion

```fortran
! === FORTRAN - Varios tipos de loops ===
subroutine procesar_datos(datos, n, resultado)
    implicit none
    integer, intent(in) :: n
    real(8), intent(in) :: datos(n)
    real(8), intent(out) :: resultado

    real(8) :: temp(n)
    integer :: i
    real(8) :: media, desv

    ! Loop simple
    resultado = 0.0d0
    do i = 1, n
        resultado = resultado + datos(i)
    end do

    ! Loop con condicion
    do i = 1, n
        if (datos(i) > 0.0d0) then
            temp(i) = sqrt(datos(i))
        else
            temp(i) = 0.0d0
        end if
    end do

    ! Loop con paso
    do i = 1, n, 2
        temp(i) = temp(i) * 2.0d0
    end do

    ! Loop implicito (array constructor)
    temp = [(real(i, 8)**2, i = 1, n)]

    ! Calcular estadisticas
    media = sum(datos) / real(n, 8)
    desv = sqrt(sum((datos - media)**2) / real(n, 8))

end subroutine
```

```python
# === TRADUCCION A PYTHON ===
import numpy as np

def procesar_datos(datos: np.ndarray) -> float:
    n = len(datos)

    # Loop simple -> sum()
    resultado = np.sum(datos)

    # Loop con condicion -> np.where o comprension
    temp = np.where(datos > 0, np.sqrt(datos), 0.0)

    # Loop con paso -> slicing
    temp[::2] *= 2.0  # indices 0, 2, 4, ...

    # Loop implicito -> arange
    temp = np.arange(1, n + 1, dtype=np.float64) ** 2

    # Estadisticas vectorizadas
    media = np.mean(datos)
    desv = np.std(datos)

    return resultado

# Traduccion literal (para comparacion)
def procesar_datos_literal(datos):
    n = len(datos)

    # Loop simple
    resultado = 0.0
    for i in range(n):
        resultado += datos[i]

    # Loop con condicion
    temp = np.zeros(n)
    for i in range(n):
        if datos[i] > 0:
            temp[i] = np.sqrt(datos[i])
        else:
            temp[i] = 0.0

    # Loop con paso (Fortran 1,n,2 = Python 0,n,2)
    for i in range(0, n, 2):
        temp[i] *= 2.0

    return resultado
```

### 5.3 COMMON Blocks a Modulos/Clases

```fortran
! === FORTRAN - COMMON blocks ===
! archivo: globals.f90
      common /config/ max_iter, tolerancia, verbose
      integer max_iter
      real*8 tolerancia
      logical verbose

      common /datos/ matriz, n_filas, n_cols
      real*8 matriz(1000, 1000)
      integer n_filas, n_cols

! archivo: main.f90
      program main
      common /config/ max_iter, tolerancia, verbose
      integer max_iter
      real*8 tolerancia
      logical verbose

      max_iter = 100
      tolerancia = 1.0d-6
      verbose = .true.

      call procesar()
      end

! archivo: procesador.f90
      subroutine procesar()
      common /config/ max_iter, tolerancia, verbose
      common /datos/ matriz, n_filas, n_cols
      integer max_iter, n_filas, n_cols
      real*8 tolerancia, matriz(1000, 1000)
      logical verbose

      integer iter
      real*8 error

      do iter = 1, max_iter
          ! ... procesar matriz ...
          if (error < tolerancia) exit
          if (verbose) print *, 'Iteracion:', iter
      end do
      end
```

```python
# === TRADUCCION A PYTHON ===

# Opcion 1: Modulo con variables globales
# archivo: config.py
max_iter: int = 100
tolerancia: float = 1e-6
verbose: bool = True

# archivo: datos.py
import numpy as np
matriz: np.ndarray = np.zeros((1000, 1000))
n_filas: int = 0
n_cols: int = 0

# archivo: procesador.py
import config
import datos

def procesar():
    for iter in range(1, config.max_iter + 1):
        # ... procesar datos.matriz ...
        error = 0.0  # calcular error
        if error < config.tolerancia:
            break
        if config.verbose:
            print(f'Iteracion: {iter}')

# archivo: main.py
import config
import procesador

config.max_iter = 100
config.tolerancia = 1e-6
config.verbose = True

procesador.procesar()


# Opcion 2: Clases (preferido)
# archivo: solver.py
import numpy as np
from dataclasses import dataclass
from typing import Optional

@dataclass
class ConfiguracionSolver:
    max_iter: int = 100
    tolerancia: float = 1e-6
    verbose: bool = True

@dataclass
class DatosSolver:
    matriz: np.ndarray
    n_filas: int
    n_cols: int

    @classmethod
    def crear(cls, filas: int, cols: int) -> 'DatosSolver':
        return cls(
            matriz=np.zeros((filas, cols)),
            n_filas=filas,
            n_cols=cols
        )

class Solver:
    def __init__(self, config: Optional[ConfiguracionSolver] = None):
        self.config = config or ConfiguracionSolver()
        self.datos: Optional[DatosSolver] = None

    def inicializar(self, filas: int, cols: int):
        self.datos = DatosSolver.crear(filas, cols)

    def procesar(self):
        if self.datos is None:
            raise ValueError("Datos no inicializados")

        for iter in range(1, self.config.max_iter + 1):
            error = self._iterar()

            if error < self.config.tolerancia:
                if self.config.verbose:
                    print(f'Convergencia en iteracion {iter}')
                break

            if self.config.verbose:
                print(f'Iteracion {iter}: error = {error}')

    def _iterar(self) -> float:
        # Logica de iteracion
        return 0.0

# Uso
solver = Solver(ConfiguracionSolver(max_iter=200, verbose=True))
solver.inicializar(100, 100)
solver.procesar()
```

---

## 6. Preservacion Semantica

### 6.1 Verificacion de Equivalencia

```python
# === Framework de verificacion ===
import hashlib
from typing import Any, Callable, List, Tuple
import numpy as np

class VerificadorTraduccion:
    """Verifica que traduccion preserve semantica"""

    def __init__(self):
        self.casos_prueba: List[Tuple[tuple, Any]] = []
        self.tolerancia_float = 1e-10

    def agregar_caso(self, inputs: tuple, expected_output: Any):
        """Agregar caso de prueba"""
        self.casos_prueba.append((inputs, expected_output))

    def verificar(self,
                  func_original: Callable,
                  func_traducida: Callable) -> bool:
        """Verificar equivalencia funcional"""

        for inputs, expected in self.casos_prueba:
            try:
                resultado_original = func_original(*inputs)
                resultado_traducido = func_traducida(*inputs)

                if not self._son_equivalentes(resultado_original,
                                              resultado_traducido):
                    print(f"FALLO: inputs={inputs}")
                    print(f"  Original: {resultado_original}")
                    print(f"  Traducido: {resultado_traducido}")
                    return False

            except Exception as e:
                print(f"EXCEPCION: inputs={inputs}, error={e}")
                return False

        return True

    def _son_equivalentes(self, a: Any, b: Any) -> bool:
        """Comparar resultados con tolerancia"""

        if isinstance(a, (int, bool, str)):
            return a == b

        if isinstance(a, float):
            return abs(a - b) < self.tolerancia_float

        if isinstance(a, np.ndarray):
            return np.allclose(a, b, rtol=self.tolerancia_float)

        if isinstance(a, (list, tuple)):
            if len(a) != len(b):
                return False
            return all(self._son_equivalentes(x, y) for x, y in zip(a, b))

        if isinstance(a, dict):
            if set(a.keys()) != set(b.keys()):
                return False
            return all(self._son_equivalentes(a[k], b[k]) for k in a)

        return a == b

    def generar_reporte(self,
                        func_original: Callable,
                        func_traducida: Callable) -> str:
        """Generar reporte detallado de verificacion"""

        lineas = ["=== REPORTE DE VERIFICACION ===\n"]
        exitos = 0
        fallos = 0

        for i, (inputs, expected) in enumerate(self.casos_prueba):
            try:
                r_orig = func_original(*inputs)
                r_trad = func_traducida(*inputs)
                equiv = self._son_equivalentes(r_orig, r_trad)

                if equiv:
                    exitos += 1
                    lineas.append(f"[OK] Caso {i+1}")
                else:
                    fallos += 1
                    lineas.append(f"[FALLO] Caso {i+1}")
                    lineas.append(f"  Inputs: {inputs}")
                    lineas.append(f"  Original: {r_orig}")
                    lineas.append(f"  Traducido: {r_trad}")

            except Exception as e:
                fallos += 1
                lineas.append(f"[ERROR] Caso {i+1}: {e}")

        lineas.append(f"\nResumen: {exitos} exitos, {fallos} fallos")

        return "\n".join(lineas)


# Ejemplo de uso
def cobol_calcular_interes(principal, tasa, periodos):
    """Simulacion de calculo COBOL"""
    # COMPUTE INTERES = PRINCIPAL * ((1 + TASA) ** PERIODOS - 1)
    interes = principal * ((1 + tasa) ** periodos - 1)
    # COBOL redondea a 2 decimales
    return round(interes, 2)

def python_calcular_interes(principal, tasa, periodos):
    """Traduccion a Python"""
    from decimal import Decimal, ROUND_HALF_UP

    p = Decimal(str(principal))
    t = Decimal(str(tasa))
    n = periodos

    interes = p * ((1 + t) ** n - 1)
    return float(interes.quantize(Decimal('0.01'), rounding=ROUND_HALF_UP))

# Verificar
verificador = VerificadorTraduccion()
verificador.agregar_caso((1000.0, 0.05, 12), 795.86)
verificador.agregar_caso((5000.0, 0.10, 24), 4840.25)
verificador.agregar_caso((100.0, 0.01, 1), 1.0)

print(verificador.generar_reporte(cobol_calcular_interes, python_calcular_interes))
```

### 6.2 Manejo de Diferencias Numericas

```python
# === Precision numerica entre lenguajes ===

class PrecisionManager:
    """Gestiona diferencias de precision entre lenguajes"""

    # Precisiones tipicas por lenguaje
    PRECISIONES = {
        'cobol_pic_9v99': 2,       # PIC 9(7)V99
        'cobol_comp_3': 18,        # COMP-3 packed decimal
        'c_float': 7,              # float IEEE 754
        'c_double': 15,            # double IEEE 754
        'fortran_real4': 7,        # REAL*4
        'fortran_real8': 15,       # REAL*8
        'python_float': 15,        # float (double)
        'python_decimal': 28,      # Decimal default
    }

    @staticmethod
    def cobol_to_python_decimal(valor_str: str, pic: str):
        """
        Convierte valor COBOL a Decimal Python

        pic: formato PICTURE, ej: '9(7)V99', 'S9(5)V9(4)'
        """
        from decimal import Decimal
        import re

        # Parsear PIC para obtener decimales
        match = re.search(r'V9+|\V9\((\d+)\)', pic)
        if match:
            if match.group(1):
                decimales = int(match.group(1))
            else:
                decimales = len(match.group(0)) - 1
        else:
            decimales = 0

        # Valor COBOL no tiene punto decimal explicito
        valor_limpio = valor_str.replace(' ', '0')

        if decimales > 0:
            # Insertar punto decimal
            punto_pos = len(valor_limpio) - decimales
            valor_con_punto = valor_limpio[:punto_pos] + '.' + valor_limpio[punto_pos:]
        else:
            valor_con_punto = valor_limpio

        return Decimal(valor_con_punto)

    @staticmethod
    def fortran_to_python(valor: float, kind: int = 8):
        """
        Ajusta precision Fortran a Python

        kind: 4 para REAL*4, 8 para REAL*8
        """
        import numpy as np

        if kind == 4:
            return float(np.float32(valor))
        else:
            return float(np.float64(valor))

    @staticmethod
    def comparar_con_tolerancia(v1, v2, decimales: int):
        """Compara valores con tolerancia apropiada"""
        from decimal import Decimal

        tolerancia = Decimal(10) ** (-decimales)

        d1 = Decimal(str(v1))
        d2 = Decimal(str(v2))

        return abs(d1 - d2) <= tolerancia
```

---

## 7. Casos Especiales

### 7.1 GOTO y Control de Flujo No Estructurado

```fortran
! === FORTRAN con GOTO ===
      subroutine procesar(x, resultado, error)
      real x, resultado
      integer error

      error = 0
      if (x .lt. 0) goto 100
      if (x .gt. 1000) goto 200

      resultado = sqrt(x)
      goto 999

  100 error = 1
      resultado = 0
      goto 999

  200 error = 2
      resultado = 0

  999 continue
      return
      end
```

```python
# === TRADUCCION - Eliminando GOTO ===

# Opcion 1: Excepciones
class ErrorValorNegativo(Exception):
    pass

class ErrorValorMuyGrande(Exception):
    pass

def procesar_excepciones(x):
    """Usar excepciones para flujo de control"""
    try:
        if x < 0:
            raise ErrorValorNegativo()
        if x > 1000:
            raise ErrorValorMuyGrande()

        return (x ** 0.5, 0)

    except ErrorValorNegativo:
        return (0.0, 1)
    except ErrorValorMuyGrande:
        return (0.0, 2)


# Opcion 2: Returns tempranos
def procesar_returns(x):
    """Usar returns tempranos"""
    if x < 0:
        return (0.0, 1)

    if x > 1000:
        return (0.0, 2)

    return (x ** 0.5, 0)


# Opcion 3: Resultado estructurado
from dataclasses import dataclass
from typing import Optional

@dataclass
class ResultadoProcesar:
    valor: float
    error: int
    mensaje: Optional[str] = None

def procesar_estructurado(x: float) -> ResultadoProcesar:
    """Retornar estructura con resultado y error"""
    if x < 0:
        return ResultadoProcesar(0.0, 1, "Valor negativo")

    if x > 1000:
        return ResultadoProcesar(0.0, 2, "Valor muy grande")

    return ResultadoProcesar(x ** 0.5, 0)
```

### 7.2 Aritmetica de Punteros

```c
// === C - Aritmetica de punteros ===
void procesar_buffer(char* buffer, int size) {
    char* ptr = buffer;
    char* end = buffer + size;

    // Recorrer con punteros
    while (ptr < end) {
        *ptr = toupper(*ptr);
        ptr++;
    }

    // Acceso con offset
    char tercero = *(buffer + 2);

    // Diferencia de punteros
    ptrdiff_t restante = end - ptr;

    // Puntero a mitad del buffer
    char* medio = buffer + size / 2;
}
```

```python
# === TRADUCCION A PYTHON ===

# Opcion 1: Con indices explicitos
def procesar_buffer_indices(buffer: bytearray) -> bytearray:
    """Traduccion literal con indices"""
    size = len(buffer)
    ptr = 0  # indice en lugar de puntero
    end = size

    while ptr < end:
        buffer[ptr] = ord(chr(buffer[ptr]).upper())
        ptr += 1

    tercero = buffer[2]
    restante = end - ptr
    medio = size // 2

    return buffer


# Opcion 2: Pythonica
def procesar_buffer_pythonic(buffer: bytes) -> bytes:
    """Version idiomatica Python"""
    return buffer.upper()


# Opcion 3: Con memoryview para eficiencia
def procesar_buffer_memoryview(buffer: bytearray) -> bytearray:
    """Usando memoryview para acceso eficiente"""
    view = memoryview(buffer)

    for i in range(len(view)):
        if 97 <= view[i] <= 122:  # a-z
            view[i] -= 32  # convertir a mayuscula

    return buffer
```

```rust
// === TRADUCCION A RUST ===
fn procesar_buffer(buffer: &mut [u8]) {
    // Iterador mutable - seguro y eficiente
    for byte in buffer.iter_mut() {
        *byte = byte.to_ascii_uppercase();
    }

    // Acceso por indice
    let tercero = buffer.get(2).copied();

    // Slice de la mitad
    let medio = &buffer[buffer.len() / 2..];
}

// Version con punteros raw (unsafe)
fn procesar_buffer_unsafe(buffer: &mut [u8]) {
    unsafe {
        let ptr = buffer.as_mut_ptr();
        let end = ptr.add(buffer.len());
        let mut current = ptr;

        while current < end {
            *current = (*current).to_ascii_uppercase();
            current = current.add(1);
        }
    }
}
```

---

## 8. Estrategias Automatizadas

### 8.1 Motor de Traduccion

```python
# === Motor de traduccion automatizada ===
from abc import ABC, abstractmethod
from typing import Dict, List, Any
from dataclasses import dataclass
import ast

@dataclass
class NodoAST:
    """Nodo del AST intermedio"""
    tipo: str
    valor: Any
    hijos: List['NodoAST']
    metadatos: Dict[str, Any]

class Traductor(ABC):
    """Clase base para traductores"""

    @abstractmethod
    def traducir(self, nodo: NodoAST) -> str:
        pass

    @abstractmethod
    def get_lenguaje_destino(self) -> str:
        pass

class TraductorCOBOLaPython(Traductor):
    """Traductor de COBOL a Python"""

    def __init__(self):
        self.indentacion = 0
        self.variables = {}
        self.imports_necesarios = set()

    def get_lenguaje_destino(self) -> str:
        return "python"

    def traducir(self, nodo: NodoAST) -> str:
        metodo = getattr(self, f'_traducir_{nodo.tipo}', self._traducir_default)
        return metodo(nodo)

    def _traducir_programa(self, nodo: NodoAST) -> str:
        lineas = []

        # Imports
        if self.imports_necesarios:
            for imp in sorted(self.imports_necesarios):
                lineas.append(imp)
            lineas.append("")

        # Clase principal
        nombre = nodo.metadatos.get('nombre', 'ProgramaCOBOL')
        lineas.append(f"class {nombre}:")

        self.indentacion += 1

        # Constructor con variables
        lineas.append(self._indent("def __init__(self):"))
        self.indentacion += 1
        for hijo in nodo.hijos:
            if hijo.tipo == 'variable':
                lineas.append(self._traducir_variable(hijo))
        self.indentacion -= 1

        # Metodos
        for hijo in nodo.hijos:
            if hijo.tipo == 'parrafo':
                lineas.append("")
                lineas.append(self._traducir_parrafo(hijo))

        self.indentacion -= 1

        return "\n".join(lineas)

    def _traducir_variable(self, nodo: NodoAST) -> str:
        nombre = nodo.valor
        tipo_python = self._pic_a_tipo_python(nodo.metadatos.get('pic', 'X'))
        valor_inicial = nodo.metadatos.get('valor', tipo_python['default'])

        return self._indent(f"self.{nombre} = {valor_inicial}")

    def _traducir_parrafo(self, nodo: NodoAST) -> str:
        lineas = []
        nombre = self._nombre_a_python(nodo.valor)

        lineas.append(self._indent(f"def {nombre}(self):"))
        self.indentacion += 1

        for hijo in nodo.hijos:
            lineas.append(self._indent(self.traducir(hijo)))

        if not nodo.hijos:
            lineas.append(self._indent("pass"))

        self.indentacion -= 1
        return "\n".join(lineas)

    def _traducir_move(self, nodo: NodoAST) -> str:
        origen = nodo.metadatos['origen']
        destino = nodo.metadatos['destino']
        return f"self.{destino} = {origen}"

    def _traducir_compute(self, nodo: NodoAST) -> str:
        destino = nodo.metadatos['destino']
        expresion = self._traducir_expresion(nodo.hijos[0])
        return f"self.{destino} = {expresion}"

    def _traducir_if(self, nodo: NodoAST) -> str:
        condicion = self._traducir_condicion(nodo.metadatos['condicion'])
        lineas = [f"if {condicion}:"]

        self.indentacion += 1
        for hijo in nodo.hijos:
            if hijo.tipo != 'else':
                lineas.append(self._indent(self.traducir(hijo)))
        self.indentacion -= 1

        # Else
        else_nodos = [h for h in nodo.hijos if h.tipo == 'else']
        if else_nodos:
            lineas.append("else:")
            self.indentacion += 1
            for hijo in else_nodos[0].hijos:
                lineas.append(self._indent(self.traducir(hijo)))
            self.indentacion -= 1

        return "\n".join(lineas)

    def _traducir_perform(self, nodo: NodoAST) -> str:
        parrafo = self._nombre_a_python(nodo.valor)

        if 'until' in nodo.metadatos:
            condicion = self._traducir_condicion(nodo.metadatos['until'])
            return f"while not ({condicion}):\n" + \
                   self._indent(f"    self.{parrafo}()")
        elif 'times' in nodo.metadatos:
            veces = nodo.metadatos['times']
            return f"for _ in range({veces}):\n" + \
                   self._indent(f"    self.{parrafo}()")
        else:
            return f"self.{parrafo}()"

    def _traducir_display(self, nodo: NodoAST) -> str:
        valor = nodo.valor
        if valor.startswith('"') or valor.startswith("'"):
            return f"print({valor})"
        else:
            return f"print(self.{valor})"

    def _pic_a_tipo_python(self, pic: str) -> Dict[str, Any]:
        """Convierte PICTURE COBOL a tipo Python"""
        import re

        if re.match(r'9+|9\(\d+\)', pic):
            return {'tipo': 'int', 'default': '0'}
        elif re.match(r'S?9.*V9|S?9.*V\(', pic):
            self.imports_necesarios.add("from decimal import Decimal")
            return {'tipo': 'Decimal', 'default': "Decimal('0')"}
        elif re.match(r'X+|X\(\d+\)', pic):
            return {'tipo': 'str', 'default': "''"}
        else:
            return {'tipo': 'str', 'default': "''"}

    def _nombre_a_python(self, nombre: str) -> str:
        """Convierte nombre COBOL a Python"""
        return nombre.lower().replace('-', '_')

    def _traducir_condicion(self, cond: str) -> str:
        """Traduce condicion COBOL a Python"""
        cond = cond.replace(' = ', ' == ')
        cond = cond.replace(' AND ', ' and ')
        cond = cond.replace(' OR ', ' or ')
        cond = cond.replace(' NOT ', ' not ')
        cond = cond.replace(' GREATER THAN ', ' > ')
        cond = cond.replace(' LESS THAN ', ' < ')
        return cond

    def _traducir_expresion(self, nodo: NodoAST) -> str:
        """Traduce expresion aritmetica"""
        if nodo.tipo == 'literal':
            return str(nodo.valor)
        elif nodo.tipo == 'variable':
            return f"self.{nodo.valor}"
        elif nodo.tipo == 'operacion':
            op = nodo.metadatos['operador']
            izq = self._traducir_expresion(nodo.hijos[0])
            der = self._traducir_expresion(nodo.hijos[1])
            return f"({izq} {op} {der})"
        return str(nodo.valor)

    def _traducir_default(self, nodo: NodoAST) -> str:
        return f"# TODO: traducir {nodo.tipo}"

    def _indent(self, texto: str) -> str:
        return "    " * self.indentacion + texto


# Uso del motor
def ejemplo_traduccion():
    # AST de ejemplo (normalmente generado por parser)
    ast = NodoAST(
        tipo='programa',
        valor=None,
        metadatos={'nombre': 'CalculadoraInteres'},
        hijos=[
            NodoAST('variable', 'principal', [], {'pic': '9(7)V99'}),
            NodoAST('variable', 'tasa', [], {'pic': '9V9(4)'}),
            NodoAST('variable', 'resultado', [], {'pic': '9(9)V99'}),
            NodoAST('parrafo', 'CALCULAR-INTERES', [
                NodoAST('compute', None,
                    [NodoAST('operacion', None,
                        [NodoAST('variable', 'principal', [], {}),
                         NodoAST('variable', 'tasa', [], {})],
                        {'operador': '*'})],
                    {'destino': 'resultado'}),
                NodoAST('display', 'resultado', [], {})
            ], {})
        ]
    )

    traductor = TraductorCOBOLaPython()
    codigo_python = traductor.traducir(ast)
    print(codigo_python)
```

---

## Referencias

1. Fowler, Martin - "Refactoring: Improving the Design of Existing Code"
2. Feathers, Michael - "Working Effectively with Legacy Code"
3. COBOL Standards - ISO/IEC 1989
4. Fortran Standards - ISO/IEC 1539
5. C Standards - ISO/IEC 9899
6. Rust Book - The Rust Programming Language
7. Go Specification - golang.org/ref/spec

---

*ARCHAEON - Guardian del Codigo Ancestral*
*Traduccion - Construyendo Puentes Entre Epocas*

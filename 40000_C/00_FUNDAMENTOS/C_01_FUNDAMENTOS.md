# C_01: Fundamentos del Lenguaje C

> "C es Assembly portátil. Te da el poder del hardware con la abstracción del software."

---

## Historia

```
1969-1973: Dennis Ritchie desarrolla C en Bell Labs
1972: C usado para reescribir UNIX
1978: "The C Programming Language" (K&R)
1989: ANSI C (C89/C90)
1999: C99 (inline, _Bool, // comments)
2011: C11 (threads, generic)
2018: C17/C18 (correcciones)
2023: C23 (mejoras modernas)
```

### Dónde Se Usa C Hoy

| Dominio | Ejemplos |
|---------|----------|
| Sistemas Operativos | Linux, Windows kernel, macOS |
| Embedded | Arduino, ESP32, STM32 |
| Compiladores | GCC, Clang, LLVM |
| Databases | PostgreSQL, SQLite, MySQL |
| Lenguajes | Python (CPython), Ruby, PHP |
| Networking | Nginx, Apache, cURL |

---

## Estructura de un Programa C

```c
/* Directivas del preprocesador */
#include <stdio.h>      // Biblioteca estándar I/O
#include <stdlib.h>     // Utilidades generales
#include <string.h>     // Manejo de strings
#include "miheader.h"   // Header propio

/* Macros y constantes */
#define MAX_SIZE 100
#define SQUARE(x) ((x) * (x))

/* Declaraciones globales */
int global_var;
static int file_scope_var;

/* Prototipos de funciones */
int suma(int a, int b);
void procesar(char *datos);

/* Función principal */
int main(int argc, char *argv[]) {
    /* Variables locales */
    int resultado;

    /* Código */
    resultado = suma(5, 3);
    printf("Resultado: %d\n", resultado);

    return 0;  /* 0 = éxito */
}

/* Definiciones de funciones */
int suma(int a, int b) {
    return a + b;
}
```

### Compilación

```bash
# Compilación básica
gcc programa.c -o programa

# Con warnings y debug
gcc -Wall -Wextra -g programa.c -o programa

# Optimizado para producción
gcc -O2 -DNDEBUG programa.c -o programa

# Múltiples archivos
gcc -c modulo1.c -o modulo1.o
gcc -c modulo2.c -o modulo2.o
gcc modulo1.o modulo2.o -o programa

# Con bibliotecas
gcc programa.c -lm -lpthread -o programa
```

---

## Tipos de Datos

### Tipos Fundamentales

```c
/* Enteros */
char c;              /* 1 byte, -128 a 127 */
unsigned char uc;    /* 1 byte, 0 a 255 */
short s;             /* 2 bytes, -32768 a 32767 */
unsigned short us;   /* 2 bytes, 0 a 65535 */
int i;               /* 4 bytes típico */
unsigned int ui;     /* 4 bytes típico */
long l;              /* 4-8 bytes */
long long ll;        /* 8 bytes */

/* Punto flotante */
float f;             /* 4 bytes, ~7 dígitos precisión */
double d;            /* 8 bytes, ~15 dígitos precisión */
long double ld;      /* 12-16 bytes */

/* Tipos de C99 con tamaño exacto */
#include <stdint.h>
int8_t   i8;         /* Exactamente 8 bits */
int16_t  i16;        /* Exactamente 16 bits */
int32_t  i32;        /* Exactamente 32 bits */
int64_t  i64;        /* Exactamente 64 bits */
uint8_t  u8;         /* Sin signo */
uint16_t u16;
uint32_t u32;
uint64_t u64;

/* Tamaño de puntero */
size_t size;         /* Unsigned, tamaño de objeto */
ptrdiff_t diff;      /* Signed, diferencia de punteros */
intptr_t ptr_int;    /* Entero del tamaño de puntero */
```

### Modificadores

```c
/* const - no modificable */
const int MAX = 100;
const char *ptr;     /* Puntero a const char */
char *const ptr2;    /* Const puntero a char */

/* volatile - puede cambiar externamente */
volatile int *hardware_reg;

/* static - persistencia/scope limitado */
static int counter = 0;    /* En función: persiste entre llamadas */
static void helper(void);  /* En archivo: solo visible aquí */

/* extern - declaración sin definición */
extern int global_from_other_file;

/* register - sugerencia al compilador */
register int i;  /* Generalmente ignorado hoy */
```

### Sizeof

```c
printf("char: %zu bytes\n", sizeof(char));       /* 1 */
printf("int: %zu bytes\n", sizeof(int));         /* 4 típico */
printf("long: %zu bytes\n", sizeof(long));       /* 4 u 8 */
printf("pointer: %zu bytes\n", sizeof(void*));   /* 4 u 8 */
printf("array: %zu bytes\n", sizeof(int[10]));   /* 40 típico */
```

---

## Variables y Constantes

### Declaración e Inicialización

```c
/* Declaración */
int a;               /* Valor indefinido */
int b = 0;           /* Inicializado */
int c = 5, d = 10;   /* Múltiples */

/* Constantes literales */
int decimal = 42;
int octal = 052;        /* Prefijo 0 */
int hex = 0x2A;         /* Prefijo 0x */
int binary = 0b101010;  /* C23: prefijo 0b */

long big = 1000000L;
unsigned u = 42U;
long long huge = 9223372036854775807LL;

float f = 3.14f;
double d = 3.14159265358979;
double sci = 6.022e23;

char c = 'A';
char newline = '\n';
char tab = '\t';
char null = '\0';
char hex_char = '\x41';  /* 'A' */

/* Strings */
char str[] = "Hello";          /* Array de 6 chars */
char *ptr = "World";           /* Puntero a string literal */
```

### Enumeraciones

```c
/* Enum básico */
enum Color { RED, GREEN, BLUE };  /* 0, 1, 2 */

/* Con valores específicos */
enum Status {
    SUCCESS = 0,
    ERROR_FILE = -1,
    ERROR_MEMORY = -2,
    ERROR_NETWORK = -3
};

/* Uso */
enum Color c = GREEN;
if (c == RED) {
    /* ... */
}
```

---

## Operadores

### Aritméticos

```c
int a = 10, b = 3;

int suma = a + b;       /* 13 */
int resta = a - b;      /* 7 */
int mult = a * b;       /* 30 */
int div = a / b;        /* 3 (división entera) */
int mod = a % b;        /* 1 (módulo) */

/* Incremento/Decremento */
a++;        /* Post-incremento */
++a;        /* Pre-incremento */
b--;        /* Post-decremento */
--b;        /* Pre-decremento */

/* Asignación compuesta */
a += 5;     /* a = a + 5 */
a -= 3;     /* a = a - 3 */
a *= 2;     /* a = a * 2 */
a /= 4;     /* a = a / 4 */
a %= 3;     /* a = a % 3 */
```

### Relacionales y Lógicos

```c
/* Relacionales (devuelven 0 o 1) */
a == b      /* Igual */
a != b      /* Diferente */
a > b       /* Mayor */
a >= b      /* Mayor o igual */
a < b       /* Menor */
a <= b      /* Menor o igual */

/* Lógicos */
!a          /* NOT */
a && b      /* AND (short-circuit) */
a || b      /* OR (short-circuit) */

/* Ejemplo short-circuit */
if (ptr != NULL && ptr->value > 0) {
    /* ptr->value solo se evalúa si ptr != NULL */
}
```

### Bitwise

```c
unsigned int a = 0b11001100;
unsigned int b = 0b10101010;

a & b       /* AND: 0b10001000 */
a | b       /* OR:  0b11101110 */
a ^ b       /* XOR: 0b01100110 */
~a          /* NOT: 0b00110011 (invierte bits) */

a << 2      /* Shift left: 0b110011000 (mult por 4) */
a >> 2      /* Shift right: 0b00110011 (div por 4) */

/* Operaciones comunes */
/* Set bit n */
value |= (1 << n);

/* Clear bit n */
value &= ~(1 << n);

/* Toggle bit n */
value ^= (1 << n);

/* Check bit n */
if (value & (1 << n)) {
    /* Bit n está set */
}

/* Extraer bits [start:end] */
mask = ((1 << (end - start + 1)) - 1) << start;
extracted = (value & mask) >> start;
```

### Otros Operadores

```c
/* Ternario */
int max = (a > b) ? a : b;

/* Comma */
for (i = 0, j = 10; i < j; i++, j--) {
    /* ... */
}

/* Sizeof */
size_t size = sizeof(int);
size_t arr_size = sizeof(array) / sizeof(array[0]);

/* Cast */
double d = 3.14;
int i = (int)d;  /* i = 3 */

/* Address-of y Dereference */
int x = 42;
int *ptr = &x;    /* ptr contiene dirección de x */
int y = *ptr;     /* y = 42 (valor en la dirección) */

/* Member access */
struct Point p;
p.x = 10;         /* Acceso directo */
struct Point *pp = &p;
pp->x = 10;       /* Acceso via puntero ((*pp).x) */
```

### Precedencia de Operadores

```
Más alta:
1.  () [] -> .                    Postfix
2.  ! ~ ++ -- + - * & sizeof      Unary (derecha a izq)
3.  (type)                        Cast
4.  * / %                         Multiplicativos
5.  + -                           Aditivos
6.  << >>                         Shift
7.  < <= > >=                     Relacional
8.  == !=                         Igualdad
9.  &                             Bitwise AND
10. ^                             Bitwise XOR
11. |                             Bitwise OR
12. &&                            Logical AND
13. ||                            Logical OR
14. ?:                            Ternario
15. = += -= *= /= etc.            Asignación (derecha a izq)
16. ,                             Comma
Más baja
```

---

## Control de Flujo

### if-else

```c
if (condicion) {
    /* ... */
}

if (condicion) {
    /* ... */
} else {
    /* ... */
}

if (condicion1) {
    /* ... */
} else if (condicion2) {
    /* ... */
} else if (condicion3) {
    /* ... */
} else {
    /* ... */
}

/* Operador ternario para asignación simple */
int resultado = (x > 0) ? x : -x;  /* abs(x) */
```

### switch

```c
switch (opcion) {
    case 1:
        printf("Opción 1\n");
        break;
    case 2:
        printf("Opción 2\n");
        break;
    case 3:
    case 4:
        printf("Opción 3 o 4\n");  /* Fall-through intencional */
        break;
    default:
        printf("Opción no válida\n");
        break;
}

/* Switch con enum */
enum Estado { INICIO, PROCESO, FIN };
enum Estado estado = INICIO;

switch (estado) {
    case INICIO:
        iniciar();
        break;
    case PROCESO:
        procesar();
        break;
    case FIN:
        terminar();
        break;
}
```

### while

```c
/* while - evalúa antes */
while (condicion) {
    /* ... */
}

/* Ejemplo */
int i = 0;
while (i < 10) {
    printf("%d ", i);
    i++;
}

/* Loop infinito */
while (1) {
    /* ... */
    if (salir) break;
}
```

### do-while

```c
/* do-while - evalúa después (ejecuta al menos una vez) */
do {
    /* ... */
} while (condicion);

/* Ejemplo: menú */
int opcion;
do {
    printf("1. Opción 1\n");
    printf("2. Opción 2\n");
    printf("0. Salir\n");
    scanf("%d", &opcion);
    procesar_opcion(opcion);
} while (opcion != 0);
```

### for

```c
/* for clásico */
for (int i = 0; i < 10; i++) {
    printf("%d ", i);
}

/* Múltiples variables */
for (int i = 0, j = 10; i < j; i++, j--) {
    printf("i=%d, j=%d\n", i, j);
}

/* Loop infinito */
for (;;) {
    /* ... */
}

/* Recorrer array */
int arr[] = {1, 2, 3, 4, 5};
size_t len = sizeof(arr) / sizeof(arr[0]);
for (size_t i = 0; i < len; i++) {
    printf("%d ", arr[i]);
}

/* Countdown */
for (int i = 10; i >= 0; i--) {
    printf("%d ", i);
}
```

### break, continue, goto

```c
/* break - sale del loop/switch */
for (int i = 0; i < 100; i++) {
    if (i == 50) break;  /* Sale cuando i == 50 */
}

/* continue - salta a siguiente iteración */
for (int i = 0; i < 100; i++) {
    if (i % 2 == 0) continue;  /* Salta pares */
    printf("%d ", i);  /* Solo imprime impares */
}

/* goto - salto incondicional (usar con cuidado) */
/* Único uso aceptable: cleanup en errores */
int procesar_archivo(const char *nombre) {
    FILE *f = NULL;
    char *buffer = NULL;
    int resultado = -1;

    f = fopen(nombre, "r");
    if (!f) goto cleanup;

    buffer = malloc(1024);
    if (!buffer) goto cleanup;

    /* Procesar... */
    resultado = 0;

cleanup:
    if (buffer) free(buffer);
    if (f) fclose(f);
    return resultado;
}
```

---

## Funciones

### Declaración y Definición

```c
/* Prototipo (declaración) */
int suma(int a, int b);
void imprimir(const char *mensaje);
double promedio(int *array, size_t n);

/* Definición */
int suma(int a, int b) {
    return a + b;
}

void imprimir(const char *mensaje) {
    printf("%s\n", mensaje);
    /* void no necesita return */
}

double promedio(int *array, size_t n) {
    if (n == 0) return 0.0;

    double sum = 0;
    for (size_t i = 0; i < n; i++) {
        sum += array[i];
    }
    return sum / n;
}
```

### Paso de Parámetros

```c
/* Por valor - copia */
void duplicar_valor(int x) {
    x = x * 2;  /* No afecta al original */
}

/* Por referencia (puntero) - modifica original */
void duplicar_ref(int *x) {
    *x = *x * 2;  /* Modifica el original */
}

/* Uso */
int num = 5;
duplicar_valor(num);   /* num sigue siendo 5 */
duplicar_ref(&num);    /* num ahora es 10 */

/* Arrays siempre se pasan por referencia */
void llenar_array(int arr[], size_t n) {
    for (size_t i = 0; i < n; i++) {
        arr[i] = i;  /* Modifica array original */
    }
}
```

### Funciones con Número Variable de Argumentos

```c
#include <stdarg.h>

/* Función variádica */
double promedio_var(int count, ...) {
    va_list args;
    va_start(args, count);

    double sum = 0;
    for (int i = 0; i < count; i++) {
        sum += va_arg(args, int);
    }

    va_end(args);
    return sum / count;
}

/* Uso */
double p = promedio_var(4, 10, 20, 30, 40);  /* 25.0 */
```

### Funciones Inline

```c
/* Sugerencia al compilador para expandir en línea */
static inline int max(int a, int b) {
    return (a > b) ? a : b;
}

/* El compilador puede ignorar inline */
/* Mejor rendimiento para funciones pequeñas */
```

### Punteros a Funciones

```c
/* Declaración */
int (*operacion)(int, int);

/* Funciones */
int suma(int a, int b) { return a + b; }
int resta(int a, int b) { return a - b; }
int mult(int a, int b) { return a * b; }

/* Asignación y uso */
operacion = suma;
int resultado = operacion(5, 3);  /* 8 */

operacion = resta;
resultado = operacion(5, 3);  /* 2 */

/* Array de punteros a funciones */
int (*ops[])(int, int) = {suma, resta, mult};
resultado = ops[0](10, 5);  /* 15 (suma) */
resultado = ops[1](10, 5);  /* 5 (resta) */
resultado = ops[2](10, 5);  /* 50 (mult) */

/* Typedef para legibilidad */
typedef int (*BinaryOp)(int, int);
BinaryOp mi_op = suma;

/* Callback */
void procesar_array(int *arr, size_t n, int (*func)(int)) {
    for (size_t i = 0; i < n; i++) {
        arr[i] = func(arr[i]);
    }
}

int duplicar(int x) { return x * 2; }
procesar_array(array, len, duplicar);
```

---

## Arrays

### Arrays Unidimensionales

```c
/* Declaración */
int arr[10];                /* 10 elementos, sin inicializar */
int arr2[5] = {1, 2, 3, 4, 5};  /* Inicializado */
int arr3[] = {1, 2, 3};     /* Tamaño inferido: 3 */
int arr4[10] = {0};         /* Todos a 0 */
int arr5[10] = {1, 2};      /* 1, 2, 0, 0, 0, 0, 0, 0, 0, 0 */

/* Acceso */
arr[0] = 100;               /* Primer elemento */
arr[9] = 999;               /* Último elemento */
int x = arr[5];             /* Leer elemento */

/* Tamaño */
size_t len = sizeof(arr) / sizeof(arr[0]);

/* Arrays y punteros */
int *ptr = arr;             /* arr decae a puntero */
int *ptr2 = &arr[0];        /* Equivalente */
ptr[0] = 1;                 /* Acceso como array */
*(ptr + 1) = 2;             /* Aritmética de punteros */
```

### Arrays Multidimensionales

```c
/* Matriz 2D */
int matriz[3][4];           /* 3 filas, 4 columnas */
int matriz2[2][3] = {
    {1, 2, 3},
    {4, 5, 6}
};

/* Acceso */
matriz[0][0] = 1;           /* Fila 0, Columna 0 */
matriz[2][3] = 12;          /* Fila 2, Columna 3 */

/* En memoria: row-major order */
/* matriz[0][0], matriz[0][1], matriz[0][2], matriz[0][3],
   matriz[1][0], matriz[1][1], ... */

/* Matriz 3D */
int cubo[2][3][4];

/* Recorrer matriz 2D */
for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 4; j++) {
        printf("%d ", matriz[i][j]);
    }
    printf("\n");
}
```

### Arrays como Parámetros

```c
/* El tamaño de la primera dimensión se pierde */
void procesar_1d(int arr[], size_t n) {
    for (size_t i = 0; i < n; i++) {
        arr[i] *= 2;
    }
}

/* Para 2D, necesita tamaño de columnas */
void procesar_2d(int arr[][4], size_t filas) {
    for (size_t i = 0; i < filas; i++) {
        for (size_t j = 0; j < 4; j++) {
            arr[i][j] *= 2;
        }
    }
}

/* Alternativa con punteros (C99 VLA) */
void procesar_vla(size_t filas, size_t cols, int arr[filas][cols]) {
    for (size_t i = 0; i < filas; i++) {
        for (size_t j = 0; j < cols; j++) {
            arr[i][j] *= 2;
        }
    }
}
```

---

## Strings

### Strings en C

```c
/* Strings son arrays de char terminados en '\0' */
char str1[] = "Hello";      /* {'H','e','l','l','o','\0'} */
char str2[10] = "World";    /* Con espacio extra */
char str3[6] = {'H','i','\0'};  /* Manual */

/* String literal (read-only) */
const char *ptr = "Literal";  /* No modificar! */

/* Longitud */
#include <string.h>
size_t len = strlen(str1);  /* 5 (no cuenta '\0') */

/* PELIGRO: Buffer overflow */
char small[5];
strcpy(small, "Esto es muy largo");  /* ¡Overflow! */
```

### Funciones de string.h

```c
#include <string.h>

char dest[100];
const char *src = "Hello";

/* Longitud */
size_t len = strlen(src);       /* 5 */

/* Copiar */
strcpy(dest, src);              /* Copia todo */
strncpy(dest, src, 3);          /* Copia hasta n chars */
/* Nota: strncpy no garantiza '\0' si src >= n */

/* Concatenar */
strcat(dest, " World");         /* dest = "Hello World" */
strncat(dest, "!!!", 2);        /* Agrega hasta n chars */

/* Comparar */
int cmp = strcmp("abc", "abd"); /* <0 (c < d) */
cmp = strcmp("abc", "abc");     /* 0 (iguales) */
cmp = strcmp("abd", "abc");     /* >0 (d > c) */
cmp = strncmp("abc", "abd", 2); /* 0 (primeros 2 iguales) */

/* Buscar */
char *pos = strchr(dest, 'o');  /* Primera 'o' */
pos = strrchr(dest, 'o');       /* Última 'o' */
pos = strstr(dest, "World");    /* Substring */

/* Tokenizar */
char str[] = "uno,dos,tres";
char *token = strtok(str, ",");
while (token != NULL) {
    printf("%s\n", token);
    token = strtok(NULL, ",");  /* Siguiente token */
}

/* Memoria */
memset(dest, 0, sizeof(dest));  /* Llenar con 0 */
memcpy(dest, src, len + 1);     /* Copiar bytes */
memmove(dest, src, len + 1);    /* Copiar (overlap safe) */
int eq = memcmp(a, b, n);       /* Comparar bytes */
```

### Funciones Seguras (C11)

```c
/* Versiones con límite de buffer */
errno_t err;

err = strcpy_s(dest, sizeof(dest), src);
err = strcat_s(dest, sizeof(dest), " World");
err = strncpy_s(dest, sizeof(dest), src, 5);

/* snprintf siempre termina en '\0' */
int written = snprintf(dest, sizeof(dest),
                       "Valor: %d", 42);
```

---

## Entrada/Salida

### printf

```c
#include <stdio.h>

/* Formato básico */
printf("Hola Mundo\n");

/* Especificadores de formato */
int i = 42;
printf("Entero: %d\n", i);           /* Decimal */
printf("Unsigned: %u\n", (unsigned)i);
printf("Hex: %x, %X\n", i, i);       /* 2a, 2A */
printf("Octal: %o\n", i);            /* 52 */

double d = 3.14159;
printf("Float: %f\n", d);            /* 3.141590 */
printf("Científico: %e\n", d);       /* 3.141590e+00 */
printf("Auto: %g\n", d);             /* 3.14159 */
printf("Precisión: %.2f\n", d);      /* 3.14 */

char c = 'A';
printf("Char: %c\n", c);

char *s = "Hello";
printf("String: %s\n", s);
printf("Parcial: %.3s\n", s);        /* Hel */

void *ptr = &i;
printf("Pointer: %p\n", ptr);

/* Ancho y alineación */
printf("[%10d]\n", 42);              /* [        42] */
printf("[%-10d]\n", 42);             /* [42        ] */
printf("[%010d]\n", 42);             /* [0000000042] */
printf("[%+d]\n", 42);               /* [+42] */

/* Tamaño */
printf("size_t: %zu\n", sizeof(int));
printf("long: %ld\n", 1000000L);
printf("long long: %lld\n", 1LL << 40);
```

### scanf

```c
int i;
double d;
char c;
char str[100];

/* Leer tipos básicos */
scanf("%d", &i);        /* Entero */
scanf("%lf", &d);       /* Double (nota: lf, no f) */
scanf("%c", &c);        /* Char (incluye whitespace!) */
scanf(" %c", &c);       /* Char (ignora whitespace previo) */
scanf("%s", str);       /* String (hasta whitespace) */
scanf("%99s", str);     /* Con límite (importante!) */

/* Leer línea completa */
fgets(str, sizeof(str), stdin);

/* Validar retorno */
if (scanf("%d", &i) != 1) {
    printf("Error de entrada\n");
}

/* Limpiar buffer */
int ch;
while ((ch = getchar()) != '\n' && ch != EOF);
```

### Archivos

```c
#include <stdio.h>

FILE *f;

/* Abrir archivo */
f = fopen("archivo.txt", "r");   /* Lectura */
f = fopen("archivo.txt", "w");   /* Escritura (trunca) */
f = fopen("archivo.txt", "a");   /* Append */
f = fopen("archivo.bin", "rb");  /* Binario lectura */
f = fopen("archivo.bin", "wb");  /* Binario escritura */
f = fopen("archivo.txt", "r+");  /* Lectura y escritura */

if (f == NULL) {
    perror("Error abriendo archivo");
    return 1;
}

/* Leer/Escribir texto */
char buffer[256];
fgets(buffer, sizeof(buffer), f);   /* Leer línea */
fputs("Línea de texto\n", f);       /* Escribir línea */
fprintf(f, "Valor: %d\n", 42);      /* Printf a archivo */
fscanf(f, "%d", &valor);            /* Scanf de archivo */

/* Leer/Escribir binario */
size_t leidos = fread(buffer, 1, 100, f);
size_t escritos = fwrite(buffer, 1, 100, f);

/* Posición */
long pos = ftell(f);                /* Posición actual */
fseek(f, 0, SEEK_SET);              /* Inicio */
fseek(f, 0, SEEK_END);              /* Final */
fseek(f, -10, SEEK_CUR);            /* 10 bytes atrás */
rewind(f);                          /* Al inicio */

/* Estado */
if (feof(f)) { /* Fin de archivo */ }
if (ferror(f)) { /* Error */ }
clearerr(f);                        /* Limpiar errores */

/* Cerrar */
fclose(f);

/* Ejemplo completo: copiar archivo */
int copiar_archivo(const char *src, const char *dst) {
    FILE *in = fopen(src, "rb");
    if (!in) return -1;

    FILE *out = fopen(dst, "wb");
    if (!out) {
        fclose(in);
        return -1;
    }

    char buffer[4096];
    size_t bytes;

    while ((bytes = fread(buffer, 1, sizeof(buffer), in)) > 0) {
        if (fwrite(buffer, 1, bytes, out) != bytes) {
            fclose(in);
            fclose(out);
            return -1;
        }
    }

    fclose(in);
    fclose(out);
    return 0;
}
```

---

## Preprocesador

### Directivas

```c
/* Include */
#include <stdio.h>      /* Sistema */
#include "miheader.h"   /* Local */

/* Define constantes */
#define PI 3.14159
#define MAX_SIZE 1024
#define DEBUG

/* Define macros */
#define SQUARE(x) ((x) * (x))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define PRINT_VAR(var) printf(#var " = %d\n", var)
#define CONCAT(a, b) a##b

/* Uso de # y ## */
PRINT_VAR(contador);    /* printf("contador" " = %d\n", contador); */
int CONCAT(var, 1) = 5; /* int var1 = 5; */

/* Compilación condicional */
#ifdef DEBUG
    printf("Debug: valor = %d\n", x);
#endif

#ifndef HEADER_H
#define HEADER_H
/* Contenido del header */
#endif

#if VERSION >= 2
    /* Código para versión 2+ */
#elif VERSION == 1
    /* Código para versión 1 */
#else
    /* Código default */
#endif

/* Macros predefinidas */
printf("Archivo: %s\n", __FILE__);
printf("Línea: %d\n", __LINE__);
printf("Fecha: %s\n", __DATE__);
printf("Hora: %s\n", __TIME__);
printf("Función: %s\n", __func__);

/* Undef */
#undef DEBUG

/* Error y Warning */
#if BUFFER_SIZE < 64
    #error "BUFFER_SIZE debe ser al menos 64"
#endif

#warning "Esta función está deprecada"

/* Pragma */
#pragma once            /* Include guard moderno */
#pragma pack(1)         /* Empaquetar structs */
#pragma pack()          /* Restaurar */
```

### Macros Variádicas

```c
/* C99: macros con argumentos variables */
#define DEBUG_LOG(fmt, ...) \
    fprintf(stderr, "[DEBUG] " fmt "\n", ##__VA_ARGS__)

DEBUG_LOG("Iniciando");
DEBUG_LOG("Valor: %d", 42);
DEBUG_LOG("x=%d, y=%d", x, y);
```

---

## Próximos Documentos

- **C_02_MEMORIA.md**: Punteros, memoria dinámica, stack vs heap
- **C_03_ESTRUCTURAS.md**: struct, union, typedef, bit fields
- **C_04_ARCHIVOS.md**: I/O avanzado, streams, buffering
- **C_05_AVANZADO.md**: Técnicas avanzadas, optimización

---

*"C te da suficiente cuerda para ahorcarte, y suficiente poder para construir cualquier cosa."*


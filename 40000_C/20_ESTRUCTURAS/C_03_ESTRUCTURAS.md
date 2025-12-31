# C_03: Estructuras, Unions y Tipos Personalizados

> "Las estructuras son la forma en que C organiza la complejidad. Son el puente entre datos primitivos y abstracciones."

---

## Estructuras (struct)

### Definición Básica

```c
/* Definición de estructura */
struct Punto {
    int x;
    int y;
};

/* Declaración de variable */
struct Punto p1;

/* Inicialización */
struct Punto p2 = {10, 20};
struct Punto p3 = {.x = 10, .y = 20};  /* Designated initializers (C99) */
struct Punto p4 = {.y = 20};           /* x será 0 */

/* Acceso a miembros */
p1.x = 5;
p1.y = 10;
printf("(%d, %d)\n", p1.x, p1.y);

/* Asignación entre estructuras */
struct Punto p5 = p2;  /* Copia todos los miembros */
```

### Estructuras Anidadas

```c
struct Fecha {
    int dia;
    int mes;
    int anio;
};

struct Persona {
    char nombre[50];
    int edad;
    struct Fecha nacimiento;    /* Estructura anidada */
    struct {                    /* Estructura anónima */
        char calle[100];
        char ciudad[50];
        int codigo_postal;
    } direccion;
};

/* Uso */
struct Persona p;
strcpy(p.nombre, "Juan");
p.edad = 30;
p.nacimiento.dia = 15;
p.nacimiento.mes = 6;
p.nacimiento.anio = 1990;
strcpy(p.direccion.calle, "Calle Principal 123");
strcpy(p.direccion.ciudad, "Madrid");
p.direccion.codigo_postal = 28001;
```

### Estructuras y Punteros

```c
struct Punto {
    int x;
    int y;
};

struct Punto p = {10, 20};
struct Punto *ptr = &p;

/* Acceso mediante puntero */
(*ptr).x = 30;        /* Forma explícita */
ptr->x = 30;          /* Operador flecha (equivalente) */
ptr->y = 40;

printf("(%d, %d)\n", ptr->x, ptr->y);

/* Estructuras dinámicas */
struct Punto *punto_dinamico = malloc(sizeof(struct Punto));
if (punto_dinamico != NULL) {
    punto_dinamico->x = 100;
    punto_dinamico->y = 200;
    /* usar... */
    free(punto_dinamico);
}

/* Array de estructuras */
struct Punto puntos[100];
puntos[0].x = 1;
puntos[0].y = 2;

/* Array dinámico de estructuras */
struct Punto *arr = malloc(100 * sizeof(struct Punto));
arr[0].x = 1;
arr[0].y = 2;
/* o */
(arr + 0)->x = 1;
free(arr);
```

### Estructuras como Parámetros

```c
/* Por valor - copia la estructura completa */
void imprimir_punto(struct Punto p) {
    printf("(%d, %d)\n", p.x, p.y);
}

/* Por referencia - más eficiente para estructuras grandes */
void mover_punto(struct Punto *p, int dx, int dy) {
    p->x += dx;
    p->y += dy;
}

/* Por referencia constante - lectura sin copia */
void mostrar_persona(const struct Persona *p) {
    printf("Nombre: %s, Edad: %d\n", p->nombre, p->edad);
    /* p->edad = 50;  ERROR: const */
}

/* Retornar estructura */
struct Punto crear_punto(int x, int y) {
    struct Punto p = {x, y};
    return p;  /* Devuelve copia */
}

/* Retornar puntero a estructura dinámica */
struct Punto *crear_punto_dinamico(int x, int y) {
    struct Punto *p = malloc(sizeof(struct Punto));
    if (p) {
        p->x = x;
        p->y = y;
    }
    return p;  /* Llamador debe liberar */
}
```

### Estructuras Autoreferenciadas

```c
/* Nodo de lista enlazada */
struct Nodo {
    int dato;
    struct Nodo *siguiente;  /* Puntero a sí mismo */
};

/* Árbol binario */
struct NodoArbol {
    int valor;
    struct NodoArbol *izquierdo;
    struct NodoArbol *derecho;
};

/* Grafo (lista de adyacencia) */
struct Vertice {
    int id;
    struct Arista *aristas;
};

struct Arista {
    struct Vertice *destino;
    int peso;
    struct Arista *siguiente;
};
```

---

## Typedef

### Uso Básico

```c
/* Alias para tipos primitivos */
typedef unsigned char byte;
typedef unsigned int uint;
typedef long long int64;

byte b = 255;
uint u = 1000;
int64 big = 9223372036854775807LL;

/* Alias para punteros */
typedef char* string;
typedef int* int_ptr;

string nombre = "Hola";
int_ptr p = malloc(sizeof(int));
```

### Typedef con Estructuras

```c
/* Método 1: Separado */
struct Punto {
    int x;
    int y;
};
typedef struct Punto Punto;

/* Método 2: Combinado */
typedef struct Punto {
    int x;
    int y;
} Punto;

/* Método 3: Sin nombre de struct */
typedef struct {
    int x;
    int y;
} Punto;

/* Uso */
Punto p1 = {10, 20};
Punto *p2 = malloc(sizeof(Punto));

/* Para estructuras autoreferenciadas, necesitas nombre */
typedef struct Nodo {
    int dato;
    struct Nodo *siguiente;  /* Debe usar 'struct Nodo' aquí */
} Nodo;

/* Alternativa con forward declaration */
typedef struct Nodo Nodo;
struct Nodo {
    int dato;
    Nodo *siguiente;  /* Ahora podemos usar 'Nodo' */
};
```

### Typedef para Punteros a Funciones

```c
/* Sin typedef */
int (*comparador)(const void*, const void*);

/* Con typedef - mucho más legible */
typedef int (*Comparador)(const void*, const void*);

Comparador cmp;
cmp = strcmp;  /* Asignar función */

/* Typedef para callbacks */
typedef void (*EventHandler)(int evento, void *datos);

void registrar_handler(EventHandler handler) {
    /* ... */
}

void mi_handler(int evento, void *datos) {
    printf("Evento: %d\n", evento);
}

registrar_handler(mi_handler);

/* Array de punteros a funciones */
typedef int (*Operacion)(int, int);

int suma(int a, int b) { return a + b; }
int resta(int a, int b) { return a - b; }

Operacion operaciones[] = {suma, resta};
int resultado = operaciones[0](5, 3);  /* 8 */
```

### Typedef para Arrays

```c
/* Array de tamaño fijo */
typedef int Vector3[3];
typedef char Nombre[50];
typedef double Matriz4x4[4][4];

Vector3 posicion = {1, 2, 3};
Nombre usuario = "Juan";
Matriz4x4 transformacion;

/* Uso en funciones */
void normalizar(Vector3 v) {
    /* ... */
}
```

---

## Unions

### Concepto

```c
/*
 * Una union almacena todos sus miembros en la misma ubicación de memoria.
 * Solo un miembro puede tener valor válido a la vez.
 * El tamaño es el del miembro más grande.
 */

union Dato {
    int entero;
    float flotante;
    char texto[4];
};

/* Tamaño */
printf("sizeof(union Dato) = %zu\n", sizeof(union Dato));  /* 4 */

/* Uso */
union Dato d;
d.entero = 65;
printf("Como entero: %d\n", d.entero);    /* 65 */
printf("Como float: %f\n", d.flotante);   /* Basura */
printf("Como texto: %s\n", d.texto);      /* "A" (ASCII 65) */

d.flotante = 3.14f;
printf("Como float: %f\n", d.flotante);   /* 3.14 */
printf("Como entero: %d\n", d.entero);    /* Representación binaria */
```

### Casos de Uso

```c
/* 1. Tipo variante (tagged union) */
typedef enum { INT, FLOAT, STRING } TipoDato;

typedef struct {
    TipoDato tipo;
    union {
        int i;
        float f;
        char s[32];
    } valor;
} Variante;

void imprimir_variante(Variante *v) {
    switch (v->tipo) {
        case INT:
            printf("int: %d\n", v->valor.i);
            break;
        case FLOAT:
            printf("float: %f\n", v->valor.f);
            break;
        case STRING:
            printf("string: %s\n", v->valor.s);
            break;
    }
}

/* Uso */
Variante v1 = {.tipo = INT, .valor.i = 42};
Variante v2 = {.tipo = FLOAT, .valor.f = 3.14f};
Variante v3 = {.tipo = STRING};
strcpy(v3.valor.s, "Hola");

/* 2. Conversión de tipos (type punning) */
union FloatInt {
    float f;
    uint32_t i;
};

/* Ver representación binaria de float */
union FloatInt fi;
fi.f = 3.14f;
printf("3.14 en hex: 0x%08X\n", fi.i);  /* 0x4048F5C3 */

/* 3. Acceso a bytes individuales */
union Word {
    uint32_t word;
    uint8_t bytes[4];
};

union Word w;
w.word = 0x12345678;
printf("Byte 0: 0x%02X\n", w.bytes[0]);  /* Depende de endianness */
printf("Byte 3: 0x%02X\n", w.bytes[3]);

/* 4. Registros de hardware */
union StatusRegister {
    uint8_t byte;
    struct {
        uint8_t carry     : 1;
        uint8_t zero      : 1;
        uint8_t interrupt : 1;
        uint8_t decimal   : 1;
        uint8_t brk       : 1;
        uint8_t unused    : 1;
        uint8_t overflow  : 1;
        uint8_t negative  : 1;
    } flags;
};

union StatusRegister sr;
sr.byte = 0x00;
sr.flags.zero = 1;
sr.flags.carry = 1;
printf("SR: 0x%02X\n", sr.byte);  /* 0x03 */
```

---

## Bit Fields

### Definición

```c
/* Bit fields permiten especificar el ancho en bits de cada miembro */
struct Flags {
    unsigned int activo    : 1;   /* 1 bit */
    unsigned int prioridad : 3;   /* 3 bits (0-7) */
    unsigned int tipo      : 4;   /* 4 bits (0-15) */
    unsigned int reservado : 24;  /* 24 bits */
};  /* Total: 32 bits = 4 bytes */

struct Flags f;
f.activo = 1;
f.prioridad = 5;
f.tipo = 12;

printf("Activo: %u\n", f.activo);       /* 1 */
printf("Prioridad: %u\n", f.prioridad); /* 5 */
printf("Tipo: %u\n", f.tipo);           /* 12 */

/* CUIDADO: No se puede tomar dirección de bit field */
/* unsigned int *p = &f.activo;  ERROR */
```

### Aplicaciones de Bit Fields

```c
/* 1. Protocolo de red (header IP simplificado) */
struct IPHeader {
    unsigned int version    : 4;
    unsigned int ihl        : 4;
    unsigned int dscp       : 6;
    unsigned int ecn        : 2;
    unsigned int length     : 16;
    /* ... más campos ... */
};

/* 2. Formato de pixel RGB565 */
struct RGB565 {
    uint16_t blue  : 5;
    uint16_t green : 6;
    uint16_t red   : 5;
};

struct RGB565 pixel;
pixel.red = 31;    /* Máximo rojo */
pixel.green = 0;
pixel.blue = 0;

/* 3. Instrucción de CPU */
struct Instruccion {
    unsigned int opcode    : 6;
    unsigned int rs        : 5;
    unsigned int rt        : 5;
    unsigned int rd        : 5;
    unsigned int shamt     : 5;
    unsigned int funct     : 6;
};

/* 4. Registro de estado */
struct EstadoProcesador {
    unsigned int carry     : 1;
    unsigned int zero      : 1;
    unsigned int negative  : 1;
    unsigned int overflow  : 1;
    unsigned int interrupt : 1;
    unsigned int           : 3;  /* Padding sin nombre */
    unsigned int modo      : 2;
    unsigned int           : 6;  /* Más padding */
};
```

### Precauciones con Bit Fields

```c
/* El orden de bits depende de la implementación */
/* No usar para portabilidad entre plataformas */

/* Mejor usar máscaras explícitas para portabilidad */
#define FLAG_ACTIVO     (1 << 0)
#define FLAG_PRIORIDAD  (0x7 << 1)
#define FLAG_TIPO       (0xF << 4)

uint32_t flags = 0;
flags |= FLAG_ACTIVO;                    /* Set activo */
flags |= (5 << 1);                       /* Set prioridad = 5 */
flags |= (12 << 4);                      /* Set tipo = 12 */

int activo = (flags & FLAG_ACTIVO) != 0;
int prioridad = (flags >> 1) & 0x7;
int tipo = (flags >> 4) & 0xF;
```

---

## Alineación y Padding

### El Problema

```c
/* Las estructuras pueden tener "padding" por alineación */
struct EjemploPadding {
    char a;      /* 1 byte */
    /* 3 bytes de padding para alinear int */
    int b;       /* 4 bytes */
    char c;      /* 1 byte */
    /* 3 bytes de padding al final */
};

printf("sizeof: %zu\n", sizeof(struct EjemploPadding));  /* 12, no 6 */

/* Visualización de memoria:
 * [a][pad][pad][pad][b][b][b][b][c][pad][pad][pad]
 *  0   1    2    3   4  5  6  7  8   9   10   11
 */
```

### Optimizar Orden de Miembros

```c
/* Mal ordenado - mucho padding */
struct Mal {
    char a;     /* 1 byte + 7 padding */
    double b;   /* 8 bytes */
    char c;     /* 1 byte + 7 padding */
    double d;   /* 8 bytes */
};  /* Total: 32 bytes */

/* Bien ordenado - mínimo padding */
struct Bien {
    double b;   /* 8 bytes */
    double d;   /* 8 bytes */
    char a;     /* 1 byte */
    char c;     /* 1 byte + 6 padding */
};  /* Total: 24 bytes (25% menos) */

/* Regla: ordenar de mayor a menor tamaño */
```

### Controlar Alineación

```c
/* 1. pragma pack (compilador específico) */
#pragma pack(push, 1)  /* Alineación de 1 byte */
struct Empaquetado {
    char a;
    int b;
    char c;
};  /* Ahora es 6 bytes */
#pragma pack(pop)  /* Restaurar */

/* 2. Atributo GCC/Clang */
struct __attribute__((packed)) Empaquetado2 {
    char a;
    int b;
    char c;
};

/* 3. alignas (C11) */
struct alignas(16) Alineado16 {
    float datos[4];
};  /* Alineado a 16 bytes (para SIMD) */

/* Obtener alineación */
#include <stdalign.h>
printf("Alineación de double: %zu\n", alignof(double));  /* 8 típico */
```

### offsetof

```c
#include <stddef.h>

struct Ejemplo {
    char a;
    int b;
    char c;
};

/* Obtener offset de un miembro */
size_t offset_a = offsetof(struct Ejemplo, a);  /* 0 */
size_t offset_b = offsetof(struct Ejemplo, b);  /* 4 */
size_t offset_c = offsetof(struct Ejemplo, c);  /* 8 */

/* Útil para serialización y acceso genérico */
void *ptr = &s;
int *pb = (int*)((char*)ptr + offsetof(struct Ejemplo, b));
```

---

## Estructuras Flexibles (Flexible Array Member)

```c
/* C99: Último miembro puede ser array sin tamaño */
struct Buffer {
    size_t longitud;
    char datos[];  /* Flexible array member */
};

/* Asignar con tamaño variable */
size_t n = 100;
struct Buffer *buf = malloc(sizeof(struct Buffer) + n);
buf->longitud = n;

/* Usar */
strcpy(buf->datos, "Hola mundo");
printf("%s\n", buf->datos);

free(buf);

/* Ejemplo: Mensaje de red */
struct Mensaje {
    uint16_t tipo;
    uint16_t longitud;
    uint8_t payload[];
};

struct Mensaje *crear_mensaje(uint16_t tipo, const void *datos, size_t len) {
    struct Mensaje *msg = malloc(sizeof(struct Mensaje) + len);
    if (msg) {
        msg->tipo = tipo;
        msg->longitud = len;
        memcpy(msg->payload, datos, len);
    }
    return msg;
}
```

---

## Patrones Comunes

### Constructor/Destructor

```c
typedef struct {
    char *nombre;
    int edad;
    double *notas;
    size_t num_notas;
} Estudiante;

/* Constructor */
Estudiante *estudiante_crear(const char *nombre, int edad) {
    Estudiante *e = malloc(sizeof(Estudiante));
    if (e == NULL) return NULL;

    e->nombre = strdup(nombre);  /* Copia el string */
    if (e->nombre == NULL) {
        free(e);
        return NULL;
    }

    e->edad = edad;
    e->notas = NULL;
    e->num_notas = 0;

    return e;
}

/* Destructor */
void estudiante_destruir(Estudiante *e) {
    if (e == NULL) return;
    free(e->nombre);
    free(e->notas);
    free(e);
}

/* Método */
int estudiante_agregar_nota(Estudiante *e, double nota) {
    double *nuevas = realloc(e->notas,
                             (e->num_notas + 1) * sizeof(double));
    if (nuevas == NULL) return -1;

    e->notas = nuevas;
    e->notas[e->num_notas++] = nota;
    return 0;
}

double estudiante_promedio(const Estudiante *e) {
    if (e->num_notas == 0) return 0.0;

    double suma = 0;
    for (size_t i = 0; i < e->num_notas; i++) {
        suma += e->notas[i];
    }
    return suma / e->num_notas;
}

/* Uso */
Estudiante *e = estudiante_crear("Ana García", 20);
estudiante_agregar_nota(e, 8.5);
estudiante_agregar_nota(e, 9.0);
estudiante_agregar_nota(e, 7.5);
printf("Promedio: %.2f\n", estudiante_promedio(e));
estudiante_destruir(e);
```

### Interfaz Opaca (Encapsulación)

```c
/* archivo: stack.h */
#ifndef STACK_H
#define STACK_H

#include <stdbool.h>

/* Tipo opaco - estructura definida en .c */
typedef struct Stack Stack;

/* API pública */
Stack *stack_crear(size_t capacidad);
void stack_destruir(Stack *s);
bool stack_push(Stack *s, int valor);
bool stack_pop(Stack *s, int *valor);
bool stack_esta_vacio(const Stack *s);
size_t stack_tamanio(const Stack *s);

#endif

/* archivo: stack.c */
#include "stack.h"
#include <stdlib.h>

/* Definición privada */
struct Stack {
    int *datos;
    size_t tope;
    size_t capacidad;
};

Stack *stack_crear(size_t capacidad) {
    Stack *s = malloc(sizeof(Stack));
    if (s == NULL) return NULL;

    s->datos = malloc(capacidad * sizeof(int));
    if (s->datos == NULL) {
        free(s);
        return NULL;
    }

    s->tope = 0;
    s->capacidad = capacidad;
    return s;
}

void stack_destruir(Stack *s) {
    if (s == NULL) return;
    free(s->datos);
    free(s);
}

bool stack_push(Stack *s, int valor) {
    if (s->tope >= s->capacidad) return false;
    s->datos[s->tope++] = valor;
    return true;
}

bool stack_pop(Stack *s, int *valor) {
    if (s->tope == 0) return false;
    *valor = s->datos[--s->tope];
    return true;
}

bool stack_esta_vacio(const Stack *s) {
    return s->tope == 0;
}

size_t stack_tamanio(const Stack *s) {
    return s->tope;
}

/* Uso desde otro archivo */
#include "stack.h"

int main() {
    Stack *s = stack_crear(100);
    stack_push(s, 10);
    stack_push(s, 20);

    int valor;
    while (stack_pop(s, &valor)) {
        printf("%d\n", valor);
    }

    stack_destruir(s);
    return 0;
}
```

### Herencia Simulada

```c
/* "Clase base" */
typedef struct {
    int x, y;
} Forma;

void forma_mover(Forma *f, int dx, int dy) {
    f->x += dx;
    f->y += dy;
}

/* "Clase derivada" */
typedef struct {
    Forma base;  /* Primer miembro = "herencia" */
    int radio;
} Circulo;

Circulo *circulo_crear(int x, int y, int radio) {
    Circulo *c = malloc(sizeof(Circulo));
    if (c) {
        c->base.x = x;
        c->base.y = y;
        c->radio = radio;
    }
    return c;
}

/* Polimorfismo: Circulo* puede usarse como Forma* */
Circulo *c = circulo_crear(10, 20, 5);
forma_mover((Forma*)c, 5, 5);  /* c->base.x = 15, c->base.y = 25 */

/* O usando cast implícito si Forma es primer miembro */
forma_mover(&c->base, 5, 5);
```

### Tabla de Funciones Virtuales

```c
/* Estructura base con vtable */
typedef struct Figura Figura;

typedef struct {
    double (*area)(const Figura*);
    double (*perimetro)(const Figura*);
    void (*destruir)(Figura*);
} FiguraVTable;

struct Figura {
    const FiguraVTable *vtable;
};

/* Circulo */
typedef struct {
    Figura base;
    double radio;
} Circulo;

double circulo_area(const Figura *f) {
    const Circulo *c = (const Circulo*)f;
    return 3.14159 * c->radio * c->radio;
}

double circulo_perimetro(const Figura *f) {
    const Circulo *c = (const Circulo*)f;
    return 2 * 3.14159 * c->radio;
}

void circulo_destruir(Figura *f) {
    free(f);
}

static const FiguraVTable circulo_vtable = {
    .area = circulo_area,
    .perimetro = circulo_perimetro,
    .destruir = circulo_destruir
};

Figura *circulo_crear(double radio) {
    Circulo *c = malloc(sizeof(Circulo));
    if (c) {
        c->base.vtable = &circulo_vtable;
        c->radio = radio;
    }
    return (Figura*)c;
}

/* Rectangulo */
typedef struct {
    Figura base;
    double ancho, alto;
} Rectangulo;

double rectangulo_area(const Figura *f) {
    const Rectangulo *r = (const Rectangulo*)f;
    return r->ancho * r->alto;
}

double rectangulo_perimetro(const Figura *f) {
    const Rectangulo *r = (const Rectangulo*)f;
    return 2 * (r->ancho + r->alto);
}

void rectangulo_destruir(Figura *f) {
    free(f);
}

static const FiguraVTable rectangulo_vtable = {
    .area = rectangulo_area,
    .perimetro = rectangulo_perimetro,
    .destruir = rectangulo_destruir
};

Figura *rectangulo_crear(double ancho, double alto) {
    Rectangulo *r = malloc(sizeof(Rectangulo));
    if (r) {
        r->base.vtable = &rectangulo_vtable;
        r->ancho = ancho;
        r->alto = alto;
    }
    return (Figura*)r;
}

/* Uso polimórfico */
void procesar_figura(Figura *f) {
    printf("Área: %.2f\n", f->vtable->area(f));
    printf("Perímetro: %.2f\n", f->vtable->perimetro(f));
}

int main() {
    Figura *figuras[] = {
        circulo_crear(5.0),
        rectangulo_crear(4.0, 3.0),
        circulo_crear(2.5)
    };

    for (int i = 0; i < 3; i++) {
        procesar_figura(figuras[i]);
        figuras[i]->vtable->destruir(figuras[i]);
    }

    return 0;
}
```

---

## Próximos Documentos

- **C_04_ARCHIVOS.md**: I/O avanzado, streams, serialización
- **C_05_AVANZADO.md**: Macros avanzados, técnicas de optimización

---

*"Las estructuras en C son la arcilla con la que moldeas tus abstracciones. El límite es tu imaginación."*


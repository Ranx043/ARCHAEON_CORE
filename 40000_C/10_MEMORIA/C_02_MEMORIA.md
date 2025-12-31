# C_02: Punteros y Gestión de Memoria

> "Los punteros son el poder y el peligro de C. Domínalos o ellos te dominarán."

---

## Fundamentos de Punteros

### Qué es un Puntero

```c
/*
 * Un puntero es una variable que almacena una dirección de memoria.
 * El tipo del puntero indica qué tipo de dato está en esa dirección.
 */

int x = 42;          /* Variable normal */
int *ptr = &x;       /* ptr contiene la dirección de x */

printf("Valor de x: %d\n", x);           /* 42 */
printf("Dirección de x: %p\n", &x);      /* ej: 0x7ffd12345678 */
printf("Valor de ptr: %p\n", ptr);       /* misma dirección */
printf("Valor apuntado: %d\n", *ptr);    /* 42 */

/* Modificar a través del puntero */
*ptr = 100;
printf("Nuevo valor de x: %d\n", x);     /* 100 */
```

### Operadores de Punteros

```c
int x = 42;
int *ptr;

/* & - Operador "dirección de" */
ptr = &x;           /* ptr ahora apunta a x */

/* * - Operador "contenido de" (dereference) */
int y = *ptr;       /* y = 42 (valor en la dirección) */
*ptr = 100;         /* Modifica x a través de ptr */

/* Declaración vs Uso */
int *p;             /* Declaración: p es puntero a int */
*p = 5;             /* Uso: asigna 5 al lugar apuntado */

/* Múltiples punteros en una línea */
int *a, *b, *c;     /* Tres punteros a int */
int *d, e, f;       /* d es puntero, e y f son int */
```

### Puntero NULL

```c
#include <stddef.h>  /* o <stdio.h> */

int *ptr = NULL;     /* Puntero que no apunta a nada */

/* SIEMPRE verificar antes de usar */
if (ptr != NULL) {
    *ptr = 42;       /* Solo si ptr es válido */
}

/* Patrón común */
if (ptr) {           /* Equivalente a ptr != NULL */
    /* usar ptr */
}

if (!ptr) {          /* Equivalente a ptr == NULL */
    /* ptr es NULL */
}
```

### Aritmética de Punteros

```c
int arr[] = {10, 20, 30, 40, 50};
int *ptr = arr;     /* ptr apunta al primer elemento */

/* Incrementar puntero */
ptr++;              /* Avanza sizeof(int) bytes */
printf("%d\n", *ptr);  /* 20 */

/* Sumar a puntero */
ptr = arr;
printf("%d\n", *(ptr + 2));  /* 30 (tercer elemento) */

/* Diferencia de punteros */
int *end = &arr[4];
int *start = arr;
ptrdiff_t diff = end - start;  /* 4 (elementos, no bytes) */

/* Relación con arrays */
arr[i]      /* equivale a */   *(arr + i)
&arr[i]     /* equivale a */   arr + i

/* CUIDADO: el tipo importa */
char *cp;   /* cp++ avanza 1 byte */
int *ip;    /* ip++ avanza 4 bytes (típico) */
double *dp; /* dp++ avanza 8 bytes */
```

### Punteros a Punteros

```c
int x = 42;
int *ptr = &x;
int **pptr = &ptr;

printf("x = %d\n", x);           /* 42 */
printf("*ptr = %d\n", *ptr);     /* 42 */
printf("**pptr = %d\n", **pptr); /* 42 */

/* Modificar */
**pptr = 100;        /* x ahora es 100 */

/* Uso común: modificar un puntero en una función */
void obtener_memoria(int **ptr, size_t n) {
    *ptr = malloc(n * sizeof(int));
}

int *datos = NULL;
obtener_memoria(&datos, 100);  /* datos ahora apunta a memoria */
```

---

## Arrays y Punteros

### Relación Array-Puntero

```c
int arr[5] = {1, 2, 3, 4, 5};

/* El nombre del array "decae" a puntero al primer elemento */
int *ptr = arr;          /* Equivalente a &arr[0] */

/* Estas expresiones son equivalentes */
arr[0]    ==   *arr         ==   *ptr         ==   ptr[0]
arr[1]    ==   *(arr+1)     ==   *(ptr+1)     ==   ptr[1]
arr[i]    ==   *(arr+i)     ==   *(ptr+i)     ==   ptr[i]
&arr[i]   ==   arr+i        ==   ptr+i

/* DIFERENCIA IMPORTANTE */
sizeof(arr)     /* 20 (5 * sizeof(int)) - tamaño total */
sizeof(ptr)     /* 4 u 8 - tamaño del puntero */

/* arr NO es modificable (no se puede reasignar) */
arr = otro_array;   /* ERROR */
ptr = otro_array;   /* OK */
```

### Arrays Dinámicos

```c
#include <stdlib.h>

/* Array dinámico 1D */
int *arr = malloc(100 * sizeof(int));
if (arr == NULL) {
    /* Manejar error */
}

for (int i = 0; i < 100; i++) {
    arr[i] = i;         /* Usar como array normal */
}

free(arr);
arr = NULL;             /* Buena práctica */

/* Array dinámico 2D - Método 1: Array de punteros */
int rows = 3, cols = 4;
int **matrix = malloc(rows * sizeof(int*));
for (int i = 0; i < rows; i++) {
    matrix[i] = malloc(cols * sizeof(int));
}

matrix[1][2] = 42;      /* Acceso normal */

/* Liberar en orden inverso */
for (int i = 0; i < rows; i++) {
    free(matrix[i]);
}
free(matrix);

/* Array dinámico 2D - Método 2: Bloque contiguo */
int *matrix2 = malloc(rows * cols * sizeof(int));
/* Acceso: matrix2[i * cols + j] */
matrix2[1 * cols + 2] = 42;
free(matrix2);

/* Array dinámico 2D - Método 3: VLA (C99, opcional) */
int (*matrix3)[cols] = malloc(rows * sizeof(*matrix3));
matrix3[1][2] = 42;     /* Acceso natural */
free(matrix3);
```

### Punteros a Arrays vs Arrays de Punteros

```c
/* Array de punteros */
int *arr_ptr[10];       /* 10 punteros a int */
arr_ptr[0] = malloc(5 * sizeof(int));

/* Puntero a array */
int (*ptr_arr)[10];     /* 1 puntero a array de 10 int */
int data[10];
ptr_arr = &data;
(*ptr_arr)[0] = 42;     /* Acceso */

/* Puntero a array para matrices */
void procesar_matriz(int (*m)[4], int filas) {
    for (int i = 0; i < filas; i++) {
        for (int j = 0; j < 4; j++) {
            printf("%d ", m[i][j]);
        }
    }
}

int matriz[3][4] = {{1,2,3,4}, {5,6,7,8}, {9,10,11,12}};
procesar_matriz(matriz, 3);
```

---

## Memoria: Stack vs Heap

### Stack (Pila)

```
┌─────────────────────────────────────┐
│           STACK                      │
│  ┌─────────────────────────────┐    │
│  │ Variables locales           │    │ ← Crece hacia abajo
│  │ Parámetros de función       │    │
│  │ Direcciones de retorno      │    │
│  │ Registros guardados         │    │
│  └─────────────────────────────┘    │
│                                      │
│  Características:                    │
│  - Asignación automática            │
│  - LIFO (Last In, First Out)        │
│  - Muy rápido                       │
│  - Tamaño limitado (~1-8 MB)        │
│  - Se libera automáticamente        │
└─────────────────────────────────────┘
```

```c
void funcion() {
    int local = 42;          /* En stack */
    char buffer[100];        /* En stack */
    double arr[1000];        /* En stack */

    /* Se liberan automáticamente al salir de la función */
}

/* PELIGRO: Devolver puntero a variable local */
int *mala_funcion() {
    int local = 42;
    return &local;           /* ERROR: local ya no existe */
}
```

### Heap (Montón)

```
┌─────────────────────────────────────┐
│           HEAP                       │
│                                      │
│  Características:                    │
│  - Asignación manual (malloc/free)  │
│  - Acceso más lento que stack       │
│  - Tamaño limitado por RAM          │
│  - Fragmentación posible            │
│  - Debe liberarse manualmente       │
│                                      │
│  ┌─────────────────────────────┐    │
│  │ Bloques asignados           │    │ ← Crece hacia arriba
│  │ Bloques libres              │    │
│  │ Metadatos de gestión        │    │
│  └─────────────────────────────┘    │
└─────────────────────────────────────┘
```

```c
void funcion() {
    int *ptr = malloc(1000 * sizeof(int));  /* En heap */

    /* DEBE liberarse explícitamente */
    free(ptr);
}

/* OK: Devolver puntero a memoria del heap */
int *buena_funcion(size_t n) {
    int *ptr = malloc(n * sizeof(int));
    /* Inicializar... */
    return ptr;  /* OK: la memoria persiste */
}

/* El llamador debe liberar */
int *datos = buena_funcion(100);
/* usar datos... */
free(datos);
```

### Mapa de Memoria de un Proceso

```
┌─────────────────────────────────────┐  Direcciones altas
│          Stack                       │  ← Crece hacia abajo
│          ↓                           │
├─────────────────────────────────────┤
│                                      │
│         (espacio libre)              │
│                                      │
├─────────────────────────────────────┤
│          ↑                           │
│          Heap                        │  ← Crece hacia arriba
├─────────────────────────────────────┤
│          BSS                         │  Variables no inicializadas
├─────────────────────────────────────┤
│          Data                        │  Variables inicializadas
├─────────────────────────────────────┤
│          Text (Code)                 │  Código del programa
└─────────────────────────────────────┘  Direcciones bajas
```

---

## Funciones de Memoria

### malloc, calloc, realloc, free

```c
#include <stdlib.h>

/* malloc - asigna memoria sin inicializar */
int *p1 = malloc(100 * sizeof(int));
if (p1 == NULL) {
    /* Error: no hay memoria */
    exit(1);
}
/* p1 contiene basura - debe inicializarse */

/* calloc - asigna e inicializa a cero */
int *p2 = calloc(100, sizeof(int));
if (p2 == NULL) {
    exit(1);
}
/* p2 está todo en ceros */

/* realloc - cambia el tamaño */
int *p3 = malloc(100 * sizeof(int));
/* llenar p3... */

int *temp = realloc(p3, 200 * sizeof(int));
if (temp == NULL) {
    /* Error: p3 sigue válido, pero no creció */
    free(p3);
    exit(1);
}
p3 = temp;  /* p3 ahora tiene espacio para 200 int */
/* Los primeros 100 valores se conservan */

/* free - libera memoria */
free(p1);
free(p2);
free(p3);

/* Buena práctica: poner a NULL después de free */
p1 = NULL;
p2 = NULL;
p3 = NULL;

/* free(NULL) es seguro (no hace nada) */
free(NULL);  /* OK */
```

### Errores Comunes de Memoria

```c
/* 1. Memory leak (fuga de memoria) */
void leak() {
    int *p = malloc(100 * sizeof(int));
    /* No se llama free() */
}  /* Memoria perdida para siempre */

/* 2. Use after free */
int *p = malloc(sizeof(int));
*p = 42;
free(p);
*p = 100;  /* ERROR: p ya no es válido */

/* 3. Double free */
int *p = malloc(sizeof(int));
free(p);
free(p);  /* ERROR: ya fue liberado */

/* 4. Buffer overflow */
int *p = malloc(10 * sizeof(int));
p[10] = 42;  /* ERROR: índice fuera de rango */

/* 5. Usar memoria no inicializada */
int *p = malloc(sizeof(int));
printf("%d\n", *p);  /* ERROR: valor indefinido */

/* 6. Perder la única referencia */
int *p = malloc(100);
p = malloc(200);  /* Leak: perdimos referencia a los primeros 100 bytes */
```

### Patrón de Gestión de Memoria

```c
/* Función con manejo correcto de memoria */
int *crear_array(size_t n) {
    if (n == 0) {
        return NULL;
    }

    int *arr = malloc(n * sizeof(int));
    if (arr == NULL) {
        return NULL;
    }

    for (size_t i = 0; i < n; i++) {
        arr[i] = 0;  /* Inicializar */
    }

    return arr;
}

void destruir_array(int **arr) {
    if (arr == NULL || *arr == NULL) {
        return;
    }
    free(*arr);
    *arr = NULL;  /* Previene use-after-free */
}

/* Uso */
int *datos = crear_array(100);
if (datos == NULL) {
    /* Manejar error */
}
/* usar datos... */
destruir_array(&datos);  /* datos ahora es NULL */
```

---

## Punteros y Funciones

### Paso por Referencia

```c
/* Por valor - NO modifica el original */
void no_modifica(int x) {
    x = 100;  /* Solo modifica la copia local */
}

/* Por referencia - SÍ modifica el original */
void si_modifica(int *x) {
    *x = 100;  /* Modifica el valor original */
}

int num = 42;
no_modifica(num);
printf("%d\n", num);  /* 42 */

si_modifica(&num);
printf("%d\n", num);  /* 100 */

/* Swap clásico */
void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

int x = 5, y = 10;
swap(&x, &y);
printf("x=%d, y=%d\n", x, y);  /* x=10, y=5 */
```

### Retornar Múltiples Valores

```c
/* Mediante punteros */
void dividir(int dividendo, int divisor,
             int *cociente, int *residuo) {
    *cociente = dividendo / divisor;
    *residuo = dividendo % divisor;
}

int q, r;
dividir(17, 5, &q, &r);
printf("%d / %d = %d resto %d\n", 17, 5, q, r);

/* Mediante struct */
typedef struct {
    int cociente;
    int residuo;
} DivResult;

DivResult dividir2(int dividendo, int divisor) {
    DivResult result;
    result.cociente = dividendo / divisor;
    result.residuo = dividendo % divisor;
    return result;
}

DivResult res = dividir2(17, 5);
printf("Cociente: %d, Residuo: %d\n", res.cociente, res.residuo);
```

### Punteros a Funciones

```c
/* Declaración */
int (*funcion_ptr)(int, int);

/* Funciones */
int sumar(int a, int b) { return a + b; }
int restar(int a, int b) { return a - b; }
int multiplicar(int a, int b) { return a * b; }

/* Asignación */
funcion_ptr = sumar;
int resultado = funcion_ptr(5, 3);  /* 8 */

funcion_ptr = restar;
resultado = funcion_ptr(5, 3);  /* 2 */

/* Typedef para claridad */
typedef int (*OperacionBinaria)(int, int);

OperacionBinaria operaciones[] = {sumar, restar, multiplicar};
resultado = operaciones[2](5, 3);  /* 15 */

/* Callbacks */
void aplicar_a_array(int *arr, size_t n, int (*func)(int)) {
    for (size_t i = 0; i < n; i++) {
        arr[i] = func(arr[i]);
    }
}

int duplicar(int x) { return x * 2; }
int cuadrado(int x) { return x * x; }

int nums[] = {1, 2, 3, 4, 5};
aplicar_a_array(nums, 5, duplicar);  /* {2, 4, 6, 8, 10} */

/* qsort: ejemplo real de callback */
int comparar_int(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

int arr[] = {5, 2, 8, 1, 9};
qsort(arr, 5, sizeof(int), comparar_int);
/* arr ahora es {1, 2, 5, 8, 9} */
```

---

## Punteros y Strings

### Strings son Punteros

```c
/* String literal */
const char *str1 = "Hello";  /* Puntero a memoria de solo lectura */

/* Array de caracteres */
char str2[] = "World";       /* Copia modificable */

/* Diferencias */
str1[0] = 'h';  /* ERROR: memoria de solo lectura */
str2[0] = 'W';  /* OK: array modificable */

str1 = "Another";  /* OK: reasignar puntero */
str2 = "Other";    /* ERROR: no se puede reasignar array */

/* Tamaños */
sizeof(str1);  /* Tamaño del puntero (4 u 8) */
sizeof(str2);  /* Tamaño del array (6 incluyendo '\0') */
strlen(str1);  /* 5 (longitud sin '\0') */
```

### Manipulación de Strings

```c
/* Copiar string manualmente */
void mi_strcpy(char *dest, const char *src) {
    while (*src != '\0') {
        *dest = *src;
        dest++;
        src++;
    }
    *dest = '\0';
}

/* Versión compacta */
void mi_strcpy2(char *dest, const char *src) {
    while ((*dest++ = *src++) != '\0');
}

/* Longitud */
size_t mi_strlen(const char *s) {
    const char *p = s;
    while (*p != '\0') {
        p++;
    }
    return p - s;
}

/* Comparar */
int mi_strcmp(const char *s1, const char *s2) {
    while (*s1 && (*s1 == *s2)) {
        s1++;
        s2++;
    }
    return *(unsigned char*)s1 - *(unsigned char*)s2;
}

/* Buscar carácter */
char *mi_strchr(const char *s, int c) {
    while (*s != '\0') {
        if (*s == c) {
            return (char*)s;
        }
        s++;
    }
    return NULL;
}
```

### Array de Strings

```c
/* Array de punteros a string */
const char *dias[] = {
    "Lunes",
    "Martes",
    "Miércoles",
    "Jueves",
    "Viernes",
    "Sábado",
    "Domingo"
};

/* Acceso */
printf("%s\n", dias[0]);      /* Lunes */
printf("%c\n", dias[0][0]);   /* L */

/* Iterar */
for (int i = 0; i < 7; i++) {
    printf("%s\n", dias[i]);
}

/* Array 2D de chars (strings de tamaño fijo) */
char dias2[7][12] = {
    "Lunes",
    "Martes",
    "Miércoles",
    "Jueves",
    "Viernes",
    "Sábado",
    "Domingo"
};

/* Diferencia de tamaño */
sizeof(dias);   /* 7 * sizeof(char*) = 28 o 56 */
sizeof(dias2);  /* 7 * 12 = 84 */
```

---

## Punteros Const

### Variaciones de Const

```c
/* 1. Puntero a const (dato no modificable) */
const int *ptr1;
int const *ptr2;  /* Equivalente */
/* *ptr1 = 5;  ERROR: no se puede modificar el valor */
/* ptr1 = &otra;  OK: se puede reasignar el puntero */

/* 2. Puntero const (puntero no modificable) */
int *const ptr3 = &x;
/* *ptr3 = 5;  OK: se puede modificar el valor */
/* ptr3 = &otra;  ERROR: no se puede reasignar */

/* 3. Ambos const */
const int *const ptr4 = &x;
/* *ptr4 = 5;  ERROR */
/* ptr4 = &otra;  ERROR */

/* Uso común en funciones */
void imprimir_array(const int *arr, size_t n) {
    /* arr[i] = 0;  ERROR: no podemos modificar */
    for (size_t i = 0; i < n; i++) {
        printf("%d ", arr[i]);  /* Solo lectura */
    }
}

/* const char* para strings que no se modifican */
void saludar(const char *nombre) {
    printf("Hola, %s!\n", nombre);
}
```

---

## Punteros Void

```c
/* void* - puntero genérico */
void *ptr;

int i = 42;
double d = 3.14;
char c = 'A';

/* Puede apuntar a cualquier tipo */
ptr = &i;
ptr = &d;
ptr = &c;

/* PERO: no se puede dereferenciar directamente */
/* *ptr = 100;  ERROR */

/* Necesita cast para usar */
ptr = &i;
int valor = *(int*)ptr;  /* 42 */

/* Uso: funciones genéricas */
void *memcpy(void *dest, const void *src, size_t n);
void *malloc(size_t size);
void qsort(void *base, size_t nmemb, size_t size,
           int (*compar)(const void *, const void *));

/* Ejemplo: swap genérico */
void swap_generico(void *a, void *b, size_t size) {
    char temp[size];  /* VLA o usar malloc */
    memcpy(temp, a, size);
    memcpy(a, b, size);
    memcpy(b, temp, size);
}

int x = 5, y = 10;
swap_generico(&x, &y, sizeof(int));

double d1 = 1.5, d2 = 2.5;
swap_generico(&d1, &d2, sizeof(double));
```

---

## Estructuras de Datos con Punteros

### Lista Enlazada

```c
/* Nodo de lista */
typedef struct Node {
    int data;
    struct Node *next;
} Node;

/* Crear nodo */
Node *crear_nodo(int data) {
    Node *nuevo = malloc(sizeof(Node));
    if (nuevo == NULL) return NULL;
    nuevo->data = data;
    nuevo->next = NULL;
    return nuevo;
}

/* Insertar al inicio */
void insertar_inicio(Node **head, int data) {
    Node *nuevo = crear_nodo(data);
    if (nuevo == NULL) return;
    nuevo->next = *head;
    *head = nuevo;
}

/* Insertar al final */
void insertar_final(Node **head, int data) {
    Node *nuevo = crear_nodo(data);
    if (nuevo == NULL) return;

    if (*head == NULL) {
        *head = nuevo;
        return;
    }

    Node *actual = *head;
    while (actual->next != NULL) {
        actual = actual->next;
    }
    actual->next = nuevo;
}

/* Buscar */
Node *buscar(Node *head, int data) {
    Node *actual = head;
    while (actual != NULL) {
        if (actual->data == data) {
            return actual;
        }
        actual = actual->next;
    }
    return NULL;
}

/* Eliminar nodo */
void eliminar(Node **head, int data) {
    if (*head == NULL) return;

    /* Si es el primero */
    if ((*head)->data == data) {
        Node *temp = *head;
        *head = (*head)->next;
        free(temp);
        return;
    }

    /* Buscar el anterior */
    Node *actual = *head;
    while (actual->next != NULL && actual->next->data != data) {
        actual = actual->next;
    }

    if (actual->next != NULL) {
        Node *temp = actual->next;
        actual->next = temp->next;
        free(temp);
    }
}

/* Liberar toda la lista */
void liberar_lista(Node **head) {
    Node *actual = *head;
    while (actual != NULL) {
        Node *siguiente = actual->next;
        free(actual);
        actual = siguiente;
    }
    *head = NULL;
}

/* Imprimir */
void imprimir_lista(Node *head) {
    Node *actual = head;
    while (actual != NULL) {
        printf("%d -> ", actual->data);
        actual = actual->next;
    }
    printf("NULL\n");
}

/* Uso */
int main() {
    Node *lista = NULL;

    insertar_final(&lista, 10);
    insertar_final(&lista, 20);
    insertar_inicio(&lista, 5);

    imprimir_lista(lista);  /* 5 -> 10 -> 20 -> NULL */

    eliminar(&lista, 10);
    imprimir_lista(lista);  /* 5 -> 20 -> NULL */

    liberar_lista(&lista);
    return 0;
}
```

### Árbol Binario

```c
typedef struct TreeNode {
    int data;
    struct TreeNode *left;
    struct TreeNode *right;
} TreeNode;

TreeNode *crear_tree_node(int data) {
    TreeNode *nuevo = malloc(sizeof(TreeNode));
    if (nuevo == NULL) return NULL;
    nuevo->data = data;
    nuevo->left = NULL;
    nuevo->right = NULL;
    return nuevo;
}

/* Insertar en BST */
TreeNode *insertar_bst(TreeNode *root, int data) {
    if (root == NULL) {
        return crear_tree_node(data);
    }

    if (data < root->data) {
        root->left = insertar_bst(root->left, data);
    } else {
        root->right = insertar_bst(root->right, data);
    }

    return root;
}

/* Recorrido inorder */
void inorder(TreeNode *root) {
    if (root == NULL) return;
    inorder(root->left);
    printf("%d ", root->data);
    inorder(root->right);
}

/* Liberar árbol */
void liberar_arbol(TreeNode *root) {
    if (root == NULL) return;
    liberar_arbol(root->left);
    liberar_arbol(root->right);
    free(root);
}
```

---

## Herramientas de Depuración

### Valgrind

```bash
# Compilar con símbolos de debug
gcc -g programa.c -o programa

# Ejecutar con Valgrind
valgrind --leak-check=full ./programa

# Salida típica de error:
# ==12345== Invalid write of size 4
# ==12345==    at 0x1234: funcion (programa.c:42)
# ==12345==
# ==12345== LEAK SUMMARY:
# ==12345==    definitely lost: 100 bytes in 1 blocks
```

### AddressSanitizer

```bash
# Compilar con sanitizer
gcc -fsanitize=address -g programa.c -o programa

# Ejecutar
./programa

# Detecta:
# - Buffer overflow
# - Use after free
# - Memory leaks
# - Double free
```

---

## Próximos Documentos

- **C_03_ESTRUCTURAS.md**: struct, union, typedef, bit fields
- **C_04_ARCHIVOS.md**: I/O avanzado y streams
- **C_05_AVANZADO.md**: Técnicas avanzadas

---

*"Cada puntero es una promesa: 'hay algo útil en esta dirección'. Asegúrate de cumplirla."*


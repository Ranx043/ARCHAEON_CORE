# C_04: ARCHIVOS Y ENTRADA/SALIDA AVANZADA

> *"El flujo de datos es el torrente sanguíneo del sistema - domínalo y controlarás la vida misma del programa."*

## Índice
1. [Streams y Buffering](#1-streams-y-buffering)
2. [Operaciones con FILE](#2-operaciones-con-file)
3. [I/O Binario](#3-io-binario)
4. [Acceso Aleatorio](#4-acceso-aleatorio)
5. [Serialización](#5-serialización)
6. [I/O de Bajo Nivel](#6-io-de-bajo-nivel)
7. [Manejo de Directorios](#7-manejo-de-directorios)
8. [Memory-Mapped Files](#8-memory-mapped-files)

---

## 1. Streams y Buffering

### 1.1 El Modelo de Streams en C

```c
/*
 * CONCEPTOS FUNDAMENTALES DE STREAMS
 * ==================================
 *
 * Un stream es una abstracción que representa un flujo
 * de datos secuencial - puede ser archivo, terminal,
 * socket, pipe, dispositivo, etc.
 *
 * Streams estándar predefinidos:
 *   stdin  - Entrada estándar (teclado)
 *   stdout - Salida estándar (pantalla)
 *   stderr - Error estándar (pantalla, sin buffer)
 */

#include <stdio.h>

/* FILE - Estructura opaca que representa un stream */
/* Nunca accedas directamente a sus miembros */

int main(void) {
    /* Los tres streams estándar están siempre disponibles */
    fprintf(stdout, "Mensaje normal\n");
    fprintf(stderr, "Mensaje de error\n");

    int valor;
    fscanf(stdin, "%d", &valor);

    return 0;
}
```

### 1.2 Tipos de Buffering

```c
/*
 * MODOS DE BUFFERING
 * ==================
 *
 * _IOFBF (Full Buffering):
 *   - Datos se escriben cuando buffer está lleno
 *   - Usado para archivos regulares
 *   - Mejor rendimiento para I/O masivo
 *
 * _IOLBF (Line Buffering):
 *   - Datos se escriben al encontrar '\n'
 *   - Típico para terminales interactivos
 *   - stdout cuando está conectado a terminal
 *
 * _IONBF (No Buffering):
 *   - Cada operación I/O ejecuta syscall
 *   - stderr siempre usa este modo
 *   - Necesario para output crítico inmediato
 */

#include <stdio.h>

int main(void) {
    FILE *fp = fopen("datos.txt", "w");
    if (!fp) return 1;

    /* Buffer personalizado - DEBE ser static o heap */
    static char mi_buffer[8192];

    /* setvbuf DEBE llamarse antes de cualquier I/O */
    if (setvbuf(fp, mi_buffer, _IOFBF, sizeof(mi_buffer)) != 0) {
        fprintf(stderr, "Error configurando buffer\n");
    }

    /* Alternativa: setbuf (más simple, menos control) */
    /* setbuf(fp, mi_buffer);  // Full buffering con BUFSIZ */
    /* setbuf(fp, NULL);       // No buffering */

    /* Escribir datos - van al buffer primero */
    for (int i = 0; i < 10000; i++) {
        fprintf(fp, "Línea %d\n", i);
    }

    /* Forzar escritura del buffer al sistema */
    fflush(fp);  /* Crítico antes de operaciones importantes */

    fclose(fp);  /* fclose también hace flush automático */
    return 0;
}
```

### 1.3 Control Avanzado de Buffering

```c
#include <stdio.h>
#include <stdlib.h>

/*
 * ESTRATEGIAS DE BUFFERING POR CASO DE USO
 */

void ejemplo_log_critico(void) {
    FILE *log = fopen("critico.log", "a");
    if (!log) return;

    /* Sin buffering para logs críticos - escritura inmediata */
    setvbuf(log, NULL, _IONBF, 0);

    fprintf(log, "Evento crítico: %s\n", "sistema iniciado");
    /* No necesita fflush - se escribe inmediatamente */

    fclose(log);
}

void ejemplo_procesamiento_masivo(void) {
    FILE *entrada = fopen("big_data.csv", "r");
    FILE *salida = fopen("resultado.csv", "w");
    if (!entrada || !salida) return;

    /* Buffers grandes para mejor rendimiento */
    static char buf_in[65536];   /* 64KB */
    static char buf_out[65536];

    setvbuf(entrada, buf_in, _IOFBF, sizeof(buf_in));
    setvbuf(salida, buf_out, _IOFBF, sizeof(buf_out));

    char linea[1024];
    while (fgets(linea, sizeof(linea), entrada)) {
        /* Procesar línea... */
        fputs(linea, salida);
    }

    fclose(entrada);
    fclose(salida);
}

void ejemplo_interactivo(void) {
    /* stdout line-buffered para feedback inmediato */
    setvbuf(stdout, NULL, _IOLBF, 0);

    printf("Ingrese comando: ");  /* Se muestra inmediatamente */
    /* (porque termina implícitamente con flush por stdin) */

    char cmd[256];
    fgets(cmd, sizeof(cmd), stdin);

    printf("Procesando: %s", cmd);  /* Con \n, se muestra */
}
```

---

## 2. Operaciones con FILE

### 2.1 Apertura y Cierre de Archivos

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

/*
 * MODOS DE APERTURA fopen()
 * =========================
 *
 * Modo    | Lectura | Escritura | Crear | Truncar | Posición
 * --------|---------|-----------|-------|---------|----------
 * "r"     |   Sí    |    No     |  No   |   No    | Inicio
 * "w"     |   No    |    Sí     |  Sí   |   Sí    | Inicio
 * "a"     |   No    |    Sí     |  Sí   |   No    | Final
 * "r+"    |   Sí    |    Sí     |  No   |   No    | Inicio
 * "w+"    |   Sí    |    Sí     |  Sí   |   Sí    | Inicio
 * "a+"    |   Sí    |    Sí     |  Sí   |   No    | Final(esc)
 *
 * Añadir 'b' para modo binario: "rb", "wb", "ab", etc.
 * En Unix no hay diferencia, en Windows sí (traducción \n)
 */

/* Patrón robusto de apertura */
FILE *abrir_archivo_seguro(const char *nombre, const char *modo) {
    if (!nombre || !modo) {
        fprintf(stderr, "Parámetros inválidos\n");
        return NULL;
    }

    FILE *fp = fopen(nombre, modo);

    if (!fp) {
        fprintf(stderr, "Error abriendo '%s': %s\n",
                nombre, strerror(errno));
        return NULL;
    }

    return fp;
}

/* Cierre seguro con verificación */
int cerrar_archivo_seguro(FILE **fp) {
    if (!fp || !*fp) {
        return 0;  /* Ya cerrado o NULL */
    }

    int resultado = fclose(*fp);
    *fp = NULL;  /* Prevenir double-free */

    if (resultado == EOF) {
        fprintf(stderr, "Error cerrando archivo: %s\n",
                strerror(errno));
        return -1;
    }

    return 0;
}

int main(void) {
    FILE *fp = abrir_archivo_seguro("datos.txt", "r");
    if (!fp) return 1;

    /* ... operaciones ... */

    cerrar_archivo_seguro(&fp);
    /* fp ahora es NULL - seguro llamar de nuevo */
    cerrar_archivo_seguro(&fp);  /* No hace nada */

    return 0;
}
```

### 2.2 Lectura de Archivos

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*
 * FUNCIONES DE LECTURA
 * ====================
 */

/* fgetc - Lee un carácter */
void leer_caracter_por_caracter(const char *archivo) {
    FILE *fp = fopen(archivo, "r");
    if (!fp) return;

    int ch;  /* int, no char - para detectar EOF */
    while ((ch = fgetc(fp)) != EOF) {
        putchar(ch);
    }

    /* Verificar si terminó por error o fin de archivo */
    if (ferror(fp)) {
        perror("Error de lectura");
    }

    fclose(fp);
}

/* fgets - Lee línea (RECOMENDADO para texto) */
void leer_linea_por_linea(const char *archivo) {
    FILE *fp = fopen(archivo, "r");
    if (!fp) return;

    char linea[1024];
    int num_linea = 0;

    /* fgets incluye '\n' si cabe en el buffer */
    while (fgets(linea, sizeof(linea), fp)) {
        num_linea++;

        /* Remover '\n' si existe */
        linea[strcspn(linea, "\n")] = '\0';

        printf("%4d: %s\n", num_linea, linea);
    }

    fclose(fp);
}

/* fread - Lectura binaria en bloques */
void leer_bloques_binarios(const char *archivo) {
    FILE *fp = fopen(archivo, "rb");
    if (!fp) return;

    unsigned char buffer[4096];
    size_t leidos;
    size_t total = 0;

    while ((leidos = fread(buffer, 1, sizeof(buffer), fp)) > 0) {
        total += leidos;
        /* Procesar buffer... */
    }

    printf("Total bytes leídos: %zu\n", total);
    fclose(fp);
}

/* fscanf - Lectura formateada (usar con cuidado) */
void leer_datos_formateados(const char *archivo) {
    FILE *fp = fopen(archivo, "r");
    if (!fp) return;

    char nombre[50];
    int edad;
    float salario;

    /* %49s previene buffer overflow */
    while (fscanf(fp, "%49s %d %f", nombre, &edad, &salario) == 3) {
        printf("Empleado: %s, Edad: %d, Salario: %.2f\n",
               nombre, edad, salario);
    }

    fclose(fp);
}

/* Leer archivo completo a memoria */
char *leer_archivo_completo(const char *archivo, size_t *tam) {
    FILE *fp = fopen(archivo, "rb");
    if (!fp) return NULL;

    /* Obtener tamaño */
    fseek(fp, 0, SEEK_END);
    long tamanio = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    if (tamanio < 0) {
        fclose(fp);
        return NULL;
    }

    /* Reservar memoria (+1 para null terminator si es texto) */
    char *contenido = malloc(tamanio + 1);
    if (!contenido) {
        fclose(fp);
        return NULL;
    }

    /* Leer todo */
    size_t leido = fread(contenido, 1, tamanio, fp);
    contenido[leido] = '\0';  /* Para uso como string */

    if (tam) *tam = leido;

    fclose(fp);
    return contenido;
}
```

### 2.3 Escritura de Archivos

```c
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

/*
 * FUNCIONES DE ESCRITURA
 * ======================
 */

/* fputc - Escribe un carácter */
void escribir_caracter(FILE *fp, int ch) {
    if (fputc(ch, fp) == EOF) {
        perror("Error escribiendo carácter");
    }
}

/* fputs - Escribe string (NO añade '\n') */
void escribir_lineas(const char *archivo) {
    FILE *fp = fopen(archivo, "w");
    if (!fp) return;

    fputs("Primera línea\n", fp);
    fputs("Segunda línea\n", fp);

    /* puts() añade '\n', fputs() NO */

    fclose(fp);
}

/* fprintf - Escritura formateada */
void escribir_formateado(const char *archivo) {
    FILE *fp = fopen(archivo, "w");
    if (!fp) return;

    /* Encabezado CSV */
    fprintf(fp, "ID,Nombre,Precio,Stock\n");

    /* Datos */
    fprintf(fp, "%d,%s,%.2f,%d\n", 1, "Widget", 19.99, 100);
    fprintf(fp, "%d,%s,%.2f,%d\n", 2, "Gadget", 29.99, 50);
    fprintf(fp, "%d,%s,%.2f,%d\n", 3, "Gizmo", 9.99, 200);

    fclose(fp);
}

/* fwrite - Escritura binaria */
void escribir_binario(const char *archivo) {
    FILE *fp = fopen(archivo, "wb");
    if (!fp) return;

    int datos[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    size_t n_elementos = sizeof(datos) / sizeof(datos[0]);

    size_t escritos = fwrite(datos, sizeof(int), n_elementos, fp);

    if (escritos != n_elementos) {
        fprintf(stderr, "Error: solo se escribieron %zu de %zu\n",
                escritos, n_elementos);
    }

    fclose(fp);
}

/* Logger personalizado con timestamp */
void log_mensaje(FILE *log, const char *nivel, const char *fmt, ...) {
    time_t ahora = time(NULL);
    struct tm *tm_info = localtime(&ahora);

    /* Timestamp */
    fprintf(log, "[%04d-%02d-%02d %02d:%02d:%02d] [%s] ",
            tm_info->tm_year + 1900, tm_info->tm_mon + 1, tm_info->tm_mday,
            tm_info->tm_hour, tm_info->tm_min, tm_info->tm_sec,
            nivel);

    /* Mensaje con argumentos variables */
    va_list args;
    va_start(args, fmt);
    vfprintf(log, fmt, args);
    va_end(args);

    fprintf(log, "\n");
    fflush(log);  /* Asegurar escritura inmediata */
}
```

### 2.4 Estado y Errores

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

/*
 * FUNCIONES DE ESTADO
 * ===================
 */

void verificar_estado_stream(FILE *fp) {
    /*
     * feof(fp)   - Retorna no-cero si se alcanzó EOF
     * ferror(fp) - Retorna no-cero si hubo error
     * clearerr() - Limpia indicadores EOF y error
     */

    if (feof(fp)) {
        printf("Se alcanzó el fin del archivo\n");
    }

    if (ferror(fp)) {
        printf("Error en el stream: %s\n", strerror(errno));
        clearerr(fp);  /* Limpiar para permitir reintentos */
    }
}

/* Patrón correcto para leer hasta EOF */
void leer_correctamente(FILE *fp) {
    int ch;

    /* INCORRECTO - feof() es true DESPUÉS de intentar leer más allá */
    /* while (!feof(fp)) { ch = fgetc(fp); ... } */

    /* CORRECTO - verificar resultado de la lectura */
    while ((ch = fgetc(fp)) != EOF) {
        putchar(ch);
    }

    /* Ahora verificar POR QUÉ terminó */
    if (ferror(fp)) {
        perror("Error durante lectura");
    }
    /* Si llegamos aquí sin ferror, fue EOF legítimo */
}

/* freopen - Redirigir stream */
void redirigir_salida(void) {
    /* Redirigir stdout a archivo */
    if (freopen("salida.txt", "w", stdout) == NULL) {
        perror("Error redirigiendo stdout");
        return;
    }

    printf("Esto va al archivo, no a la pantalla\n");

    /* Restaurar stdout (dependiente de plataforma) */
    #ifdef _WIN32
    freopen("CON", "w", stdout);
    #else
    freopen("/dev/tty", "w", stdout);
    #endif

    printf("Esto va a la pantalla\n");
}
```

---

## 3. I/O Binario

### 3.1 Lectura y Escritura de Estructuras

```c
#include <stdio.h>
#include <string.h>
#include <stdint.h>

/*
 * I/O BINARIO CON ESTRUCTURAS
 * ===========================
 *
 * Ventajas:
 *   - Rápido (sin parsing)
 *   - Compacto (sin overhead de texto)
 *   - Exacto (sin pérdida de precisión)
 *
 * Desventajas:
 *   - No portable entre arquitecturas
 *   - No legible por humanos
 *   - Sensible a cambios de estructura
 */

/* Estructura con padding predecible */
#pragma pack(push, 1)  /* Empaquetar sin padding */
typedef struct {
    uint32_t id;
    char nombre[32];
    uint8_t edad;
    float salario;  /* Cuidado: representación float varía */
} EmpleadoPacked;
#pragma pack(pop)

/* Guardar array de estructuras */
int guardar_empleados(const char *archivo,
                      EmpleadoPacked *empleados,
                      size_t cantidad) {
    FILE *fp = fopen(archivo, "wb");
    if (!fp) return -1;

    /* Escribir cantidad primero (header simple) */
    if (fwrite(&cantidad, sizeof(cantidad), 1, fp) != 1) {
        fclose(fp);
        return -1;
    }

    /* Escribir todas las estructuras */
    size_t escritos = fwrite(empleados, sizeof(EmpleadoPacked),
                              cantidad, fp);

    fclose(fp);
    return (escritos == cantidad) ? 0 : -1;
}

/* Cargar array de estructuras */
EmpleadoPacked *cargar_empleados(const char *archivo,
                                  size_t *cantidad) {
    FILE *fp = fopen(archivo, "rb");
    if (!fp) return NULL;

    /* Leer cantidad */
    size_t n;
    if (fread(&n, sizeof(n), 1, fp) != 1) {
        fclose(fp);
        return NULL;
    }

    /* Reservar memoria */
    EmpleadoPacked *empleados = malloc(n * sizeof(EmpleadoPacked));
    if (!empleados) {
        fclose(fp);
        return NULL;
    }

    /* Leer estructuras */
    size_t leidos = fread(empleados, sizeof(EmpleadoPacked), n, fp);
    fclose(fp);

    if (leidos != n) {
        free(empleados);
        return NULL;
    }

    *cantidad = n;
    return empleados;
}
```

### 3.2 Formato de Archivo Personalizado

```c
#include <stdio.h>
#include <stdint.h>
#include <string.h>

/*
 * DISEÑO DE FORMATO DE ARCHIVO BINARIO
 * =====================================
 *
 * Un buen formato binario incluye:
 * - Magic number (identificador único)
 * - Versión del formato
 * - Checksum o hash
 * - Metadatos (tamaño, cantidad de registros)
 * - Datos
 */

#define MAGIC_NUMBER 0x41524348  /* "ARCH" en little-endian */
#define VERSION_MAJOR 1
#define VERSION_MINOR 0

#pragma pack(push, 1)
typedef struct {
    uint32_t magic;          /* Identificador del formato */
    uint8_t version_major;   /* Versión mayor */
    uint8_t version_minor;   /* Versión menor */
    uint16_t flags;          /* Flags de configuración */
    uint32_t record_count;   /* Número de registros */
    uint32_t data_offset;    /* Offset donde empiezan los datos */
    uint32_t checksum;       /* CRC32 de los datos */
    uint8_t reserved[12];    /* Reservado para futuro uso */
} FileHeader;

typedef struct {
    uint32_t id;
    uint32_t timestamp;
    uint16_t type;
    uint16_t length;
    /* Datos variables después */
} RecordHeader;
#pragma pack(pop)

/* Calcular checksum simple (CRC32 real sería mejor) */
uint32_t calcular_checksum(const void *data, size_t len) {
    const uint8_t *bytes = data;
    uint32_t sum = 0;
    for (size_t i = 0; i < len; i++) {
        sum = ((sum << 5) + sum) + bytes[i];  /* hash * 33 + c */
    }
    return sum;
}

/* Escribir archivo con formato personalizado */
int escribir_archivo_formato(const char *nombre,
                              void *datos, size_t tam_datos,
                              uint32_t n_registros) {
    FILE *fp = fopen(nombre, "wb");
    if (!fp) return -1;

    /* Preparar header */
    FileHeader header = {
        .magic = MAGIC_NUMBER,
        .version_major = VERSION_MAJOR,
        .version_minor = VERSION_MINOR,
        .flags = 0,
        .record_count = n_registros,
        .data_offset = sizeof(FileHeader),
        .checksum = calcular_checksum(datos, tam_datos)
    };
    memset(header.reserved, 0, sizeof(header.reserved));

    /* Escribir header */
    if (fwrite(&header, sizeof(header), 1, fp) != 1) {
        fclose(fp);
        return -1;
    }

    /* Escribir datos */
    if (fwrite(datos, tam_datos, 1, fp) != 1) {
        fclose(fp);
        return -1;
    }

    fclose(fp);
    return 0;
}

/* Leer y validar archivo */
void *leer_archivo_formato(const char *nombre,
                           FileHeader *header_out,
                           size_t *tam_datos) {
    FILE *fp = fopen(nombre, "rb");
    if (!fp) return NULL;

    FileHeader header;
    if (fread(&header, sizeof(header), 1, fp) != 1) {
        fclose(fp);
        return NULL;
    }

    /* Validar magic number */
    if (header.magic != MAGIC_NUMBER) {
        fprintf(stderr, "Formato de archivo inválido\n");
        fclose(fp);
        return NULL;
    }

    /* Verificar versión compatible */
    if (header.version_major > VERSION_MAJOR) {
        fprintf(stderr, "Versión de archivo no soportada: %d.%d\n",
                header.version_major, header.version_minor);
        fclose(fp);
        return NULL;
    }

    /* Calcular tamaño de datos */
    fseek(fp, 0, SEEK_END);
    long tam_archivo = ftell(fp);
    size_t datos_size = tam_archivo - header.data_offset;

    /* Leer datos */
    fseek(fp, header.data_offset, SEEK_SET);
    void *datos = malloc(datos_size);
    if (!datos) {
        fclose(fp);
        return NULL;
    }

    if (fread(datos, datos_size, 1, fp) != 1) {
        free(datos);
        fclose(fp);
        return NULL;
    }

    /* Verificar checksum */
    uint32_t checksum = calcular_checksum(datos, datos_size);
    if (checksum != header.checksum) {
        fprintf(stderr, "Checksum inválido - archivo corrupto\n");
        free(datos);
        fclose(fp);
        return NULL;
    }

    fclose(fp);

    if (header_out) *header_out = header;
    if (tam_datos) *tam_datos = datos_size;

    return datos;
}
```

---

## 4. Acceso Aleatorio

### 4.1 Posicionamiento con fseek/ftell

```c
#include <stdio.h>
#include <stdint.h>

/*
 * ACCESO ALEATORIO (Random Access)
 * =================================
 *
 * fseek(fp, offset, whence):
 *   SEEK_SET - Desde inicio del archivo
 *   SEEK_CUR - Desde posición actual
 *   SEEK_END - Desde final del archivo
 *
 * ftell(fp):
 *   Retorna posición actual (long)
 *
 * rewind(fp):
 *   Equivale a fseek(fp, 0, SEEK_SET) + clearerr(fp)
 *
 * fgetpos/fsetpos:
 *   Para archivos > 2GB (usa fpos_t)
 */

/* Base de datos simple con registros de tamaño fijo */
#define RECORD_SIZE 64

typedef struct {
    uint32_t id;
    char data[60];
} FixedRecord;

/* Ir a un registro específico */
int ir_a_registro(FILE *fp, uint32_t num_registro) {
    long offset = (long)num_registro * RECORD_SIZE;
    return fseek(fp, offset, SEEK_SET);
}

/* Leer registro por número */
int leer_registro(FILE *fp, uint32_t num, FixedRecord *rec) {
    if (ir_a_registro(fp, num) != 0) return -1;
    return (fread(rec, sizeof(*rec), 1, fp) == 1) ? 0 : -1;
}

/* Escribir registro por número */
int escribir_registro(FILE *fp, uint32_t num, const FixedRecord *rec) {
    if (ir_a_registro(fp, num) != 0) return -1;
    return (fwrite(rec, sizeof(*rec), 1, fp) == 1) ? 0 : -1;
}

/* Obtener cantidad de registros */
uint32_t contar_registros(FILE *fp) {
    long pos_actual = ftell(fp);  /* Guardar posición */

    fseek(fp, 0, SEEK_END);
    long tam = ftell(fp);

    fseek(fp, pos_actual, SEEK_SET);  /* Restaurar posición */

    return (uint32_t)(tam / RECORD_SIZE);
}

/* Ejemplo de uso */
void ejemplo_base_datos(void) {
    FILE *db = fopen("registros.db", "r+b");
    if (!db) {
        /* Crear si no existe */
        db = fopen("registros.db", "w+b");
        if (!db) return;
    }

    printf("Registros en DB: %u\n", contar_registros(db));

    /* Escribir registro #5 */
    FixedRecord nuevo = {.id = 5, .data = "Datos del registro 5"};
    escribir_registro(db, 5, &nuevo);

    /* Leer registro #5 */
    FixedRecord leido;
    if (leer_registro(db, 5, &leido) == 0) {
        printf("Registro %u: %s\n", leido.id, leido.data);
    }

    fclose(db);
}
```

### 4.2 Archivos Grandes (> 2GB)

```c
#include <stdio.h>

/*
 * MANEJO DE ARCHIVOS GRANDES
 * ==========================
 *
 * ftell/fseek usan 'long' que es 32 bits en muchos sistemas.
 * Para archivos > 2GB:
 *
 * Opción 1: fgetpos/fsetpos (portable)
 * Opción 2: _fseeki64/_ftelli64 (Windows)
 * Opción 3: fseeko/ftello (POSIX con _FILE_OFFSET_BITS=64)
 */

/* Método portable con fgetpos/fsetpos */
void posicion_archivos_grandes(FILE *fp) {
    fpos_t posicion;

    /* Guardar posición actual */
    if (fgetpos(fp, &posicion) != 0) {
        perror("Error obteniendo posición");
        return;
    }

    /* ... operaciones ... */

    /* Restaurar posición */
    if (fsetpos(fp, &posicion) != 0) {
        perror("Error restaurando posición");
    }
}

#ifdef _WIN32
/* Windows: usar funciones específicas */
#include <io.h>

int64_t obtener_posicion_win(FILE *fp) {
    return _ftelli64(fp);
}

int ir_a_posicion_win(FILE *fp, int64_t offset) {
    return _fseeki64(fp, offset, SEEK_SET);
}

#else
/* POSIX: compilar con -D_FILE_OFFSET_BITS=64 */
#define _FILE_OFFSET_BITS 64
#include <stdio.h>

off_t obtener_posicion_posix(FILE *fp) {
    return ftello(fp);
}

int ir_a_posicion_posix(FILE *fp, off_t offset) {
    return fseeko(fp, offset, SEEK_SET);
}
#endif
```

---

## 5. Serialización

### 5.1 Serialización Portable

```c
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <arpa/inet.h>  /* htonl, ntohl - o implementar manualmente */

/*
 * SERIALIZACIÓN PORTABLE
 * ======================
 *
 * Problemas de portabilidad:
 * - Endianness (byte order)
 * - Tamaño de tipos (int puede ser 16, 32, o 64 bits)
 * - Padding de estructuras
 * - Representación de flotantes
 *
 * Solución: serializar/deserializar campo por campo
 */

/* Funciones para escribir en formato big-endian (network order) */
void write_u8(FILE *fp, uint8_t val) {
    fputc(val, fp);
}

void write_u16(FILE *fp, uint16_t val) {
    fputc((val >> 8) & 0xFF, fp);
    fputc(val & 0xFF, fp);
}

void write_u32(FILE *fp, uint32_t val) {
    fputc((val >> 24) & 0xFF, fp);
    fputc((val >> 16) & 0xFF, fp);
    fputc((val >> 8) & 0xFF, fp);
    fputc(val & 0xFF, fp);
}

void write_string(FILE *fp, const char *str, uint16_t max_len) {
    uint16_t len = (uint16_t)strlen(str);
    if (len > max_len) len = max_len;

    write_u16(fp, len);
    fwrite(str, 1, len, fp);
}

/* Funciones para leer en formato big-endian */
uint8_t read_u8(FILE *fp) {
    return (uint8_t)fgetc(fp);
}

uint16_t read_u16(FILE *fp) {
    uint16_t val = 0;
    val |= (uint16_t)fgetc(fp) << 8;
    val |= (uint16_t)fgetc(fp);
    return val;
}

uint32_t read_u32(FILE *fp) {
    uint32_t val = 0;
    val |= (uint32_t)fgetc(fp) << 24;
    val |= (uint32_t)fgetc(fp) << 16;
    val |= (uint32_t)fgetc(fp) << 8;
    val |= (uint32_t)fgetc(fp);
    return val;
}

char *read_string(FILE *fp, char *buffer, uint16_t buf_size) {
    uint16_t len = read_u16(fp);
    if (len >= buf_size) len = buf_size - 1;

    fread(buffer, 1, len, fp);
    buffer[len] = '\0';
    return buffer;
}

/* Ejemplo: estructura compleja */
typedef struct {
    uint32_t id;
    char nombre[50];
    uint8_t edad;
    uint32_t salario_centavos;  /* Evitar float para precisión */
} Persona;

void serializar_persona(FILE *fp, const Persona *p) {
    write_u32(fp, p->id);
    write_string(fp, p->nombre, 50);
    write_u8(fp, p->edad);
    write_u32(fp, p->salario_centavos);
}

void deserializar_persona(FILE *fp, Persona *p) {
    p->id = read_u32(fp);
    read_string(fp, p->nombre, sizeof(p->nombre));
    p->edad = read_u8(fp);
    p->salario_centavos = read_u32(fp);
}
```

### 5.2 Serialización de Estructuras Anidadas

```c
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/*
 * SERIALIZACIÓN DE DATOS COMPLEJOS
 * =================================
 * - Arrays de tamaño variable
 * - Punteros (referencias)
 * - Estructuras anidadas
 */

typedef struct {
    char nombre[32];
    uint32_t cantidad;
    uint32_t precio_centavos;
} Producto;

typedef struct {
    uint32_t id_pedido;
    uint32_t fecha_unix;
    uint32_t n_productos;
    Producto *productos;  /* Array dinámico */
    char notas[256];
} Pedido;

/* Serializar pedido completo */
int serializar_pedido(FILE *fp, const Pedido *p) {
    write_u32(fp, p->id_pedido);
    write_u32(fp, p->fecha_unix);
    write_u32(fp, p->n_productos);

    /* Serializar array de productos */
    for (uint32_t i = 0; i < p->n_productos; i++) {
        write_string(fp, p->productos[i].nombre, 32);
        write_u32(fp, p->productos[i].cantidad);
        write_u32(fp, p->productos[i].precio_centavos);
    }

    write_string(fp, p->notas, 256);

    return ferror(fp) ? -1 : 0;
}

/* Deserializar pedido */
Pedido *deserializar_pedido(FILE *fp) {
    Pedido *p = calloc(1, sizeof(Pedido));
    if (!p) return NULL;

    p->id_pedido = read_u32(fp);
    p->fecha_unix = read_u32(fp);
    p->n_productos = read_u32(fp);

    /* Validar antes de malloc */
    if (p->n_productos > 10000) {  /* Límite razonable */
        free(p);
        return NULL;
    }

    p->productos = calloc(p->n_productos, sizeof(Producto));
    if (!p->productos) {
        free(p);
        return NULL;
    }

    for (uint32_t i = 0; i < p->n_productos; i++) {
        read_string(fp, p->productos[i].nombre,
                    sizeof(p->productos[i].nombre));
        p->productos[i].cantidad = read_u32(fp);
        p->productos[i].precio_centavos = read_u32(fp);
    }

    read_string(fp, p->notas, sizeof(p->notas));

    if (ferror(fp)) {
        free(p->productos);
        free(p);
        return NULL;
    }

    return p;
}

void liberar_pedido(Pedido *p) {
    if (p) {
        free(p->productos);
        free(p);
    }
}
```

---

## 6. I/O de Bajo Nivel

### 6.1 File Descriptors (POSIX)

```c
/*
 * I/O DE BAJO NIVEL (POSIX)
 * =========================
 *
 * Usa file descriptors (int) en lugar de FILE*
 * Más control pero menos portable
 *
 * Descriptores estándar:
 *   0 - stdin
 *   1 - stdout
 *   2 - stderr
 */

#ifndef _WIN32  /* Solo POSIX */

#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>

/* Abrir archivo */
int abrir_bajo_nivel(const char *archivo, int modo) {
    /*
     * Flags comunes:
     *   O_RDONLY   - Solo lectura
     *   O_WRONLY   - Solo escritura
     *   O_RDWR     - Lectura y escritura
     *   O_CREAT    - Crear si no existe
     *   O_TRUNC    - Truncar si existe
     *   O_APPEND   - Escribir al final
     *   O_EXCL     - Fallar si existe (con O_CREAT)
     */

    int fd = open(archivo, modo, 0644);  /* permisos rw-r--r-- */

    if (fd < 0) {
        fprintf(stderr, "Error abriendo %s: %s\n",
                archivo, strerror(errno));
    }

    return fd;
}

/* Lectura de bajo nivel */
ssize_t leer_bajo_nivel(int fd, void *buffer, size_t tam) {
    ssize_t total = 0;
    char *buf = buffer;

    /* read() puede leer menos de lo solicitado */
    while (total < (ssize_t)tam) {
        ssize_t n = read(fd, buf + total, tam - total);

        if (n < 0) {
            if (errno == EINTR) continue;  /* Interrumpido, reintentar */
            return -1;  /* Error real */
        }
        if (n == 0) break;  /* EOF */

        total += n;
    }

    return total;
}

/* Escritura de bajo nivel */
ssize_t escribir_bajo_nivel(int fd, const void *buffer, size_t tam) {
    ssize_t total = 0;
    const char *buf = buffer;

    while (total < (ssize_t)tam) {
        ssize_t n = write(fd, buf + total, tam - total);

        if (n < 0) {
            if (errno == EINTR) continue;
            return -1;
        }

        total += n;
    }

    return total;
}

/* Copiar archivo eficientemente */
int copiar_archivo(const char *origen, const char *destino) {
    int fd_in = open(origen, O_RDONLY);
    if (fd_in < 0) return -1;

    int fd_out = open(destino, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd_out < 0) {
        close(fd_in);
        return -1;
    }

    char buffer[65536];  /* Buffer grande para eficiencia */
    ssize_t n;

    while ((n = read(fd_in, buffer, sizeof(buffer))) > 0) {
        if (write(fd_out, buffer, n) != n) {
            close(fd_in);
            close(fd_out);
            return -1;
        }
    }

    close(fd_in);
    close(fd_out);

    return (n < 0) ? -1 : 0;
}

/* Convertir entre FILE* y fd */
void conversion_fd_file(void) {
    /* FILE* -> fd */
    FILE *fp = fopen("archivo.txt", "r");
    int fd = fileno(fp);  /* Obtener fd subyacente */

    /* fd -> FILE* */
    int fd2 = open("otro.txt", O_RDONLY);
    FILE *fp2 = fdopen(fd2, "r");  /* Crear FILE* desde fd */

    /* Nota: fclose() cierra el fd subyacente */
    fclose(fp);
    fclose(fp2);
}

#endif /* POSIX */
```

### 6.2 File Descriptors (Windows)

```c
#ifdef _WIN32

#include <windows.h>
#include <io.h>
#include <fcntl.h>

/*
 * WINDOWS I/O DE BAJO NIVEL
 * =========================
 *
 * Windows tiene dos APIs:
 * 1. CRT (_open, _read, _write) - similar a POSIX
 * 2. Win32 API (CreateFile, ReadFile, WriteFile) - más potente
 */

/* Usando CRT (compatible con código POSIX) */
void ejemplo_crt_windows(void) {
    int fd = _open("archivo.txt", _O_RDONLY | _O_BINARY);
    if (fd < 0) return;

    char buffer[1024];
    int leidos = _read(fd, buffer, sizeof(buffer));

    _close(fd);
}

/* Usando Win32 API (más control) */
void ejemplo_win32_api(void) {
    HANDLE hFile = CreateFile(
        "archivo.txt",
        GENERIC_READ,
        FILE_SHARE_READ,
        NULL,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        NULL
    );

    if (hFile == INVALID_HANDLE_VALUE) {
        DWORD err = GetLastError();
        printf("Error: %lu\n", err);
        return;
    }

    char buffer[1024];
    DWORD leidos;

    if (ReadFile(hFile, buffer, sizeof(buffer), &leidos, NULL)) {
        printf("Leídos: %lu bytes\n", leidos);
    }

    CloseHandle(hFile);
}

#endif /* _WIN32 */
```

---

## 7. Manejo de Directorios

### 7.1 Operaciones con Directorios (POSIX)

```c
#ifndef _WIN32

#include <dirent.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * OPERACIONES CON DIRECTORIOS (POSIX)
 * ====================================
 */

/* Listar contenido de directorio */
void listar_directorio(const char *ruta) {
    DIR *dir = opendir(ruta);
    if (!dir) {
        perror("Error abriendo directorio");
        return;
    }

    struct dirent *entrada;

    while ((entrada = readdir(dir)) != NULL) {
        /* Saltar . y .. */
        if (strcmp(entrada->d_name, ".") == 0 ||
            strcmp(entrada->d_name, "..") == 0) {
            continue;
        }

        /* Construir ruta completa */
        char ruta_completa[PATH_MAX];
        snprintf(ruta_completa, sizeof(ruta_completa),
                 "%s/%s", ruta, entrada->d_name);

        /* Obtener información del archivo */
        struct stat info;
        if (stat(ruta_completa, &info) == 0) {
            char tipo = S_ISDIR(info.st_mode) ? 'd' :
                        S_ISREG(info.st_mode) ? '-' : '?';

            printf("%c %10ld %s\n", tipo, info.st_size, entrada->d_name);
        }
    }

    closedir(dir);
}

/* Crear directorio */
int crear_directorio(const char *ruta) {
    if (mkdir(ruta, 0755) != 0) {  /* rwxr-xr-x */
        perror("Error creando directorio");
        return -1;
    }
    return 0;
}

/* Crear directorios recursivamente (como mkdir -p) */
int mkdir_recursivo(const char *ruta) {
    char tmp[PATH_MAX];
    char *p = NULL;
    size_t len;

    snprintf(tmp, sizeof(tmp), "%s", ruta);
    len = strlen(tmp);

    /* Remover trailing slash */
    if (tmp[len - 1] == '/') {
        tmp[len - 1] = '\0';
    }

    /* Crear cada nivel */
    for (p = tmp + 1; *p; p++) {
        if (*p == '/') {
            *p = '\0';
            mkdir(tmp, 0755);  /* Ignorar error si ya existe */
            *p = '/';
        }
    }

    return mkdir(tmp, 0755);
}

/* Recorrer directorio recursivamente */
void recorrer_recursivo(const char *ruta, int profundidad) {
    DIR *dir = opendir(ruta);
    if (!dir) return;

    struct dirent *entrada;
    char ruta_completa[PATH_MAX];

    while ((entrada = readdir(dir)) != NULL) {
        if (entrada->d_name[0] == '.') continue;  /* Ocultos */

        snprintf(ruta_completa, sizeof(ruta_completa),
                 "%s/%s", ruta, entrada->d_name);

        struct stat info;
        if (stat(ruta_completa, &info) != 0) continue;

        /* Indentar según profundidad */
        for (int i = 0; i < profundidad; i++) printf("  ");
        printf("%s\n", entrada->d_name);

        if (S_ISDIR(info.st_mode)) {
            recorrer_recursivo(ruta_completa, profundidad + 1);
        }
    }

    closedir(dir);
}

/* Eliminar directorio y contenido */
int eliminar_directorio_recursivo(const char *ruta) {
    DIR *dir = opendir(ruta);
    if (!dir) return -1;

    struct dirent *entrada;
    char ruta_completa[PATH_MAX];

    while ((entrada = readdir(dir)) != NULL) {
        if (strcmp(entrada->d_name, ".") == 0 ||
            strcmp(entrada->d_name, "..") == 0) {
            continue;
        }

        snprintf(ruta_completa, sizeof(ruta_completa),
                 "%s/%s", ruta, entrada->d_name);

        struct stat info;
        if (stat(ruta_completa, &info) != 0) continue;

        if (S_ISDIR(info.st_mode)) {
            eliminar_directorio_recursivo(ruta_completa);
        } else {
            unlink(ruta_completa);
        }
    }

    closedir(dir);
    return rmdir(ruta);
}

#endif /* POSIX */
```

### 7.2 Operaciones con Directorios (Windows)

```c
#ifdef _WIN32

#include <windows.h>
#include <stdio.h>
#include <string.h>

/*
 * OPERACIONES CON DIRECTORIOS (WINDOWS)
 * ======================================
 */

/* Listar directorio */
void listar_directorio_win(const char *ruta) {
    WIN32_FIND_DATA findData;
    char patron[MAX_PATH];

    snprintf(patron, sizeof(patron), "%s\\*", ruta);

    HANDLE hFind = FindFirstFile(patron, &findData);
    if (hFind == INVALID_HANDLE_VALUE) {
        printf("Error: %lu\n", GetLastError());
        return;
    }

    do {
        if (strcmp(findData.cFileName, ".") == 0 ||
            strcmp(findData.cFileName, "..") == 0) {
            continue;
        }

        char tipo = (findData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
                    ? 'd' : '-';

        LARGE_INTEGER tam;
        tam.LowPart = findData.nFileSizeLow;
        tam.HighPart = findData.nFileSizeHigh;

        printf("%c %10lld %s\n", tipo, tam.QuadPart, findData.cFileName);

    } while (FindNextFile(hFind, &findData));

    FindClose(hFind);
}

/* Crear directorio */
int crear_directorio_win(const char *ruta) {
    if (!CreateDirectory(ruta, NULL)) {
        DWORD err = GetLastError();
        if (err != ERROR_ALREADY_EXISTS) {
            printf("Error creando directorio: %lu\n", err);
            return -1;
        }
    }
    return 0;
}

#endif /* _WIN32 */
```

---

## 8. Memory-Mapped Files

### 8.1 mmap (POSIX)

```c
#ifndef _WIN32

#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>

/*
 * MEMORY-MAPPED FILES (POSIX)
 * ===========================
 *
 * Ventajas:
 * - Acceso como si fuera memoria (punteros)
 * - El kernel maneja buffering automáticamente
 * - Eficiente para archivos grandes
 * - Permite compartir memoria entre procesos
 *
 * Ideal para:
 * - Bases de datos
 * - Procesamiento de archivos grandes
 * - IPC (comunicación inter-procesos)
 */

typedef struct {
    void *data;
    size_t size;
    int fd;
} MappedFile;

/* Mapear archivo a memoria */
MappedFile *mapear_archivo(const char *nombre, int escribir) {
    int flags = escribir ? O_RDWR : O_RDONLY;
    int fd = open(nombre, flags);
    if (fd < 0) return NULL;

    /* Obtener tamaño */
    struct stat st;
    if (fstat(fd, &st) < 0) {
        close(fd);
        return NULL;
    }

    /* Mapear */
    int prot = PROT_READ | (escribir ? PROT_WRITE : 0);
    void *data = mmap(NULL, st.st_size, prot, MAP_SHARED, fd, 0);

    if (data == MAP_FAILED) {
        close(fd);
        return NULL;
    }

    MappedFile *mf = malloc(sizeof(MappedFile));
    mf->data = data;
    mf->size = st.st_size;
    mf->fd = fd;

    return mf;
}

/* Desmapear */
void desmapear_archivo(MappedFile *mf) {
    if (mf) {
        munmap(mf->data, mf->size);
        close(mf->fd);
        free(mf);
    }
}

/* Ejemplo: contar palabras en archivo mapeado */
size_t contar_palabras_mmap(const char *archivo) {
    MappedFile *mf = mapear_archivo(archivo, 0);
    if (!mf) return 0;

    const char *p = mf->data;
    size_t palabras = 0;
    int en_palabra = 0;

    for (size_t i = 0; i < mf->size; i++) {
        int es_espacio = (p[i] == ' ' || p[i] == '\n' ||
                          p[i] == '\t' || p[i] == '\r');

        if (!es_espacio && !en_palabra) {
            palabras++;
            en_palabra = 1;
        } else if (es_espacio) {
            en_palabra = 0;
        }
    }

    desmapear_archivo(mf);
    return palabras;
}

/* Crear archivo mapeado de tamaño fijo */
MappedFile *crear_archivo_mapeado(const char *nombre, size_t tam) {
    int fd = open(nombre, O_RDWR | O_CREAT | O_TRUNC, 0644);
    if (fd < 0) return NULL;

    /* Establecer tamaño */
    if (ftruncate(fd, tam) < 0) {
        close(fd);
        return NULL;
    }

    void *data = mmap(NULL, tam, PROT_READ | PROT_WRITE,
                      MAP_SHARED, fd, 0);

    if (data == MAP_FAILED) {
        close(fd);
        return NULL;
    }

    MappedFile *mf = malloc(sizeof(MappedFile));
    mf->data = data;
    mf->size = tam;
    mf->fd = fd;

    return mf;
}

/* Sincronizar cambios a disco */
int sync_archivo_mapeado(MappedFile *mf) {
    return msync(mf->data, mf->size, MS_SYNC);
}

#endif /* POSIX */
```

### 8.2 Memory-Mapped Files (Windows)

```c
#ifdef _WIN32

#include <windows.h>
#include <stdio.h>

/*
 * MEMORY-MAPPED FILES (WINDOWS)
 * =============================
 */

typedef struct {
    void *data;
    size_t size;
    HANDLE hFile;
    HANDLE hMapping;
} MappedFileWin;

MappedFileWin *mapear_archivo_win(const char *nombre, int escribir) {
    DWORD access = escribir ? GENERIC_READ | GENERIC_WRITE : GENERIC_READ;
    DWORD share = FILE_SHARE_READ;

    HANDLE hFile = CreateFile(nombre, access, share, NULL,
                               OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

    if (hFile == INVALID_HANDLE_VALUE) return NULL;

    /* Obtener tamaño */
    LARGE_INTEGER size;
    if (!GetFileSizeEx(hFile, &size)) {
        CloseHandle(hFile);
        return NULL;
    }

    /* Crear mapping */
    DWORD prot = escribir ? PAGE_READWRITE : PAGE_READONLY;
    HANDLE hMapping = CreateFileMapping(hFile, NULL, prot, 0, 0, NULL);

    if (!hMapping) {
        CloseHandle(hFile);
        return NULL;
    }

    /* Mapear vista */
    DWORD mapAccess = escribir ? FILE_MAP_WRITE : FILE_MAP_READ;
    void *data = MapViewOfFile(hMapping, mapAccess, 0, 0, 0);

    if (!data) {
        CloseHandle(hMapping);
        CloseHandle(hFile);
        return NULL;
    }

    MappedFileWin *mf = malloc(sizeof(MappedFileWin));
    mf->data = data;
    mf->size = (size_t)size.QuadPart;
    mf->hFile = hFile;
    mf->hMapping = hMapping;

    return mf;
}

void desmapear_archivo_win(MappedFileWin *mf) {
    if (mf) {
        UnmapViewOfFile(mf->data);
        CloseHandle(mf->hMapping);
        CloseHandle(mf->hFile);
        free(mf);
    }
}

/* Sincronizar cambios */
int sync_archivo_win(MappedFileWin *mf) {
    return FlushViewOfFile(mf->data, mf->size) ? 0 : -1;
}

#endif /* _WIN32 */
```

---

## Referencia Rápida

### Funciones de stdio.h

| Función | Descripción |
|---------|-------------|
| `fopen()` | Abrir archivo |
| `fclose()` | Cerrar archivo |
| `fread()` | Lectura binaria |
| `fwrite()` | Escritura binaria |
| `fgetc()` | Leer carácter |
| `fputc()` | Escribir carácter |
| `fgets()` | Leer línea |
| `fputs()` | Escribir string |
| `fprintf()` | Escritura formateada |
| `fscanf()` | Lectura formateada |
| `fseek()` | Posicionar cursor |
| `ftell()` | Obtener posición |
| `rewind()` | Ir al inicio |
| `fflush()` | Vaciar buffer |
| `feof()` | Verificar EOF |
| `ferror()` | Verificar error |
| `clearerr()` | Limpiar errores |
| `setvbuf()` | Configurar buffer |

### Modos de Apertura

```
"r"   - Lectura (debe existir)
"w"   - Escritura (crea/trunca)
"a"   - Append (crea si no existe)
"r+"  - Lectura/Escritura (debe existir)
"w+"  - Lectura/Escritura (crea/trunca)
"a+"  - Lectura/Append (crea si no existe)
"rb"  - Modo binario lectura
"wb"  - Modo binario escritura
```

---

## Ejercicios Propuestos

### Ejercicio 1: Parser de Configuración
Implementar parser que lea archivos `.ini` con secciones `[seccion]` y pares `clave=valor`.

### Ejercicio 2: Base de Datos Simple
Crear una base de datos de registros fijos con soporte para CRUD y búsqueda por índice.

### Ejercicio 3: Copiador de Archivos
Implementar una utilidad que copie archivos con barra de progreso y manejo de errores.

### Ejercicio 4: Serializador JSON Simple
Crear funciones para serializar/deserializar estructuras C a formato JSON básico.

### Ejercicio 5: Watch de Directorio
Implementar un programa que monitoree un directorio y reporte cambios en archivos.

---

## Conclusión

El manejo de archivos en C ofrece control total sobre la entrada/salida de datos. Desde los streams de alto nivel de `stdio.h` hasta el acceso directo con file descriptors y memory-mapped files, C proporciona las herramientas para cualquier necesidad de I/O.

**Puntos clave:**
1. Usar buffering apropiado según el caso de uso
2. Siempre verificar errores en operaciones I/O
3. Cerrar archivos correctamente para evitar pérdida de datos
4. Considerar portabilidad al diseñar formatos binarios
5. Memory-mapped files para archivos grandes o acceso aleatorio intensivo

---

*"El disco es lento, la memoria es rápida, pero el programador sabio conoce cuándo usar cada uno."*

# C_05: PROGRAMACIÓN AVANZADA EN C

> *"En las profundidades del sistema, donde los hilos se entrelazan y las señales reverberan, el maestro de C encuentra su verdadero poder."*

## Índice
1. [Multithreading](#1-multithreading)
2. [Señales](#2-señales)
3. [Gestión de Procesos](#3-gestión-de-procesos)
4. [Comunicación Inter-Procesos](#4-comunicación-inter-procesos)
5. [Optimización y Performance](#5-optimización-y-performance)
6. [Inline Assembly](#6-inline-assembly)
7. [Debugging y Profiling](#7-debugging-y-profiling)
8. [Patrones de Diseño en C](#8-patrones-de-diseño-en-c)

---

## 1. Multithreading

### 1.1 POSIX Threads (pthreads)

```c
/*
 * PTHREADS - POSIX THREADS
 * ========================
 *
 * Compilar con: gcc -pthread programa.c
 *
 * Conceptos clave:
 * - pthread_t: identificador de thread
 * - Función de thread: void* func(void* arg)
 * - Sincronización: mutex, condiciones, barreras
 */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* Estructura para pasar datos al thread */
typedef struct {
    int id;
    int inicio;
    int fin;
    long resultado;
} ThreadData;

/* Función ejecutada por cada thread */
void *calcular_suma(void *arg) {
    ThreadData *data = (ThreadData *)arg;
    long suma = 0;

    for (int i = data->inicio; i <= data->fin; i++) {
        suma += i;
    }

    data->resultado = suma;
    printf("Thread %d: suma de %d a %d = %ld\n",
           data->id, data->inicio, data->fin, suma);

    return NULL;
}

int main(void) {
    const int NUM_THREADS = 4;
    const int RANGO = 1000000;

    pthread_t threads[NUM_THREADS];
    ThreadData datos[NUM_THREADS];

    /* Crear threads */
    for (int i = 0; i < NUM_THREADS; i++) {
        datos[i].id = i;
        datos[i].inicio = i * (RANGO / NUM_THREADS) + 1;
        datos[i].fin = (i + 1) * (RANGO / NUM_THREADS);

        int rc = pthread_create(&threads[i], NULL,
                                 calcular_suma, &datos[i]);
        if (rc != 0) {
            fprintf(stderr, "Error creando thread %d\n", i);
            exit(1);
        }
    }

    /* Esperar a que terminen */
    long total = 0;
    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_join(threads[i], NULL);
        total += datos[i].resultado;
    }

    printf("Suma total: %ld\n", total);
    return 0;
}
```

### 1.2 Mutex y Sincronización

```c
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * MUTEX - EXCLUSIÓN MUTUA
 * =======================
 *
 * Protege secciones críticas donde múltiples threads
 * acceden a recursos compartidos.
 */

/* Variables globales compartidas */
int contador = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *incrementar_con_mutex(void *arg) {
    int id = *(int *)arg;

    for (int i = 0; i < 100000; i++) {
        pthread_mutex_lock(&mutex);   /* INICIO sección crítica */
        contador++;
        pthread_mutex_unlock(&mutex); /* FIN sección crítica */
    }

    printf("Thread %d terminó\n", id);
    return NULL;
}

/* Sin mutex - RACE CONDITION (MAL) */
void *incrementar_sin_mutex(void *arg) {
    for (int i = 0; i < 100000; i++) {
        contador++;  /* ¡PELIGRO! Race condition */
    }
    return NULL;
}

/* Mutex con timeout (try_lock) */
void *intentar_lock(void *arg) {
    int intentos = 0;

    while (pthread_mutex_trylock(&mutex) != 0) {
        intentos++;
        if (intentos > 1000) {
            printf("No se pudo obtener lock después de 1000 intentos\n");
            return NULL;
        }
        usleep(1000);  /* Esperar 1ms */
    }

    /* Tenemos el lock */
    contador++;
    pthread_mutex_unlock(&mutex);

    return NULL;
}

/* Mutex recursivo (mismo thread puede lock múltiples veces) */
pthread_mutex_t mutex_recursivo;

void inicializar_mutex_recursivo(void) {
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(&mutex_recursivo, &attr);
    pthread_mutexattr_destroy(&attr);
}

void funcion_recursiva(int n) {
    pthread_mutex_lock(&mutex_recursivo);

    if (n > 0) {
        printf("Nivel %d\n", n);
        funcion_recursiva(n - 1);  /* Mismo thread hace lock otra vez */
    }

    pthread_mutex_unlock(&mutex_recursivo);
}
```

### 1.3 Variables de Condición

```c
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/*
 * VARIABLES DE CONDICIÓN
 * ======================
 *
 * Permiten que threads esperen hasta que se cumpla
 * una condición específica.
 *
 * Patrón Productor-Consumidor
 */

#define BUFFER_SIZE 10

typedef struct {
    int buffer[BUFFER_SIZE];
    int count;
    int in;   /* Índice para producir */
    int out;  /* Índice para consumir */

    pthread_mutex_t mutex;
    pthread_cond_t no_lleno;   /* Señal: hay espacio */
    pthread_cond_t no_vacio;   /* Señal: hay datos */

    bool terminado;
} Buffer;

Buffer *crear_buffer(void) {
    Buffer *b = calloc(1, sizeof(Buffer));
    pthread_mutex_init(&b->mutex, NULL);
    pthread_cond_init(&b->no_lleno, NULL);
    pthread_cond_init(&b->no_vacio, NULL);
    return b;
}

void destruir_buffer(Buffer *b) {
    pthread_mutex_destroy(&b->mutex);
    pthread_cond_destroy(&b->no_lleno);
    pthread_cond_destroy(&b->no_vacio);
    free(b);
}

void producir(Buffer *b, int item) {
    pthread_mutex_lock(&b->mutex);

    /* Esperar mientras buffer esté lleno */
    while (b->count == BUFFER_SIZE) {
        pthread_cond_wait(&b->no_lleno, &b->mutex);
        /* wait: libera mutex, espera señal, re-adquiere mutex */
    }

    /* Insertar item */
    b->buffer[b->in] = item;
    b->in = (b->in + 1) % BUFFER_SIZE;
    b->count++;

    printf("Producido: %d (count=%d)\n", item, b->count);

    /* Señalar que ya no está vacío */
    pthread_cond_signal(&b->no_vacio);

    pthread_mutex_unlock(&b->mutex);
}

int consumir(Buffer *b) {
    pthread_mutex_lock(&b->mutex);

    /* Esperar mientras buffer esté vacío */
    while (b->count == 0 && !b->terminado) {
        pthread_cond_wait(&b->no_vacio, &b->mutex);
    }

    if (b->count == 0 && b->terminado) {
        pthread_mutex_unlock(&b->mutex);
        return -1;  /* No hay más datos */
    }

    /* Extraer item */
    int item = b->buffer[b->out];
    b->out = (b->out + 1) % BUFFER_SIZE;
    b->count--;

    printf("Consumido: %d (count=%d)\n", item, b->count);

    /* Señalar que ya no está lleno */
    pthread_cond_signal(&b->no_lleno);

    pthread_mutex_unlock(&b->mutex);

    return item;
}

void *productor(void *arg) {
    Buffer *b = (Buffer *)arg;

    for (int i = 0; i < 20; i++) {
        producir(b, i);
        usleep(rand() % 100000);  /* Simular trabajo */
    }

    /* Señalar fin de producción */
    pthread_mutex_lock(&b->mutex);
    b->terminado = true;
    pthread_cond_broadcast(&b->no_vacio);  /* Despertar TODOS */
    pthread_mutex_unlock(&b->mutex);

    return NULL;
}

void *consumidor(void *arg) {
    Buffer *b = (Buffer *)arg;
    int item;

    while ((item = consumir(b)) != -1) {
        usleep(rand() % 150000);  /* Simular procesamiento */
    }

    return NULL;
}
```

### 1.4 Read-Write Locks

```c
#include <pthread.h>

/*
 * READ-WRITE LOCKS
 * ================
 *
 * Múltiples lectores simultáneos, pero solo un escritor.
 * Ideal para datos leídos frecuentemente pero escritos raramente.
 */

typedef struct {
    int *datos;
    int size;
    pthread_rwlock_t rwlock;
} DatosCompartidos;

DatosCompartidos *crear_datos(int size) {
    DatosCompartidos *d = malloc(sizeof(DatosCompartidos));
    d->datos = calloc(size, sizeof(int));
    d->size = size;
    pthread_rwlock_init(&d->rwlock, NULL);
    return d;
}

/* Múltiples threads pueden leer simultáneamente */
int leer_dato(DatosCompartidos *d, int indice) {
    pthread_rwlock_rdlock(&d->rwlock);  /* Lock de lectura */

    int valor = (indice >= 0 && indice < d->size) ?
                d->datos[indice] : -1;

    pthread_rwlock_unlock(&d->rwlock);
    return valor;
}

/* Solo un thread puede escribir (exclusivo) */
void escribir_dato(DatosCompartidos *d, int indice, int valor) {
    pthread_rwlock_wrlock(&d->rwlock);  /* Lock de escritura */

    if (indice >= 0 && indice < d->size) {
        d->datos[indice] = valor;
    }

    pthread_rwlock_unlock(&d->rwlock);
}

/* Try locks para evitar bloqueo */
int intentar_leer(DatosCompartidos *d, int indice) {
    if (pthread_rwlock_tryrdlock(&d->rwlock) != 0) {
        return -1;  /* No se pudo obtener lock */
    }

    int valor = d->datos[indice];
    pthread_rwlock_unlock(&d->rwlock);
    return valor;
}
```

### 1.5 Windows Threads

```c
#ifdef _WIN32

#include <windows.h>
#include <process.h>
#include <stdio.h>

/*
 * WINDOWS THREADS
 * ===============
 *
 * API nativa de Windows para multithreading.
 */

typedef struct {
    int id;
    int valor;
    HANDLE completado;  /* Evento de sincronización */
} ThreadDataWin;

/* Función de thread Windows */
unsigned __stdcall funcion_thread(void *arg) {
    ThreadDataWin *data = (ThreadDataWin *)arg;

    printf("Thread %d iniciado\n", data->id);

    /* Simular trabajo */
    Sleep(1000);
    data->valor = data->id * 100;

    /* Señalar que terminamos */
    SetEvent(data->completado);

    printf("Thread %d terminado\n", data->id);
    return 0;
}

void ejemplo_threads_windows(void) {
    const int NUM_THREADS = 4;
    HANDLE threads[NUM_THREADS];
    ThreadDataWin datos[NUM_THREADS];

    /* Crear threads */
    for (int i = 0; i < NUM_THREADS; i++) {
        datos[i].id = i;
        datos[i].valor = 0;
        datos[i].completado = CreateEvent(NULL, FALSE, FALSE, NULL);

        threads[i] = (HANDLE)_beginthreadex(
            NULL,           /* Seguridad */
            0,              /* Stack size (0 = default) */
            funcion_thread, /* Función */
            &datos[i],      /* Argumento */
            0,              /* Flags */
            NULL            /* Thread ID (opcional) */
        );
    }

    /* Esperar a que terminen */
    WaitForMultipleObjects(NUM_THREADS, threads, TRUE, INFINITE);

    /* Limpiar */
    for (int i = 0; i < NUM_THREADS; i++) {
        CloseHandle(threads[i]);
        CloseHandle(datos[i].completado);
        printf("Thread %d resultado: %d\n", i, datos[i].valor);
    }
}

/* Critical Section (más rápida que Mutex en Windows) */
CRITICAL_SECTION cs;
int contador_win = 0;

void inicializar_cs(void) {
    InitializeCriticalSection(&cs);
}

void incrementar_seguro_win(void) {
    EnterCriticalSection(&cs);
    contador_win++;
    LeaveCriticalSection(&cs);
}

void limpiar_cs(void) {
    DeleteCriticalSection(&cs);
}

#endif /* _WIN32 */
```

### 1.6 Thread-Local Storage

```c
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * THREAD-LOCAL STORAGE (TLS)
 * ==========================
 *
 * Variables que tienen valor independiente por thread.
 */

/* Método 1: __thread (GCC/Clang) o _Thread_local (C11) */
#ifdef __GNUC__
__thread int variable_local_thread = 0;
#else
_Thread_local int variable_local_thread = 0;
#endif

/* Método 2: pthread_key (más portable) */
pthread_key_t key;

typedef struct {
    int id;
    char nombre[32];
} DatosThread;

void destructor_datos(void *data) {
    printf("Destruyendo datos thread: %s\n",
           ((DatosThread *)data)->nombre);
    free(data);
}

void inicializar_tls(void) {
    pthread_key_create(&key, destructor_datos);
}

void *thread_con_tls(void *arg) {
    int id = *(int *)arg;

    /* Crear datos específicos para este thread */
    DatosThread *mis_datos = malloc(sizeof(DatosThread));
    mis_datos->id = id;
    snprintf(mis_datos->nombre, sizeof(mis_datos->nombre),
             "Thread-%d", id);

    pthread_setspecific(key, mis_datos);

    /* Más tarde, obtener los datos */
    DatosThread *datos = pthread_getspecific(key);
    printf("Soy %s\n", datos->nombre);

    /* También usando __thread */
    variable_local_thread = id * 100;
    printf("Mi variable local: %d\n", variable_local_thread);

    return NULL;
}

void limpiar_tls(void) {
    pthread_key_delete(key);
}
```

---

## 2. Señales

### 2.1 Manejo de Señales (POSIX)

```c
#ifndef _WIN32

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

/*
 * SEÑALES POSIX
 * =============
 *
 * Señales comunes:
 *   SIGINT   (2)  - Ctrl+C
 *   SIGTERM (15)  - Terminar (kill)
 *   SIGKILL  (9)  - Matar (no capturable)
 *   SIGSEGV (11)  - Segmentation fault
 *   SIGPIPE (13)  - Pipe roto
 *   SIGALRM (14)  - Alarma
 *   SIGUSR1 (10)  - Usuario definido 1
 *   SIGUSR2 (12)  - Usuario definido 2
 *   SIGCHLD (17)  - Hijo terminó
 */

/* Variable volatile para comunicar con handler */
volatile sig_atomic_t terminar = 0;

/* Handler simple */
void handler_sigint(int sig) {
    /* En handler: solo operaciones async-signal-safe */
    terminar = 1;
}

/* Configurar handler moderno (sigaction) */
void configurar_seniales(void) {
    struct sigaction sa;

    /* Limpiar estructura */
    memset(&sa, 0, sizeof(sa));

    /* Configurar handler */
    sa.sa_handler = handler_sigint;

    /* Bloquear otras señales durante handler */
    sigemptyset(&sa.sa_mask);
    sigaddset(&sa.sa_mask, SIGTERM);

    /* Flags */
    sa.sa_flags = SA_RESTART;  /* Reiniciar syscalls interrumpidas */

    /* Instalar handler para SIGINT */
    if (sigaction(SIGINT, &sa, NULL) < 0) {
        perror("sigaction");
        exit(1);
    }

    /* SIGTERM con el mismo handler */
    sigaction(SIGTERM, &sa, NULL);
}

/* Handler con información adicional */
void handler_con_info(int sig, siginfo_t *info, void *context) {
    printf("Señal %d recibida de PID %d\n", sig, info->si_pid);
}

void configurar_handler_info(void) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));

    sa.sa_sigaction = handler_con_info;
    sa.sa_flags = SA_SIGINFO;  /* Usar sa_sigaction en lugar de sa_handler */
    sigemptyset(&sa.sa_mask);

    sigaction(SIGUSR1, &sa, NULL);
}

/* Ignorar señal */
void ignorar_sigpipe(void) {
    signal(SIGPIPE, SIG_IGN);

    /* O con sigaction */
    struct sigaction sa;
    sa.sa_handler = SIG_IGN;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGPIPE, &sa, NULL);
}

/* Bloquear/desbloquear señales */
void ejemplo_mascara_seniales(void) {
    sigset_t mask, oldmask;

    /* Crear máscara con SIGINT y SIGTERM */
    sigemptyset(&mask);
    sigaddset(&mask, SIGINT);
    sigaddset(&mask, SIGTERM);

    /* Bloquear señales */
    sigprocmask(SIG_BLOCK, &mask, &oldmask);

    /* ... sección crítica - señales diferidas ... */

    /* Restaurar máscara anterior */
    sigprocmask(SIG_SETMASK, &oldmask, NULL);
}

/* Esperar señal específica */
void esperar_senal(void) {
    sigset_t mask;
    int sig;

    sigemptyset(&mask);
    sigaddset(&mask, SIGUSR1);
    sigaddset(&mask, SIGUSR2);

    /* Bloquear primero */
    sigprocmask(SIG_BLOCK, &mask, NULL);

    printf("Esperando SIGUSR1 o SIGUSR2...\n");

    /* Esperar una de las señales */
    sigwait(&mask, &sig);

    printf("Recibida señal %d\n", sig);
}

/* Temporizador con SIGALRM */
volatile sig_atomic_t alarma_activada = 0;

void handler_alarma(int sig) {
    alarma_activada = 1;
}

void timeout_operacion(int segundos) {
    signal(SIGALRM, handler_alarma);
    alarm(segundos);

    /* Operación potencialmente larga */
    while (!alarma_activada) {
        /* ... trabajo ... */
        sleep(1);
    }

    alarm(0);  /* Cancelar alarma si terminamos antes */
    printf("Timeout o completado\n");
}

int main(void) {
    configurar_seniales();

    printf("PID: %d - Presiona Ctrl+C para terminar\n", getpid());

    while (!terminar) {
        printf("Trabajando...\n");
        sleep(1);
    }

    printf("Terminando limpiamente\n");
    return 0;
}

#endif /* POSIX */
```

### 2.2 Señales en Multithreading

```c
#ifndef _WIN32

#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>

/*
 * SEÑALES CON THREADS
 * ===================
 *
 * Reglas importantes:
 * 1. Señales se entregan a UN solo thread (no especificado cuál)
 * 2. Cada thread tiene su propia máscara de señales
 * 3. Usar pthread_sigmask en threads (no sigprocmask)
 */

/* Thread dedicado para manejar señales */
void *thread_seniales(void *arg) {
    sigset_t *mask = (sigset_t *)arg;
    int sig;

    while (1) {
        /* Esperar señales de la máscara */
        if (sigwait(mask, &sig) == 0) {
            printf("Thread de señales recibió: %d\n", sig);

            if (sig == SIGTERM || sig == SIGINT) {
                printf("Señal de terminación recibida\n");
                break;
            }
        }
    }

    return NULL;
}

int main_threads_seniales(void) {
    pthread_t thread;
    sigset_t mask;

    /* Bloquear señales en main (heredado por threads) */
    sigemptyset(&mask);
    sigaddset(&mask, SIGINT);
    sigaddset(&mask, SIGTERM);
    sigaddset(&mask, SIGUSR1);
    pthread_sigmask(SIG_BLOCK, &mask, NULL);

    /* Crear thread dedicado para señales */
    pthread_create(&thread, NULL, thread_seniales, &mask);

    /* Main y otros threads no recibirán estas señales */
    printf("Main thread trabajando (PID=%d)\n", getpid());

    for (int i = 0; i < 10; i++) {
        printf("Trabajo %d\n", i);
        sleep(1);
    }

    /* Enviar señal para terminar */
    pthread_kill(thread, SIGTERM);
    pthread_join(thread, NULL);

    return 0;
}

/* Enviar señal a thread específico */
void enviar_senal_a_thread(pthread_t thread, int sig) {
    pthread_kill(thread, sig);
}

#endif /* POSIX */
```

---

## 3. Gestión de Procesos

### 3.1 Fork y Exec (POSIX)

```c
#ifndef _WIN32

#include <unistd.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * FORK - CREACIÓN DE PROCESOS
 * ===========================
 *
 * fork() crea copia exacta del proceso actual.
 * Retorna:
 *   - 0 en el hijo
 *   - PID del hijo en el padre
 *   - -1 en error
 */

void ejemplo_fork_basico(void) {
    printf("Antes de fork - PID: %d\n", getpid());

    pid_t pid = fork();

    if (pid < 0) {
        perror("fork falló");
        exit(1);
    }
    else if (pid == 0) {
        /* PROCESO HIJO */
        printf("Hijo - PID: %d, Padre: %d\n", getpid(), getppid());
        exit(0);
    }
    else {
        /* PROCESO PADRE */
        printf("Padre - PID: %d, Hijo: %d\n", getpid(), pid);

        /* Esperar a que el hijo termine */
        int status;
        waitpid(pid, &status, 0);

        if (WIFEXITED(status)) {
            printf("Hijo terminó con código: %d\n", WEXITSTATUS(status));
        }
    }
}

/*
 * EXEC - REEMPLAZAR IMAGEN DEL PROCESO
 * ====================================
 *
 * Variantes:
 *   execl  - Lista de args, path completo
 *   execlp - Lista de args, busca en PATH
 *   execle - Lista de args, con environment
 *   execv  - Array de args, path completo
 *   execvp - Array de args, busca en PATH
 *   execve - Array de args, con environment
 */

void ejecutar_comando(const char *cmd, char *const args[]) {
    pid_t pid = fork();

    if (pid < 0) {
        perror("fork");
        return;
    }

    if (pid == 0) {
        /* Hijo: ejecutar comando */
        execvp(cmd, args);

        /* Si llegamos aquí, exec falló */
        perror("exec");
        exit(127);
    }

    /* Padre: esperar */
    int status;
    waitpid(pid, &status, 0);
}

/* Ejemplo: ejecutar ls -la */
void ejemplo_exec(void) {
    char *args[] = {"ls", "-la", "/tmp", NULL};
    ejecutar_comando("ls", args);
}

/* Pipeline: cmd1 | cmd2 */
void ejecutar_pipeline(void) {
    int pipefd[2];

    if (pipe(pipefd) < 0) {
        perror("pipe");
        return;
    }

    pid_t pid1 = fork();

    if (pid1 == 0) {
        /* Primer hijo: ls */
        close(pipefd[0]);  /* No lee del pipe */
        dup2(pipefd[1], STDOUT_FILENO);  /* stdout -> pipe */
        close(pipefd[1]);

        execlp("ls", "ls", "-la", NULL);
        exit(1);
    }

    pid_t pid2 = fork();

    if (pid2 == 0) {
        /* Segundo hijo: grep */
        close(pipefd[1]);  /* No escribe al pipe */
        dup2(pipefd[0], STDIN_FILENO);  /* stdin <- pipe */
        close(pipefd[0]);

        execlp("grep", "grep", ".txt", NULL);
        exit(1);
    }

    /* Padre: cerrar pipe y esperar */
    close(pipefd[0]);
    close(pipefd[1]);

    waitpid(pid1, NULL, 0);
    waitpid(pid2, NULL, 0);
}

/* Fork bomb protection - limitar procesos */
#include <sys/resource.h>

void limitar_procesos(int max) {
    struct rlimit rl;
    rl.rlim_cur = max;
    rl.rlim_max = max;
    setrlimit(RLIMIT_NPROC, &rl);
}

#endif /* POSIX */
```

### 3.2 CreateProcess (Windows)

```c
#ifdef _WIN32

#include <windows.h>
#include <stdio.h>

/*
 * WINDOWS - CREACIÓN DE PROCESOS
 * ==============================
 */

void ejecutar_proceso_windows(const char *comando) {
    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    /* Necesitamos copia modificable del comando */
    char cmd[MAX_PATH];
    strncpy(cmd, comando, sizeof(cmd) - 1);

    if (!CreateProcess(
        NULL,           /* Módulo (NULL = usar línea de comandos) */
        cmd,            /* Línea de comandos */
        NULL,           /* Atributos de seguridad proceso */
        NULL,           /* Atributos de seguridad thread */
        FALSE,          /* Heredar handles */
        0,              /* Flags de creación */
        NULL,           /* Environment (NULL = heredar) */
        NULL,           /* Directorio actual (NULL = heredar) */
        &si,            /* STARTUPINFO */
        &pi             /* PROCESS_INFORMATION */
    )) {
        printf("CreateProcess falló: %lu\n", GetLastError());
        return;
    }

    /* Esperar a que termine */
    WaitForSingleObject(pi.hProcess, INFINITE);

    /* Obtener código de salida */
    DWORD exitCode;
    GetExitCodeProcess(pi.hProcess, &exitCode);
    printf("Proceso terminó con código: %lu\n", exitCode);

    /* Limpiar handles */
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
}

/* Ejecutar proceso con redirección */
void ejecutar_con_redireccion(void) {
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = NULL;

    HANDLE hReadPipe, hWritePipe;
    CreatePipe(&hReadPipe, &hWritePipe, &sa, 0);

    /* No heredar el lado de lectura */
    SetHandleInformation(hReadPipe, HANDLE_FLAG_INHERIT, 0);

    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    si.hStdOutput = hWritePipe;
    si.hStdError = hWritePipe;
    si.dwFlags |= STARTF_USESTDHANDLES;
    ZeroMemory(&pi, sizeof(pi));

    char cmd[] = "cmd /c dir";

    if (CreateProcess(NULL, cmd, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
        CloseHandle(hWritePipe);

        /* Leer output */
        char buffer[4096];
        DWORD bytesRead;
        while (ReadFile(hReadPipe, buffer, sizeof(buffer) - 1, &bytesRead, NULL)) {
            if (bytesRead == 0) break;
            buffer[bytesRead] = '\0';
            printf("%s", buffer);
        }

        WaitForSingleObject(pi.hProcess, INFINITE);
        CloseHandle(pi.hProcess);
        CloseHandle(pi.hThread);
    }

    CloseHandle(hReadPipe);
}

#endif /* _WIN32 */
```

---

## 4. Comunicación Inter-Procesos

### 4.1 Pipes

```c
#ifndef _WIN32

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>

/*
 * PIPES ANÓNIMOS
 * ==============
 *
 * Comunicación unidireccional entre procesos relacionados.
 */

void ejemplo_pipe_simple(void) {
    int pipefd[2];  /* [0] = lectura, [1] = escritura */

    if (pipe(pipefd) < 0) {
        perror("pipe");
        return;
    }

    pid_t pid = fork();

    if (pid == 0) {
        /* HIJO - Escritor */
        close(pipefd[0]);  /* Cerrar lectura */

        const char *msg = "Hola desde el hijo!";
        write(pipefd[1], msg, strlen(msg) + 1);

        close(pipefd[1]);
        _exit(0);
    }

    /* PADRE - Lector */
    close(pipefd[1]);  /* Cerrar escritura */

    char buffer[128];
    ssize_t n = read(pipefd[0], buffer, sizeof(buffer));

    if (n > 0) {
        printf("Padre recibió: %s\n", buffer);
    }

    close(pipefd[0]);
    wait(NULL);
}

/*
 * NAMED PIPES (FIFO)
 * ==================
 *
 * Comunicación entre procesos no relacionados.
 */

#include <sys/stat.h>
#include <fcntl.h>

void crear_fifo(const char *nombre) {
    /* Crear FIFO con permisos rw-rw-rw- */
    if (mkfifo(nombre, 0666) < 0) {
        perror("mkfifo");
    }
}

void escribir_fifo(const char *nombre, const char *mensaje) {
    int fd = open(nombre, O_WRONLY);
    if (fd < 0) {
        perror("open fifo write");
        return;
    }

    write(fd, mensaje, strlen(mensaje));
    close(fd);
}

void leer_fifo(const char *nombre) {
    int fd = open(nombre, O_RDONLY);
    if (fd < 0) {
        perror("open fifo read");
        return;
    }

    char buffer[256];
    ssize_t n;

    while ((n = read(fd, buffer, sizeof(buffer) - 1)) > 0) {
        buffer[n] = '\0';
        printf("FIFO: %s\n", buffer);
    }

    close(fd);
}

#endif /* POSIX */
```

### 4.2 Memoria Compartida

```c
#ifndef _WIN32

#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <semaphore.h>

/*
 * MEMORIA COMPARTIDA POSIX
 * ========================
 */

#define SHM_NAME "/mi_memoria"
#define SHM_SIZE 4096

typedef struct {
    int contador;
    char mensaje[256];
    sem_t sem;  /* Semáforo para sincronización */
} DatosCompartidos;

/* Crear memoria compartida */
DatosCompartidos *crear_shm(void) {
    /* Crear objeto de memoria compartida */
    int fd = shm_open(SHM_NAME, O_CREAT | O_RDWR, 0666);
    if (fd < 0) {
        perror("shm_open");
        return NULL;
    }

    /* Establecer tamaño */
    ftruncate(fd, sizeof(DatosCompartidos));

    /* Mapear a memoria */
    DatosCompartidos *data = mmap(NULL, sizeof(DatosCompartidos),
                                   PROT_READ | PROT_WRITE,
                                   MAP_SHARED, fd, 0);

    close(fd);  /* Ya no necesitamos el fd */

    if (data == MAP_FAILED) {
        perror("mmap");
        return NULL;
    }

    /* Inicializar semáforo (compartido entre procesos) */
    sem_init(&data->sem, 1, 1);  /* 1 = proceso, 1 = valor inicial */

    return data;
}

/* Abrir memoria compartida existente */
DatosCompartidos *abrir_shm(void) {
    int fd = shm_open(SHM_NAME, O_RDWR, 0666);
    if (fd < 0) return NULL;

    DatosCompartidos *data = mmap(NULL, sizeof(DatosCompartidos),
                                   PROT_READ | PROT_WRITE,
                                   MAP_SHARED, fd, 0);
    close(fd);

    return (data == MAP_FAILED) ? NULL : data;
}

/* Usar memoria compartida */
void usar_shm(DatosCompartidos *data, int es_escritor) {
    if (es_escritor) {
        sem_wait(&data->sem);  /* Adquirir semáforo */

        data->contador++;
        snprintf(data->mensaje, sizeof(data->mensaje),
                 "Actualizado por PID %d", getpid());

        sem_post(&data->sem);  /* Liberar semáforo */
    } else {
        sem_wait(&data->sem);

        printf("Contador: %d\n", data->contador);
        printf("Mensaje: %s\n", data->mensaje);

        sem_post(&data->sem);
    }
}

/* Limpiar */
void limpiar_shm(DatosCompartidos *data) {
    sem_destroy(&data->sem);
    munmap(data, sizeof(DatosCompartidos));
    shm_unlink(SHM_NAME);
}

#endif /* POSIX */
```

### 4.3 Message Queues

```c
#ifndef _WIN32

#include <mqueue.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>

/*
 * COLAS DE MENSAJES POSIX
 * =======================
 *
 * Comunicación asíncrona entre procesos.
 */

#define QUEUE_NAME "/mi_cola"
#define MAX_MSG_SIZE 256
#define MAX_MSGS 10

/* Crear cola */
mqd_t crear_cola(void) {
    struct mq_attr attr;
    attr.mq_flags = 0;
    attr.mq_maxmsg = MAX_MSGS;
    attr.mq_msgsize = MAX_MSG_SIZE;
    attr.mq_curmsgs = 0;

    mqd_t mq = mq_open(QUEUE_NAME,
                        O_CREAT | O_RDWR,
                        0666,
                        &attr);

    if (mq == (mqd_t)-1) {
        perror("mq_open");
    }

    return mq;
}

/* Enviar mensaje */
int enviar_mensaje(mqd_t mq, const char *msg, unsigned int prioridad) {
    if (mq_send(mq, msg, strlen(msg) + 1, prioridad) < 0) {
        perror("mq_send");
        return -1;
    }
    return 0;
}

/* Recibir mensaje */
int recibir_mensaje(mqd_t mq, char *buffer, size_t buf_size) {
    unsigned int prioridad;
    ssize_t n = mq_receive(mq, buffer, buf_size, &prioridad);

    if (n < 0) {
        perror("mq_receive");
        return -1;
    }

    printf("Recibido (prioridad %u): %s\n", prioridad, buffer);
    return 0;
}

/* Limpiar */
void limpiar_cola(mqd_t mq) {
    mq_close(mq);
    mq_unlink(QUEUE_NAME);
}

#endif /* POSIX */
```

---

## 5. Optimización y Performance

### 5.1 Optimizaciones del Compilador

```c
/*
 * DIRECTIVAS DE OPTIMIZACIÓN
 * ==========================
 *
 * Niveles de GCC:
 *   -O0: Sin optimización (debug)
 *   -O1: Optimización básica
 *   -O2: Optimización estándar (recomendado producción)
 *   -O3: Optimización agresiva (puede aumentar tamaño)
 *   -Os: Optimizar tamaño
 *   -Ofast: -O3 + optimizaciones no estándar
 */

/* Sugerencias al compilador */

/* likely/unlikely - hint de predicción de branches */
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)

void procesar_datos(int *datos, int n) {
    for (int i = 0; i < n; i++) {
        if (unlikely(datos[i] < 0)) {
            /* Caso raro - compilador optimiza el otro path */
            manejar_error(i);
            continue;
        }
        /* Caso común */
        procesar_elemento(datos[i]);
    }
}

/* Prefetch - cargar datos a cache antes de usarlos */
void procesar_con_prefetch(int *arr, int n) {
    for (int i = 0; i < n; i++) {
        /* Prefetch datos que usaremos en futuras iteraciones */
        __builtin_prefetch(&arr[i + 16], 0, 3);
        /* 0 = lectura, 3 = alta localidad temporal */

        procesar(arr[i]);
    }
}

/* restrict - prometer que punteros no se solapan */
void copiar_rapido(int * restrict dest,
                   const int * restrict src,
                   int n) {
    /* Compilador puede optimizar más agresivamente */
    for (int i = 0; i < n; i++) {
        dest[i] = src[i];
    }
}

/* Forzar inline */
static inline __attribute__((always_inline))
int suma_inline(int a, int b) {
    return a + b;
}

/* Prevenir inline */
__attribute__((noinline))
void funcion_grande(void) {
    /* ... código extenso ... */
}

/* Hot/Cold - código frecuente vs raro */
__attribute__((hot))
void funcion_frecuente(void) {
    /* Código ejecutado muy seguido */
}

__attribute__((cold))
void funcion_error(void) {
    /* Código raramente ejecutado */
}

/* Alineación para SIMD */
float datos[1024] __attribute__((aligned(32)));

/* Pure y const - funciones sin efectos secundarios */
__attribute__((pure))
int calcular_hash(const char *str) {
    /* Lee memoria pero no la modifica */
    int hash = 0;
    while (*str) hash = hash * 31 + *str++;
    return hash;
}

__attribute__((const))
int cuadrado(int x) {
    /* No lee ni modifica memoria global */
    return x * x;
}
```

### 5.2 Cache y Localidad

```c
/*
 * OPTIMIZACIÓN DE CACHE
 * =====================
 *
 * Cache lines típicos: 64 bytes
 * Principios:
 * - Localidad espacial: acceder memoria cercana
 * - Localidad temporal: reusar datos recientes
 */

#include <stdint.h>
#include <string.h>

/* MAL: acceso por columnas (cache miss frecuente) */
void sumar_columnas_mal(int matriz[1000][1000], int resultado[1000]) {
    for (int j = 0; j < 1000; j++) {
        resultado[j] = 0;
        for (int i = 0; i < 1000; i++) {
            resultado[j] += matriz[i][j];  /* Saltos de 4000 bytes */
        }
    }
}

/* BIEN: acceso por filas (secuencial) */
void sumar_columnas_bien(int matriz[1000][1000], int resultado[1000]) {
    memset(resultado, 0, 1000 * sizeof(int));

    for (int i = 0; i < 1000; i++) {
        for (int j = 0; j < 1000; j++) {
            resultado[j] += matriz[i][j];  /* Acceso secuencial */
        }
    }
}

/* Blocking - procesar en bloques que caben en cache */
#define BLOCK_SIZE 64

void multiplicar_matrices_blocked(
    double A[512][512],
    double B[512][512],
    double C[512][512],
    int n
) {
    memset(C, 0, n * n * sizeof(double));

    for (int i0 = 0; i0 < n; i0 += BLOCK_SIZE) {
        for (int j0 = 0; j0 < n; j0 += BLOCK_SIZE) {
            for (int k0 = 0; k0 < n; k0 += BLOCK_SIZE) {
                /* Procesar bloque */
                for (int i = i0; i < i0 + BLOCK_SIZE && i < n; i++) {
                    for (int j = j0; j < j0 + BLOCK_SIZE && j < n; j++) {
                        double sum = C[i][j];
                        for (int k = k0; k < k0 + BLOCK_SIZE && k < n; k++) {
                            sum += A[i][k] * B[k][j];
                        }
                        C[i][j] = sum;
                    }
                }
            }
        }
    }
}

/* Evitar false sharing en multithreading */
typedef struct {
    int contador;
    char padding[60];  /* Asegurar diferentes cache lines */
} ContadorPadded;

ContadorPadded contadores[4];  /* Cada thread usa uno */

/* O usar alineación explícita */
typedef struct {
    _Alignas(64) int contador;  /* C11 */
} ContadorAlineado;
```

### 5.3 Loop Optimizations

```c
/*
 * OPTIMIZACIÓN DE BUCLES
 * ======================
 */

/* Loop unrolling manual */
void sumar_unrolled(int *arr, int n, int *resultado) {
    int sum = 0;
    int i;

    /* Procesar de 4 en 4 */
    for (i = 0; i <= n - 4; i += 4) {
        sum += arr[i];
        sum += arr[i + 1];
        sum += arr[i + 2];
        sum += arr[i + 3];
    }

    /* Resto */
    for (; i < n; i++) {
        sum += arr[i];
    }

    *resultado = sum;
}

/* Loop fusion - combinar bucles */
/* MAL: dos pasadas por el array */
void procesar_mal(int *arr, int n) {
    for (int i = 0; i < n; i++) arr[i] *= 2;
    for (int i = 0; i < n; i++) arr[i] += 1;
}

/* BIEN: una sola pasada */
void procesar_bien(int *arr, int n) {
    for (int i = 0; i < n; i++) {
        arr[i] = arr[i] * 2 + 1;
    }
}

/* Loop hoisting - sacar invariantes */
/* MAL */
void calcular_mal(int *arr, int n, int factor) {
    for (int i = 0; i < n; i++) {
        arr[i] = arr[i] * factor + strlen("constante");  /* strlen en cada iter */
    }
}

/* BIEN */
void calcular_bien(int *arr, int n, int factor) {
    size_t len = strlen("constante");  /* Una sola vez */
    for (int i = 0; i < n; i++) {
        arr[i] = arr[i] * factor + len;
    }
}

/* Strength reduction - reemplazar operaciones costosas */
/* División es más lenta que multiplicación */
void dividir_por_constante(int *arr, int n) {
    /* Compilador hace esto automáticamente con -O2 */
    const int divisor = 8;
    const int shift = 3;  /* 8 = 2^3 */

    for (int i = 0; i < n; i++) {
        arr[i] = arr[i] >> shift;  /* En lugar de arr[i] / 8 */
    }
}
```

---

## 6. Inline Assembly

### 6.1 GCC Inline Assembly

```c
/*
 * INLINE ASSEMBLY (GCC/Clang)
 * ===========================
 *
 * Sintaxis: asm [volatile] (
 *     "instrucciones"
 *     : outputs
 *     : inputs
 *     : clobbers
 * );
 */

#include <stdint.h>

/* Ejemplo simple - leer timestamp counter */
uint64_t rdtsc(void) {
    uint32_t lo, hi;
    asm volatile (
        "rdtsc"
        : "=a" (lo), "=d" (hi)  /* outputs: eax -> lo, edx -> hi */
    );
    return ((uint64_t)hi << 32) | lo;
}

/* Suma con inline asm */
int suma_asm(int a, int b) {
    int resultado;
    asm (
        "addl %2, %1\n\t"     /* b = a + b */
        "movl %1, %0"         /* resultado = b */
        : "=r" (resultado)    /* output: registro -> resultado */
        : "r" (a), "r" (b)    /* inputs: a en registro, b en registro */
    );
    return resultado;
}

/* CPUID - obtener info del procesador */
void cpuid(uint32_t eax_in, uint32_t *eax, uint32_t *ebx,
           uint32_t *ecx, uint32_t *edx) {
    asm volatile (
        "cpuid"
        : "=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx)
        : "a" (eax_in)
    );
}

/* Atomic compare-and-swap */
int atomic_cas(int *ptr, int expected, int new_val) {
    int old;
    asm volatile (
        "lock cmpxchgl %2, %1"
        : "=a" (old), "+m" (*ptr)
        : "r" (new_val), "0" (expected)
        : "memory"
    );
    return old;
}

/* Memory barrier */
#define memory_barrier() asm volatile ("" ::: "memory")
#define full_barrier()   asm volatile ("mfence" ::: "memory")

/* Pause hint para spin loops */
#define cpu_relax() asm volatile ("pause" ::: "memory")

void spin_lock(int *lock) {
    while (__sync_lock_test_and_set(lock, 1)) {
        while (*lock) {
            cpu_relax();  /* Reduce consumo en espera */
        }
    }
}
```

### 6.2 SIMD con Intrinsics

```c
/*
 * SIMD INTRINSICS
 * ===============
 *
 * Operaciones vectoriales sin assembly directo.
 * Más portable que asm inline.
 */

#include <immintrin.h>  /* SSE, AVX */

/* Suma de arrays con SSE */
void sumar_arrays_sse(float *a, float *b, float *c, int n) {
    int i;

    /* Procesar 4 floats a la vez */
    for (i = 0; i <= n - 4; i += 4) {
        __m128 va = _mm_loadu_ps(&a[i]);  /* Cargar 4 floats */
        __m128 vb = _mm_loadu_ps(&b[i]);
        __m128 vc = _mm_add_ps(va, vb);   /* Sumar en paralelo */
        _mm_storeu_ps(&c[i], vc);         /* Guardar resultado */
    }

    /* Resto */
    for (; i < n; i++) {
        c[i] = a[i] + b[i];
    }
}

/* Producto punto con AVX */
float dot_product_avx(float *a, float *b, int n) {
    __m256 sum = _mm256_setzero_ps();  /* 8 zeros */
    int i;

    for (i = 0; i <= n - 8; i += 8) {
        __m256 va = _mm256_loadu_ps(&a[i]);
        __m256 vb = _mm256_loadu_ps(&b[i]);
        sum = _mm256_fmadd_ps(va, vb, sum);  /* sum += a * b */
    }

    /* Reducción horizontal */
    __m128 hi = _mm256_extractf128_ps(sum, 1);
    __m128 lo = _mm256_castps256_ps128(sum);
    __m128 sum128 = _mm_add_ps(hi, lo);
    sum128 = _mm_hadd_ps(sum128, sum128);
    sum128 = _mm_hadd_ps(sum128, sum128);

    float resultado = _mm_cvtss_f32(sum128);

    /* Resto */
    for (; i < n; i++) {
        resultado += a[i] * b[i];
    }

    return resultado;
}

/* Verificar soporte de instrucciones */
int soporta_avx(void) {
    uint32_t eax, ebx, ecx, edx;
    cpuid(1, &eax, &ebx, &ecx, &edx);
    return (ecx & (1 << 28)) != 0;  /* Bit 28 de ECX */
}
```

---

## 7. Debugging y Profiling

### 7.1 Técnicas de Debugging

```c
/*
 * DEBUGGING EN C
 * ==============
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* Macros de debug condicional */
#ifdef DEBUG
    #define DEBUG_PRINT(fmt, ...) \
        fprintf(stderr, "[DEBUG] %s:%d: " fmt "\n", \
                __FILE__, __LINE__, ##__VA_ARGS__)

    #define DEBUG_ASSERT(cond) assert(cond)
#else
    #define DEBUG_PRINT(fmt, ...) ((void)0)
    #define DEBUG_ASSERT(cond)    ((void)0)
#endif

/* Trace de funciones */
#define TRACE_ENTER() DEBUG_PRINT("Entrando %s", __func__)
#define TRACE_EXIT()  DEBUG_PRINT("Saliendo %s", __func__)

void funcion_compleja(int x) {
    TRACE_ENTER();

    DEBUG_PRINT("x = %d", x);

    /* ... código ... */

    DEBUG_ASSERT(x >= 0);

    TRACE_EXIT();
}

/* Verificación de heap (con compilador soportado) */
#ifdef __GNUC__
void verificar_heap(void) {
    /* GCC con glibc */
    extern int __malloc_hook;
    /* Usar valgrind o AddressSanitizer en su lugar */
}
#endif

/* Backtrace manual */
#ifndef _WIN32
#include <execinfo.h>

void imprimir_backtrace(void) {
    void *array[20];
    int size = backtrace(array, 20);
    char **strings = backtrace_symbols(array, size);

    fprintf(stderr, "Backtrace:\n");
    for (int i = 0; i < size; i++) {
        fprintf(stderr, "  %s\n", strings[i]);
    }

    free(strings);
}

/* Handler para SIGSEGV con backtrace */
#include <signal.h>

void handler_sigsegv(int sig) {
    fprintf(stderr, "¡Segmentation fault!\n");
    imprimir_backtrace();
    _exit(1);
}

void instalar_handler_crash(void) {
    signal(SIGSEGV, handler_sigsegv);
    signal(SIGABRT, handler_sigsegv);
}
#endif

/* Canary values para detectar corrupción */
#define CANARY_VALUE 0xDEADBEEF

typedef struct {
    uint32_t canary_start;
    /* ... datos ... */
    char buffer[256];
    uint32_t canary_end;
} BufferProtegido;

void verificar_canaries(BufferProtegido *b) {
    if (b->canary_start != CANARY_VALUE ||
        b->canary_end != CANARY_VALUE) {
        fprintf(stderr, "¡Buffer overflow detectado!\n");
        abort();
    }
}
```

### 7.2 Profiling

```c
/*
 * PROFILING Y BENCHMARKING
 * ========================
 *
 * Herramientas:
 * - gprof: gcc -pg
 * - perf: Linux perf events
 * - Valgrind: cachegrind, callgrind
 * - Intel VTune
 */

#include <time.h>
#include <stdio.h>

/* Medición de tiempo simple */
typedef struct {
    struct timespec inicio;
    struct timespec fin;
} Timer;

void timer_start(Timer *t) {
    clock_gettime(CLOCK_MONOTONIC, &t->inicio);
}

double timer_stop(Timer *t) {
    clock_gettime(CLOCK_MONOTONIC, &t->fin);

    double segundos = t->fin.tv_sec - t->inicio.tv_sec;
    double nanos = t->fin.tv_nsec - t->inicio.tv_nsec;

    return segundos + nanos / 1e9;
}

/* Macro para benchmark */
#define BENCHMARK(nombre, iteraciones, codigo) do { \
    Timer _t; \
    timer_start(&_t); \
    for (int _i = 0; _i < (iteraciones); _i++) { \
        codigo; \
    } \
    double _tiempo = timer_stop(&_t); \
    printf("%s: %.3f ms (%.0f ops/sec)\n", \
           nombre, _tiempo * 1000, (iteraciones) / _tiempo); \
} while(0)

/* Uso */
void benchmark_ejemplo(void) {
    int arr[1000];

    BENCHMARK("suma", 100000, {
        int sum = 0;
        for (int i = 0; i < 1000; i++) sum += arr[i];
    });
}

/* Medición con RDTSC (más precisa) */
uint64_t rdtsc(void);  /* Definida antes */

#define RDTSC_BENCHMARK(nombre, iteraciones, codigo) do { \
    uint64_t _start = rdtsc(); \
    for (int _i = 0; _i < (iteraciones); _i++) { \
        codigo; \
    } \
    uint64_t _end = rdtsc(); \
    printf("%s: %.2f ciclos/iteración\n", \
           nombre, (double)(_end - _start) / (iteraciones)); \
} while(0)
```

---

## 8. Patrones de Diseño en C

### 8.1 Singleton Thread-Safe

```c
#include <pthread.h>
#include <stdlib.h>

/*
 * SINGLETON THREAD-SAFE
 * =====================
 */

typedef struct {
    int valor;
    char *config;
} Configuracion;

static Configuracion *instancia = NULL;
static pthread_once_t once_control = PTHREAD_ONCE_INIT;

static void crear_configuracion(void) {
    instancia = malloc(sizeof(Configuracion));
    instancia->valor = 42;
    instancia->config = "default";
}

Configuracion *obtener_configuracion(void) {
    pthread_once(&once_control, crear_configuracion);
    return instancia;
}

/* Alternativa con double-checked locking */
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

Configuracion *obtener_config_dcl(void) {
    if (instancia == NULL) {
        pthread_mutex_lock(&mutex);
        if (instancia == NULL) {  /* Double check */
            Configuracion *temp = malloc(sizeof(Configuracion));
            temp->valor = 42;
            temp->config = "default";

            /* Memory barrier antes de publicar */
            __sync_synchronize();
            instancia = temp;
        }
        pthread_mutex_unlock(&mutex);
    }
    return instancia;
}
```

### 8.2 Object Pool

```c
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

/*
 * OBJECT POOL
 * ===========
 *
 * Pre-asignar objetos para evitar malloc/free frecuentes.
 */

#define POOL_SIZE 100

typedef struct {
    int id;
    char datos[256];
} Objeto;

typedef struct {
    Objeto objetos[POOL_SIZE];
    int disponibles[POOL_SIZE];
    int top;
    pthread_mutex_t mutex;
} ObjectPool;

ObjectPool *crear_pool(void) {
    ObjectPool *pool = malloc(sizeof(ObjectPool));
    pthread_mutex_init(&pool->mutex, NULL);

    /* Todos disponibles inicialmente */
    for (int i = 0; i < POOL_SIZE; i++) {
        pool->disponibles[i] = i;
        pool->objetos[i].id = i;
    }
    pool->top = POOL_SIZE;

    return pool;
}

Objeto *pool_acquire(ObjectPool *pool) {
    pthread_mutex_lock(&pool->mutex);

    Objeto *obj = NULL;
    if (pool->top > 0) {
        int idx = pool->disponibles[--pool->top];
        obj = &pool->objetos[idx];
    }

    pthread_mutex_unlock(&pool->mutex);
    return obj;
}

void pool_release(ObjectPool *pool, Objeto *obj) {
    pthread_mutex_lock(&pool->mutex);

    int idx = obj->id;
    memset(obj->datos, 0, sizeof(obj->datos));  /* Limpiar */
    pool->disponibles[pool->top++] = idx;

    pthread_mutex_unlock(&pool->mutex);
}

void destruir_pool(ObjectPool *pool) {
    pthread_mutex_destroy(&pool->mutex);
    free(pool);
}
```

### 8.3 State Machine

```c
/*
 * MÁQUINA DE ESTADOS
 * ==================
 */

typedef enum {
    STATE_IDLE,
    STATE_CONNECTING,
    STATE_CONNECTED,
    STATE_ERROR,
    STATE_COUNT
} Estado;

typedef enum {
    EVENT_CONNECT,
    EVENT_DISCONNECT,
    EVENT_DATA,
    EVENT_TIMEOUT,
    EVENT_COUNT
} Evento;

/* Forward declaration */
typedef struct StateMachine StateMachine;

/* Tipo de función handler */
typedef Estado (*EventHandler)(StateMachine *sm, void *data);

struct StateMachine {
    Estado estado_actual;
    EventHandler handlers[STATE_COUNT][EVENT_COUNT];
    void *contexto;
};

/* Handlers de ejemplo */
Estado handler_idle_connect(StateMachine *sm, void *data) {
    printf("Conectando...\n");
    return STATE_CONNECTING;
}

Estado handler_connecting_data(StateMachine *sm, void *data) {
    printf("Conexión establecida\n");
    return STATE_CONNECTED;
}

Estado handler_connecting_timeout(StateMachine *sm, void *data) {
    printf("Timeout de conexión\n");
    return STATE_ERROR;
}

Estado handler_connected_data(StateMachine *sm, void *data) {
    printf("Datos recibidos\n");
    return STATE_CONNECTED;  /* Permanecer */
}

Estado handler_connected_disconnect(StateMachine *sm, void *data) {
    printf("Desconectando\n");
    return STATE_IDLE;
}

/* Inicializar máquina */
void sm_init(StateMachine *sm) {
    sm->estado_actual = STATE_IDLE;

    /* Limpiar tabla */
    memset(sm->handlers, 0, sizeof(sm->handlers));

    /* Configurar transiciones válidas */
    sm->handlers[STATE_IDLE][EVENT_CONNECT] = handler_idle_connect;
    sm->handlers[STATE_CONNECTING][EVENT_DATA] = handler_connecting_data;
    sm->handlers[STATE_CONNECTING][EVENT_TIMEOUT] = handler_connecting_timeout;
    sm->handlers[STATE_CONNECTED][EVENT_DATA] = handler_connected_data;
    sm->handlers[STATE_CONNECTED][EVENT_DISCONNECT] = handler_connected_disconnect;
}

/* Procesar evento */
int sm_process(StateMachine *sm, Evento evento, void *data) {
    EventHandler handler = sm->handlers[sm->estado_actual][evento];

    if (handler == NULL) {
        printf("Evento %d no válido en estado %d\n",
               evento, sm->estado_actual);
        return -1;
    }

    Estado nuevo_estado = handler(sm, data);
    printf("Transición: %d -> %d\n", sm->estado_actual, nuevo_estado);
    sm->estado_actual = nuevo_estado;

    return 0;
}
```

---

## Referencia Rápida

### Funciones pthread

| Función | Descripción |
|---------|-------------|
| `pthread_create()` | Crear thread |
| `pthread_join()` | Esperar thread |
| `pthread_exit()` | Terminar thread |
| `pthread_mutex_lock()` | Bloquear mutex |
| `pthread_mutex_unlock()` | Desbloquear mutex |
| `pthread_cond_wait()` | Esperar condición |
| `pthread_cond_signal()` | Señalar condición |
| `pthread_rwlock_rdlock()` | Lock lectura |
| `pthread_rwlock_wrlock()` | Lock escritura |

### Señales Comunes

| Señal | Número | Acción Default |
|-------|--------|----------------|
| SIGINT | 2 | Terminar |
| SIGKILL | 9 | Terminar (no capturable) |
| SIGSEGV | 11 | Core dump |
| SIGTERM | 15 | Terminar |
| SIGUSR1 | 10 | Terminar |

### Flags de Optimización GCC

```bash
-O0        # Sin optimización
-O2        # Producción (recomendado)
-O3        # Agresivo
-march=native  # Optimizar para CPU actual
-flto      # Link-time optimization
-ffast-math    # Matemáticas rápidas (no IEEE)
```

---

## Conclusión

La programación avanzada en C abre las puertas al control total del sistema. Desde el manejo de múltiples hilos de ejecución hasta la comunicación entre procesos, desde la optimización de bajo nivel hasta el inline assembly, C proporciona las herramientas para construir software de alto rendimiento.

**Puntos clave:**
1. Threads requieren sincronización cuidadosa para evitar race conditions
2. Las señales son asíncronas - handlers deben ser async-signal-safe
3. IPC ofrece múltiples mecanismos según necesidades
4. Optimización: medir primero, optimizar donde importa
5. Inline assembly solo cuando sea absolutamente necesario

---

*"El verdadero maestro de C no teme a los hilos ni a las señales - los domina con disciplina y comprensión profunda del sistema."*

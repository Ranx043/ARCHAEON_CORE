---
title: "ARSENAL Reference: C Standard Library"
category: reference
domain: systems
language: C
standards: [C89, C99, C11, C17, C23]
version: 1.0.0
last_updated: 2025-12-31
tags: [c, stdlib, libc, posix, systems-programming]
---

# ARSENAL_REF_CLIB
## Complete C Standard Library Reference

---

# TABLE OF CONTENTS

1. [stdio.h - Input/Output](#stdioh---inputoutput)
2. [stdlib.h - General Utilities](#stdlibh---general-utilities)
3. [string.h - String Handling](#stringh---string-handling)
4. [math.h - Mathematics](#mathh---mathematics)
5. [time.h - Date and Time](#timeh---date-and-time)
6. [POSIX Extensions](#posix-extensions)
7. [Common Pitfalls](#common-pitfalls)

---

# stdio.h - INPUT/OUTPUT

## printf Format Specifiers

### Basic Specifiers

| Specifier | Description | Example | Output |
|-----------|-------------|---------|--------|
| `%d` / `%i` | Signed decimal integer | `printf("%d", -42)` | `-42` |
| `%u` | Unsigned decimal integer | `printf("%u", 42)` | `42` |
| `%o` | Unsigned octal | `printf("%o", 64)` | `100` |
| `%x` | Unsigned hex (lowercase) | `printf("%x", 255)` | `ff` |
| `%X` | Unsigned hex (uppercase) | `printf("%X", 255)` | `FF` |
| `%f` | Floating point (lowercase) | `printf("%f", 3.14)` | `3.140000` |
| `%F` | Floating point (uppercase) | `printf("%F", INFINITY)` | `INF` |
| `%e` | Scientific (lowercase) | `printf("%e", 1234.5)` | `1.234500e+03` |
| `%E` | Scientific (uppercase) | `printf("%E", 1234.5)` | `1.234500E+03` |
| `%g` | Shorter of %e or %f | `printf("%g", 0.00001)` | `1e-05` |
| `%G` | Shorter of %E or %F | `printf("%G", 0.00001)` | `1E-05` |
| `%a` | Hex floating point | `printf("%a", 15.5)` | `0x1.fp+3` |
| `%c` | Character | `printf("%c", 65)` | `A` |
| `%s` | String | `printf("%s", "hi")` | `hi` |
| `%p` | Pointer | `printf("%p", ptr)` | `0x7ffd5e8c` |
| `%n` | Chars written so far | (stores to int*) | - |
| `%%` | Literal % | `printf("%%")` | `%` |

### Length Modifiers

| Modifier | d/i | u/o/x/X | n |
|----------|-----|---------|---|
| `hh` | signed char | unsigned char | signed char* |
| `h` | short | unsigned short | short* |
| (none) | int | unsigned int | int* |
| `l` | long | unsigned long | long* |
| `ll` | long long | unsigned long long | long long* |
| `j` | intmax_t | uintmax_t | intmax_t* |
| `z` | ssize_t | size_t | size_t* |
| `t` | ptrdiff_t | ptrdiff_t | ptrdiff_t* |
| `L` | - | - | - (for long double with f/e/g) |

### Width and Precision

| Format | Description | Example | Output |
|--------|-------------|---------|--------|
| `%10d` | Min width 10, right-aligned | `printf("%10d", 42)` | `        42` |
| `%-10d` | Min width 10, left-aligned | `printf("%-10d", 42)` | `42        ` |
| `%010d` | Zero-padded to width 10 | `printf("%010d", 42)` | `0000000042` |
| `%+d` | Always show sign | `printf("%+d", 42)` | `+42` |
| `% d` | Space for positive | `printf("% d", 42)` | ` 42` |
| `%#x` | Alternate form (0x prefix) | `printf("%#x", 255)` | `0xff` |
| `%#o` | Alternate form (0 prefix) | `printf("%#o", 64)` | `0100` |
| `%.2f` | Precision 2 decimals | `printf("%.2f", 3.14159)` | `3.14` |
| `%.5s` | Max 5 chars from string | `printf("%.5s", "hello world")` | `hello` |
| `%*d` | Width from argument | `printf("%*d", 5, 42)` | `   42` |
| `%.*f` | Precision from argument | `printf("%.*f", 2, 3.14159)` | `3.14` |

## File Operations

### Opening and Closing

```c
FILE *fopen(const char *path, const char *mode);
FILE *freopen(const char *path, const char *mode, FILE *stream);
int fclose(FILE *stream);
int fflush(FILE *stream);
```

### File Modes

| Mode | Description | File Exists | File Not Exists |
|------|-------------|-------------|-----------------|
| `"r"` | Read text | Open | Fail |
| `"w"` | Write text | Truncate | Create |
| `"a"` | Append text | Append | Create |
| `"r+"` | Read/Write text | Open | Fail |
| `"w+"` | Read/Write text | Truncate | Create |
| `"a+"` | Read/Append text | Append | Create |
| `"rb"` | Read binary | Open | Fail |
| `"wb"` | Write binary | Truncate | Create |
| `"ab"` | Append binary | Append | Create |
| `"r+b"` / `"rb+"` | Read/Write binary | Open | Fail |
| `"w+b"` / `"wb+"` | Read/Write binary | Truncate | Create |
| `"a+b"` / `"ab+"` | Read/Append binary | Append | Create |

### Reading Functions

```c
int fgetc(FILE *stream);                              // Read char, return EOF on error
char *fgets(char *s, int n, FILE *stream);            // Read line (safe, includes \n)
int getchar(void);                                     // Read char from stdin
size_t fread(void *ptr, size_t size, size_t n, FILE *stream);  // Read binary

int fscanf(FILE *stream, const char *format, ...);    // Formatted input
int scanf(const char *format, ...);                   // From stdin
int sscanf(const char *s, const char *format, ...);   // From string
```

### Writing Functions

```c
int fputc(int c, FILE *stream);                       // Write char
int fputs(const char *s, FILE *stream);               // Write string (no \n)
int putchar(int c);                                    // Write char to stdout
int puts(const char *s);                              // Write string + \n to stdout
size_t fwrite(const void *ptr, size_t size, size_t n, FILE *stream);  // Write binary

int fprintf(FILE *stream, const char *format, ...);   // Formatted output
int printf(const char *format, ...);                  // To stdout
int sprintf(char *s, const char *format, ...);        // To string (UNSAFE!)
int snprintf(char *s, size_t n, const char *format, ...);  // Safe string output
```

### Positioning

```c
int fseek(FILE *stream, long offset, int whence);     // Set position
long ftell(FILE *stream);                             // Get position
void rewind(FILE *stream);                            // Go to beginning
int fgetpos(FILE *stream, fpos_t *pos);              // Get position (portable)
int fsetpos(FILE *stream, const fpos_t *pos);        // Set position (portable)
```

| whence | Description |
|--------|-------------|
| `SEEK_SET` | Beginning of file |
| `SEEK_CUR` | Current position |
| `SEEK_END` | End of file |

### Error Handling

```c
int feof(FILE *stream);                               // Check end-of-file
int ferror(FILE *stream);                             // Check error
void clearerr(FILE *stream);                          // Clear indicators
void perror(const char *s);                           // Print error message
```

---

# stdlib.h - GENERAL UTILITIES

## Memory Allocation

| Function | Description | Safety |
|----------|-------------|--------|
| `void *malloc(size_t size)` | Allocate uninitialized memory | Check NULL |
| `void *calloc(size_t n, size_t size)` | Allocate zeroed memory | Check NULL, overflow safe |
| `void *realloc(void *ptr, size_t size)` | Resize allocation | Check NULL |
| `void free(void *ptr)` | Deallocate memory | NULL-safe |
| `void *aligned_alloc(size_t align, size_t size)` | Aligned allocation (C11) | Check NULL |

### Memory Examples
```c
// Basic allocation
int *arr = malloc(100 * sizeof(int));
if (arr == NULL) { /* handle error */ }

// Zero-initialized allocation (safer for size*count)
int *arr = calloc(100, sizeof(int));

// Resizing - save old pointer!
int *new_arr = realloc(arr, 200 * sizeof(int));
if (new_arr == NULL) {
    free(arr);  // Original still valid
    /* handle error */
}
arr = new_arr;

// Always free when done
free(arr);
arr = NULL;  // Prevent use-after-free
```

## String Conversions

| Function | Description | Base |
|----------|-------------|------|
| `int atoi(const char *s)` | String to int | 10 |
| `long atol(const char *s)` | String to long | 10 |
| `long long atoll(const char *s)` | String to long long | 10 |
| `double atof(const char *s)` | String to double | - |
| `long strtol(const char *s, char **end, int base)` | String to long | 2-36 |
| `unsigned long strtoul(const char *s, char **end, int base)` | String to unsigned long | 2-36 |
| `long long strtoll(const char *s, char **end, int base)` | String to long long | 2-36 |
| `double strtod(const char *s, char **end)` | String to double | - |
| `float strtof(const char *s, char **end)` | String to float | - |
| `long double strtold(const char *s, char **end)` | String to long double | - |

### Conversion Examples
```c
// Safe conversion with error checking
char *input = "42abc";
char *end;
errno = 0;
long value = strtol(input, &end, 10);

if (errno == ERANGE) {
    // Overflow or underflow
} else if (end == input) {
    // No digits found
} else if (*end != '\0') {
    // Trailing characters (end points to 'a')
} else {
    // Success
}

// Different bases
strtol("0xFF", NULL, 0);   // Auto-detect: 255
strtol("FF", NULL, 16);    // Hex: 255
strtol("377", NULL, 8);    // Octal: 255
strtol("11111111", NULL, 2); // Binary: 255
```

## Random Numbers

```c
int rand(void);                    // Returns 0 to RAND_MAX
void srand(unsigned int seed);     // Seed the generator

// Example: random in range [min, max]
int random_range(int min, int max) {
    return min + rand() % (max - min + 1);
}

// Better seeding
srand((unsigned int)time(NULL));
```

## Environment and Program Control

```c
char *getenv(const char *name);                       // Get env variable
int setenv(const char *name, const char *value, int overwrite);  // POSIX
int putenv(char *string);                             // Set env (POSIX)

void exit(int status);                                // Normal termination
void _Exit(int status);                               // Immediate termination (C99)
void abort(void);                                     // Abnormal termination
int atexit(void (*func)(void));                      // Register exit handler
int at_quick_exit(void (*func)(void));               // C11 quick_exit handler

int system(const char *command);                      // Execute shell command
```

## Searching and Sorting

```c
void qsort(void *base, size_t n, size_t size,
           int (*compar)(const void *, const void *));

void *bsearch(const void *key, const void *base, size_t n,
              size_t size, int (*compar)(const void *, const void *));
```

### Sorting Example
```c
int compare_ints(const void *a, const void *b) {
    int ia = *(const int *)a;
    int ib = *(const int *)b;
    return (ia > ib) - (ia < ib);  // Safe comparison
}

int arr[] = {5, 2, 8, 1, 9};
qsort(arr, 5, sizeof(int), compare_ints);

int key = 8;
int *found = bsearch(&key, arr, 5, sizeof(int), compare_ints);
```

## Integer Arithmetic

```c
int abs(int n);
long labs(long n);
long long llabs(long long n);

div_t div(int numer, int denom);       // Returns {quot, rem}
ldiv_t ldiv(long numer, long denom);
lldiv_t lldiv(long long numer, long long denom);
```

---

# string.h - STRING HANDLING

## String Operations

| Function | Description | Safety Notes |
|----------|-------------|--------------|
| `size_t strlen(const char *s)` | String length | No NULL check |
| `char *strcpy(char *dst, const char *src)` | Copy string | UNSAFE: no bounds |
| `char *strncpy(char *dst, const char *src, size_t n)` | Copy n chars | May not null-terminate! |
| `char *strcat(char *dst, const char *src)` | Concatenate | UNSAFE: no bounds |
| `char *strncat(char *dst, const char *src, size_t n)` | Concat n chars | Always null-terminates |
| `int strcmp(const char *s1, const char *s2)` | Compare strings | Returns <0, 0, >0 |
| `int strncmp(const char *s1, const char *s2, size_t n)` | Compare n chars | Safer for buffers |
| `char *strchr(const char *s, int c)` | Find char (first) | Returns NULL if not found |
| `char *strrchr(const char *s, int c)` | Find char (last) | Returns NULL if not found |
| `char *strstr(const char *hay, const char *needle)` | Find substring | Returns NULL if not found |
| `char *strpbrk(const char *s, const char *accept)` | Find any of chars | - |
| `size_t strspn(const char *s, const char *accept)` | Length of prefix in set | - |
| `size_t strcspn(const char *s, const char *reject)` | Length until char in set | - |
| `char *strtok(char *s, const char *delim)` | Tokenize string | UNSAFE: modifies, not reentrant |

### Safe String Examples
```c
// UNSAFE: strcpy can overflow
char buf[10];
strcpy(buf, "this is way too long");  // BUFFER OVERFLOW!

// SAFER: strncpy (but watch null-termination)
strncpy(buf, src, sizeof(buf) - 1);
buf[sizeof(buf) - 1] = '\0';  // Ensure null-termination

// BEST: snprintf (always null-terminates)
snprintf(buf, sizeof(buf), "%s", src);

// Safe concatenation
char result[100] = "Hello, ";
size_t len = strlen(result);
snprintf(result + len, sizeof(result) - len, "%s!", name);
```

## Memory Operations

| Function | Description | Safety Notes |
|----------|-------------|--------------|
| `void *memset(void *s, int c, size_t n)` | Fill memory | - |
| `void *memcpy(void *dst, const void *src, size_t n)` | Copy memory | No overlap! |
| `void *memmove(void *dst, const void *src, size_t n)` | Copy (overlap safe) | Slower but safe |
| `int memcmp(const void *s1, const void *s2, size_t n)` | Compare memory | - |
| `void *memchr(const void *s, int c, size_t n)` | Find byte | - |

### Memory Examples
```c
// Zero a structure
struct data d;
memset(&d, 0, sizeof(d));

// Copy structures
struct data src, dst;
memcpy(&dst, &src, sizeof(struct data));

// Safe for overlapping regions
memmove(buf + 5, buf, strlen(buf) + 1);  // Shift string right

// Compare memory
if (memcmp(buf1, buf2, sizeof(buf1)) == 0) {
    // Buffers are identical
}
```

## Error Messages

```c
char *strerror(int errnum);           // Error number to string
int strerror_r(int errnum, char *buf, size_t buflen);  // Thread-safe (POSIX)
```

---

# math.h - MATHEMATICS

## Basic Functions

| Function | Description | Domain | Example |
|----------|-------------|--------|---------|
| `double fabs(double x)` | Absolute value | All | `fabs(-3.5)` = 3.5 |
| `double floor(double x)` | Round down | All | `floor(3.7)` = 3.0 |
| `double ceil(double x)` | Round up | All | `ceil(3.1)` = 4.0 |
| `double round(double x)` | Round to nearest | All | `round(3.5)` = 4.0 |
| `double trunc(double x)` | Truncate toward zero | All | `trunc(-3.7)` = -3.0 |
| `double fmod(double x, double y)` | Floating modulo | y != 0 | `fmod(5.3, 2)` = 1.3 |
| `double remainder(double x, double y)` | IEEE remainder | y != 0 | - |
| `double fmax(double x, double y)` | Maximum | All | `fmax(3, 5)` = 5 |
| `double fmin(double x, double y)` | Minimum | All | `fmin(3, 5)` = 3 |
| `double fdim(double x, double y)` | Positive difference | All | `fdim(5, 3)` = 2 |
| `double fma(double x, double y, double z)` | Fused multiply-add | All | x*y+z |

## Exponential and Logarithmic

| Function | Description | Example |
|----------|-------------|---------|
| `double exp(double x)` | e^x | `exp(1)` = 2.718... |
| `double exp2(double x)` | 2^x | `exp2(3)` = 8 |
| `double expm1(double x)` | e^x - 1 (precise) | - |
| `double log(double x)` | Natural log | `log(E)` = 1 |
| `double log10(double x)` | Base-10 log | `log10(100)` = 2 |
| `double log2(double x)` | Base-2 log | `log2(8)` = 3 |
| `double log1p(double x)` | log(1+x) (precise) | - |
| `double pow(double x, double y)` | x^y | `pow(2, 10)` = 1024 |
| `double sqrt(double x)` | Square root | `sqrt(16)` = 4 |
| `double cbrt(double x)` | Cube root | `cbrt(27)` = 3 |
| `double hypot(double x, double y)` | sqrt(x^2+y^2) | `hypot(3, 4)` = 5 |

## Trigonometric Functions

| Function | Description | Domain/Range |
|----------|-------------|--------------|
| `double sin(double x)` | Sine | x in radians, [-1, 1] |
| `double cos(double x)` | Cosine | x in radians, [-1, 1] |
| `double tan(double x)` | Tangent | x in radians |
| `double asin(double x)` | Arc sine | x in [-1,1], returns [-pi/2, pi/2] |
| `double acos(double x)` | Arc cosine | x in [-1,1], returns [0, pi] |
| `double atan(double x)` | Arc tangent | returns [-pi/2, pi/2] |
| `double atan2(double y, double x)` | Arc tangent of y/x | returns [-pi, pi] |

## Hyperbolic Functions

| Function | Description |
|----------|-------------|
| `double sinh(double x)` | Hyperbolic sine |
| `double cosh(double x)` | Hyperbolic cosine |
| `double tanh(double x)` | Hyperbolic tangent |
| `double asinh(double x)` | Inverse hyperbolic sine |
| `double acosh(double x)` | Inverse hyperbolic cosine |
| `double atanh(double x)` | Inverse hyperbolic tangent |

## Special Functions

| Function | Description |
|----------|-------------|
| `double erf(double x)` | Error function |
| `double erfc(double x)` | Complementary error function |
| `double tgamma(double x)` | Gamma function |
| `double lgamma(double x)` | Log gamma function |

## Floating-Point Classification

| Macro/Function | Description |
|----------------|-------------|
| `int fpclassify(x)` | Classify float |
| `int isfinite(x)` | Not inf or NaN |
| `int isinf(x)` | Is infinity |
| `int isnan(x)` | Is NaN |
| `int isnormal(x)` | Is normalized |
| `int signbit(x)` | Sign bit set |

### Classification Constants
| Constant | Meaning |
|----------|---------|
| `FP_INFINITE` | Positive or negative infinity |
| `FP_NAN` | Not a number |
| `FP_NORMAL` | Normal value |
| `FP_SUBNORMAL` | Subnormal (denormalized) |
| `FP_ZERO` | Positive or negative zero |

## Float/Double/Long Double Variants

Most functions have variants for different types:

| double | float | long double |
|--------|-------|-------------|
| `sin` | `sinf` | `sinl` |
| `cos` | `cosf` | `cosl` |
| `sqrt` | `sqrtf` | `sqrtl` |
| `pow` | `powf` | `powl` |
| `fabs` | `fabsf` | `fabsl` |

---

# time.h - DATE AND TIME

## Types

| Type | Description |
|------|-------------|
| `time_t` | Calendar time (usually seconds since epoch) |
| `clock_t` | Processor time |
| `struct tm` | Broken-down time |
| `struct timespec` | Nanosecond precision (C11) |

### struct tm Members

| Member | Description | Range |
|--------|-------------|-------|
| `tm_sec` | Seconds | 0-60 |
| `tm_min` | Minutes | 0-59 |
| `tm_hour` | Hours | 0-23 |
| `tm_mday` | Day of month | 1-31 |
| `tm_mon` | Month | 0-11 |
| `tm_year` | Years since 1900 | - |
| `tm_wday` | Day of week | 0-6 (Sun=0) |
| `tm_yday` | Day of year | 0-365 |
| `tm_isdst` | Daylight saving | >0, 0, <0 |

## Time Functions

```c
time_t time(time_t *tloc);                    // Current calendar time
clock_t clock(void);                          // Processor time used
double difftime(time_t t1, time_t t0);        // Difference in seconds

struct tm *gmtime(const time_t *timer);       // Convert to UTC (not thread-safe)
struct tm *localtime(const time_t *timer);    // Convert to local (not thread-safe)
time_t mktime(struct tm *tm);                 // Convert tm to time_t

char *ctime(const time_t *timer);             // time_t to string
char *asctime(const struct tm *tm);           // tm to string
size_t strftime(char *s, size_t max, const char *fmt, const struct tm *tm);
```

### strftime Format Specifiers

| Specifier | Description | Example |
|-----------|-------------|---------|
| `%Y` | 4-digit year | 2024 |
| `%y` | 2-digit year | 24 |
| `%m` | Month (01-12) | 12 |
| `%d` | Day of month (01-31) | 25 |
| `%H` | Hour 24h (00-23) | 14 |
| `%I` | Hour 12h (01-12) | 02 |
| `%M` | Minute (00-59) | 30 |
| `%S` | Second (00-60) | 45 |
| `%p` | AM/PM | PM |
| `%A` | Full weekday | Wednesday |
| `%a` | Abbrev weekday | Wed |
| `%B` | Full month | December |
| `%b` | Abbrev month | Dec |
| `%j` | Day of year (001-366) | 360 |
| `%U` | Week number (Sun start) | 51 |
| `%W` | Week number (Mon start) | 52 |
| `%Z` | Timezone name | EST |
| `%z` | Timezone offset | -0500 |
| `%%` | Literal % | % |

### Time Examples
```c
// Get current time
time_t now = time(NULL);

// Format as string
char buf[64];
struct tm *tm_info = localtime(&now);
strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", tm_info);
printf("%s\n", buf);  // 2024-12-25 14:30:45

// Measure elapsed time
clock_t start = clock();
// ... work ...
clock_t end = clock();
double cpu_time = (double)(end - start) / CLOCKS_PER_SEC;

// Build a specific date
struct tm date = {0};
date.tm_year = 2024 - 1900;
date.tm_mon = 11;  // December (0-indexed)
date.tm_mday = 25;
time_t christmas = mktime(&date);
```

---

# POSIX EXTENSIONS

## File Descriptors

```c
int open(const char *path, int flags, mode_t mode);
int close(int fd);
ssize_t read(int fd, void *buf, size_t count);
ssize_t write(int fd, const void *buf, size_t count);
off_t lseek(int fd, off_t offset, int whence);
int dup(int oldfd);
int dup2(int oldfd, int newfd);
int pipe(int pipefd[2]);
```

### Open Flags

| Flag | Description |
|------|-------------|
| `O_RDONLY` | Read only |
| `O_WRONLY` | Write only |
| `O_RDWR` | Read and write |
| `O_CREAT` | Create if not exists |
| `O_TRUNC` | Truncate to zero |
| `O_APPEND` | Append mode |
| `O_EXCL` | Fail if exists (with O_CREAT) |
| `O_NONBLOCK` | Non-blocking I/O |

## String Functions

```c
char *strdup(const char *s);                  // Duplicate string (malloc'd)
char *strndup(const char *s, size_t n);       // Duplicate n chars
size_t strnlen(const char *s, size_t maxlen); // Bounded strlen
int strcasecmp(const char *s1, const char *s2);     // Case-insensitive compare
int strncasecmp(const char *s1, const char *s2, size_t n);
char *strtok_r(char *s, const char *delim, char **saveptr);  // Thread-safe strtok
```

## Memory Mapping

```c
void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset);
int munmap(void *addr, size_t length);
int mprotect(void *addr, size_t len, int prot);
int msync(void *addr, size_t length, int flags);
```

## Directory Operations

```c
DIR *opendir(const char *name);
struct dirent *readdir(DIR *dirp);
int closedir(DIR *dirp);
int mkdir(const char *path, mode_t mode);
int rmdir(const char *path);
char *getcwd(char *buf, size_t size);
int chdir(const char *path);
```

---

# COMMON PITFALLS

## Buffer Overflows

```c
// DANGEROUS: No bounds checking
char buf[10];
gets(buf);                    // NEVER USE - removed in C11
strcpy(buf, user_input);      // Overflow if input > 9 chars
sprintf(buf, "%s", input);    // Overflow possible

// SAFE alternatives
fgets(buf, sizeof(buf), stdin);
strncpy(buf, input, sizeof(buf) - 1); buf[sizeof(buf)-1] = '\0';
snprintf(buf, sizeof(buf), "%s", input);
```

## Memory Leaks

```c
// LEAK: Overwriting pointer before free
char *p = malloc(100);
p = malloc(200);  // First allocation leaked!
free(p);

// CORRECT
char *p = malloc(100);
free(p);
p = malloc(200);
free(p);
```

## Null Pointer Dereference

```c
// DANGEROUS
char *p = malloc(100);
strcpy(p, "hello");  // Crash if malloc failed!

// SAFE
char *p = malloc(100);
if (p == NULL) {
    // Handle error
    return;
}
strcpy(p, "hello");
```

## Integer Overflow

```c
// DANGEROUS: Integer overflow before malloc
size_t n = get_user_count();
size_t size = n * sizeof(int);  // Can overflow!
int *arr = malloc(size);

// SAFE: Check for overflow
if (n > SIZE_MAX / sizeof(int)) {
    // Overflow would occur
    return NULL;
}
int *arr = malloc(n * sizeof(int));

// BEST: Use calloc (checks internally)
int *arr = calloc(n, sizeof(int));
```

## Format String Vulnerabilities

```c
// DANGEROUS: User controls format string
char *user_input = get_input();
printf(user_input);  // Format string attack!

// SAFE: Always use format string
printf("%s", user_input);
```

## Use After Free

```c
// DANGEROUS
free(ptr);
ptr->value = 5;  // Use after free!

// SAFE: Null after free
free(ptr);
ptr = NULL;
```

## Secure Alternatives Summary

| Unsafe | Safe Alternative | Notes |
|--------|------------------|-------|
| `gets()` | `fgets()` | `gets` removed in C11 |
| `strcpy()` | `strncpy()`, `snprintf()` | Always null-terminate |
| `strcat()` | `strncat()`, `snprintf()` | Track remaining space |
| `sprintf()` | `snprintf()` | Always use buffer size |
| `scanf("%s")` | `scanf("%99s")` | Limit field width |
| `strtok()` | `strtok_r()` | Thread-safe version |
| `gmtime()` | `gmtime_r()` | Thread-safe version |
| `localtime()` | `localtime_r()` | Thread-safe version |
| `ctime()` | `ctime_r()` / `strftime()` | Thread-safe version |
| `asctime()` | `asctime_r()` / `strftime()` | Thread-safe version |

---

*ARCHAEON Arsenal - C Standard Library Reference v1.0*
*"With great power comes great responsibility for memory management"*

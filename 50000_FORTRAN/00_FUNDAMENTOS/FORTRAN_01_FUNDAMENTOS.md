# FORTRAN_01: FUNDAMENTOS DEL LENGUAJE

> *"En el principio fue FORTRAN - el primer lenguaje de alto nivel, nacido para domar la bestia de los números y hacer cantar a las máquinas científicas."*

## Índice
1. [Historia y Evolución](#1-historia-y-evolución)
2. [Estructura del Programa](#2-estructura-del-programa)
3. [Tipos de Datos](#3-tipos-de-datos)
4. [Variables y Constantes](#4-variables-y-constantes)
5. [Operadores](#5-operadores)
6. [Control de Flujo](#6-control-de-flujo)
7. [Procedimientos](#7-procedimientos)
8. [Formato Libre vs Fijo](#8-formato-libre-vs-fijo)

---

## 1. Historia y Evolución

### 1.1 El Nacimiento de FORTRAN

```
CRONOLOGÍA FORTRAN
==================

1954-1957: FORTRAN I (IBM 704)
           - John Backus y equipo IBM
           - Primer lenguaje de alto nivel exitoso
           - "FORmula TRANslation"
           - Revolucionó la programación científica

1958: FORTRAN II
      - Subrutinas y funciones separadas
      - COMMON blocks

1962: FORTRAN IV
      - Tipos de datos explícitos
      - Mayor portabilidad

1966: FORTRAN 66 (ANSI X3.9-1966)
      - Primer estándar oficial ANSI
      - Base para portabilidad

1978: FORTRAN 77 (ANSI X3.9-1978)
      - Structured programming
      - CHARACTER data type
      - IF-THEN-ELSE blocks
      - Más usado en código legacy

1991: Fortran 90 (ISO/IEC 1539:1991)
      - Formato libre
      - Módulos
      - Arrays dinámicos
      - Punteros
      - Recursión
      - Tipos derivados

1997: Fortran 95
      - FORALL y PURE procedures
      - Mejoras a arrays

2004: Fortran 2003
      - Programación orientada a objetos
      - Interoperabilidad con C
      - IEEE floating point

2010: Fortran 2008
      - Coarrays para paralelismo
      - DO CONCURRENT

2018: Fortran 2018
      - Mejoras a coarrays
      - Enhanced C interoperability
```

### 1.2 Dominio de FORTRAN

```fortran
! FORTRAN domina en:
!
! 1. COMPUTACIÓN CIENTÍFICA
!    - Física computacional
!    - Química cuántica
!    - Dinámica molecular
!
! 2. INGENIERÍA
!    - Análisis por elementos finitos (FEM)
!    - Dinámica de fluidos (CFD)
!    - Análisis estructural
!
! 3. CLIMA Y METEOROLOGÍA
!    - Modelos climáticos globales
!    - Predicción del tiempo
!
! 4. HIGH PERFORMANCE COMPUTING (HPC)
!    - Supercomputadoras
!    - Simulaciones a gran escala
!
! Bibliotecas clásicas en FORTRAN:
!    - BLAS (Basic Linear Algebra)
!    - LAPACK (Linear Algebra Package)
!    - FFTW (Fast Fourier Transform)
!    - ARPACK (Eigenvalue problems)
```

---

## 2. Estructura del Programa

### 2.1 Programa Principal

```fortran
! ========================================
! ESTRUCTURA BÁSICA - Fortran 90+
! ========================================

program nombre_programa
    ! Declaraciones implícitas (desactivar recomendado)
    implicit none

    ! Declaración de variables
    integer :: contador
    real :: resultado
    character(len=20) :: nombre

    ! Cuerpo del programa
    contador = 0
    resultado = 0.0
    nombre = "FORTRAN"

    ! Llamadas a procedimientos
    call mi_subrutina(contador)
    resultado = mi_funcion(3.14)

    ! Salida
    print *, "Resultado:", resultado

end program nombre_programa
```

### 2.2 Estructura Fortran 77 (Legacy)

```fortran
C ========================================
C ESTRUCTURA FORTRAN 77 (FORMATO FIJO)
C ========================================
C Columnas:
C   1     : Comentario (C, c, *, !)
C   1-5   : Etiquetas numéricas
C   6     : Continuación (cualquier carácter)
C   7-72  : Código
C   73-80 : Secuencia (ignorado)

      PROGRAM EJEMPLO
C     Declaraciones
      INTEGER I, J, K
      REAL X, Y, Z
      CHARACTER*20 NOMBRE

C     Código
      I = 10
      X = 3.14159
      NOMBRE = 'FORTRAN 77'

C     Línea de continuación (+ en columna 6)
      Y = 1.0 + 2.0 + 3.0 + 4.0 + 5.0 +
     +    6.0 + 7.0 + 8.0 + 9.0 + 10.0

      WRITE(*,*) 'Resultado: ', Y

      STOP
      END
```

### 2.3 Unidades de Programa

```fortran
! ========================================
! TIPOS DE UNIDADES DE PROGRAMA
! ========================================

! 1. PROGRAM - Programa principal
program main
    implicit none
    call worker()
end program main

! 2. SUBROUTINE - Procedimiento sin valor de retorno
subroutine worker()
    implicit none
    print *, "Trabajando..."
end subroutine worker

! 3. FUNCTION - Procedimiento con valor de retorno
function square(x) result(res)
    implicit none
    real, intent(in) :: x
    real :: res
    res = x * x
end function square

! 4. MODULE - Contenedor de datos y procedimientos
module utilidades
    implicit none
    real, parameter :: PI = 3.14159265359
contains
    function circunferencia(r) result(c)
        real, intent(in) :: r
        real :: c
        c = 2.0 * PI * r
    end function circunferencia
end module utilidades

! 5. BLOCK DATA - Inicialización de COMMON (legacy)
block data inicializador
    common /bloque1/ x, y, z
    real x, y, z
    data x, y, z /1.0, 2.0, 3.0/
end block data inicializador
```

---

## 3. Tipos de Datos

### 3.1 Tipos Intrínsecos

```fortran
program tipos_datos
    implicit none

    ! ========================================
    ! TIPOS NUMÉRICOS ENTEROS
    ! ========================================

    ! INTEGER - Entero por defecto (generalmente 32 bits)
    integer :: entero_normal

    ! Especificar KIND para tamaño específico
    integer(kind=1) :: entero_1byte    ! -128 a 127
    integer(kind=2) :: entero_2bytes   ! -32768 a 32767
    integer(kind=4) :: entero_4bytes   ! ~±2 mil millones
    integer(kind=8) :: entero_8bytes   ! ~±9 quintillones

    ! Usando selected_int_kind para portabilidad
    integer, parameter :: short = selected_int_kind(4)   ! Al menos 4 dígitos
    integer, parameter :: long = selected_int_kind(15)   ! Al menos 15 dígitos
    integer(kind=short) :: pequeño
    integer(kind=long) :: grande

    ! ========================================
    ! TIPOS NUMÉRICOS REALES (Punto flotante)
    ! ========================================

    ! REAL - Precisión simple (32 bits, ~7 dígitos)
    real :: real_simple

    ! DOUBLE PRECISION - Doble precisión (64 bits, ~15 dígitos)
    double precision :: real_doble

    ! Con KIND explícito
    real(kind=4) :: single_precision   ! 32 bits
    real(kind=8) :: double_precision   ! 64 bits
    real(kind=16) :: quad_precision    ! 128 bits (si disponible)

    ! Usando selected_real_kind para portabilidad
    integer, parameter :: sp = selected_real_kind(6, 37)   ! 6 dígitos, exp 37
    integer, parameter :: dp = selected_real_kind(15, 307) ! 15 dígitos, exp 307
    real(kind=sp) :: x_simple
    real(kind=dp) :: x_doble

    ! ========================================
    ! NÚMEROS COMPLEJOS
    ! ========================================

    complex :: z1
    complex(kind=8) :: z2      ! Doble precisión
    complex(kind=dp) :: z3     ! Con kind definido

    z1 = (3.0, 4.0)           ! 3 + 4i
    z2 = cmplx(1.0_dp, 2.0_dp, kind=dp)

    print *, "Parte real:", real(z1)
    print *, "Parte imaginaria:", aimag(z1)
    print *, "Conjugado:", conjg(z1)
    print *, "Magnitud:", abs(z1)

    ! ========================================
    ! LÓGICOS
    ! ========================================

    logical :: flag
    logical(kind=1) :: flag_byte

    flag = .true.
    flag = .false.

    ! ========================================
    ! CARACTERES
    ! ========================================

    character :: letra                    ! Un carácter
    character(len=20) :: nombre           ! String de 20 caracteres
    character(len=*), parameter :: cte = "Constante"  ! Longitud asumida
    character(len=:), allocatable :: dinamico  ! Longitud dinámica (F2003)

    nombre = "FORTRAN"
    print *, "Longitud:", len(nombre)
    print *, "Sin espacios:", len_trim(nombre)

end program tipos_datos
```

### 3.2 Tipos Derivados (Estructuras)

```fortran
program tipos_derivados
    implicit none

    ! ========================================
    ! DEFINICIÓN DE TIPO DERIVADO
    ! ========================================

    type :: punto
        real :: x
        real :: y
        real :: z
    end type punto

    type :: particula
        type(punto) :: posicion
        type(punto) :: velocidad
        real :: masa
        character(len=20) :: nombre
    end type particula

    ! ========================================
    ! USO DE TIPOS DERIVADOS
    ! ========================================

    type(punto) :: p1, p2
    type(particula) :: electron

    ! Inicialización con constructor
    p1 = punto(1.0, 2.0, 3.0)

    ! Acceso a componentes con %
    p2%x = 4.0
    p2%y = 5.0
    p2%z = 6.0

    ! Tipo anidado
    electron%posicion = punto(0.0, 0.0, 0.0)
    electron%velocidad%x = 1.0e6
    electron%masa = 9.109e-31
    electron%nombre = "electron"

    print *, "Posición:", electron%posicion
    print *, "Masa:", electron%masa

    ! ========================================
    ! TIPO CON COMPONENTES ALLOCATABLE (F2003)
    ! ========================================

    type :: vector_dinamico
        real, allocatable :: datos(:)
        integer :: tamanio
    end type vector_dinamico

    type(vector_dinamico) :: vec

    vec%tamanio = 100
    allocate(vec%datos(vec%tamanio))
    vec%datos = 0.0

    deallocate(vec%datos)

end program tipos_derivados
```

### 3.3 Conversiones de Tipo

```fortran
program conversiones
    implicit none

    integer :: i
    real :: r
    double precision :: d
    complex :: z
    character(len=20) :: s

    ! ========================================
    ! FUNCIONES DE CONVERSIÓN
    ! ========================================

    ! A entero
    i = int(3.7)          ! 3 (trunca)
    i = nint(3.7)         ! 4 (redondea)
    i = floor(3.7)        ! 3 (hacia -infinito)
    i = ceiling(3.2)      ! 4 (hacia +infinito)

    ! A real
    r = real(42)          ! 42.0
    r = float(42)         ! 42.0 (legacy)

    ! A doble precisión
    d = dble(42)          ! 42.0D0
    d = real(42, kind=8)  ! Portátil

    ! A complejo
    z = cmplx(3.0, 4.0)   ! (3.0, 4.0)

    ! Caracteres a números (F2003)
    read(s, *) i          ! String a entero
    write(s, '(I10)') i   ! Entero a string

    ! ========================================
    ! TRANSFER - Reinterpretación de bits
    ! ========================================

    integer :: bits
    real :: valor

    valor = 1.0
    bits = transfer(valor, bits)  ! Bits de 1.0 como entero
    print *, "Bits de 1.0:", bits

end program conversiones
```

---

## 4. Variables y Constantes

### 4.1 Declaración de Variables

```fortran
program variables
    implicit none

    ! ========================================
    ! DECLARACIÓN BÁSICA
    ! ========================================

    integer :: i, j, k
    real :: x, y, z
    logical :: flag
    character(len=50) :: mensaje

    ! ========================================
    ! ATRIBUTOS DE VARIABLES
    ! ========================================

    ! PARAMETER - Constante
    real, parameter :: PI = 3.14159265359
    integer, parameter :: MAX_SIZE = 1000

    ! DIMENSION - Arrays
    real, dimension(10) :: vector
    real, dimension(3,3) :: matriz
    real :: otro_vector(100)           ! Sintaxis alternativa

    ! ALLOCATABLE - Tamaño dinámico
    real, allocatable :: dinamico(:)
    real, allocatable :: matriz_din(:,:)

    ! SAVE - Persistir valor entre llamadas
    integer, save :: contador = 0

    ! TARGET y POINTER
    real, target :: objetivo
    real, pointer :: ptr

    ! INTENT - Para argumentos de procedimientos
    ! intent(in)    - Solo lectura
    ! intent(out)   - Solo escritura
    ! intent(inout) - Lectura y escritura

    ! ========================================
    ! INICIALIZACIÓN
    ! ========================================

    ! En declaración
    integer :: inicializado = 42
    real :: array_init(3) = [1.0, 2.0, 3.0]

    ! Con DATA statement
    real :: valores(5)
    data valores /1.0, 2.0, 3.0, 4.0, 5.0/

    ! Repetición en DATA
    integer :: ceros(10)
    data ceros /10*0/   ! 10 ceros

    ! Constructor de array
    integer :: secuencia(5) = [(i, i=1,5)]  ! [1,2,3,4,5]

end program variables
```

### 4.2 Scope y Visibilidad

```fortran
module scope_ejemplo
    implicit none

    ! Variables de módulo - accesibles donde se use el módulo
    integer :: variable_modulo = 100

    ! PRIVATE - Solo visible dentro del módulo
    integer, private :: variable_privada

    ! PUBLIC - Explícitamente pública
    integer, public :: variable_publica

    ! Por defecto todos public, cambiar con:
    private  ! Hace todo private por defecto
    public :: variable_publica, subrutina_publica

contains

    subroutine subrutina_publica()
        ! variable_modulo accesible aquí
        variable_modulo = 200
    end subroutine

end module scope_ejemplo


program usar_scope
    use scope_ejemplo  ! Importa todo lo público
    ! use scope_ejemplo, only: variable_publica  ! Importa selectivo
    ! use scope_ejemplo, var => variable_publica  ! Renombrar

    implicit none

    integer :: variable_local  ! Solo en este programa

    call subrutina_publica()
    print *, variable_publica

contains

    subroutine interna()
        ! Puede acceder a variables del programa host
        variable_local = 10
    end subroutine

end program usar_scope
```

### 4.3 COMMON Blocks (Legacy)

```fortran
! ========================================
! COMMON BLOCKS - Método legacy para compartir datos
! Usar MODULES en código nuevo
! ========================================

program usar_common
    implicit none

    ! Declarar variables del COMMON
    real :: x, y, z
    integer :: contador

    ! COMMON sin nombre (blank common)
    common x, y, z

    ! COMMON con nombre
    common /bloque1/ contador

    x = 1.0
    y = 2.0
    z = 3.0
    contador = 0

    call modificar_common()

    print *, "x =", x  ! Modificado por subrutina

end program usar_common


subroutine modificar_common()
    implicit none

    ! Debe declarar las mismas variables en el mismo orden
    real :: a, b, c
    integer :: cont

    common a, b, c
    common /bloque1/ cont

    ! a, b, c son los mismos que x, y, z del programa principal
    a = 10.0
    cont = cont + 1

end subroutine modificar_common
```

---

## 5. Operadores

### 5.1 Operadores Aritméticos

```fortran
program operadores_aritmeticos
    implicit none

    integer :: i, j, resultado_int
    real :: x, y, resultado_real

    i = 17
    j = 5
    x = 17.0
    y = 5.0

    ! ========================================
    ! OPERADORES BÁSICOS
    ! ========================================

    resultado_int = i + j      ! Suma: 22
    resultado_int = i - j      ! Resta: 12
    resultado_int = i * j      ! Multiplicación: 85
    resultado_int = i / j      ! División entera: 3
    resultado_real = x / y     ! División real: 3.4
    resultado_int = i ** 2     ! Potencia: 289
    resultado_real = x ** 0.5  ! Raíz: ~4.12

    ! ========================================
    ! MÓDULO (Resto)
    ! ========================================

    resultado_int = mod(i, j)   ! 17 mod 5 = 2
    resultado_int = modulo(i, j) ! Similar pero diferente con negativos

    ! mod(-17, 5) = -2  (mismo signo que dividendo)
    ! modulo(-17, 5) = 3 (mismo signo que divisor)

    ! ========================================
    ! FUNCIONES MATEMÁTICAS
    ! ========================================

    resultado_real = abs(-5.0)      ! Valor absoluto: 5.0
    resultado_real = sqrt(25.0)     ! Raíz cuadrada: 5.0
    resultado_real = exp(1.0)       ! e^x: 2.718...
    resultado_real = log(2.718)     ! Logaritmo natural
    resultado_real = log10(100.0)   ! Logaritmo base 10: 2.0
    resultado_real = sin(3.14159/2) ! Seno: ~1.0
    resultado_real = cos(0.0)       ! Coseno: 1.0
    resultado_real = tan(0.0)       ! Tangente: 0.0
    resultado_real = asin(1.0)      ! Arcoseno: PI/2
    resultado_real = acos(1.0)      ! Arcocoseno: 0.0
    resultado_real = atan(1.0)      ! Arcotangente: PI/4
    resultado_real = atan2(1.0, 1.0) ! atan2(y,x): PI/4

    ! Trigonométricas hiperbólicas
    resultado_real = sinh(1.0)
    resultado_real = cosh(1.0)
    resultado_real = tanh(1.0)

    ! Max/Min
    resultado_real = max(1.0, 2.0, 3.0)  ! 3.0
    resultado_real = min(1.0, 2.0, 3.0)  ! 1.0
    resultado_int = max(i, j)             ! 17

    ! Signo
    resultado_real = sign(5.0, -1.0)  ! -5.0 (magnitud de 1ro, signo de 2do)

end program operadores_aritmeticos
```

### 5.2 Operadores Relacionales

```fortran
program operadores_relacionales
    implicit none

    integer :: a, b
    real :: x, y
    logical :: resultado

    a = 5
    b = 10
    x = 3.14
    y = 3.14

    ! ========================================
    ! OPERADORES RELACIONALES
    ! ========================================

    ! Estilo Fortran 90+ (recomendado)
    resultado = (a == b)    ! Igual
    resultado = (a /= b)    ! No igual
    resultado = (a < b)     ! Menor que
    resultado = (a <= b)    ! Menor o igual
    resultado = (a > b)     ! Mayor que
    resultado = (a >= b)    ! Mayor o igual

    ! Estilo Fortran 77 (legacy, todavía válido)
    resultado = (a .EQ. b)  ! Igual
    resultado = (a .NE. b)  ! No igual
    resultado = (a .LT. b)  ! Menor que
    resultado = (a .LE. b)  ! Menor o igual
    resultado = (a .GT. b)  ! Mayor que
    resultado = (a .GE. b)  ! Mayor o igual

    ! ========================================
    ! COMPARACIÓN DE REALES (¡CUIDADO!)
    ! ========================================

    ! MAL - Comparación directa de flotantes
    if (x == 3.14) then  ! Puede fallar por precisión
        print *, "Igual"
    end if

    ! BIEN - Usar tolerancia
    real, parameter :: TOL = 1.0e-6
    if (abs(x - 3.14) < TOL) then
        print *, "Aproximadamente igual"
    end if

end program operadores_relacionales
```

### 5.3 Operadores Lógicos

```fortran
program operadores_logicos
    implicit none

    logical :: p, q, resultado

    p = .true.
    q = .false.

    ! ========================================
    ! OPERADORES LÓGICOS
    ! ========================================

    resultado = .NOT. p        ! Negación: .false.
    resultado = p .AND. q      ! Y lógico: .false.
    resultado = p .OR. q       ! O lógico: .true.
    resultado = p .EQV. q      ! Equivalencia: .false.
    resultado = p .NEQV. q     ! No equivalencia (XOR): .true.

    ! ========================================
    ! TABLAS DE VERDAD
    ! ========================================
    !
    ! .AND.  | T | F          .OR.   | T | F
    ! -------|---|---         -------|---|---
    !   T    | T | F            T    | T | T
    !   F    | F | F            F    | T | F
    !
    ! .EQV.  | T | F          .NEQV. | T | F
    ! -------|---|---         -------|---|---
    !   T    | T | F            T    | F | T
    !   F    | F | T            F    | T | F

    ! ========================================
    ! FUNCIONES LÓGICAS PARA ARRAYS
    ! ========================================

    logical :: array_log(5) = [.true., .false., .true., .true., .false.]

    resultado = all(array_log)   ! .false. (no todos son .true.)
    resultado = any(array_log)   ! .true.  (al menos uno es .true.)
    print *, count(array_log)    ! 3 (cantidad de .true.)

end program operadores_logicos
```

### 5.4 Operadores de Caracteres

```fortran
program operadores_caracteres
    implicit none

    character(len=20) :: str1, str2, resultado
    character(len=50) :: concatenado
    integer :: pos
    logical :: igual

    str1 = "Hola"
    str2 = "Mundo"

    ! ========================================
    ! CONCATENACIÓN
    ! ========================================

    concatenado = str1 // " " // str2   ! "Hola Mundo"

    ! ========================================
    ! COMPARACIÓN
    ! ========================================

    ! Comparación lexicográfica
    igual = (str1 == str2)           ! .false.
    igual = (str1 < str2)            ! .true. (H < M en ASCII)

    ! Funciones de comparación (independiente de representación)
    igual = llt(str1, str2)  ! Lexically Less Than
    igual = lle(str1, str2)  ! Lexically Less or Equal
    igual = lgt(str1, str2)  ! Lexically Greater Than
    igual = lge(str1, str2)  ! Lexically Greater or Equal

    ! ========================================
    ! FUNCIONES DE CADENAS
    ! ========================================

    print *, len(str1)              ! 20 (longitud declarada)
    print *, len_trim(str1)         ! 4 (sin espacios finales)

    resultado = trim(str1)          ! "Hola" (quita espacios finales)
    resultado = adjustl(str1)       ! Alinear a la izquierda
    resultado = adjustr(str1)       ! Alinear a la derecha

    pos = index(concatenado, "Mundo")  ! 6 (posición de substring)
    pos = scan(str1, "aeiou")          ! 2 (primera vocal)
    pos = verify(str1, "Hola")         ! 0 (todos están en "Hola")

    resultado = repeat("*", 10)        ! "**********"

    ! Subcadenas
    resultado = str1(1:2)           ! "Ho"
    resultado = concatenado(6:10)   ! "Mundo"

    ! Conversión de caso (F2003+)
    ! No hay función estándar, usar biblioteca o:

end program operadores_caracteres
```

---

## 6. Control de Flujo

### 6.1 Condicionales IF

```fortran
program condicionales
    implicit none

    integer :: x, nota
    real :: temperatura

    x = 10
    nota = 85
    temperatura = 25.5

    ! ========================================
    ! IF SIMPLE
    ! ========================================

    if (x > 0) print *, "Positivo"

    ! ========================================
    ! IF-THEN-ELSE
    ! ========================================

    if (x > 0) then
        print *, "Positivo"
    else if (x < 0) then
        print *, "Negativo"
    else
        print *, "Cero"
    end if

    ! ========================================
    ! IF-THEN-ELSE ANIDADO
    ! ========================================

    if (nota >= 90) then
        print *, "A"
    else if (nota >= 80) then
        print *, "B"
    else if (nota >= 70) then
        print *, "C"
    else if (nota >= 60) then
        print *, "D"
    else
        print *, "F"
    end if

    ! ========================================
    ! IF CON NOMBRE (para claridad)
    ! ========================================

    temp_check: if (temperatura > 30.0) then
        print *, "Caliente"
    else if (temperatura > 20.0) then temp_check
        print *, "Templado"
    else temp_check
        print *, "Frío"
    end if temp_check

    ! ========================================
    ! IF ARITMÉTICO (Legacy - EVITAR)
    ! ========================================

    ! if (expresión) etiqueta_neg, etiqueta_cero, etiqueta_pos
    ! if (x) 100, 200, 300
    ! 100 print *, "Negativo"
    !     go to 400
    ! 200 print *, "Cero"
    !     go to 400
    ! 300 print *, "Positivo"
    ! 400 continue

end program condicionales
```

### 6.2 SELECT CASE

```fortran
program select_case_ejemplo
    implicit none

    integer :: mes, dia_semana
    character(len=20) :: color, resultado

    mes = 3
    dia_semana = 5
    color = "rojo"

    ! ========================================
    ! SELECT CASE BÁSICO
    ! ========================================

    select case (mes)
        case (1)
            print *, "Enero"
        case (2)
            print *, "Febrero"
        case (3)
            print *, "Marzo"
        case (4, 5, 6)
            print *, "Segundo trimestre"
        case (7:9)
            print *, "Tercer trimestre"
        case (10:12)
            print *, "Cuarto trimestre"
        case default
            print *, "Mes inválido"
    end select

    ! ========================================
    ! SELECT CASE CON CARACTERES
    ! ========================================

    select case (color)
        case ("rojo", "naranja", "amarillo")
            resultado = "color cálido"
        case ("azul", "verde", "violeta")
            resultado = "color frío"
        case ("blanco", "negro", "gris")
            resultado = "color neutro"
        case default
            resultado = "color desconocido"
    end select

    print *, trim(color), " es ", trim(resultado)

    ! ========================================
    ! SELECT CASE CON NOMBRE
    ! ========================================

    dia_check: select case (dia_semana)
        case (1, 7)
            print *, "Fin de semana"
        case (2:6)
            print *, "Día laboral"
        case default dia_check
            print *, "Día inválido"
    end select dia_check

end program select_case_ejemplo
```

### 6.3 Bucles DO

```fortran
program bucles_do
    implicit none

    integer :: i, j, suma, n
    real :: x

    n = 10
    suma = 0

    ! ========================================
    ! DO CON CONTADOR
    ! ========================================

    ! DO variable = inicio, fin [, incremento]
    do i = 1, 10
        suma = suma + i
    end do
    print *, "Suma 1-10:", suma

    ! Con incremento
    do i = 10, 1, -1
        print *, i  ! Cuenta regresiva
    end do

    ! Incremento diferente de 1
    do i = 0, 100, 5
        print *, i  ! 0, 5, 10, ..., 100
    end do

    ! ========================================
    ! DO WHILE
    ! ========================================

    x = 1.0
    do while (x < 1000.0)
        x = x * 2.0
    end do
    print *, "x =", x

    ! ========================================
    ! DO INFINITO CON EXIT
    ! ========================================

    suma = 0
    do
        read *, i
        if (i < 0) exit   ! Salir del bucle
        suma = suma + i
    end do
    print *, "Suma:", suma

    ! ========================================
    ! CYCLE - Saltar a siguiente iteración
    ! ========================================

    suma = 0
    do i = 1, 100
        if (mod(i, 2) == 0) cycle  ! Saltar pares
        suma = suma + i             ! Solo suma impares
    end do
    print *, "Suma impares:", suma

    ! ========================================
    ! DO CON NOMBRE
    ! ========================================

    outer: do i = 1, 10
        inner: do j = 1, 10
            if (i * j > 50) exit outer   ! Sale del bucle externo
            if (mod(i * j, 7) == 0) cycle inner
            print *, i, j, i * j
        end do inner
    end do outer

    ! ========================================
    ! DO CONCURRENT (F2008) - Paralelizable
    ! ========================================

    real :: a(100), b(100), c(100)

    ! El compilador puede paralelizar esto
    do concurrent (i = 1:100)
        c(i) = a(i) + b(i)
    end do

    ! Con máscara
    do concurrent (i = 1:100, a(i) > 0)
        c(i) = sqrt(a(i))
    end do

end program bucles_do
```

### 6.4 GOTO y Etiquetas (Legacy)

```fortran
program goto_ejemplo
    implicit none

    ! ========================================
    ! GOTO - EVITAR EN CÓDIGO NUEVO
    ! ========================================
    ! Solo documentado para entender código legacy

    integer :: i

    i = 0

10  i = i + 1
    print *, i
    if (i < 10) go to 10   ! Salto incondicional

    ! COMPUTED GOTO (obsoleto)
    ! go to (100, 200, 300), i
    ! Salta a etiqueta según valor de i

    ! ASSIGNED GOTO (obsoleto, eliminado en F95)
    ! assign 100 to label
    ! go to label

    print *, "Fin"

100 continue  ! Etiqueta de destino

end program goto_ejemplo
```

---

## 7. Procedimientos

### 7.1 Subrutinas

```fortran
program ejemplo_subrutinas
    implicit none

    integer :: a, b, resultado
    real :: vector(10)

    a = 5
    b = 3

    ! Llamada a subrutina
    call sumar(a, b, resultado)
    print *, "Suma:", resultado

    call intercambiar(a, b)
    print *, "Después de swap:", a, b

    call inicializar_vector(vector, 10)

contains

    ! ========================================
    ! SUBRUTINA SIMPLE
    ! ========================================

    subroutine sumar(x, y, res)
        implicit none
        integer, intent(in) :: x, y      ! Solo entrada
        integer, intent(out) :: res      ! Solo salida

        res = x + y
    end subroutine sumar

    ! ========================================
    ! SUBRUTINA CON INTENT(INOUT)
    ! ========================================

    subroutine intercambiar(x, y)
        implicit none
        integer, intent(inout) :: x, y
        integer :: temp

        temp = x
        x = y
        y = temp
    end subroutine intercambiar

    ! ========================================
    ! SUBRUTINA CON ARRAY
    ! ========================================

    subroutine inicializar_vector(v, n)
        implicit none
        integer, intent(in) :: n
        real, intent(out) :: v(n)   ! Array de tamaño explícito
        integer :: i

        do i = 1, n
            v(i) = real(i)
        end do
    end subroutine inicializar_vector

end program ejemplo_subrutinas
```

### 7.2 Funciones

```fortran
program ejemplo_funciones
    implicit none

    real :: x, y, area, perimetro
    integer :: n, fact

    x = 3.0
    y = 4.0
    n = 5

    ! Llamadas a funciones
    area = area_rectangulo(x, y)
    print *, "Área:", area

    fact = factorial(n)
    print *, "Factorial de", n, "=", fact

    print *, "Hipotenusa:", hipotenusa(x, y)

contains

    ! ========================================
    ! FUNCIÓN SIMPLE
    ! ========================================

    function area_rectangulo(base, altura)
        implicit none
        real, intent(in) :: base, altura
        real :: area_rectangulo   ! Tipo de retorno = nombre

        area_rectangulo = base * altura
    end function area_rectangulo

    ! ========================================
    ! FUNCIÓN CON RESULT
    ! ========================================

    function hipotenusa(a, b) result(c)
        implicit none
        real, intent(in) :: a, b
        real :: c  ! Variable de resultado

        c = sqrt(a**2 + b**2)
    end function hipotenusa

    ! ========================================
    ! FUNCIÓN RECURSIVA
    ! ========================================

    recursive function factorial(n) result(res)
        implicit none
        integer, intent(in) :: n
        integer :: res

        if (n <= 1) then
            res = 1
        else
            res = n * factorial(n - 1)
        end if
    end function factorial

    ! ========================================
    ! FUNCIÓN PURE (sin efectos secundarios)
    ! ========================================

    pure function cuadrado(x) result(res)
        implicit none
        real, intent(in) :: x
        real :: res

        res = x * x
        ! No puede modificar variables globales
        ! No puede hacer I/O
        ! No puede llamar funciones no-pure
    end function cuadrado

    ! ========================================
    ! FUNCIÓN ELEMENTAL (opera en scalares y arrays)
    ! ========================================

    elemental function celsius_a_fahrenheit(c) result(f)
        implicit none
        real, intent(in) :: c
        real :: f

        f = c * 9.0/5.0 + 32.0
    end function celsius_a_fahrenheit

end program ejemplo_funciones
```

### 7.3 Interfaces

```fortran
module interfaces_ejemplo
    implicit none

    ! ========================================
    ! INTERFACE EXPLÍCITA
    ! ========================================

    interface
        function funcion_externa(x) result(y)
            real, intent(in) :: x
            real :: y
        end function funcion_externa
    end interface

    ! ========================================
    ! INTERFACE GENÉRICA (overloading)
    ! ========================================

    interface sumar
        module procedure sumar_int
        module procedure sumar_real
        module procedure sumar_complejo
    end interface sumar

    ! ========================================
    ! INTERFACE OPERATOR (sobrecarga de operadores)
    ! ========================================

    type :: vector2d
        real :: x, y
    end type vector2d

    interface operator(+)
        module procedure sumar_vectores
    end interface

    interface operator(*)
        module procedure escalar_por_vector
    end interface

contains

    function sumar_int(a, b) result(c)
        integer, intent(in) :: a, b
        integer :: c
        c = a + b
    end function

    function sumar_real(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a + b
    end function

    function sumar_complejo(a, b) result(c)
        complex, intent(in) :: a, b
        complex :: c
        c = a + b
    end function

    function sumar_vectores(v1, v2) result(v3)
        type(vector2d), intent(in) :: v1, v2
        type(vector2d) :: v3
        v3%x = v1%x + v2%x
        v3%y = v1%y + v2%y
    end function

    function escalar_por_vector(s, v) result(vr)
        real, intent(in) :: s
        type(vector2d), intent(in) :: v
        type(vector2d) :: vr
        vr%x = s * v%x
        vr%y = s * v%y
    end function

end module interfaces_ejemplo


program usar_interfaces
    use interfaces_ejemplo
    implicit none

    integer :: i
    real :: r
    type(vector2d) :: v1, v2, v3

    ! Usa la versión correcta automáticamente
    i = sumar(1, 2)       ! Llama sumar_int
    r = sumar(1.0, 2.0)   ! Llama sumar_real

    ! Operadores sobrecargados
    v1 = vector2d(1.0, 2.0)
    v2 = vector2d(3.0, 4.0)
    v3 = v1 + v2          ! Llama sumar_vectores
    v3 = 2.0 * v1         ! Llama escalar_por_vector

end program usar_interfaces
```

---

## 8. Formato Libre vs Fijo

### 8.1 Comparación

```fortran
! ========================================
! FORMATO FIJO (Fortran 77)
! ========================================
! - Archivo con extensión .f o .for
! - Columnas 1-5: etiquetas
! - Columna 6: continuación
! - Columnas 7-72: código
! - Columnas 73-80: ignoradas

C     Este es un comentario
      PROGRAM FORMATO_FIJO
      INTEGER I, SUMA
      SUMA = 0
      DO 10 I = 1, 100
         SUMA = SUMA + I
 10   CONTINUE
C     Línea muy larga debe continuar
      WRITE(*,*) 'EL RESULTADO ES MUY'
     +           ' LARGO PARA UNA LINEA'
      STOP
      END


! ========================================
! FORMATO LIBRE (Fortran 90+)
! ========================================
! - Archivo con extensión .f90, .f95, etc.
! - Sin restricciones de columnas
! - Líneas hasta 132 caracteres
! - & para continuación

program formato_libre
    implicit none
    integer :: i, suma

    suma = 0
    do i = 1, 100
        suma = suma + i
    end do

    ! Continuación con &
    print *, "El resultado es muy &
             &largo para una linea"

end program formato_libre
```

### 8.2 Mejores Prácticas Modernas

```fortran
! ========================================
! ESTILO MODERNO RECOMENDADO
! ========================================

module matematicas
    implicit none
    private                          ! Todo privado por defecto
    public :: calcular_estadisticas  ! Exportar explícitamente

    ! Constantes con precisión específica
    integer, parameter :: dp = selected_real_kind(15, 307)
    real(dp), parameter :: PI = 3.14159265358979323846_dp

contains

    subroutine calcular_estadisticas(datos, n, promedio, desviacion)
        ! Documentación de la subrutina
        !
        ! Argumentos:
        !   datos     - Array de datos de entrada
        !   n         - Número de elementos
        !   promedio  - Promedio calculado (salida)
        !   desviacion - Desviación estándar (salida)

        integer, intent(in) :: n
        real(dp), intent(in) :: datos(n)
        real(dp), intent(out) :: promedio, desviacion

        ! Variables locales
        real(dp) :: suma, suma_cuadrados
        integer :: i

        ! Validación
        if (n <= 0) then
            promedio = 0.0_dp
            desviacion = 0.0_dp
            return
        end if

        ! Cálculo
        suma = sum(datos)
        promedio = suma / real(n, dp)

        suma_cuadrados = sum((datos - promedio)**2)
        desviacion = sqrt(suma_cuadrados / real(n - 1, dp))

    end subroutine calcular_estadisticas

end module matematicas
```

---

## Referencia Rápida

### Tipos de Datos

| Tipo | Descripción | KIND común |
|------|-------------|------------|
| `integer` | Entero | 1, 2, 4, 8 |
| `real` | Punto flotante | 4, 8, 16 |
| `complex` | Número complejo | 4, 8, 16 |
| `logical` | Booleano | 1, 4 |
| `character` | Cadena | - |

### Operadores

| Operador | Descripción |
|----------|-------------|
| `+`, `-`, `*`, `/` | Aritméticos |
| `**` | Potencia |
| `==`, `/=` | Igual, No igual |
| `<`, `<=`, `>`, `>=` | Comparación |
| `.AND.`, `.OR.`, `.NOT.` | Lógicos |
| `//` | Concatenación |

### Estructuras de Control

```fortran
if (cond) then ... else ... end if
select case (var) ... end select
do i = a, b, step ... end do
do while (cond) ... end do
```

---

## Conclusión

FORTRAN, nacido en los albores de la programación, sigue siendo el rey de la computación científica y numérica. Su evolución desde el formato fijo de los años 50 hasta el moderno Fortran 2018 con coarrays demuestra su capacidad de adaptación sin perder su esencia: el procesamiento numérico eficiente.

**Puntos clave:**
1. Usar `implicit none` siempre
2. Preferir formato libre (F90+)
3. Usar módulos en lugar de COMMON
4. Especificar KIND para portabilidad
5. INTENT en todos los argumentos

---

*"FORTRAN no es solo un lenguaje - es la lingua franca de la ciencia computacional, donde los números danzan y las ecuaciones cobran vida."*

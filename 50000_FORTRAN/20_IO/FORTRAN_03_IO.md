# FORTRAN_03: I/O Y FORMATOS

> *"La entrada y salida son los sentidos de un programa - el oído para escuchar los datos del mundo y la voz para proclamar los resultados de sus cálculos."*

## Índice
1. [I/O Básico](#1-io-básico)
2. [Descriptores de Formato](#2-descriptores-de-formato)
3. [I/O con Archivos](#3-io-con-archivos)
4. [I/O Avanzado](#4-io-avanzado)
5. [Manejo de Errores](#5-manejo-de-errores)
6. [Formatos Especiales](#6-formatos-especiales)
7. [Buenas Prácticas](#7-buenas-prácticas)

---

## 1. I/O Básico

### 1.1 Unidades de I/O Estándar

```fortran
program unidades_estandar
    implicit none

    ! ========================================
    ! UNIDADES PREDEFINIDAS
    ! ========================================
    !
    ! En Fortran, el I/O se realiza a través de "unidades"
    ! (números que identifican dispositivos o archivos)
    !
    ! Unidades estándar (típicas, pueden variar por sistema):
    !
    !   Unidad 5  = stdin  (entrada estándar, teclado)
    !   Unidad 6  = stdout (salida estándar, pantalla)
    !   Unidad 0  = stderr (error estándar)
    !
    ! El asterisco (*) representa la unidad por defecto:
    !   READ(*,...)  = leer de stdin
    !   WRITE(*,...) = escribir a stdout

    integer :: numero
    real :: valor
    character(len=50) :: texto

    ! ========================================
    ! LECTURA BÁSICA
    ! ========================================

    ! Forma más simple: formato libre con *
    print *, "Ingrese un número entero:"
    read *, numero

    print *, "Ingrese un número real:"
    read *, valor

    print *, "Ingrese texto:"
    read *, texto

    ! Forma explícita con unidades
    write(6, *) "Usando unidad 6 (stdout)"
    read(5, *) numero  ! Lee de unidad 5 (stdin)

    ! ========================================
    ! CONSTANTES INPUT_UNIT, OUTPUT_UNIT (F2003)
    ! ========================================

    use, intrinsic :: iso_fortran_env, only: &
        input_unit, output_unit, error_unit

    write(output_unit, *) "Esto va a stdout"
    write(error_unit, *)  "Esto va a stderr"
    read(input_unit, *)   numero

end program unidades_estandar
```

### 1.2 READ Statement

```fortran
program read_ejemplos
    implicit none

    integer :: i, j, k
    real :: x, y, z
    character(len=100) :: linea
    logical :: flag
    integer :: iostat_val

    ! ========================================
    ! READ CON FORMATO LIBRE (List-Directed)
    ! ========================================

    ! Sintaxis: READ *, lista_variables
    ! o:        READ(unit, *) lista_variables

    ! Leer un valor
    read *, i

    ! Leer múltiples valores (separados por espacio o coma)
    read *, i, j, k

    ! Leer diferentes tipos
    read *, x, i, linea

    ! El usuario puede ingresar:
    !   3.14 42 "Hola Mundo"
    ! o:
    !   3.14, 42, "Hola Mundo"

    ! ========================================
    ! READ CON FORMATO ESPECÍFICO
    ! ========================================

    ! Sintaxis: READ(unit, format) lista_variables
    ! El formato puede ser:
    !   - String con formato
    !   - Etiqueta numérica de FORMAT
    !   - Variable character con formato

    ! Leer entero de 5 dígitos
    read(*, '(I5)') i

    ! Leer real con 10 caracteres, 3 decimales
    read(*, '(F10.3)') x

    ! Leer string de 20 caracteres
    read(*, '(A20)') linea

    ! Múltiples campos
    read(*, '(I5, F10.2, A20)') i, x, linea

    ! ========================================
    ! READ CON ETIQUETA FORMAT
    ! ========================================

    read(*, 100) i, x, linea
100 format(I5, F10.2, A20)

    ! ========================================
    ! READ CON VARIABLE DE FORMATO
    ! ========================================

    character(len=50) :: mi_formato
    mi_formato = '(I5, F10.2)'
    read(*, mi_formato) i, x

    ! ========================================
    ! READ CON CONTROL DE ERRORES
    ! ========================================

    ! Con IOSTAT (retorna 0 si éxito, negativo si EOF, positivo si error)
    read(*, *, iostat=iostat_val) i
    if (iostat_val /= 0) then
        print *, "Error de lectura o fin de archivo"
    end if

    ! Con ERR y END (legacy pero aún válido)
    read(*, *, err=900, end=800) i
    go to 999
800 print *, "Fin de archivo alcanzado"
    go to 999
900 print *, "Error de lectura"
999 continue

    ! ========================================
    ! READ AVANZADO
    ! ========================================

    ! ADVANCE='NO' - No avanzar a siguiente línea
    read(*, '(A1)', advance='NO') linea(1:1)

    ! Leer línea completa
    read(*, '(A)') linea

    ! Leer con tamaño variable
    integer :: chars_read
    read(*, '(A)', advance='NO', size=chars_read) linea

end program read_ejemplos
```

### 1.3 WRITE Statement

```fortran
program write_ejemplos
    implicit none

    integer :: i = 42
    real :: pi = 3.14159265359
    character(len=20) :: nombre = "Fortran"
    logical :: activo = .true.

    ! ========================================
    ! WRITE CON FORMATO LIBRE
    ! ========================================

    ! Sintaxis básica
    write(*, *) "Valor de i:", i
    write(*, *) "Pi =", pi, "Nombre =", nombre

    ! Múltiples valores automáticamente espaciados
    write(*, *) i, pi, nombre, activo

    ! ========================================
    ! PRINT STATEMENT (Solo para stdout)
    ! ========================================

    ! PRINT es un atajo para WRITE(*, ...)
    print *, "Hola desde PRINT"
    print *, "i =", i

    ! PRINT con formato
    print '(A, I5)', "Entero: ", i
    print '(A, F10.4)', "Pi: ", pi

    ! ========================================
    ! WRITE CON FORMATO ESPECÍFICO
    ! ========================================

    ! Entero en 10 columnas, alineado a derecha
    write(*, '(I10)') i

    ! Real con 15 caracteres, 6 decimales
    write(*, '(F15.6)') pi

    ! String en 30 columnas
    write(*, '(A30)') nombre

    ! Combinación
    write(*, '(A, I5, A, F10.4)') "i=", i, " pi=", pi

    ! ========================================
    ! WRITE CON ETIQUETA FORMAT
    ! ========================================

    write(*, 200) nombre, i, pi
200 format('Nombre: ', A, ' Numero: ', I5, ' Pi: ', F8.4)

    ! ========================================
    ! FORMATO EN MÚLTIPLES LÍNEAS
    ! ========================================

    write(*, '(A)') "==================================="
    write(*, '(A, T20, A)') "Campo", "Valor"
    write(*, '(A)') "==================================="
    write(*, '(A, T20, I10)') "Entero:", i
    write(*, '(A, T20, F10.4)') "Real:", pi
    write(*, '(A, T20, A)') "String:", trim(nombre)
    write(*, '(A)') "==================================="

    ! ========================================
    ! CONTROL DE AVANCE
    ! ========================================

    ! ADVANCE='NO' - No avanzar a siguiente línea
    write(*, '(A)', advance='NO') "Ingrese valor: "
    read *, i

    ! Escribir en la misma línea progresivamente
    integer :: j
    do j = 1, 10
        write(*, '(I3)', advance='NO') j
    end do
    write(*, *)  ! Nueva línea al final

    ! ========================================
    ! WRITE A STRING (Internal I/O)
    ! ========================================

    character(len=100) :: buffer

    ! Escribir a variable string
    write(buffer, '(A, I5, A, F8.3)') "Valor=", i, " Pi=", pi
    print *, trim(buffer)

end program write_ejemplos
```

### 1.4 PRINT Statement

```fortran
program print_ejemplos
    implicit none

    integer :: i, j
    real :: x, y
    real :: matriz(3, 3)
    character(len=50) :: msg

    i = 10
    j = 20
    x = 3.14159
    y = 2.71828
    matriz = reshape([(real(k), k=1,9)], [3,3])

    ! ========================================
    ! PRINT BÁSICO
    ! ========================================

    ! PRINT * equivale a WRITE(*,*)
    print *, "Mensaje simple"
    print *, "i =", i, "j =", j
    print *, "Valores:", x, y

    ! ========================================
    ! PRINT CON FORMATO
    ! ========================================

    ! Formato inline
    print '(A)', "Línea formateada"
    print '(A, I5)', "Entero: ", i
    print '(A, 2F10.4)', "Reales: ", x, y

    ! ========================================
    ! PRINT CON ETIQUETA FORMAT
    ! ========================================

    print 100, i, j, x, y
100 format('Enteros: ', 2I5, ' Reales: ', 2F10.4)

    ! ========================================
    ! PRINT PARA ARRAYS
    ! ========================================

    ! Array completo (formato libre)
    print *, "Matriz:", matriz

    ! Array fila por fila
    integer :: fila
    print '(A)', "Matriz formateada:"
    do fila = 1, 3
        print '(3F8.2)', matriz(fila, :)
    end do

    ! ========================================
    ! PRINT PARA DEBUGGING
    ! ========================================

    ! Útil para depuración rápida
    print *, "DEBUG: Entrando a función"
    print *, "DEBUG: i=", i, "x=", x
    print *, "DEBUG: matriz(1,1)=", matriz(1,1)

    ! ========================================
    ! DIFERENCIAS ENTRE PRINT Y WRITE
    ! ========================================
    !
    ! PRINT:
    !   - Solo escribe a stdout (unidad *)
    !   - Sintaxis más simple
    !   - No permite especificar unidad
    !   - No permite ADVANCE, IOSTAT, etc.
    !
    ! WRITE:
    !   - Puede escribir a cualquier unidad
    !   - Más flexible y poderoso
    !   - Soporta todas las opciones de I/O
    !
    ! Recomendación:
    !   - Usar PRINT para debugging y salida simple
    !   - Usar WRITE para control completo

end program print_ejemplos
```

---

## 2. Descriptores de Formato

### 2.1 Descriptores para Enteros

```fortran
program formato_enteros
    implicit none

    integer :: num = 12345
    integer :: neg = -9876
    integer :: small = 7
    integer :: big = 1234567890

    ! ========================================
    ! I - DESCRIPTOR DE ENTEROS
    ! ========================================
    !
    ! Iw     - Entero en w columnas
    ! Iw.m   - Entero en w columnas, mínimo m dígitos
    !
    ! El número se alinea a la derecha
    ! Si no cabe, se muestran asteriscos (****)

    print '(A)', "=== Formato I (Enteros) ==="

    ! Iw - Ancho fijo
    print '(A, I10)', "I10:     |", num      ! "     12345"
    print '(A, I5)',  "I5:      |", num      ! "12345"
    print '(A, I3)',  "I3:      |", num      ! "***" (no cabe)

    ! Iw.m - Con mínimo de dígitos (rellena con ceros)
    print '(A, I10.6)', "I10.6:   |", num    ! "    012345"
    print '(A, I10.8)', "I10.8:   |", small  ! "  00000007"

    ! Negativos
    print '(A, I10)', "Negativo:|", neg      ! "     -9876"
    print '(A, I10.6)', "Neg.6:  |", neg     ! "   -009876"

    ! ========================================
    ! I0 - ANCHO MÍNIMO (F95+)
    ! ========================================

    ! I0 usa solo el espacio necesario
    print '(A, I0)', "I0:      |", num       ! "12345"
    print '(A, I0)', "I0:      |", small     ! "7"
    print '(A, I0)', "I0:      |", neg       ! "-9876"

    ! Combinar con texto
    print '(A, I0, A, I0)', "De ", 1, " a ", 100

    ! ========================================
    ! B, O, Z - BINARIO, OCTAL, HEXADECIMAL
    ! ========================================

    integer :: valor = 255

    print '(A)', "=== Otras bases ==="
    print '(A, I10)',  "Decimal: ", valor    ! "       255"
    print '(A, B10)',  "Binario: ", valor    ! " 11111111"
    print '(A, O10)',  "Octal:   ", valor    ! "       377"
    print '(A, Z10)',  "Hex:     ", valor    ! "        FF"

    ! Con ancho mínimo
    print '(A, B16.16)', "Bin16: ", valor    ! "0000000011111111"
    print '(A, Z8.4)',   "Hex4:  ", valor    ! "    00FF"

end program formato_enteros
```

### 2.2 Descriptores para Reales

```fortran
program formato_reales
    implicit none

    real :: pi = 3.14159265359
    real :: grande = 6.022e23      ! Número de Avogadro
    real :: pequeño = 1.602e-19   ! Carga del electrón
    real :: negativo = -273.15

    ! ========================================
    ! F - FORMATO PUNTO FIJO
    ! ========================================
    !
    ! Fw.d - w columnas totales, d decimales
    !
    ! w incluye: signo, dígitos, punto, decimales

    print '(A)', "=== Formato F (Punto Fijo) ==="

    print '(A, F10.4)', "F10.4:   |", pi          ! "    3.1416"
    print '(A, F10.2)', "F10.2:   |", pi          ! "      3.14"
    print '(A, F8.6)',  "F8.6:    |", pi          ! "3.141593"
    print '(A, F10.4)', "Negativo:|", negativo    ! "  -273.1500"

    ! F0.d - Ancho mínimo (F95+)
    print '(A, F0.4)', "F0.4:    |", pi           ! "3.1416"

    ! ========================================
    ! E - FORMATO EXPONENCIAL
    ! ========================================
    !
    ! Ew.d    - Exponencial estándar
    ! Ew.dEe  - Con e dígitos en exponente

    print '(A)', "=== Formato E (Exponencial) ==="

    print '(A, E15.6)',   "E15.6:  |", pi        ! "  0.314159E+01"
    print '(A, E15.6)',   "E15.6:  |", grande    ! "  0.602200E+24"
    print '(A, E15.6)',   "E15.6:  |", pequeño   ! "  0.160200E-18"
    print '(A, E15.6E3)', "E15.6E3:|", grande    ! " 0.602200E+024"

    ! ========================================
    ! ES - FORMATO CIENTÍFICO
    ! ========================================
    !
    ! ESw.d - Notación científica (1.xxx × 10^n)
    ! La mantisa está entre 1 y 10

    print '(A)', "=== Formato ES (Científico) ==="

    print '(A, ES15.6)', "ES15.6: |", pi         ! "  3.141593E+00"
    print '(A, ES15.6)', "ES15.6: |", grande     ! "  6.022000E+23"
    print '(A, ES15.6)', "ES15.6: |", pequeño    ! "  1.602000E-19"

    ! ========================================
    ! EN - FORMATO INGENIERÍA
    ! ========================================
    !
    ! ENw.d - Exponente múltiplo de 3 (kilo, mega, etc.)
    ! La mantisa está entre 1 y 1000

    print '(A)', "=== Formato EN (Ingeniería) ==="

    real :: resistencia = 4700.0    ! 4.7 kΩ
    real :: capacitor = 0.000047    ! 47 µF
    real :: frecuencia = 2400000.0  ! 2.4 MHz

    print '(A, EN15.3)', "4.7k:   |", resistencia  ! "  4.700E+03"
    print '(A, EN15.3)', "47µ:    |", capacitor    ! " 47.000E-06"
    print '(A, EN15.3)', "2.4M:   |", frecuencia   ! "  2.400E+06"

    ! ========================================
    ! G - FORMATO GENERAL
    ! ========================================
    !
    ! Gw.d - Automáticamente elige F o E según magnitud

    print '(A)', "=== Formato G (General) ==="

    print '(A, G15.6)', "G pi:     |", pi        ! "  3.14159     "
    print '(A, G15.6)', "G grande: |", grande    ! " 0.602200E+24"
    print '(A, G15.6)', "G pequeño:|", pequeño   ! " 0.160200E-18"

    ! ========================================
    ! G0 - ANCHO MÍNIMO (F2008)
    ! ========================================

    print '(A, G0)', "G0: ", pi                   ! "3.14159265"

    ! ========================================
    ! D - DOBLE PRECISIÓN (Legacy)
    ! ========================================
    !
    ! Dw.d - Similar a E pero con 'D' en lugar de 'E'
    ! Usado tradicionalmente para double precision

    double precision :: dp_valor = 3.141592653589793d0
    print '(A, D20.12)', "D20.12: |", dp_valor    ! " 0.314159265359D+01"

end program formato_reales
```

### 2.3 Descriptores para Caracteres y Lógicos

```fortran
program formato_caracteres_logicos
    implicit none

    character(len=20) :: nombre = "Fortran"
    character(len=50) :: largo = "Este es un texto bastante largo"
    logical :: verdad = .true.
    logical :: falso = .false.

    ! ========================================
    ! A - DESCRIPTOR DE CARACTERES
    ! ========================================
    !
    ! A   - Usa el tamaño del string
    ! Aw  - Exactamente w caracteres

    print '(A)', "=== Formato A (Caracteres) ==="

    ! A sin ancho - usa tamaño declarado
    print '(A, A)', "A:      |", nombre           ! "Fortran             "

    ! Aw - ancho específico
    print '(A, A10)', "A10:    |", nombre         ! "   Fortran"
    print '(A, A5)',  "A5:     |", nombre         ! "Fortr" (trunca)
    print '(A, A30)', "A30:    |", nombre         ! "                     Fortran"

    ! Alinear a izquierda con ADJUSTL
    print '(A, A30)', "Left:   |", adjustl(nombre)

    ! ========================================
    ! STRINGS LITERALES EN FORMATO
    ! ========================================

    ! Strings entre comillas simples o dobles
    print '("Resultado: ", I5)', 42
    print "('Valor = ', F10.4)", 3.14159

    ! Apóstrofe dentro de string: duplicar
    print '("Don''t panic!")'
    print "('It''s Fortran!')"

    ! ========================================
    ! L - DESCRIPTOR LÓGICO
    ! ========================================
    !
    ! Lw - Lógico en w columnas
    ! Muestra T o F (alineado a derecha)

    print '(A)', "=== Formato L (Lógicos) ==="

    print '(A, L5)', "L5 true:  |", verdad      ! "    T"
    print '(A, L5)', "L5 false: |", falso       ! "    F"
    print '(A, L1)', "L1 true:  |", verdad      ! "T"
    print '(A, L1)', "L1 false: |", falso       ! "F"

    ! Array de lógicos
    logical :: flags(5) = [.true., .false., .true., .true., .false.]
    print '(A, 5L2)', "Flags:    |", flags      ! " T F T T F"

    ! ========================================
    ! COMBINACIONES COMUNES
    ! ========================================

    integer :: id = 42
    real :: score = 95.5

    ! Tabla formateada
    print '(A)', "================================"
    print '(A10, A20, A10)', "ID", "Nombre", "Score"
    print '(A)', "================================"
    print '(I10, A20, F10.1)', id, nombre, score
    print '(A)', "================================"

end program formato_caracteres_logicos
```

### 2.4 Descriptores de Posición y Control

```fortran
program formato_control
    implicit none

    integer :: i
    real :: x = 3.14159

    ! ========================================
    ! X - ESPACIOS
    ! ========================================
    !
    ! nX - Insertar n espacios

    print '(A)', "=== Espacios (X) ==="
    print '(I5, 5X, I5)', 10, 20       ! "   10     20"
    print '(A, 10X, A)', "Uno", "Dos"  ! "Uno          Dos"

    ! ========================================
    ! T, TL, TR - TABULACIÓN
    ! ========================================
    !
    ! Tn  - Ir a columna n (absoluto)
    ! TLn - Retroceder n columnas (relativo)
    ! TRn - Avanzar n columnas (relativo)

    print '(A)', "=== Tabulación (T) ==="

    ! Columnas absolutas
    print '(T1, A, T20, A, T40, A)', "Col 1", "Col 20", "Col 40"

    ! Crear tabla con columnas fijas
    print '(A)', "ID        Nombre              Valor"
    print '(I5, T11, A15, T30, F10.2)', 1, "Primero", 100.50
    print '(I5, T11, A15, T30, F10.2)', 2, "Segundo", 200.75
    print '(I5, T11, A15, T30, F10.2)', 3, "Tercero", 300.25

    ! TL y TR (movimiento relativo)
    print '(A, TR5, A)', "Texto", "Más texto"    ! Avanza 5
    print '(T20, A, TL10, A)', "Final", "Medio"  ! Retrocede 10

    ! ========================================
    ! / - NUEVA LÍNEA
    ! ========================================
    !
    ! / - Saltar a nueva línea
    ! Se pueden encadenar: /// = 3 líneas

    print '(A)', "=== Nueva línea (/) ==="

    print '(A, /, A, /, A)', "Línea 1", "Línea 2", "Línea 3"

    ! Múltiples saltos de línea
    print '(A, ///, A)', "Arriba", "Abajo (3 líneas después)"

    ! Al inicio o final del formato
    print '(/, A)', "Después de línea vacía"
    print '(A, /)', "Antes de línea vacía"

    ! ========================================
    ! : - TERMINADOR DE FORMATO
    ! ========================================
    !
    ! : - Detiene el procesamiento si no hay más datos
    ! Útil para evitar caracteres extra al final

    print '(A)', "=== Terminador (:) ==="

    ! Sin terminador - imprime "Valor:" incluso sin segundo valor
    print '("Valores: ", I5, " y ", I5)', 10, 20  ! "Valores:    10 y    20"

    ! Con terminador - solo imprime lo necesario
    integer :: arr(3) = [1, 2, 3]
    print '(10(I5, :, ","))', arr     ! "    1,    2,    3"
    ! Sin el : imprimiría una coma al final

    ! ========================================
    ! $ o \ - SUPRIMIR NUEVA LÍNEA (No estándar)
    ! ========================================
    !
    ! Algunos compiladores soportan $ para suprimir newline
    ! La forma estándar es usar ADVANCE='NO'

    ! Estándar F90+
    write(*, '(A)', advance='NO') "Ingrese valor: "
    read *, i

    ! ========================================
    ! SP, SS, S - CONTROL DE SIGNO
    ! ========================================
    !
    ! SP - Mostrar signo + siempre
    ! SS - Suprimir signo + (por defecto)
    ! S  - Restaurar modo por defecto

    print '(A)', "=== Control de signo ==="

    print '(SP, F10.2)', 3.14     ! "     +3.14"
    print '(SS, F10.2)', 3.14     ! "      3.14"
    print '(SP, I5, SS, I5)', 10, 10  ! "  +10   10"

    ! ========================================
    ! BN, BZ - BLANCOS EN ENTRADA
    ! ========================================
    !
    ! BN - Blancos como nulos (ignorar)
    ! BZ - Blancos como ceros
    !
    ! Solo afecta a READ, no WRITE

    ! Si input es "  123"
    ! Con BN: lee como 123
    ! Con BZ: lee como 00123 (ceros a la izquierda)

    ! ========================================
    ! kP - FACTOR DE ESCALA
    ! ========================================
    !
    ! kP - Multiplica por 10^k para F
    !      Mueve punto decimal k lugares para E

    print '(A)', "=== Factor de escala (P) ==="

    ! Para formato F: multiplica el valor mostrado por 10^k
    print '(2P, F10.4)', x    ! Valor * 100
    print '(-2P, F10.4)', x   ! Valor / 100
    print '(0P, F10.4)', x    ! Sin escala (reset)

end program formato_control
```

### 2.5 Repetición y Anidamiento

```fortran
program formato_repeticion
    implicit none

    integer :: ints(5) = [1, 2, 3, 4, 5]
    real :: reals(4) = [1.1, 2.2, 3.3, 4.4]
    integer :: matriz(3, 4)
    integer :: i, j

    matriz = reshape([(i, i=1,12)], [3, 4])

    ! ========================================
    ! REPETICIÓN SIMPLE
    ! ========================================
    !
    ! nX donde n es el número de repeticiones
    ! nIw, nFw.d, nAw, etc.

    print '(A)', "=== Repetición Simple ==="

    ! Repetir descriptor
    print '(5I5)', ints                    ! 5 enteros de ancho 5
    print '(4F8.2)', reals                 ! 4 reales de ancho 8

    ! Repetir espacio
    print '(I5, 10X, I5)', 10, 20          ! 10 espacios entre números

    ! ========================================
    ! GRUPOS REPETIDOS
    ! ========================================
    !
    ! n(...) repite el grupo entre paréntesis

    print '(A)', "=== Grupos Repetidos ==="

    ! Grupo repetido
    print '(3(I3, A))', 1, "-", 2, "-", 3, "-"    ! " 1- 2- 3-"

    ! Formato de tabla
    print '(3(A10, "|"))', "Col1", "Col2", "Col3"

    ! Combinación
    print '(2(I5, 2X, F8.2, /))', 1, 1.1, 2, 2.2

    ! ========================================
    ! ANIDAMIENTO DE GRUPOS
    ! ========================================

    print '(A)', "=== Anidamiento ==="

    ! Grupos dentro de grupos
    print '(2(3(I3), /))', ((i, i=1,3), j=1,2)

    ! Formato para matriz
    print '(A)', "Matriz:"
    do i = 1, 3
        print '(4(I5))', matriz(i, :)
    end do

    ! ========================================
    ! REUSO DE FORMATO
    ! ========================================
    !
    ! Si hay más datos que descriptores,
    ! el formato se reutiliza desde el último
    ! grupo entre paréntesis (o el inicio)

    print '(A)', "=== Reuso de Formato ==="

    ! Más datos que descriptores
    integer :: muchos(10) = [(i, i=1,10)]

    ! (I5) se repite automáticamente
    print '(I5)', muchos    ! Cada uno en su línea

    ! Con grupo, repite el grupo
    print '(5I5)', muchos   ! 5 por línea, 2 líneas

    ! El grupo final se repite
    print '("Inicio: ", 4I5)', muchos
    ! Primera línea: "Inicio:     1    2    3    4"
    ! Siguientes:    "    5    6    7    8" etc.

    ! ========================================
    ! ASTERISCO PARA REPETICIÓN INFINITA
    ! ========================================
    !
    ! *(descriptor) repite indefinidamente (F2008)

    print '(*(I5))', muchos    ! Todos en una línea (si caben)
    print '(*(I5, :, ","))', muchos  ! Separados por comas

end program formato_repeticion
```

---

## 3. I/O con Archivos

### 3.1 OPEN Statement

```fortran
program open_ejemplos
    implicit none

    integer :: unidad, iostat_val
    character(len=256) :: nombre_archivo
    character(len=256) :: mensaje_error

    ! ========================================
    ! OPEN BÁSICO
    ! ========================================
    !
    ! OPEN(unit, file, ...)
    ! Abre un archivo y lo asocia a una unidad

    ! Forma más simple
    open(unit=10, file="datos.txt")

    ! Con verificación de error
    open(unit=11, file="salida.txt", iostat=iostat_val)
    if (iostat_val /= 0) then
        print *, "Error abriendo archivo"
        stop 1
    end if

    ! ========================================
    ! PARÁMETROS DE OPEN
    ! ========================================

    ! UNIT - Número de unidad (entero positivo)
    ! Evitar 5, 6, 0 (stdin, stdout, stderr)

    ! FILE - Nombre del archivo
    open(unit=12, file="ruta/al/archivo.dat")
    open(unit=13, file="C:\datos\archivo.txt")  ! Windows

    ! ========================================
    ! STATUS - Estado del archivo
    ! ========================================
    !
    ! 'OLD'     - Debe existir (error si no)
    ! 'NEW'     - No debe existir (error si existe)
    ! 'REPLACE' - Reemplaza si existe, crea si no
    ! 'SCRATCH' - Archivo temporal (se borra al cerrar)
    ! 'UNKNOWN' - Dependiente del sistema (default)

    ! Leer archivo existente
    open(unit=20, file="entrada.dat", status='OLD')

    ! Crear archivo nuevo (error si existe)
    open(unit=21, file="nuevo.dat", status='NEW')

    ! Crear o reemplazar
    open(unit=22, file="salida.dat", status='REPLACE')

    ! Archivo temporal
    open(unit=23, status='SCRATCH')  ! Sin nombre

    ! ========================================
    ! ACTION - Modo de acceso
    ! ========================================
    !
    ! 'READ'      - Solo lectura
    ! 'WRITE'     - Solo escritura
    ! 'READWRITE' - Lectura y escritura (default)

    open(unit=30, file="datos.txt", action='READ')
    open(unit=31, file="salida.txt", action='WRITE')
    open(unit=32, file="log.txt", action='READWRITE')

    ! ========================================
    ! ACCESS - Tipo de acceso
    ! ========================================
    !
    ! 'SEQUENTIAL' - Acceso secuencial (default)
    ! 'DIRECT'     - Acceso directo (por registro)
    ! 'STREAM'     - Acceso por bytes (F2003)

    ! Secuencial (texto normal)
    open(unit=40, file="secuencial.txt", access='SEQUENTIAL')

    ! Directo (registros de tamaño fijo)
    open(unit=41, file="directo.dat", access='DIRECT', recl=100)

    ! Stream (acceso por posición de byte)
    open(unit=42, file="stream.bin", access='STREAM')

    ! ========================================
    ! FORM - Formato de datos
    ! ========================================
    !
    ! 'FORMATTED'   - Texto legible (default para secuencial)
    ! 'UNFORMATTED' - Binario (default para directo)

    ! Archivo de texto
    open(unit=50, file="texto.txt", form='FORMATTED')

    ! Archivo binario
    open(unit=51, file="binario.bin", form='UNFORMATTED')

    ! ========================================
    ! RECL - Longitud de registro
    ! ========================================
    !
    ! Requerido para ACCESS='DIRECT'
    ! Opcional para otros modos

    ! Para acceso directo - tamaño de cada registro
    open(unit=60, file="registros.dat", access='DIRECT', &
         form='UNFORMATTED', recl=80)

    ! ========================================
    ! POSITION - Posición inicial
    ! ========================================
    !
    ! 'ASIS'   - Donde esté (default)
    ! 'REWIND' - Al principio
    ! 'APPEND' - Al final

    ! Agregar a archivo existente
    open(unit=70, file="log.txt", position='APPEND')

    ! ========================================
    ! NEWUNIT - Unidad automática (F2008)
    ! ========================================
    !
    ! El compilador asigna un número de unidad libre

    integer :: mi_unidad
    open(newunit=mi_unidad, file="auto.txt")
    print *, "Unidad asignada:", mi_unidad
    ! Usar mi_unidad para read/write/close

    ! ========================================
    ! IOMSG - Mensaje de error (F2003)
    ! ========================================

    open(unit=80, file="noexiste.txt", status='OLD', &
         iostat=iostat_val, iomsg=mensaje_error)

    if (iostat_val /= 0) then
        print *, "Error: ", trim(mensaje_error)
    end if

    ! ========================================
    ! ENCODING - Codificación (F2003)
    ! ========================================
    !
    ! 'DEFAULT' - Codificación del sistema
    ! 'UTF-8'   - Unicode UTF-8

    open(unit=90, file="unicode.txt", encoding='UTF-8')

    ! ========================================
    ! EJEMPLO COMPLETO
    ! ========================================

    integer :: data_unit
    character(len=100) :: errmsg

    open(newunit=data_unit, &
         file='datos_experimento.dat', &
         status='REPLACE', &
         action='WRITE', &
         access='SEQUENTIAL', &
         form='FORMATTED', &
         iostat=iostat_val, &
         iomsg=errmsg)

    if (iostat_val /= 0) then
        print *, "Error abriendo archivo: ", trim(errmsg)
        stop 1
    end if

    write(data_unit, '(A)') "Datos del experimento"
    close(data_unit)

end program open_ejemplos
```

### 3.2 CLOSE Statement

```fortran
program close_ejemplos
    implicit none

    integer :: unidad, iostat_val
    character(len=100) :: errmsg

    ! ========================================
    ! CLOSE BÁSICO
    ! ========================================

    ! Abrir archivo
    open(unit=10, file="temporal.txt", status='REPLACE')

    ! ... usar el archivo ...

    ! Cerrar archivo
    close(10)

    ! ========================================
    ! CLOSE CON PARÁMETROS
    ! ========================================

    open(unit=20, file="datos.txt", status='REPLACE')
    write(20, *) "Datos importantes"

    ! IOSTAT - Verificar errores al cerrar
    close(20, iostat=iostat_val)
    if (iostat_val /= 0) then
        print *, "Error al cerrar archivo"
    end if

    ! IOMSG - Mensaje de error (F2003)
    close(20, iostat=iostat_val, iomsg=errmsg)

    ! ========================================
    ! STATUS EN CLOSE
    ! ========================================
    !
    ! 'KEEP'   - Mantener archivo (default para no-SCRATCH)
    ! 'DELETE' - Eliminar archivo (default para SCRATCH)

    ! Mantener archivo
    open(unit=30, file="mantener.txt", status='REPLACE')
    write(30, *) "Guardar esto"
    close(30, status='KEEP')  ! El archivo persiste

    ! Eliminar archivo al cerrar
    open(unit=31, file="temporal.txt", status='REPLACE')
    write(31, *) "Datos temporales"
    close(31, status='DELETE')  ! Se elimina el archivo

    ! SCRATCH se elimina automáticamente
    open(unit=32, status='SCRATCH')
    write(32, *) "Temporal"
    close(32)  ! Automáticamente se elimina

    ! ========================================
    ! BUENAS PRÁCTICAS
    ! ========================================

    ! Siempre cerrar archivos cuando ya no se necesitan
    ! para liberar recursos del sistema

    integer :: archivo

    open(newunit=archivo, file="proceso.txt", status='REPLACE')

    ! Usar bloque para asegurar cierre (similar a try-finally)
    block
        write(archivo, *) "Línea 1"
        write(archivo, *) "Línea 2"
        ! Si ocurre un error aquí, el archivo queda abierto
    end block

    close(archivo)  ! Asegurar cierre

end program close_ejemplos
```

### 3.3 Archivos Secuenciales

```fortran
program archivos_secuenciales
    implicit none

    integer :: unidad, i, iostat_val
    real :: x, y, z
    character(len=50) :: nombre
    character(len=100) :: linea

    ! ========================================
    ! ESCRITURA SECUENCIAL
    ! ========================================

    open(newunit=unidad, file="secuencial.txt", status='REPLACE')

    ! Escribir con formato libre
    write(unidad, *) "Encabezado del archivo"
    write(unidad, *) 1, 2, 3
    write(unidad, *) 3.14159, 2.71828

    ! Escribir con formato específico
    write(unidad, '(A20, 3F10.4)') "Coordenadas:", 1.0, 2.0, 3.0

    ! Escribir múltiples líneas
    do i = 1, 10
        write(unidad, '(I5, F10.2)') i, real(i)**2
    end do

    close(unidad)

    ! ========================================
    ! LECTURA SECUENCIAL
    ! ========================================

    open(newunit=unidad, file="secuencial.txt", status='OLD')

    ! Leer con formato libre
    read(unidad, *) linea
    print *, "Primera línea:", trim(linea)

    read(unidad, *) i, x, y
    print *, "Enteros:", i, x, y

    ! Leer línea completa como string
    read(unidad, '(A)') linea
    print *, "Línea completa:", trim(linea)

    ! Leer hasta fin de archivo
    do
        read(unidad, *, iostat=iostat_val) i, x
        if (iostat_val /= 0) exit
        print *, "Leído:", i, x
    end do

    close(unidad)

    ! ========================================
    ! REWIND, BACKSPACE, ENDFILE
    ! ========================================

    open(newunit=unidad, file="control.txt", status='REPLACE')

    ! Escribir datos
    write(unidad, *) "Línea 1"
    write(unidad, *) "Línea 2"
    write(unidad, *) "Línea 3"

    ! REWIND - Volver al principio
    rewind(unidad)

    ! Ahora podemos leer desde el inicio
    read(unidad, '(A)') linea
    print *, "Después de rewind:", trim(linea)

    ! BACKSPACE - Retroceder un registro
    read(unidad, '(A)') linea  ! Lee línea 2
    backspace(unidad)          ! Retrocede
    read(unidad, '(A)') linea  ! Lee línea 2 otra vez
    print *, "Después de backspace:", trim(linea)

    ! ENDFILE - Marcar fin de archivo
    rewind(unidad)
    write(unidad, *) "Nueva línea 1"
    endfile(unidad)  ! Trunca el archivo aquí

    close(unidad)

    ! ========================================
    ! APPEND - Agregar a archivo existente
    ! ========================================

    ! Crear archivo inicial
    open(newunit=unidad, file="log.txt", status='REPLACE')
    write(unidad, *) "Entrada 1"
    write(unidad, *) "Entrada 2"
    close(unidad)

    ! Agregar más datos
    open(newunit=unidad, file="log.txt", position='APPEND')
    write(unidad, *) "Entrada 3"
    write(unidad, *) "Entrada 4"
    close(unidad)

    ! El archivo ahora tiene 4 líneas

end program archivos_secuenciales
```

### 3.4 Archivos de Acceso Directo

```fortran
program archivos_directos
    implicit none

    ! ========================================
    ! ACCESO DIRECTO (Random Access)
    ! ========================================
    !
    ! Permite leer/escribir cualquier registro
    ! sin recorrer los anteriores.
    !
    ! Todos los registros tienen el mismo tamaño (RECL)
    ! Los registros se numeran desde 1

    integer :: unidad, i, iostat_val
    integer, parameter :: RECL_SIZE = 100  ! Tamaño de registro

    ! Tipo para registros de tamaño fijo
    type :: empleado
        integer :: id
        character(len=30) :: nombre
        real :: salario
        integer :: departamento
    end type empleado

    type(empleado) :: emp, emp_leido

    ! ========================================
    ! ABRIR ARCHIVO DE ACCESO DIRECTO
    ! ========================================

    open(newunit=unidad, &
         file="empleados.dat", &
         access='DIRECT', &
         form='UNFORMATTED', &
         recl=RECL_SIZE, &
         status='REPLACE')

    ! ========================================
    ! ESCRIBIR REGISTROS
    ! ========================================

    ! Escribir en registro específico con REC=
    emp = empleado(101, "Ana García", 50000.0, 1)
    write(unidad, rec=1) emp

    emp = empleado(102, "Juan López", 45000.0, 2)
    write(unidad, rec=2) emp

    emp = empleado(103, "María Ruiz", 55000.0, 1)
    write(unidad, rec=3) emp

    ! Escribir en orden no secuencial
    emp = empleado(110, "Pedro Sánchez", 60000.0, 3)
    write(unidad, rec=10) emp  ! Registro 10 directamente

    ! ========================================
    ! LEER REGISTROS
    ! ========================================

    ! Leer registro específico
    read(unidad, rec=2) emp_leido
    print *, "Registro 2:", emp_leido%id, trim(emp_leido%nombre)

    ! Leer registro 10 directamente
    read(unidad, rec=10) emp_leido
    print *, "Registro 10:", emp_leido%id, trim(emp_leido%nombre)

    ! Volver a leer registro 1
    read(unidad, rec=1) emp_leido
    print *, "Registro 1:", emp_leido%id, trim(emp_leido%nombre)

    ! ========================================
    ! ACTUALIZAR REGISTRO
    ! ========================================

    ! Leer, modificar, escribir de vuelta
    read(unidad, rec=2) emp
    emp%salario = emp%salario * 1.10  ! Aumento del 10%
    write(unidad, rec=2) emp

    close(unidad)

    ! ========================================
    ! CALCULAR RECL CORRECTO
    ! ========================================

    ! El valor de RECL depende del compilador:
    ! - Algunos usan bytes
    ! - Otros usan "palabras" de 4 bytes
    !
    ! Usar INQUIRE para determinar tamaño:

    integer :: record_length

    inquire(iolength=record_length) emp
    print *, "Tamaño de registro calculado:", record_length

    ! Usar este valor para RECL
    open(newunit=unidad, &
         file="datos.dat", &
         access='DIRECT', &
         form='UNFORMATTED', &
         recl=record_length, &
         status='REPLACE')

    ! ========================================
    ! ACCESO DIRECTO FORMATEADO
    ! ========================================

    ! También se puede usar con formato
    open(newunit=unidad, &
         file="registros.txt", &
         access='DIRECT', &
         form='FORMATTED', &
         recl=80, &
         status='REPLACE')

    write(unidad, '(I5, A30, F10.2)', rec=1) 1, "Primero", 100.0
    write(unidad, '(I5, A30, F10.2)', rec=2) 2, "Segundo", 200.0

    character(len=80) :: linea
    read(unidad, '(A80)', rec=1) linea
    print *, "Registro 1:", trim(linea)

    close(unidad)

end program archivos_directos
```

### 3.5 INQUIRE Statement

```fortran
program inquire_ejemplos
    implicit none

    integer :: unidad, iostat_val
    logical :: existe, abierto
    character(len=256) :: nombre_archivo
    character(len=20) :: modo_acceso, formato
    integer :: tamanio_registro, numero_registros
    integer :: posicion
    integer :: iolength

    ! ========================================
    ! INQUIRE POR NOMBRE DE ARCHIVO
    ! ========================================

    ! Verificar si archivo existe
    inquire(file="datos.txt", exist=existe)
    if (existe) then
        print *, "El archivo datos.txt existe"
    else
        print *, "El archivo datos.txt NO existe"
    end if

    ! Más información sobre archivo
    inquire(file="datos.txt", &
            exist=existe, &
            opened=abierto)

    if (existe .and. .not. abierto) then
        print *, "Archivo existe pero no está abierto"
    end if

    ! ========================================
    ! INQUIRE POR UNIDAD
    ! ========================================

    open(unit=20, file="prueba.txt", status='REPLACE')
    write(20, *) "Contenido de prueba"

    ! Consultar estado de la unidad
    inquire(unit=20, &
            opened=abierto, &
            name=nombre_archivo, &
            access=modo_acceso, &
            form=formato)

    print *, "Unidad 20:"
    print *, "  Abierto:", abierto
    print *, "  Nombre:", trim(nombre_archivo)
    print *, "  Acceso:", trim(modo_acceso)
    print *, "  Formato:", trim(formato)

    close(20)

    ! ========================================
    ! PROPIEDADES CONSULTABLES
    ! ========================================

    character(len=20) :: accion, posicion_str, estado
    character(len=20) :: leer_ok, escribir_ok, readwrite_ok
    integer :: recl_actual

    open(unit=30, file="info.txt", status='REPLACE', action='READWRITE')

    inquire(unit=30, &
            action=accion, &           ! READ, WRITE, READWRITE
            access=modo_acceso, &      ! SEQUENTIAL, DIRECT, STREAM
            form=formato, &            ! FORMATTED, UNFORMATTED
            position=posicion_str, &   ! REWIND, APPEND, ASIS
            read=leer_ok, &            ! YES, NO, UNKNOWN
            write=escribir_ok, &       ! YES, NO, UNKNOWN
            readwrite=readwrite_ok, &  ! YES, NO, UNKNOWN
            recl=recl_actual)          ! Longitud de registro

    print *, "Propiedades de unidad 30:"
    print *, "  Acción:", trim(accion)
    print *, "  Puede leer:", trim(leer_ok)
    print *, "  Puede escribir:", trim(escribir_ok)

    close(30)

    ! ========================================
    ! INQUIRE IOLENGTH
    ! ========================================
    !
    ! Determinar tamaño necesario para RECL
    ! en archivos de acceso directo

    type :: registro
        integer :: id
        character(len=50) :: descripcion
        real(8) :: valores(10)
    end type registro

    type(registro) :: r

    inquire(iolength=iolength) r
    print *, "IOLENGTH para registro:", iolength

    ! Ahora podemos usar iolength como RECL
    open(unit=40, file="registros.dat", &
         access='DIRECT', form='UNFORMATTED', &
         recl=iolength, status='REPLACE')

    r%id = 1
    r%descripcion = "Registro de prueba"
    r%valores = 1.0
    write(40, rec=1) r

    close(40)

    ! ========================================
    ! INQUIRE SIZE (F2003)
    ! ========================================
    !
    ! Obtener tamaño del archivo en bytes

    integer(8) :: tamanio_bytes

    ! Crear archivo de prueba
    open(unit=50, file="tamanio.txt", status='REPLACE')
    write(50, '(A)') "Línea de texto para medir tamaño"
    close(50)

    inquire(file="tamanio.txt", size=tamanio_bytes)
    print *, "Tamaño del archivo:", tamanio_bytes, "bytes"

    ! ========================================
    ! INQUIRE NEXTREC (Acceso Directo)
    ! ========================================
    !
    ! Siguiente registro a leer/escribir

    integer :: siguiente_registro

    open(unit=60, file="directo.dat", access='DIRECT', &
         form='UNFORMATTED', recl=4, status='REPLACE')

    write(60, rec=1) 100
    write(60, rec=2) 200

    inquire(unit=60, nextrec=siguiente_registro)
    print *, "Siguiente registro:", siguiente_registro  ! 3

    close(60)

    ! ========================================
    ! INQUIRE POS (Stream I/O, F2003)
    ! ========================================
    !
    ! Posición actual en bytes para stream I/O

    integer(8) :: posicion_actual

    open(unit=70, file="stream.bin", access='STREAM', &
         form='UNFORMATTED', status='REPLACE')

    write(70) 1, 2, 3, 4  ! Escribir 4 enteros

    inquire(unit=70, pos=posicion_actual)
    print *, "Posición después de escribir:", posicion_actual

    close(70)

end program inquire_ejemplos
```

---

## 4. I/O Avanzado

### 4.1 NAMELIST

```fortran
program namelist_ejemplo
    implicit none

    ! ========================================
    ! NAMELIST - I/O de Variables con Nombre
    ! ========================================
    !
    ! NAMELIST permite leer/escribir grupos de variables
    ! con sus nombres, en cualquier orden.
    !
    ! Formato del archivo:
    !   &nombre_grupo
    !     variable1 = valor1,
    !     variable2 = valor2
    !   /

    integer :: iteraciones = 100
    real :: tolerancia = 1.0e-6
    real :: dt = 0.001
    logical :: verbose = .false.
    character(len=50) :: archivo_salida = "resultado.dat"
    real :: parametros(3) = [1.0, 2.0, 3.0]

    ! Declarar NAMELIST
    namelist /configuracion/ iteraciones, tolerancia, dt, &
                             verbose, archivo_salida, parametros

    integer :: unidad

    ! ========================================
    ! ESCRIBIR NAMELIST
    ! ========================================

    open(newunit=unidad, file="config.nml", status='REPLACE')
    write(unidad, nml=configuracion)
    close(unidad)

    ! El archivo contendrá algo como:
    ! &CONFIGURACION
    !  ITERACIONES=         100,
    !  TOLERANCIA=  1.00000005E-06,
    !  DT=  1.00000005E-03,
    !  VERBOSE=F,
    !  ARCHIVO_SALIDA="resultado.dat                             ",
    !  PARAMETROS=  1.00000000    ,  2.00000000    ,  3.00000000
    !  /

    ! ========================================
    ! LEER NAMELIST
    ! ========================================

    ! Cambiar valores por defecto
    iteraciones = 0
    tolerancia = 0.0
    dt = 0.0

    open(newunit=unidad, file="config.nml", status='OLD')
    read(unidad, nml=configuracion)
    close(unidad)

    print *, "Configuración leída:"
    print *, "  Iteraciones:", iteraciones
    print *, "  Tolerancia:", tolerancia
    print *, "  dt:", dt
    print *, "  Verbose:", verbose
    print *, "  Archivo:", trim(archivo_salida)
    print *, "  Parámetros:", parametros

    ! ========================================
    ! NAMELIST PARCIAL
    ! ========================================
    !
    ! No es necesario especificar todas las variables
    ! Las no especificadas mantienen su valor actual

    ! Archivo de entrada puede contener solo:
    ! &CONFIGURACION
    !   iteraciones = 500,
    !   verbose = .true.
    ! /
    !
    ! El resto de variables mantienen sus valores

    ! ========================================
    ! MÚLTIPLES NAMELISTS
    ! ========================================

    real :: x0, y0, z0
    real :: vx, vy, vz
    real :: masa

    namelist /posicion_inicial/ x0, y0, z0
    namelist /velocidad_inicial/ vx, vy, vz
    namelist /propiedades/ masa

    ! Leer múltiples namelists de un archivo
    open(newunit=unidad, file="particula.nml", status='OLD')
    read(unidad, nml=posicion_inicial)
    read(unidad, nml=velocidad_inicial)
    read(unidad, nml=propiedades)
    close(unidad)

    ! ========================================
    ! NAMELIST CON ARRAYS
    ! ========================================

    integer :: indices(5)
    real :: matriz(3, 3)

    namelist /datos_array/ indices, matriz

    ! Formato de entrada para arrays:
    ! &DATOS_ARRAY
    !   indices = 1, 2, 3, 4, 5,
    !   matriz(1,1) = 1.0, matriz(1,2) = 2.0,
    !   matriz = 9*0.0
    ! /

    ! ========================================
    ! VENTAJAS DE NAMELIST
    ! ========================================
    !
    ! 1. Auto-documentado (nombres de variables visibles)
    ! 2. Orden flexible (variables en cualquier orden)
    ! 3. Parcial (no requiere todas las variables)
    ! 4. Comentarios permitidos en archivo (!)
    ! 5. Ideal para archivos de configuración

end program namelist_ejemplo
```

### 4.2 Internal I/O

```fortran
program internal_io
    implicit none

    ! ========================================
    ! INTERNAL I/O
    ! ========================================
    !
    ! Leer de y escribir a variables CHARACTER
    ! en lugar de archivos externos.
    !
    ! Útil para:
    ! - Convertir números a strings
    ! - Parsear strings
    ! - Construir mensajes formateados

    character(len=100) :: buffer
    character(len=20) :: num_str
    integer :: i, j
    real :: x, y
    integer :: iostat_val

    ! ========================================
    ! WRITE A STRING
    ! ========================================

    ! Convertir número a string
    i = 42
    write(buffer, '(I10)') i
    print *, "Entero como string: '", trim(adjustl(buffer)), "'"

    ! Convertir real a string
    x = 3.14159
    write(buffer, '(F10.4)') x
    print *, "Real como string: '", trim(adjustl(buffer)), "'"

    ! Formato compuesto
    i = 1
    x = 99.5
    write(buffer, '(A, I3, A, F6.1, A)') "Item ", i, " tiene valor ", x, "%"
    print *, trim(buffer)

    ! ========================================
    ! READ DE STRING
    ! ========================================

    ! Parsear número de string
    buffer = "   12345   "
    read(buffer, *) i
    print *, "Parseado:", i

    ! Parsear múltiples valores
    buffer = "10  20  30.5  40.7"
    read(buffer, *) i, j, x, y
    print *, "Valores:", i, j, x, y

    ! Con formato específico
    buffer = "00042003.14"
    read(buffer, '(I5, F5.2)') i, x
    print *, "Con formato:", i, x

    ! ========================================
    ! MANEJO DE ERRORES
    ! ========================================

    buffer = "no_es_numero"
    read(buffer, *, iostat=iostat_val) i
    if (iostat_val /= 0) then
        print *, "Error: no se pudo parsear como entero"
    end if

    ! ========================================
    ! CONSTRUCCIÓN DE NOMBRES DE ARCHIVO
    ! ========================================

    character(len=100) :: nombre_archivo
    integer :: numero_archivo

    do numero_archivo = 1, 5
        write(nombre_archivo, '(A, I3.3, A)') "datos_", numero_archivo, ".txt"
        print *, "Archivo:", trim(nombre_archivo)
        ! datos_001.txt, datos_002.txt, etc.
    end do

    ! Alternativa con I0
    write(nombre_archivo, '(A, I0, A)') "resultado_", 42, ".dat"
    print *, "Archivo:", trim(nombre_archivo)  ! resultado_42.dat

    ! ========================================
    ! ARRAYS DE CARACTERES
    ! ========================================

    character(len=50) :: lineas(3)
    real :: valores(3) = [1.1, 2.2, 3.3]

    ! Escribir a array de strings
    do i = 1, 3
        write(lineas(i), '(A, I1, A, F5.2)') "Línea ", i, ": ", valores(i)
    end do

    ! Imprimir
    do i = 1, 3
        print *, trim(lineas(i))
    end do

    ! ========================================
    ! SPLIT DE STRING
    ! ========================================

    character(len=100) :: csv_line
    character(len=20) :: campos(5)
    integer :: pos, campo_num

    csv_line = "uno,dos,tres,cuatro,cinco"

    ! Parsear CSV simple (sin comas en valores)
    read(csv_line, *) campos  ! Funciona si separado por espacios

    ! Para CSV real, necesitas parsear manualmente
    ! o usar bibliotecas especializadas

    ! ========================================
    ! TRIM Y ADJUSTL
    ! ========================================

    ! Crear string limpio
    x = 123.456
    write(buffer, '(F20.6)') x
    num_str = adjustl(buffer)       ! Mover a izquierda
    num_str = trim(num_str)         ! Quitar espacios finales
    print *, "Limpio: '", num_str, "'"

    ! En una línea
    write(num_str, '(G0)') x        ! G0 usa espacio mínimo (F2008)
    print *, "Con G0: '", trim(num_str), "'"

end program internal_io
```

### 4.3 Unformatted I/O

```fortran
program unformatted_io
    implicit none

    ! ========================================
    ! UNFORMATTED I/O (Binario)
    ! ========================================
    !
    ! Ventajas:
    ! - Más rápido (sin conversión texto<->binario)
    ! - Más compacto (números en representación nativa)
    ! - Precisión exacta (sin pérdida por formateo)
    !
    ! Desventajas:
    ! - No portable entre arquitecturas
    ! - No legible por humanos
    ! - Dependiente del compilador/sistema

    integer :: unidad
    integer :: i
    real :: x
    real :: array(1000)
    real :: matriz(100, 100)

    ! Inicializar datos
    x = 3.14159265358979
    array = [(real(i), i=1,1000)]
    matriz = reshape([(real(i), i=1,10000)], [100, 100])

    ! ========================================
    ! ESCRITURA UNFORMATTED SECUENCIAL
    ! ========================================

    open(newunit=unidad, file="datos.bin", &
         form='UNFORMATTED', status='REPLACE')

    ! Cada WRITE crea un "registro"
    ! El sistema añade marcadores de longitud

    write(unidad) i                    ! Un entero
    write(unidad) x                    ! Un real
    write(unidad) array                ! Array completo
    write(unidad) matriz               ! Matriz completa

    ! Múltiples valores en un registro
    write(unidad) i, x, array(1:10)

    close(unidad)

    ! ========================================
    ! LECTURA UNFORMATTED SECUENCIAL
    ! ========================================

    integer :: i_leido
    real :: x_leido
    real :: array_leido(1000)
    real :: matriz_leida(100, 100)

    open(newunit=unidad, file="datos.bin", &
         form='UNFORMATTED', status='OLD')

    read(unidad) i_leido
    read(unidad) x_leido
    read(unidad) array_leido
    read(unidad) matriz_leida

    close(unidad)

    print *, "Leído: i=", i_leido, " x=", x_leido
    print *, "Primer elemento array:", array_leido(1)
    print *, "Elemento matriz(50,50):", matriz_leida(50,50)

    ! ========================================
    ! UNFORMATTED DIRECTO
    ! ========================================

    type :: registro
        integer :: id
        real :: datos(10)
    end type registro

    type(registro) :: r, r_leido
    integer :: recl

    ! Calcular tamaño de registro
    inquire(iolength=recl) r

    open(newunit=unidad, file="registros.bin", &
         access='DIRECT', form='UNFORMATTED', &
         recl=recl, status='REPLACE')

    ! Escribir registros
    do i = 1, 100
        r%id = i
        r%datos = real(i)
        write(unidad, rec=i) r
    end do

    ! Leer registro específico
    read(unidad, rec=50) r_leido
    print *, "Registro 50: id=", r_leido%id

    close(unidad)

    ! ========================================
    ! ESTRUCTURA DE ARCHIVO UNFORMATTED
    ! ========================================
    !
    ! Formato secuencial (simplificado):
    !
    ! [longitud][datos...][longitud]
    ! [longitud][datos...][longitud]
    ! ...
    !
    ! Los marcadores de longitud son añadidos
    ! automáticamente y varían por compilador.
    !
    ! Por eso los archivos unformatted pueden
    ! no ser portables entre compiladores.

    ! ========================================
    ! CONSIDERACIONES DE PORTABILIDAD
    ! ========================================
    !
    ! Para datos portables entre sistemas:
    !
    ! 1. Usar ACCESS='STREAM' (F2003)
    ! 2. Usar formato texto para archivos de intercambio
    ! 3. Usar bibliotecas estándar (NetCDF, HDF5)
    ! 4. Documentar: compilador, endianness, tamaños

end program unformatted_io
```

### 4.4 Stream I/O (F2003)

```fortran
program stream_io
    implicit none

    ! ========================================
    ! STREAM I/O (F2003)
    ! ========================================
    !
    ! Acceso por posición de byte (como C)
    ! Sin marcadores de registro
    ! Más portable que unformatted tradicional

    integer :: unidad
    integer :: i
    integer(1) :: byte
    real :: x
    character(len=100) :: texto

    ! ========================================
    ! STREAM UNFORMATTED
    ! ========================================

    open(newunit=unidad, file="stream.bin", &
         access='STREAM', form='UNFORMATTED', &
         status='REPLACE')

    ! Escribir datos directamente
    x = 3.14159
    write(unidad) x           ! 4 bytes (real simple)
    write(unidad) 42          ! 4 bytes (entero)
    write(unidad) "HOLA"      ! 4 bytes (caracteres)

    close(unidad)

    ! Leer datos
    open(newunit=unidad, file="stream.bin", &
         access='STREAM', form='UNFORMATTED', &
         status='OLD')

    read(unidad) x
    read(unidad) i
    read(unidad) texto(1:4)

    print *, "x =", x
    print *, "i =", i
    print *, "texto =", texto(1:4)

    close(unidad)

    ! ========================================
    ! POSICIONAMIENTO CON POS=
    ! ========================================

    integer(8) :: posicion
    real :: valores(100)

    valores = [(real(i), i=1,100)]

    open(newunit=unidad, file="posicion.bin", &
         access='STREAM', form='UNFORMATTED', &
         status='REPLACE')

    write(unidad) valores

    ! Ir a posición específica
    ! La posición es en bytes, empezando en 1
    ! real ocupa 4 bytes, así que elemento n está en pos = 4*(n-1)+1

    ! Leer elemento 50
    posicion = 4 * 49 + 1    ! Posición del elemento 50
    read(unidad, pos=posicion) x
    print *, "Elemento 50:", x

    ! Modificar elemento 50
    x = 999.0
    write(unidad, pos=posicion) x

    close(unidad)

    ! ========================================
    ! INQUIRE POS
    ! ========================================

    open(newunit=unidad, file="stream.bin", &
         access='STREAM', form='UNFORMATTED', &
         status='OLD')

    read(unidad) x
    inquire(unit=unidad, pos=posicion)
    print *, "Posición después de leer real:", posicion

    close(unidad)

    ! ========================================
    ! STREAM FORMATTED
    ! ========================================

    ! También se puede usar stream con formato
    ! Útil para procesar archivos de texto byte a byte

    open(newunit=unidad, file="stream.txt", &
         access='STREAM', form='FORMATTED', &
         status='REPLACE')

    write(unidad, '(A)', advance='NO') "Línea 1"
    write(unidad, '(A)') ""  ! Nueva línea
    write(unidad, '(A)') "Línea 2"

    close(unidad)

    ! ========================================
    ! LEER ARCHIVO BYTE A BYTE
    ! ========================================

    integer(1) :: bytes(100)
    integer :: n_bytes

    open(newunit=unidad, file="cualquier.bin", &
         access='STREAM', form='UNFORMATTED', &
         status='OLD')

    ! Leer primeros 100 bytes
    read(unidad) bytes

    ! Mostrar en hexadecimal
    do i = 1, min(20, size(bytes))
        write(*, '(Z2.2, 1X)', advance='NO') bytes(i)
    end do
    print *

    close(unidad)

    ! ========================================
    ! VENTAJAS DE STREAM I/O
    ! ========================================
    !
    ! 1. Sin marcadores de registro (más portable)
    ! 2. Control preciso de posición
    ! 3. Compatible con formatos binarios estándar
    ! 4. Interoperabilidad con C más fácil
    ! 5. Ideal para formatos de archivo personalizados

end program stream_io
```

---

## 5. Manejo de Errores

### 5.1 IOSTAT, ERR, END

```fortran
program manejo_errores_io
    implicit none

    integer :: unidad, iostat_val
    character(len=100) :: linea
    real :: x
    integer :: i

    ! ========================================
    ! IOSTAT - Código de estado
    ! ========================================
    !
    ! iostat = 0  -> Éxito
    ! iostat < 0  -> Fin de archivo (EOF) o fin de registro
    ! iostat > 0  -> Error

    ! Ejemplo de lectura con IOSTAT
    open(newunit=unidad, file="datos.txt", status='OLD', &
         iostat=iostat_val)

    if (iostat_val /= 0) then
        print *, "Error abriendo archivo, código:", iostat_val
        stop 1
    end if

    ! Leer hasta fin de archivo
    do
        read(unidad, *, iostat=iostat_val) x
        if (iostat_val < 0) then
            print *, "Fin de archivo alcanzado"
            exit
        else if (iostat_val > 0) then
            print *, "Error de lectura, código:", iostat_val
            exit
        end if
        print *, "Valor leído:", x
    end do

    close(unidad)

    ! ========================================
    ! CÓDIGOS IOSTAT ESTÁNDAR (F2003)
    ! ========================================

    use, intrinsic :: iso_fortran_env, only: &
        iostat_end, iostat_eor

    ! iostat_end - Valor retornado al alcanzar EOF
    ! iostat_eor - Valor retornado al alcanzar fin de registro

    open(newunit=unidad, file="datos.txt", status='OLD')

    do
        read(unidad, '(A)', iostat=iostat_val) linea
        if (iostat_val == iostat_end) then
            print *, "EOF"
            exit
        else if (iostat_val == iostat_eor) then
            print *, "Fin de registro"
            cycle
        else if (iostat_val > 0) then
            print *, "Error"
            exit
        end if
        print *, trim(linea)
    end do

    close(unidad)

    ! ========================================
    ! ERR= y END= (Estilo legacy)
    ! ========================================
    !
    ! ERR=etiqueta - Saltar a etiqueta si error
    ! END=etiqueta - Saltar a etiqueta si EOF

    open(newunit=unidad, file="datos.txt", status='OLD', err=900)

    do
        read(unidad, *, err=800, end=700) x
        print *, "Leído:", x
    end do

700 continue
    print *, "Fin de archivo"
    go to 999

800 continue
    print *, "Error de lectura"
    go to 999

900 continue
    print *, "Error abriendo archivo"

999 continue
    close(unidad, iostat=iostat_val)  ! Ignorar error de close

    ! ========================================
    ! COMBINANDO ERR, END, IOSTAT
    ! ========================================
    !
    ! Se pueden usar juntos, pero IOSTAT tiene prioridad
    ! Si IOSTAT está presente, no se salta a ERR/END

    read(unidad, *, iostat=iostat_val, err=100, end=200) x
    ! Con IOSTAT presente, ERR y END son ignorados
    ! Solo se usa el valor de iostat_val

    ! Sin IOSTAT, ERR y END funcionan
    read(unidad, *, err=100, end=200) x
    ! Salta a 100 si error, a 200 si EOF

    ! ========================================
    ! EOR= (Fin de Registro)
    ! ========================================

    character(len=10) :: buffer
    integer :: chars_read

    open(newunit=unidad, file="lineas.txt", status='OLD')

    ! Leer con non-advancing, detectar fin de línea
    read(unidad, '(A)', advance='NO', size=chars_read, &
         eor=300, end=400) buffer
    go to 500

300 print *, "Fin de línea, leídos:", chars_read
    go to 500

400 print *, "Fin de archivo"

500 continue
    close(unidad)

end program manejo_errores_io
```

### 5.2 IOMSG (F2003)

```fortran
program iomsg_ejemplo
    implicit none

    ! ========================================
    ! IOMSG - Mensaje de Error Descriptivo
    ! ========================================
    !
    ! F2003 añade IOMSG para obtener descripción
    ! textual del error, no solo el código

    integer :: unidad, iostat_val
    character(len=256) :: mensaje_error
    character(len=100) :: nombre_archivo
    real :: x

    nombre_archivo = "archivo_inexistente.txt"

    ! ========================================
    ! IOMSG EN OPEN
    ! ========================================

    open(newunit=unidad, file=nombre_archivo, status='OLD', &
         iostat=iostat_val, iomsg=mensaje_error)

    if (iostat_val /= 0) then
        print *, "Error abriendo archivo:"
        print *, "  Código:", iostat_val
        print *, "  Mensaje:", trim(mensaje_error)
        ! Mensaje típico: "No such file or directory"
    end if

    ! ========================================
    ! IOMSG EN READ/WRITE
    ! ========================================

    ! Crear archivo de prueba
    open(newunit=unidad, file="prueba.txt", status='REPLACE')
    write(unidad, *) "texto no numerico"
    close(unidad)

    open(newunit=unidad, file="prueba.txt", status='OLD')

    read(unidad, *, iostat=iostat_val, iomsg=mensaje_error) x

    if (iostat_val /= 0) then
        print *, "Error de lectura:"
        print *, "  Código:", iostat_val
        print *, "  Mensaje:", trim(mensaje_error)
        ! Mensaje típico: "Bad value during integer read"
    end if

    close(unidad)

    ! ========================================
    ! IOMSG EN CLOSE
    ! ========================================

    ! Intentar cerrar unidad no abierta
    close(999, iostat=iostat_val, iomsg=mensaje_error)

    if (iostat_val /= 0) then
        print *, "Error al cerrar:"
        print *, "  Mensaje:", trim(mensaje_error)
    end if

    ! ========================================
    ! FUNCIÓN DE MANEJO DE ERRORES
    ! ========================================

contains

    subroutine manejar_error_io(operacion, iostat, iomsg)
        character(len=*), intent(in) :: operacion
        integer, intent(in) :: iostat
        character(len=*), intent(in) :: iomsg

        if (iostat == 0) return

        print '(A)', "=========================================="
        print '(A, A)', "Error de I/O en: ", operacion
        print '(A, I0)', "Código IOSTAT: ", iostat

        if (iostat < 0) then
            print '(A)', "Tipo: Fin de archivo/registro"
        else
            print '(A)', "Tipo: Error de I/O"
        end if

        print '(A, A)', "Mensaje: ", trim(iomsg)
        print '(A)', "=========================================="

    end subroutine manejar_error_io

end program iomsg_ejemplo
```

### 5.3 Patrones de Manejo de Errores

```fortran
module io_utilities
    implicit none

    ! ========================================
    ! MÓDULO DE UTILIDADES DE I/O
    ! ========================================

    integer, parameter :: MAX_MSG_LEN = 256

contains

    ! ========================================
    ! ABRIR ARCHIVO CON MANEJO DE ERRORES
    ! ========================================

    function abrir_archivo(nombre, modo, unidad) result(exito)
        character(len=*), intent(in) :: nombre
        character(len=*), intent(in) :: modo  ! 'r', 'w', 'a'
        integer, intent(out) :: unidad
        logical :: exito

        integer :: iostat
        character(len=MAX_MSG_LEN) :: iomsg
        character(len=20) :: status, action, position

        ! Configurar según modo
        select case (modo)
        case ('r', 'R')
            status = 'OLD'
            action = 'READ'
            position = 'REWIND'
        case ('w', 'W')
            status = 'REPLACE'
            action = 'WRITE'
            position = 'REWIND'
        case ('a', 'A')
            status = 'UNKNOWN'
            action = 'WRITE'
            position = 'APPEND'
        case default
            print *, "Modo inválido: ", modo
            exito = .false.
            return
        end select

        open(newunit=unidad, file=nombre, status=status, &
             action=action, position=position, &
             iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) then
            print *, "Error abriendo ", trim(nombre)
            print *, "  ", trim(iomsg)
            exito = .false.
        else
            exito = .true.
        end if

    end function abrir_archivo

    ! ========================================
    ! LEER LÍNEA SEGURA
    ! ========================================

    function leer_linea(unidad, linea) result(estado)
        integer, intent(in) :: unidad
        character(len=*), intent(out) :: linea
        integer :: estado  ! 0=OK, -1=EOF, >0=Error

        integer :: iostat
        character(len=MAX_MSG_LEN) :: iomsg

        read(unidad, '(A)', iostat=iostat, iomsg=iomsg) linea

        if (iostat == 0) then
            estado = 0
        else if (iostat < 0) then
            estado = -1  ! EOF
        else
            print *, "Error leyendo: ", trim(iomsg)
            estado = iostat
        end if

    end function leer_linea

    ! ========================================
    ! LEER VALORES CON VALIDACIÓN
    ! ========================================

    function leer_entero(unidad, valor, minimo, maximo) result(exito)
        integer, intent(in) :: unidad
        integer, intent(out) :: valor
        integer, intent(in), optional :: minimo, maximo
        logical :: exito

        integer :: iostat
        character(len=MAX_MSG_LEN) :: iomsg

        read(unidad, *, iostat=iostat, iomsg=iomsg) valor

        if (iostat /= 0) then
            print *, "Error leyendo entero: ", trim(iomsg)
            exito = .false.
            return
        end if

        ! Validar rango
        if (present(minimo)) then
            if (valor < minimo) then
                print *, "Valor menor que mínimo:", minimo
                exito = .false.
                return
            end if
        end if

        if (present(maximo)) then
            if (valor > maximo) then
                print *, "Valor mayor que máximo:", maximo
                exito = .false.
                return
            end if
        end if

        exito = .true.

    end function leer_entero

    ! ========================================
    ! LEER ARCHIVO COMPLETO A ARRAY
    ! ========================================

    function leer_archivo_lineas(nombre, lineas, n_lineas) result(exito)
        character(len=*), intent(in) :: nombre
        character(len=*), allocatable, intent(out) :: lineas(:)
        integer, intent(out) :: n_lineas
        logical :: exito

        integer :: unidad, iostat
        character(len=MAX_MSG_LEN) :: iomsg
        character(len=1000) :: buffer
        integer :: i

        ! Contar líneas
        if (.not. abrir_archivo(nombre, 'r', unidad)) then
            exito = .false.
            return
        end if

        n_lineas = 0
        do
            read(unidad, '(A)', iostat=iostat) buffer
            if (iostat /= 0) exit
            n_lineas = n_lineas + 1
        end do

        ! Allocar array
        allocate(lineas(n_lineas))

        ! Leer líneas
        rewind(unidad)
        do i = 1, n_lineas
            read(unidad, '(A)') lineas(i)
        end do

        close(unidad)
        exito = .true.

    end function leer_archivo_lineas

end module io_utilities


program usar_io_utilities
    use io_utilities
    implicit none

    integer :: unidad, valor
    character(len=100) :: linea
    character(len=100), allocatable :: contenido(:)
    integer :: n_lineas, i
    logical :: ok

    ! Abrir archivo para lectura
    if (abrir_archivo("datos.txt", 'r', unidad)) then
        ! Leer línea por línea
        do
            if (leer_linea(unidad, linea) /= 0) exit
            print *, trim(linea)
        end do
        close(unidad)
    end if

    ! Leer archivo completo
    if (leer_archivo_lineas("datos.txt", contenido, n_lineas)) then
        print *, "Líneas leídas:", n_lineas
        do i = 1, n_lineas
            print *, trim(contenido(i))
        end do
        deallocate(contenido)
    end if

end program usar_io_utilities
```

---

## 6. Formatos Especiales

### 6.1 Formato de Tabla

```fortran
program formato_tabla
    implicit none

    ! ========================================
    ! CREAR TABLAS FORMATEADAS
    ! ========================================

    integer, parameter :: N = 5
    character(len=20) :: nombres(N)
    integer :: edades(N)
    real :: salarios(N)
    integer :: i

    ! Datos de ejemplo
    nombres = ["Ana García", "Juan López", "María Ruiz", &
               "Pedro Sánchez", "Laura Martín"]
    edades = [28, 35, 42, 31, 29]
    salarios = [45000.50, 52000.75, 61000.00, 48500.25, 47000.00]

    ! ========================================
    ! TABLA CON BORDES
    ! ========================================

    print '(A)', "+--------------------+------+--------------+"
    print '(A)', "| Nombre             | Edad | Salario      |"
    print '(A)', "+--------------------+------+--------------+"

    do i = 1, N
        print '(A, A18, A, I4, A, F12.2, A)', &
              "| ", nombres(i), " | ", edades(i), " | ", salarios(i), " |"
    end do

    print '(A)', "+--------------------+------+--------------+"

    ! ========================================
    ! TABLA CON TABULACIÓN
    ! ========================================

    print '(/A)', "=== Usando Tabulación ==="
    print '(A, T22, A, T28, A)', "Nombre", "Edad", "Salario"
    print '(A)', repeat("-", 45)

    do i = 1, N
        print '(A20, T22, I4, T28, F12.2)', &
              nombres(i), edades(i), salarios(i)
    end do

    ! ========================================
    ! TABLA JUSTIFICADA
    ! ========================================

    print '(/A)', "=== Justificación ==="

    ! Encabezado centrado (aproximado)
    print '(10X, A)', "REPORTE DE EMPLEADOS"
    print '(10X, A)', "===================="
    print *

    ! Columnas con ancho fijo
    print '(A20, A10, A15)', "NOMBRE", "EDAD", "SALARIO"
    print '(A)', repeat("=", 45)

    do i = 1, N
        print '(A20, I10, F15.2)', nombres(i), edades(i), salarios(i)
    end do

    ! ========================================
    ! TABLA NUMÉRICA CIENTÍFICA
    ! ========================================

    real :: datos(5, 3)
    integer :: j

    datos = reshape([&
        1.234e-5, 2.345e-4, 3.456e-3, 4.567e-2, 5.678e-1, &
        1.111e+1, 2.222e+2, 3.333e+3, 4.444e+4, 5.555e+5, &
        9.876e+6, 8.765e+7, 7.654e+8, 6.543e+9, 5.432e+10], [5, 3])

    print '(/A)', "=== Datos Científicos ==="
    print '(A5, 3A15)', "i", "Columna 1", "Columna 2", "Columna 3"
    print '(A)', repeat("-", 50)

    do i = 1, 5
        print '(I5, 3ES15.4)', i, (datos(i, j), j=1,3)
    end do

end program formato_tabla
```

### 6.2 Formatos para Datos CSV

```fortran
program formato_csv
    implicit none

    integer :: unidad, i
    character(len=100) :: linea
    integer :: iostat

    ! ========================================
    ! ESCRIBIR CSV
    ! ========================================

    integer, parameter :: N = 5
    character(len=20) :: nombres(N) = ["Ana", "Juan", "María", "Pedro", "Laura"]
    integer :: ids(N) = [101, 102, 103, 104, 105]
    real :: valores(N) = [1.5, 2.7, 3.2, 4.1, 5.9]

    open(newunit=unidad, file="datos.csv", status='REPLACE')

    ! Encabezado
    write(unidad, '(A)') "ID,Nombre,Valor"

    ! Datos
    do i = 1, N
        write(unidad, '(I0, ",", A, ",", F0.2)') &
              ids(i), trim(nombres(i)), valores(i)
    end do

    close(unidad)

    print *, "CSV escrito correctamente"

    ! ========================================
    ! LEER CSV
    ! ========================================

    character(len=20) :: nombre_leido
    integer :: id_leido
    real :: valor_leido
    character(len=100) :: campo1, campo2, campo3
    integer :: pos1, pos2

    open(newunit=unidad, file="datos.csv", status='OLD')

    ! Saltar encabezado
    read(unidad, '(A)') linea

    ! Leer datos
    do
        read(unidad, '(A)', iostat=iostat) linea
        if (iostat /= 0) exit

        ! Parsear manualmente (método simple)
        pos1 = index(linea, ',')
        campo1 = linea(1:pos1-1)
        linea = linea(pos1+1:)

        pos1 = index(linea, ',')
        campo2 = linea(1:pos1-1)
        campo3 = linea(pos1+1:)

        ! Convertir campos
        read(campo1, *) id_leido
        nombre_leido = trim(campo2)
        read(campo3, *) valor_leido

        print *, "ID:", id_leido, " Nombre:", trim(nombre_leido), &
                 " Valor:", valor_leido
    end do

    close(unidad)

    ! ========================================
    ! CSV CON COMILLAS (Valores con comas)
    ! ========================================

    character(len=50) :: texto_con_coma

    open(newunit=unidad, file="comillas.csv", status='REPLACE')

    ! Para valores que contienen comas, usar comillas
    texto_con_coma = '"García, Ana María"'
    write(unidad, '(I0, ",", A, ",", F0.2)') 1, trim(texto_con_coma), 100.5

    texto_con_coma = '"López, Juan Carlos"'
    write(unidad, '(I0, ",", A, ",", F0.2)') 2, trim(texto_con_coma), 200.3

    close(unidad)

end program formato_csv
```

### 6.3 Formato JSON Básico

```fortran
program formato_json
    implicit none

    integer :: unidad, i
    integer, parameter :: N = 3

    character(len=20) :: nombres(N) = ["Ana", "Juan", "María"]
    integer :: edades(N) = [25, 30, 28]
    real :: puntajes(N) = [85.5, 92.3, 88.7]

    ! ========================================
    ! ESCRIBIR JSON
    ! ========================================

    open(newunit=unidad, file="datos.json", status='REPLACE')

    write(unidad, '(A)') "{"
    write(unidad, '(A)') '  "empleados": ['

    do i = 1, N
        write(unidad, '(A)') "    {"
        write(unidad, '(A, A, A)') '      "nombre": "', trim(nombres(i)), '",'
        write(unidad, '(A, I0, A)') '      "edad": ', edades(i), ','
        write(unidad, '(A, F0.1)') '      "puntaje": ', puntajes(i)

        if (i < N) then
            write(unidad, '(A)') "    },"
        else
            write(unidad, '(A)') "    }"
        end if
    end do

    write(unidad, '(A)') "  ]"
    write(unidad, '(A)') "}"

    close(unidad)

    print *, "JSON escrito correctamente"

    ! ========================================
    ! NOTA SOBRE JSON
    ! ========================================
    !
    ! Para JSON complejo, es mejor usar una
    ! biblioteca especializada como json-fortran
    !
    ! Este ejemplo es solo para casos simples

end program formato_json
```

---

## 7. Buenas Prácticas

### 7.1 Patrones Recomendados

```fortran
module io_best_practices
    use, intrinsic :: iso_fortran_env, only: &
        input_unit, output_unit, error_unit, &
        iostat_end, iostat_eor
    implicit none

    private
    public :: leer_datos_seguro, escribir_reporte

contains

    ! ========================================
    ! LECTURA CON MANEJO COMPLETO DE ERRORES
    ! ========================================

    subroutine leer_datos_seguro(archivo, datos, n, exito)
        character(len=*), intent(in) :: archivo
        real, allocatable, intent(out) :: datos(:)
        integer, intent(out) :: n
        logical, intent(out) :: exito

        integer :: unidad, iostat, i
        character(len=256) :: iomsg
        real :: temp

        exito = .false.
        n = 0

        ! Abrir archivo
        open(newunit=unidad, file=archivo, status='OLD', &
             action='READ', iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) then
            write(error_unit, '(A)') "Error abriendo: " // trim(archivo)
            write(error_unit, '(A)') trim(iomsg)
            return
        end if

        ! Primera pasada: contar elementos
        do
            read(unidad, *, iostat=iostat) temp
            if (iostat == iostat_end) exit
            if (iostat > 0) then
                write(error_unit, '(A)') "Error leyendo datos"
                close(unidad)
                return
            end if
            n = n + 1
        end do

        ! Allocar array
        allocate(datos(n))

        ! Segunda pasada: leer datos
        rewind(unidad)
        do i = 1, n
            read(unidad, *, iostat=iostat) datos(i)
            if (iostat /= 0) then
                write(error_unit, '(A,I0)') "Error en línea ", i
                deallocate(datos)
                close(unidad)
                return
            end if
        end do

        close(unidad)
        exito = .true.

    end subroutine leer_datos_seguro

    ! ========================================
    ! ESCRITURA CON FORMATO PROFESIONAL
    ! ========================================

    subroutine escribir_reporte(archivo, titulo, datos, n, exito)
        character(len=*), intent(in) :: archivo, titulo
        real, intent(in) :: datos(:)
        integer, intent(in) :: n
        logical, intent(out) :: exito

        integer :: unidad, iostat, i
        character(len=256) :: iomsg
        character(len=8) :: fecha
        character(len=10) :: hora
        real :: suma, promedio, minimo, maximo

        exito = .false.

        ! Obtener fecha/hora
        call date_and_time(date=fecha, time=hora)

        ! Calcular estadísticas
        suma = sum(datos(1:n))
        promedio = suma / real(n)
        minimo = minval(datos(1:n))
        maximo = maxval(datos(1:n))

        ! Abrir archivo
        open(newunit=unidad, file=archivo, status='REPLACE', &
             action='WRITE', iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) then
            write(error_unit, '(A)') "Error creando: " // trim(archivo)
            write(error_unit, '(A)') trim(iomsg)
            return
        end if

        ! Escribir encabezado
        write(unidad, '(A)') repeat("=", 60)
        write(unidad, '(A)') titulo
        write(unidad, '(A)') repeat("=", 60)
        write(unidad, '(A,A,A,A,A,A)') "Fecha: ", fecha(1:4), "-", &
              fecha(5:6), "-", fecha(7:8)
        write(unidad, '(A,A,A,A,A,A)') "Hora:  ", hora(1:2), ":", &
              hora(3:4), ":", hora(5:6)
        write(unidad, '(A)') repeat("-", 60)
        write(unidad, *)

        ! Escribir estadísticas
        write(unidad, '(A)') "RESUMEN ESTADÍSTICO"
        write(unidad, '(A)') repeat("-", 30)
        write(unidad, '(A, I10)')   "  Cantidad:  ", n
        write(unidad, '(A, F15.4)') "  Suma:      ", suma
        write(unidad, '(A, F15.4)') "  Promedio:  ", promedio
        write(unidad, '(A, F15.4)') "  Mínimo:    ", minimo
        write(unidad, '(A, F15.4)') "  Máximo:    ", maximo
        write(unidad, *)

        ! Escribir datos
        write(unidad, '(A)') "DATOS"
        write(unidad, '(A)') repeat("-", 30)
        write(unidad, '(A10, A20)') "Índice", "Valor"
        write(unidad, '(A)') repeat("-", 30)

        do i = 1, n
            write(unidad, '(I10, F20.6)') i, datos(i)
        end do

        write(unidad, '(A)') repeat("=", 60)
        write(unidad, '(A)') "Fin del reporte"

        close(unidad)
        exito = .true.

    end subroutine escribir_reporte

end module io_best_practices
```

### 7.2 Resumen de Buenas Prácticas

```fortran
! ========================================
! RESUMEN DE BUENAS PRÁCTICAS DE I/O
! ========================================

! 1. SIEMPRE usar IOSTAT e IOMSG para detectar errores
!    open(newunit=u, file=f, iostat=ios, iomsg=msg)

! 2. Usar NEWUNIT en lugar de números fijos de unidad
!    open(newunit=unidad, file="archivo.txt")

! 3. Usar módulo iso_fortran_env para constantes estándar
!    use, intrinsic :: iso_fortran_env

! 4. Cerrar archivos cuando ya no se necesitan
!    close(unidad)

! 5. Usar STATUS apropiado en OPEN
!    'OLD' para leer existentes
!    'REPLACE' para crear/sobrescribir
!    'NEW' para crear solo si no existe

! 6. Usar ACTION para restringir acceso
!    action='READ' para solo lectura
!    action='WRITE' para solo escritura

! 7. Preferir formato explícito sobre formato libre
!    para datos críticos o de intercambio

! 8. Usar G0 (F2008) para salida compacta
!    print '(G0)', valor

! 9. Validar datos leídos
!    verificar rangos, tipos, formatos

! 10. Documentar formatos de archivo
!     comentarios en código y archivos README
```

---

## Referencia Rápida

### Descriptores de Formato

| Descriptor | Descripción | Ejemplo |
|------------|-------------|---------|
| `Iw` | Entero en w columnas | `I5` |
| `Iw.m` | Entero, mínimo m dígitos | `I5.3` |
| `Fw.d` | Real punto fijo | `F10.4` |
| `Ew.d` | Real exponencial | `E12.4` |
| `ESw.d` | Notación científica | `ES12.4` |
| `ENw.d` | Notación ingeniería | `EN12.4` |
| `Gw.d` | General (auto F/E) | `G12.4` |
| `Aw` | Caracteres en w columnas | `A20` |
| `Lw` | Lógico en w columnas | `L5` |
| `nX` | n espacios | `5X` |
| `Tn` | Tabulación a columna n | `T20` |
| `/` | Nueva línea | `/` |
| `:` | Terminar si no hay más datos | `:` |

### Parámetros de OPEN

| Parámetro | Valores | Descripción |
|-----------|---------|-------------|
| `STATUS` | OLD, NEW, REPLACE, SCRATCH | Estado del archivo |
| `ACTION` | READ, WRITE, READWRITE | Modo de acceso |
| `ACCESS` | SEQUENTIAL, DIRECT, STREAM | Tipo de acceso |
| `FORM` | FORMATTED, UNFORMATTED | Formato de datos |
| `POSITION` | REWIND, APPEND, ASIS | Posición inicial |
| `RECL` | entero | Longitud de registro |

### Valores de IOSTAT

| Valor | Significado |
|-------|-------------|
| `= 0` | Operación exitosa |
| `< 0` | Fin de archivo (EOF) |
| `> 0` | Error de I/O |
| `iostat_end` | Constante para EOF |
| `iostat_eor` | Constante para fin de registro |

---

## Conclusión

El sistema de I/O de Fortran, aunque puede parecer arcaico en su sintaxis, es extremadamente poderoso y flexible. Desde el simple `PRINT *` hasta el sofisticado I/O de stream, Fortran ofrece herramientas para manejar cualquier necesidad de entrada y salida en computación científica.

**Puntos clave:**
1. Usar NEWUNIT para evitar conflictos de unidades
2. Siempre verificar IOSTAT después de operaciones de I/O
3. Preferir formato explícito para datos científicos
4. Usar NAMELIST para archivos de configuración
5. Stream I/O (F2003) para máxima portabilidad binaria

---

*"El I/O es el puente entre el mundo abstracto de los algoritmos y la realidad tangible de los datos - en Fortran, ese puente ha sido forjado con precisión durante más de seis décadas."*

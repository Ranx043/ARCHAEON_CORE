# FORTRAN_02: ARRAYS Y MATRICES

> *"En Fortran, los arrays no son meros contenedores - son ciudadanos de primera clase, optimizados desde el núcleo del lenguaje para el cálculo científico."*

## Índice
1. [Declaración de Arrays](#1-declaración-de-arrays)
2. [Indexación y Secciones](#2-indexación-y-secciones)
3. [Operaciones con Arrays](#3-operaciones-con-arrays)
4. [Arrays Dinámicos](#4-arrays-dinámicos)
5. [Arrays en Procedimientos](#5-arrays-en-procedimientos)
6. [Funciones Intrínsecas de Arrays](#6-funciones-intrínsecas-de-arrays)
7. [WHERE y FORALL](#7-where-y-forall)
8. [Álgebra Lineal](#8-álgebra-lineal)

---

## 1. Declaración de Arrays

### 1.1 Arrays Estáticos

```fortran
program arrays_estaticos
    implicit none

    ! ========================================
    ! DECLARACIÓN CON DIMENSION
    ! ========================================

    ! Vectores (1D)
    real, dimension(10) :: vector1
    real :: vector2(10)                    ! Sintaxis alternativa
    integer, dimension(1:10) :: vector3    ! Límites explícitos
    integer, dimension(0:9) :: vector4     ! Base 0 (como C)
    integer, dimension(-5:5) :: vector5    ! Índices negativos

    ! Matrices (2D)
    real, dimension(3, 4) :: matriz1       ! 3 filas, 4 columnas
    real :: matriz2(3, 4)                  ! Sintaxis alternativa
    real, dimension(1:3, 1:4) :: matriz3   ! Límites explícitos

    ! Arrays multidimensionales (hasta 15 dimensiones en F2008)
    real, dimension(2, 3, 4) :: tensor3d
    real, dimension(2, 2, 2, 2) :: tensor4d

    ! ========================================
    ! INICIALIZACIÓN
    ! ========================================

    ! Constructor de array
    integer :: a(5) = [1, 2, 3, 4, 5]
    real :: b(3) = [1.0, 2.0, 3.0]

    ! DO implícito
    integer :: i
    integer :: secuencia(10) = [(i, i=1,10)]        ! [1,2,3,...,10]
    integer :: pares(5) = [(2*i, i=1,5)]            ! [2,4,6,8,10]
    real :: senos(4) = [(sin(real(i)), i=1,4)]

    ! Repetición
    integer :: ceros(100) = 0                       ! Todos ceros
    real :: unos(50) = 1.0

    ! Reshape para matrices
    integer :: mat(2,3) = reshape([1,2,3,4,5,6], [2,3])
    ! mat = | 1 3 5 |
    !       | 2 4 6 |
    ! (Fortran es column-major)

    ! DATA statement
    real :: datos(4)
    data datos /1.0, 2.0, 3.0, 4.0/

    ! DO implícito en DATA
    integer :: triangular(10)
    data (triangular(i), i=1,10) /1, 3, 6, 10, 15, 21, 28, 36, 45, 55/

end program arrays_estaticos
```

### 1.2 Column-Major Order

```fortran
program column_major
    implicit none

    ! ========================================
    ! FORTRAN USA COLUMN-MAJOR ORDER
    ! ========================================
    !
    ! La memoria se organiza por columnas:
    ! matriz(i,j) -> memoria[i + (j-1)*num_filas]
    !
    ! Para matriz(3,4):
    !
    ! Lógico:           En memoria:
    ! | 1 4 7 10 |      [1,2,3,4,5,6,7,8,9,10,11,12]
    ! | 2 5 8 11 |       ^col1^ ^col2^ ^col3^ ^col4^
    ! | 3 6 9 12 |

    integer :: mat(3, 4)
    integer :: i, j, k

    ! Llenar en orden de memoria (eficiente)
    k = 1
    do j = 1, 4           ! Columnas externas
        do i = 1, 3       ! Filas internas
            mat(i, j) = k
            k = k + 1
        end do
    end do

    ! Imprimir
    do i = 1, 3
        print '(4I4)', mat(i, :)
    end do

    ! ========================================
    ! IMPORTANCIA PARA RENDIMIENTO
    ! ========================================

    real :: grande(1000, 1000)
    real :: suma

    ! BIEN: acceso por columnas (acceso secuencial en memoria)
    suma = 0.0
    do j = 1, 1000
        do i = 1, 1000
            suma = suma + grande(i, j)
        end do
    end do

    ! MAL: acceso por filas (saltos en memoria, cache misses)
    suma = 0.0
    do i = 1, 1000
        do j = 1, 1000
            suma = suma + grande(i, j)  ! Ineficiente
        end do
    end do

end program column_major
```

---

## 2. Indexación y Secciones

### 2.1 Indexación Básica

```fortran
program indexacion
    implicit none

    integer :: vec(10)
    integer :: mat(4, 5)
    integer :: i, j

    ! Inicializar
    vec = [(i, i=1,10)]
    mat = reshape([(i, i=1,20)], [4, 5])

    ! ========================================
    ! ACCESO A ELEMENTOS
    ! ========================================

    print *, vec(1)       ! Primer elemento
    print *, vec(10)      ! Último elemento
    print *, mat(2, 3)    ! Fila 2, columna 3

    ! Modificación
    vec(5) = 100
    mat(1, 1) = 999

    ! ========================================
    ! LÍMITES DE ARRAY
    ! ========================================

    print *, "Límite inferior vec:", lbound(vec)      ! 1
    print *, "Límite superior vec:", ubound(vec)      ! 10
    print *, "Tamaño vec:", size(vec)                 ! 10

    print *, "Límites mat:", lbound(mat), ubound(mat)
    print *, "Shape mat:", shape(mat)                 ! [4, 5]
    print *, "Size mat:", size(mat)                   ! 20
    print *, "Filas mat:", size(mat, 1)               ! 4
    print *, "Columnas mat:", size(mat, 2)            ! 5

end program indexacion
```

### 2.2 Secciones de Arrays (Array Slicing)

```fortran
program secciones
    implicit none

    integer :: vec(10), mat(5, 5)
    integer :: i, j

    ! Inicializar
    vec = [(i, i=1,10)]
    mat = reshape([(i, i=1,25)], [5, 5])

    ! ========================================
    ! SECCIONES DE VECTORES
    ! ========================================

    ! array(inicio:fin:paso)

    print *, vec(1:5)         ! Primeros 5: [1,2,3,4,5]
    print *, vec(6:10)        ! Últimos 5: [6,7,8,9,10]
    print *, vec(1:10:2)      ! Impares: [1,3,5,7,9]
    print *, vec(2:10:2)      ! Pares: [2,4,6,8,10]
    print *, vec(10:1:-1)     ! Reverso: [10,9,8,...,1]
    print *, vec(:5)          ! Desde inicio hasta 5
    print *, vec(6:)          ! Desde 6 hasta fin
    print *, vec(:)           ! Todo el array
    print *, vec(::2)         ! Todo con paso 2

    ! ========================================
    ! SECCIONES DE MATRICES
    ! ========================================

    ! mat(filas, columnas)

    print *, mat(1, :)        ! Primera fila completa
    print *, mat(:, 1)        ! Primera columna completa
    print *, mat(2:4, 2:4)    ! Submatriz 3x3 central
    print *, mat(1:5:2, :)    ! Filas impares
    print *, mat(:, 1:5:2)    ! Columnas impares

    ! Diagonal (no directamente, usar do loop)
    print *, [(mat(i,i), i=1,5)]  ! Diagonal principal

    ! ========================================
    ! ASIGNACIÓN CON SECCIONES
    ! ========================================

    vec(1:5) = 0              ! Primeros 5 a cero
    vec(6:10) = [60, 70, 80, 90, 100]

    mat(1, :) = 999           ! Primera fila a 999
    mat(:, 5) = -1            ! Última columna a -1
    mat(2:4, 2:4) = 0         ! Submatriz central a cero

    ! Copiar sección a otra
    vec(1:5) = vec(6:10)      ! Copiar últimos 5 a primeros 5

end program secciones
```

### 2.3 Indexación con Vectores

```fortran
program indexacion_vectorial
    implicit none

    integer :: vec(10), resultado(4)
    integer :: indices(4)
    integer :: mat(5, 5), fila_indices(3), col_indices(3)

    vec = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

    ! ========================================
    ! VECTOR SUBSCRIPT
    ! ========================================

    indices = [1, 3, 5, 7]
    resultado = vec(indices)    ! [10, 30, 50, 70]

    ! Orden arbitrario
    indices = [10, 1, 5, 3]
    resultado = vec(indices)    ! [100, 10, 50, 30]

    ! Repetición de índices (solo para lectura)
    indices = [1, 1, 2, 2]
    resultado = vec(indices)    ! [10, 10, 20, 20]

    ! ========================================
    ! ASIGNACIÓN CON VECTOR SUBSCRIPT
    ! ========================================

    ! Los índices deben ser únicos para asignación
    indices = [2, 4, 6, 8]
    vec(indices) = 0            ! vec(2), vec(4), vec(6), vec(8) = 0

    ! ========================================
    ! MATRICES CON VECTOR SUBSCRIPT
    ! ========================================

    mat = reshape([(i, i=1,25)], [5, 5])

    fila_indices = [1, 3, 5]
    col_indices = [2, 4]

    ! mat(fila_indices, col_indices) da una submatriz 3x2

    ! ========================================
    ! MASK LÓGICO (alternativa)
    ! ========================================

    logical :: mask(10)

    vec = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
    mask = vec > 50

    ! pack extrae elementos donde mask es .true.
    print *, pack(vec, mask)    ! [60, 70, 80, 90, 100]

end program indexacion_vectorial
```

---

## 3. Operaciones con Arrays

### 3.1 Operaciones Elemento a Elemento

```fortran
program operaciones_elementales
    implicit none

    real :: a(5), b(5), c(5)
    real :: mat1(3,3), mat2(3,3), mat3(3,3)
    real :: escalar

    a = [1.0, 2.0, 3.0, 4.0, 5.0]
    b = [5.0, 4.0, 3.0, 2.0, 1.0]
    escalar = 10.0

    ! ========================================
    ! OPERACIONES ARITMÉTICAS
    ! ========================================

    c = a + b           ! [6, 6, 6, 6, 6]
    c = a - b           ! [-4, -2, 0, 2, 4]
    c = a * b           ! [5, 8, 9, 8, 5]  (elemento a elemento)
    c = a / b           ! [0.2, 0.5, 1, 2, 5]
    c = a ** 2          ! [1, 4, 9, 16, 25]

    ! Con escalar
    c = a + escalar     ! [11, 12, 13, 14, 15]
    c = escalar * a     ! [10, 20, 30, 40, 50]
    c = a / 2.0         ! [0.5, 1, 1.5, 2, 2.5]

    ! ========================================
    ! FUNCIONES ELEMENTALES
    ! ========================================

    c = sqrt(a)         ! Raíz de cada elemento
    c = sin(a)          ! Seno de cada elemento
    c = exp(a)          ! e^x de cada elemento
    c = log(a)          ! ln de cada elemento
    c = abs(a - b)      ! |a - b| elemento a elemento

    ! ========================================
    ! OPERACIONES CON MATRICES
    ! ========================================

    mat1 = reshape([1,2,3,4,5,6,7,8,9], [3,3])
    mat2 = reshape([9,8,7,6,5,4,3,2,1], [3,3])

    mat3 = mat1 + mat2         ! Suma elemento a elemento
    mat3 = mat1 * mat2         ! Producto elemento a elemento (NO matmul)
    mat3 = mat1 ** 2           ! Cuadrado elemento a elemento
    mat3 = sqrt(mat1)          ! Raíz elemento a elemento

    ! ========================================
    ! COMPARACIONES (retorna array lógico)
    ! ========================================

    logical :: mask(5)

    mask = a > 3.0             ! [F, F, F, T, T]
    mask = a == b              ! Comparación elemento a elemento
    mask = a >= 2.0 .and. a <= 4.0  ! [F, T, T, T, F]

end program operaciones_elementales
```

### 3.2 Producto Matricial

```fortran
program producto_matricial
    implicit none

    real :: A(2, 3), B(3, 4), C(2, 4)
    real :: vec1(3), vec2(3)
    real :: mat(3, 3), vec(3), resultado_vec(3)
    real :: dot

    ! Inicializar
    A = reshape([1,2,3,4,5,6], [2,3])
    B = reshape([1,2,3,4,5,6,7,8,9,10,11,12], [3,4])

    ! ========================================
    ! MATMUL - Producto matricial
    ! ========================================

    ! Matriz x Matriz
    C = matmul(A, B)    ! C(2,4) = A(2,3) x B(3,4)

    ! Matriz x Vector
    mat = reshape([1,0,0,0,2,0,0,0,3], [3,3])  ! Diagonal
    vec = [1.0, 2.0, 3.0]
    resultado_vec = matmul(mat, vec)  ! [1, 4, 9]

    ! Vector x Matriz
    resultado_vec = matmul(vec, mat)

    ! ========================================
    ! DOT_PRODUCT - Producto punto
    ! ========================================

    vec1 = [1.0, 2.0, 3.0]
    vec2 = [4.0, 5.0, 6.0]

    dot = dot_product(vec1, vec2)  ! 1*4 + 2*5 + 3*6 = 32

    ! Para complejos: dot_product(a, b) = sum(conjg(a) * b)

    ! ========================================
    ! TRANSPOSE - Transpuesta
    ! ========================================

    real :: original(2, 3), transpuesta(3, 2)

    original = reshape([1,2,3,4,5,6], [2,3])
    transpuesta = transpose(original)

    ! original:     transpuesta:
    ! | 1 3 5 |     | 1 2 |
    ! | 2 4 6 |     | 3 4 |
    !               | 5 6 |

end program producto_matricial
```

---

## 4. Arrays Dinámicos

### 4.1 ALLOCATABLE

```fortran
program arrays_allocatable
    implicit none

    real, allocatable :: vector(:)
    real, allocatable :: matriz(:,:)
    real, allocatable :: tensor(:,:,:)
    integer :: n, m, k, ierr

    print *, "Ingrese dimensión del vector:"
    read *, n

    ! ========================================
    ! ALLOCATE - Reservar memoria
    ! ========================================

    allocate(vector(n), stat=ierr)
    if (ierr /= 0) then
        print *, "Error de asignación"
        stop 1
    end if

    ! Inicializar
    vector = 0.0

    ! ========================================
    ! ALLOCATED - Verificar si está asignado
    ! ========================================

    if (allocated(vector)) then
        print *, "Vector asignado, tamaño:", size(vector)
    end if

    ! ========================================
    ! DEALLOCATE - Liberar memoria
    ! ========================================

    deallocate(vector, stat=ierr)

    ! ========================================
    ! MATRICES DINÁMICAS
    ! ========================================

    print *, "Ingrese filas y columnas:"
    read *, n, m

    allocate(matriz(n, m))
    matriz = 0.0

    ! Usar la matriz...

    deallocate(matriz)

    ! ========================================
    ! ALLOCATE CON LÍMITES ESPECÍFICOS
    ! ========================================

    allocate(vector(0:99))         ! Índices 0 a 99
    allocate(matriz(-5:5, -5:5))   ! Índices negativos

    ! ========================================
    ! ALLOCATE CON SOURCE (F2003)
    ! ========================================

    real, allocatable :: copia(:)
    real :: original(5) = [1, 2, 3, 4, 5]

    ! Copiar contenido y forma
    allocate(copia, source=original)
    ! copia ahora es [1, 2, 3, 4, 5]

    ! ========================================
    ! ALLOCATE CON MOLD (F2008)
    ! ========================================

    real, allocatable :: misma_forma(:)

    ! Solo copiar forma, no contenido
    allocate(misma_forma, mold=original)
    ! misma_forma tiene tamaño 5 pero valores indefinidos

end program arrays_allocatable
```

### 4.2 Reasignación Automática

```fortran
program reasignacion
    implicit none

    ! ========================================
    ! F2003+: Reasignación automática
    ! ========================================

    real, allocatable :: arr(:)
    integer :: i

    ! Sin allocate previo - se asigna automáticamente
    arr = [1.0, 2.0, 3.0]  ! arr ahora tiene tamaño 3

    print *, "Tamaño:", size(arr)

    ! Reasignar a diferente tamaño
    arr = [1.0, 2.0, 3.0, 4.0, 5.0]  ! Ahora tamaño 5

    print *, "Nuevo tamaño:", size(arr)

    ! ========================================
    ! CRECER ARRAY PRESERVANDO DATOS
    ! ========================================

    real, allocatable :: datos(:), temp(:)
    integer :: old_size, new_size

    allocate(datos(5))
    datos = [(real(i), i=1,5)]

    ! Crecer a 10 elementos
    old_size = size(datos)
    new_size = 10

    ! Mover a temporal
    call move_alloc(datos, temp)

    ! Nuevo array más grande
    allocate(datos(new_size))
    datos(1:old_size) = temp
    datos(old_size+1:new_size) = 0.0  ! Nuevos elementos

    deallocate(temp)

    ! ========================================
    ! F2008: MOVE_ALLOC más elegante
    ! ========================================

    ! move_alloc mueve la asignación sin copiar
    real, allocatable :: source(:), dest(:)

    allocate(source(100))
    source = 1.0

    ! Mueve source a dest, source queda no-asignado
    call move_alloc(source, dest)

    ! allocated(source) = .false.
    ! allocated(dest) = .true.

end program reasignacion
```

### 4.3 Arrays de Tamaño Diferido en Tipos

```fortran
module tipos_con_arrays
    implicit none

    ! ========================================
    ! TIPO CON ARRAYS ALLOCATABLE (F2003)
    ! ========================================

    type :: vector_dinamico
        real, allocatable :: datos(:)
        integer :: longitud
    end type vector_dinamico

    type :: matriz_dinamica
        real, allocatable :: elementos(:,:)
        integer :: filas, columnas
    end type matriz_dinamica

contains

    subroutine crear_vector(v, n)
        type(vector_dinamico), intent(out) :: v
        integer, intent(in) :: n

        allocate(v%datos(n))
        v%datos = 0.0
        v%longitud = n
    end subroutine

    subroutine destruir_vector(v)
        type(vector_dinamico), intent(inout) :: v

        if (allocated(v%datos)) deallocate(v%datos)
        v%longitud = 0
    end subroutine

    subroutine crear_matriz(m, filas, cols)
        type(matriz_dinamica), intent(out) :: m
        integer, intent(in) :: filas, cols

        allocate(m%elementos(filas, cols))
        m%elementos = 0.0
        m%filas = filas
        m%columnas = cols
    end subroutine

end module tipos_con_arrays


program usar_tipos
    use tipos_con_arrays
    implicit none

    type(vector_dinamico) :: v
    type(matriz_dinamica) :: mat

    call crear_vector(v, 100)
    v%datos(1) = 1.0

    call crear_matriz(mat, 10, 20)
    mat%elementos(5, 10) = 999.0

    call destruir_vector(v)

end program usar_tipos
```

---

## 5. Arrays en Procedimientos

### 5.1 Arrays de Forma Explícita

```fortran
module arrays_procedimientos
    implicit none

contains

    ! ========================================
    ! FORMA EXPLÍCITA - tamaño conocido
    ! ========================================

    subroutine procesar_vector_fijo(vec)
        real, intent(inout) :: vec(100)  ! Exactamente 100 elementos
        integer :: i

        do i = 1, 100
            vec(i) = vec(i) * 2.0
        end do
    end subroutine

    ! Con límites específicos
    subroutine procesar_matriz_fija(mat)
        real, intent(inout) :: mat(3, 3)
        mat = mat + 1.0
    end subroutine

    ! ========================================
    ! FORMA ASUMIDA (Assumed-Size) - Legacy
    ! ========================================

    ! El * indica tamaño desconocido de última dimensión
    subroutine sumar_vector_legacy(vec, n, suma)
        integer, intent(in) :: n
        real, intent(in) :: vec(*)     ! Tamaño asumido
        real, intent(out) :: suma
        integer :: i

        suma = 0.0
        do i = 1, n
            suma = suma + vec(i)
        end do
        ! No se puede usar size(), shape() con arrays (*)
    end subroutine

end module arrays_procedimientos
```

### 5.2 Arrays de Forma Asumida

```fortran
module arrays_forma_asumida
    implicit none

contains

    ! ========================================
    ! ASSUMED-SHAPE - Forma moderna (F90+)
    ! ========================================

    ! El : indica cualquier tamaño, se preserva la forma
    subroutine procesar_vector(vec)
        real, intent(inout) :: vec(:)  ! Cualquier tamaño
        integer :: n

        n = size(vec)  ! Podemos obtener el tamaño
        vec = vec * 2.0
    end subroutine

    function suma_array(arr) result(s)
        real, intent(in) :: arr(:)
        real :: s

        s = sum(arr)  ! Funciones intrínsecas funcionan
    end function

    subroutine procesar_matriz(mat)
        real, intent(inout) :: mat(:,:)
        integer :: filas, cols

        filas = size(mat, 1)
        cols = size(mat, 2)
        print *, "Matriz de", filas, "x", cols
    end subroutine

    ! Multidimensional
    subroutine procesar_3d(tensor)
        real, intent(inout) :: tensor(:,:,:)
        print *, "Forma:", shape(tensor)
    end subroutine

    ! ========================================
    ! CON LÍMITES NO ESTÁNDAR
    ! ========================================

    subroutine con_limites(arr)
        real, intent(in) :: arr(0:)  ! Empieza en 0
        integer :: i

        do i = 0, ubound(arr, 1)
            print *, arr(i)
        end do
    end subroutine

end module arrays_forma_asumida
```

### 5.3 Arrays Automáticos

```fortran
module arrays_automaticos
    implicit none

contains

    ! ========================================
    ! ARRAYS AUTOMÁTICOS (en stack)
    ! ========================================

    subroutine con_array_automatico(n)
        integer, intent(in) :: n
        real :: trabajo(n)      ! Tamaño determinado en tiempo de ejecución
        real :: matriz(n, n)    ! Array automático 2D

        trabajo = 0.0
        matriz = 0.0

        ! Se liberan automáticamente al salir
    end subroutine

    function crear_copia(original) result(copia)
        real, intent(in) :: original(:)
        real :: copia(size(original))  ! Mismo tamaño que original

        copia = original
    end function

    subroutine multiplicar_matrices(A, B, C)
        real, intent(in) :: A(:,:), B(:,:)
        real, intent(out) :: C(:,:)
        real :: temp(size(A,1), size(B,2))  ! Array automático de trabajo

        ! Usar temp para cálculos intermedios...
        C = matmul(A, B)
    end subroutine

end module arrays_automaticos
```

### 5.4 Punteros a Arrays

```fortran
program punteros_arrays
    implicit none

    real, target :: datos(100)
    real, pointer :: ptr(:)
    real, pointer :: seccion(:)
    real, pointer :: ptr_2d(:,:)
    real, target :: matriz(10, 10)

    ! Inicializar
    datos = [(real(i), i=1,100)]
    matriz = reshape([(real(i), i=1,100)], [10, 10])

    ! ========================================
    ! PUNTERO A ARRAY COMPLETO
    ! ========================================

    ptr => datos        ! ptr apunta a datos
    print *, ptr(50)    ! Acceso a través del puntero

    ! ========================================
    ! PUNTERO A SECCIÓN
    ! ========================================

    seccion => datos(1:10)    ! Apunta a los primeros 10
    print *, size(seccion)    ! 10

    seccion => datos(1:100:2) ! Apunta a elementos impares
    print *, size(seccion)    ! 50

    ! ========================================
    ! REASIGNAR PUNTERO
    ! ========================================

    ptr => datos(50:100)      ! Ahora apunta a últimos 51

    ! ========================================
    ! PUNTEROS A MATRICES
    ! ========================================

    ptr_2d => matriz          ! Toda la matriz
    ptr_2d => matriz(1:5, 1:5)  ! Submatriz

    ! ========================================
    ! ALLOCATE CON PUNTEROS
    ! ========================================

    real, pointer :: dinamico(:)

    allocate(dinamico(100))
    dinamico = 0.0

    ! ...usar...

    deallocate(dinamico)
    nullify(dinamico)         ! Buena práctica

    ! ========================================
    ! VERIFICAR ASOCIACIÓN
    ! ========================================

    if (associated(ptr)) then
        print *, "ptr está asociado"
    end if

    if (associated(ptr, datos)) then
        print *, "ptr apunta a datos"
    end if

end program punteros_arrays
```

---

## 6. Funciones Intrínsecas de Arrays

### 6.1 Funciones de Reducción

```fortran
program funciones_reduccion
    implicit none

    real :: vec(10) = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    real :: mat(3, 4)
    logical :: mask(10)

    mat = reshape([(real(i), i=1,12)], [3,4])
    mask = vec > 5

    ! ========================================
    ! SUMA, PRODUCTO
    ! ========================================

    print *, "Suma:", sum(vec)              ! 55
    print *, "Producto:", product(vec)      ! 3628800

    ! Con máscara
    print *, "Suma >5:", sum(vec, mask=mask)  ! 40 (6+7+8+9+10)

    ! Por dimensión (matriz)
    print *, "Suma por columnas:", sum(mat, dim=1)  ! [6, 15, 24, 33]
    print *, "Suma por filas:", sum(mat, dim=2)     ! [22, 26, 30]

    ! ========================================
    ! MÁXIMO, MÍNIMO
    ! ========================================

    print *, "Máximo:", maxval(vec)         ! 10
    print *, "Mínimo:", minval(vec)         ! 1

    ! Ubicación
    print *, "Posición max:", maxloc(vec)   ! [10]
    print *, "Posición min:", minloc(vec)   ! [1]

    ! En matriz
    print *, "Max por columna:", maxval(mat, dim=1)
    print *, "Ubicación max:", maxloc(mat)  ! [fila, columna]

    ! ========================================
    ! ALL, ANY, COUNT
    ! ========================================

    print *, "Todos >0:", all(vec > 0)      ! .true.
    print *, "Alguno >5:", any(vec > 5)     ! .true.
    print *, "Cuántos >5:", count(vec > 5)  ! 5

    ! Por dimensión
    logical :: mat_log(3, 3)
    mat_log = reshape([.true.,.false.,.true., &
                       .true.,.true.,.false., &
                       .true.,.true.,.true.], [3,3])

    print *, "All por columna:", all(mat_log, dim=1)
    print *, "Any por fila:", any(mat_log, dim=2)

    ! ========================================
    ! DOT_PRODUCT
    ! ========================================

    real :: a(5) = [1, 2, 3, 4, 5]
    real :: b(5) = [5, 4, 3, 2, 1]

    print *, "Producto punto:", dot_product(a, b)  ! 35

end program funciones_reduccion
```

### 6.2 Funciones de Consulta

```fortran
program funciones_consulta
    implicit none

    real :: arr(10)
    real :: mat(-2:2, 0:3)
    real, allocatable :: din(:,:)

    arr = 0.0

    ! ========================================
    ! FORMA Y TAMAÑO
    ! ========================================

    print *, "Size arr:", size(arr)         ! 10
    print *, "Shape arr:", shape(arr)       ! [10]
    print *, "Rank arr:", rank(arr)         ! 1 (F2008)

    print *, "Size mat:", size(mat)         ! 20
    print *, "Size mat dim 1:", size(mat, 1)  ! 5
    print *, "Size mat dim 2:", size(mat, 2)  ! 4
    print *, "Shape mat:", shape(mat)       ! [5, 4]

    ! ========================================
    ! LÍMITES
    ! ========================================

    print *, "Lbound mat:", lbound(mat)     ! [-2, 0]
    print *, "Ubound mat:", ubound(mat)     ! [2, 3]
    print *, "Lbound mat dim 1:", lbound(mat, 1)  ! -2

    ! ========================================
    ! ALLOCATED
    ! ========================================

    print *, "Din asignado:", allocated(din)  ! .false.
    allocate(din(5, 5))
    print *, "Din asignado:", allocated(din)  ! .true.

end program funciones_consulta
```

### 6.3 Funciones de Manipulación

```fortran
program funciones_manipulacion
    implicit none

    integer :: vec(5) = [1, 2, 3, 4, 5]
    integer :: mat(3, 3)
    integer :: resultado(5)
    integer :: i

    mat = reshape([(i, i=1,9)], [3,3])

    ! ========================================
    ! RESHAPE
    ! ========================================

    integer :: lineal(12) = [(i, i=1,12)]
    integer :: mat3x4(3, 4)
    integer :: mat4x3(4, 3)

    mat3x4 = reshape(lineal, [3, 4])
    mat4x3 = reshape(lineal, [4, 3])

    ! Con PAD (rellenar si fuente es pequeña)
    integer :: pequeño(4) = [1, 2, 3, 4]
    integer :: grande(6)
    grande = reshape(pequeño, [6], pad=[0, 0])  ! [1,2,3,4,0,0]

    ! Con ORDER (cambiar orden de llenado)
    mat3x4 = reshape(lineal, [3, 4], order=[2, 1])  ! Por filas

    ! ========================================
    ! TRANSPOSE
    ! ========================================

    integer :: t_mat(3, 3)
    t_mat = transpose(mat)

    ! ========================================
    ! SPREAD - Replicar array en nueva dimensión
    ! ========================================

    integer :: fila(4) = [1, 2, 3, 4]
    integer :: replicado(3, 4)

    ! spread(source, dim, ncopies)
    replicado = spread(fila, 1, 3)  ! Replicar fila 3 veces
    ! Resultado: | 1 2 3 4 |
    !            | 1 2 3 4 |
    !            | 1 2 3 4 |

    ! ========================================
    ! MERGE - Seleccionar según máscara
    ! ========================================

    integer :: a(5) = [1, 2, 3, 4, 5]
    integer :: b(5) = [10, 20, 30, 40, 50]
    logical :: m(5) = [.true., .false., .true., .false., .true.]

    resultado = merge(a, b, m)  ! [1, 20, 3, 40, 5]

    ! ========================================
    ! PACK / UNPACK
    ! ========================================

    real :: datos(10) = [(real(i), i=1,10)]
    logical :: mask(10)
    real :: empaquetado(5)
    real :: desempaquetado(10)

    mask = datos > 5.0
    empaquetado = pack(datos, mask)  ! [6, 7, 8, 9, 10]

    desempaquetado = unpack(empaquetado, mask, 0.0)
    ! [0, 0, 0, 0, 0, 6, 7, 8, 9, 10]

    ! ========================================
    ! CSHIFT - Desplazamiento circular
    ! ========================================

    resultado = cshift(vec, 2)      ! [3, 4, 5, 1, 2]
    resultado = cshift(vec, -1)     ! [5, 1, 2, 3, 4]

    ! En matrices por dimensión
    integer :: m2(3, 3), r2(3, 3)
    m2 = reshape([1,2,3,4,5,6,7,8,9], [3,3])
    r2 = cshift(m2, 1, dim=1)       ! Shift filas hacia arriba
    r2 = cshift(m2, 1, dim=2)       ! Shift columnas a la izquierda

    ! ========================================
    ! EOSHIFT - Desplazamiento con relleno
    ! ========================================

    resultado = eoshift(vec, 2)           ! [3, 4, 5, 0, 0]
    resultado = eoshift(vec, 2, boundary=99)  ! [3, 4, 5, 99, 99]
    resultado = eoshift(vec, -2)          ! [0, 0, 1, 2, 3]

end program funciones_manipulacion
```

---

## 7. WHERE y FORALL

### 7.1 WHERE - Operaciones Condicionales

```fortran
program where_ejemplo
    implicit none

    real :: a(10), b(10), c(10)
    integer :: i

    a = [(real(i), i=1,10)]
    b = [(real(11-i), i=1,10)]

    ! ========================================
    ! WHERE SIMPLE
    ! ========================================

    where (a > 5.0)
        c = a * 2.0    ! Solo donde a > 5
    end where

    ! ========================================
    ! WHERE-ELSEWHERE
    ! ========================================

    where (a > 5.0)
        c = a * 2.0
    elsewhere
        c = a * 0.5
    end where

    ! ========================================
    ! WHERE MÚLTIPLE ELSEWHERE
    ! ========================================

    where (a < 3.0)
        c = 0.0
    elsewhere (a < 7.0)
        c = a
    elsewhere
        c = a * 2.0
    end where

    ! ========================================
    ! WHERE EN UNA LÍNEA
    ! ========================================

    where (a > 5.0) c = a ** 2

    ! ========================================
    ! WHERE CON MÁSCARAS COMPLEJAS
    ! ========================================

    real :: matriz(10, 10)
    matriz = 0.0

    where (matriz == 0.0 .and. a > 0.0)
        ! Condición con múltiples arrays
    end where

    ! ========================================
    ! ANIDADO (F95+)
    ! ========================================

    where (a > 0.0)
        where (b > 0.0)
            c = sqrt(a * b)
        elsewhere
            c = sqrt(a)
        end where
    end where

end program where_ejemplo
```

### 7.2 FORALL

```fortran
program forall_ejemplo
    implicit none

    real :: a(10, 10), b(10, 10)
    integer :: i, j

    ! ========================================
    ! FORALL - Asignación paralela
    ! ========================================

    ! Todas las asignaciones son conceptualmente simultáneas
    forall (i = 1:10)
        a(i, i) = 1.0  ! Diagonal
    end forall

    ! ========================================
    ! FORALL CON MÚLTIPLES ÍNDICES
    ! ========================================

    forall (i = 1:10, j = 1:10)
        a(i, j) = real(i + j)
    end forall

    ! ========================================
    ! FORALL CON MÁSCARA
    ! ========================================

    forall (i = 1:10, j = 1:10, i /= j)
        a(i, j) = 0.0  ! Todo excepto diagonal
    end forall

    ! ========================================
    ! FORALL EN UNA LÍNEA
    ! ========================================

    forall (i = 1:10) a(i, i) = real(i)

    ! ========================================
    ! FORALL CON WHERE
    ! ========================================

    forall (i = 1:10, j = 1:10)
        where (b > 0.0)
            a(i, j) = b(i, j)
        end where
    end forall

    ! ========================================
    ! DO CONCURRENT (F2008) - Preferido
    ! ========================================

    ! Más flexible y optimizable que FORALL
    do concurrent (i = 1:10, j = 1:10)
        a(i, j) = real(i * j)
    end do

    ! Con máscara
    do concurrent (i = 1:10, j = 1:10, i > j)
        a(i, j) = 0.0  ! Triangular inferior
    end do

end program forall_ejemplo
```

---

## 8. Álgebra Lineal

### 8.1 Operaciones Básicas

```fortran
program algebra_lineal
    implicit none

    real :: A(3, 3), B(3, 3), C(3, 3)
    real :: x(3), y(3), z(3)
    real :: escalar
    integer :: i

    ! Inicializar
    A = reshape([1,2,3,4,5,6,7,8,9], [3,3])
    B = reshape([9,8,7,6,5,4,3,2,1], [3,3])
    x = [1.0, 2.0, 3.0]
    y = [4.0, 5.0, 6.0]

    ! ========================================
    ! OPERACIONES VECTORIALES
    ! ========================================

    ! Suma de vectores
    z = x + y

    ! Producto escalar (dot product)
    escalar = dot_product(x, y)

    ! Norma (magnitud)
    escalar = sqrt(dot_product(x, x))  ! ||x||

    ! Producto cruz (3D) - no hay función intrínseca
    z(1) = x(2)*y(3) - x(3)*y(2)
    z(2) = x(3)*y(1) - x(1)*y(3)
    z(3) = x(1)*y(2) - x(2)*y(1)

    ! ========================================
    ! OPERACIONES MATRICIALES
    ! ========================================

    ! Suma de matrices
    C = A + B

    ! Producto elemento a elemento
    C = A * B

    ! Producto matricial
    C = matmul(A, B)

    ! Transpuesta
    C = transpose(A)

    ! Producto matriz-vector
    z = matmul(A, x)

    ! Traza (suma de diagonal)
    escalar = sum([(A(i,i), i=1,3)])

    ! ========================================
    ! MATRICES ESPECIALES
    ! ========================================

    real :: identidad(3, 3)
    real :: diagonal(3)

    ! Matriz identidad
    identidad = 0.0
    forall (i = 1:3) identidad(i, i) = 1.0

    ! Extraer diagonal
    diagonal = [(A(i,i), i=1,3)]

    ! Crear matriz diagonal
    real :: D(3, 3)
    D = 0.0
    forall (i = 1:3) D(i, i) = diagonal(i)

end program algebra_lineal
```

### 8.2 Usando BLAS/LAPACK

```fortran
program usar_lapack
    implicit none

    ! ========================================
    ! EJEMPLO CON LAPACK
    ! ========================================
    !
    ! Compilar con: gfortran programa.f90 -llapack -lblas
    !
    ! LAPACK provee:
    ! - Sistemas lineales (gesv, posv)
    ! - Eigenvalores (geev, syev)
    ! - Descomposiciones (getrf, potrf, gesvd)
    ! - Mínimos cuadrados (gels)

    integer, parameter :: n = 3
    real :: A(n, n), b(n), x(n)
    integer :: ipiv(n), info

    ! Sistema Ax = b
    A = reshape([3.0, 2.0, -1.0, &
                 2.0, -2.0, 4.0, &
                 -1.0, 0.5, -1.0], [3, 3])

    b = [1.0, -2.0, 0.0]
    x = b  ! LAPACK sobrescribe b con solución

    ! Interface a LAPACK (declaración externa)
    external :: sgesv

    ! Resolver sistema
    call sgesv(n, 1, A, n, ipiv, x, n, info)

    if (info == 0) then
        print *, "Solución:", x
    else
        print *, "Error LAPACK:", info
    end if

    ! ========================================
    ! EIGENVALORES CON LAPACK
    ! ========================================

    real :: matriz(3, 3), eigenvalores(3)
    real :: trabajo(100)
    integer :: lwork

    matriz = reshape([1.0, 2.0, 0.0, &
                      2.0, 1.0, 2.0, &
                      0.0, 2.0, 1.0], [3, 3])

    lwork = 100
    external :: ssyev

    ! Eigenvalores de matriz simétrica
    call ssyev('N', 'U', 3, matriz, 3, eigenvalores, trabajo, lwork, info)

    if (info == 0) then
        print *, "Eigenvalores:", eigenvalores
    end if

end program usar_lapack
```

---

## Referencia Rápida

### Declaración de Arrays

```fortran
real :: vec(n)              ! Tamaño fijo
real :: mat(m, n)           ! Matriz
real :: arr(0:n-1)          ! Base 0
real, allocatable :: d(:)   ! Dinámico
real, pointer :: p(:)       ! Puntero
```

### Funciones de Arrays

| Función | Descripción |
|---------|-------------|
| `size(a[,dim])` | Tamaño total o por dimensión |
| `shape(a)` | Forma como array |
| `lbound(a)`, `ubound(a)` | Límites |
| `sum(a)`, `product(a)` | Reducción |
| `maxval(a)`, `minval(a)` | Máx/Mín |
| `maxloc(a)`, `minloc(a)` | Ubicación de máx/mín |
| `matmul(a,b)` | Producto matricial |
| `dot_product(a,b)` | Producto punto |
| `transpose(a)` | Transpuesta |
| `reshape(a,shape)` | Cambiar forma |
| `pack(a,mask)` | Empaquetar |
| `spread(a,dim,n)` | Replicar |

### Secciones

```fortran
a(i)        ! Elemento
a(i:j)      ! Rango
a(i:j:k)    ! Rango con paso
a(:)        ! Todo
a(::2)      ! Cada 2
a(n:1:-1)   ! Reverso
```

---

## Conclusión

Los arrays en Fortran son herramientas de primera clase, diseñados desde el inicio del lenguaje para el cálculo numérico eficiente. Con operaciones elemento a elemento, secciones flexibles, y funciones intrínsecas poderosas, Fortran sigue siendo el estándar de oro para la computación científica.

**Puntos clave:**
1. Column-major order - optimizar acceso por columnas
2. Usar assumed-shape (`:`) en procedimientos modernos
3. ALLOCATABLE preferido sobre POINTER cuando sea posible
4. DO CONCURRENT preferido sobre FORALL para paralelismo
5. Integrar con BLAS/LAPACK para álgebra lineal de alto rendimiento

---

*"En Fortran, un array no es solo datos - es una promesa de eficiencia numérica cumplida desde hace más de seis décadas."*

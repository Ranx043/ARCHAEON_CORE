# FORTRAN_05: FORTRAN MODERNO (F90-F2018)

> *"El viejo guerrero no muere - evoluciona. Fortran 90 no abandonó su esencia numérica, la refinó con los paradigmas del nuevo milenio: módulos, objetos, y el paralelismo que las supercomputadoras demandaban."*

## Indice

1. [Modulos](#1-modulos)
2. [Programacion Orientada a Objetos](#2-programacion-orientada-a-objetos-f2003)
3. [Interoperabilidad con C](#3-interoperabilidad-con-c-f2003)
4. [Paralelismo](#4-paralelismo)
5. [Caracteristicas Modernas](#5-caracteristicas-modernas)
6. [Manejo de Excepciones IEEE](#6-manejo-de-excepciones-ieee)
7. [Referencia Rapida](#7-referencia-rapida)

---

## 1. Modulos

> *"Los modulos son las catedrales del Fortran moderno - reemplazan el caos de COMMON con la estructura de la encapsulacion."*

### 1.1 Estructura Basica de Modulos

```fortran
! ========================================
! MODULO BASICO - Fortran 90+
! ========================================

module matematicas_basicas
    implicit none

    ! ----------------------------------------
    ! SECCION DE ESPECIFICACION
    ! Variables y constantes del modulo
    ! ----------------------------------------

    ! Constantes publicas
    integer, parameter :: dp = selected_real_kind(15, 307)
    real(dp), parameter :: PI = 3.14159265358979323846_dp
    real(dp), parameter :: E  = 2.71828182845904523536_dp

    ! Variables de modulo (estado compartido)
    integer, save :: contador_operaciones = 0

contains

    ! ----------------------------------------
    ! SECCION DE IMPLEMENTACION
    ! Procedimientos del modulo
    ! ----------------------------------------

    pure function area_circulo(radio) result(area)
        real(dp), intent(in) :: radio
        real(dp) :: area

        area = PI * radio**2
    end function area_circulo

    pure function volumen_esfera(radio) result(vol)
        real(dp), intent(in) :: radio
        real(dp) :: vol

        vol = (4.0_dp / 3.0_dp) * PI * radio**3
    end function volumen_esfera

    subroutine incrementar_contador()
        contador_operaciones = contador_operaciones + 1
    end subroutine incrementar_contador

end module matematicas_basicas


! ========================================
! USO DEL MODULO
! ========================================

program usar_modulo
    use matematicas_basicas  ! Importa TODO el modulo
    implicit none

    real(dp) :: r, a

    r = 5.0_dp
    a = area_circulo(r)

    print '(A,F10.4)', "Area del circulo: ", a
    print '(A,F10.4)', "Volumen esfera: ", volumen_esfera(r)

end program usar_modulo
```

### 1.2 PUBLIC y PRIVATE

```fortran
! ========================================
! CONTROL DE VISIBILIDAD
! ========================================

module algebra_lineal
    implicit none

    ! Por defecto todo es PRIVATE
    private

    ! Exportar solo lo necesario
    public :: dp, vector, crear_vector, norma, producto_punto
    public :: operator(.dot.)

    ! Precision de trabajo
    integer, parameter :: dp = selected_real_kind(15, 307)

    ! Tipo publico
    type :: vector
        real(dp), allocatable :: componentes(:)
        integer :: dimension
    end type vector

    ! Interfaz para operador personalizado
    interface operator(.dot.)
        module procedure producto_punto
    end interface

    ! Variables privadas (solo uso interno)
    real(dp), save :: tolerancia = 1.0e-12_dp

contains

    ! ----------------------------------------
    ! PROCEDIMIENTOS PUBLICOS
    ! ----------------------------------------

    function crear_vector(n, valor_inicial) result(v)
        integer, intent(in) :: n
        real(dp), intent(in), optional :: valor_inicial
        type(vector) :: v

        v%dimension = n
        allocate(v%componentes(n))

        if (present(valor_inicial)) then
            v%componentes = valor_inicial
        else
            v%componentes = 0.0_dp
        end if
    end function crear_vector

    pure function norma(v) result(n)
        type(vector), intent(in) :: v
        real(dp) :: n

        n = sqrt(sum(v%componentes**2))
    end function norma

    pure function producto_punto(v1, v2) result(res)
        type(vector), intent(in) :: v1, v2
        real(dp) :: res

        ! Verificacion interna usando procedimiento privado
        res = sum(v1%componentes * v2%componentes)
    end function producto_punto

    ! ----------------------------------------
    ! PROCEDIMIENTOS PRIVADOS (solo uso interno)
    ! ----------------------------------------

    pure function es_cercano_cero(x) result(es_cero)
        real(dp), intent(in) :: x
        logical :: es_cero

        es_cero = abs(x) < tolerancia
    end function es_cercano_cero

end module algebra_lineal


! ========================================
! USO CON IMPORTACION SELECTIVA
! ========================================

program usar_algebra
    ! Importar solo lo necesario
    use algebra_lineal, only: dp, vector, crear_vector, norma
    use algebra_lineal, only: operator(.dot.)

    implicit none

    type(vector) :: v1, v2
    real(dp) :: producto

    v1 = crear_vector(3, 1.0_dp)
    v2 = crear_vector(3, 2.0_dp)

    ! Usar operador personalizado
    producto = v1 .dot. v2

    print '(A,F10.4)', "Norma v1: ", norma(v1)
    print '(A,F10.4)', "Producto punto: ", producto

end program usar_algebra
```

### 1.3 Submodules (F2008)

```fortran
! ========================================
! MODULO PADRE - Define interfaces
! ========================================

module fisica_modulo
    implicit none
    private

    integer, parameter, public :: dp = selected_real_kind(15, 307)

    ! Interfaces de procedimientos (sin implementacion)
    interface
        module function calcular_energia_cinetica(masa, velocidad) result(ec)
            real(dp), intent(in) :: masa, velocidad
            real(dp) :: ec
        end function calcular_energia_cinetica

        module function calcular_energia_potencial(masa, altura) result(ep)
            real(dp), intent(in) :: masa, altura
            real(dp) :: ep
        end function calcular_energia_potencial

        module subroutine simular_movimiento(x0, v0, t, x, v)
            real(dp), intent(in) :: x0, v0, t
            real(dp), intent(out) :: x, v
        end subroutine simular_movimiento
    end interface

    public :: calcular_energia_cinetica, calcular_energia_potencial
    public :: simular_movimiento

end module fisica_modulo


! ========================================
! SUBMODULO - Implementacion separada
! ========================================

submodule (fisica_modulo) fisica_implementacion
    implicit none

    ! Constantes locales al submodulo
    real(dp), parameter :: G = 9.81_dp  ! Gravedad

contains

    ! Implementacion de la funcion de energia cinetica
    module procedure calcular_energia_cinetica
        ec = 0.5_dp * masa * velocidad**2
    end procedure calcular_energia_cinetica

    ! Implementacion de energia potencial
    module procedure calcular_energia_potencial
        ep = masa * G * altura
    end procedure calcular_energia_potencial

    ! Implementacion de simulacion
    module procedure simular_movimiento
        ! Movimiento uniformemente acelerado
        x = x0 + v0*t - 0.5_dp*G*t**2
        v = v0 - G*t
    end procedure simular_movimiento

end submodule fisica_implementacion


! ========================================
! SUBMODULO ANIDADO (F2008)
! ========================================

submodule (fisica_modulo:fisica_implementacion) fisica_avanzada
    ! Extiende el submodulo con funcionalidad adicional
    implicit none

contains

    ! Procedimientos adicionales pueden agregarse aqui

end submodule fisica_avanzada


! ========================================
! USO - El usuario no ve la separacion
! ========================================

program usar_fisica
    use fisica_modulo
    implicit none

    real(dp) :: masa, vel, alt, ec, ep
    real(dp) :: x0, v0, t, x, v

    masa = 10.0_dp   ! kg
    vel = 5.0_dp     ! m/s
    alt = 20.0_dp    ! m

    ec = calcular_energia_cinetica(masa, vel)
    ep = calcular_energia_potencial(masa, alt)

    print '(A,F10.2,A)', "Energia cinetica: ", ec, " J"
    print '(A,F10.2,A)', "Energia potencial: ", ep, " J"

    ! Simular caida libre
    x0 = 100.0_dp    ! metros
    v0 = 0.0_dp      ! velocidad inicial
    t = 2.0_dp       ! segundos

    call simular_movimiento(x0, v0, t, x, v)
    print '(A,F10.2,A)', "Posicion en t=2s: ", x, " m"
    print '(A,F10.2,A)', "Velocidad en t=2s: ", v, " m/s"

end program usar_fisica
```

---

## 2. Programacion Orientada a Objetos (F2003)

> *"Fortran 2003 abrio las puertas de la OOP sin abandonar su naturaleza numerica - tipos con procedimientos, herencia, y polimorfismo para el computo cientifico."*

### 2.1 TYPE con Procedimientos

```fortran
! ========================================
! TIPO CON PROCEDIMIENTOS LIGADOS (F2003)
! ========================================

module geometria_modulo
    implicit none
    private

    integer, parameter, public :: dp = selected_real_kind(15, 307)
    real(dp), parameter :: PI = 3.14159265358979323846_dp

    ! Tipo con procedimientos type-bound
    type, public :: circulo
        private
        real(dp) :: radio = 0.0_dp
        real(dp) :: centro_x = 0.0_dp
        real(dp) :: centro_y = 0.0_dp
    contains
        ! Procedimientos type-bound
        procedure :: set_radio
        procedure :: get_radio
        procedure :: area
        procedure :: perimetro
        procedure :: mover
        procedure :: escalar
        procedure :: contiene_punto
        procedure :: imprimir

        ! Procedimiento generico (sobrecarga)
        generic :: asignar => set_radio, set_posicion
        procedure, private :: set_posicion
    end type circulo

    ! Constructor personalizado
    interface circulo
        module procedure crear_circulo
    end interface circulo

    public :: crear_circulo

contains

    ! ----------------------------------------
    ! CONSTRUCTOR
    ! ----------------------------------------

    function crear_circulo(radio, cx, cy) result(c)
        real(dp), intent(in) :: radio
        real(dp), intent(in), optional :: cx, cy
        type(circulo) :: c

        c%radio = radio
        if (present(cx)) c%centro_x = cx
        if (present(cy)) c%centro_y = cy
    end function crear_circulo

    ! ----------------------------------------
    ! GETTERS Y SETTERS
    ! ----------------------------------------

    subroutine set_radio(this, r)
        class(circulo), intent(inout) :: this
        real(dp), intent(in) :: r

        if (r > 0.0_dp) then
            this%radio = r
        else
            print *, "ERROR: Radio debe ser positivo"
        end if
    end subroutine set_radio

    subroutine set_posicion(this, x, y)
        class(circulo), intent(inout) :: this
        real(dp), intent(in) :: x, y

        this%centro_x = x
        this%centro_y = y
    end subroutine set_posicion

    pure function get_radio(this) result(r)
        class(circulo), intent(in) :: this
        real(dp) :: r
        r = this%radio
    end function get_radio

    ! ----------------------------------------
    ! METODOS DE CALCULO
    ! ----------------------------------------

    pure function area(this) result(a)
        class(circulo), intent(in) :: this
        real(dp) :: a
        a = PI * this%radio**2
    end function area

    pure function perimetro(this) result(p)
        class(circulo), intent(in) :: this
        real(dp) :: p
        p = 2.0_dp * PI * this%radio
    end function perimetro

    ! ----------------------------------------
    ! METODOS DE TRANSFORMACION
    ! ----------------------------------------

    subroutine mover(this, dx, dy)
        class(circulo), intent(inout) :: this
        real(dp), intent(in) :: dx, dy

        this%centro_x = this%centro_x + dx
        this%centro_y = this%centro_y + dy
    end subroutine mover

    subroutine escalar(this, factor)
        class(circulo), intent(inout) :: this
        real(dp), intent(in) :: factor

        if (factor > 0.0_dp) then
            this%radio = this%radio * factor
        end if
    end subroutine escalar

    ! ----------------------------------------
    ! METODOS DE CONSULTA
    ! ----------------------------------------

    pure function contiene_punto(this, x, y) result(contiene)
        class(circulo), intent(in) :: this
        real(dp), intent(in) :: x, y
        logical :: contiene

        real(dp) :: distancia

        distancia = sqrt((x - this%centro_x)**2 + (y - this%centro_y)**2)
        contiene = distancia <= this%radio
    end function contiene_punto

    subroutine imprimir(this)
        class(circulo), intent(in) :: this

        print '(A)', "=== Circulo ==="
        print '(A,F10.4)', "  Radio: ", this%radio
        print '(A,F10.4,A,F10.4,A)', "  Centro: (", this%centro_x, ", ", this%centro_y, ")"
        print '(A,F10.4)', "  Area: ", this%area()
        print '(A,F10.4)', "  Perimetro: ", this%perimetro()
    end subroutine imprimir

end module geometria_modulo


! ========================================
! USO DE TIPOS CON PROCEDIMIENTOS
! ========================================

program usar_circulo
    use geometria_modulo
    implicit none

    type(circulo) :: c1, c2

    ! Crear usando constructor
    c1 = circulo(5.0_dp, 0.0_dp, 0.0_dp)

    ! Usar metodos
    call c1%imprimir()

    ! Transformar
    call c1%mover(10.0_dp, 5.0_dp)
    call c1%escalar(2.0_dp)

    print '(/,A)', "Despues de transformaciones:"
    call c1%imprimir()

    ! Verificar punto
    if (c1%contiene_punto(12.0_dp, 7.0_dp)) then
        print '(A)', "El punto (12, 7) esta dentro del circulo"
    end if

end program usar_circulo
```

### 2.2 Herencia con EXTENDS

```fortran
! ========================================
! HERENCIA EN FORTRAN 2003
! ========================================

module formas_modulo
    implicit none
    private

    integer, parameter, public :: dp = selected_real_kind(15, 307)
    real(dp), parameter :: PI = 3.14159265358979323846_dp

    ! ----------------------------------------
    ! CLASE BASE
    ! ----------------------------------------

    type, public :: forma
        character(len=20) :: nombre = "forma"
        real(dp) :: pos_x = 0.0_dp
        real(dp) :: pos_y = 0.0_dp
    contains
        procedure :: area => forma_area
        procedure :: perimetro => forma_perimetro
        procedure :: mover => forma_mover
        procedure :: describir => forma_describir
    end type forma

    ! ----------------------------------------
    ! CLASE DERIVADA: RECTANGULO
    ! ----------------------------------------

    type, extends(forma), public :: rectangulo
        real(dp) :: ancho = 0.0_dp
        real(dp) :: alto = 0.0_dp
    contains
        procedure :: area => rectangulo_area
        procedure :: perimetro => rectangulo_perimetro
        procedure :: describir => rectangulo_describir
        procedure :: diagonal
    end type rectangulo

    ! ----------------------------------------
    ! CLASE DERIVADA: CIRCULO
    ! ----------------------------------------

    type, extends(forma), public :: circulo
        real(dp) :: radio = 0.0_dp
    contains
        procedure :: area => circulo_area
        procedure :: perimetro => circulo_perimetro
        procedure :: describir => circulo_describir
    end type circulo

    ! ----------------------------------------
    ! CLASE DERIVADA: TRIANGULO
    ! ----------------------------------------

    type, extends(forma), public :: triangulo
        real(dp) :: lado_a = 0.0_dp
        real(dp) :: lado_b = 0.0_dp
        real(dp) :: lado_c = 0.0_dp
    contains
        procedure :: area => triangulo_area
        procedure :: perimetro => triangulo_perimetro
        procedure :: describir => triangulo_describir
        procedure :: es_valido
    end type triangulo

contains

    ! ========================================
    ! METODOS DE FORMA BASE
    ! ========================================

    pure function forma_area(this) result(a)
        class(forma), intent(in) :: this
        real(dp) :: a
        a = 0.0_dp  ! Forma generica no tiene area
    end function forma_area

    pure function forma_perimetro(this) result(p)
        class(forma), intent(in) :: this
        real(dp) :: p
        p = 0.0_dp
    end function forma_perimetro

    subroutine forma_mover(this, dx, dy)
        class(forma), intent(inout) :: this
        real(dp), intent(in) :: dx, dy
        this%pos_x = this%pos_x + dx
        this%pos_y = this%pos_y + dy
    end subroutine forma_mover

    subroutine forma_describir(this)
        class(forma), intent(in) :: this
        print '(A,A)', "Forma: ", trim(this%nombre)
        print '(A,F8.2,A,F8.2,A)', "Posicion: (", this%pos_x, ", ", this%pos_y, ")"
    end subroutine forma_describir

    ! ========================================
    ! METODOS DE RECTANGULO
    ! ========================================

    pure function rectangulo_area(this) result(a)
        class(rectangulo), intent(in) :: this
        real(dp) :: a
        a = this%ancho * this%alto
    end function rectangulo_area

    pure function rectangulo_perimetro(this) result(p)
        class(rectangulo), intent(in) :: this
        real(dp) :: p
        p = 2.0_dp * (this%ancho + this%alto)
    end function rectangulo_perimetro

    subroutine rectangulo_describir(this)
        class(rectangulo), intent(in) :: this
        call this%forma%describir()  ! Llamar metodo padre
        print '(A,F8.2,A,F8.2)', "  Dimensiones: ", this%ancho, " x ", this%alto
        print '(A,F8.2)', "  Area: ", this%area()
        print '(A,F8.2)', "  Diagonal: ", this%diagonal()
    end subroutine rectangulo_describir

    pure function diagonal(this) result(d)
        class(rectangulo), intent(in) :: this
        real(dp) :: d
        d = sqrt(this%ancho**2 + this%alto**2)
    end function diagonal

    ! ========================================
    ! METODOS DE CIRCULO
    ! ========================================

    pure function circulo_area(this) result(a)
        class(circulo), intent(in) :: this
        real(dp) :: a
        a = PI * this%radio**2
    end function circulo_area

    pure function circulo_perimetro(this) result(p)
        class(circulo), intent(in) :: this
        real(dp) :: p
        p = 2.0_dp * PI * this%radio
    end function circulo_perimetro

    subroutine circulo_describir(this)
        class(circulo), intent(in) :: this
        call this%forma%describir()
        print '(A,F8.2)', "  Radio: ", this%radio
        print '(A,F8.2)', "  Area: ", this%area()
    end subroutine circulo_describir

    ! ========================================
    ! METODOS DE TRIANGULO
    ! ========================================

    pure function triangulo_area(this) result(a)
        class(triangulo), intent(in) :: this
        real(dp) :: a, s

        ! Formula de Heron
        s = this%perimetro() / 2.0_dp
        a = sqrt(s * (s - this%lado_a) * (s - this%lado_b) * (s - this%lado_c))
    end function triangulo_area

    pure function triangulo_perimetro(this) result(p)
        class(triangulo), intent(in) :: this
        real(dp) :: p
        p = this%lado_a + this%lado_b + this%lado_c
    end function triangulo_perimetro

    subroutine triangulo_describir(this)
        class(triangulo), intent(in) :: this
        call this%forma%describir()
        print '(A,F8.2,A,F8.2,A,F8.2)', "  Lados: ", this%lado_a, ", ", this%lado_b, ", ", this%lado_c
        print '(A,F8.2)', "  Area: ", this%area()
    end subroutine triangulo_describir

    pure function es_valido(this) result(valido)
        class(triangulo), intent(in) :: this
        logical :: valido

        ! Desigualdad triangular
        valido = (this%lado_a + this%lado_b > this%lado_c) .and. &
                 (this%lado_b + this%lado_c > this%lado_a) .and. &
                 (this%lado_a + this%lado_c > this%lado_b)
    end function es_valido

end module formas_modulo
```

### 2.3 ABSTRACT y DEFERRED

```fortran
! ========================================
! CLASES ABSTRACTAS (F2003)
! ========================================

module animales_modulo
    implicit none
    private

    ! ----------------------------------------
    ! CLASE ABSTRACTA BASE
    ! ----------------------------------------

    type, abstract, public :: animal
        character(len=50) :: nombre
        integer :: edad = 0
    contains
        ! Procedimientos abstractos (DEBEN implementarse)
        procedure(sonido_interface), deferred :: hacer_sonido
        procedure(mover_interface), deferred :: mover

        ! Procedimientos concretos (heredados)
        procedure :: presentarse
        procedure :: cumplir_anios
    end type animal

    ! Interfaces abstractas
    abstract interface
        subroutine sonido_interface(this)
            import :: animal
            class(animal), intent(in) :: this
        end subroutine sonido_interface

        subroutine mover_interface(this, distancia)
            import :: animal
            class(animal), intent(inout) :: this
            real, intent(in) :: distancia
        end subroutine mover_interface
    end interface

    ! ----------------------------------------
    ! CLASE CONCRETA: PERRO
    ! ----------------------------------------

    type, extends(animal), public :: perro
        character(len=30) :: raza
        real :: posicion = 0.0
    contains
        procedure :: hacer_sonido => perro_sonido
        procedure :: mover => perro_mover
        procedure :: ladrar
    end type perro

    ! ----------------------------------------
    ! CLASE CONCRETA: GATO
    ! ----------------------------------------

    type, extends(animal), public :: gato
        character(len=30) :: color
        real :: posicion = 0.0
    contains
        procedure :: hacer_sonido => gato_sonido
        procedure :: mover => gato_mover
        procedure :: ronronear
    end type gato

    ! ----------------------------------------
    ! CLASE CONCRETA: PAJARO
    ! ----------------------------------------

    type, extends(animal), public :: pajaro
        real :: altura = 0.0
        real :: posicion_h = 0.0
    contains
        procedure :: hacer_sonido => pajaro_sonido
        procedure :: mover => pajaro_mover
        procedure :: volar
    end type pajaro

contains

    ! ========================================
    ! METODOS BASE (no abstractos)
    ! ========================================

    subroutine presentarse(this)
        class(animal), intent(in) :: this
        print '(A,A,A,I0,A)', "Soy ", trim(this%nombre), ", tengo ", this%edad, " anios"
    end subroutine presentarse

    subroutine cumplir_anios(this)
        class(animal), intent(inout) :: this
        this%edad = this%edad + 1
        print '(A,A,A,I0,A)', trim(this%nombre), " ahora tiene ", this%edad, " anios!"
    end subroutine cumplir_anios

    ! ========================================
    ! IMPLEMENTACIONES PARA PERRO
    ! ========================================

    subroutine perro_sonido(this)
        class(perro), intent(in) :: this
        print '(A,A,A)', trim(this%nombre), " (", trim(this%raza), ") dice: GUAU GUAU!"
    end subroutine perro_sonido

    subroutine perro_mover(this, distancia)
        class(perro), intent(inout) :: this
        real, intent(in) :: distancia
        this%posicion = this%posicion + distancia
        print '(A,A,F6.1)', trim(this%nombre), " corre a posicion ", this%posicion
    end subroutine perro_mover

    subroutine ladrar(this, veces)
        class(perro), intent(in) :: this
        integer, intent(in) :: veces
        integer :: i
        do i = 1, veces
            call this%hacer_sonido()
        end do
    end subroutine ladrar

    ! ========================================
    ! IMPLEMENTACIONES PARA GATO
    ! ========================================

    subroutine gato_sonido(this)
        class(gato), intent(in) :: this
        print '(A,A,A)', trim(this%nombre), " (gato ", trim(this%color), ") dice: MIAU!"
    end subroutine gato_sonido

    subroutine gato_mover(this, distancia)
        class(gato), intent(inout) :: this
        real, intent(in) :: distancia
        this%posicion = this%posicion + distancia * 0.5  ! Gatos son mas lentos
        print '(A,A,F6.1)', trim(this%nombre), " camina sigilosamente a ", this%posicion
    end subroutine gato_mover

    subroutine ronronear(this)
        class(gato), intent(in) :: this
        print '(A,A)', trim(this%nombre), ": prrrrrrr..."
    end subroutine ronronear

    ! ========================================
    ! IMPLEMENTACIONES PARA PAJARO
    ! ========================================

    subroutine pajaro_sonido(this)
        class(pajaro), intent(in) :: this
        print '(A,A)', trim(this%nombre), " dice: PIO PIO!"
    end subroutine pajaro_sonido

    subroutine pajaro_mover(this, distancia)
        class(pajaro), intent(inout) :: this
        real, intent(in) :: distancia
        this%posicion_h = this%posicion_h + distancia * 2.0  ! Volar es mas rapido
        print '(A,A,F6.1,A,F6.1)', trim(this%nombre), " vuela a h=", this%posicion_h, &
              ", altura=", this%altura
    end subroutine pajaro_mover

    subroutine volar(this, altura_nueva)
        class(pajaro), intent(inout) :: this
        real, intent(in) :: altura_nueva
        this%altura = altura_nueva
        print '(A,A,F6.1,A)', trim(this%nombre), " vuela a ", altura_nueva, " metros"
    end subroutine volar

end module animales_modulo
```

### 2.4 Polimorfismo con CLASS y SELECT TYPE

```fortran
! ========================================
! POLIMORFISMO EN FORTRAN 2003
! ========================================

module zoo_modulo
    use animales_modulo
    implicit none
    private

    public :: procesar_animal, mostrar_zoo

contains

    ! ----------------------------------------
    ! PROCEDIMIENTO POLIMORFICO
    ! ----------------------------------------

    subroutine procesar_animal(a)
        class(animal), intent(inout) :: a  ! CLASS permite polimorfismo

        print '(/,A)', "=== Procesando animal ==="

        ! Metodos comunes funcionan polimorficamente
        call a%presentarse()
        call a%hacer_sonido()
        call a%mover(10.0)

        ! SELECT TYPE para comportamiento especifico
        select type (a)
            type is (perro)
                print '(A,A)', "Es un perro de raza: ", trim(a%raza)
                call a%ladrar(2)

            type is (gato)
                print '(A,A)', "Es un gato de color: ", trim(a%color)
                call a%ronronear()

            type is (pajaro)
                print '(A)', "Es un pajaro"
                call a%volar(50.0)

            class is (animal)
                print '(A)', "Es algun tipo de animal derivado"

            class default
                print '(A)', "Tipo desconocido"
        end select

    end subroutine procesar_animal

    ! ----------------------------------------
    ! ARRAY POLIMORFICO
    ! ----------------------------------------

    subroutine mostrar_zoo(animales)
        class(animal), intent(in) :: animales(:)
        integer :: i

        print '(/,A)', "========== ZOO =========="
        print '(A,I0,A)', "Tenemos ", size(animales), " animales:"
        print '(A)', "========================="

        do i = 1, size(animales)
            print '(/,A,I0,A)', "--- Animal #", i, " ---"
            call animales(i)%presentarse()
            call animales(i)%hacer_sonido()
        end do

    end subroutine mostrar_zoo

end module zoo_modulo


! ========================================
! PROGRAMA PRINCIPAL
! ========================================

program usar_polimorfismo
    use animales_modulo
    use zoo_modulo
    implicit none

    type(perro) :: mi_perro
    type(gato) :: mi_gato
    type(pajaro) :: mi_pajaro

    class(animal), allocatable :: mascota

    ! Inicializar animales
    mi_perro%nombre = "Firulais"
    mi_perro%edad = 3
    mi_perro%raza = "Pastor Aleman"

    mi_gato%nombre = "Michi"
    mi_gato%edad = 2
    mi_gato%color = "naranja"

    mi_pajaro%nombre = "Piolín"
    mi_pajaro%edad = 1

    ! Procesar cada animal polimorficamente
    call procesar_animal(mi_perro)
    call procesar_animal(mi_gato)
    call procesar_animal(mi_pajaro)

    ! Variable polimorfica
    print '(/,A)', "=== Usando variable polimorfica ==="

    allocate(perro :: mascota)
    select type (mascota)
        type is (perro)
            mascota%nombre = "Rex"
            mascota%edad = 5
            mascota%raza = "Labrador"
    end select

    call procesar_animal(mascota)

    deallocate(mascota)

end program usar_polimorfismo
```

### 2.5 FINAL - Destructores

```fortran
! ========================================
! DESTRUCTORES EN FORTRAN 2003
! ========================================

module recursos_modulo
    implicit none
    private

    integer, parameter, public :: dp = selected_real_kind(15, 307)

    type, public :: matriz_grande
        private
        real(dp), allocatable :: datos(:,:)
        integer :: filas = 0
        integer :: columnas = 0
        integer :: id = 0
    contains
        procedure :: inicializar
        procedure :: liberar
        procedure :: info
        final :: destructor_matriz  ! DESTRUCTOR
    end type matriz_grande

    ! Contador global para IDs
    integer, save :: contador_matrices = 0

contains

    subroutine inicializar(this, n, m)
        class(matriz_grande), intent(inout) :: this
        integer, intent(in) :: n, m

        ! Liberar si ya esta asignada
        if (allocated(this%datos)) deallocate(this%datos)

        this%filas = n
        this%columnas = m
        allocate(this%datos(n, m))
        this%datos = 0.0_dp

        contador_matrices = contador_matrices + 1
        this%id = contador_matrices

        print '(A,I0,A,I0,A,I0,A)', "Matriz #", this%id, " creada (", n, "x", m, ")"
    end subroutine inicializar

    subroutine liberar(this)
        class(matriz_grande), intent(inout) :: this

        if (allocated(this%datos)) then
            deallocate(this%datos)
            print '(A,I0,A)', "Matriz #", this%id, " liberada manualmente"
        end if
        this%filas = 0
        this%columnas = 0
    end subroutine liberar

    subroutine info(this)
        class(matriz_grande), intent(in) :: this

        print '(A,I0)', "  ID: ", this%id
        print '(A,I0,A,I0)', "  Dimensiones: ", this%filas, " x ", this%columnas
        if (allocated(this%datos)) then
            print '(A,I0,A)', "  Memoria: ", size(this%datos) * 8, " bytes"
        else
            print '(A)', "  Memoria: no asignada"
        end if
    end subroutine info

    ! ----------------------------------------
    ! DESTRUCTOR (FINAL)
    ! Llamado automaticamente cuando la variable
    ! sale de scope o es deallocated
    ! ----------------------------------------

    subroutine destructor_matriz(this)
        type(matriz_grande), intent(inout) :: this

        if (allocated(this%datos)) then
            print '(A,I0,A)', "[DESTRUCTOR] Liberando matriz #", this%id, " automaticamente"
            deallocate(this%datos)
        end if
    end subroutine destructor_matriz

end module recursos_modulo


program usar_destructores
    use recursos_modulo
    implicit none

    print '(A)', "=== Inicio del programa ==="

    call demo_scope()

    print '(/,A)', "=== Fin del programa ==="

contains

    subroutine demo_scope()
        type(matriz_grande) :: m1, m2

        print '(/,A)', "--- Dentro de demo_scope ---"

        call m1%inicializar(100, 100)
        call m2%inicializar(50, 200)

        call m1%info()
        call m2%info()

        print '(/,A)', "--- Saliendo de demo_scope ---"
        print '(A)', "(Los destructores se llamaran automaticamente)"

        ! Al salir del scope, se llaman los destructores
    end subroutine demo_scope

end program usar_destructores
```

---

## 3. Interoperabilidad con C (F2003)

> *"ISO_C_BINDING es el puente diplomatico entre dos mundos: Fortran el calculador cientifico, y C el arquitecto de sistemas. Juntos, pueden construir imperios computacionales."*

### 3.1 ISO_C_BINDING Basico

```fortran
! ========================================
! TIPOS C INTEROPERABLES
! ========================================

module c_tipos_modulo
    use, intrinsic :: iso_c_binding
    implicit none

    ! Tipos C disponibles:
    !
    ! C_INT          -> int
    ! C_SHORT        -> short int
    ! C_LONG         -> long int
    ! C_LONG_LONG    -> long long int
    ! C_SIGNED_CHAR  -> signed char
    !
    ! C_FLOAT        -> float
    ! C_DOUBLE       -> double
    ! C_LONG_DOUBLE  -> long double
    !
    ! C_CHAR         -> char
    ! C_BOOL         -> _Bool (C99)
    !
    ! C_PTR          -> void*
    ! C_FUNPTR       -> function pointer
    ! C_NULL_PTR     -> NULL
    ! C_NULL_FUNPTR  -> NULL function pointer

    ! Ejemplo de declaraciones interoperables
    integer(c_int) :: entero_c
    real(c_float) :: float_c
    real(c_double) :: double_c
    character(kind=c_char) :: char_c
    logical(c_bool) :: bool_c
    type(c_ptr) :: puntero_c

end module c_tipos_modulo
```

### 3.2 Llamar Funciones C desde Fortran

```fortran
! ========================================
! INTERFAZ PARA FUNCIONES C
! ========================================

module c_interfaces
    use, intrinsic :: iso_c_binding
    implicit none

    ! ----------------------------------------
    ! Interfaces para funciones C estandar
    ! ----------------------------------------

    interface
        ! double sqrt(double x);
        function c_sqrt(x) bind(C, name="sqrt")
            import :: c_double
            real(c_double), value :: x
            real(c_double) :: c_sqrt
        end function c_sqrt

        ! double sin(double x);
        function c_sin(x) bind(C, name="sin")
            import :: c_double
            real(c_double), value :: x
            real(c_double) :: c_sin
        end function c_sin

        ! double cos(double x);
        function c_cos(x) bind(C, name="cos")
            import :: c_double
            real(c_double), value :: x
            real(c_double) :: c_cos
        end function c_cos

        ! int printf(const char *format, ...);
        ! (No se puede usar directamente por argumentos variables)

        ! void *malloc(size_t size);
        function c_malloc(size) bind(C, name="malloc")
            import :: c_ptr, c_size_t
            integer(c_size_t), value :: size
            type(c_ptr) :: c_malloc
        end function c_malloc

        ! void free(void *ptr);
        subroutine c_free(ptr) bind(C, name="free")
            import :: c_ptr
            type(c_ptr), value :: ptr
        end subroutine c_free

        ! int puts(const char *str);
        function c_puts(str) bind(C, name="puts")
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: str(*)
            integer(c_int) :: c_puts
        end function c_puts

        ! size_t strlen(const char *str);
        function c_strlen(str) bind(C, name="strlen")
            import :: c_char, c_size_t
            character(kind=c_char), intent(in) :: str(*)
            integer(c_size_t) :: c_strlen
        end function c_strlen
    end interface

contains

    ! Funcion auxiliar para convertir string Fortran a C
    function f_to_c_string(f_string) result(c_string)
        character(len=*), intent(in) :: f_string
        character(kind=c_char), allocatable :: c_string(:)
        integer :: i, n

        n = len_trim(f_string)
        allocate(c_string(n + 1))

        do i = 1, n
            c_string(i) = f_string(i:i)
        end do
        c_string(n + 1) = c_null_char  ! Terminador nulo
    end function f_to_c_string

end module c_interfaces


! ========================================
! USO DE FUNCIONES C
! ========================================

program llamar_c
    use, intrinsic :: iso_c_binding
    use c_interfaces
    implicit none

    real(c_double) :: x, resultado
    character(kind=c_char), allocatable :: mensaje(:)
    integer(c_int) :: ret

    ! Usar funciones matematicas de C
    x = 2.0_c_double
    resultado = c_sqrt(x)
    print '(A,F10.6)', "sqrt(2) = ", resultado

    resultado = c_sin(3.14159265_c_double / 2.0_c_double)
    print '(A,F10.6)', "sin(pi/2) = ", resultado

    ! Usar puts de C
    mensaje = f_to_c_string("Hola desde C!")
    ret = c_puts(mensaje)
    deallocate(mensaje)

    ! Mostrar longitud usando strlen
    mensaje = f_to_c_string("Fortran y C juntos")
    print '(A,I0)', "Longitud del string: ", c_strlen(mensaje)
    deallocate(mensaje)

end program llamar_c
```

### 3.3 Funciones Fortran Llamables desde C

```fortran
! ========================================
! FUNCIONES FORTRAN EXPORTADAS A C
! ========================================

module fortran_para_c
    use, intrinsic :: iso_c_binding
    implicit none

contains

    ! ----------------------------------------
    ! Funcion simple con BIND(C)
    ! ----------------------------------------

    ! Declaracion C: double fortran_sumar(double a, double b);
    function fortran_sumar(a, b) result(suma) bind(C, name="fortran_sumar")
        real(c_double), value :: a, b
        real(c_double) :: suma

        suma = a + b
    end function fortran_sumar

    ! ----------------------------------------
    ! Funcion con array
    ! ----------------------------------------

    ! Declaracion C: double fortran_norma(double *vec, int n);
    function fortran_norma(vec, n) result(norma) bind(C, name="fortran_norma")
        integer(c_int), value :: n
        real(c_double), intent(in) :: vec(n)
        real(c_double) :: norma

        norma = sqrt(sum(vec**2))
    end function fortran_norma

    ! ----------------------------------------
    ! Subrutina que modifica datos
    ! ----------------------------------------

    ! Declaracion C: void fortran_escalar(double *vec, int n, double factor);
    subroutine fortran_escalar(vec, n, factor) bind(C, name="fortran_escalar")
        integer(c_int), value :: n
        real(c_double), intent(inout) :: vec(n)
        real(c_double), value :: factor

        vec = vec * factor
    end subroutine fortran_escalar

    ! ----------------------------------------
    ! Funcion con struct/tipo derivado
    ! ----------------------------------------

    ! Tipo interoperable con struct C
    type, bind(C) :: punto3d
        real(c_double) :: x
        real(c_double) :: y
        real(c_double) :: z
    end type punto3d

    ! Declaracion C: double fortran_distancia(struct punto3d *p1, struct punto3d *p2);
    function fortran_distancia(p1, p2) result(dist) bind(C, name="fortran_distancia")
        type(punto3d), intent(in) :: p1, p2
        real(c_double) :: dist

        dist = sqrt((p2%x - p1%x)**2 + (p2%y - p1%y)**2 + (p2%z - p1%z)**2)
    end function fortran_distancia

    ! ----------------------------------------
    ! Trabajar con punteros C
    ! ----------------------------------------

    ! Declaracion C: void fortran_procesar_ptr(double *data, int *size);
    subroutine fortran_procesar_ptr(data_ptr, size_ptr) bind(C, name="fortran_procesar_ptr")
        type(c_ptr), value :: data_ptr
        type(c_ptr), value :: size_ptr

        real(c_double), pointer :: data(:)
        integer(c_int), pointer :: n
        integer :: i

        ! Convertir punteros C a punteros Fortran
        call c_f_pointer(size_ptr, n)
        call c_f_pointer(data_ptr, data, [n])

        ! Procesar datos
        do i = 1, n
            data(i) = data(i) * 2.0_c_double
        end do
    end subroutine fortran_procesar_ptr

    ! ----------------------------------------
    ! Funcion que retorna puntero
    ! ----------------------------------------

    ! Declaracion C: double* fortran_crear_array(int n);
    function fortran_crear_array(n) result(ptr) bind(C, name="fortran_crear_array")
        integer(c_int), value :: n
        type(c_ptr) :: ptr

        real(c_double), pointer :: array(:)
        integer :: i

        allocate(array(n))
        do i = 1, n
            array(i) = real(i, c_double)
        end do

        ptr = c_loc(array(1))
    end function fortran_crear_array

end module fortran_para_c
```

### 3.4 Ejemplo Completo: Codigo C Usando Fortran

```c
/* ========================================
 * ARCHIVO: usar_fortran.c
 * Ejemplo de C llamando funciones Fortran
 * ======================================== */

#include <stdio.h>

/* Declaraciones de funciones Fortran */
extern double fortran_sumar(double a, double b);
extern double fortran_norma(double *vec, int n);
extern void fortran_escalar(double *vec, int n, double factor);

struct punto3d {
    double x;
    double y;
    double z;
};

extern double fortran_distancia(struct punto3d *p1, struct punto3d *p2);

int main() {
    /* Usar funcion simple */
    double resultado = fortran_sumar(3.5, 2.5);
    printf("fortran_sumar(3.5, 2.5) = %f\n", resultado);

    /* Usar funcion con array */
    double vector[] = {3.0, 4.0};
    double norma = fortran_norma(vector, 2);
    printf("Norma de [3, 4] = %f\n", norma);

    /* Modificar array */
    double datos[] = {1.0, 2.0, 3.0, 4.0, 5.0};
    printf("Antes: ");
    for (int i = 0; i < 5; i++) printf("%f ", datos[i]);
    printf("\n");

    fortran_escalar(datos, 5, 2.0);

    printf("Despues (x2): ");
    for (int i = 0; i < 5; i++) printf("%f ", datos[i]);
    printf("\n");

    /* Usar structs */
    struct punto3d p1 = {0.0, 0.0, 0.0};
    struct punto3d p2 = {1.0, 1.0, 1.0};
    double distancia = fortran_distancia(&p1, &p2);
    printf("Distancia entre puntos: %f\n", distancia);

    return 0;
}
```

---

## 4. Paralelismo

> *"El paralelismo no es una opcion en la era de los multicores - es una necesidad. Fortran lo abrazo con DO CONCURRENT, OpenMP, y los revolucionarios Coarrays."*

### 4.1 DO CONCURRENT (F2008)

```fortran
! ========================================
! DO CONCURRENT - Bucles paralelizables
! ========================================

program do_concurrent_demo
    implicit none

    integer, parameter :: n = 1000000
    real, dimension(n) :: a, b, c
    integer :: i

    ! Inicializar
    do concurrent (i = 1:n)
        a(i) = real(i)
        b(i) = real(n - i + 1)
    end do

    ! ----------------------------------------
    ! DO CONCURRENT basico
    ! El compilador puede paralelizar esto
    ! ----------------------------------------

    do concurrent (i = 1:n)
        c(i) = a(i) + b(i)
    end do

    ! ----------------------------------------
    ! Con mascara (condicion)
    ! ----------------------------------------

    do concurrent (i = 1:n, a(i) > 500000.0)
        c(i) = sqrt(a(i))
    end do

    ! ----------------------------------------
    ! Multiples indices
    ! ----------------------------------------

    real, dimension(100, 100) :: matriz
    integer :: j

    do concurrent (i = 1:100, j = 1:100)
        matriz(i, j) = real(i * j)
    end do

    ! ----------------------------------------
    ! Con LOCAL y LOCAL_INIT (F2018)
    ! Variables locales a cada iteracion
    ! ----------------------------------------

    real :: temp

    do concurrent (i = 1:n) local(temp)
        temp = a(i) * 2.0
        c(i) = temp + b(i)
    end do

    ! ----------------------------------------
    ! Con REDUCE (F2018)
    ! Reduccion paralela
    ! ----------------------------------------

    real :: suma
    suma = 0.0

    do concurrent (i = 1:n) reduce(+: suma)
        suma = suma + a(i)
    end do

    print '(A,E15.7)', "Suma total: ", suma

    ! ----------------------------------------
    ! RESTRICCIONES de DO CONCURRENT
    ! ----------------------------------------
    ! - No puede tener efectos secundarios entre iteraciones
    ! - No puede tener dependencias de datos
    ! - No puede llamar procedimientos impuros
    ! - No puede usar I/O
    ! - No puede usar STOP, EXIT, CYCLE, RETURN, GOTO

end program do_concurrent_demo
```

### 4.2 OpenMP Basico

```fortran
! ========================================
! OPENMP EN FORTRAN
! Compilar con: gfortran -fopenmp programa.f90
! ========================================

program openmp_demo
    use omp_lib  ! Biblioteca OpenMP
    implicit none

    integer, parameter :: n = 10000000
    real(8), allocatable :: a(:), b(:), c(:)
    real(8) :: suma, t_inicio, t_fin
    integer :: i, num_threads

    allocate(a(n), b(n), c(n))

    ! ----------------------------------------
    ! Obtener informacion de threads
    ! ----------------------------------------

    !$omp parallel
    !$omp single
    num_threads = omp_get_num_threads()
    print '(A,I0)', "Numero de threads: ", num_threads
    !$omp end single
    !$omp end parallel

    ! Inicializar datos (secuencial)
    do i = 1, n
        a(i) = real(i, 8)
        b(i) = real(n - i + 1, 8)
    end do

    ! ----------------------------------------
    ! PARALLEL DO - Bucle paralelo
    ! ----------------------------------------

    t_inicio = omp_get_wtime()

    !$omp parallel do
    do i = 1, n
        c(i) = a(i) + b(i)
    end do
    !$omp end parallel do

    t_fin = omp_get_wtime()
    print '(A,F10.6,A)', "Tiempo parallel do: ", t_fin - t_inicio, " s"

    ! ----------------------------------------
    ! REDUCTION - Suma paralela
    ! ----------------------------------------

    suma = 0.0d0

    t_inicio = omp_get_wtime()

    !$omp parallel do reduction(+:suma)
    do i = 1, n
        suma = suma + c(i)
    end do
    !$omp end parallel do

    t_fin = omp_get_wtime()
    print '(A,E15.7)', "Suma: ", suma
    print '(A,F10.6,A)', "Tiempo reduction: ", t_fin - t_inicio, " s"

    ! ----------------------------------------
    ! PRIVATE y SHARED
    ! ----------------------------------------

    real(8) :: temp

    !$omp parallel do private(temp) shared(a, b, c)
    do i = 1, n
        temp = a(i) * 2.0d0    ! temp es local a cada thread
        c(i) = temp + b(i)      ! a, b, c son compartidos
    end do
    !$omp end parallel do

    ! ----------------------------------------
    ! SCHEDULE - Control de distribucion
    ! ----------------------------------------

    ! static: divide equitativamente
    !$omp parallel do schedule(static)
    do i = 1, n
        c(i) = sqrt(a(i))
    end do
    !$omp end parallel do

    ! dynamic: asigna chunks dinamicamente
    !$omp parallel do schedule(dynamic, 1000)
    do i = 1, n
        c(i) = sin(a(i)) * cos(b(i))
    end do
    !$omp end parallel do

    ! guided: chunks decrecientes
    !$omp parallel do schedule(guided)
    do i = 1, n
        c(i) = exp(-a(i)/real(n,8))
    end do
    !$omp end parallel do

    ! ----------------------------------------
    ! CRITICAL - Seccion critica
    ! ----------------------------------------

    real(8) :: maximo
    maximo = -huge(1.0d0)

    !$omp parallel do
    do i = 1, n
        if (c(i) > maximo) then
            !$omp critical
            if (c(i) > maximo) maximo = c(i)
            !$omp end critical
        end if
    end do
    !$omp end parallel do

    print '(A,E15.7)', "Maximo: ", maximo

    ! ----------------------------------------
    ! SECTIONS - Tareas paralelas
    ! ----------------------------------------

    !$omp parallel sections

    !$omp section
    call procesar_parte1(a, n)

    !$omp section
    call procesar_parte2(b, n)

    !$omp section
    call procesar_parte3(c, n)

    !$omp end parallel sections

    deallocate(a, b, c)

contains

    subroutine procesar_parte1(arr, size)
        real(8), intent(inout) :: arr(:)
        integer, intent(in) :: size
        print '(A,I0)', "Parte 1 ejecutada por thread ", omp_get_thread_num()
    end subroutine

    subroutine procesar_parte2(arr, size)
        real(8), intent(inout) :: arr(:)
        integer, intent(in) :: size
        print '(A,I0)', "Parte 2 ejecutada por thread ", omp_get_thread_num()
    end subroutine

    subroutine procesar_parte3(arr, size)
        real(8), intent(inout) :: arr(:)
        integer, intent(in) :: size
        print '(A,I0)', "Parte 3 ejecutada por thread ", omp_get_thread_num()
    end subroutine

end program openmp_demo
```

### 4.3 Coarrays (F2008/F2018)

```fortran
! ========================================
! COARRAYS - Paralelismo PGAS
! Compilar con: gfortran -fcoarray=lib programa.f90 -lcaf_mpi
! Ejecutar con: cafrun -np 4 ./programa
! ========================================

program coarray_demo
    implicit none

    ! ----------------------------------------
    ! DECLARACION DE COARRAYS
    ! [*] significa distribuido entre imagenes
    ! ----------------------------------------

    integer :: mi_imagen, total_imagenes
    real :: dato_local[*]                    ! Escalar coarray
    real, allocatable :: vector_local(:)[:]  ! Array coarray allocatable
    real :: matriz(10, 10)[*]                ! Array coarray fijo

    ! Obtener informacion de imagen
    mi_imagen = this_image()
    total_imagenes = num_images()

    print '(A,I0,A,I0)', "Imagen ", mi_imagen, " de ", total_imagenes

    ! ----------------------------------------
    ! COMUNICACION ENTRE IMAGENES
    ! ----------------------------------------

    ! Cada imagen tiene su dato_local
    dato_local = real(mi_imagen * 100)

    ! Sincronizar antes de leer datos de otras imagenes
    sync all

    ! Leer dato de otra imagen
    if (mi_imagen == 1) then
        print '(A)', "Imagen 1 lee datos de otras imagenes:"
        integer :: i
        do i = 1, total_imagenes
            print '(A,I0,A,F8.2)', "  dato_local[", i, "] = ", dato_local[i]
        end do
    end if

    sync all

    ! ----------------------------------------
    ! COARRAYS ALLOCATABLE
    ! ----------------------------------------

    allocate(vector_local(1000)[*])

    ! Inicializar localmente
    vector_local = real(mi_imagen)

    sync all

    ! Operaciones colectivas
    if (mi_imagen == 1) then
        real :: suma
        suma = 0.0
        do i = 1, total_imagenes
            suma = suma + sum(vector_local(:)[i])
        end do
        print '(A,E15.7)', "Suma total: ", suma
    end if

    deallocate(vector_local)

    ! ----------------------------------------
    ! SINCRONIZACION
    ! ----------------------------------------

    ! Sincronizar todas las imagenes
    sync all

    ! Sincronizar con imagenes especificas
    if (mi_imagen == 1) then
        ! Esperar solo a imagen 2
        sync images(2)
    else if (mi_imagen == 2) then
        sync images(1)
    end if

    ! ----------------------------------------
    ! CRITICAL - Seccion critica
    ! ----------------------------------------

    critical
        print '(A,I0,A)', "Imagen ", mi_imagen, " en seccion critica"
    end critical

    sync all

    ! ----------------------------------------
    ! OPERACIONES ATOMICAS (F2018)
    ! ----------------------------------------

    integer :: contador[*]
    integer :: valor_temp

    if (mi_imagen == 1) contador = 0
    sync all

    ! Incremento atomico
    call atomic_add(contador[1], 1)

    sync all

    if (mi_imagen == 1) then
        print '(A,I0)', "Contador final: ", contador
    end if

    ! ----------------------------------------
    ! CO_SUM, CO_MAX, CO_MIN, CO_BROADCAST (F2018)
    ! Colectivas intrinsecas
    ! ----------------------------------------

    real :: mi_valor, suma_global, max_global

    mi_valor = real(mi_imagen * 10)

    ! Suma colectiva
    call co_sum(mi_valor, result_image=1)
    if (mi_imagen == 1) then
        print '(A,F10.2)', "Suma colectiva: ", mi_valor
    end if

    mi_valor = real(mi_imagen * 10)

    ! Maximo colectivo
    call co_max(mi_valor)
    print '(A,I0,A,F10.2)', "Imagen ", mi_imagen, " max global: ", mi_valor

    ! Broadcast desde imagen 1
    if (mi_imagen == 1) mi_valor = 999.0
    call co_broadcast(mi_valor, source_image=1)
    print '(A,I0,A,F10.2)', "Imagen ", mi_imagen, " recibe: ", mi_valor

end program coarray_demo
```

### 4.4 Ejemplo: Calculo Pi con Coarrays

```fortran
! ========================================
! CALCULO DE PI CON COARRAYS
! Metodo de Monte Carlo distribuido
! ========================================

program pi_coarray
    implicit none

    integer, parameter :: dp = selected_real_kind(15, 307)
    integer(8), parameter :: puntos_por_imagen = 100000000_8

    integer :: mi_imagen, total_imagenes
    integer(8) :: dentro_local[*], dentro_total
    integer(8) :: i
    real(dp) :: x, y, pi_estimado
    real(dp) :: semilla

    mi_imagen = this_image()
    total_imagenes = num_images()

    ! Inicializar generador con semilla unica por imagen
    semilla = real(mi_imagen, dp) * 12345.6789_dp
    call random_seed()
    call random_number(x)  ! Calentar generador

    dentro_local = 0_8

    ! Cada imagen calcula su parte
    do i = 1, puntos_por_imagen
        call random_number(x)
        call random_number(y)

        if (x*x + y*y <= 1.0_dp) then
            dentro_local = dentro_local + 1_8
        end if
    end do

    sync all

    ! Imagen 1 recolecta resultados
    if (mi_imagen == 1) then
        dentro_total = 0_8
        do i = 1, total_imagenes
            dentro_total = dentro_total + dentro_local[i]
        end do

        pi_estimado = 4.0_dp * real(dentro_total, dp) / &
                      real(puntos_por_imagen * total_imagenes, dp)

        print '(A)', "========================================="
        print '(A,I0)', "Imagenes utilizadas: ", total_imagenes
        print '(A,I0)', "Puntos por imagen: ", puntos_por_imagen
        print '(A,I0)', "Puntos totales: ", puntos_por_imagen * total_imagenes
        print '(A)', "-----------------------------------------"
        print '(A,F18.15)', "PI estimado:  ", pi_estimado
        print '(A,F18.15)', "PI real:      ", acos(-1.0_dp)
        print '(A,E12.5)', "Error:        ", abs(pi_estimado - acos(-1.0_dp))
        print '(A)', "========================================="
    end if

end program pi_coarray
```

---

## 5. Caracteristicas Modernas

> *"Las caracteristicas modernas de Fortran no son adornos - son herramientas de precision que transforman codigo espagueti en arquitectura limpia."*

### 5.1 Punteros a Procedimientos

```fortran
! ========================================
! PUNTEROS A PROCEDIMIENTOS (F2003)
! ========================================

module funciones_modulo
    implicit none

    integer, parameter :: dp = selected_real_kind(15, 307)

    ! ----------------------------------------
    ! Interfaz abstracta para el tipo de funcion
    ! ----------------------------------------

    abstract interface
        function func_real(x) result(y)
            import :: dp
            real(dp), intent(in) :: x
            real(dp) :: y
        end function func_real
    end interface

contains

    ! Funciones concretas

    function cuadrado(x) result(y)
        real(dp), intent(in) :: x
        real(dp) :: y
        y = x * x
    end function cuadrado

    function cubo(x) result(y)
        real(dp), intent(in) :: x
        real(dp) :: y
        y = x * x * x
    end function cubo

    function seno(x) result(y)
        real(dp), intent(in) :: x
        real(dp) :: y
        y = sin(x)
    end function seno

    function exponencial(x) result(y)
        real(dp), intent(in) :: x
        real(dp) :: y
        y = exp(x)
    end function exponencial

    ! ----------------------------------------
    ! Funcion que recibe puntero a procedimiento
    ! ----------------------------------------

    function integrar_trapecio(f, a, b, n) result(integral)
        procedure(func_real), pointer :: f   ! Puntero a procedimiento
        real(dp), intent(in) :: a, b
        integer, intent(in) :: n
        real(dp) :: integral

        real(dp) :: h, suma
        integer :: i

        h = (b - a) / real(n, dp)
        suma = 0.5_dp * (f(a) + f(b))

        do i = 1, n - 1
            suma = suma + f(a + real(i, dp) * h)
        end do

        integral = h * suma
    end function integrar_trapecio

    ! ----------------------------------------
    ! Funcion que retorna puntero a procedimiento
    ! ----------------------------------------

    function obtener_funcion(nombre) result(f)
        character(len=*), intent(in) :: nombre
        procedure(func_real), pointer :: f

        select case (trim(nombre))
            case ("cuadrado")
                f => cuadrado
            case ("cubo")
                f => cubo
            case ("seno")
                f => seno
            case ("exp")
                f => exponencial
            case default
                f => null()
        end select
    end function obtener_funcion

end module funciones_modulo


program punteros_procedimientos
    use funciones_modulo
    implicit none

    procedure(func_real), pointer :: mi_func
    real(dp) :: resultado

    ! Asignar puntero
    mi_func => cuadrado
    print '(A,F10.4)', "cuadrado(5) = ", mi_func(5.0_dp)

    mi_func => cubo
    print '(A,F10.4)', "cubo(3) = ", mi_func(3.0_dp)

    ! Integrar diferentes funciones
    mi_func => seno
    resultado = integrar_trapecio(mi_func, 0.0_dp, 3.14159265_dp, 1000)
    print '(A,F10.6)', "Integral de sin(x) de 0 a pi = ", resultado

    mi_func => exponencial
    resultado = integrar_trapecio(mi_func, 0.0_dp, 1.0_dp, 1000)
    print '(A,F10.6)', "Integral de exp(x) de 0 a 1 = ", resultado

    ! Obtener funcion por nombre
    mi_func => obtener_funcion("cuadrado")
    if (associated(mi_func)) then
        print '(A,F10.4)', "Funcion obtenida: cuadrado(4) = ", mi_func(4.0_dp)
    end if

end program punteros_procedimientos
```

### 5.2 ASSOCIATE Construct

```fortran
! ========================================
! ASSOCIATE - Alias temporales
! ========================================

program associate_demo
    implicit none

    type :: datos_complejos
        real :: matriz(100, 100)
        character(len=50) :: nombre_muy_largo
        integer :: indices(3)
    end type datos_complejos

    type(datos_complejos) :: estructura
    real :: resultado
    integer :: i, j

    ! Inicializar
    estructura%nombre_muy_largo = "Nombre descriptivo largo"
    estructura%indices = [10, 20, 30]

    do i = 1, 100
        do j = 1, 100
            estructura%matriz(i, j) = real(i + j)
        end do
    end do

    ! ----------------------------------------
    ! ASSOCIATE simplifica codigo
    ! ----------------------------------------

    ! Sin ASSOCIATE (verboso):
    resultado = estructura%matriz(estructura%indices(1), estructura%indices(2)) + &
                estructura%matriz(estructura%indices(2), estructura%indices(3))

    ! Con ASSOCIATE (claro):
    associate(m => estructura%matriz, &
              i1 => estructura%indices(1), &
              i2 => estructura%indices(2), &
              i3 => estructura%indices(3))

        resultado = m(i1, i2) + m(i2, i3)

        ! Los alias son modificables si el original lo es
        m(i1, i1) = 999.0

        print '(A,F10.2)', "Resultado: ", resultado
        print '(A,F10.2)', "m(i1,i1) = ", m(i1, i1)

    end associate

    ! ----------------------------------------
    ! ASSOCIATE con expresiones
    ! ----------------------------------------

    real :: x, y
    x = 3.0
    y = 4.0

    associate(hipotenusa => sqrt(x**2 + y**2), &
              angulo => atan2(y, x))

        print '(A,F10.4)', "Hipotenusa: ", hipotenusa
        print '(A,F10.4)', "Angulo (rad): ", angulo

        ! hipotenusa y angulo son SOLO LECTURA
        ! (porque son expresiones, no variables)

    end associate

    ! ----------------------------------------
    ! ASSOCIATE anidado
    ! ----------------------------------------

    associate(fila1 => estructura%matriz(1, :))
        associate(promedio => sum(fila1) / size(fila1))
            print '(A,F10.4)', "Promedio fila 1: ", promedio
        end associate
    end associate

end program associate_demo
```

### 5.3 BLOCK Construct

```fortran
! ========================================
! BLOCK - Scope local dentro de codigo
! ========================================

program block_demo
    implicit none

    integer :: i, j
    real :: x

    x = 10.0
    print '(A,F10.2)', "x inicial: ", x

    ! ----------------------------------------
    ! BLOCK crea scope local
    ! ----------------------------------------

    block
        ! Variables locales al bloque
        real :: x, y, temp    ! Este 'x' es diferente del externo
        integer :: k

        x = 99.0
        y = 50.0
        temp = x
        x = y
        y = temp

        print '(A,F10.2)', "x dentro del block: ", x

    end block   ! Variables locales se destruyen aqui

    print '(A,F10.2)', "x despues del block: ", x  ! Sigue siendo 10.0

    ! ----------------------------------------
    ! BLOCK para inicializacion local
    ! ----------------------------------------

    do i = 1, 5
        block
            real :: factor
            factor = real(i) * 0.5
            print '(A,I0,A,F6.2)', "Iteracion ", i, ", factor = ", factor
        end block
    end do

    ! ----------------------------------------
    ! BLOCK con ALLOCATABLE local
    ! ----------------------------------------

    block
        real, allocatable :: temp_array(:)

        allocate(temp_array(1000))
        temp_array = 0.0

        ! Usar temp_array...
        temp_array(500) = 123.456
        print '(A,F10.3)', "temp_array(500) = ", temp_array(500)

        ! deallocate es automatico al salir del block
    end block

    ! ----------------------------------------
    ! BLOCK con nombre
    ! ----------------------------------------

    outer_block: block
        integer :: resultado

        inner_block: block
            resultado = 42

            if (resultado == 42) exit inner_block

            resultado = 0  ! Esto no se ejecuta
        end block inner_block

        print '(A,I0)', "Resultado: ", resultado

    end block outer_block

    ! ----------------------------------------
    ! BLOCK para manejo de errores
    ! ----------------------------------------

    error_handler: block
        integer :: status
        real, allocatable :: datos(:)

        allocate(datos(1000000000), stat=status)  ! Puede fallar

        if (status /= 0) then
            print '(A)', "ERROR: No se pudo asignar memoria"
            exit error_handler
        end if

        ! Usar datos...
        print '(A)', "Memoria asignada exitosamente"

        deallocate(datos)
    end block error_handler

end program block_demo
```

### 5.4 IMPORT Statement

```fortran
! ========================================
! IMPORT - Importar entidades al scope
! ========================================

module tipos_modulo
    implicit none

    integer, parameter :: dp = selected_real_kind(15, 307)

    type :: vector3d
        real(dp) :: x, y, z
    end type vector3d

end module tipos_modulo


module interfaces_modulo
    use tipos_modulo
    implicit none

    ! ----------------------------------------
    ! IMPORT en interfaces
    ! ----------------------------------------

    interface
        ! Sin IMPORT, dp y vector3d no serian visibles
        function crear_vector_externo(x, y, z) result(v)
            import :: dp, vector3d   ! Importa del scope padre
            real(dp), intent(in) :: x, y, z
            type(vector3d) :: v
        end function crear_vector_externo

        ! IMPORT ALL - importa todo (F2018)
        function normalizar_externo(v) result(vn)
            import    ! Importa todo del host
            type(vector3d), intent(in) :: v
            type(vector3d) :: vn
        end function normalizar_externo

        ! IMPORT NONE - no importa nada
        subroutine procedimiento_simple(x)
            import, none
            ! Solo tipos intrinsecos disponibles
            integer, intent(in) :: x
        end subroutine procedimiento_simple

        ! IMPORT ONLY (F2018) - importa selectivamente
        function calcular_magnitud(v) result(mag)
            import, only: dp, vector3d
            type(vector3d), intent(in) :: v
            real(dp) :: mag
        end function calcular_magnitud
    end interface

    ! ----------------------------------------
    ! IMPORT en BLOCK
    ! ----------------------------------------

contains

    subroutine ejemplo_import_block()
        integer :: x

        x = 10

        block
            import :: x   ! Importa x del scope padre
            integer :: y
            y = x * 2
            print *, "y = ", y
        end block

    end subroutine ejemplo_import_block

end module interfaces_modulo
```

---

## 6. Manejo de Excepciones IEEE

> *"IEEE 754 definio las reglas del punto flotante. Fortran 2003 le dio al programador el poder de controlarlas - infinitos, NaN, underflow ya no son misterios sino eventos manejables."*

### 6.1 IEEE_EXCEPTIONS

```fortran
! ========================================
! MANEJO DE EXCEPCIONES IEEE
! ========================================

program ieee_excepciones_demo
    use, intrinsic :: ieee_exceptions
    use, intrinsic :: ieee_arithmetic
    implicit none

    real :: x, y, resultado
    logical :: flag_overflow, flag_divzero, flag_invalid
    type(ieee_flag_type) :: flags_activos(5)
    type(ieee_status_type) :: estado_guardado

    ! ----------------------------------------
    ! LIMPIAR FLAGS DE EXCEPCION
    ! ----------------------------------------

    call ieee_set_flag(ieee_all, .false.)

    ! ----------------------------------------
    ! DETECTAR OVERFLOW
    ! ----------------------------------------

    x = huge(x)  ! Valor maximo representable
    y = 2.0

    resultado = x * y  ! Esto causa overflow

    call ieee_get_flag(ieee_overflow, flag_overflow)

    if (flag_overflow) then
        print '(A)', "ADVERTENCIA: Se detecto overflow"
        print '(A,E15.7)', "Resultado: ", resultado
    end if

    call ieee_set_flag(ieee_overflow, .false.)  ! Limpiar flag

    ! ----------------------------------------
    ! DETECTAR DIVISION POR CERO
    ! ----------------------------------------

    x = 1.0
    y = 0.0

    resultado = x / y  ! Division por cero

    call ieee_get_flag(ieee_divide_by_zero, flag_divzero)

    if (flag_divzero) then
        print '(A)', "ADVERTENCIA: Division por cero"

        if (ieee_is_nan(resultado)) then
            print '(A)', "Resultado es NaN"
        else if (.not. ieee_is_finite(resultado)) then
            print '(A)', "Resultado es Infinito"
        end if
    end if

    call ieee_set_flag(ieee_divide_by_zero, .false.)

    ! ----------------------------------------
    ! DETECTAR OPERACION INVALIDA
    ! ----------------------------------------

    x = -1.0
    resultado = sqrt(x)  ! Raiz de negativo

    call ieee_get_flag(ieee_invalid, flag_invalid)

    if (flag_invalid) then
        print '(A)', "ADVERTENCIA: Operacion invalida (sqrt de negativo)"
        print '(A,L1)', "Resultado es NaN: ", ieee_is_nan(resultado)
    end if

    call ieee_set_flag(ieee_invalid, .false.)

    ! ----------------------------------------
    ! GUARDAR Y RESTAURAR ESTADO
    ! ----------------------------------------

    ! Guardar estado actual de flags
    call ieee_get_status(estado_guardado)

    ! Hacer operaciones que pueden fallar...
    resultado = log(0.0)  ! Genera infinito negativo

    ! Restaurar estado (ignora excepciones)
    call ieee_set_status(estado_guardado)

    ! ----------------------------------------
    ! CONTROLAR HALTING (detener programa)
    ! ----------------------------------------

    logical :: soportado, halting_actual

    ! Verificar si se puede controlar halting
    call ieee_get_halting_mode(ieee_overflow, halting_actual)

    ! Desactivar halting para overflow (programa continua)
    call ieee_set_halting_mode(ieee_overflow, .false.)

    ! Activar halting para division por cero (programa para)
    ! call ieee_set_halting_mode(ieee_divide_by_zero, .true.)

    ! ----------------------------------------
    ! OBTENER TODOS LOS FLAGS ACTIVOS
    ! ----------------------------------------

    call ieee_set_flag(ieee_all, .false.)

    ! Generar varias excepciones
    resultado = huge(x) * 2.0   ! overflow
    resultado = 1.0 / 0.0       ! div by zero
    resultado = sqrt(-1.0)       ! invalid

    call ieee_get_flag(ieee_overflow, flag_overflow)
    call ieee_get_flag(ieee_divide_by_zero, flag_divzero)
    call ieee_get_flag(ieee_invalid, flag_invalid)

    print '(/,A)', "=== Resumen de excepciones ==="
    print '(A,L1)', "Overflow: ", flag_overflow
    print '(A,L1)', "Div by zero: ", flag_divzero
    print '(A,L1)', "Invalid: ", flag_invalid

end program ieee_excepciones_demo
```

### 6.2 IEEE_ARITHMETIC

```fortran
! ========================================
! ARITMETICA IEEE - Consultas y valores especiales
! ========================================

program ieee_aritmetica_demo
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: ieee_exceptions
    implicit none

    real :: x, positivo_inf, negativo_inf, nan_valor
    real :: tiny_val, huge_val
    real(8) :: x_doble

    ! ----------------------------------------
    ! VALORES ESPECIALES IEEE
    ! ----------------------------------------

    ! Obtener infinitos
    positivo_inf = ieee_value(x, ieee_positive_inf)
    negativo_inf = ieee_value(x, ieee_negative_inf)

    print '(A,E15.7)', "+Infinito: ", positivo_inf
    print '(A,E15.7)', "-Infinito: ", negativo_inf

    ! Obtener NaN (Not a Number)
    nan_valor = ieee_value(x, ieee_quiet_nan)
    print '(A,E15.7)', "NaN: ", nan_valor

    ! Otros valores especiales
    print '(/,A)', "=== Valores especiales ==="
    print '(A,E15.7)', "Cero positivo: ", ieee_value(x, ieee_positive_zero)
    print '(A,E15.7)', "Cero negativo: ", ieee_value(x, ieee_negative_zero)
    print '(A,E15.7)', "Normal positivo: ", ieee_value(x, ieee_positive_normal)
    print '(A,E15.7)', "Denormal positivo: ", ieee_value(x, ieee_positive_denormal)

    ! ----------------------------------------
    ! FUNCIONES DE CONSULTA
    ! ----------------------------------------

    x = 3.14

    print '(/,A)', "=== Consultas para x = 3.14 ==="
    print '(A,L1)', "Es finito: ", ieee_is_finite(x)
    print '(A,L1)', "Es NaN: ", ieee_is_nan(x)
    print '(A,L1)', "Es normal: ", ieee_is_normal(x)
    print '(A,L1)', "Es negativo: ", ieee_is_negative(x)

    ! Consultas para infinito
    print '(/,A)', "=== Consultas para +Infinito ==="
    print '(A,L1)', "Es finito: ", ieee_is_finite(positivo_inf)
    print '(A,L1)', "Es NaN: ", ieee_is_nan(positivo_inf)

    ! Consultas para NaN
    print '(/,A)', "=== Consultas para NaN ==="
    print '(A,L1)', "Es finito: ", ieee_is_finite(nan_valor)
    print '(A,L1)', "Es NaN: ", ieee_is_nan(nan_valor)
    print '(A,L1)', "NaN == NaN: ", nan_valor == nan_valor  ! Siempre FALSE!

    ! ----------------------------------------
    ! CLASIFICACION DE NUMEROS
    ! ----------------------------------------

    type(ieee_class_type) :: clase

    x = 0.0
    clase = ieee_class(x)
    print '(/,A)', "Clase de 0.0: positivo_zero"

    x = 1.0e-40  ! Puede ser denormal
    clase = ieee_class(x)

    select case (clase)
        case (ieee_positive_zero)
            print *, "Cero positivo"
        case (ieee_negative_zero)
            print *, "Cero negativo"
        case (ieee_positive_normal)
            print *, "Normal positivo"
        case (ieee_negative_normal)
            print *, "Normal negativo"
        case (ieee_positive_denormal)
            print *, "Denormal positivo"
        case (ieee_positive_inf)
            print *, "Infinito positivo"
        case (ieee_quiet_nan)
            print *, "NaN silencioso"
        case (ieee_signaling_nan)
            print *, "NaN senalizador"
    end select

    ! ----------------------------------------
    ! SOPORTE DE CARACTERISTICAS
    ! ----------------------------------------

    print '(/,A)', "=== Soporte IEEE ==="
    print '(A,L1)', "Aritmetica IEEE soportada: ", ieee_support_standard(x)
    print '(A,L1)', "Datatype IEEE: ", ieee_support_datatype(x)
    print '(A,L1)', "Infinito soportado: ", ieee_support_inf(x)
    print '(A,L1)', "NaN soportado: ", ieee_support_nan(x)
    print '(A,L1)', "Denormales soportados: ", ieee_support_denormal(x)

    ! ----------------------------------------
    ! MODOS DE REDONDEO
    ! ----------------------------------------

    type(ieee_round_type) :: modo_redondeo

    call ieee_get_rounding_mode(modo_redondeo)

    print '(/,A)', "=== Modo de redondeo ==="

    if (ieee_support_rounding(ieee_nearest, x)) then
        call ieee_set_rounding_mode(ieee_nearest)
        print *, "Modo: Redondeo al mas cercano"
    end if

    ! Otros modos:
    ! ieee_to_zero    - hacia cero (truncar)
    ! ieee_up         - hacia +infinito
    ! ieee_down       - hacia -infinito
    ! ieee_nearest    - al mas cercano (por defecto)

    ! ----------------------------------------
    ! FUNCIONES UTILES
    ! ----------------------------------------

    real :: a, b

    a = 1.0
    b = tiny(a)  ! Valor mas pequeno normalizado

    print '(/,A)', "=== Valores limite ==="
    print '(A,E15.7)', "TINY (menor normalizado): ", tiny(a)
    print '(A,E15.7)', "HUGE (mayor finito): ", huge(a)
    print '(A,E15.7)', "EPSILON (precision): ", epsilon(a)

    ! Siguiente/anterior representable
    print '(/,A)', "=== Vecinos de 1.0 ==="
    print '(A,E20.13)', "1.0: ", 1.0
    print '(A,E20.13)', "Siguiente: ", ieee_next_after(1.0, 2.0)
    print '(A,E20.13)', "Anterior: ", ieee_next_after(1.0, 0.0)

end program ieee_aritmetica_demo
```

---

## 7. Referencia Rapida

### Modulos

```fortran
module nombre
    implicit none
    private
    public :: procedimiento_publico

    integer, parameter :: dp = selected_real_kind(15, 307)

contains
    subroutine procedimiento_publico()
    end subroutine
end module nombre

! Uso
use nombre, only: procedimiento_publico
```

### OOP - Tipos con Procedimientos

```fortran
type :: mi_tipo
    private
    real :: dato
contains
    procedure :: metodo
    procedure :: funcion
    final :: destructor
end type

! CLASS para polimorfismo
subroutine polimorfico(obj)
    class(mi_tipo), intent(in) :: obj
end subroutine
```

### Herencia

```fortran
type, extends(padre) :: hijo
    real :: dato_extra
contains
    procedure :: metodo => metodo_hijo
end type

! Clases abstractas
type, abstract :: base
contains
    procedure(interfaz), deferred :: metodo_abstracto
end type
```

### Interoperabilidad C

```fortran
use, intrinsic :: iso_c_binding

! Tipos: c_int, c_double, c_float, c_ptr, c_char

! Funcion llamable desde C
function mi_func(x) result(y) bind(C, name="mi_func")
    real(c_double), value :: x
    real(c_double) :: y
end function

! Tipo interoperable
type, bind(C) :: mi_struct
    integer(c_int) :: valor
end type
```

### Paralelismo

```fortran
! DO CONCURRENT
do concurrent (i = 1:n)
    a(i) = b(i) + c(i)
end do

! OpenMP
!$omp parallel do reduction(+:suma)
do i = 1, n
    suma = suma + a(i)
end do
!$omp end parallel do

! Coarrays
integer :: dato[*]      ! Coarray
sync all                ! Sincronizar
dato[2] = 100           ! Escribir a imagen 2
```

### Construcciones Modernas

```fortran
! ASSOCIATE
associate(x => estructura%componente_largo)
    resultado = x + 1
end associate

! BLOCK
block
    real :: temp
    temp = calcular()
end block

! IMPORT (en interfaces)
interface
    function f(x)
        import :: dp
        real(dp) :: x, f
    end function
end interface
```

### IEEE

```fortran
use, intrinsic :: ieee_exceptions
use, intrinsic :: ieee_arithmetic

! Detectar excepciones
call ieee_get_flag(ieee_overflow, flag)

! Valores especiales
inf = ieee_value(x, ieee_positive_inf)
nan = ieee_value(x, ieee_quiet_nan)

! Consultas
if (ieee_is_nan(x)) ...
if (ieee_is_finite(x)) ...
```

---

## Tabla de Versiones Fortran

| Version | Ano | Caracteristicas Principales |
|---------|-----|------------------------------|
| Fortran 90 | 1991 | Formato libre, modulos, arrays dinamicos, punteros |
| Fortran 95 | 1997 | FORALL, PURE, mejoras a arrays |
| Fortran 2003 | 2004 | OOP, interoperabilidad C, IEEE, allocatables mejorados |
| Fortran 2008 | 2010 | Coarrays, DO CONCURRENT, submodules |
| Fortran 2018 | 2018 | Mejoras coarrays, reduce en DO CONCURRENT |

---

## Conclusion

Fortran moderno no es el Fortran de tu abuelo. Desde los modulos que reemplazaron COMMON, pasando por la programacion orientada a objetos que permite estructurar codigo cientifico complejo, hasta los coarrays que hacen del paralelismo una ciudadano de primera clase - Fortran ha evolucionado sin perder su esencia: la eficiencia numerica.

**Puntos clave del Fortran moderno:**

1. **Modulos**: Encapsulacion y organizacion del codigo
2. **OOP**: Tipos derivados con procedimientos, herencia, polimorfismo
3. **Interoperabilidad C**: Puente hacia ecosistemas mas amplios
4. **Paralelismo nativo**: DO CONCURRENT y Coarrays
5. **Control IEEE**: Manejo preciso de excepciones de punto flotante

El codigo legacy vive en produccion, pero el codigo nuevo merece las herramientas modernas. Fortran 2018 demuestra que un lenguaje puede tener 60 anos y seguir siendo relevante.

---

*"Fortran evoluciono de FORmula TRANslator a arquitecto de supercomputadoras. Los que lo subestiman no han visto una simulacion climatica global corriendo en un millon de nucleos."*

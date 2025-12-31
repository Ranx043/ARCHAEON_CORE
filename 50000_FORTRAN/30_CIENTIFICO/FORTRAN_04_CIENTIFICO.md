# FORTRAN_04: COMPUTACION CIENTIFICA

> *"La ciencia avanza sobre los hombros de Fortran - donde cada bit de precision es una verdad numerica conquistada al caos del redondeo."*

## Indice
1. [Precision Numerica](#1-precision-numerica)
2. [Funciones Matematicas Intrinsecas](#2-funciones-matematicas-intrinsecas)
3. [Numeros Aleatorios](#3-numeros-aleatorios)
4. [Integracion Numerica](#4-integracion-numerica)
5. [Ecuaciones Diferenciales](#5-ecuaciones-diferenciales)
6. [Algebra Lineal Numerica](#6-algebra-lineal-numerica)
7. [FFT y Procesamiento de Senales](#7-fft-y-procesamiento-de-senales)

---

## 1. Precision Numerica

### 1.1 KIND y Tipos de Precision

```fortran
program precision_numerica
    implicit none

    ! ========================================
    ! SISTEMA DE KINDS EN FORTRAN
    ! ========================================
    !
    ! KIND define el "tipo" de almacenamiento
    ! Los valores de KIND son dependientes del compilador
    ! Para portabilidad, usar selected_real_kind
    !
    ! KIND tipicos:
    !   real(4)  -> 32 bits, ~7 digitos decimales
    !   real(8)  -> 64 bits, ~15 digitos decimales
    !   real(16) -> 128 bits, ~33 digitos decimales (si disponible)

    ! Declaracion directa con KIND
    real(kind=4) :: single_val      ! Precision simple
    real(kind=8) :: double_val      ! Doble precision
    real(kind=16) :: quad_val       ! Cuadruple precision

    ! Estilo legacy (aun valido)
    real :: x                       ! Depende del compilador
    double precision :: y           ! Tipicamente 64 bits

    ! ========================================
    ! SELECTED_REAL_KIND - Portabilidad
    ! ========================================
    !
    ! selected_real_kind(p, r)
    !   p = digitos decimales de precision
    !   r = rango exponencial
    !
    ! Retorna el KIND mas pequeno que satisface los requisitos
    ! Retorna -1 si no existe precision suficiente
    ! Retorna -2 si no existe rango suficiente
    ! Retorna -3 si ninguno existe

    integer, parameter :: sp = selected_real_kind(6, 37)    ! ~7 digitos
    integer, parameter :: dp = selected_real_kind(15, 307)  ! ~15 digitos
    integer, parameter :: qp = selected_real_kind(33, 4931) ! ~33 digitos

    real(sp) :: valor_simple
    real(dp) :: valor_doble
    real(qp) :: valor_quad

    ! Constantes con KIND especifico
    real(dp), parameter :: PI = 3.14159265358979323846_dp
    real(dp), parameter :: E  = 2.71828182845904523536_dp

    ! El sufijo _dp indica el KIND del literal
    valor_doble = 1.0_dp / 3.0_dp

    ! ========================================
    ! SELECTED_INT_KIND - Para enteros
    ! ========================================

    integer, parameter :: i16 = selected_int_kind(4)   ! Al menos 4 digitos
    integer, parameter :: i32 = selected_int_kind(9)   ! Al menos 9 digitos
    integer, parameter :: i64 = selected_int_kind(18)  ! Al menos 18 digitos

    integer(i32) :: contador
    integer(i64) :: numero_grande

    ! ========================================
    ! FUNCIONES DE CONSULTA DE PRECISION
    ! ========================================

    print *, "=== Precision Simple (sp) ==="
    print *, "KIND:", sp
    print *, "Digitos:", digits(valor_simple)
    print *, "Precision decimal:", precision(valor_simple)
    print *, "Rango exponencial:", range(valor_simple)
    print *, "Epsilon:", epsilon(valor_simple)
    print *, "Tiny:", tiny(valor_simple)
    print *, "Huge:", huge(valor_simple)

    print *, ""
    print *, "=== Doble Precision (dp) ==="
    print *, "KIND:", dp
    print *, "Digitos:", digits(valor_doble)
    print *, "Precision decimal:", precision(valor_doble)
    print *, "Rango exponencial:", range(valor_doble)
    print *, "Epsilon:", epsilon(valor_doble)
    print *, "Tiny:", tiny(valor_doble)
    print *, "Huge:", huge(valor_doble)

end program precision_numerica
```

### 1.2 IEEE 754 Floating Point

```fortran
program ieee_754
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: ieee_features
    implicit none

    ! ========================================
    ! MODULO IEEE_ARITHMETIC (Fortran 2003+)
    ! ========================================
    !
    ! Proporciona control sobre:
    ! - Valores especiales (Inf, NaN)
    ! - Modos de redondeo
    ! - Flags de excepcion
    ! - Aritmetica IEEE completa

    integer, parameter :: dp = selected_real_kind(15)
    real(dp) :: x, y, resultado
    type(ieee_round_type) :: modo_redondeo

    ! ========================================
    ! VALORES ESPECIALES IEEE
    ! ========================================

    ! Infinito positivo
    x = ieee_value(x, ieee_positive_inf)
    print *, "Infinito positivo:", x

    ! Infinito negativo
    y = ieee_value(y, ieee_negative_inf)
    print *, "Infinito negativo:", y

    ! NaN (Not a Number)
    resultado = ieee_value(resultado, ieee_quiet_nan)
    print *, "NaN silencioso:", resultado

    ! Cero negativo
    x = ieee_value(x, ieee_negative_zero)
    print *, "Cero negativo:", x

    ! ========================================
    ! VERIFICACION DE VALORES ESPECIALES
    ! ========================================

    x = 1.0_dp / 0.0_dp  ! Genera infinito

    if (ieee_is_finite(x)) then
        print *, "x es finito"
    else
        print *, "x NO es finito (es Inf o NaN)"
    end if

    if (ieee_is_nan(x)) then
        print *, "x es NaN"
    end if

    if (ieee_is_negative(x)) then
        print *, "x es negativo"
    end if

    ! Clasificacion completa
    select case (ieee_class(x))
        case (ieee_signaling_nan)
            print *, "NaN senalizador"
        case (ieee_quiet_nan)
            print *, "NaN silencioso"
        case (ieee_negative_inf)
            print *, "Infinito negativo"
        case (ieee_positive_inf)
            print *, "Infinito positivo"
        case (ieee_negative_normal)
            print *, "Numero normal negativo"
        case (ieee_positive_normal)
            print *, "Numero normal positivo"
        case (ieee_negative_denormal)
            print *, "Numero denormalizado negativo"
        case (ieee_positive_denormal)
            print *, "Numero denormalizado positivo"
        case (ieee_negative_zero)
            print *, "Cero negativo"
        case (ieee_positive_zero)
            print *, "Cero positivo"
    end select

    ! ========================================
    ! MODOS DE REDONDEO
    ! ========================================

    ! Obtener modo actual
    call ieee_get_rounding_mode(modo_redondeo)

    ! Tipos de redondeo:
    !   ieee_nearest       - Al mas cercano (default)
    !   ieee_to_zero       - Hacia cero (truncamiento)
    !   ieee_up            - Hacia +infinito
    !   ieee_down          - Hacia -infinito

    ! Cambiar modo
    call ieee_set_rounding_mode(ieee_to_zero)

    x = 1.0_dp / 3.0_dp
    print *, "1/3 con truncamiento:", x

    call ieee_set_rounding_mode(ieee_up)
    x = 1.0_dp / 3.0_dp
    print *, "1/3 hacia arriba:", x

    ! Restaurar modo original
    call ieee_set_rounding_mode(modo_redondeo)

end program ieee_754
```

### 1.3 Control de Errores de Redondeo

```fortran
module precision_control
    implicit none
    integer, parameter :: wp = selected_real_kind(15, 307)  ! Working precision

    ! Tolerancias para comparaciones
    real(wp), parameter :: TOL_ABS = 1.0e-10_wp
    real(wp), parameter :: TOL_REL = 1.0e-12_wp

contains

    ! ========================================
    ! COMPARACION SEGURA DE FLOTANTES
    ! ========================================

    pure logical function nearly_equal(a, b, tol)
        real(wp), intent(in) :: a, b
        real(wp), intent(in), optional :: tol
        real(wp) :: tolerance

        if (present(tol)) then
            tolerance = tol
        else
            tolerance = TOL_REL
        end if

        ! Comparacion relativa y absoluta combinada
        nearly_equal = abs(a - b) <= tolerance * max(abs(a), abs(b), 1.0_wp)
    end function nearly_equal

    ! ========================================
    ! SUMA DE KAHAN (Suma compensada)
    ! ========================================
    !
    ! Reduce error de redondeo en sumas largas
    ! Precision O(epsilon) en lugar de O(n*epsilon)

    pure function kahan_sum(arr) result(s)
        real(wp), intent(in) :: arr(:)
        real(wp) :: s
        real(wp) :: c, y, t
        integer :: i

        s = 0.0_wp
        c = 0.0_wp  ! Compensacion por error acumulado

        do i = 1, size(arr)
            y = arr(i) - c       ! Corregir por error previo
            t = s + y            ! Nueva suma parcial
            c = (t - s) - y      ! Error de esta operacion
            s = t
        end do
    end function kahan_sum

    ! ========================================
    ! SUMA PAIRWISE (Divide y venceras)
    ! ========================================
    !
    ! Otra tecnica para sumas precisas
    ! Precision O(log(n)*epsilon)

    recursive function pairwise_sum(arr) result(s)
        real(wp), intent(in) :: arr(:)
        real(wp) :: s
        integer :: n, mid

        n = size(arr)

        if (n == 0) then
            s = 0.0_wp
        else if (n == 1) then
            s = arr(1)
        else if (n <= 8) then
            ! Suma directa para arrays pequenos
            s = sum(arr)
        else
            mid = n / 2
            s = pairwise_sum(arr(1:mid)) + pairwise_sum(arr(mid+1:n))
        end if
    end function pairwise_sum

    ! ========================================
    ! PRODUCTO ESTABLE
    ! ========================================

    pure function stable_product(arr) result(p)
        real(wp), intent(in) :: arr(:)
        real(wp) :: p
        real(wp) :: log_sum
        integer :: i, sign_count

        ! Evitar overflow/underflow usando logaritmos
        log_sum = 0.0_wp
        sign_count = 0

        do i = 1, size(arr)
            if (arr(i) < 0.0_wp) sign_count = sign_count + 1
            log_sum = log_sum + log(abs(arr(i)))
        end do

        p = exp(log_sum)
        if (mod(sign_count, 2) == 1) p = -p
    end function stable_product

    ! ========================================
    ! ANALISIS DE ERROR NUMERICO
    ! ========================================

    subroutine analyze_precision(valor, nombre)
        real(wp), intent(in) :: valor
        character(*), intent(in) :: nombre

        print '(A)', '=== Analisis de ' // trim(nombre) // ' ==='
        print '(A, ES23.16)', 'Valor: ', valor
        print '(A, ES10.3)', 'Error relativo maquina: ', &
              epsilon(valor) * abs(valor)
        print '(A, I0)', 'Bits significativos: ', precision(valor) * 4
        print *
    end subroutine analyze_precision

end module precision_control


program test_precision
    use precision_control
    implicit none

    real(wp) :: arr(1000000)
    real(wp) :: suma_normal, suma_kahan, suma_pairwise
    integer :: i

    ! Crear array con valores pequenos
    arr = [(1.0e-10_wp, i=1,1000000)]

    ! Comparar metodos de suma
    suma_normal = sum(arr)
    suma_kahan = kahan_sum(arr)
    suma_pairwise = pairwise_sum(arr)

    print *, "Suma esperada:   ", 1.0e-4_wp
    print *, "Suma normal:     ", suma_normal
    print *, "Suma Kahan:      ", suma_kahan
    print *, "Suma Pairwise:   ", suma_pairwise
    print *, "Error normal:    ", abs(suma_normal - 1.0e-4_wp)
    print *, "Error Kahan:     ", abs(suma_kahan - 1.0e-4_wp)

end program test_precision
```

---

## 2. Funciones Matematicas Intrinsecas

### 2.1 Funciones Trigonometricas

```fortran
program funciones_trigonometricas
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    real(dp), parameter :: PI = 3.14159265358979323846_dp
    real(dp) :: angulo, resultado

    ! ========================================
    ! FUNCIONES TRIGONOMETRICAS BASICAS
    ! ========================================

    angulo = PI / 4.0_dp  ! 45 grados

    print *, "Angulo (rad):", angulo
    print *, "Angulo (grad):", angulo * 180.0_dp / PI

    ! Seno, coseno, tangente (argumento en radianes)
    print *, "sin(45):", sin(angulo)     ! 0.7071...
    print *, "cos(45):", cos(angulo)     ! 0.7071...
    print *, "tan(45):", tan(angulo)     ! 1.0

    ! ========================================
    ! FUNCIONES TRIGONOMETRICAS INVERSAS
    ! ========================================

    resultado = 0.5_dp

    print *, "asin(0.5):", asin(resultado)     ! PI/6 = 0.5236...
    print *, "acos(0.5):", acos(resultado)     ! PI/3 = 1.0472...
    print *, "atan(1.0):", atan(1.0_dp)        ! PI/4 = 0.7854...

    ! ATAN2 - Arcotangente de dos argumentos (cuadrante correcto)
    print *, "atan2(1,1):", atan2(1.0_dp, 1.0_dp)    ! PI/4
    print *, "atan2(1,-1):", atan2(1.0_dp, -1.0_dp)  ! 3*PI/4
    print *, "atan2(-1,-1):", atan2(-1.0_dp, -1.0_dp) ! -3*PI/4
    print *, "atan2(-1,1):", atan2(-1.0_dp, 1.0_dp)  ! -PI/4

    ! ========================================
    ! IDENTIDADES TRIGONOMETRICAS
    ! ========================================

    angulo = 0.7_dp

    print *, "Verificacion sin^2 + cos^2 = 1:"
    print *, sin(angulo)**2 + cos(angulo)**2

    print *, "Verificacion tan = sin/cos:"
    print *, tan(angulo), sin(angulo)/cos(angulo)

    ! ========================================
    ! GRADOS A RADIANES (F2018)
    ! ========================================
    !
    ! Fortran 2018 introduce funciones en grados:
    ! sind, cosd, tand - trigonometricas en grados
    ! asind, acosd, atand - inversas retornando grados

    ! En compiladores que soporten F2018:
    ! print *, sind(45.0_dp)   ! sin(45 grados) = 0.7071...

end program funciones_trigonometricas
```

### 2.2 Funciones Hiperbolicas

```fortran
program funciones_hiperbolicas
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    real(dp) :: x, resultado

    ! ========================================
    ! FUNCIONES HIPERBOLICAS
    ! ========================================
    !
    ! sinh(x) = (e^x - e^(-x)) / 2
    ! cosh(x) = (e^x + e^(-x)) / 2
    ! tanh(x) = sinh(x) / cosh(x)

    x = 1.0_dp

    print *, "sinh(1):", sinh(x)    ! 1.1752...
    print *, "cosh(1):", cosh(x)    ! 1.5431...
    print *, "tanh(1):", tanh(x)    ! 0.7616...

    ! Verificacion de definicion
    print *, "sinh via exp:", (exp(x) - exp(-x)) / 2.0_dp
    print *, "cosh via exp:", (exp(x) + exp(-x)) / 2.0_dp

    ! ========================================
    ! FUNCIONES HIPERBOLICAS INVERSAS
    ! ========================================

    resultado = 2.0_dp

    print *, "asinh(2):", asinh(resultado)   ! 1.4436...
    print *, "acosh(2):", acosh(resultado)   ! 1.3169...
    print *, "atanh(0.5):", atanh(0.5_dp)    ! 0.5493...

    ! ========================================
    ! IDENTIDADES HIPERBOLICAS
    ! ========================================

    x = 1.5_dp

    print *, "Verificacion cosh^2 - sinh^2 = 1:"
    print *, cosh(x)**2 - sinh(x)**2

    ! ========================================
    ! APLICACIONES
    ! ========================================

    ! Catenaria: y = a * cosh(x/a)
    real(dp) :: a, x_cat, y_cat
    a = 2.0_dp
    x_cat = 1.0_dp
    y_cat = a * cosh(x_cat / a)
    print *, "Catenaria en x=1, a=2:", y_cat

    ! Funcion logistica: 1/(1+e^(-x)) = (1 + tanh(x/2))/2
    x = 2.0_dp
    print *, "Logistica via tanh:", (1.0_dp + tanh(x/2.0_dp)) / 2.0_dp
    print *, "Logistica directa:", 1.0_dp / (1.0_dp + exp(-x))

end program funciones_hiperbolicas
```

### 2.3 Funciones Exponenciales y Logaritmicas

```fortran
program funciones_exp_log
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    real(dp), parameter :: E = 2.71828182845904523536_dp
    real(dp) :: x, base, resultado

    ! ========================================
    ! FUNCION EXPONENCIAL
    ! ========================================

    x = 2.0_dp

    print *, "exp(2):", exp(x)         ! e^2 = 7.389...
    print *, "exp(1):", exp(1.0_dp)    ! e = 2.718...
    print *, "exp(0):", exp(0.0_dp)    ! 1

    ! ========================================
    ! FUNCIONES LOGARITMICAS
    ! ========================================

    x = 10.0_dp

    ! Logaritmo natural (base e)
    print *, "log(10):", log(x)        ! 2.302...

    ! Logaritmo base 10
    print *, "log10(10):", log10(x)    ! 1.0
    print *, "log10(100):", log10(100.0_dp)  ! 2.0

    ! Logaritmo base 2 (Fortran 2008)
    print *, "log(8)/log(2):", log(8.0_dp)/log(2.0_dp)  ! 3.0

    ! Cambio de base: log_b(x) = log(x) / log(b)
    base = 3.0_dp
    x = 27.0_dp
    print *, "log_3(27):", log(x) / log(base)  ! 3.0

    ! ========================================
    ! IDENTIDADES
    ! ========================================

    x = 5.0_dp

    print *, "exp(log(x)) = x:", exp(log(x))
    print *, "log(exp(x)) = x:", log(exp(x))
    print *, "10^(log10(x)) = x:", 10.0_dp ** log10(x)

    ! ========================================
    ! POTENCIAS Y RAICES
    ! ========================================

    x = 27.0_dp

    print *, "sqrt(27):", sqrt(x)          ! Raiz cuadrada
    print *, "27^(1/3):", x ** (1.0_dp/3.0_dp)  ! Raiz cubica
    print *, "27^0.5:", x ** 0.5_dp            ! Igual que sqrt

    ! Para raices n-esimas de numeros negativos, usar complejos
    complex(dp) :: z
    z = (-8.0_dp, 0.0_dp)
    print *, "(-8)^(1/3) complejo:", z ** (1.0_dp/3.0_dp)

end program funciones_exp_log
```

### 2.4 Funciones Especiales (Fortran 2008+)

```fortran
program funciones_especiales
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    real(dp) :: x, orden
    integer :: n

    ! ========================================
    ! FUNCIONES DE BESSEL (Fortran 2008)
    ! ========================================
    !
    ! Bessel J_n(x) - Primera especie
    ! Bessel Y_n(x) - Segunda especie (Neumann)
    !
    ! Aplicaciones: ondas, vibraciones, difusion

    x = 2.5_dp

    ! Bessel de primera especie
    print *, "J_0(2.5):", bessel_j0(x)      ! Orden 0
    print *, "J_1(2.5):", bessel_j1(x)      ! Orden 1
    print *, "J_5(2.5):", bessel_jn(5, x)   ! Orden n

    ! Bessel de segunda especie
    print *, "Y_0(2.5):", bessel_y0(x)
    print *, "Y_1(2.5):", bessel_y1(x)
    print *, "Y_5(2.5):", bessel_yn(5, x)

    ! ========================================
    ! FUNCION GAMMA Y RELACIONADAS
    ! ========================================
    !
    ! Gamma(n) = (n-1)! para enteros positivos
    ! Gamma(x) = integral de 0 a inf de t^(x-1) * e^(-t) dt

    x = 5.0_dp
    print *, "Gamma(5) = 4!:", gamma(x)     ! 24.0
    print *, "Gamma(0.5) = sqrt(pi):", gamma(0.5_dp)

    ! Log-Gamma (para valores grandes evita overflow)
    print *, "log(Gamma(100)):", log_gamma(100.0_dp)

    ! ========================================
    ! FUNCION ERROR (erf, erfc)
    ! ========================================
    !
    ! erf(x) = (2/sqrt(pi)) * integral de 0 a x de e^(-t^2) dt
    ! erfc(x) = 1 - erf(x) (complemento)
    !
    ! Aplicaciones: probabilidad, estadistica, difusion

    x = 1.0_dp
    print *, "erf(1):", erf(x)       ! 0.8427...
    print *, "erfc(1):", erfc(x)     ! 0.1573...

    ! Verificacion
    print *, "erf(x) + erfc(x) = 1:", erf(x) + erfc(x)

    ! ========================================
    ! FUNCIONES HIPERGEOMETRICAS (via series)
    ! ========================================

    ! No hay intrinseca, pero podemos implementar:

contains

    ! Funcion hipergeometrica confluente 1F1(a;b;x)
    function hypergeom_1f1(a, b, x) result(suma)
        real(dp), intent(in) :: a, b, x
        real(dp) :: suma
        real(dp) :: term
        integer :: k
        integer, parameter :: max_iter = 200
        real(dp), parameter :: tol = 1.0e-15_dp

        suma = 1.0_dp
        term = 1.0_dp

        do k = 1, max_iter
            term = term * (a + k - 1) * x / ((b + k - 1) * k)
            suma = suma + term
            if (abs(term) < tol * abs(suma)) exit
        end do
    end function hypergeom_1f1

end program funciones_especiales
```

### 2.5 Funciones de Numeros Complejos

```fortran
program numeros_complejos
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    complex(dp) :: z1, z2, resultado
    real(dp) :: magnitud, fase

    ! ========================================
    ! CREACION DE NUMEROS COMPLEJOS
    ! ========================================

    z1 = (3.0_dp, 4.0_dp)                    ! Constructor directo
    z2 = cmplx(1.0_dp, 2.0_dp, kind=dp)      ! Funcion cmplx

    ! Desde polar: z = r * e^(i*theta)
    magnitud = 5.0_dp
    fase = 0.927295_dp  ! arctan(4/3)
    resultado = cmplx(magnitud * cos(fase), magnitud * sin(fase), dp)

    ! ========================================
    ! FUNCIONES BASICAS
    ! ========================================

    z1 = (3.0_dp, 4.0_dp)

    print *, "z1 =", z1

    ! Parte real e imaginaria
    print *, "Parte real:", real(z1)         ! 3.0
    print *, "Parte real (dp):", real(z1, dp)
    print *, "Parte imaginaria:", aimag(z1)  ! 4.0

    ! Conjugado
    print *, "Conjugado:", conjg(z1)         ! (3, -4)

    ! Magnitud (modulo)
    print *, "Magnitud:", abs(z1)            ! 5.0

    ! Fase (argumento)
    print *, "Fase:", atan2(aimag(z1), real(z1))  ! 0.927...

    ! ========================================
    ! OPERACIONES ARITMETICAS
    ! ========================================

    z1 = (3.0_dp, 4.0_dp)
    z2 = (1.0_dp, 2.0_dp)

    print *, "z1 + z2 =", z1 + z2
    print *, "z1 - z2 =", z1 - z2
    print *, "z1 * z2 =", z1 * z2    ! (3*1-4*2, 3*2+4*1) = (-5, 10)
    print *, "z1 / z2 =", z1 / z2
    print *, "z1 ** 2 =", z1 ** 2

    ! ========================================
    ! FUNCIONES TRASCENDENTALES
    ! ========================================

    z1 = (1.0_dp, 1.0_dp)

    print *, "exp(z):", exp(z1)
    print *, "log(z):", log(z1)
    print *, "sqrt(z):", sqrt(z1)

    print *, "sin(z):", sin(z1)
    print *, "cos(z):", cos(z1)
    print *, "tan(z):", tan(z1)

    ! ========================================
    ! FORMULA DE EULER
    ! ========================================

    real(dp), parameter :: PI = 3.14159265358979323846_dp
    complex(dp), parameter :: I = (0.0_dp, 1.0_dp)

    ! e^(i*pi) + 1 = 0
    resultado = exp(I * PI) + 1.0_dp
    print *, "e^(i*pi) + 1 =", resultado  ! Aproximadamente 0

    ! e^(i*theta) = cos(theta) + i*sin(theta)
    fase = PI / 4.0_dp
    print *, "e^(i*pi/4):", exp(I * fase)
    print *, "cos + i*sin:", cmplx(cos(fase), sin(fase), dp)

    ! ========================================
    ! RAICES DE NUMEROS COMPLEJOS
    ! ========================================

    ! Raices n-esimas de z
    z1 = (1.0_dp, 0.0_dp)  ! Raices de la unidad
    integer :: k
    complex(dp) :: raiz

    print *, "Raices cubicas de 1:"
    do k = 0, 2
        raiz = exp(I * 2.0_dp * PI * k / 3.0_dp)
        print *, k, raiz
    end do

end program numeros_complejos
```

---

## 3. Numeros Aleatorios

### 3.1 RANDOM_NUMBER y RANDOM_SEED

```fortran
program numeros_aleatorios
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    real(dp) :: x, arr(10)
    integer :: i

    ! ========================================
    ! RANDOM_NUMBER - Generador uniforme [0,1)
    ! ========================================

    ! Un solo valor
    call random_number(x)
    print *, "Aleatorio simple:", x

    ! Array de valores
    call random_number(arr)
    print *, "Array aleatorio:"
    do i = 1, 10
        print '(I3, F12.8)', i, arr(i)
    end do

    ! ========================================
    ! ESCALAR A DIFERENTES RANGOS
    ! ========================================

    real(dp) :: a, b, valor

    ! Uniforme [a, b]
    a = 5.0_dp
    b = 15.0_dp
    call random_number(x)
    valor = a + (b - a) * x
    print *, "Uniforme [5, 15]:", valor

    ! Entero aleatorio [1, n]
    integer :: n, entero_aleatorio
    n = 6
    call random_number(x)
    entero_aleatorio = int(x * n) + 1  ! Dado de 6 caras
    print *, "Dado (1-6):", entero_aleatorio

    ! ========================================
    ! RANDOM_SEED - Control del generador
    ! ========================================

    integer :: seed_size
    integer, allocatable :: seed(:)

    ! Obtener tamano de semilla
    call random_seed(size=seed_size)
    print *, "Tamano de semilla:", seed_size

    allocate(seed(seed_size))

    ! Obtener semilla actual
    call random_seed(get=seed)
    print *, "Semilla actual:", seed

    ! Establecer semilla para reproducibilidad
    seed = 12345
    call random_seed(put=seed)

    ! Ahora la secuencia es reproducible
    call random_number(x)
    print *, "Despues de seed=12345:", x

    ! Resetear con misma semilla
    seed = 12345
    call random_seed(put=seed)
    call random_number(x)
    print *, "Mismo valor:", x

    ! Inicializar con fuente del sistema (no reproducible)
    call random_seed()  ! Usa reloj del sistema u otra fuente

    deallocate(seed)

end program numeros_aleatorios
```

### 3.2 Distribuciones Estadisticas

```fortran
module distribuciones
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    real(dp), parameter :: PI = 3.14159265358979323846_dp

contains

    ! ========================================
    ! DISTRIBUCION UNIFORME [a, b]
    ! ========================================

    function uniforme(a, b) result(x)
        real(dp), intent(in) :: a, b
        real(dp) :: x, u

        call random_number(u)
        x = a + (b - a) * u
    end function uniforme

    ! ========================================
    ! DISTRIBUCION NORMAL (Box-Muller)
    ! ========================================
    !
    ! Transforma dos uniformes en dos normales estandar
    ! Metodo Box-Muller

    subroutine normal_box_muller(z1, z2)
        real(dp), intent(out) :: z1, z2
        real(dp) :: u1, u2, r, theta

        call random_number(u1)
        call random_number(u2)

        ! Evitar log(0)
        do while (u1 <= 0.0_dp)
            call random_number(u1)
        end do

        r = sqrt(-2.0_dp * log(u1))
        theta = 2.0_dp * PI * u2

        z1 = r * cos(theta)
        z2 = r * sin(theta)
    end subroutine normal_box_muller

    ! Normal con media mu y desviacion sigma
    function normal(mu, sigma) result(x)
        real(dp), intent(in) :: mu, sigma
        real(dp) :: x
        real(dp) :: z1, z2

        call normal_box_muller(z1, z2)
        x = mu + sigma * z1
    end function normal

    ! ========================================
    ! DISTRIBUCION EXPONENCIAL
    ! ========================================
    !
    ! f(x) = lambda * exp(-lambda * x)
    ! Usando transformada inversa

    function exponencial(lambda) result(x)
        real(dp), intent(in) :: lambda
        real(dp) :: x, u

        call random_number(u)
        do while (u <= 0.0_dp)
            call random_number(u)
        end do

        x = -log(u) / lambda
    end function exponencial

    ! ========================================
    ! DISTRIBUCION DE POISSON
    ! ========================================
    !
    ! Para lambda pequeno, usa algoritmo de Knuth

    function poisson(lambda) result(k)
        real(dp), intent(in) :: lambda
        integer :: k
        real(dp) :: L, p, u

        L = exp(-lambda)
        k = 0
        p = 1.0_dp

        do
            call random_number(u)
            p = p * u
            if (p <= L) exit
            k = k + 1
        end do
    end function poisson

    ! ========================================
    ! DISTRIBUCION BINOMIAL
    ! ========================================
    !
    ! Numero de exitos en n ensayos con probabilidad p

    function binomial(n, p) result(k)
        integer, intent(in) :: n
        real(dp), intent(in) :: p
        integer :: k
        integer :: i
        real(dp) :: u

        k = 0
        do i = 1, n
            call random_number(u)
            if (u < p) k = k + 1
        end do
    end function binomial

    ! ========================================
    ! DISTRIBUCION GAMMA
    ! ========================================
    !
    ! Para shape >= 1, usa metodo de Marsaglia y Tsang

    function gamma_dist(shape, scale) result(x)
        real(dp), intent(in) :: shape, scale
        real(dp) :: x
        real(dp) :: d, c, v, u, z1, z2

        if (shape < 1.0_dp) then
            ! Para shape < 1, usar transformacion
            call random_number(u)
            x = gamma_dist(shape + 1.0_dp, scale) * u**(1.0_dp/shape)
            return
        end if

        d = shape - 1.0_dp/3.0_dp
        c = 1.0_dp / sqrt(9.0_dp * d)

        do
            do
                call normal_box_muller(z1, z2)
                v = (1.0_dp + c * z1)**3
                if (v > 0.0_dp) exit
            end do

            call random_number(u)

            if (u < 1.0_dp - 0.0331_dp * z1**4) then
                x = d * v * scale
                return
            end if

            if (log(u) < 0.5_dp * z1**2 + d * (1.0_dp - v + log(v))) then
                x = d * v * scale
                return
            end if
        end do
    end function gamma_dist

    ! ========================================
    ! DISTRIBUCION CHI-CUADRADO
    ! ========================================
    !
    ! Chi-cuadrado con k grados de libertad = Gamma(k/2, 2)

    function chi_cuadrado(k) result(x)
        integer, intent(in) :: k
        real(dp) :: x

        x = gamma_dist(real(k, dp) / 2.0_dp, 2.0_dp)
    end function chi_cuadrado

    ! ========================================
    ! MUESTREO DE PERMUTACION (Fisher-Yates)
    ! ========================================

    subroutine shuffle(arr)
        integer, intent(inout) :: arr(:)
        integer :: n, i, j, temp
        real(dp) :: u

        n = size(arr)

        do i = n, 2, -1
            call random_number(u)
            j = int(u * i) + 1
            temp = arr(i)
            arr(i) = arr(j)
            arr(j) = temp
        end do
    end subroutine shuffle

end module distribuciones


program test_distribuciones
    use distribuciones
    implicit none

    integer :: i
    real(dp) :: x, suma, suma2, media, varianza
    integer :: k
    real(dp) :: datos(10000)

    print *, "=== Distribucion Normal N(0,1) ==="
    suma = 0.0_dp
    suma2 = 0.0_dp
    do i = 1, 10000
        x = normal(0.0_dp, 1.0_dp)
        datos(i) = x
        suma = suma + x
        suma2 = suma2 + x**2
    end do
    media = suma / 10000.0_dp
    varianza = suma2 / 10000.0_dp - media**2
    print *, "Media (esperada 0):", media
    print *, "Varianza (esperada 1):", varianza

    print *, ""
    print *, "=== Distribucion Poisson(5) ==="
    suma = 0.0_dp
    do i = 1, 10000
        k = poisson(5.0_dp)
        suma = suma + real(k, dp)
    end do
    print *, "Media (esperada 5):", suma / 10000.0_dp

    print *, ""
    print *, "=== Shuffle de [1..10] ==="
    integer :: arr(10) = [(i, i=1,10)]
    call shuffle(arr)
    print *, "Permutacion:", arr

end program test_distribuciones
```

---

## 4. Integracion Numerica

### 4.1 Metodo del Trapecio

```fortran
module integracion_trapecio
    implicit none
    integer, parameter :: dp = selected_real_kind(15)

contains

    ! ========================================
    ! REGLA DEL TRAPECIO SIMPLE
    ! ========================================
    !
    ! Aproxima el area bajo la curva con un trapecio
    ! I = (b-a) * (f(a) + f(b)) / 2
    !
    ! Error: O(h^3 * f'')

    function trapecio_simple(f, a, b) result(integral)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: a, b
        real(dp) :: integral

        integral = (b - a) * (f(a) + f(b)) / 2.0_dp
    end function trapecio_simple

    ! ========================================
    ! REGLA DEL TRAPECIO COMPUESTA
    ! ========================================
    !
    ! Divide [a,b] en n subintervalos
    ! I = h * (f(a)/2 + sum(f(x_i)) + f(b)/2)
    !
    ! Error: O(h^2) donde h = (b-a)/n

    function trapecio_compuesto(f, a, b, n) result(integral)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: a, b
        integer, intent(in) :: n
        real(dp) :: integral

        real(dp) :: h, x, suma
        integer :: i

        h = (b - a) / real(n, dp)
        suma = (f(a) + f(b)) / 2.0_dp

        do i = 1, n - 1
            x = a + real(i, dp) * h
            suma = suma + f(x)
        end do

        integral = h * suma
    end function trapecio_compuesto

    ! ========================================
    ! TRAPECIO ADAPTATIVO
    ! ========================================
    !
    ! Refina automaticamente donde se necesita

    recursive function trapecio_adaptativo(f, a, b, tol, nivel_max, nivel) result(integral)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: a, b, tol
        integer, intent(in) :: nivel_max
        integer, intent(in), optional :: nivel
        real(dp) :: integral

        real(dp) :: c, I1, I2, I_izq, I_der
        integer :: nivel_actual

        if (present(nivel)) then
            nivel_actual = nivel
        else
            nivel_actual = 0
        end if

        c = (a + b) / 2.0_dp

        ! Integral con un trapecio
        I1 = (b - a) * (f(a) + f(b)) / 2.0_dp

        ! Integral con dos trapecios
        I_izq = (c - a) * (f(a) + f(c)) / 2.0_dp
        I_der = (b - c) * (f(c) + f(b)) / 2.0_dp
        I2 = I_izq + I_der

        ! Verificar convergencia
        if (nivel_actual >= nivel_max .or. abs(I2 - I1) < 3.0_dp * tol) then
            integral = I2 + (I2 - I1) / 3.0_dp  ! Extrapolacion de Richardson
        else
            ! Subdividir
            integral = trapecio_adaptativo(f, a, c, tol/2.0_dp, nivel_max, nivel_actual + 1) + &
                       trapecio_adaptativo(f, c, b, tol/2.0_dp, nivel_max, nivel_actual + 1)
        end if
    end function trapecio_adaptativo

end module integracion_trapecio
```

### 4.2 Metodo de Simpson

```fortran
module integracion_simpson
    implicit none
    integer, parameter :: dp = selected_real_kind(15)

contains

    ! ========================================
    ! REGLA DE SIMPSON 1/3 SIMPLE
    ! ========================================
    !
    ! Usa parabola a traves de 3 puntos
    ! I = (b-a)/6 * (f(a) + 4*f((a+b)/2) + f(b))
    !
    ! Error: O(h^5 * f'''')

    function simpson_simple(f, a, b) result(integral)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: a, b
        real(dp) :: integral

        real(dp) :: c

        c = (a + b) / 2.0_dp
        integral = (b - a) / 6.0_dp * (f(a) + 4.0_dp * f(c) + f(b))
    end function simpson_simple

    ! ========================================
    ! REGLA DE SIMPSON 1/3 COMPUESTA
    ! ========================================
    !
    ! n debe ser par
    ! I = h/3 * (f(a) + 4*sum_impares + 2*sum_pares + f(b))

    function simpson_compuesto(f, a, b, n) result(integral)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: a, b
        integer, intent(in) :: n
        real(dp) :: integral

        real(dp) :: h, x, suma_impar, suma_par
        integer :: i, n_par

        ! Asegurar n par
        if (mod(n, 2) /= 0) then
            n_par = n + 1
        else
            n_par = n
        end if

        h = (b - a) / real(n_par, dp)
        suma_impar = 0.0_dp
        suma_par = 0.0_dp

        ! Suma de terminos impares (coeficiente 4)
        do i = 1, n_par - 1, 2
            x = a + real(i, dp) * h
            suma_impar = suma_impar + f(x)
        end do

        ! Suma de terminos pares (coeficiente 2)
        do i = 2, n_par - 2, 2
            x = a + real(i, dp) * h
            suma_par = suma_par + f(x)
        end do

        integral = h / 3.0_dp * (f(a) + 4.0_dp * suma_impar + 2.0_dp * suma_par + f(b))
    end function simpson_compuesto

    ! ========================================
    ! REGLA DE SIMPSON 3/8
    ! ========================================
    !
    ! Usa polinomio cubico a traves de 4 puntos
    ! I = 3h/8 * (f(a) + 3*f(a+h) + 3*f(a+2h) + f(b))

    function simpson_38(f, a, b) result(integral)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: a, b
        real(dp) :: integral

        real(dp) :: h

        h = (b - a) / 3.0_dp
        integral = 3.0_dp * h / 8.0_dp * &
                   (f(a) + 3.0_dp * f(a + h) + 3.0_dp * f(a + 2.0_dp * h) + f(b))
    end function simpson_38

    ! ========================================
    ! SIMPSON ADAPTATIVO
    ! ========================================

    recursive function simpson_adaptativo(f, a, b, tol, nivel_max, nivel) result(integral)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: a, b, tol
        integer, intent(in) :: nivel_max
        integer, intent(in), optional :: nivel
        real(dp) :: integral

        real(dp) :: c, I1, I2, I_izq, I_der
        integer :: nivel_actual

        if (present(nivel)) then
            nivel_actual = nivel
        else
            nivel_actual = 0
        end if

        c = (a + b) / 2.0_dp

        I1 = simpson_simple(f, a, b)
        I_izq = simpson_simple(f, a, c)
        I_der = simpson_simple(f, c, b)
        I2 = I_izq + I_der

        if (nivel_actual >= nivel_max .or. abs(I2 - I1) < 15.0_dp * tol) then
            integral = I2 + (I2 - I1) / 15.0_dp  ! Extrapolacion
        else
            integral = simpson_adaptativo(f, a, c, tol/2.0_dp, nivel_max, nivel_actual + 1) + &
                       simpson_adaptativo(f, c, b, tol/2.0_dp, nivel_max, nivel_actual + 1)
        end if
    end function simpson_adaptativo

end module integracion_simpson
```

### 4.3 Cuadratura de Gauss

```fortran
module cuadratura_gauss
    implicit none
    integer, parameter :: dp = selected_real_kind(15)

    ! ========================================
    ! NODOS Y PESOS GAUSS-LEGENDRE
    ! ========================================
    !
    ! Integracion exacta para polinomios de grado <= 2n-1
    ! donde n es el numero de puntos

    ! 2 puntos
    real(dp), parameter :: x2(2) = [-0.5773502691896257_dp, 0.5773502691896257_dp]
    real(dp), parameter :: w2(2) = [1.0_dp, 1.0_dp]

    ! 3 puntos
    real(dp), parameter :: x3(3) = [-0.7745966692414834_dp, 0.0_dp, 0.7745966692414834_dp]
    real(dp), parameter :: w3(3) = [0.5555555555555556_dp, 0.8888888888888889_dp, 0.5555555555555556_dp]

    ! 4 puntos
    real(dp), parameter :: x4(4) = [-0.8611363115940526_dp, -0.3399810435848563_dp, &
                                     0.3399810435848563_dp,  0.8611363115940526_dp]
    real(dp), parameter :: w4(4) = [0.3478548451374538_dp, 0.6521451548625461_dp, &
                                    0.6521451548625461_dp, 0.3478548451374538_dp]

    ! 5 puntos
    real(dp), parameter :: x5(5) = [-0.9061798459386640_dp, -0.5384693101056831_dp, 0.0_dp, &
                                     0.5384693101056831_dp,  0.9061798459386640_dp]
    real(dp), parameter :: w5(5) = [0.2369268850561891_dp, 0.4786286704993665_dp, 0.5688888888888889_dp, &
                                    0.4786286704993665_dp, 0.2369268850561891_dp]

contains

    ! ========================================
    ! GAUSS-LEGENDRE EN [-1, 1]
    ! ========================================

    function gauss_legendre(f, n) result(integral)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
        end interface
        integer, intent(in) :: n
        real(dp) :: integral

        integer :: i

        integral = 0.0_dp

        select case (n)
            case (2)
                do i = 1, 2
                    integral = integral + w2(i) * f(x2(i))
                end do
            case (3)
                do i = 1, 3
                    integral = integral + w3(i) * f(x3(i))
                end do
            case (4)
                do i = 1, 4
                    integral = integral + w4(i) * f(x4(i))
                end do
            case (5)
                do i = 1, 5
                    integral = integral + w5(i) * f(x5(i))
                end do
            case default
                print *, "Error: n debe ser 2, 3, 4 o 5"
                integral = 0.0_dp
        end select
    end function gauss_legendre

    ! ========================================
    ! GAUSS-LEGENDRE EN [a, b]
    ! ========================================
    !
    ! Cambio de variable: x = (b-a)/2 * t + (a+b)/2
    ! dx = (b-a)/2 * dt

    function gauss_legendre_ab(f, a, b, n) result(integral)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: a, b
        integer, intent(in) :: n
        real(dp) :: integral

        real(dp) :: c1, c2, t, x
        integer :: i

        c1 = (b - a) / 2.0_dp
        c2 = (a + b) / 2.0_dp
        integral = 0.0_dp

        select case (n)
            case (2)
                do i = 1, 2
                    x = c1 * x2(i) + c2
                    integral = integral + w2(i) * f(x)
                end do
            case (3)
                do i = 1, 3
                    x = c1 * x3(i) + c2
                    integral = integral + w3(i) * f(x)
                end do
            case (4)
                do i = 1, 4
                    x = c1 * x4(i) + c2
                    integral = integral + w4(i) * f(x)
                end do
            case (5)
                do i = 1, 5
                    x = c1 * x5(i) + c2
                    integral = integral + w5(i) * f(x)
                end do
        end select

        integral = c1 * integral
    end function gauss_legendre_ab

    ! ========================================
    ! GAUSS-LEGENDRE COMPUESTO
    ! ========================================
    !
    ! Divide [a,b] en m subintervalos y aplica Gauss

    function gauss_legendre_compuesto(f, a, b, n, m) result(integral)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: a, b
        integer, intent(in) :: n, m
        real(dp) :: integral

        real(dp) :: h, ai, bi
        integer :: i

        h = (b - a) / real(m, dp)
        integral = 0.0_dp

        do i = 0, m - 1
            ai = a + real(i, dp) * h
            bi = ai + h
            integral = integral + gauss_legendre_ab(f, ai, bi, n)
        end do
    end function gauss_legendre_compuesto

end module cuadratura_gauss


program test_integracion
    use integracion_trapecio
    use integracion_simpson
    use cuadratura_gauss
    implicit none

    real(dp) :: resultado, exacto
    real(dp), parameter :: PI = 3.14159265358979323846_dp

    ! Integral de sin(x) de 0 a pi = 2
    exacto = 2.0_dp

    print *, "=== Integral de sin(x) de 0 a pi ==="
    print *, "Valor exacto:", exacto

    resultado = trapecio_compuesto(sin_func, 0.0_dp, PI, 100)
    print *, "Trapecio (n=100):", resultado, "Error:", abs(resultado - exacto)

    resultado = simpson_compuesto(sin_func, 0.0_dp, PI, 100)
    print *, "Simpson (n=100):", resultado, "Error:", abs(resultado - exacto)

    resultado = gauss_legendre_ab(sin_func, 0.0_dp, PI, 5)
    print *, "Gauss-5 (1 intervalo):", resultado, "Error:", abs(resultado - exacto)

    resultado = gauss_legendre_compuesto(sin_func, 0.0_dp, PI, 5, 4)
    print *, "Gauss-5 (4 intervalos):", resultado, "Error:", abs(resultado - exacto)

contains

    function sin_func(x) result(y)
        real(dp), intent(in) :: x
        real(dp) :: y
        y = sin(x)
    end function sin_func

end program test_integracion
```

---

## 5. Ecuaciones Diferenciales

### 5.1 Metodo de Euler

```fortran
module metodo_euler
    implicit none
    integer, parameter :: dp = selected_real_kind(15)

contains

    ! ========================================
    ! EULER EXPLICITO (Forward Euler)
    ! ========================================
    !
    ! y' = f(t, y)
    ! y_{n+1} = y_n + h * f(t_n, y_n)
    !
    ! Error local: O(h^2)
    ! Error global: O(h)

    subroutine euler_explicito(f, t0, y0, tf, n, t, y)
        interface
            function f(t, y)
                import :: dp
                real(dp), intent(in) :: t, y
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: t0, y0, tf
        integer, intent(in) :: n
        real(dp), intent(out) :: t(0:n), y(0:n)

        real(dp) :: h
        integer :: i

        h = (tf - t0) / real(n, dp)

        t(0) = t0
        y(0) = y0

        do i = 0, n - 1
            y(i + 1) = y(i) + h * f(t(i), y(i))
            t(i + 1) = t(i) + h
        end do
    end subroutine euler_explicito

    ! ========================================
    ! EULER IMPLICITO (Backward Euler)
    ! ========================================
    !
    ! y_{n+1} = y_n + h * f(t_{n+1}, y_{n+1})
    !
    ! Requiere resolver ecuacion no lineal
    ! Mas estable para sistemas stiff

    subroutine euler_implicito(f, df_dy, t0, y0, tf, n, t, y)
        interface
            function f(t, y)
                import :: dp
                real(dp), intent(in) :: t, y
                real(dp) :: f
            end function f
            function df_dy(t, y)
                import :: dp
                real(dp), intent(in) :: t, y
                real(dp) :: df_dy
            end function df_dy
        end interface
        real(dp), intent(in) :: t0, y0, tf
        integer, intent(in) :: n
        real(dp), intent(out) :: t(0:n), y(0:n)

        real(dp) :: h, y_new, y_old, residuo, jacobiano
        integer :: i, iter
        integer, parameter :: max_iter = 100
        real(dp), parameter :: tol = 1.0e-12_dp

        h = (tf - t0) / real(n, dp)

        t(0) = t0
        y(0) = y0

        do i = 0, n - 1
            t(i + 1) = t(i) + h

            ! Newton-Raphson para resolver y_{n+1} = y_n + h*f(t_{n+1}, y_{n+1})
            y_new = y(i)  ! Prediccion inicial

            do iter = 1, max_iter
                y_old = y_new
                residuo = y_new - y(i) - h * f(t(i + 1), y_new)
                jacobiano = 1.0_dp - h * df_dy(t(i + 1), y_new)
                y_new = y_old - residuo / jacobiano

                if (abs(y_new - y_old) < tol) exit
            end do

            y(i + 1) = y_new
        end do
    end subroutine euler_implicito

    ! ========================================
    ! EULER PARA SISTEMAS
    ! ========================================
    !
    ! y' = f(t, y) donde y es un vector

    subroutine euler_sistema(f, t0, y0, tf, n, neq, t, y)
        interface
            subroutine f(t, y, dydt, neq)
                import :: dp
                integer, intent(in) :: neq
                real(dp), intent(in) :: t, y(neq)
                real(dp), intent(out) :: dydt(neq)
            end subroutine f
        end interface
        real(dp), intent(in) :: t0, y0(:), tf
        integer, intent(in) :: n, neq
        real(dp), intent(out) :: t(0:n), y(0:n, neq)

        real(dp) :: h, dydt(neq)
        integer :: i

        h = (tf - t0) / real(n, dp)

        t(0) = t0
        y(0, :) = y0

        do i = 0, n - 1
            call f(t(i), y(i, :), dydt, neq)
            y(i + 1, :) = y(i, :) + h * dydt
            t(i + 1) = t(i) + h
        end do
    end subroutine euler_sistema

end module metodo_euler
```

### 5.2 Metodo de Runge-Kutta

```fortran
module metodo_runge_kutta
    implicit none
    integer, parameter :: dp = selected_real_kind(15)

contains

    ! ========================================
    ! RUNGE-KUTTA DE ORDEN 2 (RK2 - Heun)
    ! ========================================
    !
    ! k1 = f(t_n, y_n)
    ! k2 = f(t_n + h, y_n + h*k1)
    ! y_{n+1} = y_n + h/2 * (k1 + k2)
    !
    ! Error local: O(h^3)

    subroutine rk2_heun(f, t0, y0, tf, n, t, y)
        interface
            function f(t, y)
                import :: dp
                real(dp), intent(in) :: t, y
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: t0, y0, tf
        integer, intent(in) :: n
        real(dp), intent(out) :: t(0:n), y(0:n)

        real(dp) :: h, k1, k2
        integer :: i

        h = (tf - t0) / real(n, dp)

        t(0) = t0
        y(0) = y0

        do i = 0, n - 1
            k1 = f(t(i), y(i))
            k2 = f(t(i) + h, y(i) + h * k1)
            y(i + 1) = y(i) + h / 2.0_dp * (k1 + k2)
            t(i + 1) = t(i) + h
        end do
    end subroutine rk2_heun

    ! ========================================
    ! RUNGE-KUTTA DE ORDEN 4 CLASICO (RK4)
    ! ========================================
    !
    ! k1 = f(t_n, y_n)
    ! k2 = f(t_n + h/2, y_n + h/2 * k1)
    ! k3 = f(t_n + h/2, y_n + h/2 * k2)
    ! k4 = f(t_n + h, y_n + h * k3)
    ! y_{n+1} = y_n + h/6 * (k1 + 2*k2 + 2*k3 + k4)
    !
    ! Error local: O(h^5)
    ! Error global: O(h^4)

    subroutine rk4(f, t0, y0, tf, n, t, y)
        interface
            function f(t, y)
                import :: dp
                real(dp), intent(in) :: t, y
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: t0, y0, tf
        integer, intent(in) :: n
        real(dp), intent(out) :: t(0:n), y(0:n)

        real(dp) :: h, k1, k2, k3, k4
        integer :: i

        h = (tf - t0) / real(n, dp)

        t(0) = t0
        y(0) = y0

        do i = 0, n - 1
            k1 = f(t(i), y(i))
            k2 = f(t(i) + h/2.0_dp, y(i) + h/2.0_dp * k1)
            k3 = f(t(i) + h/2.0_dp, y(i) + h/2.0_dp * k2)
            k4 = f(t(i) + h, y(i) + h * k3)

            y(i + 1) = y(i) + h/6.0_dp * (k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4)
            t(i + 1) = t(i) + h
        end do
    end subroutine rk4

    ! ========================================
    ! RK4 PARA SISTEMAS DE ECUACIONES
    ! ========================================

    subroutine rk4_sistema(f, t0, y0, tf, n, neq, t, y)
        interface
            subroutine f(t, y, dydt, neq)
                import :: dp
                integer, intent(in) :: neq
                real(dp), intent(in) :: t, y(neq)
                real(dp), intent(out) :: dydt(neq)
            end subroutine f
        end interface
        real(dp), intent(in) :: t0, y0(:), tf
        integer, intent(in) :: n, neq
        real(dp), intent(out) :: t(0:n), y(0:n, neq)

        real(dp) :: h
        real(dp) :: k1(neq), k2(neq), k3(neq), k4(neq)
        real(dp) :: y_temp(neq)
        integer :: i

        h = (tf - t0) / real(n, dp)

        t(0) = t0
        y(0, :) = y0

        do i = 0, n - 1
            call f(t(i), y(i, :), k1, neq)

            y_temp = y(i, :) + h/2.0_dp * k1
            call f(t(i) + h/2.0_dp, y_temp, k2, neq)

            y_temp = y(i, :) + h/2.0_dp * k2
            call f(t(i) + h/2.0_dp, y_temp, k3, neq)

            y_temp = y(i, :) + h * k3
            call f(t(i) + h, y_temp, k4, neq)

            y(i + 1, :) = y(i, :) + h/6.0_dp * (k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4)
            t(i + 1) = t(i) + h
        end do
    end subroutine rk4_sistema

    ! ========================================
    ! RK4-5 FEHLBERG (con control de paso)
    ! ========================================
    !
    ! Usa RK4 y RK5 para estimar error
    ! Ajusta paso automaticamente

    subroutine rkf45(f, t0, y0, tf, tol, t, y, nsteps)
        interface
            function f(t, y)
                import :: dp
                real(dp), intent(in) :: t, y
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: t0, y0, tf, tol
        real(dp), allocatable, intent(out) :: t(:), y(:)
        integer, intent(out) :: nsteps

        ! Coeficientes RKF45
        real(dp), parameter :: c2 = 1.0_dp/4.0_dp
        real(dp), parameter :: c3 = 3.0_dp/8.0_dp
        real(dp), parameter :: c4 = 12.0_dp/13.0_dp
        real(dp), parameter :: c5 = 1.0_dp
        real(dp), parameter :: c6 = 1.0_dp/2.0_dp

        real(dp), parameter :: a21 = 1.0_dp/4.0_dp
        real(dp), parameter :: a31 = 3.0_dp/32.0_dp, a32 = 9.0_dp/32.0_dp
        real(dp), parameter :: a41 = 1932.0_dp/2197.0_dp, a42 = -7200.0_dp/2197.0_dp, a43 = 7296.0_dp/2197.0_dp
        real(dp), parameter :: a51 = 439.0_dp/216.0_dp, a52 = -8.0_dp, a53 = 3680.0_dp/513.0_dp, a54 = -845.0_dp/4104.0_dp
        real(dp), parameter :: a61 = -8.0_dp/27.0_dp, a62 = 2.0_dp, a63 = -3544.0_dp/2565.0_dp
        real(dp), parameter :: a64 = 1859.0_dp/4104.0_dp, a65 = -11.0_dp/40.0_dp

        real(dp), parameter :: b1 = 16.0_dp/135.0_dp, b3 = 6656.0_dp/12825.0_dp
        real(dp), parameter :: b4 = 28561.0_dp/56430.0_dp, b5 = -9.0_dp/50.0_dp, b6 = 2.0_dp/55.0_dp

        real(dp), parameter :: e1 = 1.0_dp/360.0_dp, e3 = -128.0_dp/4275.0_dp, e4 = -2197.0_dp/75240.0_dp
        real(dp), parameter :: e5 = 1.0_dp/50.0_dp, e6 = 2.0_dp/55.0_dp

        real(dp) :: h, h_new, t_curr, y_curr
        real(dp) :: k1, k2, k3, k4, k5, k6
        real(dp) :: y_new, error, s
        real(dp), allocatable :: t_temp(:), y_temp(:)
        integer :: max_steps, step
        real(dp), parameter :: h_min = 1.0e-10_dp, h_max = 1.0_dp, safety = 0.9_dp

        max_steps = 100000
        allocate(t_temp(max_steps), y_temp(max_steps))

        h = 0.01_dp * (tf - t0)
        t_curr = t0
        y_curr = y0
        step = 1
        t_temp(1) = t0
        y_temp(1) = y0

        do while (t_curr < tf .and. step < max_steps)
            ! Calcular k's
            k1 = h * f(t_curr, y_curr)
            k2 = h * f(t_curr + c2*h, y_curr + a21*k1)
            k3 = h * f(t_curr + c3*h, y_curr + a31*k1 + a32*k2)
            k4 = h * f(t_curr + c4*h, y_curr + a41*k1 + a42*k2 + a43*k3)
            k5 = h * f(t_curr + c5*h, y_curr + a51*k1 + a52*k2 + a53*k3 + a54*k4)
            k6 = h * f(t_curr + c6*h, y_curr + a61*k1 + a62*k2 + a63*k3 + a64*k4 + a65*k5)

            ! Solucion RK5
            y_new = y_curr + b1*k1 + b3*k3 + b4*k4 + b5*k5 + b6*k6

            ! Error estimado
            error = abs(e1*k1 + e3*k3 + e4*k4 + e5*k5 + e6*k6)

            ! Ajustar paso
            if (error < tol .or. h <= h_min) then
                step = step + 1
                t_curr = t_curr + h
                y_curr = y_new
                t_temp(step) = t_curr
                y_temp(step) = y_curr
            end if

            ! Calcular nuevo paso
            if (error > 0.0_dp) then
                s = safety * (tol / error) ** 0.25_dp
                h_new = min(max(s * h, h_min), h_max)
                h = min(h_new, tf - t_curr)
            end if
        end do

        nsteps = step
        allocate(t(nsteps), y(nsteps))
        t = t_temp(1:nsteps)
        y = y_temp(1:nsteps)
        deallocate(t_temp, y_temp)

    end subroutine rkf45

end module metodo_runge_kutta


program test_ode
    use metodo_euler
    use metodo_runge_kutta
    implicit none

    integer, parameter :: n = 100
    real(dp) :: t(0:n), y(0:n)
    real(dp) :: error_euler, error_rk4
    integer :: i

    ! Resolver y' = -y, y(0) = 1
    ! Solucion exacta: y = exp(-t)

    print *, "=== Resolver y' = -y, y(0) = 1 ==="

    call euler_explicito(f_decay, 0.0_dp, 1.0_dp, 5.0_dp, n, t, y)
    error_euler = abs(y(n) - exp(-5.0_dp))
    print *, "Euler (n=100): y(5) =", y(n), "Error:", error_euler

    call rk4(f_decay, 0.0_dp, 1.0_dp, 5.0_dp, n, t, y)
    error_rk4 = abs(y(n) - exp(-5.0_dp))
    print *, "RK4 (n=100): y(5) =", y(n), "Error:", error_rk4

    print *, "Valor exacto: y(5) =", exp(-5.0_dp)

contains

    function f_decay(t, y) result(dydt)
        real(dp), intent(in) :: t, y
        real(dp) :: dydt
        dydt = -y
    end function f_decay

end program test_ode
```

---

## 6. Algebra Lineal Numerica

### 6.1 Eliminacion Gaussiana

```fortran
module eliminacion_gaussiana
    implicit none
    integer, parameter :: dp = selected_real_kind(15)

contains

    ! ========================================
    ! ELIMINACION GAUSSIANA CON PIVOTEO PARCIAL
    ! ========================================
    !
    ! Resuelve Ax = b
    ! Transforma A en forma triangular superior
    ! Luego usa sustitucion hacia atras

    subroutine gauss_pivot(A, b, x, n, info)
        integer, intent(in) :: n
        real(dp), intent(inout) :: A(n, n), b(n)
        real(dp), intent(out) :: x(n)
        integer, intent(out) :: info

        real(dp) :: factor, max_val, temp
        integer :: i, j, k, max_row, temp_int

        info = 0

        ! Eliminacion hacia adelante con pivoteo parcial
        do k = 1, n - 1
            ! Buscar pivote maximo en columna k
            max_val = abs(A(k, k))
            max_row = k
            do i = k + 1, n
                if (abs(A(i, k)) > max_val) then
                    max_val = abs(A(i, k))
                    max_row = i
                end if
            end do

            ! Verificar singularidad
            if (max_val < 1.0e-15_dp) then
                info = -k  ! Matriz singular
                return
            end if

            ! Intercambiar filas si es necesario
            if (max_row /= k) then
                do j = k, n
                    temp = A(k, j)
                    A(k, j) = A(max_row, j)
                    A(max_row, j) = temp
                end do
                temp = b(k)
                b(k) = b(max_row)
                b(max_row) = temp
            end if

            ! Eliminar debajo de pivote
            do i = k + 1, n
                factor = A(i, k) / A(k, k)
                do j = k + 1, n
                    A(i, j) = A(i, j) - factor * A(k, j)
                end do
                A(i, k) = 0.0_dp
                b(i) = b(i) - factor * b(k)
            end do
        end do

        ! Verificar ultimo pivote
        if (abs(A(n, n)) < 1.0e-15_dp) then
            info = -n
            return
        end if

        ! Sustitucion hacia atras
        x(n) = b(n) / A(n, n)
        do i = n - 1, 1, -1
            x(i) = b(i)
            do j = i + 1, n
                x(i) = x(i) - A(i, j) * x(j)
            end do
            x(i) = x(i) / A(i, i)
        end do

    end subroutine gauss_pivot

    ! ========================================
    ! GAUSS-JORDAN (Inversa de matriz)
    ! ========================================

    subroutine gauss_jordan_inversa(A, A_inv, n, info)
        integer, intent(in) :: n
        real(dp), intent(in) :: A(n, n)
        real(dp), intent(out) :: A_inv(n, n)
        integer, intent(out) :: info

        real(dp) :: Aug(n, 2*n), factor, max_val, temp
        integer :: i, j, k, max_row

        info = 0

        ! Crear matriz aumentada [A | I]
        Aug(:, 1:n) = A
        Aug(:, n+1:2*n) = 0.0_dp
        do i = 1, n
            Aug(i, n + i) = 1.0_dp
        end do

        ! Eliminacion con pivoteo
        do k = 1, n
            ! Buscar pivote
            max_val = abs(Aug(k, k))
            max_row = k
            do i = k + 1, n
                if (abs(Aug(i, k)) > max_val) then
                    max_val = abs(Aug(i, k))
                    max_row = i
                end if
            end do

            if (max_val < 1.0e-15_dp) then
                info = -k
                return
            end if

            ! Intercambiar filas
            if (max_row /= k) then
                do j = 1, 2*n
                    temp = Aug(k, j)
                    Aug(k, j) = Aug(max_row, j)
                    Aug(max_row, j) = temp
                end do
            end if

            ! Escalar fila pivote
            factor = Aug(k, k)
            Aug(k, :) = Aug(k, :) / factor

            ! Eliminar columna k en todas las otras filas
            do i = 1, n
                if (i /= k) then
                    factor = Aug(i, k)
                    Aug(i, :) = Aug(i, :) - factor * Aug(k, :)
                end if
            end do
        end do

        ! Extraer inversa
        A_inv = Aug(:, n+1:2*n)

    end subroutine gauss_jordan_inversa

end module eliminacion_gaussiana
```

### 6.2 Descomposicion LU

```fortran
module descomposicion_lu
    implicit none
    integer, parameter :: dp = selected_real_kind(15)

contains

    ! ========================================
    ! DESCOMPOSICION LU SIN PIVOTEO
    ! ========================================
    !
    ! A = L * U
    ! L: triangular inferior con 1's en diagonal
    ! U: triangular superior

    subroutine lu_decompose(A, L, U, n, info)
        integer, intent(in) :: n
        real(dp), intent(in) :: A(n, n)
        real(dp), intent(out) :: L(n, n), U(n, n)
        integer, intent(out) :: info

        real(dp) :: suma
        integer :: i, j, k

        info = 0
        L = 0.0_dp
        U = 0.0_dp

        do i = 1, n
            L(i, i) = 1.0_dp
        end do

        do j = 1, n
            ! Calcular fila j de U
            do i = 1, j
                suma = A(i, j)
                do k = 1, i - 1
                    suma = suma - L(i, k) * U(k, j)
                end do
                U(i, j) = suma
            end do

            ! Verificar pivote
            if (abs(U(j, j)) < 1.0e-15_dp) then
                info = -j
                return
            end if

            ! Calcular columna j de L
            do i = j + 1, n
                suma = A(i, j)
                do k = 1, j - 1
                    suma = suma - L(i, k) * U(k, j)
                end do
                L(i, j) = suma / U(j, j)
            end do
        end do

    end subroutine lu_decompose

    ! ========================================
    ! DESCOMPOSICION LU CON PIVOTEO PARCIAL
    ! ========================================
    !
    ! P * A = L * U
    ! P: matriz de permutacion

    subroutine lu_pivot(A, L, U, P, n, info)
        integer, intent(in) :: n
        real(dp), intent(in) :: A(n, n)
        real(dp), intent(out) :: L(n, n), U(n, n)
        integer, intent(out) :: P(n), info

        real(dp) :: work(n, n), max_val, temp
        integer :: i, j, k, max_row, temp_int

        info = 0
        work = A
        L = 0.0_dp
        U = 0.0_dp

        ! Inicializar permutacion
        do i = 1, n
            P(i) = i
            L(i, i) = 1.0_dp
        end do

        do k = 1, n
            ! Buscar pivote
            max_val = abs(work(k, k))
            max_row = k
            do i = k + 1, n
                if (abs(work(i, k)) > max_val) then
                    max_val = abs(work(i, k))
                    max_row = i
                end if
            end do

            if (max_val < 1.0e-15_dp) then
                info = -k
                return
            end if

            ! Intercambiar
            if (max_row /= k) then
                temp_int = P(k)
                P(k) = P(max_row)
                P(max_row) = temp_int

                do j = 1, n
                    temp = work(k, j)
                    work(k, j) = work(max_row, j)
                    work(max_row, j) = temp
                end do

                do j = 1, k - 1
                    temp = L(k, j)
                    L(k, j) = L(max_row, j)
                    L(max_row, j) = temp
                end do
            end if

            ! Eliminar
            do i = k + 1, n
                L(i, k) = work(i, k) / work(k, k)
                do j = k, n
                    work(i, j) = work(i, j) - L(i, k) * work(k, j)
                end do
            end do
        end do

        U = work

    end subroutine lu_pivot

    ! ========================================
    ! RESOLVER USANDO LU
    ! ========================================
    !
    ! A*x = b  ->  L*U*x = b
    ! 1) Resolver L*y = b (sustitucion hacia adelante)
    ! 2) Resolver U*x = y (sustitucion hacia atras)

    subroutine lu_solve(L, U, b, x, n)
        integer, intent(in) :: n
        real(dp), intent(in) :: L(n, n), U(n, n), b(n)
        real(dp), intent(out) :: x(n)

        real(dp) :: y(n)
        integer :: i, j

        ! Sustitucion hacia adelante: L*y = b
        do i = 1, n
            y(i) = b(i)
            do j = 1, i - 1
                y(i) = y(i) - L(i, j) * y(j)
            end do
            ! L(i,i) = 1 asi que no dividimos
        end do

        ! Sustitucion hacia atras: U*x = y
        do i = n, 1, -1
            x(i) = y(i)
            do j = i + 1, n
                x(i) = x(i) - U(i, j) * x(j)
            end do
            x(i) = x(i) / U(i, i)
        end do

    end subroutine lu_solve

end module descomposicion_lu
```

### 6.3 Eigenvalores (Introduccion)

```fortran
module eigenvalores
    implicit none
    integer, parameter :: dp = selected_real_kind(15)

contains

    ! ========================================
    ! METODO DE LA POTENCIA
    ! ========================================
    !
    ! Encuentra el eigenvalor dominante (mayor magnitud)
    ! y su eigenvector correspondiente

    subroutine power_method(A, n, eigenval, eigenvec, max_iter, tol, info)
        integer, intent(in) :: n, max_iter
        real(dp), intent(in) :: A(n, n), tol
        real(dp), intent(out) :: eigenval, eigenvec(n)
        integer, intent(out) :: info

        real(dp) :: x(n), y(n), lambda, lambda_old, norma
        integer :: iter, i

        info = 0

        ! Vector inicial
        x = 1.0_dp

        ! Normalizar
        norma = sqrt(dot_product(x, x))
        x = x / norma

        lambda = 0.0_dp

        do iter = 1, max_iter
            lambda_old = lambda

            ! y = A * x
            y = matmul(A, x)

            ! Cociente de Rayleigh para eigenvalor
            lambda = dot_product(x, y)

            ! Normalizar y
            norma = sqrt(dot_product(y, y))
            if (norma < 1.0e-15_dp) then
                info = -1  ! Vector nulo
                return
            end if
            x = y / norma

            ! Verificar convergencia
            if (abs(lambda - lambda_old) < tol) then
                eigenval = lambda
                eigenvec = x
                return
            end if
        end do

        ! No convergio
        info = 1
        eigenval = lambda
        eigenvec = x

    end subroutine power_method

    ! ========================================
    ! METODO DE LA POTENCIA INVERSA
    ! ========================================
    !
    ! Encuentra eigenvalor mas cercano a mu
    ! Util cuando se tiene aproximacion inicial

    subroutine inverse_power(A, n, mu, eigenval, eigenvec, max_iter, tol, info)
        use descomposicion_lu, only: lu_decompose, lu_solve
        integer, intent(in) :: n, max_iter
        real(dp), intent(in) :: A(n, n), mu, tol
        real(dp), intent(out) :: eigenval, eigenvec(n)
        integer, intent(out) :: info

        real(dp) :: B(n, n), L(n, n), U(n, n)
        real(dp) :: x(n), y(n), lambda, lambda_old, norma
        integer :: iter, i

        info = 0

        ! B = A - mu*I
        B = A
        do i = 1, n
            B(i, i) = B(i, i) - mu
        end do

        ! Descomponer LU
        call lu_decompose(B, L, U, n, info)
        if (info /= 0) return

        ! Vector inicial
        x = 1.0_dp
        norma = sqrt(dot_product(x, x))
        x = x / norma

        lambda = 0.0_dp

        do iter = 1, max_iter
            lambda_old = lambda

            ! Resolver B*y = x  (equivalente a y = B^{-1}*x)
            call lu_solve(L, U, x, y, n)

            ! Cociente de Rayleigh
            lambda = dot_product(x, y)

            ! Normalizar
            norma = sqrt(dot_product(y, y))
            x = y / norma

            ! Verificar convergencia
            if (abs(lambda - lambda_old) < tol * abs(lambda)) exit
        end do

        ! Eigenvalor de A
        eigenval = mu + 1.0_dp / lambda
        eigenvec = x

    end subroutine inverse_power

    ! ========================================
    ! ITERACION QR BASICA
    ! ========================================
    !
    ! Encuentra todos los eigenvalores
    ! Converge lentamente sin shifts

    subroutine qr_iteration(A, n, eigenvalues, max_iter, tol, info)
        integer, intent(in) :: n, max_iter
        real(dp), intent(inout) :: A(n, n)
        real(dp), intent(out) :: eigenvalues(n)
        real(dp), intent(in) :: tol
        integer, intent(out) :: info

        real(dp) :: Q(n, n), R(n, n)
        real(dp) :: off_diag
        integer :: iter, i, j

        info = 0

        do iter = 1, max_iter
            ! Descomposicion QR (Gram-Schmidt modificado)
            call qr_gram_schmidt(A, Q, R, n)

            ! A_new = R * Q
            A = matmul(R, Q)

            ! Verificar convergencia (elementos off-diagonal)
            off_diag = 0.0_dp
            do i = 2, n
                do j = 1, i - 1
                    off_diag = off_diag + abs(A(i, j))
                end do
            end do

            if (off_diag < tol) exit
        end do

        ! Extraer eigenvalores de diagonal
        do i = 1, n
            eigenvalues(i) = A(i, i)
        end do

        if (iter >= max_iter) info = 1

    end subroutine qr_iteration

    ! ========================================
    ! GRAM-SCHMIDT MODIFICADO PARA QR
    ! ========================================

    subroutine qr_gram_schmidt(A, Q, R, n)
        integer, intent(in) :: n
        real(dp), intent(in) :: A(n, n)
        real(dp), intent(out) :: Q(n, n), R(n, n)

        real(dp) :: v(n), norma
        integer :: i, j

        Q = A
        R = 0.0_dp

        do i = 1, n
            ! Ortogonalizar contra columnas anteriores
            do j = 1, i - 1
                R(j, i) = dot_product(Q(:, j), Q(:, i))
                Q(:, i) = Q(:, i) - R(j, i) * Q(:, j)
            end do

            ! Normalizar
            norma = sqrt(dot_product(Q(:, i), Q(:, i)))
            R(i, i) = norma
            if (norma > 1.0e-15_dp) then
                Q(:, i) = Q(:, i) / norma
            end if
        end do

    end subroutine qr_gram_schmidt

end module eigenvalores
```

---

## 7. FFT y Procesamiento de Senales

### 7.1 Introduccion a FFT

```fortran
module fft_basico
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    real(dp), parameter :: PI = 3.14159265358979323846_dp
    complex(dp), parameter :: I = (0.0_dp, 1.0_dp)

contains

    ! ========================================
    ! DFT DIRECTA (O(n^2))
    ! ========================================
    !
    ! X[k] = sum_{n=0}^{N-1} x[n] * exp(-2*pi*i*k*n/N)
    !
    ! Uso didactico, no eficiente para N grande

    subroutine dft(x, X, n)
        integer, intent(in) :: n
        complex(dp), intent(in) :: x(0:n-1)
        complex(dp), intent(out) :: X(0:n-1)

        complex(dp) :: W, Wk
        integer :: j, k

        W = exp(-2.0_dp * PI * I / real(n, dp))

        do k = 0, n - 1
            X(k) = (0.0_dp, 0.0_dp)
            Wk = W ** k
            do j = 0, n - 1
                X(k) = X(k) + x(j) * Wk ** j
            end do
        end do

    end subroutine dft

    ! ========================================
    ! DFT INVERSA
    ! ========================================
    !
    ! x[n] = (1/N) * sum_{k=0}^{N-1} X[k] * exp(2*pi*i*k*n/N)

    subroutine idft(X, x, n)
        integer, intent(in) :: n
        complex(dp), intent(in) :: X(0:n-1)
        complex(dp), intent(out) :: x(0:n-1)

        complex(dp) :: W, Wk
        integer :: j, k

        W = exp(2.0_dp * PI * I / real(n, dp))

        do k = 0, n - 1
            x(k) = (0.0_dp, 0.0_dp)
            Wk = W ** k
            do j = 0, n - 1
                x(k) = x(k) + X(j) * Wk ** j
            end do
            x(k) = x(k) / real(n, dp)
        end do

    end subroutine idft

    ! ========================================
    ! FFT COOLEY-TUKEY (Radix-2)
    ! ========================================
    !
    ! O(n*log(n))
    ! Requiere n = potencia de 2

    recursive subroutine fft(x, n)
        integer, intent(in) :: n
        complex(dp), intent(inout) :: x(0:n-1)

        complex(dp) :: even(0:n/2-1), odd(0:n/2-1)
        complex(dp) :: W, Wk, t
        integer :: k

        if (n <= 1) return

        ! Separar pares e impares
        even = x(0:n-1:2)
        odd = x(1:n-1:2)

        ! FFT recursiva
        call fft(even, n/2)
        call fft(odd, n/2)

        ! Combinar
        W = exp(-2.0_dp * PI * I / real(n, dp))
        Wk = (1.0_dp, 0.0_dp)

        do k = 0, n/2 - 1
            t = Wk * odd(k)
            x(k) = even(k) + t
            x(k + n/2) = even(k) - t
            Wk = Wk * W
        end do

    end subroutine fft

    ! ========================================
    ! FFT INVERSA
    ! ========================================

    subroutine ifft(X, n)
        integer, intent(in) :: n
        complex(dp), intent(inout) :: X(0:n-1)

        integer :: k

        ! Conjugar, aplicar FFT, conjugar y dividir
        X = conjg(X)
        call fft(X, n)
        X = conjg(X) / real(n, dp)

    end subroutine ifft

    ! ========================================
    ! FFT ITERATIVA (Bit-reversal)
    ! ========================================
    !
    ! Version sin recursion, mas eficiente

    subroutine fft_iterative(x, n)
        integer, intent(in) :: n
        complex(dp), intent(inout) :: x(0:n-1)

        integer :: i, j, k, m, m2, log2n
        complex(dp) :: W, Wm, t, u

        ! Calcular log2(n)
        log2n = 0
        m = n
        do while (m > 1)
            m = m / 2
            log2n = log2n + 1
        end do

        ! Bit-reversal permutation
        j = 0
        do i = 0, n - 2
            if (i < j) then
                t = x(i)
                x(i) = x(j)
                x(j) = t
            end if
            k = n / 2
            do while (k <= j)
                j = j - k
                k = k / 2
            end do
            j = j + k
        end do

        ! Mariposas iterativas
        m = 2
        do while (m <= n)
            m2 = m / 2
            Wm = exp(-2.0_dp * PI * I / real(m, dp))

            do k = 0, n - 1, m
                W = (1.0_dp, 0.0_dp)
                do j = 0, m2 - 1
                    t = W * x(k + j + m2)
                    u = x(k + j)
                    x(k + j) = u + t
                    x(k + j + m2) = u - t
                    W = W * Wm
                end do
            end do
            m = m * 2
        end do

    end subroutine fft_iterative

    ! ========================================
    ! UTILIDADES FFT
    ! ========================================

    ! Siguiente potencia de 2
    pure function next_pow2(n) result(m)
        integer, intent(in) :: n
        integer :: m
        m = 1
        do while (m < n)
            m = m * 2
        end do
    end function next_pow2

    ! Frecuencias correspondientes a FFT
    subroutine fft_frequencies(n, fs, freq)
        integer, intent(in) :: n
        real(dp), intent(in) :: fs  ! Frecuencia de muestreo
        real(dp), intent(out) :: freq(0:n-1)

        integer :: k

        do k = 0, n/2
            freq(k) = real(k, dp) * fs / real(n, dp)
        end do
        do k = n/2 + 1, n - 1
            freq(k) = real(k - n, dp) * fs / real(n, dp)
        end do
    end subroutine fft_frequencies

    ! Magnitud del espectro
    subroutine spectrum_magnitude(X, n, mag)
        integer, intent(in) :: n
        complex(dp), intent(in) :: X(0:n-1)
        real(dp), intent(out) :: mag(0:n-1)

        mag = abs(X)
    end subroutine spectrum_magnitude

    ! Fase del espectro
    subroutine spectrum_phase(X, n, phase)
        integer, intent(in) :: n
        complex(dp), intent(in) :: X(0:n-1)
        real(dp), intent(out) :: phase(0:n-1)

        integer :: k
        do k = 0, n - 1
            phase(k) = atan2(aimag(X(k)), real(X(k)))
        end do
    end subroutine spectrum_phase

end module fft_basico
```

### 7.2 Uso de FFTW

```fortran
! ========================================
! INTERFACE PARA FFTW3
! ========================================
!
! FFTW (Fastest Fourier Transform in the West)
! Biblioteca altamente optimizada
!
! Compilar con: gfortran programa.f90 -lfftw3

module fftw_interface
    use, intrinsic :: iso_c_binding
    implicit none

    ! Constantes FFTW
    integer(c_int), parameter :: FFTW_FORWARD = -1
    integer(c_int), parameter :: FFTW_BACKWARD = +1

    integer(c_int), parameter :: FFTW_MEASURE = 0
    integer(c_int), parameter :: FFTW_ESTIMATE = 64
    integer(c_int), parameter :: FFTW_PATIENT = 32

    ! Interfaces a funciones FFTW
    interface
        ! Crear plan para FFT compleja 1D
        type(c_ptr) function fftw_plan_dft_1d(n, in, out, sign, flags) &
                bind(c, name='fftw_plan_dft_1d')
            import :: c_ptr, c_int
            integer(c_int), value :: n
            type(c_ptr), value :: in, out
            integer(c_int), value :: sign
            integer(c_int), value :: flags
        end function fftw_plan_dft_1d

        ! Ejecutar plan
        subroutine fftw_execute(plan) bind(c, name='fftw_execute')
            import :: c_ptr
            type(c_ptr), value :: plan
        end subroutine fftw_execute

        ! Destruir plan
        subroutine fftw_destroy_plan(plan) bind(c, name='fftw_destroy_plan')
            import :: c_ptr
            type(c_ptr), value :: plan
        end subroutine fftw_destroy_plan

        ! Asignar memoria alineada
        type(c_ptr) function fftw_malloc(n) bind(c, name='fftw_malloc')
            import :: c_ptr, c_size_t
            integer(c_size_t), value :: n
        end function fftw_malloc

        ! Liberar memoria
        subroutine fftw_free(p) bind(c, name='fftw_free')
            import :: c_ptr
            type(c_ptr), value :: p
        end subroutine fftw_free
    end interface

end module fftw_interface


program ejemplo_fftw
    use fftw_interface
    use, intrinsic :: iso_c_binding
    implicit none

    integer, parameter :: dp = selected_real_kind(15)
    integer, parameter :: n = 1024

    complex(c_double_complex), allocatable, target :: input(:), output(:)
    type(c_ptr) :: plan_forward, plan_backward
    real(dp), parameter :: PI = 3.14159265358979323846_dp
    real(dp) :: fs, dt, t, freq
    integer :: i

    allocate(input(n), output(n))

    ! ========================================
    ! CREAR SENAL DE PRUEBA
    ! ========================================

    fs = 1000.0_dp  ! Frecuencia de muestreo (Hz)
    dt = 1.0_dp / fs

    do i = 1, n
        t = real(i - 1, dp) * dt
        ! Senal: suma de dos sinusoides
        input(i) = cmplx(sin(2.0_dp * PI * 50.0_dp * t) + &
                         0.5_dp * sin(2.0_dp * PI * 120.0_dp * t), 0.0_dp, dp)
    end do

    ! ========================================
    ! CREAR PLAN Y EJECUTAR FFT
    ! ========================================

    plan_forward = fftw_plan_dft_1d(n, c_loc(input), c_loc(output), &
                                     FFTW_FORWARD, FFTW_ESTIMATE)

    call fftw_execute(plan_forward)

    ! ========================================
    ! MOSTRAR PICOS DEL ESPECTRO
    ! ========================================

    print *, "=== Espectro de frecuencias ==="
    print *, "Freq (Hz)     Magnitud"

    do i = 1, n/2
        freq = real(i - 1, dp) * fs / real(n, dp)
        if (abs(output(i)) > real(n, dp) * 0.1_dp) then
            print '(F10.2, F15.4)', freq, abs(output(i)) / real(n, dp)
        end if
    end do

    ! ========================================
    ! FFT INVERSA
    ! ========================================

    plan_backward = fftw_plan_dft_1d(n, c_loc(output), c_loc(input), &
                                      FFTW_BACKWARD, FFTW_ESTIMATE)

    call fftw_execute(plan_backward)

    ! Normalizar (FFTW no normaliza)
    input = input / real(n, dp)

    print *, ""
    print *, "=== Verificacion de reconstruccion ==="
    print *, "Primeras muestras de senal reconstruida:"
    do i = 1, 5
        t = real(i - 1, dp) * dt
        print '(A, F8.5, A, F10.6)', "t=", t, " y=", real(input(i))
    end do

    ! ========================================
    ! LIMPIAR
    ! ========================================

    call fftw_destroy_plan(plan_forward)
    call fftw_destroy_plan(plan_backward)
    deallocate(input, output)

end program ejemplo_fftw
```

### 7.3 Aplicaciones de FFT

```fortran
module aplicaciones_fft
    use fft_basico
    implicit none

contains

    ! ========================================
    ! CONVOLUCION VIA FFT
    ! ========================================
    !
    ! Convolucion circular: c = IFFT(FFT(a) * FFT(b))
    ! O(n*log(n)) en lugar de O(n^2)

    subroutine convolucion_fft(a, b, c, n)
        integer, intent(in) :: n
        real(dp), intent(in) :: a(n), b(n)
        real(dp), intent(out) :: c(n)

        complex(dp) :: A_fft(0:n-1), B_fft(0:n-1), C_fft(0:n-1)
        integer :: i

        ! Convertir a complejo
        A_fft = cmplx(a, 0.0_dp, dp)
        B_fft = cmplx(b, 0.0_dp, dp)

        ! Transformar
        call fft_iterative(A_fft, n)
        call fft_iterative(B_fft, n)

        ! Multiplicar en dominio de frecuencia
        C_fft = A_fft * B_fft

        ! Transformar de vuelta
        call ifft(C_fft, n)

        ! Extraer parte real
        c = real(C_fft)

    end subroutine convolucion_fft

    ! ========================================
    ! CORRELACION VIA FFT
    ! ========================================
    !
    ! corr(a, b) = IFFT(FFT(a) * conj(FFT(b)))

    subroutine correlacion_fft(a, b, corr, n)
        integer, intent(in) :: n
        real(dp), intent(in) :: a(n), b(n)
        real(dp), intent(out) :: corr(n)

        complex(dp) :: A_fft(0:n-1), B_fft(0:n-1), C_fft(0:n-1)

        A_fft = cmplx(a, 0.0_dp, dp)
        B_fft = cmplx(b, 0.0_dp, dp)

        call fft_iterative(A_fft, n)
        call fft_iterative(B_fft, n)

        ! Correlacion: multiplicar por conjugado
        C_fft = A_fft * conjg(B_fft)

        call ifft(C_fft, n)

        corr = real(C_fft)

    end subroutine correlacion_fft

    ! ========================================
    ! FILTRO PASA-BAJOS
    ! ========================================

    subroutine filtro_pasa_bajos(signal, filtered, n, fs, cutoff)
        integer, intent(in) :: n
        real(dp), intent(in) :: signal(n), fs, cutoff
        real(dp), intent(out) :: filtered(n)

        complex(dp) :: X(0:n-1)
        real(dp) :: freq, df
        integer :: k

        X = cmplx(signal, 0.0_dp, dp)
        call fft_iterative(X, n)

        df = fs / real(n, dp)

        do k = 0, n - 1
            if (k <= n/2) then
                freq = real(k, dp) * df
            else
                freq = real(k - n, dp) * df
            end if

            if (abs(freq) > cutoff) then
                X(k) = (0.0_dp, 0.0_dp)
            end if
        end do

        call ifft(X, n)
        filtered = real(X)

    end subroutine filtro_pasa_bajos

    ! ========================================
    ! ESPECTRO DE POTENCIA
    ! ========================================
    !
    ! PSD = |X(f)|^2 / N

    subroutine power_spectrum(signal, psd, n)
        integer, intent(in) :: n
        real(dp), intent(in) :: signal(n)
        real(dp), intent(out) :: psd(0:n/2)

        complex(dp) :: X(0:n-1)
        integer :: k

        X = cmplx(signal, 0.0_dp, dp)
        call fft_iterative(X, n)

        do k = 0, n/2
            psd(k) = (abs(X(k))**2) / real(n, dp)
        end do

        ! Duplicar para frecuencias positivas (excepto DC y Nyquist)
        psd(1:n/2-1) = 2.0_dp * psd(1:n/2-1)

    end subroutine power_spectrum

end module aplicaciones_fft


program demo_fft
    use fft_basico
    use aplicaciones_fft
    implicit none

    integer, parameter :: n = 256
    real(dp) :: signal(n), filtered(n), psd(0:n/2)
    real(dp) :: fs, dt, t
    integer :: i

    fs = 1000.0_dp
    dt = 1.0_dp / fs

    ! Crear senal con ruido
    do i = 1, n
        t = real(i - 1, dp) * dt
        ! Senal: 50Hz + ruido
        call random_number(signal(i))
        signal(i) = sin(2.0_dp * PI * 50.0_dp * t) + 0.5_dp * (signal(i) - 0.5_dp)
    end do

    ! Filtrar a 75Hz
    call filtro_pasa_bajos(signal, filtered, n, fs, 75.0_dp)

    ! Calcular espectro de potencia
    call power_spectrum(signal, psd, n)

    print *, "=== Espectro de potencia (primeras frecuencias) ==="
    do i = 0, 20
        print '(A, F6.1, A, ES12.4)', "f=", real(i, dp) * fs / real(n, dp), &
              " Hz, PSD=", psd(i)
    end do

end program demo_fft
```

---

## Referencia Rapida

### Precision y KIND

```fortran
integer, parameter :: sp = selected_real_kind(6)   ! ~7 digitos
integer, parameter :: dp = selected_real_kind(15)  ! ~15 digitos
real(dp), parameter :: x = 1.0_dp                  ! Literal con KIND
```

### Funciones Matematicas

| Funcion | Descripcion |
|---------|-------------|
| `sin`, `cos`, `tan` | Trigonometricas (radianes) |
| `asin`, `acos`, `atan` | Inversas |
| `sinh`, `cosh`, `tanh` | Hiperbolicas |
| `exp`, `log`, `log10` | Exponencial/Logaritmo |
| `sqrt`, `**` | Raiz/Potencia |
| `bessel_j0`, `bessel_y0` | Bessel (F2008) |
| `gamma`, `log_gamma` | Funcion Gamma |
| `erf`, `erfc` | Funcion Error |

### Numeros Aleatorios

```fortran
call random_number(x)      ! Uniforme [0,1)
call random_seed(put=seed) ! Establecer semilla
call random_seed(get=seed) ! Obtener semilla
```

### Integracion Numerica

| Metodo | Orden | Mejor para |
|--------|-------|------------|
| Trapecio | O(h^2) | Funciones suaves |
| Simpson | O(h^4) | Funciones suaves |
| Gauss-Legendre | Exacta grado 2n-1 | Alta precision |

### Ecuaciones Diferenciales

| Metodo | Orden | Estabilidad |
|--------|-------|-------------|
| Euler | O(h) | Condicional |
| RK2 | O(h^2) | Condicional |
| RK4 | O(h^4) | Condicional |
| RKF45 | Adaptativo | Buena |

---

## Conclusion

La computacion cientifica en Fortran combina precision matematica con eficiencia computacional. Desde el control fino de precision numerica hasta algoritmos sofisticados de FFT, Fortran proporciona las herramientas necesarias para simular el universo con fidelidad digital.

**Puntos clave:**
1. Usar `selected_real_kind` para portabilidad de precision
2. Aplicar suma de Kahan para reducir errores de redondeo
3. RK4 es el caballo de batalla para EDOs
4. La descomposicion LU es mas eficiente que Gauss para multiples sistemas
5. FFT via FFTW para rendimiento optimo

---

*"En el templo de la computacion cientifica, Fortran sigue siendo el sumo sacerdote - traduciendo ecuaciones en resultados desde 1957, un numero de punto flotante a la vez."*

---
title: "ARSENAL Reference: Fortran Intrinsics"
category: reference
domain: scientific
language: Fortran
standards: [F77, F90, F95, F2003, F2008, F2018]
version: 1.0.0
last_updated: 2025-12-31
tags: [fortran, intrinsics, scientific-computing, legacy]
---

# ARSENAL_REF_FORTRAN
## Complete Fortran Intrinsic Functions Reference

---

# TABLE OF CONTENTS

1. [Numeric Functions](#numeric-functions)
2. [Mathematical Functions](#mathematical-functions)
3. [Character Functions](#character-functions)
4. [Array Intrinsics](#array-intrinsics)
5. [Inquiry Functions](#inquiry-functions)
6. [I/O Functions and Format](#io-functions-and-format)
7. [F77 vs F90+ Equivalents](#f77-vs-f90-equivalents)

---

# NUMERIC FUNCTIONS

## Absolute Value and Sign

| Function | Description | Example | Result |
|----------|-------------|---------|--------|
| `ABS(a)` | Absolute value | `ABS(-5)` | `5` |
| `IABS(a)` | Integer absolute (F77) | `IABS(-5)` | `5` |
| `DABS(a)` | Double absolute (F77) | `DABS(-5.0D0)` | `5.0D0` |
| `CABS(a)` | Complex magnitude (F77) | `CABS((3.0,4.0))` | `5.0` |
| `SIGN(a, b)` | Magnitude of a, sign of b | `SIGN(5, -3)` | `-5` |
| `ISIGN(a, b)` | Integer sign (F77) | `ISIGN(5, -3)` | `-5` |
| `DSIGN(a, b)` | Double sign (F77) | `DSIGN(5.0D0, -3.0D0)` | `-5.0D0` |

### Examples
```fortran
! ABS works for any numeric type in F90+
INTEGER :: i = -42
REAL :: r = -3.14
COMPLEX :: c = (3.0, 4.0)

PRINT *, ABS(i)    ! 42
PRINT *, ABS(r)    ! 3.14
PRINT *, ABS(c)    ! 5.0 (magnitude)
```

## Modulo and Remainder

| Function | Description | Formula | Example |
|----------|-------------|---------|---------|
| `MOD(a, p)` | Remainder | a - INT(a/p)*p | `MOD(8, 3)` = `2` |
| `MODULO(a, p)` | Modulo (F90) | a - FLOOR(a/p)*p | `MODULO(-8, 3)` = `1` |
| `AMOD(a, p)` | Real remainder (F77) | - | `AMOD(8.0, 3.0)` = `2.0` |
| `DMOD(a, p)` | Double remainder (F77) | - | `DMOD(8.0D0, 3.0D0)` = `2.0D0` |

### MOD vs MODULO
```fortran
! MOD: sign of result matches sign of dividend
PRINT *, MOD(8, 3)    !  2
PRINT *, MOD(-8, 3)   ! -2
PRINT *, MOD(8, -3)   !  2
PRINT *, MOD(-8, -3)  ! -2

! MODULO: sign of result matches sign of divisor
PRINT *, MODULO(8, 3)    !  2
PRINT *, MODULO(-8, 3)   !  1  (different!)
PRINT *, MODULO(8, -3)   ! -1  (different!)
PRINT *, MODULO(-8, -3)  ! -2
```

## Min and Max

| Function | Description | Example | Result |
|----------|-------------|---------|--------|
| `MAX(a1, a2, ...)` | Maximum value | `MAX(3, 1, 4)` | `4` |
| `MIN(a1, a2, ...)` | Minimum value | `MIN(3, 1, 4)` | `1` |
| `MAX0(a1, a2, ...)` | Integer max (F77) | - | - |
| `AMAX1(a1, a2, ...)` | Real max (F77) | - | - |
| `DMAX1(a1, a2, ...)` | Double max (F77) | - | - |
| `MIN0(a1, a2, ...)` | Integer min (F77) | - | - |
| `AMIN1(a1, a2, ...)` | Real min (F77) | - | - |
| `DMIN1(a1, a2, ...)` | Double min (F77) | - | - |

## Type Conversion

| Function | Description | Result Type | Example |
|----------|-------------|-------------|---------|
| `INT(a)` | Convert to integer | INTEGER | `INT(3.7)` = `3` |
| `NINT(a)` | Nearest integer | INTEGER | `NINT(3.7)` = `4` |
| `ANINT(a)` | Nearest whole number | REAL | `ANINT(3.7)` = `4.0` |
| `REAL(a)` | Convert to real | REAL | `REAL(5)` = `5.0` |
| `FLOAT(a)` | Integer to real (F77) | REAL | `FLOAT(5)` = `5.0` |
| `DBLE(a)` | Convert to double | DOUBLE | `DBLE(5)` = `5.0D0` |
| `CMPLX(x, y)` | Create complex | COMPLEX | `CMPLX(3.0, 4.0)` |
| `DCMPLX(x, y)` | Double complex | DOUBLE COMPLEX | - |
| `AINT(a)` | Truncate to whole | REAL | `AINT(3.7)` = `3.0` |
| `CEILING(a)` | Round up (F90) | INTEGER | `CEILING(3.1)` = `4` |
| `FLOOR(a)` | Round down (F90) | INTEGER | `FLOOR(3.9)` = `3` |

## Complex Number Functions

| Function | Description | Example |
|----------|-------------|---------|
| `REAL(z)` | Real part | `REAL((3.0, 4.0))` = `3.0` |
| `AIMAG(z)` | Imaginary part | `AIMAG((3.0, 4.0))` = `4.0` |
| `CONJG(z)` | Complex conjugate | `CONJG((3.0, 4.0))` = `(3.0, -4.0)` |

## Bit Operations

| Function | Description | Example |
|----------|-------------|---------|
| `IAND(i, j)` | Bitwise AND | `IAND(12, 10)` = `8` |
| `IOR(i, j)` | Bitwise OR | `IOR(12, 10)` = `14` |
| `IEOR(i, j)` | Bitwise XOR | `IEOR(12, 10)` = `6` |
| `NOT(i)` | Bitwise NOT | `NOT(0)` = `-1` |
| `ISHFT(i, shift)` | Bit shift | `ISHFT(4, 1)` = `8` |
| `ISHFTC(i, shift, size)` | Circular shift | - |
| `IBSET(i, pos)` | Set bit | `IBSET(0, 3)` = `8` |
| `IBCLR(i, pos)` | Clear bit | `IBCLR(15, 3)` = `7` |
| `BTEST(i, pos)` | Test bit | `BTEST(8, 3)` = `.TRUE.` |
| `IBITS(i, pos, len)` | Extract bits | `IBITS(255, 2, 4)` = `15` |
| `MVBITS(from, frompos, len, to, topos)` | Move bits (subroutine) | - |

---

# MATHEMATICAL FUNCTIONS

## Exponential and Logarithmic

| Function | Description | Domain | Example |
|----------|-------------|--------|---------|
| `EXP(x)` | e^x | All | `EXP(1.0)` = `2.718...` |
| `DEXP(x)` | Double exp (F77) | All | - |
| `CEXP(x)` | Complex exp (F77) | All | - |
| `LOG(x)` | Natural log | x > 0 | `LOG(2.718)` = `1.0` |
| `ALOG(x)` | Real log (F77) | x > 0 | - |
| `DLOG(x)` | Double log (F77) | x > 0 | - |
| `CLOG(x)` | Complex log (F77) | x != 0 | - |
| `LOG10(x)` | Base-10 log | x > 0 | `LOG10(100.0)` = `2.0` |
| `ALOG10(x)` | Real log10 (F77) | x > 0 | - |
| `DLOG10(x)` | Double log10 (F77) | x > 0 | - |

## Power and Root

| Function | Description | Domain | Example |
|----------|-------------|--------|---------|
| `SQRT(x)` | Square root | x >= 0 | `SQRT(16.0)` = `4.0` |
| `DSQRT(x)` | Double sqrt (F77) | x >= 0 | - |
| `CSQRT(x)` | Complex sqrt (F77) | All | - |
| `x ** y` | Power | - | `2.0 ** 3.0` = `8.0` |

## Trigonometric Functions

| Function | Description | Domain | Range |
|----------|-------------|--------|-------|
| `SIN(x)` | Sine | Radians | [-1, 1] |
| `COS(x)` | Cosine | Radians | [-1, 1] |
| `TAN(x)` | Tangent | Radians | R |
| `ASIN(x)` | Arc sine | [-1, 1] | [-pi/2, pi/2] |
| `ACOS(x)` | Arc cosine | [-1, 1] | [0, pi] |
| `ATAN(x)` | Arc tangent | R | [-pi/2, pi/2] |
| `ATAN2(y, x)` | Arc tangent of y/x | - | [-pi, pi] |

### F77 Specific Type Names
| Generic (F90) | REAL (F77) | DOUBLE (F77) | COMPLEX (F77) |
|---------------|------------|--------------|---------------|
| `SIN` | `SIN` | `DSIN` | `CSIN` |
| `COS` | `COS` | `DCOS` | `CCOS` |
| `TAN` | `TAN` | `DTAN` | - |
| `ASIN` | `ASIN` | `DASIN` | - |
| `ACOS` | `ACOS` | `DACOS` | - |
| `ATAN` | `ATAN` | `DATAN` | - |
| `ATAN2` | `ATAN2` | `DATAN2` | - |

## Hyperbolic Functions

| Function | Description | Example |
|----------|-------------|---------|
| `SINH(x)` | Hyperbolic sine | `SINH(0.0)` = `0.0` |
| `COSH(x)` | Hyperbolic cosine | `COSH(0.0)` = `1.0` |
| `TANH(x)` | Hyperbolic tangent | `TANH(0.0)` = `0.0` |
| `ASINH(x)` | Inverse sinh (F2008) | - |
| `ACOSH(x)` | Inverse cosh (F2008) | - |
| `ATANH(x)` | Inverse tanh (F2008) | - |

## Special Functions (F2008+)

| Function | Description |
|----------|-------------|
| `BESSEL_J0(x)` | Bessel function J0 |
| `BESSEL_J1(x)` | Bessel function J1 |
| `BESSEL_JN(n, x)` | Bessel function Jn |
| `BESSEL_Y0(x)` | Bessel function Y0 |
| `BESSEL_Y1(x)` | Bessel function Y1 |
| `BESSEL_YN(n, x)` | Bessel function Yn |
| `ERF(x)` | Error function |
| `ERFC(x)` | Complementary error |
| `ERFC_SCALED(x)` | Scaled complementary error |
| `GAMMA(x)` | Gamma function |
| `LOG_GAMMA(x)` | Log of gamma |
| `HYPOT(x, y)` | sqrt(x^2 + y^2) |

---

# CHARACTER FUNCTIONS

## Character Information

| Function | Description | Example | Result |
|----------|-------------|---------|--------|
| `LEN(string)` | Declared length | `LEN('Hello')` | `5` |
| `LEN_TRIM(string)` | Length without trailing spaces | `LEN_TRIM('Hi  ')` | `2` |
| `ICHAR(c)` | Character to code | `ICHAR('A')` | `65` |
| `CHAR(i)` | Code to character | `CHAR(65)` | `'A'` |
| `IACHAR(c)` | Character to ASCII | `IACHAR('A')` | `65` |
| `ACHAR(i)` | ASCII to character | `ACHAR(65)` | `'A'` |

## String Manipulation

| Function | Description | Example | Result |
|----------|-------------|---------|--------|
| `TRIM(string)` | Remove trailing spaces | `TRIM('Hi  ')` | `'Hi'` |
| `ADJUSTL(string)` | Left-justify | `ADJUSTL('  Hi')` | `'Hi  '` |
| `ADJUSTR(string)` | Right-justify | `ADJUSTR('Hi  ')` | `'  Hi'` |
| `REPEAT(string, n)` | Repeat n times | `REPEAT('ab', 3)` | `'ababab'` |

## String Search

| Function | Description | Example | Result |
|----------|-------------|---------|--------|
| `INDEX(str, sub)` | Find substring | `INDEX('Hello', 'l')` | `3` |
| `INDEX(str, sub, .TRUE.)` | From end | `INDEX('Hello', 'l', .TRUE.)` | `4` |
| `SCAN(str, set)` | Find any char from set | `SCAN('Hello', 'ol')` | `2` |
| `VERIFY(str, set)` | Find char NOT in set | `VERIFY('Hello', 'Helo')` | `0` |

### Search Examples
```fortran
CHARACTER(LEN=20) :: str = 'Hello World'

! Find first 'o'
PRINT *, INDEX(str, 'o')      ! 5

! Find last 'o'
PRINT *, INDEX(str, 'o', BACK=.TRUE.)  ! 8

! Find any vowel
PRINT *, SCAN(str, 'aeiou')   ! 2 (position of 'e')

! Find first non-letter
PRINT *, VERIFY(str, 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')  ! 6 (space)
```

## String Comparison

| Function | Description | Returns |
|----------|-------------|---------|
| `LGE(str1, str2)` | str1 >= str2 (lexical) | LOGICAL |
| `LGT(str1, str2)` | str1 > str2 | LOGICAL |
| `LLE(str1, str2)` | str1 <= str2 | LOGICAL |
| `LLT(str1, str2)` | str1 < str2 | LOGICAL |

---

# ARRAY INTRINSICS

## Array Information

| Function | Description | Example |
|----------|-------------|---------|
| `SIZE(array [, dim])` | Total elements or along dim | `SIZE(A)`, `SIZE(A, 1)` |
| `SHAPE(source)` | Array of dimensions | `SHAPE(A)` = `[3, 4]` |
| `LBOUND(array [, dim])` | Lower bounds | `LBOUND(A)` = `[1, 1]` |
| `UBOUND(array [, dim])` | Upper bounds | `UBOUND(A)` = `[3, 4]` |

### Examples
```fortran
REAL :: A(3, 4, 5)

PRINT *, SIZE(A)        ! 60
PRINT *, SIZE(A, 1)     ! 3
PRINT *, SIZE(A, 2)     ! 4
PRINT *, SHAPE(A)       ! 3 4 5
PRINT *, LBOUND(A)      ! 1 1 1
PRINT *, UBOUND(A)      ! 3 4 5
```

## Array Construction and Reshaping

| Function | Description | Example |
|----------|-------------|---------|
| `RESHAPE(source, shape [, pad, order])` | Reshape array | See below |
| `SPREAD(source, dim, ncopies)` | Replicate array | See below |
| `MERGE(tsource, fsource, mask)` | Conditional merge | See below |
| `PACK(array, mask [, vector])` | Pack to 1D | See below |
| `UNPACK(vector, mask, field)` | Unpack to array | See below |

### Examples
```fortran
! RESHAPE
INTEGER :: A(6) = [1, 2, 3, 4, 5, 6]
INTEGER :: B(2, 3)
B = RESHAPE(A, [2, 3])
! B = | 1 3 5 |
!     | 2 4 6 |

! SPREAD - add a dimension
REAL :: vec(3) = [1.0, 2.0, 3.0]
REAL :: mat(3, 4)
mat = SPREAD(vec, DIM=2, NCOPIES=4)
! mat = | 1 1 1 1 |
!       | 2 2 2 2 |
!       | 3 3 3 3 |

! MERGE - conditional selection
REAL :: A(5) = [1, 2, 3, 4, 5]
REAL :: B(5) = [10, 20, 30, 40, 50]
LOGICAL :: mask(5) = [.TRUE., .FALSE., .TRUE., .FALSE., .TRUE.]
REAL :: C(5)
C = MERGE(A, B, mask)
! C = [1, 20, 3, 40, 5]

! PACK - extract elements
REAL :: A(3, 3) = RESHAPE([1,2,3,4,5,6,7,8,9], [3,3])
REAL, ALLOCATABLE :: B(:)
B = PACK(A, A > 5)
! B = [6, 7, 8, 9]
```

## Array Reduction Functions

| Function | Description | Example |
|----------|-------------|---------|
| `SUM(array [, dim, mask])` | Sum elements | `SUM([1,2,3])` = `6` |
| `PRODUCT(array [, dim, mask])` | Product of elements | `PRODUCT([1,2,3])` = `6` |
| `MAXVAL(array [, dim, mask])` | Maximum value | `MAXVAL([1,5,3])` = `5` |
| `MINVAL(array [, dim, mask])` | Minimum value | `MINVAL([1,5,3])` = `1` |
| `COUNT(mask [, dim])` | Count .TRUE. | `COUNT([.T.,.F.,.T.])` = `2` |
| `ANY(mask [, dim])` | Any .TRUE.? | `ANY([.F.,.T.,.F.])` = `.T.` |
| `ALL(mask [, dim])` | All .TRUE.? | `ALL([.T.,.T.,.F.])` = `.F.` |

### Reduction with DIM
```fortran
INTEGER :: A(3, 4)
A = RESHAPE([1,2,3,4,5,6,7,8,9,10,11,12], [3,4])
! A = | 1  4  7 10 |
!     | 2  5  8 11 |
!     | 3  6  9 12 |

! Sum all elements
PRINT *, SUM(A)              ! 78

! Sum along rows (dim=1)
PRINT *, SUM(A, DIM=1)       ! [6, 15, 24, 33]

! Sum along columns (dim=2)
PRINT *, SUM(A, DIM=2)       ! [22, 26, 30]

! Sum with mask
PRINT *, SUM(A, MASK=A>5)    ! 57
```

## Array Location Functions

| Function | Description | Returns |
|----------|-------------|---------|
| `MAXLOC(array [, dim, mask])` | Location of maximum | Array of indices |
| `MINLOC(array [, dim, mask])` | Location of minimum | Array of indices |
| `FINDLOC(array, value [, dim, mask])` | Location of value (F2008) | Array of indices |

### Location Examples
```fortran
REAL :: A(3, 3) = RESHAPE([1,4,2,5,9,6,3,8,7], [3,3])
! A = | 1 5 3 |
!     | 4 9 8 |
!     | 2 6 7 |

PRINT *, MAXLOC(A)           ! [2, 2] (position of 9)
PRINT *, MINLOC(A)           ! [1, 1] (position of 1)
PRINT *, MAXLOC(A, DIM=1)    ! [2, 2, 2] (max in each column)
PRINT *, MAXLOC(A, MASK=A<8) ! [2, 3] (position of 7, max < 8)
```

## Matrix Operations

| Function | Description | Example |
|----------|-------------|---------|
| `MATMUL(a, b)` | Matrix multiplication | `MATMUL(A, B)` |
| `DOT_PRODUCT(a, b)` | Dot product | `DOT_PRODUCT(v1, v2)` |
| `TRANSPOSE(matrix)` | Matrix transpose | `TRANSPOSE(A)` |

### Matrix Examples
```fortran
REAL :: A(2, 3), B(3, 2), C(2, 2)
REAL :: v1(3), v2(3)

! Matrix multiplication: C = A * B
! A(2,3) * B(3,2) = C(2,2)
C = MATMUL(A, B)

! Dot product
v1 = [1.0, 2.0, 3.0]
v2 = [4.0, 5.0, 6.0]
PRINT *, DOT_PRODUCT(v1, v2)  ! 32.0 (1*4 + 2*5 + 3*6)

! Transpose
PRINT *, SHAPE(A)             ! 2 3
PRINT *, SHAPE(TRANSPOSE(A))  ! 3 2
```

## Array Shift Functions

| Function | Description | Example |
|----------|-------------|---------|
| `CSHIFT(array, shift [, dim])` | Circular shift | See below |
| `EOSHIFT(array, shift [, boundary, dim])` | End-off shift | See below |

### Shift Examples
```fortran
INTEGER :: A(5) = [1, 2, 3, 4, 5]

! Circular shift
PRINT *, CSHIFT(A, 2)         ! [3, 4, 5, 1, 2]
PRINT *, CSHIFT(A, -1)        ! [5, 1, 2, 3, 4]

! End-off shift (zeros fill)
PRINT *, EOSHIFT(A, 2)        ! [3, 4, 5, 0, 0]
PRINT *, EOSHIFT(A, -2)       ! [0, 0, 1, 2, 3]
PRINT *, EOSHIFT(A, 2, 99)    ! [3, 4, 5, 99, 99] (custom boundary)
```

---

# INQUIRY FUNCTIONS

## Type Inquiry

| Function | Description | Example |
|----------|-------------|---------|
| `KIND(x)` | Kind type parameter | `KIND(1.0)` |
| `SELECTED_INT_KIND(r)` | Integer kind for range | `SELECTED_INT_KIND(9)` |
| `SELECTED_REAL_KIND(p, r)` | Real kind for precision | `SELECTED_REAL_KIND(15)` |

### Kind Examples
```fortran
! Define types with specific precision
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)   ! Single
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307) ! Double
INTEGER, PARAMETER :: qp = SELECTED_REAL_KIND(33, 4931) ! Quad

REAL(sp) :: single_val
REAL(dp) :: double_val
REAL(qp) :: quad_val

! Check kind values
PRINT *, KIND(single_val)  ! Usually 4
PRINT *, KIND(double_val)  ! Usually 8
```

## Numeric Inquiry

| Function | Description | Example |
|----------|-------------|---------|
| `HUGE(x)` | Largest value | `HUGE(1.0)` |
| `TINY(x)` | Smallest positive | `TINY(1.0)` |
| `EPSILON(x)` | Machine epsilon | `EPSILON(1.0)` |
| `PRECISION(x)` | Decimal precision | `PRECISION(1.0D0)` |
| `RANGE(x)` | Decimal exponent range | `RANGE(1.0)` |
| `RADIX(x)` | Base of model | `RADIX(1.0)` (usually 2) |
| `DIGITS(x)` | Significant digits | `DIGITS(1.0)` |
| `MAXEXPONENT(x)` | Maximum exponent | `MAXEXPONENT(1.0)` |
| `MINEXPONENT(x)` | Minimum exponent | `MINEXPONENT(1.0)` |
| `BIT_SIZE(i)` | Number of bits | `BIT_SIZE(1)` |

### Inquiry Examples
```fortran
PRINT *, 'HUGE real:     ', HUGE(1.0)        ! ~3.4E+38
PRINT *, 'HUGE double:   ', HUGE(1.0D0)      ! ~1.8E+308
PRINT *, 'TINY real:     ', TINY(1.0)        ! ~1.2E-38
PRINT *, 'EPSILON real:  ', EPSILON(1.0)     ! ~1.2E-7
PRINT *, 'EPSILON double:', EPSILON(1.0D0)   ! ~2.2E-16
PRINT *, 'PRECISION:     ', PRECISION(1.0D0) ! 15
PRINT *, 'BIT_SIZE int:  ', BIT_SIZE(1)      ! 32 or 64
```

## Allocation Status

| Function | Description | Example |
|----------|-------------|---------|
| `ALLOCATED(array)` | Is array allocated? | `IF (ALLOCATED(A)) ...` |
| `ASSOCIATED(ptr [, target])` | Is pointer associated? | `IF (ASSOCIATED(p)) ...` |
| `PRESENT(a)` | Is optional arg present? | `IF (PRESENT(x)) ...` |

### Examples
```fortran
REAL, ALLOCATABLE :: A(:)
REAL, POINTER :: p => NULL()

! Check allocation
IF (.NOT. ALLOCATED(A)) ALLOCATE(A(100))

! Check pointer association
IF (ASSOCIATED(p)) THEN
    PRINT *, p
END IF

! Optional argument
SUBROUTINE process(x, optional_flag)
    REAL, INTENT(IN) :: x
    LOGICAL, OPTIONAL, INTENT(IN) :: optional_flag

    IF (PRESENT(optional_flag)) THEN
        IF (optional_flag) PRINT *, 'Flag is set'
    END IF
END SUBROUTINE
```

---

# I/O FUNCTIONS AND FORMAT

## Format Specifiers

### Numeric Formats

| Format | Description | Example | Output |
|--------|-------------|---------|--------|
| `Iw` | Integer, width w | `I5` | ` 123` |
| `Iw.m` | Integer, min m digits | `I5.3` | ` 012` |
| `Fw.d` | Fixed decimal | `F8.2` | ` 123.45` |
| `Ew.d` | Scientific | `E12.4` | ` 0.1234E+03` |
| `ENw.d` | Engineering | `EN12.3` | ` 123.456E+00` |
| `ESw.d` | Scientific | `ES12.4` | ` 1.2345E+02` |
| `Gw.d` | General (auto E/F) | `G12.4` | Varies |
| `Dw.d` | Double scientific | `D12.4` | ` 0.1234D+03` |

### Character and Logical Formats

| Format | Description | Example | Output |
|--------|-------------|---------|--------|
| `Aw` | Character, width w | `A10` | `    Hello` |
| `A` | Character, actual length | `A` | `Hello` |
| `Lw` | Logical, width w | `L2` | ` T` or ` F` |

### Positional Formats

| Format | Description | Example |
|--------|-------------|---------|
| `nX` | Skip n positions | `5X` |
| `Tc` | Tab to column c | `T10` |
| `TLn` | Tab left n columns | `TL5` |
| `TRn` | Tab right n columns | `TR5` |
| `/` | New line | `/` |
| `'text'` | Literal string | `'Value: '` |

### Format Examples
```fortran
INTEGER :: i = 42
REAL :: x = 123.456
CHARACTER(LEN=10) :: s = 'Hello'

! Basic formatting
WRITE(*, '(I5)') i           !    42
WRITE(*, '(F10.3)') x        !   123.456
WRITE(*, '(E12.4)') x        ! 0.1235E+03
WRITE(*, '(A)') s            ! Hello

! Combined format
WRITE(*, '(A, I5, A, F10.3)') 'Count: ', i, ' Value: ', x
! Count:    42 Value:    123.456

! Repeated format
WRITE(*, '(3I5)') 1, 2, 3    !    1    2    3

! Multi-line
WRITE(*, '(A, /, A)') 'Line 1', 'Line 2'
```

## I/O Statements

### READ
```fortran
READ(unit, format [, iostat=ios, err=label, end=label]) var_list
READ *, var_list                    ! List-directed from stdin
READ '(format)', var_list           ! Formatted from stdin
READ(unit, *) var_list              ! List-directed from unit
READ(unit, '(format)') var_list     ! Formatted from unit
```

### WRITE
```fortran
WRITE(unit, format [, iostat=ios, err=label]) var_list
WRITE(*, *) var_list                ! List-directed to stdout
PRINT *, var_list                   ! List-directed to stdout
PRINT '(format)', var_list          ! Formatted to stdout
```

### File Operations
```fortran
OPEN(unit, FILE='name' [, STATUS='status', ACTION='action', ...])
CLOSE(unit [, STATUS='status'])
REWIND(unit)
BACKSPACE(unit)
ENDFILE(unit)
INQUIRE(unit, ...)
```

### OPEN Options
| Option | Values | Description |
|--------|--------|-------------|
| `STATUS` | `'OLD', 'NEW', 'REPLACE', 'SCRATCH', 'UNKNOWN'` | File existence |
| `ACTION` | `'READ', 'WRITE', 'READWRITE'` | Access mode |
| `ACCESS` | `'SEQUENTIAL', 'DIRECT', 'STREAM'` | Access method |
| `FORM` | `'FORMATTED', 'UNFORMATTED'` | Record format |
| `POSITION` | `'ASIS', 'REWIND', 'APPEND'` | Initial position |
| `IOSTAT` | Variable | Error status |

---

# F77 vs F90+ EQUIVALENTS

## Generic vs Specific Names

| Generic (F90+) | Integer (F77) | Real (F77) | Double (F77) | Complex (F77) |
|----------------|---------------|------------|--------------|---------------|
| `ABS` | `IABS` | `ABS` | `DABS` | `CABS` |
| `MOD` | `MOD` | `AMOD` | `DMOD` | - |
| `MAX` | `MAX0` | `AMAX1` | `DMAX1` | - |
| `MIN` | `MIN0` | `AMIN1` | `DMIN1` | - |
| `SIGN` | `ISIGN` | `SIGN` | `DSIGN` | - |
| `INT` | - | `INT`, `IFIX` | `IDINT` | - |
| `REAL` | `FLOAT`, `REAL` | - | `SNGL` | `REAL` |
| `NINT` | - | `NINT` | `IDNINT` | - |

## Deprecated Functions

| Deprecated (F77) | Modern (F90+) | Notes |
|------------------|---------------|-------|
| `ALOG` | `LOG` | Generic works for all types |
| `ALOG10` | `LOG10` | Generic works for all types |
| `AMAX0` | `MAX` | Mixed type max |
| `AMIN0` | `MIN` | Mixed type min |
| `MAX1` | `MAX` | Mixed type max |
| `MIN1` | `MIN` | Mixed type min |
| `FLOAT` | `REAL` | Use generic |
| `IFIX` | `INT` | Use generic |
| `DFLOAT` | `DBLE` | Vendor extension |

## New in F90 and Later

### F90 Additions
- Array intrinsics: `RESHAPE`, `SPREAD`, `PACK`, `UNPACK`
- Reductions: `SUM`, `PRODUCT`, `MAXVAL`, `MINVAL`, `COUNT`, `ANY`, `ALL`
- Matrix ops: `MATMUL`, `DOT_PRODUCT`, `TRANSPOSE`
- String functions: `TRIM`, `ADJUSTL`, `ADJUSTR`, `REPEAT`
- Array info: `SIZE`, `SHAPE`, `LBOUND`, `UBOUND`
- Kind selection: `SELECTED_INT_KIND`, `SELECTED_REAL_KIND`
- Inquiry: `ALLOCATED`, `ASSOCIATED`, `PRESENT`

### F95 Additions
- `NULL()` - Null pointer
- `CPU_TIME()` - Processor time
- Elemental user functions

### F2003 Additions
- `MOVE_ALLOC()` - Move allocation
- `COMMAND_ARGUMENT_COUNT()` - Command line
- `GET_COMMAND()`, `GET_COMMAND_ARGUMENT()`
- `GET_ENVIRONMENT_VARIABLE()`
- IEEE intrinsic modules

### F2008 Additions
- `FINDLOC()` - Find value location
- `BESSEL_J0/J1/JN()`, `BESSEL_Y0/Y1/YN()` - Bessel functions
- `ERF()`, `ERFC()`, `ERFC_SCALED()` - Error functions
- `GAMMA()`, `LOG_GAMMA()` - Gamma functions
- `HYPOT()` - Hypotenuse
- `ACOSH()`, `ASINH()`, `ATANH()` - Inverse hyperbolic
- Coarray intrinsics

### F2018 Additions
- `OUT_OF_RANGE()` - Check conversion
- `REDUCE()` - User-defined reduction
- Enhanced coarray features

---

*ARCHAEON Arsenal - Fortran Intrinsics Reference v1.0*
*"From FORTRAN 66 to Fortran 2018: The language that shaped scientific computing"*

---
title: "ARCHAEON Arsenal - Fortran Snippets"
version: "1.0.0"
category: "ARSENAL/SNIPPETS"
language: "Fortran (F90/F95/F2003/F2008)"
purpose: "Practical Fortran patterns for scientific computing and HPC"
created: "2025-12-31"
tags: ["fortran", "scientific", "hpc", "openmp", "numerical", "arrays"]
complexity: "Intermediate to Advanced"
---

# ARCHAEON ARSENAL: Fortran Snippets

> **Mission**: Provide production-ready Fortran patterns for scientific computing,
> numerical methods, parallel processing, and high-performance applications.

---

## Table of Contents

1. [Program Structure](#program-structure)
2. [Array Operations](#array-operations)
3. [File I/O Patterns](#file-io-patterns)
4. [Matrix Operations](#matrix-operations)
5. [OpenMP Parallelization](#openmp-parallelization)
6. [Module Patterns](#module-patterns)
7. [Numerical Methods](#numerical-methods)
8. [Interoperability](#interoperability)

---

## Program Structure

### Standard Program Template

```fortran
!===============================================================================
! Program: SAMPLE_PROGRAM
! Purpose: Standard Fortran program template with modern features
! Author:  ARCHAEON Arsenal
! Date:    2025-12-31
!===============================================================================
program sample_program
    use iso_fortran_env, only: real64, int32, stderr => error_unit
    use omp_lib            ! OpenMP library (if needed)
    implicit none

    ! ---- Constants ----
    integer, parameter :: dp = real64
    integer, parameter :: sp = kind(1.0)
    real(dp), parameter :: PI = 3.141592653589793238_dp
    real(dp), parameter :: E  = 2.718281828459045235_dp

    ! ---- Variables ----
    integer :: n, i, ios
    real(dp), allocatable :: data_array(:)
    character(len=256) :: filename
    logical :: file_exists

    ! ---- Main Program ----
    write(*,'(A)') '============================================'
    write(*,'(A)') 'SAMPLE_PROGRAM - Starting execution'
    write(*,'(A)') '============================================'

    ! Initialize
    call initialize()

    ! Process
    call process_data()

    ! Finalize
    call cleanup()

    write(*,'(A)') 'Program completed successfully.'

contains

    !---------------------------------------------------------------------------
    ! Subroutine: initialize
    ! Purpose: Initialize program resources
    !---------------------------------------------------------------------------
    subroutine initialize()
        ! Allocate arrays
        n = 1000
        allocate(data_array(n), stat=ios)
        if (ios /= 0) then
            write(stderr,'(A)') 'ERROR: Failed to allocate data_array'
            stop 1
        end if

        ! Initialize values
        data_array = 0.0_dp
    end subroutine initialize

    !---------------------------------------------------------------------------
    ! Subroutine: process_data
    ! Purpose: Main data processing routine
    !---------------------------------------------------------------------------
    subroutine process_data()
        integer :: j

        ! Process array
        do j = 1, n
            data_array(j) = sin(real(j, dp) * PI / real(n, dp))
        end do

        write(*,'(A,ES15.7)') 'Sum of array: ', sum(data_array)
        write(*,'(A,ES15.7)') 'Max value:    ', maxval(data_array)
        write(*,'(A,ES15.7)') 'Min value:    ', minval(data_array)
    end subroutine process_data

    !---------------------------------------------------------------------------
    ! Subroutine: cleanup
    ! Purpose: Free resources
    !---------------------------------------------------------------------------
    subroutine cleanup()
        if (allocated(data_array)) deallocate(data_array)
    end subroutine cleanup

end program sample_program
```

---

## Array Operations

### Array Slicing and Manipulation

```fortran
!===============================================================================
! Array Operations and Slicing Patterns
!===============================================================================
module array_operations
    use iso_fortran_env, only: real64, int32
    implicit none

    integer, parameter :: dp = real64

contains

    !---------------------------------------------------------------------------
    ! Array Slicing Examples
    !---------------------------------------------------------------------------
    subroutine array_slicing_demo()
        real(dp) :: matrix(10, 10)
        real(dp) :: vector(10)
        real(dp) :: submatrix(5, 5)
        integer :: i, j

        ! Initialize matrix
        do j = 1, 10
            do i = 1, 10
                matrix(i, j) = real(i * 10 + j, dp)
            end do
        end do

        ! ---- Basic Slicing ----
        ! Extract row
        vector = matrix(5, :)         ! 5th row

        ! Extract column
        vector = matrix(:, 3)         ! 3rd column

        ! Extract submatrix
        submatrix = matrix(1:5, 1:5)  ! Upper-left 5x5

        ! Extract with stride
        vector = matrix(1, 1:10:2)    ! Every other element of row 1

        ! Reverse array
        vector = matrix(1, 10:1:-1)   ! Row 1, reversed

        ! ---- Array Assignment ----
        ! Set entire row
        matrix(1, :) = 0.0_dp

        ! Set diagonal
        do i = 1, 10
            matrix(i, i) = 1.0_dp
        end do

        ! Set submatrix
        matrix(6:10, 6:10) = 99.0_dp

        ! ---- WHERE Construct ----
        where (matrix > 50.0_dp)
            matrix = matrix * 2.0_dp
        elsewhere
            matrix = matrix + 10.0_dp
        end where

    end subroutine array_slicing_demo

    !---------------------------------------------------------------------------
    ! Array Intrinsic Functions
    !---------------------------------------------------------------------------
    subroutine array_intrinsics_demo()
        real(dp) :: a(100), b(100), c(100)
        real(dp) :: matrix(10, 10)
        integer :: indices(3)
        integer :: i

        ! Initialize
        a = [(real(i, dp), i=1, 100)]
        b = [(real(i, dp)**2, i=1, 100)]

        ! ---- Reduction Operations ----
        print '(A,ES15.7)', 'Sum:     ', sum(a)
        print '(A,ES15.7)', 'Product: ', product(a(1:10))  ! First 10 only
        print '(A,ES15.7)', 'Maxval:  ', maxval(a)
        print '(A,ES15.7)', 'Minval:  ', minval(a)
        print '(A,I10)',    'Count>50:', count(a > 50.0_dp)

        ! Conditional operations
        print '(A,ES15.7)', 'Sum where >50: ', sum(a, mask=(a > 50.0_dp))
        print '(A,ES15.7)', 'Max where <30: ', maxval(a, mask=(a < 30.0_dp))

        ! ---- Location Functions ----
        print '(A,I5)', 'Location of max: ', maxloc(a, dim=1)
        print '(A,I5)', 'Location of min: ', minloc(a, dim=1)

        ! ---- Element-wise Operations ----
        c = a + b           ! Element-wise addition
        c = a * b           ! Element-wise multiplication
        c = sqrt(a)         ! Element-wise sqrt
        c = exp(a(1:20))    ! exp of first 20 elements
        c = merge(a, b, a > 50.0_dp)  ! Conditional merge

        ! ---- Array Manipulation ----
        ! Pack: gather elements matching mask
        ! (result array must be sized correctly)

        ! Spread: replicate array along dimension
        matrix = spread(a(1:10), dim=2, ncopies=10)

        ! Reshape: change array shape
        matrix = reshape([(real(i,dp), i=1,100)], [10, 10])

        ! Transpose
        matrix = transpose(matrix)

        ! ---- Matrix Operations ----
        ! Dot product
        print '(A,ES15.7)', 'Dot product: ', dot_product(a, b)

        ! Matrix multiply
        matrix = matmul(matrix, matrix)

    end subroutine array_intrinsics_demo

    !---------------------------------------------------------------------------
    ! Dynamic Array Allocation
    !---------------------------------------------------------------------------
    subroutine dynamic_arrays_demo()
        real(dp), allocatable :: dynamic_1d(:)
        real(dp), allocatable :: dynamic_2d(:,:)
        real(dp), allocatable :: dynamic_3d(:,:,:)
        integer :: n, m, p, ios
        character(len=100) :: errmsg

        n = 100; m = 50; p = 25

        ! ---- Allocate with error handling ----
        allocate(dynamic_1d(n), stat=ios, errmsg=errmsg)
        if (ios /= 0) then
            print '(A,A)', 'Allocation error: ', trim(errmsg)
            return
        end if

        allocate(dynamic_2d(n, m), source=0.0_dp)  ! Allocate and initialize
        allocate(dynamic_3d(n, m, p), mold=dynamic_2d)  ! Same type, not values

        ! ---- Reallocate (F2003+) ----
        ! Resize 1D array
        call resize_array(dynamic_1d, 200)

        ! ---- Check allocation status ----
        if (allocated(dynamic_1d)) then
            print '(A,I10)', 'Size of dynamic_1d: ', size(dynamic_1d)
            print '(A,I10,A,I10)', 'Bounds: ', lbound(dynamic_1d,1), ' to ', &
                                             ubound(dynamic_1d,1)
        end if

        ! ---- Move allocation (F2003+) ----
        ! call move_alloc(dynamic_1d, another_array)

        ! ---- Deallocate ----
        if (allocated(dynamic_1d)) deallocate(dynamic_1d)
        if (allocated(dynamic_2d)) deallocate(dynamic_2d)
        if (allocated(dynamic_3d)) deallocate(dynamic_3d)

    end subroutine dynamic_arrays_demo

    !---------------------------------------------------------------------------
    ! Resize allocatable array
    !---------------------------------------------------------------------------
    subroutine resize_array(arr, new_size)
        real(dp), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: new_size
        real(dp), allocatable :: temp(:)
        integer :: copy_size

        if (.not. allocated(arr)) then
            allocate(arr(new_size))
            arr = 0.0_dp
            return
        end if

        allocate(temp(new_size))
        copy_size = min(size(arr), new_size)
        temp(1:copy_size) = arr(1:copy_size)
        if (new_size > size(arr)) then
            temp(copy_size+1:new_size) = 0.0_dp
        end if

        call move_alloc(temp, arr)
    end subroutine resize_array

end module array_operations
```

---

## File I/O Patterns

### Robust File Operations

```fortran
!===============================================================================
! File I/O Patterns
!===============================================================================
module file_io
    use iso_fortran_env, only: real64, int32, iostat_end, iostat_eor
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: MAX_LINE_LEN = 1024

contains

    !---------------------------------------------------------------------------
    ! Read data file into array
    !---------------------------------------------------------------------------
    subroutine read_data_file(filename, data, nrows, ncols, success)
        character(len=*), intent(in) :: filename
        real(dp), allocatable, intent(out) :: data(:,:)
        integer, intent(out) :: nrows, ncols
        logical, intent(out) :: success

        integer :: unit_num, ios, i, j
        character(len=MAX_LINE_LEN) :: line
        real(dp), allocatable :: row_data(:)
        logical :: file_exists

        success = .false.
        nrows = 0
        ncols = 0

        ! Check file exists
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) then
            print '(A,A)', 'ERROR: File not found: ', trim(filename)
            return
        end if

        ! Open file
        open(newunit=unit_num, file=filename, status='old', &
             action='read', iostat=ios)
        if (ios /= 0) then
            print '(A,I0)', 'ERROR: Cannot open file, iostat=', ios
            return
        end if

        ! First pass: count rows and determine columns
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios == iostat_end) exit
            if (ios /= 0) then
                print '(A,I0)', 'ERROR: Read error, iostat=', ios
                close(unit_num)
                return
            end if
            if (len_trim(line) > 0) then
                nrows = nrows + 1
                if (nrows == 1) then
                    ! Determine number of columns from first row
                    ncols = count_columns(line)
                end if
            end if
        end do

        if (nrows == 0 .or. ncols == 0) then
            print '(A)', 'ERROR: Empty or invalid file'
            close(unit_num)
            return
        end if

        ! Allocate data array
        allocate(data(nrows, ncols), stat=ios)
        if (ios /= 0) then
            print '(A)', 'ERROR: Cannot allocate data array'
            close(unit_num)
            return
        end if

        ! Second pass: read data
        rewind(unit_num)
        allocate(row_data(ncols))

        do i = 1, nrows
            read(unit_num, *, iostat=ios) row_data
            if (ios /= 0) then
                print '(A,I0,A,I0)', 'ERROR: Read error at row ', i, &
                      ', iostat=', ios
                deallocate(data)
                close(unit_num)
                return
            end if
            data(i, :) = row_data
        end do

        deallocate(row_data)
        close(unit_num)
        success = .true.

        print '(A,I0,A,I0,A)', 'Successfully read ', nrows, ' rows x ', &
              ncols, ' columns'

    end subroutine read_data_file

    !---------------------------------------------------------------------------
    ! Count columns in a line
    !---------------------------------------------------------------------------
    function count_columns(line) result(ncols)
        character(len=*), intent(in) :: line
        integer :: ncols

        integer :: i, n
        logical :: in_field

        ncols = 0
        in_field = .false.
        n = len_trim(line)

        do i = 1, n
            if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) then
                if (.not. in_field) then
                    ncols = ncols + 1
                    in_field = .true.
                end if
            else
                in_field = .false.
            end if
        end do
    end function count_columns

    !---------------------------------------------------------------------------
    ! Write data to file
    !---------------------------------------------------------------------------
    subroutine write_data_file(filename, data, fmt_str, success)
        character(len=*), intent(in) :: filename
        real(dp), intent(in) :: data(:,:)
        character(len=*), intent(in), optional :: fmt_str
        logical, intent(out) :: success

        integer :: unit_num, ios, i, j
        integer :: nrows, ncols
        character(len=64) :: format_string

        success = .false.
        nrows = size(data, 1)
        ncols = size(data, 2)

        ! Default format or use provided
        if (present(fmt_str)) then
            format_string = fmt_str
        else
            write(format_string, '(A,I0,A)') '(', ncols, '(ES15.7,1X))'
        end if

        ! Open file
        open(newunit=unit_num, file=filename, status='replace', &
             action='write', iostat=ios)
        if (ios /= 0) then
            print '(A,I0)', 'ERROR: Cannot create file, iostat=', ios
            return
        end if

        ! Write data
        do i = 1, nrows
            write(unit_num, format_string, iostat=ios) data(i, :)
            if (ios /= 0) then
                print '(A,I0)', 'ERROR: Write error, iostat=', ios
                close(unit_num)
                return
            end if
        end do

        close(unit_num)
        success = .true.

        print '(A,A)', 'Successfully wrote file: ', trim(filename)

    end subroutine write_data_file

    !---------------------------------------------------------------------------
    ! Binary file I/O
    !---------------------------------------------------------------------------
    subroutine write_binary(filename, data, success)
        character(len=*), intent(in) :: filename
        real(dp), intent(in) :: data(:,:)
        logical, intent(out) :: success

        integer :: unit_num, ios
        integer :: nrows, ncols

        success = .false.
        nrows = size(data, 1)
        ncols = size(data, 2)

        open(newunit=unit_num, file=filename, form='unformatted', &
             access='stream', status='replace', iostat=ios)
        if (ios /= 0) return

        ! Write header (dimensions)
        write(unit_num) nrows, ncols
        ! Write data
        write(unit_num) data

        close(unit_num)
        success = .true.
    end subroutine write_binary

    subroutine read_binary(filename, data, success)
        character(len=*), intent(in) :: filename
        real(dp), allocatable, intent(out) :: data(:,:)
        logical, intent(out) :: success

        integer :: unit_num, ios
        integer :: nrows, ncols

        success = .false.

        open(newunit=unit_num, file=filename, form='unformatted', &
             access='stream', status='old', iostat=ios)
        if (ios /= 0) return

        ! Read header
        read(unit_num, iostat=ios) nrows, ncols
        if (ios /= 0) then
            close(unit_num)
            return
        end if

        ! Allocate and read data
        allocate(data(nrows, ncols), stat=ios)
        if (ios /= 0) then
            close(unit_num)
            return
        end if

        read(unit_num, iostat=ios) data
        close(unit_num)

        success = (ios == 0)
    end subroutine read_binary

end module file_io
```

---

## Matrix Operations

### Linear Algebra Operations

```fortran
!===============================================================================
! Matrix Operations Module
!===============================================================================
module matrix_ops
    use iso_fortran_env, only: real64, int32
    implicit none

    integer, parameter :: dp = real64

contains

    !---------------------------------------------------------------------------
    ! Matrix-Vector Multiplication: y = A * x
    !---------------------------------------------------------------------------
    subroutine matvec_multiply(A, x, y)
        real(dp), intent(in) :: A(:,:)
        real(dp), intent(in) :: x(:)
        real(dp), intent(out) :: y(:)

        integer :: m, n, i, j

        m = size(A, 1)
        n = size(A, 2)

        ! Using intrinsic (recommended)
        y = matmul(A, x)

        ! Or manual implementation for educational purposes:
        ! y = 0.0_dp
        ! do j = 1, n
        !     do i = 1, m
        !         y(i) = y(i) + A(i,j) * x(j)
        !     end do
        ! end do

    end subroutine matvec_multiply

    !---------------------------------------------------------------------------
    ! Matrix-Matrix Multiplication: C = A * B
    !---------------------------------------------------------------------------
    subroutine matmat_multiply(A, B, C)
        real(dp), intent(in) :: A(:,:), B(:,:)
        real(dp), intent(out) :: C(:,:)

        ! Using intrinsic
        C = matmul(A, B)
    end subroutine matmat_multiply

    !---------------------------------------------------------------------------
    ! Gaussian Elimination with Partial Pivoting
    !---------------------------------------------------------------------------
    subroutine gauss_eliminate(A, b, x, success)
        real(dp), intent(inout) :: A(:,:)
        real(dp), intent(inout) :: b(:)
        real(dp), intent(out) :: x(:)
        logical, intent(out) :: success

        integer :: n, i, j, k, maxrow
        real(dp) :: maxval_elem, factor, temp
        real(dp), parameter :: eps = 1.0e-12_dp

        n = size(A, 1)
        success = .true.
        x = 0.0_dp

        ! Forward elimination
        do k = 1, n-1
            ! Find pivot (partial pivoting)
            maxrow = k
            maxval_elem = abs(A(k,k))
            do i = k+1, n
                if (abs(A(i,k)) > maxval_elem) then
                    maxval_elem = abs(A(i,k))
                    maxrow = i
                end if
            end do

            ! Check for singular matrix
            if (maxval_elem < eps) then
                success = .false.
                return
            end if

            ! Swap rows if needed
            if (maxrow /= k) then
                do j = k, n
                    temp = A(k,j)
                    A(k,j) = A(maxrow,j)
                    A(maxrow,j) = temp
                end do
                temp = b(k)
                b(k) = b(maxrow)
                b(maxrow) = temp
            end if

            ! Eliminate column
            do i = k+1, n
                factor = A(i,k) / A(k,k)
                A(i,k) = 0.0_dp
                do j = k+1, n
                    A(i,j) = A(i,j) - factor * A(k,j)
                end do
                b(i) = b(i) - factor * b(k)
            end do
        end do

        ! Check last pivot
        if (abs(A(n,n)) < eps) then
            success = .false.
            return
        end if

        ! Back substitution
        x(n) = b(n) / A(n,n)
        do i = n-1, 1, -1
            x(i) = b(i)
            do j = i+1, n
                x(i) = x(i) - A(i,j) * x(j)
            end do
            x(i) = x(i) / A(i,i)
        end do

    end subroutine gauss_eliminate

    !---------------------------------------------------------------------------
    ! LU Decomposition (Doolittle algorithm)
    !---------------------------------------------------------------------------
    subroutine lu_decompose(A, L, U, success)
        real(dp), intent(in) :: A(:,:)
        real(dp), intent(out) :: L(:,:), U(:,:)
        logical, intent(out) :: success

        integer :: n, i, j, k
        real(dp) :: sum_val
        real(dp), parameter :: eps = 1.0e-12_dp

        n = size(A, 1)
        success = .true.

        L = 0.0_dp
        U = 0.0_dp

        ! Set diagonal of L to 1
        do i = 1, n
            L(i,i) = 1.0_dp
        end do

        do j = 1, n
            ! Upper triangular
            do i = 1, j
                sum_val = 0.0_dp
                do k = 1, i-1
                    sum_val = sum_val + L(i,k) * U(k,j)
                end do
                U(i,j) = A(i,j) - sum_val
            end do

            ! Check for zero pivot
            if (abs(U(j,j)) < eps) then
                success = .false.
                return
            end if

            ! Lower triangular
            do i = j+1, n
                sum_val = 0.0_dp
                do k = 1, j-1
                    sum_val = sum_val + L(i,k) * U(k,j)
                end do
                L(i,j) = (A(i,j) - sum_val) / U(j,j)
            end do
        end do

    end subroutine lu_decompose

    !---------------------------------------------------------------------------
    ! Compute Matrix Determinant
    !---------------------------------------------------------------------------
    function determinant(A) result(det)
        real(dp), intent(in) :: A(:,:)
        real(dp) :: det

        real(dp), allocatable :: U(:,:), L(:,:)
        integer :: n, i
        logical :: success

        n = size(A, 1)
        allocate(L(n,n), U(n,n))

        call lu_decompose(A, L, U, success)

        if (.not. success) then
            det = 0.0_dp
        else
            det = 1.0_dp
            do i = 1, n
                det = det * U(i,i)
            end do
        end if

        deallocate(L, U)
    end function determinant

    !---------------------------------------------------------------------------
    ! Matrix Norms
    !---------------------------------------------------------------------------
    function frobenius_norm(A) result(norm)
        real(dp), intent(in) :: A(:,:)
        real(dp) :: norm

        norm = sqrt(sum(A**2))
    end function frobenius_norm

    function infinity_norm(A) result(norm)
        real(dp), intent(in) :: A(:,:)
        real(dp) :: norm

        integer :: i
        norm = 0.0_dp
        do i = 1, size(A, 1)
            norm = max(norm, sum(abs(A(i,:))))
        end do
    end function infinity_norm

end module matrix_ops
```

---

## OpenMP Parallelization

### OpenMP Patterns

```fortran
!===============================================================================
! OpenMP Parallelization Patterns
!===============================================================================
module parallel_ops
    use iso_fortran_env, only: real64, int32
    use omp_lib
    implicit none

    integer, parameter :: dp = real64

contains

    !---------------------------------------------------------------------------
    ! Parallel DO Loop
    !---------------------------------------------------------------------------
    subroutine parallel_array_init(A, n)
        real(dp), intent(out) :: A(:)
        integer, intent(in) :: n

        integer :: i

        !$omp parallel do private(i) schedule(static)
        do i = 1, n
            A(i) = sin(real(i, dp) * 0.01_dp)
        end do
        !$omp end parallel do

    end subroutine parallel_array_init

    !---------------------------------------------------------------------------
    ! Parallel Reduction
    !---------------------------------------------------------------------------
    function parallel_sum(A) result(total)
        real(dp), intent(in) :: A(:)
        real(dp) :: total

        integer :: i, n

        n = size(A)
        total = 0.0_dp

        !$omp parallel do private(i) reduction(+:total) schedule(static)
        do i = 1, n
            total = total + A(i)
        end do
        !$omp end parallel do

    end function parallel_sum

    !---------------------------------------------------------------------------
    ! Parallel Matrix-Vector Multiplication
    !---------------------------------------------------------------------------
    subroutine parallel_matvec(A, x, y)
        real(dp), intent(in) :: A(:,:)
        real(dp), intent(in) :: x(:)
        real(dp), intent(out) :: y(:)

        integer :: m, n, i, j
        real(dp) :: sum_val

        m = size(A, 1)
        n = size(A, 2)

        !$omp parallel do private(i, j, sum_val) schedule(dynamic, 32)
        do i = 1, m
            sum_val = 0.0_dp
            do j = 1, n
                sum_val = sum_val + A(i,j) * x(j)
            end do
            y(i) = sum_val
        end do
        !$omp end parallel do

    end subroutine parallel_matvec

    !---------------------------------------------------------------------------
    ! Parallel Matrix-Matrix Multiplication
    !---------------------------------------------------------------------------
    subroutine parallel_matmul(A, B, C)
        real(dp), intent(in) :: A(:,:), B(:,:)
        real(dp), intent(out) :: C(:,:)

        integer :: m, n, p, i, j, k
        real(dp) :: sum_val

        m = size(A, 1)
        n = size(A, 2)
        p = size(B, 2)

        !$omp parallel do private(i, j, k, sum_val) collapse(2) schedule(static)
        do j = 1, p
            do i = 1, m
                sum_val = 0.0_dp
                do k = 1, n
                    sum_val = sum_val + A(i,k) * B(k,j)
                end do
                C(i,j) = sum_val
            end do
        end do
        !$omp end parallel do

    end subroutine parallel_matmul

    !---------------------------------------------------------------------------
    ! Parallel Sections
    !---------------------------------------------------------------------------
    subroutine parallel_sections_demo(A, B, sumA, sumB)
        real(dp), intent(in) :: A(:), B(:)
        real(dp), intent(out) :: sumA, sumB

        !$omp parallel sections
        !$omp section
        sumA = sum(A)
        print '(A,I2)', 'Section 1 on thread ', omp_get_thread_num()

        !$omp section
        sumB = sum(B)
        print '(A,I2)', 'Section 2 on thread ', omp_get_thread_num()
        !$omp end parallel sections

    end subroutine parallel_sections_demo

    !---------------------------------------------------------------------------
    ! Thread-Safe Random Number
    !---------------------------------------------------------------------------
    subroutine parallel_random_init(A)
        real(dp), intent(out) :: A(:)

        integer :: i, n, thread_id
        integer, allocatable :: seed(:)
        integer :: seed_size

        n = size(A)

        !$omp parallel private(i, thread_id, seed, seed_size)
        thread_id = omp_get_thread_num()

        ! Each thread gets unique seed
        call random_seed(size=seed_size)
        allocate(seed(seed_size))
        seed = 12345 + thread_id * 1000
        call random_seed(put=seed)

        !$omp do schedule(static)
        do i = 1, n
            call random_number(A(i))
        end do
        !$omp end do

        deallocate(seed)
        !$omp end parallel

    end subroutine parallel_random_init

    !---------------------------------------------------------------------------
    ! Query OpenMP Environment
    !---------------------------------------------------------------------------
    subroutine print_omp_info()
        integer :: nthreads, max_threads, num_procs

        nthreads = omp_get_num_threads()
        max_threads = omp_get_max_threads()
        num_procs = omp_get_num_procs()

        print '(A)'
        print '(A)', '=== OpenMP Information ==='
        print '(A,I4)', 'Number of processors:   ', num_procs
        print '(A,I4)', 'Max threads available:  ', max_threads
        print '(A,I4)', 'Current threads active: ', nthreads
        print '(A)'

    end subroutine print_omp_info

end module parallel_ops
```

---

## Module Patterns

### Complete Module Example

```fortran
!===============================================================================
! Comprehensive Module Example
! Demonstrates derived types, interfaces, operator overloading
!===============================================================================
module vector3d_mod
    use iso_fortran_env, only: real64, int32
    implicit none

    private
    public :: vector3d, operator(+), operator(-), operator(*), &
              dot_product, cross_product, magnitude, normalize

    integer, parameter :: dp = real64

    !---------------------------------------------------------------------------
    ! Vector3D Derived Type
    !---------------------------------------------------------------------------
    type :: vector3d
        real(dp) :: x = 0.0_dp
        real(dp) :: y = 0.0_dp
        real(dp) :: z = 0.0_dp
    contains
        procedure :: print => vector3d_print
        procedure :: set => vector3d_set
        procedure :: length => vector3d_length
    end type vector3d

    !---------------------------------------------------------------------------
    ! Operator Interfaces
    !---------------------------------------------------------------------------
    interface operator(+)
        module procedure vector_add
    end interface

    interface operator(-)
        module procedure vector_subtract
    end interface

    interface operator(*)
        module procedure vector_scalar_mult
        module procedure scalar_vector_mult
    end interface

contains

    !---------------------------------------------------------------------------
    ! Type-bound procedures
    !---------------------------------------------------------------------------
    subroutine vector3d_print(this, label)
        class(vector3d), intent(in) :: this
        character(len=*), intent(in), optional :: label

        if (present(label)) then
            print '(A,A,3(ES12.4,1X),A)', trim(label), ' = (', &
                  this%x, this%y, this%z, ')'
        else
            print '(A,3(ES12.4,1X),A)', '(', this%x, this%y, this%z, ')'
        end if
    end subroutine vector3d_print

    subroutine vector3d_set(this, x, y, z)
        class(vector3d), intent(inout) :: this
        real(dp), intent(in) :: x, y, z

        this%x = x
        this%y = y
        this%z = z
    end subroutine vector3d_set

    function vector3d_length(this) result(len)
        class(vector3d), intent(in) :: this
        real(dp) :: len

        len = sqrt(this%x**2 + this%y**2 + this%z**2)
    end function vector3d_length

    !---------------------------------------------------------------------------
    ! Operator implementations
    !---------------------------------------------------------------------------
    function vector_add(a, b) result(c)
        type(vector3d), intent(in) :: a, b
        type(vector3d) :: c

        c%x = a%x + b%x
        c%y = a%y + b%y
        c%z = a%z + b%z
    end function vector_add

    function vector_subtract(a, b) result(c)
        type(vector3d), intent(in) :: a, b
        type(vector3d) :: c

        c%x = a%x - b%x
        c%y = a%y - b%y
        c%z = a%z - b%z
    end function vector_subtract

    function vector_scalar_mult(v, s) result(c)
        type(vector3d), intent(in) :: v
        real(dp), intent(in) :: s
        type(vector3d) :: c

        c%x = v%x * s
        c%y = v%y * s
        c%z = v%z * s
    end function vector_scalar_mult

    function scalar_vector_mult(s, v) result(c)
        real(dp), intent(in) :: s
        type(vector3d), intent(in) :: v
        type(vector3d) :: c

        c = vector_scalar_mult(v, s)
    end function scalar_vector_mult

    !---------------------------------------------------------------------------
    ! Module procedures
    !---------------------------------------------------------------------------
    function dot_product(a, b) result(d)
        type(vector3d), intent(in) :: a, b
        real(dp) :: d

        d = a%x*b%x + a%y*b%y + a%z*b%z
    end function dot_product

    function cross_product(a, b) result(c)
        type(vector3d), intent(in) :: a, b
        type(vector3d) :: c

        c%x = a%y*b%z - a%z*b%y
        c%y = a%z*b%x - a%x*b%z
        c%z = a%x*b%y - a%y*b%x
    end function cross_product

    function magnitude(v) result(m)
        type(vector3d), intent(in) :: v
        real(dp) :: m

        m = sqrt(v%x**2 + v%y**2 + v%z**2)
    end function magnitude

    function normalize(v) result(n)
        type(vector3d), intent(in) :: v
        type(vector3d) :: n

        real(dp) :: m

        m = magnitude(v)
        if (m > 0.0_dp) then
            n = v * (1.0_dp / m)
        else
            n = v
        end if
    end function normalize

end module vector3d_mod
```

---

## Numerical Methods

### Common Numerical Algorithms

```fortran
!===============================================================================
! Numerical Methods Module
!===============================================================================
module numerical_methods
    use iso_fortran_env, only: real64, int32
    implicit none

    integer, parameter :: dp = real64
    real(dp), parameter :: eps = 1.0e-12_dp

contains

    !---------------------------------------------------------------------------
    ! Newton-Raphson Root Finding
    !---------------------------------------------------------------------------
    function newton_raphson(f, df, x0, tol, maxiter, success) result(root)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
            function df(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: df
            end function df
        end interface
        real(dp), intent(in) :: x0, tol
        integer, intent(in) :: maxiter
        logical, intent(out) :: success
        real(dp) :: root

        integer :: iter
        real(dp) :: x, fx, dfx

        success = .false.
        x = x0

        do iter = 1, maxiter
            fx = f(x)
            dfx = df(x)

            if (abs(dfx) < eps) exit

            x = x - fx / dfx

            if (abs(fx) < tol) then
                success = .true.
                exit
            end if
        end do

        root = x
    end function newton_raphson

    !---------------------------------------------------------------------------
    ! Trapezoidal Integration
    !---------------------------------------------------------------------------
    function integrate_trapz(f, a, b, n) result(integral)
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

        real(dp) :: h, x
        integer :: i

        h = (b - a) / real(n, dp)
        integral = 0.5_dp * (f(a) + f(b))

        do i = 1, n-1
            x = a + real(i, dp) * h
            integral = integral + f(x)
        end do

        integral = integral * h
    end function integrate_trapz

    !---------------------------------------------------------------------------
    ! Simpson's Rule Integration
    !---------------------------------------------------------------------------
    function integrate_simpson(f, a, b, n) result(integral)
        interface
            function f(x)
                import :: dp
                real(dp), intent(in) :: x
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: a, b
        integer, intent(in) :: n  ! Must be even
        real(dp) :: integral

        real(dp) :: h, x
        integer :: i, n_even

        ! Ensure n is even
        n_even = n
        if (mod(n, 2) /= 0) n_even = n + 1

        h = (b - a) / real(n_even, dp)
        integral = f(a) + f(b)

        do i = 1, n_even-1, 2  ! Odd indices
            x = a + real(i, dp) * h
            integral = integral + 4.0_dp * f(x)
        end do

        do i = 2, n_even-2, 2  ! Even indices
            x = a + real(i, dp) * h
            integral = integral + 2.0_dp * f(x)
        end do

        integral = integral * h / 3.0_dp
    end function integrate_simpson

    !---------------------------------------------------------------------------
    ! 4th Order Runge-Kutta ODE Solver
    !---------------------------------------------------------------------------
    subroutine rk4_solve(f, t0, tf, y0, n, t, y)
        interface
            function f(t, y)
                import :: dp
                real(dp), intent(in) :: t, y
                real(dp) :: f
            end function f
        end interface
        real(dp), intent(in) :: t0, tf, y0
        integer, intent(in) :: n
        real(dp), intent(out) :: t(:), y(:)

        real(dp) :: h, k1, k2, k3, k4
        integer :: i

        h = (tf - t0) / real(n, dp)
        t(1) = t0
        y(1) = y0

        do i = 1, n
            k1 = h * f(t(i), y(i))
            k2 = h * f(t(i) + 0.5_dp*h, y(i) + 0.5_dp*k1)
            k3 = h * f(t(i) + 0.5_dp*h, y(i) + 0.5_dp*k2)
            k4 = h * f(t(i) + h, y(i) + k3)

            t(i+1) = t(i) + h
            y(i+1) = y(i) + (k1 + 2.0_dp*k2 + 2.0_dp*k3 + k4) / 6.0_dp
        end do
    end subroutine rk4_solve

end module numerical_methods
```

---

## Interoperability

### C Interoperability

```fortran
!===============================================================================
! C Interoperability Module
!===============================================================================
module c_interface
    use iso_c_binding
    implicit none

contains

    !---------------------------------------------------------------------------
    ! Fortran subroutine callable from C
    !---------------------------------------------------------------------------
    subroutine fortran_routine(n, x, result) bind(C, name="fortran_routine")
        integer(c_int), value, intent(in) :: n
        real(c_double), intent(in) :: x(n)
        real(c_double), intent(out) :: result

        result = sum(x)
    end subroutine fortran_routine

    !---------------------------------------------------------------------------
    ! Call C function from Fortran
    !---------------------------------------------------------------------------
    subroutine call_c_function()
        interface
            function c_sqrt(x) bind(C, name="sqrt")
                import :: c_double
                real(c_double), value :: x
                real(c_double) :: c_sqrt
            end function c_sqrt
        end interface

        real(c_double) :: val, result

        val = 2.0_c_double
        result = c_sqrt(val)

        print '(A,F10.6)', 'sqrt(2) = ', result
    end subroutine call_c_function

    !---------------------------------------------------------------------------
    ! Pass array to C
    !---------------------------------------------------------------------------
    subroutine pass_array_to_c()
        interface
            subroutine c_process_array(arr, n) bind(C, name="process_array")
                import :: c_double, c_int, c_ptr
                type(c_ptr), value :: arr
                integer(c_int), value :: n
            end subroutine c_process_array
        end interface

        real(c_double), target :: data(100)
        integer(c_int) :: n

        data = 1.0_c_double
        n = 100

        call c_process_array(c_loc(data), n)
    end subroutine pass_array_to_c

end module c_interface
```

---

*ARCHAEON Arsenal - Fortran Division*
*"FORTRAN, the lingua franca of high-performance computing"*

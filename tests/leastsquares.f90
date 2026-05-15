program LeastSquaresAnalysis
    implicit none
    integer, parameter :: dp = 8
    integer :: i, iostat
    real(dp), dimension(500) :: x_all, y_all
    real(dp), allocatable :: x(:), y(:)
    real(dp) :: a1, b1
    real(dp) :: a2, b2, c2
	
    ! Doc du lieu tu file. File NewData can dat o cung thu muc voi file code Fortran
    open(unit=10, file='NewData.dat', status='old', action='read')
    do i = 1, 500
        read(10, *, iostat=iostat) x_all(i), y_all(i)
        if (iostat /= 0) exit
    end do
    close(10)

    ! Khop ham bac nhat
    allocate(x(100), y(100))
    x = x_all(101:200)
    y = y_all(101:200)
    call fit_linear(x, y, 100, a1, b1)
    write(*, "(A)") "--- Ham bac nhat ---"
    write(*, "(A, F10.4, A, F10.4)") "y = ", a1, " * x + ", b1
    deallocate(x, y)

    ! Khop ham bac hai
    allocate(x(200), y(200))
    x = x_all(101:300)
    y = y_all(101:300)
    call fit_quadratic(x, y, 200, a2, b2, c2)
    write(*, "(A)") "--- Ham bac hai ---"
    write(*, "(A, F10.4, A, F10.4, A, F10.4)") "y = ", a2, " * x^2 + ", b2, " * x + ", c2
    deallocate(x, y)

contains

    ! Thuat toan khop ham bac nhat
    subroutine fit_linear(x, y, n, a, b)
        integer, intent(in) :: n
        real(dp), intent(in) :: x(n), y(n)
        real(dp), intent(out) :: a, b
        real(dp) :: sum_x, sum_y, sum_xx, sum_xy, det
        
        sum_x = sum(x)
        sum_y = sum(y)
        sum_xx = sum(x*x)
        sum_xy = sum(x*y)
        
        det = n * sum_xx - sum_x**2
        a = (n * sum_xy - sum_x * sum_y) / det
        b = (sum_xx * sum_y - sum_x * sum_xy) / det
    end subroutine fit_linear

    ! Thuat toan khop ham bac hai
    subroutine fit_quadratic(x, y, n, a, b, c)
        integer, intent(in) :: n
        real(dp), intent(in) :: x(n), y(n)
        real(dp), intent(out) :: a, b, c
        real(dp) :: s0, s1, s2, s3, s4, sy, sxy, sx2y, det
        
        s0 = real(n, dp)
        s1 = sum(x)
        s2 = sum(x**2)
        s3 = sum(x**3)
        s4 = sum(x**4)
        sy = sum(y)
        sxy = sum(x*y)
        sx2y = sum(x**2 * y)
         
        det = s4*(s2*s0 - s1*s1) - s3*(s3*s0 - s1*s2) + s2*(s3*s1 - s2*s2)
        
        a = (sx2y*(s2*s0 - s1*s1) - s3*(sxy*s0 - s1*sy) + s2*(sxy*s1 - s2*sy)) / det
        b = (s4*(sxy*s0 - s1*sy) - sx2y*(s3*s0 - s1*s2) + s2*(s3*sy - sxy*s2)) / det
        c = (s4*(s2*sy - sxy*s1) - s3*(s3*sy - sx2y*s1) + sx2y*(s3*s1 - s2*s2)) / det
    end subroutine fit_quadratic

end program LeastSquaresAnalysis

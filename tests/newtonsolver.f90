! Giai phuong trinh phi tuyen bang pp Newton

program NewtonSolver
    implicit none
    integer :: j
    double precision :: x, y
    
    x = 4
    
    do j = 1, 10
        y = x - f(x) / diff(x)
        
        write(*, "(A, I2, A, F12.8, A, E12.4)") &
            "Step = ", j, " | x = ", y, " | delta = ", abs(y-x)
        
        if (abs(y-x) < 1.0e-10) exit
        x = y
    end do

contains

    function f(x)
        double precision :: f
        double precision, intent(in) :: x
        f = x**3 - 3*x**2 - 5.0
    end function f

    function diff(x)
        double precision :: diff
        double precision, intent(in) :: x
        double precision :: dx
        
        dx = 1.0e-7
        diff = (f(x + dx) - f(x - dx)) / (2.0 * dx)
    end function diff

end program NewtonSolver

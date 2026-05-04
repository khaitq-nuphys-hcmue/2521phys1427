program integral
	implicit none
	real :: f
	real :: a, b, h
	real :: Si, S, x, y
    integer :: n, i
	allocatable :: x(:)
	
! Nhap a, b	
    write(*,*)"Input a and b"
    read(*,*)a, b
    write(*,*)"Input n"
    read(*,*)n
	
! Phuong phap hinh thang. So khoang chia = n
    write(*,*)"1 - Trapezoidal method"
    allocate(x(0:n))
    h = (b-a)/n
    do i = 0,n
		x(i) = a + i*h
    enddo
        
    S = 0.0
    do i = 1, n
		Si = (f(x(i)) + f(x(i-1)))*h/2.0
		S = S + Si
    enddo
    deallocate(x)
    write(*,*)"Integral = ", S
	
! Phuong phap Simpson. So khoang chia = 2n
    write(*,*)"2 - Simpson's method"
    allocate(x(0:2*n))
    h = (b - a)/(2.0*n)
    do i = 0, 2*n
		x(i) = a + i*h
    enddo
	
    S = 0.0
    do i = 1,n
		Si = (f(x(2*i-2)) + 4.0*f(x(2*i-1)) + f(x(2*i)))*h/3.0
		S = S + Si
    enddo
    deallocate(x)
    write(*,*)"Integral = ",S 
	
end program integral

! Khai bao ham so f(x)
function f(x)
	implicit none
	real :: f, x
	f = 1/(x+1)
end function f

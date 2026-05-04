! Giai pt phi tuyen bang pp lap
program fixedPoint
	implicit none
	integer :: j
	real :: g, x, y
	
	x = 4
	do j = 1, 10
		y = g(x)
		write(*,*) "step =", j
		write(*,*) "x =", y, "	delta =", abs(y-x)
		x = y
	end do
end program fixedPoint

! Khai bao ham so g(x). Dua pt ve dang x = g(x) truoc khi chay code
function g(x)
	implicit none
	real :: g, x
	
	g = 5.0/x**2 + 3.0 ! => pt: x^3 - 3x^2 - 5 = 0
end function g

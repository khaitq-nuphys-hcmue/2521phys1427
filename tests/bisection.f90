! Giai pt phi tuyen bang pp chia doi

program bisection
	implicit none
	real :: a, b, c
	real :: eps, delta
	real :: f
	
	a = 1.0
	b = 2.0
	eps = 1e-4
	delta = 1e-7
	
	do
		c = (b+a)/2.0
		write(*,*) "c =", c
		write(*,*) "	f(c) =", f(c)
		if (abs(f(c)) < delta) exit
		if (f(a)*f(c) < 0.0) then
			b = c
		else
			a = c
		end if
		write(*,*) "	f(a) =", f(a)
		write(*,*) "   new range =", a, " ->", b
		if (b-a < eps) exit
	end do
	write(*,*) "root =", c
end program bisection

! Khai bao ham so f(x)

function f(x)
	implicit none
	real :: f, x
	f = x**3.0 - x - 2.0
end function f

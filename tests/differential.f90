program diff1
	implicit none
	double precision :: f, x0, dx
	real :: diff
	
	write (*,*) "x0 = "
	read (*,*) x0
	
	dx = 1e-8
	diff = (f(x0+dx) -f(x0)) / dx
	
	write (*,*) "diff = ", diff

end program diff1

function f(x)
	implicit none
	double precision :: f, x
	f = (x+1)/(x-1)
end function f

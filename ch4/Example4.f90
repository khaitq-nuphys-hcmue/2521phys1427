program forth
	integer :: a,b
	real :: answer
	
	write (*,*) "Nhap vao hai so a va b:"
    read (*,*) a,b
	write (*,*) "Ban vua nhap vao hai so", a, "va", b
	answer = a+b
	write (*,*) "Tong cua chung la", answer
end program forth 
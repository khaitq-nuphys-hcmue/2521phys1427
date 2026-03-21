program iosys2
    implicit none
    
    integer :: i, lineCount, io
    real :: dumb
    real, allocatable :: A(:)
    
    open(unit=105, file="data2.txt", status="old")
    
    lineCount = 0
    do
        read(105, *, iostat=io)
        if (io.ne.0) exit
        lineCount = lineCount + 1
    end do
    close(105)
    
    write(*,*)"Number of lines = ", lineCount
    !allocate(A(lineCount))
    
    !open(unit=106, file="data2.txt", status="old")
    rewind(105)
    do i = 1, lineCount
        read(106,*) dumb, A(i)
    enddo
    write(*,*) A(lineCount)
    
    !close(106)
    !deallocate(A)

end program iosys2

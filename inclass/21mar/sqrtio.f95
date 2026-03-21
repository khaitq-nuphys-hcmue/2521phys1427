program iosys1
    implicit none
    
    integer :: i
    
    open(unit=102, file="data2.txt")
    do i = 1, 100
        write(102, *) i, sqrt(i*1.0)
    end do

end program iosys1

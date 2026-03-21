program dynarr

    implicit none
    integer, allocatable :: A(:,:), B(:,:), C(:,:)
    integer :: colA, colB, colC, rowA, rowB, rowC
    integer :: i, j, k
    
    write(*,*) "Number of row of A?"
    read(*,*) rowA
    write (*,*) "Number of column of A?"
    read(*,*) colA
    write(*,*) "Number of row of B?"
    read(*,*) rowB
    write(*,*) "Number of column of B?"
    read(*,*) colB
    
    allocate(A(rowA, colA), B(rowB, colB))
    
    do i = 1, rowA
        do j = 1, colA
            write(*,*)"Input value for A(", i, ",", j,")"
            read(*,*) A(i,j)
        end do
    end do
    do i = 1, rowB
        do j = 1, colB
            write(*,*)"Input value for B(", i, ",", j,")"
            read(*,*) B(i,j)
        end do
    end do
    
    write(*,*)"You have input A matrix as"
    do i = 1, rowA
        write(*,*) (A(i, j), j = 1, colA)
    end do
    
    write(*,*)"You have input B matrix as"
    do i = 1, rowB
        write(*,*) (B(i, j), j = 1, colB)
    end do
    
    
    if (colA.ne.rowB) then
        write(*,*)"Can not multiply A to B"
    else
        rowC = rowA
        colC = colB
        
        allocate(C(rowC, colC))
    endif
    
    do i = 1, rowC
        do j = 1, colC
            C(i, j) = 0
            do k = 1, colA
                C(i, j) = C(i, j) + (A(i, k) * B(k, j))
            end do 
        end do
    end do
    
    write(*,*)"Result:"
    do i = 1, rowC
        write(*,*) (C(i, j), j = 1, colC)
    end do
    
end program dynarr

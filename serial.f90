program serial

  implicit none
  integer, dimension(:,:), allocatable :: A
  integer :: n = 4  ! A = n x n matrix 
  integer i, j 
  integer dateTime(8);


  call date_and_time(values = dateTime)
  allocate(A(n,n))
  call srand(dateTime(8))
  do j = 1 , n
    do i = 1, n
      if( rand() * 6 <= 3) then 
        A(i,j) = 1
      else 
        A(i,j) = 0
      end if  
    end do 
  end do
  
  do i = 1, n 
    print *, A(i,:)
  end do 
  deallocate(A) 

end program serial

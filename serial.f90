program serial

  implicit none
  integer, dimension(:,:), allocatable :: A
  integer, dimension(:,:), allocatable :: newA
  integer :: n = 4  ! A = n x n matrix 
  integer i, j , x , y, counter, xpos, ypos
  integer dateTime(8);


  call date_and_time(values = dateTime)
  allocate(A(0:n-1,0:n-1))
  allocate(newA(0:n-1, 0:n-1))
  call srand(dateTime(8))
  do j = 0 , n-1
    do i = 0, n-1
      if( rand() * 6 <= 3) then 
        A(i,j) = 1
      else 
        A(i,j) = 0
      end if  
    end do 
  end do
  
  print *, 'A:'
  do i = 0, n-1 
    print *, A(i,:)
  end do
 
  do x = 0 , n-1
    do y = 0 , n-1
      counter = 0 
      do i = -1, 1
        do j = -1, 1
          xpos = mod(x + i + n, n) 
          ypos = mod(y + i + n, n)
          if(.not. (j==0 .and. i==0) .and. A(xpos,ypos) == 1) then 
            counter = counter + 1
          end if 
        end do 
      end do
      if(counter == 3) then 
        newA(x,y) = 1
      else if(counter == 2) then 
        newA(x,y) = A(x,y)
      else 
        newA(x,y) = 0
      end if 

    end do 
  end do 

  
  print *, 'newA:'
  do i = 0, n-1 
    print *, newA(i,:)
  end do
 
  deallocate(A) 
  deallocate(newA)

end program serial

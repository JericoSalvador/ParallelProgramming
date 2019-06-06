program parallel
  implicit none
  include 'mpif.h'

  integer, dimension(:,:), allocatable :: A
  integer, dimension(:,:), allocatable :: partialA 
  integer, dimension(:,:), allocatable :: temp
  integer :: n = 4  ! A = n x n matrix 
  integer i, j , x , y, counter, xpos, ypos, localn
  integer dateTime(8);
  
  integer ierr, myid, numprocs, rightId , leftId, tag
  integer stat(MPI_STATUS_SIZE)
  integer, allocatable :: sendbuf(:), recvbuf(:)
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,numprocs,ierr)
  
  call srand(floor(MPI_WTIME()) + myid)
  print *, 'processor id:', myid
  if(mod(n,numprocs) /= 0) then
    if( myid == 0) then  
      print *, 'numprocs does not divide lenght of matrix'
    end if 
    do
      numprocs = numprocs - 1
      if(mod(n, numprocs) == 0) exit
    end do
    if (myid == 0) then  
      print *, 'using', numprocs , 'processors' 
    end if 
  end if
  print *, 'initiating processes'
  localn = n/numprocs 
  
  if(myid == 0) then
    allocate(A(n,n))
  end if 
  
  print *, 'allocating', myid

  allocate(partialA(n,localn)) 
  do j = 1 , localn
    do i = 1,n
      if( rand() * 6 < 2) then 
        partialA(i,j) = 1
      else 
        partialA(i,j) = 0
      end if  
    end do 
  end do
  print *, 'done local:', myid 
 
!  print *, myid, ':', partialA
!  do i = 1, localn 
!    print *, myid, ':', partialA(:,i)
!  end do 
  call MPI_GATHER(partialA, n*localn, MPI_INTEGER, A, n*localn, MPI_INTEGER, 0, &
                 & MPI_COMM_WORLD, ierr)  
  if(myid == 0) then  
    print *, 'A:'
    do i = 1, n 
      print *, A(i,:)
    end do
  end if
  
  rightId = mod(myid+1, numprocs) 
  leftId = mod(numprocs + myid-1, numprocs)  
  allocate(temp(0:n+1, 0:localn+1))

! this is to take care of the top and bottom ghost cells 
  temp(1:n, 1:localn) = partialA
  temp(0,1:localn) = partialA(n,:) 
  temp(n+1,1:localn) = partialA(1,:)  
  print *, 'done with top, bottom'

  allocate(sendbuf(n), recvbuf(n)) 
! This is to take care of the right and left ghost cells 
  call MPI_SENDRECV(temp(:,1), n+2, MPI_INTEGER, leftId, tag, &
&      temp(:,localn+1), n+2, MPI_INTEGER, rightId, tag, MPI_COMM_WORLD, stat, ierr)  
  print *, 'done with left'
  
  call MPI_SENDRECV(temp(:,localn), n+2, MPI_INTEGER, rightId, tag, &
&      temp(:,0), n+2, MPI_INTEGER, leftId, tag, MPI_COMM_WORLD, stat, ierr)  

  print *, myid, 'finished send recv'

! This is to take care of the corner pieces
  if(myid == 0) then 
    call MPI_SENDRECV(temp(1,1), 1, MPI_INTEGER, numprocs-1, tag, &
&      temp(n+1,0), 1, MPI_INTEGER, numprocs-1, tag, MPI_COMM_WORLD, stat,ierr)

    call MPI_SENDRECV(temp(n,1), 1, MPI_INTEGER, numprocs-1, tag, &
&      temp(0,0), 1, MPI_INTEGER, numprocs-1, tag, MPI_COMM_WORLD, stat,ierr)
  end if 

  if(myid == numprocs-1) then 
    call MPI_SENDRECV(temp(1,localn), 1, MPI_INTEGER, 0, tag, &
&      temp(n+1,localn+1), 1, MPI_INTEGER, 0, tag, MPI_COMM_WORLD, stat,ierr)

    call MPI_SENDRECV(temp(n,localn), 1, MPI_INTEGER, 0, tag, &
&      temp(0,localn+1), 1, MPI_INTEGER, 0, tag, MPI_COMM_WORLD, stat,ierr)
  end if 
!  if(myid == 1) then 
!    do i = 0, n+1 
!      print *, temp(i,:) 
!    end do 
!  end if  
! Calculate the new partial matrix 
  do y = 1, localn
    do x = 1 , n
      counter = 0 
      do j = -1, 1
        do i = -1, 1
          if(.not. (j==0 .and. i==0) .and. temp(x+i,y+j) == 1) then 
            counter = counter + 1
          end if 
        end do 
      end do
      if(counter == 3) then 
        partialA(x,y) = 1
      else if(counter == 2) then 
        partialA(x,y) = partialA(x,y)
      else 
        partialA(x,y) = 0
      end if 

    end do 
  end do 

  print *, myid, 'finished new matrix'
if(myid == 0) then   
  print *, myid, ':', partialA
  do i = 1, localn 
    print *, myid, ':', partialA(:,i)
  end do 
end if 
  call MPI_GATHER(partialA, n*localn, MPI_INTEGER, A, n*localn, MPI_INTEGER, 0, &
                 & MPI_COMM_WORLD, ierr)  
  
  if(myid == 0) then  
    print *, 'newA:'
    do i = 1, n 
      print *, A(i,:)
    end do
  end if
  deallocate(temp)

  if(myid == 0) then 
    deallocate(A) 
  end if 
  deallocate(partialA)
  call MPI_BARRIER(MPI_COMM_WORLD,ierr) 
  call MPI_FINALIZE(ierr)

!stop
end program parallel

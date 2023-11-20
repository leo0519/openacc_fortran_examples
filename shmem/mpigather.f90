program gather
   use mpi
   implicit none
   integer, parameter :: max_rank = 8
   integer, parameter :: buf_size = 100000
   integer, parameter :: steps = 32
   integer :: ierr, rank, size, i, j, k
   real(8) :: buf(buf_size), d(buf_size, max_rank), ans

   ! Initialize MPI
   call MPI_Init(ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

   do i = 1, steps
      ! Write data
      do j = 1, buf_size
         buf(j) = rank * buf_size + i + j
      end do
      
      ! Do gather
      call MPI_Gather(buf, buf_size, MPI_REAL8, d, buf_size, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)

      ! Validate data d(:,:) in rank 0
      if (rank .eq. 0) then
         do j = 1, size
            do k = 1, buf_size
               ans = (j - 1) * buf_size + i + k
               if (d(k, j) .ne. ans) then
                  print *, "Failed"
                  call exit(1)
               end if
            end do
         end do
      end if

      call MPI_Barrier(MPI_COMM_WORLD, ierr)
   end do

   if (rank .eq. 0) then
      print *, "Passed"
   end if

   call MPI_Finalize(ierr)
end program gather

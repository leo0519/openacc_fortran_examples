program gather
   use mpi
   use nvtx
   use cudafor
   implicit none
   integer, parameter :: bufcount = 100000 ! The size of data updated by one process (total is bufcount * number of process)
   integer, parameter :: warmup = 512      ! The number of warmup steps
   integer, parameter :: steps = 1024      ! The summation of warmup steps and benchmark steps
   integer, parameter :: root = 0          ! The rank of root process
   integer :: ierror, rank, size, i, j
   real(8) :: sendbuf(bufcount)
   real(8), allocatable :: recvbuf(:, :)
   integer :: start_count, end_count, count_rate
   logical :: correct

   ! Initialize MPI and buffer
   call MPI_Init(ierror)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
   call MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
   allocate(recvbuf(bufcount, size))

   ! Initialize data
   do i = 1, bufcount
      sendbuf(i) = rank * bufcount + i
   end do

   ! Benchmark gather
   do i = 1, steps
      call MPI_Gather(sendbuf, bufcount, MPI_REAL8, recvbuf, bufcount, MPI_REAL8, root, MPI_COMM_WORLD, ierror)
      if (i .eq. warmup) then
         call system_clock(start_count, count_rate)
      end if
   end do
   call system_clock(end_count)

   if (rank .eq. root) then
      print *, "Elapsed time: ", real(end_count - start_count) / real(count_rate), " s"

      ! Validate data recvbuf(:,:) in rank 0
      correct = .TRUE.
      outer: do i = 1, size
         do j = 1, bufcount
            if (recvbuf(j, i) .ne. (i - 1) * bufcount + j) then
               correct = .FALSE.
               exit outer
            end if
         end do
      end do outer
      if (correct) then
         print *, "Data validation passed"
      else
         print *, "Data validation failed"
      end if
   end if

   call MPI_Finalize(ierror)
end program gather

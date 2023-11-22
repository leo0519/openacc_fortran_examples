program shmem
   use mpi
   use nvtx
   use cudafor
   use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer
   implicit none
   integer, parameter :: bufcount = 100000 ! The size of data updated by one process (total is bufcount * number of process)
   integer, parameter :: warmup = 512      ! The number of warmup steps
   integer, parameter :: steps = 1024      ! The summation of warmup steps and benchmark steps
   integer, parameter :: root = 0          ! The rank of root process
   integer, parameter :: split_key = 0     ! The key to assign rank. (set zero so the assigned rank is equal to default rank)
   integer :: ierror, rank, size, disp, i, j, k
   integer :: win, shmcomm, shmrank, shmsize
   integer(kind=MPI_ADDRESS_KIND) :: winsize
   type(c_ptr) :: baseptr
   real(8), pointer :: shmbuffer(:, :)
   real(8) :: sendbuf(bufcount)
   integer :: arrayshape(2)
   integer :: start_count, end_count, count_rate
   logical :: correct


   ! Initialize
   call MPI_Init(ierror)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
   call MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
   call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, split_key, MPI_INFO_NULL, shmcomm, ierror)
   call MPI_Comm_rank(shmcomm, shmrank, ierror)
   call MPI_Comm_size(shmcomm, shmsize, ierror)

   ! Initialize data
   do i = 1, bufcount
      sendbuf(i) = rank * bufcount + i
   end do

   ! Allocate shared memory and query the pointer array
   if (shmrank .eq. root) then
      winsize = int(shmsize * bufcount, MPI_ADDRESS_KIND) * 8_MPI_ADDRESS_KIND
   else
      winsize = 0_MPI_ADDRESS_KIND
   end if
   disp = 8
   call MPI_Win_allocate_shared(winsize, disp, MPI_INFO_NULL, shmcomm, baseptr, win, ierror)
   if (shmrank .ne. root) then
      call MPI_Win_shared_query(win, root, winsize, disp, baseptr, ierror)
   end if
   arrayshape = (/bufcount, shmsize/)
   call c_f_pointer(baseptr, shmbuffer, arrayshape)

   ! Benchmark gather by shared memory
   do i = 1, steps
      shmbuffer(:, shmrank + 1) = sendbuf(:)
      call MPI_Win_fence(0, win, ierror)
      if (i .eq. warmup) then
         call system_clock(start_count, count_rate)
      end if
   end do
   call system_clock(end_count)

   ! Validate shmbuffer(:,:) in rank 0
   if (shmrank .eq. root) then
      print *, "Elapsed time: ", real(end_count - start_count) / real(count_rate), " s"
      correct = .TRUE.
      outer: do j = 1, size
         do k = 1, bufcount
            if (shmbuffer(k, j) .ne. (j - 1) * bufcount + k) then
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

   call MPI_Win_free(win, ierror)
   call MPI_Finalize(ierror)
end program shmem

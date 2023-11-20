program shmem
   use mpi
   use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer
   implicit none
   integer, parameter :: max_rank = 8
   integer, parameter :: buf_size = 100000
   integer, parameter :: steps = 32
   integer :: win, hostcomm, hostrank
   integer(kind=MPI_ADDRESS_KIND) :: win_size
   integer :: disp_unit, rank, ierr, size, i, j, k
   type(c_ptr) :: baseptr
   real(8), pointer :: d(:, :)
   real(8) :: ans
   integer, allocatable :: arrayshape(:)

   ! Initialize MPI
   call MPI_Init(ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
   call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, hostcomm, ierr)
   call MPI_Comm_rank(hostcomm, hostrank, ierr)

   ! Define the mapping array dimension
   allocate(arrayshape(2))
   arrayshape = (/buf_size, max_rank/)

   ! Allocate shared memory and query the pointer array
   if (hostrank == 0) then
      win_size = int(max_rank * buf_size, MPI_ADDRESS_KIND) * 8_MPI_ADDRESS_KIND
   else
      win_size = 0_MPI_ADDRESS_KIND
   end if
   disp_unit = 1
   call MPI_Win_allocate_shared(win_size, disp_unit, MPI_INFO_NULL, hostcomm, baseptr, win, ierr)
   if (hostrank .ne. 0) then
      call MPI_Win_shared_query(win, 0, win_size, disp_unit, baseptr, ierr)
   end if
   call c_f_pointer(baseptr, d, arrayshape)

   do i = 1, steps
      ! Write data
      do j = 1, buf_size
         d(j, hostrank + 1) = hostrank * buf_size + i + j
      end do

      ! Do synchronization
      call MPI_Win_fence(0, win, ierr)

      ! Validate shared d(:,:) in rank 0
      if (hostrank .eq. 0) then
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

      ! This barrier is to avoid rank > 0 modifying shmem before validation in rank 0
      call MPI_Win_fence(0, win, ierr)
   end do

   if (rank .eq. 0) then
      print *, "Passed"
   end if

   call MPI_Win_free(win, ierr)
   call MPI_Finalize(IERR)
end program shmem

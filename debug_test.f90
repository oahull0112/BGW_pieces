PROGRAM debug_test

  USE HDF5
  USE mpi

  IMPLICIT NONE

  integer, allocatable :: my_matrix(:,:)

  ! MPI stuff:
  integer :: mpierror, endmpierr       ! MPI error flag
  integer :: comm, info
  integer :: inode, npes, an_id
  integer :: file_write, ierr!, status(MPI_STATUS_SIZE), sender
  integer, parameter :: return_data_tag = 2001

  comm = MPI_COMM_WORLD
  info = MPI_INFO_NULL

  call MPI_INIT(endmpierr)
  call MPI_COMM_SIZE(comm, npes, mpierror)
  call MPI_COMM_RANK(comm, inode, mpierror)

  allocate(my_matrix(2,2))
  my_matrix = 0

  my_matrix(3, 3) = 1 

  call MPI_FINALIZE(endmpierr)


END PROGRAM debug_test



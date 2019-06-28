!
! Number of processes is assumed to be 2
!
PROGRAM selectbands_inclusion_parallel

  USE HDF5 ! This module contains all necessary modules 
     
  IMPLICIT NONE

  include 'mpif.h'
  CHARACTER(LEN=10), PARAMETER :: filename = "sds_row.h5"  ! File name
  CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray" ! Dataset name

  INTEGER(HID_T) :: file_id       ! File identifier 
  INTEGER(HID_T) :: dset_id       ! Dataset identifier 
  INTEGER(HID_T) :: filespace     ! Dataspace identifier in file 
  INTEGER(HID_T) :: memspace      ! Dataspace identifier in memory
  INTEGER(HID_T) :: dataspace     ! Dataspace identifier in file (likely)
  INTEGER(HID_T) :: plist_id      ! Property list identifier 

  INTEGER :: nbands = 14
  INTEGER ::  ncols = 4
  INTEGER(HSIZE_T), DIMENSION(2) :: dimsf ! Dataset dimensions
                                                    ! in the file.
  INTEGER(HSIZE_T), DIMENSION(2) :: dimsfi != (/6,8/)
  INTEGER(HSIZE_T), DIMENSION(2) :: dimsm != (/3,8/) ! Dataset dimensions
 ! INTEGER(HSIZE_T), ALLOCATABLE :: dimsm(:, :)      ! in memory.

  INTEGER(HSIZE_T), DIMENSION(2) :: count, count_out
  INTEGER(HSSIZE_T), DIMENSION(2) :: offset, offset_out
  INTEGER(HSIZE_T), DIMENSION(2) :: stride
  INTEGER(HSIZE_T), DIMENSION(2) :: block
  INTEGER, ALLOCATABLE :: data (:,:)  ! Data to write
  INTEGER, ALLOCATABLE :: data_out(:,:) ! Data read back in
  INTEGER :: rank = 2 ! Dataset rank 

  INTEGER :: error, error_n  ! Error flags

  ! MPI definitions and calls.
  INTEGER :: mpierror       ! MPI error flag
  INTEGER :: comm, info
  INTEGER :: mpi_size, mpi_rank
  INTEGER :: file_write, ierr, status(MPI_STATUS_SIZE)
  integer, parameter :: send_data_tag = 2001
  integer, parameter :: return_data_tag = 2002
  integer :: bands_per_proc, my_bands_per_proc, remainder_procs

  INTEGER :: i, j
  INTEGER :: fact = 1 ! just for the trivial data

  comm = MPI_COMM_WORLD
  info = MPI_INFO_NULL

  CALL MPI_INIT(mpierror)
  CALL MPI_COMM_SIZE(comm, mpi_size, mpierror)
  CALL MPI_COMM_RANK(comm, mpi_rank, mpierror) 
  ! Quit if mpi_size is not 2

  ! Initialize HDF5 library and Fortran interfaces.
  CALL h5open_f(error) 

  dimsf(1) = nbands
  dimsf(2) = ncols
  dimsfi(1) = nbands
  dimsfi(2) = ncols

  remainder_procs = MOD(nbands, mpi_size)
  my_bands_per_proc = bands_per_proc
  write(*,*) "remainder procs: ", remainder_procs

  bands_per_proc = nbands/mpi_size ! note that this is only going to work if
  if (mpi_rank .eq. mpi_size - 1) then
    my_bands_per_proc = bands_per_proc + remainder_procs
    write(*,*) "PROC: ", mpi_rank, "BPP: ", my_bands_per_proc
  end if
    ! nbands is actually divisible by mpi_size
    ! After it scales with number of procs without issue
    ! will need to go back and make it more general
    ! Basically make it so the last processor
    ! Gets the "extra" bands

  dimsm(2) = ncols
  !dimsm(1) = bands_per_proc
  dimsm(1) = my_bands_per_proc

  call make_file()
  call read_file()


  ! Close FORTRAN interfaces and HDF5 library.
  CALL h5close_f(error)

  CALL MPI_FINALIZE(mpierror)

  contains

    subroutine make_file()
          
      CALL h5open_f(error) 
    
      ! Setup file access property list with parallel I/O access.
      CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
      CALL h5pset_fapl_mpio_f(plist_id, comm, info, error)
    
      ! Create the file collectively.
      CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, &
                              error, access_prp = plist_id)
      CALL h5pclose_f(plist_id, error)
    
      ! Create the data space for the  dataset. 
      CALL h5screate_simple_f(rank, dimsf, filespace, error)
      CALL h5screate_simple_f(rank, dimsm, memspace, error)
    
      ! Create the dataset with default properties.
      CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, filespace, &
                       dset_id, error)
      CALL h5sclose_f(filespace, error)
    
      ! Each process defines dataset in memory and writes it to the hyperslab
      ! in the file. 
      count(1)  = dimsm(1)
      count(2)  = 1
      offset(1) = mpi_rank
      offset(2) = 0
!      stride(1) = 2
      stride(1) = mpi_size
      stride(2) = 1 
      block(1)  = 1
      block(2)  = dimsf(2)
       
      ! Select hyperslab in the file.
      CALL h5dget_space_f(dset_id, filespace, error)
      CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, &
                       offset, count, error, stride, block)
       
      ! Initialize data buffer with trivial data.
      ALLOCATE (data(dimsm(1),dimsm(2)))
      do i = 1, dimsm(1)
        data(i,:) = (mpi_rank+1)*fact
        fact = fact*10
      end do
    
      do i = 1, dimsm(1)
        write(*, *) (data(i, j), j = 1, dimsm(2))
      end do
      
      ! Create property list for collective dataset write
      CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
      CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
      
      CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dimsfi, error, & 
        file_space_id = filespace, mem_space_id = memspace, xfer_prp = plist_id)
      
      ! Write the dataset independently. 
     ! CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dimsfi, error, &
     !    file_space_id = filespace, mem_space_id = memspace)
      
      ! Deallocate data buffer.
      DEALLOCATE(data)
    
      ! Close dataspaces.
      CALL h5sclose_f(filespace, error)
      CALL h5sclose_f(memspace, error)
    
      ! Close the dataset and property list.
      CALL h5dclose_f(dset_id, error)
      CALL h5pclose_f(plist_id, error)
    
      ! Close the file.
      CALL h5fclose_f(file_id, error)

  end subroutine make_file

  subroutine read_file()

    allocate (data_out(dimsm(1), dimsm(2)))
    ! dimsm needs to change to be more generalized
    ! for any number of processors

    call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
    call h5pset_fapl_mpio_f(plist_id, comm, info, mpierror)

    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, & 
      error, access_prp=plist_id)

    call h5pclose_f(plist_id, error)

   ! count(1) = nbands/mpi_size
    count(1) = 1
    count(2) = ncols


    offset(1) = bands_per_proc*mpi_rank ! this may need a -1?
    offset(2) = 0

    count_out(2) = ncols
    count_out(1) = 1
    offset_out(2) = 0
    offset_out(1) = 0

    do j = 1, dimsm(2)
      do i = 1, count(1)
        data_out(i, j ) = mpi_rank
      end do
    end do

    write(*,*) "PID: ", mpi_rank, "has count = ", count(1), &
      "and offset = ", offset(1)

    call h5dopen_f(file_id, dsetname, dset_id, error)

    call h5dget_space_f(dset_id, dataspace, error)

    call h5screate_simple_f(2, dimsm, memspace, error) ! was dimsm
    ! BGW: call h5screate_simple_f(2, count, memspace, error)


    call h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, offset, & 
              count, error)

            ! This is proof of concept that hyperslab unions work.
            ! but, these i indices are not general
            ! AND in general won't be taking a union of every single
            ! row as its own hyperslab.
            ! This part will eventually model the non-parallel version
            ! That was written previously
            ! In order to do that, we also need to figure out how to
            ! Split up the pair matrix.
    do i = 1, bands_per_proc - 1
      offset(1) = offset(1) + 1
      call h5sselect_hyperslab_f(dataspace, H5S_SELECT_OR_F, offset, &
           count, error)
    end do

    call h5sselect_hyperslab_f(memspace, H5S_SELECT_SET_F, offset_out, &
      count_out, error)

    do i = 1, bands_per_proc - 1
      offset_out(1) = offset_out(1) + 1
      call h5sselect_hyperslab_f(memspace, H5S_SELECT_OR_F, offset_out, &
        count_out, error)
    end do

    ! For this chunk, go back and look at the BGW code after lunch
    ! This two calls were AFTER the hyperslab selections but before the read
    call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
    call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error) 
    ! was H5FD_MPIO_INDEPENDENT_F or H5FD_MPIO_COLLECTIVE_F
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, dimsm, error, memspace, &
      dataspace, xfer_prp=plist_id) ! this was previously data instead of
    ! data_out


    do i = 1, dimsm(1)
!      write(*,*) "PID: ", mpi_rank
      write(*,*) (mpi_rank, data_out(i, j), j=1, dimsm(2))
    end do


    call h5pclose_f(plist_id, error)
    call h5sclose_f(memspace, error)
    call h5sclose_f(dataspace, error)
    call h5dclose_f(dset_id, error)
!
    ! bands subroutine stuff ends here

    call h5fclose_f(file_id, error)

  end subroutine read_file



END PROGRAM selectbands_inclusion_parallel

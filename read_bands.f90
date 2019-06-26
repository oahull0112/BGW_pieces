PROGRAM read_bands

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

  INTEGER :: rank = 2 ! Dataset rank
  INTEGER :: nbands = 15
  INTEGER ::  ncols = 4
  INTEGER, DIMENSION(2) :: incl_size
  INTEGER, DIMENSION(3, 2) :: incl_bands
  INTEGER, ALLOCATABLE :: my_incl_bands(:,:)
  INTEGER :: nbands_excl, nbands_incl, ncore_excl

  INTEGER(HSIZE_T), DIMENSION(2) :: dimsf ! Dataset dimensions in file
  INTEGER(HSIZE_T), DIMENSION(2) :: dimsm ! dataset dims in memory
  INTEGER(HSIZE_T), DIMENSION(2) :: count, count_out
  INTEGER(HSSIZE_T), DIMENSION(2) :: offset, offset_out
  INTEGER(HSIZE_T), DIMENSION(2) :: stride
  INTEGER(HSIZE_T), DIMENSION(2) :: block

  INTEGER, ALLOCATABLE :: data (:,:)  ! Data to write
  INTEGER, ALLOCATABLE :: data_out(:,:) ! Data read back in

  INTEGER :: error, error_n  ! hdf5 error flags

  ! MPI definitions and calls.
  INTEGER :: mpierror       ! MPI error flag
  INTEGER :: comm, info
  INTEGER :: mpi_size, mpi_rank
  INTEGER :: file_write, ierr, status(MPI_STATUS_SIZE)
  integer, parameter :: send_data_tag = 2001
  integer, parameter :: return_data_tag = 2002
  integer :: bands_per_proc, my_bands_per_proc, remainder_procs

  ! Misc indexers and minor things
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

 ! Define the inclusion matrix 
  incl_bands(1, 1) = 2
  incl_bands(1, 2) = 2
  incl_bands(2, 1) = 4
  incl_bands(2, 2) = 6
  incl_bands(3, 1) = 8
  incl_bands(3, 2) = 9 


  dimsf(1) = nbands
  dimsf(2) = ncols

  remainder_procs = MOD(nbands, mpi_size)

  bands_per_proc = nbands/mpi_size ! note that this is only going to work if
  my_bands_per_proc = bands_per_proc

  ! The last processor gets the remainder bands
  if (mpi_rank .eq. mpi_size - 1) then
    my_bands_per_proc = bands_per_proc + remainder_procs
  end if

  dimsm(2) = ncols
!  dimsm(1) = bands_per_proc
  dimsm(1) = my_bands_per_proc

  ! Proc 0 makes the dummy data file
  call make_file()

  ! Wait for proc 0 to finish making the dummy file
  call MPI_Barrier(comm, mpierror)

  ! Actual work done here
  call read_file()


  ! Close FORTRAN interfaces and HDF5 library.
  CALL h5close_f(error)

  CALL MPI_FINALIZE(mpierror)

  contains

    subroutine make_file()

      allocate(data( nbands, ncols) )

      if (mpi_rank .eq. 0) then
        do i = 1, nbands
          do j = 1, ncols
            data(i, j) = i
          end do
        end do

        do i = 1, nbands
            write(*,*) (data(i, j), j = 1, ncols)
          end do

        call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
        call h5screate_simple_f(2, dimsf, dataspace, error)
        call h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
          dset_id, error)
        call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, dimsf, error)
        call h5sclose_f(dataspace, error)
        call h5dclose_f(dset_id, error)
        call h5fclose_f(file_id, error)

        file_write = 1

      end if

  end subroutine make_file

  subroutine read_file()

    allocate (data_out(dimsm(1), dimsm(2)))

    call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
    call h5pset_fapl_mpio_f(plist_id, comm, info, mpierror)

    call h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, & 
      error, access_prp=plist_id)

    call h5pclose_f(plist_id, error)

   ! count(1) = nbands/mpi_size
    count(1) = 1
    count(2) = ncols

    ! This whole section of code is building up the hyperslabs one-by-one with a
    ! count of 1, not taking advantage of the hyperslab "chunks"
    ! Now that we have my_incl_array, we can take advantage of larger hyperslabs

    offset(1) = bands_per_proc*mpi_rank
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

            ! This part will eventually model the non-parallel version
            ! That was written previously
            ! In order to do that, we also need to figure out how to
            ! Split up the pair matrix.
    do i = 1, my_bands_per_proc - 1
      offset(1) = offset(1) + 1
      call h5sselect_hyperslab_f(dataspace, H5S_SELECT_OR_F, offset, &
           count, error)
    end do

    call h5sselect_hyperslab_f(memspace, H5S_SELECT_SET_F, offset_out, &
      count_out, error)

    do i = 1, my_bands_per_proc - 1
      offset_out(1) = offset_out(1) + 1
      call h5sselect_hyperslab_f(memspace, H5S_SELECT_OR_F, offset_out, &
        count_out, error)
    end do

    call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
    call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error) 
    ! was H5FD_MPIO_INDEPENDENT_F or H5FD_MPIO_COLLECTIVE_F
    call h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, dimsm, error, memspace, &
      dataspace, xfer_prp=plist_id)


    do i = 1, dimsm(1)
      write(*,*) (mpi_rank, data_out(i, j), j=1, dimsm(2))
    end do


    call h5pclose_f(plist_id, error)
    call h5sclose_f(memspace, error)
    call h5sclose_f(dataspace, error)
    call h5dclose_f(dset_id, error)
    call h5fclose_f(file_id, error)

  end subroutine read_file

  subroutine inclusion_route_a()

    ! **************
    !
    ! This route takes an overall inclusion array
    ! and then tells which processors to deal with which
    ! rows based on the inclusion array
    !
    ! **************

    

    
  end subroutine inclusion_route_a



END PROGRAM read_bands

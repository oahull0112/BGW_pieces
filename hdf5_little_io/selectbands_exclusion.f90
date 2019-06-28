     PROGRAM SELECTEXAMPLE
! This is a (somewhat heavily) modified version of https://support.hdfgroup.org/ftp/HDF5/examples/introductory/F90/hyperslab.f90
! That creates an array, then selects only particular rows of the array to carry over to the memory dataspace
! It uses unions of hyperslabs to achieve this.
! Change the elements in excl_bands to change which rows are excluded
! This program accomplishes the same goal as selectbands_inclusion, except through the use of an array explicitly stating
! which bands to exclude, whereas selectbands_inclusion.f90 takes a 2D array (rather than 1D) that specifies the ranges of pairs
! To include. Ultimately, selectbands_inclusion.f90 will likely be more efficient.

     USE HDF5 ! This module contains all necessary modules 
    
     IMPLICIT NONE

     CHARACTER(LEN=7), PARAMETER :: filename = "sdsf.h5"  ! File name
     CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray" ! Dataset name

     INTEGER(HID_T) :: file_id       ! File identifier 
     INTEGER(HID_T) :: dset_id       ! Dataset identifier 
     INTEGER(HID_T) :: dataspace     ! Dataspace identifier 
     INTEGER(HID_T) :: memspace      ! memspace identifier 

     INTEGER(HSIZE_T), DIMENSION(2) :: dimsm = (/10,6/) ! Dataset dimensionsin memory
     INTEGER(HSIZE_T), DIMENSION(2) :: dims_out ! Buffer to read in dataset dimesions
     INTEGER(HSIZE_T), DIMENSION(2) :: dimsf = (/10,6/) ! Dataset dimensions.
     INTEGER(HSIZE_T), DIMENSION(2) :: count = (/1,6/) ! Size of the hyperslab in the file
     INTEGER(HSIZE_T), DIMENSION(2) :: offset = (/1,0/) !hyperslab offset in the file 
     INTEGER(HSIZE_T), DIMENSION(2) :: count_out = (/1,6/) !Size of the hyperslab in memory 
     INTEGER(HSIZE_T), DIMENSION(2) :: offset_out = (/0,0/) !hyperslab offset in memory 

     INTEGER, DIMENSION(4) :: excl_bands
     INTEGER, DIMENSION(1) :: nbands_excl_temp
     INTEGER :: nbands_excl
     INTEGER :: ncore_excl

     INTEGER, DIMENSION(10,6) :: data ! Data to write
     INTEGER, DIMENSION(10,6) :: data_out ! Output buffer
     INTEGER :: dsetrank = 2 ! Dataset rank ( in file )
     INTEGER :: memrank = 2  ! Dataset rank ( in memory )
     INTEGER :: rank 
     INTEGER :: i, j, k 

     INTEGER :: error, error_n  ! Error flags
     INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
    
     excl_bands(1) = 1 
     excl_bands(2) = 3 
     excl_bands(3) = 6 
     excl_bands(4) = 11

! Figure out the number of excluded core states
     nbands_excl_temp = SHAPE(excl_bands)
     nbands_excl = nbands_excl_temp(1)
     ncore_excl = 0 
    
     if (excl_bands(1) .eq. 1) then ! depending on how to label bands this may = 1
       ncore_excl = ncore_excl + 1 
       do i = 1, nbands_excl-1
         if (excl_bands(i + 1) - excl_bands(i) .ne. 1) exit
         ncore_excl = ncore_excl + 1 
       enddo
     endif

     write(*,*) ncore_excl

   ! Write data to the HDF5 file.  
     ! Data initialization. 
     do i = 1, 10
          do j = 1, 6
               data(i,j) = 1 + (i-1) + (j-1);
          end do
     end do

     do i = 1, 10
         print *, (data(i,j), j = 1,6)
     end do

     ! Initialize FORTRAN interface. 
     CALL h5open_f(error)

     ! Create a new file using default properties.
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

     ! Create the data space for the  dataset. 
     CALL h5screate_simple_f(dsetrank, dimsf, dataspace, error)

     ! Create the dataset with default properties.
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
                      dset_id, error)

     ! Write the dataset.
     data_dims(1) = 10
     data_dims(2) = 6
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, data_dims, error)

     ! Close the dataspace for the dataset.
     CALL h5sclose_f(dataspace, error)

     ! Close the dataset.
     CALL h5dclose_f(dset_id, error)

     ! Close the file.
     CALL h5fclose_f(file_id, error)

  ! This  part of the code reads the hyperslab from the sds.h5 file just 
  ! created, into a 2-dimensional array

     ! Initialize data_out array.
     do j = 1, 6
         do i = 1, 10
             data_out(i,j) = 0;
         end do
     end do

     ! Open the file.
     CALL h5fopen_f (filename, H5F_ACC_RDWR_F, file_id, error)

     ! Open the  dataset.
     CALL h5dopen_f(file_id, dsetname, dset_id, error)

     ! Get dataset's dataspace identifier.
     CALL h5dget_space_f(dset_id, dataspace, error)

     ! Select hyperslab in the dataset.
     offset(1) = ncore_excl ! see if this should be ncore_excl or ncore_excl + 1
     count(1) = excl_bands(ncore_excl + 1) - excl_bands(ncore_excl) - 1
     CALL h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, &
                                offset, count, error)

     do i = 1, ( nbands_excl - ncore_excl) - 1
       offset(1) = excl_bands(ncore_excl + i) !+ 1
       count(1) = excl_bands(ncore_excl + i + 1) - excl_bands(ncore_excl + i) - 1
       CALL h5sselect_hyperslab_f(dataspace, H5S_SELECT_OR_F, offset, count, error)
     enddo

     ! Create memory dataspace.
     CALL h5screate_simple_f(memrank, dimsm, memspace, error)
     ! Select hyperslab in memory.

     count_out(1) = excl_bands(ncore_excl + 1) - excl_bands(ncore_excl) - 1
     ! offset_out does not change for the first call!
     CALL h5sselect_hyperslab_f(memspace, H5S_SELECT_SET_F, &
                                offset_out, count_out, error)

     write(*,*) offset_out(1), count_out(1)

     offset_out(1) = count_out(1)
     do i = 1, (nbands_excl - ncore_excl) - 1
       count_out(1) = excl_bands(ncore_excl + i + 1) - excl_bands(ncore_excl + i) - 1
       write(*,*) offset_out(1), count_out(1)
       CALL h5sselect_hyperslab_f(memspace, H5S_SELECT_OR_F, offset_out, count_out, error)
       offset_out(1) = offset_out(1) + count_out(1)
     enddo

     ! Read data from hyperslab in the file into the hyperslab in 
     ! memory and display.
     data_dims(1) = 10
     data_dims(2) = 6
     CALL H5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error, &
                    memspace, dataspace)
     ! Display data_out array
     do i = 1, 10
         print *, (data_out(i,j), j = 1,6)
     end do

     ! Close the dataspace for the dataset.
     CALL h5sclose_f(dataspace, error)

     ! Close the memoryspace.
     CALL h5sclose_f(memspace, error)

     ! Close the dataset.
     CALL h5dclose_f(dset_id, error)

     ! Close the file.
     CALL h5fclose_f(file_id, error)

     ! Close FORTRAN interface.
     CALL h5close_f(error)

     END PROGRAM SELECTEXAMPLE
                                 

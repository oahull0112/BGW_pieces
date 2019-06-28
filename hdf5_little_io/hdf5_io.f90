! File hdf5_io.f90 automatically created from hdf5_io.f90p by mako_preprocess.py.
! Do not edit the resulting file (hdf5_io.f90) directly!

!>=========================================================================
!!
!! Module:
!!
!! hdf5_io_m        Originally by FHJ     Last Modified 03/2018 (FHJ)
!!
!!     High-level routines to read and write data in HDF5 format.
!!     This module uses python Mako template to generate Fortran code.
!!     Make sure to run the `mako_preprocess.py` if you edit the source
!!     .f90p file, and don`t edit the generated .f90 directly.
!!
!!=========================================================================

#include "f_defs.h"

module hdf5_io_m
  use, intrinsic :: iso_c_binding
  use global_m
#ifdef HDF5
  use hdf5
  
  implicit none
  
  private
  
  public :: &
    hdf5_read_int, &
    hdf5_read_int_array, &
    hdf5_read_int_hyperslab, &
    hdf5_write_int, &
    hdf5_write_int_array, &
    hdf5_write_int_hyperslab, &
    hdf5_read_double, &
    hdf5_read_double_array, &
    hdf5_read_double_hyperslab, &
    hdf5_write_double, &
    hdf5_write_double_array, &
    hdf5_write_double_hyperslab, &
    hdf5_read_complex, &
    hdf5_read_complex_array, &
    hdf5_read_complex_hyperslab, &
    hdf5_write_complex, &
    hdf5_write_complex_array, &
    hdf5_write_complex_hyperslab, &
    hdf5_read_scalar, &
    hdf5_read_scalar_array, &
    hdf5_read_scalar_hyperslab, &
    hdf5_write_scalar, &
    hdf5_write_scalar_array, &
    hdf5_write_scalar_hyperslab, &
    hdf5_read_logical, &
    hdf5_read_logical_array, &
    hdf5_read_logical_hyperslab, &
    hdf5_write_logical, &
    hdf5_write_logical_array, &
    hdf5_write_logical_hyperslab, &
    hdf5_require_version        , &
    hdf5_require_flavor         , &
    hdf5_create_dset            , &
    hdf5_create_group           , &
    ptr_complex2real            , &
    ptr_real2complex

  interface hdf5_read_scalar
    module procedure hdf5_read_double, hdf5_read_complex
  end interface
  interface hdf5_read_scalar_array
    module procedure &
hdf5_read_double_array_1, hdf5_read_complex_array_1, &
hdf5_read_double_array_2, hdf5_read_complex_array_2, &
hdf5_read_double_array_3, hdf5_read_complex_array_3, &
hdf5_read_double_array_4, hdf5_read_complex_array_4, &
hdf5_read_double_array_5, hdf5_read_complex_array_5, &
hdf5_read_double_array_6, hdf5_read_complex_array_6, &
hdf5_read_double_array_7, hdf5_read_complex_array_7
  end interface
  interface hdf5_read_scalar_hyperslab
    module procedure &
hdf5_read_double_hyperslab_1, hdf5_read_complex_hyperslab_1, &
hdf5_read_double_hyperslab_2, hdf5_read_complex_hyperslab_2, &
hdf5_read_double_hyperslab_3, hdf5_read_complex_hyperslab_3, &
hdf5_read_double_hyperslab_4, hdf5_read_complex_hyperslab_4, &
hdf5_read_double_hyperslab_5, hdf5_read_complex_hyperslab_5, &
hdf5_read_double_hyperslab_6, hdf5_read_complex_hyperslab_6, &
hdf5_read_double_hyperslab_7, hdf5_read_complex_hyperslab_7
  end interface
  interface hdf5_write_scalar
    module procedure hdf5_write_double, hdf5_write_complex
  end interface
  interface hdf5_write_scalar_array
    module procedure &
hdf5_write_double_array_1, hdf5_write_complex_array_1, &
hdf5_write_double_array_2, hdf5_write_complex_array_2, &
hdf5_write_double_array_3, hdf5_write_complex_array_3, &
hdf5_write_double_array_4, hdf5_write_complex_array_4, &
hdf5_write_double_array_5, hdf5_write_complex_array_5, &
hdf5_write_double_array_6, hdf5_write_complex_array_6, &
hdf5_write_double_array_7, hdf5_write_complex_array_7
  end interface
  interface hdf5_write_scalar_hyperslab
    module procedure &
hdf5_write_double_hyperslab_1, hdf5_write_complex_hyperslab_1, &
hdf5_write_double_hyperslab_2, hdf5_write_complex_hyperslab_2, &
hdf5_write_double_hyperslab_3, hdf5_write_complex_hyperslab_3, &
hdf5_write_double_hyperslab_4, hdf5_write_complex_hyperslab_4, &
hdf5_write_double_hyperslab_5, hdf5_write_complex_hyperslab_5, &
hdf5_write_double_hyperslab_6, hdf5_write_complex_hyperslab_6, &
hdf5_write_double_hyperslab_7, hdf5_write_complex_hyperslab_7
  end interface
    
contains


!> Return a pointer that is a complex reinterpretation of the double precision
!! input array data_in. No data is copied if the input array is contiguous.
!! `sz` is the total number of elements of the input array.
function ptr_complex2real(data_in, sz) result(data_out)
  complex(DPC), intent(inout), target :: data_in(*)
  integer, intent(in) :: sz
  real(DP), pointer :: data_out(:)

  call c_f_pointer(c_loc(data_in), data_out, [2*sz])

end function ptr_complex2real


!> Return a pointer that is a double precision reinterpretation of the complex
!! input array data_in. No data is copied if the input array is contiguous.
!! `sz` is the total number of elements of the input array.
function ptr_real2complex(data_in, sz) result(data_out)
  real(DP), intent(inout), target :: data_in(*)
  integer, intent(in) :: sz
  complex(DPC), pointer :: data_out(:)

  call c_f_pointer(c_loc(data_in), data_out, [sz/2])

end function ptr_real2complex


!> Make sure that an hdf5 file has the correct version number.
subroutine hdf5_require_version(loc_id, dset_name, req_version, fname, allow_greater)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in) :: req_version !< version to require
  character(LEN=*), intent(in) :: fname !< file name, for debugging purposes
  !> allow the file to have a version greater than req_version? Defaults to .false.
  logical, intent(in), optional :: allow_greater
  
  integer :: file_version, errcode
  logical :: allow_greater_

  PUSH_SUB(hdf5_require_version)

  file_version = -1
  call hdf5_read_int(loc_id, dset_name, file_version, errcode)
  allow_greater_ = .false.
  if (present(allow_greater)) allow_greater_ = allow_greater
  if (file_version<req_version .or. &
    (file_version/=req_version.and..not.allow_greater_) .or. errcode/=0) then
    if (peinf%inode==0) then
      write(0,*)
      write(0,*) 'ERROR: Incorrect version in file ', trim(fname),' while reading ',trim(dset_name)
      write(0,*) '       Expecting: ', req_version
      write(0,*) '       Got: ', file_version
      write(0,*) '       Errcode: ', errcode
      write(0,*) 'Your file was probably generated by an older version of BerkeleyGW and'
      write(0,*) 'is now obsolete. Consult the documentation and use the appropriate converter.'
      write(0,*)
    endif
    call die("Wrong version for file '"+trim(fname)+"'.", only_root_writes=.true.)
  endif

  POP_SUB(hdf5_require_version)

end subroutine hdf5_require_version


!> Make sure that an hdf5 file has the correct flavor.
subroutine hdf5_require_flavor(loc_id, dset_name, req_flavor, fname)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in) :: req_flavor !< flavor to require
  character(LEN=*), intent(in) :: fname !< file name, for debugging purposes
  
  integer :: file_flavor, errcode

  PUSH_SUB(hdf5_require_flavor)

  file_flavor = -1
  call hdf5_read_int(loc_id, dset_name, file_flavor, errcode)
  if (file_flavor/=req_flavor.or.errcode/=0) then
    if (peinf%inode==0) then
      write(0,*)
      write(0,*) 'ERROR: Incorrect flavor in file ', trim(fname), ' while reading ',trim(dset_name)
      write(0,*) '       Expecting: ', req_flavor
      write(0,*) '       Got: ', file_flavor
      write(0,*) '       Errcode: ', errcode
      write(0,*) 'You are probably linking the wrong file or running the BerkeleyGW binary with'
      write(0,*) 'the wrong flavor.'
      write(0,*)
    endif
    call die("Wrong flavor in file "+trim(fname)+"'.", only_root_writes=.true.)
  endif

  POP_SUB(hdf5_require_flavor)

end subroutine hdf5_require_flavor


!> Create an empty dataset.
subroutine hdf5_create_dset(loc_id, dset_name, dtype, dims, error)
  integer(HID_T), intent(in) :: loc_id !< hdf5 file id
  character(LEN=*), intent(in) :: dset_name !< hdf5 dataset name
  integer(HID_T), intent(in) :: dtype !< hdf5 data type
  integer, intent(in) :: dims(:) !< dimensions of the array
  integer, intent(out), optional :: error !< error code
  
  integer(HSIZE_T) :: hdims(size(dims))
  integer(HID_T) :: dset_id
  integer(HID_T) :: dspace
  integer :: errcode

  PUSH_SUB(hdf5_create_dset)

  hdims(:) = dims(:)
  call h5screate_simple_f(size(dims), hdims, dspace, errcode)
  call h5dcreate_f(loc_id, dset_name, dtype, dspace, dset_id, errcode)
  call h5dclose_f(dset_id, errcode)
  call h5sclose_f(dspace, errcode)

  POP_SUB(hdf5_create_dset)

end subroutine hdf5_create_dset


!> Create an empty group.
subroutine hdf5_create_group(loc_id, group_name, errcode)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: group_name !< HDF5 group name
  integer, intent(out) :: errcode
  
  integer(HID_T) :: group_id

  PUSH_SUB(hdf5_create_group)

  call h5gcreate_f(loc_id, group_name, group_id, errcode)
  call h5gclose_f(group_id, errcode)
  
  POP_SUB(hdf5_create_group)

end subroutine hdf5_create_group


!######################################
!# Beginning of high-level interfaces #
!######################################


!-------------------------------
! Routines for int data type
!-------------------------------

!> read int value (rank-0 array) to/from an HDF5 file.
subroutine hdf5_read_int(loc_id, dset_name, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(inout), target :: buf !< data buffer
  integer, intent(out), optional :: error !< HDF5 error code

  integer, pointer :: buf_1d(:)

  PUSH_SUB(hdf5_read_int)
  call c_f_pointer(c_loc(buf), buf_1d, [1])
  call hdf5_read_int_lowlevel(loc_id, dset_name, [-1], buf_1d, error=error)
  POP_SUB(hdf5_read_int)

end subroutine hdf5_read_int

!> read int array to/from an HDF5 file.
subroutine hdf5_read_int_array(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  integer, intent(inout), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_int_array)
  call hdf5_read_int_lowlevel(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_int_array)

end subroutine hdf5_read_int_array

!> read section (hyperslab) of int array to/from an HDF5 file.
subroutine hdf5_read_int_hyperslab(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  integer, intent(inout), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_int_hyperslab)
  call hdf5_read_int_lowlevel(loc_id, dset_name, read_count, buf, error, offsetf=offset)
  POP_SUB(hdf5_read_int_hyperslab)

end subroutine hdf5_read_int_hyperslab

!> write int value (rank-0 array) to/from an HDF5 file.
subroutine hdf5_write_int(loc_id, dset_name, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), target :: buf !< data buffer
  integer, intent(out), optional :: error !< HDF5 error code

  integer, pointer :: buf_1d(:)

  PUSH_SUB(hdf5_write_int)
  call c_f_pointer(c_loc(buf), buf_1d, [1])
  call hdf5_write_int_lowlevel(loc_id, dset_name, [-1], buf_1d, error=error)
  POP_SUB(hdf5_write_int)

end subroutine hdf5_write_int

!> write int array to/from an HDF5 file.
subroutine hdf5_write_int_array(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  integer, intent(in), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_int_array)
  call hdf5_write_int_lowlevel(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_int_array)

end subroutine hdf5_write_int_array

!> write section (hyperslab) of int array to/from an HDF5 file.
subroutine hdf5_write_int_hyperslab(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  integer, intent(in), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_int_hyperslab)
  call hdf5_write_int_lowlevel(loc_id, dset_name, read_count, buf, error, offsetf=offset)
  POP_SUB(hdf5_write_int_hyperslab)

end subroutine hdf5_write_int_hyperslab

!-------------------------------
! Routines for double data type
!-------------------------------

!> read double value (rank-0 array) to/from an HDF5 file.
subroutine hdf5_read_double(loc_id, dset_name, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  real(DP), intent(inout), target :: buf !< data buffer
  integer, intent(out), optional :: error !< HDF5 error code

  real(DP), pointer :: buf_1d(:)

  PUSH_SUB(hdf5_read_double)
  call c_f_pointer(c_loc(buf), buf_1d, [1])
  call hdf5_read_double_lowlevel(loc_id, dset_name, [-1], buf_1d, error=error)
  POP_SUB(hdf5_read_double)

end subroutine hdf5_read_double

!> read double array to/from an HDF5 file.
subroutine hdf5_read_double_array(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(inout), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_array)
  call hdf5_read_double_lowlevel(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_double_array)

end subroutine hdf5_read_double_array

!> read section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_read_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(inout), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_hyperslab)
  call hdf5_read_double_lowlevel(loc_id, dset_name, read_count, buf, error, offsetf=offset)
  POP_SUB(hdf5_read_double_hyperslab)

end subroutine hdf5_read_double_hyperslab

!> write double value (rank-0 array) to/from an HDF5 file.
subroutine hdf5_write_double(loc_id, dset_name, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  real(DP), intent(in), target :: buf !< data buffer
  integer, intent(out), optional :: error !< HDF5 error code

  real(DP), pointer :: buf_1d(:)

  PUSH_SUB(hdf5_write_double)
  call c_f_pointer(c_loc(buf), buf_1d, [1])
  call hdf5_write_double_lowlevel(loc_id, dset_name, [-1], buf_1d, error=error)
  POP_SUB(hdf5_write_double)

end subroutine hdf5_write_double

!> write double array to/from an HDF5 file.
subroutine hdf5_write_double_array(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(in), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_array)
  call hdf5_write_double_lowlevel(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_double_array)

end subroutine hdf5_write_double_array

!> write section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_write_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(in), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_hyperslab)
  call hdf5_write_double_lowlevel(loc_id, dset_name, read_count, buf, error, offsetf=offset)
  POP_SUB(hdf5_write_double_hyperslab)

end subroutine hdf5_write_double_hyperslab

!-------------------------------
! Routines for complex data type
!-------------------------------

!> read complex value (rank-0 array) to/from an HDF5 file.
subroutine hdf5_read_complex(loc_id, dset_name, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  complex(DPC), intent(inout), target :: buf !< data buffer
  integer, intent(out), optional :: error !< HDF5 error code

  complex(DPC), pointer :: buf_1d(:)

  PUSH_SUB(hdf5_read_complex)
  call c_f_pointer(c_loc(buf), buf_1d, [1])
  call hdf5_read_complex_lowlevel(loc_id, dset_name, [-1], buf_1d, error=error)
  POP_SUB(hdf5_read_complex)

end subroutine hdf5_read_complex

!> read complex array to/from an HDF5 file.
subroutine hdf5_read_complex_array(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(inout), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_array)
  call hdf5_read_complex_lowlevel(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_complex_array)

end subroutine hdf5_read_complex_array

!> read section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_read_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(inout), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_hyperslab)
  call hdf5_read_complex_lowlevel(loc_id, dset_name, read_count, buf, error, offsetf=offset)
  POP_SUB(hdf5_read_complex_hyperslab)

end subroutine hdf5_read_complex_hyperslab

!> write complex value (rank-0 array) to/from an HDF5 file.
subroutine hdf5_write_complex(loc_id, dset_name, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  complex(DPC), intent(in), target :: buf !< data buffer
  integer, intent(out), optional :: error !< HDF5 error code

  complex(DPC), pointer :: buf_1d(:)

  PUSH_SUB(hdf5_write_complex)
  call c_f_pointer(c_loc(buf), buf_1d, [1])
  call hdf5_write_complex_lowlevel(loc_id, dset_name, [-1], buf_1d, error=error)
  POP_SUB(hdf5_write_complex)

end subroutine hdf5_write_complex

!> write complex array to/from an HDF5 file.
subroutine hdf5_write_complex_array(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(in), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_array)
  call hdf5_write_complex_lowlevel(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_complex_array)

end subroutine hdf5_write_complex_array

!> write section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_write_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(in), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_hyperslab)
  call hdf5_write_complex_lowlevel(loc_id, dset_name, read_count, buf, error, offsetf=offset)
  POP_SUB(hdf5_write_complex_hyperslab)

end subroutine hdf5_write_complex_hyperslab

!-------------------------------
! Routines for logical data type
!-------------------------------

!> read logical value (rank-0 array) to/from an HDF5 file.
subroutine hdf5_read_logical(loc_id, dset_name, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  logical, intent(inout), target :: buf !< data buffer
  integer, intent(out), optional :: error !< HDF5 error code

  logical, pointer :: buf_1d(:)

  PUSH_SUB(hdf5_read_logical)
  call c_f_pointer(c_loc(buf), buf_1d, [1])
  call hdf5_read_logical_lowlevel(loc_id, dset_name, [-1], buf_1d, error=error)
  POP_SUB(hdf5_read_logical)

end subroutine hdf5_read_logical

!> read logical array to/from an HDF5 file.
subroutine hdf5_read_logical_array(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  logical, intent(inout), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_logical_array)
  call hdf5_read_logical_lowlevel(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_logical_array)

end subroutine hdf5_read_logical_array

!> read section (hyperslab) of logical array to/from an HDF5 file.
subroutine hdf5_read_logical_hyperslab(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  logical, intent(inout), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_logical_hyperslab)
  call hdf5_read_logical_lowlevel(loc_id, dset_name, read_count, buf, error, offsetf=offset)
  POP_SUB(hdf5_read_logical_hyperslab)

end subroutine hdf5_read_logical_hyperslab

!> write logical value (rank-0 array) to/from an HDF5 file.
subroutine hdf5_write_logical(loc_id, dset_name, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  logical, intent(in), target :: buf !< data buffer
  integer, intent(out), optional :: error !< HDF5 error code

  logical, pointer :: buf_1d(:)

  PUSH_SUB(hdf5_write_logical)
  call c_f_pointer(c_loc(buf), buf_1d, [1])
  call hdf5_write_logical_lowlevel(loc_id, dset_name, [-1], buf_1d, error=error)
  POP_SUB(hdf5_write_logical)

end subroutine hdf5_write_logical

!> write logical array to/from an HDF5 file.
subroutine hdf5_write_logical_array(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  logical, intent(in), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_logical_array)
  call hdf5_write_logical_lowlevel(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_logical_array)

end subroutine hdf5_write_logical_array

!> write section (hyperslab) of logical array to/from an HDF5 file.
subroutine hdf5_write_logical_hyperslab(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  logical, intent(in), dimension(*) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_logical_hyperslab)
  call hdf5_write_logical_lowlevel(loc_id, dset_name, read_count, buf, error, offsetf=offset)
  POP_SUB(hdf5_write_logical_hyperslab)

end subroutine hdf5_write_logical_hyperslab

!################################
!# End of high-level interfaces #
!################################


!#####################################################
!# Intermediate routines for ASSUMED-SHAPE arguments #
!#####################################################


!> read double array to/from an HDF5 file.
subroutine hdf5_read_double_array_1(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(inout), dimension(:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_array_1)
  call hdf5_read_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_double_array_1)

end subroutine hdf5_read_double_array_1

!> read section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_read_double_hyperslab_1(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(inout), dimension(:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_hyperslab_1)
  call hdf5_read_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_double_hyperslab_1)

end subroutine hdf5_read_double_hyperslab_1

!> read double array to/from an HDF5 file.
subroutine hdf5_read_double_array_2(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(inout), dimension(:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_array_2)
  call hdf5_read_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_double_array_2)

end subroutine hdf5_read_double_array_2

!> read section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_read_double_hyperslab_2(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(inout), dimension(:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_hyperslab_2)
  call hdf5_read_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_double_hyperslab_2)

end subroutine hdf5_read_double_hyperslab_2

!> read double array to/from an HDF5 file.
subroutine hdf5_read_double_array_3(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(inout), dimension(:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_array_3)
  call hdf5_read_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_double_array_3)

end subroutine hdf5_read_double_array_3

!> read section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_read_double_hyperslab_3(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(inout), dimension(:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_hyperslab_3)
  call hdf5_read_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_double_hyperslab_3)

end subroutine hdf5_read_double_hyperslab_3

!> read double array to/from an HDF5 file.
subroutine hdf5_read_double_array_4(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(inout), dimension(:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_array_4)
  call hdf5_read_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_double_array_4)

end subroutine hdf5_read_double_array_4

!> read section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_read_double_hyperslab_4(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(inout), dimension(:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_hyperslab_4)
  call hdf5_read_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_double_hyperslab_4)

end subroutine hdf5_read_double_hyperslab_4

!> read double array to/from an HDF5 file.
subroutine hdf5_read_double_array_5(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(inout), dimension(:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_array_5)
  call hdf5_read_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_double_array_5)

end subroutine hdf5_read_double_array_5

!> read section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_read_double_hyperslab_5(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(inout), dimension(:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_hyperslab_5)
  call hdf5_read_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_double_hyperslab_5)

end subroutine hdf5_read_double_hyperslab_5

!> read double array to/from an HDF5 file.
subroutine hdf5_read_double_array_6(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(inout), dimension(:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_array_6)
  call hdf5_read_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_double_array_6)

end subroutine hdf5_read_double_array_6

!> read section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_read_double_hyperslab_6(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(inout), dimension(:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_hyperslab_6)
  call hdf5_read_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_double_hyperslab_6)

end subroutine hdf5_read_double_hyperslab_6

!> read double array to/from an HDF5 file.
subroutine hdf5_read_double_array_7(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(inout), dimension(:,:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_array_7)
  call hdf5_read_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_double_array_7)

end subroutine hdf5_read_double_array_7

!> read section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_read_double_hyperslab_7(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(inout), dimension(:,:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_double_hyperslab_7)
  call hdf5_read_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_double_hyperslab_7)

end subroutine hdf5_read_double_hyperslab_7

!> write double array to/from an HDF5 file.
subroutine hdf5_write_double_array_1(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(in), dimension(:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_array_1)
  call hdf5_write_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_double_array_1)

end subroutine hdf5_write_double_array_1

!> write section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_write_double_hyperslab_1(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(in), dimension(:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_hyperslab_1)
  call hdf5_write_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_double_hyperslab_1)

end subroutine hdf5_write_double_hyperslab_1

!> write double array to/from an HDF5 file.
subroutine hdf5_write_double_array_2(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(in), dimension(:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_array_2)
  call hdf5_write_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_double_array_2)

end subroutine hdf5_write_double_array_2

!> write section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_write_double_hyperslab_2(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(in), dimension(:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_hyperslab_2)
  call hdf5_write_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_double_hyperslab_2)

end subroutine hdf5_write_double_hyperslab_2

!> write double array to/from an HDF5 file.
subroutine hdf5_write_double_array_3(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(in), dimension(:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_array_3)
  call hdf5_write_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_double_array_3)

end subroutine hdf5_write_double_array_3

!> write section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_write_double_hyperslab_3(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(in), dimension(:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_hyperslab_3)
  call hdf5_write_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_double_hyperslab_3)

end subroutine hdf5_write_double_hyperslab_3

!> write double array to/from an HDF5 file.
subroutine hdf5_write_double_array_4(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(in), dimension(:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_array_4)
  call hdf5_write_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_double_array_4)

end subroutine hdf5_write_double_array_4

!> write section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_write_double_hyperslab_4(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(in), dimension(:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_hyperslab_4)
  call hdf5_write_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_double_hyperslab_4)

end subroutine hdf5_write_double_hyperslab_4

!> write double array to/from an HDF5 file.
subroutine hdf5_write_double_array_5(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(in), dimension(:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_array_5)
  call hdf5_write_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_double_array_5)

end subroutine hdf5_write_double_array_5

!> write section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_write_double_hyperslab_5(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(in), dimension(:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_hyperslab_5)
  call hdf5_write_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_double_hyperslab_5)

end subroutine hdf5_write_double_hyperslab_5

!> write double array to/from an HDF5 file.
subroutine hdf5_write_double_array_6(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(in), dimension(:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_array_6)
  call hdf5_write_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_double_array_6)

end subroutine hdf5_write_double_array_6

!> write section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_write_double_hyperslab_6(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(in), dimension(:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_hyperslab_6)
  call hdf5_write_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_double_hyperslab_6)

end subroutine hdf5_write_double_hyperslab_6

!> write double array to/from an HDF5 file.
subroutine hdf5_write_double_array_7(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  real(DP), intent(in), dimension(:,:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_array_7)
  call hdf5_write_double_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_double_array_7)

end subroutine hdf5_write_double_array_7

!> write section (hyperslab) of double array to/from an HDF5 file.
subroutine hdf5_write_double_hyperslab_7(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  real(DP), intent(in), dimension(:,:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_double_hyperslab_7)
  call hdf5_write_double_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_double_hyperslab_7)

end subroutine hdf5_write_double_hyperslab_7

!> read complex array to/from an HDF5 file.
subroutine hdf5_read_complex_array_1(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(inout), dimension(:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_array_1)
  call hdf5_read_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_complex_array_1)

end subroutine hdf5_read_complex_array_1

!> read section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_read_complex_hyperslab_1(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(inout), dimension(:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_hyperslab_1)
  call hdf5_read_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_complex_hyperslab_1)

end subroutine hdf5_read_complex_hyperslab_1

!> read complex array to/from an HDF5 file.
subroutine hdf5_read_complex_array_2(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(inout), dimension(:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_array_2)
  call hdf5_read_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_complex_array_2)

end subroutine hdf5_read_complex_array_2

!> read section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_read_complex_hyperslab_2(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(inout), dimension(:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_hyperslab_2)
  call hdf5_read_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_complex_hyperslab_2)

end subroutine hdf5_read_complex_hyperslab_2

!> read complex array to/from an HDF5 file.
subroutine hdf5_read_complex_array_3(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(inout), dimension(:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_array_3)
  call hdf5_read_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_complex_array_3)

end subroutine hdf5_read_complex_array_3

!> read section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_read_complex_hyperslab_3(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(inout), dimension(:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_hyperslab_3)
  call hdf5_read_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_complex_hyperslab_3)

end subroutine hdf5_read_complex_hyperslab_3

!> read complex array to/from an HDF5 file.
subroutine hdf5_read_complex_array_4(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(inout), dimension(:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_array_4)
  call hdf5_read_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_complex_array_4)

end subroutine hdf5_read_complex_array_4

!> read section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_read_complex_hyperslab_4(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(inout), dimension(:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_hyperslab_4)
  call hdf5_read_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_complex_hyperslab_4)

end subroutine hdf5_read_complex_hyperslab_4

!> read complex array to/from an HDF5 file.
subroutine hdf5_read_complex_array_5(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(inout), dimension(:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_array_5)
  call hdf5_read_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_complex_array_5)

end subroutine hdf5_read_complex_array_5

!> read section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_read_complex_hyperslab_5(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(inout), dimension(:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_hyperslab_5)
  call hdf5_read_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_complex_hyperslab_5)

end subroutine hdf5_read_complex_hyperslab_5

!> read complex array to/from an HDF5 file.
subroutine hdf5_read_complex_array_6(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(inout), dimension(:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_array_6)
  call hdf5_read_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_complex_array_6)

end subroutine hdf5_read_complex_array_6

!> read section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_read_complex_hyperslab_6(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(inout), dimension(:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_hyperslab_6)
  call hdf5_read_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_complex_hyperslab_6)

end subroutine hdf5_read_complex_hyperslab_6

!> read complex array to/from an HDF5 file.
subroutine hdf5_read_complex_array_7(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(inout), dimension(:,:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_array_7)
  call hdf5_read_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_read_complex_array_7)

end subroutine hdf5_read_complex_array_7

!> read section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_read_complex_hyperslab_7(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(inout), dimension(:,:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_read_complex_hyperslab_7)
  call hdf5_read_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_read_complex_hyperslab_7)

end subroutine hdf5_read_complex_hyperslab_7

!> write complex array to/from an HDF5 file.
subroutine hdf5_write_complex_array_1(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(in), dimension(:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_array_1)
  call hdf5_write_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_complex_array_1)

end subroutine hdf5_write_complex_array_1

!> write section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_write_complex_hyperslab_1(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(in), dimension(:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_hyperslab_1)
  call hdf5_write_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_complex_hyperslab_1)

end subroutine hdf5_write_complex_hyperslab_1

!> write complex array to/from an HDF5 file.
subroutine hdf5_write_complex_array_2(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(in), dimension(:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_array_2)
  call hdf5_write_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_complex_array_2)

end subroutine hdf5_write_complex_array_2

!> write section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_write_complex_hyperslab_2(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(in), dimension(:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_hyperslab_2)
  call hdf5_write_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_complex_hyperslab_2)

end subroutine hdf5_write_complex_hyperslab_2

!> write complex array to/from an HDF5 file.
subroutine hdf5_write_complex_array_3(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(in), dimension(:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_array_3)
  call hdf5_write_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_complex_array_3)

end subroutine hdf5_write_complex_array_3

!> write section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_write_complex_hyperslab_3(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(in), dimension(:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_hyperslab_3)
  call hdf5_write_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_complex_hyperslab_3)

end subroutine hdf5_write_complex_hyperslab_3

!> write complex array to/from an HDF5 file.
subroutine hdf5_write_complex_array_4(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(in), dimension(:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_array_4)
  call hdf5_write_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_complex_array_4)

end subroutine hdf5_write_complex_array_4

!> write section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_write_complex_hyperslab_4(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(in), dimension(:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_hyperslab_4)
  call hdf5_write_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_complex_hyperslab_4)

end subroutine hdf5_write_complex_hyperslab_4

!> write complex array to/from an HDF5 file.
subroutine hdf5_write_complex_array_5(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(in), dimension(:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_array_5)
  call hdf5_write_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_complex_array_5)

end subroutine hdf5_write_complex_array_5

!> write section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_write_complex_hyperslab_5(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(in), dimension(:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_hyperslab_5)
  call hdf5_write_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_complex_hyperslab_5)

end subroutine hdf5_write_complex_hyperslab_5

!> write complex array to/from an HDF5 file.
subroutine hdf5_write_complex_array_6(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(in), dimension(:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_array_6)
  call hdf5_write_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_complex_array_6)

end subroutine hdf5_write_complex_array_6

!> write section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_write_complex_hyperslab_6(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(in), dimension(:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_hyperslab_6)
  call hdf5_write_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_complex_hyperslab_6)

end subroutine hdf5_write_complex_hyperslab_6

!> write complex array to/from an HDF5 file.
subroutine hdf5_write_complex_array_7(loc_id, dset_name, dims, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  integer, intent(in), dimension(:) :: dims !< size of the buffer buf
  complex(DPC), intent(in), dimension(:,:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_array_7)
  call hdf5_write_complex_array(loc_id, dset_name, dims, buf, error=error)
  POP_SUB(hdf5_write_complex_array_7)

end subroutine hdf5_write_complex_array_7

!> write section (hyperslab) of complex array to/from an HDF5 file.
subroutine hdf5_write_complex_hyperslab_7(loc_id, dset_name, read_count, offset, buf, error)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(len=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !> This does not need to be the same as the size of the dataset.
  integer, intent(in) :: read_count(:)
  !> Offset when reading dataset from file.
  integer, intent(in) :: offset(:)
  complex(DPC), intent(in), dimension(:,:,:,:,:,:,:) :: buf !< data buffer
  integer, intent(out), optional :: error !< error code
  
  PUSH_SUB(hdf5_write_complex_hyperslab_7)
  call hdf5_write_complex_hyperslab(loc_id, dset_name, read_count, offset, buf, error=error)
  POP_SUB(hdf5_write_complex_hyperslab_7)

end subroutine hdf5_write_complex_hyperslab_7

!############################################################
!# End of intermediate routines for ASSUMED-SHAPE arguments #
!############################################################

!###############################################
!# LOW LEVEL routines -- INT + DOUBLE versions #
!###############################################


!> read int to/from an HDF5 file.
subroutine hdf5_read_int_lowlevel(loc_id, dset_name, countf, buf, error, offsetf)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !! Pass (/-1/) to read/write a scalar rank-0 array.
  integer, intent(in) :: countf(:)
  !> Data buffer. We treat it as a flat contiguous 1D array.
  integer, intent(inout), dimension(*) :: buf
  integer, intent(out), optional :: error !< error code
  !> Offset when reading dataset from file.
  integer, intent(in), optional :: offsetf(:)
  
  integer(HSIZE_T) :: hcountf(size(countf)) !< Count for file dataspace
  integer(HSIZE_T) :: hcountm(1) !< Count for memory dataspace
  integer(HSIZE_T) :: hoffsetf(size(countf)) !< Offset for file filespace_id
  integer(HID_T) :: dset_id, filespace_id, memspace_id
  integer :: errcode, rank
  logical :: exists

  PUSH_SUB(hdf5_read_int_lowlevel)

  if (present(error)) error = 0
  call h5lexists_f(loc_id, dset_name, exists, errcode)
  if (check_and_exit('h5lexists_f', .not.exists)) return

  ! FHJ: Create or get file dataspace
  if (present(offsetf)) then
    hoffsetf(:) = offsetf(:)
  else
    hoffsetf(:) = 0
  endif
  hcountf(:) = countf(:)
  rank = size(hcountf)
  if (any(countf<1)) then
    rank = 0
    ! Note: hcountf and hoffsetf are not referenced if rank==0
  endif

  if (.not.exists) then
    ! FHJ: Create file dataspace
    call h5screate_simple_f(rank, hcountf, filespace_id, errcode)
    if (check_and_exit('h5screate_simple_f')) return
    ! FHJ: Create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_INTEGER, filespace_id, dset_id, errcode)
    if (check_and_exit('h5dcreate_f')) return
  else
    ! FHJ: Open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, errcode)
    if (check_and_exit('h5dopen_f')) return
    ! FHJ: Get file dataspace
    call h5dget_space_f(dset_id, filespace_id, errcode)
    if (check_and_exit('h5dget_space_f')) return
  endif

  ! FHJ: Select hyperslab from file
  if (rank>0) then
    call h5sselect_hyperslab_f(filespace_id, H5S_SELECT_SET_F, hoffsetf, hcountf, errcode)
    if (check_and_exit('h5sselect_hyperslab_f')) return
  endif

  ! FHJ: Create flat memory filespace_id
  hcountm(1) = max(1, product(countf))
  call h5screate_simple_f(1, hcountm, memspace_id, errcode)
  if (check_and_exit('h5screate_simple_f')) return

  ! FHJ: read filespace_id
  call h5dread_f(dset_id, H5T_NATIVE_INTEGER, buf, hcountm, errcode, &
    memspace_id, filespace_id)
  if (check_and_exit('h5dread_f')) return
  call h5sclose_f(memspace_id, errcode)
  call h5sclose_f(filespace_id, errcode)
  call h5dclose_f(dset_id, errcode)

  POP_SUB(hdf5_read_int_lowlevel)

  contains 

    logical function check_and_exit(err_routine, extra_trigger)
      character(len=*), intent(in) :: err_routine
      logical, intent(in), optional :: extra_trigger

      logical :: extra_trigger_, should_die
      character(len=256) :: err_str
  
      check_and_exit = .false.
      extra_trigger_ = .false.
      if (present(extra_trigger)) extra_trigger_ = extra_trigger
      ! If there was an error and the user did not specify the optional
      ! argument "error", then die.
      if (extra_trigger_.or.(errcode/=0)) then
      check_and_exit = .false.
        check_and_exit = .true.
        if (present(error)) then
          if (errcode/=0) then
            error = errcode
          else
            error = -1
          endif
        else
          write(err_str, '(3a,i0)') 'inside hdf5_read_int_lowlevel, hdf5 routine ', &
            trim(err_routine), ' returned error code ', errcode
          call die(err_str, only_root_writes=.true.)
        endif
      endif

    end function check_and_exit

end subroutine hdf5_read_int_lowlevel


!> write int to/from an HDF5 file.
subroutine hdf5_write_int_lowlevel(loc_id, dset_name, countf, buf, error, offsetf)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !! Pass (/-1/) to read/write a scalar rank-0 array.
  integer, intent(in) :: countf(:)
  !> Data buffer. We treat it as a flat contiguous 1D array.
  integer, intent(in), dimension(*) :: buf
  integer, intent(out), optional :: error !< error code
  !> Offset when reading dataset from file.
  integer, intent(in), optional :: offsetf(:)
  
  integer(HSIZE_T) :: hcountf(size(countf)) !< Count for file dataspace
  integer(HSIZE_T) :: hcountm(1) !< Count for memory dataspace
  integer(HSIZE_T) :: hoffsetf(size(countf)) !< Offset for file filespace_id
  integer(HID_T) :: dset_id, filespace_id, memspace_id
  integer :: errcode, rank
  logical :: exists

  PUSH_SUB(hdf5_write_int_lowlevel)

  if (present(error)) error = 0
  call h5lexists_f(loc_id, dset_name, exists, errcode)
  if (.not.exists .and. present(offsetf)) then
    ! FHJ: The developer should manually create the dataset, because countf
    ! is *NOT* the total size of the dataset, so we don`t have enough into to
    ! create the dataset.
    call die('Internal error: cannot automatically create dataset with "offsetf" argument.', &
      only_root_writes=.true.)
  endif

  ! FHJ: Create or get file dataspace
  if (present(offsetf)) then
    hoffsetf(:) = offsetf(:)
  else
    hoffsetf(:) = 0
  endif
  hcountf(:) = countf(:)
  rank = size(hcountf)
  if (any(countf<1)) then
    rank = 0
    ! Note: hcountf and hoffsetf are not referenced if rank==0
  endif

  if (.not.exists) then
    ! FHJ: Create file dataspace
    call h5screate_simple_f(rank, hcountf, filespace_id, errcode)
    if (check_and_exit('h5screate_simple_f')) return
    ! FHJ: Create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_INTEGER, filespace_id, dset_id, errcode)
    if (check_and_exit('h5dcreate_f')) return
  else
    ! FHJ: Open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, errcode)
    if (check_and_exit('h5dopen_f')) return
    ! FHJ: Get file dataspace
    call h5dget_space_f(dset_id, filespace_id, errcode)
    if (check_and_exit('h5dget_space_f')) return
  endif

  ! FHJ: Select hyperslab from file
  if (rank>0) then
    call h5sselect_hyperslab_f(filespace_id, H5S_SELECT_SET_F, hoffsetf, hcountf, errcode)
    if (check_and_exit('h5sselect_hyperslab_f')) return
  endif

  ! FHJ: Create flat memory filespace_id
  hcountm(1) = max(1, product(countf))
  call h5screate_simple_f(1, hcountm, memspace_id, errcode)
  if (check_and_exit('h5screate_simple_f')) return

  ! FHJ: write filespace_id
  call h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, buf, hcountm, errcode, &
    memspace_id, filespace_id)
  if (check_and_exit('h5dwrite_f')) return
  call h5sclose_f(memspace_id, errcode)
  call h5sclose_f(filespace_id, errcode)
  call h5dclose_f(dset_id, errcode)

  POP_SUB(hdf5_write_int_lowlevel)

  contains 

    logical function check_and_exit(err_routine, extra_trigger)
      character(len=*), intent(in) :: err_routine
      logical, intent(in), optional :: extra_trigger

      logical :: extra_trigger_, should_die
      character(len=256) :: err_str
  
      check_and_exit = .false.
      extra_trigger_ = .false.
      if (present(extra_trigger)) extra_trigger_ = extra_trigger
      ! If there was an error and the user did not specify the optional
      ! argument "error", then die.
      if (extra_trigger_.or.(errcode/=0)) then
      check_and_exit = .false.
        check_and_exit = .true.
        if (present(error)) then
          if (errcode/=0) then
            error = errcode
          else
            error = -1
          endif
        else
          write(err_str, '(3a,i0)') 'inside hdf5_write_int_lowlevel, hdf5 routine ', &
            trim(err_routine), ' returned error code ', errcode
          call die(err_str, only_root_writes=.true.)
        endif
      endif

    end function check_and_exit

end subroutine hdf5_write_int_lowlevel


!> read double to/from an HDF5 file.
subroutine hdf5_read_double_lowlevel(loc_id, dset_name, countf, buf, error, offsetf)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !! Pass (/-1/) to read/write a scalar rank-0 array.
  integer, intent(in) :: countf(:)
  !> Data buffer. We treat it as a flat contiguous 1D array.
  real(DP), intent(inout), dimension(*) :: buf
  integer, intent(out), optional :: error !< error code
  !> Offset when reading dataset from file.
  integer, intent(in), optional :: offsetf(:)
  
  integer(HSIZE_T) :: hcountf(size(countf)) !< Count for file dataspace
  integer(HSIZE_T) :: hcountm(1) !< Count for memory dataspace
  integer(HSIZE_T) :: hoffsetf(size(countf)) !< Offset for file filespace_id
  integer(HID_T) :: dset_id, filespace_id, memspace_id
  integer :: errcode, rank
  logical :: exists

  PUSH_SUB(hdf5_read_double_lowlevel)

  if (present(error)) error = 0
  call h5lexists_f(loc_id, dset_name, exists, errcode)
  if (check_and_exit('h5lexists_f', .not.exists)) return

  ! FHJ: Create or get file dataspace
  if (present(offsetf)) then
    hoffsetf(:) = offsetf(:)
  else
    hoffsetf(:) = 0
  endif
  hcountf(:) = countf(:)
  rank = size(hcountf)
  if (any(countf<1)) then
    rank = 0
    ! Note: hcountf and hoffsetf are not referenced if rank==0
  endif

  if (.not.exists) then
    ! FHJ: Create file dataspace
    call h5screate_simple_f(rank, hcountf, filespace_id, errcode)
    if (check_and_exit('h5screate_simple_f')) return
    ! FHJ: Create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_DOUBLE, filespace_id, dset_id, errcode)
    if (check_and_exit('h5dcreate_f')) return
  else
    ! FHJ: Open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, errcode)
    if (check_and_exit('h5dopen_f')) return
    ! FHJ: Get file dataspace
    call h5dget_space_f(dset_id, filespace_id, errcode)
    if (check_and_exit('h5dget_space_f')) return
  endif

  ! FHJ: Select hyperslab from file
  if (rank>0) then
    call h5sselect_hyperslab_f(filespace_id, H5S_SELECT_SET_F, hoffsetf, hcountf, errcode)
    if (check_and_exit('h5sselect_hyperslab_f')) return
  endif

  ! FHJ: Create flat memory filespace_id
  hcountm(1) = max(1, product(countf))
  call h5screate_simple_f(1, hcountm, memspace_id, errcode)
  if (check_and_exit('h5screate_simple_f')) return

  ! FHJ: read filespace_id
  call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, buf, hcountm, errcode, &
    memspace_id, filespace_id)
  if (check_and_exit('h5dread_f')) return
  call h5sclose_f(memspace_id, errcode)
  call h5sclose_f(filespace_id, errcode)
  call h5dclose_f(dset_id, errcode)

  POP_SUB(hdf5_read_double_lowlevel)

  contains 

    logical function check_and_exit(err_routine, extra_trigger)
      character(len=*), intent(in) :: err_routine
      logical, intent(in), optional :: extra_trigger

      logical :: extra_trigger_, should_die
      character(len=256) :: err_str
  
      check_and_exit = .false.
      extra_trigger_ = .false.
      if (present(extra_trigger)) extra_trigger_ = extra_trigger
      ! If there was an error and the user did not specify the optional
      ! argument "error", then die.
      if (extra_trigger_.or.(errcode/=0)) then
      check_and_exit = .false.
        check_and_exit = .true.
        if (present(error)) then
          if (errcode/=0) then
            error = errcode
          else
            error = -1
          endif
        else
          write(err_str, '(3a,i0)') 'inside hdf5_read_double_lowlevel, hdf5 routine ', &
            trim(err_routine), ' returned error code ', errcode
          call die(err_str, only_root_writes=.true.)
        endif
      endif

    end function check_and_exit

end subroutine hdf5_read_double_lowlevel


!> write double to/from an HDF5 file.
subroutine hdf5_write_double_lowlevel(loc_id, dset_name, countf, buf, error, offsetf)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !! Pass (/-1/) to read/write a scalar rank-0 array.
  integer, intent(in) :: countf(:)
  !> Data buffer. We treat it as a flat contiguous 1D array.
  real(DP), intent(in), dimension(*) :: buf
  integer, intent(out), optional :: error !< error code
  !> Offset when reading dataset from file.
  integer, intent(in), optional :: offsetf(:)
  
  integer(HSIZE_T) :: hcountf(size(countf)) !< Count for file dataspace
  integer(HSIZE_T) :: hcountm(1) !< Count for memory dataspace
  integer(HSIZE_T) :: hoffsetf(size(countf)) !< Offset for file filespace_id
  integer(HID_T) :: dset_id, filespace_id, memspace_id
  integer :: errcode, rank
  logical :: exists

  PUSH_SUB(hdf5_write_double_lowlevel)

  if (present(error)) error = 0
  call h5lexists_f(loc_id, dset_name, exists, errcode)
  if (.not.exists .and. present(offsetf)) then
    ! FHJ: The developer should manually create the dataset, because countf
    ! is *NOT* the total size of the dataset, so we don`t have enough into to
    ! create the dataset.
    call die('Internal error: cannot automatically create dataset with "offsetf" argument.', &
      only_root_writes=.true.)
  endif

  ! FHJ: Create or get file dataspace
  if (present(offsetf)) then
    hoffsetf(:) = offsetf(:)
  else
    hoffsetf(:) = 0
  endif
  hcountf(:) = countf(:)
  rank = size(hcountf)
  if (any(countf<1)) then
    rank = 0
    ! Note: hcountf and hoffsetf are not referenced if rank==0
  endif

  if (.not.exists) then
    ! FHJ: Create file dataspace
    call h5screate_simple_f(rank, hcountf, filespace_id, errcode)
    if (check_and_exit('h5screate_simple_f')) return
    ! FHJ: Create dataset
    call h5dcreate_f(loc_id, dset_name, H5T_NATIVE_DOUBLE, filespace_id, dset_id, errcode)
    if (check_and_exit('h5dcreate_f')) return
  else
    ! FHJ: Open dataset
    call h5dopen_f(loc_id, dset_name, dset_id, errcode)
    if (check_and_exit('h5dopen_f')) return
    ! FHJ: Get file dataspace
    call h5dget_space_f(dset_id, filespace_id, errcode)
    if (check_and_exit('h5dget_space_f')) return
  endif

  ! FHJ: Select hyperslab from file
  if (rank>0) then
    call h5sselect_hyperslab_f(filespace_id, H5S_SELECT_SET_F, hoffsetf, hcountf, errcode)
    if (check_and_exit('h5sselect_hyperslab_f')) return
  endif

  ! FHJ: Create flat memory filespace_id
  hcountm(1) = max(1, product(countf))
  call h5screate_simple_f(1, hcountm, memspace_id, errcode)
  if (check_and_exit('h5screate_simple_f')) return

  ! FHJ: write filespace_id
  call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, buf, hcountm, errcode, &
    memspace_id, filespace_id)
  if (check_and_exit('h5dwrite_f')) return
  call h5sclose_f(memspace_id, errcode)
  call h5sclose_f(filespace_id, errcode)
  call h5dclose_f(dset_id, errcode)

  POP_SUB(hdf5_write_double_lowlevel)

  contains 

    logical function check_and_exit(err_routine, extra_trigger)
      character(len=*), intent(in) :: err_routine
      logical, intent(in), optional :: extra_trigger

      logical :: extra_trigger_, should_die
      character(len=256) :: err_str
  
      check_and_exit = .false.
      extra_trigger_ = .false.
      if (present(extra_trigger)) extra_trigger_ = extra_trigger
      ! If there was an error and the user did not specify the optional
      ! argument "error", then die.
      if (extra_trigger_.or.(errcode/=0)) then
      check_and_exit = .false.
        check_and_exit = .true.
        if (present(error)) then
          if (errcode/=0) then
            error = errcode
          else
            error = -1
          endif
        else
          write(err_str, '(3a,i0)') 'inside hdf5_write_double_lowlevel, hdf5 routine ', &
            trim(err_routine), ' returned error code ', errcode
          call die(err_str, only_root_writes=.true.)
        endif
      endif

    end function check_and_exit

end subroutine hdf5_write_double_lowlevel



!########################################
!# LOW LEVEL routine -- LOGICAL version #
!########################################


!> read logical to/from an HDF5 file.
!! Note that this just maps the logical data to integers, and calls hdf5_read_int_lowlevel.
subroutine hdf5_read_logical_lowlevel(loc_id, dset_name, countf, buf, error, offsetf)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !! Pass (/-1/) to read/write a scalar rank-0 array.
  integer, intent(in) :: countf(:)
  !> Data buffer. We treat it as a flat contiguous 1D array.
  logical, intent(inout), dimension(*) :: buf
  integer, intent(out), optional :: error !< error code
  !> Offset when reading dataset from file.
  integer, intent(in), optional :: offsetf(:)
 
  integer :: buf_int(max(1, product(countf))), sz

  PUSH_SUB(hdf5_read_logical_lowlevel)

  sz = max(1, product(countf))
  call hdf5_read_int_lowlevel(loc_id, dset_name, countf, buf_int, error=error, offsetf=offsetf)
  buf(1:sz) = buf_int(:) /= 0

  POP_SUB(hdf5_read_logical_lowlevel)

end subroutine hdf5_read_logical_lowlevel

!> write logical to/from an HDF5 file.
!! Note that this just maps the logical data to integers, and calls hdf5_write_int_lowlevel.
subroutine hdf5_write_logical_lowlevel(loc_id, dset_name, countf, buf, error, offsetf)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !! Pass (/-1/) to read/write a scalar rank-0 array.
  integer, intent(in) :: countf(:)
  !> Data buffer. We treat it as a flat contiguous 1D array.
  logical, intent(in), dimension(*) :: buf
  integer, intent(out), optional :: error !< error code
  !> Offset when reading dataset from file.
  integer, intent(in), optional :: offsetf(:)
 
  integer :: buf_int(max(1, product(countf))), sz

  PUSH_SUB(hdf5_write_logical_lowlevel)

  sz = max(1, product(countf))
  where(buf(1:sz))
    buf_int = 1
  elsewhere
    buf_int = 0
  endwhere
  call hdf5_write_int_lowlevel(loc_id, dset_name, countf, buf_int, error=error, offsetf=offsetf)

  POP_SUB(hdf5_write_logical_lowlevel)

end subroutine hdf5_write_logical_lowlevel




!> read complex to/from an HDF5 file.
!! Note that this just maps the complex data to doubles, and calls hdf5_read_double_lowlevel.
subroutine hdf5_read_complex_lowlevel(loc_id, dset_name, countf, buf, error, offsetf)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !! Pass (/-1/) to read/write a scalar rank-0 array.
  integer, intent(in) :: countf(:)
  !> Data buffer. We treat it as a flat contiguous 1D array.
  complex(DPC), intent(inout), dimension(*), target :: buf
  integer, intent(out), optional :: error !< error code
  !> Offset when reading dataset from file.
  integer, intent(in), optional :: offsetf(:)
 
  integer :: rank_double, size_double
  integer :: countf_double(size(countf)+1)
  integer :: offsetf_double(size(countf)+1)
  real(DP), pointer :: buf_double(:)

  PUSH_SUB(hdf5_read_complex_lowlevel)

  ! We map scalars and arrays to arrays, by adding extra dimension with size=2.
  ! Scalar is trickier, because size(countf) is meaningless.
  countf_double(1) = 2
  countf_double(2:) = countf
  offsetf_double(:) = 0
  if (any(countf<1)) then
    rank_double = 1
    size_double = 2
  else
    rank_double = 1 + size(countf)
    size_double = 2*product(countf)
  endif
  call c_f_pointer(c_loc(buf), buf_double, [size_double])
  if (present(offsetf)) then
    offsetf_double(2:) = offsetf
    call hdf5_read_double_lowlevel(loc_id, dset_name, countf_double(1:rank_double), &
      buf_double, error=error, offsetf=offsetf_double(1:rank_double))
  else
    call hdf5_read_double_lowlevel(loc_id, dset_name, countf_double(1:rank_double), &
      buf_double, error=error)
  endif

  POP_SUB(hdf5_read_complex_lowlevel)

end subroutine hdf5_read_complex_lowlevel

!> write complex to/from an HDF5 file.
!! Note that this just maps the complex data to doubles, and calls hdf5_write_double_lowlevel.
subroutine hdf5_write_complex_lowlevel(loc_id, dset_name, countf, buf, error, offsetf)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  !> Number of elements to read from the dataset for each dimention.
  !! Pass (/-1/) to read/write a scalar rank-0 array.
  integer, intent(in) :: countf(:)
  !> Data buffer. We treat it as a flat contiguous 1D array.
  complex(DPC), intent(in), dimension(*), target :: buf
  integer, intent(out), optional :: error !< error code
  !> Offset when reading dataset from file.
  integer, intent(in), optional :: offsetf(:)
 
  integer :: rank_double, size_double
  integer :: countf_double(size(countf)+1)
  integer :: offsetf_double(size(countf)+1)
  real(DP), pointer :: buf_double(:)

  PUSH_SUB(hdf5_write_complex_lowlevel)

  ! We map scalars and arrays to arrays, by adding extra dimension with size=2.
  ! Scalar is trickier, because size(countf) is meaningless.
  countf_double(1) = 2
  countf_double(2:) = countf
  offsetf_double(:) = 0
  if (any(countf<1)) then
    rank_double = 1
    size_double = 2
  else
    rank_double = 1 + size(countf)
    size_double = 2*product(countf)
  endif
  call c_f_pointer(c_loc(buf), buf_double, [size_double])
  if (present(offsetf)) then
    offsetf_double(2:) = offsetf
    call hdf5_write_double_lowlevel(loc_id, dset_name, countf_double(1:rank_double), &
      buf_double, error=error, offsetf=offsetf_double(1:rank_double))
  else
    call hdf5_write_double_lowlevel(loc_id, dset_name, countf_double(1:rank_double), &
      buf_double, error=error)
  endif

  POP_SUB(hdf5_write_complex_lowlevel)

end subroutine hdf5_write_complex_lowlevel

#endif
end module hdf5_io_m
!! Local Variables:
!! mode: f90
!! coding: utf-8
!! End:

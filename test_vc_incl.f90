program test_vc_incl

  integer, dimension(4,2) :: incl_array
  integer :: nvalence = 8
  integer, allocatable :: incl_array_v(:,:)
  integer, allocatable :: incl_array_c(:,:)

  integer :: vcount = 0
  integer :: j = 0
  integer :: nrows
  integer :: find_v, k
  integer :: crows

  incl_array(1,1) = 1
  incl_array(1,2) = 1
  incl_array(2,1) = 3
  incl_array(2,2) = 4
  incl_array(3,1) = 6
  incl_array(3,2) = 8
  incl_array(4,1) = 11
  incl_array(4,2) = 12
   
  nrows = size(incl_array, 1)

  do while (vcount .lt. nvalence)
    j = j + 1
    vcount = vcount + incl_array(j, 2) - incl_array(j, 1) + 1
  end do

  write(*,*) "nvalence: ", nvalence
  
  crows = nrows - j
  find_v = incl_array(j, 2)

  if (vcount .eq. nvalence) then
    allocate(incl_array_c(crows, 2))
    allocate(incl_array_v(j, 2))
    incl_array_v = incl_array(1:j, :)
    incl_array_c = incl_array(j+1:nrows, :)
  else 
    do while (vcount .ne. nvalence)
      vcount = vcount - 1
      find_v = find_v - 1
    end do
    allocate(incl_array_v(j,2))
    allocate(incl_array_c(crows+1 , 2))

    write(*,*) "find_v: ", find_v
    write(*,*) "vcount: ", vcount

    incl_array_v = incl_array(1:j, :)
    incl_array_c = incl_array(j:nrows, :)
    incl_array_v(j, 2) = find_v 
    incl_array_c(1, 1) = find_v + 1
  end if

  write(*,*) "incl_array_v: "
  do i = 1, j
    write(*,*) (incl_array_v(i, k), k = 1, 2)
  end do

  write(*,*) "incl_array_c: "
  write(*,*)
  do i = 1, size(incl_array_c, 1)
    write(*,*) (incl_array_c(i, k), k = 1,2)
  end do



end program test_vc_incl

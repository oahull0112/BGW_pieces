program test_vc_incl

  ! test:
  ! nval = 2 not working, but nval = 4 work
  ! nval = 5 not working, gives same result as nval = 4
  ! nval = 7 not working

  ! evens: 1, 3, 6, 8
  ! odds: 2, 4, 5, 7

  ! working now: 4, 2, 5, 7

  integer, dimension(4,2) :: incl_array
  integer :: nvalence = 7
  integer :: nconduction = 8 ! nval = 2, not working
  integer, allocatable :: incl_array_v(:,:)
  integer, allocatable :: incl_array_c(:,:)

  integer :: vcount = 0
  integer :: j = 0
  integer :: nrows
  integer :: find_v, k
  integer :: ccount

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

  write(*,*) "j = ", j," and vcount = ", vcount
  
  ccount = nrows - j
  find_v = incl_array(j, 2)

  if (vcount .eq. nvalence) then
    allocate(incl_array_c(ccount, 2))
    allocate(incl_array_v(j, 2))
    incl_array_v = incl_array(1:j, :)
    incl_array_c = incl_array(j+1:nrows, :)
  else 
    do while (vcount .ne. nvalence)
      vcount = vcount - 1
      find_v = find_v - 1
    end do
    allocate(incl_array_v(j,2))
    allocate(incl_array_c(ccount+1 , 2))

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

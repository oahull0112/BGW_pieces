program make_read_ranges

  implicit none

  integer, allocatable :: incl_array(:,:)
  integer, allocatable :: read_ranges_incl(:,:)

  allocate(incl_array(4,2))
  
  incl_array(1,1) = 1
  incl_array(1,2) = 5
  incl_array(2,1) = 7
  incl_array(2,2) = 11
  incl_array(3,1) = 13
  incl_array(3,2) = 15
  incl_array(4,1) = 17
  incl_array(4,2) = 25

  call make_read_range_incl(incl_array, read_ranges_incl)

  contains

  subroutine make_read_range_incl(inc_array, read_ranges_incl)

    integer, intent(in) :: inc_array(:,:)
    integer, allocatable, intent(out) :: read_ranges_incl(:,:)
    integer, allocatable :: incl_array(:,:)
    integer, allocatable :: incl_array2(:,:)
    integer :: max_number_bands ! max bands to read in at once

    integer :: cbn ! current band number
    integer :: i, j, k, nrows
    integer :: nb_inchunk

    nrows = size(inc_array, 1)

    max_number_bands=5
    incl_array = inc_array 
    incl_array2 = inc_array
    i=1
    j=0 ! index rows of read_ranges_incl
    k=0 ! index the third column of read_ranges_incl
    nb_inchunk = max_number_bands

    do while (i .le. nrows)
      cbn=incl_array(i,2)-incl_array(i,1)+1
      if (cbn .lt. nb_inchunk) then
        nb_inchunk=nb_inchunk-cbn
        i=i+1
        j=j+1
      else if (cbn .gt. nb_inchunk) then
        j=j+1
        k=k+1
        incl_array(i,1)=incl_array(i,1)+nb_inchunk
        nb_inchunk=max_number_bands
      else ! cbn .eq. nb_inchunk
        j=j+1
        nb_inchunk=max_number_bands
        k=k+1
        i=i+1
      end if
    end do

    write(*,*) "rows to allocate: ",j

    allocate(read_ranges_incl(j, 3))

    ! Now, go back through and fill in the values

    do i=1,size(incl_array2,2)
      write(*,*) (incl_array2(i,j), j=1,2)
    end do

    i=1
    j=1 ! index rows of read_ranges_incl
    k=0 ! index the third column of read_ranges_incl
    nb_inchunk = max_number_bands

    do while (i .le. nrows)
      cbn=incl_array2(i,2)-incl_array2(i,1)+1
      if (cbn .lt. nb_inchunk) then
        ! then copy the current row over:
        read_ranges_incl(j,1)=incl_array2(i,1)
        read_ranges_incl(j,2)=incl_array2(i,2)
        read_ranges_incl(j,3)=k
        nb_inchunk = nb_inchunk-cbn
        i=i+1
        j=j+1
      else if (cbn .gt. nb_inchunk) then
        read_ranges_incl(j,1)=incl_array2(i,1)
        read_ranges_incl(j,2)=incl_array2(i,1)+nb_inchunk-1
        read_ranges_incl(j,3)=k
        j=j+1
        k=k+1
        incl_array2(i,1)=incl_array2(i,1)+nb_inchunk
        nb_inchunk=max_number_bands
      else ! cbn .eq. nb_inchunk
        read_ranges_incl(j,1)=incl_array2(i,1)
        read_ranges_incl(j,2)=incl_array2(i,2)
        read_ranges_incl(j,3)=k
        j=j+1
        nb_inchunk=max_number_bands
        k=k+1
        i=i+1
      end if
    end do

    write(*,*)
    write(*,*)"read_ranges_incl: "
    do i=1,size(read_ranges_incl,1)
      write(*,*) (read_ranges_incl(i,j), j=1,3)
    end do


  end subroutine make_read_range_incl

end program make_read_ranges

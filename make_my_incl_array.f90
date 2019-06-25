program make_my_incl_array

  ! In an actual subroutine,
  ! INTENT IN incl_array
  ! INTENT IN do_i_own
  ! INTENT OUT my_incl_array

  implicit none

  integer, dimension(3,2) :: incl_array
  integer, allocatable :: my_incl_array(:, :)
  logical, allocatable :: do_i_own(:)

  integer :: nrows, ncols
  integer :: my_count = 0 ! for allocating my_incl_array
  integer :: irow ! row indexer of my_incl_array
  integer :: next_row
  integer :: i_ia = 1 ! "(i)ndex of (i)nclusion (a)rray" 
  integer :: i, j, k
  integer :: init = 0
  
  ! Define a dummy inclusion array
  incl_array(1,1) = 2
  incl_array(1,2) = 2
  incl_array(2,1) = 4
  incl_array(2,2) = 6
  incl_array(3,1) = 8
  incl_array(3,2) = 9
  ! rows included: 2, 4, 5, 6, 8, 9

  ! Allocation will actually be done in distribution()
  ! And in here we will just need size(do_i_own)
  allocate(do_i_own(6))

  ! rows for this task: 2, 4, 5, 9
  do_i_own = (/ .false., .true., .false., .true., .true., .true./)

  nrows = SIZE(incl_array, 1) ! first dim of incl_array
  ncols = 2 ! Fixed by def of inclusion array

  ! We waste some memory by pre-allocating my_incl_array to
  ! the size of incl_array, but this is preferable to trying
  ! to figure out the exact size of my_incl_array
  ! because it saves us some computation*, and we don't expect
  ! incl_array to be horrifically large in the first place

  call find_max_size() ! find the worst case scenario size
  allocate(my_incl_array(my_count, ncols)) ! allocate worst case scenario
  my_incl_array = -1 ! -1 if row is extra

  ! main routine:

  next_row = incl_array(1,1)
  i_ia = 1
  irow = 2
  do i = 1, size(do_i_own)

    ! print out current my_incl_array
    do k = 1, my_count
      write(*,*) (my_incl_array(k,j), j = 1, ncols)
    end do
    write(*,*) " "

    if (do_i_own(i)) then
      if ( init .eq. 0) then
        ! if I am the first addition to my_incl_array, then just add me with
        ! no further logic:
        my_incl_array(1, 1) = next_row
        my_incl_array(1, 2) = next_row
        init = init + 1
      else
        ! Must include i in my_incl_array, but have to figure out if i
        ! needs its own row, or should be added to the end of the current row
        call add_row_to_my_incl_array()
      end if ! init .eq. 0
    end if ! do_i_own(i)
    if ( i .ne. size(do_i_own)) then
      call get_next_row()
    end if ! i .ne. size(do_i_own)
  end do


  do i = 1, my_count
    write(*,*) (my_incl_array(i, j), j = 1, ncols)
  end do

  contains

    subroutine add_row_to_my_incl_array()

      integer :: current_last_incl_row

      ! grab the bottom right value in my_incl_array
      current_last_incl_row = my_incl_array(irow - 1, 2)
      ! if the next value is adjacent, then just increment the bottom right
      ! value to the next row.
      if ( next_row .eq. current_last_incl_row + 1 ) then
        my_incl_array(irow - 1, 2) = my_incl_array(irow - 1, 2) + 1
!        my_incl_array(irow, 2) = my_incl_array(irow, 2) + 1
      else ! if not adjacent, then the band is not in the current cluster, so
        ! start a new one 
        irow = irow + 1
        my_incl_array(irow - 1, 1) = next_row
        my_incl_array(irow - 1, 2) = next_row
      end if ! (next_row...)

    end subroutine add_row_to_my_incl_array

    subroutine get_next_row()
      ! need to figure out if the next band to look at is inside the current
      ! range or not. If the ending value in the current range is greater than
      ! the enxt row, then increment the row but keep the inclusion array
      ! index the same, because we're still inside the same cluster. Otherwise,
      ! we're outside the current cluster and need to move to the next
      ! inclusion_array row, so increment i_ia. 
      if (incl_array(i_ia, 2) .gt. next_row) then
        next_row = next_row + 1
      else
        next_row = incl_array(i_ia+1, 1)
       i_ia = i_ia + 1
     end if ! incl_array(i_ia) ... 
    end subroutine get_next_row

    subroutine find_max_size()

      do i = 1, nrows
        my_count = my_count + incl_array(i, 2) - incl_array(i, 1) + 1
      end do
      write(*,*) "my count: ", my_count
      
    end subroutine find_max_size

end program make_my_incl_array

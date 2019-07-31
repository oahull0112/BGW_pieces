program determine_n_incl

  implicit none

  integer, allocatable :: incl_array(:,:)
  integer, allocatable :: incl_array_v(:,:)
  integer, allocatable :: incl_array_c(:,:)
  integer :: cwfn_nband
  integer :: vwfn_nband
  integer :: pol_ncrit
  integer :: n_excl, nv_excl, ncrit_excl

  allocate(incl_array(2,2))

  incl_array(1,1) = 3
  incl_array(1,2) = 6
  incl_array(2,1) = 9
  incl_array(2,2) = 9

  ! 1-4: valence, 5-6: crit, 7-10: conduction
  ! nv_incl: 4, nc_incl: 4, ncrit_incl: 0
  cwfn_nband = 29
  vwfn_nband = 5
  pol_ncrit = 3

  call determine_ns(incl_array, cwfn_nband, vwfn_nband, &
    pol_ncrit, n_excl, nv_excl, ncrit_excl)

  call make_vc_incl_array(incl_array, vwfn_nband+pol_ncrit,&
  cwfn_nband-vwfn_nband, pol_ncrit, incl_array_v, incl_array_c)

  contains

  subroutine determine_ns(incl_array, ntot, nv_tot, ncrit, n_excl, &
      nv_excl, ncrit_excl)
  
    integer, intent(in) :: incl_array(:,:)
    integer, intent(inout) :: ntot ! cwfn_nband
    integer, intent(inout) :: nv_tot ! vwfn_nband
    integer, intent(inout) :: ncrit ! pol_ncrit
    integer, intent(out) :: n_excl ! total number of excluded bands
    integer, intent(out) :: nv_excl ! number of excluded valence bands
    integer, intent(out) :: ncrit_excl ! number of excluded partially occupieds
  
    integer :: i, ir
    integer :: last_v, first_c, search_v, search_c
    integer :: n_incl_rows
    integer :: c_range
  
    integer :: nv_incl, nc_incl, ncrit_incl
    integer :: v_place, c_place ! the row number of the last v (c) band
    integer :: vr_place, cr_place ! place inside of row range
    integer :: start_ncrit, end_ncrit, last_ncrit, first_ncrit
    integer :: search_ncrit_above, search_ncrit_below
  
    n_incl_rows = size(incl_array, 1)
    last_v = nv_tot
    first_c = nv_tot + ncrit + 1
  
    search_v = 0
    search_c = 0
    vr_place = 0
    cr_place = 0
    nc_incl = 0
    nv_incl = 0
    ncrit_incl = 0
  
    do i = 1, n_incl_rows
      if (incl_array(i,2) .le. last_v .and. incl_array(i,1) .le. last_v) then
        ! then add in the whole row range:
        nv_incl = nv_incl + incl_array(i,2) - incl_array(i,1) + 1
        search_v = incl_array(i,2)
        vr_place = incl_array(i,2) - incl_array(i,1) + 1       
        ! The last v is somewhere in the middle of a row range:
      else if (incl_array(i,2) .ge. last_v .and. incl_array(i,1) .le. last_v) then
        search_v = incl_array(i,1)
        do while (search_v .le. last_v)
          nv_incl = nv_incl + 1
          search_v = search_v + 1
          vr_place = vr_place + 1
        end do
        search_v = search_v - 1
      else
        cycle
      end if
      v_place = i
    end do
  
    do i = 1, n_incl_rows
      ir = n_incl_rows + 1 - i
      if (incl_array(ir, 1) .ge. first_c .and. & 
        incl_array(ir, 2) .ge. first_c) then
        nc_incl = nc_incl + incl_array(ir, 2) - incl_array(ir, 1) + 1
        search_c = incl_array(ir,1)
        cr_place = 1
      else if (incl_array(ir,1) .le. first_c .and. &
        incl_array(ir,2) .ge. first_c) then
        search_c = incl_array(ir, 1)
        nc_incl = nc_incl + incl_array(ir,2) - incl_array(ir,1) + 1
        do while (search_c .lt. first_c)
          search_c = search_c + 1
          nc_incl = nc_incl - 1
          cr_place = cr_place + 1
        end do
        write(*,*) "cr_place before switch: ", cr_place
        write(*,*) "incl_array(ir,:): ", incl_array(ir,:)
        cr_place = cr_place + incl_array(ir,1)
!        cr_place = incl_array(i,2) - incl_array(i,1) + 2 - cr_place
        write(*,*) "cr_place switch: ", cr_place
      else
        cycle
      end if
      c_place = ir ! pretty sure we can move both c_place and v_place to outside
      ! the whole thing because it just needs the last (i.e. after end do, c_place
      ! = ir)
    end do
    
    write(*,*) " "

    if (ncrit .ne. 0) then
      start_ncrit = vr_place ! row to start looking
      end_ncrit = cr_place - 1
    !  last_ncrit = search_c - 1 ! the highest possible ncrit value
    !  first_ncrit = search_v + 1
      first_ncrit = nv_tot + 1
      last_ncrit = first_ncrit + ncrit - 1
      write(*,*) "first_ncrit: ", first_ncrit, " last_ncrit: ", last_ncrit
      write(*,*) "v_place: ", v_place, "c_place: ", c_place
      do i = v_place, c_place ! go through the in-between
         ! row contains no ncrits
         write(*,*) "incl_array: ", incl_array(i,:)
         if (incl_array(i,2) .lt. first_ncrit &
           .or. incl_array(i,1) .gt. last_ncrit) then
         write (*,*) "row contains no crits"
           cycle
         ! In-between range situation:
       !  else if (incl_array(i,2).le.last_ncrit &
       !    .and. incl_array(i,1) .ge. first_ncrit) then
       !  write (*,*) "in-between range executing"
       !    search_ncrit_below = incl_array(i,1)
       !    search_ncrit_above = incl_array(i,2)
       !    do while (search_ncrit_below .lt. first_ncrit)
       !      search_ncrit_below = search_ncrit_below + 1
       !    end do
       !    do while (search_ncrit_above .gt. last_ncrit)
       !      search_ncrit_above = search_ncrit_above - 1
       !    end do
       !    ! Don't need to add ncrit to itself here because if this situation
       !    ! occurs, then all ncrits were in this one row anyway
       !    ncrit_incl = search_ncrit_above - search_ncrit_below + 1
         ! Row only contains crits:
       else if (incl_array(i,2) .ge. last_ncrit &
         .and. incl_array(i,1) .le. first_ncrit) then
         write(*,*) "ncrits totally contained in one row"
         ncrit_incl = ncrit
         else if (incl_array(i,1) .ge. first_ncrit &
           .and. incl_array(i,2) .le. last_ncrit) then
         write (*,*) "row contains crits only executing"
           ncrit_incl = ncrit_incl + incl_array(i,2) - incl_array(i,1) + 1
         ! RHS situation:
         else if (incl_array(i,1).le.first_ncrit &
           .and. incl_array(i,2).le.last_ncrit) then
         write (*,*) "RHS executing"
           ncrit_incl = ncrit_incl + incl_array(i,2) - incl_array(i,1) & 
             - vr_place + 1 ! maybe +1?
         else if (incl_array(i,1) .ge. first_ncrit &
           .and. incl_array(i,2) .gt. last_ncrit) then
         write(*,*) " "
         write (*,*) "LHS executing"
         write(*,*) "ncrit_incl before:", ncrit_incl
         write(*,*) "incl_array(i,:): ", incl_array(i,:)
         write(*,*) "cr_place: ", cr_place
         write(*,*) " "
         
         c_range = incl_array(i,2) - cr_place + 1
         ncrit_incl = ncrit_incl + (incl_array(i,2) - incl_array(i,1) + 1) - c_range
!           ncrit_incl = ncrit_incl + incl_array(i,2) - incl_array(i,1) &
!             - cr_place + 1
         write(*,*) "ncrit_incl in LHS: ", ncrit_incl
         else
           cycle
         end if
     end do
    else
      ncrit_excl = 0
    end if
  
    write(*,*) " "
    write(*,*) "ncrit_incl: ", ncrit_incl
    write(*,*) "nv_incl: ", nv_incl
    write(*,*) "nc_incl: ", nc_incl
    ntot = nv_incl + nc_incl + ncrit_incl
    nv_tot = nv_incl
    ncrit = ncrit_incl
  end subroutine determine_ns

  subroutine make_vc_incl_array(incl_array, nvalence, nconduction, &
             ncrit, incl_array_v, incl_array_c)
           ! OAH 7/22: take ncrit as input, and adjust the nconduction in this
           ! way. Have whatever the first index of nconduction is be subtracted
           ! back by ncrit, which should give the proper starting value. And see
           ! if this works. May need to then adjust my_incl_array subroutine,
           ! but current problem may be that ncrit is not getting included in
           ! the conduction inclusion array, and therefore not read in properly.
    
    integer, intent(in) :: incl_array(:,:)
    integer, intent(in) :: nvalence ! nv_incl
    integer, intent(in) :: nconduction ! nc_incl
    integer, intent(in) :: ncrit
    integer, allocatable, intent(out) :: incl_array_v(:,:)
    integer, allocatable, intent(out) :: incl_array_c(:,:)

    integer :: vcount
    integer :: i, j, k
    integer :: nrows ! total number of rows in incl_array
    integer :: crows ! index of start of conduction band
    integer :: find_v
    integer :: find_c, ccount    
    write(*,*) " "
    write(*,*) "nvalence: ", nvalence
    write(*,*) "nconduction: ", nconduction
    write(*,*) "ncrit: ", ncrit

    vcount = 0
    j = 0
    nrows = size(incl_array, 1)

    ! Figure out the global incl_array_v, incl_array_c.
    ! First do while gets the index (j) of the row that contains both valence
    ! and conduction bands. The remaining logic splits the array accordingly.
    do while (vcount .lt. nvalence)
      j = j + 1
      vcount = vcount + incl_array(j,2) - incl_array(j, 1) + 1
    end do ! while
    crows = nrows - j
    find_v = incl_array(j, 2)
    write(*,*) "find_v: ", find_v
    ! If the last band in the range equals nvalence, then we take this row and
    ! all rows above it and assign to incl_array_v, and the rest to
    ! incl_array_c. This situation will occur if frontier bands are excluded.

    ! This is an issue because it does not allow for ncrit band endings...
   ! if (vcount .eq. nvalence) then
   !   allocate(incl_array_c(crows, 2))
   !   allocate(incl_array_v(j, 2))
   !   incl_array_v = incl_array(1:j, :)
   !   incl_array_c = incl_array(j+1:nrows, :)
   ! ! Otherwise, the "split" between v and c occurs somewhere inside a range of
   ! ! band values. We determine where the split occurs, then assign v, c
   ! ! incl_arrays accordingly.
   ! else
    do while (vcount .ne. nvalence)
      vcount = vcount - 1
      find_v = find_v - 1
    end do ! while
    allocate(incl_array_v(j, 2))
    !allocate(incl_array_c(crows+1, 2))
    incl_array_v = incl_array(1:j, :)
    !incl_array_c = incl_array(j:nrows, :)
    incl_array_v(j, 2) = find_v
   ! incl_array_c(1, 1) = find_v + 1
    !write(*,*) "iac: ", incl_array_c(1,:)
   ! end if ! vcount .eq. nvalence

    ! actually, dont change this here. Change it only in the wfn_io file...
    ! and compare and see if it gives the same thing
!    incl_array_c(1,1) = incl_array_c(1,1) - ncrit
!    write(*,*) "iac: ", incl_array_c(1,:)

    ! Ultimately need to do a c_count separately from v_count. This is where all
    ! the issues are stemming from, and can be avoided just by having two
    ! separate "look fors"

    write(*,*) " "
    ccount = 0
    j = size(incl_array, 1)
    k = 0
    do while (ccount .lt. nconduction) ! maybe + ncrit, need to check
      ccount= ccount + incl_array(j,2) - incl_array(j,1) + 1
      write(*,*) "j : ", j
      write(*,*) "nconduction: ", nconduction
      write(*,*) "ccount: ", ccount
      write(*,*) " "
      j = j-1
      k = k + 1
    end do
    j = j + 1
    find_c = incl_array(j,1)
      write(*,*) " "
      write(*,*) "find_c: ", find_c
      write(*,*) "ccount: ", ccount
    do while (ccount.ne.nconduction)
      find_c = find_c + 1
      ccount = ccount-1
      write(*,*) " "
      write(*,*) "find_c: ", find_c
      write(*,*) "ccount: ", ccount
    end do

    write(*,*) "find_c: ", find_c
    write(*,*) "ccount: ", ccount

    write(*,*) "k :", k
    write(*,*) "j :", j
    allocate(incl_array_c(k, 2))
    incl_array_c = incl_array(j:, :)
    incl_array_c(1,1) = find_c

    write(*,*) "incl_array_v:"
    do i = 1, size(incl_array_v,1)
      write(*,*) (incl_array_v(i,j), j=1,2)
    end do
    write(*,*)
    write(*,*) "incl_array_c:"
    do i = 1, size(incl_array_c,1)
      write(*,*) (incl_array_c(i,j), j=1,2)
    end do


  end subroutine make_vc_incl_array

end program determine_n_incl

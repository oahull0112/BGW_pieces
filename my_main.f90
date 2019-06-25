program my_main

  use HDF5
  use MPI

  implicit none

  !include 'mpi.h'

  ! Createpools stuff:
  integer :: nvalence = 4
  integer :: nconduction = 6
  integer :: npools 
  integer :: nvownmax 
  integer :: ncownmax

  ! Distribution subroutine stuff:
  integer :: mypool, mypoolrank, myipe
  integer :: nvownactual, ncownactual
  integer, allocatable  :: global_pairowner(:,:)
  logical, allocatable  :: doiownv(:)
  logical, allocatable  :: doiownc(:)
  logical, allocatable  :: does_it_ownv(:,:)
  logical, allocatable  :: does_it_ownc(:,:)
  integer, allocatable  :: global_nvown(:)
  integer, allocatable  :: global_ncown(:)
  integer, allocatable  :: indexv(:)
  integer, allocatable  :: global_indexv(:,:)
  integer, allocatable  :: indexc(:)
  integer, allocatable  :: invindexv(:)
  integer, allocatable  :: invindexc(:)
  integer :: i, j, ic, iv, ipool, ipe

  ! Temporary variables for distribution:
  integer, allocatable :: global_pairowner_temp(:,:)
  integer, allocatable :: global_nvown_temp(:)
  integer, allocatable :: global_ncown_temp(:)
  integer, allocatable :: global_indexv_temp(:,:)

  ! MPI stuff:
  integer :: mpierror
  integer :: comm, info
  integer :: inode, npes, an_id
  integer :: file_write, ierr!, status(MPI_STATUS_SIZE), sender
  integer, parameter :: return_data_tag = 2001


  ! Inclusion array stuff:
  !integer, dimension(3,2) :: incl_array
  integer, dimension(2, 2) :: incl_array_v
  integer, dimension(2, 2) :: incl_array_c
  integer, allocatable :: my_incl_array_v(:,:), my_incl_array_c(:,:)


  comm = MPI_COMM_WORLD
  info = MPI_INFO_NULL

  call MPI_INIT(mpierror)
  call MPI_COMM_SIZE(comm, npes, mpierror)
  call MPI_COMM_RANK(comm, inode, mpierror)

  ! Define a dummy inclusion array
 ! incl_array(1,1) = 2
 ! incl_array(1,2) = 2
 ! incl_array(2,1) = 4
 ! incl_array(2,2) = 6
 ! incl_array(3,1) = 8
 ! incl_array(3,2) = 9
  ! rows included: 2, 4, 5, 6, 8, 9

  incl_array_v(1,1) = 2
  incl_array_v(1,2) = 2
  incl_array_v(2,1) = 4
  incl_array_v(2,2) = 6

  incl_array_c(1,1) = 8
  incl_array_c(1,2) = 9
  incl_array_c(2,1) = 11
  incl_array_c(2,2) = 14
  ! ISSUE: in real code, the valence bands are read in separately from the
  ! conduction bands in hdf_read_bands_block, but here we have the total
  ! incl_array. We can generate an incl_array_v, incl_array_c, or...

  call my_distribution()
 ! call make_my_incl_array(incl_array_v, doiownv, my_incl_array_v, nvownactual)
  call make_my_incl_array(incl_array_c, doiownc, my_incl_array_c, ncownactual)
!  call read_bands

  deallocate(global_pairowner)
  deallocate(doiownv)
  deallocate(doiownc)
  deallocate(does_it_ownv)
  deallocate(does_it_ownc)
  deallocate(global_nvown)
  deallocate(global_ncown)
  deallocate(indexv)
  deallocate(global_indexv)
  deallocate(indexc)
  deallocate(invindexv)
  deallocate(invindexc)

  call MPI_FINALIZE(mpierror)

  contains

    subroutine my_distribution()

      ! Make allocations:
      allocate( global_pairowner(nvalence, nconduction))
      global_pairowner(:,:) = 0
      allocate( doiownv(nvalence))
      doiownv(:) = .false.
      allocate( doiownc(nconduction))
      doiownc(:) = .false.
      allocate( does_it_ownv(nvalence, npes))
      does_it_ownv(:,:) = .false.
      allocate( does_it_ownc(nconduction, npes))
      does_it_ownc(:,:) = .false.
      allocate( global_nvown(npes))
      global_nvown(:) = 0
      allocate( global_ncown(npes))
      global_ncown(:) = 0
      allocate( indexv(nvalence))
      indexv(:) = 0
      allocate( global_indexv(nvalence, npes))
      global_indexv(:,:) = 0
      allocate( indexc( nconduction))
      indexc(:) = 0
      allocate( invindexv( nvownmax))
      invindexv(:) = 0
      allocate( invindexc( ncownmax))
      invindexc(:) = 0
      ! try to do this after lunch
    
      call createpools(nvalence, nconduction, npes, npools, nvownmax, ncownmax)
    
     ! if (npools .gt. npes) then
        if (mod(nvalence, npools) .eq. 0) then
          nvownmax = nvalence / npools
        else
          nvownmax = (nvalence/npools) + 1
        end if
    
        if (mod(nconduction, (npes/npools)) .eq. 0) then
          ncownmax = (nconduction) / (npes/npools)
        else
          ncownmax = (nconduction) / (npes/npools) + 1
        end if
     ! end if
    
      nvownactual = 0
      ncownactual = 0
    
      mypool = inode/(npes/npools)
      mypoolrank = mod(inode, (npes/npools))
      myipe = inode + 1
      if (inode .eq. 0) then
        write (*,*) "npools = ", npools, "nvownmax = ", nvownmax, "ncownmax = ", &
          ncownmax
      end if
!      write(*,*) "My ID: ", inode, "My pool: ", mypool, "My pool rank: ", mypoolrank
    
      do iv = 1, nvalence
        ipool = (iv - 1)/nvownmax
    
        if (mypool .eq. ipool .and. inode .lt. npes) then
    
          nvownactual = nvownactual+1
          global_nvown(myipe) = nvownactual
          indexv(iv) = nvownactual
          global_indexv(iv, myipe) = indexv(iv)
          invindexv(nvownactual) = iv
          doiownv(iv) = .true.
    
          do ic = 1, nconduction
            if ( (ic - 1)/ ncownmax == mypoolrank) then
              if (nvownactual .eq. 1) then
                 ncownactual = ncownactual + 1
                 global_ncown(myipe) = ncownactual
                 invindexc(ncownactual) = ic
                 indexc(ic) = ncownactual
                 doiownc(ic) = .true.
               end if
               global_pairowner(iv, ic) = myipe
             end if
         end do ! end do ic = 1, nconduction
    
        endif ! end if my pool the current pool
      end do ! end do iv = 1, nvalence
    
      if (inode .lt. npes) then
        does_it_ownv(:, inode+1) = doiownv(:)
        does_it_ownc(:, inode+1) = doiownc(:)
      end if ! inode < npes
    
    
      do ipe=0, npes-1
        call MPI_Bcast(does_it_ownv(1, ipe+1), nvalence, &
          MPI_LOGICAL, ipe, MPI_COMM_WORLD, mpierror)
        call MPI_Bcast(does_it_ownc(1, ipe+1), nconduction, &
          MPI_LOGICAL, ipe, MPI_COMM_WORLD, mpierror)
      end do !ipe = 0, npes-1
    
      allocate(global_pairowner_temp(nvalence, nconduction))
      call MPI_ALLREDUCE(global_pairowner(1,1), global_pairowner_temp(1,1), &
        nvalence*nconduction, MPI_INTEGER, MPI_SUM, &
        MPI_COMM_WORLD, mpierror)
      global_pairowner = global_pairowner_temp
      deallocate(global_pairowner_temp)
    
      allocate(global_nvown_temp(npes))
      call MPI_ALLREDUCE(global_nvown, global_nvown_temp, npes, MPI_INTEGER, &
        MPI_SUM, MPI_COMM_WORLD, mpierror)
      global_nvown = global_nvown_temp
      deallocate(global_nvown_temp)
    
      allocate(global_ncown_temp(npes))
      call MPI_ALLREDUCE(global_ncown, global_ncown_temp, npes, MPI_INTEGER, &
        MPI_SUM, MPI_COMM_WORLD, mpierror)
      global_ncown = global_ncown_temp
      deallocate(global_ncown_temp)
    
      allocate(global_indexv_temp(nvalence, npes))
      call MPI_ALLREDUCE(global_indexv(1,1), global_indexv_temp(1,1), &
        npes*nvalence, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, mpierror)
      global_indexv = global_indexv_temp
      deallocate(global_indexv_temp)
    
      call MPI_BARRIER(comm, mpierror)
    
      if ( inode .eq. 0) then
!        write(*,*) "Global pair owner: "
!        do i = 1, nvalence
!          write(*,*) (global_pairowner(i, j), j = 1, nconduction)
!        end do
        
        write(*,*) "Does_it_ownv:"
        do i = 1, nvalence
          write(*,*) (does_it_ownv(i, j), j = 1, npes)
        end do
        
        write(*,*) "Does_it_ownc:"
        do i = 1, nconduction
          write(*,*) (does_it_ownc(i, j), j = 1, npes)
        end do
    
!        write(*,*) "global_nvown:"
!        write(*,*) global_nvown
!    
!        write(*,*) "global_ncown:"
!        write(*,*) global_ncown
!    
!        write(*,*) "global indexv:"
!        do i = 1, nvalence
!          write(*,*) (global_indexv(i, j), j = 1, npes)
!        end do
    
      end if ! if inode == 0

    end subroutine my_distribution

      
    subroutine createpools(npq, nsq, npess, npoolsout, npqownmaxout, nsqownmaxout)

      integer, intent(in) :: npq, nsq, npess
      integer, intent(out) :: npoolsout, npqownmaxout, nsqownmaxout
      integer :: nmemmin, npoolss, nsqownmax, npqownmax, nmem, npes_per_npoolss

      nmemmin = npq + nsq + 1
      npoolsout = 0
      npqownmaxout = 0
      nsqownmaxout = 0

      do npoolss = 1, min(npq, npess)
        npqownmax = (npq + npoolss - 1) / npoolss
        npes_per_npoolss = npess/npoolss
        nsqownmax = (nsq + npes_per_npoolss - 1) / npes_per_npoolss

        nmem = npqownmax + nsqownmax

        if (nmem .lt. nmemmin) then
          nmemmin = nmem
          npoolsout = npoolss
          npqownmaxout = npqownmax
          nsqownmaxout = nsqownmax
        end if

      end do

      return

    end subroutine createpools

    subroutine make_my_incl_array(incl_array, do_i_own, my_incl_array,&
      nownactual)

      integer, intent(in) :: incl_array(:,:)
      integer, intent(in) :: nownactual
      integer, intent(out), allocatable :: my_incl_array(:, :)
      logical, intent(in) :: do_i_own(:)
    
      integer :: current_last_incl_row
      integer :: nrows, ncols
      !integer :: my_count = 0 ! for allocating my_incl_array
      integer :: irow ! row indexer of my_incl_array
      integer :: next_row
      integer :: i_ia  ! "(i)ndex of (i)nclusion (a)rray" 
      integer :: i, j, k
      integer :: init = 0
      
      nrows = SIZE(incl_array, 1) ! first dim of incl_array
      ncols = 2 ! Fixed by def of inclusion array
    
      allocate(my_incl_array(nownactual, ncols)) ! allocate worst case scenario
      my_incl_array = -1 ! -1 if row is extra
    
      next_row = incl_array(1,1)
      i_ia = 1
      irow = 2
      do i = 1, size(do_i_own)
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
            ! grab the bottom right value in my_incl_array
            current_last_incl_row = my_incl_array(irow - 1, 2)
            ! if the next value is adjacent, then just increment the bottom right
            ! value to the next row.
            if ( next_row .eq. current_last_incl_row + 1 ) then
              my_incl_array(irow - 1, 2) = my_incl_array(irow - 1, 2) + 1
  !            my_incl_array(irow, 2) = my_incl_array(irow, 2) + 1
            else ! if not adjacent, then the band is not in the current cluster, so
              ! start a new one 
              irow = irow + 1
              my_incl_array(irow - 1, 1) = next_row
              my_incl_array(irow - 1, 2) = next_row
            end if ! (next_row...)
          end if ! init .eq. 0
        end if ! do_i_own(i)
        if ( i .ne. size(do_i_own)) then
          if (incl_array(i_ia, 2) .gt. next_row) then
            next_row = next_row + 1
          else
            next_row = incl_array(i_ia+1, 1)
           i_ia = i_ia + 1
          end if ! incl_array(i_ia) ... 
        end if ! i .ne. size(do_i_own)
      end do
    
     write(*,*) " "
      do i = 1, nownactual
        write(*,*) (inode, my_incl_array(i, j), j = 1, ncols)
      end do
      write(*,*) " "
    
      end subroutine make_my_incl_array

      subroutine make_vc_incl_array(incl_array, nvalence, nconduction, &
          incl_array_v, incl_array_c)

        integer, intent(in) :: incl_array(:,:)
        integer, intent(in) :: nvalence
        integer, intent(in) :: nconduction    
        integer, allocatable, intent(out) :: incl_array_v
        integer, allocatable, intent(out) :: incl_array_c

        integer :: vcount = 0
        integer :: j = 1
        integer :: nrows
        integer :: find_v

        nrows = size(incl_array, 1)

        do while vcount .le. nvalence
          vcount = vcount + incl_array(j, 2) - incl_array(j, 1) + 1
          j = j + 1
        end do while

        allocate(incl_array_v(j, 2))
        allocate(incl_array_c(nrows - j, 2))

        incl_array_v = incl_array(1:j, :)
        incl_array_c = incl_array(1:(nrows - j), :)

        if (vcount .ne. nvalence) then
          do i = 1, (incl_array(j, 2) - incl_array(j,1))
            if (vcount .ne. nvalence) then
              vcount = vcount - 1
              find_v = find_v + 1




      end subroutine make_vc_incl_array
!    subroutine read_bands
!
!    end subroutine read_bands


end program my_main

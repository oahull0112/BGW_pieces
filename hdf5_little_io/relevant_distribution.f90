PROGRAM relevant_distribution

  USE HDF5
  USE mpi

  IMPLICIT NONE

!  include 'mpif.h'

  ! Createpools stuff:
  integer :: nvalence = 5
  integer :: nconduction = 3
  integer :: npools 
  integer :: nvownmax 
  integer :: ncownmax

  ! Distribution subroutine stuff:
  integer :: mypool, mypoolrank, myipe
  integer :: nvownactual, ncownactual
  integer, allocatable :: global_pairowner(:,:)
  logical, allocatable :: doiownv(:)
  logical, allocatable :: doiownc(:)
  logical, allocatable :: does_it_ownv(:,:)
  logical, allocatable :: does_it_ownc(:,:)
  integer, allocatable :: global_nvown(:)
  integer, allocatable :: global_ncown(:)
  integer, allocatable :: indexv(:)
  integer, allocatable :: global_indexv(:,:)
  integer, allocatable :: indexc(:)
  integer, allocatable :: invindexv(:)
  integer, allocatable :: invindexc(:)
  integer :: i, j, ic, iv, ipool, ipe

  ! Temporary variables for distribution:
  integer, allocatable :: global_pairowner_temp(:,:)
  integer, allocatable :: global_nvown_temp(:)
  integer, allocatable :: global_ncown_temp(:)
  integer, allocatable :: global_indexv_temp(:,:)

  ! MPI stuff:
  integer :: mpierror, endmpierr       ! MPI error flag
  integer :: comm, info
  integer :: inode, npes, an_id
  integer :: file_write, ierr!, status(MPI_STATUS_SIZE), sender
  integer, parameter :: return_data_tag = 2001

  comm = MPI_COMM_WORLD
  info = MPI_INFO_NULL

  call MPI_INIT(endmpierr)
  call MPI_COMM_SIZE(comm, npes, mpierror)
  call MPI_COMM_RANK(comm, inode, mpierror)

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
  write(*,*) "My ID: ", inode, "My pool: ", mypool, "My pool rank: ", mypoolrank

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
    write(*,*) "Global pair owner: "
    do i = 1, nvalence
      write(*,*) (global_pairowner(i, j), j = 1, nconduction)
    end do
    
    write(*,*) "Does_it_ownv:"
    do i = 1, nvalence
      write(*,*) (does_it_ownv(i, j), j = 1, npes)
    end do
    
    write(*,*) "Does_it_ownc:"
    do i = 1, nconduction
      write(*,*) (does_it_ownc(i, j), j = 1, npes)
    end do

    write(*,*) "global_nvown:"
    write(*,*) global_nvown

    write(*,*) "global_ncown:"
    write(*,*) global_ncown

    write(*,*) "global indexv:"
    do i = 1, nvalence
      write(*,*) (global_indexv(i, j), j = 1, npes)
    end do


   ! write(*,*) "invindexv"
   ! write(*,*) invindexv

   ! write(*,*) "invindexc"
   ! write(*,*) invindexc

  end if ! if inode == 0

  call MPI_Barrier(comm, mpierror)

  write(*,*) "indexc, MPI_task: ", inode
  write(*,*) indexc

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

  call MPI_FINALIZE(endmpierr)

  contains
    
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

END PROGRAM relevant_distribution



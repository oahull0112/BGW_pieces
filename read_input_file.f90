program read_input_file

  implicit none

  integer :: nbands
  integer :: nband_ranges
  integer :: n_incl_rows
  integer, allocatable :: incl_array(:,:) ! (number_band_ranges, 2)
  character*256 :: blockword, keyword, line, errmsg
  integer :: ii, i, j, iostat
  integer, dimension(2) :: what_is_this


  call open_file(55,file='epsilon.inp',form='formatted',status='old')

  do while (0 .eq. 0)

    read(55, '(a256)', iostat=iostat) line
    if (iostat < 0) exit

    if (len_trim(line).eq.0) cycle
    if(line(1:1).eq.'#') cycle

    keyword=line(1:scan(line," ")-1)
    line=adjustl(line(scan(line," ")+1:256))

    if (trim(keyword).eq.'begin') then
      blockword=line(1:scan(line," ")-1)
      ii = 0
      do while (trim(line).ne.'end')
        read(55, '(a256)', iostat=iostat) line
        if(trim(line).ne.'end') then
          ii = ii+1
          if(trim(blockword).eq.'band_ranges') then
            if( allocated(incl_array)) then
            !  read(line,*,iostat=iostat) what_is_this(1),what_is_this(2)
              read(line,*,iostat=iostat) incl_array(ii, 1), incl_array(ii, 2)
              ! need to allocate incl_array based on number_band_ranges
              !write(*,*) incl_array(ii, 1), incl_array(ii,2)
            else
              write(errmsg,'(3a)') "number_band_ranges must be listed before &
                begin_band_ranges in the input file"
              call die(errmsg, only_root_writes = .true.)
            end if ! allocated
          end if ! trim(blockword)
        end if ! trim(line).ne.'end'
      end do ! while (trim(line).ne.'end'
!    end if ! trim keyword .eq. begin

    elseif(trim(keyword).eq.'number_band_ranges') then
      read(line,*,iostat=iostat) n_incl_rows
      allocate(incl_array(n_incl_rows, 2))
    else
      write(*,*) ' '
    end if


  end do

  do i = 1, n_incl_rows
    write(*,*) (incl_array(i, j), j = 1,2)
  end do

  
    contains

      function TRUNC(s)
        character(len=*), intent(in) :: s
        character(len=len_trim(adjustl(s))) :: TRUNC

        TRUNC = trim(adjustl(s))
      end function TRUNC

      subroutine die(str, only_root_writes)

        character (len=*), intent(in) :: str
        logical, optional, intent(in) :: only_root_writes

        logical :: should_write, should_write_prefix, is_open

        should_write = .true.
        should_write_prefix = .false.
        if (present(only_root_writes)) then
          if (.not.only_root_writes) then
            should_write = .true.
            should_write_prefix = .true.
          endif
        endif

        ! FHJ: FLUSH is not really reliable because the OS might cache the stdout.
        ! Sleeping for 1s is the best solution I found to make the output clean,
        ! otherwise the error message would show up before regular output.
        call sleep(1)
        ! FHJ: if we are not writing, wait 5s for the root node to get here and
        ! write the error message. If the root doesn`t get here, we all print the
        ! error messsage anyways and die.
        if (.not.should_write) then
          call sleep(5)
        endif
        write(0,*)
        if (should_write_prefix) write(0, '(a, i6, a)', advance='no') 
        write(0, '(2a)') "ERROR: ", TRUNC(str)
        write(0,*)
        FLUSH(0)

        ! FHJ: Use libc`s abort funciton.
        ! Fortran`s `stop` or `error stop` don`t give us the traceback!
        call abort()

      end subroutine die

      subroutine open_file(unit, file, status, form, position, access, iostat)
  
      integer,          intent(in) :: unit
      character(len=*), intent(in) :: file
      character(len=*), intent(in) :: status
      character(len=*), optional, intent(in) :: form
      character(len=*), optional, intent(in) :: position
      character(len=*), optional, intent(in) :: access
      integer, optional, intent(out) :: iostat
  
      integer :: ierr, unit_other
      character*80 :: form_, position_, name, unit_str, access_, unit_other_str
      character*200 :: string
      logical :: is_open, does_exist
  
      if(unit == 0) call die("You may not open unit 0, it is reserved for standard error.")
      if(unit == 5) call die("You may not open unit 5, it is reserved for standard input.")
      if(unit == 6) call die("You may not open unit 6, it is reserved for standard output.")
      if(unit == 100) call die("You may not open unit 100, it is reserved for standard input (crayftn).")
      if(unit == 101) call die("You may not open unit 101, it is reserved for standard output (crayftn).")
      if(unit == 102) call die("You may not open unit 102, it is reserved for standard error (crayftn).")
  
      inquire(unit = unit, opened = is_open, name = name)
      if(is_open) then
        write(string,'(3a,i6,3a)') "Cannot open file '", TRUNC(file), "' on unit ", unit, &
          ": unit already open for file '", TRUNC(name), "'."
        call die(string)
      endif
  
      if((trim(status) == 'old' .or. trim(status) == 'OLD') .and. .not. present(iostat)) then 
        inquire(file = TRUNC(file), exist = does_exist, opened = is_open, number = unit_other)
        if(.not. does_exist) call die("Cannot open file '" // TRUNC(file) // "' for reading: does not exist.")
        if(is_open) then
          write(unit_str,*)       unit
          write(unit_other_str,*) unit_other
          call die("Cannot open file '" // TRUNC(file) // "' for reading on unit " // TRUNC(unit_str) &
            // ": already opened on unit " // TRUNC(unit_other_str) // ".")
        endif
  
  ! From the Fortran 95 standard, Section 9.3.4:
  ! 
  !    If a file is already connected to a unit, execution of an OPEN
  !    statement on that file and a different unit is not permitted.
  ! 
  ! From the Fortran 77 Standard, Section 12.3.2:
  ! 
  !    A unit must not be connected to more than one file at the same time,
  !    and a file must not be connected to more than one unit at the same time.
  
      endif
  
      if((trim(status) == 'new' .or. trim(status) == 'NEW') .and. .not. present(iostat)) then 
        inquire(file = TRUNC(file), exist = does_exist)
        if(does_exist) call die("Cannot open file '" // TRUNC(file) // "' for writing as 'new': already exists.")
      endif
  
      form_   = 'formatted'
      if(present(form    )) form_     = form
      position_ = 'asis'
      if(present(position)) position_ = position
      access_ = 'sequential'
      if(present(access)) access_ = access
  
      ! passing the optionals to 'open' if not given to this routine does not work!
      open(unit=unit, file = TRUNC(file), form=trim(form_), access=trim(access_), &
        position=trim(position_), status=trim(status), iostat=ierr)
      if(present(iostat)) then
        iostat = ierr
      else if(ierr /= 0) then
        write(string,'(5a,i4)') "Failed to open file '", TRUNC(file), "' as status ", trim(status), " with error ", ierr
        call die(string)
      endif
  
      return
    end subroutine open_file

    subroutine close_file(unit, delete)

      integer,           intent(in) :: unit
      logical, optional, intent(in) :: delete

      character*80 :: string, status
      logical :: is_open
      integer :: ierr

      if(unit == 0) call die("You may not close unit 0, it is reserved for standard error.")
      if(unit == 5) call die("You may not close unit 5, it is reserved for standard input.")
      if(unit == 6) call die("You may not close unit 6, it is reserved for standard output.")
      ! Cray Fortran has its own reserved units: http://docs.cray.com/books/S-3695-35/html-S-3695-35/pdollsmg.html
      if(unit == 100) call die("You may not close unit 100, it is reserved for standard input (crayftn).")
      if(unit == 101) call die("You may not close unit 101, it is reserved for standard output (crayftn).")
      if(unit == 102) call die("You may not close unit 102, it is reserved for standard error (crayftn).")

      ! these issues would be caught below too, but we can give more helpful messages than just an error code
      inquire(unit = unit, opened = is_open, iostat = ierr)
      if(ierr /= 0) then
        write(string,'(a,i6,a,i4)') "inquire in close_file failed for unit ", unit, " with error ", ierr
        call die(string)
      endif
      if(.not. is_open) then
        write(string,'(a,i6,a)') "Cannot close unit ", unit, ": not open."
        call die(string)
      endif

      status = 'keep'
      if(present(delete)) then
        if(delete) status = 'delete'
      endif

      close(unit=unit, status=trim(status), iostat=ierr)
      if(ierr /= 0) then
        write(string,'(a,i6,a,i4)') "Failed to close unit ", unit, " with error ", ierr
        call die(string)
      endif

      return
  end subroutine close_file
end program read_input_file

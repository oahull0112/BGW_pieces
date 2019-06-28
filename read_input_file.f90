program read_input_file

  implicit none

  integer :: nbands
  integer :: nband_ranges
  integer, allocatable :: incl_array(:,:) ! (number_band_ranges, 2)
  character*256 :: blockword, keyword, line, errmsg
  integer :: ii


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
    end if
  end do

  write(*,*) "keyword = ", keyword
  write(*,*) "blockword = ", blockword
  
    contains
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

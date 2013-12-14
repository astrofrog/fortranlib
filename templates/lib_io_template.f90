! ------------------------------------------------------------------------------
! Copyright (c) 2009-13, Thomas P. Robitaille
!
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
!  * Redistributions of source code must retain the above copyright notice, this
!    list of conditions and the following disclaimer.
!
!  * Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ------------------------------------------------------------------------------

module lib_io

  use iso_fortran_env, only : iostat_end
  use lib_messages
  use posix

  implicit none
  save

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  public :: list_files
  public :: delete_dir,delete_file
  public :: dir_exists,file_exists
  public :: check_file_exists,check_dir_exists
  public :: len_ascii,len_bin,file_n_lines,file_n_header_lines
  public :: next_unit,open_safe,open_status

  logical,private :: debug = .false.

  public :: read_column
  interface read_column
     module procedure read_column_char
     module procedure read_column_logical
     module procedure read_column_integer
     module procedure read_column_real4
     module procedure read_column_real8
  end interface read_column

contains

  subroutine list_files(directory,files,pattern)
    implicit none
    character(len=*),intent(in)  :: directory,pattern
    character(len=*),allocatable,intent(out) :: files(:)
    integer :: unit,n_files,f
    character(len=1000) :: buffer
    if(allocated(files)) deallocate(files)
    call system('find '//trim(directory)//'  -name "'//trim(pattern)//'" | sort > /tmp/list.temp')
    n_files = file_n_lines('/tmp/list.temp')
    allocate(files(n_files))
    call open_safe(unit,file='/tmp/list.temp',status='old')
    do f=1,n_files
       read(unit,'(A1000)') buffer
       files(f)=adjustl(trim(buffer))
    end do
    close(unit)
    call system('rm /tmp/list.temp')
  end subroutine list_files

  !**********************************************************************
  ! Delete directory (with prompt) and make new empty directory
  !**********************************************************************

  subroutine delete_dir(dir)

    implicit none

    character(len=*),intent(in) :: dir
    ! the directory to look for

    character(len=3) :: rep
    ! user reply (YES/NO)

    character(len=100) :: command,pretty_command
    ! command to delete the directory

    character(len=3) :: len_char
    ! length of string

    if(dir_exists(dir)) then

       command="rm -r "//trim(dir)
       pretty_command="["//trim(command)//"]"

       write(len_char,'(I3.3)') len_trim(dir)
       write(*,'(" WARNING: Directory exists: ",A'//len_char//')') dir

       write(len_char,'(I3.3)') len_trim(pretty_command)
       write(*,'(" The following command will be run: ",A'//len_char//')') pretty_command

       write(*,'(" Do you wish to continue? (y/n) ")')

       do
          read *,rep
          if(rep=="y".or.rep=="n") exit
          print *,'Please type y or n (case sensitive)'
       end do
       if(rep=='n') then
          print *,'Aborting to avoid overwriting files'
          stop
       end if
       call system(command)
    end if

    print *,' -> Creating directory: ',trim(dir)
    call system('mkdir '//dir)

  end subroutine delete_dir

  !**********************************************************************
  ! Delete file (with prompt)
  !**********************************************************************

  subroutine delete_file(file)

    implicit none

    character(len=*),intent(in) :: file
    ! the fileectory to look for

    character(len=3) :: rep
    ! user reply (YES/NO)

    character(len=100) :: command,pretty_command
    ! command to delete the file

    character(len=3) :: len_char
    ! length of the character to display

    if(file_exists(file)) then

       command="rm "//trim(file)
       pretty_command="["//trim(command)//"]"

       write(len_char,'(I3.3)') len_trim(file)
       write(*,'(" WARNING: File exists: ",A'//len_char//')') file

       write(len_char,'(I3.3)') len_trim(pretty_command)
       write(*,'(" The following command will be run: ",A'//len_char//')') pretty_command

       write(*,'(" Do you wish to continue? (y/n) ")')

       do
          read *,rep
          if(rep=="y".or.rep=="n") exit
          print *,'Please type y or n (case sensitive)'
       end do
       if(rep=='n') then
          print *,'Aborting to avoid overwriting file'
          stop
       end if

       call system(command)

    end if

    open (29,file=file, form='unformatted')
    close (29,status='delete')
    !    call flush(29)

  end subroutine delete_file

  !**********************************************************************
  ! Check that file exists and produce error if not
  !**********************************************************************

  subroutine check_file_exists(file)

    implicit none

    character(len=*),intent(in) :: file
    ! the file to look for

    if(.not.file_exists(file)) then
       call error("check_file_exists","File does not exist : "//trim(file))
    end if

  end subroutine check_file_exists

  !**********************************************************************
  ! Check that directory exists and produce error if not
  !**********************************************************************

  subroutine check_dir_exists(dir)

    implicit none

    character(len=*),intent(in) :: dir
    ! the directory to look for

    if(.not.file_exists(dir)) then
       call error("check_dir_exists","Directory does not exist : "//trim(dir))
    end if

  end subroutine check_dir_exists

  !**********************************************************************
  ! Find length of unformatted file
  !**********************************************************************

  integer function len_bin(filename,header)

    implicit none

    character(len=*),intent(in) :: filename
    ! the binary file to find the length of

    logical,intent(in) :: header

    integer :: count
    ! how many lines have been read so far

    integer :: ioerr
    ! used for I/O errors

    count=0

    open(unit=10,file=filename,form='unformatted',status='old',iostat=ioerr)
    call open_status(ioerr,filename)

    if(header) read(10)
    do
       read(10,iostat=ioerr)
       if(ioerr.ne.0) exit
       count=count+1
    end do
    close(10)

    len_bin=count

  end function len_bin

  !**********************************************************************
  ! Find length of ASCII file
  !**********************************************************************

  integer function len_ascii(filename,header) ! V2C

    implicit none

    character(len=*),intent(in) :: filename
    ! the ascii file to find the length of

    logical,intent(in) :: header

    integer :: count
    ! how many lines have been read so far

    integer :: ioerr
    ! used for I/O errors

    count=0

    open(unit=10,file=filename,status='old',iostat=ioerr)
    call open_status(ioerr,filename)

    if(header) read(10,*)
    do
       read(10,*,iostat=ioerr)
       if(ioerr.ne.0) exit
       count=count+1
    end do
    close(10)

    len_ascii=count

  end function len_ascii

  !**********************************************************************
  ! Find length of ASCII file (new)
  !**********************************************************************

  integer function file_n_lines(filename,unformatted)

    implicit none

    character(len=*),intent(in) :: filename
    ! the ascii file to find the length of

    integer :: count
    ! how many lines have been read so far

    integer :: ioerr
    ! used for I/O errors

    integer :: u

    logical,optional :: unformatted
    logical :: formatted

    formatted = .true.

    if(present(unformatted)) then
       formatted = .not.unformatted
    end if

    count=0

    call open_safe(u,filename,status='old')

    do

       if(formatted) then
          read(u,*,iostat=ioerr)
       else
          read(u,iostat=ioerr)
       end if

       if(ioerr.ne.0) exit
       count=count+1

    end do

    close(u)

    file_n_lines=count

  end function file_n_lines


  !**********************************************************************
  ! Find length of header (comments) section in ASCII file
  !**********************************************************************

  integer function file_n_header_lines(filename)

    implicit none

    character(len=*),intent(in) :: filename
    ! the ascii file to find the length of

    integer :: count
    ! how many lines have been read so far

    integer :: ioerr
    ! used for I/O errors

    integer :: u

    character(len=1) :: first

    character(len=1),parameter :: slash = achar(92)

    character(len=1000) :: line

    count=0

    call open_safe(u,filename,status='old')

    do
       read(u,'(A1000)',iostat=ioerr) line
       line = adjustl(line)
       first = line(1:1)
       if(ioerr.ne.0) exit
       select case(first)
       case('#',slash,'|')
          count = count + 1
       case default
          exit
       end select
    end do

    close(u)

    file_n_header_lines=count

  end function file_n_header_lines

  !**********************************************************************
  ! Display open status message
  !**********************************************************************

  subroutine open_status(ioerr,filename)
    implicit none
    integer,intent(in) :: ioerr
    character(len=*),intent(in) :: filename
    if(ioerr.ne.0) call error("open_status","File not found : "//trim(filename))
  end subroutine open_status

  !**********************************************************************
  ! Open file (with error checking)
  !**********************************************************************

  subroutine open_safe(unit,file,status,form,position)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: file
    ! the file to open

    character(len=*),intent(in),optional :: status,form,position
    ! whether the file exists already, whether to write it in ascii form,
    ! and whether to write in append mode

    ! --- Output --- !

    integer,intent(out) :: unit
    ! the unit that is used

    character(len=15) :: opt_status,opt_form,opt_position

    ! Find free unit

    unit = next_unit()

    ! Set default options

    opt_status = 'replace'
    opt_form   = 'formatted'
    opt_position = 'asis'

    if(present(status)) opt_status = status
    if(present(form))   opt_form   = form
    if(present(position)) opt_position = position

    if(opt_position=='append') opt_status='old'

    if(opt_status=='old') then
       call check_file_exists(file)
    else
       call delete_file(file)
    end if

    open(unit=unit,file=file,status=opt_status,form=opt_form,position=opt_position)

  end subroutine open_safe

  !**********************************************************************
  ! Find next free I/O unit
  !**********************************************************************

  integer function next_unit()

    implicit none

    integer,parameter :: u_start = 10
    ! the unit number to start searching at

    integer :: u
    ! loop variable

    logical :: op
    ! whether the unit is already in use

    do u=u_start,1000
       inquire(unit=u,opened=op)
       if(.not.op) then
          next_unit=u
          exit
       end if
    end do

    return

  end function next_unit

  !**********************************************************************
  ! Read column from ASCII file
  !**********************************************************************

  !!@FOR character(len=*):char integer:integer real(sp):real4 real(dp):real8 logical:logical

  subroutine read_column_<T>(filename,column,values)

    implicit none

    character(len=*),intent(in) :: filename
    ! the name of the file to read the columns from

    integer,intent(in) :: column
    ! the column number

    @T,intent(out), allocatable :: values(:)
    ! the values

    integer :: i,j,u
    ! loop variable

    integer :: n_header,n_total,n_read
    ! the number of lines in the file

    character(len=100) :: dum
    ! dummy variable

    integer :: ioerr
    ! I/O error status

    if(allocated(values)) deallocate(values)

    n_header = file_n_header_lines(filename)
    n_total  = file_n_lines(filename)
    n_read   = n_total - n_header

    allocate(values(n_read))

    call open_safe(u,filename,status='old')

    if(debug) write(*,'("Skipping ",I6," header lines")') n_header
    do i=1,n_header
       read(u,*)
    end do

    if(debug) write(*,'("Reading  ",I6," data lines")') n_read
    do i=1,n_read
       read(u,*,iostat=ioerr) (dum,j=1,column-1),values(i)
       if(ioerr==iostat_end) then
          call error("read_column_<T>","end of file reached in "//trim(filename))
       end if
    end do

    close(unit=u)

  end subroutine read_column_<T>

  !!@END FOR

  !**********************************************************************
  ! The following subroutine tries to find a unique solution to a filename
  ! containing wildcards. This means that for example if the user calls the
  ! subroutine with 'hello*.txt', the subroutine will try to find a unique
  ! match for this filename, and will cause an error if more than one file
  ! is found
  !**********************************************************************

  subroutine solve_wildcard(filename)

    implicit none

    character(len=*),intent(inout) :: filename
    character(len=500) :: filename_temp
    ! the filename to solve

    integer :: u
    ! unit number for temporary file

    integer :: r,n_results
    ! number of possibilities and length of ascii file

    integer :: ioerr
    ! used for I/O errors

    call system('ls '//trim(filename)//' > /tmp/wildcard.tmp')

    u=next_unit()

    n_results=len_ascii('/tmp/wildcard.tmp',.false.)

    if(n_results==0) then

       write(0,*)
       write(0,*) "ERROR - no match found for: ",trim(filename)
       write(0,*)
       stop

    else if(n_results==1) then

       open(unit=u,file='/tmp/wildcard.tmp',status='old',iostat=ioerr)
       call open_status(ioerr,'/tmp/wildcard.tmp')
       read(u,'(A500)') filename_temp
       close(unit=u)

    else

       write(0,*)
       write(0,*) "ERROR - more than one possibility for: ",trim(filename)

       open(unit=u,file='/tmp/wildcard.tmp',status='old',iostat=ioerr)
       call open_status(ioerr,'/tmp/wildcard.tmp')
       do r=1,n_results
          read(u,*) filename_temp
          write(0,*) " -> "//trim(filename_temp)
       end do
       close(unit=u)

       write(0,*)
       stop

    end if

    filename=trim(filename_temp)

  end subroutine solve_wildcard

end module lib_io

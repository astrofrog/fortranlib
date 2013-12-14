! Array related routines (Integration, Interpolation, etc.)
!
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
!
! This module contains subroutines to make it easy to read in
! configuration files. The expected format of a .conf file is
! an ASCII file with lines containing lines such as:
!
! parameter = value
!
! Note that character variables should be enclosed in inverted commas
! especially when giving paths to files. Commented lines (starting with
! a #) and blank lines are allowed. Comments directly following a
! parameter statement on the same line are NOT allowed:
!
! parameter = value  # this is an INVALID comment
!
! The subroutines are:
!
! load_config_file(filename) - load a config file into RAM, ignoring
!                              comments and blank lines
!
! read_config(parameter,value) - read in the value for 'parameter'
!
! read_config is in fact an interface to four different subroutines
! depending on the type of the 'value' variable. This variable can
! be real(sp), real(dp), integer, or character(len=*)

module lib_conf

  implicit none
  save

  private
  public :: load_config_file

  integer,parameter :: idp = selected_int_kind(13)
  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  integer,parameter :: config_file_len=1000
  integer,parameter :: config_line_len=100
  character(len=6),parameter :: config_line_fmt='(A100)'

  integer,private :: n_lines
  ! number of lines is the currently loaded config file

  character(len=config_line_len),dimension(config_file_len),private :: line
  ! the lines of the config file

  public :: read_config
  interface read_config
     module procedure config_real8,config_real4,config_char,config_int4,config_int8,config_logical
  end interface read_config

  interface read_from_string
     module procedure read_int4_from_string
     module procedure read_int8_from_string
     module procedure read_real4_from_string
     module procedure read_real8_from_string
     module procedure read_char_from_string
     module procedure read_logical_from_string
  end interface read_from_string

contains

  subroutine read_int4_from_string(string,value)
    implicit none
    character(len=*),intent(in) :: string
    integer,intent(out) :: value
    read(string,*) value
  end subroutine read_int4_from_string

  subroutine read_int8_from_string(string,value)
    implicit none
    character(len=*),intent(in) :: string
    integer(idp),intent(out) :: value
    read(string,*) value
  end subroutine read_int8_from_string

  subroutine read_real4_from_string(string,value)
    implicit none
    character(len=*),intent(in) :: string
    real(sp),intent(out) :: value
    read(string,*) value
  end subroutine read_real4_from_string

  subroutine read_real8_from_string(string,value)
    implicit none
    character(len=*),intent(in) :: string
    real(dp),intent(out) :: value
    read(string,*) value
  end subroutine read_real8_from_string

  subroutine read_char_from_string(string,value)
    implicit none
    character(len=*),intent(in) :: string
    character(len=*),intent(out) :: value
    value = trim(adjustl(string))
  end subroutine read_char_from_string

  subroutine read_logical_from_string(string,value)
    implicit none
    character(len=*),intent(in) :: string
    logical,intent(out) :: value
    select case(trim(adjustl(string)))
    case('Y','y','yes','YES','Yes','true',"'YES'")
       value=.true.
    case('N','n','no','NO','No','false',"'NO'")
       value=.false.
    case default
       print *,"unknown logical"//trim(string)
    end select
  end subroutine read_logical_from_string

  subroutine load_config_file(filename)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: filename
    ! the name of the .conf file to read in

    ! --- Local variables --- !

    character(len=config_line_len) :: line_temp

    integer :: ioerr
    ! used for I/O errors

    open(unit=80, file=filename, status='old')

    n_lines = 0
    line = ''

    do

       ! --- Read in the next line --- !

       read(80,config_line_fmt,iostat=ioerr) line_temp
       if(ioerr.ne.0) exit

       ! --- Remove preceding whitespace --- !

       line_temp = adjustl(line_temp)

       ! --- Ignore lines with nothing and lines with comments --- !

       if(line_temp(1:1).ne.'#'.and.trim(line_temp).ne.'') then

          n_lines = n_lines + 1
          line(n_lines) = line_temp

       end if

    end do

    close(unit=80)

  end subroutine load_config_file

  !!@FOR integer:int4 integer(idp):int8 character(len=*):char real(sp):real4 real(dp):real8 logical:logical

  subroutine config_<T>(par_name,value,element)

    implicit none

    ! --- Input --- !

    character(len=*),intent(in) :: par_name
    ! parameter to search for

    integer,optional,intent(in) :: element
    ! array element (if needed)

    ! --- Output --- !

    @T,intent(out) :: value
    ! the value that was read in

    character(len=100) :: c_element,par_name_new,name_check
    ! temporary parameter name (e.g. with array index)

    integer :: i,pos
    ! loop and position variables

    logical :: found
    ! whether the parameter was found

    found = .false.

    if(present(element)) then
       write(c_element,'(I0)') element
       par_name_new = trim(par_name)//'('//trim(c_element)//')'
    else
       par_name_new = par_name
    end if

    do i=1,n_lines

       if(index(line(i),trim(par_name_new)).gt.0) then

          pos = index(line(i),'=')
          name_check = adjustl(line(i)(1:pos-1))
          if(trim(name_check) == trim(par_name_new)) then
             call read_from_string(line(i)(pos+1:),value)
             found = .true.
             exit
          end if

       end if

    end do

    if(.not.found) then
       print *, "Parameter not found : "//trim(par_name_new)
       stop
    end if

  end subroutine config_<T>

  !!@END FOR

end module lib_conf

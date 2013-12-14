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

module lib_messages

  implicit none
  save

  private
  public :: message_section
  public :: error
  public :: warning, warn
  public :: delimit
  public :: message
  public :: set_verbose_level
  public :: now

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  integer :: verbose_level = 0

  public :: message_number
  interface message_number
     module procedure message_number_dp,message_number_int
  end interface message_number

contains

  subroutine set_verbose_level(level)
    implicit none
    integer,intent(in) :: level
    verbose_level = level
  end subroutine set_verbose_level

  subroutine message(level,text)
    implicit none
    integer,intent(in)          :: level
    character(len=*),intent(in) :: text
    character(len=10)           :: fmt
    write(fmt,'("(",I0,"A)")') len(text)
    if(level <= verbose_level) write(*,fmt) text
  end subroutine message

  subroutine message_number_dp(level,text,number,format,units)
    implicit none
    integer,intent(in)          :: level
    character(len=*),intent(in) :: text
    real(dp),intent(in)         :: number
    character(len=*),intent(in) :: format,units
    character(len=20)           :: char_number
    write(char_number,format) number
    if(level <= verbose_level) write(*,*) trim(text)//' '//trim(adjustl(char_number))//' '//trim(units)
  end subroutine message_number_dp

  subroutine message_number_int(level,text,number,format,units)
    implicit none
    integer,intent(in)          :: level
    character(len=*),intent(in) :: text
    integer,intent(in)          :: number
    character(len=*),intent(in) :: format,units
    character(len=20)           :: char_number
    write(char_number,format) number
    if(level <= verbose_level) write(*,*) trim(text)//' '//trim(adjustl(char_number))//' '//trim(units)
  end subroutine message_number_int

  subroutine message_section(text)
    implicit none
    character(len=*),intent(in) :: text
    write(*,*)
    call delimit
    write(*,*) ' => '//trim(text)
    call delimit
    write(*,*)
  end subroutine message_section

  subroutine warning(location,text)
    implicit none
    character(len=*),intent(in) :: location,text
    call delimit
    write(*,*) "WARNING : ",trim(text)
    write(*,*) "WHERE   : ",trim(location)
    call delimit
  end subroutine warning

  subroutine warn(location, text)
    implicit none
    character(len=*),intent(in) :: location,text
    write(*,'(" WARNING: ",A," [",A,"]")') text,location
  end subroutine warn

  character(len=30) function now()
    implicit none
    character(len=8) :: date
    character(len=10) :: time
    integer :: m
    call date_and_time(date,time)
    read(date(5:6),*) m
    now = date(7:8)//" "//trim(month(m))//" "//trim(date(1:4))//" at "//time(1:2)//":"//time(3:4)//":"//time(5:6)
  end function now

  subroutine error(location,text)

    implicit none

    character(len=*),intent(in) :: location,text

    character(len=8) :: date
    character(len=10) :: time

    integer :: m, imin, imax, j
    integer, parameter :: width = 61

    call date_and_time(date,time)
    read(date(5:6),*) m

    write(0,*) repeat('-',72)

    ! The following deals with the wrapping of the text, since it allows more
    ! verbose errors without messing up the formatting
    imin = 1
    do
       if(imin + width > len(text)) then
          ! End of message has been reached
          imax = len(text)
       else
          ! Look for spaces
          do j = width, 1, -1
             imax = imin + j
             if(text(imax:imax) == ' ') exit
          end do
          ! No spaces found, just force cut
          if(j == 0) imax = imin + width
       end if
       if(imin == 1) then
          write(0,*) "ERROR   : ",text(imin:imax)
       else
          write(0,*) "          ",text(imin:imax)
       end if
       if(imax == len(text)) exit
       imin = imax + 1
    end do

    write(0,*) "WHERE   : ",trim(location)
    write(0,*) repeat('-',72)

    write(0,*)
    write(0,*) " *** Execution aborted on "&
         &//date(7:8)//" "//trim(month(m))//" "//trim(date(1:4))//" at "&
         &//time(1:2)//":"//time(3:4)//":"//time(5:6)//" ***"
    write(0,*)

    stop

  end subroutine error

  character(len=20) function month(i)
    implicit none
    integer,intent(in) :: i
    if(i==1) month="January"
    if(i==2) month="February"
    if(i==3) month="March"
    if(i==4) month="April"
    if(i==5) month="May"
    if(i==6) month="June"
    if(i==7) month="July"
    if(i==8) month="August"
    if(i==9) month="September"
    if(i==10) month="October"
    if(i==11) month="November"
    if(i==12) month="December"
  end function month

  subroutine delimit
    implicit none
    write(*,*) repeat('-',72)
  end subroutine delimit

end module lib_messages

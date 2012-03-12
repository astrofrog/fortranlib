! Message/error handling
! Thomas Robitaille (c) 2009

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

    integer :: m

    call date_and_time(date,time)
    read(date(5:6),*) m

    call delimit
    write(0,*) "ERROR   : ",trim(text)
    write(0,*) "WHERE   : ",trim(location)
    call delimit

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
    write(*,*) repeat('-',60)
  end subroutine delimit

end module lib_messages

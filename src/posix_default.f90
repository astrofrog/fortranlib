module posix

  use iso_c_binding

  implicit none
  save

  interface
     subroutine usleep(useconds) bind(C)
       use iso_c_binding
       implicit none
       integer(c_int32_t), value :: useconds
     end subroutine usleep
  end interface

contains

  logical function file_exists(file)
    implicit none
    character(len=*),intent(in) :: file
    integer :: access
    file_exists = access(file,' ').eq.0
  end function file_exists

  logical function dir_exists(dir)
    implicit none
    character(len=*),intent(in) :: dir
    integer :: access
    dir_exists = access(dir,' ').eq.0
  end function dir_exists

  subroutine microsleep(microseconds)
    integer,intent(in) :: microseconds
    call usleep(int(microseconds, c_int32_t)) 
  end subroutine microsleep

end module posix

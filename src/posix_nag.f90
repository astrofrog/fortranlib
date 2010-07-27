module posix

  use iso_c_binding

  use f90_unix, only : flush
  use f90_unix_file
  use f90_unix_proc, only : system

  implicit none

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
    integer :: errno
    call access(trim(file),F_OK,errno)
    file_exists = errno == 0
  end function file_exists

  logical function dir_exists(dir)
    implicit none
    character(len=*),intent(in) :: dir
    integer :: errno
    call access(trim(dir),F_OK,errno)
    dir_exists = errno == 0
  end function dir_exists

  subroutine microsleep(microseconds)
    integer,intent(in) :: microseconds
    call usleep(int(microseconds, c_int32_t)) 
  end subroutine microsleep

end module posix

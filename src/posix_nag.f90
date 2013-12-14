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

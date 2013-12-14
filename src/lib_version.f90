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

module lib_version

  type version
     character(len=10) :: string
  end type version

  interface operator(>=)
     module procedure greater_or_equal
  end interface operator(>=)

  interface operator(<=)
     module procedure smaller_or_equal
  end interface operator(<=)

  interface operator(>)
     module procedure greater
  end interface operator(>)

  interface operator(<)
     module procedure smaller
  end interface operator(<)

  interface operator(==)
     module procedure equal
  end interface operator(==)

contains

  logical function greater_or_equal(a, b)
    implicit none
    type(version),intent(in) :: a, b
    greater_or_equal = .not. smaller(a, b)
  end function greater_or_equal

  logical function smaller_or_equal(a, b)
    implicit none
    type(version),intent(in) :: a, b
    smaller_or_equal = .not. greater(a, b)
  end function smaller_or_equal

  logical function smaller(a, b)
    implicit none
    type(version),intent(in) :: a, b
    smaller = .not. greater(a, b) .and. .not. equal(a, b)
  end function smaller

  logical function equal(a, b)
    implicit none
    type(version),intent(in) :: a, b
    equal = a%string == b%string
  end function equal

  logical function greater(a, b)
    implicit none
    type(version),intent(in) :: a, b
    integer :: p1a, p2a, p1b, p2b, ia, ib

    p1a = 1
    p1b = 1

    do

       p2a = index(a%string(p1a:), '.')
       p2b = index(b%string(p1b:), '.')

       if(p2a == 0 .neqv. p2b == 0) then
          stop "ERROR: version strings need to have the same precision"
       end if

       if(p2a == 0) then
          read(a%string(p1a:), *) ia
          read(b%string(p1b:), *) ib
       else
          read(a%string(p1a:p1a + p2a - 2), *) ia
          read(b%string(p1b:p1b + p2b - 2), *) ib
       end if

       if(ia > ib) then
          greater = .true.
          return
       else if(ia < ib) then
          greater = .false.
          return
       end if

       if(p2a == 0 .or. p2b == 0) exit

       p1a = p2a + p1a
       p1b = p2b + p1b

    end do

    greater = .false.

  end function greater

end module lib_version



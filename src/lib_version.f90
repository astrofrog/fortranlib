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
    character(len=10) :: string
    integer :: p1a, p2a, p1b, p2b, ia, ib

    p1a = 1
    p1b = 1

    do

       p2a = index(a%string(p1a:), '.')
       p2b = index(b%string(p1a:), '.')

       if(p2a == 0 .neqv. p2b == 0) then
          stop "ERROR: version strings need to have the same precision"
       end if

       if(p2a == 0) then
          read(a%string(p1a:), *) ia
          read(b%string(p1b:), *) ib
       else
          read(a%string(p1a:p1a + p2a - 1), *) ia
          read(b%string(p1b:p1b + p2b - 1), *) ib
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



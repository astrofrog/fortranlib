! Stokes vector related routines
! Thomas Robitaille (c) 2009

module type_stokes

  implicit none
  save

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  public :: stokes_dp
  type stokes_dp
     real(dp) :: I,U,Q,V
  end type stokes_dp

  public :: stokes_sp
  type stokes_sp
     real(sp) :: I,U,Q,V
  end type stokes_sp

  public :: operator(+)
  interface operator(+)
     module procedure add_stokes_sp
     module procedure add_stokes_dp
  end interface operator(+)

  public :: operator(-)
  interface operator(-)
     module procedure sub_stokes_sp
     module procedure sub_stokes_dp
  end interface operator(-)

  public :: operator(*)
  interface operator(*)
     module procedure scalar_stokes_mult_sp,stokes_scalar_mult_sp
     module procedure scalar_stokes_mult_dp,stokes_scalar_mult_dp
  end interface operator(*)

  public :: operator(/)
  interface operator(/)
     module procedure scalar_stokes_div_sp,stokes_scalar_div_sp
     module procedure scalar_stokes_div_dp,stokes_scalar_div_dp
  end interface operator(/)

contains

  !!@FOR real(sp):sp real(dp):dp

  !**********************************************************************!
  ! Stokes addition
  !**********************************************************************!

  type(stokes_<T>) function add_stokes_<T>(a,b) result(s)

    implicit none

    type(stokes_<T>),intent(in) :: a,b

    s%I = a%I + b%I
    s%Q = a%Q + b%Q
    s%U = a%U + b%U
    s%V = a%V + b%V

  end function add_stokes_<T>

  !**********************************************************************!
  ! Stokes subtraction
  !**********************************************************************!

  type(stokes_<T>) function sub_stokes_<T>(a,b) result(s)

    implicit none

    type(stokes_<T>),intent(in) :: a,b

    s%I = a%I - b%I
    s%Q = a%Q - b%Q
    s%U = a%U - b%U
    s%V = a%V - b%V

  end function sub_stokes_<T>

  !**********************************************************************!
  ! Scalar * Stokes
  !**********************************************************************!

  type(stokes_<T>) function scalar_stokes_mult_<T>(a,b) result(s)

    implicit none

    @T,intent(in)               :: a
    type(stokes_<T>),intent(in) :: b

    s%I = a * b%I
    s%Q = a * b%Q
    s%U = a * b%U
    s%V = a * b%V

  end function scalar_stokes_mult_<T>

  type(stokes_<T>) function stokes_scalar_mult_<T>(a,b) result(s)

    implicit none

    type(stokes_<T>),intent(in) :: a
    @T,intent(in)               :: b

    s%I = a%I * b
    s%Q = a%Q * b
    s%U = a%U * b
    s%V = a%V * b

  end function stokes_scalar_mult_<T>

  !**********************************************************************!
  ! Scalar / Stokes
  !**********************************************************************!

  type(stokes_<T>) function scalar_stokes_div_<T>(a,b) result(s)

    implicit none

    @T,intent(in)               :: a
    type(stokes_<T>),intent(in) :: b

    s%I = a / b%I
    s%Q = a / b%Q
    s%U = a / b%U
    s%V = a / b%V

  end function scalar_stokes_div_<T>

  type(stokes_<T>) function stokes_scalar_div_<T>(a,b) result(s)

    implicit none

    type(stokes_<T>),intent(in) :: a
    @T,intent(in)               :: b

    s%I = a%I / b
    s%Q = a%Q / b
    s%U = a%U / b
    s%V = a%V / b

  end function stokes_scalar_div_<T>

  !!@END FOR

end module type_stokes

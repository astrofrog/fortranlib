! MD5 of template: 1168e5117d09b9ae21ba9a1d637e9527
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
  end interface

  public :: operator(-)
  interface operator(-)
     module procedure sub_stokes_sp
     module procedure sub_stokes_dp
  end interface

  public :: operator(*)
  interface operator(*)
     module procedure scalar_stokes_mult_sp,stokes_scalar_mult_sp
     module procedure scalar_stokes_mult_dp,stokes_scalar_mult_dp
  end interface

  public :: operator(/)
  interface operator(/)
     module procedure scalar_stokes_div_sp,stokes_scalar_div_sp
     module procedure scalar_stokes_div_dp,stokes_scalar_div_dp
  end interface

contains


  !**********************************************************************!
  ! Stokes addition
  !**********************************************************************!

  type(stokes_dp) function add_stokes_dp(a,b) result(s)

    implicit none

    type(stokes_dp),intent(in) :: a,b

    s%I = a%I + b%I
    s%Q = a%Q + b%Q
    s%U = a%U + b%U
    s%V = a%V + b%V

  end function add_stokes_dp

  !**********************************************************************!
  ! Stokes subtraction
  !**********************************************************************!

  type(stokes_dp) function sub_stokes_dp(a,b) result(s)

    implicit none

    type(stokes_dp),intent(in) :: a,b

    s%I = a%I - b%I
    s%Q = a%Q - b%Q
    s%U = a%U - b%U
    s%V = a%V - b%V

  end function sub_stokes_dp

  !**********************************************************************!
  ! Scalar * Stokes
  !**********************************************************************!

  type(stokes_dp) function scalar_stokes_mult_dp(a,b) result(s)

    implicit none

    real(dp),intent(in)               :: a
    type(stokes_dp),intent(in) :: b

    s%I = a * b%I
    s%Q = a * b%Q
    s%U = a * b%U
    s%V = a * b%V

  end function scalar_stokes_mult_dp

  type(stokes_dp) function stokes_scalar_mult_dp(a,b) result(s)

    implicit none

    type(stokes_dp),intent(in) :: a
    real(dp),intent(in)               :: b

    s%I = a%I * b
    s%Q = a%Q * b
    s%U = a%U * b
    s%V = a%V * b

  end function stokes_scalar_mult_dp

  !**********************************************************************!
  ! Scalar / Stokes
  !**********************************************************************!

  type(stokes_dp) function scalar_stokes_div_dp(a,b) result(s)

    implicit none

    real(dp),intent(in)               :: a
    type(stokes_dp),intent(in) :: b

    s%I = a / b%I
    s%Q = a / b%Q
    s%U = a / b%U
    s%V = a / b%V

  end function scalar_stokes_div_dp

  type(stokes_dp) function stokes_scalar_div_dp(a,b) result(s)

    implicit none

    type(stokes_dp),intent(in) :: a
    real(dp),intent(in)               :: b

    s%I = a%I / b
    s%Q = a%Q / b
    s%U = a%U / b
    s%V = a%V / b

  end function stokes_scalar_div_dp
  

  !**********************************************************************!
  ! Stokes addition
  !**********************************************************************!

  type(stokes_sp) function add_stokes_sp(a,b) result(s)

    implicit none

    type(stokes_sp),intent(in) :: a,b

    s%I = a%I + b%I
    s%Q = a%Q + b%Q
    s%U = a%U + b%U
    s%V = a%V + b%V

  end function add_stokes_sp

  !**********************************************************************!
  ! Stokes subtraction
  !**********************************************************************!

  type(stokes_sp) function sub_stokes_sp(a,b) result(s)

    implicit none

    type(stokes_sp),intent(in) :: a,b

    s%I = a%I - b%I
    s%Q = a%Q - b%Q
    s%U = a%U - b%U
    s%V = a%V - b%V

  end function sub_stokes_sp

  !**********************************************************************!
  ! Scalar * Stokes
  !**********************************************************************!

  type(stokes_sp) function scalar_stokes_mult_sp(a,b) result(s)

    implicit none

    real(sp),intent(in)               :: a
    type(stokes_sp),intent(in) :: b

    s%I = a * b%I
    s%Q = a * b%Q
    s%U = a * b%U
    s%V = a * b%V

  end function scalar_stokes_mult_sp

  type(stokes_sp) function stokes_scalar_mult_sp(a,b) result(s)

    implicit none

    type(stokes_sp),intent(in) :: a
    real(sp),intent(in)               :: b

    s%I = a%I * b
    s%Q = a%Q * b
    s%U = a%U * b
    s%V = a%V * b

  end function stokes_scalar_mult_sp

  !**********************************************************************!
  ! Scalar / Stokes
  !**********************************************************************!

  type(stokes_sp) function scalar_stokes_div_sp(a,b) result(s)

    implicit none

    real(sp),intent(in)               :: a
    type(stokes_sp),intent(in) :: b

    s%I = a / b%I
    s%Q = a / b%Q
    s%U = a / b%U
    s%V = a / b%V

  end function scalar_stokes_div_sp

  type(stokes_sp) function stokes_scalar_div_sp(a,b) result(s)

    implicit none

    type(stokes_sp),intent(in) :: a
    real(sp),intent(in)               :: b

    s%I = a%I / b
    s%Q = a%Q / b
    s%U = a%U / b
    s%V = a%V / b

  end function stokes_scalar_div_sp
  

end module type_stokes

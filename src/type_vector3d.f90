! MD5 of template: 8258d7d7d899421e5c06c75777d6c651
! 3D vector related routines
! Thomas Robitaille (c) 2009

! Depends on lib_random

module type_vector3d

  implicit none
  save

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  public :: vector3d_sp
  type vector3d_sp
     real(sp) :: x,y,z
  end type vector3d_sp

  public :: vector3d_dp
  type vector3d_dp
     real(dp) :: x,y,z
  end type vector3d_dp

  public :: operator(+)
  interface operator(+)
     module procedure add_vector3d_sp
     module procedure add_vector3d_dp
  end interface

  public :: operator(-)
  interface operator(-)
     module procedure sub_vector3d_sp
     module procedure sub_vector3d_dp
  end interface

  public :: operator(.dot.)
  interface operator(.dot.)
     module procedure dot_product_sp
     module procedure dot_product_dp
  end interface

  public :: operator(.cross.)
  interface operator(.cross.)
     module procedure cross_product_sp
     module procedure cross_product_dp
  end interface

  public :: operator(*)
  interface operator(*)
     module procedure scalar_vector3d_mult_sp
     module procedure scalar_vector3d_mult_dp
     module procedure vector3d_scalar_mult_sp
     module procedure vector3d_scalar_mult_dp
  end interface

  public :: operator(/)
  interface operator(/)
     module procedure scalar_vector3d_div_sp
     module procedure scalar_vector3d_div_dp
     module procedure vector3d_scalar_div_sp
     module procedure vector3d_scalar_div_dp
  end interface

  public :: vector3d_to_angle3d
  interface vector3d_to_angle3d
     module procedure vector3d_to_angle3d_sp
     module procedure vector3d_to_angle3d_dp
  end interface

  public :: angle3d_to_vector3d
  interface angle3d_to_vector3d
     module procedure angle3d_to_vector3d_sp
     module procedure angle3d_to_vector3d_dp
  end interface

  public :: random_sphere_vector3d
  interface random_sphere_vector3d
     module procedure random_sphere_vector3d_sp
     module procedure random_sphere_vector3d_dp
  end interface

contains


  !**********************************************************************!
  ! Vector addition
  !**********************************************************************!

  type(vector3d_dp) function add_vector3d_dp(a,b) result(v)

    implicit none

    type(vector3d_dp),intent(in) :: a,b

    v%x = a%x + b%x
    v%y = a%y + b%y
    v%z = a%z + b%z

  end function add_vector3d_dp

  !**********************************************************************!
  ! Vector subtraction
  !**********************************************************************!

  type(vector3d_dp) function sub_vector3d_dp(a,b) result(v)

    implicit none

    type(vector3d_dp),intent(in) :: a,b

    v%x = a%x - b%x
    v%y = a%y - b%y
    v%z = a%z - b%z

  end function sub_vector3d_dp

  !**********************************************************************!
  ! Vector dot product
  !**********************************************************************!

  real(dp) function dot_product_dp(a,b) result(p)

    implicit none

    type(vector3d_dp),intent(in) :: a,b

    p = a%x*b%x + a%y*b%y + a%z*b%z

  end function dot_product_dp

  !**********************************************************************!
  ! Vector cross product
  !**********************************************************************!

  type(vector3d_dp) function cross_product_dp(a,b) result(p)

    implicit none

    type(vector3d_dp),intent(in) :: a,b

    p%x = a%y*b%z - a%z*b%y
    p%y = a%z*b%x - a%x*b%z
    p%z = a%x*b%y - a%y*b%x

  end function cross_product_dp

  !**********************************************************************!
  ! Scalar * Vector
  !**********************************************************************!

  type(vector3d_dp) function scalar_vector3d_mult_dp(a,b) result(v)

    implicit none

    real(dp),intent(in)          :: a
    type(vector3d_dp),intent(in) :: b

    v%x = a * b%x
    v%y = a * b%y
    v%z = a * b%z

  end function scalar_vector3d_mult_dp

  type(vector3d_dp) function vector3d_scalar_mult_dp(a,b) result(v)

    implicit none

    type(vector3d_dp),intent(in) :: a
    real(dp),intent(in)        :: b

    v%x = a%x * b
    v%y = a%y * b
    v%z = a%z * b

  end function vector3d_scalar_mult_dp

  !**********************************************************************!
  ! Scalar / Vector
  !**********************************************************************!

  type(vector3d_dp) function scalar_vector3d_div_dp(a,b) result(v)

    implicit none

    real(dp),intent(in)          :: a
    type(vector3d_dp),intent(in) :: b

    v%x = a / b%x
    v%y = a / b%y
    v%z = a / b%z

  end function scalar_vector3d_div_dp

  type(vector3d_dp) function vector3d_scalar_div_dp(a,b) result(v)

    implicit none

    type(vector3d_dp),intent(in) :: a
    real(dp),intent(in)          :: b

    v%x = a%x / b
    v%y = a%y / b
    v%z = a%z / b

  end function vector3d_scalar_div_dp

  !**********************************************************************!
  ! Vector to angle and vice-versa
  !**********************************************************************!

  subroutine vector3d_to_angle3d_dp(v,a)

    use type_angle3d

    implicit none

    type(vector3d_dp),intent(in) :: v
    ! input 3d vector

    type(angle3d_dp),intent(out) :: a
    ! output 3d angle

    real(dp) :: small_r,big_r

    small_r = sqrt( v%x * v%x + v%y * v%y )
    big_r   = sqrt( v%x * v%x + v%y * v%y + v%z * v%z )

    a%cosp = v%x / small_r
    a%sinp = v%y / small_r

    a%cost = v%z / big_r
    a%sint = small_r / big_r 

  end subroutine vector3d_to_angle3d_dp

  subroutine angle3d_to_vector3d_dp(a,v)

    use type_angle3d

    implicit none

    type(angle3d_dp),intent(in) :: a
    ! input 3d angle

    type(vector3d_dp),intent(out) :: v
    ! output 3d vector

    v%x = a%sint * a%cosp
    v%y = a%sint * a%sinp
    v%z = a%cost 

  end subroutine angle3d_to_vector3d_dp

  !**********************************************************************!
  ! Random position on a unit sphere
  !**********************************************************************!

  subroutine random_sphere_vector3d_dp(v)
    use lib_random
    implicit none
    type(vector3d_dp),intent(out) :: v
    real(dp) :: mu,phi,radius_cut
    call random_sphere(mu,phi)
    radius_cut = sqrt(1._dp-mu*mu)
    v%x = radius_cut * cos(phi)
    v%y = radius_cut * sin(phi)
    v%z = mu
  end subroutine random_sphere_vector3d_dp


  !**********************************************************************!
  ! Vector addition
  !**********************************************************************!

  type(vector3d_sp) function add_vector3d_sp(a,b) result(v)

    implicit none

    type(vector3d_sp),intent(in) :: a,b

    v%x = a%x + b%x
    v%y = a%y + b%y
    v%z = a%z + b%z

  end function add_vector3d_sp

  !**********************************************************************!
  ! Vector subtraction
  !**********************************************************************!

  type(vector3d_sp) function sub_vector3d_sp(a,b) result(v)

    implicit none

    type(vector3d_sp),intent(in) :: a,b

    v%x = a%x - b%x
    v%y = a%y - b%y
    v%z = a%z - b%z

  end function sub_vector3d_sp

  !**********************************************************************!
  ! Vector dot product
  !**********************************************************************!

  real(sp) function dot_product_sp(a,b) result(p)

    implicit none

    type(vector3d_sp),intent(in) :: a,b

    p = a%x*b%x + a%y*b%y + a%z*b%z

  end function dot_product_sp

  !**********************************************************************!
  ! Vector cross product
  !**********************************************************************!

  type(vector3d_sp) function cross_product_sp(a,b) result(p)

    implicit none

    type(vector3d_sp),intent(in) :: a,b

    p%x = a%y*b%z - a%z*b%y
    p%y = a%z*b%x - a%x*b%z
    p%z = a%x*b%y - a%y*b%x

  end function cross_product_sp

  !**********************************************************************!
  ! Scalar * Vector
  !**********************************************************************!

  type(vector3d_sp) function scalar_vector3d_mult_sp(a,b) result(v)

    implicit none

    real(sp),intent(in)          :: a
    type(vector3d_sp),intent(in) :: b

    v%x = a * b%x
    v%y = a * b%y
    v%z = a * b%z

  end function scalar_vector3d_mult_sp

  type(vector3d_sp) function vector3d_scalar_mult_sp(a,b) result(v)

    implicit none

    type(vector3d_sp),intent(in) :: a
    real(dp),intent(in)        :: b

    v%x = a%x * b
    v%y = a%y * b
    v%z = a%z * b

  end function vector3d_scalar_mult_sp

  !**********************************************************************!
  ! Scalar / Vector
  !**********************************************************************!

  type(vector3d_sp) function scalar_vector3d_div_sp(a,b) result(v)

    implicit none

    real(sp),intent(in)          :: a
    type(vector3d_sp),intent(in) :: b

    v%x = a / b%x
    v%y = a / b%y
    v%z = a / b%z

  end function scalar_vector3d_div_sp

  type(vector3d_sp) function vector3d_scalar_div_sp(a,b) result(v)

    implicit none

    type(vector3d_sp),intent(in) :: a
    real(sp),intent(in)          :: b

    v%x = a%x / b
    v%y = a%y / b
    v%z = a%z / b

  end function vector3d_scalar_div_sp

  !**********************************************************************!
  ! Vector to angle and vice-versa
  !**********************************************************************!

  subroutine vector3d_to_angle3d_sp(v,a)

    use type_angle3d

    implicit none

    type(vector3d_sp),intent(in) :: v
    ! input 3d vector

    type(angle3d_sp),intent(out) :: a
    ! output 3d angle

    real(sp) :: small_r,big_r

    small_r = sqrt( v%x * v%x + v%y * v%y )
    big_r   = sqrt( v%x * v%x + v%y * v%y + v%z * v%z )

    a%cosp = v%x / small_r
    a%sinp = v%y / small_r

    a%cost = v%z / big_r
    a%sint = small_r / big_r 

  end subroutine vector3d_to_angle3d_sp

  subroutine angle3d_to_vector3d_sp(a,v)

    use type_angle3d

    implicit none

    type(angle3d_sp),intent(in) :: a
    ! input 3d angle

    type(vector3d_sp),intent(out) :: v
    ! output 3d vector

    v%x = a%sint * a%cosp
    v%y = a%sint * a%sinp
    v%z = a%cost 

  end subroutine angle3d_to_vector3d_sp

  !**********************************************************************!
  ! Random position on a unit sphere
  !**********************************************************************!

  subroutine random_sphere_vector3d_sp(v)
    use lib_random
    implicit none
    type(vector3d_sp),intent(out) :: v
    real(sp) :: mu,phi,radius_cut
    call random_sphere(mu,phi)
    radius_cut = sqrt(1._sp-mu*mu)
    v%x = radius_cut * cos(phi)
    v%y = radius_cut * sin(phi)
    v%z = mu
  end subroutine random_sphere_vector3d_sp


end module type_vector3d

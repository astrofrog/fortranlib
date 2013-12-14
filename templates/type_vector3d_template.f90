! 3D vector related routines
!
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

  public :: operator(.eq.)
  interface operator(.eq.)
     module procedure equal_sp
     module procedure equal_dp
  end interface operator(.eq.)

  public :: operator(+)
  interface operator(+)
     module procedure add_vector3d_sp
     module procedure add_vector3d_dp
  end interface operator(+)

  public :: operator(-)
  interface operator(-)
     module procedure sub_vector3d_sp
     module procedure sub_vector3d_dp
     module procedure minus_vector3d_sp
     module procedure minus_vector3d_dp
  end interface operator(-)

  public :: operator(.dot.)
  interface operator(.dot.)
     module procedure dot_product_sp
     module procedure dot_product_dp
  end interface operator(.dot.)

  public :: operator(.cross.)
  interface operator(.cross.)
     module procedure cross_product_sp
     module procedure cross_product_dp
  end interface operator(.cross.)

  public :: operator(*)
  interface operator(*)
     module procedure scalar_vector3d_mult_sp
     module procedure scalar_vector3d_mult_dp
     module procedure vector3d_scalar_mult_sp
     module procedure vector3d_scalar_mult_dp
  end interface operator(*)

  public :: operator(/)
  interface operator(/)
     module procedure scalar_vector3d_div_sp
     module procedure scalar_vector3d_div_dp
     module procedure vector3d_scalar_div_sp
     module procedure vector3d_scalar_div_dp
  end interface operator(/)

  public :: vector3d_to_angle3d
  interface vector3d_to_angle3d
     module procedure vector3d_to_angle3d_sp
     module procedure vector3d_to_angle3d_dp
  end interface vector3d_to_angle3d

  public :: angle3d_to_vector3d
  interface angle3d_to_vector3d
     module procedure angle3d_to_vector3d_sp
     module procedure angle3d_to_vector3d_dp
  end interface angle3d_to_vector3d

  public :: random_sphere_vector3d
  interface random_sphere_vector3d
     module procedure random_sphere_vector3d_sp
     module procedure random_sphere_vector3d_dp
  end interface random_sphere_vector3d

contains

  !!@FOR real(sp):sp real(dp):dp

  !**********************************************************************!
  ! Test equality
  !**********************************************************************!

  logical function equal_<T>(a, b) result(e)

    implicit none

    type(vector3d_<T>),intent(in) :: a, b

    e = a%x == b%x .and. a%y == b%y .and. a%z == b%z

  end function equal_<T>

  !**********************************************************************!
  ! Vector addition
  !**********************************************************************!

  type(vector3d_<T>) function add_vector3d_<T>(a,b) result(v)

    implicit none

    type(vector3d_<T>),intent(in) :: a,b

    v%x = a%x + b%x
    v%y = a%y + b%y
    v%z = a%z + b%z

  end function add_vector3d_<T>

  !**********************************************************************!
  ! Vector subtraction
  !**********************************************************************!

  type(vector3d_<T>) function sub_vector3d_<T>(a,b) result(v)

    implicit none

    type(vector3d_<T>),intent(in) :: a,b

    v%x = a%x - b%x
    v%y = a%y - b%y
    v%z = a%z - b%z

  end function sub_vector3d_<T>

  !**********************************************************************!
  ! Unary minus for vectors
  !**********************************************************************!

  type(vector3d_<T>) function minus_vector3d_<T>(a) result(v)

    implicit none

    type(vector3d_<T>),intent(in) :: a

    v%x = -a%x
    v%y = -a%y
    v%z = -a%z

  end function minus_vector3d_<T>

  !**********************************************************************!
  ! Vector dot product
  !**********************************************************************!

  real(<T>) function dot_product_<T>(a,b) result(p)

    implicit none

    type(vector3d_<T>),intent(in) :: a,b

    p = a%x*b%x + a%y*b%y + a%z*b%z

  end function dot_product_<T>

  !**********************************************************************!
  ! Vector cross product
  !**********************************************************************!

  type(vector3d_<T>) function cross_product_<T>(a,b) result(p)

    implicit none

    type(vector3d_<T>),intent(in) :: a,b

    p%x = a%y*b%z - a%z*b%y
    p%y = a%z*b%x - a%x*b%z
    p%z = a%x*b%y - a%y*b%x

  end function cross_product_<T>

  !**********************************************************************!
  ! Scalar * Vector
  !**********************************************************************!

  type(vector3d_<T>) function scalar_vector3d_mult_<T>(a,b) result(v)

    implicit none

    real(<T>),intent(in)          :: a
    type(vector3d_<T>),intent(in) :: b

    v%x = a * b%x
    v%y = a * b%y
    v%z = a * b%z

  end function scalar_vector3d_mult_<T>

  type(vector3d_<T>) function vector3d_scalar_mult_<T>(a,b) result(v)

    implicit none

    type(vector3d_<T>),intent(in) :: a
    real(dp),intent(in)        :: b

    v%x = a%x * b
    v%y = a%y * b
    v%z = a%z * b

  end function vector3d_scalar_mult_<T>

  !**********************************************************************!
  ! Scalar / Vector
  !**********************************************************************!

  type(vector3d_<T>) function scalar_vector3d_div_<T>(a,b) result(v)

    implicit none

    real(<T>),intent(in)          :: a
    type(vector3d_<T>),intent(in) :: b

    v%x = a / b%x
    v%y = a / b%y
    v%z = a / b%z

  end function scalar_vector3d_div_<T>

  type(vector3d_<T>) function vector3d_scalar_div_<T>(a,b) result(v)

    implicit none

    type(vector3d_<T>),intent(in) :: a
    real(<T>),intent(in)          :: b

    v%x = a%x / b
    v%y = a%y / b
    v%z = a%z / b

  end function vector3d_scalar_div_<T>

  !**********************************************************************!
  ! Vector to angle and vice-versa
  !**********************************************************************!

  subroutine vector3d_to_angle3d_<T>(v,a)

    use type_angle3d

    implicit none

    type(vector3d_<T>),intent(in) :: v
    ! input 3d vector

    type(angle3d_<T>),intent(out) :: a
    ! output 3d angle

    real(<T>) :: small_r,big_r

    small_r = sqrt( v%x * v%x + v%y * v%y )
    big_r   = sqrt( v%x * v%x + v%y * v%y + v%z * v%z )

    a%cosp = v%x / small_r
    a%sinp = v%y / small_r

    a%cost = v%z / big_r
    a%sint = small_r / big_r

  end subroutine vector3d_to_angle3d_<T>

  subroutine angle3d_to_vector3d_<T>(a,v)

    use type_angle3d

    implicit none

    type(angle3d_<T>),intent(in) :: a
    ! input 3d angle

    type(vector3d_<T>),intent(out) :: v
    ! output 3d vector

    v%x = a%sint * a%cosp
    v%y = a%sint * a%sinp
    v%z = a%cost

  end subroutine angle3d_to_vector3d_<T>

  !**********************************************************************!
  ! Random position on a unit sphere
  !**********************************************************************!

  subroutine random_sphere_vector3d_<T>(v)
    use lib_random
    implicit none
    type(vector3d_<T>),intent(out) :: v
    real(<T>) :: mu,phi,radius_cut
    call random_sphere(mu,phi)
    radius_cut = sqrt(1._<T>-mu*mu)
    v%x = radius_cut * cos(phi)
    v%y = radius_cut * sin(phi)
    v%z = mu
  end subroutine random_sphere_vector3d_<T>

  !!@END FOR

end module type_vector3d

! MD5 of template: 4487f0f616ecccd76ea2b91ab000241b
! 3D angle related routines
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

module type_angle3d

  implicit none
  save

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  real(dp),parameter  :: pi = 3.14159265358979323846_dp
  real(dp),parameter  :: deg2rad = pi / 180._dp
  real(dp),parameter  :: rad2deg = 180._dp / pi

  public :: angle3d_sp
  type angle3d_sp
     real(sp) :: cost,sint,cosp,sinp
  end type angle3d_sp

  public :: angle3d_dp
  type angle3d_dp
     real(dp) :: cost,sint,cosp,sinp
  end type angle3d_dp

  public :: operator(.eq.)
  interface operator(.eq.)
     module procedure equal_sp
     module procedure equal_dp
  end interface operator(.eq.)

  public :: angle3d_deg
  interface angle3d_deg
     module procedure angle3d_deg_sp
     module procedure angle3d_deg_dp
  end interface angle3d_deg

  public :: display_angle
  interface display_angle
     module procedure display_angle_sp
     module procedure display_angle_dp
  end interface display_angle

  public :: operator(.dot.)
  interface operator(.dot.)
     module procedure dot_product_sp
     module procedure dot_product_dp
  end interface operator(.dot.)

  public :: rotate_angle3d
  interface rotate_angle3d
     module procedure rotate_angle3d_sp
     module procedure rotate_angle3d_dp
  end interface rotate_angle3d

  public :: difference_angle3d
  interface difference_angle3d
     module procedure difference_angle3d_sp
     module procedure difference_angle3d_dp
  end interface difference_angle3d

  public :: random_sphere_angle3d
  interface random_sphere_angle3d
     module procedure random_sphere_angle3d_sp
     module procedure random_sphere_angle3d_dp
  end interface random_sphere_angle3d

  public :: operator(-)
  interface operator(-)
     module procedure minus_angle_sp
     module procedure minus_angle_dp
  end interface operator(-)

  interface sin2cos
     module procedure sin2cos_sp
     module procedure sin2cos_dp
  end interface sin2cos

  interface cos2sin
     module procedure sin2cos_sp
     module procedure sin2cos_dp
  end interface cos2sin

contains



  logical function equal_dp(a, b) result(e)

    implicit none

    type(angle3d_dp),intent(in) :: a, b

    e = a%cost == b%cost .and. a%sint == b%sint .and. a%cosp == b%cosp .and. a%sinp == b%sinp

  end function equal_dp

  type(angle3d_dp) function angle3d_deg_dp(theta,phi) result(a)

    implicit none

    real(dp),intent(in) :: theta,phi

    a%cost = cos(theta*deg2rad)
    a%sint = sin(theta*deg2rad)
    a%cosp = cos(phi*deg2rad)
    a%sinp = sin(phi*deg2rad)

  end function angle3d_deg_dp


  subroutine display_angle_dp(a)
    implicit none
    type(angle3d_dp),intent(in) :: a
    print '("Theta = ",F8.4," degrees")',atan2(a%sint,a%cost)*rad2deg
    print '("Phi   = ",F8.4," degrees")',atan2(a%sinp,a%cosp)*rad2deg
  end subroutine display_angle_dp


  real(dp) function dot_product_dp(a, b) result(p)

    implicit none

    type(angle3d_dp),intent(in) :: a, b

    p = a%sint*a%cosp*b%sint*b%cosp + a%sint*a%sinp*b%sint*b%sinp + a%cost * b%cost

  end function dot_product_dp


  subroutine rotate_angle3d_dp(a_local,a_coord,a_final)

    ! This subroutine is used to add a local angle, such as as
    ! photon emission angle on the surface of a star, or a photon
    ! scattering angle, to an already existing position angle.
    ! The former is given by a_local, the latter, by a_coord, and
    ! the final angle by a_angle. We solve this using spherical
    ! trigonometry. Consider a spherical triangle with corner angles
    ! (A,B,C) and side angles (a,b,c). The angle B is attached to the
    ! z axis, and the sides a and c are on the great circles passing
    ! through the z-axis. The meaning of the angles is as follows:

    ! a =   old theta angle (initial direction angle)
    ! b = local theta angle (scattering or emission angle)
    ! c =   new theta angle (final direction angle)

    ! A = no meaning (but useful for scattering)
    ! B = new phi - old phi
    ! C = local phi angle (scattering or emission angle)

    implicit none

    type(angle3d_dp),intent(in) :: a_local
    type(angle3d_dp),intent(in) :: a_coord
    type(angle3d_dp),intent(out) :: a_final

    real(dp) :: cos_a,sin_a
    real(dp) :: cos_b,sin_b
    real(dp) :: cos_c,sin_c

    real(dp) :: cos_big_b,sin_big_b
    real(dp) :: cos_big_c,sin_big_c

    real(dp) :: delta
    logical :: same_sign

    ! Special case - if coord%theta is 0, then final = local
    if(abs(a_coord%sint) < 1.e-10_dp) then
       if(a_coord%cost > 0._dp) then
          a_final = a_local
          a_final%cosp = + a_local%cosp * a_coord%cosp + a_local%sinp * a_coord%sinp
          a_final%sinp = + a_local%cosp * a_coord%sinp - a_local%sinp * a_coord%cosp
       else
          a_final = a_local
          a_final%cost = - a_local%cost
          a_final%cosp = + a_local%cosp * a_coord%cosp - a_local%sinp * a_coord%sinp
          a_final%sinp = + a_local%cosp * a_coord%sinp + a_local%sinp * a_coord%cosp
       end if
       return
    end if

    ! --- Assign spherical triangle angles values --- !

    ! The angles in the spherical triangle are as follows:

    cos_a = a_coord%cost
    sin_a = a_coord%sint

    cos_b = a_local%cost
    sin_b = a_local%sint

    if(a_local%sinp < 0._dp) then ! the angle in the triangle is then actually 2*pi - local phi
       cos_big_C = + a_local%cosp
       sin_big_C = - a_local%sinp
    else ! the angle is local phi
       cos_big_C = + a_local%cosp
       sin_big_C = + a_local%sinp
    end if

    ! --- Solve the spherical triangle --- !

    if (abs(sin_a) > abs(cos_a)) then
       same_sign = sin_a > 0._dp .eqv. sin_b > 0._dp
       delta = cos_b - cos_a
    else
       same_sign = cos_a > 0._dp .eqv. cos_b > 0._dp
       delta = sin_b - sin_a
    end if

    if(same_sign .and. abs(delta) < 1.e-5_dp .and. sin_big_C < 1.e-5_dp .and. cos_big_c > 0._dp) then
       if (abs(sin_a) > abs(cos_a)) then
          sin_c = sqrt(delta * delta * (1._dp + (cos_a/sin_a)**2) + sin_a * sin_b * sin_big_C * sin_big_C)
       else
          sin_c = sqrt(delta * delta * (1._dp + (sin_a/cos_a)**2) + sin_a * sin_b * sin_big_C * sin_big_C)
       end if
       cos_c = sin2cos(sin_c)
    else
       cos_c = cos_a * cos_b + sin_a * sin_b * cos_big_c
       sin_c = cos2sin(cos_c)
    end if

    ! Special case - if local and coord theta are the same and C = 0, return
    ! vertical vector. We can't do better than that because we are limited by
    ! numerical precision, in particular for delta (above) which will be limited
    ! in precision
    if(abs(sin_c) < 1.e-10_dp) then
       write(*,'(" WARNING: final angle is vertical, and phi is undetermined (set to 0) [rotate_angle3d]")')
       if(cos_c > 0._dp) then
          a_final = angle3d_deg(0._dp,0._dp)
       else
          a_final = angle3d_deg(180._dp,0._dp)
       end if
       return
    end if

    cos_big_b = ( cos_b - cos_a * cos_c ) / ( sin_a * sin_c )
    sin_big_b = + sin_big_c * sin_b / sin_c

    ! --- Find final theta and phi values --- !

    a_final%cost = cos_c
    a_final%sint = sin_c

    if(a_local%sinp < 0._dp) then ! the top angle is old phi - new phi
       a_final%cosp = + cos_big_b * a_coord%cosp + sin_big_b * a_coord%sinp
       a_final%sinp = + cos_big_b * a_coord%sinp - sin_big_b * a_coord%cosp
    else ! the top angle is new phi - old phi
       a_final%cosp = + cos_big_b * a_coord%cosp - sin_big_b * a_coord%sinp
       a_final%sinp = + cos_big_b * a_coord%sinp + sin_big_b * a_coord%cosp
    end if

  end subroutine rotate_angle3d_dp

  subroutine difference_angle3d_dp(a_coord,a_final,a_local)

    ! This subroutine is used to find the local angle by which
    ! a coordinate angle would have to be rotated to give the
    ! final angle specified. This is complementary to the rotate_
    ! angle3d routine.

    ! We solve this using spherical
    ! trigonometry. Consider a spherical triangle with corner angles
    ! (A,B,C) and side angles (a,b,c). The angle B is attached to the
    ! z axis, and the sides a and c are on the great circles passing
    ! through the z-axis. The meaning of the angles is as follows:

    ! a =   old theta angle (initial direction angle)
    ! b = local theta angle (scattering or emission angle)
    ! c =   new theta angle (final direction angle)

    ! A = no meaning (but useful for scattering)
    ! B = new phi - old phi
    ! C = local phi angle (scattering or emission angle)

    implicit none

    type(angle3d_dp),intent(out) :: a_local
    type(angle3d_dp),intent(in) :: a_coord
    type(angle3d_dp),intent(in) :: a_final

    real(dp) :: cos_a,sin_a
    real(dp) :: cos_b,sin_b
    real(dp) :: cos_c,sin_c

    real(dp) :: cos_big_b,sin_big_b
    real(dp) :: cos_big_c,sin_big_c

    real(dp) :: delta,diff
    logical :: same_sign

    ! Special case - if coord%theta is 0, then final = local
    if(abs(a_coord%sint) < 1.e-10_dp) then
       if(a_coord%cost > 0._dp) then
          a_local = a_final
          a_local%cosp = + a_coord%cosp * a_final%cosp + a_coord%sinp * a_final%sinp
          a_local%sinp = - a_coord%cosp * a_final%sinp + a_coord%sinp * a_final%cosp
       else
          a_local = a_final
          a_local%cost = - a_local%cost
          a_local%cosp = + a_coord%cosp * a_final%cosp + a_coord%sinp * a_final%sinp
          a_local%sinp = + a_coord%cosp * a_final%sinp - a_coord%sinp * a_final%cosp
       end if
       return
    end if

    ! --- Assign spherical triangle angles values --- !

    ! The angles in the spherical triangle are as follows:

    cos_a = a_coord%cost
    sin_a = a_coord%sint

    cos_c = a_final%cost
    sin_c = a_final%sint

    cos_big_B = a_coord%cosp * a_final%cosp + a_coord%sinp * a_final%sinp
    sin_big_B = a_coord%sinp * a_final%cosp - a_coord%cosp * a_final%sinp

    ! --- Solve the spherical triangle --- !

    cos_b = cos_a * cos_c + sin_a * sin_c * cos_big_B
    sin_b = cos2sin(cos_b)

    ! If cos_b is -1, then the angles are opposite and phi is undefined
    if(abs(cos_b + 1._dp) < 1.e-10_dp) then
       a_local = angle3d_deg(180._dp,0._dp)
       return
    end if

    ! If cos_b is +1, then the angles are the same and phi is undefined
    if(abs(cos_b - 1._dp) < 1.e-10_dp) then
       a_local = angle3d_deg(0._dp,0._dp)
       return
    end if

    same_sign = cos_a > 0._dp .eqv. cos_b > 0._dp .and. sin_a > 0._dp .eqv. sin_b > 0._dp
    if(abs(sin_a) > abs(cos_a)) then
       delta = cos_b - cos_a
    else
       delta = sin_b - sin_a
    end if

    if(same_sign .and. abs(delta) < 1.e-5_dp .and. sin_c < 1.e-5_dp) then

       if (abs(sin_a) > abs(cos_a)) then
          diff = (sin_c * sin_c - delta * delta * (1._dp + (cos_a / sin_a)**2)) / (sin_a * sin_b)
       else
          diff = (sin_c * sin_c - delta * delta * (1._dp + (sin_a / cos_a)**2)) / (sin_a * sin_b)
       end if

       if(diff >= 0._dp) then
          sin_big_c = sqrt(diff)
       else
          sin_big_c = 0._dp
       end if

       if(cos_c > 0._dp) then
          cos_big_c = sin2cos(sin_big_c)
       else
          cos_big_c = - sin2cos(sin_big_c)
       end if

    else
       sin_big_c = + abs(sin_big_b) * sin_c / sin_b
       cos_big_c = ( cos_c - cos_a * cos_b ) / ( sin_a * sin_b )
    end if

    ! If sin_big_c is zero, this can cause issues in other routines, so we
    ! make it the next floating point, which should have no effect on
    ! calculations but prevents issues.
    if(sin_big_c == 0._dp) sin_big_c = tiny(1._dp)

    ! --- Find final theta and phi values --- !

    a_local%cost = cos_b
    a_local%sint = sin_b

    if(sin_big_b < 0._dp) then

       a_local%cosp = cos_big_C
       a_local%sinp = sin_big_C

    else

       a_local%cosp = cos_big_C
       a_local%sinp = - sin_big_C

    end if

  end subroutine difference_angle3d_dp

  real(dp) function sin2cos_dp(x) result(y)
    implicit none
    real(dp), intent(in) :: x
    if(x * x < 1._dp) then
       y = sqrt(1._dp - x * x)
    else
       y = 0._dp
    end if
  end function sin2cos_dp

  subroutine random_sphere_angle3d_dp(a)
    ! Random position on a unit sphere
    use lib_random
    implicit none
    type(angle3d_dp),intent(out) :: a
    real(dp) :: phi
    call random_sphere(a%cost,phi)
    a%sint = sqrt(1._dp - a%cost * a%cost)
    a%cosp = cos(phi)
    a%sinp = sin(phi)
  end subroutine random_sphere_angle3d_dp

  type(angle3d_dp) function minus_angle_dp(a) result(b)
    implicit none
    type(angle3d_dp),intent(in) :: a
    b = angle3d_dp(-a%cost, a%sint, -a%cosp, -a%sinp)
  end function minus_angle_dp


  logical function equal_sp(a, b) result(e)

    implicit none

    type(angle3d_sp),intent(in) :: a, b

    e = a%cost == b%cost .and. a%sint == b%sint .and. a%cosp == b%cosp .and. a%sinp == b%sinp

  end function equal_sp

  type(angle3d_sp) function angle3d_deg_sp(theta,phi) result(a)

    implicit none

    real(sp),intent(in) :: theta,phi

    a%cost = cos(theta*deg2rad)
    a%sint = sin(theta*deg2rad)
    a%cosp = cos(phi*deg2rad)
    a%sinp = sin(phi*deg2rad)

  end function angle3d_deg_sp


  subroutine display_angle_sp(a)
    implicit none
    type(angle3d_sp),intent(in) :: a
    print '("Theta = ",F8.4," degrees")',atan2(a%sint,a%cost)*rad2deg
    print '("Phi   = ",F8.4," degrees")',atan2(a%sinp,a%cosp)*rad2deg
  end subroutine display_angle_sp


  real(sp) function dot_product_sp(a, b) result(p)

    implicit none

    type(angle3d_sp),intent(in) :: a, b

    p = a%sint*a%cosp*b%sint*b%cosp + a%sint*a%sinp*b%sint*b%sinp + a%cost * b%cost

  end function dot_product_sp


  subroutine rotate_angle3d_sp(a_local,a_coord,a_final)

    ! This subroutine is used to add a local angle, such as as
    ! photon emission angle on the surface of a star, or a photon
    ! scattering angle, to an already existing position angle.
    ! The former is given by a_local, the latter, by a_coord, and
    ! the final angle by a_angle. We solve this using spherical
    ! trigonometry. Consider a spherical triangle with corner angles
    ! (A,B,C) and side angles (a,b,c). The angle B is attached to the
    ! z axis, and the sides a and c are on the great circles passing
    ! through the z-axis. The meaning of the angles is as follows:

    ! a =   old theta angle (initial direction angle)
    ! b = local theta angle (scattering or emission angle)
    ! c =   new theta angle (final direction angle)

    ! A = no meaning (but useful for scattering)
    ! B = new phi - old phi
    ! C = local phi angle (scattering or emission angle)

    implicit none

    type(angle3d_sp),intent(in) :: a_local
    type(angle3d_sp),intent(in) :: a_coord
    type(angle3d_sp),intent(out) :: a_final

    real(sp) :: cos_a,sin_a
    real(sp) :: cos_b,sin_b
    real(sp) :: cos_c,sin_c

    real(sp) :: cos_big_b,sin_big_b
    real(sp) :: cos_big_c,sin_big_c

    real(sp) :: delta
    logical :: same_sign

    ! Special case - if coord%theta is 0, then final = local
    if(abs(a_coord%sint) < 1.e-10_sp) then
       if(a_coord%cost > 0._sp) then
          a_final = a_local
          a_final%cosp = + a_local%cosp * a_coord%cosp + a_local%sinp * a_coord%sinp
          a_final%sinp = + a_local%cosp * a_coord%sinp - a_local%sinp * a_coord%cosp
       else
          a_final = a_local
          a_final%cost = - a_local%cost
          a_final%cosp = + a_local%cosp * a_coord%cosp - a_local%sinp * a_coord%sinp
          a_final%sinp = + a_local%cosp * a_coord%sinp + a_local%sinp * a_coord%cosp
       end if
       return
    end if

    ! --- Assign spherical triangle angles values --- !

    ! The angles in the spherical triangle are as follows:

    cos_a = a_coord%cost
    sin_a = a_coord%sint

    cos_b = a_local%cost
    sin_b = a_local%sint

    if(a_local%sinp < 0._sp) then ! the angle in the triangle is then actually 2*pi - local phi
       cos_big_C = + a_local%cosp
       sin_big_C = - a_local%sinp
    else ! the angle is local phi
       cos_big_C = + a_local%cosp
       sin_big_C = + a_local%sinp
    end if

    ! --- Solve the spherical triangle --- !

    if (abs(sin_a) > abs(cos_a)) then
       same_sign = sin_a > 0._sp .eqv. sin_b > 0._sp
       delta = cos_b - cos_a
    else
       same_sign = cos_a > 0._sp .eqv. cos_b > 0._sp
       delta = sin_b - sin_a
    end if

    if(same_sign .and. abs(delta) < 1.e-5_sp .and. sin_big_C < 1.e-5_sp .and. cos_big_c > 0._sp) then
       if (abs(sin_a) > abs(cos_a)) then
          sin_c = sqrt(delta * delta * (1._sp + (cos_a/sin_a)**2) + sin_a * sin_b * sin_big_C * sin_big_C)
       else
          sin_c = sqrt(delta * delta * (1._sp + (sin_a/cos_a)**2) + sin_a * sin_b * sin_big_C * sin_big_C)
       end if
       cos_c = sin2cos(sin_c)
    else
       cos_c = cos_a * cos_b + sin_a * sin_b * cos_big_c
       sin_c = cos2sin(cos_c)
    end if

    ! Special case - if local and coord theta are the same and C = 0, return
    ! vertical vector. We can't do better than that because we are limited by
    ! numerical precision, in particular for delta (above) which will be limited
    ! in precision
    if(abs(sin_c) < 1.e-10_sp) then
       write(*,'(" WARNING: final angle is vertical, and phi is undetermined (set to 0) [rotate_angle3d]")')
       if(cos_c > 0._sp) then
          a_final = angle3d_deg(0._sp,0._sp)
       else
          a_final = angle3d_deg(180._sp,0._sp)
       end if
       return
    end if

    cos_big_b = ( cos_b - cos_a * cos_c ) / ( sin_a * sin_c )
    sin_big_b = + sin_big_c * sin_b / sin_c

    ! --- Find final theta and phi values --- !

    a_final%cost = cos_c
    a_final%sint = sin_c

    if(a_local%sinp < 0._sp) then ! the top angle is old phi - new phi
       a_final%cosp = + cos_big_b * a_coord%cosp + sin_big_b * a_coord%sinp
       a_final%sinp = + cos_big_b * a_coord%sinp - sin_big_b * a_coord%cosp
    else ! the top angle is new phi - old phi
       a_final%cosp = + cos_big_b * a_coord%cosp - sin_big_b * a_coord%sinp
       a_final%sinp = + cos_big_b * a_coord%sinp + sin_big_b * a_coord%cosp
    end if

  end subroutine rotate_angle3d_sp

  subroutine difference_angle3d_sp(a_coord,a_final,a_local)

    ! This subroutine is used to find the local angle by which
    ! a coordinate angle would have to be rotated to give the
    ! final angle specified. This is complementary to the rotate_
    ! angle3d routine.

    ! We solve this using spherical
    ! trigonometry. Consider a spherical triangle with corner angles
    ! (A,B,C) and side angles (a,b,c). The angle B is attached to the
    ! z axis, and the sides a and c are on the great circles passing
    ! through the z-axis. The meaning of the angles is as follows:

    ! a =   old theta angle (initial direction angle)
    ! b = local theta angle (scattering or emission angle)
    ! c =   new theta angle (final direction angle)

    ! A = no meaning (but useful for scattering)
    ! B = new phi - old phi
    ! C = local phi angle (scattering or emission angle)

    implicit none

    type(angle3d_sp),intent(out) :: a_local
    type(angle3d_sp),intent(in) :: a_coord
    type(angle3d_sp),intent(in) :: a_final

    real(sp) :: cos_a,sin_a
    real(sp) :: cos_b,sin_b
    real(sp) :: cos_c,sin_c

    real(sp) :: cos_big_b,sin_big_b
    real(sp) :: cos_big_c,sin_big_c

    real(dp) :: delta,diff
    logical :: same_sign

    ! Special case - if coord%theta is 0, then final = local
    if(abs(a_coord%sint) < 1.e-10_sp) then
       if(a_coord%cost > 0._sp) then
          a_local = a_final
          a_local%cosp = + a_coord%cosp * a_final%cosp + a_coord%sinp * a_final%sinp
          a_local%sinp = - a_coord%cosp * a_final%sinp + a_coord%sinp * a_final%cosp
       else
          a_local = a_final
          a_local%cost = - a_local%cost
          a_local%cosp = + a_coord%cosp * a_final%cosp + a_coord%sinp * a_final%sinp
          a_local%sinp = + a_coord%cosp * a_final%sinp - a_coord%sinp * a_final%cosp
       end if
       return
    end if

    ! --- Assign spherical triangle angles values --- !

    ! The angles in the spherical triangle are as follows:

    cos_a = a_coord%cost
    sin_a = a_coord%sint

    cos_c = a_final%cost
    sin_c = a_final%sint

    cos_big_B = a_coord%cosp * a_final%cosp + a_coord%sinp * a_final%sinp
    sin_big_B = a_coord%sinp * a_final%cosp - a_coord%cosp * a_final%sinp

    ! --- Solve the spherical triangle --- !

    cos_b = cos_a * cos_c + sin_a * sin_c * cos_big_B
    sin_b = cos2sin(cos_b)

    ! If cos_b is -1, then the angles are opposite and phi is undefined
    if(abs(cos_b + 1._sp) < 1.e-10_sp) then
       a_local = angle3d_deg(180._sp,0._sp)
       return
    end if

    ! If cos_b is +1, then the angles are the same and phi is undefined
    if(abs(cos_b - 1._sp) < 1.e-10_sp) then
       a_local = angle3d_deg(0._sp,0._sp)
       return
    end if

    same_sign = cos_a > 0._sp .eqv. cos_b > 0._sp .and. sin_a > 0._sp .eqv. sin_b > 0._sp
    if(abs(sin_a) > abs(cos_a)) then
       delta = cos_b - cos_a
    else
       delta = sin_b - sin_a
    end if

    if(same_sign .and. abs(delta) < 1.e-5_sp .and. sin_c < 1.e-5_sp) then

       if (abs(sin_a) > abs(cos_a)) then
          diff = (sin_c * sin_c - delta * delta * (1._sp + (cos_a / sin_a)**2)) / (sin_a * sin_b)
       else
          diff = (sin_c * sin_c - delta * delta * (1._sp + (sin_a / cos_a)**2)) / (sin_a * sin_b)
       end if

       if(diff >= 0._sp) then
          sin_big_c = sqrt(diff)
       else
          sin_big_c = 0._sp
       end if

       if(cos_c > 0._sp) then
          cos_big_c = sin2cos(sin_big_c)
       else
          cos_big_c = - sin2cos(sin_big_c)
       end if

    else
       sin_big_c = + abs(sin_big_b) * sin_c / sin_b
       cos_big_c = ( cos_c - cos_a * cos_b ) / ( sin_a * sin_b )
    end if

    ! If sin_big_c is zero, this can cause issues in other routines, so we
    ! make it the next floating point, which should have no effect on
    ! calculations but prevents issues.
    if(sin_big_c == 0._sp) sin_big_c = tiny(1._sp)

    ! --- Find final theta and phi values --- !

    a_local%cost = cos_b
    a_local%sint = sin_b

    if(sin_big_b < 0._sp) then

       a_local%cosp = cos_big_C
       a_local%sinp = sin_big_C

    else

       a_local%cosp = cos_big_C
       a_local%sinp = - sin_big_C

    end if

  end subroutine difference_angle3d_sp

  real(sp) function sin2cos_sp(x) result(y)
    implicit none
    real(sp), intent(in) :: x
    if(x * x < 1._sp) then
       y = sqrt(1._sp - x * x)
    else
       y = 0._sp
    end if
  end function sin2cos_sp

  subroutine random_sphere_angle3d_sp(a)
    ! Random position on a unit sphere
    use lib_random
    implicit none
    type(angle3d_sp),intent(out) :: a
    real(sp) :: phi
    call random_sphere(a%cost,phi)
    a%sint = sqrt(1._sp - a%cost * a%cost)
    a%cosp = cos(phi)
    a%sinp = sin(phi)
  end subroutine random_sphere_angle3d_sp

  type(angle3d_sp) function minus_angle_sp(a) result(b)
    implicit none
    type(angle3d_sp),intent(in) :: a
    b = angle3d_sp(-a%cost, a%sint, -a%cosp, -a%sinp)
  end function minus_angle_sp


end module type_angle3d

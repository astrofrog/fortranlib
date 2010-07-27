! MD5 of template: 43eb20585c74b72a78af9748f820a965
! 3D angle related routines
! Thomas Robitaille (c) 2009

! Depends on lib_random

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

  public :: angle3d_deg
  interface angle3d_deg
     module procedure angle3d_deg_sp
     module procedure angle3d_deg_dp
  end interface

  public :: display_angle
  interface display_angle
     module procedure display_angle_sp
     module procedure display_angle_dp
  end interface

  public :: operator(.dot.)
  interface operator(.dot.)
     module procedure dot_product_sp
     module procedure dot_product_dp
  end interface

  public :: rotate_angle3d
  interface rotate_angle3d
     module procedure rotate_angle3d_sp
     module procedure rotate_angle3d_dp
  end interface

  public :: difference_angle3d
  interface difference_angle3d
     module procedure difference_angle3d_sp
     module procedure difference_angle3d_dp
  end interface

  public :: random_sphere_angle3d
  interface random_sphere_angle3d
     module procedure random_sphere_angle3d_sp
     module procedure random_sphere_angle3d_dp
  end interface

  public :: operator(-)
  interface operator(-)
     module procedure minus_angle_sp
     module procedure minus_angle_dp
  end interface

contains



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


  subroutine rotate_angle3d_dp(a_local,a_coord,a_final,cos_big_a,sin_big_a)

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

    ! In some circumstances, it might be useful to know the angle A,
    ! for example when finding the changes in polarization. Therefore
    ! we add cos_A and sin_A as optional arguments to the routine

    implicit none

    type(angle3d_dp),intent(in) :: a_local
    type(angle3d_dp),intent(in) :: a_coord
    type(angle3d_dp),intent(out) :: a_final

    real(dp) :: cos_a,sin_a
    real(dp) :: cos_b,sin_b
    real(dp) :: cos_c,sin_c

    real(dp),intent(out),optional :: cos_big_a,sin_big_a
    real(dp) :: cos_big_b,sin_big_b
    real(dp) :: cos_big_c,sin_big_c

    real(dp),parameter :: tol = 1.e-5_dp

    ! --- Assign spherical triangle angles values --- !

    ! The angles in the spherical triangle are as follows:

    cos_a = a_coord%cost
    sin_a = a_coord%sint

    cos_b = a_local%cost
    sin_b = a_local%sint

    ! Special case - if coord%theta is 0, then final = local
    if(abs(abs(cos_a)-1._dp) < tol .and. abs(sin_a) < tol) then
       if(cos_a > 0._dp) then
          a_final = a_local
       else
          a_final = a_local
          a_final%cost = -a_local%cost
          a_final%cosp = -a_local%cosp
          a_final%sinp = -a_local%sinp
       end if
       return
    end if

    if(a_local%sinp < 0._dp) then

       ! the angle in the triangle is then actually 2*pi - local phi

       cos_big_C = + a_local%cosp
       sin_big_C = - a_local%sinp

    else

       ! the angle is local phi

       cos_big_C = + a_local%cosp
       sin_big_C = + a_local%sinp

    end if

    ! --- Solve the spherical triangle --- !

    cos_c     = cos_a * cos_b + sin_a * sin_b * cos_big_c
    sin_c     = sqrt( 1._dp - cos_c * cos_c )

    ! Special case - if local and coord theta are the same and C=0, return vertical vector
    if(abs(sin_c) < tol) then
       if(cos_c > 0._dp) then
          a_final = angle3d_deg(0._dp,0._dp)
       else
          a_final = angle3d_deg(180._dp,0._dp)
       end if
       return
    end if

    cos_big_b = ( cos_b - cos_a * cos_c ) / ( sin_a * sin_c )
    sin_big_b = + sin_big_c * sin_b / sin_c

    if(present(cos_big_a).and.present(sin_big_a)) then
       cos_big_a = - cos_big_b * cos_big_c + sin_big_b * sin_big_c * cos_a
       sin_big_a = + sin_big_c * sin_a / sin_c
    end if

    ! --- Find final theta and phi values --- !

    a_final%cost = cos_c
    a_final%sint = sin_c

    if(a_local%sinp < 0._dp) then

       ! the top angle is old phi - new phi

       a_final%cosp = + cos_big_b * a_coord%cosp + sin_big_b * a_coord%sinp
       a_final%sinp = + cos_big_b * a_coord%sinp - sin_big_b * a_coord%cosp       

    else

       ! the top angle is new phi - old phi

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

    ! --- Assign spherical triangle angles values --- !

    ! The angles in the spherical triangle are as follows:

    cos_a = a_coord%cost
    sin_a = a_coord%sint

    cos_c = a_final%cost
    sin_c = a_final%sint

    cos_big_B = a_coord%cosp * a_final%cosp + a_coord%sinp * a_final%sinp
    sin_big_B = a_coord%sinp * a_final%cosp - a_coord%cosp * a_final%sinp

    ! Special case - if coord%theta is 0, then final = local
    ! if(cos_a==1..and.sin_a==0.) then
    !   a_final = a_local
    !   return
    ! end if

    ! --- Solve the spherical triangle --- !

    cos_b = cos_a * cos_c + sin_a * sin_c * cos_big_B
    sin_b = sqrt( 1._dp - cos_b * cos_b )

    ! Special case - if local and coord theta are the same and C=0, return vertical vector
    ! if(sin_c == 0._dp) then
    !   if(cos_c > 0._dp) then
    !     a_final = angle3d_deg(0._dp,0._dp)
    !   else
    !     a_final = angle3d_deg(180._dp,0._dp)
    !   end if
    !   return
    ! end if

    cos_big_c = ( cos_c - cos_a * cos_b ) / ( sin_a * sin_b )
    sin_big_c = + abs(sin_big_b) * sin_c / sin_b

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
    b = angle3d_dp(a%cost, -a%sint, a%cosp, -a%sinp)
  end function minus_angle_dp


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


  subroutine rotate_angle3d_sp(a_local,a_coord,a_final,cos_big_a,sin_big_a)

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

    ! In some circumstances, it might be useful to know the angle A,
    ! for example when finding the changes in polarization. Therefore
    ! we add cos_A and sin_A as optional arguments to the routine

    implicit none

    type(angle3d_sp),intent(in) :: a_local
    type(angle3d_sp),intent(in) :: a_coord
    type(angle3d_sp),intent(out) :: a_final

    real(sp) :: cos_a,sin_a
    real(sp) :: cos_b,sin_b
    real(sp) :: cos_c,sin_c

    real(sp),intent(out),optional :: cos_big_a,sin_big_a
    real(sp) :: cos_big_b,sin_big_b
    real(sp) :: cos_big_c,sin_big_c

    real(sp),parameter :: tol = 1.e-5_sp

    ! --- Assign spherical triangle angles values --- !

    ! The angles in the spherical triangle are as follows:

    cos_a = a_coord%cost
    sin_a = a_coord%sint

    cos_b = a_local%cost
    sin_b = a_local%sint

    ! Special case - if coord%theta is 0, then final = local
    if(abs(abs(cos_a)-1._sp) < tol .and. abs(sin_a) < tol) then
       if(cos_a > 0._sp) then
          a_final = a_local
       else
          a_final = a_local
          a_final%cost = -a_local%cost
          a_final%cosp = -a_local%cosp
          a_final%sinp = -a_local%sinp
       end if
       return
    end if

    if(a_local%sinp < 0._sp) then

       ! the angle in the triangle is then actually 2*pi - local phi

       cos_big_C = + a_local%cosp
       sin_big_C = - a_local%sinp

    else

       ! the angle is local phi

       cos_big_C = + a_local%cosp
       sin_big_C = + a_local%sinp

    end if

    ! --- Solve the spherical triangle --- !

    cos_c     = cos_a * cos_b + sin_a * sin_b * cos_big_c
    sin_c     = sqrt( 1._sp - cos_c * cos_c )

    ! Special case - if local and coord theta are the same and C=0, return vertical vector
    if(abs(sin_c) < tol) then
       if(cos_c > 0._sp) then
          a_final = angle3d_deg(0._sp,0._sp)
       else
          a_final = angle3d_deg(180._sp,0._sp)
       end if
       return
    end if

    cos_big_b = ( cos_b - cos_a * cos_c ) / ( sin_a * sin_c )
    sin_big_b = + sin_big_c * sin_b / sin_c

    if(present(cos_big_a).and.present(sin_big_a)) then
       cos_big_a = - cos_big_b * cos_big_c + sin_big_b * sin_big_c * cos_a
       sin_big_a = + sin_big_c * sin_a / sin_c
    end if

    ! --- Find final theta and phi values --- !

    a_final%cost = cos_c
    a_final%sint = sin_c

    if(a_local%sinp < 0._sp) then

       ! the top angle is old phi - new phi

       a_final%cosp = + cos_big_b * a_coord%cosp + sin_big_b * a_coord%sinp
       a_final%sinp = + cos_big_b * a_coord%sinp - sin_big_b * a_coord%cosp       

    else

       ! the top angle is new phi - old phi

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

    ! --- Assign spherical triangle angles values --- !

    ! The angles in the spherical triangle are as follows:

    cos_a = a_coord%cost
    sin_a = a_coord%sint

    cos_c = a_final%cost
    sin_c = a_final%sint

    cos_big_B = a_coord%cosp * a_final%cosp + a_coord%sinp * a_final%sinp
    sin_big_B = a_coord%sinp * a_final%cosp - a_coord%cosp * a_final%sinp

    ! Special case - if coord%theta is 0, then final = local
    ! if(cos_a==1..and.sin_a==0.) then
    !   a_final = a_local
    !   return
    ! end if

    ! --- Solve the spherical triangle --- !

    cos_b = cos_a * cos_c + sin_a * sin_c * cos_big_B
    sin_b = sqrt( 1._sp - cos_b * cos_b )

    ! Special case - if local and coord theta are the same and C=0, return vertical vector
    ! if(sin_c == 0._sp) then
    !   if(cos_c > 0._sp) then
    !     a_final = angle3d_deg(0._sp,0._sp)
    !   else
    !     a_final = angle3d_deg(180._sp,0._sp)
    !   end if
    !   return
    ! end if

    cos_big_c = ( cos_c - cos_a * cos_b ) / ( sin_a * sin_b )
    sin_big_c = + abs(sin_big_b) * sin_c / sin_b

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
    b = angle3d_sp(a%cost, -a%sint, a%cosp, -a%sinp)
  end function minus_angle_sp


end module type_angle3d

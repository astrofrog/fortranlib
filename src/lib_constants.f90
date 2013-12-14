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

module lib_constants

  implicit none
  save

  integer,parameter,private :: sp = selected_real_kind(p=6,r=37)
  integer,parameter,private :: dp = selected_real_kind(p=15,r=307)

  real(sp),parameter :: zero_sp = 0._sp
  real(dp),parameter :: zero_dp = 0._dp

  real(dp),parameter :: zero = 0._dp
  real(dp),parameter :: half = 0.5_dp
  real(dp),parameter :: one = 1._dp
  real(dp),parameter :: two = 2._dp

  ! Physical constants  

  real(dp),parameter :: G_cgs = 6.67300d-08
  real(dp),parameter :: G_si  = 6.67300d-11

  !  N = kg.m/s^2 = [G]*kg^2/m**2
  !  [G] = m^3/s^2/kg

  real(dp),parameter :: k_cgs = 1.380650424d-16 ! erg/K
  real(dp),parameter :: k_si  = 1.380650424d-23 !   J/K

  real(dp),parameter :: h_cgs = 6.6260689633d-27   ! ergs.s  
  real(dp),parameter :: h_si  = 6.6260689633e-34_dp !    J.s

  real(dp),parameter :: c_si  = 2.99792458e08_dp ! m / s
  real(dp),parameter :: c_cgs = 2.99792458e10_dp ! cm / s
  ! speed of light

  real(dp),parameter :: kpc_si  = 3.08568025e19_dp ! m
  real(dp),parameter :: kpc_cgs = 3.08568025e21_dp ! cm
  ! kiloparsec

  real(dp),parameter :: pi = 3.14159265358979323846_dp
  real(sp),parameter :: pi_sp = 3.14159265358979323846_sp
  real(dp),parameter :: pi_dp = 3.14159265358979323846_dp

  real(dp),parameter :: twopi = pi + pi
  real(sp),parameter :: twopi_sp = pi_sp + pi_sp
  real(dp),parameter :: twopi_dp = pi_dp + pi_dp

  real(dp),parameter  :: deg2rad = pi / 180._dp
  real(dp),parameter  :: rad2deg = 180._dp / pi
  real(sp),parameter :: deg2rad_sp = pi_sp / 180._sp
  real(sp),parameter :: rad2deg_sp = 180._sp / pi_sp
  real(dp),parameter :: deg2rad_dp = pi_dp / 180._dp
  real(dp),parameter :: rad2deg_dp = 180._dp / pi_dp

  real(dp),parameter :: lsun_cgs = 3.846e33_dp ! erg/s

  real(dp),parameter :: rsun_cgs = 6.95508e10_dp ! cm

  real(dp),parameter :: au_cgs = 1.49598e13_dp ! cm

  real(dp),parameter :: year_cgs = 3600._dp * 24._dp * 365.25_dp

  real(dp),parameter :: msun_cgs = 1.989e33_dp ! g
  ! Conversions

  real(dp),parameter :: ergs2mJy = 1.e26_dp
  real(dp),parameter :: microns2cm = 1.e-4_dp
  real(dp),parameter :: microns2m  = 1.e-6_dp

  real(dp),parameter :: stef_boltz = 5.670400e-5_dp

contains

  real(sp) function infinity_sp()
    implicit none
    real(sp) :: x
    x = huge(1._sp)
    infinity_sp = x + x
  end function infinity_sp

  real(dp) function infinity_dp()
    implicit none
    real(dp) :: x
    x = huge(1._dp)
    infinity_dp = x + x
  end function infinity_dp

end module lib_constants

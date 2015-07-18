! Random number generation related routines
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

module lib_random

  implicit none
  save

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  integer :: idum = -1204132124
  real(dp) :: u(97)
  !$OMP THREADPRIVATE(idum, u)

  real(dp),parameter :: pi = 3.14159265358979323846_dp
  real(sp),parameter :: pi_sp = 3.14159265358979323846_sp
  real(dp),parameter :: pi_dp = 3.14159265358979323846_dp

  real(dp),parameter :: twopi = 2._dp * pi
  real(sp),parameter :: twopi_sp = pi_sp + pi_sp
  real(dp),parameter :: twopi_dp = pi_dp + pi_dp

  public :: set_seed
  public :: set_seed_64

  public :: random
  interface random
     module procedure random_sp
     module procedure random_dp
  end interface random

  public :: random_exp
  interface random_exp
     module procedure random_exp_sp
     module procedure random_exp_dp
  end interface random_exp

  public :: random_uni
  interface random_uni
     module procedure random_uni_sp
     module procedure random_uni_dp
  end interface random_uni

  public :: random_gau
  interface random_gau
     module procedure random_gau_sp
     module procedure random_gau_dp
  end interface random_gau

  public :: random_sphere
  interface random_sphere
     module procedure random_sphere_sp
     module procedure random_sphere_dp
  end interface random_sphere

  public :: random_poisson
  interface random_poisson
     module procedure random_poisson_sp
     module procedure random_poisson_dp
  end interface random_poisson

  public :: random_planck_frequency
  interface random_planck_frequency
     module procedure random_planck_frequency_sp
     module procedure random_planck_frequency_dp
  end interface random_planck_frequency

contains

  subroutine set_seed(seed)
    ! Note: this should be called with a different seed in each thread or
    ! process.
    implicit none
    integer,intent(in) :: seed
    idum = -abs(seed)
    call set_seed_64(abs(seed), 987654321)
  end subroutine set_seed

  subroutine set_seed_64(seed1,seed2)
    implicit none
    integer,intent(in) :: seed1,seed2
    integer i,j,x,y
    real(dp) :: s,t
    x=seed1
    y=seed2
    do i=1,97
       s=0._dp
       t=0.5_dp
       do j=1,53
          x=mod(6969*x,65543)
          y=mod(8888*x,65579)
          if (iand(ieor(x,y),32).gt.0) s=s+t
          t=0.5_dp*t
       end do
       u(i)=s
    end do
  end subroutine set_seed_64

  subroutine random_string(string)
    implicit none
    character(len=*),intent(out) :: string
    integer :: i,j
    real(dp) :: xi
    do i=1,len(string)
       call random(xi)
       j = int(xi*26._dp)
       call random(xi)
       if(xi < 0.5) then
          j = j + 65
       else
          j = j + 97
       end if
       string(i:i) = char(j)
    end do
  end subroutine random_string

  subroutine random_sp(xi)
    ! Random number between 0 and 1
    ! Inspired by Numerical Recipes
    implicit none
    real(sp),intent(out) :: xi
    real(sp),save        :: am
    !$OMP THREADPRIVATE(am)
    integer, parameter :: ia=16807,im=2147483647,iq=127773,ir=2836
    integer, save :: ix=-1,iy=-1,k
    !$OMP THREADPRIVATE(ix, iy, k)
    if (idum <= 0 .or. iy < 0) then
       am=nearest(1.0_sp,-1.0_sp)/im
       iy=ior(ieor(888889999,abs(idum)),1)
       ix=ieor(777755555,abs(idum))
       idum=abs(idum)+1
    end if
    ix=ieor(ix,ishft(ix,13))
    ix=ieor(ix,ishft(ix,-17))
    ix=ieor(ix,ishft(ix,5))
    k=iy/iq
    iy=ia*(iy-k*iq)-ir*k
    if (iy < 0) iy=iy+im
    xi=am*ior(iand(im,ieor(ix,iy)),1)
  end subroutine random_sp

  subroutine random_dp(xi)
    ! Random number between 0 and 1
    ! Based on "The 64-bit universal RNG", Marsaglia & Tsang (2004)
    implicit none
    real(dp),intent(out) :: xi
    integer,save :: i=97
    integer,save :: j=33
    real(dp),save :: c=0
    !$OMP THREADPRIVATE(i, j, c)
    real(dp) :: x
    real(dp), parameter :: r=9007199254740881._dp/9007199254740992._dp
    real(dp), parameter :: d=362436069876._dp/9007199254740992._dp
    x=u(i)-u(j)
    if (x.lt.0.0) x=x+1.0_dp
    u(i)=x
    i=i-1
    if (i.eq.0) i=97
    j=j-1
    if (j.eq.0) j=97
    c=c-d
    if (c.lt.0.0) c=c+r
    x=x-c
    xi=x
    if (x.lt.0.) xi=x+1._dp
    return
  end subroutine random_dp

  !!@FOR real(sp):sp real(dp):dp

  subroutine random_uni_<T>(xi,a,b)
    ! Uniform random number between a and b
    implicit none
    real(<T>),intent(in)  :: a,b
    real(<T>),intent(out) :: xi
    call random(xi)
    xi = a + (b-a) * xi
  end subroutine random_uni_<T>

  subroutine random_gau_<T>(xi,c,w)
    ! Gaussian random number with center c and 1-sigma w
    implicit none
    real(<T>),intent(in)  :: c,w
    real(<T>),intent(out) :: xi
    real(<T>) :: r,g1,g2,x,y,t
    do
       call random_uni(x,-1._<T>,+1._<T>)
       call random_uni(y,-1._<T>,+1._<T>)
       r = x*x + y*y
       if(r.lt.1) exit
    end do
    t=sqrt(-2._<T>*log(r)/r)
    g1 = x*t
    g2 = y*t
    xi = g1*w+c
  end subroutine random_gau_<T>

  subroutine random_exp_<T>(xi)
    ! Random number sampled from exp(-tau)
    implicit none
    real(<T>),intent(out) :: xi
    do
       call random(xi)
       if(xi < 1._<T>) exit
    end do
    xi = - log( 1._<T> - xi )
  end subroutine random_exp_<T>


  subroutine random_sphere_<T>(mu,phi)
    ! Random longitude/latitude on a sphere
    implicit none
    real(<T>),intent(out) :: mu,phi
    call random_uni(mu,-1._<T>,+1._<T>)
    call random_uni(phi,0._<T>,twopi_<T>)
  end subroutine random_sphere_<T>


  subroutine random_poisson_<T>(xi,xm)
    ! Random number drawn from Poisson distribution with
    ! expected value xm

    implicit none

    real(<T>),intent(in) :: xm
    real(<T>),intent(out) :: xi
    real(<T>) :: em,harvest,t,y

    real(<T>), save :: alxm,g,oldm=-1.0_<T>,sq
    !$OMP THREADPRIVATE(alxm,g,oldm,sq)

    if (xm < 12.0) then
       if (xm /= oldm) then
          oldm=xm
          g=exp(-xm)
       end if
       em=-1
       t=1.0
       do
          em=em+1.0_<T>
          call random_<T>(harvest)
          t=t*harvest
          if (t <= g) exit
       end do
    else
       if (xm /= oldm) then
          oldm=xm
          sq=sqrt(2.0_<T>*xm)
          alxm=log(xm)
          g=xm*alxm-gammln_<T>(xm+1.0_<T>)
       end if
       do
          do
             call random_<T>(harvest)
             y=tan(pi*harvest)
             em=sq*y+xm
             if (em >= 0.0) exit
          end do
          em=int(em)
          t=0.9_<T>*(1.0_<T>+y**2)*exp(em*alxm-gammln_<T>(em+1.0_<T>)-g)
          call random_<T>(harvest)
          if (harvest <= t) exit
       end do
    end if
    xi = em
  end subroutine random_poisson_<T>

  subroutine random_planck_frequency_<T>(nu,T)

    ! Random frequency sampled from a planck function with temperature T

    ! The algorithm is taken from 'Sampling a random variable distributed
    ! according to planck's law' by Barnett and Canfield

    implicit none

    real(<T>),intent(in) :: T
    real(<T>),intent(out) :: nu
    real(<T>) :: x,r,r1,r2,r3,r4,a,y,z

    real(<T>),parameter :: k  = 1.3806503e-23_<T> ! J/K
    real(<T>),parameter :: h  = 6.626068e-34_<T> ! J.s

    ! Sample a random number from x^3/(exp(x)-1)

    do

       call random(r1)
       call random(r2)
       call random(r3)
       call random(r4)

       r = r1*r2*r3*r4

       if(r > 0._dp) exit

    end do

    x = - log(r)

    a = 1._<T>
    y = 1._<T>
    z = 1._<T>

    call random(r1)
    do
       if(1.08232_<T>*r1 <= a) exit
       y = y + 1._<T>
       z = 1._<T>/y
       a = a + z*z*z*z
    end do
    x = x * z

    ! Convert to frequency

    nu = x * k * T / h

  end subroutine random_planck_frequency_<T>

  real(<T>) function gammln_<T>(xx)

    implicit none

    real(<T>),intent(in) :: xx
    integer :: j
    real(<T>) :: ser,tmp,x,y

    real(<T>),save :: cof(6) = (/76.18009172947146_<T>,&
         &-86.50532032941677_<T>,24.01409824083091_<T>,&
         &-1.231739572450155_<T>,.1208650973866179e-2_<T>,&
         &-.5395239384953e-5_<T>/)
    !$OMP THREADPRIVATE(cof)

    real(<T>),save :: stp = 2.5066282746310005_<T>
    !$OMP THREADPRIVATE(stp)

    x=xx
    y=x
    tmp=x+5.5_<T>
    tmp=(x+0.5_<T>)*log(tmp)-tmp
    ser=1.000000000190015_<T>

    do j=1,6
       y=y+1._<T>
       ser=ser+cof(j)/y
    end do

    gammln_<T>=tmp+log(stp*ser/x)
    return

  end function gammln_<T>

  !!@END FOR


end module lib_random

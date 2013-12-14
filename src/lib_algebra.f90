! MD5 of template: 9b6b4bb463e1543bdbfe618866a80346
! Algebra routines
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

module lib_algebra

  implicit none
  save

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  public :: cbrt
  interface cbrt
     module procedure cbrt_sp
     module procedure cbrt_dp
  end interface cbrt

  public :: quadratic
  interface quadratic
     module procedure quadratic_sp
     module procedure quadratic_dp
  end interface quadratic

  public :: quadratic_reduced
  interface quadratic_reduced
     module procedure quadratic_reduced_sp
     module procedure quadratic_reduced_dp
  end interface quadratic_reduced

  public :: quadratic_pascal
  interface quadratic_pascal
     module procedure quadratic_pascal_sp
     module procedure quadratic_pascal_dp
  end interface quadratic_pascal

  public :: quadratic_pascal_reduced
  interface quadratic_pascal_reduced
     module procedure quadratic_pascal_reduced_sp
     module procedure quadratic_pascal_reduced_dp
  end interface quadratic_pascal_reduced

  public :: lineq_gausselim
  interface lineq_gausselim
     module procedure lineq_gausselim_sp
     module procedure lineq_gausselim_dp
  end interface lineq_gausselim

contains


  real(dp) function cbrt_dp(x)
    implicit none
    real(dp) :: x
    real(dp),parameter :: alpha = 1._dp / 3._dp
    if(x >= 0.) then
       cbrt_dp = x**alpha
    else
       cbrt_dp = - (abs(x))**alpha
    end if
  end function cbrt_dp

  subroutine quadratic_reduced_dp(b,c,x1,x2)
    implicit none
    real(dp),intent(in)  :: b,c
    real(dp),intent(out) :: x1,x2
    real(dp) :: delta
    delta = b*b - 4._dp*c
    if(delta > 0) then
       delta = sqrt(delta)
       x1 = ( - b - delta ) * 0.5_dp
       x2 = ( - b + delta ) * 0.5_dp
    else
       x1 = huge(x1)
       x2 = huge(x2)
    end if
  end subroutine quadratic_reduced_dp

  subroutine quadratic_dp(a,b,c,x1,x2)
    implicit none
    real(dp),intent(in)  :: a,b,c
    real(dp),intent(out) :: x1,x2
    real(dp) :: delta,factor
    delta = b*b - 4._dp*a*c
    if(delta > 0) then
       delta = sqrt(delta)
       factor = 0.5_dp / a
       x1 = ( - b - delta ) * factor
       x2 = ( - b + delta ) * factor
    else
       x1 = huge(x1)
       x2 = huge(x2)
    end if
  end subroutine quadratic_dp

  subroutine quadratic_pascal_dp(a,b,c,x1,x2)
    implicit none
    real(dp),intent(in)  :: a,b,c
    real(dp),intent(out) :: x1,x2
    real(dp) :: q,delta
    delta = b*b - 4._dp*a*c
    if(delta > 0) then
       delta = sqrt(delta)
       delta = sign(delta,b)
       q = -0.5_dp * ( b + delta )
       x1 = q / a
       x2 = c / q
    else if(delta < 0) then
       x1 = -huge(x1)
       x2 = -huge(x2)
    else
       x1 = - 2.0_dp * c / b
       x2 = -huge(x2)
    end if
  end subroutine quadratic_pascal_dp

  subroutine quadratic_pascal_reduced_dp(b,c,x1,x2)
    implicit none
    real(dp),intent(in)  :: b,c
    real(dp),intent(out) :: x1,x2
    real(dp) :: q,delta
    delta = b*b - 4._dp*c
    if(delta > 0) then
       delta = sqrt(delta)
       delta = sign(delta,b)
       q = -0.5_dp * ( b + delta )
       x1 = q
       x2 = c / q
    else if(delta < 0) then
       x1 = -huge(x1)
       x2 = -huge(x2)
    else
       x1 = - 2.0_dp * c / b
       x2 = -huge(x2)
    end if
  end subroutine quadratic_pascal_reduced_dp

  subroutine lineq_gausselim_dp(a,b)

    implicit none
    real(dp),intent(inout) :: a(:,:),b(:)
    real(dp) :: frac
    integer :: i,j
    integer :: n

    n = size(a,1)

    do i=1,n-1
       if(a(i,i)==0) stop "Zero pivot value"
       do j=i+1,n
          if(a(i,j).ne.0.) then
             frac = a(i,j)/a(i,i)
             b(j) = b(j) - frac * b(i)
             a(i:,j) = a(i:,j) - frac * a(i:,i)
          end if
       end do
    end do

    do i=n,2,-1
       do j=i-1,1,-1
          if(a(i,j).ne.0.) then
             frac = a(i,j)/a(i,i)
             b(j) = b(j) - frac * b(i)
             a(i:,j) = a(i:,j) - frac * a(i:,i)
          end if
       end do
    end do

    do i=1,n
       b(i) = b(i) / a(i,i)
    end do

  end subroutine lineq_gausselim_dp


  real(sp) function cbrt_sp(x)
    implicit none
    real(sp) :: x
    real(sp),parameter :: alpha = 1._sp / 3._sp
    if(x >= 0.) then
       cbrt_sp = x**alpha
    else
       cbrt_sp = - (abs(x))**alpha
    end if
  end function cbrt_sp

  subroutine quadratic_reduced_sp(b,c,x1,x2)
    implicit none
    real(sp),intent(in)  :: b,c
    real(sp),intent(out) :: x1,x2
    real(sp) :: delta
    delta = b*b - 4._sp*c
    if(delta > 0) then
       delta = sqrt(delta)
       x1 = ( - b - delta ) * 0.5_sp
       x2 = ( - b + delta ) * 0.5_sp
    else
       x1 = huge(x1)
       x2 = huge(x2)
    end if
  end subroutine quadratic_reduced_sp

  subroutine quadratic_sp(a,b,c,x1,x2)
    implicit none
    real(sp),intent(in)  :: a,b,c
    real(sp),intent(out) :: x1,x2
    real(sp) :: delta,factor
    delta = b*b - 4._sp*a*c
    if(delta > 0) then
       delta = sqrt(delta)
       factor = 0.5_sp / a
       x1 = ( - b - delta ) * factor
       x2 = ( - b + delta ) * factor
    else
       x1 = huge(x1)
       x2 = huge(x2)
    end if
  end subroutine quadratic_sp

  subroutine quadratic_pascal_sp(a,b,c,x1,x2)
    implicit none
    real(sp),intent(in)  :: a,b,c
    real(sp),intent(out) :: x1,x2
    real(sp) :: q,delta
    delta = b*b - 4._sp*a*c
    if(delta > 0) then
       delta = sqrt(delta)
       delta = sign(delta,b)
       q = -0.5_sp * ( b + delta )
       x1 = q / a
       x2 = c / q
    else if(delta < 0) then
       x1 = -huge(x1)
       x2 = -huge(x2)
    else
       x1 = - 2.0_sp * c / b
       x2 = -huge(x2)
    end if
  end subroutine quadratic_pascal_sp

  subroutine quadratic_pascal_reduced_sp(b,c,x1,x2)
    implicit none
    real(sp),intent(in)  :: b,c
    real(sp),intent(out) :: x1,x2
    real(sp) :: q,delta
    delta = b*b - 4._sp*c
    if(delta > 0) then
       delta = sqrt(delta)
       delta = sign(delta,b)
       q = -0.5_sp * ( b + delta )
       x1 = q
       x2 = c / q
    else if(delta < 0) then
       x1 = -huge(x1)
       x2 = -huge(x2)
    else
       x1 = - 2.0_sp * c / b
       x2 = -huge(x2)
    end if
  end subroutine quadratic_pascal_reduced_sp

  subroutine lineq_gausselim_sp(a,b)

    implicit none
    real(sp),intent(inout) :: a(:,:),b(:)
    real(sp) :: frac
    integer :: i,j
    integer :: n

    n = size(a,1)

    do i=1,n-1
       if(a(i,i)==0) stop "Zero pivot value"
       do j=i+1,n
          if(a(i,j).ne.0.) then
             frac = a(i,j)/a(i,i)
             b(j) = b(j) - frac * b(i)
             a(i:,j) = a(i:,j) - frac * a(i:,i)
          end if
       end do
    end do

    do i=n,2,-1
       do j=i-1,1,-1
          if(a(i,j).ne.0.) then
             frac = a(i,j)/a(i,i)
             b(j) = b(j) - frac * b(i)
             a(i:,j) = a(i:,j) - frac * a(i:,i)
          end if
       end do
    end do

    do i=1,n
       b(i) = b(i) / a(i,i)
    end do

  end subroutine lineq_gausselim_sp


end module lib_algebra

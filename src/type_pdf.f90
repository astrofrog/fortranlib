! MD5 of template: e8ce72b1fbc6a396e11ba55e6dc29db9
! Probability Distribution Function (PDF) related routines
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

module type_pdf

  use lib_array
  use lib_random

  implicit none

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  real,parameter :: unset = -1.e30


  public :: pdf_dp
  type pdf_dp
     integer :: n = 0
     real(dp),allocatable :: x(:)
     real(dp),allocatable :: pdf(:)
     real(dp),allocatable :: cdf(:)
     logical :: log = .false.
     logical :: normalized = .false.

     ! Simple mode means that the interpolation in the CDF is done in an
     ! approximate way, and does not take into account that computing a CDF from
     ! a linear function gives a non-linear CDF. The correct calculation is done
     ! by default, and actually ends up being faster because there are fewer calls
     ! to functions. The 'correct' way required 3 or 4 additional arrays to be
     ! pre-computed to make it faster, so it is more RAM intensive.
     logical :: simple = .false.
     real(dp),allocatable :: a(:),b(:),r(:),rx(:),rc(:)

  end type pdf_dp

  public :: pdf_discrete_dp
  type pdf_discrete_dp
     integer :: n = 0
     real(dp),allocatable :: pdf(:)
     real(dp),allocatable :: cdf(:)
     logical :: normalized = .false.
  end type pdf_discrete_dp


  public :: pdf_sp
  type pdf_sp
     integer :: n = 0
     real(sp),allocatable :: x(:)
     real(sp),allocatable :: pdf(:)
     real(sp),allocatable :: cdf(:)
     logical :: log = .false.
     logical :: normalized = .false.

     ! Simple mode means that the interpolation in the CDF is done in an
     ! approximate way, and does not take into account that computing a CDF from
     ! a linear function gives a non-linear CDF. The correct calculation is done
     ! by default, and actually ends up being faster because there are fewer calls
     ! to functions. The 'correct' way required 3 or 4 additional arrays to be
     ! pre-computed to make it faster, so it is more RAM intensive.
     logical :: simple = .false.
     real(sp),allocatable :: a(:),b(:),r(:),rx(:),rc(:)

  end type pdf_sp

  public :: pdf_discrete_sp
  type pdf_discrete_sp
     integer :: n = 0
     real(sp),allocatable :: pdf(:)
     real(sp),allocatable :: cdf(:)
     logical :: normalized = .false.
  end type pdf_discrete_sp


  public :: allocate_pdf
  interface allocate_pdf
     module procedure allocate_pdf_discrete_sp
     module procedure allocate_pdf_discrete_dp
     module procedure allocate_pdf_cont_sp
     module procedure allocate_pdf_cont_dp
  end interface allocate_pdf

  public :: set_pdf
  interface set_pdf
     module procedure set_pdf_discrete_sp
     module procedure set_pdf_discrete_dp
     module procedure set_pdf_cont_sp
     module procedure set_pdf_cont_dp
  end interface set_pdf

  public :: normalize_pdf
  interface normalize_pdf
     module procedure normalize_pdf_discrete_sp
     module procedure normalize_pdf_discrete_dp
     module procedure normalize_pdf_cont_sp
     module procedure normalize_pdf_cont_dp
  end interface normalize_pdf

  public :: check_pdf
  interface check_pdf
     module procedure check_pdf_discrete_sp
     module procedure check_pdf_discrete_dp
     module procedure check_pdf_cont_sp
     module procedure check_pdf_cont_dp
  end interface check_pdf

  public :: find_cdf
  interface find_cdf
     module procedure find_cdf_discrete_sp
     module procedure find_cdf_discrete_dp
     module procedure find_cdf_cont_sp
     module procedure find_cdf_cont_dp
  end interface find_cdf

  public :: sample_pdf
  interface sample_pdf
     module procedure sample_pdf_discrete_sp
     module procedure sample_pdf_discrete_dp
     module procedure sample_pdf_cont_sp
     module procedure sample_pdf_cont_dp
  end interface sample_pdf

  public :: sample_pdf_log
  interface sample_pdf_log
     module procedure sample_pdf_cont_log_sp
     module procedure sample_pdf_cont_log_dp
  end interface sample_pdf_log

  public :: interpolate_pdf
  interface interpolate_pdf
     module procedure interpolate_pdf_cont_sp
     module procedure interpolate_pdf_cont_dp
  end interface interpolate_pdf

contains


  subroutine find_cdf_discrete_dp(p)
    implicit none
    type(pdf_discrete_dp),intent(inout) :: p
    integer :: i
    real(dp) :: norm
    if(p%n==0) stop "[find_cdf_discrete] PDF is not set correctly (y)"
    p%cdf(1) = p%pdf(1)
    do i=2,p%n
       p%cdf(i) = p%cdf(i-1) + p%pdf(i)
    end do
    norm = p%cdf(p%n)
    if(norm==0._dp) stop "[find_cdf_discrete] all PDF elements are zero"
    p%cdf = p%cdf / norm
  end subroutine find_cdf_discrete_dp

  subroutine allocate_pdf_discrete_dp(p,n)
    implicit none
    type(pdf_discrete_dp),intent(out) :: p
    integer,intent(in) :: n
    p%n = n
    allocate(p%pdf(n))
    allocate(p%cdf(n))
  end subroutine allocate_pdf_discrete_dp

  subroutine allocate_pdf_cont_dp(p,n)
    implicit none
    type(pdf_dp),intent(out) :: p
    integer,intent(in) :: n
    p%n = n
    allocate(p%x(n))
    allocate(p%pdf(n))
    allocate(p%cdf(n))
  end subroutine allocate_pdf_cont_dp

  subroutine normalize_pdf_discrete_dp(p)
    implicit none
    type(pdf_discrete_dp),intent(inout) :: p
    real(dp) :: norm
    norm = sum(p%pdf)
    if(norm==0._dp) stop "[normalize_pdf_discrete] all PDF elements are zero"
    p%pdf = p%pdf / norm
    p%normalized = .true.
  end subroutine normalize_pdf_discrete_dp

  subroutine normalize_pdf_cont_dp(p)
    implicit none
    type(pdf_dp),intent(inout) :: p
    if(p%log) then
       p%pdf = p%pdf / integral_loglog(p%x, p%pdf)
    else
       p%pdf = p%pdf / integral(p%x, p%pdf)
    end if
    p%normalized = .true.
  end subroutine normalize_pdf_cont_dp

  subroutine set_pdf_discrete_dp(p,y)
    implicit none
    type(pdf_discrete_dp),intent(out) :: p
    real(dp),intent(in) :: y(:)
    call allocate_pdf(p,size(y))
    p%pdf = y
    call normalize_pdf(p)
    call find_cdf(p)
    call check_pdf(p)
  end subroutine set_pdf_discrete_dp

  subroutine set_pdf_cont_dp(p,x,y,log,simple)
    implicit none
    type(pdf_dp),intent(out) :: p
    real(dp),intent(in) :: x(:),y(:)
    logical,intent(in),optional :: log,simple
    if(size(x).ne.size(y)) stop "[set_pdf] x and y array sizes differ"
    call allocate_pdf(p,size(y))
    p%x   = x
    p%pdf = y
    if(present(log)) p%log = log
    if(present(simple)) p%simple = simple
    call normalize_pdf(p)
    call find_cdf(p)
    call check_pdf(p)
  end subroutine set_pdf_cont_dp

  subroutine check_pdf_discrete_dp(p)
    implicit none
    type(pdf_discrete_dp),intent(in) :: p
    if(p%n==0) stop "[check_pdf_discrete] PDF size not set"
    if(.not.allocated(p%pdf)) stop "[check_pdf_discrete] PDF pdf array not allocated"
    if(.not.allocated(p%cdf)) stop "[check_pdf_discrete] PDF cdf array not allocated"
    if(p%n.ne.size(p%pdf)) stop "[check_pdf_discrete] PDF pdf array has incorrect size"
    if(p%n.ne.size(p%cdf)) stop "[check_pdf_discrete] PDF cdf array has incorrect size"
    if(any(p%pdf < 0.)) stop "[check_pdf_discrete] PDF has negative probabilities"
    if(.not.p%normalized) stop "[check_pdf_discrete] PDF is not normalized"
  end subroutine check_pdf_discrete_dp

  subroutine check_pdf_cont_dp(p)
    implicit none
    type(pdf_dp),intent(in) :: p
    integer :: i
    if(p%n==0) stop "[check_pdf] PDF size not set"
    if(.not.allocated(p%x))   stop "[check_pdf] PDF x array not allocated"
    if(.not.allocated(p%pdf)) stop "[check_pdf] PDF pdf array not allocated"
    if(.not.allocated(p%cdf)) stop "[check_pdf] PDF cdf array not allocated"
    if(p%n.ne.size(p%x))   stop "[check_pdf] PDF x array has incorrect size"
    if(p%n.ne.size(p%pdf)) stop "[check_pdf] PDF pdf array has incorrect size"
    if(p%n.ne.size(p%cdf)) stop "[check_pdf] PDF cdf array has incorrect size"
    do i=2,p%n
       if(.not.(p%x(i)>p%x(i-1))) stop "[check_pdf] PDF x array is not sorted"
    end do
    if(any(p%pdf < 0.)) stop "[check_pdf] PDF has negative probabilities"
    if(.not.p%normalized) stop "[check_pdf] PDF is not normalized"
  end subroutine check_pdf_cont_dp

  subroutine find_cdf_cont_dp(p)
    implicit none
    type(pdf_dp),intent(inout) :: p
    integer :: i
    call check_pdf(p)
    if(p%log) then
       p%cdf = cumulative_integral_loglog(p%x,p%pdf)
    else
       p%cdf = cumulative_integral(p%x,p%pdf)
    end if
    p%cdf = p%cdf / p%cdf(p%n)
    if(.not.p%simple) then
       if(p%log) then
          allocate(p%b(p%n-1))
          allocate(p%r(p%n-1))
          do i=1,p%n-1
             p%b(i) = log10(p%pdf(i) / p%pdf(i+1)) / log10(p%x(i) / p%x(i+1))
             p%r(i) = (p%x(i+1) / p%x(i)) ** (p%b(i) + 1._dp)
          end do
       else
          allocate(p%a(p%n-1))
          allocate(p%b(p%n-1))
          allocate(p%rx(p%n-1))
          allocate(p%rc(p%n-1))
          do i=1,p%n-1
             p%a(i) = (p%pdf(i) - p%pdf(i+1)) / (p%x(i) - p%x(i+1))
             p%b(i) = p%pdf(i) - p%a(i) * p%x(i)
             p%rx(i) = p%x(i+1) / p%x(i)
             p%rc(i) = p%b(i) / p%a(i)
          end do
       end if
    end if
  end subroutine find_cdf_cont_dp

  integer function sample_pdf_discrete_dp(p)
    implicit none
    type(pdf_discrete_dp) :: p
    integer :: j,jmin,jmax
    real(dp) :: xi
    call random(xi)
    if(xi <= p%cdf(1)) then
       sample_pdf_discrete_dp = 1
    else if(xi >= p%cdf(p%n)) then
       sample_pdf_discrete_dp = p%n
    else
       jmin = 1
       jmax = p%n
       do
          j = (jmax + jmin) / 2
          if(xi > p%cdf(j)) then
             jmin = j
          else
             jmax = j
          end if
          if(jmax == jmin + 1) exit
       end do
       sample_pdf_discrete_dp = jmax
    end if
  end function sample_pdf_discrete_dp

  real(dp) function sample_pdf_cont_dp(p,xi_alt)
    implicit none
    type(pdf_dp),intent(in) :: p
    real(dp),optional,intent(in) :: xi_alt
    real(dp) :: xi
    integer :: i
    if(present(xi_alt)) then
       xi = xi_alt
    else
       call random(xi)
    end if
    if(xi <= p%cdf(1)) then
       sample_pdf_cont_dp = p%x(1)
    else if(xi >= p%cdf(p%n)) then
       sample_pdf_cont_dp = p%x(p%n)
    else
       if(p%simple) then
          if(p%log) then
             sample_pdf_cont_dp = interp1d_linlog(p%cdf(:), p%x(:), xi)
          else
             sample_pdf_cont_dp = interp1d(p%cdf(:), p%x(:), xi)
          end if
       else
          i = locate(p%cdf, xi)
          xi = (xi - p%cdf(i)) / (p%cdf(i+1) - p%cdf(i))
          if(p%log) then
             sample_pdf_cont_dp = (xi * (p%r(i) - 1._dp) + 1._dp) ** (1._dp / (p%b(i) + 1._dp)) * p%x(i)
          else
             if(p%a(i)==0._dp) then
                sample_pdf_cont_dp = xi * (p%x(i+1) - p%x(i)) + p%x(i)
             else if(p%x(i)==0._dp) then
                sample_pdf_cont_dp = - p%rc(i) + sign(sqrt(p%rc(i) * p%rc(i) &
                     &                                      + xi * p%x(i+1) * p%x(i+1) &
                     &                                      + 2._dp * p%rc(i) * xi * p%x(i+1)), p%a(i))
             else
                sample_pdf_cont_dp = - p%rc(i) + sign(sqrt(p%rc(i) * p%rc(i) &
                     &                                      + p%x(i) * p%x(i) * (xi * (p%rx(i) * p%rx(i) - 1._dp) + 1._dp) &
                     &                                      + 2._dp * p%rc(i) * p%x(i) * (xi * (p%rx(i) - 1._dp) + 1._dp)), p%a(i))
             end if
          end if
       end if
    end if
  end function sample_pdf_cont_dp

  real(dp) function sample_pdf_cont_log_dp(p,xi_alt)
    implicit none
    type(pdf_dp),intent(in) :: p
    real(dp),optional,intent(in) :: xi_alt
    real(dp) :: xi
    if(present(xi_alt)) then
       xi = xi_alt
    else
       call random(xi)
    end if
    sample_pdf_cont_log_dp = interp1d_loglog(p%cdf(:),p%x(:),xi)
  end function sample_pdf_cont_log_dp

  real(dp) function interpolate_pdf_cont_dp(p, x, bounds_error, fill_value) result(prob)
    implicit none
    type(pdf_dp),intent(in) :: p
    real(dp),intent(in) :: x
    logical,intent(in),optional :: bounds_error
    real(dp),intent(in),optional :: fill_value
    if(.not.p%normalized) stop "[interpolate_pdf] PDF is not normalized"
    if(p%log) then
       prob = interp1d_loglog(p%x, p%pdf, x, bounds_error, fill_value)
    else
       prob = interp1d(p%x, p%pdf, x, bounds_error, fill_value)
    end if
  end function interpolate_pdf_cont_dp


  subroutine find_cdf_discrete_sp(p)
    implicit none
    type(pdf_discrete_sp),intent(inout) :: p
    integer :: i
    real(sp) :: norm
    if(p%n==0) stop "[find_cdf_discrete] PDF is not set correctly (y)"
    p%cdf(1) = p%pdf(1)
    do i=2,p%n
       p%cdf(i) = p%cdf(i-1) + p%pdf(i)
    end do
    norm = p%cdf(p%n)
    if(norm==0._sp) stop "[find_cdf_discrete] all PDF elements are zero"
    p%cdf = p%cdf / norm
  end subroutine find_cdf_discrete_sp

  subroutine allocate_pdf_discrete_sp(p,n)
    implicit none
    type(pdf_discrete_sp),intent(out) :: p
    integer,intent(in) :: n
    p%n = n
    allocate(p%pdf(n))
    allocate(p%cdf(n))
  end subroutine allocate_pdf_discrete_sp

  subroutine allocate_pdf_cont_sp(p,n)
    implicit none
    type(pdf_sp),intent(out) :: p
    integer,intent(in) :: n
    p%n = n
    allocate(p%x(n))
    allocate(p%pdf(n))
    allocate(p%cdf(n))
  end subroutine allocate_pdf_cont_sp

  subroutine normalize_pdf_discrete_sp(p)
    implicit none
    type(pdf_discrete_sp),intent(inout) :: p
    real(sp) :: norm
    norm = sum(p%pdf)
    if(norm==0._sp) stop "[normalize_pdf_discrete] all PDF elements are zero"
    p%pdf = p%pdf / norm
    p%normalized = .true.
  end subroutine normalize_pdf_discrete_sp

  subroutine normalize_pdf_cont_sp(p)
    implicit none
    type(pdf_sp),intent(inout) :: p
    if(p%log) then
       p%pdf = p%pdf / integral_loglog(p%x, p%pdf)
    else
       p%pdf = p%pdf / integral(p%x, p%pdf)
    end if
    p%normalized = .true.
  end subroutine normalize_pdf_cont_sp

  subroutine set_pdf_discrete_sp(p,y)
    implicit none
    type(pdf_discrete_sp),intent(out) :: p
    real(sp),intent(in) :: y(:)
    call allocate_pdf(p,size(y))
    p%pdf = y
    call normalize_pdf(p)
    call find_cdf(p)
    call check_pdf(p)
  end subroutine set_pdf_discrete_sp

  subroutine set_pdf_cont_sp(p,x,y,log,simple)
    implicit none
    type(pdf_sp),intent(out) :: p
    real(sp),intent(in) :: x(:),y(:)
    logical,intent(in),optional :: log,simple
    if(size(x).ne.size(y)) stop "[set_pdf] x and y array sizes differ"
    call allocate_pdf(p,size(y))
    p%x   = x
    p%pdf = y
    if(present(log)) p%log = log
    if(present(simple)) p%simple = simple
    call normalize_pdf(p)
    call find_cdf(p)
    call check_pdf(p)
  end subroutine set_pdf_cont_sp

  subroutine check_pdf_discrete_sp(p)
    implicit none
    type(pdf_discrete_sp),intent(in) :: p
    if(p%n==0) stop "[check_pdf_discrete] PDF size not set"
    if(.not.allocated(p%pdf)) stop "[check_pdf_discrete] PDF pdf array not allocated"
    if(.not.allocated(p%cdf)) stop "[check_pdf_discrete] PDF cdf array not allocated"
    if(p%n.ne.size(p%pdf)) stop "[check_pdf_discrete] PDF pdf array has incorrect size"
    if(p%n.ne.size(p%cdf)) stop "[check_pdf_discrete] PDF cdf array has incorrect size"
    if(any(p%pdf < 0.)) stop "[check_pdf_discrete] PDF has negative probabilities"
    if(.not.p%normalized) stop "[check_pdf_discrete] PDF is not normalized"
  end subroutine check_pdf_discrete_sp

  subroutine check_pdf_cont_sp(p)
    implicit none
    type(pdf_sp),intent(in) :: p
    integer :: i
    if(p%n==0) stop "[check_pdf] PDF size not set"
    if(.not.allocated(p%x))   stop "[check_pdf] PDF x array not allocated"
    if(.not.allocated(p%pdf)) stop "[check_pdf] PDF pdf array not allocated"
    if(.not.allocated(p%cdf)) stop "[check_pdf] PDF cdf array not allocated"
    if(p%n.ne.size(p%x))   stop "[check_pdf] PDF x array has incorrect size"
    if(p%n.ne.size(p%pdf)) stop "[check_pdf] PDF pdf array has incorrect size"
    if(p%n.ne.size(p%cdf)) stop "[check_pdf] PDF cdf array has incorrect size"
    do i=2,p%n
       if(.not.(p%x(i)>p%x(i-1))) stop "[check_pdf] PDF x array is not sorted"
    end do
    if(any(p%pdf < 0.)) stop "[check_pdf] PDF has negative probabilities"
    if(.not.p%normalized) stop "[check_pdf] PDF is not normalized"
  end subroutine check_pdf_cont_sp

  subroutine find_cdf_cont_sp(p)
    implicit none
    type(pdf_sp),intent(inout) :: p
    integer :: i
    call check_pdf(p)
    if(p%log) then
       p%cdf = cumulative_integral_loglog(p%x,p%pdf)
    else
       p%cdf = cumulative_integral(p%x,p%pdf)
    end if
    p%cdf = p%cdf / p%cdf(p%n)
    if(.not.p%simple) then
       if(p%log) then
          allocate(p%b(p%n-1))
          allocate(p%r(p%n-1))
          do i=1,p%n-1
             p%b(i) = log10(p%pdf(i) / p%pdf(i+1)) / log10(p%x(i) / p%x(i+1))
             p%r(i) = (p%x(i+1) / p%x(i)) ** (p%b(i) + 1._dp)
          end do
       else
          allocate(p%a(p%n-1))
          allocate(p%b(p%n-1))
          allocate(p%rx(p%n-1))
          allocate(p%rc(p%n-1))
          do i=1,p%n-1
             p%a(i) = (p%pdf(i) - p%pdf(i+1)) / (p%x(i) - p%x(i+1))
             p%b(i) = p%pdf(i) - p%a(i) * p%x(i)
             p%rx(i) = p%x(i+1) / p%x(i)
             p%rc(i) = p%b(i) / p%a(i)
          end do
       end if
    end if
  end subroutine find_cdf_cont_sp

  integer function sample_pdf_discrete_sp(p)
    implicit none
    type(pdf_discrete_sp) :: p
    integer :: j,jmin,jmax
    real(sp) :: xi
    call random(xi)
    if(xi <= p%cdf(1)) then
       sample_pdf_discrete_sp = 1
    else if(xi >= p%cdf(p%n)) then
       sample_pdf_discrete_sp = p%n
    else
       jmin = 1
       jmax = p%n
       do
          j = (jmax + jmin) / 2
          if(xi > p%cdf(j)) then
             jmin = j
          else
             jmax = j
          end if
          if(jmax == jmin + 1) exit
       end do
       sample_pdf_discrete_sp = jmax
    end if
  end function sample_pdf_discrete_sp

  real(sp) function sample_pdf_cont_sp(p,xi_alt)
    implicit none
    type(pdf_sp),intent(in) :: p
    real(sp),optional,intent(in) :: xi_alt
    real(sp) :: xi
    integer :: i
    if(present(xi_alt)) then
       xi = xi_alt
    else
       call random(xi)
    end if
    if(xi <= p%cdf(1)) then
       sample_pdf_cont_sp = p%x(1)
    else if(xi >= p%cdf(p%n)) then
       sample_pdf_cont_sp = p%x(p%n)
    else
       if(p%simple) then
          if(p%log) then
             sample_pdf_cont_sp = interp1d_linlog(p%cdf(:), p%x(:), xi)
          else
             sample_pdf_cont_sp = interp1d(p%cdf(:), p%x(:), xi)
          end if
       else
          i = locate(p%cdf, xi)
          xi = (xi - p%cdf(i)) / (p%cdf(i+1) - p%cdf(i))
          if(p%log) then
             sample_pdf_cont_sp = (xi * (p%r(i) - 1._dp) + 1._dp) ** (1._dp / (p%b(i) + 1._dp)) * p%x(i)
          else
             if(p%a(i)==0._dp) then
                sample_pdf_cont_sp = xi * (p%x(i+1) - p%x(i)) + p%x(i)
             else if(p%x(i)==0._dp) then
                sample_pdf_cont_sp = - p%rc(i) + sign(sqrt(p%rc(i) * p%rc(i) &
                     &                                      + xi * p%x(i+1) * p%x(i+1) &
                     &                                      + 2._sp * p%rc(i) * xi * p%x(i+1)), p%a(i))
             else
                sample_pdf_cont_sp = - p%rc(i) + sign(sqrt(p%rc(i) * p%rc(i) &
                     &                                      + p%x(i) * p%x(i) * (xi * (p%rx(i) * p%rx(i) - 1._sp) + 1._sp) &
                     &                                      + 2._sp * p%rc(i) * p%x(i) * (xi * (p%rx(i) - 1._sp) + 1._sp)), p%a(i))
             end if
          end if
       end if
    end if
  end function sample_pdf_cont_sp

  real(sp) function sample_pdf_cont_log_sp(p,xi_alt)
    implicit none
    type(pdf_sp),intent(in) :: p
    real(sp),optional,intent(in) :: xi_alt
    real(sp) :: xi
    if(present(xi_alt)) then
       xi = xi_alt
    else
       call random(xi)
    end if
    sample_pdf_cont_log_sp = interp1d_loglog(p%cdf(:),p%x(:),xi)
  end function sample_pdf_cont_log_sp

  real(sp) function interpolate_pdf_cont_sp(p, x, bounds_error, fill_value) result(prob)
    implicit none
    type(pdf_sp),intent(in) :: p
    real(sp),intent(in) :: x
    logical,intent(in),optional :: bounds_error
    real(sp),intent(in),optional :: fill_value
    if(.not.p%normalized) stop "[interpolate_pdf] PDF is not normalized"
    if(p%log) then
       prob = interp1d_loglog(p%x, p%pdf, x, bounds_error, fill_value)
    else
       prob = interp1d(p%x, p%pdf, x, bounds_error, fill_value)
    end if
  end function interpolate_pdf_cont_sp


end module type_pdf

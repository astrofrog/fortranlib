! MD5 of template: 13982be359376d65d4f966553ef27636
! Array related routines (Integration, Interpolation, etc.)
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

module lib_array

  implicit none
  save

  private

  integer,parameter :: idp = selected_int_kind(13)
  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  public :: is_nan
  interface is_nan
     module procedure is_nan_int
     module procedure is_nan_int8
     module procedure is_nan_sp
     module procedure is_nan_dp
  end interface is_nan

  public :: linspace
  interface linspace
     module procedure linspace_sp
     module procedure linspace_dp
  end interface linspace

  public :: logspace
  interface logspace
     module procedure logspace_sp
     module procedure logspace_dp
  end interface logspace

  public :: integral
  interface integral
     module procedure integral_sp
     module procedure integral_dp
     module procedure integral_subset_sp
     module procedure integral_subset_dp
  end interface integral

  public :: cumulative_integral
  interface cumulative_integral
     module procedure cumulative_integral_sp
     module procedure cumulative_integral_dp
     ! module procedure cumulative_integral_subset_sp
     ! module procedure cumulative_integral_subset_dp
  end interface cumulative_integral

  public :: integral_linlog
  interface integral_linlog
     module procedure integral_linlog_sp
     module procedure integral_linlog_dp
     module procedure integral_linlog_subset_sp
     module procedure integral_linlog_subset_dp
  end interface integral_linlog

  public :: cumulative_integral_linlog
  interface cumulative_integral_linlog
     module procedure cumulative_integral_linlog_sp
     module procedure cumulative_integral_linlog_dp
     ! module procedure cumulative_integral_linlog_subset_sp
     ! module procedure cumulative_integral_linlog_subset_dp
  end interface cumulative_integral_linlog

  public :: integral_loglin
  interface integral_loglin
     module procedure integral_loglin_sp
     module procedure integral_loglin_dp
     module procedure integral_loglin_subset_sp
     module procedure integral_loglin_subset_dp
  end interface integral_loglin

  public :: cumulative_integral_loglin
  interface cumulative_integral_loglin
     module procedure cumulative_integral_loglin_sp
     module procedure cumulative_integral_loglin_dp
     ! module procedure cumulative_integral_loglin_subset_sp
     ! module procedure cumulative_integral_loglin_subset_dp
  end interface cumulative_integral_loglin

  public :: integral_loglog
  interface integral_loglog
     module procedure integral_loglog_sp
     module procedure integral_loglog_dp
     module procedure integral_loglog_subset_sp
     module procedure integral_loglog_subset_dp
  end interface integral_loglog

  public :: cumulative_integral_loglog
  interface cumulative_integral_loglog
     module procedure cumulative_integral_loglog_sp
     module procedure cumulative_integral_loglog_dp
     ! module procedure cumulative_integral_loglog_subset_sp
     ! module procedure cumulative_integral_loglog_subset_dp
  end interface cumulative_integral_loglog

  public :: locate
  interface locate
     module procedure locate_sp
     module procedure locate_dp
  end interface locate

  public :: interp1d
  interface interp1d
     module procedure interp1d_sp
     module procedure interp1d_dp
     module procedure interp1d_array_sp
     module procedure interp1d_array_dp
  end interface interp1d

  public :: interp1d_linlog
  interface interp1d_linlog
     module procedure interp1d_linlog_sp
     module procedure interp1d_linlog_dp
     module procedure interp1d_array_linlog_sp
     module procedure interp1d_array_linlog_dp
  end interface interp1d_linlog

  public :: interp1d_loglin
  interface interp1d_loglin
     module procedure interp1d_loglin_sp
     module procedure interp1d_loglin_dp
     module procedure interp1d_array_loglin_sp
     module procedure interp1d_array_loglin_dp
  end interface interp1d_loglin

  public :: interp1d_loglog
  interface interp1d_loglog
     module procedure interp1d_loglog_sp
     module procedure interp1d_loglog_dp
     module procedure interp1d_array_loglog_sp
     module procedure interp1d_array_loglog_dp
  end interface interp1d_loglog

  public :: rebin
  interface rebin
     module procedure rebin_sp
     module procedure rebin_dp
  end interface rebin

  public :: rebin_linlog
  interface rebin_linlog
     module procedure rebin_linlog_sp
     module procedure rebin_linlog_dp
  end interface rebin_linlog

  public :: rebin_loglin
  interface rebin_loglin
     module procedure rebin_loglin_sp
     module procedure rebin_loglin_dp
  end interface rebin_loglin

  public :: rebin_loglog
  interface rebin_loglog
     module procedure rebin_loglog_sp
     module procedure rebin_loglog_dp
  end interface rebin_loglog

  public :: interp2d
  interface interp2d
     module procedure interp2d_sp
     module procedure interp2d_dp
  end interface interp2d

  public :: histogram1d
  interface histogram1d
     module procedure histogram1d_sp
     module procedure histogram1d_dp
  end interface histogram1d

  public :: histogram2d
  interface histogram2d
     module procedure histogram2d_sp
     module procedure histogram2d_dp
  end interface histogram2d

  public :: ipos
  interface ipos
     module procedure ipos_sp
     module procedure ipos_dp
  end interface ipos

  public :: xval
  interface xval
     module procedure xval_sp
     module procedure xval_dp
  end interface xval

  public :: quicksort
  interface quicksort
     module procedure quicksort_sp
     module procedure quicksort_all_sp
     module procedure quicksort_dp
     module procedure quicksort_all_dp
  end interface quicksort

  public :: quicksort_index
  interface quicksort_index
     module procedure quicksort_index_sp
     module procedure quicksort_index_all_sp
     module procedure quicksort_index_dp
     module procedure quicksort_index_all_dp
  end interface quicksort_index

  public :: test_quicksort_sp
  public :: test_quicksort_dp

  public :: int
  interface int
     module procedure logical2int
  end interface int

contains

  elemental integer function logical2int(a) result(b)
    implicit none
    logical,intent(in) :: a
    if(a) then
       b = 1
    else
       b = 0
    end if
  end function logical2int


  logical elemental function is_nan_int8(value) result(nan)
    ! x != x does not work with pgfortran
    integer(idp),intent(in) :: value
    nan = .not. (value > -1 .or. value < 1)
  end function is_nan_int8


  logical elemental function is_nan_int(value) result(nan)
    ! x != x does not work with pgfortran
    integer,intent(in) :: value
    nan = .not. (value > -1 .or. value < 1)
  end function is_nan_int


  logical elemental function is_nan_dp(value) result(nan)
    ! x != x does not work with pgfortran
    real(dp),intent(in) :: value
    nan = .not. (value > -1 .or. value < 1)
  end function is_nan_dp


  logical elemental function is_nan_sp(value) result(nan)
    ! x != x does not work with pgfortran
    real(sp),intent(in) :: value
    nan = .not. (value > -1 .or. value < 1)
  end function is_nan_sp



  subroutine linspace_dp(xmin,xmax,x)
    implicit none
    real(dp),intent(in) :: xmin,xmax
    real(dp),intent(out) :: x(:)
    integer :: i,n
    n = size(x)
    if (n == 1) then
       if(xmin /= xmax) then
          write(0,'("ERROR: Cannot call linspace with n=1 and xmin /= xmax")')
          stop
       else
          x = xmin
       end if
    else
       do i=1,n
          x(i) = (xmax-xmin) * real(i-1,dp) / real(n-1,dp) + xmin
       end do
    end if
  end subroutine linspace_dp

  subroutine logspace_dp(xmin,xmax,x)
    implicit none
    real(dp),intent(in) :: xmin,xmax
    real(dp),intent(out) :: x(:)
    if (size(x) == 1 .and. xmin /= xmax) then
       write(0,'("ERROR: Cannot call logspace with n=1 and xmin /= xmax")')
       stop
    end if
    call linspace(log10(xmin),log10(xmax),x)
    x = 10._dp**x
  end subroutine logspace_dp

  real(dp) function integral_dp(x,y)
    ! Total integral of a function
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    integral_dp = integral_general_dp(x, y, trapezium_dp)
  end function integral_dp

  real(dp) function integral_subset_dp(x,y,x1,x2)
    ! Integral of a function between two limits
    implicit none
    real(dp),intent(in) :: x(:),y(:),x1,x2
    integral_subset_dp = integral_general_subset_dp(x, y, x1, x2, interp1d_dp, trapezium_dp)
  end function integral_subset_dp

  real(dp) function integral_linlog_dp(x,y)
    ! Total integral of a function
    ! (uses linlog interpolation)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    integral_linlog_dp = integral_general_dp(x, y, trapezium_linlog_dp)
  end function integral_linlog_dp

  real(dp) function integral_linlog_subset_dp(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses linlog interpolation)
    implicit none
    real(dp),intent(in) :: x(:),y(:),x1,x2
    integral_linlog_subset_dp = integral_general_subset_dp(x, y, x1, x2, interp1d_linlog_dp, trapezium_linlog_dp)
  end function integral_linlog_subset_dp

  real(dp) function integral_loglin_dp(x,y)
    ! Total integral of a function
    ! (uses loglin interpolation)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    integral_loglin_dp = integral_general_dp(x, y, trapezium_loglin_dp)
  end function integral_loglin_dp

  real(dp) function integral_loglin_subset_dp(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses loglin interpolation)
    implicit none
    real(dp),intent(in) :: x(:),y(:),x1,x2
    integral_loglin_subset_dp = integral_general_subset_dp(x, y, x1, x2, interp1d_loglin_dp, trapezium_loglin_dp)
  end function integral_loglin_subset_dp

  real(dp) function integral_loglog_dp(x,y)
    ! Total integral of a function
    ! (uses log10 interpolation)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    integral_loglog_dp = integral_general_dp(x, y, trapezium_loglog_dp)
  end function integral_loglog_dp

  real(dp) function integral_loglog_subset_dp(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses log10 interpolation)
    implicit none
    real(dp),intent(in) :: x(:),y(:),x1,x2
    integral_loglog_subset_dp = integral_general_subset_dp(x, y, x1, x2, interp1d_loglog_dp, trapezium_loglog_dp)
  end function integral_loglog_subset_dp

  function cumulative_integral_dp(x,y)
    ! Total cumulative_integral of a function
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp), dimension(size(y)) :: cumulative_integral_dp
    cumulative_integral_dp = cumulative_integral_general_dp(x, y, trapezium_dp)
  end function cumulative_integral_dp

  function cumulative_integral_linlog_dp(x,y)
    ! Total cumulative_integral of a function
    ! (uses linlog interpolation)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp), dimension(size(y)) :: cumulative_integral_linlog_dp
    cumulative_integral_linlog_dp = cumulative_integral_general_dp(x, y, trapezium_linlog_dp)
  end function cumulative_integral_linlog_dp

  function cumulative_integral_loglin_dp(x,y)
    ! Total cumulative_integral of a function
    ! (uses loglin interpolation)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp), dimension(size(y)) :: cumulative_integral_loglin_dp
    cumulative_integral_loglin_dp = cumulative_integral_general_dp(x, y, trapezium_loglin_dp)
  end function cumulative_integral_loglin_dp

  function cumulative_integral_loglog_dp(x,y)
    ! Total cumulative_integral of a function
    ! (uses log10 interpolation)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp), dimension(size(y)) :: cumulative_integral_loglog_dp
    cumulative_integral_loglog_dp = cumulative_integral_general_dp(x, y, trapezium_loglog_dp)
  end function cumulative_integral_loglog_dp

  real(dp) function integral_general_dp(x,y,f_chunk) result(sum)
    ! Total integral of a function
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    integer :: j
    interface
       real(dp) function f_chunk(x1,y1,x2,y2)
         import :: dp
         implicit none
         real(dp),intent(in) :: x1,y1,x2,y2
       end function f_chunk
    end interface
    sum = 0._dp
    do j=1,size(x)-1
       sum=sum+f_chunk(x(j),y(j),x(j+1),y(j+1))
    end do
  end function integral_general_dp

  function cumulative_integral_general_dp(x,y,f_chunk) result(c)
    ! Total integral of a function
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    integer :: j
    interface
       real(dp) function f_chunk(x1,y1,x2,y2)
         import :: dp
         implicit none
         real(dp),intent(in) :: x1,y1,x2,y2
       end function f_chunk
    end interface
    real(dp), dimension(size(y)) :: c
    c(1) = 0._dp
    do j=1,size(x)-1
       c(j+1)=c(j)+f_chunk(x(j),y(j),x(j+1),y(j+1))
    end do
  end function cumulative_integral_general_dp

  real(dp) function integral_general_subset_dp(x,y,x1,x2,f_interp,f_chunk) result(sum)
    ! Integral of a function between two limits

    implicit none

    real(dp),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(dp) :: f1,f2
    integer :: n

    interface
       real(dp) function f_interp(x, y, xval,bounds_error,fill_value)
         import :: dp
         implicit none
         real(dp),dimension(:),intent(in) :: x,y
         real(dp),intent(in) :: xval
         logical,intent(in),optional :: bounds_error
         real(dp),intent(in),optional :: fill_value
       end function f_interp
       real(dp) function f_chunk(x1,y1,x2,y2)
         import :: dp
         implicit none
         real(dp),intent(in) :: x1,y1,x2,y2
       end function f_chunk
    end interface

    n = size(x)

    if(x1.gt.x(n).or.x2.lt.x(1)) then
       sum = 0._dp
       return
    end if

    if(x1.gt.x(1)) then
       i1 = locate(x,x1)
       f1 = f_interp(x,y,x1)
    else
       i1 = 0
       f1 = 0._dp
    end if

    if(x2.lt.x(n)) then
       i2 = locate(x,x2)
       f2 = f_interp(x,y,x2)
    else
       i2 = n
       f2 = 0._dp
    end if

    if(i2.gt.i1) then

       ! Add main part
       if(i2 > i1+1) then
          sum = integral_general_dp(x(i1+1:i2), y(i1+1:i2), f_chunk)
       else
          sum = 0._dp
       end if

       ! Add extremities
       sum = sum + f_chunk(x1,f1,x(i1+1),y(i1+1))
       sum = sum + f_chunk(x(i2),y(i2),x2,f2)

    else

       sum = f_chunk(x1,f1,x2,f2)

    end if

  end function integral_general_subset_dp

  real(dp) function trapezium_dp(x1,y1,x2,y2)
    implicit none
    real(dp),intent(in) :: x1,y1,x2,y2
    trapezium_dp = 0.5_dp*(y1+y2)*(x2-x1)
  end function trapezium_dp

  real(dp) function trapezium_loglin_dp(x1,y1,x2,y2)
    implicit none
    real(dp),intent(in) :: x1,y1,x2,y2
    real(dp) :: a,b
    if(x1==x2) then
       trapezium_loglin_dp = 0._dp
    else
       a = (y1-y2) / log10(x1/x2)
       b = y1 - a * log10(x1)
       trapezium_loglin_dp = a*(x2*log10(x2) - x1*log10(x1)) &
            &                     + (b-a/log(10._dp)) * (x2 - x1)
    end if
  end function trapezium_loglin_dp

  real(dp) function trapezium_linlog_dp(x1,y1,x2,y2)
    implicit none
    real(dp),intent(in) :: x1,y1,x2,y2
    real(dp) :: dx
    if(x1==x2) then
       trapezium_linlog_dp = 0._dp
    else if(y1==y2) then
       trapezium_linlog_dp = y1*(x2-x1)
    else
       dx = x2 - x1
       trapezium_linlog_dp = (y2-y1)*(x2-x1)/log(10._dp)/log10(y2/y1)
    end if
  end function trapezium_linlog_dp

  real(dp) function trapezium_loglog_dp(x1,y1,x2,y2)
    implicit none
    real(dp),intent(in) :: x1,y1,x2,y2
    real(dp) :: b
    if(x1==x2) then
       trapezium_loglog_dp = 0._dp
    else if(y1==0..or.y2==0.) then
       trapezium_loglog_dp = 0._dp
    else
       b = log10(y1/y2) / log10(x1/x2)
       if(abs(b+1._dp) < 1e-10) then
          trapezium_loglog_dp = x1 * y1 * log(x2/x1)
       else
          trapezium_loglog_dp = y1 * (x2*(x2/x1)**b-x1) / (b+1)
       end if
    end if
  end function trapezium_loglog_dp

  real(dp) function interp1d_single_dp(x1,y1,x2,y2,xval) result(yval)
    real(dp),intent(in) :: x1,y1,x2,y2,xval
    real(dp) :: frac
    frac = ( xval - x1 ) / ( x2 - x1 )
    yval = y1 + frac * ( y2 - y1 )
  end function interp1d_single_dp

  real(dp) function interp1d_single_linlog_dp(x1,y1,x2,y2,xval) result(yval)
    real(dp),intent(in) :: x1,y1,x2,y2,xval
    real(dp) :: frac
    if(y1==0..or.y2==0.) then
       yval = 0._dp
    else
       frac = ( xval - x1 ) / ( x2 - x1 )
       yval = 10._dp**(log10(y1) + frac * ( log10(y2) - log10(y1) ))
    end if
  end function interp1d_single_linlog_dp

  real(dp) function interp1d_single_loglin_dp(x1,y1,x2,y2,xval) result(yval)
    real(dp),intent(in) :: x1,y1,x2,y2,xval
    real(dp) :: frac
    frac = ( log10(xval) - log10(x1) ) / ( log10(x2) - log10(x1) )
    yval = y1 + frac * ( y2 - y1 )
  end function interp1d_single_loglin_dp

  real(dp) function interp1d_single_loglog_dp(x1,y1,x2,y2,xval) result(yval)
    real(dp),intent(in) :: x1,y1,x2,y2,xval
    real(dp) :: frac
    if(y1==0..or.y2==0.) then
       yval = 0._dp
    else
       frac = ( log10(xval) - log10(x1) ) / ( log10(x2) - log10(x1) )
       yval = 10._dp**(log10(y1) + frac * ( log10(y2) - log10(y1) ))
    end if
  end function interp1d_single_loglog_dp

  real(dp) function interp1d_dp(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(dp),dimension(:),intent(in) :: x,y
    real(dp),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(dp),intent(in),optional :: fill_value
    yval = interp1d_general_dp(x,y,xval,interp1d_single_dp,bounds_error,fill_value)
  end function interp1d_dp

  function interp1d_array_dp(x,y,xval,bounds_error,fill_value)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(dp),intent(in),optional :: fill_value
    real(dp) :: interp1d_array_dp(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_dp(i) = interp1d_dp(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_dp

  real(dp) function interp1d_linlog_dp(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(dp),dimension(:),intent(in) :: x,y
    real(dp),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(dp),intent(in),optional :: fill_value
    yval = interp1d_general_dp(x,y,xval,interp1d_single_linlog_dp,bounds_error,fill_value)
  end function interp1d_linlog_dp

  function interp1d_array_linlog_dp(x,y,xval,bounds_error,fill_value)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(dp),intent(in),optional :: fill_value
    real(dp) :: interp1d_array_linlog_dp(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_linlog_dp(i) = interp1d_linlog_dp(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_linlog_dp

  real(dp) function interp1d_loglin_dp(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(dp),dimension(:),intent(in) :: x,y
    real(dp),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(dp),intent(in),optional :: fill_value
    yval = interp1d_general_dp(x,y,xval,interp1d_single_loglin_dp,bounds_error,fill_value)
  end function interp1d_loglin_dp

  function interp1d_array_loglin_dp(x,y,xval,bounds_error,fill_value)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(dp),intent(in),optional :: fill_value
    real(dp) :: interp1d_array_loglin_dp(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_loglin_dp(i) = interp1d_loglin_dp(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_loglin_dp

  real(dp) function interp1d_loglog_dp(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(dp),dimension(:),intent(in) :: x,y
    real(dp),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(dp),intent(in),optional :: fill_value
    yval = interp1d_general_dp(x,y,xval,interp1d_single_loglog_dp,bounds_error,fill_value)
  end function interp1d_loglog_dp

  function interp1d_array_loglog_dp(x,y,xval,bounds_error,fill_value)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(dp),intent(in),optional :: fill_value
    real(dp) :: interp1d_array_loglog_dp(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_loglog_dp(i) = interp1d_loglog_dp(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_loglog_dp

  real(dp) function interp1d_general_dp(x,y,xval,f_single,bounds_error,fill_value) result(yval)

    implicit none

    integer :: n
    ! the size of the array

    real(dp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(dp),intent(in) :: xval
    ! the value at which to interpolate y

    interface
       real(dp) function f_single(x1,y1,x2,y2,xval)
         import :: dp
         implicit none
         real(dp),intent(in) :: x1,y1,x2,y2,xval
       end function f_single
    end interface

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(dp),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    integer :: ipos
    ! position of x value in x array

    logical :: bounds_error_tmp
    real(dp) :: fill_value_tmp

    if(present(bounds_error)) then
       bounds_error_tmp = bounds_error
    else
       bounds_error_tmp = .true.
    end if

    if(.not.bounds_error_tmp) then
       if(present(fill_value)) then
          fill_value_tmp = fill_value
       else
          fill_value_tmp = 0._dp
       end if
    end if

    n = size(x)

    ipos = locate(x,xval)

    ! --- First some error checking --- !

    if(ipos == -1) then
       if(bounds_error_tmp) then
          write(0,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') xval,x(1),x(n)
          stop
       else
          yval = fill_value_tmp
          return
       end if
    end if

    if( ipos < n .and. ipos > 0) then
       yval = f_single(x(ipos), y(ipos), x(ipos+1), y(ipos+1), xval)
    else if(ipos == n) then
       yval = y(n)
    else if(ipos == 0) then
       yval = y(1)
    else
       write(0,'("ERROR: Unexpected value of ipos : ",I0)') ipos
       stop
    end if

  end function interp1d_general_dp

  function interp2d_dp(x,y,array,x0,y0,bounds_error,fill_value) result(value)
    ! Bilinar interpolation of array = f(x,y) at (x0,y0)

    implicit none

    real(dp),intent(in) :: x(:),y(:),array(:,:),x0,y0

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(dp),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    real(dp) :: value,norm
    integer :: i1,i2,j1,j2

    logical :: bounds_error_tmp
    real(dp) :: fill_value_tmp

    if(present(bounds_error)) then
       bounds_error_tmp = bounds_error
    else
       bounds_error_tmp = .true.
    end if

    if(.not.bounds_error_tmp) then
       if(present(fill_value)) then
          fill_value_tmp = fill_value
       else
          fill_value_tmp = 0._dp
       end if
    end if

    if(size(x).ne.size(array,1)) stop "x does not match array"
    if(size(y).ne.size(array,2)) stop "y does not match array"

    i1 = locate(x,x0) ; i2 = i1 + 1
    j1 = locate(y,y0) ; j2 = j1 + 1

    if(i1==-1) then
       if(bounds_error_tmp) then
          write(0,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') x0,x(1),x(size(x))
          stop
       else
          value = fill_value_tmp
          return
       end if
    end if

    if(j1==-1) then
       if(bounds_error_tmp) then
          write(0,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') y0,y(1),y(size(y))
          stop
       else
          value = fill_value_tmp
          return
       end if
    end if

    norm = 1._dp / (x(i2) - x(i1)) / (y(j2)-y(j1))

    value =   array(i1,j1) * (x(i2)-x0) * (y(j2)-y0) * norm &
         &       + array(i2,j1) * (x0-x(i1)) * (y(j2)-y0) * norm &
         &       + array(i1,j2) * (x(i2)-x0) * (y0-y(j1)) * norm &
         &       + array(i2,j2) * (x0-x(i1)) * (y0-y(j1)) * norm

  end function interp2d_dp

  real(dp) function average_lin_dp(a,b)
    implicit none
    real(dp),intent(in) :: a,b
    average_lin_dp = 0.5_dp * (a + b)
  end function average_lin_dp

  real(dp) function average_log_dp(a,b)
    implicit none
    real(dp),intent(in) :: a,b
    average_log_dp = sqrt(a*b)
  end function average_log_dp

  subroutine rebin_dp(x, y, xx, yy)
    real(dp),intent(in) :: x(:), y(:), xx(:)
    real(dp),intent(out) :: yy(:)
    call rebin_general_dp(x, y, xx, yy, average_lin_dp, integral_subset_dp)
  end subroutine rebin_dp

  subroutine rebin_linlog_dp(x, y, xx, yy)
    real(dp),intent(in) :: x(:), y(:), xx(:)
    real(dp),intent(out) :: yy(:)
    call rebin_general_dp(x, y, xx, yy, average_lin_dp, integral_linlog_subset_dp)
  end subroutine rebin_linlog_dp

  subroutine rebin_loglin_dp(x, y, xx, yy)
    real(dp),intent(in) :: x(:), y(:), xx(:)
    real(dp),intent(out) :: yy(:)
    call rebin_general_dp(x, y, xx, yy, average_log_dp, integral_loglin_subset_dp)
  end subroutine rebin_loglin_dp

  subroutine rebin_loglog_dp(x, y, xx, yy)
    real(dp),intent(in) :: x(:), y(:), xx(:)
    real(dp),intent(out) :: yy(:)
    call rebin_general_dp(x, y, xx, yy, average_log_dp, integral_loglog_subset_dp)
  end subroutine rebin_loglog_dp

  subroutine rebin_general_dp(x, y, xx, yy, f_average, f_integral)
    implicit none
    real(dp),intent(in) :: x(:), y(:), xx(:)
    real(dp),intent(out) :: yy(:)
    integer :: i
    real(dp) :: xmin, xmax
    interface
       real(dp) function f_average(a,b)
         import :: dp
         implicit none
         real(dp),intent(in) :: a,b
       end function f_average
       real(dp) function f_integral(x,y,x1,x2)
         import :: dp
         implicit none
         real(dp),intent(in) :: x(:),y(:),x1,x2
       end function f_integral
    end interface
    do i=1,size(xx)
       if(i==1) then
          xmin = xx(1)
       else
          xmin = f_average(xx(i-1), xx(i))
       end if
       if(i==size(xx)) then
          xmax = xx(size(xx))
       else
          xmax = f_average(xx(i), xx(i+1))
       end if
       yy(i) = f_integral(x, y, xmin, xmax) / (xmax - xmin)
    end do
  end subroutine rebin_general_dp

  integer function locate_dp(xx,x)
    ! Locate a value in a sorted array

    implicit none
    real(dp), dimension(:), intent(in) :: xx
    real(dp), intent(in) :: x
    integer :: n,jl,jm,ju
    logical :: ascnd
    n=size(xx)
    ascnd = (xx(n) >= xx(1))
    jl=0
    ju=n+1
    do
       if (ju-jl <= 1) exit
       jm=(ju+jl)/2
       if (ascnd .eqv. (x >= xx(jm))) then
          jl=jm
       else
          ju=jm
       end if
    end do

    if (x == xx(1)) then
       locate_dp = 1
    else if (x == xx(n)) then
       locate_dp = n-1
    else if(ascnd.and. (x > xx(n) .or. x < xx(1))) then
       locate_dp = -1
    else if(.not.ascnd.and. (x < xx(n) .or. x > xx(1))) then
       locate_dp = -1
    else
       locate_dp = jl
    end if

  end function locate_dp


  integer function ipos_dp(xmin,xmax,x,nbin)
    ! Find bin a value falls in for a regular histogram

    implicit none

    real(dp),intent(in) :: xmin,xmax
    ! range of values

    real(dp),intent(in) :: x
    ! the value to bin

    integer,intent(in) :: nbin
    ! number of bins

    real(dp) :: frac

    if(xmax > xmin) then

       if(x < xmin) then
          ipos_dp = 0
       else if(x > xmax) then
          ipos_dp = nbin+1
       else if(x < xmax) then
          frac=(x-xmin)/(xmax-xmin)
          ipos_dp=int(frac*real(nbin, dp))+1
       else  ! x == xmax
          ipos_dp = nbin
       end if

    else

       if(x < xmax) then
          ipos_dp = 0
       else if(x > xmin) then
          ipos_dp = nbin+1
       else if(x < xmin) then
          frac=(x-xmin)/(xmax-xmin)
          ipos_dp=int(frac*real(nbin, dp))+1
       else  ! x == xmin
          ipos_dp = nbin
       end if

    end if

  end function ipos_dp


  real(dp) function xval_dp(xmin,xmax,i,nbin)
    ! Find central value of a bin for a regular histogram

    implicit none

    real(dp),intent(in) :: xmin,xmax
    ! range of values

    integer,intent(in) :: i
    ! the bin number

    integer,intent(in) :: nbin
    ! number of bins

    real(dp) :: frac

    frac=(real(i-1)+0.5)/real(nbin)

    xval_dp=frac*(xmax-xmin)+xmin

  end function xval_dp


  subroutine histogram1d_dp(array,xmin,xmax,nbin,hist_x,hist_y,mask,weights)
    ! Bin 1D array of values into 1D regular histogram

    implicit none

    real(dp),dimension(:),intent(in) :: array
    real(dp),dimension(:),intent(in),optional :: weights
    ! the array of values to bin

    real(dp),intent(in) :: xmin,xmax
    ! the range of the histogram

    integer,intent(in) :: nbin
    ! number of bins

    real(dp),dimension(nbin),intent(out) :: hist_x,hist_y
    ! the histogram

    integer :: i,ibin
    ! binning variables

    logical,optional,intent(in) :: mask(:)
    logical,allocatable:: keep(:)

    allocate(keep(size(array)))

    if(present(mask)) then
       keep = mask
    else
       keep = .true.
    end if

    hist_x=0._dp ; hist_y=0._dp

    do i=1,size(array)
       if(keep(i)) then
          ibin=ipos(xmin,xmax,array(i),nbin)
          if(ibin.ge.1.and.ibin.le.nbin) then
             if(present(weights)) then
                hist_y(ibin)=hist_y(ibin)+weights(i)
             else
                hist_y(ibin)=hist_y(ibin)+1._dp
             end if
          end if
       end if
    end do

    do ibin=1,nbin
       hist_x(ibin)=xval(xmin,xmax,ibin,nbin)
    end do

    deallocate(keep)

  end subroutine histogram1d_dp


  subroutine histogram2d_dp(n,x,y,xmin,xmax,ymin,ymax,nx,ny,array,mask_in)
    ! Bin two 1D array of values into 2D regular grid

    implicit none

    integer,intent(in) :: n
    ! number of values to bin

    real(dp),dimension(n),intent(in) :: x,y
    logical,dimension(n),intent(in),optional :: mask_in
    ! the array of values to bin

    real(dp),intent(in) :: xmin,xmax,ymin,ymax
    ! the range of the histogram

    integer,intent(in) :: nx,ny
    ! number of bins

    real(dp),dimension(nx,ny),intent(out) :: array
    ! the histogram

    integer :: i,ix,iy
    ! binning variables

    logical,dimension(n) :: mask

    if(present(mask_in)) then
       mask = mask_in
    else
       mask = .true.
    end if

    array = 0._dp

    do i=1,n

       if(mask(i)) then

          ix=ipos(xmin,xmax,x(i),nx)
          iy=ipos(ymin,ymax,y(i),ny)

          if(ix.ge.1.and.ix.le.nx) then
             if(iy.ge.1.and.iy.le.ny) then
                array(ix,iy) = array(ix,iy) + 1.
             end if
          end if

       end if

    end do

  end subroutine histogram2d_dp


  subroutine swap_dp(array, i, j)
    implicit none
    real(dp),intent(inout) :: array(:)
    integer,intent(in) :: i, j
    real(dp) :: temp
    temp = array(j)
    array(j) = array(i)
    array(i) = temp
  end subroutine swap_dp

  subroutine partition_dp(array, left, right, pivot_index, store_index)
    implicit none
    real(dp),intent(inout) :: array(:)
    integer,intent(in) :: left, right, pivot_index
    integer,intent(out) :: store_index
    real(dp) :: pivot_value
    integer :: i
    pivot_value = array(pivot_index)
    call swap_dp(array, pivot_index, right)
    store_index = left
    do i=left, right-1
       if(array(i) <= pivot_value) then
          call swap_dp(array, i, store_index)
          store_index = store_index + 1
       end if
    end do
    call swap_dp(array, store_index, right)
  end subroutine partition_dp

  recursive subroutine quicksort_dp(array, left, right)
    implicit none
    real(dp),intent(inout) :: array(:)
    integer,intent(in) :: left, right
    integer :: pivot_index, new_pivot_index
    if(right > left) then
       pivot_index = left+(right-left)/2
       call partition_dp(array, left, right, pivot_index, new_pivot_index)
       call quicksort_dp(array, left, new_pivot_index - 1)
       call quicksort_dp(array, new_pivot_index + 1, right)
    end if
  end subroutine quicksort_dp

  recursive subroutine quicksort_all_dp(array)
    implicit none
    real(dp),intent(inout) :: array(:)
    call quicksort_dp(array, 1, size(array))
  end subroutine quicksort_all_dp

  subroutine swap_index_dp(array, index, i, j)
    implicit none
    real(dp),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    integer,intent(in) :: i, j
    real(dp) :: temp
    integer :: temp_i
    temp = array(j)
    temp_i = index(j)
    array(j) = array(i)
    index(j) = index(i)
    array(i) = temp
    index(i) = temp_i
  end subroutine swap_index_dp

  subroutine partition_index_dp(array, index, left, right, pivot_index, store_index)
    implicit none
    real(dp),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    integer,intent(in) :: left, right, pivot_index
    integer,intent(out) :: store_index
    real(dp) :: pivot_value
    integer :: i
    pivot_value = array(pivot_index)
    call swap_index_dp(array, index, pivot_index, right)
    store_index = left
    do i=left, right-1
       if(array(i) <= pivot_value) then
          call swap_index_dp(array, index, i, store_index)
          store_index = store_index + 1
       end if
    end do
    call swap_index_dp(array, index, store_index, right)
  end subroutine partition_index_dp

  recursive subroutine quicksort_index_dp(array, index, left, right)
    implicit none
    real(dp),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    integer,intent(in) :: left, right
    integer :: pivot_index, new_pivot_index
    if(right > left) then
       pivot_index = left+(right-left)/2
       call partition_index_dp(array, index, left, right, pivot_index, new_pivot_index)
       call quicksort_index_dp(array, index, left, new_pivot_index - 1)
       call quicksort_index_dp(array, index, new_pivot_index + 1, right)
    end if
  end subroutine quicksort_index_dp

  recursive subroutine quicksort_index_all_dp(array, index)
    implicit none
    real(dp),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    call quicksort_index_dp(array, index, 1, size(array))
  end subroutine quicksort_index_all_dp

  subroutine check_sort_dp(array)
    implicit none
    real(dp),intent(inout) :: array(:)
    integer :: j
    do j=1,size(array)-1
       if(array(j+1) < array(j)) stop "Incorrectly sorted array"
    end do
  end subroutine check_sort_dp

  subroutine test_quicksort_dp()

    implicit none

    real(dp),allocatable :: array1(:), array2(:)
    integer,allocatable :: index(:)
    integer :: j,i

    write(*,'(" Testing quicksort")')

    do j=1,20

       write(*,'("  - n=",I0)') 2**j

       allocate(array1(2**j))

       call random_number(array1)

       call quicksort(array1)

       call check_sort_dp(array1)

       deallocate(array1)

    end do

    write(*,'(" Testing quicksort_index")')

    do j=1,20

       write(*,'("  - n=",I0)') 2**j

       allocate(array1(2**j), array2(2**j), index(2**j))

       call random_number(array1)
       array2 = array1
       forall(i=1:2**j) index(i) = i

       call quicksort_index(array1, index)

       call check_sort_dp(array1)

       if(any(array2(index).ne.array1)) stop "Failure in quicksort_index"

       deallocate(array1, array2, index)

    end do

  end subroutine test_quicksort_dp


  subroutine linspace_sp(xmin,xmax,x)
    implicit none
    real(sp),intent(in) :: xmin,xmax
    real(sp),intent(out) :: x(:)
    integer :: i,n
    n = size(x)
    if (n == 1) then
       if(xmin /= xmax) then
          write(0,'("ERROR: Cannot call linspace with n=1 and xmin /= xmax")')
          stop
       else
          x = xmin
       end if
    else
       do i=1,n
          x(i) = (xmax-xmin) * real(i-1,sp) / real(n-1,sp) + xmin
       end do
    end if
  end subroutine linspace_sp

  subroutine logspace_sp(xmin,xmax,x)
    implicit none
    real(sp),intent(in) :: xmin,xmax
    real(sp),intent(out) :: x(:)
    if (size(x) == 1 .and. xmin /= xmax) then
       write(0,'("ERROR: Cannot call logspace with n=1 and xmin /= xmax")')
       stop
    end if
    call linspace(log10(xmin),log10(xmax),x)
    x = 10._sp**x
  end subroutine logspace_sp

  real(sp) function integral_sp(x,y)
    ! Total integral of a function
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    integral_sp = integral_general_sp(x, y, trapezium_sp)
  end function integral_sp

  real(sp) function integral_subset_sp(x,y,x1,x2)
    ! Integral of a function between two limits
    implicit none
    real(sp),intent(in) :: x(:),y(:),x1,x2
    integral_subset_sp = integral_general_subset_sp(x, y, x1, x2, interp1d_sp, trapezium_sp)
  end function integral_subset_sp

  real(sp) function integral_linlog_sp(x,y)
    ! Total integral of a function
    ! (uses linlog interpolation)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    integral_linlog_sp = integral_general_sp(x, y, trapezium_linlog_sp)
  end function integral_linlog_sp

  real(sp) function integral_linlog_subset_sp(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses linlog interpolation)
    implicit none
    real(sp),intent(in) :: x(:),y(:),x1,x2
    integral_linlog_subset_sp = integral_general_subset_sp(x, y, x1, x2, interp1d_linlog_sp, trapezium_linlog_sp)
  end function integral_linlog_subset_sp

  real(sp) function integral_loglin_sp(x,y)
    ! Total integral of a function
    ! (uses loglin interpolation)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    integral_loglin_sp = integral_general_sp(x, y, trapezium_loglin_sp)
  end function integral_loglin_sp

  real(sp) function integral_loglin_subset_sp(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses loglin interpolation)
    implicit none
    real(sp),intent(in) :: x(:),y(:),x1,x2
    integral_loglin_subset_sp = integral_general_subset_sp(x, y, x1, x2, interp1d_loglin_sp, trapezium_loglin_sp)
  end function integral_loglin_subset_sp

  real(sp) function integral_loglog_sp(x,y)
    ! Total integral of a function
    ! (uses log10 interpolation)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    integral_loglog_sp = integral_general_sp(x, y, trapezium_loglog_sp)
  end function integral_loglog_sp

  real(sp) function integral_loglog_subset_sp(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses log10 interpolation)
    implicit none
    real(sp),intent(in) :: x(:),y(:),x1,x2
    integral_loglog_subset_sp = integral_general_subset_sp(x, y, x1, x2, interp1d_loglog_sp, trapezium_loglog_sp)
  end function integral_loglog_subset_sp

  function cumulative_integral_sp(x,y)
    ! Total cumulative_integral of a function
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp), dimension(size(y)) :: cumulative_integral_sp
    cumulative_integral_sp = cumulative_integral_general_sp(x, y, trapezium_sp)
  end function cumulative_integral_sp

  function cumulative_integral_linlog_sp(x,y)
    ! Total cumulative_integral of a function
    ! (uses linlog interpolation)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp), dimension(size(y)) :: cumulative_integral_linlog_sp
    cumulative_integral_linlog_sp = cumulative_integral_general_sp(x, y, trapezium_linlog_sp)
  end function cumulative_integral_linlog_sp

  function cumulative_integral_loglin_sp(x,y)
    ! Total cumulative_integral of a function
    ! (uses loglin interpolation)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp), dimension(size(y)) :: cumulative_integral_loglin_sp
    cumulative_integral_loglin_sp = cumulative_integral_general_sp(x, y, trapezium_loglin_sp)
  end function cumulative_integral_loglin_sp

  function cumulative_integral_loglog_sp(x,y)
    ! Total cumulative_integral of a function
    ! (uses log10 interpolation)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp), dimension(size(y)) :: cumulative_integral_loglog_sp
    cumulative_integral_loglog_sp = cumulative_integral_general_sp(x, y, trapezium_loglog_sp)
  end function cumulative_integral_loglog_sp

  real(sp) function integral_general_sp(x,y,f_chunk) result(sum)
    ! Total integral of a function
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    integer :: j
    interface
       real(sp) function f_chunk(x1,y1,x2,y2)
         import :: sp
         implicit none
         real(sp),intent(in) :: x1,y1,x2,y2
       end function f_chunk
    end interface
    sum = 0._sp
    do j=1,size(x)-1
       sum=sum+f_chunk(x(j),y(j),x(j+1),y(j+1))
    end do
  end function integral_general_sp

  function cumulative_integral_general_sp(x,y,f_chunk) result(c)
    ! Total integral of a function
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    integer :: j
    interface
       real(sp) function f_chunk(x1,y1,x2,y2)
         import :: sp
         implicit none
         real(sp),intent(in) :: x1,y1,x2,y2
       end function f_chunk
    end interface
    real(sp), dimension(size(y)) :: c
    c(1) = 0._sp
    do j=1,size(x)-1
       c(j+1)=c(j)+f_chunk(x(j),y(j),x(j+1),y(j+1))
    end do
  end function cumulative_integral_general_sp

  real(sp) function integral_general_subset_sp(x,y,x1,x2,f_interp,f_chunk) result(sum)
    ! Integral of a function between two limits

    implicit none

    real(sp),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(sp) :: f1,f2
    integer :: n

    interface
       real(sp) function f_interp(x, y, xval,bounds_error,fill_value)
         import :: sp
         implicit none
         real(sp),dimension(:),intent(in) :: x,y
         real(sp),intent(in) :: xval
         logical,intent(in),optional :: bounds_error
         real(sp),intent(in),optional :: fill_value
       end function f_interp
       real(sp) function f_chunk(x1,y1,x2,y2)
         import :: sp
         implicit none
         real(sp),intent(in) :: x1,y1,x2,y2
       end function f_chunk
    end interface

    n = size(x)

    if(x1.gt.x(n).or.x2.lt.x(1)) then
       sum = 0._sp
       return
    end if

    if(x1.gt.x(1)) then
       i1 = locate(x,x1)
       f1 = f_interp(x,y,x1)
    else
       i1 = 0
       f1 = 0._sp
    end if

    if(x2.lt.x(n)) then
       i2 = locate(x,x2)
       f2 = f_interp(x,y,x2)
    else
       i2 = n
       f2 = 0._sp
    end if

    if(i2.gt.i1) then

       ! Add main part
       if(i2 > i1+1) then
          sum = integral_general_sp(x(i1+1:i2), y(i1+1:i2), f_chunk)
       else
          sum = 0._sp
       end if

       ! Add extremities
       sum = sum + f_chunk(x1,f1,x(i1+1),y(i1+1))
       sum = sum + f_chunk(x(i2),y(i2),x2,f2)

    else

       sum = f_chunk(x1,f1,x2,f2)

    end if

  end function integral_general_subset_sp

  real(sp) function trapezium_sp(x1,y1,x2,y2)
    implicit none
    real(sp),intent(in) :: x1,y1,x2,y2
    trapezium_sp = 0.5_sp*(y1+y2)*(x2-x1)
  end function trapezium_sp

  real(sp) function trapezium_loglin_sp(x1,y1,x2,y2)
    implicit none
    real(sp),intent(in) :: x1,y1,x2,y2
    real(sp) :: a,b
    if(x1==x2) then
       trapezium_loglin_sp = 0._sp
    else
       a = (y1-y2) / log10(x1/x2)
       b = y1 - a * log10(x1)
       trapezium_loglin_sp = a*(x2*log10(x2) - x1*log10(x1)) &
            &                     + (b-a/log(10._sp)) * (x2 - x1)
    end if
  end function trapezium_loglin_sp

  real(sp) function trapezium_linlog_sp(x1,y1,x2,y2)
    implicit none
    real(sp),intent(in) :: x1,y1,x2,y2
    real(sp) :: dx
    if(x1==x2) then
       trapezium_linlog_sp = 0._sp
    else if(y1==y2) then
       trapezium_linlog_sp = y1*(x2-x1)
    else
       dx = x2 - x1
       trapezium_linlog_sp = (y2-y1)*(x2-x1)/log(10._sp)/log10(y2/y1)
    end if
  end function trapezium_linlog_sp

  real(sp) function trapezium_loglog_sp(x1,y1,x2,y2)
    implicit none
    real(sp),intent(in) :: x1,y1,x2,y2
    real(sp) :: b
    if(x1==x2) then
       trapezium_loglog_sp = 0._sp
    else if(y1==0..or.y2==0.) then
       trapezium_loglog_sp = 0._sp
    else
       b = log10(y1/y2) / log10(x1/x2)
       if(abs(b+1._dp) < 1e-10) then
          trapezium_loglog_sp = x1 * y1 * log(x2/x1)
       else
          trapezium_loglog_sp = y1 * (x2*(x2/x1)**b-x1) / (b+1)
       end if
    end if
  end function trapezium_loglog_sp

  real(sp) function interp1d_single_sp(x1,y1,x2,y2,xval) result(yval)
    real(sp),intent(in) :: x1,y1,x2,y2,xval
    real(sp) :: frac
    frac = ( xval - x1 ) / ( x2 - x1 )
    yval = y1 + frac * ( y2 - y1 )
  end function interp1d_single_sp

  real(sp) function interp1d_single_linlog_sp(x1,y1,x2,y2,xval) result(yval)
    real(sp),intent(in) :: x1,y1,x2,y2,xval
    real(sp) :: frac
    if(y1==0..or.y2==0.) then
       yval = 0._sp
    else
       frac = ( xval - x1 ) / ( x2 - x1 )
       yval = 10._sp**(log10(y1) + frac * ( log10(y2) - log10(y1) ))
    end if
  end function interp1d_single_linlog_sp

  real(sp) function interp1d_single_loglin_sp(x1,y1,x2,y2,xval) result(yval)
    real(sp),intent(in) :: x1,y1,x2,y2,xval
    real(sp) :: frac
    frac = ( log10(xval) - log10(x1) ) / ( log10(x2) - log10(x1) )
    yval = y1 + frac * ( y2 - y1 )
  end function interp1d_single_loglin_sp

  real(sp) function interp1d_single_loglog_sp(x1,y1,x2,y2,xval) result(yval)
    real(sp),intent(in) :: x1,y1,x2,y2,xval
    real(sp) :: frac
    if(y1==0..or.y2==0.) then
       yval = 0._sp
    else
       frac = ( log10(xval) - log10(x1) ) / ( log10(x2) - log10(x1) )
       yval = 10._sp**(log10(y1) + frac * ( log10(y2) - log10(y1) ))
    end if
  end function interp1d_single_loglog_sp

  real(sp) function interp1d_sp(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(sp),dimension(:),intent(in) :: x,y
    real(sp),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(sp),intent(in),optional :: fill_value
    yval = interp1d_general_sp(x,y,xval,interp1d_single_sp,bounds_error,fill_value)
  end function interp1d_sp

  function interp1d_array_sp(x,y,xval,bounds_error,fill_value)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(sp),intent(in),optional :: fill_value
    real(sp) :: interp1d_array_sp(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_sp(i) = interp1d_sp(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_sp

  real(sp) function interp1d_linlog_sp(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(sp),dimension(:),intent(in) :: x,y
    real(sp),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(sp),intent(in),optional :: fill_value
    yval = interp1d_general_sp(x,y,xval,interp1d_single_linlog_sp,bounds_error,fill_value)
  end function interp1d_linlog_sp

  function interp1d_array_linlog_sp(x,y,xval,bounds_error,fill_value)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(sp),intent(in),optional :: fill_value
    real(sp) :: interp1d_array_linlog_sp(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_linlog_sp(i) = interp1d_linlog_sp(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_linlog_sp

  real(sp) function interp1d_loglin_sp(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(sp),dimension(:),intent(in) :: x,y
    real(sp),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(sp),intent(in),optional :: fill_value
    yval = interp1d_general_sp(x,y,xval,interp1d_single_loglin_sp,bounds_error,fill_value)
  end function interp1d_loglin_sp

  function interp1d_array_loglin_sp(x,y,xval,bounds_error,fill_value)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(sp),intent(in),optional :: fill_value
    real(sp) :: interp1d_array_loglin_sp(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_loglin_sp(i) = interp1d_loglin_sp(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_loglin_sp

  real(sp) function interp1d_loglog_sp(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(sp),dimension(:),intent(in) :: x,y
    real(sp),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(sp),intent(in),optional :: fill_value
    yval = interp1d_general_sp(x,y,xval,interp1d_single_loglog_sp,bounds_error,fill_value)
  end function interp1d_loglog_sp

  function interp1d_array_loglog_sp(x,y,xval,bounds_error,fill_value)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(sp),intent(in),optional :: fill_value
    real(sp) :: interp1d_array_loglog_sp(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_loglog_sp(i) = interp1d_loglog_sp(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_loglog_sp

  real(sp) function interp1d_general_sp(x,y,xval,f_single,bounds_error,fill_value) result(yval)

    implicit none

    integer :: n
    ! the size of the array

    real(sp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(sp),intent(in) :: xval
    ! the value at which to interpolate y

    interface
       real(sp) function f_single(x1,y1,x2,y2,xval)
         import :: sp
         implicit none
         real(sp),intent(in) :: x1,y1,x2,y2,xval
       end function f_single
    end interface

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(sp),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    integer :: ipos
    ! position of x value in x array

    logical :: bounds_error_tmp
    real(sp) :: fill_value_tmp

    if(present(bounds_error)) then
       bounds_error_tmp = bounds_error
    else
       bounds_error_tmp = .true.
    end if

    if(.not.bounds_error_tmp) then
       if(present(fill_value)) then
          fill_value_tmp = fill_value
       else
          fill_value_tmp = 0._sp
       end if
    end if

    n = size(x)

    ipos = locate(x,xval)

    ! --- First some error checking --- !

    if(ipos == -1) then
       if(bounds_error_tmp) then
          write(0,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') xval,x(1),x(n)
          stop
       else
          yval = fill_value_tmp
          return
       end if
    end if

    if( ipos < n .and. ipos > 0) then
       yval = f_single(x(ipos), y(ipos), x(ipos+1), y(ipos+1), xval)
    else if(ipos == n) then
       yval = y(n)
    else if(ipos == 0) then
       yval = y(1)
    else
       write(0,'("ERROR: Unexpected value of ipos : ",I0)') ipos
       stop
    end if

  end function interp1d_general_sp

  function interp2d_sp(x,y,array,x0,y0,bounds_error,fill_value) result(value)
    ! Bilinar interpolation of array = f(x,y) at (x0,y0)

    implicit none

    real(sp),intent(in) :: x(:),y(:),array(:,:),x0,y0

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(sp),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    real(sp) :: value,norm
    integer :: i1,i2,j1,j2

    logical :: bounds_error_tmp
    real(sp) :: fill_value_tmp

    if(present(bounds_error)) then
       bounds_error_tmp = bounds_error
    else
       bounds_error_tmp = .true.
    end if

    if(.not.bounds_error_tmp) then
       if(present(fill_value)) then
          fill_value_tmp = fill_value
       else
          fill_value_tmp = 0._sp
       end if
    end if

    if(size(x).ne.size(array,1)) stop "x does not match array"
    if(size(y).ne.size(array,2)) stop "y does not match array"

    i1 = locate(x,x0) ; i2 = i1 + 1
    j1 = locate(y,y0) ; j2 = j1 + 1

    if(i1==-1) then
       if(bounds_error_tmp) then
          write(0,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') x0,x(1),x(size(x))
          stop
       else
          value = fill_value_tmp
          return
       end if
    end if

    if(j1==-1) then
       if(bounds_error_tmp) then
          write(0,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') y0,y(1),y(size(y))
          stop
       else
          value = fill_value_tmp
          return
       end if
    end if

    norm = 1._sp / (x(i2) - x(i1)) / (y(j2)-y(j1))

    value =   array(i1,j1) * (x(i2)-x0) * (y(j2)-y0) * norm &
         &       + array(i2,j1) * (x0-x(i1)) * (y(j2)-y0) * norm &
         &       + array(i1,j2) * (x(i2)-x0) * (y0-y(j1)) * norm &
         &       + array(i2,j2) * (x0-x(i1)) * (y0-y(j1)) * norm

  end function interp2d_sp

  real(sp) function average_lin_sp(a,b)
    implicit none
    real(sp),intent(in) :: a,b
    average_lin_sp = 0.5_sp * (a + b)
  end function average_lin_sp

  real(sp) function average_log_sp(a,b)
    implicit none
    real(sp),intent(in) :: a,b
    average_log_sp = sqrt(a*b)
  end function average_log_sp

  subroutine rebin_sp(x, y, xx, yy)
    real(sp),intent(in) :: x(:), y(:), xx(:)
    real(sp),intent(out) :: yy(:)
    call rebin_general_sp(x, y, xx, yy, average_lin_sp, integral_subset_sp)
  end subroutine rebin_sp

  subroutine rebin_linlog_sp(x, y, xx, yy)
    real(sp),intent(in) :: x(:), y(:), xx(:)
    real(sp),intent(out) :: yy(:)
    call rebin_general_sp(x, y, xx, yy, average_lin_sp, integral_linlog_subset_sp)
  end subroutine rebin_linlog_sp

  subroutine rebin_loglin_sp(x, y, xx, yy)
    real(sp),intent(in) :: x(:), y(:), xx(:)
    real(sp),intent(out) :: yy(:)
    call rebin_general_sp(x, y, xx, yy, average_log_sp, integral_loglin_subset_sp)
  end subroutine rebin_loglin_sp

  subroutine rebin_loglog_sp(x, y, xx, yy)
    real(sp),intent(in) :: x(:), y(:), xx(:)
    real(sp),intent(out) :: yy(:)
    call rebin_general_sp(x, y, xx, yy, average_log_sp, integral_loglog_subset_sp)
  end subroutine rebin_loglog_sp

  subroutine rebin_general_sp(x, y, xx, yy, f_average, f_integral)
    implicit none
    real(sp),intent(in) :: x(:), y(:), xx(:)
    real(sp),intent(out) :: yy(:)
    integer :: i
    real(sp) :: xmin, xmax
    interface
       real(sp) function f_average(a,b)
         import :: sp
         implicit none
         real(sp),intent(in) :: a,b
       end function f_average
       real(sp) function f_integral(x,y,x1,x2)
         import :: sp
         implicit none
         real(sp),intent(in) :: x(:),y(:),x1,x2
       end function f_integral
    end interface
    do i=1,size(xx)
       if(i==1) then
          xmin = xx(1)
       else
          xmin = f_average(xx(i-1), xx(i))
       end if
       if(i==size(xx)) then
          xmax = xx(size(xx))
       else
          xmax = f_average(xx(i), xx(i+1))
       end if
       yy(i) = f_integral(x, y, xmin, xmax) / (xmax - xmin)
    end do
  end subroutine rebin_general_sp

  integer function locate_sp(xx,x)
    ! Locate a value in a sorted array

    implicit none
    real(sp), dimension(:), intent(in) :: xx
    real(sp), intent(in) :: x
    integer :: n,jl,jm,ju
    logical :: ascnd
    n=size(xx)
    ascnd = (xx(n) >= xx(1))
    jl=0
    ju=n+1
    do
       if (ju-jl <= 1) exit
       jm=(ju+jl)/2
       if (ascnd .eqv. (x >= xx(jm))) then
          jl=jm
       else
          ju=jm
       end if
    end do

    if (x == xx(1)) then
       locate_sp = 1
    else if (x == xx(n)) then
       locate_sp = n-1
    else if(ascnd.and. (x > xx(n) .or. x < xx(1))) then
       locate_sp = -1
    else if(.not.ascnd.and. (x < xx(n) .or. x > xx(1))) then
       locate_sp = -1
    else
       locate_sp = jl
    end if

  end function locate_sp


  integer function ipos_sp(xmin,xmax,x,nbin)
    ! Find bin a value falls in for a regular histogram

    implicit none

    real(sp),intent(in) :: xmin,xmax
    ! range of values

    real(sp),intent(in) :: x
    ! the value to bin

    integer,intent(in) :: nbin
    ! number of bins

    real(sp) :: frac

    if(xmax > xmin) then

       if(x < xmin) then
          ipos_sp = 0
       else if(x > xmax) then
          ipos_sp = nbin+1
       else if(x < xmax) then
          frac=(x-xmin)/(xmax-xmin)
          ipos_sp=int(frac*real(nbin, sp))+1
       else  ! x == xmax
          ipos_sp = nbin
       end if

    else

       if(x < xmax) then
          ipos_sp = 0
       else if(x > xmin) then
          ipos_sp = nbin+1
       else if(x < xmin) then
          frac=(x-xmin)/(xmax-xmin)
          ipos_sp=int(frac*real(nbin, sp))+1
       else  ! x == xmin
          ipos_sp = nbin
       end if

    end if

  end function ipos_sp


  real(sp) function xval_sp(xmin,xmax,i,nbin)
    ! Find central value of a bin for a regular histogram

    implicit none

    real(sp),intent(in) :: xmin,xmax
    ! range of values

    integer,intent(in) :: i
    ! the bin number

    integer,intent(in) :: nbin
    ! number of bins

    real(sp) :: frac

    frac=(real(i-1)+0.5)/real(nbin)

    xval_sp=frac*(xmax-xmin)+xmin

  end function xval_sp


  subroutine histogram1d_sp(array,xmin,xmax,nbin,hist_x,hist_y,mask,weights)
    ! Bin 1D array of values into 1D regular histogram

    implicit none

    real(sp),dimension(:),intent(in) :: array
    real(sp),dimension(:),intent(in),optional :: weights
    ! the array of values to bin

    real(sp),intent(in) :: xmin,xmax
    ! the range of the histogram

    integer,intent(in) :: nbin
    ! number of bins

    real(sp),dimension(nbin),intent(out) :: hist_x,hist_y
    ! the histogram

    integer :: i,ibin
    ! binning variables

    logical,optional,intent(in) :: mask(:)
    logical,allocatable:: keep(:)

    allocate(keep(size(array)))

    if(present(mask)) then
       keep = mask
    else
       keep = .true.
    end if

    hist_x=0._sp ; hist_y=0._sp

    do i=1,size(array)
       if(keep(i)) then
          ibin=ipos(xmin,xmax,array(i),nbin)
          if(ibin.ge.1.and.ibin.le.nbin) then
             if(present(weights)) then
                hist_y(ibin)=hist_y(ibin)+weights(i)
             else
                hist_y(ibin)=hist_y(ibin)+1._sp
             end if
          end if
       end if
    end do

    do ibin=1,nbin
       hist_x(ibin)=xval(xmin,xmax,ibin,nbin)
    end do

    deallocate(keep)

  end subroutine histogram1d_sp


  subroutine histogram2d_sp(n,x,y,xmin,xmax,ymin,ymax,nx,ny,array,mask_in)
    ! Bin two 1D array of values into 2D regular grid

    implicit none

    integer,intent(in) :: n
    ! number of values to bin

    real(sp),dimension(n),intent(in) :: x,y
    logical,dimension(n),intent(in),optional :: mask_in
    ! the array of values to bin

    real(sp),intent(in) :: xmin,xmax,ymin,ymax
    ! the range of the histogram

    integer,intent(in) :: nx,ny
    ! number of bins

    real(sp),dimension(nx,ny),intent(out) :: array
    ! the histogram

    integer :: i,ix,iy
    ! binning variables

    logical,dimension(n) :: mask

    if(present(mask_in)) then
       mask = mask_in
    else
       mask = .true.
    end if

    array = 0._sp

    do i=1,n

       if(mask(i)) then

          ix=ipos(xmin,xmax,x(i),nx)
          iy=ipos(ymin,ymax,y(i),ny)

          if(ix.ge.1.and.ix.le.nx) then
             if(iy.ge.1.and.iy.le.ny) then
                array(ix,iy) = array(ix,iy) + 1.
             end if
          end if

       end if

    end do

  end subroutine histogram2d_sp


  subroutine swap_sp(array, i, j)
    implicit none
    real(sp),intent(inout) :: array(:)
    integer,intent(in) :: i, j
    real(sp) :: temp
    temp = array(j)
    array(j) = array(i)
    array(i) = temp
  end subroutine swap_sp

  subroutine partition_sp(array, left, right, pivot_index, store_index)
    implicit none
    real(sp),intent(inout) :: array(:)
    integer,intent(in) :: left, right, pivot_index
    integer,intent(out) :: store_index
    real(sp) :: pivot_value
    integer :: i
    pivot_value = array(pivot_index)
    call swap_sp(array, pivot_index, right)
    store_index = left
    do i=left, right-1
       if(array(i) <= pivot_value) then
          call swap_sp(array, i, store_index)
          store_index = store_index + 1
       end if
    end do
    call swap_sp(array, store_index, right)
  end subroutine partition_sp

  recursive subroutine quicksort_sp(array, left, right)
    implicit none
    real(sp),intent(inout) :: array(:)
    integer,intent(in) :: left, right
    integer :: pivot_index, new_pivot_index
    if(right > left) then
       pivot_index = left+(right-left)/2
       call partition_sp(array, left, right, pivot_index, new_pivot_index)
       call quicksort_sp(array, left, new_pivot_index - 1)
       call quicksort_sp(array, new_pivot_index + 1, right)
    end if
  end subroutine quicksort_sp

  recursive subroutine quicksort_all_sp(array)
    implicit none
    real(sp),intent(inout) :: array(:)
    call quicksort_sp(array, 1, size(array))
  end subroutine quicksort_all_sp

  subroutine swap_index_sp(array, index, i, j)
    implicit none
    real(sp),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    integer,intent(in) :: i, j
    real(sp) :: temp
    integer :: temp_i
    temp = array(j)
    temp_i = index(j)
    array(j) = array(i)
    index(j) = index(i)
    array(i) = temp
    index(i) = temp_i
  end subroutine swap_index_sp

  subroutine partition_index_sp(array, index, left, right, pivot_index, store_index)
    implicit none
    real(sp),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    integer,intent(in) :: left, right, pivot_index
    integer,intent(out) :: store_index
    real(sp) :: pivot_value
    integer :: i
    pivot_value = array(pivot_index)
    call swap_index_sp(array, index, pivot_index, right)
    store_index = left
    do i=left, right-1
       if(array(i) <= pivot_value) then
          call swap_index_sp(array, index, i, store_index)
          store_index = store_index + 1
       end if
    end do
    call swap_index_sp(array, index, store_index, right)
  end subroutine partition_index_sp

  recursive subroutine quicksort_index_sp(array, index, left, right)
    implicit none
    real(sp),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    integer,intent(in) :: left, right
    integer :: pivot_index, new_pivot_index
    if(right > left) then
       pivot_index = left+(right-left)/2
       call partition_index_sp(array, index, left, right, pivot_index, new_pivot_index)
       call quicksort_index_sp(array, index, left, new_pivot_index - 1)
       call quicksort_index_sp(array, index, new_pivot_index + 1, right)
    end if
  end subroutine quicksort_index_sp

  recursive subroutine quicksort_index_all_sp(array, index)
    implicit none
    real(sp),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    call quicksort_index_sp(array, index, 1, size(array))
  end subroutine quicksort_index_all_sp

  subroutine check_sort_sp(array)
    implicit none
    real(sp),intent(inout) :: array(:)
    integer :: j
    do j=1,size(array)-1
       if(array(j+1) < array(j)) stop "Incorrectly sorted array"
    end do
  end subroutine check_sort_sp

  subroutine test_quicksort_sp()

    implicit none

    real(sp),allocatable :: array1(:), array2(:)
    integer,allocatable :: index(:)
    integer :: j,i

    write(*,'(" Testing quicksort")')

    do j=1,20

       write(*,'("  - n=",I0)') 2**j

       allocate(array1(2**j))

       call random_number(array1)

       call quicksort(array1)

       call check_sort_sp(array1)

       deallocate(array1)

    end do

    write(*,'(" Testing quicksort_index")')

    do j=1,20

       write(*,'("  - n=",I0)') 2**j

       allocate(array1(2**j), array2(2**j), index(2**j))

       call random_number(array1)
       array2 = array1
       forall(i=1:2**j) index(i) = i

       call quicksort_index(array1, index)

       call check_sort_sp(array1)

       if(any(array2(index).ne.array1)) stop "Failure in quicksort_index"

       deallocate(array1, array2, index)

    end do

  end subroutine test_quicksort_sp


  subroutine invert_matrix(n,a,c)

    implicit none

    integer,intent(in) :: n
    ! size of the matrix

    real(dp),dimension(n,n),intent(in)  :: a
    real(dp),dimension(n,n),intent(out) :: c
    ! the input and output matrices

    real(dp),dimension(n,n) :: b

    integer :: i,j
    real(dp) :: scale

    ! a is the input matrix

    ! b is initially set to a

    b = a

    ! c is initially an identity matrix

    c = 0._dp
    forall(i=1:n) c(i,i) = 1._dp

    ! Go through each row, and sutract from all subsequent rows

    do i=1,n
       do j=i+1,n
          scale = -b(i,j)/b(i,i)
          b(:,j) = b(:,j)+scale*b(:,i)
          c(:,j) = c(:,j)+scale*c(:,i)
       end do
    end do

    do j=1,n-1
       do i=j+1,n
          scale = -b(i,j)/b(i,i)
          b(:,j) = b(:,j)+scale*b(:,i)
          c(:,j) = c(:,j)+scale*c(:,i)
       end do
    end do

    forall(i=1:n,j=1:n) c(i,j) = c(i,j)/b(j,j)
    forall(i=1:n,j=1:n) b(i,j) = b(i,j)/b(j,j)

  end subroutine invert_matrix


  subroutine print_matrix(a)

    implicit none

    integer :: nx,ny
    real(dp),dimension(:,:),intent(in) :: a

    integer :: i

    character(len=20) :: fmt

    nx = size(a,1)
    ny = size(a,2)

    write(fmt,'("(",I3.3,"(F9.5,1X))")') nx

    do i=1,ny
       write(*,fmt) a(:,i)
    end do

  end subroutine print_matrix


  subroutine bin_array(n,x,y,n_bin,xmin,xmax,x_bin,y_bin)

    implicit none

    integer,intent(in) :: n,n_bin
    real(dp),dimension(n),intent(in) :: x,y
    real(dp),dimension(n_bin),intent(out) :: x_bin,y_bin

    real(dp),dimension(n_bin) :: s,c

    real(dp) :: xmin,xmax

    integer :: i,ix

    s = 0.
    c = 0.

    do i=1,n

       ix = ipos(xmin,xmax,x(i),n_bin)

       if(ix.ge.1.and.ix.le.n_bin) then
          c(ix)  = c(ix) + 1.
          s(ix)  = s(ix) + y(i)
       end if

    end do

    do i=1,n_bin
       x_bin(i) = xval(xmin,xmax,i,n_bin)
    end do

    y_bin = s / c

  end subroutine bin_array


  subroutine smooth_2d(array,sigma)

    implicit none

    real,intent(inout) :: array(:,:)

    integer :: nx,ny
    real,allocatable :: array_orig(:,:),array_count(:,:)

    real,intent(in) :: sigma

    integer :: i,j,ii,jj,imin,imax,jmin,jmax,w

    real :: dx,dy,d

    nx = size(array,1)
    ny = size(array,2)

    allocate(array_orig(nx,ny),array_count(nx,ny))

    array_orig  = array
    array       = 0.
    array_count = 0.

    w = nint(sigma * 5._dp)

    do i=1,nx
       do j=1,ny

          imin = max( 1,i-w)
          imax = min(nx,i+w)
          jmin = max( 1,j-w)
          jmax = min(ny,j+w)

          do ii=imin,imax
             do jj=jmin,jmax

                dx = real(ii-i) / sigma
                dy = real(jj-j) / sigma

                d = dx*dx+dy*dy

                array(i,j) = array(i,j) + array_orig(ii,jj) * exp(-d/2._dp)
                array_count(i,j) = array_count(i,j) + exp(-d/2._dp)

             end do
          end do

       end do
    end do

    array = array / array_count

  end subroutine smooth_2d

end module lib_array

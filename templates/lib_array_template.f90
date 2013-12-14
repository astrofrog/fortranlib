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

  !!@FOR real(sp):sp real(dp):dp integer:int integer(idp):int8

  logical elemental function is_nan_<T>(value) result(nan)
    ! x != x does not work with pgfortran
    @T,intent(in) :: value
    nan = .not. (value > -1 .or. value < 1)
  end function is_nan_<T>

  !!@END FOR

  !!@FOR real(sp):sp real(dp):dp

  subroutine linspace_<T>(xmin,xmax,x)
    implicit none
    @T,intent(in) :: xmin,xmax
    @T,intent(out) :: x(:)
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
          x(i) = (xmax-xmin) * real(i-1,<T>) / real(n-1,<T>) + xmin
       end do
    end if
  end subroutine linspace_<T>

  subroutine logspace_<T>(xmin,xmax,x)
    implicit none
    @T,intent(in) :: xmin,xmax
    @T,intent(out) :: x(:)
    if (size(x) == 1 .and. xmin /= xmax) then
       write(0,'("ERROR: Cannot call logspace with n=1 and xmin /= xmax")')
       stop
    end if
    call linspace(log10(xmin),log10(xmax),x)
    x = 10._<T>**x
  end subroutine logspace_<T>

  real(<T>) function integral_<T>(x,y)
    ! Total integral of a function
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    integral_<T> = integral_general_<T>(x, y, trapezium_<T>)
  end function integral_<T>

  real(<T>) function integral_subset_<T>(x,y,x1,x2)
    ! Integral of a function between two limits
    implicit none
    real(<T>),intent(in) :: x(:),y(:),x1,x2
    integral_subset_<T> = integral_general_subset_<T>(x, y, x1, x2, interp1d_<T>, trapezium_<T>)
  end function integral_subset_<T>

  real(<T>) function integral_linlog_<T>(x,y)
    ! Total integral of a function
    ! (uses linlog interpolation)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    integral_linlog_<T> = integral_general_<T>(x, y, trapezium_linlog_<T>)
  end function integral_linlog_<T>

  real(<T>) function integral_linlog_subset_<T>(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses linlog interpolation)
    implicit none
    real(<T>),intent(in) :: x(:),y(:),x1,x2
    integral_linlog_subset_<T> = integral_general_subset_<T>(x, y, x1, x2, interp1d_linlog_<T>, trapezium_linlog_<T>)
  end function integral_linlog_subset_<T>

  real(<T>) function integral_loglin_<T>(x,y)
    ! Total integral of a function
    ! (uses loglin interpolation)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    integral_loglin_<T> = integral_general_<T>(x, y, trapezium_loglin_<T>)
  end function integral_loglin_<T>

  real(<T>) function integral_loglin_subset_<T>(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses loglin interpolation)
    implicit none
    real(<T>),intent(in) :: x(:),y(:),x1,x2
    integral_loglin_subset_<T> = integral_general_subset_<T>(x, y, x1, x2, interp1d_loglin_<T>, trapezium_loglin_<T>)
  end function integral_loglin_subset_<T>

  real(<T>) function integral_loglog_<T>(x,y)
    ! Total integral of a function
    ! (uses log10 interpolation)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    integral_loglog_<T> = integral_general_<T>(x, y, trapezium_loglog_<T>)
  end function integral_loglog_<T>

  real(<T>) function integral_loglog_subset_<T>(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses log10 interpolation)
    implicit none
    real(<T>),intent(in) :: x(:),y(:),x1,x2
    integral_loglog_subset_<T> = integral_general_subset_<T>(x, y, x1, x2, interp1d_loglog_<T>, trapezium_loglog_<T>)
  end function integral_loglog_subset_<T>

  function cumulative_integral_<T>(x,y)
    ! Total cumulative_integral of a function
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    real(<T>), dimension(size(y)) :: cumulative_integral_<T>
    cumulative_integral_<T> = cumulative_integral_general_<T>(x, y, trapezium_<T>)
  end function cumulative_integral_<T>

  function cumulative_integral_linlog_<T>(x,y)
    ! Total cumulative_integral of a function
    ! (uses linlog interpolation)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    real(<T>), dimension(size(y)) :: cumulative_integral_linlog_<T>
    cumulative_integral_linlog_<T> = cumulative_integral_general_<T>(x, y, trapezium_linlog_<T>)
  end function cumulative_integral_linlog_<T>

  function cumulative_integral_loglin_<T>(x,y)
    ! Total cumulative_integral of a function
    ! (uses loglin interpolation)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    real(<T>), dimension(size(y)) :: cumulative_integral_loglin_<T>
    cumulative_integral_loglin_<T> = cumulative_integral_general_<T>(x, y, trapezium_loglin_<T>)
  end function cumulative_integral_loglin_<T>

  function cumulative_integral_loglog_<T>(x,y)
    ! Total cumulative_integral of a function
    ! (uses log10 interpolation)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    real(<T>), dimension(size(y)) :: cumulative_integral_loglog_<T>
    cumulative_integral_loglog_<T> = cumulative_integral_general_<T>(x, y, trapezium_loglog_<T>)
  end function cumulative_integral_loglog_<T>

  real(<T>) function integral_general_<T>(x,y,f_chunk) result(sum)
    ! Total integral of a function
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    integer :: j
    interface
       real(<T>) function f_chunk(x1,y1,x2,y2)
         import :: <T>
         implicit none
         real(<T>),intent(in) :: x1,y1,x2,y2
       end function f_chunk
    end interface
    sum = 0._<T>
    do j=1,size(x)-1
       sum=sum+f_chunk(x(j),y(j),x(j+1),y(j+1))
    end do
  end function integral_general_<T>

  function cumulative_integral_general_<T>(x,y,f_chunk) result(c)
    ! Total integral of a function
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    integer :: j
    interface
       real(<T>) function f_chunk(x1,y1,x2,y2)
         import :: <T>
         implicit none
         real(<T>),intent(in) :: x1,y1,x2,y2
       end function f_chunk
    end interface
    real(<T>), dimension(size(y)) :: c
    c(1) = 0._<T>
    do j=1,size(x)-1
       c(j+1)=c(j)+f_chunk(x(j),y(j),x(j+1),y(j+1))
    end do
  end function cumulative_integral_general_<T>

  real(<T>) function integral_general_subset_<T>(x,y,x1,x2,f_interp,f_chunk) result(sum)
    ! Integral of a function between two limits

    implicit none

    real(<T>),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(<T>) :: f1,f2
    integer :: n

    interface
       real(<T>) function f_interp(x, y, xval,bounds_error,fill_value)
         import :: <T>
         implicit none
         real(<T>),dimension(:),intent(in) :: x,y
         real(<T>),intent(in) :: xval
         logical,intent(in),optional :: bounds_error
         real(<T>),intent(in),optional :: fill_value
       end function f_interp
       real(<T>) function f_chunk(x1,y1,x2,y2)
         import :: <T>
         implicit none
         real(<T>),intent(in) :: x1,y1,x2,y2
       end function f_chunk
    end interface

    n = size(x)

    if(x1.gt.x(n).or.x2.lt.x(1)) then
       sum = 0._<T>
       return
    end if

    if(x1.gt.x(1)) then
       i1 = locate(x,x1)
       f1 = f_interp(x,y,x1)
    else
       i1 = 0
       f1 = 0._<T>
    end if

    if(x2.lt.x(n)) then
       i2 = locate(x,x2)
       f2 = f_interp(x,y,x2)
    else
       i2 = n
       f2 = 0._<T>
    end if

    if(i2.gt.i1) then

       ! Add main part
       if(i2 > i1+1) then
          sum = integral_general_<T>(x(i1+1:i2), y(i1+1:i2), f_chunk)
       else
          sum = 0._<T>
       end if

       ! Add extremities
       sum = sum + f_chunk(x1,f1,x(i1+1),y(i1+1))
       sum = sum + f_chunk(x(i2),y(i2),x2,f2)

    else

       sum = f_chunk(x1,f1,x2,f2)

    end if

  end function integral_general_subset_<T>

  real(<T>) function trapezium_<T>(x1,y1,x2,y2)
    implicit none
    real(<T>),intent(in) :: x1,y1,x2,y2
    trapezium_<T> = 0.5_<T>*(y1+y2)*(x2-x1)
  end function trapezium_<T>

  real(<T>) function trapezium_loglin_<T>(x1,y1,x2,y2)
    implicit none
    real(<T>),intent(in) :: x1,y1,x2,y2
    real(<T>) :: a,b
    if(x1==x2) then
       trapezium_loglin_<T> = 0._<T>
    else
       a = (y1-y2) / log10(x1/x2)
       b = y1 - a * log10(x1)
       trapezium_loglin_<T> = a*(x2*log10(x2) - x1*log10(x1)) &
            &                     + (b-a/log(10._<T>)) * (x2 - x1)
    end if
  end function trapezium_loglin_<T>

  real(<T>) function trapezium_linlog_<T>(x1,y1,x2,y2)
    implicit none
    real(<T>),intent(in) :: x1,y1,x2,y2
    real(<T>) :: dx
    if(x1==x2) then
       trapezium_linlog_<T> = 0._<T>
    else if(y1==y2) then
       trapezium_linlog_<T> = y1*(x2-x1)
    else
       dx = x2 - x1
       trapezium_linlog_<T> = (y2-y1)*(x2-x1)/log(10._<T>)/log10(y2/y1)
    end if
  end function trapezium_linlog_<T>

  real(<T>) function trapezium_loglog_<T>(x1,y1,x2,y2)
    implicit none
    real(<T>),intent(in) :: x1,y1,x2,y2
    real(<T>) :: b
    if(x1==x2) then
       trapezium_loglog_<T> = 0._<T>
    else if(y1==0..or.y2==0.) then
       trapezium_loglog_<T> = 0._<T>
    else
       b = log10(y1/y2) / log10(x1/x2)
       if(abs(b+1._dp) < 1e-10) then
          trapezium_loglog_<T> = x1 * y1 * log(x2/x1)
       else
          trapezium_loglog_<T> = y1 * (x2*(x2/x1)**b-x1) / (b+1)
       end if
    end if
  end function trapezium_loglog_<T>

  real(<T>) function interp1d_single_<T>(x1,y1,x2,y2,xval) result(yval)
    real(<T>),intent(in) :: x1,y1,x2,y2,xval
    real(<T>) :: frac
    frac = ( xval - x1 ) / ( x2 - x1 )
    yval = y1 + frac * ( y2 - y1 )
  end function interp1d_single_<T>

  real(<T>) function interp1d_single_linlog_<T>(x1,y1,x2,y2,xval) result(yval)
    real(<T>),intent(in) :: x1,y1,x2,y2,xval
    real(<T>) :: frac
    if(y1==0..or.y2==0.) then
       yval = 0._<T>
    else
       frac = ( xval - x1 ) / ( x2 - x1 )
       yval = 10._<T>**(log10(y1) + frac * ( log10(y2) - log10(y1) ))
    end if
  end function interp1d_single_linlog_<T>

  real(<T>) function interp1d_single_loglin_<T>(x1,y1,x2,y2,xval) result(yval)
    real(<T>),intent(in) :: x1,y1,x2,y2,xval
    real(<T>) :: frac
    frac = ( log10(xval) - log10(x1) ) / ( log10(x2) - log10(x1) )
    yval = y1 + frac * ( y2 - y1 )
  end function interp1d_single_loglin_<T>

  real(<T>) function interp1d_single_loglog_<T>(x1,y1,x2,y2,xval) result(yval)
    real(<T>),intent(in) :: x1,y1,x2,y2,xval
    real(<T>) :: frac
    if(y1==0..or.y2==0.) then
       yval = 0._<T>
    else
       frac = ( log10(xval) - log10(x1) ) / ( log10(x2) - log10(x1) )
       yval = 10._<T>**(log10(y1) + frac * ( log10(y2) - log10(y1) ))
    end if
  end function interp1d_single_loglog_<T>

  real(<T>) function interp1d_<T>(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(<T>),dimension(:),intent(in) :: x,y
    real(<T>),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(<T>),intent(in),optional :: fill_value
    yval = interp1d_general_<T>(x,y,xval,interp1d_single_<T>,bounds_error,fill_value)
  end function interp1d_<T>

  function interp1d_array_<T>(x,y,xval,bounds_error,fill_value)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    real(<T>),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(<T>),intent(in),optional :: fill_value
    real(<T>) :: interp1d_array_<T>(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_<T>(i) = interp1d_<T>(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_<T>

  real(<T>) function interp1d_linlog_<T>(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(<T>),dimension(:),intent(in) :: x,y
    real(<T>),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(<T>),intent(in),optional :: fill_value
    yval = interp1d_general_<T>(x,y,xval,interp1d_single_linlog_<T>,bounds_error,fill_value)
  end function interp1d_linlog_<T>

  function interp1d_array_linlog_<T>(x,y,xval,bounds_error,fill_value)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    real(<T>),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(<T>),intent(in),optional :: fill_value
    real(<T>) :: interp1d_array_linlog_<T>(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_linlog_<T>(i) = interp1d_linlog_<T>(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_linlog_<T>

  real(<T>) function interp1d_loglin_<T>(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(<T>),dimension(:),intent(in) :: x,y
    real(<T>),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(<T>),intent(in),optional :: fill_value
    yval = interp1d_general_<T>(x,y,xval,interp1d_single_loglin_<T>,bounds_error,fill_value)
  end function interp1d_loglin_<T>

  function interp1d_array_loglin_<T>(x,y,xval,bounds_error,fill_value)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    real(<T>),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(<T>),intent(in),optional :: fill_value
    real(<T>) :: interp1d_array_loglin_<T>(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_loglin_<T>(i) = interp1d_loglin_<T>(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_loglin_<T>

  real(<T>) function interp1d_loglog_<T>(x,y,xval,bounds_error,fill_value) result(yval)
    implicit none
    real(<T>),dimension(:),intent(in) :: x,y
    real(<T>),intent(in) :: xval
    logical,intent(in),optional :: bounds_error
    real(<T>),intent(in),optional :: fill_value
    yval = interp1d_general_<T>(x,y,xval,interp1d_single_loglog_<T>,bounds_error,fill_value)
  end function interp1d_loglog_<T>

  function interp1d_array_loglog_<T>(x,y,xval,bounds_error,fill_value)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    real(<T>),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(<T>),intent(in),optional :: fill_value
    real(<T>) :: interp1d_array_loglog_<T>(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_loglog_<T>(i) = interp1d_loglog_<T>(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_loglog_<T>

  real(<T>) function interp1d_general_<T>(x,y,xval,f_single,bounds_error,fill_value) result(yval)

    implicit none

    integer :: n
    ! the size of the array

    real(<T>),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(<T>),intent(in) :: xval
    ! the value at which to interpolate y

    interface
       real(<T>) function f_single(x1,y1,x2,y2,xval)
         import :: <T>
         implicit none
         real(<T>),intent(in) :: x1,y1,x2,y2,xval
       end function f_single
    end interface

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(<T>),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    integer :: ipos
    ! position of x value in x array

    logical :: bounds_error_tmp
    real(<T>) :: fill_value_tmp

    if(present(bounds_error)) then
       bounds_error_tmp = bounds_error
    else
       bounds_error_tmp = .true.
    end if

    if(.not.bounds_error_tmp) then
       if(present(fill_value)) then
          fill_value_tmp = fill_value
       else
          fill_value_tmp = 0._<T>
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

  end function interp1d_general_<T>

  function interp2d_<T>(x,y,array,x0,y0,bounds_error,fill_value) result(value)
    ! Bilinar interpolation of array = f(x,y) at (x0,y0)

    implicit none

    real(<T>),intent(in) :: x(:),y(:),array(:,:),x0,y0

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(<T>),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    real(<T>) :: value,norm
    integer :: i1,i2,j1,j2

    logical :: bounds_error_tmp
    real(<T>) :: fill_value_tmp

    if(present(bounds_error)) then
       bounds_error_tmp = bounds_error
    else
       bounds_error_tmp = .true.
    end if

    if(.not.bounds_error_tmp) then
       if(present(fill_value)) then
          fill_value_tmp = fill_value
       else
          fill_value_tmp = 0._<T>
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

    norm = 1._<T> / (x(i2) - x(i1)) / (y(j2)-y(j1))

    value =   array(i1,j1) * (x(i2)-x0) * (y(j2)-y0) * norm &
         &       + array(i2,j1) * (x0-x(i1)) * (y(j2)-y0) * norm &
         &       + array(i1,j2) * (x(i2)-x0) * (y0-y(j1)) * norm &
         &       + array(i2,j2) * (x0-x(i1)) * (y0-y(j1)) * norm

  end function interp2d_<T>

  real(<T>) function average_lin_<T>(a,b)
    implicit none
    real(<T>),intent(in) :: a,b
    average_lin_<T> = 0.5_<T> * (a + b)
  end function average_lin_<T>

  real(<T>) function average_log_<T>(a,b)
    implicit none
    real(<T>),intent(in) :: a,b
    average_log_<T> = sqrt(a*b)
  end function average_log_<T>

  subroutine rebin_<T>(x, y, xx, yy)
    real(<T>),intent(in) :: x(:), y(:), xx(:)
    real(<T>),intent(out) :: yy(:)
    call rebin_general_<T>(x, y, xx, yy, average_lin_<T>, integral_subset_<T>)
  end subroutine rebin_<T>

  subroutine rebin_linlog_<T>(x, y, xx, yy)
    real(<T>),intent(in) :: x(:), y(:), xx(:)
    real(<T>),intent(out) :: yy(:)
    call rebin_general_<T>(x, y, xx, yy, average_lin_<T>, integral_linlog_subset_<T>)
  end subroutine rebin_linlog_<T>

  subroutine rebin_loglin_<T>(x, y, xx, yy)
    real(<T>),intent(in) :: x(:), y(:), xx(:)
    real(<T>),intent(out) :: yy(:)
    call rebin_general_<T>(x, y, xx, yy, average_log_<T>, integral_loglin_subset_<T>)
  end subroutine rebin_loglin_<T>

  subroutine rebin_loglog_<T>(x, y, xx, yy)
    real(<T>),intent(in) :: x(:), y(:), xx(:)
    real(<T>),intent(out) :: yy(:)
    call rebin_general_<T>(x, y, xx, yy, average_log_<T>, integral_loglog_subset_<T>)
  end subroutine rebin_loglog_<T>

  subroutine rebin_general_<T>(x, y, xx, yy, f_average, f_integral)
    implicit none
    real(<T>),intent(in) :: x(:), y(:), xx(:)
    real(<T>),intent(out) :: yy(:)
    integer :: i
    real(<T>) :: xmin, xmax
    interface
       real(<T>) function f_average(a,b)
         import :: <T>
         implicit none
         real(<T>),intent(in) :: a,b
       end function f_average
       real(<T>) function f_integral(x,y,x1,x2)
         import :: <T>
         implicit none
         real(<T>),intent(in) :: x(:),y(:),x1,x2
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
  end subroutine rebin_general_<T>

  integer function locate_<T>(xx,x)
    ! Locate a value in a sorted array

    implicit none
    real(<T>), dimension(:), intent(in) :: xx
    real(<T>), intent(in) :: x
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
       locate_<T> = 1
    else if (x == xx(n)) then
       locate_<T> = n-1
    else if(ascnd.and. (x > xx(n) .or. x < xx(1))) then
       locate_<T> = -1
    else if(.not.ascnd.and. (x < xx(n) .or. x > xx(1))) then
       locate_<T> = -1
    else
       locate_<T> = jl
    end if

  end function locate_<T>


  integer function ipos_<T>(xmin,xmax,x,nbin)
    ! Find bin a value falls in for a regular histogram

    implicit none

    real(<T>),intent(in) :: xmin,xmax
    ! range of values

    real(<T>),intent(in) :: x
    ! the value to bin

    integer,intent(in) :: nbin
    ! number of bins

    real(<T>) :: frac

    if(xmax > xmin) then

       if(x < xmin) then
          ipos_<T> = 0
       else if(x > xmax) then
          ipos_<T> = nbin+1
       else if(x < xmax) then
          frac=(x-xmin)/(xmax-xmin)
          ipos_<T>=int(frac*real(nbin, <T>))+1
       else  ! x == xmax
          ipos_<T> = nbin
       end if

    else

       if(x < xmax) then
          ipos_<T> = 0
       else if(x > xmin) then
          ipos_<T> = nbin+1
       else if(x < xmin) then
          frac=(x-xmin)/(xmax-xmin)
          ipos_<T>=int(frac*real(nbin, <T>))+1
       else  ! x == xmin
          ipos_<T> = nbin
       end if

    end if

  end function ipos_<T>


  real(<T>) function xval_<T>(xmin,xmax,i,nbin)
    ! Find central value of a bin for a regular histogram

    implicit none

    real(<T>),intent(in) :: xmin,xmax
    ! range of values

    integer,intent(in) :: i
    ! the bin number

    integer,intent(in) :: nbin
    ! number of bins

    real(<T>) :: frac

    frac=(real(i-1)+0.5)/real(nbin)

    xval_<T>=frac*(xmax-xmin)+xmin

  end function xval_<T>


  subroutine histogram1d_<T>(array,xmin,xmax,nbin,hist_x,hist_y,mask,weights)
    ! Bin 1D array of values into 1D regular histogram

    implicit none

    real(<T>),dimension(:),intent(in) :: array
    real(<T>),dimension(:),intent(in),optional :: weights
    ! the array of values to bin

    real(<T>),intent(in) :: xmin,xmax
    ! the range of the histogram

    integer,intent(in) :: nbin
    ! number of bins

    real(<T>),dimension(nbin),intent(out) :: hist_x,hist_y
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

    hist_x=0._<T> ; hist_y=0._<T>

    do i=1,size(array)
       if(keep(i)) then
          ibin=ipos(xmin,xmax,array(i),nbin)
          if(ibin.ge.1.and.ibin.le.nbin) then
             if(present(weights)) then
                hist_y(ibin)=hist_y(ibin)+weights(i)
             else
                hist_y(ibin)=hist_y(ibin)+1._<T>
             end if
          end if
       end if
    end do

    do ibin=1,nbin
       hist_x(ibin)=xval(xmin,xmax,ibin,nbin)
    end do

    deallocate(keep)

  end subroutine histogram1d_<T>


  subroutine histogram2d_<T>(n,x,y,xmin,xmax,ymin,ymax,nx,ny,array,mask_in)
    ! Bin two 1D array of values into 2D regular grid

    implicit none

    integer,intent(in) :: n
    ! number of values to bin

    real(<T>),dimension(n),intent(in) :: x,y
    logical,dimension(n),intent(in),optional :: mask_in
    ! the array of values to bin

    real(<T>),intent(in) :: xmin,xmax,ymin,ymax
    ! the range of the histogram

    integer,intent(in) :: nx,ny
    ! number of bins

    real(<T>),dimension(nx,ny),intent(out) :: array
    ! the histogram

    integer :: i,ix,iy
    ! binning variables

    logical,dimension(n) :: mask

    if(present(mask_in)) then
       mask = mask_in
    else
       mask = .true.
    end if

    array = 0._<T>

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

  end subroutine histogram2d_<T>


  subroutine swap_<T>(array, i, j)
    implicit none
    real(<T>),intent(inout) :: array(:)
    integer,intent(in) :: i, j
    real(<T>) :: temp
    temp = array(j)
    array(j) = array(i)
    array(i) = temp
  end subroutine swap_<T>

  subroutine partition_<T>(array, left, right, pivot_index, store_index)
    implicit none
    real(<T>),intent(inout) :: array(:)
    integer,intent(in) :: left, right, pivot_index
    integer,intent(out) :: store_index
    real(<T>) :: pivot_value
    integer :: i
    pivot_value = array(pivot_index)
    call swap_<T>(array, pivot_index, right)
    store_index = left
    do i=left, right-1
       if(array(i) <= pivot_value) then
          call swap_<T>(array, i, store_index)
          store_index = store_index + 1
       end if
    end do
    call swap_<T>(array, store_index, right)
  end subroutine partition_<T>

  recursive subroutine quicksort_<T>(array, left, right)
    implicit none
    real(<T>),intent(inout) :: array(:)
    integer,intent(in) :: left, right
    integer :: pivot_index, new_pivot_index
    if(right > left) then
       pivot_index = left+(right-left)/2
       call partition_<T>(array, left, right, pivot_index, new_pivot_index)
       call quicksort_<T>(array, left, new_pivot_index - 1)
       call quicksort_<T>(array, new_pivot_index + 1, right)
    end if
  end subroutine quicksort_<T>

  recursive subroutine quicksort_all_<T>(array)
    implicit none
    real(<T>),intent(inout) :: array(:)
    call quicksort_<T>(array, 1, size(array))
  end subroutine quicksort_all_<T>

  subroutine swap_index_<T>(array, index, i, j)
    implicit none
    real(<T>),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    integer,intent(in) :: i, j
    real(<T>) :: temp
    integer :: temp_i
    temp = array(j)
    temp_i = index(j)
    array(j) = array(i)
    index(j) = index(i)
    array(i) = temp
    index(i) = temp_i
  end subroutine swap_index_<T>

  subroutine partition_index_<T>(array, index, left, right, pivot_index, store_index)
    implicit none
    real(<T>),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    integer,intent(in) :: left, right, pivot_index
    integer,intent(out) :: store_index
    real(<T>) :: pivot_value
    integer :: i
    pivot_value = array(pivot_index)
    call swap_index_<T>(array, index, pivot_index, right)
    store_index = left
    do i=left, right-1
       if(array(i) <= pivot_value) then
          call swap_index_<T>(array, index, i, store_index)
          store_index = store_index + 1
       end if
    end do
    call swap_index_<T>(array, index, store_index, right)
  end subroutine partition_index_<T>

  recursive subroutine quicksort_index_<T>(array, index, left, right)
    implicit none
    real(<T>),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    integer,intent(in) :: left, right
    integer :: pivot_index, new_pivot_index
    if(right > left) then
       pivot_index = left+(right-left)/2
       call partition_index_<T>(array, index, left, right, pivot_index, new_pivot_index)
       call quicksort_index_<T>(array, index, left, new_pivot_index - 1)
       call quicksort_index_<T>(array, index, new_pivot_index + 1, right)
    end if
  end subroutine quicksort_index_<T>

  recursive subroutine quicksort_index_all_<T>(array, index)
    implicit none
    real(<T>),intent(inout) :: array(:)
    integer,intent(inout) :: index(:)
    call quicksort_index_<T>(array, index, 1, size(array))
  end subroutine quicksort_index_all_<T>

  subroutine check_sort_<T>(array)
    implicit none
    real(<T>),intent(inout) :: array(:)
    integer :: j
    do j=1,size(array)-1
       if(array(j+1) < array(j)) stop "Incorrectly sorted array"
    end do
  end subroutine check_sort_<T>

  subroutine test_quicksort_<T>()

    implicit none

    real(<T>),allocatable :: array1(:), array2(:)
    integer,allocatable :: index(:)
    integer :: j,i

    write(*,'(" Testing quicksort")')

    do j=1,20

       write(*,'("  - n=",I0)') 2**j

       allocate(array1(2**j))

       call random_number(array1)

       call quicksort(array1)

       call check_sort_<T>(array1)

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

       call check_sort_<T>(array1)

       if(any(array2(index).ne.array1)) stop "Failure in quicksort_index"

       deallocate(array1, array2, index)

    end do

  end subroutine test_quicksort_<T>

  !!@END FOR

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

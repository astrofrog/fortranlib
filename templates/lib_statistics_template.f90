! Statistics
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

module lib_statistics

  use lib_array
  implicit none
  save

  private

  integer,parameter :: idp = selected_int_kind(13)
  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  public :: mean
  interface mean
     module procedure mean_sp
     module procedure mean_dp
  end interface mean

  public :: median
  interface median
     module procedure median_sp
     module procedure median_dp
  end interface median

  public :: quantile
  interface quantile
     module procedure quantile_sp
     module procedure quantile_dp
  end interface quantile

  public :: variance
  interface variance
     module procedure variance_sp
     module procedure variance_dp
  end interface variance

  public :: clipped_mean
  interface clipped_mean
     module procedure clipped_mean_sp
     module procedure clipped_mean_dp
  end interface clipped_mean

contains

  !!@FOR real(sp):sp real(dp):dp

  @T function mean_<T>(x, mask)
    implicit none
    @T,intent(in) :: x(:)
    logical,intent(in),optional :: mask(:)
    if(present(mask)) then
       mean_<T> = sum(x, mask=mask)/size(x)
    else
       mean_<T> = sum(x)/size(x)
    end if
  end function mean_<T>

  @T function median_<T>(x)
    implicit none
    @T,intent(in) :: x(:)
    @T,dimension(size(x)) :: x_sorted
    integer :: n
    n = size(x)
    x_sorted = x
    call quicksort(x_sorted)
    if(mod(n,2).eq.0) then
       median_<T>=(x_sorted(n/2)+x_sorted(n/2+1))/2.
    else
       median_<T>=x_sorted((n-1)/2+1)
    end if
  end function median_<T>

  @T function quantile_<T>(x, percent, mask)
    implicit none
    @T,intent(in) :: x(:), percent
    logical,intent(in),optional :: mask(:)
    @T,allocatable :: x_sorted(:)
    integer :: n, ipos
    if(present(mask)) then
       n = count(mask)
       allocate(x_sorted(n))
       x_sorted = pack(x, mask)
    else
       n = size(x)
       allocate(x_sorted(n))
       x_sorted = x
    end if
    call quicksort(x_sorted)
    if(percent >= 100._<T>) then
       ipos = n
    else if(percent <= 0._<T>) then
       ipos = 1
    else
       ipos=nint(percent/100._<T>*real(n-1, <T>))+1
    end if
    quantile_<T>=x_sorted(ipos)
  end function quantile_<T>

  @T function variance_<T>(x, mask)
    implicit none
    @T,intent(in) :: x(:)
    logical,intent(in),optional :: mask(:)
    variance_<T> = sum(x-mean(x, mask=mask)**2._<T>)/(size(x)-1)
  end function variance_<T>

  @T function clipped_mean_<T>(x, n)
    implicit none
    @T,intent(in) :: x(:)
    integer,intent(in) :: n
    logical,dimension(size(x)) :: keep
    @T :: sigma
    integer :: n_before
    keep = .true.
    n_before = 0
    do
       clipped_mean_<T> = mean(x, mask=keep)
       sigma = sqrt(variance(x, mask=keep))
       keep = keep .and. abs(x-clipped_mean_<T>) < real(n, <T>) * sigma
       if(count(keep)==n_before) exit
       n_before = count(keep)
    end do
  end function clipped_mean_<T>

  !!@END FOR

end module lib_statistics

! MD5 of template: c1ad43ed9e5862b91b0d659b2ae0bfb1
! Statistics
! Thomas Robitaille (c) 2010

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


  real(dp) function mean_dp(x, mask)
    implicit none
    real(dp),intent(in) :: x(:)
    logical,intent(in),optional :: mask(:)
    if(present(mask)) then
       mean_dp = sum(x, mask=mask)/size(x)  
    else
       mean_dp = sum(x)/size(x)  
    end if
  end function mean_dp

  real(dp) function median_dp(x)
    implicit none
    real(dp),intent(in) :: x(:)
    real(dp),dimension(size(x)) :: x_sorted
    integer :: n
    n = size(x)
    x_sorted = x
    call quicksort(x_sorted)
    if(mod(n,2).eq.0) then
       median_dp=(x_sorted(n/2)+x_sorted(n/2+1))/2.
    else
       median_dp=x_sorted((n-1)/2+1)
    end if
  end function median_dp

  real(dp) function quantile_dp(x, percent, mask)
    implicit none
    real(dp),intent(in) :: x(:), percent
    logical,intent(in),optional :: mask(:)
    real(dp),allocatable :: x_sorted(:)
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
    if(percent >= 100._dp) then
       ipos = n
    else if(percent <= 0._dp) then
       ipos = 1
    else
       ipos=nint(percent/100._dp*real(n-1, dp))+1
    end if
    quantile_dp=x_sorted(ipos)
  end function quantile_dp

  real(dp) function variance_dp(x, mask)
    implicit none
    real(dp),intent(in) :: x(:)
    logical,intent(in),optional :: mask(:)
    variance_dp = sum(x-mean(x, mask=mask)**2._dp)/(size(x)-1)  
  end function variance_dp

  real(dp) function clipped_mean_dp(x, n)
    implicit none
    real(dp),intent(in) :: x(:)
    integer,intent(in) :: n
    logical,dimension(size(x)) :: keep
    real(dp) :: sigma
    integer :: n_before
    keep = .true.
    n_before = 0
    do
       clipped_mean_dp = mean(x, mask=keep)
       sigma = sqrt(variance(x, mask=keep))
       keep = keep .and. abs(x-clipped_mean_dp) < real(n, dp) * sigma
       if(count(keep)==n_before) exit
       n_before = count(keep)
    end do
  end function clipped_mean_dp


  real(sp) function mean_sp(x, mask)
    implicit none
    real(sp),intent(in) :: x(:)
    logical,intent(in),optional :: mask(:)
    if(present(mask)) then
       mean_sp = sum(x, mask=mask)/size(x)  
    else
       mean_sp = sum(x)/size(x)  
    end if
  end function mean_sp

  real(sp) function median_sp(x)
    implicit none
    real(sp),intent(in) :: x(:)
    real(sp),dimension(size(x)) :: x_sorted
    integer :: n
    n = size(x)
    x_sorted = x
    call quicksort(x_sorted)
    if(mod(n,2).eq.0) then
       median_sp=(x_sorted(n/2)+x_sorted(n/2+1))/2.
    else
       median_sp=x_sorted((n-1)/2+1)
    end if
  end function median_sp

  real(sp) function quantile_sp(x, percent, mask)
    implicit none
    real(sp),intent(in) :: x(:), percent
    logical,intent(in),optional :: mask(:)
    real(sp),allocatable :: x_sorted(:)
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
    if(percent >= 100._sp) then
       ipos = n
    else if(percent <= 0._sp) then
       ipos = 1
    else
       ipos=nint(percent/100._sp*real(n-1, sp))+1
    end if
    quantile_sp=x_sorted(ipos)
  end function quantile_sp

  real(sp) function variance_sp(x, mask)
    implicit none
    real(sp),intent(in) :: x(:)
    logical,intent(in),optional :: mask(:)
    variance_sp = sum(x-mean(x, mask=mask)**2._sp)/(size(x)-1)  
  end function variance_sp

  real(sp) function clipped_mean_sp(x, n)
    implicit none
    real(sp),intent(in) :: x(:)
    integer,intent(in) :: n
    logical,dimension(size(x)) :: keep
    real(sp) :: sigma
    integer :: n_before
    keep = .true.
    n_before = 0
    do
       clipped_mean_sp = mean(x, mask=keep)
       sigma = sqrt(variance(x, mask=keep))
       keep = keep .and. abs(x-clipped_mean_sp) < real(n, sp) * sigma
       if(count(keep)==n_before) exit
       n_before = count(keep)
    end do
  end function clipped_mean_sp


end module lib_statistics

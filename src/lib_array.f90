! MD5 of template: 483bdffc8944c3cf3b8ce14a69d216da
! Array related routines (Integration, Interpolation, etc.)
! Thomas Robitaille (c) 2009

module lib_array

  implicit none
  save

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

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
     module procedure integral_all_sp
     module procedure integral_all_dp
  end interface integral

  public :: integral_log10
  interface integral_log10
     module procedure integral_log10_sp
     module procedure integral_log10_dp
     module procedure integral_all_log10_sp
     module procedure integral_all_log10_dp
  end interface integral_log10

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

  public :: interp2d
  interface interp2d
     module procedure interp2d_sp
     module procedure interp2d_dp
  end interface interp2d

  public :: interp1d_log10
  interface interp1d_log10
     module procedure interp1d_log10_sp
     module procedure interp1d_log10_dp
     module procedure interp1d_array_log10_sp
     module procedure interp1d_array_log10_dp
  end interface interp1d_log10

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

contains


  subroutine linspace_dp(xmin,xmax,x)
    implicit none
    real(dp),intent(in) :: xmin,xmax
    real(dp),intent(out) :: x(:)
    integer :: i,n
    n = size(x)
    do i=1,n
       x(i) = (xmax-xmin) * real(i-1,dp) / real(n-1,dp) + xmin
    end do
  end subroutine linspace_dp

  subroutine logspace_dp(xmin,xmax,x)
    implicit none
    real(dp),intent(in) :: xmin,xmax
    real(dp),intent(out) :: x(:)
    call linspace(log10(xmin),log10(xmax),x)
    x = 10._dp**x
  end subroutine logspace_dp

  real(dp) function integral_all_dp(x,y)
    ! Total integral of a function
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp)            :: x1,x2
    x1 = minval(x,1)
    x2 = maxval(x,1)
    integral_all_dp = integral_dp(x,y,x1,x2)
  end function integral_all_dp

  real(dp) function integral_dp(x,y,x1,x2)
    ! Integral of a function between two limits

    implicit none

    real(dp),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(dp) :: f1,f2,sum
    integer :: j

    if(x1.gt.x(size(x)).or.x2.lt.x(1)) then
       integral_dp=0._dp
       return
    end if

    if(x1.gt.x(1)) then
       i1=locate(x,x1)
       f1 = interp1d(x,y,x1)
    else 
       i1=0
    end if

    if(x2.lt.x(size(x))) then
       i2=locate(x,x2)
       f2=interp1d(x,y,x2)
    else
       i2=size(x)
    end if

    sum=0.d0

    if(i2.gt.i1) then

       ! Add central part:
       do j=i1+1,i2-1
          sum=sum+0.5_dp*(y(j)+y(j+1))*(x(j+1)-x(j))
       end do

       ! Add extremities
       if(x1.gt.x(1)) sum=sum+0.5_dp*(f1+y(i1+1))*(x(i1+1)-x1)
       if(x2.lt.x(size(x))) sum=sum+0.5_dp*(f2+y(i2))*(x2-x(i2))

       integral_dp=real(sum, dp)

    else

       integral_dp=0.5_dp*(f1+f2)*(x2-x1)

    end if

  end function integral_dp


  real(dp) function integral_all_log10_dp(x,y)
    ! Total integral of a function
    ! (uses log10 interpolation)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp)            :: x1,x2
    x1 = minval(x,1)
    x2 = maxval(x,1)
    integral_all_log10_dp = integral_log10_dp(x,y,x1,x2)
  end function integral_all_log10_dp


  real(dp) function integral_log10_dp(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses log10 interpolation)

    implicit none

    real(dp),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(dp) :: f1,f2,sum
    integer :: j

    if(x1.gt.x(size(x)).or.x2.lt.x(1)) then
       integral_log10_dp=0._dp
       return
    end if

    if(x1.gt.x(1)) then
       i1=locate(x,x1)
       f1 = interp1d_log10(x,y,x1)
    else 
       i1=0
    end if

    if(x2.lt.x(size(x))) then
       i2=locate(x,x2)
       f2=interp1d_log10(x,y,x2)
    else
       i2=size(x)
    end if

    sum=0.d0

    if(i2.gt.i1) then

       ! Add central part:
       do j=i1+1,i2-1
          sum=sum+trapezium_log10(x(j),y(j),x(j+1),y(j+1))
       end do

       ! Add extremities
       if(x1.gt.x(1)) sum=sum+trapezium_log10(x1,f1,x(i1+1),y(i1+1))
       if(x2.lt.x(size(x))) sum=sum+trapezium_log10(x(i2),y(i2),x2,f2)

       integral_log10_dp=real(sum, dp)

    else

       integral_log10_dp=trapezium_log10(x1,f1,x2,f2)

    end if

  contains

    real(dp) function trapezium_log10(x1,y1,x2,y2)
      implicit none
      real(dp),intent(in) :: x1,y1,x2,y2
      real(dp) :: b
      if(x1==x2) then
         trapezium_log10 = 0.
      else if(y1==0..or.y2==0.) then
         trapezium_log10 = 0.
      else
         b = log10(y1/y2) / log10(x1/x2)
         trapezium_log10 = y1 * (x2*(x2/x1)**b-x1) / (b+1)
      end if
    end function trapezium_log10

  end function integral_log10_dp

  function interp1d_array_log10_dp(x,y,xval,bounds_error,fill_value)
    implicit none
    real(dp),intent(in) :: x(:),y(:)
    real(dp),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(dp),intent(in),optional :: fill_value
    real(dp) :: interp1d_array_log10_dp(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_log10_dp(i) = interp1d_log10_dp(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_log10_dp

  real(dp) function interp1d_log10_dp(x,y,xval,bounds_error,fill_value)
    ! Interpolate y = f(x) at xval in log10 space
    ! This is faster than using interp1d(log10(x),log10(y),log10(xval))
    ! because we only take the log10() of 5 values instead of the
    ! entire input arrays. But the results should be the same

    implicit none

    ! --- Input --- !

    integer :: n
    ! the size of the array

    real(dp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(dp),intent(in) :: xval
    ! the value at which to interpolate y

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(dp),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    ! --- Local variables --- !

    integer :: ipos
    ! position of x value in x array

    real(dp) :: frac
    ! temporary fraction

    real(dp) :: x1,x2,y1,y2

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
          fill_value_tmp = 0.
       end if
    end if

    n = size(x)

    ipos = locate(x,xval)

    ! --- First some error checking --- !

    if(ipos == -1) then
       if(bounds_error_tmp) then
          write(*,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') xval,x(1),x(n)
          stop
       else
          interp1d_log10_dp = fill_value_tmp
          return
       end if
    end if

    if( ipos < n .and. ipos > 0) then
       if(y(ipos)==0..or.y(ipos+1)==0.) then
          interp1d_log10_dp = 0.
       else
          x1 = log10(x(ipos))
          x2 = log10(x(ipos+1))
          y1 = log10(y(ipos))
          y2 = log10(y(ipos+1))
          frac = ( log10(xval) - x1 ) / ( x2 - x1 )
          interp1d_log10_dp = y1 + frac * ( y2 - y1 )
          interp1d_log10_dp = 10._dp**(interp1d_log10_dp)
       end if
    else if(ipos == n) then
       interp1d_log10_dp = y(n)
    else if(ipos == 0) then
       interp1d_log10_dp = y(1)
    else
       write(*,'("ERROR: Unexpected value of ipos : ",I0)') ipos
       stop
    end if

  end function interp1d_log10_dp

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

  real(dp) function interp1d_dp(x,y,xval,bounds_error,fill_value)
    ! Interpolate y = f(x) at xval

    implicit none

    ! --- Input --- !

    integer :: n
    ! the size of the array

    real(dp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(dp),intent(in) :: xval
    ! the value at which to interpolate y

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(dp),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    ! --- Local variables --- !

    integer :: ipos
    ! position of x value in x array

    real(dp) :: frac
    ! temporary fraction

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
          fill_value_tmp = 0.
       end if
    end if

    n = size(x)

    ipos = locate(x,xval)

    ! --- First some error checking --- !

    if(ipos == -1) then
       if(bounds_error_tmp) then
          write(*,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') xval,x(1),x(n)
          stop
       else
          interp1d_dp = fill_value_tmp
          return
       end if
    end if

    if( ipos < n .and. ipos > 0) then
       frac = ( xval - x(ipos) ) / ( x(ipos+1) - x(ipos) )
       interp1d_dp = y(ipos) + frac * ( y(ipos+1) - y(ipos) )
    else if(ipos == n) then
       interp1d_dp = y(n)
    else if(ipos == 0) then
       interp1d_dp = y(1)
    else
       write(*,'("ERROR: Unexpected value of ipos : ",I0)') ipos
       stop
    end if

  end function interp1d_dp


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
          fill_value_tmp = 0.
       end if
    end if

    if(size(x).ne.size(array,1)) stop "x does not match array"
    if(size(y).ne.size(array,2)) stop "y does not match array"

    i1 = locate(x,x0) ; i2 = i1 + 1
    j1 = locate(y,y0) ; j2 = j1 + 1

    if(i1==-1) then
       if(bounds_error_tmp) then
          write(*,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') x0,x(1),x(size(x))
          stop
       else
          value = fill_value_tmp
          return
       end if
    end if

    if(j1==-1) then
       if(bounds_error_tmp) then
          write(*,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') y0,y(1),y(size(y))
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


  integer pure function ipos_dp(xmin,xmax,x,nbin)
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
       else
          frac=(x-xmin)/(xmax-xmin)
          ipos_dp=int(frac*real(nbin))+1
       end if

    else

       if(x < xmax) then
          ipos_dp = 0
       else if(x > xmin) then
          ipos_dp = nbin+1
       else
          frac=(x-xmin)/(xmax-xmin)
          ipos_dp=int(frac*real(nbin))+1
       end if

    end if

  end function ipos_dp


  real(dp) pure function xval_dp(xmin,xmax,i,nbin)
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


  pure subroutine histogram1d_dp(array,xmin,xmax,nbin,hist_x,hist_y,mask,weights)
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


  subroutine linspace_sp(xmin,xmax,x)
    implicit none
    real(sp),intent(in) :: xmin,xmax
    real(sp),intent(out) :: x(:)
    integer :: i,n
    n = size(x)
    do i=1,n
       x(i) = (xmax-xmin) * real(i-1,sp) / real(n-1,sp) + xmin
    end do
  end subroutine linspace_sp

  subroutine logspace_sp(xmin,xmax,x)
    implicit none
    real(sp),intent(in) :: xmin,xmax
    real(sp),intent(out) :: x(:)
    call linspace(log10(xmin),log10(xmax),x)
    x = 10._sp**x
  end subroutine logspace_sp

  real(sp) function integral_all_sp(x,y)
    ! Total integral of a function
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp)            :: x1,x2
    x1 = minval(x,1)
    x2 = maxval(x,1)
    integral_all_sp = integral_sp(x,y,x1,x2)
  end function integral_all_sp

  real(sp) function integral_sp(x,y,x1,x2)
    ! Integral of a function between two limits

    implicit none

    real(sp),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(sp) :: f1,f2,sum
    integer :: j

    if(x1.gt.x(size(x)).or.x2.lt.x(1)) then
       integral_sp=0._sp
       return
    end if

    if(x1.gt.x(1)) then
       i1=locate(x,x1)
       f1 = interp1d(x,y,x1)
    else 
       i1=0
    end if

    if(x2.lt.x(size(x))) then
       i2=locate(x,x2)
       f2=interp1d(x,y,x2)
    else
       i2=size(x)
    end if

    sum=0.d0

    if(i2.gt.i1) then

       ! Add central part:
       do j=i1+1,i2-1
          sum=sum+0.5_sp*(y(j)+y(j+1))*(x(j+1)-x(j))
       end do

       ! Add extremities
       if(x1.gt.x(1)) sum=sum+0.5_sp*(f1+y(i1+1))*(x(i1+1)-x1)
       if(x2.lt.x(size(x))) sum=sum+0.5_sp*(f2+y(i2))*(x2-x(i2))

       integral_sp=real(sum, sp)

    else

       integral_sp=0.5_sp*(f1+f2)*(x2-x1)

    end if

  end function integral_sp


  real(sp) function integral_all_log10_sp(x,y)
    ! Total integral of a function
    ! (uses log10 interpolation)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp)            :: x1,x2
    x1 = minval(x,1)
    x2 = maxval(x,1)
    integral_all_log10_sp = integral_log10_sp(x,y,x1,x2)
  end function integral_all_log10_sp


  real(sp) function integral_log10_sp(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses log10 interpolation)

    implicit none

    real(sp),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(sp) :: f1,f2,sum
    integer :: j

    if(x1.gt.x(size(x)).or.x2.lt.x(1)) then
       integral_log10_sp=0._sp
       return
    end if

    if(x1.gt.x(1)) then
       i1=locate(x,x1)
       f1 = interp1d_log10(x,y,x1)
    else 
       i1=0
    end if

    if(x2.lt.x(size(x))) then
       i2=locate(x,x2)
       f2=interp1d_log10(x,y,x2)
    else
       i2=size(x)
    end if

    sum=0.d0

    if(i2.gt.i1) then

       ! Add central part:
       do j=i1+1,i2-1
          sum=sum+trapezium_log10(x(j),y(j),x(j+1),y(j+1))
       end do

       ! Add extremities
       if(x1.gt.x(1)) sum=sum+trapezium_log10(x1,f1,x(i1+1),y(i1+1))
       if(x2.lt.x(size(x))) sum=sum+trapezium_log10(x(i2),y(i2),x2,f2)

       integral_log10_sp=real(sum, sp)

    else

       integral_log10_sp=trapezium_log10(x1,f1,x2,f2)

    end if

  contains

    real(sp) function trapezium_log10(x1,y1,x2,y2)
      implicit none
      real(sp),intent(in) :: x1,y1,x2,y2
      real(sp) :: b
      if(x1==x2) then
         trapezium_log10 = 0.
      else if(y1==0..or.y2==0.) then
         trapezium_log10 = 0.
      else
         b = log10(y1/y2) / log10(x1/x2)
         trapezium_log10 = y1 * (x2*(x2/x1)**b-x1) / (b+1)
      end if
    end function trapezium_log10

  end function integral_log10_sp

  function interp1d_array_log10_sp(x,y,xval,bounds_error,fill_value)
    implicit none
    real(sp),intent(in) :: x(:),y(:)
    real(sp),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(sp),intent(in),optional :: fill_value
    real(sp) :: interp1d_array_log10_sp(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_log10_sp(i) = interp1d_log10_sp(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_log10_sp

  real(sp) function interp1d_log10_sp(x,y,xval,bounds_error,fill_value)
    ! Interpolate y = f(x) at xval in log10 space
    ! This is faster than using interp1d(log10(x),log10(y),log10(xval))
    ! because we only take the log10() of 5 values instead of the
    ! entire input arrays. But the results should be the same

    implicit none

    ! --- Input --- !

    integer :: n
    ! the size of the array

    real(sp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(sp),intent(in) :: xval
    ! the value at which to interpolate y

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(sp),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    ! --- Local variables --- !

    integer :: ipos
    ! position of x value in x array

    real(sp) :: frac
    ! temporary fraction

    real(sp) :: x1,x2,y1,y2

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
          fill_value_tmp = 0.
       end if
    end if

    n = size(x)

    ipos = locate(x,xval)

    ! --- First some error checking --- !

    if(ipos == -1) then
       if(bounds_error_tmp) then
          write(*,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') xval,x(1),x(n)
          stop
       else
          interp1d_log10_sp = fill_value_tmp
          return
       end if
    end if

    if( ipos < n .and. ipos > 0) then
       if(y(ipos)==0..or.y(ipos+1)==0.) then
          interp1d_log10_sp = 0.
       else
          x1 = log10(x(ipos))
          x2 = log10(x(ipos+1))
          y1 = log10(y(ipos))
          y2 = log10(y(ipos+1))
          frac = ( log10(xval) - x1 ) / ( x2 - x1 )
          interp1d_log10_sp = y1 + frac * ( y2 - y1 )
          interp1d_log10_sp = 10._sp**(interp1d_log10_sp)
       end if
    else if(ipos == n) then
       interp1d_log10_sp = y(n)
    else if(ipos == 0) then
       interp1d_log10_sp = y(1)
    else
       write(*,'("ERROR: Unexpected value of ipos : ",I0)') ipos
       stop
    end if

  end function interp1d_log10_sp

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

  real(sp) function interp1d_sp(x,y,xval,bounds_error,fill_value)
    ! Interpolate y = f(x) at xval

    implicit none

    ! --- Input --- !

    integer :: n
    ! the size of the array

    real(sp),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(sp),intent(in) :: xval
    ! the value at which to interpolate y

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(sp),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    ! --- Local variables --- !

    integer :: ipos
    ! position of x value in x array

    real(sp) :: frac
    ! temporary fraction

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
          fill_value_tmp = 0.
       end if
    end if

    n = size(x)

    ipos = locate(x,xval)

    ! --- First some error checking --- !

    if(ipos == -1) then
       if(bounds_error_tmp) then
          write(*,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') xval,x(1),x(n)
          stop
       else
          interp1d_sp = fill_value_tmp
          return
       end if
    end if

    if( ipos < n .and. ipos > 0) then
       frac = ( xval - x(ipos) ) / ( x(ipos+1) - x(ipos) )
       interp1d_sp = y(ipos) + frac * ( y(ipos+1) - y(ipos) )
    else if(ipos == n) then
       interp1d_sp = y(n)
    else if(ipos == 0) then
       interp1d_sp = y(1)
    else
       write(*,'("ERROR: Unexpected value of ipos : ",I0)') ipos
       stop
    end if

  end function interp1d_sp


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
          fill_value_tmp = 0.
       end if
    end if

    if(size(x).ne.size(array,1)) stop "x does not match array"
    if(size(y).ne.size(array,2)) stop "y does not match array"

    i1 = locate(x,x0) ; i2 = i1 + 1
    j1 = locate(y,y0) ; j2 = j1 + 1

    if(i1==-1) then
       if(bounds_error_tmp) then
          write(*,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') x0,x(1),x(size(x))
          stop
       else
          value = fill_value_tmp
          return
       end if
    end if

    if(j1==-1) then
       if(bounds_error_tmp) then
          write(*,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') y0,y(1),y(size(y))
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


  integer pure function ipos_sp(xmin,xmax,x,nbin)
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
       else
          frac=(x-xmin)/(xmax-xmin)
          ipos_sp=int(frac*real(nbin))+1
       end if

    else

       if(x < xmax) then
          ipos_sp = 0
       else if(x > xmin) then
          ipos_sp = nbin+1
       else
          frac=(x-xmin)/(xmax-xmin)
          ipos_sp=int(frac*real(nbin))+1
       end if

    end if

  end function ipos_sp


  real(sp) pure function xval_sp(xmin,xmax,i,nbin)
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


  pure subroutine histogram1d_sp(array,xmin,xmax,nbin,hist_x,hist_y,mask,weights)
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

    w = nint(sigma * 5.)

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

                array(i,j) = array(i,j) + array_orig(ii,jj) * exp(-d/2.)
                array_count(i,j) = array_count(i,j) + exp(-d/2.)

             end do
          end do

       end do
    end do

    array = array / array_count

  end subroutine smooth_2d

end module lib_array

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

contains

  !!@FOR real(sp):sp real(dp):dp

  subroutine linspace_<T>(xmin,xmax,x)
    implicit none
    @T,intent(in) :: xmin,xmax
    @T,intent(out) :: x(:)
    integer :: i,n
    n = size(x)
    do i=1,n
       x(i) = (xmax-xmin) * real(i-1,<T>) / real(n-1,<T>) + xmin
    end do
  end subroutine linspace_<T>

  subroutine logspace_<T>(xmin,xmax,x)
    implicit none
    @T,intent(in) :: xmin,xmax
    @T,intent(out) :: x(:)
    call linspace(log10(xmin),log10(xmax),x)
    x = 10._<T>**x
  end subroutine logspace_<T>

  real(<T>) function integral_all_<T>(x,y)
    ! Total integral of a function
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    real(<T>)            :: x1,x2
    x1 = minval(x,1)
    x2 = maxval(x,1)
    integral_all_<T> = integral_<T>(x,y,x1,x2)
  end function integral_all_<T>

  real(<T>) function integral_<T>(x,y,x1,x2)
    ! Integral of a function between two limits

    implicit none

    real(<T>),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(<T>) :: f1,f2,sum
    integer :: j

    if(x1.gt.x(size(x)).or.x2.lt.x(1)) then
       integral_<T>=0._<T>
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
          sum=sum+0.5_<T>*(y(j)+y(j+1))*(x(j+1)-x(j))
       end do

       ! Add extremities
       if(x1.gt.x(1)) sum=sum+0.5_<T>*(f1+y(i1+1))*(x(i1+1)-x1)
       if(x2.lt.x(size(x))) sum=sum+0.5_<T>*(f2+y(i2))*(x2-x(i2))

       integral_<T>=real(sum, <T>)

    else

       integral_<T>=0.5_<T>*(f1+f2)*(x2-x1)

    end if

  end function integral_<T>


  real(<T>) function integral_all_log10_<T>(x,y)
    ! Total integral of a function
    ! (uses log10 interpolation)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    real(<T>)            :: x1,x2
    x1 = minval(x,1)
    x2 = maxval(x,1)
    integral_all_log10_<T> = integral_log10_<T>(x,y,x1,x2)
  end function integral_all_log10_<T>


  real(<T>) function integral_log10_<T>(x,y,x1,x2)
    ! Integral of a function between two limits
    ! (uses log10 interpolation)

    implicit none

    real(<T>),intent(in) :: x(:),y(:),x1,x2
    integer :: i1,i2
    real(<T>) :: f1,f2,sum
    integer :: j

    if(x1.gt.x(size(x)).or.x2.lt.x(1)) then
       integral_log10_<T>=0._<T>
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

       integral_log10_<T>=real(sum, <T>)

    else

       integral_log10_<T>=trapezium_log10(x1,f1,x2,f2)

    end if

  contains

    real(<T>) function trapezium_log10(x1,y1,x2,y2)
      implicit none
      real(<T>),intent(in) :: x1,y1,x2,y2
      real(<T>) :: b
      if(x1==x2) then
         trapezium_log10 = 0.
      else if(y1==0..or.y2==0.) then
         trapezium_log10 = 0.
      else
         b = log10(y1/y2) / log10(x1/x2)
         trapezium_log10 = y1 * (x2*(x2/x1)**b-x1) / (b+1)
      end if
    end function trapezium_log10

  end function integral_log10_<T>

  function interp1d_array_log10_<T>(x,y,xval,bounds_error,fill_value)
    implicit none
    real(<T>),intent(in) :: x(:),y(:)
    real(<T>),intent(in) :: xval(:)
    logical,intent(in),optional :: bounds_error
    real(<T>),intent(in),optional :: fill_value
    real(<T>) :: interp1d_array_log10_<T>(size(xval))
    integer :: i
    do i=1,size(xval)
       interp1d_array_log10_<T>(i) = interp1d_log10_<T>(x,y,xval(i),bounds_error,fill_value)
    end do
  end function interp1d_array_log10_<T>

  real(<T>) function interp1d_log10_<T>(x,y,xval,bounds_error,fill_value)
    ! Interpolate y = f(x) at xval in log10 space
    ! This is faster than using interp1d(log10(x),log10(y),log10(xval))
    ! because we only take the log10() of 5 values instead of the
    ! entire input arrays. But the results should be the same

    implicit none

    ! --- Input --- !

    integer :: n
    ! the size of the array

    real(<T>),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(<T>),intent(in) :: xval
    ! the value at which to interpolate y

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(<T>),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    ! --- Local variables --- !

    integer :: ipos
    ! position of x value in x array

    real(<T>) :: frac
    ! temporary fraction

    real(<T>) :: x1,x2,y1,y2

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
          interp1d_log10_<T> = fill_value_tmp
          return
       end if
    end if

    if( ipos < n .and. ipos > 0) then
       if(y(ipos)==0..or.y(ipos+1)==0.) then
          interp1d_log10_<T> = 0.
       else
          x1 = log10(x(ipos))
          x2 = log10(x(ipos+1))
          y1 = log10(y(ipos))
          y2 = log10(y(ipos+1))
          frac = ( log10(xval) - x1 ) / ( x2 - x1 )
          interp1d_log10_<T> = y1 + frac * ( y2 - y1 )
          interp1d_log10_<T> = 10._<T>**(interp1d_log10_<T>)
       end if
    else if(ipos == n) then
       interp1d_log10_<T> = y(n)
    else if(ipos == 0) then
       interp1d_log10_<T> = y(1)
    else
       write(*,'("ERROR: Unexpected value of ipos : ",I0)') ipos
       stop
    end if

  end function interp1d_log10_<T>

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

  real(<T>) function interp1d_<T>(x,y,xval,bounds_error,fill_value)
    ! Interpolate y = f(x) at xval

    implicit none

    ! --- Input --- !

    integer :: n
    ! the size of the array

    real(<T>),dimension(:),intent(in) :: x,y
    ! the x and y arrays

    real(<T>),intent(in) :: xval
    ! the value at which to interpolate y

    logical,intent(in),optional :: bounds_error
    ! whether to raise an out of bounds error

    real(<T>),intent(in),optional :: fill_value
    ! value for out of bounds if bounds_error is .false.

    ! --- Local variables --- !

    integer :: ipos
    ! position of x value in x array

    real(<T>) :: frac
    ! temporary fraction

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
          interp1d_<T> = fill_value_tmp
          return
       end if
    end if

    if( ipos < n .and. ipos > 0) then
       frac = ( xval - x(ipos) ) / ( x(ipos+1) - x(ipos) )
       interp1d_<T> = y(ipos) + frac * ( y(ipos+1) - y(ipos) )
    else if(ipos == n) then
       interp1d_<T> = y(n)
    else if(ipos == 0) then
       interp1d_<T> = y(1)
    else
       write(*,'("ERROR: Unexpected value of ipos : ",I0)') ipos
       stop
    end if

  end function interp1d_<T>


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

    norm = 1._<T> / (x(i2) - x(i1)) / (y(j2)-y(j1))

    value =   array(i1,j1) * (x(i2)-x0) * (y(j2)-y0) * norm &
         &       + array(i2,j1) * (x0-x(i1)) * (y(j2)-y0) * norm &
         &       + array(i1,j2) * (x(i2)-x0) * (y0-y(j1)) * norm &
         &       + array(i2,j2) * (x0-x(i1)) * (y0-y(j1)) * norm

  end function interp2d_<T>


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


  integer pure function ipos_<T>(xmin,xmax,x,nbin)
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
       else
          frac=(x-xmin)/(xmax-xmin)
          ipos_<T>=int(frac*real(nbin))+1
       end if

    else

       if(x < xmax) then
          ipos_<T> = 0
       else if(x > xmin) then
          ipos_<T> = nbin+1
       else
          frac=(x-xmin)/(xmax-xmin)
          ipos_<T>=int(frac*real(nbin))+1
       end if

    end if

  end function ipos_<T>


  real(<T>) pure function xval_<T>(xmin,xmax,i,nbin)
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


  pure subroutine histogram1d_<T>(array,xmin,xmax,nbin,hist_x,hist_y,mask,weights)
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


  pure subroutine swap_<T>(array, i, j)
    implicit none
    real(<T>),intent(inout) :: array(:)
    integer,intent(in) :: i, j
    real(<T>) :: temp
    temp = array(j)
    array(j) = array(i)
    array(i) = temp
  end subroutine swap_<T>

  pure subroutine partition_<T>(array, left, right, pivot_index, store_index)
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

  recursive pure subroutine quicksort_<T>(array, left, right)
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

  recursive pure subroutine quicksort_all_<T>(array)
    implicit none
    real(<T>),intent(inout) :: array(:)
    call quicksort_<T>(array, 1, size(array))
  end subroutine quicksort_all_<T>

  pure subroutine swap_index_<T>(array, index, i, j)
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

  pure subroutine partition_index_<T>(array, index, left, right, pivot_index, store_index)
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

  recursive pure subroutine quicksort_index_<T>(array, index, left, right)
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

  recursive pure subroutine quicksort_index_all_<T>(array, index)
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

! Algebra routines
! Thomas Robitaille (c) 2009

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
  end interface

  public :: quadratic
  interface quadratic
     module procedure quadratic_sp
     module procedure quadratic_dp
  end interface

  public :: quadratic_reduced
  interface quadratic_reduced
     module procedure quadratic_reduced_sp
     module procedure quadratic_reduced_dp
  end interface

  public :: quadratic_pascal
  interface quadratic_pascal
     module procedure quadratic_pascal_sp
     module procedure quadratic_pascal_dp
  end interface

  public :: quadratic_pascal_reduced
  interface quadratic_pascal_reduced
     module procedure quadratic_pascal_reduced_sp
     module procedure quadratic_pascal_reduced_dp
  end interface
  
  public :: lineq_gausselim
  interface lineq_gausselim
     module procedure lineq_gausselim_sp
     module procedure lineq_gausselim_dp
  end interface

contains

  !!@FOR real(sp):sp real(dp):dp

  @T function cbrt_<T>(x)
    implicit none
    @T :: x
    @T,parameter :: alpha = 1._<T> / 3._<T>
    if(x >= 0.) then
       cbrt_<T> = x**alpha
    else
       cbrt_<T> = - (abs(x))**alpha
    end if
  end function cbrt_<T>

  subroutine quadratic_reduced_<T>(b,c,x1,x2)
    implicit none
    @T,intent(in)  :: b,c
    @T,intent(out) :: x1,x2
    @T :: delta
    delta = b*b - 4._<T>*c
    if(delta > 0) then
       delta = sqrt(delta)
       x1 = ( - b - delta ) * 0.5_<T>
       x2 = ( - b + delta ) * 0.5_<T>
    else
       x1 = huge(x1)
       x2 = huge(x2)
    end if
  end subroutine quadratic_reduced_<T>

  subroutine quadratic_<T>(a,b,c,x1,x2)
    implicit none
    @T,intent(in)  :: a,b,c
    @T,intent(out) :: x1,x2
    @T :: delta,factor
    delta = b*b - 4._<T>*a*c
    if(delta > 0) then
       delta = sqrt(delta)
       factor = 0.5_<T> / a
       x1 = ( - b - delta ) * factor
       x2 = ( - b + delta ) * factor
    else
       x1 = huge(x1)
       x2 = huge(x2)
    end if
  end subroutine quadratic_<T>

  subroutine quadratic_pascal_<T>(a,b,c,x1,x2)
    implicit none
    @T,intent(in)  :: a,b,c
    @T,intent(out) :: x1,x2
    @T :: q,delta
    delta = b*b - 4._<T>*a*c
    if(delta > 0) then
       delta = sqrt(delta)
       delta = sign(delta,b)
       q = -0.5_<T> * ( b + delta )
       x1 = q / a
       x2 = c / q
    else if(delta < 0) then
       x1 = -huge(x1)
       x2 = -huge(x2)
    else
       x1 = - 2.0_<T> * c / b
       x2 = -huge(x2)
    end if
  end subroutine quadratic_pascal_<T>

  subroutine quadratic_pascal_reduced_<T>(b,c,x1,x2)
    implicit none
    @T,intent(in)  :: b,c
    @T,intent(out) :: x1,x2
    @T :: q,delta
    delta = b*b - 4._<T>*c
    if(delta > 0) then
       delta = sqrt(delta)
       delta = sign(delta,b)
       q = -0.5_<T> * ( b + delta )
       x1 = q
       x2 = c / q
    else if(delta < 0) then
       x1 = -huge(x1)
       x2 = -huge(x2)
    else
       x1 = - 2.0_<T> * c / b
       x2 = -huge(x2)
    end if
  end subroutine quadratic_pascal_reduced_<T>

  subroutine lineq_gausselim_<T>(a,b)
  
    implicit none
    real(<T>),intent(inout) :: a(:,:),b(:)
    real(<T>) :: frac
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

  end subroutine lineq_gausselim_<T>
 
    !!@END FOR

end module lib_algebra

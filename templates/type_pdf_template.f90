! Probability Distribution Function (PDF) related routines
! Thomas Robitaille (c) 2009

module type_pdf

  use lib_array
  use lib_random

  implicit none

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  real,parameter :: unset = -1.e30

  !!@FOR real(sp):sp real(dp):dp

  public :: pdf_<T>
  type pdf_<T>
     integer :: n = 0
     @T,allocatable :: x(:)
     @T,allocatable :: pdf(:)
     @T,allocatable :: cdf(:)
     logical :: log = .false.
     logical :: normalized = .false.
  end type pdf_<T>

  public :: pdf_discrete_<T>
  type pdf_discrete_<T>
     integer :: n = 0
     @T,allocatable :: pdf(:)
     @T,allocatable :: cdf(:)
     logical :: normalized = .false.
  end type pdf_discrete_<T>

  !!@END FOR

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

  !!@FOR real(sp):sp real(dp):dp

  subroutine find_cdf_discrete_<T>(p)
    implicit none
    type(pdf_discrete_<T>),intent(inout) :: p
    integer :: i
    @T :: norm
    if(p%n==0) stop "[find_cdf_discrete] PDF is not set correctly (y)"
    p%cdf(1) = p%pdf(1)
    do i=2,p%n
       p%cdf(i) = p%cdf(i-1) + p%pdf(i)
    end do
    norm = p%cdf(p%n)
    if(norm==0._<T>) stop "[find_cdf_discrete] all PDF elements are zero"
    p%cdf = p%cdf / norm
  end subroutine find_cdf_discrete_<T>

  subroutine allocate_pdf_discrete_<T>(p,n)
    implicit none
    type(pdf_discrete_<T>),intent(out) :: p
    integer,intent(in) :: n
    p%n = n
    allocate(p%pdf(n))
    allocate(p%cdf(n))
  end subroutine allocate_pdf_discrete_<T>

  subroutine allocate_pdf_cont_<T>(p,n)
    implicit none
    type(pdf_<T>),intent(out) :: p
    integer,intent(in) :: n
    p%n = n
    allocate(p%x(n))
    allocate(p%pdf(n))
    allocate(p%cdf(n))
  end subroutine allocate_pdf_cont_<T>

  subroutine normalize_pdf_discrete_<T>(p)
    implicit none
    type(pdf_discrete_<T>),intent(inout) :: p
    p%pdf = p%pdf / sum(p%pdf)
    p%normalized = .true.
  end subroutine normalize_pdf_discrete_<T>

  subroutine normalize_pdf_cont_<T>(p)
    implicit none
    type(pdf_<T>),intent(inout) :: p
    if(p%log) then
       p%pdf = p%pdf / integral_loglog(p%x, p%pdf)
    else
       p%pdf = p%pdf / integral(p%x, p%pdf)
    end if
    p%normalized = .true.
  end subroutine normalize_pdf_cont_<T>

  subroutine set_pdf_discrete_<T>(p,y)
    implicit none
    type(pdf_discrete_<T>),intent(out) :: p
    @T,intent(in) :: y(:)
    call allocate_pdf(p,size(y))
    p%pdf = y
    call normalize_pdf(p)
    call find_cdf(p)
    call check_pdf(p)
  end subroutine set_pdf_discrete_<T>

  subroutine set_pdf_cont_<T>(p,x,y,log)
    implicit none
    type(pdf_<T>),intent(out) :: p
    @T,intent(in) :: x(:),y(:)
    logical,intent(in),optional :: log
    if(size(x).ne.size(y)) stop "[set_pdf] x and y array sizes differ"
    call allocate_pdf(p,size(y))
    p%x   = x
    p%pdf = y
    if(present(log)) then
       p%log = log
       where(p%pdf==0.) p%pdf=tiny(p%pdf)
    end if
    call normalize_pdf(p)
    call find_cdf(p)
    call check_pdf(p)    
  end subroutine set_pdf_cont_<T>

  subroutine check_pdf_discrete_<T>(p)
    implicit none
    type(pdf_discrete_<T>),intent(in) :: p
    if(p%n==0) stop "[check_pdf_discrete] PDF size not set"
    if(.not.allocated(p%pdf)) stop "[check_pdf_discrete] PDF pdf array not allocated"
    if(.not.allocated(p%cdf)) stop "[check_pdf_discrete] PDF cdf array not allocated"
    if(p%n.ne.size(p%pdf)) stop "[check_pdf_discrete] PDF pdf array has incorrect size"
    if(p%n.ne.size(p%cdf)) stop "[check_pdf_discrete] PDF cdf array has incorrect size"
    if(any(p%pdf < 0.)) stop "[check_pdf_discrete] PDF has negative probabilities"
    if(.not.p%normalized) stop "[check_pdf_discrete] PDF is not normalized"
  end subroutine check_pdf_discrete_<T>

  subroutine check_pdf_cont_<T>(p)
    implicit none
    type(pdf_<T>),intent(in) :: p
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
  end subroutine check_pdf_cont_<T>

  subroutine find_cdf_cont_<T>(p)
    implicit none
    type(pdf_<T>),intent(inout) :: p
    integer :: i
    call check_pdf(p)
    p%cdf(1) = tiny(p%cdf)
    do i=2,p%n
       if(p%log) then
          p%cdf(i) = p%cdf(i-1) + integral_loglog(p%x,p%pdf,p%x(i-1),p%x(i))
       else
          p%cdf(i) = p%cdf(i-1) + integral(p%x,p%pdf,p%x(i-1),p%x(i))
       end if
    end do
    p%cdf = p%cdf / p%cdf(p%n)
  end subroutine find_cdf_cont_<T>

  integer function sample_pdf_discrete_<T>(p)
    implicit none
    type(pdf_discrete_<T>) :: p
    integer :: j,jmin,jmax
    @T :: xi
    call random(xi)
    if(xi <= p%cdf(1)) then
       sample_pdf_discrete_<T> = 1
    else if(xi >= p%cdf(p%n)) then
       sample_pdf_discrete_<T> = p%n      
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
       sample_pdf_discrete_<T> = jmax
    end if
  end function sample_pdf_discrete_<T>

  @T function sample_pdf_cont_<T>(p,xi_alt)
    implicit none
    type(pdf_<T>),intent(in) :: p
    @T,optional,intent(in) :: xi_alt
    @T :: xi
    if(present(xi_alt)) then
       xi = xi_alt
    else
       call random(xi)
    end if
    if(xi <= p%cdf(1)) then
       sample_pdf_cont_<T> = p%x(1)
    else if(xi >= p%cdf(p%n)) then
       sample_pdf_cont_<T> = p%x(p%n)    
    else
       if(p%log) then
          sample_pdf_cont_<T> = interp1d_linlog(p%cdf(:),p%x(:),xi)
       else
          sample_pdf_cont_<T> = interp1d(p%cdf(:),p%x(:),xi)
       end if
    end if
  end function sample_pdf_cont_<T>

  @T function sample_pdf_cont_log_<T>(p,xi_alt)
    implicit none
    type(pdf_<T>),intent(in) :: p
    @T,optional,intent(in) :: xi_alt
    @T :: xi
    if(present(xi_alt)) then
       xi = xi_alt
    else
       call random(xi)
    end if
    sample_pdf_cont_log_<T> = interp1d_loglog(p%cdf(:),p%x(:),xi)
  end function sample_pdf_cont_log_<T>

  @T function interpolate_pdf_cont_<T>(p, x, bounds_error, fill_value) result(prob)
    implicit none
    type(pdf_<T>),intent(in) :: p
    @T,intent(in) :: x
    logical,intent(in),optional :: bounds_error
    real(<T>),intent(in),optional :: fill_value
    if(.not.p%normalized) stop "[interpolate_pdf] PDF is not normalized"
    if(p%log) then
       prob = interp1d_loglog(p%x, p%pdf, x, bounds_error, fill_value)
    else
       prob = interp1d(p%x, p%pdf, x, bounds_error, fill_value)
    end if
  end function interpolate_pdf_cont_<T>

  !!@END FOR

end module type_pdf

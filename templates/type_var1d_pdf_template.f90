module type_var1d_pdf

  use lib_array, only : locate
  use lib_random, only : random
  use type_pdf, only : pdf_sp, pdf_dp, sample_pdf, set_pdf

  implicit none
  save

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)


  ! The purpose of this class is to implement a 1-d PDF that depends on another variable that is provided when sampling. The two PDFs neighboring the value requested are then sampled with the same random number, and the resulting value is then interpolated.

  !!@FOR real(sp):sp real(<T>):dp

  public :: var1d_pdf_<T>
  type var1d_pdf_<T>
     integer :: nz
     real(<T>), allocatable :: z(:)
     type(pdf_<T>), allocatable :: p(:)
  end type var1d_pdf_<T>

  !!@END FOR

  public :: set_var1d_pdf
  interface set_var1d_pdf
     module procedure set_var1d_pdf_sp
     module procedure set_var1d_pdf_dp
  end interface set_var1d_pdf

  public :: sample_var1d_pdf
  interface sample_var1d_pdf
     module procedure sample_var1d_pdf_sp
     module procedure sample_var1d_pdf_dp
  end interface sample_var1d_pdf

contains

  !!@FOR real(sp):sp real(<T>):dp

  type(var1d_pdf_<T>) function set_var1d_pdf_<T>(x, z, prob) result(v)

    ! Initialize a var1d_pdf_<T> object
    !
    ! This version assumes that all the PDFs are defined on the same grid.
    ! We can easily create a version that has different x values for each
    ! PDF.
    !
    ! Parameters
    ! ----------
    ! x : 1-d array (size nx)
    !     x values in the PDFs
    ! z : 1-d array (size nz)
    !     Values that the PDFs are defined for
    ! prob : 2-d array (size nx, nz)
    !     The probabilities for all the x and z values

    implicit none
    real(<T>),intent(in) :: x(:), z(:), prob(:,:)
    integer :: k

    if(size(prob,1) /= size(x)) stop "incorrect dimensions for prob"
    if(size(prob,2) /= size(z)) stop "incorrect dimensions for prob"

    v%nz = size(z)

    allocate(v%z(v%nz))
    allocate(v%p(v%nz))

    v%z = z

    do k=1,v%nz
       call set_pdf(v%p(k), x, prob(:, k))
    end do

  end function set_var1d_pdf_<T>

  real(<T>) function sample_var1d_pdf_<T>(z, v) result(x)

    ! Sample a var1d_pdf_<T> object
    !
    ! Parameters
    ! ----------
    ! z : real(<T>)
    !     The z value to sample the PDFs for
    ! v : var1d_pdf_<T>
    !     The variable PDF to sample
    ! 
    ! Returns
    ! -------
    ! x : real(<T>)
    !     The sampled value

    real(<T>),intent(in) :: z
    type(var1d_pdf_<T>),intent(in) :: v
    real(<T>) :: x1, x2, xi
    integer :: iz

    ! Find bin in z array
    iz = locate(v%z, z)

    ! Sample random value
    call random(xi)

    ! Sample both PDFs
    x1 = sample_pdf(v%p(iz), xi_alt=xi)
    x2 = sample_pdf(v%p(iz+1), xi_alt=xi)

    ! Calculate result
    x = (z - v%z(iz)) / (v%z(iz+1) - v%z(iz)) * (x2 - x1) + x1

  end function sample_var1d_pdf_<T>

  !!@END FOR

end module type_var1d_pdf


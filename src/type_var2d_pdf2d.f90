! MD5 of template: f29ef24e5a466701d28eb0b497fa28fa
module type_var2d_pdf2d

  use lib_array, only : locate
  use lib_random, only : random
  use type_pdf2d, only : pdf2d_sp, pdf2d_dp, sample_pdf2d, set_pdf2d

  implicit none
  save

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  ! The purpose of this class is to implement a 2-d PDF that depends on two
  ! other variables that are provided when sampling. The four PDFs neighboring
  ! the value requested are then sampled with the same random number, and the
  ! resulting value is then interpolated using bilinear interpolation.


  public :: var2d_pdf2d_dp
  type var2d_pdf2d_dp
     integer :: nw, nz
     real(dp), allocatable :: w(:)
     real(dp), allocatable :: z(:)
     type(pdf2d_dp), allocatable :: p(:,:)
  end type var2d_pdf2d_dp


  public :: var2d_pdf2d_sp
  type var2d_pdf2d_sp
     integer :: nw, nz
     real(sp), allocatable :: w(:)
     real(sp), allocatable :: z(:)
     type(pdf2d_sp), allocatable :: p(:,:)
  end type var2d_pdf2d_sp


  public :: set_var2d_pdf2d
  interface set_var2d_pdf2d
     module procedure set_var2d_pdf2d_sp
     module procedure set_var2d_pdf2d_dp
  end interface set_var2d_pdf2d

  public :: sample_var2d_pdf2d
  interface sample_var2d_pdf2d
     module procedure sample_var2d_pdf2d_sp
     module procedure sample_var2d_pdf2d_dp
  end interface sample_var2d_pdf2d

contains


  type(var2d_pdf2d_dp) function set_var2d_pdf2d_dp(x, y, w, z, prob) result(v)

    ! Initialize a var2d_pdf2d_dp object
    !
    ! This version assumes that all the PDFs are defined on the same grid.
    ! We can easily create a version that has different x and y values for
    ! each PDF.
    !
    ! Parameters
    ! ----------
    ! x : 1-d array (size nx)
    !     x values in the PDFs
    ! y : 1-d array (size ny)
    !     y values in the PDFs
    ! w : 1-d array (size nw)
    !     First set of values that the PDFs are defined for
    ! z : 1-d array (size nz)
    !     Second set of values that the PDFs are defined for
    ! prob : 2-d array (size nx, ny, nw, nz)
    !     The probabilities for all the x, y, w, and z values

    implicit none
    real(dp),intent(in) :: x(:), y(:), w(:), z(:), prob(:,:,:,:)
    integer :: iw,iz

    if(size(prob,1) /= size(x)) stop "incorrect dimensions for prob"
    if(size(prob,2) /= size(y)) stop "incorrect dimensions for prob"
    if(size(prob,3) /= size(w)) stop "incorrect dimensions for prob"
    if(size(prob,4) /= size(z)) stop "incorrect dimensions for prob"

    v%nw = size(w)
    v%nz = size(z)

    allocate(v%w(v%nw))
    allocate(v%z(v%nz))
    allocate(v%p(v%nw, v%nz))

    v%w = w
    v%z = z

    do iw=1,v%nw
       do iz=1,v%nz
          v%p(iw,iz) = set_pdf2d(x, y, prob(:, :, iw, iz))
       end do
    end do

  end function set_var2d_pdf2d_dp

  subroutine sample_var2d_pdf2d_dp(w, z, v, x, y)

    ! Sample a var2d_pdf2d_dp object
    !
    ! Parameters
    ! ----------
    ! w, z : real(dp)
    !     The w and z value to sample the PDFs for
    ! v : var2d_pdf2d_dp
    !     The variable PDF to sample
    ! 
    ! Returns
    ! -------
    ! x, y : real(dp)
    !     The sampled values

    real(dp),intent(in) :: w, z
    type(var2d_pdf2d_dp),intent(in) :: v
    real(dp),intent(out) :: x, y
    real(dp) :: x11,x12,x21,x22,y11,y12,y21,y22,xi(4)
    integer :: iw, iz
    integer :: i

    ! Find bin in w and z arrays
    iw = locate(v%w, w)
    iz = locate(v%z, z)

    ! Sample random values
    do i=1,4
       call random(xi(i))
    end do

    ! Sample neighboring PDFs
    call sample_pdf2d(v%p(iw, iz), x11, y11, xi_alt=xi)
    call sample_pdf2d(v%p(iw+1, iz), x21, y21, xi_alt=xi)
    call sample_pdf2d(v%p(iw, iz+1), x12, y12, xi_alt=xi)
    call sample_pdf2d(v%p(iw+1, iz+1), x22, y22, xi_alt=xi)

    ! Calculate result using bilinear interpolation

    x = (x11 * (v%w(iw + 1) - w) * (v%z(iz + 1) - z) &
         &  + x21 * (w - v%w(iw)) * (v%z(iz + 1) - z) &
         &  + x12 * (v%w(iw + 1) - w) * (z - v%z(iz)) &
         &  + x22 * (w - v%w(iw)) * (z - v%z(iz))) &
         &  / (v%w(iw+1) - v%w(iw)) / (v%z(iz+1) - v%z(iz))

    y = (y11 * (v%w(iw + 1) - w) * (v%z(iz + 1) - z) &
         &  + y21 * (w - v%w(iw)) * (v%z(iz + 1) - z) &
         &  + y12 * (v%w(iw + 1) - w) * (z - v%z(iz)) &
         &  + y22 * (w - v%w(iw)) * (z - v%z(iz))) &
         &  / (v%w(iw+1) - v%w(iw)) / (v%z(iz+1) - v%z(iz))

  end subroutine sample_var2d_pdf2d_dp


  type(var2d_pdf2d_sp) function set_var2d_pdf2d_sp(x, y, w, z, prob) result(v)

    ! Initialize a var2d_pdf2d_sp object
    !
    ! This version assumes that all the PDFs are defined on the same grid.
    ! We can easily create a version that has different x and y values for
    ! each PDF.
    !
    ! Parameters
    ! ----------
    ! x : 1-d array (size nx)
    !     x values in the PDFs
    ! y : 1-d array (size ny)
    !     y values in the PDFs
    ! w : 1-d array (size nw)
    !     First set of values that the PDFs are defined for
    ! z : 1-d array (size nz)
    !     Second set of values that the PDFs are defined for
    ! prob : 2-d array (size nx, ny, nw, nz)
    !     The probabilities for all the x, y, w, and z values

    implicit none
    real(sp),intent(in) :: x(:), y(:), w(:), z(:), prob(:,:,:,:)
    integer :: iw,iz

    if(size(prob,1) /= size(x)) stop "incorrect dimensions for prob"
    if(size(prob,2) /= size(y)) stop "incorrect dimensions for prob"
    if(size(prob,3) /= size(w)) stop "incorrect dimensions for prob"
    if(size(prob,4) /= size(z)) stop "incorrect dimensions for prob"

    v%nw = size(w)
    v%nz = size(z)

    allocate(v%w(v%nw))
    allocate(v%z(v%nz))
    allocate(v%p(v%nw, v%nz))

    v%w = w
    v%z = z

    do iw=1,v%nw
       do iz=1,v%nz
          v%p(iw,iz) = set_pdf2d(x, y, prob(:, :, iw, iz))
       end do
    end do

  end function set_var2d_pdf2d_sp

  subroutine sample_var2d_pdf2d_sp(w, z, v, x, y)

    ! Sample a var2d_pdf2d_sp object
    !
    ! Parameters
    ! ----------
    ! w, z : real(sp)
    !     The w and z value to sample the PDFs for
    ! v : var2d_pdf2d_sp
    !     The variable PDF to sample
    ! 
    ! Returns
    ! -------
    ! x, y : real(sp)
    !     The sampled values

    real(sp),intent(in) :: w, z
    type(var2d_pdf2d_sp),intent(in) :: v
    real(sp),intent(out) :: x, y
    real(sp) :: x11,x12,x21,x22,y11,y12,y21,y22,xi(4)
    integer :: iw, iz
    integer :: i

    ! Find bin in w and z arrays
    iw = locate(v%w, w)
    iz = locate(v%z, z)

    ! Sample random values
    do i=1,4
       call random(xi(i))
    end do

    ! Sample neighboring PDFs
    call sample_pdf2d(v%p(iw, iz), x11, y11, xi_alt=xi)
    call sample_pdf2d(v%p(iw+1, iz), x21, y21, xi_alt=xi)
    call sample_pdf2d(v%p(iw, iz+1), x12, y12, xi_alt=xi)
    call sample_pdf2d(v%p(iw+1, iz+1), x22, y22, xi_alt=xi)

    ! Calculate result using bilinear interpolation

    x = (x11 * (v%w(iw + 1) - w) * (v%z(iz + 1) - z) &
         &  + x21 * (w - v%w(iw)) * (v%z(iz + 1) - z) &
         &  + x12 * (v%w(iw + 1) - w) * (z - v%z(iz)) &
         &  + x22 * (w - v%w(iw)) * (z - v%z(iz))) &
         &  / (v%w(iw+1) - v%w(iw)) / (v%z(iz+1) - v%z(iz))

    y = (y11 * (v%w(iw + 1) - w) * (v%z(iz + 1) - z) &
         &  + y21 * (w - v%w(iw)) * (v%z(iz + 1) - z) &
         &  + y12 * (v%w(iw + 1) - w) * (z - v%z(iz)) &
         &  + y22 * (w - v%w(iw)) * (z - v%z(iz))) &
         &  / (v%w(iw+1) - v%w(iw)) / (v%z(iz+1) - v%z(iz))

  end subroutine sample_var2d_pdf2d_sp


end module type_var2d_pdf2d


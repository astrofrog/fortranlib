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

module type_var2d_pdf2d

  use lib_array, only : locate, interp2d
  use lib_random, only : random
  use type_pdf2d, only : pdf2d_sp, pdf2d_dp, sample_pdf2d, set_pdf2d, interpolate_pdf2d

  implicit none
  save

  private

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  ! The purpose of this class is to implement a 2-d PDF that depends on two
  ! other variables that are provided when sampling. The four PDFs neighboring
  ! the value requested are then sampled with the same random number, and the
  ! resulting value is then interpolated using bilinear interpolation.

  !!@FOR real(sp):sp real(<T>):dp

  public :: var2d_pdf2d_<T>
  type var2d_pdf2d_<T>
     integer :: nw, nz
     real(<T>), allocatable :: w(:)
     real(<T>), allocatable :: z(:)
     type(pdf2d_<T>), allocatable :: p(:,:)
  end type var2d_pdf2d_<T>

  !!@END FOR

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

  public :: interpolate_var2d_pdf2d
  interface interpolate_var2d_pdf2d
     module procedure interpolate_var2d_pdf2d_sp
     module procedure interpolate_var2d_pdf2d_dp
  end interface interpolate_var2d_pdf2d

contains

  !!@FOR real(sp):sp real(<T>):dp

  type(var2d_pdf2d_<T>) function set_var2d_pdf2d_<T>(x, y, w, z, prob) result(v)

    ! Initialize a var2d_pdf2d_<T> object
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
    real(<T>),intent(in) :: x(:), y(:), w(:), z(:), prob(:,:,:,:)
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

  end function set_var2d_pdf2d_<T>

  subroutine sample_var2d_pdf2d_<T>(w, z, v, x, y)

    ! Sample a var2d_pdf2d_<T> object
    !
    ! Parameters
    ! ----------
    ! w, z : @T
    !     The w and z value to sample the PDFs for
    ! v : var2d_pdf2d_<T>
    !     The variable PDF to sample
    !
    ! Returns
    ! -------
    ! x, y : @T
    !     The sampled values

    real(<T>),intent(in) :: w, z
    type(var2d_pdf2d_<T>),intent(in) :: v
    real(<T>),intent(out) :: x, y
    real(<T>) :: x11,x12,x21,x22,y11,y12,y21,y22,xi(4)
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

  end subroutine sample_var2d_pdf2d_<T>


  @T function interpolate_var2d_pdf2d_<T>(w, z, v, x, y, bounds_error, fill_value) result(prob)

    ! Interpolate a 2-d PDF
    !
    ! Parameters
    ! ----------
    ! w, z : @T
    !     The w and z value to sample the PDFs for
    ! v : var2d_pdf2d_<T>
    !     The variable PDF to interpolate
    ! x, y : @T
    !     Position at which to interpolate the 2-d PDF
    ! bounds_error : logical, optional
    !     Whether to raise an error if the interpolation is out of bounds
    ! fill_value : @T
    !     The value to use for out-of-bounds interpolation if bounds_error = .false.
    !
    ! Returns
    ! -------
    ! prob : @T
    !     The probability at the position requested

    implicit none

    real(<T>),intent(in) :: w, z
    type(var2d_pdf2d_<T>),intent(in) :: v
    @T,intent(in) :: x, y
    logical,intent(in),optional :: bounds_error
    real(<T>),intent(in),optional :: fill_value

    @T :: p11,p12,p21,p22
    integer :: iw, iz

    ! Find bin in w and z arrays
    iw = locate(v%w, w)
    iz = locate(v%z, z)

    ! Interpolate neighboring PDFs
    p11 = interpolate_pdf2d(v%p(iw, iz), x, y, bounds_error, fill_value)
    p21 = interpolate_pdf2d(v%p(iw+1, iz), x, y, bounds_error, fill_value)
    p12 = interpolate_pdf2d(v%p(iw, iz+1), x, y, bounds_error, fill_value)
    p22 = interpolate_pdf2d(v%p(iw+1, iz+1), x, y, bounds_error, fill_value)

    ! Calculate result using bilinear interpolation

    prob = (p11 * (v%w(iw + 1) - w) * (v%z(iz + 1) - z) &
         &  + p21 * (w - v%w(iw)) * (v%z(iz + 1) - z) &
         &  + p12 * (v%w(iw + 1) - w) * (z - v%z(iz)) &
         &  + p22 * (w - v%w(iw)) * (z - v%z(iz))) &
         &  / (v%w(iw+1) - v%w(iw)) / (v%z(iz+1) - v%z(iz))

  end function interpolate_var2d_pdf2d_<T>

  !!@END FOR

end module type_var2d_pdf2d


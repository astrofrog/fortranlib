! MD5 of template: f092f5ca93d239079bc43db4fba9bf60
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


  public :: var1d_pdf_dp
  type var1d_pdf_dp
     integer :: nz
     real(dp), allocatable :: z(:)
     type(pdf_dp), allocatable :: p(:)
  end type var1d_pdf_dp


  public :: var1d_pdf_sp
  type var1d_pdf_sp
     integer :: nz
     real(sp), allocatable :: z(:)
     type(pdf_sp), allocatable :: p(:)
  end type var1d_pdf_sp


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


  type(var1d_pdf_dp) function set_var1d_pdf_dp(x, z, prob) result(v)

    ! Initialize a var1d_pdf_dp object
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
    real(dp),intent(in) :: x(:), z(:), prob(:,:)
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

  end function set_var1d_pdf_dp

  real(dp) function sample_var1d_pdf_dp(z, v) result(x)

    ! Sample a var1d_pdf_dp object
    !
    ! Parameters
    ! ----------
    ! z : real(dp)
    !     The z value to sample the PDFs for
    ! v : var1d_pdf_dp
    !     The variable PDF to sample
    !
    ! Returns
    ! -------
    ! x : real(dp)
    !     The sampled value

    real(dp),intent(in) :: z
    type(var1d_pdf_dp),intent(in) :: v
    real(dp) :: x1, x2, xi
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

  end function sample_var1d_pdf_dp


  type(var1d_pdf_sp) function set_var1d_pdf_sp(x, z, prob) result(v)

    ! Initialize a var1d_pdf_sp object
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
    real(sp),intent(in) :: x(:), z(:), prob(:,:)
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

  end function set_var1d_pdf_sp

  real(sp) function sample_var1d_pdf_sp(z, v) result(x)

    ! Sample a var1d_pdf_sp object
    !
    ! Parameters
    ! ----------
    ! z : real(sp)
    !     The z value to sample the PDFs for
    ! v : var1d_pdf_sp
    !     The variable PDF to sample
    !
    ! Returns
    ! -------
    ! x : real(sp)
    !     The sampled value

    real(sp),intent(in) :: z
    type(var1d_pdf_sp),intent(in) :: v
    real(sp) :: x1, x2, xi
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

  end function sample_var1d_pdf_sp


end module type_var1d_pdf


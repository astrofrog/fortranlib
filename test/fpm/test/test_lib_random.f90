module test_lib_random_suite

  use testdrive, only : new_unittest, unittest_type, error_type, check
  use lib_random
  implicit none
  private
  public :: collect_lib_random

  integer, parameter :: dp = selected_real_kind(p=15, r=307)
  integer, parameter :: N = 200000

contains

  subroutine collect_lib_random(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("uniform_range", test_uniform), &
      new_unittest("gaussian_moments", test_gaussian), &
      new_unittest("sphere", test_sphere), &
      new_unittest("exponential", test_exponential), &
      new_unittest("poisson_mean", test_poisson) ]
  end subroutine collect_lib_random

  subroutine test_uniform(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: xi, s
    integer :: i
    call set_seed(1)
    s = 0._dp
    do i = 1, N
       call random(xi)
       call check(error, xi >= 0._dp .and. xi < 1._dp, "random outside [0,1)")
       if (allocated(error)) return
       s = s + xi
    end do
    call check(error, s / N, 0.5_dp, thr=0.01_dp)                 ! mean of U(0,1)
  end subroutine test_uniform

  ! mean -> c, std -> w
  subroutine test_gaussian(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: c = 3._dp, w = 2._dp
    real(dp) :: xi, s, s2
    integer :: i
    call set_seed(2)
    s = 0._dp; s2 = 0._dp
    do i = 1, N
       call random_gau(xi, c, w)
       s = s + xi; s2 = s2 + xi*xi
    end do
    call check(error, s / N, c, thr=0.05_dp);                     if (allocated(error)) return
    call check(error, sqrt(s2/N - (s/N)**2), w, thr=0.05_dp)      ! std dev
  end subroutine test_gaussian

  ! mu in [-1,1], phi in [0,2pi), <mu> -> 0
  subroutine test_sphere(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: mu, phi, smu
    integer :: i
    call set_seed(3)
    smu = 0._dp
    do i = 1, N
       call random_sphere(mu, phi)
       call check(error, mu >= -1._dp .and. mu <= 1._dp, "mu out of [-1,1]")
       if (allocated(error)) return
       call check(error, phi >= 0._dp .and. phi < 6.2831853_dp, "phi out of [0,2pi)")
       if (allocated(error)) return
       smu = smu + mu
    end do
    call check(error, smu / N, 0._dp, thr=0.01_dp)
  end subroutine test_sphere

  ! exponential: non-negative, mean -> 1
  subroutine test_exponential(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: xi, s
    integer :: i
    call set_seed(4)
    s = 0._dp
    do i = 1, N
       call random_exp(xi)
       call check(error, xi >= 0._dp, "random_exp negative");  if (allocated(error)) return
       s = s + xi
    end do
    call check(error, s / N, 1._dp, thr=0.02_dp)
  end subroutine test_exponential

  ! Poisson: mean -> xm
  subroutine test_poisson(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp), parameter :: xm = 4._dp
    real(dp) :: xi, s
    integer :: i
    call set_seed(5)
    s = 0._dp
    do i = 1, N
       call random_poisson(xi, xm)
       s = s + xi
    end do
    call check(error, s / N, xm, thr=0.05_dp)
  end subroutine test_poisson

end module test_lib_random_suite

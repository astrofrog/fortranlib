module test_lib_algebra_suite

  use testdrive, only : new_unittest, unittest_type, error_type, check
  use lib_algebra
  implicit none
  private
  public :: collect_lib_algebra

  integer, parameter :: dp = selected_real_kind(p=15, r=307)
  real(dp), parameter :: tol = 1.e-12_dp

contains

  subroutine collect_lib_algebra(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("cbrt", test_cbrt), &
      new_unittest("quadratic", test_quadratic), &
      new_unittest("lineq_gausselim", test_lineq) ]
  end subroutine collect_lib_algebra

  subroutine test_cbrt(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, cbrt(27._dp),  3._dp, thr=tol);  if (allocated(error)) return
    call check(error, cbrt(-8._dp), -2._dp, thr=tol);  if (allocated(error)) return
    call check(error, cbrt(0._dp),   0._dp, thr=tol)
  end subroutine test_cbrt

  ! roots returned smallest-first: x1 <= x2
  subroutine test_quadratic(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: x1, x2
    call quadratic(1._dp, -3._dp, 2._dp, x1, x2)       ! x^2 - 3x + 2 -> 1, 2
    call check(error, x1, 1._dp, thr=tol);  if (allocated(error)) return
    call check(error, x2, 2._dp, thr=tol);  if (allocated(error)) return
    call quadratic_reduced(-5._dp, 6._dp, x1, x2)      ! x^2 - 5x + 6 -> 2, 3
    call check(error, x1, 2._dp, thr=tol);  if (allocated(error)) return
    call check(error, x2, 3._dp, thr=tol)
  end subroutine test_quadratic

  ! solve a.x = b in place; 2x+y=3, x+3y=5 -> x=0.8, y=1.4
  subroutine test_lineq(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: a(2,2), b(2)
    a = reshape([2._dp, 1._dp, 1._dp, 3._dp], [2,2])   ! rows: (2,1) and (1,3)
    b = [3._dp, 5._dp]
    call lineq_gausselim(a, b)
    call check(error, b(1), 0.8_dp, thr=1.e-12_dp);  if (allocated(error)) return
    call check(error, b(2), 1.4_dp, thr=1.e-12_dp)
  end subroutine test_lineq

end module test_lib_algebra_suite

module test_type_stokes_suite

  use testdrive, only : new_unittest, unittest_type, error_type, check
  use type_stokes
  implicit none
  private
  public :: collect_type_stokes

  integer, parameter :: dp = selected_real_kind(p=15, r=307)
  real(dp), parameter :: tol = 1.e-12_dp

contains

  subroutine collect_type_stokes(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("arithmetic", test_arithmetic) ]
  end subroutine collect_type_stokes

  ! stokes fields are (I, U, Q, V); +, -, * act component-wise
  subroutine test_arithmetic(error)
    type(error_type), allocatable, intent(out) :: error
    type(stokes_dp) :: a, b, c
    a = stokes_dp(1._dp, 2._dp, 3._dp, 4._dp)
    b = stokes_dp(0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp)
    c = a + b
    call check(error, c%i, 1.5_dp, thr=tol);  if (allocated(error)) return
    call check(error, c%v, 4.5_dp, thr=tol);  if (allocated(error)) return
    c = a - b
    call check(error, c%q, 2.5_dp, thr=tol);  if (allocated(error)) return
    c = a * 2._dp
    call check(error, c%i, 2._dp, thr=tol);  if (allocated(error)) return
    call check(error, c%u, 4._dp, thr=tol)
  end subroutine test_arithmetic

end module test_type_stokes_suite

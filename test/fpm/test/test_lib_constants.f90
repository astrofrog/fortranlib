module test_lib_constants_suite

  use testdrive, only : new_unittest, unittest_type, error_type, check
  use lib_constants
  implicit none
  private
  public :: collect_lib_constants

  integer, parameter :: dp = selected_real_kind(p=15, r=307)
  real(dp), parameter :: tol = 1.e-12_dp

contains

  subroutine collect_lib_constants(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("angles", test_angles) ]
  end subroutine collect_lib_constants

  subroutine test_angles(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, pi, 3.141592653589793_dp, thr=1.e-14_dp);  if (allocated(error)) return
    call check(error, twopi, 2._dp * pi, thr=tol);               if (allocated(error)) return
    call check(error, deg2rad, pi / 180._dp, thr=tol);           if (allocated(error)) return
    call check(error, rad2deg, 180._dp / pi, thr=tol);           if (allocated(error)) return
    call check(error, deg2rad * rad2deg, 1._dp, thr=tol);        if (allocated(error)) return
    call check(error, stef_boltz > 0._dp, "stef_boltz not positive")
  end subroutine test_angles

end module test_lib_constants_suite

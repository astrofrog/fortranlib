module test_type_vector3d_suite

  use testdrive, only : new_unittest, unittest_type, error_type, check
  use type_vector3d
  use type_angle3d, only : angle3d_dp
  implicit none
  private
  public :: collect_type_vector3d

  integer, parameter :: dp = selected_real_kind(p=15, r=307)
  real(dp), parameter :: tol = 1.e-12_dp

contains

  subroutine collect_type_vector3d(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("dot_cross_add", test_dot_cross_add), &
      new_unittest("angle_roundtrip", test_roundtrip) ]
  end subroutine collect_type_vector3d

  subroutine test_dot_cross_add(error)
    type(error_type), allocatable, intent(out) :: error
    type(vector3d_dp) :: x, y, z, s
    x = vector3d_dp(1._dp, 0._dp, 0._dp)
    y = vector3d_dp(0._dp, 1._dp, 0._dp)
    call check(error, x .dot. y, 0._dp, thr=tol);  if (allocated(error)) return   ! perpendicular
    call check(error, x .dot. x, 1._dp, thr=tol);  if (allocated(error)) return
    z = x .cross. y                                                               ! -> +z
    call check(error, z%x, 0._dp, thr=tol);  if (allocated(error)) return
    call check(error, z%y, 0._dp, thr=tol);  if (allocated(error)) return
    call check(error, z%z, 1._dp, thr=tol);  if (allocated(error)) return
    s = x + y
    call check(error, s%x, 1._dp, thr=tol);  if (allocated(error)) return
    call check(error, s%y, 1._dp, thr=tol)
  end subroutine test_dot_cross_add

  ! vector -> angle -> vector returns the same (unit) direction
  subroutine test_roundtrip(error)
    type(error_type), allocatable, intent(out) :: error
    type(vector3d_dp) :: v, w
    type(angle3d_dp)  :: a
    v = vector3d_dp(0.6_dp, 0._dp, 0.8_dp)   ! unit vector, off the pole (phi defined)
    call vector3d_to_angle3d(v, a)
    call angle3d_to_vector3d(a, w)
    call check(error, w%x, 0.6_dp, thr=1.e-12_dp);  if (allocated(error)) return
    call check(error, w%y, 0._dp,  thr=1.e-12_dp);  if (allocated(error)) return
    call check(error, w%z, 0.8_dp, thr=1.e-12_dp)
  end subroutine test_roundtrip

end module test_type_vector3d_suite

module test_type_angle3d_suite

  use testdrive, only : new_unittest, unittest_type, error_type, check
  use type_angle3d
  implicit none
  private
  public :: collect_type_angle3d

  integer, parameter :: dp = selected_real_kind(p=15, r=307)
  real(dp), parameter :: tol = 1.e-12_dp

contains

  subroutine collect_type_angle3d(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("from_degrees", test_from_degrees), &
      new_unittest("dot_product", test_dot) ]
  end subroutine collect_type_angle3d

  ! theta measured from +z; angle3d_deg(90,0) points along +x
  subroutine test_from_degrees(error)
    type(error_type), allocatable, intent(out) :: error
    type(angle3d_dp) :: a
    a = angle3d_deg(90._dp, 0._dp)
    call check(error, a%cost, 0._dp, thr=tol);  if (allocated(error)) return
    call check(error, a%sint, 1._dp, thr=tol);  if (allocated(error)) return
    call check(error, a%cosp, 1._dp, thr=tol);  if (allocated(error)) return
    call check(error, a%sinp, 0._dp, thr=tol)
  end subroutine test_from_degrees

  ! a1 .dot. a2 is the cosine of the angle between the two directions
  subroutine test_dot(error)
    type(error_type), allocatable, intent(out) :: error
    type(angle3d_dp) :: zaxis, xaxis
    zaxis = angle3d_deg(0._dp, 0._dp)     ! +z
    xaxis = angle3d_deg(90._dp, 0._dp)    ! +x
    call check(error, zaxis .dot. zaxis, 1._dp, thr=tol);  if (allocated(error)) return
    call check(error, zaxis .dot. xaxis, 0._dp, thr=tol)                  ! perpendicular
  end subroutine test_dot

end module test_type_angle3d_suite

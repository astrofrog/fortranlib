module test_lib_statistics_suite

  use testdrive, only : new_unittest, unittest_type, error_type, check
  use lib_statistics
  implicit none
  private
  public :: collect_lib_statistics

  integer, parameter :: dp = selected_real_kind(p=15, r=307)
  real(dp), parameter :: tol = 1.e-10_dp

contains

  subroutine collect_lib_statistics(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("mean_median", test_mean_median), &
      new_unittest("variance", test_variance), &
      new_unittest("quantile", test_quantile), &
      new_unittest("clipped_mean", test_clipped_mean) ]
  end subroutine collect_lib_statistics

  subroutine test_mean_median(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: x(5) = [1._dp, 2._dp, 3._dp, 4._dp, 5._dp]
    logical  :: m(5) = [.true., .true., .true., .false., .false.]
    call check(error, mean(x), 3._dp, thr=tol);         if (allocated(error)) return
    call check(error, median(x), 3._dp, thr=tol);       if (allocated(error)) return
    call check(error, mean(x, mask=m), 2._dp, thr=tol)               ! mean of {1,2,3}
  end subroutine test_mean_median

  ! sample variance = sum((x-mean)^2)/(n-1); regression for the parenthesization fix
  subroutine test_variance(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: x(5) = [1._dp, 2._dp, 3._dp, 4._dp, 5._dp]
    real(dp) :: c(4) = [7._dp, 7._dp, 7._dp, 7._dp]
    call check(error, variance(x), 2.5_dp, thr=tol);  if (allocated(error)) return
    call check(error, variance(c), 0._dp, thr=tol)                   ! constant -> 0
  end subroutine test_variance

  ! percent is on a 0..100 scale
  subroutine test_quantile(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: x(5) = [5._dp, 1._dp, 4._dp, 2._dp, 3._dp]   ! unsorted on purpose
    call check(error, quantile(x, 0._dp),   1._dp, thr=tol);  if (allocated(error)) return
    call check(error, quantile(x, 100._dp), 5._dp, thr=tol);  if (allocated(error)) return
    call check(error, quantile(x, 50._dp),  3._dp, thr=tol)
  end subroutine test_quantile

  subroutine test_clipped_mean(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: x(5) = [1._dp, 2._dp, 3._dp, 4._dp, 5._dp]
    call check(error, clipped_mean(x, 3), 3._dp, thr=tol)            ! symmetric, no clipping
  end subroutine test_clipped_mean

end module test_lib_statistics_suite

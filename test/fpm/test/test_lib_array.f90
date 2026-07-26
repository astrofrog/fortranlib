module test_lib_array_suite

  use testdrive, only : new_unittest, unittest_type, error_type, check
  use lib_array
  implicit none
  private
  public :: collect_lib_array

  integer, parameter :: dp = selected_real_kind(p=15, r=307)
  real(dp), parameter :: tol = 1.e-10_dp

contains

  subroutine collect_lib_array(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("linspace_logspace", test_linspace_logspace), &
      new_unittest("ipos", test_ipos), &
      new_unittest("ipos_monotonic", test_ipos_monotonic), &
      new_unittest("xval", test_xval), &
      new_unittest("locate", test_locate), &
      new_unittest("interp1d", test_interp1d), &
      new_unittest("integral", test_integral), &
      new_unittest("cumulative_integral", test_cumulative_integral), &
      new_unittest("histogram1d", test_histogram1d), &
      new_unittest("quicksort", test_quicksort) ]
  end subroutine collect_lib_array

  subroutine test_linspace_logspace(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: x(5)
    call linspace(0._dp, 8._dp, x)
    call check(error, x(1), 0._dp, thr=tol);  if (allocated(error)) return
    call check(error, x(5), 8._dp, thr=tol);  if (allocated(error)) return
    call check(error, x(3), 4._dp, thr=tol);  if (allocated(error)) return   ! evenly spaced
    call logspace(1._dp, 10000._dp, x)                                     ! -> 1,10,100,1000,10000
    call check(error, x(1), 1._dp, thr=tol);         if (allocated(error)) return
    call check(error, x(5), 10000._dp, thr=1.e-6_dp); if (allocated(error)) return
    call check(error, x(3), 100._dp, thr=1.e-7_dp)                          ! log-even -> 10^2
  end subroutine test_linspace_logspace

  ! ipos: bin lookup, both ascending and descending ranges
  subroutine test_ipos(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, ipos(0._dp, 10._dp,  0._dp, 10), 1);  if (allocated(error)) return
    call check(error, ipos(0._dp, 10._dp, 10._dp, 10), 10); if (allocated(error)) return
    call check(error, ipos(0._dp, 10._dp,  5._dp, 10), 6);  if (allocated(error)) return
    call check(error, ipos(0._dp, 10._dp, -1._dp, 10), 0);  if (allocated(error)) return
    call check(error, ipos(0._dp, 10._dp, 11._dp, 10), 11); if (allocated(error)) return
    ! descending must mirror ascending (regression: reverse-range fix)
    call check(error, ipos(10._dp, 0._dp, 10._dp, 10), 1);  if (allocated(error)) return
    call check(error, ipos(10._dp, 0._dp,  0._dp, 10), 10); if (allocated(error)) return
    call check(error, ipos(10._dp, 0._dp, 11._dp, 10), 0);  if (allocated(error)) return
    call check(error, ipos(10._dp, 0._dp, -1._dp, 10), 11)
  end subroutine test_ipos

  ! sweeping x across the axis, ipos never decreases (caught the reverse-range bug)
  subroutine test_ipos_monotonic(error)
    type(error_type), allocatable, intent(out) :: error
    call sweep(error, 0._dp, 10._dp, 13);  if (allocated(error)) return
    call sweep(error, 10._dp, 0._dp, 13);  if (allocated(error)) return
    call sweep(error, 3._dp, -7._dp, 13)
  end subroutine test_ipos_monotonic

  subroutine sweep(error, a, b, n)
    type(error_type), allocatable, intent(out) :: error
    real(dp), intent(in) :: a, b
    integer, intent(in) :: n
    integer :: k, prev, cur
    real(dp) :: t, x
    prev = -1
    do k = 0, 1000
       t = -0.2_dp + 1.4_dp * real(k, dp) / 1000._dp
       x = a + t * (b - a)
       cur = ipos(a, b, x, n)
       call check(error, cur >= prev, "ipos decreased along the sweep")
       if (allocated(error)) return
       prev = cur
    end do
  end subroutine sweep

  ! xval: centre of a bin. round-trips with ipos.
  subroutine test_xval(error)
    type(error_type), allocatable, intent(out) :: error
    integer :: i
    call check(error, xval(0._dp, 10._dp, 1, 10), 0.5_dp, thr=tol);  if (allocated(error)) return
    call check(error, xval(0._dp, 10._dp, 10, 10), 9.5_dp, thr=tol); if (allocated(error)) return
    do i = 1, 10
       call check(error, ipos(0._dp, 10._dp, xval(0._dp, 10._dp, i, 10), 10), i)
       if (allocated(error)) return
    end do
  end subroutine test_xval

  ! locate: index i with xx(i) <= x < xx(i+1)
  subroutine test_locate(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: xx(5) = [1._dp, 2._dp, 3._dp, 4._dp, 5._dp]
    call check(error, locate(xx, 2.5_dp), 2);  if (allocated(error)) return
    call check(error, locate(xx, 1.0_dp), 1);  if (allocated(error)) return
    call check(error, locate(xx, 4.9_dp), 4)
  end subroutine test_locate

  ! interp1d: exact for a linear function
  subroutine test_interp1d(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: x(5) = [0._dp,1._dp,2._dp,3._dp,4._dp], y(5)
    y = 2._dp * x - 1._dp
    call check(error, interp1d(x, y, 0.5_dp), 0._dp, thr=tol);  if (allocated(error)) return
    call check(error, interp1d(x, y, 2.5_dp), 4._dp, thr=tol);  if (allocated(error)) return
    call check(error, interp1d(x, y, 3.0_dp), 5._dp, thr=tol)
  end subroutine test_interp1d

  ! integral: trapezoidal is exact for linear; loglog exact for power laws
  subroutine test_integral(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: x(5) = [1._dp,2._dp,3._dp,4._dp,5._dp], y(5)
    y = 2._dp * x - 1._dp                       ! int over [1,5] = [x^2-x] = 20
    call check(error, integral(x, y), 20._dp, thr=tol);  if (allocated(error)) return
    y = 4._dp * x**3                            ! int over [1,5] = [x^4] = 624
    call check(error, integral_loglog(x, y), 624._dp, thr=1.e-8_dp)
  end subroutine test_integral

  ! cumulative_integral: last element equals the total integral
  subroutine test_cumulative_integral(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: x(5) = [1._dp,2._dp,3._dp,4._dp,5._dp], y(5), c(5)
    y = 2._dp * x - 1._dp
    c = cumulative_integral(x, y)
    call check(error, c(1), 0._dp, thr=tol);      if (allocated(error)) return
    call check(error, c(5), 20._dp, thr=tol)                       ! total = integral
  end subroutine test_cumulative_integral

  ! histogram1d: counts land in the right bins and sum to N
  subroutine test_histogram1d(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: a(6) = [0.5_dp, 1.5_dp, 1.6_dp, 2.5_dp, 2.6_dp, 2.7_dp]
    real(dp) :: hx(3), hy(3)
    call histogram1d(a, 0._dp, 3._dp, 3, hx, hy)
    call check(error, nint(sum(hy)), 6);         if (allocated(error)) return
    call check(error, nint(hy(1)), 1);           if (allocated(error)) return   ! [0,1)
    call check(error, nint(hy(2)), 2);           if (allocated(error)) return   ! [1,2)
    call check(error, nint(hy(3)), 3)                                           ! [2,3)
  end subroutine test_histogram1d

  ! quicksort: output is sorted ascending, same multiset
  subroutine test_quicksort(error)
    type(error_type), allocatable, intent(out) :: error
    real(dp) :: a(7) = [3._dp, 1._dp, 4._dp, 1._dp, 5._dp, 9._dp, 2._dp]
    integer :: i
    call quicksort(a)
    call check(error, nint(sum(a)), 25);  if (allocated(error)) return
    do i = 1, size(a) - 1
       call check(error, a(i) <= a(i+1), "quicksort output not ascending")
       if (allocated(error)) return
    end do
  end subroutine test_quicksort

end module test_lib_array_suite

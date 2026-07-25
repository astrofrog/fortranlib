module test_type_pdf_suite

  use testdrive, only : new_unittest, unittest_type, error_type, check
  use type_pdf
  use lib_random, only : set_seed
  implicit none
  private
  public :: collect_type_pdf

  integer, parameter :: dp = selected_real_kind(p=15, r=307)
  real(dp), parameter :: tol = 1.e-10_dp

contains

  subroutine collect_type_pdf(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("continuous_uniform", test_continuous_uniform), &
      new_unittest("sample_log_first_bin", test_sample_log_first_bin) ]
  end subroutine collect_type_pdf

  ! Build a uniform PDF on [0,1]; check normalization, CDF ends, interpolation,
  ! and deterministic inverse-CDF sampling (set_pdf drives allocate/normalize/
  ! find_cdf/check_pdf; sample_pdf with xi_alt inverts the CDF without the RNG).
  subroutine test_continuous_uniform(error)
    type(error_type), allocatable, intent(out) :: error
    type(pdf_dp) :: p
    call set_pdf(p, [0._dp, 1._dp], [1._dp, 1._dp])
    call check(error, p%cdf(1), 0._dp, thr=tol);     if (allocated(error)) return
    call check(error, p%cdf(p%n), 1._dp, thr=tol);   if (allocated(error)) return
    call check(error, interpolate_pdf(p, 0.5_dp), 1._dp, thr=tol);  if (allocated(error)) return
    ! uniform CDF is the identity, so inverse-CDF sampling returns xi itself
    call check(error, sample_pdf(p, xi_alt=0.3_dp), 0.3_dp, thr=tol);  if (allocated(error)) return
    call check(error, sample_pdf(p, xi_alt=0.0_dp), 0.0_dp, thr=tol);  if (allocated(error)) return
    call check(error, sample_pdf(p, xi_alt=1.0_dp), 1.0_dp, thr=tol)
  end subroutine test_continuous_uniform

  ! Regression: a random number in the first CDF bin must not yield NaN
  ! (log-log interpolation previously fed cdf(1)=0 into a log).
  subroutine test_sample_log_first_bin(error)
    type(error_type), allocatable, intent(out) :: error
    type(pdf_dp) :: p
    real(dp) :: s
    call set_pdf(p, [1._dp, 2._dp, 4._dp, 8._dp], [1._dp, 1._dp, 1._dp, 1._dp], log=.true.)
    s = sample_pdf_log(p, xi_alt=0._dp)   ! first bin -> used to be NaN
    call check(error, s, 1._dp, thr=1.e-8_dp);  if (allocated(error)) return   ! -> x(1)
    s = sample_pdf_log(p, xi_alt=1._dp)
    call check(error, s, 8._dp, thr=1.e-8_dp)                                  ! -> x(n)
  end subroutine test_sample_log_first_bin

end module test_type_pdf_suite

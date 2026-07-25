program tester

  use, intrinsic :: iso_fortran_env, only : error_unit
  use testdrive, only : run_testsuite, new_testsuite, testsuite_type
  use test_lib_array_suite,      only : collect_lib_array
  use test_lib_statistics_suite, only : collect_lib_statistics
  use test_lib_algebra_suite,    only : collect_lib_algebra
  use test_lib_random_suite,     only : collect_lib_random
  use test_lib_constants_suite,  only : collect_lib_constants
  use test_type_stokes_suite,    only : collect_type_stokes
  use test_type_angle3d_suite,   only : collect_type_angle3d
  use test_type_vector3d_suite,  only : collect_type_vector3d
  use test_type_pdf_suite,       only : collect_type_pdf
  implicit none

  integer :: stat, is
  type(testsuite_type), allocatable :: testsuites(:)

  stat = 0
  testsuites = [ &
    new_testsuite("lib_array",      collect_lib_array), &
    new_testsuite("lib_statistics", collect_lib_statistics), &
    new_testsuite("lib_algebra",    collect_lib_algebra), &
    new_testsuite("lib_random",     collect_lib_random), &
    new_testsuite("lib_constants",  collect_lib_constants), &
    new_testsuite("type_stokes",    collect_type_stokes), &
    new_testsuite("type_angle3d",   collect_type_angle3d), &
    new_testsuite("type_vector3d",  collect_type_vector3d), &
    new_testsuite("type_pdf",       collect_type_pdf) ]

  do is = 1, size(testsuites)
     write(error_unit, '(a)') "Suite: " // testsuites(is)%name
     call run_testsuite(testsuites(is)%collect, error_unit, stat)
  end do

  if (stat > 0) then
     write(error_unit, '(i0, a)') stat, " test(s) failed!"
     error stop 1
  end if

end program tester

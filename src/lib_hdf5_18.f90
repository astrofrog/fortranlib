! MD5 of template: d5581e358c33758a641a8ab26316b6a7
! High level routines for HDF5
!
! ------------------------------------------------------------------------------
! Copyright (c) 2009-13, Thomas P. Robitaille
!
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
!  * Redistributions of source code must retain the above copyright notice, this
!    list of conditions and the following disclaimer.
!
!  * Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ------------------------------------------------------------------------------

module lib_hdf5

  use hdf5
  use h5tb
  implicit none
  save

  private
  public :: hid_t

  public :: h5t_std_i32le
  public :: h5t_std_i64le
  public :: h5t_ieee_f32le
  public :: h5t_ieee_f64le

  public :: base_hdf5_verbose_enable
  public :: base_hdf5_verbose_disable

  ! open/close
  public :: hdf5_set_compression
  public :: hdf5_test_version
  public :: hdf5_exists
  public :: hdf5_open_new
  public :: hdf5_open_read
  public :: hdf5_open_write
  public :: hdf5_close
  public :: hdf5_finalize
  public :: hdf5_create_external_link
  public :: hdf5_create_group
  public :: hdf5_open_group
  public :: hdf5_close_group
  public :: hdf5_open_dataset
  public :: hdf5_close_dataset
  public :: hdf5_path_exists
  public :: hdf5_list_groups
  public :: hdf5_list_datasets
  public :: hdf5_copy_dataset
  public :: hdf5_copy_group

  ! keywords
  public :: hdf5_list_keywords
  public :: hdf5_read_keyword
  public :: hdf5_read_keyword_vector
  public :: hdf5_read_keyword_vector_auto
  public :: hdf5_write_keyword
  public :: hdf5_write_keyword_vector
  public :: hdf5_exists_keyword
  public :: hdf5_copy_keyword

  ! image
  public :: hdf5_write_array
  public :: hdf5_read_array
  public :: hdf5_read_array_auto
  public :: hdf5_copy_array

  ! table
  public :: hdf5_test_write_table
  public :: hdf5_copy_table
  public :: hdf5_table_write_header
  public :: hdf5_table_write_column
  public :: hdf5_table_read_column
  public :: hdf5_table_read_column_auto
  ! public :: hdf5_table_new_column

  integer,parameter :: idp = selected_int_kind(13)
  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  interface hdf5_write_array
     module procedure hdf5_write_1d_array_h5t_std_i32le
     module procedure hdf5_write_1d_array_h5t_std_i64le
     module procedure hdf5_write_1d_array_h5t_ieee_f32le
     module procedure hdf5_write_1d_array_h5t_ieee_f64le
     module procedure hdf5_write_2d_array_h5t_std_i32le
     module procedure hdf5_write_2d_array_h5t_std_i64le
     module procedure hdf5_write_2d_array_h5t_ieee_f32le
     module procedure hdf5_write_2d_array_h5t_ieee_f64le
     module procedure hdf5_write_3d_array_h5t_std_i32le
     module procedure hdf5_write_3d_array_h5t_std_i64le
     module procedure hdf5_write_3d_array_h5t_ieee_f32le
     module procedure hdf5_write_3d_array_h5t_ieee_f64le
     module procedure hdf5_write_4d_array_h5t_std_i32le
     module procedure hdf5_write_4d_array_h5t_std_i64le
     module procedure hdf5_write_4d_array_h5t_ieee_f32le
     module procedure hdf5_write_4d_array_h5t_ieee_f64le
     module procedure hdf5_write_5d_array_h5t_std_i32le
     module procedure hdf5_write_5d_array_h5t_std_i64le
     module procedure hdf5_write_5d_array_h5t_ieee_f32le
     module procedure hdf5_write_5d_array_h5t_ieee_f64le
     module procedure hdf5_write_6d_array_h5t_std_i32le
     module procedure hdf5_write_6d_array_h5t_std_i64le
     module procedure hdf5_write_6d_array_h5t_ieee_f32le
     module procedure hdf5_write_6d_array_h5t_ieee_f64le
  end interface hdf5_write_array

  interface hdf5_read_array
     module procedure hdf5_read_1d_array_h5t_std_i32le
     module procedure hdf5_read_1d_array_h5t_std_i64le
     module procedure hdf5_read_1d_array_h5t_ieee_f32le
     module procedure hdf5_read_1d_array_h5t_ieee_f64le
     module procedure hdf5_read_2d_array_h5t_std_i32le
     module procedure hdf5_read_2d_array_h5t_std_i64le
     module procedure hdf5_read_2d_array_h5t_ieee_f32le
     module procedure hdf5_read_2d_array_h5t_ieee_f64le
     module procedure hdf5_read_3d_array_h5t_std_i32le
     module procedure hdf5_read_3d_array_h5t_std_i64le
     module procedure hdf5_read_3d_array_h5t_ieee_f32le
     module procedure hdf5_read_3d_array_h5t_ieee_f64le
     module procedure hdf5_read_4d_array_h5t_std_i32le
     module procedure hdf5_read_4d_array_h5t_std_i64le
     module procedure hdf5_read_4d_array_h5t_ieee_f32le
     module procedure hdf5_read_4d_array_h5t_ieee_f64le
     module procedure hdf5_read_5d_array_h5t_std_i32le
     module procedure hdf5_read_5d_array_h5t_std_i64le
     module procedure hdf5_read_5d_array_h5t_ieee_f32le
     module procedure hdf5_read_5d_array_h5t_ieee_f64le
     module procedure hdf5_read_6d_array_h5t_std_i32le
     module procedure hdf5_read_6d_array_h5t_std_i64le
     module procedure hdf5_read_6d_array_h5t_ieee_f32le
     module procedure hdf5_read_6d_array_h5t_ieee_f64le
  end interface hdf5_read_array

  interface hdf5_read_array_auto
     module procedure hdf5_read_1d_array_alloc_h5t_std_i32le
     module procedure hdf5_read_1d_array_alloc_h5t_std_i64le
     module procedure hdf5_read_1d_array_alloc_h5t_ieee_f32le
     module procedure hdf5_read_1d_array_alloc_h5t_ieee_f64le
     module procedure hdf5_read_2d_array_alloc_h5t_std_i32le
     module procedure hdf5_read_2d_array_alloc_h5t_std_i64le
     module procedure hdf5_read_2d_array_alloc_h5t_ieee_f32le
     module procedure hdf5_read_2d_array_alloc_h5t_ieee_f64le
     module procedure hdf5_read_3d_array_alloc_h5t_std_i32le
     module procedure hdf5_read_3d_array_alloc_h5t_std_i64le
     module procedure hdf5_read_3d_array_alloc_h5t_ieee_f32le
     module procedure hdf5_read_3d_array_alloc_h5t_ieee_f64le
     module procedure hdf5_read_4d_array_alloc_h5t_std_i32le
     module procedure hdf5_read_4d_array_alloc_h5t_std_i64le
     module procedure hdf5_read_4d_array_alloc_h5t_ieee_f32le
     module procedure hdf5_read_4d_array_alloc_h5t_ieee_f64le
     module procedure hdf5_read_5d_array_alloc_h5t_std_i32le
     module procedure hdf5_read_5d_array_alloc_h5t_std_i64le
     module procedure hdf5_read_5d_array_alloc_h5t_ieee_f32le
     module procedure hdf5_read_5d_array_alloc_h5t_ieee_f64le
     module procedure hdf5_read_6d_array_alloc_h5t_std_i32le
     module procedure hdf5_read_6d_array_alloc_h5t_std_i64le
     module procedure hdf5_read_6d_array_alloc_h5t_ieee_f32le
     module procedure hdf5_read_6d_array_alloc_h5t_ieee_f64le
  end interface hdf5_read_array_auto

  interface hdf5_read_keyword
     module procedure hdf5_read_k_logical
     module procedure hdf5_read_k_h5t_std_i32le
     module procedure hdf5_read_k_h5t_std_i64le
     module procedure hdf5_read_k_h5t_ieee_f32le
     module procedure hdf5_read_k_h5t_ieee_f64le
     module procedure hdf5_read_k_string
  end interface hdf5_read_keyword

  interface hdf5_read_keyword_vector
     module procedure hdf5_read_k_vector_h5t_std_i32le
     module procedure hdf5_read_k_vector_h5t_std_i64le
     module procedure hdf5_read_k_vector_h5t_ieee_f32le
     module procedure hdf5_read_k_vector_h5t_ieee_f64le
  end interface hdf5_read_keyword_vector

  interface hdf5_read_keyword_vector_auto
     module procedure hdf5_read_k_vector_alloc_h5t_std_i32le
     module procedure hdf5_read_k_vector_alloc_h5t_std_i64le
     module procedure hdf5_read_k_vector_alloc_h5t_ieee_f32le
     module procedure hdf5_read_k_vector_alloc_h5t_ieee_f64le
  end interface hdf5_read_keyword_vector_auto

  interface hdf5_write_keyword
     module procedure hdf5_write_k_logical
     module procedure hdf5_write_k_h5t_std_i32le
     module procedure hdf5_write_k_h5t_std_i64le
     module procedure hdf5_write_k_h5t_ieee_f32le
     module procedure hdf5_write_k_h5t_ieee_f64le
     module procedure hdf5_write_k_string
  end interface hdf5_write_keyword

  interface hdf5_write_keyword_vector
     module procedure hdf5_write_k_vector_h5t_std_i32le
     module procedure hdf5_write_k_vector_h5t_std_i64le
     module procedure hdf5_write_k_vector_h5t_ieee_f32le
     module procedure hdf5_write_k_vector_h5t_ieee_f64le
  end interface hdf5_write_keyword_vector

  interface hdf5_table_write_column
     module procedure write_table_column_1d_h5t_std_i32le
     module procedure write_table_column_2d_h5t_std_i32le
     module procedure write_table_column_1d_h5t_std_i64le
     module procedure write_table_column_2d_h5t_std_i64le
     module procedure write_table_column_1d_h5t_ieee_f32le
     module procedure write_table_column_2d_h5t_ieee_f32le
     module procedure write_table_column_1d_h5t_ieee_f64le
     module procedure write_table_column_2d_h5t_ieee_f64le
     module procedure write_table_column_1d_h5t_native_character
  end interface hdf5_table_write_column

  interface hdf5_table_read_column
     module procedure read_table_column_1d_h5t_std_i32le
     module procedure read_table_column_2d_h5t_std_i32le
     module procedure read_table_column_1d_h5t_std_i64le
     module procedure read_table_column_2d_h5t_std_i64le
     module procedure read_table_column_1d_h5t_ieee_f32le
     module procedure read_table_column_2d_h5t_ieee_f32le
     module procedure read_table_column_1d_h5t_ieee_f64le
     module procedure read_table_column_2d_h5t_ieee_f64le
     module procedure read_table_column_1d_h5t_native_character
  end interface hdf5_table_read_column

  interface hdf5_table_read_column_auto
     module procedure read_table_column_1d_alloc_h5t_std_i32le
     module procedure read_table_column_2d_alloc_h5t_std_i32le
     module procedure read_table_column_1d_alloc_h5t_std_i64le
     module procedure read_table_column_2d_alloc_h5t_std_i64le
     module procedure read_table_column_1d_alloc_h5t_ieee_f32le
     module procedure read_table_column_2d_alloc_h5t_ieee_f32le
     module procedure read_table_column_1d_alloc_h5t_ieee_f64le
     module procedure read_table_column_2d_alloc_h5t_ieee_f64le
     module procedure read_table_column_1d_alloc_h5t_native_character
  end interface hdf5_table_read_column_auto

  logical :: verbose = .false.

  type table_info
     integer(hsize_t) :: n_cols, n_rows
     character(len=255),allocatable :: field_names(:)
     integer(size_t),allocatable :: field_sizes(:)
     integer(size_t),allocatable :: field_offsets(:)
     integer(hid_t),allocatable :: field_types(:)
     integer(size_t) :: type_size
  end type table_info

  ! fix for 64-bit integers - read/write them as floats (not ideal)

  interface h5aread_f
     module procedure h5aread_f_i64
     module procedure h5aread_f_i64_vector
  end interface h5aread_f

  interface h5awrite_f
     module procedure h5awrite_f_i64
     module procedure h5awrite_f_i64_vector
  end interface h5awrite_f

  interface h5dread_f
     module procedure h5dread_f_i64_scalar
     module procedure h5dread_f_i64_1d
     module procedure h5dread_f_i64_2d
     module procedure h5dread_f_i64_3d
     module procedure h5dread_f_i64_4d
     module procedure h5dread_f_i64_5d
     module procedure h5dread_f_i64_6d
  end interface h5dread_f

  interface h5dwrite_f
     module procedure h5dwrite_f_i64_scalar
     module procedure h5dwrite_f_i64_1d
     module procedure h5dwrite_f_i64_2d
     module procedure h5dwrite_f_i64_3d
     module procedure h5dwrite_f_i64_4d
     module procedure h5dwrite_f_i64_5d
     module procedure h5dwrite_f_i64_6d
  end interface h5dwrite_f

  interface h5tbread_field_name_f
     module procedure h5tbread_field_name_f_i64_1d
  end interface h5tbread_field_name_f

  interface h5tbwrite_field_name_f
     module procedure h5tbwrite_field_name_f_i64_1d
  end interface h5tbwrite_field_name_f

  logical :: compress

contains

  subroutine base_hdf5_verbose_enable
    implicit none
    verbose = .true.
  end subroutine base_hdf5_verbose_enable

  subroutine base_hdf5_verbose_disable
    implicit none
    verbose = .false.
  end subroutine base_hdf5_verbose_disable

  subroutine hdf5_set_compression(compression)
    implicit none
    logical,intent(in) :: compression
    compress = compression
  end subroutine hdf5_set_compression

  subroutine check_status(hdferr,origin)
    implicit none
    integer,intent(in) :: hdferr
    character(len=*),optional,intent(in) :: origin

    if(hdferr.ne.0) then

       if(present(origin)) then
          write(*,*) repeat('-',79)
          write(*,*) ' HDF5 returned an error in '//trim(origin)
          write(*,*) ' See above for traceback'
          write(*,*) repeat('-',79)
       else
          write(*,*) repeat('-',79)
          write(*,*) ' HDF5 returned an error'
          write(*,*) ' See above for traceback'
          write(*,*) repeat('-',79)
       end if

       write(*,*)
       write(*,*) ' The program has terminated unexpectadly'
       stop

    end if

  end subroutine check_status

  integer(size_t) function sizeof(type_id)
    implicit none
    integer(hid_t),intent(in) :: type_id
    integer :: hdferr
    call h5tget_size_f(type_id, sizeof, hdferr)
  end function sizeof

  logical function hdf5_test_version(testmaj, testmin, testrel) result(test)
    ! Test whether the library version is equal or more recent to the specified version
    implicit none
    integer,intent(in) :: testmaj, testmin, testrel
    integer :: majnum, minnum, relnum, hdferr
    call h5get_libversion_f(majnum, minnum, relnum, hdferr)
    call check_status(hdferr,'hdf5_test_version')
    test = (majnum > testmaj .or. (majnum == testmaj .and. (minnum > testmin .or. (minnum == testmin .and. relnum >= testrel))))
  end function hdf5_test_version

  logical function hdf5_exists(filename)
    implicit none
    character(len=*),intent(in) :: filename
    logical :: status
    integer :: hdferr
    call h5eset_auto_f(0, hdferr)
    call h5fis_hdf5_f(filename,status,hdferr)
    hdf5_exists = hdferr==0
    call h5eset_auto_f(1, hdferr)
  end function hdf5_exists

  subroutine hdf5_delete(filename,confirm)
    implicit none
    character(len=*),intent(in) :: filename
    logical,intent(in),optional :: confirm
    logical :: confirm_delete
    integer :: status
    character(len=3) :: rep
    character(len=100) :: command
    status = 0
    confirm_delete = .true.
    if(present(confirm)) confirm_delete = confirm
    if(hdf5_exists(filename)) then
       if(confirm_delete) then
          command="rm "//trim(filename)
          write(*,*)
          write(*,'(" WARNING: File exists: ")',advance='no')
          write(*,*) trim(filename)
          write(*,'(" The following command will be run: ")',advance='no')
          write(*,*) trim(command)
          do
             write(*,'(" Do you wish to continue? (y/n) ")')
             read *,rep
             if(rep=="y".or.rep=="n") exit
             write(*,*) 'Please type y or n (case sensitive)'
          end do
          if(rep=='n') stop 'Aborting to avoid overwriting file'
       end if
       open(unit=451,file=filename,form='unformatted')
       close(unit=451,status='delete')
    end if
  end subroutine hdf5_delete

  integer(hid_t) function hdf5_open_new(filename, confirm) result(handle)
    implicit none
    character(len=*),intent(in) :: filename
    logical,intent(in),optional :: confirm
    integer :: hdferr
    call hdf5_delete(filename, confirm)
    call h5open_f(hdferr)
    call h5fcreate_f(filename, h5f_acc_trunc_f, handle, hdferr)
    call check_status(hdferr,'hdf5_open_new')
  end function hdf5_open_new

  integer(hid_t) function hdf5_open_read(filename) result(handle)
    implicit none
    character(len=*),intent(in) :: filename
    integer :: hdferr
    call h5open_f(hdferr)
    call h5fopen_f(filename, h5f_acc_rdonly_f, handle, hdferr)
    call check_status(hdferr,'hdf5_open_read')
  end function hdf5_open_read

  integer(hid_t) function hdf5_open_write(filename) result(handle)
    implicit none
    character(len=*),intent(in) :: filename
    integer :: hdferr
    call h5open_f(hdferr)
    call h5fopen_f(filename, h5f_acc_rdwr_f, handle, hdferr)
    call check_status(hdferr,'hdf5_open_write')
  end function hdf5_open_write

  subroutine hdf5_close(handle)
    implicit none
    integer(hid_t),intent(in) :: handle
    integer :: hdferr
    call h5fclose_f(handle, hdferr)
    call check_status(hdferr,'hdf5_close')
  end subroutine hdf5_close

  subroutine hdf5_finalize()
    implicit none
    integer :: hdferr
    call h5close_f(hdferr)
    call check_status(hdferr,'hdf5_finalize')
  end subroutine hdf5_finalize

  subroutine hdf5_create_external_link(handle, path, filename, object)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, filename, object
    integer :: hdferr
    call h5lcreate_external_f(filename, object, handle, path, hdferr)
    call check_status(hdferr,'hdf5_create_external_link')
  end subroutine hdf5_create_external_link

  integer(hid_t) function hdf5_create_group(handle, path) result(grp_id)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer :: hdferr
    call h5gcreate_f(handle, path, grp_id, hdferr)
    call check_status(hdferr,'hdf5_create_group')
  end function hdf5_create_group

  integer(hid_t) function hdf5_open_group(handle, path) result(grp_id)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer :: hdferr
    call h5gopen_f(handle, path, grp_id, hdferr)
    call check_status(hdferr,'hdf5_open_group')
  end function hdf5_open_group

  subroutine hdf5_close_group(grp_id)
    implicit none
    integer(hid_t),intent(in) :: grp_id
    integer :: hdferr
    call h5gclose_f(grp_id, hdferr)
    call check_status(hdferr,'hdf5_close_group')
  end subroutine hdf5_close_group

  integer(hid_t) function hdf5_open_dataset(handle, path) result(dset_id)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer :: hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_open_dataset')
  end function hdf5_open_dataset

  subroutine hdf5_close_dataset(dset_id)
    implicit none
    integer(hid_t),intent(in) :: dset_id
    integer :: hdferr
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_close_dataset')
  end subroutine hdf5_close_dataset

  logical function hdf5_path_exists(handle, path) result(exists)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer :: hdferr
    call h5lexists_f(handle, path, exists, hdferr)
    call check_status(hdferr,'hdf5_path_exists')
  end function hdf5_path_exists

  integer function hdf5_count_members(handle, path) result(n_members)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer :: hdferr
    call h5gn_members_f(handle, path, n_members, hdferr)
    call check_status(hdferr,'hdf5_count_members')
  end function hdf5_count_members

  integer function hdf5_count_keywords(handle, path) result(n_keywords)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(hid_t) :: obj_id
    integer :: hdferr
    call h5oopen_f(handle, path, obj_id, hdferr)
    call check_status(hdferr,'hdf5_count_keywords [1]')
    call h5aget_num_attrs_f(obj_id, n_keywords, hdferr)
    call check_status(hdferr,'hdf5_count_keywords [2]')
    ! The following lines require a recent version of HDF5, so we leave them
    ! commented for now.
    ! call h5oclose_f(obj_id, hdferr)
    ! call check_status(hdferr,'hdf5_count_keywords [3]')
  end function hdf5_count_keywords

  subroutine hdf5_member_info(handle, path, index, name, type)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,intent(in) :: index
    character(len=*),intent(out) :: name
    integer,intent(out) :: type
    integer :: hdferr
    call h5gget_obj_info_idx_f(handle, path, index, name, type, hdferr)
    call check_status(hdferr,'hdf5_member_info')
  end subroutine hdf5_member_info

  subroutine hdf5_list_keywords(handle, path, keywords)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    character(len=*),allocatable,intent(out) :: keywords(:)
    integer :: n_keywords
    integer(HSIZE_T) :: idx
    integer :: hdferr
    n_keywords = hdf5_count_keywords(handle, path)
    allocate(keywords(n_keywords))
    do idx=1,n_keywords
       call h5aget_name_by_idx_f(handle, path, h5_index_name_f, h5_iter_native_f, idx-1, keywords(idx), hdferr)
       call check_status(hdferr,'hdf5_list_keywords')
    end do
  end subroutine hdf5_list_keywords

  subroutine hdf5_list_objects(handle, path, objects, filter_type)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    character(len=*),allocatable,intent(out) :: objects(:)
    integer :: n_objects_all, n_objects, im, ig
    integer,optional,intent(in) :: filter_type
    character(len=len(objects)) :: name
    integer :: type

    ! Find total number of objects
    n_objects_all = hdf5_count_members(handle, path)

    ! Count how many objects to list
    n_objects = 0
    do im=1,n_objects_all
       call hdf5_member_info(handle, path, im-1, name, type)
       if(type==filter_type) n_objects = n_objects + 1
    end do
    allocate(objects(n_objects))
    ig = 0
    do im=1,n_objects_all
       call hdf5_member_info(handle, path, im-1, name, type)
       if(type==filter_type) then
          ig=ig + 1
          objects(ig) = name
       end if
    end do
  end subroutine hdf5_list_objects

  subroutine hdf5_list_groups(handle, path, groups)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    character(len=*),allocatable,intent(out) :: groups(:)
    call hdf5_list_objects(handle, path, groups, H5G_GROUP_F)
  end subroutine hdf5_list_groups

  subroutine hdf5_list_datasets(handle, path, datasets)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    character(len=*),allocatable,intent(out) :: datasets(:)
    call hdf5_list_objects(handle, path, datasets, H5G_DATASET_F)
  end subroutine hdf5_list_datasets

  subroutine hdf5_list_links(handle, path, links)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    character(len=*),allocatable,intent(out) :: links(:)
    call hdf5_list_objects(handle, path, links, H5G_LINK_F)
  end subroutine hdf5_list_links

  recursive subroutine hdf5_copy_group(handle_in, path_in, handle_out, path_out)

    implicit none

    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*), intent(in) :: path_in, path_out
    character(len=255),allocatable :: keywords(:)
    character(len=255),allocatable :: groups(:)
    character(len=255),allocatable :: datasets(:)
    integer :: data_idx, key_idx, group_idx
    integer(hid_t) :: orig_id, dest_id, grp_id

    ! Get reference to input and output groups
    orig_id = hdf5_open_group(handle_in, path_in)
    dest_id = hdf5_open_group(handle_out, path_out)

    ! Loop over attributes and copy over
    call hdf5_list_keywords(orig_id, '.', keywords)
    do key_idx=1,size(keywords)
       call hdf5_copy_keyword(orig_id, '.', keywords(key_idx), dest_id, '.', keywords(key_idx))
    end do

    ! Loop over datasets and copy over
    call hdf5_list_datasets(orig_id, '.', datasets)
    do data_idx=1,size(datasets)

       ! Copy over dataset
       call hdf5_copy_dataset(orig_id, datasets(data_idx), dest_id, datasets(data_idx))

       ! Loop over dataset keywords and copy over
       call hdf5_list_keywords(orig_id, datasets(data_idx), keywords)
       do key_idx=1,size(keywords)
          if(.not.hdf5_exists_keyword(dest_id, datasets(data_idx), keywords(key_idx))) then
             call hdf5_copy_keyword(orig_id, datasets(data_idx), keywords(key_idx), dest_id, datasets(data_idx), keywords(key_idx))
          end if
       end do

    end do

    ! Loop over groups in reference and create groups for copy
    call hdf5_list_groups(orig_id, '.', groups)
    do group_idx=1,size(groups)
       grp_id = hdf5_create_group(dest_id, groups(group_idx))
       call hdf5_copy_group(orig_id, groups(group_idx), grp_id, '.')
    end do

  end subroutine hdf5_copy_group

  subroutine hdf5_copy_dataset(handle_in, path_in, handle_out, path_out)
    implicit none
    integer(hid_t), intent(in) :: handle_in, handle_out
    character(len=*), intent(in) :: path_in, path_out
    integer :: class
    integer(hid_t) :: dset_id, datatype_id
    integer :: hdferr
    call h5dopen_f(handle_in, path_in, dset_id, hdferr)
    call h5dget_type_f(dset_id, datatype_id, hdferr)
    call h5tget_class_f(datatype_id, class, hdferr)
    if(class == h5t_compound_f) then
       call hdf5_copy_table(handle_in, path_in, handle_out, path_out)
    else
       call hdf5_copy_array(handle_in, path_in, handle_out, path_out)
    end if
  end subroutine hdf5_copy_dataset

  subroutine hdf5_copy_table(handle_in, path_in, handle_out, path_out)

    implicit none

    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out
    type(table_info) :: info
    integer(hid_t) :: datatype_id
    integer :: hdferr
    integer :: field_idx
    integer :: class
    logical :: is_string
    logical :: is_array
    logical :: is_h5t_std_i32le
    logical :: is_h5t_std_i64le
    logical :: is_h5t_ieee_f32le
    logical :: is_h5t_ieee_f64le
    character(len=255) :: col_name
    integer :: rank

    ! Copy over the table header
    info = hdf5_read_table_info(handle_in, path_in)
    call hdf5_table_write_header_info(handle_out, path_out, info)

    ! Copy over the fields
    do field_idx=1,info%n_cols

       datatype_id = info%field_types(field_idx)
       col_name = info%field_names(field_idx)

       ! Check if column is a 2D array
       call h5tget_class_f(datatype_id, class, hdferr)
       is_array = class == h5t_array_f

       if(is_array) then
          call h5tget_array_ndims_f(datatype_id, rank, hdferr)
          if(rank /= 1) then
             write(0,'("ERROR: cannot copy ", I0,"-dimensional fields")') rank
             stop
          end if
          call h5tget_super_f(datatype_id, datatype_id, hdferr)
       end if

       ! Check the type of the array to copy
       call h5tget_class_f(datatype_id, class, hdferr)
       is_string = class == h5t_string_f
       call h5tequal_f(datatype_id, h5t_std_i32le, is_h5t_std_i32le, hdferr)
       call h5tequal_f(datatype_id, h5t_std_i64le, is_h5t_std_i64le, hdferr)
       call h5tequal_f(datatype_id, h5t_ieee_f32le, is_h5t_ieee_f32le, hdferr)
       call h5tequal_f(datatype_id, h5t_ieee_f64le, is_h5t_ieee_f64le, hdferr)

       ! Close the datatype
       call h5tclose_f(datatype_id, hdferr)

       ! Copy over the array using the appropriate routine
       if(is_string) then
          if(is_array) then
             write(0,'("ERROR: cannot read 2-d string columns")')
             stop
          else
             call copy_table_column_1d_h5t_native_character(handle_in, path_in, col_name, handle_out, path_out, col_name)
          end if
       else if(is_h5t_std_i32le) then
          if(is_array) then
             call copy_table_column_2d_h5t_std_i32le(handle_in, path_in, col_name, handle_out, path_out, col_name)
          else
             call copy_table_column_1d_h5t_std_i32le(handle_in, path_in, col_name, handle_out, path_out, col_name)
          end if
       else if(is_h5t_std_i64le) then
          if(is_array) then
             call copy_table_column_2d_h5t_std_i64le(handle_in, path_in, col_name, handle_out, path_out, col_name)
          else
             call copy_table_column_1d_h5t_std_i64le(handle_in, path_in, col_name, handle_out, path_out, col_name)
          end if
       else if(is_h5t_ieee_f32le) then
          if(is_array) then
             call copy_table_column_2d_h5t_ieee_f32le(handle_in, path_in, col_name, handle_out, path_out, col_name)
          else
             call copy_table_column_1d_h5t_ieee_f32le(handle_in, path_in, col_name, handle_out, path_out, col_name)
          end if
       else if(is_h5t_ieee_f64le) then
          if(is_array) then
             call copy_table_column_2d_h5t_ieee_f64le(handle_in, path_in, col_name, handle_out, path_out, col_name)
          else
             call copy_table_column_1d_h5t_ieee_f64le(handle_in, path_in, col_name, handle_out, path_out, col_name)
          end if
       else
          write(0,'("ERROR: unknown datatype ", I0)') datatype_id
          stop
       end if

    end do

  end subroutine hdf5_copy_table

  subroutine hdf5_table_write_header(handle,path,n_rows,n_cols,names,widths,types)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,intent(in) :: n_cols, n_rows
    character(len=*),intent(in) :: names(:)
    integer,intent(in) :: widths(:)
    integer(hid_t),intent(in) :: types(:)
    integer(size_t) :: offsets(size(names))
    integer(size_t) :: type_size
    integer(hsize_t) :: chunk_size = 10
    integer,parameter :: compress = 0
    integer :: hdferr
    integer :: i
    type_size = 0
    do i=1,n_cols
       offsets(i) = type_size
       if (types(i) == h5t_std_i32le) then
          type_size = type_size + sizeof(h5t_std_i32le) * widths(i)
       else if(types(i) == h5t_ieee_f32le) then
          type_size = type_size + sizeof(h5t_ieee_f32le) * widths(i)
       else if(types(i) == h5t_ieee_f64le) then
          type_size = type_size + sizeof(h5t_ieee_f64le) * widths(i)
       else if(types(i) == h5t_native_character) then
          type_size = type_size + sizeof(h5t_native_character) * widths(i)
       else
          stop "unknown type"
       end if
    end do
    if(.not.hdf5_test_write_table(names)) then
       write(0,'("ERROR: cannot write table, version of HDF5 is not recent enough")')
       stop
    end if
    call h5tbmake_table_f('table_title',handle,path,int(n_cols,hsize_t),int(n_rows,hsize_t),type_size, &
         & names,offsets,types,chunk_size,compress,hdferr)
    call check_status(hdferr,'hdf5_table_write_header')
  end subroutine hdf5_table_write_header

  subroutine hdf5_table_write_header_info(handle,path,info)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    type(table_info),intent(in) :: info
    integer(hsize_t) :: chunk_size = 10
    integer,parameter :: compress = 0
    integer :: hdferr
    if(.not.hdf5_test_write_table(info%field_names)) then
       write(0,'("ERROR: cannot write table, version of HDF5 is not recent enough")')
       stop
    end if
    call h5tbmake_table_f('table_title',handle,path,info%n_cols,info%n_rows,info%type_size, &
         & info%field_names,info%field_offsets,info%field_types,chunk_size,compress,hdferr)
    call check_status(hdferr,'hdf5_table_write_header_info')
  end subroutine hdf5_table_write_header_info

  logical function hdf5_test_write_table(field_names) result(test)

    ! Check whether the table can be written with the current version of HDF5

    implicit none

    character(len=*),intent(in) :: field_names(:)
    integer :: i

    ! Set can_write to .true. and then change to .false. if any tests fail
    test = .true.

    ! If version is 1.8.6 or more recent, there are no issues with writing
    ! tables with fields that have different lengths
    if(hdf5_test_version(1, 8, 6)) return

    ! Otherwise, need to check that all fields match the field name length
    do i = 1, size(field_names)
       if (len(field_names(i)) /= len(trim(field_names(i)))) then
          test = .false.
          return
       end if
    end do

  end function hdf5_test_write_table

  subroutine read_table_column_1d_h5t_native_character(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    character(len=*),intent(out) :: values(:)
    integer(hsize_t), parameter :: start = 0_hsize_t
    integer :: hdferr
    integer :: col_id
    type(table_info) :: info
    info = hdf5_read_table_info(handle, path)
    col_id = hdf5_table_column_number(info, col_name)
    call h5tbread_field_name_f(handle, path, col_name, start, info%n_rows, info%field_sizes(col_id), values, hdferr)
    call clean_string(values)
    call check_status(hdferr,'read_table_column_1d_h5t_native_character')
  end subroutine read_table_column_1d_h5t_native_character

  subroutine read_table_column_1d_alloc_h5t_native_character(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    character(len=*),allocatable,intent(out) :: values(:)
    integer :: col_id
    type(table_info) :: info
    info = hdf5_read_table_info(handle, path)
    col_id = hdf5_table_column_number(info, col_name)
    allocate(values(info%n_rows))
    call read_table_column_1d_h5t_native_character(handle, path, col_name, values)
  end subroutine read_table_column_1d_alloc_h5t_native_character


  subroutine read_table_column_1d_h5t_ieee_f64le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(dp),intent(out) :: values(:)
    real(dp) :: values_temp(size(values),1)
    call read_table_column_2d_h5t_ieee_f64le(handle, path, col_name, values_temp)
    values = values_temp(:,1)
  end subroutine read_table_column_1d_h5t_ieee_f64le

  subroutine read_table_column_1d_alloc_h5t_ieee_f64le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(dp),allocatable,intent(out) :: values(:)
    real(dp), allocatable :: values_temp(:,:)
    call read_table_column_2d_alloc_h5t_ieee_f64le(handle, path, col_name, values_temp)
    if(size(values_temp, 1) .ne. 1) stop "column is not 1d"
    allocate(values(size(values_temp, 2)))
    values = values_temp(1,:)
  end subroutine read_table_column_1d_alloc_h5t_ieee_f64le

  subroutine read_table_column_2d_h5t_ieee_f64le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(dp),intent(out) :: values(:,:)
    integer(hsize_t), parameter :: start = 0_hsize_t
    real(dp),allocatable :: values_tmp(:)
    integer :: hdferr
    integer :: col_id
    type(table_info) :: info
    allocate(values_tmp(size(values)))
    info = hdf5_read_table_info(handle, path)
    col_id = hdf5_table_column_number(info, col_name)
    call h5tbread_field_name_f(handle, path, col_name, start, info%n_rows, info%field_sizes(col_id), values_tmp, hdferr)
    values = reshape(values_tmp, (/size(values, 1), size(values, 2)/))
    call check_status(hdferr,'read_table_column_2d_h5t_ieee_f64le')
  end subroutine read_table_column_2d_h5t_ieee_f64le

  subroutine read_table_column_2d_alloc_h5t_ieee_f64le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(dp),allocatable,intent(out) :: values(:,:)
    integer :: col_id
    type(table_info) :: info
    info = hdf5_read_table_info(handle, path)
    col_id = hdf5_table_column_number(info, col_name)
    allocate(values(info%field_sizes(col_id)/sizeof(h5t_ieee_f64le), info%n_rows))
    call read_table_column_2d_h5t_ieee_f64le(handle, path, col_name, values)
  end subroutine read_table_column_2d_alloc_h5t_ieee_f64le

  subroutine copy_table_column_2d_h5t_ieee_f64le(handle_in,path_in,col_name_in, handle_out, path_out, col_name_out)
    implicit none
    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out, col_name_in, col_name_out
    real(dp),allocatable :: values(:,:)
    call read_table_column_2d_alloc_h5t_ieee_f64le(handle_in, path_in, col_name_in, values)
    call write_table_column_2d_h5t_ieee_f64le(handle_out, path_out, col_name_out, values)
  end subroutine copy_table_column_2d_h5t_ieee_f64le


  subroutine read_table_column_1d_h5t_ieee_f32le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(sp),intent(out) :: values(:)
    real(sp) :: values_temp(size(values),1)
    call read_table_column_2d_h5t_ieee_f32le(handle, path, col_name, values_temp)
    values = values_temp(:,1)
  end subroutine read_table_column_1d_h5t_ieee_f32le

  subroutine read_table_column_1d_alloc_h5t_ieee_f32le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(sp),allocatable,intent(out) :: values(:)
    real(sp), allocatable :: values_temp(:,:)
    call read_table_column_2d_alloc_h5t_ieee_f32le(handle, path, col_name, values_temp)
    if(size(values_temp, 1) .ne. 1) stop "column is not 1d"
    allocate(values(size(values_temp, 2)))
    values = values_temp(1,:)
  end subroutine read_table_column_1d_alloc_h5t_ieee_f32le

  subroutine read_table_column_2d_h5t_ieee_f32le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(sp),intent(out) :: values(:,:)
    integer(hsize_t), parameter :: start = 0_hsize_t
    real(sp),allocatable :: values_tmp(:)
    integer :: hdferr
    integer :: col_id
    type(table_info) :: info
    allocate(values_tmp(size(values)))
    info = hdf5_read_table_info(handle, path)
    col_id = hdf5_table_column_number(info, col_name)
    call h5tbread_field_name_f(handle, path, col_name, start, info%n_rows, info%field_sizes(col_id), values_tmp, hdferr)
    values = reshape(values_tmp, (/size(values, 1), size(values, 2)/))
    call check_status(hdferr,'read_table_column_2d_h5t_ieee_f32le')
  end subroutine read_table_column_2d_h5t_ieee_f32le

  subroutine read_table_column_2d_alloc_h5t_ieee_f32le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(sp),allocatable,intent(out) :: values(:,:)
    integer :: col_id
    type(table_info) :: info
    info = hdf5_read_table_info(handle, path)
    col_id = hdf5_table_column_number(info, col_name)
    allocate(values(info%field_sizes(col_id)/sizeof(h5t_ieee_f32le), info%n_rows))
    call read_table_column_2d_h5t_ieee_f32le(handle, path, col_name, values)
  end subroutine read_table_column_2d_alloc_h5t_ieee_f32le

  subroutine copy_table_column_2d_h5t_ieee_f32le(handle_in,path_in,col_name_in, handle_out, path_out, col_name_out)
    implicit none
    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out, col_name_in, col_name_out
    real(sp),allocatable :: values(:,:)
    call read_table_column_2d_alloc_h5t_ieee_f32le(handle_in, path_in, col_name_in, values)
    call write_table_column_2d_h5t_ieee_f32le(handle_out, path_out, col_name_out, values)
  end subroutine copy_table_column_2d_h5t_ieee_f32le


  subroutine read_table_column_1d_h5t_std_i64le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer(idp),intent(out) :: values(:)
    integer(idp) :: values_temp(size(values),1)
    call read_table_column_2d_h5t_std_i64le(handle, path, col_name, values_temp)
    values = values_temp(:,1)
  end subroutine read_table_column_1d_h5t_std_i64le

  subroutine read_table_column_1d_alloc_h5t_std_i64le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer(idp),allocatable,intent(out) :: values(:)
    integer(idp), allocatable :: values_temp(:,:)
    call read_table_column_2d_alloc_h5t_std_i64le(handle, path, col_name, values_temp)
    if(size(values_temp, 1) .ne. 1) stop "column is not 1d"
    allocate(values(size(values_temp, 2)))
    values = values_temp(1,:)
  end subroutine read_table_column_1d_alloc_h5t_std_i64le

  subroutine read_table_column_2d_h5t_std_i64le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer(idp),intent(out) :: values(:,:)
    integer(hsize_t), parameter :: start = 0_hsize_t
    integer(idp),allocatable :: values_tmp(:)
    integer :: hdferr
    integer :: col_id
    type(table_info) :: info
    allocate(values_tmp(size(values)))
    info = hdf5_read_table_info(handle, path)
    col_id = hdf5_table_column_number(info, col_name)
    call h5tbread_field_name_f(handle, path, col_name, start, info%n_rows, info%field_sizes(col_id), values_tmp, hdferr)
    values = reshape(values_tmp, (/size(values, 1), size(values, 2)/))
    call check_status(hdferr,'read_table_column_2d_h5t_std_i64le')
  end subroutine read_table_column_2d_h5t_std_i64le

  subroutine read_table_column_2d_alloc_h5t_std_i64le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer(idp),allocatable,intent(out) :: values(:,:)
    integer :: col_id
    type(table_info) :: info
    info = hdf5_read_table_info(handle, path)
    col_id = hdf5_table_column_number(info, col_name)
    allocate(values(info%field_sizes(col_id)/sizeof(h5t_std_i64le), info%n_rows))
    call read_table_column_2d_h5t_std_i64le(handle, path, col_name, values)
  end subroutine read_table_column_2d_alloc_h5t_std_i64le

  subroutine copy_table_column_2d_h5t_std_i64le(handle_in,path_in,col_name_in, handle_out, path_out, col_name_out)
    implicit none
    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out, col_name_in, col_name_out
    integer(idp),allocatable :: values(:,:)
    call read_table_column_2d_alloc_h5t_std_i64le(handle_in, path_in, col_name_in, values)
    call write_table_column_2d_h5t_std_i64le(handle_out, path_out, col_name_out, values)
  end subroutine copy_table_column_2d_h5t_std_i64le


  subroutine read_table_column_1d_h5t_std_i32le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer,intent(out) :: values(:)
    integer :: values_temp(size(values),1)
    call read_table_column_2d_h5t_std_i32le(handle, path, col_name, values_temp)
    values = values_temp(:,1)
  end subroutine read_table_column_1d_h5t_std_i32le

  subroutine read_table_column_1d_alloc_h5t_std_i32le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer,allocatable,intent(out) :: values(:)
    integer, allocatable :: values_temp(:,:)
    call read_table_column_2d_alloc_h5t_std_i32le(handle, path, col_name, values_temp)
    if(size(values_temp, 1) .ne. 1) stop "column is not 1d"
    allocate(values(size(values_temp, 2)))
    values = values_temp(1,:)
  end subroutine read_table_column_1d_alloc_h5t_std_i32le

  subroutine read_table_column_2d_h5t_std_i32le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer,intent(out) :: values(:,:)
    integer(hsize_t), parameter :: start = 0_hsize_t
    integer,allocatable :: values_tmp(:)
    integer :: hdferr
    integer :: col_id
    type(table_info) :: info
    allocate(values_tmp(size(values)))
    info = hdf5_read_table_info(handle, path)
    col_id = hdf5_table_column_number(info, col_name)
    call h5tbread_field_name_f(handle, path, col_name, start, info%n_rows, info%field_sizes(col_id), values_tmp, hdferr)
    values = reshape(values_tmp, (/size(values, 1), size(values, 2)/))
    call check_status(hdferr,'read_table_column_2d_h5t_std_i32le')
  end subroutine read_table_column_2d_h5t_std_i32le

  subroutine read_table_column_2d_alloc_h5t_std_i32le(handle, path, col_name, values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer,allocatable,intent(out) :: values(:,:)
    integer :: col_id
    type(table_info) :: info
    info = hdf5_read_table_info(handle, path)
    col_id = hdf5_table_column_number(info, col_name)
    allocate(values(info%field_sizes(col_id)/sizeof(h5t_std_i32le), info%n_rows))
    call read_table_column_2d_h5t_std_i32le(handle, path, col_name, values)
  end subroutine read_table_column_2d_alloc_h5t_std_i32le

  subroutine copy_table_column_2d_h5t_std_i32le(handle_in,path_in,col_name_in, handle_out, path_out, col_name_out)
    implicit none
    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out, col_name_in, col_name_out
    integer,allocatable :: values(:,:)
    call read_table_column_2d_alloc_h5t_std_i32le(handle_in, path_in, col_name_in, values)
    call write_table_column_2d_h5t_std_i32le(handle_out, path_out, col_name_out, values)
  end subroutine copy_table_column_2d_h5t_std_i32le



  subroutine write_table_column_1d_h5t_native_character(handle,path,col_name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    character(len=*),intent(in) :: values(:)
    integer(hsize_t) :: nrecords
    integer(size_t) :: type_size
    integer(hsize_t),parameter :: start=0
    integer :: hdferr
    nrecords = size(values)
    type_size = sizeof(h5t_native_character)
    call h5tbwrite_field_name_f(handle,path,col_name,start,nrecords,type_size,values,hdferr)
    call check_status(hdferr,'write_table_column_1d_h5t_native_character')
  end subroutine write_table_column_1d_h5t_native_character

  subroutine write_table_column_2d_h5t_native_character(handle,path,col_name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    character(len=*),intent(in) :: values(:,:)
    integer(hsize_t) :: nrecords
    integer(size_t) :: type_size
    integer(hsize_t),parameter :: start=0
    integer :: hdferr
    nrecords = size(values, 2)
    type_size = sizeof(h5t_native_character) * size(values, 1)
    call h5tbwrite_field_name_f(handle,path,col_name,start,nrecords,type_size,reshape(values,(/size(values)/)),hdferr)
    call check_status(hdferr,'write_table_column_2d_h5t_native_character')
  end subroutine write_table_column_2d_h5t_native_character

  subroutine copy_table_column_1d_h5t_native_character(handle_in,path_in,col_name_in, handle_out, path_out, col_name_out)
    implicit none
    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out, col_name_in, col_name_out
    character(len=1000),allocatable :: values(:)
    call read_table_column_1d_alloc_h5t_native_character(handle_in, path_in, col_name_in, values)
    call write_table_column_1d_h5t_native_character(handle_out, path_out, col_name_out, values)
  end subroutine copy_table_column_1d_h5t_native_character


  subroutine write_table_column_1d_h5t_ieee_f64le(handle,path,col_name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(dp),intent(in) :: values(:)
    integer(hsize_t) :: nrecords
    integer(size_t) :: type_size
    integer(hsize_t),parameter :: start=0
    integer :: hdferr
    nrecords = size(values)
    type_size = sizeof(h5t_ieee_f64le)
    call h5tbwrite_field_name_f(handle,path,col_name,start,nrecords,type_size,values,hdferr)
    call check_status(hdferr,'write_table_column_1d_h5t_ieee_f64le')
  end subroutine write_table_column_1d_h5t_ieee_f64le

  subroutine write_table_column_2d_h5t_ieee_f64le(handle,path,col_name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(dp),intent(in) :: values(:,:)
    integer(hsize_t) :: nrecords
    integer(size_t) :: type_size
    integer(hsize_t),parameter :: start=0
    integer :: hdferr
    nrecords = size(values, 2)
    type_size = sizeof(h5t_ieee_f64le) * size(values, 1)
    call h5tbwrite_field_name_f(handle,path,col_name,start,nrecords,type_size,reshape(values,(/size(values)/)),hdferr)
    call check_status(hdferr,'write_table_column_2d_h5t_ieee_f64le')
  end subroutine write_table_column_2d_h5t_ieee_f64le

  subroutine copy_table_column_1d_h5t_ieee_f64le(handle_in,path_in,col_name_in, handle_out, path_out, col_name_out)
    implicit none
    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out, col_name_in, col_name_out
    real(dp),allocatable :: values(:)
    call read_table_column_1d_alloc_h5t_ieee_f64le(handle_in, path_in, col_name_in, values)
    call write_table_column_1d_h5t_ieee_f64le(handle_out, path_out, col_name_out, values)
  end subroutine copy_table_column_1d_h5t_ieee_f64le


  subroutine write_table_column_1d_h5t_ieee_f32le(handle,path,col_name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(sp),intent(in) :: values(:)
    integer(hsize_t) :: nrecords
    integer(size_t) :: type_size
    integer(hsize_t),parameter :: start=0
    integer :: hdferr
    nrecords = size(values)
    type_size = sizeof(h5t_ieee_f32le)
    call h5tbwrite_field_name_f(handle,path,col_name,start,nrecords,type_size,values,hdferr)
    call check_status(hdferr,'write_table_column_1d_h5t_ieee_f32le')
  end subroutine write_table_column_1d_h5t_ieee_f32le

  subroutine write_table_column_2d_h5t_ieee_f32le(handle,path,col_name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    real(sp),intent(in) :: values(:,:)
    integer(hsize_t) :: nrecords
    integer(size_t) :: type_size
    integer(hsize_t),parameter :: start=0
    integer :: hdferr
    nrecords = size(values, 2)
    type_size = sizeof(h5t_ieee_f32le) * size(values, 1)
    call h5tbwrite_field_name_f(handle,path,col_name,start,nrecords,type_size,reshape(values,(/size(values)/)),hdferr)
    call check_status(hdferr,'write_table_column_2d_h5t_ieee_f32le')
  end subroutine write_table_column_2d_h5t_ieee_f32le

  subroutine copy_table_column_1d_h5t_ieee_f32le(handle_in,path_in,col_name_in, handle_out, path_out, col_name_out)
    implicit none
    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out, col_name_in, col_name_out
    real(sp),allocatable :: values(:)
    call read_table_column_1d_alloc_h5t_ieee_f32le(handle_in, path_in, col_name_in, values)
    call write_table_column_1d_h5t_ieee_f32le(handle_out, path_out, col_name_out, values)
  end subroutine copy_table_column_1d_h5t_ieee_f32le


  subroutine write_table_column_1d_h5t_std_i64le(handle,path,col_name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer(idp),intent(in) :: values(:)
    integer(hsize_t) :: nrecords
    integer(size_t) :: type_size
    integer(hsize_t),parameter :: start=0
    integer :: hdferr
    nrecords = size(values)
    type_size = sizeof(h5t_std_i64le)
    call h5tbwrite_field_name_f(handle,path,col_name,start,nrecords,type_size,values,hdferr)
    call check_status(hdferr,'write_table_column_1d_h5t_std_i64le')
  end subroutine write_table_column_1d_h5t_std_i64le

  subroutine write_table_column_2d_h5t_std_i64le(handle,path,col_name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer(idp),intent(in) :: values(:,:)
    integer(hsize_t) :: nrecords
    integer(size_t) :: type_size
    integer(hsize_t),parameter :: start=0
    integer :: hdferr
    nrecords = size(values, 2)
    type_size = sizeof(h5t_std_i64le) * size(values, 1)
    call h5tbwrite_field_name_f(handle,path,col_name,start,nrecords,type_size,reshape(values,(/size(values)/)),hdferr)
    call check_status(hdferr,'write_table_column_2d_h5t_std_i64le')
  end subroutine write_table_column_2d_h5t_std_i64le

  subroutine copy_table_column_1d_h5t_std_i64le(handle_in,path_in,col_name_in, handle_out, path_out, col_name_out)
    implicit none
    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out, col_name_in, col_name_out
    integer(idp),allocatable :: values(:)
    call read_table_column_1d_alloc_h5t_std_i64le(handle_in, path_in, col_name_in, values)
    call write_table_column_1d_h5t_std_i64le(handle_out, path_out, col_name_out, values)
  end subroutine copy_table_column_1d_h5t_std_i64le


  subroutine write_table_column_1d_h5t_std_i32le(handle,path,col_name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer,intent(in) :: values(:)
    integer(hsize_t) :: nrecords
    integer(size_t) :: type_size
    integer(hsize_t),parameter :: start=0
    integer :: hdferr
    nrecords = size(values)
    type_size = sizeof(h5t_std_i32le)
    call h5tbwrite_field_name_f(handle,path,col_name,start,nrecords,type_size,values,hdferr)
    call check_status(hdferr,'write_table_column_1d_h5t_std_i32le')
  end subroutine write_table_column_1d_h5t_std_i32le

  subroutine write_table_column_2d_h5t_std_i32le(handle,path,col_name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, col_name
    integer,intent(in) :: values(:,:)
    integer(hsize_t) :: nrecords
    integer(size_t) :: type_size
    integer(hsize_t),parameter :: start=0
    integer :: hdferr
    nrecords = size(values, 2)
    type_size = sizeof(h5t_std_i32le) * size(values, 1)
    call h5tbwrite_field_name_f(handle,path,col_name,start,nrecords,type_size,reshape(values,(/size(values)/)),hdferr)
    call check_status(hdferr,'write_table_column_2d_h5t_std_i32le')
  end subroutine write_table_column_2d_h5t_std_i32le

  subroutine copy_table_column_1d_h5t_std_i32le(handle_in,path_in,col_name_in, handle_out, path_out, col_name_out)
    implicit none
    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out, col_name_in, col_name_out
    integer,allocatable :: values(:)
    call read_table_column_1d_alloc_h5t_std_i32le(handle_in, path_in, col_name_in, values)
    call write_table_column_1d_h5t_std_i32le(handle_out, path_out, col_name_out, values)
  end subroutine copy_table_column_1d_h5t_std_i32le



  subroutine hdf5_read_1d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:) :: array
    integer(hsize_t) :: dims(1)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_ieee_f64le [1]')
    call h5dread_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_ieee_f64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_ieee_f64le [3]')
  end subroutine hdf5_read_1d_array_h5t_ieee_f64le

  subroutine hdf5_read_2d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:,:) :: array
    integer(hsize_t) :: dims(2)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_ieee_f64le [1]')
    call h5dread_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_ieee_f64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_ieee_f64le [3]')
  end subroutine hdf5_read_2d_array_h5t_ieee_f64le

  subroutine hdf5_read_3d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:,:,:) :: array
    integer(hsize_t) :: dims(3)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_ieee_f64le [1]')
    call h5dread_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_ieee_f64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_ieee_f64le [3]')
  end subroutine hdf5_read_3d_array_h5t_ieee_f64le

  subroutine hdf5_read_4d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:,:,:,:) :: array
    integer(hsize_t) :: dims(4)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_ieee_f64le [1]')
    call h5dread_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_ieee_f64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_ieee_f64le [3]')
  end subroutine hdf5_read_4d_array_h5t_ieee_f64le

  subroutine hdf5_read_5d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:,:,:,:,:) :: array
    integer(hsize_t) :: dims(5)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4),  size(array,5)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_ieee_f64le [1]')
    call h5dread_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_ieee_f64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_ieee_f64le [3]')
  end subroutine hdf5_read_5d_array_h5t_ieee_f64le

  subroutine hdf5_read_6d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:,:,:,:,:,:) :: array
    integer(hsize_t) :: dims(6)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4),  size(array,5),  size(array,6)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_ieee_f64le [1]')
    call h5dread_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_ieee_f64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_ieee_f64le [3]')
  end subroutine hdf5_read_6d_array_h5t_ieee_f64le

  subroutine hdf5_read_1d_array_alloc_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),allocatable, dimension(:),intent(out) :: array
    integer(hsize_t),dimension(1) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.1) stop "rank of array is not 1"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1)))
    call hdf5_read_1d_array_h5t_ieee_f64le(handle, path, array)
  end subroutine hdf5_read_1d_array_alloc_h5t_ieee_f64le

  subroutine hdf5_read_2d_array_alloc_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),allocatable, dimension(:,:),intent(out) :: array
    integer(hsize_t),dimension(2) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.2) stop "rank of array is not 2"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2)))
    call hdf5_read_2d_array_h5t_ieee_f64le(handle, path, array)
  end subroutine hdf5_read_2d_array_alloc_h5t_ieee_f64le

  subroutine hdf5_read_3d_array_alloc_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),allocatable, dimension(:,:,:),intent(out) :: array
    integer(hsize_t),dimension(3) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.3) stop "rank of array is not 3"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3)))
    call hdf5_read_3d_array_h5t_ieee_f64le(handle, path, array)
  end subroutine hdf5_read_3d_array_alloc_h5t_ieee_f64le

  subroutine hdf5_read_4d_array_alloc_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),allocatable, dimension(:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(4) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.4) stop "rank of array is not 4"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4)))
    call hdf5_read_4d_array_h5t_ieee_f64le(handle, path, array)
  end subroutine hdf5_read_4d_array_alloc_h5t_ieee_f64le

  subroutine hdf5_read_5d_array_alloc_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),allocatable, dimension(:,:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(5) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.5) stop "rank of array is not 5"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5)))
    call hdf5_read_5d_array_h5t_ieee_f64le(handle, path, array)
  end subroutine hdf5_read_5d_array_alloc_h5t_ieee_f64le

  subroutine hdf5_read_6d_array_alloc_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),allocatable, dimension(:,:,:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(6) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.6) stop "rank of array is not 6"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5), dims(6)))
    call hdf5_read_6d_array_h5t_ieee_f64le(handle, path, array)
  end subroutine hdf5_read_6d_array_alloc_h5t_ieee_f64le

  subroutine hdf5_write_1d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:) :: array
    integer(hsize_t) :: dims(1)
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: hdferr
    dims = [size(array,1)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_1d_array_h5t_ieee_f64le

  subroutine hdf5_write_2d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:,:) :: array
    integer(hsize_t) :: dims(2)
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_2d_array_h5t_ieee_f64le

  subroutine hdf5_write_3d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:,:,:) :: array
    integer(hsize_t) :: dims(3)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_3d_array_h5t_ieee_f64le

  subroutine hdf5_write_4d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:,:,:,:) :: array
    integer(hsize_t) :: dims(4)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_4d_array_h5t_ieee_f64le

  subroutine hdf5_write_5d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:,:,:,:,:) :: array
    integer(hsize_t) :: dims(5)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4), size(array,5)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_5d_array_h5t_ieee_f64le

  subroutine hdf5_write_6d_array_h5t_ieee_f64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(dp),dimension(:,:,:,:,:,:) :: array
    integer(hsize_t) :: dims(6)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4), size(array,5), size(array,6)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_6d_array_h5t_ieee_f64le

  subroutine hdf5_copy_array_h5t_ieee_f64le(handle_in, path_in, handle_out, path_out)

    implicit none

    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out
    real(dp),dimension(:),allocatable :: array

    integer(hsize_t),dimension(:),allocatable :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: rank, hdferr

    ! Read in data into 1D array
    call h5dopen_f(handle_in, path_in, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    allocate(dims(rank), maxdims(rank))
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    ! Can't check hdferr here, because above routine sets it to rank
    allocate(array(product(dims)))
    call h5dread_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)

    ! Write data out
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle_out, path_out, h5t_ieee_f64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)

  end subroutine hdf5_copy_array_h5t_ieee_f64le


  subroutine hdf5_read_1d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:) :: array
    integer(hsize_t) :: dims(1)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_ieee_f32le [1]')
    call h5dread_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_ieee_f32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_ieee_f32le [3]')
  end subroutine hdf5_read_1d_array_h5t_ieee_f32le

  subroutine hdf5_read_2d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:,:) :: array
    integer(hsize_t) :: dims(2)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_ieee_f32le [1]')
    call h5dread_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_ieee_f32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_ieee_f32le [3]')
  end subroutine hdf5_read_2d_array_h5t_ieee_f32le

  subroutine hdf5_read_3d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:,:,:) :: array
    integer(hsize_t) :: dims(3)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_ieee_f32le [1]')
    call h5dread_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_ieee_f32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_ieee_f32le [3]')
  end subroutine hdf5_read_3d_array_h5t_ieee_f32le

  subroutine hdf5_read_4d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:,:,:,:) :: array
    integer(hsize_t) :: dims(4)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_ieee_f32le [1]')
    call h5dread_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_ieee_f32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_ieee_f32le [3]')
  end subroutine hdf5_read_4d_array_h5t_ieee_f32le

  subroutine hdf5_read_5d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:,:,:,:,:) :: array
    integer(hsize_t) :: dims(5)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4),  size(array,5)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_ieee_f32le [1]')
    call h5dread_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_ieee_f32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_ieee_f32le [3]')
  end subroutine hdf5_read_5d_array_h5t_ieee_f32le

  subroutine hdf5_read_6d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:,:,:,:,:,:) :: array
    integer(hsize_t) :: dims(6)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4),  size(array,5),  size(array,6)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_ieee_f32le [1]')
    call h5dread_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_ieee_f32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_ieee_f32le [3]')
  end subroutine hdf5_read_6d_array_h5t_ieee_f32le

  subroutine hdf5_read_1d_array_alloc_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),allocatable, dimension(:),intent(out) :: array
    integer(hsize_t),dimension(1) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.1) stop "rank of array is not 1"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1)))
    call hdf5_read_1d_array_h5t_ieee_f32le(handle, path, array)
  end subroutine hdf5_read_1d_array_alloc_h5t_ieee_f32le

  subroutine hdf5_read_2d_array_alloc_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),allocatable, dimension(:,:),intent(out) :: array
    integer(hsize_t),dimension(2) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.2) stop "rank of array is not 2"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2)))
    call hdf5_read_2d_array_h5t_ieee_f32le(handle, path, array)
  end subroutine hdf5_read_2d_array_alloc_h5t_ieee_f32le

  subroutine hdf5_read_3d_array_alloc_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),allocatable, dimension(:,:,:),intent(out) :: array
    integer(hsize_t),dimension(3) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.3) stop "rank of array is not 3"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3)))
    call hdf5_read_3d_array_h5t_ieee_f32le(handle, path, array)
  end subroutine hdf5_read_3d_array_alloc_h5t_ieee_f32le

  subroutine hdf5_read_4d_array_alloc_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),allocatable, dimension(:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(4) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.4) stop "rank of array is not 4"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4)))
    call hdf5_read_4d_array_h5t_ieee_f32le(handle, path, array)
  end subroutine hdf5_read_4d_array_alloc_h5t_ieee_f32le

  subroutine hdf5_read_5d_array_alloc_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),allocatable, dimension(:,:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(5) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.5) stop "rank of array is not 5"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5)))
    call hdf5_read_5d_array_h5t_ieee_f32le(handle, path, array)
  end subroutine hdf5_read_5d_array_alloc_h5t_ieee_f32le

  subroutine hdf5_read_6d_array_alloc_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),allocatable, dimension(:,:,:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(6) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.6) stop "rank of array is not 6"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5), dims(6)))
    call hdf5_read_6d_array_h5t_ieee_f32le(handle, path, array)
  end subroutine hdf5_read_6d_array_alloc_h5t_ieee_f32le

  subroutine hdf5_write_1d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:) :: array
    integer(hsize_t) :: dims(1)
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: hdferr
    dims = [size(array,1)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_1d_array_h5t_ieee_f32le

  subroutine hdf5_write_2d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:,:) :: array
    integer(hsize_t) :: dims(2)
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_2d_array_h5t_ieee_f32le

  subroutine hdf5_write_3d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:,:,:) :: array
    integer(hsize_t) :: dims(3)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_3d_array_h5t_ieee_f32le

  subroutine hdf5_write_4d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:,:,:,:) :: array
    integer(hsize_t) :: dims(4)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_4d_array_h5t_ieee_f32le

  subroutine hdf5_write_5d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:,:,:,:,:) :: array
    integer(hsize_t) :: dims(5)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4), size(array,5)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_5d_array_h5t_ieee_f32le

  subroutine hdf5_write_6d_array_h5t_ieee_f32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    real(sp),dimension(:,:,:,:,:,:) :: array
    integer(hsize_t) :: dims(6)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4), size(array,5), size(array,6)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_ieee_f32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_6d_array_h5t_ieee_f32le

  subroutine hdf5_copy_array_h5t_ieee_f32le(handle_in, path_in, handle_out, path_out)

    implicit none

    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out
    real(sp),dimension(:),allocatable :: array

    integer(hsize_t),dimension(:),allocatable :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: rank, hdferr

    ! Read in data into 1D array
    call h5dopen_f(handle_in, path_in, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    allocate(dims(rank), maxdims(rank))
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    ! Can't check hdferr here, because above routine sets it to rank
    allocate(array(product(dims)))
    call h5dread_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)

    ! Write data out
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle_out, path_out, h5t_ieee_f32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_ieee_f32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)

  end subroutine hdf5_copy_array_h5t_ieee_f32le


  subroutine hdf5_read_1d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:) :: array
    integer(hsize_t) :: dims(1)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_std_i64le [1]')
    call h5dread_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_std_i64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_std_i64le [3]')
  end subroutine hdf5_read_1d_array_h5t_std_i64le

  subroutine hdf5_read_2d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:,:) :: array
    integer(hsize_t) :: dims(2)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_std_i64le [1]')
    call h5dread_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_std_i64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_std_i64le [3]')
  end subroutine hdf5_read_2d_array_h5t_std_i64le

  subroutine hdf5_read_3d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:,:,:) :: array
    integer(hsize_t) :: dims(3)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_std_i64le [1]')
    call h5dread_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_std_i64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_std_i64le [3]')
  end subroutine hdf5_read_3d_array_h5t_std_i64le

  subroutine hdf5_read_4d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:,:,:,:) :: array
    integer(hsize_t) :: dims(4)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_std_i64le [1]')
    call h5dread_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_std_i64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_std_i64le [3]')
  end subroutine hdf5_read_4d_array_h5t_std_i64le

  subroutine hdf5_read_5d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:,:,:,:,:) :: array
    integer(hsize_t) :: dims(5)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4),  size(array,5)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_std_i64le [1]')
    call h5dread_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_std_i64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_std_i64le [3]')
  end subroutine hdf5_read_5d_array_h5t_std_i64le

  subroutine hdf5_read_6d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:,:,:,:,:,:) :: array
    integer(hsize_t) :: dims(6)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4),  size(array,5),  size(array,6)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_std_i64le [1]')
    call h5dread_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_std_i64le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_std_i64le [3]')
  end subroutine hdf5_read_6d_array_h5t_std_i64le

  subroutine hdf5_read_1d_array_alloc_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),allocatable, dimension(:),intent(out) :: array
    integer(hsize_t),dimension(1) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.1) stop "rank of array is not 1"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1)))
    call hdf5_read_1d_array_h5t_std_i64le(handle, path, array)
  end subroutine hdf5_read_1d_array_alloc_h5t_std_i64le

  subroutine hdf5_read_2d_array_alloc_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),allocatable, dimension(:,:),intent(out) :: array
    integer(hsize_t),dimension(2) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.2) stop "rank of array is not 2"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2)))
    call hdf5_read_2d_array_h5t_std_i64le(handle, path, array)
  end subroutine hdf5_read_2d_array_alloc_h5t_std_i64le

  subroutine hdf5_read_3d_array_alloc_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),allocatable, dimension(:,:,:),intent(out) :: array
    integer(hsize_t),dimension(3) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.3) stop "rank of array is not 3"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3)))
    call hdf5_read_3d_array_h5t_std_i64le(handle, path, array)
  end subroutine hdf5_read_3d_array_alloc_h5t_std_i64le

  subroutine hdf5_read_4d_array_alloc_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),allocatable, dimension(:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(4) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.4) stop "rank of array is not 4"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4)))
    call hdf5_read_4d_array_h5t_std_i64le(handle, path, array)
  end subroutine hdf5_read_4d_array_alloc_h5t_std_i64le

  subroutine hdf5_read_5d_array_alloc_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),allocatable, dimension(:,:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(5) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.5) stop "rank of array is not 5"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5)))
    call hdf5_read_5d_array_h5t_std_i64le(handle, path, array)
  end subroutine hdf5_read_5d_array_alloc_h5t_std_i64le

  subroutine hdf5_read_6d_array_alloc_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),allocatable, dimension(:,:,:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(6) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.6) stop "rank of array is not 6"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5), dims(6)))
    call hdf5_read_6d_array_h5t_std_i64le(handle, path, array)
  end subroutine hdf5_read_6d_array_alloc_h5t_std_i64le

  subroutine hdf5_write_1d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:) :: array
    integer(hsize_t) :: dims(1)
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: hdferr
    dims = [size(array,1)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_1d_array_h5t_std_i64le

  subroutine hdf5_write_2d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:,:) :: array
    integer(hsize_t) :: dims(2)
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_2d_array_h5t_std_i64le

  subroutine hdf5_write_3d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:,:,:) :: array
    integer(hsize_t) :: dims(3)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_3d_array_h5t_std_i64le

  subroutine hdf5_write_4d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:,:,:,:) :: array
    integer(hsize_t) :: dims(4)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_4d_array_h5t_std_i64le

  subroutine hdf5_write_5d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:,:,:,:,:) :: array
    integer(hsize_t) :: dims(5)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4), size(array,5)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_5d_array_h5t_std_i64le

  subroutine hdf5_write_6d_array_h5t_std_i64le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(idp),dimension(:,:,:,:,:,:) :: array
    integer(hsize_t) :: dims(6)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4), size(array,5), size(array,6)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_6d_array_h5t_std_i64le

  subroutine hdf5_copy_array_h5t_std_i64le(handle_in, path_in, handle_out, path_out)

    implicit none

    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out
    integer(idp),dimension(:),allocatable :: array

    integer(hsize_t),dimension(:),allocatable :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: rank, hdferr

    ! Read in data into 1D array
    call h5dopen_f(handle_in, path_in, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    allocate(dims(rank), maxdims(rank))
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    ! Can't check hdferr here, because above routine sets it to rank
    allocate(array(product(dims)))
    call h5dread_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)

    ! Write data out
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle_out, path_out, h5t_std_i64le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i64le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)

  end subroutine hdf5_copy_array_h5t_std_i64le


  subroutine hdf5_read_1d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:) :: array
    integer(hsize_t) :: dims(1)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_std_i32le [1]')
    call h5dread_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_std_i32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_1d_array_h5t_std_i32le [3]')
  end subroutine hdf5_read_1d_array_h5t_std_i32le

  subroutine hdf5_read_2d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:,:) :: array
    integer(hsize_t) :: dims(2)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_std_i32le [1]')
    call h5dread_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_std_i32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_2d_array_h5t_std_i32le [3]')
  end subroutine hdf5_read_2d_array_h5t_std_i32le

  subroutine hdf5_read_3d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:,:,:) :: array
    integer(hsize_t) :: dims(3)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_std_i32le [1]')
    call h5dread_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_std_i32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_3d_array_h5t_std_i32le [3]')
  end subroutine hdf5_read_3d_array_h5t_std_i32le

  subroutine hdf5_read_4d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:,:,:,:) :: array
    integer(hsize_t) :: dims(4)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_std_i32le [1]')
    call h5dread_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_std_i32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_4d_array_h5t_std_i32le [3]')
  end subroutine hdf5_read_4d_array_h5t_std_i32le

  subroutine hdf5_read_5d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:,:,:,:,:) :: array
    integer(hsize_t) :: dims(5)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4),  size(array,5)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_std_i32le [1]')
    call h5dread_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_std_i32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_5d_array_h5t_std_i32le [3]')
  end subroutine hdf5_read_5d_array_h5t_std_i32le

  subroutine hdf5_read_6d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:,:,:,:,:,:) :: array
    integer(hsize_t) :: dims(6)
    integer(hid_t) :: dset_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4),  size(array,5),  size(array,6)]
    call h5dopen_f(handle, path, dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_std_i32le [1]')
    call h5dread_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_std_i32le [2]')
    call h5dclose_f(dset_id, hdferr)
    call check_status(hdferr,'hdf5_read_6d_array_h5t_std_i32le [3]')
  end subroutine hdf5_read_6d_array_h5t_std_i32le

  subroutine hdf5_read_1d_array_alloc_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,allocatable, dimension(:),intent(out) :: array
    integer(hsize_t),dimension(1) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.1) stop "rank of array is not 1"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1)))
    call hdf5_read_1d_array_h5t_std_i32le(handle, path, array)
  end subroutine hdf5_read_1d_array_alloc_h5t_std_i32le

  subroutine hdf5_read_2d_array_alloc_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,allocatable, dimension(:,:),intent(out) :: array
    integer(hsize_t),dimension(2) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.2) stop "rank of array is not 2"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2)))
    call hdf5_read_2d_array_h5t_std_i32le(handle, path, array)
  end subroutine hdf5_read_2d_array_alloc_h5t_std_i32le

  subroutine hdf5_read_3d_array_alloc_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,allocatable, dimension(:,:,:),intent(out) :: array
    integer(hsize_t),dimension(3) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.3) stop "rank of array is not 3"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3)))
    call hdf5_read_3d_array_h5t_std_i32le(handle, path, array)
  end subroutine hdf5_read_3d_array_alloc_h5t_std_i32le

  subroutine hdf5_read_4d_array_alloc_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,allocatable, dimension(:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(4) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.4) stop "rank of array is not 4"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4)))
    call hdf5_read_4d_array_h5t_std_i32le(handle, path, array)
  end subroutine hdf5_read_4d_array_alloc_h5t_std_i32le

  subroutine hdf5_read_5d_array_alloc_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,allocatable, dimension(:,:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(5) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.5) stop "rank of array is not 5"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5)))
    call hdf5_read_5d_array_h5t_std_i32le(handle, path, array)
  end subroutine hdf5_read_5d_array_alloc_h5t_std_i32le

  subroutine hdf5_read_6d_array_alloc_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,allocatable, dimension(:,:,:,:,:,:),intent(out) :: array
    integer(hsize_t),dimension(6) :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id
    integer :: rank, hdferr
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    if(rank.ne.6) stop "rank of array is not 6"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    allocate(array(dims(1), dims(2), dims(3), dims(4), dims(5), dims(6)))
    call hdf5_read_6d_array_h5t_std_i32le(handle, path, array)
  end subroutine hdf5_read_6d_array_alloc_h5t_std_i32le

  subroutine hdf5_write_1d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:) :: array
    integer(hsize_t) :: dims(1)
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: hdferr
    dims = [size(array,1)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_1d_array_h5t_std_i32le

  subroutine hdf5_write_2d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:,:) :: array
    integer(hsize_t) :: dims(2)
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_2d_array_h5t_std_i32le

  subroutine hdf5_write_3d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:,:,:) :: array
    integer(hsize_t) :: dims(3)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_3d_array_h5t_std_i32le

  subroutine hdf5_write_4d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:,:,:,:) :: array
    integer(hsize_t) :: dims(4)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_4d_array_h5t_std_i32le

  subroutine hdf5_write_5d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:,:,:,:,:) :: array
    integer(hsize_t) :: dims(5)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4), size(array,5)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_5d_array_h5t_std_i32le

  subroutine hdf5_write_6d_array_h5t_std_i32le(handle, path, array)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer,dimension(:,:,:,:,:,:) :: array
    integer(hsize_t) :: dims(6)
    integer(hid_t) :: dspace_id, dset_id,dprop_id
    integer :: hdferr
    dims = [size(array,1), size(array,2), size(array,3), size(array,4), size(array,5), size(array,6)]
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle, path, h5t_std_i32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)
  end subroutine hdf5_write_6d_array_h5t_std_i32le

  subroutine hdf5_copy_array_h5t_std_i32le(handle_in, path_in, handle_out, path_out)

    implicit none

    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out
    integer,dimension(:),allocatable :: array

    integer(hsize_t),dimension(:),allocatable :: dims, maxdims
    integer(hid_t) :: dspace_id, dset_id, dprop_id
    integer :: rank, hdferr

    ! Read in data into 1D array
    call h5dopen_f(handle_in, path_in, dset_id, hdferr)
    call h5dget_space_f(dset_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    allocate(dims(rank), maxdims(rank))
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    ! Can't check hdferr here, because above routine sets it to rank
    allocate(array(product(dims)))
    call h5dread_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)

    ! Write data out
    call h5screate_simple_f(size(dims), dims, dspace_id, hdferr)
    call h5pcreate_f(h5p_dataset_create_f, dprop_id, hdferr)
    call h5pset_chunk_f(dprop_id, size(dims), dims, hdferr)
    if(compress) call h5pset_deflate_f(dprop_id, 9, hdferr)
    call h5dcreate_f(handle_out, path_out, h5t_std_i32le, dspace_id, dset_id, hdferr, dprop_id)
    call h5dwrite_f(dset_id, h5t_std_i32le, array, dims, hdferr)
    call h5dclose_f(dset_id, hdferr)
    call h5pclose_f(dprop_id, hdferr)
    call h5sclose_f(dspace_id, hdferr)

  end subroutine hdf5_copy_array_h5t_std_i32le


  subroutine hdf5_copy_array(handle_in, path_in, handle_out, path_out)

    implicit none

    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out
    integer(hid_t) :: dset_id, datatype_id
    integer :: hdferr
    logical :: is_h5t_std_i32le
    logical :: is_h5t_std_i64le
    logical :: is_h5t_ieee_f32le
    logical :: is_h5t_ieee_f64le

    ! Read in array type
    call h5dopen_f(handle_in, path_in, dset_id, hdferr)
    call h5dget_type_f(dset_id, datatype_id, hdferr)

    ! Check the type of the array to copy
    call h5tequal_f(datatype_id, h5t_std_i32le, is_h5t_std_i32le, hdferr)
    call h5tequal_f(datatype_id, h5t_std_i64le, is_h5t_std_i64le, hdferr)
    call h5tequal_f(datatype_id, h5t_ieee_f32le, is_h5t_ieee_f32le, hdferr)
    call h5tequal_f(datatype_id, h5t_ieee_f64le, is_h5t_ieee_f64le, hdferr)

    ! Close the datatype
    call h5tclose_f(datatype_id, hdferr)

    ! Copy over the array using the appropriate routine
    if(is_h5t_std_i32le) then
       call hdf5_copy_array_h5t_std_i32le(handle_in, path_in, handle_out, path_out)
    else if(is_h5t_std_i64le) then
       call hdf5_copy_array_h5t_std_i64le(handle_in, path_in, handle_out, path_out)
    else if(is_h5t_ieee_f32le) then
       call hdf5_copy_array_h5t_ieee_f32le(handle_in, path_in, handle_out, path_out)
    else if(is_h5t_ieee_f64le) then
       call hdf5_copy_array_h5t_ieee_f64le(handle_in, path_in, handle_out, path_out)
    else
       write(0,'("ERROR: unknown datatype ", I0)') datatype_id
       stop
    end if

  end subroutine hdf5_copy_array

  logical function hdf5_exists_keyword(handle, path, name)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer :: hdferr
    call h5aexists_by_name_f(handle, path, name, hdf5_exists_keyword, hdferr)
  end function hdf5_exists_keyword

  subroutine hdf5_check_exists_keyword(handle, path, name)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    if(.not.hdf5_exists_keyword(handle, path, name)) then
       write(0,*) "ERROR: attribute "//trim(name)//" does not exist in HDF5 file"
       stop
    end if
  end subroutine hdf5_check_exists_keyword

  subroutine hdf5_read_k_logical(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    logical,intent(out) :: value
    character(len=3) :: string_value
    call hdf5_check_exists_keyword(handle, path, name)
    call hdf5_read_k_string(handle, path, name, string_value)
    select case(trim(string_value))
    case('yes','y','Yes','YES','1','T')
       value = .true.
    case('no','n','No','NO','0','F')
       value = .false.
    case default
       write(*,*) "Unknown logical string "//string_value
       stop
    end select
  end subroutine hdf5_read_k_logical

  subroutine hdf5_read_k_string(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id
    character(len=*),intent(out) :: value
    integer :: hdferr
    integer(hid_t) :: atype_id
    call hdf5_check_exists_keyword(handle, path, name)
    call h5tcopy_f(h5t_native_character, atype_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_string [1]')
    call h5tset_size_f(atype_id, int(len(value),size_t), hdferr)
    call check_status(hdferr,'hdf5_read_k_string [2]')
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_string [3]')
    call h5aread_f(attr_id, atype_id, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_read_k_string [4]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_string [5]')
  end subroutine hdf5_read_k_string

  subroutine hdf5_write_k_logical(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    logical,intent(in) :: value
    if (value) then
       call hdf5_write_k_string(handle, path, name, 'yes')
    else
       call hdf5_write_k_string(handle, path, name, 'no')
    end if
  end subroutine hdf5_write_k_logical

  subroutine hdf5_write_k_string(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    character(len=*),intent(in) :: value
    integer(hid_t) :: dspace_id, attr_id
    integer :: hdferr
    integer(hid_t) :: atype_id
    call h5tcopy_f(h5t_native_character, atype_id, hdferr)
    call check_status(hdferr,'hdf5_write_k_string [1]')
    call h5tset_size_f(atype_id, int(len(value),size_t), hdferr)
    call check_status(hdferr,'hdf5_write_k_string [2]')
    if(hdf5_exists_keyword(handle, path, name)) then
       call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_string [3]')
    else
       call h5screate_f(h5s_scalar_f, dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_string [4]')
       call h5acreate_by_name_f(handle, path, name, atype_id, dspace_id, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_string [5]')
       call h5sclose_f(dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_string [6]')
    end if
    call h5awrite_f(attr_id, atype_id, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_write_k_string [7]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_write_k_string [8]')
  end subroutine hdf5_write_k_string

  subroutine hdf5_copy_k_string(handle_in, path_in, name_in, handle_out, path_out, name_out)
    implicit none
    integer(hid_t) :: handle_in, handle_out
    character(len=*) :: path_in, name_in, path_out, name_out
    character(len=1000) :: value
    call hdf5_read_k_string(handle_in, path_in, name_in, value)
    call hdf5_write_k_string(handle_out, path_out, name_out, value)
  end subroutine hdf5_copy_k_string


  subroutine hdf5_read_k_h5t_ieee_f64le(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id
    real(dp),intent(out) :: value
    integer :: hdferr
    call hdf5_check_exists_keyword(handle, path, name)
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f64le [1]')
    call h5aread_f(attr_id, h5t_ieee_f64le, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f64le [2]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f64le [3]')
  end subroutine hdf5_read_k_h5t_ieee_f64le

  subroutine hdf5_write_k_h5t_ieee_f64le(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    real(dp),intent(in) :: value
    integer(hid_t) :: dspace_id, attr_id
    integer :: hdferr
    if(hdf5_exists_keyword(handle, path, name)) then
       call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [1]')
    else
       call h5screate_f(h5s_scalar_f, dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [1]')
       call h5acreate_by_name_f(handle, path, name, h5t_ieee_f64le, dspace_id, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [2]')
       call h5sclose_f(dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [3]')
    end if
    call h5awrite_f(attr_id, h5t_ieee_f64le, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [4]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [5]')
  end subroutine hdf5_write_k_h5t_ieee_f64le

  subroutine hdf5_copy_k_h5t_ieee_f64le(handle_in, path_in, name_in, handle_out, path_out, name_out)
    implicit none
    integer(hid_t) :: handle_in, handle_out
    character(len=*) :: path_in, name_in, path_out, name_out
    real(dp) :: value
    call hdf5_read_k_h5t_ieee_f64le(handle_in, path_in, name_in, value)
    call hdf5_write_k_h5t_ieee_f64le(handle_out, path_out, name_out, value)
  end subroutine hdf5_copy_k_h5t_ieee_f64le

  subroutine hdf5_read_k_vector_alloc_h5t_ieee_f64le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id, dspace_id
    real(dp),intent(out),allocatable :: values(:)
    integer :: hdferr, rank, ndims
    integer(hsize_t) :: dims(1), maxdims(1)

    ! Check that keyword exists
    call hdf5_check_exists_keyword(handle, path, name)

    ! Open the attribute
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_ieee_f64le [1]')

    ! Get dataspace and find dimensions
    call h5aget_space_f(attr_id, dspace_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_ieee_f64le [2]')
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_ieee_f64le [3]')
    if(rank.ne.1) stop "rank of array is not 1"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    ! failure is only hdferr = -1
    allocate(values(dims(1)))

    ! Read in the values
    call hdf5_read_k_vector_h5t_ieee_f64le(handle,path,name,values)

  end subroutine hdf5_read_k_vector_alloc_h5t_ieee_f64le

  subroutine hdf5_read_k_vector_h5t_ieee_f64le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id
    real(dp),intent(out) :: values(:)
    integer :: hdferr
    call hdf5_check_exists_keyword(handle, path, name)
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f64le [1]')
    call h5aread_f(attr_id, h5t_ieee_f64le, values, int(shape(values), hsize_t), hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f64le [2]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f64le [3]')
  end subroutine hdf5_read_k_vector_h5t_ieee_f64le

  subroutine hdf5_write_k_vector_h5t_ieee_f64le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    real(dp),intent(in) :: values(:)
    integer(hid_t) :: dspace_id, attr_id
    integer :: hdferr
    if(hdf5_exists_keyword(handle, path, name)) then
       call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [1]')
    else
       call h5screate_simple_f(1, int(shape(values), hsize_t), dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [1]')
       call h5acreate_by_name_f(handle, path, name, h5t_ieee_f64le, dspace_id, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [2]')
       call h5sclose_f(dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [3]')
    end if
    call h5awrite_f(attr_id, h5t_ieee_f64le, values, int(shape(values), hsize_t), hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [4]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_ieee_f64le [5]')
  end subroutine hdf5_write_k_vector_h5t_ieee_f64le

  subroutine hdf5_copy_k_vector_h5t_ieee_f64le(handle_in, path_in, name_in, handle_out, path_out, name_out)
    implicit none
    integer(hid_t) :: handle_in, handle_out
    character(len=*) :: path_in, name_in, path_out, name_out
    real(dp),allocatable :: values(:)
    call hdf5_read_k_vector_alloc_h5t_ieee_f64le(handle_in, path_in, name_in, values)
    call hdf5_write_k_vector_h5t_ieee_f64le(handle_out, path_out, name_out, values)
  end subroutine hdf5_copy_k_vector_h5t_ieee_f64le


  subroutine hdf5_read_k_h5t_ieee_f32le(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id
    real(sp),intent(out) :: value
    integer :: hdferr
    call hdf5_check_exists_keyword(handle, path, name)
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f32le [1]')
    call h5aread_f(attr_id, h5t_ieee_f32le, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f32le [2]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f32le [3]')
  end subroutine hdf5_read_k_h5t_ieee_f32le

  subroutine hdf5_write_k_h5t_ieee_f32le(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    real(sp),intent(in) :: value
    integer(hid_t) :: dspace_id, attr_id
    integer :: hdferr
    if(hdf5_exists_keyword(handle, path, name)) then
       call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [1]')
    else
       call h5screate_f(h5s_scalar_f, dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [1]')
       call h5acreate_by_name_f(handle, path, name, h5t_ieee_f32le, dspace_id, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [2]')
       call h5sclose_f(dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [3]')
    end if
    call h5awrite_f(attr_id, h5t_ieee_f32le, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [4]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [5]')
  end subroutine hdf5_write_k_h5t_ieee_f32le

  subroutine hdf5_copy_k_h5t_ieee_f32le(handle_in, path_in, name_in, handle_out, path_out, name_out)
    implicit none
    integer(hid_t) :: handle_in, handle_out
    character(len=*) :: path_in, name_in, path_out, name_out
    real(sp) :: value
    call hdf5_read_k_h5t_ieee_f32le(handle_in, path_in, name_in, value)
    call hdf5_write_k_h5t_ieee_f32le(handle_out, path_out, name_out, value)
  end subroutine hdf5_copy_k_h5t_ieee_f32le

  subroutine hdf5_read_k_vector_alloc_h5t_ieee_f32le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id, dspace_id
    real(sp),intent(out),allocatable :: values(:)
    integer :: hdferr, rank, ndims
    integer(hsize_t) :: dims(1), maxdims(1)

    ! Check that keyword exists
    call hdf5_check_exists_keyword(handle, path, name)

    ! Open the attribute
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_ieee_f32le [1]')

    ! Get dataspace and find dimensions
    call h5aget_space_f(attr_id, dspace_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_ieee_f32le [2]')
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_ieee_f32le [3]')
    if(rank.ne.1) stop "rank of array is not 1"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    ! failure is only hdferr = -1
    allocate(values(dims(1)))

    ! Read in the values
    call hdf5_read_k_vector_h5t_ieee_f32le(handle,path,name,values)

  end subroutine hdf5_read_k_vector_alloc_h5t_ieee_f32le

  subroutine hdf5_read_k_vector_h5t_ieee_f32le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id
    real(sp),intent(out) :: values(:)
    integer :: hdferr
    call hdf5_check_exists_keyword(handle, path, name)
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f32le [1]')
    call h5aread_f(attr_id, h5t_ieee_f32le, values, int(shape(values), hsize_t), hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f32le [2]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_ieee_f32le [3]')
  end subroutine hdf5_read_k_vector_h5t_ieee_f32le

  subroutine hdf5_write_k_vector_h5t_ieee_f32le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    real(sp),intent(in) :: values(:)
    integer(hid_t) :: dspace_id, attr_id
    integer :: hdferr
    if(hdf5_exists_keyword(handle, path, name)) then
       call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [1]')
    else
       call h5screate_simple_f(1, int(shape(values), hsize_t), dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [1]')
       call h5acreate_by_name_f(handle, path, name, h5t_ieee_f32le, dspace_id, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [2]')
       call h5sclose_f(dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [3]')
    end if
    call h5awrite_f(attr_id, h5t_ieee_f32le, values, int(shape(values), hsize_t), hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [4]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_ieee_f32le [5]')
  end subroutine hdf5_write_k_vector_h5t_ieee_f32le

  subroutine hdf5_copy_k_vector_h5t_ieee_f32le(handle_in, path_in, name_in, handle_out, path_out, name_out)
    implicit none
    integer(hid_t) :: handle_in, handle_out
    character(len=*) :: path_in, name_in, path_out, name_out
    real(sp),allocatable :: values(:)
    call hdf5_read_k_vector_alloc_h5t_ieee_f32le(handle_in, path_in, name_in, values)
    call hdf5_write_k_vector_h5t_ieee_f32le(handle_out, path_out, name_out, values)
  end subroutine hdf5_copy_k_vector_h5t_ieee_f32le


  subroutine hdf5_read_k_h5t_std_i64le(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id
    integer(idp),intent(out) :: value
    integer :: hdferr
    call hdf5_check_exists_keyword(handle, path, name)
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i64le [1]')
    call h5aread_f(attr_id, h5t_std_i64le, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i64le [2]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i64le [3]')
  end subroutine hdf5_read_k_h5t_std_i64le

  subroutine hdf5_write_k_h5t_std_i64le(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(idp),intent(in) :: value
    integer(hid_t) :: dspace_id, attr_id
    integer :: hdferr
    if(hdf5_exists_keyword(handle, path, name)) then
       call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [1]')
    else
       call h5screate_f(h5s_scalar_f, dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [1]')
       call h5acreate_by_name_f(handle, path, name, h5t_std_i64le, dspace_id, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [2]')
       call h5sclose_f(dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [3]')
    end if
    call h5awrite_f(attr_id, h5t_std_i64le, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [4]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [5]')
  end subroutine hdf5_write_k_h5t_std_i64le

  subroutine hdf5_copy_k_h5t_std_i64le(handle_in, path_in, name_in, handle_out, path_out, name_out)
    implicit none
    integer(hid_t) :: handle_in, handle_out
    character(len=*) :: path_in, name_in, path_out, name_out
    integer(idp) :: value
    call hdf5_read_k_h5t_std_i64le(handle_in, path_in, name_in, value)
    call hdf5_write_k_h5t_std_i64le(handle_out, path_out, name_out, value)
  end subroutine hdf5_copy_k_h5t_std_i64le

  subroutine hdf5_read_k_vector_alloc_h5t_std_i64le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id, dspace_id
    integer(idp),intent(out),allocatable :: values(:)
    integer :: hdferr, rank, ndims
    integer(hsize_t) :: dims(1), maxdims(1)

    ! Check that keyword exists
    call hdf5_check_exists_keyword(handle, path, name)

    ! Open the attribute
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_std_i64le [1]')

    ! Get dataspace and find dimensions
    call h5aget_space_f(attr_id, dspace_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_std_i64le [2]')
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_std_i64le [3]')
    if(rank.ne.1) stop "rank of array is not 1"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    ! failure is only hdferr = -1
    allocate(values(dims(1)))

    ! Read in the values
    call hdf5_read_k_vector_h5t_std_i64le(handle,path,name,values)

  end subroutine hdf5_read_k_vector_alloc_h5t_std_i64le

  subroutine hdf5_read_k_vector_h5t_std_i64le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id
    integer(idp),intent(out) :: values(:)
    integer :: hdferr
    call hdf5_check_exists_keyword(handle, path, name)
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i64le [1]')
    call h5aread_f(attr_id, h5t_std_i64le, values, int(shape(values), hsize_t), hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i64le [2]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i64le [3]')
  end subroutine hdf5_read_k_vector_h5t_std_i64le

  subroutine hdf5_write_k_vector_h5t_std_i64le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(idp),intent(in) :: values(:)
    integer(hid_t) :: dspace_id, attr_id
    integer :: hdferr
    if(hdf5_exists_keyword(handle, path, name)) then
       call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [1]')
    else
       call h5screate_simple_f(1, int(shape(values), hsize_t), dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [1]')
       call h5acreate_by_name_f(handle, path, name, h5t_std_i64le, dspace_id, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [2]')
       call h5sclose_f(dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [3]')
    end if
    call h5awrite_f(attr_id, h5t_std_i64le, values, int(shape(values), hsize_t), hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [4]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_std_i64le [5]')
  end subroutine hdf5_write_k_vector_h5t_std_i64le

  subroutine hdf5_copy_k_vector_h5t_std_i64le(handle_in, path_in, name_in, handle_out, path_out, name_out)
    implicit none
    integer(hid_t) :: handle_in, handle_out
    character(len=*) :: path_in, name_in, path_out, name_out
    integer(idp),allocatable :: values(:)
    call hdf5_read_k_vector_alloc_h5t_std_i64le(handle_in, path_in, name_in, values)
    call hdf5_write_k_vector_h5t_std_i64le(handle_out, path_out, name_out, values)
  end subroutine hdf5_copy_k_vector_h5t_std_i64le


  subroutine hdf5_read_k_h5t_std_i32le(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id
    integer,intent(out) :: value
    integer :: hdferr
    call hdf5_check_exists_keyword(handle, path, name)
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i32le [1]')
    call h5aread_f(attr_id, h5t_std_i32le, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i32le [2]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i32le [3]')
  end subroutine hdf5_read_k_h5t_std_i32le

  subroutine hdf5_write_k_h5t_std_i32le(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer,intent(in) :: value
    integer(hid_t) :: dspace_id, attr_id
    integer :: hdferr
    if(hdf5_exists_keyword(handle, path, name)) then
       call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [1]')
    else
       call h5screate_f(h5s_scalar_f, dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [1]')
       call h5acreate_by_name_f(handle, path, name, h5t_std_i32le, dspace_id, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [2]')
       call h5sclose_f(dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [3]')
    end if
    call h5awrite_f(attr_id, h5t_std_i32le, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [4]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [5]')
  end subroutine hdf5_write_k_h5t_std_i32le

  subroutine hdf5_copy_k_h5t_std_i32le(handle_in, path_in, name_in, handle_out, path_out, name_out)
    implicit none
    integer(hid_t) :: handle_in, handle_out
    character(len=*) :: path_in, name_in, path_out, name_out
    integer :: value
    call hdf5_read_k_h5t_std_i32le(handle_in, path_in, name_in, value)
    call hdf5_write_k_h5t_std_i32le(handle_out, path_out, name_out, value)
  end subroutine hdf5_copy_k_h5t_std_i32le

  subroutine hdf5_read_k_vector_alloc_h5t_std_i32le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id, dspace_id
    integer,intent(out),allocatable :: values(:)
    integer :: hdferr, rank, ndims
    integer(hsize_t) :: dims(1), maxdims(1)

    ! Check that keyword exists
    call hdf5_check_exists_keyword(handle, path, name)

    ! Open the attribute
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_std_i32le [1]')

    ! Get dataspace and find dimensions
    call h5aget_space_f(attr_id, dspace_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_std_i32le [2]')
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    call check_status(hdferr,'hdf5_read_k_vector_alloc_h5t_std_i32le [3]')
    if(rank.ne.1) stop "rank of array is not 1"
    call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)
    ! failure is only hdferr = -1
    allocate(values(dims(1)))

    ! Read in the values
    call hdf5_read_k_vector_h5t_std_i32le(handle,path,name,values)

  end subroutine hdf5_read_k_vector_alloc_h5t_std_i32le

  subroutine hdf5_read_k_vector_h5t_std_i32le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer(hid_t) :: attr_id
    integer,intent(out) :: values(:)
    integer :: hdferr
    call hdf5_check_exists_keyword(handle, path, name)
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i32le [1]')
    call h5aread_f(attr_id, h5t_std_i32le, values, int(shape(values), hsize_t), hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i32le [2]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_h5t_std_i32le [3]')
  end subroutine hdf5_read_k_vector_h5t_std_i32le

  subroutine hdf5_write_k_vector_h5t_std_i32le(handle,path,name,values)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    integer,intent(in) :: values(:)
    integer(hid_t) :: dspace_id, attr_id
    integer :: hdferr
    if(hdf5_exists_keyword(handle, path, name)) then
       call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [1]')
    else
       call h5screate_simple_f(1, int(shape(values), hsize_t), dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [1]')
       call h5acreate_by_name_f(handle, path, name, h5t_std_i32le, dspace_id, attr_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [2]')
       call h5sclose_f(dspace_id, hdferr)
       call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [3]')
    end if
    call h5awrite_f(attr_id, h5t_std_i32le, values, int(shape(values), hsize_t), hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [4]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_write_k_h5t_std_i32le [5]')
  end subroutine hdf5_write_k_vector_h5t_std_i32le

  subroutine hdf5_copy_k_vector_h5t_std_i32le(handle_in, path_in, name_in, handle_out, path_out, name_out)
    implicit none
    integer(hid_t) :: handle_in, handle_out
    character(len=*) :: path_in, name_in, path_out, name_out
    integer,allocatable :: values(:)
    call hdf5_read_k_vector_alloc_h5t_std_i32le(handle_in, path_in, name_in, values)
    call hdf5_write_k_vector_h5t_std_i32le(handle_out, path_out, name_out, values)
  end subroutine hdf5_copy_k_vector_h5t_std_i32le


  subroutine hdf5_copy_keyword(handle_in, path_in, attribute_in, handle_out, path_out, attribute_out)

    implicit none

    integer(hid_t),intent(in) :: handle_in, handle_out
    character(len=*),intent(in) :: path_in, path_out
    character(len=*),intent(in) :: attribute_in, attribute_out
    integer(hid_t) :: attr_id, datatype_id, dspace_id
    integer :: hdferr, rank
    logical :: is_string, is_vector
    logical :: is_h5t_std_i32le
    logical :: is_h5t_std_i64le
    logical :: is_h5t_ieee_f32le
    logical :: is_h5t_ieee_f64le
    integer :: class

    ! Read in keyword type
    call h5aopen_by_name_f(handle_in, path_in, attribute_in, attr_id, hdferr)
    call h5aget_type_f(attr_id, datatype_id, hdferr)
    call h5aget_space_f(attr_id, dspace_id, hdferr)
    call h5sget_simple_extent_ndims_f(dspace_id, rank, hdferr)
    call h5aclose_f(attr_id, hdferr)

    ! Check the type of the array to copy
    call h5tget_class_f(datatype_id, class, hdferr)
    is_string = class == h5t_string_f
    call h5tequal_f(datatype_id, h5t_std_i32le, is_h5t_std_i32le, hdferr)
    call h5tequal_f(datatype_id, h5t_std_i64le, is_h5t_std_i64le, hdferr)
    call h5tequal_f(datatype_id, h5t_ieee_f32le, is_h5t_ieee_f32le, hdferr)
    call h5tequal_f(datatype_id, h5t_ieee_f64le, is_h5t_ieee_f64le, hdferr)

    ! Close the datatype
    call h5tclose_f(datatype_id, hdferr)

    ! Copy over the keyword using the appropriate routine
    if(rank == 0) then
       if(is_string) then
          call hdf5_copy_k_string(handle_in, path_in, attribute_in, handle_out, path_out, attribute_out)
       else if(is_h5t_std_i32le) then
          call hdf5_copy_k_h5t_std_i32le(handle_in, path_in, attribute_in, handle_out, path_out, attribute_out)
       else if(is_h5t_std_i64le) then
          call hdf5_copy_k_h5t_std_i64le(handle_in, path_in, attribute_in, handle_out, path_out, attribute_out)
       else if(is_h5t_ieee_f32le) then
          call hdf5_copy_k_h5t_ieee_f32le(handle_in, path_in, attribute_in, handle_out, path_out, attribute_out)
       else if(is_h5t_ieee_f64le) then
          call hdf5_copy_k_h5t_ieee_f64le(handle_in, path_in, attribute_in, handle_out, path_out, attribute_out)
       else
          write(0,'("ERROR: unknown datatype ", I0)') datatype_id
          stop
       end if
    else
       if(is_h5t_std_i32le) then
          call hdf5_copy_k_vector_h5t_std_i32le(handle_in, path_in, attribute_in, handle_out, path_out, attribute_out)
       else if(is_h5t_std_i64le) then
          call hdf5_copy_k_vector_h5t_std_i64le(handle_in, path_in, attribute_in, handle_out, path_out, attribute_out)
       else if(is_h5t_ieee_f32le) then
          call hdf5_copy_k_vector_h5t_ieee_f32le(handle_in, path_in, attribute_in, handle_out, path_out, attribute_out)
       else if(is_h5t_ieee_f64le) then
          call hdf5_copy_k_vector_h5t_ieee_f64le(handle_in, path_in, attribute_in, handle_out, path_out, attribute_out)
       else
          write(0,'("ERROR: unknown datatype ", I0)') datatype_id
          stop
       end if
    end if

  end subroutine hdf5_copy_keyword

  type(table_info) function hdf5_read_table_info(handle, path) result(info)

    implicit none

    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer(hid_t) :: dset_id, datatype_id
    integer :: field_idx, hdferr

    ! Find out the overall table info
    call h5tbget_table_info_f(handle, path, info%n_cols, info%n_rows, hdferr)

    allocate(info%field_names(info%n_cols))
    allocate(info%field_sizes(info%n_cols))
    allocate(info%field_offsets(info%n_cols))
    allocate(info%field_types(info%n_cols))

    ! Read the default information
    call h5tbget_field_info_f(handle, path, info%n_cols, info%field_names,&
         & info%field_sizes, info%field_offsets, info%type_size, hdferr)
    call check_status(hdferr,'hdf5_read_table_info')

    ! Work out the datatypes for the columns
    call h5dopen_f(handle, path, dset_id, hdferr)
    call h5dget_type_f(dset_id, datatype_id, hdferr)
    do field_idx=1,info%n_cols
       call h5tget_member_type_f(datatype_id, field_idx-1, info%field_types(field_idx), hdferr)
    end do
    call h5tclose_f(datatype_id, hdferr)
    call h5dclose_f(dset_id, hdferr)

  end function hdf5_read_table_info

  integer function hdf5_table_column_number(info, col_name) result(col_id)
    implicit none
    type(table_info),intent(in) :: info
    character(len=*),intent(in) :: col_name
    do col_id=1,info%n_cols
       if(trim(adjustl(info%field_names(col_id))) == trim(col_name)) exit
    end do
    if(col_id==info%n_cols+1) then
       write(0,'("Column not found", A)') trim(col_name)
       stop
    end if
  end function hdf5_table_column_number

  !   subroutine hdf5_table_new_column(unit,colnum,ttype,tform)
  !     implicit none
  !     integer,intent(in)          :: unit,colnum
  !     character(len=*),intent(in) :: ttype,tform
  !     integer                     :: status
  !     status = 0
  !     call fticol(unit,colnum,ttype,tform,status)
  !     call check_status(hdferr,'hdf5_table_new_column')
  !   end subroutine hdf5_table_new_column

  ! code required to get 64-bit integers to work

  subroutine h5aread_f_i64(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(out) :: buf
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real
    call h5aread_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
    buf = int(buf_real, idp)
  end subroutine h5aread_f_i64

  subroutine h5awrite_f_i64(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(in) :: buf
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real
    buf_real = real(buf, dp)
    call h5awrite_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
  end subroutine h5awrite_f_i64

  subroutine h5aread_f_i64_vector(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(out) :: buf(:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf))
    call h5aread_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
    buf = int(buf_real, idp)
  end subroutine h5aread_f_i64_vector

  subroutine h5awrite_f_i64_vector(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(in) :: buf(:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf))
    buf_real = real(buf, dp)
    call h5awrite_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
  end subroutine h5awrite_f_i64_vector

  subroutine h5dread_f_i64_scalar(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(out) :: buf
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real
    call h5dread_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
    buf = int(buf_real, idp)
  end subroutine h5dread_f_i64_scalar

  subroutine h5dread_f_i64_1d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(out) :: buf(:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1))
    call h5dread_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
    buf = int(buf_real, idp)
  end subroutine h5dread_f_i64_1d

  subroutine h5dread_f_i64_2d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(out) :: buf(:,:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1),size(buf,2))
    call h5dread_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
    buf = int(buf_real, idp)
  end subroutine h5dread_f_i64_2d

  subroutine h5dread_f_i64_3d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(out) :: buf(:,:,:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1),size(buf,2),size(buf,3))
    call h5dread_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
    buf = int(buf_real, idp)
  end subroutine h5dread_f_i64_3d

  subroutine h5dread_f_i64_4d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(out) :: buf(:,:,:,:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1),size(buf,2),size(buf,3),size(buf,4))
    call h5dread_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
    buf = int(buf_real, idp)
  end subroutine h5dread_f_i64_4d

  subroutine h5dread_f_i64_5d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(out) :: buf(:,:,:,:,:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1),size(buf,2),size(buf,3),size(buf,4),size(buf,5))
    call h5dread_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
    buf = int(buf_real, idp)
  end subroutine h5dread_f_i64_5d

  subroutine h5dread_f_i64_6d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(out) :: buf(:,:,:,:,:,:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1),size(buf,2),size(buf,3),size(buf,4),size(buf,5),size(buf,6))
    call h5dread_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
    buf = int(buf_real, idp)
  end subroutine h5dread_f_i64_6d

  subroutine h5dwrite_f_i64_scalar(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(in) :: buf
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real
    buf_real = real(buf, dp)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
  end subroutine h5dwrite_f_i64_scalar

  subroutine h5dwrite_f_i64_1d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(in) :: buf(:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1))
    buf_real = real(buf, dp)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
  end subroutine h5dwrite_f_i64_1d

  subroutine h5dwrite_f_i64_2d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(in) :: buf(:,:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1),size(buf,2))
    buf_real = real(buf, dp)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
  end subroutine h5dwrite_f_i64_2d

  subroutine h5dwrite_f_i64_3d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(in) :: buf(:,:,:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1),size(buf,2),size(buf,3))
    buf_real = real(buf, dp)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
  end subroutine h5dwrite_f_i64_3d

  subroutine h5dwrite_f_i64_4d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(in) :: buf(:,:,:,:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1),size(buf,2),size(buf,3),size(buf,4))
    buf_real = real(buf, dp)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
  end subroutine h5dwrite_f_i64_4d

  subroutine h5dwrite_f_i64_5d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(in) :: buf(:,:,:,:,:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1),size(buf,2),size(buf,3),size(buf,4),size(buf,5))
    buf_real = real(buf, dp)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
  end subroutine h5dwrite_f_i64_5d

  subroutine h5dwrite_f_i64_6d(dset_id, memtype_id, buf, dims, hdferr)
    implicit none
    integer(hid_t),intent(in) :: dset_id, memtype_id
    integer(idp),intent(in) :: buf(:,:,:,:,:,:)
    integer(hsize_t),intent(in) :: dims(:)
    integer, intent(out) :: hdferr
    real(dp) :: buf_real(size(buf,1),size(buf,2),size(buf,3),size(buf,4),size(buf,5),size(buf,6))
    buf_real = real(buf, dp)
    call h5dwrite_f(dset_id, h5t_ieee_f64le, buf_real, dims, hdferr)
  end subroutine h5dwrite_f_i64_6d

  subroutine h5tbread_field_name_f_i64_1d(loc_id, dset_name, field_name, start, nrecords, type_size, buf, errcode)
    implicit none
    integer(hid_t), intent(in) :: loc_id           ! file or group identifier
    character(len=*), intent(in) :: dset_name      ! name of the dataset
    character(len=*), intent(in) :: field_name     ! name of the field
    integer(hsize_t), intent(in) :: start          ! start record
    integer(hsize_t), intent(in) :: nrecords       ! records
    integer(size_t), intent(in) :: type_size       ! type size
    integer(idp), intent(out) :: buf(:)             ! data buffer
    integer :: errcode                             ! error code
    real(dp) :: buf_real(size(buf, 1))
    call h5tbread_field_name_f(loc_id, dset_name, field_name, start, nrecords, type_size, buf_real, errcode)
    buf = int(buf_real, idp)
  end subroutine h5tbread_field_name_f_i64_1d

  subroutine h5tbwrite_field_name_f_i64_1d(loc_id, dset_name, field_name, start, nrecords, type_size, buf, errcode)
    implicit none
    integer(hid_t), intent(in) :: loc_id           ! file or group identifier
    character(len=*), intent(in) :: dset_name      ! name of the dataset
    character(len=*), intent(in) :: field_name     ! name of the field
    integer(hsize_t), intent(in) :: start          ! start record
    integer(hsize_t), intent(in) :: nrecords       ! records
    integer(size_t), intent(in) :: type_size       ! type size
    integer(idp), intent(in) :: buf(:)             ! data buffer
    integer :: errcode                             ! error code
    real(dp) :: buf_real(size(buf, 1))
    buf_real = real(buf, dp)
    call h5tbwrite_field_name_f(loc_id, dset_name, field_name, start, nrecords, type_size, buf_real, errcode)
  end subroutine h5tbwrite_field_name_f_i64_1d

  elemental subroutine clean_string(string)
    implicit none
    character(len=*),intent(inout) :: string
    integer :: i
    do i=1,len(string)
       if(iachar(string(i:i)) < 32) string(i:i) = " "
    end do
  end subroutine clean_string

end module lib_hdf5

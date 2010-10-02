! MD5 of template: 3fd1526ecf0aa6423c17aeae57fdd37e
! High level routines for HDF5
! Thomas Robitaille (c) 2010

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
  public :: hdf5_exists
  public :: hdf5_open_new
  public :: hdf5_open_read
  public :: hdf5_open_write
  public :: hdf5_close
  public :: hdf5_create_group
  public :: hdf5_open_group
  public :: hdf5_close_group
  public :: hdf5_open_dataset
  public :: hdf5_close_dataset
  public :: hdf5_path_exists
  public :: hdf5_list_groups

  ! keywords
  public :: hdf5_read_keyword
  public :: hdf5_write_keyword
  public :: hdf5_exists_keyword

  ! image
  public :: hdf5_write_array
  public :: hdf5_read_array
  public :: hdf5_read_array_auto

  ! table
  public :: hdf5_table_write_header
  public :: hdf5_table_write_column
  public :: hdf5_table_read_column
  public :: hdf5_table_read_column_auto
  ! public :: hdf5_table_new_column

  integer,parameter :: idp = selected_int_kind(13)
  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  interface hdf5_write_array
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

  interface hdf5_write_keyword
     module procedure hdf5_write_k_h5t_std_i32le
     module procedure hdf5_write_k_h5t_std_i64le
     module procedure hdf5_write_k_h5t_ieee_f32le
     module procedure hdf5_write_k_h5t_ieee_f64le
     module procedure hdf5_write_k_string
  end interface hdf5_write_keyword

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
     integer(size_t) :: type_size
  end type table_info

  ! fix for 64-bit integers - read/write them as floats (not ideal)

  interface h5aread_f
     module procedure h5aread_f_i64
  end interface h5aread_f

  interface h5awrite_f
     module procedure h5awrite_f_i64
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
             write(*,'(" Do you wish to continue? (y/n) ")',advance='no')
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
    call h5close_f(hdferr)
    call check_status(hdferr,'hdf5_close')
  end subroutine hdf5_close

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

  subroutine hdf5_list_groups(handle, path, groups)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    character(len=*),allocatable,intent(out) :: groups(:)
    integer :: n_members, n_groups, im, ig
    character(len=len(groups)) :: name
    integer :: type
    n_members = hdf5_count_members(handle, path)
    n_groups = 0
    do im=1,n_members
       call hdf5_member_info(handle, path, im-1, name, type)
       if(type==H5G_GROUP_F) n_groups = n_groups + 1
    end do
    allocate(groups(n_groups))
    ig = 0
    do im=1,n_members
       call hdf5_member_info(handle, path, im-1, name, type)
       if(type==H5G_GROUP_F) then
          ig=ig + 1
          groups(ig) = name
       end if
    end do
  end subroutine hdf5_list_groups


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
    call h5tbmake_table_f('table_title',handle,path,int(n_cols,hsize_t),int(n_rows,hsize_t),type_size, &
         & names,offsets,types,chunk_size,compress,hdferr)
    call check_status(hdferr,'hdf5_table_write_header')
  end subroutine hdf5_table_write_header

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
    values = clean_string(values)
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
        print *, "ERROR: attribute "//trim(name)//" does not exist in HDF5 file" 
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
    INTEGER(HID_T) :: atype_id
    call hdf5_check_exists_keyword(handle, path, name)
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_string [1]')
    CALL h5tset_size_f(atype_id, int(len(value),size_t), hdferr)
    call check_status(hdferr,'hdf5_read_k_string [2]')
    call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_string [3]')
    call h5aread_f(attr_id, atype_id, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_read_k_string [4]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_string [5]')
  end subroutine hdf5_read_k_string

  subroutine hdf5_write_k_string(handle,path,name,value)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path, name
    character(len=*),intent(in) :: value
    integer(hid_t) :: dspace_id, attr_id
    integer :: hdferr
    INTEGER(HID_T) :: atype_id
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_string [1]')
    CALL h5tset_size_f(atype_id, int(len(value),size_t), hdferr)
    call check_status(hdferr,'hdf5_read_k_string [2]')
    if(hdf5_exists_keyword(handle, path, name)) then
       call h5aopen_by_name_f(handle, path, name, attr_id, hdferr)
       call check_status(hdferr,'hdf5_read_k_string [3]')
    else
       call h5screate_f(h5s_scalar_f, dspace_id, hdferr)
       call check_status(hdferr,'hdf5_read_k_string [4]')
       call h5acreate_by_name_f(handle, path, name, atype_id, dspace_id, attr_id, hdferr)
       call check_status(hdferr,'hdf5_read_k_string [5]')
       call h5sclose_f(dspace_id, hdferr)
       call check_status(hdferr,'hdf5_read_k_string [6]')
    end if
    call h5awrite_f(attr_id, atype_id, value, (/1_hsize_t/), hdferr)
    call check_status(hdferr,'hdf5_read_k_string [7]')
    call h5aclose_f(attr_id, hdferr)
    call check_status(hdferr,'hdf5_read_k_string [8]')
  end subroutine hdf5_write_k_string


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



  !   subroutine hdf5_write_k_d(unit,name,value,comment)
  !     implicit none
  !     integer,intent(in) :: unit
  !     character(len=*),intent(in) :: name
  !     real(dp),intent(in) :: value
  !     character(len=*),intent(in),optional :: comment
  !     integer :: status
  !     status = 0
  !     if(present(comment)) then
  !        call ftukyd(unit,name,value,10,comment,status)
  !     else
  !        call ftukyd(unit,name,value,10,"",status)
  !     end if
  !     call check_status(hdferr,'hdf5_write_k_d')
  !   end subroutine hdf5_write_k_d


  !   subroutine hdf5_write_k_e(unit,name,value,comment)
  !     implicit none
  !     integer,intent(in) :: unit
  !     character(len=*),intent(in) :: name
  !     real(sp),intent(in) :: value
  !     character(len=*),intent(in),optional :: comment
  !     integer :: status
  !     status = 0
  !     if(present(comment)) then
  !        call ftukye(unit,name,value,10,comment,status)
  !     else
  !        call ftukye(unit,name,value,10,"",status)
  !     end if
  !     call check_status(hdferr,'hdf5_write_k_e')
  !   end subroutine hdf5_write_k_e


  type(table_info) function hdf5_read_table_info(handle, path) result(info)
    implicit none
    integer(hid_t),intent(in) :: handle
    character(len=*),intent(in) :: path
    integer :: hdferr
    call h5tbget_table_info_f(handle, path, info%n_cols, info%n_rows, hdferr)
    allocate(info%field_names(info%n_cols))
    allocate(info%field_sizes(info%n_cols))
    allocate(info%field_offsets(info%n_cols))
    call h5tbget_field_info_f(handle, path, info%n_cols, info%field_names,&
         & info%field_sizes, info%field_offsets, info%type_size, hdferr)
    call check_status(hdferr,'hdf5_read_table_info')
  end function hdf5_read_table_info

  integer function hdf5_table_column_number(info, col_name) result(col_id)
    implicit none
    type(table_info),intent(in) :: info
    character(len=*),intent(in) :: col_name
    do col_id=1,info%n_cols
       if(trim(adjustl(info%field_names(col_id))) == trim(col_name)) exit
    end do
    if(col_id==info%n_cols+1) then
       print *, "column not found:"//trim(col_name)
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

  elemental function clean_string(string)
    implicit none
    character(len=*),intent(in) :: string
    character(len=len(string)) :: clean_string
    integer :: i
    do i=1,len(string)
       if(iachar(string(i:i)) >= 32) then
          clean_string(i:i) = string(i:i)
       else
          clean_string(i:i) = " "
       end if
    end do
  end function clean_string

end module lib_hdf5

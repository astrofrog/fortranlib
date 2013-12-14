! High level routines for cfitsio
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

module lib_cfitsio

  implicit none
  save

  private
  public :: check_status

  public :: base_cfitsio_verbose_enable
  public :: base_cfitsio_verbose_disable

  ! OPEN/CLOSE
  public :: fits_open_new
  public :: fits_open_read
  public :: fits_open_write
  public :: fits_close

  ! KEYWORDS
  public :: fits_read_keyword
  public :: fits_write_keyword
  public :: fits_exists_keyword

  ! HDU
  public :: fits_number_hdu
  public :: fits_create_hdu
  public :: fits_move_hdu
  public :: fits_write_primary_header

  ! IMAGE
  public :: fits_read_array_line
  public :: fits_write_array_line
  public :: fits_write_array
  public :: fits_read_array
  public :: fits_read_array_auto

  ! TABLE
  public :: fits_table_write_header
  public :: fits_table_write_column
  public :: fits_table_read_column
  public :: fits_table_read_column_auto
  public :: fits_table_number_rows
  public :: fits_table_number_columns
  public :: fits_table_column_number
  public :: fits_table_column_width
  public :: fits_table_new_column

  integer,parameter :: sp = selected_real_kind(p=6,r=37)
  integer,parameter :: dp = selected_real_kind(p=15,r=307)

  integer,parameter             :: nullvalj = 0
  real(sp),parameter            :: nullvale = 0._sp
  real(dp),parameter            :: nullvald = 0._dp
  character(len=1000),parameter :: nullvals = ""
  logical,parameter             :: nullvall = .false.
  character(len=1),parameter    :: nullvalb = ' '

  interface fits_write_array
     module procedure fits_write_2d_array_j
     module procedure fits_write_2d_array_e
     module procedure fits_write_2d_array_d
     module procedure fits_write_3d_array_j
     module procedure fits_write_3d_array_e
     module procedure fits_write_3d_array_d
     module procedure fits_write_4d_array_j
     module procedure fits_write_4d_array_e
     module procedure fits_write_4d_array_d
     module procedure fits_write_5d_array_j
     module procedure fits_write_5d_array_e
     module procedure fits_write_5d_array_d
     module procedure fits_write_6d_array_j
     module procedure fits_write_6d_array_e
     module procedure fits_write_6d_array_d
  end interface fits_write_array

  interface fits_read_array
     module procedure fits_read_2d_array_j
     module procedure fits_read_2d_array_e
     module procedure fits_read_2d_array_d
     module procedure fits_read_3d_array_j
     module procedure fits_read_3d_array_e
     module procedure fits_read_3d_array_d
     module procedure fits_read_4d_array_j
     module procedure fits_read_4d_array_e
     module procedure fits_read_4d_array_d
     module procedure fits_read_5d_array_j
     module procedure fits_read_5d_array_e
     module procedure fits_read_5d_array_d
     module procedure fits_read_6d_array_j
     module procedure fits_read_6d_array_e
     module procedure fits_read_6d_array_d
  end interface fits_read_array

  interface fits_read_array_auto
     module procedure fits_read_2d_array_alloc_j
     module procedure fits_read_2d_array_alloc_e
     module procedure fits_read_2d_array_alloc_d
     module procedure fits_read_3d_array_alloc_j
     module procedure fits_read_3d_array_alloc_e
     module procedure fits_read_3d_array_alloc_d
     module procedure fits_read_4d_array_alloc_j
     module procedure fits_read_4d_array_alloc_e
     module procedure fits_read_4d_array_alloc_d
     module procedure fits_read_5d_array_alloc_j
     module procedure fits_read_5d_array_alloc_e
     module procedure fits_read_5d_array_alloc_d
     module procedure fits_read_6d_array_alloc_j
     module procedure fits_read_6d_array_alloc_e
     module procedure fits_read_6d_array_alloc_d
  end interface fits_read_array_auto

  interface fits_read_keyword
     module procedure fits_read_kj
     module procedure fits_read_ke
     module procedure fits_read_kd
     module procedure fits_read_ks
     module procedure fits_read_kl
  end interface fits_read_keyword

  interface fits_write_keyword
     module procedure fits_write_kj
     module procedure fits_write_ke
     module procedure fits_write_kd
     module procedure fits_write_ks
     module procedure fits_write_kl
  end interface fits_write_keyword

  interface fits_table_write_column
     module procedure write_table_column_0d_j
     module procedure write_table_column_1d_j
     module procedure write_table_column_2d_j
     module procedure write_table_column_0d_e
     module procedure write_table_column_1d_e
     module procedure write_table_column_2d_e
     module procedure write_table_column_0d_d
     module procedure write_table_column_1d_d
     module procedure write_table_column_2d_d
     module procedure write_table_column_0d_s
     module procedure write_table_column_1d_s
     module procedure write_table_column_2d_s
     module procedure write_table_column_0d_l
     module procedure write_table_column_1d_l
     module procedure write_table_column_2d_l
  end interface fits_table_write_column

  interface fits_table_read_column
     module procedure read_table_column_0d_j
     module procedure read_table_column_1d_j
     module procedure read_table_column_2d_j
     module procedure read_table_column_0d_e
     module procedure read_table_column_1d_e
     module procedure read_table_column_2d_e
     module procedure read_table_column_0d_d
     module procedure read_table_column_1d_d
     module procedure read_table_column_2d_d
     module procedure read_table_column_0d_s
     module procedure read_table_column_1d_s
     module procedure read_table_column_2d_s
     module procedure read_table_column_0d_l
     module procedure read_table_column_1d_l
     module procedure read_table_column_2d_l
  end interface fits_table_read_column

  interface fits_table_read_column_auto
     module procedure read_table_column_1d_alloc_j
     module procedure read_table_column_2d_alloc_j
     module procedure read_table_column_1d_alloc_e
     module procedure read_table_column_2d_alloc_e
     module procedure read_table_column_1d_alloc_d
     module procedure read_table_column_2d_alloc_d
     module procedure read_table_column_1d_alloc_s
     module procedure read_table_column_2d_alloc_s
  end interface fits_table_read_column_auto


  interface fits_write_array_line
     module procedure fits_write_array_line_e
     module procedure fits_write_array_line_d
     module procedure fits_write_array_line_b
  end interface fits_write_array_line

  interface fits_read_array_line
     module procedure fits_read_array_line_e
     module procedure fits_read_array_line_d
     module procedure fits_read_array_line_b
  end interface fits_read_array_line

  logical :: verbose = .false.

contains

  subroutine base_cfitsio_verbose_enable
    implicit none
    verbose = .true.
  end subroutine base_cfitsio_verbose_enable

  subroutine base_cfitsio_verbose_disable
    implicit none
    verbose = .false.
  end subroutine base_cfitsio_verbose_disable

  subroutine check_status(unit,status,origin)
    implicit none
    integer,intent(in) :: unit,status
    character(len=*),optional,intent(in) :: origin

    if(status/=0) then

       if(present(origin)) then
          write(*,*) 'cfitsio returned the following error in ',trim(origin),', unit=',unit
       else
          write(*,*) 'cfitsio returned the following error in : unknown, unit=',unit
       end if
       write(*,*)

       call ftrprt('STDERR',status)
       write(*,*)
       write(*,*) ' The program has terminated unexpectadly'
       stop

    end if

  end subroutine check_status

  logical function fits_exists(filename)
    implicit none
    character(len=*),intent(in) :: filename
    integer :: status,exists
    status = 0
    call ftexist(filename,exists,status)
    call check_status(1,status,'fits_exists')
    fits_exists = exists==1
  end function fits_exists

  subroutine fits_delete(filename,confirm)
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
    if(fits_exists(filename)) then
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
       !      call fits_open_write(unit,filename)
       !      call ftdelt(unit,status)
       open(unit=451,file=filename,form='unformatted')
       close(unit=451,status='delete')
       !      call check_status(unit,status,'fits_delete')
    end if
  end subroutine fits_delete

  subroutine fits_open_new(unit,filename,confirm)
    implicit none
    character(len=*),intent(in) :: filename
    integer,intent(out) :: unit
    logical,intent(in),optional :: confirm
    integer :: status,blocksize
    status = 0 ; blocksize = 0
    call fits_delete(filename,confirm)
    call ftgiou(unit,status)
    call ftinit(unit,filename,blocksize,status)
    call check_status(unit,status,'fits_open_new')
  end subroutine fits_open_new

  subroutine fits_open_read(unit,filename)
    implicit none
    character(len=*),intent(in) :: filename
    integer,intent(out) :: unit
    integer :: status
    integer,parameter :: readwrite = 0
    status = 0
    call ftgiou(unit,status)
    call ftnopn(unit,filename,readwrite,status)
    call check_status(unit,status,'fits_open_read')
  end subroutine fits_open_read

  subroutine fits_open_write(unit,filename)
    implicit none
    character(len=*),intent(in) :: filename
    integer,intent(out) :: unit
    integer :: status,blocksize
    integer,parameter :: readwrite = 1
    status = 0 ; blocksize = 0
    call ftgiou(unit,status)
    call ftopen(unit,filename,readwrite,blocksize,status)
    call check_status(unit,status,'fits_open_write')
  end subroutine fits_open_write

  integer function fits_number_hdu(unit)
    implicit none
    integer,intent(in) :: unit
    integer :: status
    status = 0
    call ftthdu(unit,fits_number_hdu,status)
    call check_status(unit,status,'fits_number_hdu')
  end function fits_number_hdu

  subroutine fits_create_hdu(unit,hdu_id)
    implicit none
    integer,intent(in) :: unit,hdu_id
    integer :: status
    status = 0
    call fits_move_hdu(unit,hdu_id-1)
    call ftcrhd(unit,status)
    call check_status(unit,status,'fits_create_hdu')
  end subroutine fits_create_hdu

  subroutine fits_move_hdu(unit,hdu_id)
    implicit none
    integer,intent(in) :: unit,hdu_id
    integer :: status,hdutype
    status = 0
    call ftmahd(unit,hdu_id,hdutype,status)
    call check_status(unit,status,'fits_move_hdu')
  end subroutine fits_move_hdu

  subroutine fits_close(unit)
    implicit none
    integer,intent(in) :: unit
    integer :: status
    status = 0
    call ftclos(unit,status)
    call ftfiou(unit,status)
    call check_status(unit,status,'fits_close')
  end subroutine fits_close

  subroutine fits_write_primary_header(unit,bitpix,naxes,extend)
    implicit none
    integer,intent(in) :: unit
    integer,intent(in) :: bitpix
    integer,intent(in) :: naxes(:)
    logical,intent(in) :: extend
    logical,parameter :: simple = .true.
    integer,parameter :: pcount = 0
    integer,parameter :: gcount = 1
    integer :: status
    status = 0
    call ftphpr(unit,simple,bitpix,size(naxes),naxes,pcount,gcount,extend,status)
    call check_status(unit,status,'fits_write_primary_header')
  end subroutine fits_write_primary_header

  subroutine fits_table_write_header(unit,n_rows,n_cols,title,form,units,extname)
    implicit none
    integer,intent(in) :: unit,n_rows,n_cols
    character(len=*),intent(in) :: title(n_cols),form(n_cols),units(n_cols)
    character(len=*),intent(in) :: extname
    integer,parameter :: varidat = 0
    integer :: status
    status = 0
    call ftphbn(unit,n_rows,n_cols,title,form,units,extname,varidat,status)
    call check_status(unit,status,'fits_table_write_header')
  end subroutine fits_table_write_header

  integer function fits_table_column_width(unit,colname)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    integer :: status,colnum,naxes(2),naxis
    status = 0
    colnum = fits_table_column_number(unit,colname)
    call ftgtdm(unit,colnum,2,naxis,naxes,status)
    call check_status(unit,status,'fits_table_column_width')
    fits_table_column_width = naxes(naxis)
  end function fits_table_column_width

  !!@FOR integer:j real(sp):e real(dp):d character(len=*):s logical:l

  subroutine read_table_column_0d_<T>(unit,colname,value,row)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    @T,intent(out) :: value
    integer,intent(in),optional :: row
    @T :: values_temp(1,1)
    call read_table_column_2d_<T>(unit,colname,values_temp,row)
    value = values_temp(1,1)
  end subroutine read_table_column_0d_<T>

  subroutine read_table_column_1d_<T>(unit,colname,values,row)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    @T,intent(out) :: values(:)
    integer,intent(in),optional :: row
    @T :: values_temp(size(values),1)
    call read_table_column_2d_<T>(unit,colname,values_temp,row)
    values = values_temp(:,1)
  end subroutine read_table_column_1d_<T>

  subroutine read_table_column_1d_alloc_<T>(unit,colname,values)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    @T,allocatable,intent(out) :: values(:)
    integer :: n_rows
    call fits_read_keyword(unit,'NAXIS2',n_rows)
    allocate(values(n_rows))
    call read_table_column_1d_<T>(unit,colname,values)
  end subroutine read_table_column_1d_alloc_<T>

  subroutine read_table_column_2d_<T>(unit,colname,values,row)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    @T,intent(out),dimension(:,:) :: values
    integer,intent(in),optional :: row
    integer,parameter :: felem = 1
    logical :: anyf
    integer :: colnum,status,frow
    if(present(row)) then
       frow = row
    else
       frow = 1
    end if
    status = 0
    colnum = fits_table_column_number(unit,colname)
    call ftgcv<T>(unit,colnum,frow,felem,size(values),nullval<T>,values,anyf,status)
    call check_status(unit,status,'read_table_column_2d_<T>')
  end subroutine read_table_column_2d_<T>

  subroutine read_table_column_2d_alloc_<T>(unit,colname,values)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    @T,allocatable,intent(out) :: values(:,:)
    integer :: n_rows,length
    call fits_read_keyword(unit,'NAXIS2',n_rows)
    length = fits_table_column_width(unit,colname)
    allocate(values(length,n_rows))
    call read_table_column_2d_<T>(unit,colname,values)
  end subroutine read_table_column_2d_alloc_<T>

  subroutine write_table_column_0d_<T>(unit,colname,value,row)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    @T,intent(in) :: value
    integer,optional :: row
    @T :: values_temp(1,1)
    values_temp(1,1) = value
    call write_table_column_2d_<T>(unit,colname,values_temp,row)
  end subroutine write_table_column_0d_<T>

  subroutine write_table_column_1d_<T>(unit,colname,values,row)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    @T,intent(in) :: values(:)
    integer,optional :: row
    @T :: values_temp(size(values),1)
    values_temp(:,1) = values(:)
    call write_table_column_2d_<T>(unit,colname,values_temp,row)
  end subroutine write_table_column_1d_<T>

  subroutine write_table_column_2d_<T>(unit,colname,values,row)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: colname
    @T,intent(in),dimension(:,:) :: values
    integer,optional :: row
    integer,parameter :: felem = 1
    integer :: status,frow,colnum
    if(present(row)) then
       frow = row
    else
       frow = 1
    end if
    status = 0
    colnum = fits_table_column_number(unit,colname)
    call ftpcl<T>(unit,colnum,frow,felem,size(values),values,status)
    call check_status(unit,status,'write_table_column_2d_<T>')
  end subroutine write_table_column_2d_<T>

  !!@END FOR

  !!@FOR integer:j real(sp):e real(dp):d

  subroutine fits_read_2d_array_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,dimension(:,:) :: array
    integer :: nx,ny
    integer,parameter :: group = 1
    integer :: status
    logical :: anyf
    real,parameter :: nullval = 0.
    status = 0
    nx = size(array,1) ; ny = size(array,2)
    call ftg2d<T>(unit,group,nullval,nx,nx,ny,array,anyf,status)
    call check_status(unit,status,'fits_read_2d_array_<T>')
  end subroutine fits_read_2d_array_<T>

  subroutine fits_read_3d_array_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,dimension(:,:,:),intent(out) :: array
    integer,parameter :: group=1,fpixel=1,nullval=0
    integer :: status
    logical :: anyf
    status = 0
    call ftgpv<T>(unit,group,fpixel,size(array),nullval,array,anyf,status)
    call check_status(unit,status,'fits_read_3d_array_<T>')
  end subroutine fits_read_3d_array_<T>

  subroutine fits_read_4d_array_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,dimension(:,:,:,:),intent(out) :: array
    integer,parameter :: group=1,fpixel=1,nullval=0
    integer :: status
    logical :: anyf
    status = 0
    call ftgpv<T>(unit,group,fpixel,size(array),nullval,array,anyf,status)
    call check_status(unit,status,'fits_read_4d_array_<T>')
  end subroutine fits_read_4d_array_<T>

  subroutine fits_read_5d_array_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,dimension(:,:,:,:,:),intent(out) :: array
    integer,parameter :: group=1,fpixel=1,nullval=0
    integer :: status
    logical :: anyf
    status = 0
    call ftgpv<T>(unit,group,fpixel,size(array),nullval,array,anyf,status)
    call check_status(unit,status,'fits_read_5d_array_<T>')
  end subroutine fits_read_5d_array_<T>

  subroutine fits_read_6d_array_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,dimension(:,:,:,:,:,:),intent(out) :: array
    integer,parameter :: group=1,fpixel=1,nullval=0
    integer :: status
    logical :: anyf
    status = 0
    call ftgpv<T>(unit,group,fpixel,size(array),nullval,array,anyf,status)
    call check_status(unit,status,'fits_read_6d_array_<T>')
  end subroutine fits_read_6d_array_<T>

  subroutine fits_read_2d_array_alloc_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,allocatable,dimension(:,:),intent(out) :: array
    integer :: nx,ny
    integer,parameter :: group = 1
    integer :: status
    logical :: anyf
    real,parameter :: nullval = 0.
    status = 0
    call fits_read_keyword(unit,'NAXIS1',nx)
    call fits_read_keyword(unit,'NAXIS2',ny)
    allocate(array(nx,ny))
    call ftg2d<T>(unit,group,nullval,nx,nx,ny,array,anyf,status)
    call check_status(unit,status,'fits_read_2d_array_alloc_<T>')
  end subroutine fits_read_2d_array_alloc_<T>

  subroutine fits_read_3d_array_alloc_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,allocatable,dimension(:,:,:),intent(out) :: array
    integer :: n1,n2,n3
    integer,parameter :: group=1,fpixel=1,nullval=0
    integer :: status
    logical :: anyf
    status = 0
    call fits_read_keyword(unit,'NAXIS1',n1)
    call fits_read_keyword(unit,'NAXIS2',n2)
    call fits_read_keyword(unit,'NAXIS3',n3)
    allocate(array(n1,n2,n3))
    call ftgpv<T>(unit,group,fpixel,size(array),nullval,array,anyf,status)
    call check_status(unit,status,'fits_read_3d_array_alloc_<T>')
  end subroutine fits_read_3d_array_alloc_<T>

  subroutine fits_read_4d_array_alloc_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,allocatable,dimension(:,:,:,:),intent(out) :: array
    integer :: n1,n2,n3,n4
    integer,parameter :: group=1,fpixel=1,nullval=0
    integer :: status
    logical :: anyf
    status = 0
    call fits_read_keyword(unit,'NAXIS1',n1)
    call fits_read_keyword(unit,'NAXIS2',n2)
    call fits_read_keyword(unit,'NAXIS3',n3)
    call fits_read_keyword(unit,'NAXIS4',n4)
    allocate(array(n1,n2,n3,n4))
    call ftgpv<T>(unit,group,fpixel,size(array),nullval,array,anyf,status)
    call check_status(unit,status,'fits_read_4d_array_alloc_<T>')
  end subroutine fits_read_4d_array_alloc_<T>

  subroutine fits_read_5d_array_alloc_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,allocatable,dimension(:,:,:,:,:),intent(out) :: array
    integer :: n1,n2,n3,n4,n5
    integer,parameter :: group=1,fpixel=1,nullval=0
    integer :: status
    logical :: anyf
    status = 0
    call fits_read_keyword(unit,'NAXIS1',n1)
    call fits_read_keyword(unit,'NAXIS2',n2)
    call fits_read_keyword(unit,'NAXIS3',n3)
    call fits_read_keyword(unit,'NAXIS4',n4)
    call fits_read_keyword(unit,'NAXIS5',n5)
    allocate(array(n1,n2,n3,n4,n5))
    call ftgpv<T>(unit,group,fpixel,size(array),nullval,array,anyf,status)
    call check_status(unit,status,'fits_read_5d_array_alloc_<T>')
  end subroutine fits_read_5d_array_alloc_<T>

  subroutine fits_read_6d_array_alloc_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,allocatable,dimension(:,:,:,:,:,:),intent(out) :: array
    integer :: n1,n2,n3,n4,n5,n6
    integer,parameter :: group=1,fpixel=1,nullval=0
    integer :: status
    logical :: anyf
    status = 0
    call fits_read_keyword(unit,'NAXIS1',n1)
    call fits_read_keyword(unit,'NAXIS2',n2)
    call fits_read_keyword(unit,'NAXIS3',n3)
    call fits_read_keyword(unit,'NAXIS4',n4)
    call fits_read_keyword(unit,'NAXIS5',n5)
    call fits_read_keyword(unit,'NAXIS6',n6)
    allocate(array(n1,n2,n3,n4,n5,n6))
    call ftgpv<T>(unit,group,fpixel,size(array),nullval,array,anyf,status)
    call check_status(unit,status,'fits_read_6d_array_alloc_<T>')
  end subroutine fits_read_6d_array_alloc_<T>

  subroutine fits_write_2d_array_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,dimension(:,:) :: array
    integer :: nx,ny
    integer,parameter :: group = 1
    integer :: status
    status = 0
    nx = size(array,1) ; ny = size(array,2)
    call ftp2d<T>(unit,group,nx,nx,ny,array,status)
    call check_status(unit,status,'fits_write_2d_array_<T>')
  end subroutine fits_write_2d_array_<T>

  subroutine fits_write_3d_array_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,dimension(:,:,:),intent(in) :: array
    integer,parameter :: group=1,fpixel=1
    integer :: status
    status = 0
    call ftppr<T>(unit,group,fpixel,size(array),array,status)
    call check_status(unit,status,'fits_write_3d_array_<T>')
  end subroutine fits_write_3d_array_<T>

  subroutine fits_write_4d_array_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,dimension(:,:,:,:),intent(in) :: array
    integer,parameter :: group=1,fpixel=1
    integer :: status
    status = 0
    call ftppr<T>(unit,group,fpixel,size(array),array,status)
    call check_status(unit,status,'fits_write_4d_array_<T>')
  end subroutine fits_write_4d_array_<T>

  subroutine fits_write_5d_array_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,dimension(:,:,:,:,:),intent(in) :: array
    integer,parameter :: group=1,fpixel=1
    integer :: status
    status = 0
    call ftppr<T>(unit,group,fpixel,size(array),array,status)
    call check_status(unit,status,'fits_write_5d_array_<T>')
  end subroutine fits_write_5d_array_<T>

  subroutine fits_write_6d_array_<T>(unit,array)
    implicit none
    integer,intent(in) :: unit
    @T,dimension(:,:,:,:,:,:),intent(in) :: array
    integer,parameter :: group=1,fpixel=1
    integer :: status
    status = 0
    call ftppr<T>(unit,group,fpixel,size(array),array,status)
    call check_status(unit,status,'fits_write_6d_array_<T>')
  end subroutine fits_write_6d_array_<T>

  !!@END FOR

  logical function fits_exists_keyword(unit,name)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: name
    character(len=100) :: value,comment
    integer :: status
    status=0
    call ftgkey(unit,name,value,comment,status)
    select case(status)
    case(0)
       fits_exists_keyword = .true.
    case(202)
       fits_exists_keyword = .false.
    case default
       call check_status(unit,status,'fits_exists_keyword')
    end select
  end function fits_exists_keyword

  !!@FOR integer:j real(sp):e real(dp):d character(len=*):s logical:l

  subroutine fits_read_k<T>(unit,name,value)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: name
    @T,intent(out) :: value
    character(len=100) :: comment
    integer :: status
    status = 0
    call ftgky<T>(unit,name,value,comment,status)
    call check_status(unit,status,'fits_read_k<T> - '//trim(name))
  end subroutine fits_read_k<T>

  !!@END FOR

  !!@FOR integer:j character(len=*):s logical:l

  subroutine fits_write_k<T>(unit,name,value,comment)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: name
    @T,intent(in) :: value
    character(len=*),intent(in),optional :: comment
    integer :: status
    status = 0
    if(present(comment)) then
       call ftuky<T>(unit,name,value,comment,status)
    else
       call ftuky<T>(unit,name,value,"",status)
    end if
    call check_status(unit,status,'fits_write_k<T>')
  end subroutine fits_write_k<T>

  !!@END FOR

  !!@FOR real(sp):e real(dp):d

  subroutine fits_write_k<T>(unit,name,value,comment)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: name
    @T,intent(in) :: value
    character(len=*),intent(in),optional :: comment
    integer :: status
    status = 0
    if(present(comment)) then
       call ftuky<T>(unit,name,value,10,comment,status)
    else
       call ftuky<T>(unit,name,value,10,"",status)
    end if
    call check_status(unit,status,'fits_write_k<T>')
  end subroutine fits_write_k<T>

  !!@END FOR

  integer function fits_table_column_number(unit,name)
    implicit none
    integer,intent(in) :: unit
    character(len=*),intent(in) :: name
    integer :: status
    status = 0
    call ftgcno(unit,.true.,name,fits_table_column_number,status)
    call check_status(unit,status,'fits_table_column_number')
  end function fits_table_column_number

  integer function fits_table_number_columns(unit)
    implicit none
    integer,intent(in) :: unit
    integer :: status
    status = 0
    call ftgncl(unit,fits_table_number_columns,status)
    call check_status(unit,status,'fits_table_number_columns')
  end function fits_table_number_columns

  integer function fits_table_number_rows(unit)
    implicit none
    integer,intent(in) :: unit
    integer :: status
    status = 0
    call ftgnrw(unit,fits_table_number_rows,status)
    call check_status(unit,status,'fits_table_number_rows')
  end function fits_table_number_rows

  subroutine fits_table_new_column(unit,colnum,ttype,tform)
    implicit none
    integer,intent(in)          :: unit,colnum
    character(len=*),intent(in) :: ttype,tform
    integer                     :: status
    status = 0
    call fticol(unit,colnum,ttype,tform,status)
    call check_status(unit,status,'fits_table_new_column')
  end subroutine fits_table_new_column

  !!@FOR real(sp):e real(dp):d character(len=1):b

  subroutine fits_write_array_line_<T>(unit,line,nx,array)
    implicit none
    integer,intent(in) :: unit,line,nx
    @T,intent(in)      :: array(:)
    integer,parameter  :: group = 1
    integer            :: status,first_pixel
    status = 0
    first_pixel = (line-1)*nx+1
    call ftppr<T>(unit,group,first_pixel,nx,array,status)
    call check_status(unit,status,'fits_write_array_<T>')
  end subroutine fits_write_array_line_<T>

  subroutine fits_read_array_line_<T>(unit,line,nx,array)
    implicit none
    integer,intent(in) :: unit,line,nx
    @T,intent(out)     :: array(:)
    integer,parameter  :: group = 1
    integer            :: status,first_pixel
    logical            :: anyf
    logical,dimension(size(array)) :: flagvals
    status = 0
    first_pixel = (line-1)*nx+1
    call ftgpf<T>(unit,group,first_pixel,nx,array,flagvals,anyf,status)
    call check_status(unit,status,'fits_read_array_<T>')
  end subroutine fits_read_array_line_<T>

  !!@END FOR

end module lib_cfitsio

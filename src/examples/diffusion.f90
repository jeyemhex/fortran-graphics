program diffusion_demo
!==============================================================================#
! DIFFUSION_DEMO
!------------------------------------------------------------------------------#
! Author:  Ed Higgins <ed.j.higgins@gmail.com>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2024-11-24
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use omp_lib
  use iso_fortran_env

  use graphics

  implicit none

  type(gfx_scene) :: gfx

  real(real64), parameter :: dx = 0.1_real64, dt=1.0_real64
  real(real64), parameter :: L = 10
  integer,      parameter :: n_steps_per_frame = 1000
  integer,      parameter :: resolution(2) = [1280,720]
  integer,      parameter :: nx = L / dx
  integer,      parameter :: ny = int(L * (real(resolution(2),kind=real64)/resolution(1)) / dx)

  real(real64), allocatable :: grid(:,:)
  integer :: i

  gfx = gfx_init(width=resolution(1), height=resolution(2), title="Fortran visualization")


  allocate(grid(nx,ny))
  call gfx_add_grid_object(gfx, grid, vrange = [0.0_real64, 600.0_real64])

  call gfx_add_line_object(gfx, real([55,10],kind=real64), real([55,110],kind=real64),                    &
                                real([1,resolution(1)],kind=real64), real([1,resolution(2)],kind=real64), &
                                [0.0_real64,1.0_real64,0.0_real64])
  call gfx_add_line_object(gfx, real([10,55],kind=real64), real([110,55],kind=real64),                    &
                                real([1,resolution(1)],kind=real64), real([1,resolution(2)],kind=real64), &
                                [1.0_real64,0.0_real64,1.0_real64])

  grid = 300

  do
    do i=1, n_steps_per_frame
      call update_grid(grid, dt)
      grid(nx/2, ny/2) = 600
    end do

    call gfx_update(gfx)
    if (gfx%should_close) exit
  end do

  deallocate(grid)
  call gfx_terminate(gfx)

contains

  subroutine update_grid(grid, dt)
    real(real64), allocatable, intent(inout) :: grid(:,:)
    real(real64),              intent(in)    :: dt

    real(real64), allocatable :: laplacian(:,:)
    integer :: i, j

    allocate(laplacian, source=grid)

    laplacian = (0,0)
    do j=2, size(grid,2)-1
      do i=2, size(grid,1)-1
        laplacian(i,j) = (grid(i+1,j) - 2*grid(i,j) + grid(i-1,j) &
                       + grid(i,j+1) - 2*grid(i,j) + grid(i,j-1)) * dx**2
      end do
    end do

    grid = grid + laplacian*dt

    deallocate(laplacian)

  end subroutine update_grid

end program diffusion_demo

program tdse_demo
!==============================================================================#
! TDSE_DEMO
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
  logical :: should_close

  real(real64), parameter :: pi = 3.14159265_real64
  real(real64), parameter :: dx = 0.05_real64, dt=1.0_real64
  real(real64), parameter :: hbar = 1
  real(real64), parameter :: m_e  = 1
  real(real64), parameter  :: L = 10
  integer,      parameter :: nsteps = 100


  integer :: nx = L / dx
  integer :: ny = int(L * (1080./1920) / dx)
  complex(real64), allocatable :: psi(:,:)
  real(real64),    allocatable :: prob(:,:), V(:,:)
  real(real64) :: t, start, last_frame, frame_time
  integer :: i, j

  gfx = gfx_init(width=1920, height=1080, title="Fortran visualization")


  allocate(psi(nx,ny), V(nx,ny), prob(nx,ny))
  call gfx_add_grid_object(gfx, prob, vmax=20*dx**2, vmin=0.0_real64)

  V = 0

  call initialise_psi(psi)
  call normalise(psi)

  t = 0
  start = omp_get_wtime()
  last_frame = start
  do
    do i=1, nsteps
      call update_psi(psi, V, frame_time*i*dt)
    end do
    call normalise(psi)

    do j=1, size(psi,2)
      do i=1, size(psi,1)
        prob(i,j) = real(conjg(psi(i,j)) * psi(i,j),kind=real64) * dx**2
      end do
    end do

    call gfx_update(gfx)

    if (gfx%should_close) exit
    frame_time = omp_get_wtime() - last_frame
    last_frame = last_frame + frame_time

    t = t + nsteps * frame_time*dt
    write(*,"(a,f0.3,a,f0.3,a,f0.3,a,e0.3)") "wall time = ", last_frame-start, &
                                           ", simulation time = ", t, &
                                           ", fps = ", 1./frame_time, &
                                           ", |psi**2| = ", sum(prob)
  end do

  deallocate(psi)
  call gfx_terminate(gfx)

contains

  subroutine initialise_psi(psi)
    complex(real64), allocatable, intent(inout) :: psi(:,:)
    integer :: i, j

    psi = 0
    do j=2, ny-1
      do i=2, nx-1
        psi(i,j) = exp(-(((i-nx/2)*dx) ** 2 + ((j-ny/2)*dx)**2) / dx + (0,1) * 10 * (i*dx) )
      end do
    end do

  end subroutine initialise_psi

  subroutine update_psi(psi, V, dt)
    complex(real64), allocatable, intent(inout) :: psi(:,:)
    real(real64), allocatable, intent(in)    :: V(:,:)
    real(real64),              intent(in)    :: dt

    complex(real64), allocatable :: laplacian(:,:)
    complex(real64), allocatable :: dpsi_dt(:,:)

    integer :: i, j
    allocate(laplacian, source=psi)
    allocate(dpsi_dt, source=psi)

    laplacian = (0,0)
    do j=2, size(psi,2)-1
      do i=2, size(psi,1)-1
        laplacian(i,j) = (psi(i+1,j) - 2*psi(i,j) + psi(i-1,j) &
                       + psi(i,j+1) - 2*psi(i,j) + psi(i,j-1)) * dx**2
      end do
    end do

    dpsi_dt = 1/((0,1) * hbar) *( -(hbar ** 2 / 2*m_e) * laplacian + V*psi)
    psi = psi + dpsi_dt * dt

    deallocate(laplacian, dpsi_dt)

  end subroutine update_psi

  subroutine normalise(psi)
    complex(real64), allocatable, intent(inout) :: psi(:,:)
    real(real64) :: sum
    integer :: i, j

    sum = 0
    do j=1, size(psi,2)
      do i=1, size(psi,1)
        sum = sum + real(conjg(psi(i,j)) * psi(i,j), kind=real64) * dx**2
      end do
    end do
    psi = psi / sqrt(sum)

  end subroutine normalise

end program tdse_demo

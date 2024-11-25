module graphics
!==============================================================================#
! GRAPHICS
!------------------------------------------------------------------------------#
! Author:  Ed Higgins <ed.j.higgins@gmail.com>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2024-11-24
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use iso_c_binding
  use iso_fortran_env

  use gl
  use glfw
  implicit none

  private

  public :: gfx_init, gfx_update, gfx_terminate, gfx_add_grid_object

  type, public :: gfx_scene
    type(c_ptr)                    :: window
    integer(c_int)                 :: width, height
    logical                        :: should_close
    character(kind=c_char,len=128) :: title
    class(gfx_object), pointer     :: children

  end type gfx_scene

  type, abstract :: gfx_object
    class(gfx_object), pointer :: children
    class(gfx_object), pointer :: next
  contains
    procedure :: draw => gfx_draw_object
  end type gfx_object

  type, extends(gfx_object) :: gfx_grid_object
    real(real64), pointer :: grid(:,:)
    real(real64)          :: vmin, vmax
  end type gfx_grid_object

contains

  function gfx_init(width, height, title) result(gfx)
    integer,             intent(in)  :: width, height
    character(len=*),    intent(in)  :: title
    type(gfx_scene) :: gfx

    ! Initialize GLFW
    if (.not. glfwInit()) then
      stop 'Failed to initialize GLFW'
    end if

    gfx%children => null()

    gfx%width = int(width, kind=c_int)
    gfx%height = int(height, kind=c_int)
    gfx%title = trim(title)

    ! Create window
    gfx%window = glfwCreateWindow(gfx%width, gfx%height, trim(gfx%title)//c_null_char, c_null_ptr, c_null_ptr)

    if (c_associated(gfx%window) .eqv. .false.) then
      call glfwTerminate()
      stop 'Failed to create GLFW window'
    end if

    call glfwMakeContextCurrent(gfx%window)

  end function gfx_init

  subroutine gfx_add_grid_object(gfx, grid, vmin, vmax)
    class(gfx_scene),       intent(inout) :: gfx
    real(real64), target,   intent(in)    :: grid(:,:)
    real(real64),           intent(in)    :: vmin, vmax

    class(gfx_object),     pointer :: obj
    type(gfx_grid_object), pointer :: grid_obj

    allocate(grid_obj)
    grid_obj%grid => grid
    grid_obj%vmin = vmin
    grid_obj%vmax = vmax
    grid_obj%children => null()
    grid_obj%next => null()

    obj => gfx%children
    if (.not. associated(gfx%children)) then
      gfx%children => grid_obj
    else

      do while (associated(obj))
        obj => obj%next
      end do
      obj => grid_obj
    end if

  end subroutine gfx_add_grid_object

  subroutine gfx_update(gfx)
    type(gfx_scene),        intent(inout) :: gfx

    class(gfx_object), pointer :: obj
    real(c_float) :: ratio

    if (.not. glfwWindowShouldClose(gfx%window)) then
      gfx%should_close = .false.
      call glfwGetFramebufferSize(gfx%window, gfx%width, gfx%height)
      ratio = real(gfx%height, kind=c_float) / real(gfx%width, kind=c_float)

      call glViewport(0, 0, gfx%width, gfx%height)
      call glClear(GL_COLOR_BUFFER_BIT) 

      call glMatrixMode(GL_PROJECTION)
      call glLoadIdentity()
      call glOrtho(-ratio, ratio, -1.0_c_float, -1.0_c_float, 1.0_c_float, -1.0_c_float)
      call glMatrixMode(GL_MODELVIEW)
      call glLoadIdentity()

      obj => gfx%children
      do while (associated(obj))
        call obj%draw()
        obj => obj%next
      end do

      call glfwSwapBuffers(gfx%window)
      call glfwPollEvents()
    else
      gfx%should_close = .true.
    end if

  end subroutine gfx_update

  subroutine gfx_draw_object(obj)
    class(gfx_object), intent(in) :: obj

    select type (obj)
      type is (gfx_grid_object)
        call gfx_draw_grid_object(obj)
      class default
        error stop "Unrecognised type"
    end select

  end subroutine gfx_draw_object

  subroutine gfx_draw_grid_object(obj)
    class(gfx_grid_object), intent(in) :: obj

    real(c_float) :: x1, y1, x2, y2
    real(c_float) :: color
    integer :: i, j

    call glBegin(GL_QUADS) 

    do j = 1, size(obj%grid,2) - 1
      do i = 1, size(obj%grid,1) - 1
        ! Calculate vertex positions based on obj%grid indices
        x1 = -1.0_c_float + 2.0_c_float * (i - 1) / (size(obj%grid,1) - 1)
        y1 = -1.0_c_float + 2.0_c_float * (j - 1) / (size(obj%grid,2) - 1)
        x2 = -1.0_c_float + 2.0_c_float * i / (size(obj%grid,1) - 1)
        y2 = -1.0_c_float + 2.0_c_float * j / (size(obj%grid,2) - 1)

        ! Set color based on obj%grid value (example)
        color = real((obj%grid(i, j) - obj%vmin) / (obj%vmax - obj%vmin), kind=c_float)
        if (color <= 1 .and. color >= 0) then
          call glColor3f(color, color, color)
        else if (color > 0) then
          call glColor3f(1.0_c_float, 0.0_c_float, 0.0_c_float)
        else
          call glColor3f(0.0_c_float, 0.0_c_float, 1.0_c_float)
        end if

        ! Define vertices of the quad
        call glVertex2f(x1, y1)
        call glVertex2f(x2, y1)
        call glVertex2f(x2, y2)
        call glVertex2f(x1, y2)
      end do
    end do

    call glEnd()

  end subroutine gfx_draw_grid_object

  subroutine gfx_terminate(gfx)
    type(gfx_scene), intent(inout) :: gfx

    call glfwDestroyWindow(gfx%window)
    call glfwTerminate()

  end subroutine gfx_terminate

end module graphics

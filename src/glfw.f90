module glfw
!==============================================================================#
! OPENGL_VIS
!------------------------------------------------------------------------------#
! Author:  Ed Higgins <ed.j.higgins@gmail.com>
!------------------------------------------------------------------------------#
! Version: 0.1.1, 2024-11-24
!------------------------------------------------------------------------------#
! This code is distributed under the MIT license.
!==============================================================================#
  use iso_c_binding
  implicit none

  public


  interface
    function glfwInit() bind(C, name='glfwInit')
      use iso_c_binding
      implicit none
      logical(c_bool) :: glfwInit
    end function glfwInit

    subroutine glfwTerminate() bind(C, name='glfwTerminate')
      use iso_c_binding
      implicit none
    end subroutine glfwTerminate

    function glfwCreateWindow(width, height, title, monitor, share) &
         bind(C, name='glfwCreateWindow')
      use iso_c_binding
      implicit none
      integer(c_int), value :: width, height
      character(c_char), dimension(*), intent(in) :: title
      type(c_ptr), value :: monitor, share
      type(c_ptr) :: glfwCreateWindow
    end function glfwCreateWindow

    subroutine glfwDestroyWindow(window) bind(C, name='glfwDestroyWindow')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
    end subroutine glfwDestroyWindow

    subroutine glfwMakeContextCurrent(window) &
         bind(C, name='glfwMakeContextCurrent')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
    end subroutine glfwMakeContextCurrent

    function glfwGetCurrentContext() bind(C, name='glfwGetCurrentContext')
      use iso_c_binding
      implicit none
      type(c_ptr) :: glfwGetCurrentContext
    end function glfwGetCurrentContext

    subroutine glfwSwapBuffers(window) bind(C, name='glfwSwapBuffers')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
    end subroutine glfwSwapBuffers

    subroutine glfwSwapInterval(interval) bind(C, name='glfwSwapInterval')
      use iso_c_binding
      implicit none
      integer(c_int), value :: interval
    end subroutine glfwSwapInterval

    function glfwGetWindowAttrib(window, attrib) &
         bind(C, name='glfwGetWindowAttrib')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
      integer(c_int), value :: attrib
      integer(c_int) :: glfwGetWindowAttrib
    end function glfwGetWindowAttrib

    subroutine glfwSetWindowShouldClose(window, value) &
         bind(C, name='glfwSetWindowShouldClose')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
      logical(c_bool), value :: value
    end subroutine glfwSetWindowShouldClose

    function glfwWindowShouldClose(window) &
         bind(C, name='glfwWindowShouldClose')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
      logical(c_bool) :: glfwWindowShouldClose
    end function glfwWindowShouldClose

    subroutine glfwSetWindowTitle(window, title) &
         bind(C, name='glfwSetWindowTitle')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
      character(c_char), dimension(*), intent(in) :: title
    end subroutine glfwSetWindowTitle

    subroutine glfwGetWindowPos(window, xpos, ypos) &
         bind(C, name='glfwGetWindowPos')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
      integer(c_int), intent(out) :: xpos, ypos
    end subroutine glfwGetWindowPos

    subroutine glfwSetWindowPos(window, xpos, ypos) &
         bind(C, name='glfwSetWindowPos')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
      integer(c_int), value :: xpos, ypos
    end subroutine glfwSetWindowPos

    subroutine glfwGetWindowSize(window, width, height) &
         bind(C, name='glfwGetWindowSize')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
      integer(c_int), intent(out) :: width, height
    end subroutine glfwGetWindowSize

    subroutine glfwSetWindowSize(window, width, height) &
         bind(C, name='glfwSetWindowSize')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
      integer(c_int), value :: width, height
    end subroutine glfwSetWindowSize

    subroutine glfwGetFramebufferSize(window, width, height) &
         bind(C, name='glfwGetFramebufferSize')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
      integer(c_int), intent(out) :: width, height
    end subroutine glfwGetFramebufferSize

    subroutine glfwIconifyWindow(window) bind(C, name='glfwIconifyWindow')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
    end subroutine glfwIconifyWindow

    subroutine glfwRestoreWindow(window) bind(C, name='glfwRestoreWindow')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
    end subroutine glfwRestoreWindow

    subroutine glfwShowWindow(window) bind(C, name='glfwShowWindow')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
    end subroutine glfwShowWindow

    subroutine glfwHideWindow(window) bind(C, name='glfwHideWindow')
      use iso_c_binding
      implicit none
      type(c_ptr), value :: window
    end subroutine glfwHideWindow

    subroutine glfwPollEvents() bind(C, name='glfwPollEvents')
      use iso_c_binding
      implicit none
    end subroutine glfwPollEvents

    ! ... Add more GLFW functions as needed ...

  end interface

end module glfw

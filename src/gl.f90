module gl
!==============================================================================#
! GL
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

  integer(c_int), parameter :: GL_POINTS          = 0
  integer(c_int), parameter :: GL_LINES           = 1
  integer(c_int), parameter :: GL_LINE_LOOP       = 2
  integer(c_int), parameter :: GL_LINE_STRIP      = 3
  integer(c_int), parameter :: GL_TRIANGLES       = 4
  integer(c_int), parameter :: GL_TRIANGLE_STRIP  = 5
  integer(c_int), parameter :: GL_TRIANGLE_FAN    = 6
  integer(c_int), parameter :: GL_QUADS           = 7
  integer(c_int), parameter :: GL_QUAD_STRIP      = 8
  integer(c_int), parameter :: GL_POLYGON         = 9

  integer(c_int), parameter :: GL_MODELVIEW   = 5888
  integer(c_int), parameter :: GL_PROJECTION  = 5889

  integer(c_int), parameter :: GL_COLOR_BUFFER_BIT = 16384


  interface
    subroutine glClearColor(red, green, blue, alpha) bind(C, name='glClearColor')
      use iso_c_binding
      implicit none
      real(c_float), value :: red, green, blue, alpha
    end subroutine glClearColor

    subroutine glClear(mask) bind(C, name='glClear')
      use iso_c_binding
      implicit none
      integer(c_int), value :: mask
    end subroutine glClear

    subroutine glViewport(x, y, width, height) bind(C, name='glViewport')
      use iso_c_binding
      implicit none
      integer(c_int), value :: x, y, width, height
    end subroutine glViewport

    subroutine glBegin(mode) bind(C, name='glBegin')
      use iso_c_binding
      implicit none
      integer(c_int), value :: mode
    end subroutine glBegin

    subroutine glEnd() bind(C, name='glEnd')
      use iso_c_binding
      implicit none
    end subroutine glEnd

    subroutine glVertex2f(x, y) bind(C, name='glVertex2f')
      use iso_c_binding
      implicit none
      real(c_float), value :: x, y
    end subroutine glVertex2f

    subroutine glVertex3f(x, y, z) bind(C, name='glVertex3f')
      use iso_c_binding
      implicit none
      real(c_float), value :: x, y, z
    end subroutine glVertex3f

    subroutine glColor3f(red, green, blue) bind(C, name='glColor3f')
      use iso_c_binding
      implicit none
      real(c_float), value :: red, green, blue
    end subroutine glColor3f

    subroutine glMatrixMode(mode) bind(C, name='glMatrixMode')
      use iso_c_binding
      implicit none
      integer(c_int), value :: mode
    end subroutine glMatrixMode

    subroutine glLoadIdentity() bind(C, name='glLoadIdentity')
      use iso_c_binding
      implicit none
    end subroutine glLoadIdentity

    subroutine glOrtho(left, right, bottom, top, zNear, zFar) &
         bind(C, name='glOrtho')
      use iso_c_binding
      implicit none
      real(c_float), value :: left, right, bottom, top, zNear, zFar
    end subroutine glOrtho

    subroutine glFrustum(left, right, bottom, top, zNear, zFar) &
         bind(C, name='glFrustum')
      use iso_c_binding
      implicit none
      real(c_float), value :: left, right, bottom, top, zNear, zFar
    end subroutine glFrustum

    subroutine gluPerspective(fovy, aspect, zNear, zFar) &
         bind(C, name='gluPerspective')
      use iso_c_binding
      implicit none
      real(c_float), value :: fovy, aspect, zNear, zFar
    end subroutine gluPerspective

    subroutine gluLookAt(eyeX, eyeY, eyeZ, centerX, centerY, centerZ, upX, upY, upZ) &
         bind(C, name='gluLookAt')
      use iso_c_binding
      implicit none
      real(c_float), value :: eyeX, eyeY, eyeZ, centerX, centerY, centerZ, upX, upY, upZ
    end subroutine gluLookAt

    subroutine glTranslatef(x, y, z) bind(C, name='glTranslatef')
      use iso_c_binding
      implicit none
      real(c_float), value :: x, y, z
    end subroutine glTranslatef

    subroutine glRotatef(angle, x, y, z) bind(C, name='glRotatef')
      use iso_c_binding
      implicit none
      real(c_float), value :: angle, x, y, z
    end subroutine glRotatef

    subroutine glScalef(x, y, z) bind(C, name='glScalef')
      use iso_c_binding
      implicit none
      real(c_float), value :: x, y, z
    end subroutine glScalef

    subroutine glLineWidth(width) bind(C, name='glLineWidth')
      use iso_c_binding
      implicit none
      real(c_float) :: width
    end subroutine glLineWidth
  end interface

end module gl

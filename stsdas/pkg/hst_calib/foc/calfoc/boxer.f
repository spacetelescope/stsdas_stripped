c boxer -- compute area of box overlap
c Calculate the area common to input clockwise polygon x(n), y(n) with
c square (is, js) to (is+1, js+1).
c This version is for a quadrilateral.
c
c W.B. Sparks STScI 2-June-1990.
c Phil Hodge        20-Nov-1990  Change calling sequence; single precision.

      subroutine boxer (is, js, x, y, darea)

      integer is, js
      real x(*), y(*)
      real darea
c--
      real px(4), py(4), sum
      real sgarea
      integer i

c set up coords relative to unit square at origin
      do 10 i = 1, 4
         px(i) = x(i) - is
         py(i) = y(i) - js
 10   continue
c
c for each line in the polygon (or at this stage, input quadrilateral)
c calculate the area common to the unit square (allow negative area for
c subsequent `vector' addition of subareas).
      sum = 0.0
      do 20 i = 1, 3
         sum = sum + sgarea (px(i), py(i), px(i+1), py(i+1))
 20   continue
      sum = sum + sgarea (px(4), py(4), px(1), py(1))
      darea = sum

      return
      end




      real function sgarea (x1, y1, x2, y2)
c to calculate area under a line segement within unit square at origin.
      real x1, y1, x2, y2

      real m, c, dx
      real xlo, xhi, ylo, yhi, xtop
      logical negdx

      dx = x2 - x1
c trap vertical line
      if (dx .eq. 0.0) then
         sgarea = 0.0
         go to 80
      endif

c order the two input points in x
      if (x1 .lt. x2) then
         xlo = x1
         xhi = x2
      else
         xlo = x2
         xhi = x1
      end if
c and determine the bounds ignoring y for now
      if (xlo .ge. 1.0) then
         sgarea = 0.0
         go to 80
      endif
      if (xhi .le. 0.0) then
         sgarea = 0.0
         go to 80
      endif
      xlo = max (xlo, 0.0)
      xhi = min (xhi, 1.0)

c now look at y
c basic info about the line y = mx + c
      negdx = (dx .lt. 0.0)
      m     = (y2 - y1) / dx
      c     = y1 - m * x1
      ylo = m * xlo + c
      yhi = m * xhi + c
c trap segment entirely below axis
      if (ylo .le. 0.0 .and. yhi .le. 0.0) then
         sgarea = 0.0
         go to 80
      endif
c adjust bounds if segment crosses axis (to exclude anything below axis)
      if (ylo .lt. 0.0) then
         ylo = 0.0
         xlo = -c/m
      endif
      if (yhi .lt. 0.0) then
         yhi = 0.0
         xhi = -c/m
      endif

c There are four possibilities: both y below 1, both y above 1
c and one of each.

      if (ylo .ge. 1.0 .and. yhi .ge. 1.0) then
c line segment is entirely above square
         if (negdx) then
            sgarea = xlo - xhi
         else
            sgarea = xhi - xlo
         end if
         go to 80
      endif

      if (ylo .le. 1.0 .and. yhi .le. 1.0) then
c segment is entirely within square
         if (negdx) then
            sgarea = 0.5 * (xlo-xhi) * (yhi+ylo)
         else
            sgarea = 0.5 * (xhi-xlo) * (yhi+ylo)
         end if
         go to 80
      endif

c otherwise it must cross the top of the square
      xtop = (1.0 - c) / m

      if (xtop .lt. xlo) then
         call fmessg ('warning (sgarea):  xtop < xlo')
      else if (xtop .gt. xhi) then
         call fmessg ('warning (sgarea):  xtop > xhi')
      endif

      if (ylo .lt. 1.0) then
         if (negdx) then
            sgarea = -(0.5 * (xtop-xlo) * (1.0+ylo) + xhi - xtop)
         else
            sgarea = 0.5 * (xtop-xlo) * (1.0+ylo) + xhi - xtop
         end if
         go to 80
      endif

      if (negdx) then
         sgarea = -(0.5 * (xhi-xtop) * (1.0+yhi) + xtop-xlo)
      else
         sgarea = 0.5 * (xhi-xtop) * (1.0+yhi) + xtop-xlo
      end if

   80 return
      end

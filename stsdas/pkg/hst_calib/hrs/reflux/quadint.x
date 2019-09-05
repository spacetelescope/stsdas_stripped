# QUAD_INT -- Perform quadratic (Lagrangian) interpolation
#
# Task performs quadratic interpolation within input vectors x and y
# at positions in vector xout. The input vector x should be monitonic.
#
# S. Hulbert	Jul91	Original

procedure quad_int (x, y, nx, xout, nxout, yout)

real    x[ARB]           #I: input x
real    y[ARB]           #I: input y
int     nx               #I: number of input elements
real    xout[ARB]        #I: interpolated x
real    yout[ARB]        #O: interpolated y
int     nxout            #I: number of interpolated elements

int     ipos, nx3, i, ii, k
real    xx, x0, x1, x2, x3, a, b, c, d

begin

        # set up index in inputs
        ipos = 1

        # set upper limit in inputs
        nx3 = nx - 3

        # search through output looking for appropriate range in input
        do i = 1, nxout {

            # xx is the x value to interpolate
            xx = xout[i]

            # if too high decrease index to inputs
            do ii = 1, nx {
                if (ipos == 1 || xx > x[ipos])
                    break
                else
                    ipos = ipos - 1
            }

            # if too low decrease index to inputs
            do ii = 1, nx {
                if (ipos == (nx-1) || xx < x[ipos+1])
                    break
                else
                    ipos = ipos + 1
            }

            # set position of first of four points in input to use
            k = ipos - 1
            if (k < 1)
                k = 1
            if (k > nx3)
                k = nx3

            # get input x values to use in the interpolation
            x0 = x[k]
            x1 = x[k+1]
            x2 = x[k+2]
            x3 = x[k+3]

            # compute the weights 
            a = (xx-x1) * (xx-x2) / (x0-x1) / (x0-x2)
            b = ((xx-x0) / (x1-x0) + (xx-x3) / (x1-x3)) * (xx-x2) / (x1-x2)
            c = ((xx-x0) / (x2-x0) + (xx-x3) / (x2-x3)) * (xx-x1) / (x2-x1)
            d = (xx-x1) * (xx-x2) / (x3-x1) / (x3-x2)


            # do the interpolation
            yout[i] = (a*y[k] + b*y[k+1] + c*y[k+2] + d*y[k+3])/2.0

        }

end

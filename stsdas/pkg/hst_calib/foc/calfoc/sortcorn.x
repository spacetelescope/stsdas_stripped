#
#  Subroutine to take the 8 x,y pairs in (x,y) and make 5 new pairs
#  in (xx,yy) that can go into INTERPB
#
procedure sortcorn (X, Y, XX, YY)

real	x[8], y[8], xx[5], yy[5]
int	i, nymin
real	ymin

begin
	ymin = y[1]
	nymin = 1
	do i = 2, 4 {
	    if (y[i] < ymin) {
		ymin = y[i]
		nymin = i
	    }
	}
	if (x[nymin+1] > x[nymin+3]) {
	    xx[2] = x[nymin+3]
	    xx[4] = x[nymin+1]
	    yy[2] = y[nymin+3]
	    yy[4] = y[nymin+1]
	} else {
	    xx[2] = x[nymin+1]
	    yy[2] = y[nymin+1]
	    xx[4] = x[nymin+3]
	    yy[4] = y[nymin+3]
	}
	xx[3] = x[nymin+2]
	yy[3] = y[nymin+2]
	xx[1] = x[nymin]
	yy[1] = ymin
	xx[5] = xx[1]
	yy[5] = yy[1]
end

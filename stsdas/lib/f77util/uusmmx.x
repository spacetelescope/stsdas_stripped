include	<imhdr.h>
include	<iraf77.h>

# UUSMMX -- Set the IM_MIN, IM_MAX values.
#
# N Zarate June 1992

procedure uusmmx (im, datamin, datamax)
pointer im
real	datamin, datamax
long	clktime()

begin

	IM_MIN(im) = datamin
	IM_MAX(im) = datamax
	IM_LIMTIME(im) = clktime(long(0))
end

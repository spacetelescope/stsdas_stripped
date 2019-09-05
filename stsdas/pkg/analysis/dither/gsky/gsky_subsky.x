include <imhdr.h>

# subsky  --  Subtract sky from image.
#
#
# 12-Jul-1996 (IB)

procedure subsky (im, sky)

pointer im
real    sky

pointer	linei, lineo
int	i, j

pointer	imgl2r(), impl2r()

begin
	do j = 1, IM_LEN(im, 2) {
	    linei = imgl2r (im, j)
	    lineo = impl2r (im, j)
	    do i = 1, IM_LEN(im, 1)
	        Memr[lineo+i-1] = Memr[linei+i-1] - sky
	}
end

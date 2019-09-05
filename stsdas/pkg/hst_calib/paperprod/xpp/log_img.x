include <imhdr.h>

define MAX_EXPONENT 4

procedure log10p_img(im1, im2)

pointer	im1
pointer	im2

int	npix
long    v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer buf1, buf2
real    xif_elogr()
extern  xif_elogr()
int     imgnlr(), impnlr()

begin
        call amovkl (long(1), v1, IM_MAXDIM)
        call amovkl (long(1), v2, IM_MAXDIM)
 
        npix = IM_LEN(im1, 1)

        while ((imgnlr (im1, buf1, v1) != EOF) &&
            (impnlr (im2, buf2, v2) != EOF)) {

	    call aaddkr(Memr[buf1], 1., Memr[buf1], npix)
            call alogr (Memr[buf1], Memr[buf2], npix, xif_elogr)
	}
 
end

# This function is based on the following code for the 'imfunction' routine:
# IF_LOG10 -- Compute the base 10 logarithm of image1 and write the results to
# image2.
# 
#procedure if_log10r (im1, im2)
# 
#pointer im1                             # pointer to the input image
#pointer im2                             # pointer to the output image
# 
#int     npix
#long    v1[IM_MAXDIM], v2[IM_MAXDIM]
#pointer buf1, buf2
#real    if_elogr()
#extern  if_elogr()
#int     imgnlr(), impnlr()
# 
#begin
#        call amovkl (long(1), v1, IM_MAXDIM)
#        call amovkl (long(1), v2, IM_MAXDIM)
# 
#        npix = IM_LEN(im1, 1)
#        while ((imgnlr (im1, buf1, v1) != EOF) &&
#            (impnlr (im2, buf2, v2) != EOF))
#            call alogr (Memr[buf1], Memr[buf2], npix, if_elogr)
#end
# 
# 
# IF_ELOG -- The error function for log10. Note that MAX_EXPONENT is
# currently an integer so it is converted to the appropriate data type
# before being returned.
 
real procedure xif_elogr (x)
 
real    x                               # the input pixel value
 
begin
        return (real(-MAX_EXPONENT))
end

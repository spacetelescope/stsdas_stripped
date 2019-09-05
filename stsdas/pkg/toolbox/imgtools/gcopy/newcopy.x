include <imio.h>
include <imhdr.h>

procedure newcopy (im1, im2, datamin, datamax)

pointer im1, im2, buf1, buf2
int	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	npix, junk
int	imgnls(), imgnll(), imgnlr(),imgnld()
int	impnls(), impnlr(), impnll(),impnld()
real	datamin, datamax


begin
	   npix = IM_LEN(im1,1)
	   # Setup start vector for sequential reads and writes.
	   call amovkl (long(1), v1, IM_MAXDIM)
	   call amovkl (long(1), v2, IM_MAXDIM)

	   switch (IM_PIXTYPE(im1)) {
	   case TY_SHORT:
   	       while (imgnls (im1, buf1, v1) != EOF) {
		   junk = impnls (im2, buf2, v2)
		   call amovs (Mems[buf1], Mems[buf2], npix)
	       }
	   case TY_USHORT,TY_INT,TY_LONG:
	       while (imgnll (im1, buf1, v1) != EOF) {
		   junk = impnll (im2, buf2, v2)
		   call amovl (Meml[buf1], Meml[buf2], npix)
	       }
	   case TY_REAL:
	       while (imgnlr (im1, buf1, v1) != EOF) {
		   junk = impnlr (im2, buf2, v2)
		   call amovr (Memr[buf1], Memr[buf2], npix)
	       }
	   case TY_DOUBLE:
	       while (imgnld (im1, buf1, v1) != EOF) {
		   junk = impnld (im2, buf2, v2)
		   call amovd (Memd[buf1], Memd[buf2], npix)
	       }
	   default:
	       call error (1, "unknown pixel datatype")
	   }

end

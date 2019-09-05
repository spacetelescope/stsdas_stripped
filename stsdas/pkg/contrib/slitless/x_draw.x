include <imhdr.h>


procedure draw_rectangle (xs, xe, ys, ye)
# Draws a simple rectangle using the point (xs,ys) and (xe,ye)
# Can also be used to draw small dots on the display if xs=xe and ys=ye
real xs, xe, ys, ye

pointer im, iw
int frame, wcs_status
pointer imd_mapframe(), iw_open()
char image[SZ_FNAME]

real  xs2, xe2, ys2, ye2,wx,wy
int maxx, maxy

begin
#	call printf("Drawing a rectangle.\n")
	im = imd_mapframe (frame, READ_WRITE, YES)
	iw = iw_open (im, frame, image, SZ_FNAME, wcs_status)

	wx=xs
	wy=ys
	call iw_im2fb (iw, wx,wy, xs2, ys2)
	wx=xe
	wy=ye
	call iw_im2fb (iw, wx,wy, xe2, ye2)

	maxx=IM_LEN(im,1)
	maxy=IM_LEN(im,2)

	if (xs2 < 0) xs2 = 5.0
	if (xs2 > maxx) xs2 = maxx - 5.0
	if (xe2 < 0) xe2 = 5.0
	if (xe2 > maxx) xe2 = maxx - 5.0
	if (ys2 < 0) ys2 = 0.0
	if (ys2 > maxy) ys2 = maxy
	if (ye2 < 0) ye2 = 0.0
	if (ye2 > maxy) ye2 = maxy

	call mk_drawline(im, 10.0, 10.0, 20.0,20.0 , nint(xs2), nint(xe2), nint(ys2), nint(ys2), 5)
	call mk_drawline(im, 10.0, 10.0, 20.0,20.0, nint(xe2), nint(xe2),nint(ys2), nint(ye2), 5)
	call mk_drawline(im, 10.0, 10.0, 20.0,20.0, nint(xs2), nint(xe2), nint(ye2), nint(ye2), 5)
	call mk_drawline(im, 10.0, 10.0, 20.0,20.0, nint(xs2), nint(xs2), nint(ys2), nint(ye2), 5)

	call iw_close(iw)
	call imunmap(im)
end

# MK_DRAWLINE -- Procedure to draw lines.
 
procedure mk_drawline (im, ofx, ofy, fx, fy, x1, x2, y1, y2, graylevel)
# This procedure was copied from the one shipping with IRAF 
pointer im              # pointer to the frame buffer image
real    ofx, ofy        # previous coordinates
real    fx, fy          # current coordinates
int     x1, x2          # column limits
int     y1, y2          # line limits
int     graylevel       # picture gray level
 
int     i, j, ix1, ix2, npix, itemp
pointer vp
real    m, b
pointer imps2s()
 
begin
        # Compute the slope and intercept.
        if (x2 == x1) {
            vp = imps2s (im, x1, x2, y1, y2)
            npix = y2 - y1 + 1
            do i = 1, npix
                Mems[vp+i-1] = graylevel
        } else if (y2 == y1) {
            vp = imps2s (im, x1, x2, y1, y2)
            npix = x2 - x1 + 1
            do i = 1, npix
                Mems[vp+i-1] = graylevel
        } else {
            m = (fy - ofy ) / (fx - ofx)
            b = ofy - m * ofx
            #if (m > 0.0)
                #b = y1 - m * x1
            #else
                #b = y2 - m * x1
            do i = y1, y2 {
                if (i == y1) {
                    ix1 = nint ((i - b) / m)
                    ix2 = nint ((i + 0.5 - b) / m)
                } else if (i == y2) {
                    ix1 = nint ((i - 0.5 - b) / m)
                    ix2 = nint ((i - b) / m)
                } else {
                    ix1 = nint ((i - 0.5 - b) / m)
                    ix2 = nint ((i + 0.5 - b) / m)
                }
                itemp = min (ix1, ix2)
                ix2 = max (ix1, ix2)
                ix1 = itemp
                if (ix1 < x1 || ix2 > x2)
                    next
                vp = imps2s (im, ix1, ix2, i, i)
                npix = ix2 - ix1 + 1
                do j = 1, npix
                    Mems[vp+j-1] = graylevel
            }
        }
end

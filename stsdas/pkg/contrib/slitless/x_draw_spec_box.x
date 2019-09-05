include <imhdr.h>


procedure draw_spectral_box(cx,cy,ys,ye,angle,maxx1,maxx2,npix,nline)
# Draw an a box of size maxx1+maxx2 by width centered on (cx,cy) and with the angle theta
int cx,cy,ys,ye
int maxx1,maxx2
real angle
int npix,nline
int x1,x2,x3,x4,y1,y2,y3,y4
int xt,yt
real wx, wy
pointer im, iw
int frame, wcs_status
pointer imd_mapframe(), iw_open()
char image[SZ_FNAME]
int maxx,maxy
real xr1,xr2,xr3,xr4,yr1,yr2,yr3,yr4

begin
#	call printf("Displaying a spectral box.\n")
	x1 = (cx - maxx1)
	x2 = (cx + maxx2)
	x3 = (cx + maxx2)
	x4 = (cx - maxx1)
	y1 = (ys)
	y2 = (ys)
	y3 = (ye)
	y4 = (ye)

	im = imd_mapframe (frame, READ_WRITE, YES)
	iw = iw_open (im, frame, image, SZ_FNAME, wcs_status)

	call ip_rotate_coord(cx,cy,angle,x1,y1,xt,yt)
	wx = (xt)
	wy = (yt)
	call iw_im2fb (iw, wx, wy, xr1, yr1)

	call ip_rotate_coord(cx,cy,angle,x2,y2,xt,yt)
	wx= (xt)
	wy= (yt)
	call iw_im2fb (iw, wx,wy, xr2, yr2)

	call ip_rotate_coord(cx,cy,angle,x3,y3,xt,yt)
	wx= (xt)
	wy= (yt)
	call iw_im2fb (iw, wx,wy, xr3, yr3)

	call ip_rotate_coord(cx,cy,angle,x4,y4,xt,yt)
	wx= (xt)
	wy= (yt)
	call iw_im2fb (iw, wx,wy, xr4, yr4)

	maxx=IM_LEN(im,1)
	maxy=IM_LEN(im,2)

	if (xr1 < 0) xr1 = 1.0
	if (xr2 < 0) xr2 = 1.0
	if (xr3 < 0) xr3 = 1.0
	if (xr4 < 0) xr4 = 1.0
	if (yr1 < 0) yr1 = 1.0
	if (yr2 < 0) yr2 = 1.0
	if (yr3 < 0) yr3 = 1.0
	if (yr4 < 0) yr4 = 1.0
	if (xr1 > maxx) xr1 = maxx
	if (xr2 > maxx) xr2 = maxx
	if (xr3 > maxx) xr3 = maxx
	if (xr4 > maxx) xr4 = maxx
	if (yr1 > maxy) yr1 = maxy
	if (yr2 > maxy) yr2 = maxy
	if (yr3 > maxy) yr3 = maxy
	if (yr4 > maxy) yr4 = maxy

	call mk_drawline(im, xr1, yr1, xr2, yr2, min(nint(xr1),nint(xr2)),
		max(nint(xr1),nint(xr2)),
		min(nint(yr1),nint(yr2)),
		max(nint(yr1),nint(yr2)),255)

	call mk_drawline(im, xr2, yr2, xr3, yr3, min(nint(xr2),nint(xr3)),
		max(nint(xr2),nint(xr3)),
		min(nint(yr2),nint(yr3)),
		max(nint(yr2),nint(yr3)),255)

	call mk_drawline(im, xr3, yr3, xr4, yr4, min(nint(xr3),nint(xr4)),
		max(nint(xr3),nint(xr4)),
		min(nint(yr3),nint(yr4)),
		max(nint(yr3),nint(yr4)),255)

	call mk_drawline(im, xr4, yr4, xr1, yr1, min(nint(xr4),nint(xr1)),
		max(nint(xr4),nint(xr1)),
		min(nint(yr4),nint(yr1)),
		max(nint(yr4),nint(yr1)),255)

	call iw_close(iw)
	call imunmap(im)
end
	
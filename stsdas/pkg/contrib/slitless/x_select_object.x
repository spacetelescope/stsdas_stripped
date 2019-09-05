
include <imhdr.h>

procedure select_obj(in,cx,cy)
# Allows the user to select a location of the display.
# This procedure either computed the centroid of a selected region,
# or it uses the value of a single point.
pointer in
int cx,cy
real wx1, wx2, wy1, wy2, temp
int wcs, key
char strval[SZ_LINE]
int nx, ny, xp, yp
pointer imgs2r()
int halfwidth
int junk, clgcur()

begin
	call printf("PLEASE SELECT AN OBJECT. \n")
	call printf("Select first corner, press the x key (s for single point)\n")
	junk = clgcur ("imcur",wx1,wy1,wcs,key,strval,SZ_LINE)

	if(key!='s') {
		call printf("Select second corner, press the x key\n")
		junk = clgcur ("imcur",wx2,wy2,wcs,key,strval,SZ_LINE)

		if(wx2<wx1) {
			temp=wx1
			wx1=wx2
			wx2=temp
		}
		if(wy2<wy1) {
			temp=wy1
			wy1=wy2
			wy2=temp
		}
	}
	else {
		wx2 = wx1
		wy2 = wy1
	}
	halfwidth = 10 


	nx = nint(wx2 - wx1 + 1)
	ny = nint(wy2 - wy1 + 1)

	if(nx<2 || ny <2) {
	# Discard window
		halfwidth=0	
	#Fix range to fit inside the image
		wx1 = max (wx1 - halfwidth, 1.)
		wx2 = min (wx2 + halfwidth, real(IM_LEN(in,1)))
		wy1 = max (wy1 - halfwidth, 1.)	
		wy2 = min (wy2 + halfwidth, real(IM_LEN(in,2)))
		nx = nint(wx2 - wx1 + 1)
		ny = nint(wy2 - wy1 + 1)
	}
	if (halfwidth != 0) {
		call printf("Computing centroid\n")
		call find_centroid (Memr[imgs2r(in,nint(wx1),nint(wx2),nint(wy1),nint(wy2))], nx, ny, xp, yp)
		cx = nint(wx1) + xp - 1
		cy = nint(wy1) + yp - 1
		call printf("Centroid=(%d,%d)\n")
			call pargi(cx)
			call pargi(cy)
	}
	else {
		cx = wx1
		cy = wy1
	}
end

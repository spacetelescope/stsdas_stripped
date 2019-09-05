procedure select_spectral_box(data,npix,nline,cx,cy,bs,be,width)
# Computes the average line profile and display it in the graphic window. 

real data[npix,nline]
int npix,nline
int cx,cy
int bs, be
int x,y
int width
real avgprof[1024]
int gmode
pointer gopen(), gp
real xmin,xmax,ymin,ymax
int halfwidth
int wcs, key
char strval[SZ_LINE]
real wx2,wy2
int x1t,x2t,y1t,y2t
int junk, clgcur()

begin
	call printf("PLEASE SELECT THE EXTRACTION REGION..\n")
	halfwidth=10
	x1t = max(1,cx - halfwidth)
	x2t = min(npix, cx + halfwidth)
	y1t = max(1,cy - halfwidth)
	y2t = min(nline, cy + halfwidth)
#call printf("%d %d %d %d\n")
#	call pargi(x1t)
#	call pargi(x2t)
#	call pargi(y1t)
#	call pargi(y2t)

	do y=y1t, y2t {
# Average 10 columns centered on object position
		do x=x1t, x2t { 
			avgprof[y] = avgprof[y] + data[x,y]/(x2t - x1t +1 )
		}
	}

	xmin=(cy-halfwidth)*1.0
	xmax=(cy+halfwidth)*1.0
	
	ymin=avgprof[cy]
	ymax=avgprof[cy]

	do y=cy-halfwidth, cy+halfwidth {
		if (avgprof[y] < ymin) ymin=avgprof[y]
		if (avgprof[y] > ymax) ymax=avgprof[y]
	}
	
	 gmode=NEW_FILE
        gp = gopen("stdgraph",gmode,STDGRAPH)
        call gswind (gp,xmin, xmax, ymin, ymax)
        call gsview (gp, 0.15,0.85, 0.15,0.85)
        call gamove (gp, (cy-halfwidth)*1.0, avgprof[cy-halfwidth])

        do y=cy-halfwidth, cy+halfwidth {
                call gadraw(gp, y*1.0, avgprof[y])
        }
	call glabax (gp, "Average line profile","Line number","ADU")
	call gflush(gp)
	call gclose(gp)

	call printf("Select lower limit for spectrum extraction in image display, press x\n")
	junk = clgcur ("imcur",wx2,wy2,wcs,key,strval,SZ_LINE)
	bs = nint(wy2)

	call printf("Select upper limit for spectrum extraction in image display, press x\n")
	junk = clgcur ("imcur",wx2,wy2,wcs,key,strval,SZ_LINE)
	if (wy2 < bs) {
		be = bs
		bs = wy2
	}
	else {
		be = wy2
	}

	width = be - bs + 1
end

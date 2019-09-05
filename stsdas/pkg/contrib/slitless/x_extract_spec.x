procedure ip_extract_spectrum(im,npix,nline,cx,cy,maxx1,maxx2,ys,ye,width,angle,spectrum,mingood,maxgood,dtq,usedtq,dtqstr)
# Extract a spectra of size maxx1+ maxx2 by width around the pixel cx,cy and with angle angle.
int npix, nline
int maxx1,maxx2
int ys,ye
real mingood,maxgood
real im[npix,nline]
real dtq[npix,nline]
real spectrum[maxx1+maxx2,2] # 1 == flux, 2 == used for temp storage here!!
int width
int cx, cy
int x,y,x2
int xp,yp
real angle
int xmin, xmax
int lowgood,highgood,usedtq, baddtq
real bad,total
char dtqstr[SZ_LINE]
begin
	call printf("Extracting the spectrum.%s\n")
	call pargstr(dtqstr)
	lowgood = 1
	highgood = 1
	bad = 0 
	total = 0

	do x = 1, (maxx1+ maxx2) {
		spectrum[x,1] = 0
		spectrum[x,2] = 0
	}
	do y = ys, ye {
		xmin = cx - maxx1
		xmax = cx + maxx2
		do x = xmin, xmax {
			total = total + 1
			call ip_rotate_coord(cx,cy,angle,x,y,xp,yp)
			if(xp > 0 && xp < npix && yp > 1 && yp < nline) {
				x2 = x - xmin + 1

				if(!IS_INDEF(mingood) && im[xp,yp]<mingood) lowgood = 0
				if(!IS_INDEF(maxgood) && im[xp,yp]>maxgood) highgood = 0
			call tellbits(dtq[xp,yp],dtqstr,baddtq)
				if(baddtq==1) lowgood=0

				if(lowgood == 1 && highgood == 1) {
					spectrum[x2,2]=spectrum[x2,2]+1
					spectrum[x2,1]=spectrum[x2,1]+im[xp,yp]	
				}
				else {
					bad = bad + 1
				}		
		#		call draw_rectangle(1.0*(xp),1.0*(xp),1.0*(yp),1.0*(yp))
			}
		}
	}
	call printf("%f percent pixels rejected by DTQ, mingood, and maxgood.\n")
		call pargr(bad/total)	

end

procedure ip_extract_sky(im,npix,nline,cx,cy,maxx1,maxx2,ys,ye,angle,spectrum,mingood,maxgood,dtq,usedtq,dtqstr)
# Extract a spectra of size maxx1+ maxx2 by width around the pixel cx,cy and with angle angle.
int npix, nline
int maxx1,maxx2
int ys,ye
real mingood,maxgood
int lowgood,highgood
real im[npix,nline]
real dtq[npix,nline]
real spectrum[maxx1+maxx2,2] # 1 == flux, 2 == used for temp storage here!!
int cx, cy
int x,y,x2
int xp,yp
real angle
int xmin,xmax
real bad
int usedtq
char dtqstr[SZ_LINE]
real total 
int baddtq

begin
	lowgood = 1
	highgood = 1
	bad = 0
	total = 0
	call printf("Extracting the spectral background. About (%d,%d), columns (-%d,+%d)\n")
		call pargi(cx)
		call pargi(cy)
		call pargi(maxx1)
		call pargi(maxx2)
	do x = 1, (maxx1+maxx2) {
		spectrum[x,1] = 0
		spectrum[x,2] = 0
	}
	do y = ys, ye {
		xmin = cx - maxx1
		xmax = cx + maxx2
		do x = xmin, xmax {
			total = total + 1
			call ip_rotate_coord(cx,cy,angle,x,y,xp,yp)
			if(xp > 0 && xp < npix && yp > 1 && yp < nline) {
				x2 = x - xmin + 1

				if(!IS_INDEF(mingood) && im[xp,yp]<mingood) lowgood = 0
				if(!IS_INDEF(maxgood) && im[xp,yp]>maxgood) highgood = 0
				call tellbits(dtq[xp,yp],dtqstr,baddtq)
				if(baddtq==1) lowgood=0
				if(lowgood == 1 && highgood == 1) {
					spectrum[x2,2]=spectrum[x2,2]+1
					spectrum[x2,1]=spectrum[x2,1]+im[xp,yp]
				}
				else
				{
					bad = bad + 1
				}
			#	call draw_rectangle(1.0*(xp),1.0*(xp),1.0*(yp),1.0*(yp))
			}
		}
	}
	# Normalize
	do x = 1,(maxx1+maxx2) {
		if (spectrum[x,2] > 0) {
			spectrum[x,1]=spectrum[x,1]/spectrum[x,2]	
		}
		else spectrum[x,1]=0
		spectrum[x,2]=x
	}
	call printf("%f percent pixels rejected by DTQ, mingood, and maxgood.\n")
		call pargr(bad/total)

end

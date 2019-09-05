procedure extract_sky(in, sky, tmpsky,npix,nline,cx,cy,angle,maxx1,maxx2,mingood,maxgood,dtq,usedtq,dtqsky)
# Allows the user to select region of the image to be added together to
# generate an approzimate background spectrum

pointer in
int npix, nline, cx, cy, maxx1, maxx2
real mingood,maxgood
real angle
real sky[maxx1+maxx2,2], tmpsky[maxx1+maxx2,2]
real dtq[npix,nline]
int n, nrow, tnrow
real wx1, wx2, wy1, wy2, temp
int wcs, key
char strval[SZ_LINE], title[SZ_LINE]
pointer imgs2r()
int x
int usedtq
char dtqsky[ARB]
int junk, clgcur()

begin
#	call printf("Extracting the background.\n")
	n=1
	nrow=0

	do x=1, (maxx1+maxx2) {
		sky[x,1]=0
		sky[x,2]=x
		tmpsky[x,1]=0
		tmpsky[x,2]=x
	}
	while(1==1) {

		call printf("Select region(%d) of sky\n")
			call pargi(n)
		call printf("Select 1st corner of region.  Press the x key, r to restart, q to quit...\n")
		junk = clgcur ("imcur",wx1,wy1,wcs,key,strval,SZ_LINE)
		if (key == 'q') break

		if (key == 'r') {
			call printf("Restarting, zeroing sky estimate\n")
			do x=1, (maxx1+maxx2) {
				sky[x,1]=0
				sky[x,2]=x
			}
			nrow = 0
			n = 1
			call sprintf(title,SZ_LINE,"Current background average (%d lines)")
			call pargi(n)
			call display_spec(sky,1.0,npix,"Sky",0)
			next
		}

		call printf("Select 2nd corner of region. Press the x key ...\n")
		junk = clgcur ("imcur",wx2,wy2,wcs,key,strval,SZ_LINE)
		if(key == 'q') break
		if(wy2<wy1) {
			temp=wy1
			wy1=wy2
			wy2=temp
		}

		tnrow = 0
		do x=1, (maxx1+maxx2) {
			tmpsky[x,1]=0
			tmpsky[x,2]=x
		}

		call ip_extract_sky(Memr[imgs2r(in,1,npix,1,nline)],npix,nline,cx,cy,maxx1,maxx2,nint(wy1),nint(wy2),angle,tmpsky,mingood,maxgood,dtq,usedtq,dtqsky)
		tnrow = nint(wy2 - wy1) + 1
		n = n +1
# Display the sky
		call printf("Displaying current sky estimate\n")
		call sprintf(title,SZ_LINE,"Average (%d lines), new (%d lines)")
			call pargi(nrow)
			call pargi(tnrow)
		call disp2_spec(sky,tmpsky,1.0/(0.000001+nrow*1.0),1.0/(wy2 - wy1 + 1),(maxx1+maxx2),(maxx1+maxx2),title,0)
 		call printf("Press a to accept\n")
		junk = clgcur ("imcur",wx2,wy2,wcs,key,strval,SZ_LINE)
		if(key == 'a') {
			nrow = nrow + tnrow
			call printf("  %d lines selected (total=%d)\n")
				call pargi(tnrow)
				call pargi(nrow)
			do x = 1, (maxx1+maxx2) {
				sky[x,1] = sky[x,1] + tmpsky[x,1]
			}
		}
	}
	if (nrow!=0) {
		do x=1, (maxx1+maxx2) {
			sky[x,1] = sky[x,1]/(nrow*1.0)
		}
	}
end

procedure tellbits(n,a,bad)
real n,m
int b,r
real div
char a[SZ_LINE]
int bad

begin
	bad = 0
	m = n
	do b = 1, 16 {
		div = (2.0)**((17-b)*1.0)
		r = int(m/div)

#call printf("%d %c\n")
#	call pargi(r)
#	call pargc(a[17-b])

		if (r == 1) {
			if(a[17-b]=='1') {
				bad = 1
call printf("Pixel rejected on basis of byte:%d \n")
	call pargi(17-b)
			}
		}
		m = m - r*div
	}
end

procedure add_rows(rows, result, npix, nline)
# Co-adds all the rows of an array rows and return the
# result in result.
real rows[npix,nline]
real result[npix,2]
int npix, nline
int x,y

begin
#	call printf("Adding rows.\n")
	do x=1, npix {
		do y=1, nline {
			result[x,1] = result[x,1] +  rows[x,y]
		}
	}
end






procedure ip_scalespec(cx, cy, xscale, lambda_zero, lambda_scale,spectrum, npix)
# Uses the linear relation w = lambda_zero + lambda_scale * (x - xscale*cx)
# to determine the wavelength w of a pixel x.
int cx, cy, npix
real lambda_zero, lambda_scale,xscale
real spectrum[npix,2]
int x

begin
#	call printf("Scaling the extracted spectrum.\n")
	do x=1, npix {
		spectrum[x,2] = ((x-cx)*xscale)*lambda_scale+lambda_zero
	}
end

procedure sub_sky(spectrum, sky, width, npix)
# Substract the spectrum sky from spectrum
real spectrum[npix,2]
real sky[npix,2]
int width
int npix
int x

begin
#	call printf("Substracting the background.\n")
	do x=1,npix {
		spectrum[x,1] = spectrum[x,1] - sky[x,1]*width
	}
end


procedure ip_rotate_coord(cx,cy,theta,x,y,xp,yp)
# Rotates a coordinate pair (x,y) by the angle theta about the point (cx,cy)
# The result is returned in (xp,yp)
int cx,cy
real theta
real i,j
int x,y
int xp,yp

begin
	i = x - cx	
	j = y - cy
	xp = nint(i*cos(theta) + j*sin(theta))
	yp = nint(-i*sin(theta) + j*cos(theta))
	
	xp = xp + cx
	yp = yp + cy
end





procedure find_centroid (a, nx, ny, xp, yp)
# Computes and return the centroid (xp,yp) of an array a.
real a[nx,ny]
int nx, ny
int xp, yp
int x,y

real px[100]
real py[100]
real xm,ym
real sum1,sum2

begin
#	call printf("Computing the centroid.\n")



	do x = 1, nx {
		px[x]=0
		do y = 1, ny {
			px[x] = px[x] + a[x,y]
		}
	}
	
	do y = 1, ny {
		py[y]=0
		do x = 1, nx {
			py[y] = py[y] + a[x,y]
		}
	}
	
	xm = 0
	do x = 1, nx {
		xm = xm + px[x]
	}
	xm = xm / (1.0*nx)

	ym = 0
	do y = 1, ny {
		ym = ym + py[y]
	}
	ym = ym / (1.0*ny)

	sum1 = 0
	sum2 = 0
	do x = 1, nx {
		if (px[x] >= xm) {
			sum1 = sum1 + (px[x]-xm)*x
			sum2 = sum2 + (px[x]-xm)
		}
	}

if (sum2 == 0) sum2 = 1.0
	xp = sum1/sum2

	sum1 = 0
	sum2 = 0
	do y = 1, ny {
		if (py[y] >= ym) {
			sum1 = sum1 + (py[y]-ym)*y
			sum2 = sum2 + (py[y]-ym)
		}
	}

if (sum2 == 0) sum2 = 1.0
	yp = sum1/sum2



call printf("NEW: xp:%d yp:%d\n")
	call pargi(xp)	
	call pargi(yp)


end
	

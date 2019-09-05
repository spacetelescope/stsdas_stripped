include <gset.h>

procedure display_spec(spectrum, mul, npix, title, mode)
# Displays spectrum in a graphical window
int npix
int mode
real mul
real spectrum[npix,2]
char title[SZ_LINE]
pointer gopen(), gp
int gmode, x
real xmin, xmax, ymin, ymax

begin
#	call printf("Displaying a spectrum.\n")
	xmin=spectrum[1,2]
	ymin=spectrum[1,1]*mul
	xmax=spectrum[1,2]
	ymax=spectrum[1,1]*mul

	do x=1, npix {
		if (spectrum[x,2]<xmin) xmin=spectrum[x,2]
		if (spectrum[x,2]>xmax) xmax=spectrum[x,2]
		if (spectrum[x,1]*mul<ymin) ymin=spectrum[x,1]*mul
		if (spectrum[x,1]*mul>ymax) ymax=spectrum[x,1]*mul
	}

	if(ymin<0) ymin=0
	if (mode==0) gmode=NEW_FILE
	if (mode!=0) gmode=APPEND
	gp = gopen("stdgraph",gmode,STDGRAPH)
	call gswind (gp,xmin, xmax, ymin, ymax)
	call gsview (gp, 0.15,0.85, 0.15,0.85)
	call gamove (gp, spectrum[1,2], spectrum[1,1]*mul)
	do x=1, npix-1 {
		call gadraw(gp, spectrum[x,2], spectrum[x,1]*mul)
	}
	call glabax (gp, title, "Wavelength","ADU")
	call gflush(gp)
	call gclose(gp)
end

procedure disp2_spec(spectrum1,spectrum2, mul1, mul2, npix1, npix2, title, mode)
# Displays to spectra in the same window for comparison.
int npix1, npix2
int mode
real mul1, mul2
real spectrum1[npix1,2], spectrum2[npix2,2]
char title[SZ_LINE]
pointer gopen(), gp
int gmode, x
real xmin, xmax, ymin, ymax

begin
#	call printf("Displaying two spectra. \n")
	xmin=spectrum1[1,2]
	ymin=spectrum1[1,1]*mul1
	xmax=spectrum1[1,2]
	ymax=spectrum1[1,1]*mul1
	do x=1, npix1 {
		if (spectrum1[x,2]<xmin) xmin=spectrum1[x,2]
		if (spectrum1[x,2]>xmax) xmax=spectrum1[x,2]
		if (spectrum1[x,1]*mul1<ymin) ymin=spectrum1[x,1]*mul1
		if (spectrum1[x,1]*mul1>ymax) ymax=spectrum1[x,1]*mul1
	}
	do x=1, npix2 {
		if (spectrum2[x,2]<xmin) xmin=spectrum2[x,2]
		if (spectrum2[x,2]>xmax) xmax=spectrum2[x,2]
		if (spectrum2[x,1]*mul2<ymin) ymin=spectrum2[x,1]*mul2
		if (spectrum2[x,1]*mul2>ymax) ymax=spectrum2[x,1]*mul2
	}
	if(ymin<0.) ymin=max(-10.,ymin)
	if (mode==0) gmode=NEW_FILE
	if (mode!=0) gmode=APPEND
	gp = gopen("stdgraph",gmode,STDGRAPH)
	call gswind (gp,xmin, xmax, ymin, ymax)
	call gsview (gp, 0.15,0.85, 0.15,0.85)
	call gamove (gp, spectrum1[1,2], spectrum1[1,1]*mul1)
	do x=1, npix1-1 {
		call gadraw(gp, spectrum1[x,2], spectrum1[x,1]*mul1)
	}
	call gamove (gp, spectrum2[1,2], spectrum2[1,1]*mul2)
	do x=1, npix2-1 {
		call gmark(gp, spectrum2[x,2], spectrum2[x,1]*mul2,
			GM_POINT, 5., 5.)
	}
	call glabax (gp, title, "Wavelength","ADU")
	call gflush(gp)
	call gclose(gp)
end

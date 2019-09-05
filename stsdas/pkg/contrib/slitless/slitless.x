task slitless=t_slitless

# Compile with : xc slitless.x -lds


include <imhdr.h>
include <error.h>
include <gset.h>
include <mach.h>
include <ctype.h>
include <error.h>

procedure t_slitless()

char image[SZ_LINE]
char grism[SZ_LINE]
char command[SZ_LINE]
pointer in, in2, gdtq #, ddtq
pointer immap()
int npixg, nlineg, npixd, nlined
int cx, cy, width, halfwidth, cxg, cyg
int swidth1,swidth2
pointer spectrum, sky, tmpsky
pointer imgs2r()
real lambda_step,lambda_zero, imgetr()
real ra,dec
char rastr[SZ_LINE],decstr[SZ_LINE], title[SZ_LINE]
char filename[SZ_LINE], output[SZ_LINE]
int bs, be, sbs, sbe, tmp
real ccx,ccy,angle,ccx2,ccy2
int maxx1,maxx2
real mingood,maxgood
real clgetr()
int clgeti()
real xscale,yscale
char instrument[SZ_LINE], filterkey[SZ_LINE], filtername[SZ_LINE]
bool streq(),autofilter,clgetb(), auto, override, skysub, outfits
int gdtqin, ddtqin, gdatin, ddatin
int useddtq,usegdtq
char dtqstr[SZ_LINE]
int strlen()


begin

	call printf("\nSlitless v. 0.92\n")
	call printf("----------------\n")
	call clgstr("image",image,SZ_LINE)
	call clgstr("grism",grism,SZ_LINE)
	call clgstr("output",output,SZ_LINE)

	autofilter = clgetb("autofilter")
	auto = clgetb("auto")
	override = false
	skysub = clgetb("skysub")
	outfits = clgetb("outfits")

	swidth1 = clgeti("swidth1")
	swidth2 = clgeti("swidth2")

	useddtq = 1
	usegdtq = 1


	gdatin=clgeti("gdatin")
	gdtqin=clgeti("gdtqin")
	ddatin=clgeti("gdatin")
	ddtqin=clgeti("gdtqin")


# Reading the images in:

	call sprintf(filename,SZ_LINE,"%s[%d]")
		call pargstr(image)
		call pargi(ddatin)
	in = immap(filename, READ_ONLY,0)

	call sprintf(filename,SZ_LINE,"%s[%d]")
		call pargstr(grism)
		call pargi(gdatin)
	in2 = immap(filename, READ_ONLY,0)

# Get sizes of images

	npixd = IM_LEN(in,1)
	nlined = IM_LEN(in,2)
	npixg = IM_LEN(in2,1)
	nlineg = IM_LEN(in2,2)

	if (autofilter) {
		call imgstr(in2,"INSTRUME",instrument,SZ_LINE)
		call printf("INSTRUMENT : %s ")
			call pargstr(instrument)

		if (streq(instrument,"NICMOS")) {
			call strcpy("FILTER",filterkey,SZ_LINE)
		}
		if (streq(instrument,"STIS")) {
			call strcpy("OPT_ELEM",filterkey,SZ_LINE)
		}

		call imgstr(in2,filterkey,filtername,SZ_LINE)
		call printf("FILTER: %s\n")
			call pargstr(filtername)

		call read_filter_config("slitless$slitless.cfg",instrument,filtername, maxx1, maxx2, ccx, ccy, lambda_zero, lambda_step, angle)

	}

	
	if (!autofilter) {
		call printf("Getting grism information for parameter file\n")
		lambda_step=clgetr("step_lambda")
		if(IS_INDEF(lambda_step)) {
			call error(1,"step_lambda is not defined in the parameter file")
		}
		lambda_zero=clgetr("zero_lambda")
		if(IS_INDEF(lambda_zero)) {
			call error(1,"zero_lambda is not defined in the parameter file")
		}
		ccx=clgetr("ccx")
		if(IS_INDEF(ccx)) {
			call error(1,"ccx is not defined in the parameter file")
		}
		ccy=clgetr("ccy")
		if(IS_INDEF(ccy)) {
			call error(1,"ccy is not defined in the parameter file")
		}
		angle=clgetr("angle")
		if(IS_INDEF(angle)) {
			call error(1,"angle is not defined in the parameter file")
		}
		angle = angle/180.0*3.1415
		maxx1=clgeti("maxx1")
		if(IS_INDEF(maxx1)) {
			call error(1,"maxx1 is not defined in the parameter file")
		}
		maxx2=clgeti("maxx2")
		if(IS_INDEF(maxx2)) {
			call error(1,"maxx2 is not defined in the parameter file")
		}

	}

# Even if autofilter mode is ON, if the ccx and ccy keywords are set then they
# are used!!
		ccx2=clgetr("ccx")
		if(!IS_INDEF(ccx2)) {
			call printf("\nWARNING: ccx is DEFINED in the parameter file, overwritting the default")
			ccx = ccx2
		}
		ccy2=clgetr("ccy")
		if(!IS_INDEF(ccy2)) {
			call printf("\nWARNING: ccy is DEFINED in the parameter file, overwritting the default")
			ccy = ccy2
		}






	if(IS_INDEFI(gdatin)) {
		call printf("gdatin is not defined in the parameter file. Setting it to 0.\n")
		ddatin = 0
		gdatin = 0
	}
	if(IS_INDEFI(gdtqin)) {
		call printf("gdtqin is not defined in the parameter file. Disabling use of grism image  DTQ.\n")
		usegdtq = 0
		useddtq = 0
	}
# Reading the images DTQ in:
# IF DTQ is not use then read the data frame again, and set everything to zero in there. 
	if (useddtq == 0) ddtqin = ddatin
#	call sprintf(filename,SZ_LINE,"%s[%d]")
#		call pargstr(image)
#		call pargi(ddtqin)
#	ddtq = immap(filename, READ_ONLY,0)

	if (usegdtq == 0) gdtqin = gdatin
	call sprintf(filename,SZ_LINE,"%s[%d]")
		call pargstr(grism)
		call pargi(gdtqin)
	gdtq = immap(filename, READ_ONLY,0)

	call clgstr("dtqstr",dtqstr,SZ_LINE)
	if (usegdtq==1 && strlen(dtqstr)!=16) {
		call error(1,"The DTQ String must contain 16 digits.")
	}

	mingood=clgetr("mingood")
	maxgood=clgetr("maxgood")
	call printf("Excluding values that are lower than %f and greater than %f\n")
		call pargr(mingood)
		call pargr(maxgood)
# The spectral box width is always read. It is used to show where the spectra is 
# AND when automatic mode is turned on.	
	width=clgeti("width")

# If the dispersion parameters are zero, read them from the image:
	if (lambda_zero == 0.0 && lambda_step == 0.0) {
		call printf("Reading dispersion relation from image\n")
		lambda_zero = imgetr(in2,"CRVAL1")
		lambda_step = imgetr(in2,"CD1_1")
	}
	call printf("Lambda_zero: %f Lambda_step: %f\n")
		call pargr(lambda_zero)
		call pargr(lambda_step)
	call printf("Dispersion left extent: %d pix. Dispersion right extent: %d pix.\n")
		call pargi(maxx1)
		call pargi(maxx2)
# Form title of image from the aperture position

	ra = imgetr(in2,"RA_TARG")
	dec = imgetr(in2,"DEC_TARG")
	call sprintf(rastr,SZ_LINE,"%02d%02d")
		call pargi(abs(int(int(ra)/15.0)))
		call pargi(abs( int((ra/15.0 - int(int(ra)/15.0))*60)) )
	if(dec>=0) 
		call sprintf(decstr,SZ_LINE,"+%02d%02d")
	else
		call sprintf(decstr,SZ_LINE,"-%02d%02d")

		call pargi(abs(int(int(dec))))
		call pargi(abs(int((dec - int(int(dec)))*60)) )



	call printf("Displaying the direct image %s[%d] (%d x %d)\n")
		call pargstr(image)
		call pargi(ddatin)
		call pargi(npixd)
		call pargi(nlined)

	call sprintf(command,SZ_LINE,"display %s[%d] 1")
		call pargstr(image)
		call pargi(ddatin)
	call clcmdw(command)

# Getting the  coordinates of the object in the direct image
	call select_obj(in,cx,cy)
	call printf("Object selected at %d %d\n")
		call pargi(cx)
		call pargi(cy)
#correcting for offsets	
	xscale = ((1.0*npixg) / (1.0*npixd))
	yscale = ((1.0*nlineg) / (1.0*nlined))
	cxg = nint(cx*xscale + ccx)
	cyg = nint(cy*yscale + ccy)

	call printf("Object at (%d,%d) in the GRISM image\n")
		call pargi(cxg)
		call pargi(cyg)

# Make an array that is as wide as the grism frame, with an x and a y entries too.
# Adjusting the size of the spectrum if it is larger than the image
	if ((cxg-maxx1)<1) maxx1 = cxg - 1
	if ((cxg+maxx2)>npixg) maxx2 = npixg - cxg

	call calloc(spectrum, 2*(maxx1+maxx2), TY_REAL)

	call printf("Displaying the grism image %s[%d] (%d x %d)\n")
		call pargstr(grism)
		call pargi(gdatin)
		call pargi(npixg)
		call pargi(nlineg)
	call sprintf(command,SZ_LINE,"display %s[%d] 1")
		call pargstr(grism)
		call pargi(gdatin)
	call clcmdw(command)

# Default/automatic values of extraction width.
	if ( (width/2 - int(width/2)) == 0) width = width +1
	halfwidth = (width - 1)/2
	bs = cyg - halfwidth
	be = cyg + halfwidth
	if (bs < 1) bs = 1
	if (be < 1) be = 1
	if (bs > nlineg) bs = nlineg
	if (be > nlineg) be = nlineg
	call draw_spectral_box(cxg,cyg,be,bs,angle,maxx1,maxx2,npixg,nlineg)



	if (!auto) {
		call select_spectral_box(Memr[imgs2r(in2,1,npixg,1,nlineg)],npixg,nlineg,cx,cy,bs,be,width)
	}



	call ip_extract_spectrum(Memr[imgs2r(in2,1,npixg,1,nlineg)],npixg,nlineg,cxg,cyg,maxx1,maxx2,bs,be,width,angle,Memr[spectrum],mingood,maxgood,Memr[imgs2r(gdtq,1,npixg,1,nlineg)],usegdtq,dtqstr)


	call calloc(sky, 2*(maxx1+maxx2), TY_REAL)
	call calloc(tmpsky, 2*(maxx1+maxx2), TY_REAL)

	if (skysub) {
		if (!auto) {
			call extract_sky(in2, Memr[sky], Memr[tmpsky],npixg,nlineg,cx,cy,angle,maxx1,maxx2,mingood,maxgood,Memr[imgs2r(gdtq,1,npixg,1,nlineg)],usegdtq,dtqstr)
		}
		if (auto) {
			sbs = cyg + swidth1
			sbe = cyg + swidth2
			if (sbs < 1) sbs = 1
			if (sbe < 1) sbe = 1
			if (sbs > nlineg) sbs = nlineg
			if (sbe > nlineg) sbe = nlineg
			if (sbs > sbe) {
				tmp = sbs
				sbs = sbe
				sbe = tmp
			}
			call draw_spectral_box(cxg,cyg,sbs,sbe,angle,maxx1,maxx2,npixg,nlineg)
		call ip_extract_sky(Memr[imgs2r(in2,1,npixg,1,nlineg)],npixg,nlineg,cxg,cyg,maxx1,maxx2,sbs,sbe,angle,Memr[sky],mingood,maxgood,Memr[imgs2r(gdtq,1,npixg,1,nlineg)],usegdtq,dtqstr)
		}
	}


	call ip_scalespec(maxx1,cyg,xscale,lambda_zero,lambda_step, Memr[spectrum],(maxx1+maxx2))

	if (skysub) {
		call sub_sky(Memr[spectrum],Memr[sky],be-bs, (maxx1+maxx2))
	}

# Generating a title plot
	if (skysub) {
		call sprintf(title,SZ_LINE,"%s%s X=%d Y=%d Sky subtracted")
			call pargstr(rastr)
			call pargstr(decstr)
			call pargi(cx)
			call pargi(cy)
	}
	if (!skysub) {
		call sprintf(title,SZ_LINE,"%s%s X=%d Y=%d Sky NOT subtracted")
			call pargstr(rastr)
			call pargstr(decstr)
			call pargi(cx)
			call pargi(cy)

	}
	call display_spec(Memr[spectrum],1.0,(maxx1+maxx2),title,0)
	call save_to_disk(output,title,Memr[spectrum],Memr[sky],width,(maxx1+maxx2),outfits)

	call mfree(sky, TY_REAL)
	call mfree(tmpsky, TY_REAL)
	call mfree(spectrum, TY_REAL)

	call imunmap (in)
	call imunmap (in2)
	call imunmap (gdtq)
end

include	<imhdr.h>
define	SZ_KWNAME	8
define	SZ_KEYWORD	40

#  psf_header - Update PSF file header keywords
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  20-Jan-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure psf_header (ipin, ipout, x0, y0, xcorner, ycorner, 
			xcenter, ycenter, origin, minval, maxval)

pointer	ipin, ipout 		# input: file pointers
int	x0, y0			# input: lower left corner of the subsection
int	xcorner, ycorner	# input: lower left corner of the subsection
real	xcenter, ycenter	# input: center of the subsection
char	origin[SZ_LINE]		# input: origin of the input file
real	minval, maxval
 
real	dumreal
double	dumdouble
int	dumint
char	keys[SZ_KWNAME, 20]
char	froot[SZ_FNAME], fextn[SZ_FNAME]
char	dumchar[SZ_KEYWORD], dumchar1[SZ_KEYWORD], dumchar2[SZ_KEYWORD]
char	camera[SZ_KEYWORD], filt1[SZ_KEYWORD], filt2[SZ_KEYWORD]
char	mode[SZ_KEYWORD], det[SZ_KEYWORD]
bool	calib
int	i, k
	
int	imgeti(), nowhite(), itoc()
real	imgetr()
double	imgetd()
int	fnroot()
int	fnextn()
bool	streq()
#==============================================================================
begin

	# update data min and max
	call imputr (ipout, "i_minpixval", minval)
	call imputr (ipout, "i_maxpixval", maxval)
	IM_LIMTIME(ipout) = IM_MTIME(ipout) + 1

	# pass parameters to update the output image's header keyword values 
	call imputi (ipout, "XCORNER", xcorner)
	call imputi (ipout, "YCORNER", ycorner)
	call imputr (ipout, "XCENTER", xcenter)
	call imputr (ipout, "YCENTER", ycenter)
	call impstr (ipout, "ORIGIN", origin)

	# pass header keyword values from the input image to the output image
	# INTEGER keywords
	call strcpy ("DETECTOR", 	keys[1, 1], SZ_KWNAME)
	call strcpy ("ACTUAT25", 	keys[1, 2], SZ_KWNAME)
	call strcpy ("ACTUAT26", 	keys[1, 3], SZ_KWNAME)
	call strcpy ("ACTUAT27", 	keys[1, 4], SZ_KWNAME)
	call strcpy ("ACTUAT28", 	keys[1, 5], SZ_KWNAME)
	call strcpy ("ACTUAT29", 	keys[1, 6], SZ_KWNAME)
	call strcpy ("ACTUAT30", 	keys[1, 7], SZ_KWNAME)

	do i = 1, 7 {
	    iferr (dumint = imgeti(ipin, keys[1, i])) {
	        call printf (
		    "Keyword %s does not exist in the input image %s\n")
		    call pargstr (keys[1, i])
		    call pargstr (IM_HDRFILE(ipin))
	    } else
	    	call imputi (ipout, keys[1, i], dumint)
	
	if (i == 1) k = itoc (dumint, det, SZ_KEYWORD)
	}

	# REAL keywords
	call strcpy ("CD1_1", 		keys[1, 1], SZ_KWNAME)
	call strcpy ("CD1_2", 		keys[1, 2], SZ_KWNAME)
	call strcpy ("CD2_1", 		keys[1, 3], SZ_KWNAME)
	call strcpy ("CD2_2", 		keys[1, 4], SZ_KWNAME)
	call strcpy ("EXPTIME", 	keys[1, 5], SZ_KWNAME)
	call strcpy ("PSFSCALE", 	keys[1, 6], SZ_KWNAME)

	do i = 1, 6 {
	    iferr (dumreal = imgetr(ipin, keys[1, i])) {
	        call printf (
		    "Keyword %s does not exist in the input image %s\n")
		    call pargstr (keys[1, i])
		    call pargstr (IM_HDRFILE(ipin))
	    } else
	    	call imputr (ipout, keys[1, i], dumreal)
	}

	iferr (dumreal = imgetr(ipin, "CRPIX1")) {
	    call printf (
		"Keyword CRPIX1 does not exist in the input image %s\n")
		call pargstr (IM_HDRFILE(ipin))
	} else
	    	call imputr (ipout, "CRPIX1", dumreal-x0+1)

	iferr (dumreal = imgetr(ipin, "CRPIX2")) {
	    call printf (
		"Keyword CRPIX2 does not exist in the input image %s\n")
		call pargstr (IM_HDRFILE(ipin))
	} else
	    	call imputr (ipout, "CRPIX2", dumreal-y0+1)

	# DOUBLE PRECISION keywords
	call strcpy ("CRVAL1", 		keys[1, 1], SZ_KWNAME)
	call strcpy ("CRVAL2", 		keys[1, 2], SZ_KWNAME)

	do i = 1, 2 {
	    iferr (dumdouble = imgetd(ipin, keys[1, i])) {
	        call printf (
		    "Keyword %s does not exist in the input image %s\n")
		    call pargstr (keys[1, i])
		    call pargstr (IM_HDRFILE(ipin))
	    } else
	    	call imputd (ipout, keys[1, i], dumdouble)
	}

	iferr (dumdouble = imgetd(ipin, "EXPSTART")) {
	    call printf (
		"Keyword EXPSTART does not exist in the input image %s\n")
		call pargstr (IM_HDRFILE(ipin))
	} else
	    call imputd (ipout, "MJD", int(dumdouble))

	# STRING keywords
	call strcpy ("CTYPE1", 		keys[1, 1], SZ_KWNAME)
	call strcpy ("CTYPE2", 		keys[1, 2], SZ_KWNAME)
	call strcpy ("FILTNAM1", 	keys[1, 3], SZ_KWNAME)
	call strcpy ("FILTNAM2", 	keys[1, 4], SZ_KWNAME)
	call strcpy ("CAMERA", 		keys[1, 5], SZ_KWNAME)
	call strcpy ("MODE", 		keys[1, 6], SZ_KWNAME)
	call strcpy ("ROOTNAME",	keys[1, 7], SZ_KWNAME)
	call strcpy ("SPECTRAL", 	keys[1, 8], SZ_KWNAME)
	call strcpy ("REFSPEC", 	keys[1, 9], SZ_KWNAME)
	call strcpy ("TARGNAME", 	keys[1, 10], SZ_KWNAME)

	do i = 1, 10 {
	    iferr (call imgstr (ipin, keys[1, i], dumchar, SZ_KEYWORD)) {
	        call printf (
		    "Keyword %s does not exist in the input image %s\n")
		    call pargstr (keys[1, i])
		    call pargstr (IM_HDRFILE(ipin))
	    } else
	    	call impstr (ipout, keys[1, i], dumchar)

	    if (i == 3) call strcpy (dumchar, filt1, SZ_KEYWORD)
	    if (i == 4) call strcpy (dumchar, filt2, SZ_KEYWORD)
	    if (i == 5) call strcpy (dumchar, camera, SZ_KEYWORD)
	    if (i == 6) call strcpy (dumchar, mode, 1)
	}
	iferr (call imgstr (ipin, "DATE-OBS", dumchar, SZ_KEYWORD)) {
	    call printf ( 
		    "Keyword DATE-OBS does not exist in the input image %s\n")
		call pargstr (IM_HDRFILE(ipin))
	} else
	    call impstr (ipout, "DATE_OBS", dumchar)
	iferr (call imgstr (ipin, "OBSMODE", dumchar, SZ_KEYWORD)) 
	    dumchar[1] = EOS
	if (dumchar[1] == EOS) {
	
	    # construct the OBSMODE string
	    call strcat (camera, dumchar, SZ_KEYWORD)
	    call strcat (",", dumchar, SZ_KEYWORD)
	    call strcat (det, dumchar, SZ_KEYWORD)
	    call strcat (",", dumchar, SZ_KEYWORD)
	    call strcat (mode, dumchar, SZ_KEYWORD)
	    call strcat (",DN,", dumchar, SZ_KEYWORD)
	    call strcat (filt1, dumchar, SZ_KEYWORD)
	    if (filt2[1] != EOS) {
	    	call strcat (",", dumchar, SZ_KEYWORD)
	    	call strcat (filt2, dumchar, SZ_KEYWORD)
	    }
	}
		
	call impstr (ipout, "OBSMODE", dumchar)
	
	# write the name of the output file to its header
	i = fnroot (IM_HDRFILE(ipout), froot, SZ_FNAME)
	i = fnextn (IM_HDRFILE(ipout), fextn, SZ_FNAME)
	call strcat (".", froot, SZ_FNAME)
	call strcat (fextn, froot, SZ_FNAME)
	call impstr (ipout, "DATA_FIL", froot)

	# set output header keywords CALIBRAT and FLATFILE if bias and flatfield
	# corrections are performed
	iferr {
	    call imgstr (ipin, "BIASCORR", dumchar1, SZ_KEYWORD)
	    call imgstr (ipin, "FLATCORR", dumchar2, SZ_KEYWORD)
	} then {
	    call printf ("Keyword BIASCORR or/and FLATCORR do not exist in the input image %s\n")
		call pargstr (IM_HDRFILE(ipin))
	} else {   
	    k = nowhite (dumchar1, dumchar1, SZ_KEYWORD)
	    k = nowhite (dumchar2, dumchar2, SZ_KEYWORD)
	    calib = (streq(dumchar1, "DONE") && streq(dumchar2, "DONE"))
	    if (calib) {
	    	call impstr (ipout, "CALIBRAT", "T")
	        iferr (call imgstr (ipin, "FLATFILE", dumchar, SZ_KEYWORD)) {
	            call printf (
		      "Keyword FLATFILE does not exist in the input image %s\n")
		        call pargstr (IM_HDRFILE(ipin))
	        } else
	    	    call impstr (ipout, "FLATFILE", dumchar)
	    }
	    else
	    	call impstr (ipout, "CALIBRAT", "F")
	}
end

include	<imhdr.h>
include	<imset.h>
include	<mach.h>
include	<error.h>
include	"gcombine.h"

# G_SCALE -- Get the scales, zeros, and uniform weights.
#  Possible "pixelwise weighting" is left for another routine.
#  The effects of scales on the weights using noise model and on the resultant
#  exposure time are taken into account.
#  The weights are not going to be normalized here since it will be broken
#  up by the rejection of bad pixels later on. I.e., the weighted average
#  will be performed in a straightforward way as
#
#      average = sum (data * wgt) / sum (wgt)
#	
# CYZhang 17 Mar 94 Based on images.imcombine

procedure g_scale (in, msk, err, out, nimages, szuw, nm)

pointer	in[nimages]		# Input images
pointer	msk[nimages]		# Input DQF images
pointer	err[nimages]		# Input ERROR images
pointer	out[3]			# Output images
int	nimages			# Number of images
pointer	szuw			# Pointer to scaling structure
pointer	nm			# Pointer to noise model structure

int	stype, ztype, uwtype
int	i, j, nout
real	mode, median, mean, exposure, darksecs, zmean, val
pointer	sp, exptime, modes, medians, means, expname
pointer	darkname, darktime
pointer	section, str, sname, zname, uwname
#char	text[SZ_LINE]

int	imgeti(), g_getszw()
real	imgetr(), asumr(), asumi()
bool	isblank()

include	"gcombine.com"

begin
	call smark (sp)
	call salloc (exptime, nimages, TY_REAL)
	call salloc (darktime, nimages, TY_REAL)
	call salloc (modes, nimages, TY_REAL)
	call salloc (medians, nimages, TY_REAL)
	call salloc (means, nimages, TY_REAL)
	call salloc (expname, SZ_FNAME, TY_CHAR)
	call salloc (darkname, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (sname, SZ_FNAME, TY_CHAR)
	call salloc (zname, SZ_FNAME, TY_CHAR)
	call salloc (uwname, SZ_FNAME, TY_CHAR)

	# Set the defaults.
	call amovki (1, Memi[NCOMB(szuw)], nimages)
	call amovkr (0., Memr[exptime], nimages)
	call amovkr (0., Memr[darktime], nimages)
	call amovkr (INDEFR, Memr[modes], nimages)
	call amovkr (INDEFR, Memr[medians], nimages)
	call amovkr (INDEFR, Memr[means], nimages)
	call amovkr (1., Memr[SCALES(szuw)], nimages)
	call amovkr (0., Memr[ZEROS(szuw)], nimages)
	call amovkr (1., Memr[UWTS(szuw)], nimages)

	# Get the number of images previously combined and the exposure times.
	# The default combine number is 1 and the default exposure is 0.

	call clgstr ("expname", Memc[expname], SZ_FNAME)
	call clgstr ("darkname", Memc[darkname], SZ_FNAME)
	do i = 1, nimages {
	    iferr (Memi[NCOMB(szuw)+i-1] = imgeti (in[i], "ncombine"))
		Memi[NCOMB(szuw)+i-1] = 1
	    if (!isblank(Memc[expname])) {
		iferr (Memr[exptime+i-1] = imgetr (in[i], Memc[expname]))
		    Memr[exptime+i-1] = 0.
	    } 
	    if (!isblank(Memc[darkname])) {
	        iferr (Memr[darktime+i-1] = imgetr (in[i], Memc[darkname]))
		    Memr[darktime+i-1] = 0.
	    }
	}

	# Set scales, zeros, and uweights

	stype = g_getszw ("scale", Memc[sname], STYPES, in, Memr[exptime],
			  Memr[SCALES(szuw)], nimages)
	ztype = g_getszw ("zero", Memc[zname], ZTYPES, in, Memr[exptime],
			  Memr[ZEROS(szuw)], nimages)

	if ( G_COMBINE == C_AVERAGE && G_WEIGHT == W_UNIFORM ) {
	    uwtype = g_getszw ("weight", Memc[uwname], UWTYPES,
			       in, Memr[exptime], Memr[UWTS(szuw)], nimages)
	} else {
	    uwtype = S_NONE
	}

	# Get image statistics only if needed.
	DOMODE = ((stype==S_MODE)||(ztype==S_MODE)||(uwtype==S_MODE))
	DOMEDIAN = ((stype==S_MEDIAN)||(ztype==S_MEDIAN)||(uwtype==S_MEDIAN))
	DOMEAN = ((stype==S_MEAN)||(ztype==S_MEAN)||(uwtype==S_MEAN))
	if (DOMODE || DOMEDIAN || DOMEAN) {
	    Memc[section] = EOS
	    call clgstr ("statsec", Memc[section], SZ_FNAME)

	    do i = 1, nimages {
		switch (IM_PIXTYPE(in[i])) {
		case TY_SHORT:
		    call g_secstats (in[i], msk[i], Memc[section],
				     mode, median, mean)
		default:
		    call g_secstatr (in[i], msk[i], Memc[section],
				     mode, median, mean)
		}
		if (DOMODE) {
		    Memr[modes+i-1] = mode
		    if (stype == S_MODE) {
			if (mode <= 0.0)
			    Memr[SCALES(szuw)+i-1] = -MAX_REAL
			else
			    Memr[SCALES(szuw)+i-1] = mode
		    }
		    if (ztype == S_MODE)
			Memr[ZEROS(szuw)+i-1] = mode
		    if (uwtype == S_MODE) {
			if (mode <= 0.0)
			    Memr[UWTS(szuw)+i-1] = -MAX_REAL		    
			else {
			    if ( NSMOD_W ) {
				call g_nmvar (mode, val,
					  Memr[RDNOISE(nm)+i-1],
					  Memr[GAIN(nm)+i-1],
					  Memr[SNOISE(nm)+i-1])
				# Weights prop to the reciprocal of variance
				Memr[UWTS(szuw)+i-1] = 1. / val 
			    } else
				Memr[UWTS(szuw)+i-1] = mode
			}
		    }
		}
		if (DOMEDIAN) {
		    Memr[medians+i-1] = median
		    if (stype == S_MEDIAN) {
			if (median <= 0.0) 
			    Memr[SCALES(szuw)+i-1] = -MAX_REAL		    
			else
			    Memr[SCALES(szuw)+i-1] = median
		    }
		    if (ztype == S_MEDIAN)
			Memr[ZEROS(szuw)+i-1] = median
		    if (uwtype == S_MEDIAN) {
			if (median <= 0.0)
			    Memr[UWTS(szuw)+i-1] = -MAX_REAL		    
			else {
			    if ( NSMOD_W ) {
				call g_nmvar (median, val,
					  Memr[RDNOISE(nm)+i-1],
					  Memr[GAIN(nm)+i-1],
					  Memr[SNOISE(nm)+i-1])
				# Weights prop to the reciprocal of variance
				Memr[UWTS(szuw)+i-1] = 1. / val 
			    } else 
				Memr[UWTS(szuw)+i-1] = median
			}
		    }
		}
		if (DOMEAN) {
		    Memr[means+i-1] = mean
		    if (stype == S_MEAN) {
			if (mean <= 0.0)
			    Memr[SCALES(szuw)+i-1] = -MAX_REAL
			else 
			    Memr[SCALES(szuw)+i-1] = mean
		    }
		    if (ztype == S_MEAN)
			Memr[ZEROS(szuw)+i-1] = mean
		    if (uwtype == S_MEAN) {
			if (mean <= 0.0)
			    Memr[UWTS(szuw)+i-1] = -MAX_REAL
			else {
			    if ( NSMOD_W ) {
				call g_nmvar (mean, val,
					  Memr[RDNOISE(nm)+i-1],
					  Memr[GAIN(nm)+i-1],
					  Memr[SNOISE(nm)+i-1])
				# Weights prop to the reciprocal of variance
				Memr[UWTS(szuw)+i-1] = 1. / val 
			    } else
				Memr[UWTS(szuw)+i-1] = mean
			}
		    }
		}
	    }
	}

	do i = 1, nimages {
	    if (Memr[SCALES(szuw)+i-1] <= 0.) {
		call eprintf ("WARNING: Negative scale factors")
		call eprintf (" -- ignoring scaling\n")
		call amovkr (1., Memr[SCALES(szuw)], nimages)
		break
	    }
	}
	# Convert to relative factors if needed.
	# Mean of the scales

	mean = asumr (Memr[SCALES(szuw)], nimages) / nimages

	# Scales normalized to their mean so that the sum of the
	# normalized scales equals to the total number of images.

	call adivkr (Memr[SCALES(szuw)], mean, Memr[SCALES(szuw)], nimages)

	# Divide zeros by the scales since what should be subtrated from
	# the scaled image is the scaled zero level.

	call adivr (Memr[ZEROS(szuw)], Memr[SCALES(szuw)],
		    Memr[ZEROS(szuw)], nimages)

	# Mean of the zero levels

	zmean = asumr (Memr[ZEROS(szuw)], nimages) / nimages
	# Save the mean sky ZERO level for log info
	BSKY = max (zmean, 0.0)

	# Zeros are taken as the difference between individual zero
	# level and the mean zero level. So the sum of the final zeros
	# becomes zero and images are brought to a common, mean zero level.

	call asubkr (Memr[ZEROS(szuw)], zmean, Memr[ZEROS(szuw)], nimages)
	
	# Because of finite arithmetic it is possible for the zero offsets
	# to be nonzero even when they are all equal.  Just for the sake of
	# a nice log set the zero offsets in this case.

	for (i=2; (i <= nimages) && (Memr[ZEROS(szuw)+i-1] ==
				     Memr[ZEROS(szuw)]); i=i+1)
	    ;
	if (i > nimages)
	    call aclrr (Memr[ZEROS(szuw)], nimages)

	if (uwtype != S_NONE) {
	    do i = 1, nimages {
		if (Memr[UWTS(szuw)+i-1] <= 0.) {
		    call eprintf ("WARNING: Negative weights")
		    call eprintf (" -- using only NCOMBINE weights\n")
		    do j = 1, nimages
			Memr[UWTS(szuw)+j-1] = Memi[NCOMB(szuw)+j-1]
		    break
		}
		# Since variance is computed from original, unscaled images,
		# effects of scaling on UWTS must be considered 
		# for UWTS calculated from noise model
		# 
		# NB: Normalized SCALES is used here.
		if ( NSMOD_W ) {
		    Memr[UWTS(szuw)+i-1] = Memi[NCOMB(szuw)+i-1] *
					   Memr[UWTS(szuw)+i-1] *
					   Memr[SCALES(szuw)+i-1] ** 2
		} else {
		    # When the weight is exptime, (mean, median, and
		    # mode are prop to exptime somehow, in the case of
		    # Poisson noise), no scaling enters the weight.
		    Memr[UWTS(szuw)+i-1] = Memi[NCOMB(szuw)+i-1] *
					   Memr[UWTS(szuw)+i-1]
		}	
	    }
	}

	# The normalization of weights is not done!
#	mean = asumr (Memr[UWTS(szuw)], nimages)
#	call adivkr (Memr[UWTS(szuw)], mean, Memr[UWTS(szuw)], nimages)

	# Set flags for scaling, zero offsets, sigma scaling, weights.
	# Sigma scaling may be suppressed if the scales or zeros are
	# different by a specified tolerance.
	# DOWTS was initialized in top procedure, it may be overwritten here.

	DOSCALE = false
	if (G_WEIGHT == W_UNIFORM)
	    DOWTS = false
	do i = 2, nimages {
	    if (Memr[SCALES(szuw)+i-1] != Memr[SCALES(szuw)])
		DOSCALE = true
	    if (Memr[ZEROS(szuw)+i-1] != Memr[ZEROS(szuw)])
		DOSCALE = true
	    if (G_WEIGHT == W_UNIFORM && Memr[UWTS(szuw)+i-1] != Memr[UWTS(szuw)])
		DOWTS = true
	}
#	if (doscale && zmean > 0. && sigscale != 0.) {
#	    do i = 1, nimages {
#		if (abs (scales[i] - 1) > sigscale ||
#		    abs (zeros[i] / zmean) > sigscale) {
#		    doscale1 = true
#		    break
#		}
#	    }
#	}
		    
	# Set the output header parameters.
	nout = asumi (Memi[NCOMB(szuw)], nimages)
	
	call imaddi (out[1], "ncombine", nout)

	# To get effective exposure and dark times, weights are not entered
	if (!isblank(Memc[expname])) {
	    exposure = 0.
	    do i = 1, nimages
	        exposure = exposure +
			   Memr[exptime+i-1] / Memr[SCALES(szuw)+i-1]
	    exposure = exposure / nimages
	    call imaddr (out[1], Memc[expname], exposure)
	} else
	    exposure = INDEFR
	if (!isblank(Memc[darkname])) {
	    darksecs = 0.
	    do i = 1, nimages
	        darksecs = darksecs + 
			   Memr[darktime+i-1] / Memr[SCALES(szuw)+i-1]
	    darksecs = darksecs / nimages
	    call imaddr (out[1], Memc[darkname], darksecs)
	} else
	    darksecs = INDEFR
#	if (out[2] != NULL) {
#	    call imstats (out[2], IM_IMAGENAME, Memc[str], SZ_FNAME)
#	    call imgcluster (Memc[str], text, SZ_FNAME)
#	    call imastr (out[1], "BPM", text)
#	}
	ifnoerr (mode = imgetr (out[1], "CCDMEAN"))
	    call imdelf (out[1], "CCDMEAN")
	ifnoerr (mode = imgetr (out[1], "ROOTNAME"))
	    call imdelf (out[1], "ROOTNAME")

	# Start the log here since much of the info is only available here.
	call g_log1 (in, out, Memc[sname], Memc[zname], Memc[uwname],
		     nimages)
	call g_log2 (in, msk, err, out, Memr[exptime], Memr[darktime],
		     Memr[modes], Memr[medians], Memr[means], nimages,
		     nout, Memc[expname], exposure, Memc[darkname],
		     darksecs, szuw, nm)

	call sfree (sp)
end


# G_GETSZW -- Get scale, zero, or uweight values as directed by CL parameter
# The values can be one of those in the dictionary, from a file specified
# with a @ prefix, or from an image header keyword specified by a ! prefix.
#
# CYZhang 17 Mar 94 Taken from images.imcombine

int procedure g_getszw (param, name, dic, in, exptime, values, nimages)

char	param[ARB]		#I CL parameter name
char	name[SZ_FNAME]		#O Parameter value
char	dic[ARB]		#I Dictionary string
pointer	in[nimages]		#I IMIO pointers
real	exptime[nimages]	#I Exposure times
real	values[nimages]		#O Values
int	nimages			#I Number of images

int	type			#O Type of value

int	fd, i, nowhite(), open(), fscan(), nscan(), strdic()
real	rval, imgetr()
pointer	errstr
errchk	open, imgetr()

begin
	call clgstr (param, name, SZ_FNAME)
	if (nowhite (name, name, SZ_FNAME) == 0)
	    type = S_NONE
	else if (name[1] == '@') {
	    type = S_FILE
	    fd = open (name[2], READ_ONLY, TEXT_FILE)
	    i = 0
	    while (fscan (fd) != EOF) {
		call gargr (rval)
		if (nscan() != 1)
		    next
		if (i == nimages) {
		   call eprintf (
		       "Warning: Ignoring additional %s values in %s\n")
		       call pargstr (param)
		       call pargstr (name[2])
		   break
		}
		i = i + 1
		values[i] = rval
	    }
	    call close (fd)
	    if (i < nimages) {
		call salloc (errstr, SZ_LINE, TY_CHAR)
		call sprintf (errstr, SZ_FNAME, "Insufficient %s values in %s")
		    call pargstr (param)
		    call pargstr (name[2])
		call error (1, errstr)
	    }
	} else if (name[1] == '!') {
	    type = S_KEYWORD
	    do i = 1, nimages {
		values[i] = imgetr (in[i], name[2])
	    }
	} else {
	    type = strdic (name, name, SZ_FNAME, dic)
	    if (type==S_EXPOSURE)
		do i = 1, nimages
		    values[i] = max (0.001, exptime[i])
	}

	return (type)
end

# G_NMVAR -- evaluate variance from noise model 
#
#
# CYZhang 21 Mar 94 

procedure g_nmvar (intensity, variance, g_rdn2, g_gain, g_snoise)

real	intensity		#I "True" signal level in DN
real	variance		#O Variance in DN**2 from noise model
real	g_rdn2			#I (readnoise / gain)**2 in DN**2
real	g_gain			#I Gain in e-/DN
real	g_snoise			#I Sensitivity noise in fraction

begin
	if (intensity >= 0.0) {
	    variance = g_rdn2 + intensity / g_gain +
		       (g_snoise * intensity) ** 2
	} else {
	    variance = g_rdn2
	}
end

# G_MASK -- Get a mask line
#
#
# CYZhang 21 Apr 94 

procedure g_mask (msk, mskdata, v)

pointer	msk, mskdata		# Pointers
long    v[IM_MAXDIM]           # Index of previous line

pointer	imgnli()
int 	stat

begin
	stat = imgnli (msk, mskdata, v)
end

# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 

include	<error.h>
include	<imhdr.h>
include	<math/iminterp.h>
include <math.h>
include <mach.h>

# Rebinning defs
define	INTERP_MODE	"|linear|spline3|poly3|poly5|sums|sinc|"
define	RB_LINEAR	1
define	RB_SPLINE3	2
define	RB_POLY3	3
define	RB_POLY5	4
define	RB_SUMS		5
define  RB_SINC		6
define	RB_MINDIST	0.001

procedure t_resample ()

# T_RESAMPLE -- resample HRS calibrated flux images on a linear wavelength
# scale.
#
# Version 1.1	S. Hulbert	May 1991	Allow for flipping of FOS 
#						spectra and wavelength file
#						with different name
# Version 1.2	S. Hulbert	Nov 1991	Fixed divide by zero error in
#						rs_index
#         1.2.1	S. Hulbert	Feb 1992	Bug fix--check for monotonic
#						wavelength image and modify how
#						wavelength image name is 
#						constructed
# 	  1.2.2 I. Busko        9/13/96         Bug fix--wavelength image 
#						wasn't found when wave="" and
#						task was started from another
#						directory.
#         1.2.3 M. De La Pena  02/16/98         Changed IS_INDEFR to IS_INDEFD.
#                                               Under IRAF v2.11, these are
#                                               different for IEEE systems.
#         1.2.4 M. De La Pena  10/20/98         IMPARSE is an interface vio and
#                                               the default values for cl_index/
#                                               cl_size are now -1 (formerly 0).
#         1.2.5 M. De La Pena  09/13/99         Correction 1.2.4 was never in 
#                                               the installed version.  Also,
#                                               flipping of FOS AMBER was fixed.
#                                               Task now does what help file
#                                               claims.
#               

pointer sp
pointer	flx, wav, out 
pointer imflx, imwav, imout
pointer bufflx, bufwav, bufout, bufind
pointer tmp_string
int	inlist, wavlist, outlist, mode, pixout, pixin, group, groups
int	cntflx[IM_MAXDIM], cntwav[IM_MAXDIM], cntout[IM_MAXDIM] 
int	cl_index, cl_size, window
int     bufinsize, bufoutsize
int     i
real	datamax, datamin
double	swave, frstwav, lstwav,	dwave, ewave
bool	log
char	interp_mode[SZ_LINE]
char	cluster[SZ_FNAME], ksection[SZ_FNAME], section[SZ_FNAME]
char	root[SZ_FNAME], path[SZ_FNAME], grp[SZ_FNAME]


int	imtopenp(), clgwrd(), clgeti(), imtgetim(), imgnlr(), impnlr()
int	gf_gstfval(), imgnld()
int	fnroot(), fnldir(), imaccf()
double	clgetd()
bool	clgetb()
pointer	immap()

string	strfmt "[%d]"

begin
        
	call smark (sp)
	call salloc (flx, SZ_FNAME, TY_CHAR)
	call salloc (wav, SZ_FNAME, TY_CHAR)
	call salloc (out, SZ_FNAME, TY_CHAR)
        call salloc (tmp_string, SZ_LINE, TY_CHAR)
        
	# get input from cl
	inlist = imtopenp ("input")
	wavlist = imtopenp ("wave")
	outlist = imtopenp ("output")
        
	log = clgetb ("logarithm")
        
	swave = clgetd ("wave0")
	dwave = clgetd ("delwav")
	pixout = clgeti ("pixout")
        ewave = clgetd ("ewave")
        
	mode = clgwrd ("interp_mode", interp_mode, SZ_LINE, INTERP_MODE)
	if (mode == RB_SINC)
	    window = clgeti ("window")
        
	# loop on input files to be interpolated
        bufinsize = 0
        bufoutsize = 0
	while (imtgetim (inlist, Memc[flx], SZ_FNAME) != EOF && 
               imtgetim (outlist, Memc[out], SZ_FNAME) != EOF) {
            
	    # parse input image name
	    call imparse (Memc[flx], cluster, SZ_FNAME, ksection, 
                          SZ_FNAME, section, SZ_FNAME, cl_index, cl_size) 
       
	    if (fnroot (cluster, root, SZ_FNAME) == 0)
                call error (0, "Bad flux image name.")
	    i = fnldir (cluster, path, SZ_FNAME)

	    # check to see if wavelength image given
	    # if not, then build wave image name
	    if (imtgetim (wavlist, Memc[wav], SZ_FNAME) == EOF ||
                Memc[wav] == EOS) {
	        call amovc (path, Memc[wav], SZ_FNAME)
	        call strcat (root, Memc[wav], SZ_FNAME)
	        call strcat (".c0h", Memc[wav], SZ_FNAME)
		if (cl_index > 0) {
	            call sprintf (grp, SZ_FNAME, strfmt)
                    call pargi (cl_index)
	            call strcat (grp, Memc[wav], SZ_FNAME)
		}
	        call strcat (section, Memc[wav], SZ_FNAME)
	    }
           
	    # Map the images
	    imflx = immap (Memc[flx], READ_ONLY, 0)
	    imwav = immap (Memc[wav], READ_ONLY, 0)
	    imout = immap (Memc[out], NEW_COPY, imflx)
            
	    # get size of input vector 
	    pixin = IM_LEN(imflx,1)
	    if (IM_LEN(imwav,1) != pixin) 
		call error (0, "Flux and wavelength images are not the \
same size")
            
	    # if size of output images not input, default to input size
	    if (pixout == 0)
	        pixout = pixin
            
	    # write group count to output image
	    if (cl_index != -1)
		groups = 1
	    else {
	        groups = gf_gstfval (imflx, "GCOUNT")
	        if (gf_gstfval (imwav, "GCOUNT") != groups) 
		    call error (0, "Flux and wavelength images do not the \
have the same number of groups.")
	    }
            
	    # loop on number of groups
	    do group = 1, groups {

                # Initialize the line counters.
                call amovkl (long(1), cntflx, IM_MAXDIM)
		call amovkl (long(1), cntwav, IM_MAXDIM)
		call amovkl (long(1), cntout, IM_MAXDIM)
                
                # Open the group.  If this is the "first" group, don't open
                # it; its already opened.  Also, if a group was specified on
                # command line, there will only be one group, not necessarily
                # the first.
                if (group > 1) {
                    call gf_opengr (imflx, group, datamin, datamax, 0)
                    call gf_opengr (imwav, group, datamin, datamax, 0)
                    call gf_opengr (imout, group, datamin, datamax, imflx)
                }
                
		while (imgnlr (imflx, bufflx, cntflx) != EOF &&
                       imgnld (imwav, bufwav, cntwav) != EOF ) {
                    
                    # Get the input wavelength range and check for monotomic
                    # increasing wavelength.
                    call alimd (Memd[bufwav], pixin, frstwav, lstwav)
                    if (!((frstwav == Memd[bufwav] &&
                           lstwav  == Memd[bufwav+pixin-1]) ||
                          (frstwav == Memd[bufwav+pixin-1] &&
                           lstwav  == Memd[bufwav])))
                        call error (0, "wavelength image is not monotonic")
                    # Removed the next two lines to allow FOS AMBER (decreasing
                    # wavelength scale) to work properly.
                    #frstwav = Memd[bufwav]
                    #lstwav = Memd[bufwav+pixin-1]

                    # Take care of log scaling
                    if (log) {
                        frstwav = log10 (frstwav)
                        lstwav = log10 (lstwav)
                    }

                    # If output spectrum size is not specified, determine it
                    # based on the other wavelength parameters.
                    if (IS_INDEFI(pixout)) {
                        if (IS_INDEFD(swave))
                            swave = frstwav
                        if (IS_INDEFD(ewave))
                            ewave = lstwav
                        if (IS_INDEFD(dwave)) {
                            pixout = pixin
                            dwave = (ewave - swave) / (pixout - 1.)
                        } else
                            pixout = int (((ewave-swave) / dwave) +.5) + 1
                    }
                
                    # If output spectrum size is specified, best fit the
                    # other wavelength parameters to this size.
                    else {
                        if (IS_INDEFD(dwave)) {
                            if (IS_INDEFD(swave))
                                swave = frstwav
                            if (IS_INDEFD(ewave))
                                ewave = lstwav
                            dwave = (ewave - swave) / (pixout - 1.)
                        } else {
                            if (IS_INDEFD(ewave)) {
                                if (IS_INDEFD(swave))
                                    swave = frstwav
                                ewave = swave + (dwave * (pixout - 1.))
                            } else {
                                swave = ewave - (dwave * (pixout -1.))
                            }
                        }
                    }

                    # If this is the first group, create the output image
                    # with the correct size and number of groups.
                    if (group == 1) {
                        IM_LEN(imout,1) = pixout
                        call gf_pstfval (imout, "GCOUNT", groups)
                    }

                    # Open the output group and get the output row.
                    i = impnlr (imout, bufout, cntout)
                    
                    # Allocate the index buffer.
                    if( bufoutsize != pixout ) {
                        bufoutsize = pixout
                        call realloc (bufind, pixout, TY_REAL)
                    }
                    
		    # find positions in input image at which to interpolate
		    call rs_index (bufwav, pixin, log, swave, dwave, pixout, 
                                   bufind) 
                    
		    # interpolate based on selected mode
		    switch (mode) {
		    case RB_LINEAR:
		        call reinterp (Memr[bufflx], Memr[bufout], Memr[bufind],
                                       pixout, pixin, II_LINEAR)
		    case RB_SPLINE3:
		        call reinterp (Memr[bufflx], Memr[bufout], Memr[bufind],
                                       pixout, pixin, II_SPLINE3)
		    case RB_POLY3:
		        call reinterp (Memr[bufflx], Memr[bufout], Memr[bufind],
                                       pixout, pixin, II_POLY3)
		    case RB_POLY5:
		        call reinterp (Memr[bufflx], Memr[bufout], Memr[bufind],
                                       pixout, pixin, II_POLY5)
		    case RB_SUMS:
		        call resum (Memr[bufflx], Memr[bufout], Memr[bufind],
                                    pixout, pixin)
		    case RB_SINC:
		        call sinc_interp (Memr[bufflx], Memr[bufout], 
                                          Memr[bufind], pixout, pixin, window)
		    }
		}
                
		# update header keywords
                if (imaccf (imout, "CRVAL1") == NO) {
                    call gf_addpar (imout, "CRVAL1", TY_DOUBLE, 1, "0")
                    call gf_addpar (imout, "CRPIX1", TY_REAL,   1, "0")
                    call gf_addpar (imout, "CD1_1",  TY_REAL,   1, "0")
                    call gf_addpar (imout, "CTYPE1", TY_CHAR,   8, "lambda")
                }
		call imputd (imout, "CRVAL1", swave)
		call imputr (imout, "CRPIX1", 1.0)
		call imputr (imout, "CD1_1", real(dwave))
		call impstr (imout, "CTYPE1", "lambda")
                
	    }
            
	    # Close the images
	    call imunmap (imflx)
	    call imunmap (imwav)
	    call imunmap (imout)
            
	}
        
	call imtclose (inlist)
	call imtclose (outlist)
        
        call mfree( bufind, TY_REAL )
	call sfree (sp)
        
end

# RS_INDEX -- Compute transformation table converting wavelength to
# pixel number for relinearization

procedure rs_index (bufin, pixin, log, w0out, wpcout, pixout, bufout)

pointer	bufin, bufout
int	pixin, pixout
bool	log
double	w0out, wpcout

int	i, j, jlast, jend, deltaj
double	wout, wmax, wmin, whi, wlo, delta, denom



begin
        
	# determine wavelength limits within which interpolatation is valid
	call alimd (Memd[bufin], pixin, wmin, wmax)
        
	if (log) {
	    wmax = log10 (wmax)
	    wmin = log10 (wmin)
	} 
        
	# counter to keep track of position within input vector
	if (((Memd[bufin] > Memd[bufin+pixin-1]) && (wpcout > 0.)) ||
            ((Memd[bufin] < Memd[bufin+pixin-1]) && (wpcout < 0.))) {
	    jlast = pixin-1
	    jend = 1
	    deltaj = -1 
	} else {
	    jlast = 1
	    jend = pixin-1 
	    deltaj = 1 
	}
        
	# loop on output image pixels
	do i = 1, pixout {
            
	    # calculate output wavelength point 
	    wout = w0out + (i - 1) * wpcout
            
	    # check for wavelengths outside of input range and set to 0
	    if (wout < wmin || wout > wmax) 
		Memr[bufout + i - 1] = 0.0
            
	    else {
                
	        # loop on input wavelengths until output wavelength is found
	        do j = jlast, jend, deltaj {
                    
		    wlo = Memd[bufin + j - 1]
		    whi = Memd[bufin + (j+1) - 1]
                    
		    if (log) {
		        wlo = log10 (wlo)
		        whi = log10 (whi)
		    } 
                    
		    if ((wout >= wlo && wout <= whi) || 
			(wout <= wlo && wout >= whi)) {
			denom = whi - wlo
			if (abs(denom) < EPSILOND) 
			    call error (0, "Duplicate wavelength values found in input wavelength image")
			delta = (wout - wlo) / denom
		        Memr[bufout + i - 1] = j  + delta
                        
			# keep track of position in input wavelength array
			jlast = j
                        
			break
		    } 
	        }
	    }
            
	}
        
end


# SINC_INTERP -- Sinc function interpolation. Uses a sinc function 
# weighted by a Hanning window.

procedure sinc_interp (pixin, pixout, invert, nout, nin, window)

real	pixin[ARB], pixout[ARB], invert[ARB]
int	nin, nout, window

int	ist, iend, i, ipos, j, ix, halfwin
real  	intrp, norm, xpos, x, factor
real  	sincx, sincc[100]

begin
	halfwin = (window - 1) / 2

	# set up Hanning window
	factor = 2.0 * PI / real(window - 1)
	do i = 1, window
	    sincc[i] = 0.5 * (1.0 + cos(factor * real(i - (halfwin + 1))))

	# loop on output pixels
	do j = 1, nout {
	    xpos = invert[j]
	    ipos = xpos
	    # if outside range set to zero
	    if (ipos < 1 || ipos > nin)
		pixout[j] = 0.0
	    # if close to input pixel take input value
	    else if (abs (xpos - ipos) < RB_MINDIST)
		pixout[j] = pixin[ipos]
	    # don't interpolate with a zero value in the input image
	    else if (pixin[ipos] == 0.0)
		pixout[j] = 0.0
	    # don't interpolate with a zero value in the input image
	    else if (ipos < nin && pixin[ipos+1] == 0.0)
		pixout[j] = 0.0
	    # if you make it here do the sinc function interpolation
	    else {

		# get limits for sinc window
		ist = ipos - halfwin
		iend = ipos + halfwin

		# check we aren't going over the edge
		if (ist < 1)
		    ist = 1
		if (iend > nin) 
		    iend = nin

		# zero counters
		intrp = 0.0
		norm = 0.0

		#loop through window summing up the contributions
		do i = ist, iend {
		    # set up the index into the window   
		    ix = i - ipos + (halfwin + 1)  
		    x = real(i) - xpos
		    sincx = sincc[ix] * sin(PI * x) / (PI * x)
		    norm = norm + sincx
		    intrp = intrp + pixin[i] * sincx 
		}

		#and normalize
		if (norm != 0.0) 
		    pixout[j] = intrp / norm
		else
	  	    call error (0, "error in sinc_interp, zero normalization.")

	    }

	}

	end


# RESUM -- Rebinning using a partial pixel summation technique to
#  preserve the total flux.

procedure resum (pixin, pixout, invert, ncols, nlen)

real	pixin[ARB], pixout[ARB], invert[ARB]
int	ncols, nlen

int	i
real	x1, x2, xa, xb, dx

real	pixel_parts()

begin
	# Initialize
	x1 = invert [1]
	x2 = invert [2]
	dx = x2 - x1
	xa = x1 - dx/2
	xb = x1 + dx/2
	pixout[1] = pixel_parts (pixin, nlen, xa, xb)

	do i = 2, ncols {
	    x2 = invert [i]
	    dx = x2 - x1
	    x1 = x2
	    xa = xb
	    xb = x1 + dx/2

	    pixout[i] = pixel_parts (pixin, nlen, xa, xb)
	}
end



# REINTERP -- Rebin the vector by interpolation
#
# This requires a little care to propagate bad pixels and to avoid
# interpolations in which the inversion point is essentially a pixel
# position except for very small errors.  A zero input value is assumed
# to be a bad point.  Any interpolation using a bad point is set to be
# a bad point.  The use of the image interpolator may be questionable
# in the case of bad points since there may be ringing even away from
# the zero value point.

procedure reinterp (pixin, pixout, invert, ncols, nlen, mode)

real	pixin[ARB], pixout[ARB], invert[ARB]
int	ncols, nlen, mode

int	j, ipos
real	xpos

real	arieval()

begin
	do j = 1, ncols {
	    xpos = invert[j]
	    ipos = xpos
	    if (ipos < 1 || ipos > nlen)
		pixout[j] = 0.0
	    else if (abs (xpos - ipos) < RB_MINDIST)
		pixout[j] = pixin[ipos]
	    else if (pixin[ipos] == 0.0)
		pixout[j] = 0.0
	    else if (ipos < nlen && pixin[ipos+1] == 0.0)
		pixout[j] = 0.0
	    else
		pixout[j] = arieval (xpos, pixin, nlen, mode)
	}
end


# PIXEL_PARTS -- Integrate over partial pixels to obtain total flux
# over specified region.

real procedure pixel_parts (y, n, xa, xb)

int	n
real	y[n], xa, xb

int	i, i1, i2
real	x1, x2, cx1, cx2, frac1, frac2, sum

begin
	# Remember that pixel centers occur at integral values
	# so a pixel extends from i-0.5 to i+0.5

	x1 = max (0.5, min (xa, xb))
	x2 = min (n + 0.5, max (xa, xb))
	if (x1 >= x2)
	    return (0.)

	cx1 = x1 - 0.5
	cx2 = x2 - 0.5

	i1 = int (cx1) + 1
	i2 = int (cx2) + 1

	if (i1 == i2) {
	    frac1 = x2 - x1
	    frac2 = 0.0
	} else {
	    frac1 = int (cx1) + 1.0 - cx1
	    frac2 = cx2 - int(cx2)
	}

	sum = frac1 * y[i1]  +  frac2 * y[i2]

	# Include inclusive whole pixels
	do i = i1+1, i2-1
	    sum = sum + y[i]

	return (sum)
end

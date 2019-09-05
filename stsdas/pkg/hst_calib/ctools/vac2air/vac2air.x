include <imhdr.h>

# VAC2AIR -- Convert wavelength scales from vacuum to air above     
# 2000 Angstoms.
#
# Steve Hulbert, Jun90

procedure vac2air ()

char	input[SZ_FNAME]			# uncorrected image
char	output[SZ_FNAME]		# corrected image

pointer	imin, imout, bufin, bufout
int     ngroup, naxis1, group, pix
int	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	immap(), imgnlr(), impnlr()
int	gf_gstfval()
real	datamax, datamin
real	wave, wave2, factor

begin

	# get input from cl
	call clgstr ("input", input, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)

	# map images
	imin  = immap (input, READ_ONLY, 0)
	imout  = immap (output, NEW_COPY, imin)

	# get size of input
	ngroup = gf_gstfval (imin, "GCOUNT") 
	naxis1 = IM_LEN (imin, 1) 

	# set up size of output
	call gf_pstfval (imout, "GCOUNT", ngroup)
	IM_LEN (imout, 1) =  naxis1 

	do group = 1, ngroup {

	   # point to new group in both images
	   call gf_opengr (imin, group, datamin, datamax, 0)
	   call gf_opengr (imout, group, datamin, datamax, imin)

	   call amovkl (long(1), v1, IM_MAXDIM)
	   call amovkl (long(1), v2, IM_MAXDIM)

	   while (imgnlr (imin, bufin, v1) != EOF &&
		  impnlr (imout, bufout, v2) != EOF) {

		# do the correction
		do pix = 1, naxis1 {
		    wave = Memr[bufin + pix - 1]  
		    if (wave > 2000.0) { 
			wave2 = wave * wave
                        factor = 1.0002735182 + 131.4182 / wave2 +
                                2.76249e8 / (wave2 * wave2)
                        Memr[bufout + pix - 1] = wave / factor
		    } else {
                        Memr[bufout + pix - 1] = wave
		    }
		}

	    }

	}

	call imunmap (imin)
	call imunmap (imout)
	
end

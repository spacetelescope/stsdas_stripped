include <imhdr.h>
include <tbset.h>

define	SZ_PARAM	18

procedure waveoffset ()

# WAVEOFFSET -- Compute the offset of one spectrum from a reference spectrum
#
# Version 1.0	May 91	S. Hulbert	Original
#         1.1   Jul 91  S. Hulbert      Fixed bug in writing table header

char	obs1[SZ_FNAME]	# I: image names for first observation
char	wave1[SZ_FNAME]	# I: image names for wavelength observation
char	obs2[SZ_FNAME]	# I: image names for second observation
char	cross[SZ_FNAME]	# I/O: image name for correlation 
char	table[SZ_FNAME]	# I/O: image name for output table
int	search 		# I: max distance in pixel space to search
int	sections	# I: number of sections 

int	i, pixin1, pixinw, pixin2, ioff, n, ic, width
int	fchnl1, nchnl1, xstep1, over1, pass1
int	fchnl2, nchnl2, xstep2, over2, pass2
int	ndelw, ndeld
char	det1[SZ_PARAM], det2[SZ_PARAM] 
char	fgwa1[SZ_PARAM], aper1[SZ_PARAM], aperpos1[SZ_PARAM], polar1[SZ_PARAM]
char	fgwa2[SZ_PARAM], aper2[SZ_PARAM], aperpos2[SZ_PARAM], polar2[SZ_PARAM]
pointer	imobs1, imwave1, imobs2, bufobs1, bufobs2, bufwave1, imout, bufcc
pointer	diode, tp, cp_wave, cp_diod, cp_delw, cp_deld, cp_cnts
pointer	deltad, counts, wave, deltaw
real	mean_delw, mean_deld, sig_delw, sig_deld

bool	strne()
int	clgeti(), imgl1r(), aravr()
pointer	immap(), imps2r(), tbtopn()
pointer	sp

begin

	call smark (sp)

	# get input from cl
	call clgstr ("input1", obs1, SZ_FNAME)
	call clgstr ("wave1", wave1, SZ_FNAME)
	call clgstr ("input2", obs2, SZ_FNAME)
	call clgstr ("table", table, SZ_FNAME)
	call clgstr ("cross", cross, SZ_FNAME)
	search = clgeti ("maxdist")
	sections = clgeti ("nbins")

	# map the images
	imobs1 = immap (obs1, READ_ONLY, 0)
	imwave1 = immap (wave1, READ_ONLY, 0)
	imobs2 = immap (obs2, READ_ONLY, 0)
	imout = immap (cross, NEW_COPY, imobs1)

	# set up output dimensions
	IM_NDIM(imout) = 2
	width = search * 2 + 1
	IM_LEN(imout, 1) = width
	IM_LEN(imout, 2) = sections

	# get size input 
	pixin1 = IM_LEN (imobs1, 1)
	pixinw = IM_LEN (imwave1, 1)
	if (pixin1 != pixinw)
	    call error (1, "Flux and wavelength images are not the same size")

	pixin2 = IM_LEN (imobs2, 1)
	if (pixin1 != pixinw)
	    call error (1, "Flux images are not the same size")

	# get pattern and mode info and check for agreement
	call xpattern (imobs1, fchnl1, nchnl1, xstep1, over1)
	call obsmode (imobs1, det1, fgwa1, aper1, aperpos1, polar1, pass1)
	call xpattern (imobs2, fchnl2, nchnl2, xstep2, over2)
	call obsmode (imobs2, det2, fgwa2, aper2, aperpos2, polar2, pass2)

	# check to see the images are the same kind
	if (strne (det1, det2)) 
	    call error (1, "Detectors do not match")

	if (strne (fgwa1, fgwa2))
	    call error (1, "Gratings do not match")

	if (fchnl1 != fchnl2) 
	    call error (1, "First channels do not match")

	if (nchnl1 != nchnl2)
	    call error (1, "Number of channels do not match")

	if (xstep1 != xstep2)
	    call error (1, "X-steps do not match")

	# set up and fill the buffers 
	call salloc (bufobs1, pixin1, TY_REAL)
	call salloc (bufobs2, pixin2, TY_REAL)
	call salloc (bufwave1, pixinw, TY_REAL)

	call amovr (Memr[imgl1r (imobs1)], Memr[bufobs1], pixin1)
	call amovr (Memr[imgl1r (imobs2)], Memr[bufobs2], pixin2)
	call amovr (Memr[imgl1r (imwave1)], Memr[bufwave1], pixinw)

	# do cross-correlation
	call salloc (bufcc, width * sections, TY_REAL)
	call salloc (deltad, sections, TY_REAL)
	call salloc (counts, sections, TY_REAL)
	call ycrscor (Memr[bufobs1], Memr[bufobs2], pixin1, sections, 
		search, Memr[deltad], Memr[counts], Memr[bufcc]) 

	# write correlation image
	call amovr (Memr[bufcc], Memr[imps2r (imout, 1, width, 1, sections)],
		width * sections)

	# process to put in table
	call salloc (wave, sections, TY_REAL)
	call salloc (diode, sections, TY_REAL)
	call salloc (deltaw, sections, TY_REAL)

	n = pixin1 / sections
	do i = 1, sections {
	    ioff = (i - 1) * n
	    ic = ioff + n / 2 + 1
	    Memr[diode + i - 1] = real (ic) / xstep1 + fchnl1 - 1
	    Memr[wave + i - 1] = Memr[bufwave1 + ic - 1] 
	    if (Memr[deltad + i - 1] != INDEF) {
	        Memr[deltaw + i - 1] = Memr[deltad + i - 1] * 
			(Memr[bufwave1 + (ic + 1) - 1] - 
			Memr[bufwave1 + (ic - 1) - 1)) / 2.0

	        Memr[deltad + i - 1] = Memr[deltad + i - 1] / xstep1
	    } else {
	        Memr[deltaw + i - 1] = INDEF
	    }

	}

	# write table
	tp = tbtopn (table, NEW_FILE, 0)

	# make it a column ordered table
	call tbpset (tp, TBL_WHTYPE, TBL_TYPE_S_COL)

	# set up the columns
	call tbcdef (tp, cp_wave, "WAVELENGTH", "ANGSTROMS", "", TY_REAL, 1, 1)
	call tbcdef (tp, cp_diod, "DIODE", "", "", TY_REAL, 1, 1)
	call tbcdef (tp, cp_delw, "DELTAW", "ANGSTROMS", "", TY_REAL, 1, 1)
	call tbcdef (tp, cp_deld, "DELTAD", "", "", TY_REAL, 1, 1)
	call tbcdef (tp, cp_cnts, "COUNTS", "", "", TY_REAL, 1, 1)

	# create the table
	call tbtcre (tp)
 
	# write the data
	call tbcptr (tp, cp_wave, Memr[wave], 1, sections)
	call tbcptr (tp, cp_diod, Memr[diode], 1, sections)
	call tbcptr (tp, cp_delw, Memr[deltaw], 1, sections)
	call tbcptr (tp, cp_deld, Memr[deltad], 1, sections)
	call tbcptr (tp, cp_cnts, Memr[counts], 1, sections)

	# add the header keywords
	call tbhadt (tp, "DETECTOR", det2)
	call tbhadt (tp, "FGWA_ID", fgwa2)
	call tbhadt (tp, "APER_ID", aper2)
	call tbhadt (tp, "APER_POS", aperpos2)
	call tbhadt (tp, "POLAR_ID", polar2)
	call tbhadi (tp, "PASS_DIR", pass2)
	call tbhadt (tp, "APER_REF", aper1)
	call tbhadt (tp, "APOS_REF", aperpos1)
	call tbhadt (tp, "POLR_REF", polar1)
	call tbhadi (tp, "PASS_REF", pass1)

	call tbtclo (tp)

	# write to STDOUT
	call printf (" Wavelength   Diode   DeltaW    DeltaD   Total counts\n")	
	call printf ("                                           2nd spec.\n")	
	do i = 1, sections {
	    call printf ("  %8.3f    %5.1f  %8.4f  %8.4f   %10.1f\n")
	        call pargr (Memr[wave + i - 1])
	        call pargr (Memr[diode + i - 1])
	        call pargr (Memr[deltaw + i - 1])
	        call pargr (Memr[deltad + i - 1])
	        call pargr (Memr[counts + i - 1])
	}

	# find the mean values for deta wavelength and delta diode
	# reject at 2 sigma level
	ndelw = aravr (Memr[deltaw], sections, mean_delw, sig_delw, 2.0)
	ndeld = aravr (Memr[deltad], sections, mean_deld, sig_deld, 2.0)

	call printf ("\nmean DeltaW= %8.4f   SD= %8.4f   %2d values rejected\n")
	    call pargr (mean_delw)
	    call pargr (sig_delw)
	    call pargi (sections - ndelw)

	call printf ("mean DeltaD= %8.4f   SD= %8.4f   %2d values rejected\n") 
	    call pargr (mean_deld)
	    call pargr (sig_deld)
	    call pargi (sections - ndeld)

	call sfree (sp)

	call imunmap (imobs1)
	call imunmap (imwave1)
	call imunmap (imobs2)
	call imunmap (imout)

end


procedure xpattern (im, fchnl, nchnls, nxsteps, overscan)

# XPATTERN -- get x-pattern values from FOS header

pointer	im	# image descriptor
int	fchnl		# O
int	nchnls		# O
int	nxsteps		# O
int	overscan	# O

int	imgeti()

begin

	fchnl = imgeti (im, "FCHNL")
	nchnls = imgeti (im, "NCHNLS")
	nxsteps = imgeti (im, "NXSTEPS")
	overscan = imgeti (im, "OVERSCAN")
	
end


define	SZ_PARAM	18

procedure obsmode (im, detector, fgwaid, aperid, aperpos, polarid, passdir)

# OBSMODE -- get observing mode values from FOS header

pointer	im			# I: image descriptor
char	detector[SZ_PARAM]	# O
char	fgwaid[SZ_PARAM]	# O
char	aperid[SZ_PARAM]	# O
char	aperpos[SZ_PARAM]	# O
char	polarid[SZ_PARAM]	# O
int	passdir			# O

int	imgeti()

begin

	call imgstr (im, "DETECTOR", detector, SZ_PARAM)
	call imgstr (im, "FGWA_ID", fgwaid, SZ_PARAM)
	call imgstr (im, "APER_ID", aperid, SZ_PARAM)
	call imgstr (im, "APER_POS", aperpos, SZ_PARAM)
	call imgstr (im, "POLAR_ID", polarid, SZ_PARAM)
	passdir = imgeti (im, "PASS_DIR")
	
end

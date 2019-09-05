include <imhdr.h>
include	<tbset.h>

procedure t_polbin ()

# POLBIN -- This routine takes a set of FOS Stokes parameter data (either
# directly from an original .c3h file, or a .c3h file that is combined
# data from the POLAVE task) and rebins the data so that either:
# 1) the linear polarization error per bin is equal - thus resulting in
# output data with bins of different width depending on the character of
# the spectrum and the signal-to-noise; or
# 2) the bins are equally spaced in wavelength (or inverse wavelength).
#
# The input data file MUST be in FOS c3h file format and the Stokes QUV
# data must be UNORMALIZED.  The output file is a copy of the input, with
# the Stokes and polarimetry spectra replaced by the binned spectra.
#
# Written by H.A. Bushouse, STScI, May 1993:  Based in large part on the
# Fortran programs "POLERROR" and "POLARBIN" written by J.R. Walsh, ST-ECF, ESO.
# (First released in STSDAS v1.3, Sep. 1993)
#
# Oct-Nov 93: HAB - Following modifications:
#  1) Include binned Stokes IQUV spectra in output image;
#  2) Shorten output files to include only populated bins (no padded zeros);
#  3) Produce binned wavelength output image;
#  4) Drop use of data quality vector (look for zero-valued sigmas instead);
#  5) Change "cerror" mode so that binning is calculated only once for 
#     selected reference set and then applied to all 4 data sets so that
#     they all have the same number of output bins (with no padding);
#  6) Modularized "binerr" subroutine and also fixed bug that caused incorrect
#     results when "bincen" is greater than 1.
#  7) Moved data I/O to subroutines "rdata" and "wdata".
#  8) Modified algorithms that calculate PL, PC, Theta (and errors) to
#     match those used by CALFOS (as specified in SE-06).
#  9) Added choice of uniform or variance weighting in binning.
#
# May 94: HAB - Added check for AMBER detector and subsequent flipping of
#		wavelength and data vectors. Modified t_polbin, rdata, wdata.
#
# Jun 94: HAB - Fixed bug in binerr and binwav subroutines that caused
#		divide by zero errors when weight=variance and Verr=0.
#		(STSDAS 1.3.2)
#
# Aug 94: HAB - Added option for listing bin locations in an input file;
#		added bins_list routine.
#		(STSDAS 1.3.2 development)
#
#
# Task parameters
char	input[SZ_FNAME]		# Input image name
char	output[SZ_FNAME]	# Output image name
char	mode[SZ_LINE]		# Binning mode
real	paim			# Percent error per bin for mode "cerror"
int	nbin			# Number of bins for mode "cwave"
char	lsfile[SZ_FNAME]	# Name of bin list file for mode "list"
int	refset			# Reference data set number for mode "cerror"
char	weight[SZ_LINE]		# Type of weighting to be used
real	wbin1			# Beginning wavelength for mode "cerror"
bool	lbin			# Inverse wavelength switch
pointer	wname			# Wavelength image name
bool	verbose

# Local variables
pointer	iname, oname, owname, iroot, oroot, iextn, oextn, wroot, det
pointer iptr, wptr, lptr, colptr, nulflg
pointer	I, Q, U, V, Is, Qs, Us, Vs, wave
pointer Ib, Qb, Ub, Vb, Isb, Qsb, Usb, Vsb, wavb
pointer	PL, PC, TH, PLs, PCs, THs
pointer	wav1, wav2, wbins, wbine
pointer	binstrt, binend

int     npix, ngroups, bin1, set

real	datamin, datamax

bool	first, variwgt

# IRAF functions
bool	streq(), clgetb()
int	clgeti(), immap(), imgl1r(), gf_gstfval(), tbpsta()
real	clgetr()
pointer	tbtopn(), tbcnum()
pointer	sp

string	detector "DETECTOR"

begin

	# Allocate memory for local variables
	call smark (sp)
	call salloc (iname, SZ_FNAME, TY_CHAR)
	call salloc (oname, SZ_FNAME, TY_CHAR)
	call salloc (wname, SZ_FNAME, TY_CHAR)
	call salloc (owname, SZ_FNAME, TY_CHAR)
	call salloc (iroot, SZ_FNAME, TY_CHAR)
	call salloc (iextn, SZ_FNAME, TY_CHAR)
	call salloc (oroot, SZ_FNAME, TY_CHAR)
	call salloc (oextn, SZ_FNAME, TY_CHAR)
	call salloc (wroot, SZ_FNAME, TY_CHAR)
	call salloc (det,   SZ_LINE,  TY_CHAR)

	# Get input file name
	call clgstr ("input", input, SZ_FNAME)
        call iki_init ()
	call iki_parse (input, Memc[iroot], Memc[iextn])
	call strcpy (input, Memc[iname], SZ_FNAME)
	# If no file extension was given, assume it is "c3h"
	if (streq (Memc[iextn], "")) {
	    call strcpy ("c3h", Memc[iextn], SZ_FNAME)
	    call strcat (".c3h", Memc[iname], SZ_FNAME)
	}

	# Get the output file name
	call clgstr ("output", output, SZ_FNAME)
	call iki_parse (output, Memc[oroot], Memc[oextn])
 
	# Get the wavelength file name
	call clgstr ("wave", Memc[wname], SZ_FNAME)
	# If no wavelength file name was given, use c3h root name with c0h ext
	if (streq (Memc[wname], ""))
	    call strcpy (Memc[iroot], Memc[wname], SZ_FNAME)
	call iki_parse (Memc[wname], Memc[wroot], Memc[oextn])
	if (streq (Memc[oextn], ""))
	    call strcat (".c0h", Memc[wname], SZ_FNAME)

	# Get the type of weighting desired
	call clgstr ("weight", weight, SZ_LINE)
	if (streq (weight, "variance"))
	    variwgt = true
	else
	    variwgt = false

	verbose = clgetb ("verbose")

	# Open the input data files
	iptr = immap (Memc[iname], READ_ONLY, 0)
	wptr = immap (Memc[wname], READ_ONLY, 0)

	# Get the number of pixels in the input spectra and the
	# number of groups in the input data file
	npix	= IM_LEN (iptr, 1)
	ngroups	= gf_gstfval (iptr, "GCOUNT")

	# If ngroups is not equal to 56, then this is not a true "c3h" file
	if (ngroups != 56) {
	    call imunmap (iptr)
	    call imunmap (wptr)
	    call error (1, "Input data file does not have c3h format")
	}

	# Make sure the wavelength image has same number of pixels as spectra
	if (npix != IM_LEN(wptr,1)) {
	    call imunmap (iptr)
	    call imunmap (wptr)
	    call error (1, "Wavelength image not same size as Stokes image")
	}

	# Find out which binning mode to use
	call clgstr ("bin_mode", mode, SZ_LINE)

	# Constant errors per bin mode:
	if (streq (mode, "cerror")) {

	    # Find out which data set to use as reference for binning
	    refset = clgeti ("refset")
	    if ((refset < 1) || (refset > 4))
		 call error (1, "Reference set must be between 1 and 4")

	    # Get the desired percentage polarization error per bin
	    paim = clgetr ("perror")
	    paim = paim / 100.0
	    if ((paim <= 0.0) || (paim >= 1.0))
		 call error (1, "Polarization error per bin must be > 0 and < 100")

	    # Get the wavelength at which to begin binning
	    wbin1 = clgetr ("wavcen")

	} else if (streq (mode, "cwave")) {

	# Constant wavelength per bin mode:

	    # Get the desired number of bins for the output data
	    nbin = clgeti ("bins")
	    if (nbin > npix)
		call error (1, "Number of bins cannot be > number of pixels")

	    # Find out if binning is to be in lambda or 1/lambda units
	    lbin = clgetb ("invlam")

	} else {

	# Read bin positions from a file:

	    # Get the name of the file that lists the bin positions
	    call clgstr ("listfile", lsfile, SZ_FNAME)

	    # Open the list file
	    lptr = tbtopn (lsfile, READ_ONLY, NULL)
	    nbin = tbpsta (lptr, TBL_NROWS)

	    # Read the list of wavelength pairs
	    call malloc (wbins,  nbin, TY_REAL)
	    call malloc (wbine,  nbin, TY_REAL)
	    call malloc (nulflg, nbin, TY_BOOL)
	    colptr = tbcnum (lptr, 1)
	    call tbcgtr (lptr, colptr, Memr[wbins], Memb[nulflg], 1, nbin)
	    colptr = tbcnum (lptr, 2)
	    call tbcgtr (lptr, colptr, Memr[wbine], Memb[nulflg], 1, nbin)

	    call tbtclo (lptr)
	    call mfree (nulflg, TY_BOOL)
	}

	# Allocate memory for the spectra
	call malloc (I, npix, TY_REAL)
	call malloc (Q, npix, TY_REAL)
	call malloc (U, npix, TY_REAL)
	call malloc (V, npix, TY_REAL)
	call malloc (Is, npix, TY_REAL)
	call malloc (Qs, npix, TY_REAL)
	call malloc (Us, npix, TY_REAL)
	call malloc (Vs, npix, TY_REAL)
	call calloc (PL, npix, TY_REAL)
	call calloc (PC, npix, TY_REAL)
	call calloc (TH, npix, TY_REAL)
	call calloc (PLs, npix, TY_REAL)
	call calloc (PCs, npix, TY_REAL)
	call calloc (THs, npix, TY_REAL)
	call malloc (wave, npix, TY_REAL)
	call malloc (wav1, npix, TY_REAL)
	call malloc (wav2, npix, TY_REAL)
	call calloc (Ib, npix, TY_REAL)
	call calloc (Qb, npix, TY_REAL)
	call calloc (Ub, npix, TY_REAL)
	call calloc (Vb, npix, TY_REAL)
	call calloc (Isb, npix, TY_REAL)
	call calloc (Qsb, npix, TY_REAL)
	call calloc (Usb, npix, TY_REAL)
	call calloc (Vsb, npix, TY_REAL)
	call calloc (wavb, npix, TY_REAL)
	call calloc (binstrt, npix, TY_INT)
	call calloc (binend,  npix, TY_INT)

	# Create the output files as copies of the input files
	call mkfname (Memc[oroot], Memc[iextn], 1, 56, Memc[oname], SZ_FNAME)
	call mkfname (Memc[oroot], "c0h", 1, 4, Memc[owname], SZ_FNAME)
	first = true

	if (verbose) {
	    call printf ("%s -> %s\n")
		call pargstr (Memc[iname])
		call pargstr (Memc[oname])
	    call printf ("%s -> %s\n")
		call pargstr (Memc[wname])
		call pargstr (Memc[owname])
	    call flush (STDOUT)
	}

	# Find out if this is AMBER or BLUE detector data
	iferr (call imgstr (iptr, detector, Memc[det], SZ_LINE)) {
	       call error (1, "ERROR reading image header keyword: DETECTOR")
	}

	# Read the wavelength spectra for the 2 pass directions
	call gf_opengr (wptr, 1, datamin, datamax, 0)
	call amovr (Memr[imgl1r(wptr)], Memr[wav1], npix)
	call gf_opengr (wptr, 2, datamin, datamax, 0)
	call amovr (Memr[imgl1r(wptr)], Memr[wav2], npix)

	call imunmap (iptr)
	call imunmap (wptr)

	# If this is AMBER detector data, flip the wavelength vectors
	if (streq (Memc[det],"AMBER")) {
	    call vflip (Memr[wav1], npix)
	    call vflip (Memr[wav2], npix)
	}

	# Process the data; bin mode CERROR:
	if (streq (mode, "cerror")) {

	    # Load the appropriate wavelength vector to go with the
	    # chosen reference set.
	    if (refset == 2) 
		call amovr (Memr[wav2], Memr[wave], npix)
	    else
		call amovr (Memr[wav1], Memr[wave], npix)

	    # Compute the starting bin number
	    call bins_err (Memr[wave], npix, wbin1, bin1)

	    # Read the Stokes data
	    call rdata (Memc[iname],refset,npix,Memc[det],Memr[I],Memr[Q],
			Memr[U],Memr[V],Memr[Is],Memr[Qs],Memr[Us],Memr[Vs])

	    # Compute the binned spectra for the reference set
	    call binerr (Memr[I], Memr[Is], Memr[Q], Memr[Qs], Memr[U], 
		Memr[Us], Memr[V], Memr[Vs], Memr[wave],
		npix, paim, bin1, Memi[binstrt], Memi[binend], nbin, variwgt,
		Memr[PL], Memr[PLs], Memr[TH], Memr[THs], Memr[PC], Memr[PCs], 
		Memr[Ib], Memr[Qb], Memr[Ub], Memr[Vb], Memr[Isb], 
		Memr[Qsb], Memr[Usb], Memr[Vsb], Memr[wavb])

	    # Write the binned spectra to the output file
	    call wdata (Memc[iroot], Memc[iextn], Memc[oroot], Memc[wroot], 
			refset, nbin, Memc[det], Memr[wavb], Memr[Ib], Memr[Qb],
			Memr[Ub], Memr[Vb], Memr[Isb], Memr[Qsb], Memr[Usb], 
			Memr[Vsb], Memr[PL], Memr[PC], Memr[TH], Memr[PLs],
			Memr[PCs], Memr[THs], first)

	    # Loop over the remaining sets of spectra
	    do set = 1, 4, 1 {
		if (set == refset) next

		# Use the appropriate wavelength array for this set
		if (set == 2) 
		    call amovr (Memr[wav2], Memr[wave], npix)
		else
		    call amovr (Memr[wav1], Memr[wave], npix)

		# Read the Stokes spectra for this set
	        call rdata (Memc[iname],set,npix,Memc[det],Memr[I],Memr[Q],
			    Memr[U],Memr[V],Memr[Is],Memr[Qs],Memr[Us],Memr[Vs])

		# Compute the binned spectra, using the same bin locations
		# as computed for the reference set.
		call binwav (Memr[I], Memr[Is], Memr[Q], Memr[Qs], 
			Memr[U], Memr[Us], Memr[V], Memr[Vs], Memr[wave],
			npix, nbin, Memi[binstrt], Memi[binend], variwgt,
			Memr[PL], Memr[PLs], Memr[TH], Memr[THs], Memr[PC], 
			Memr[PCs], Memr[Ib], Memr[Qb], Memr[Ub], Memr[Vb], 
			Memr[Isb], Memr[Qsb], Memr[Usb], Memr[Vsb], 
			Memr[wavb])

		# Write the binned spectra to the output file
	        call wdata (Memc[iroot], Memc[iextn], Memc[oroot], Memc[wroot], 
			set, nbin, Memc[det], Memr[wavb],
			Memr[Ib], Memr[Qb], Memr[Ub], Memr[Vb], 
			Memr[Isb], Memr[Qsb], Memr[Usb], Memr[Vsb],
			Memr[PL], Memr[PC], Memr[TH], Memr[PLs],
			Memr[PCs], Memr[THs], first)

	   } #end of loop over 4 data sets

	# Bin mode CWAVE or LIST:
	} else {

	    # Loop over the four sets of spectra
	    do set = 1, 4, 1 {

		# Use the appropriate wavelength array for this set
		if (set == 2) 
		    call amovr (Memr[wav2], Memr[wave], npix)
		else
		    call amovr (Memr[wav1], Memr[wave], npix)

		# For mode CWAVE compute the bin locations based on the 
		# chosen number of output bins.
		if (streq(mode, "cwave")) {
	            call bins_wave (Memr[wave], npix, nbin, lbin, Memi[binstrt],
				    Memi[binend])
		} else {
		# For mode LIST compute the bin locations based on the
		# chosen wavelength positions of the bins.

		    call bins_list (Memr[wave], npix, Memr[wbins], Memr[wbine],
				    nbin, Memi[binstrt], Memi[binend])
		}

		# Read the Stokes spectra
	        call rdata (Memc[iname],set,npix,Memc[det],Memr[I],Memr[Q],
			    Memr[U],Memr[V],Memr[Is],Memr[Qs],Memr[Us],Memr[Vs])

		# Compute the binned spectra
		call binwav (Memr[I], Memr[Is], Memr[Q], Memr[Qs], 
			Memr[U], Memr[Us], Memr[V], Memr[Vs], Memr[wave],
			npix, nbin, Memi[binstrt], Memi[binend], variwgt,
			Memr[PL], Memr[PLs], Memr[TH], Memr[THs],
			Memr[PC], Memr[PCs], Memr[Ib], Memr[Qb], Memr[Ub],
			Memr[Vb], Memr[Isb], Memr[Qsb], Memr[Usb], Memr[Vsb], 
			Memr[wavb])

		# Write the binned spectra to the output file
	        call wdata (Memc[iroot], Memc[iextn], Memc[oroot], Memc[wroot], 
			set, nbin, Memc[det], Memr[wavb],
			Memr[Ib], Memr[Qb], Memr[Ub], Memr[Vb], Memr[Isb], 
			Memr[Qsb], Memr[Usb], Memr[Vsb], Memr[PL], Memr[PC], 
			Memr[TH], Memr[PLs], Memr[PCs], Memr[THs], first)

	   } # end of loop over 4 sets
	} # end of cwave and list mode processing

	# All done: free memory
	call mfree (wave, TY_REAL)
	call mfree (wav1, TY_REAL)
	call mfree (wav2, TY_REAL)
	call mfree (I, TY_REAL)
	call mfree (Q, TY_REAL)
	call mfree (U, TY_REAL)
	call mfree (V, TY_REAL)
	call mfree (Is, TY_REAL)
	call mfree (Qs, TY_REAL)
	call mfree (Us, TY_REAL)
	call mfree (Vs, TY_REAL)
	call mfree (PL, TY_REAL)
	call mfree (PC, TY_REAL)
	call mfree (TH, TY_REAL)
	call mfree (PLs, TY_REAL)
	call mfree (PCs, TY_REAL)
	call mfree (THs, TY_REAL)
	call mfree (Ib, TY_REAL)
	call mfree (Qb, TY_REAL)
	call mfree (Ub, TY_REAL)
	call mfree (Vb, TY_REAL)
	call mfree (Isb, TY_REAL)
	call mfree (Qsb, TY_REAL)
	call mfree (Usb, TY_REAL)
	call mfree (Vsb, TY_REAL)
	call mfree (wavb, TY_REAL)
	call mfree (binstrt, TY_INT)
	call mfree (binend,  TY_INT)
	if (streq(mode, "list")){
	    call mfree (wbins, TY_REAL)
	    call mfree (wbine, TY_REAL)
	}

	call sfree (sp)

end

procedure bins_err (wav, npix, wbin1, bin1)

# Compute the pixel number corresponding to the requested beginning
# wavelength value.

real	wav[ARB], wbin1
int	npix, bin1

int	i

begin
	bin1 = 0
	do i = 1, npix-1, 1 {
	   if ((wbin1 > wav[i]) && (wbin1 <= wav[i+1]))
		bin1 = i
	}

	bin1 = min (max (bin1, 1), npix)

end


procedure bins_wave (wav, npix, nbin, lbin, binstrt, binend)

# Compute the pixel indices corresponding the ends of equally-spaced bins
# in either wavelength or inverse-wavelength space.

real	wav[ARB]
int	npix, nbin, binstrt[ARB], binend[ARB]
bool	lbin

int	i, j
real	lwid, gwav

begin

	# Equally-spaced bins in inverse-wavelength space.
	if (lbin) {

	    lwid = ( (1./wav[1]) - (1./wav[npix]) ) / nbin
	    do j = 1, nbin {
	       gwav = 1.0 / ( (1./wav[1]) - j*lwid )
	       do i = 1, npix-1 {
		  if ((gwav>=wav[i]) && (gwav<wav[i+1])) binend[j] = i
	       }
	       if (j > 1) binend[j]=min(max(binend[j],binend[j-1]+1),npix)
	    }
	    binend[nbin] = npix

	} else {
	# Equally-spaced bins in wavelength space.

	    lwid = ( wav[npix] - wav[1] ) / nbin
	    do j = 1, nbin {
	       gwav = wav[1] + j*lwid
	       do i = 1, npix-1 {
		  if ((gwav>=wav[i]) && (gwav<wav[i+1])) binend[j] = i
	       }
	       if (j > 1) binend[j]=min(max(binend[j],binend[j-1]+1),npix)
	    }
	    binend[nbin] = npix
	}

	binstrt[1] = 1
	if (nbin > 1) {
	    do j = 2, nbin {
	       binstrt[j] = min(binend[j-1]+1, npix)
	    }
	}

end

procedure bins_list (wave, npix, wavstrt, wavend, nbin, binstrt, binend)

# Compute the pixel indices corresponding to the input list of starting
# and ending wavelength positions for the output bins.

real	wave[ARB], wavstrt[ARB], wavend[ARB]
int	npix, nbin, binstrt[ARB], binend[ARB]

int	i, j

begin

	# Loop over bins
	do i = 1, nbin {

	   # Check for reversed start/end values
	   if (wavstrt[i]>wavend[i]) {
	       j = wavend[i]
	       wavend[i] = wavstrt[i]
	       wavstrt[i] = j
	   }

	   # Check for invalid locations
	   if ((wavstrt[i]<wave[1] && wavend[i]<wave[1]) ||
	       (wavstrt[i]>wave[npix] && wavend[i]>wave[npix]))
		call error (1, "Specified bin location not within image limits")

	   # Loop over pixels
	   do j = 1, npix-1 {
	      if (wavstrt[i]>=wave[j] && wavstrt[i]<wave[j+1]) binstrt[i] = j
	      if (wavend[i] >=wave[j] && wavend[i] <wave[j+1]) binend[i]  = j
	   }

	   if (wavstrt[i] < wave[1])    binstrt[i] = 1
	   if (wavend[i] >= wave[npix]) binend[i]  = npix
	}

end

procedure vflip (a, npix)
 
real	a[ARB]
int	npix
 
pointer	tmp
int	i
 
begin

	call malloc (tmp, npix, TY_REAL)

	do i = 0, npix-1 {
	   Memr[tmp+i] = a[npix-i]
	}
	do i = 0, npix-1 {
	   a[i+1] = Memr[tmp+i]
	}

	call mfree (tmp, TY_REAL)

end

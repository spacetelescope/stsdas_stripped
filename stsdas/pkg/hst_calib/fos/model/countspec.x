include	<tbset.h>
include	<synphot.h>
define	MAXLIST	100
define	MAXPARAM  3

procedure t_countspec()

pointer	command
pointer	obsmode
pointer	output
pointer	wavtable
pointer	grftable
pointer	cmptable
real	hstarea

bool	verbose
int	degree, nwave, done, i, ncomp
pointer	grat, wave, wptr, cptr, tptr, pcode, spect, bandps, banderr, thr
pointer	filelist, nparam, paramlist
pointer	tp, tw
pointer	sp

data	verbose / false /

bool	streq()
pointer	tbtopn()
int	anytophot(), phottoany(), tbpsta(), strsearch()
real	clgetr()

begin
	# Allocate dynamic memory for strings
	call smark(sp)
	call salloc (command, SZ_LINE, TY_CHAR)
	call salloc (obsmode, SZ_LINE, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (wavtable, SZ_FNAME, TY_CHAR)
	call salloc (grftable, SZ_FNAME, TY_CHAR)
	call salloc (cmptable, SZ_FNAME, TY_CHAR)

	call salloc (grat, 5, TY_CHAR)
	call salloc (filelist, (SZ_FNAME+1)*MAXLIST, TY_CHAR)
	call salloc (nparam, MAXLIST, TY_INT)
	call salloc (paramlist, MAXPARAM*MAXLIST, TY_REAL)
	call salloc (pcode, SZ_LINE, TY_INT)

	# Read task parameters
	call clgstr ("spectrum", Memc[command], SZ_LINE)
	call clgstr ("obsmode", Memc[obsmode], SZ_LINE)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("wavetab", Memc[wavtable], SZ_FNAME)
	call clgstr ("grtbl", Memc[grftable], SZ_FNAME)
	call clgstr ("cmptbl", Memc[cmptable], SZ_FNAME)
	hstarea = clgetr( "area" )

	# Stash the telescope area
	call put_hstarea (hstarea)

	# Create the output table
	tp = tbtopn (Memc[output], NEW_FILE, NULL)
	call tbpset (tp, TBL_ROWLEN, 3)
	call tbpset (tp, TBL_MAXCOLS, 3)
	call tbpset (tp, TBL_MAXPAR, 10)
	call tbcdef (tp, wptr, "WAVELENGTH", "angstroms", "", TY_REAL, 1, 1)
	call tbcdef (tp, cptr, "FLUX", "counts", "", TY_REAL, 1, 1)
	call tbcdef (tp, tptr, "THR", "", "", TY_REAL, 1, 1)
	call tbtcre (tp)

	# Get wavelength set
	call inisyntab
	if (Memc[wavtable] == EOS || streq (Memc[wavtable], "none") ||
	    streq (Memc[wavtable], "NONE")) {

	    nwave = 3451
	    call malloc (wave, nwave, TY_REAL)
	    do i = 1, nwave
	       Memr[wave+i-1] = 1100.0 + 2.0*(i-1)

	} else {

	    tw = tbtopn (Memc[wavtable], READ_ONLY, NULL)
	    nwave = tbpsta (tw, TBL_NROWS)
	    call malloc (wave, nwave, TY_REAL)
	    call rdwave (tw, nwave, Memr[wave])
	    call tbtclo (tw)
	}

	call tbcptr (tp, wptr, Memr[wave], 1, nwave)
	call salloc (spect, nwave, TY_REAL)
	call salloc (bandps, nwave, TY_REAL)
	call salloc (banderr, nwave, TY_REAL)
	call salloc (thr, nwave, TY_REAL)
	
	# Figure out which grating is being used
	i = strsearch(Memc[obsmode], "g")
	if (i > 0)
	    call strcpy (Memc[obsmode+i-2], Memc[grat], 5)
	else
	    call error (1, "Grating name not found in obsmode string")
	
	# Get the list of components used by the obsmode
	call searchgraf (verbose, Memc[grftable], Memc[cmptable], Memc[obsmode],
			 MAXLIST, MAXPARAM, SZ_FNAME, ncomp, Memi[nparam],
			 Memr[paramlist], Memc[filelist])

	# Modify the component list to exclude grating blaze function
	call fixlist (Memc[filelist], ncomp, SZ_FNAME, Memc[grat])

	# Compute the bandpass from the component list
	call getthruput (SZ_FNAME, ncomp, Memc[filelist], MAXPARAM,
			 Memi[nparam], Memr[paramlist], nwave, Memr[wave],
		         Memr[bandps], Memr[banderr])

	# Compute THR from the bandpass (multiplies by lam*dlam*area/hc
	# to convert flam to counts)
	call amovr (Memr[bandps], Memr[thr], nwave)
	done = anytophot ("flam", nwave, Memr[wave], Memr[thr])
	done = phottoany ("counts", nwave, Memr[wave], Memr[thr])

	# Write THR to the output table
	call tbcptr (tp, tptr, Memr[thr], 1, nwave)

	# Compile spectrum expression into pseudocode
	call syncompile (Memc[command], Memi[pcode], SZ_LINE)

	# Calculate spectrum
	call syncalc (Memi[pcode], SZ_LINE, NULL, nwave, Memr[wave], 
		      Memc[grftable], Memc[cmptable], Memr[spect], degree)

	# Multiply spectrum by bandpass
	call amulr (Memr[bandps], Memr[spect], Memr[spect], nwave)

	# Convert spectrum units to counts
	done = phottoany ("counts", nwave, Memr[wave], Memr[spect])
	
	# Write the spectrum to the output table
	call tbcptr (tp, cptr, Memr[spect], 1, nwave)

	# Close files and release memory
	call tbtclo (tp)
	call clssyntab

	call mfree (wave, TY_REAL)
	call sfree (sp)
end

procedure fixlist (filelist, ncomp, mxfile, grat)

char	filelist[mxfile,ARB]
int	ncomp
int	mxfile
char	grat[ARB]

int	ic, icomp

int	strsearch()

begin

	ic = 0

	# Search for and remove the grating throughput tables
	do icomp = 1, ncomp {

	   if (strsearch(filelist[1,icomp], grat) > 0) next

	   ic = ic + 1
	   call strcpy (filelist[1,icomp], filelist[1,ic], mxfile)
	}
	ncomp = ic

	# Search for and add an extra copy of the collimator table
	do icomp = 1, ncomp {

	   if (strsearch(filelist[1,icomp], "rflcol") > 0) {
	       call strcpy (filelist[1,icomp], filelist[1,ncomp+1], mxfile)
	       break
	   }
	}
	ncomp = ncomp+1

end

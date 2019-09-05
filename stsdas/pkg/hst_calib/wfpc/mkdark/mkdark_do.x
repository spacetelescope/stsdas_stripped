include <mach.h>
include <imhdr.h>
include	"mkdark.h"

#  mkdark_do -- Perform cosmic ray rejection
#
#  Description:
#  ------------
#  This is mostly a file housekeeping routine for the cosmic ray rejection task.
#  It takes care of input/output files open/close, check for dimensions, read/
#  write data from/to files, allocate memory spaces etc.
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Aug-1993  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure mkdark_do (fin, fout, fout2, niter, sigma, sigmas, psigma, rej, 
			initial, readout, gain, scale, hotthresh, minval, 
			fillval, verbose)

pointer	fin 			# input: file template pointer
char	fout[SZ_FNAME]		# input: output file name
char	fout2[SZ_FNAME]		# input: good point output file name
int	niter			# input: number of iterations
real	sigma[*]
char	sigmas[SZ_LINE]
real	psigma
real	rej		# input: the radius to which pixels adjacent to a
			#   (directly) rejected pixel should also be discarded
char	initial[SZ_LINE]	# input: scheme of initial estimate
real	readout, gain, scale	# input: noise values
real	hotthresh
real	minval
real	fillval
bool	verbose

char	gfout[SZ_FNAME]
char	gfout2[SZ_FNAME]
pointer	ipin[MAX_FILES]
int	init
pointer	ipest
pointer	ipout, ipout2

int	dim_x, dim_y
char	fdata[SZ_FNAME, MAX_FILES]
char	dumch[1]
char	append[SZ_FNAME]
int	nf			# number of existing files
int	ngrp
int	j, k, n, grp
pointer	v, v2, sp
real	xmin, xmax
pointer ave, npts, work, buf, buf2
char	text[SZ_LINE], tstring[SZ_LINE]
bool	out2
	
pointer	gf_map()
int	impnlr()
int	impnls()
int	itoc()
long	clktime()
#==============================================================================
begin
	call smark (sp)

	out2 = (fout2[1] != EOS)

	call cnvtime (clktime(0), tstring, SZ_LINE)
	call sprintf (text, SZ_LINE, 
		"Task MKDARK, version %s, starts at %s")
	    call pargstr (VERSION)
	    call pargstr (tstring)

	# open input files and temporary files, check the parameters
	call mkdark_check (fin, fdata, ipin, nf, initial, init, ipest, ngrp, 
				dim_x, dim_y)
	
	# allocate array space
	call salloc (v, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call malloc (ave, dim_x*dim_y, TY_REAL)
	call malloc (npts, dim_x*dim_y, TY_SHORT)
	call malloc (work, nf*dim_x, TY_REAL)

	# loop all groups
	do grp = 1, ngrp {

	    if (verbose) {
		call printf ("Begin processing group %d\n")
		call pargi (grp)
		call flush (STDOUT)
	    }

	    # if more than one group, open the next group
	    if (ngrp > 1) {

		# add the following for initial guess image. JC Hsu 6/6/94
		if (init == IMAGE) 
		    call gf_opengr (ipest, grp, xmin, xmax, 0)
	    	do n = 1, nf
		    call gf_opengr (ipin[n], grp, xmin, xmax, 0)
	    }

	    # open output file
	    call strcpy (fout, gfout, SZ_FNAME)
	    if (out2)
	    	call strcpy (fout2, gfout2, SZ_FNAME)

	    if (grp == 1) {
		call strcpy ("[1/", append, SZ_FNAME)
		k = itoc (ngrp, dumch, 1)
		call strcat (dumch, append, SZ_FNAME)
		call strcat ("]", append, SZ_FNAME)
	        call strcat (append, gfout, SZ_FNAME)
		ipout = gf_map (gfout, NEW_COPY, ipin[1])	
		if (out2) {
	            call strcat (append, gfout2, SZ_FNAME)
		    ipout2 = gf_map (gfout2, NEW_COPY, ipin[1])	
		    IM_PIXTYPE(ipout2) = TY_SHORT
		} 


		call gf_iputh(ipout, "HISTORY", text)
		if (out2) call gf_iputh(ipout2, "HISTORY", text)

	    	# write input file names to output file as history
	    	call sprintf (text, SZ_LINE, "input files used are:")
	    	call gf_iputh(ipout, "HISTORY", text)
		if (out2) call gf_iputh(ipout2, "HISTORY", text)
	    	do n = 1, nf {
		    call sprintf (text, SZ_LINE, "  %s")
		        call pargstr (fdata[1,n])
		    call gf_iputh(ipout, "HISTORY", text)
		    if (out2) call gf_iputh(ipout2, "HISTORY", text)
	    	}

		# record parameters in the header
		call sprintf (text, SZ_LINE, "Rejection levels are : %s sigmas")
		    call pargstr (sigmas)
		call gf_iputh(ipout, "HISTORY", text)
		if (out2) call gf_iputh(ipout2, "HISTORY", text)
		call sprintf (text, SZ_LINE, 
			"Expansion radius is : %0.2f pixels")
		    call pargr (rej)
		call gf_iputh(ipout, "HISTORY", text)
		if (out2) call gf_iputh(ipout2, "HISTORY", text)
		call sprintf (text, SZ_LINE, 
			"Expansion discriminant reduction factor is : %0.3f")
		    call pargr (psigma)
		call gf_iputh(ipout, "HISTORY", text)
		if (out2) call gf_iputh(ipout2, "HISTORY", text)
		call sprintf (text, SZ_LINE, "Readout noise = %0.3f(DN), Gain = %0.4f(electron/DN), Scale noise = %0.3f")
		    call pargr (readout)
		    call pargr (gain)
		    call pargr (scale)
		call gf_iputh(ipout, "HISTORY", text)
		if (out2) call gf_iputh(ipout2, "HISTORY", text)
		call sprintf (text, SZ_LINE, 
			"Cutoff level for hot pixels is : %0.2f DN")
		    call pargr (hotthresh)
		call gf_iputh(ipout, "HISTORY", text)
		if (out2) call gf_iputh(ipout2, "HISTORY", text)
		call sprintf (text, SZ_LINE, 
			"Minimum allowed DN value is : %0.2f")
		    call pargr (minval)
		call gf_iputh(ipout, "HISTORY", text)
		if (out2) call gf_iputh(ipout2, "HISTORY", text)
		call sprintf (text, SZ_LINE, 
			"Initial average image used : %s")
		    call pargstr (initial)
		call gf_iputh(ipout, "HISTORY", text)
		if (out2) call gf_iputh(ipout2, "HISTORY", text)

	    } else {
	    	call gf_opengr (ipout, grp, xmin, xmax, ipin[1])
		if (out2)
	    	    call gf_opengr (ipout2, grp, xmin, xmax, ipin[1])
	    }

	    call mkdark_init (ipin, ipest, nf, dim_x, dim_y, init, minval, 
				Memr[ave], Memr[work])

	    call mkdark_loop (ipin, nf, rej, int(rej+1.e-5), niter, 
			dim_x, dim_y, sigma, psigma, readout, gain, scale, 
			hotthresh, Memr[ave], Mems[npts], minval, fillval, 
			verbose)

	    call amovkl (long(1), Meml[v], IM_MAXDIM)
	    call amovkl (long(1), Meml[v2], IM_MAXDIM)
	    do j = 1, dim_y {
	        k = impnlr (ipout, buf, Meml[v])
		call amovr (Memr[ave+(j-1)*dim_x], Memr[buf], dim_x)
		if (out2) {
	            k = impnls (ipout2, buf2, Meml[v2])
		    call amovs (Mems[npts+(j-1)*dim_x], Mems[buf2], dim_x)
		}
	    }
	}

	# write history of ending time to output file
	call cnvtime (clktime(0), tstring, SZ_LINE)
	call sprintf (text, SZ_LINE, "Task MKDARK stops at %s")
	    call pargstr (tstring)
	call gf_iputh(ipout, "HISTORY", text)

	# blank(zero) out certain header parameters in the output files
	call gf_ipstr(ipout, "ROOTNAME", "")
	if (out2) {
	    call gf_iputh(ipout2, "HISTORY", text)
	    call gf_ipstr(ipout2, "ROOTNAME", "")
	}

	# close input files
	do n = 1, nf {
	    call gf_unmap (ipin[n])
	}

	# add the following for initial guess image.  JC Hsu 6/6/94
	if (init == IMAGE)
	    call gf_unmap (ipest)

	# close output files
	call gf_unmap (ipout)

	# print out message of which files been created
	call printf ("MKDARK: output file %s is created\n")
	    call pargstr (fout)
	
	if (out2) {
	    call gf_unmap (ipout2)
	    call printf ("MKDARK: output file %s is created\n")
	        call pargstr (fout2)
	}

	call mfree (ave, TY_REAL)
	call mfree (npts, TY_SHORT)
	call mfree (work, TY_REAL)
	call sfree (sp)
end

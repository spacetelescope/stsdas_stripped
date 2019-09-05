include <mach.h>
include <imhdr.h>
include	"crrej.h"

#  crrej_do -- Perform cosmic ray rejection
#
#  Description:
#  ------------
#  This is mostly a file bookkeeping routine for the cosmic ray rejection task.
#  It takes care of input/output files open/close, check for dimensions, read/
#  write data from/to files, allocate memory spaces etc.
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Aug-1993  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------
procedure crrej_do(tpin, fout, tpmask, par, niter, sigma)

# inputs:
pointer	tpin 			# input image template pointer
char	fout[ARB]		# output file name
pointer	tpmask 			# mask template pointer
pointer	par 			# par structure pointer
int     niter                   # number of rejection iterations
real    sigma[ARB]		# statistical thrsholds used in CR rejection

# locals:
char	gfout[SZ_FNAME]
pointer	ipin[MAX_FILES]
real	skyval[MAX_FILES]
real	sky_tot
real	efac[MAX_FILES]
real	exptot[MAX_NEXP]
int	init
pointer	readout, gain, scale
pointer	ipest
pointer	ipout
pointer	ipmask[MAX_FILES]

int	dim_x, dim_y
char	fdata[SZ_FNAME, MAX_FILES]
char	dumch[1]
char	append[SZ_FNAME]
char	str[SZ_FNAME, MAX_NEXP]
int	nfiles			# number of existing files
int	nmasks			# number of mask files
int	ngrp
int	i, j, k, n, grp, indx
pointer	v, sp
real	xmin, xmax
real	tm
pointer ave, avevar, efacsum, work, work2, buf
	
int	imtlen()
pointer	gf_map()
real	imgetr()
int	impnlr()
int	itoc()
int	strtor()
#int     imaccf()
#==============================================================================
begin
	call smark(sp)

	# open input files and temporary files, check the parameters
	call crrej_check(tpin, tpmask, par,   fdata, ipin, ipmask, 
			  ngrp, dim_x, dim_y, init, ipest)
	
	nfiles = imtlen(tpin)
	nmasks = imtlen(tpmask)

	# calculate the scaling factors due to different exposure time
	call cr_scaling(EXPNAME(par), ipin, fdata, nfiles,   efac)

	# calculate the total exposure time
	exptot[1] = 0.
	do n = 1, nfiles {
	    exptot[1] = efac[n] + exptot[1]
	}
	call strcpy(EXPNAME2(par), str[1, 2], SZ_FNAME)
	call strcpy(EXPNAME3(par), str[1, 3], SZ_FNAME)
	do i = 2, NEXPNAMES(par) {
	    exptot[i] = 0.
	    do n = 1, nfiles {
                iferr (tm = imgetr(ipin[n], str[1, i])) {
		    call printf("Warning: The keyword %s does not exist.\n")
		        call pargstr(str[1,i])
		    exptot[i] = NONEXIST
		    break
		}
	        exptot[i] = tm + exptot[i]
	    }
	}

	# allocate array space
	call salloc(v, IM_MAXDIM, TY_LONG)
	call salloc(readout, ngrp, TY_REAL)
	call salloc(gain, ngrp, TY_REAL)
	call salloc(scale, ngrp, TY_REAL)

	call calloc(ave, dim_x*dim_y, TY_REAL)
	call calloc(avevar, dim_x*dim_y, TY_REAL)
	call calloc(efacsum, dim_x*dim_y, TY_REAL)
	call calloc(work, nfiles*dim_x, TY_REAL)
	call calloc(work2, nfiles*dim_x, TY_REAL)

	# parse the noise strings
	n = strtor(READNOISE(par), Memr[readout])
	if (n > ngrp) 
	    call error(1, "too many numbers in the parameter 'readnoise'")
	else if (n < 1)
	    call error(1, "the parameter 'readnoise' is empty")
	else if (n < ngrp) {
	    do j = n+1, ngrp
		Memr[readout+j-1] = Memr[readout+n-1]
	}
	n = strtor(ATODGAIN(par), Memr[gain])
	if (n > ngrp) 
	    call error(1, "too many numbers in the parameter 'atodgain'")
	else if (n < 1)
	    call error(1, "the parameter 'atodgain' is empty")
	else if (n < ngrp) {
	    do j = n+1, ngrp
		Memr[gain+j-1] = Memr[gain+n-1]
	}
	n = strtor(SCALENOISE(par), Memr[scale])
	if (n > ngrp) 
	    call error(1, "too many numbers in the parameter 'scalenoise'")
	else if (n < 1)
	    call error(1, "the parameter 'scalenoise' is empty")
	else if (n < ngrp) {
	    do j = n+1, ngrp
		Memr[scale+j-1] = Memr[scale+n-1]
	}
	# input scale is in per cents
	do j = 1, ngrp
	    Memr[scale+j-1] = Memr[scale+j-1] / 100.
	
	# loop all groups
	do grp = 1, ngrp {

	    if (VERBOSE(par)) {
		call printf("Begin processing group %d of %d\n")
		    call pargi(grp)
		    call pargi(ngrp)
		call flush(STDOUT)
	    }

	    # if more than one group, open the next group
	    if (grp > 1) {

		# add the following for initial guess image. JC Hsu 6/6/94
		if (init == IMAGE) 
		    call gf_opengr(ipest, grp, xmin, xmax, 0)
	    	do n = 1, nfiles
		    call gf_opengr(ipin[n], grp, xmin, xmax, 0)
		if (nmasks > 0) {
	    	    do n = 1, nmasks
		        call gf_opengr(ipmask[n], grp, xmin, xmax, ipin[n])
		}
	    }

	    # calculate the sky levels 
	    call crrej_sky(SKY(par), ipin, nfiles, dim_x, dim_y, LOWER(par), 
				UPPER(par), skyval)
	    if (VERBOSE(par)) {
		do k = 1, nfiles {
		    call printf("sky of '%s' is %0.3f DN\n")
			call pargstr(fdata[1,k])
			call pargr(skyval[k])
		}
	    }

	    # open the output file by using the first input image as the 
	    # template
	    call strcpy(fout, gfout, SZ_FNAME)

	    if (grp == 1) {

		# construct the output file name with group specification
		call strcpy("[1/", append, SZ_FNAME)
		k = itoc(ngrp, dumch, 1)
		call strcat(dumch, append, SZ_FNAME)
		call strcat("]", append, SZ_FNAME)
	        call strcat(append, gfout, SZ_FNAME)
		ipout = gf_map(gfout, NEW_COPY, ipin[1])	

		# record parameters to the output file
		call cr_history(ipout, fdata, nfiles, par)
		if (nmasks > 0) {
		    do n = 1, nmasks
			call cr_history(ipmask[n], fdata, nfiles, par)
		}
	    } else {
	    	call gf_opengr(ipout, grp, xmin, xmax, ipin[1])
	    }

	    # get the initial pixel values to be used to compared against all 
	    # images.
	    call crrej_init(ipin, ipmask, nfiles, nmasks, par, dim_x, dim_y, 
				init, Memr[readout+grp-1], Memr[gain+grp-1], 
				Memr[scale+grp-1], efac, skyval, Memr[ave], 
				Memr[avevar],   Memr[work], Memr[work2])

	    # do the iterative cosmic ray rejection calculations
	    call crrej_loop(ipin, ipmask, nfiles, nmasks, par, 
				niter, init, dim_x, dim_y, sigma, 
				Memr[readout+grp-1], Memr[gain+grp-1], 
				Memr[scale+grp-1], efac, skyval, Memr[ave], 
				Memr[avevar], Memr[efacsum])
	    call amovkl(long(1), Meml[v], IM_MAXDIM)
	    do j = 1, dim_y {
		indx = ave+(j-1)*dim_x
		do i = 1, dim_x {
		    if (Memr[indx+i-1] != INDEF)
			Memr[indx+i-1] = exptot[1] * Memr[indx+i-1]
		    else
			Memr[indx+i-1] = FILLVAL(par)
		}
	        k = impnlr(ipout, buf, Meml[v])
		call amovr(Memr[indx], Memr[buf], dim_x)
	    }
	    call gf_iastr(ipout, "ROOTNAME", "")

	    # update the sky value to the input images
	    if (SKYNAME(par) != EOS) {
		sky_tot = 0.
		do n = 1, nfiles {
		    call gf_iputr(ipin[n], SKYNAME(par), skyval[n])
		    sky_tot = sky_tot + skyval[n]
		}
		call gf_iputr(ipout, SKYNAME(par), sky_tot)
	    }
	}

	# blank(zero) out certain header parameters in the output files
        ###if (imaccf(ipout, "ROOTNAME") == YES)
	    call gf_iastr(ipout, "ROOTNAME", "")

	call gf_iputr(ipout, EXPNAME(par), exptot[1])
	do i = 2, NEXPNAMES(par) {
	    if (exptot[i] != NONEXIST) call gf_iputr(ipout, str[1,i], exptot[i])
	}

	# close input files
	do n = 1, nfiles
	    call gf_unmap(ipin[n])
	
	if (init == IMAGE)
	    call gf_unmap(ipest)

	# close output files
	call gf_unmap(ipout)
	if (nmasks > 0) {
	    do n = 1, nmasks
	        call gf_unmap(ipmask[n])
	}

	# deallocate memories
	call mfree(ave, TY_REAL)
	call mfree(avevar, TY_REAL)
	call mfree(efacsum, TY_REAL)
	call mfree(work, TY_REAL)
	call mfree(work2, TY_REAL)
	call sfree(sp)
end

include	<mach.h>
include	"warmpix.h"

#  warm_do -- Perform warm pixel flagging and fixing
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  21-Jul-1995  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------
procedure warm_do (tpin, tpmask, thresh, fix_dqval, rej_val,
			chip, x, y, ep, dark, nrows, verbose)

# inputs:
pointer	tpin 			# input image template pointer
pointer	tpmask 			# input mask template pointer
real	thresh[ARB]
short	fix_dqval		# data quality value of fixed pixels
real	rej_val			# pixel value of rejected pixels
int	chip[ARB], x[ARB], y[ARB]
double	ep[ARB]
real	dark[ARB]
int	nrows
bool	verbose

# local:
pointer	ipin
pointer	ipmask
pointer	ipdark, ipflat
char	fdata[SZ_FNAME]
int	x0, y0
int	nfiles			# number of existing files
int	det
int	ngrp
int	nep
double	ep0[MAX_EPOCH]
real	dark0[MAX_EPOCH]
double	epoch, t_start, t_end
real	olddark, flat
real	darktime
int	indx
int	i, j, grp
real	xmin, xmax
pointer pic, mask, od, ff
int	npts, nout
char	text[SZ_LINE], tstring[SZ_LINE]
	
pointer	imgs2r(), imgs2s()
pointer	imps2r(), imps2s()
int	imtlen()
int	imgeti()
real	imgetr()
double	imgetd()
long	clktime()
#==============================================================================
begin
	nfiles = imtlen(tpin)

	# loop all images
	do i = 1, nfiles {

	    # open the next input file/mask, check the parameters
	    call warm_check (tpin, tpmask, fdata, ipin, ipmask, ipdark, ipflat,
				ngrp)

	    # get the epoch
	    t_start = imgetd (ipin, "EXPSTART")
	    t_end = imgetd (ipin, "EXPEND")
	    epoch = (t_start + t_end) / 2.

	    darktime = imgetr (ipin, "DARKTIME")

	    do grp = 1, ngrp {

	        # reset out-of-bound point counter
	        nout  = 0

	        call gf_opengr (ipin, grp, xmin, xmax, 0)
		call gf_opengr (ipmask, grp, xmin, xmax, 0)
		det = imgeti (ipin, "DETECTOR")

	        if (verbose) {
		    call printf ("Begin processing chip %d\n")
		        call pargi (det)
		    call flush (STDOUT)
	        }
		
		# read the data
		pic = imgs2r (ipin, 1, DIM_X, 1, DIM_Y)
		mask = imgs2s (ipmask, 1, DIM_X, 1, DIM_Y)

		# open the correct group of dark and flat field reference files
		if (ipdark != EMPTY) {
		    call gf_opengr (ipdark, det, xmin, xmax, 0)
		    od = imgs2r (ipdark, 1, DIM_X, 1, DIM_Y)
		} else olddark = 0.
		if (ipflat != EMPTY) {
		    call gf_opengr (ipflat, det, xmin, xmax, 0)
		    ff = imgs2r (ipflat, 1, DIM_X, 1, DIM_Y)
		} else flat = 1.

		# look up the warm pixel table and find the matching rows
		x0 = 0
		y0 = 0
		npts = 0
		do j = 1, nrows {
		    if (chip[j] < det) next
		    else if (chip[j] > det) break
		    else {

			# if a new (x,y) is read from the table, do the fix
			if (x[j] != x0 || y[j] != y0) {
			    if (npts != 0) {
			        call warm_fix (Memr[pic+indx], Mems[mask+indx],
						olddark, flat, epoch, darktime,
						ep0, dark0, nep, thresh, 
						fix_dqval, rej_val, nout) 
			    }
			    x0 = x[j]
			    y0 = y[j]
			    indx = (y0-1)*DIM_X+x0-1

			    # read reference values at the corresponding pixel
			    if (ipdark != EMPTY) olddark = Memr[od+indx]
			    if (ipflat != EMPTY) flat = Memr[ff+indx]
			    npts = npts + 1
			    nep = 1
			} else {
			    nep = nep + 1
			}
			ep0[nep] = ep[j]
			dark0[nep] = dark[j]

			# if this is the last row, do the fix
			if (j == nrows) {
			    call warm_fix (Memr[pic+indx], Mems[mask+indx], 
					   olddark, flat, epoch, darktime, 
					   ep0, dark0, nep, thresh, 
					   fix_dqval, rej_val, nout) 
			}
		    }
		}

		if (verbose) {
		    if (nout > 0) {
			call printf ("%d points are outside the hot pixel table's epoch range\n")
			    call pargi (nout)
		    }
		    call printf ("Total number of hot pixels listed in the table is %d\n")
			call pargi (npts)
		}

	        # write results 
	        call amovr(Memr[pic], Memr[imps2r(ipin, 1, DIM_X, 1, DIM_Y)], 
			    DIM_X*DIM_Y)
	        call amovs(Mems[mask], Mems[imps2s(ipmask, 1, DIM_X, 1, DIM_Y)],
			    DIM_X*DIM_Y)
	    }

	    # write history of ending time to output files
	    call cnvtime (clktime(0), tstring, SZ_LINE)
	    call sprintf (text, SZ_LINE, "Ran task WARMPIX, version %s at %s, rej_thresh=%0.5f, fix_thresh=%0.5f, var_thresh=%0.5f,")
	        call pargstr (VERSION)
	        call pargstr (tstring)
		call pargr (thresh[1])
		call pargr (thresh[2])
		call pargr (thresh[3])
	    call gf_iputh(ipin, "HISTORY", text)
	    call gf_iputh(ipmask, "HISTORY", text)

	    call sprintf (text, SZ_LINE, "fix_dqval=%d, rej_val=%0.1f")
		call pargs (fix_dqval)
		call pargr (rej_val)
	    call gf_iputh(ipin, "HISTORY", text)
	    call gf_iputh(ipmask, "HISTORY", text)
	
	    # close files
	    call gf_unmap (ipin)
	    call gf_unmap (ipmask)
	    if (ipdark != EMPTY) call gf_unmap (ipdark)
	    if (ipflat != EMPTY) call gf_unmap (ipflat)
	}
end

#------------------------------------------------------------------------------
procedure warm_fix (pic_val, mask_val, olddark, flat, epoch, darktime, 
			ep0, dark0, nep, thresh, fix_dqval, rej_val, nout) 
#inputs
real	pic_val
short	mask_val
real	olddark
real	flat
double	epoch
real	darktime
double	ep0[ARB]
real	dark0[ARB]
int	nep
real	thresh[ARB]
short	fix_dqval		# data quality value of fixed pixels
real	rej_val			# pixel value of rejected pixels
int	nout

short	reject_mask
real	a, b, dk, maxd
int	i

short	ors()
#==============================================================================
begin
	# if the data quality is already non-zero, do nothing
	if (mask_val != OK) return

	reject_mask = short(HP_REJECT)

	# A= after and B = before
	if (epoch <= ep0[1]) {
	    a = dark0[1]
	    b = a
	    dk = a
	    nout = nout + 1
	} else if (epoch > ep0[nep]) {
	    a = dark0[nep]
	    b = a
	    dk = a
	    nout = nout + 1
	} else {
	    do i = 1, nep-1 {
		if (epoch > ep0[i] && epoch <= ep0[i+1]) {
		    a = dark0[i+1]
		    b = dark0[i]

		    # check for decontamination
		    if (dark0[i] < DECON) b = dark0[i+1]
		    if (dark0[i+1] < DECON) a = dark0[i]
		    dk = b + (epoch-ep0[i]) * (a-b) / (ep0[i+1]-ep0[i])
		    break
		}
	    }
	}
# for debugging
#call printf("  raw data=%7.2f   raw mask=%4d old dark=%0.2f old flat = %0.2f\n")
#call pargr(pic_val)
#call pargs(mask_val)
#call pargr(olddark)
#call pargr(flat)

	# the rejection case, i.e. if the "before" or "after" dark level 
	# is higher than the "rejection" threshold (thresh[1]), flag the 
	# mask as "unfixable" and keep the pixel value intact
	maxd = max (a, b)
	if (maxd >= thresh[1]) {
	    mask_val = ors (mask_val, reject_mask)
	    if (!IS_INDEFR(rej_val)) pic_val = rej_val
	}

	# if the "before" or "after" dark level is lower than the "fix" 
	# threshold, the data is considered good, and do nothing,
	# if in between thresh[1] and thresh[2], we try to "fix" it
	else if (maxd < thresh[1] && maxd >= thresh[2]) {
	    if (abs(a-b) > thresh[3]) {
		mask_val = ors (mask_val, reject_mask)
	        if (!IS_INDEFR(rej_val)) pic_val = rej_val
	    } else {
		pic_val = pic_val - darktime * (dk - olddark) * flat
	    	mask_val = ors (mask_val, fix_dqval)
	    }
	}
# for debugging
#call printf("fixed data=%7.2f fixed mask=%4d new dark=%0.2f\n")
#call pargr(pic_val)
#call pargs(mask_val)
#call pargr(dk)
#call flush(STDOUT)
end

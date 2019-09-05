include	<imhdr.h>
include	<error.h>
#include	<syserr.h>
include	<mach.h>
include	"gcombine.h"

# T_GCOMBINE - This task combines a list of images into an output image
# and an optional sigma image.  There are many combining options from
# which to choose. DQF files associated with the input images can be
# used to mask defect pixels. The optional output mask image is the result
# of number of rejected data points at a given pixel
#
# CYZhang 23 Mar 94  Based on images.imcombine
#
# I.Busko 04 Apr 97  Added support for an output list of rejected pixel maps

procedure t_gcombine()

pointer	sp			# Pointer to a stack
pointer	output			# Pointer to output combined image name
pointer	input			# Pointer to input image name
pointer	outmsk			# Pointer to output mask file
pointer	sigma			# Pointer to output sigma file
pointer	logfile			# Pointer to log file
pointer	szuw			# Pointer to scale factors structure
pointer	nm			# Pointer to noise model structure
pointer	ilist			# Pointer to a list of input images
pointer	msklist			# Pointer to a list of input DQF images
pointer	nslist			# Pointer to a list of input noise maps
pointer	rejlist			# Pointer to a list of rejection maps
bool	mskflag			# Use DQF as masks?
bool	nsflag			# Use ERROR maps for weights?
bool	rejflag			# Write individual rejection images?
int	msknum			# Number of input DQF files
int	nsnum			# Number of input ERROR maps
int	nimages			# Number of input images
int	rejnum			# Number of output rejection maps

bool	clgetb(), streq()
real	clgetr()
int	clgwrd(), clgeti(), imtopenp(), imtlen()

include	"gcombine.com"

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (outmsk, SZ_FNAME, TY_CHAR)	
	call salloc (sigma, SZ_FNAME, TY_CHAR)
	call salloc (logfile, SZ_FNAME, TY_CHAR)

	call malloc (szuw, LEN_SZUW, TY_STRUCT)
	call malloc (nm,   LEN_NM,   TY_STRUCT)
	
	# Get task parameters. Some additional parameters are obtained later.
	ilist = imtopenp ("input")
	nslist = NULL
	msklist = NULL
	msklist = imtopenp ("masks")
	rejlist = imtopenp ("rej_list")

	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("rej_cnt", Memc[outmsk], SZ_FNAME)
	call clgstr ("out_err", Memc[sigma], SZ_FNAME)

	call clgstr ("logfile", Memc[logfile], SZ_FNAME)

	nsflag = false
	mskflag = false
	rejflag = false

	G_COMBINE = clgwrd ("combine", Memc[input], SZ_FNAME, COMBINE)
	switch (G_COMBINE) {
	case C_AVERAGE:
	    call clgstr ("weight", Memc[input], SZ_FNAME)
	    if (streq (Memc[input], "pixelwise")) {
		G_WEIGHT = W_PIXWISE
		DOWTS = true
		nslist = imtopenp ("errmap")
		nsnum = imtlen (nslist)
		if (nsnum <= 0)
		    call error (0, "Error maps needed for pixelwise weight!")
		nsflag = true
	    } else if (streq (Memc[input], "none")) {
		G_WEIGHT = W_NONE
		DOWTS = false
	    } else {
		G_WEIGHT = W_UNIFORM
		NSMOD_W = clgetb ("nsmod_w")
		DOWTS = true
	    }
	case C_MEDIAN:
	    DOWTS = false
#	case C_SUM:
#	    DOWTS = false
	}

	NSMOD_E = clgetb ("nsmod_e")
	G_REJECT = clgwrd ("reject", Memc[input], SZ_FNAME, REJECT)
	if (G_REJECT == CCDCLIP || G_REJECT == CCDCRREJ)
	    REJ_NSMOD = true
	if (G_REJECT == ERRCLIP || G_REJECT == ERRCRREJ) {
	    nslist = imtopenp ("errmap")
	    nsnum = imtlen (nslist)
	    if (nsnum <= 0)
		call error (0, "Error maps needed for rejection!")
	    nsflag = true
	}
	
	BLANK = clgetr ("blank")
	if (BLANK == INDEFR)
	    BLANK = 0.0

	FLOW = clgetr ("nlow")
	FHIGH = clgetr ("nhigh")
	LTHRESH = clgetr ("lthreshold")
	HTHRESH = clgetr ("hthreshold")
	LSIGMA = clgetr ("lsigma")
	HSIGMA = clgetr ("hsigma")
	NKEEP = clgeti ("nkeep")
#	grow = clgeti ("grow")
	MCLIP = clgetb ("mclip")
#	sigscale = clgetr ("sigscale")

	# Check parameters, map INDEFs, and set threshold flag
	if (IS_INDEFR (FLOW))
	    FLOW = 0
	if (IS_INDEFR (FHIGH))
	    FHIGH = 0
	if (IS_INDEFR (LSIGMA))
	    LSIGMA = MAX_REAL
	if (IS_INDEFR (HSIGMA))
	    HSIGMA = MAX_REAL

	if (G_REJECT == CCDCRREJ || G_REJECT == RSIGCRREJ ||
	    G_REJECT == AVSIGCRREJ || G_REJECT == ERRCRREJ)
	    LSIGMA = MAX_REAL
	
	if (IS_INDEF(LTHRESH) && IS_INDEF(HTHRESH))
	    DOTHRESH = false
	else {
	    DOTHRESH = true
	    if (IS_INDEF(LTHRESH))
		LTHRESH = -MAX_REAL
	    if (IS_INDEF(HTHRESH))
		HTHRESH = MAX_REAL
	}

	nimages = imtlen (ilist)
	if (nimages < 2) {
	    call imtclose (ilist)
	    call sfree (sp)
	    call error (0, "Too few input images to combine")
	}
	if (G_REJECT == RSIGCLIP || G_REJECT == RSIGCRREJ ||
	    G_REJECT == AVSIGCLIP ||  G_REJECT == AVSIGCRREJ) {
	    if (nimages < 2) {
		call imtclose (ilist)
		call sfree (sp)
		call error (0, "Too few images for this rejection option") 
	    }
	}
	
	# Allocate memory to scaling and noisemodel parameters
	call malloc (SCALES(szuw), nimages, TY_REAL)
	call malloc (ZEROS(szuw), nimages, TY_REAL)
	call malloc (UWTS(szuw), nimages, TY_REAL)
	call malloc (NCOMB(szuw), nimages, TY_INT)
	call malloc (RDNOISE(nm), nimages, TY_REAL)
	call malloc (GAIN(nm), nimages, TY_REAL)
	call malloc (SNOISE(nm), nimages, TY_REAL)

	# Setting mask flag to see if masking would be applied
	msknum = imtlen (msklist)
	if (msknum > 0) 
	    mskflag = true

	# Set rejection maps flag to see if rejection maps will be written
	rejnum = imtlen (rejlist)
	if (rejnum > 0) 
	    rejflag = true

	# Check if the number of images is equal to number of masks
	if (mskflag) {
	    if (nimages != msknum)
		call error (0, "Mask needed for each image!")
	}

	# Check if the number of images is equal to number of error maps
	if (nsflag) {
	    if (nimages != nsnum)
		call error (0, "Error map needed for each image!")
	}

	# Check if the number of images is equal to number of rejection maps
	if (rejflag) {
	    if (nimages != rejnum)
		call error (0, "Output rejection map needed for each image!")
	}

	call g_gcombine (ilist, mskflag, msklist, nsflag, nslist, rejflag, 
                         rejlist, Memc[output], Memc[outmsk], 
                         Memc[sigma], Memc[logfile], nimages, szuw, nm)

	call imtclose (ilist)
	if (mskflag)
	    call imtclose (msklist)
	if (nsflag)
	    call imtclose (nslist)
	if (rejflag)
	    call imtclose (rejlist)
	# Free memory 
	call mfree (SCALES(szuw), TY_REAL)
	call mfree (ZEROS(szuw), TY_REAL)
	call mfree (UWTS(szuw),  TY_REAL)
	call mfree (NCOMB(szuw), TY_INT)
	call mfree (RDNOISE(nm), TY_REAL)
	call mfree (GAIN(nm), TY_REAL)
	call mfree (SNOISE(nm), TY_REAL)
	
	call mfree (szuw, TY_STRUCT)
	call mfree (nm, TY_STRUCT)
	if (NSMOD_W || NSMOD_E || REJ_NSMOD) {
	    call mfree (rdns, TY_CHAR)
	    call mfree (gns, TY_CHAR)
	    call mfree (sns, TY_CHAR)
	}
	
	call sfree (sp)

end


# G_GCOMBINE -- Combine input list or image.
# This procedure maps the images, sets the output dimensions and datatype,
# opens the logfile, and sets IMIO parameters.
#
# CYZhang 1 May, 1994
#
# I.Busko 04 Apr 97  Added support for an output list of rejected pixel maps

procedure g_gcombine (list, mskflag, msklist, nsflag, nslist, rejflag,
                      rejlist, output, outmsk, sigma, logfile, 
                      nimages, szuw, nm)

# Calling arguments
pointer	list			# List of input images
bool	mskflag			# Use DQF files as masks?
pointer	msklist			# List of input DQF files
bool	nsflag			# Use ERROR maps for weighting?
pointer	nslist			# List of input ERROR maps
bool	rejflag			# Write individual rejection images?
pointer	rejlist			# Pointer to a list of rejection maps
char	output[ARB]		# Output image
char	outmsk[ARB]		# Output mask file
char	sigma[ARB]		# Sigma image (optional)
char	logfile[ARB]		# Logfile (optional)
int	nimages			# Number of input images
pointer	szuw, nm

# Local variables
pointer	in, msk, err, rej	# Pointers to arrays of pointers
pointer	data, mskdata, errdata  # Pointers to image lines
int	i, j, intype, outtype, npts
#char	fullname[SZ_FNAME]
pointer	sp, out[3]
int	group_range[3*MAX_RANGES]	# Decoded group range list
int	nitem				# Total number of items in group range
real 	dmin, dmax
char	glist[SZ_LINE]		# List of group range
#bool	g_accum			# Accumulating all groups of all images?
int	cl_index, ngroups, gn, out_gn
char	text[SZ_LINE]
pointer	fname

# Function used
char	clgetc()
long	clktime()
int	open(), decode_ranges(), get_next_number(), ty_max()
pointer	gc_output()
bool	isblank()

include	"gcombine.com"

begin
	call smark (sp)
	call salloc (in, nimages, TY_POINTER)
	call salloc (msk, nimages, TY_POINTER)
	call salloc (err, nimages, TY_POINTER)
	call salloc (rej, nimages, TY_POINTER)
	call salloc (fname, SZ_LINE, TY_CHAR)
	call salloc (data, nimages, TY_POINTER)
	call salloc (mskdata, nimages, TY_POINTER)
	call salloc (errdata, nimages, TY_POINTER)

	# Map input images, masks, error and rejection maps, and do
	# consistency checks

	call gc_init (list, mskflag, msklist, nsflag, nslist, rejflag, rejlist,
		       Memi[in], Memi[msk], Memi[err], Memi[rej],
		       cl_index, ngroups, nimages, intype, szuw, nm)

	if (G_REJECT == MINMAX) {
	    if (FLOW >= 1)
		FLOW = FLOW / nimages
	    if (FHIGH >= 1)
		FHIGH = FHIGH / nimages
	    i = FLOW * nimages
	    j = FHIGH * nimages
	    if (i + j == 0)
		G_REJECT = NONE
	    else if (i + j >= nimages) {
		call eprintf ("Bad minmax rejection parameters\n")
		do i = 1, nimages 
		    call imunmap (Memi[in+i-1])
		call sfree (sp)
		return
	    }
	}

	if (Memi[msk] != NULL && mskflag)
	    DOMASK = true

	if (Memi[rej] != NULL && rejflag)
	    DOREJ = true
	else
	    DOREJ = false

	# Get noise model parameters

	if (NSMOD_W || NSMOD_E || REJ_NSMOD)
	    call gc_nsmodel (Memi[in], nimages, nm)

	# Map the output image and set out pixel datatype

	call clgstr ("groups", glist, SZ_LINE)
	call xt_stripwhite (glist)
	if (glist[1] == EOS)
	    call strcpy ("*", glist, SZ_LINE)
	
	if (cl_index > 0)
	    nitem = 1
	if (cl_index == 0 && glist[1] == '*')
	    nitem = ngroups
	if (cl_index == 0 && glist[1] != '*')
	    if (decode_ranges(glist, group_range, MAX_RANGES, nitem) == ERR)
		call error (0, "Illegal group range!")
	
	out[1] = gc_output (output, Memi[in], nitem)

	npts = IM_LEN (out[1], 1)

	# Determine the highest precedence datatype and set output datatype.
	outtype = clgetc ("outtype")
	switch (outtype) {
	case 's':
	    IM_PIXTYPE(out[1]) = TY_SHORT
	case 'i':
	    IM_PIXTYPE(out[1]) = TY_INT
	case 'l':
	    IM_PIXTYPE(out[1]) = TY_INT
	case 'r':
	    IM_PIXTYPE(out[1]) = TY_REAL
	case 'd':
	    IM_PIXTYPE(out[1]) = TY_DOUBLE
	default:
	    IM_PIXTYPE(out[1]) = intype
	}

	# Open output mask file if given.
	if (!isblank(outmsk)) {
	    out[2] = gc_output (outmsk, out[1], nitem)
	    IM_PIXTYPE(out[2]) = TY_INT
	} else
	    out[2] = NULL

	# Open the sigma image if given.
	if (!isblank(sigma)) {
	    out[3] = gc_output (sigma, out[1], nitem)
	    IM_PIXTYPE(out[3]) = ty_max (TY_REAL, IM_PIXTYPE(out[1]))
	    call sprintf (IM_TITLE(out[3]), SZ_IMTITLE,
		"Combine sigma image for %s")
		call pargstr (output)
	} else
	    out[3] = NULL

	# Open the log file.
	LOGFD = NULL
	if (!isblank(logfile)) {
	    iferr (LOGFD = open (logfile, APPEND, TEXT_FILE)) {
	        LOGFD = NULL
	        call erract (EA_WARN)
	    }
	}

	if (cl_index > 0 && ngroups >= 1) {
	    gn = cl_index
	    if (gn > ngroups)
		call error (0, "Group number out of range!")
	    if (gn > 1) {
		do i = 1, nimages {
		    call gf_opengr (Memi[in+i-1], gn, dmin, dmax, 0)
		    if (Memi[msk] != NULL)
			call gf_opengr (Memi[msk+i-1], gn, dmin, dmax, 0)
		    if (Memi[err] != NULL)
			call gf_opengr (Memi[err+i-1], gn, dmin, dmax, 0)
		    if (Memi[rej] != NULL)
			call gf_opengr (Memi[rej+i-1], gn, dmin, dmax, 0)
		}
		call gf_opengr (out[1], gn, dmin, dmax, Memi[in])
		if (out[2] != NULL)
		    call gf_opengr (out[2], gn, dmin, dmax, out[1])
		if (out[3] != NULL)
		    call gf_opengr (out[3], gn, dmin, dmax, out[1])
	    }
	    switch (intype) {
	    case TY_SHORT:
		call gcombines (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                nimages, Memi[data], Memi[mskdata],
                                Memi[errdata], 
				npts, out, szuw, nm)
	    case TY_INT, TY_LONG:
		call gcombinei (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                nimages, Memi[data], Memi[mskdata],
                                Memi[errdata], 
				npts, out, szuw, nm)
	    case TY_DOUBLE:
		call gcombined (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                nimages, Memi[data], Memi[mskdata],
                                Memi[errdata], 
				npts, out, szuw, nm)
	    case TY_COMPLEX:
		call error (1, "Complex images not allowed")
	    default:
		call gcombiner (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                nimages, Memi[data], Memi[mskdata],
                                Memi[errdata], 
				npts, out, szuw, nm)
	    }
	}
	if (cl_index == 0 && glist[1] == '*' && ngroups >= 1) {
	    do gn = 1, ngroups {
		if (gn > 1) {
		    do i = 1, nimages {
			call gf_opengr (Memi[in+i-1], gn, dmin, dmax, 0)
			if (Memi[msk] != NULL)
			    call gf_opengr (Memi[msk+i-1], gn, dmin, dmax, 0)
			if (Memi[err] != NULL)
			    call gf_opengr (Memi[err+i-1], gn, dmin, dmax, 0)
		        if (Memi[rej] != NULL)
			    call gf_opengr (Memi[rej+i-1], gn, dmin, dmax, 0)
		    }
		    call gf_opengr (out[1], gn, dmin, dmax, Memi[in])
		    if (out[2] != NULL)
			call gf_opengr (out[2], gn, dmin, dmax, out[1])
		    if (out[3] != NULL)
			call gf_opengr (out[3], gn, dmin, dmax, out[1])
		}
		switch (intype) {
		case TY_SHORT:
		    call gcombines (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                    nimages, Memi[data], Memi[mskdata],
                                    Memi[errdata], 
                                    npts, out, szuw, nm)
		case TY_INT, TY_LONG:
		    call gcombinei (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                    nimages, Memi[data], Memi[mskdata],
                                    Memi[errdata], 
				    npts, out, szuw, nm)
		case TY_DOUBLE:
		    call gcombined (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                    nimages, Memi[data], Memi[mskdata],
                                    Memi[errdata], 
				    npts, out, szuw, nm)
		case TY_COMPLEX:
		    call error (1, "Complex images not allowed")
		default:
		    call gcombiner (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                    nimages, Memi[data], Memi[mskdata],
                                    Memi[errdata], 
				    npts, out, szuw, nm)
		}
	    }
	}
	if (cl_index == 0 && glist[1] != '*' && ngroups >= 1) {
	    gn = 0; out_gn = 1
	    while (get_next_number(group_range, gn) != EOF) {
		if (gn > ngroups)
		    break
		if (gn > 1) {
		    do i = 1, nimages {
			call gf_opengr (Memi[in+i-1], gn, dmin, dmax, 0)
			if (Memi[msk] != NULL)
			    call gf_opengr (Memi[msk+i-1], gn, dmin, dmax, 0)
			if (Memi[err] != NULL)
			    call gf_opengr (Memi[err+i-1], gn, dmin, dmax, 0)
		        if (Memi[rej] != NULL)
			    call gf_opengr (Memi[rej+i-1], gn, dmin, dmax, 0)
		    }
		}
		call gf_opengr (out[1], out_gn, dmin, dmax, Memi[in])
		if (out[2] != NULL)
		    call gf_opengr (out[2], out_gn, dmin, dmax, out[1])
		if (out[3] != NULL)
		    call gf_opengr (out[3], out_gn, dmin, dmax, out[1])
		
		switch (intype) {
		case TY_SHORT:
		    call gcombines (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                    nimages, Memi[data], Memi[mskdata],
                                    Memi[errdata], 
                                    npts, out, szuw, nm)
		case TY_INT, TY_LONG:
		    call gcombinei (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                    nimages, Memi[data], Memi[mskdata],
                                    Memi[errdata], 
                                    npts, out, szuw, nm)
		case TY_DOUBLE:
		    call gcombined (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                    nimages, Memi[data], Memi[mskdata],
                                    Memi[errdata], 
                                    npts, out, szuw, nm)
		case TY_COMPLEX:
		    call error (1, "Complex images not allowed")
		default:
		    call gcombiner (Memi[in], Memi[msk], Memi[err], Memi[rej], 
                                    nimages, Memi[data], Memi[mskdata],
                                    Memi[errdata], 
                                    npts, out, szuw, nm)
		}
		out_gn = out_gn + 1
	    }
	}

	call fprintf (LOGFD, "\n")
	call cnvdate (clktime(0), Memc[fname], SZ_LINE)
	call sprintf (text, SZ_LINE, "Ending GCOMBINE at %s")
	call pargstr (Memc[fname])
	call putline (LOGFD, text)
	call putline (LOGFD, "\n")
	call imputh (out[1], "HISTORY", text)

	# Unmap all the images, close the log file, and restore memory.
	# The input images must be unmapped first to insure that there
	# is a FD for the output images since the headers are opened to
	# update them.  However, the order of the NEW_COPY pointers must
	# be preserved.

	do i = 2, nimages
	    call imunmap (Memi[in+i-1])
	do i = 1, nimages {
	    if (Memi[msk+i-1] != NULL)
		call imunmap (Memi[msk+i-1])
	    if (Memi[err+i-1] != NULL)
		call imunmap (Memi[err+i-1])
	    if (Memi[rej+i-1] != NULL)
		call imunmap (Memi[rej+i-1])
	}
	if (out[2] != NULL)
	    call imunmap (out[2])
	if (out[3] != NULL)
	    call imunmap (out[3])
	if (out[1] != NULL)
	    call imunmap (out[1])
	call imunmap (Memi[in])
	
	if (LOGFD != NULL)
	    call close (LOGFD)
	call sfree (sp)

end

# GC_INIT -- Map input images, masks, and errormaps, Do consistency checks
#  on them.
#
# CYZhang 1 May, 94
#
# I.Busko 04 Apr 97  Added support for an output list of rejected pixel maps

procedure gc_init (list, mskflag, msklist, nsflag, nslist, rejflag, rejlist,
                   in, msk, err, rej,
		   cl_index, ngroups, nimages, intype, szuw, nm)

# Calling arguments
pointer	list			# List of input images
bool	mskflag			# Use DQF files as masks?
pointer	msklist			# List of input DQF files
bool	nsflag			# Use ERROR maps for weighting?
pointer	nslist			# List of input ERROR maps
bool	rejflag			# Write individual rejection images?
pointer	rejlist			# List of rejection maps
pointer	in[nimages]		# Pointers to input images
pointer	msk[nimages]		# Pointers to masks
pointer	err[nimages]		# Pointers to error maps
pointer	rej[nimages]		# Pointers to rejection maps
int	nimages			# Number of input images
int	cl_index, ngroups
int	intype
pointer	szuw, nm

# Local variables
pointer	sp
pointer	input			# Pointer to current input image name
pointer	mskfile			# Pointer to current input DQF mask file
pointer	nsfile			# Pointer to current input noise map
pointer	rejfile			# Pointer to current rejection map
int	i, j
int	ndim, mskndim, msknum, rejnum, ngrmsk
int	nsndim, nsnum, ngrns, first, nxt, nextgrp
int	cl_size, mskclind, mskclsize, nsclind, nsclsize 
int	cl_index1, cl_size1, msk1clind, msk1clsize, ns1clind, ns1clsize
int	rejclind, rejclsize, rej1clind, rej1clsize
char	cluster[SZ_FNAME], section[SZ_FNAME],section1[SZ_FNAME]
char	ksec[SZ_FNAME]
char	mskclst[SZ_FNAME], msksect[SZ_FNAME]
char	nsclst[SZ_FNAME], nssect[SZ_FNAME]
char	rejclst[SZ_FNAME], rejsect[SZ_FNAME]
char	gcount[SZ_FNAME]

# Function used
int	imtgetim(), imgeti(), imaccf(), ty_max()
#pointer	gc_input()
pointer	immap()
bool	streq(), isblank()

include	"gcombine.com"

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (mskfile, SZ_FNAME, TY_CHAR)
	call salloc (nsfile, SZ_FNAME, TY_CHAR)
	call salloc (rejfile, SZ_FNAME, TY_CHAR)

	call strcpy ("GCOUNT", gcount, SZ_FNAME)
	call imtrew (list)
	if (mskflag)
	    call imtrew (msklist)
	if (nsflag)
	    call imtrew (nslist)
	if (rejflag)
	    call imtrew (rejlist)

	i = 1
	while (imtgetim (list, Memc[input], SZ_FNAME)!=EOF) {

	    # Parse the input file name into cluster, cl_index, cl_size

	    call myparse (Memc[input], cluster, SZ_PATHNAME, ksec, SZ_FNAME,
			  section, SZ_FNAME, cl_index, cl_size)

	    if (i > 1) {
		if (cl_index != cl_index1)
		    call error (0, "Mixed group specifications!")
		if (cl_size != cl_size1)
		    call error (0, "Number of groups must be the same!")
		if ( !(streq (section, section1)) )
		    call error (0, "Mixed section specifications!")
	    }
#	    in[i] = gc_input (Memc[input])
	    in[i] = immap (Memc[input], READ_ONLY, 0)
	    ndim = IM_NDIM(in[i])
	    if (imaccf (in[i], gcount) == YES)
		ngroups = imgeti (in[i], gcount)	    
	    else
		ngroups = 1
	    
	    if (mskflag) {
		msknum = imtgetim (msklist, Memc[mskfile], SZ_FNAME)
		if (isblank(Memc[mskfile]) || msknum == EOF)
		    call error (1, "Number of masks & images must match!")
		call myparse (Memc[mskfile], mskclst, SZ_PATHNAME, ksec,
			      SZ_FNAME, msksect, SZ_FNAME, mskclind, mskclsize)
		if (i > 1) {
		    if (mskclind != msk1clind)
			call error (0, "Mixed group specifications for masks")
		    if (mskclsize != msk1clsize)
			call error (0, "Number of groups must be the same for masks")
		}
		
		if ( !(streq (msksect, section)) )
		    call error (0, "Mask & image section must match!")
		if (mskclind != cl_index)
		    call error (0, "Group No of masks & images must be the same!")
		if (mskclsize != cl_size)
		    call error (0, "No of groups for masks & images must be the same!")

#		msk[i] = gc_input (Memc[mskfile])
		msk[i] = immap (Memc[mskfile], READ_ONLY, 0)
		mskndim = IM_NDIM(msk[i])
		if (ndim != mskndim)
		    call error (0, "Image & mask dimensions must match!")

		do j = 1, ndim {
		    if (IM_LEN(in[i], j) != IM_LEN(msk[i], j))
			call error (0, "Image & mask sizes must match!")
		}
		if (imaccf (msk[i], gcount) == YES)
		    ngrmsk = imgeti (msk[i], gcount)
		else
		    ngrmsk = 1
		if (ngrmsk != ngroups)
		    call error (0, "Image & mask group numbers must match!")
	    } else {
		msk[i] = NULL
	    }
	    
	    if (rejflag) {
		rejnum = imtgetim (rejlist, Memc[rejfile], SZ_FNAME)
		if (isblank(Memc[rejfile]) || rejnum == EOF)
		    call error (1, 
                    "Number of rejection maps & images must match!")
		call myparse (Memc[rejfile], rejclst, SZ_PATHNAME, ksec,
			      SZ_FNAME, rejsect, SZ_FNAME, rejclind, rejclsize)
		if (i > 1) {
		    if (rejclind != rej1clind)
			call error (0, 
                        "Mixed group specifications for rejection maps")
		    if (rejclsize != rej1clsize)
			call error (0, 
                        "Number of groups must be the same for rejection maps")
		}
		if ( !(streq (rejsect, section)) )
		    call error (0, "Rejection map & image section must match!")
		if (rejclind != cl_index)
		    call error (0, 
                    "Group No of rejection maps & images must be the same!")
		if (rejclsize != cl_size)
		    call error (0, 
                "No of groups for rejection maps & images must be the same!")

		rej[i] = immap (Memc[rejfile], NEW_COPY, in[i])
	        IM_PIXTYPE(rej[i]) = TY_SHORT

#		if (imaccf (rej[i], gcount) == YES)
#		    ngrmsk = imgeti (msk[i], gcount)
#		else
#		    ngrmsk = 1
#		if (ngrmsk != ngroups)
#		    call error (0, "Image & mask group numbers must match!")
	    } else {
		rej[i] = NULL
	    }

	    if (nsflag) {
		nsnum = imtgetim (nslist, Memc[nsfile], SZ_FNAME)
		if (isblank(Memc[nsfile])|| nsnum == EOF)
		    call error (1, "Number of error maps & images must match!")
		call myparse (Memc[nsfile], nsclst, SZ_PATHNAME, ksec,
			      SZ_FNAME, nssect, SZ_FNAME, nsclind, nsclsize)
		if (i > 1) {
		    if (nsclind != ns1clind)
			call error (0, "Mixed group specifications in error maps!")
		    if (nsclsize != ns1clsize)
			call error (0, "Number of groups must be the same for error maps")
		}
		
		if ( !(streq (nssect, section)) )
		    call error (0, "Error maps & image section must match!")
		if (nsclind != cl_index)
		    call error (0, "Group No of error maps & images must be the same!")
		if (nsclsize != cl_size)
		    call error (0, "Number of groups for error maps & images must be the same!")

#		err[i] = gc_input (Memc[nsfile])
		err[i] = immap (Memc[nsfile], READ_ONLY, 0)
		nsndim = IM_NDIM(err[i])
		if (ndim != nsndim)
		    call error (0, "Image & error map dimensions must match!")

		do j = 1, ndim {
		    if (IM_LEN(in[i], j) != IM_LEN(err[i], j))
			call error (0, "Image & error map sizes must match!")
		}
		if (imaccf (err[i], gcount) == YES)
		    ngrns = imgeti (err[i], gcount)
		else
		    ngrns = 1
		if (ngrns != ngroups)
		    call error (0, "Image & error map group numbers must match!")
	    } else {
		err[i] = NULL
	    }
	    cl_index1 = cl_index
	    cl_size1 = cl_size
	    msk1clind = mskclind
	    msk1clsize = mskclsize
	    rej1clind = rejclind
	    rej1clsize = rejclsize
	    ns1clind = nsclind
	    ns1clsize = nsclsize
	    call strcpy (section, section1, SZ_FNAME)
	    i = i + 1
	}

	# Check if all images have the same number of groups
	first = in[1]
	intype = IM_PIXTYPE (first)
	iferr (ngroups = imgeti(first, gcount)) {
	    ngroups = 1
	}
	do i = 2, nimages {
	    nxt = in[i]
	    nextgrp = imgeti (nxt, gcount)
	    if (nextgrp != ngroups)
		call error (0, "No. of image groups not the same!")

	    # Check if all the images are of the same dimension and size
	    if (IM_NDIM(nxt) != IM_NDIM(first))
		call error (0, "Image dimensions are not the same!")
	    do j = 1, IM_NDIM(first)
		if (IM_LEN(nxt,j) != IM_LEN(first,j))
		    call error (0, "Image sizes are not the same!")
	    intype = ty_max (intype, IM_PIXTYPE(nxt))
	}
	call sfree (sp)
	
end

# TY_MAX -- Return the datatype of highest precedence.
#  See images.imcombine

int procedure ty_max (type1, type2)

int	type1, type2		# Datatypes

int	i, j, order[8]
data	order/TY_SHORT,TY_USHORT,TY_INT,TY_LONG,TY_REAL,TY_DOUBLE,TY_COMPLEX,TY_REAL/

begin
	for (i=1; (i<=7) && (type1!=order[i]); i=i+1)
	    ;
	for (j=1; (j<=7) && (type2!=order[j]); j=j+1)
	    ;
	return (order[max(i,j)])
end

#  GC_INPUT --	Map images as input
#					
# CYZhang 1 May 94 

pointer procedure gc_input (name)

#  Calling arguments:
char	name[SZ_FNAME]		# name of input image

#  Local variables:
#char	fullname[SZ_FNAME]	# full filename 
pointer	im			# input image descriptor (returned)
char	text[SZ_LINE]		# text of error message

#  Functions used:
pointer	immap()
#int	imaccess()

begin
	iferr (im = immap (name, READ_ONLY, 0)) {
	    call sprintf (text, SZ_LINE, "File %s does not exist" )
	    call pargstr (name)
	    call error (EA_ERROR, text)
	}
	return (im)
end

#  GC_OUTPUT --	Open output image.     
#
# CYZhang 1 May, 94

pointer procedure gc_output (name, template, gcnt)

#  Calling arguments:
char	name[SZ_FNAME]		# name of input image
pointer	template		# image template (usually the input image)
int	gcnt			# number of groups in output image

#  Local variables:
char	fullname[SZ_FNAME]	# full filename 
pointer	im			# output image descriptor (returned)
char	text[SZ_LINE]		# text of error message

#  Functions used:
pointer	immap()			# map image descriptor

begin

	call strcpy (name, fullname, SZ_FNAME)
	call sprintf (text, SZ_FNAME, "[1/%d]")
	call pargi (gcnt)
	call strcat (text, fullname, SZ_FNAME)

	#  Create the output image descriptor
	im = immap (fullname, NEW_COPY, template)
	return (im)
end

# GC_NSMODEL -- Get noise model parameters
#
# CYZhang 1 May, 94	
	
procedure gc_nsmodel (in, nimages, nm)

# Calling arguments
pointer	nm			# Pointer to noise model structure
pointer	in[nimages]		# Pointer to input iamges
int	nimages			# Number of images

# Local variables
int	i, ctor()
real	r, imgetr()

include	"gcombine.com"

begin
	# These pointers are passed to the common block for later use in g_log
	call malloc (rdns, SZ_FNAME, TY_CHAR)	
	call malloc (gns, SZ_FNAME, TY_CHAR)
	call malloc (sns, SZ_FNAME, TY_CHAR)
	
	# Get noise model parameters
	call clgstr ("rdnoise", Memc[rdns], SZ_FNAME) 	#in units of e-
	call clgstr ("gain", Memc[gns], SZ_FNAME)	#in e-/DN
	call clgstr ("snoise", Memc[sns], SZ_FNAME) 	#in fraction
	
	i = 1
	if (ctor (Memc[rdns], i, r) > 0) {
	    do i = 1, nimages
		Memr[RDNOISE(nm)+i-1] = r
	} else {
	    do i = 1, nimages
		Memr[RDNOISE(nm)+i-1] = imgetr (in[i], Memc[rdns])
	}
	i = 1
	if (ctor (Memc[gns], i, r) > 0) {
	    do i = 1, nimages {
		Memr[GAIN(nm)+i-1] = r
    		Memr[RDNOISE(nm)+i-1] =	(Memr[RDNOISE(nm)+i-1] / r) ** 2
	    }
	} else {
	    do i = 1, nimages {
		r = imgetr (in[i], Memc[gns])
		Memr[GAIN(nm)+i-1] = r
		Memr[RDNOISE(nm)+i-1] =	(Memr[RDNOISE(nm)+i-1] / r) ** 2
	    }
	}

	i = 1
	if (ctor (Memc[sns], i, r) > 0) {
	    do i = 1, nimages
		Memr[SNOISE(nm)+i-1] = r
	} else {
	    do i = 1, nimages
		Memr[SNOISE(nm)+i-1] = imgetr (in[i], Memc[sns])
	}

end

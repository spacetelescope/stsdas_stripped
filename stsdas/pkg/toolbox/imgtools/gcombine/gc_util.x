include	<imhdr.h>
include <mach.h>
include	<ctype.h>
include	<syserr.h>
include <imset.h>

include "gcombine.h"

# G_LOG1 -- Output log information, split into two 
#
# CYZhang Apr 4, 94

procedure g_log1 (in, out, sname, zname, uwname, nimages)

# Calling arguments
pointer	in[nimages]		# Input images
pointer	out[3]			# Output images
char	sname[ARB], zname[ARB], uwname[ARB] # Names of scaling factors
int	nimages

# Local variables and functions used
long	clktime()
pointer	sp, fname
char	text[SZ_LINE], nline[SZ_FNAME], histitle[SZ_FNAME]
bool	isblank()

include	"gcombine.com"

begin

	if (LOGFD == NULL)
	    return
	call smark (sp)
	call salloc (fname, SZ_LINE, TY_CHAR)

	call strcpy ("HISTORY", histitle, SZ_FNAME)
	call strcpy ("\n", nline, SZ_FNAME)

	# Time stamp the log
	call cnvdate (clktime(0), Memc[fname], SZ_LINE)
	call sprintf (text, SZ_LINE, "Task GCOMBINE Version %s at %s")
	call pargstr (VERSION)
	call pargstr (Memc[fname])
	call putline (LOGFD, text)
	call putline (LOGFD, nline)
	call putline (LOGFD, nline)
	call imputh (out[1], histitle, text)
	call sprintf (text, SZ_LINE, "  scale = %s, zero = %s")
	call pargstr (sname)
	call pargstr (zname)
	call putline (LOGFD, text)
	call putline (LOGFD, nline)
	call imputh (out[1], histitle, text)
	
	switch (G_COMBINE) {
	case C_AVERAGE:
	    call sprintf (text, SZ_LINE, "  combine = average,  ")
	    call putline (LOGFD, text)
	    call imputh (out[1], histitle, text)
	    if (G_WEIGHT == W_PIXWISE) {
		call sprintf (text, SZ_LINE, "  weight scheme = pixelwise")
		call putline (LOGFD, text)
		call putline (LOGFD, nline)
		call imputh (out[1], histitle, text)
	    }
	    
	    if (G_WEIGHT == W_UNIFORM) {
		call sprintf (text, SZ_LINE, "  weight scheme = uniform,  ")
		call putline (LOGFD, text)
		call putline (LOGFD, nline)
		call imputh (out[1], histitle, text)
		if (NSMOD_W) {
		    call sprintf (text, SZ_LINE, "  uniform weight = %s, with noise model")
		    call pargstr (uwname)
		    call putline (LOGFD, text)
		    call putline (LOGFD, nline)
		    call imputh (out[1], histitle, text)
		} else {
		    call sprintf (text, SZ_LINE, "  uniform weight = %s")
		    call pargstr (uwname)
		    call putline (LOGFD, text)
		    call putline (LOGFD, nline)
		    call imputh (out[1], histitle, text)
		}
	    }
	    if (G_WEIGHT == W_NONE) {
		call sprintf (text, SZ_LINE, "  no weight applied")
		call putline (LOGFD, text)
		call putline (LOGFD, nline)
		call imputh (out[1], histitle, text)
	    }
	case C_MEDIAN:
	    call sprintf (text, SZ_LINE, "  combine = median")
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	}	
	if (NSMOD_W || NSMOD_E || REJ_NSMOD) {
	    call sprintf (text, SZ_LINE, "  rdnoise = %s (e-), gain = %s (e-/DN), snoise = %s (fraction)")
	    call pargstr (Memc[rdns])
	    call pargstr (Memc[gns])
	    call pargstr (Memc[sns])
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	}
	switch (G_REJECT) {
	case NONE:
	    call sprintf (text, SZ_LINE, "  no rejection is done")
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	case MINMAX:
	    call sprintf (text, SZ_LINE, "  reject = minmax, nlow = %d, nhigh = %d")
	    call pargi (nint (FLOW * nimages))
	    call pargi (nint (FHIGH * nimages))
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	case CCDCLIP:
	    call sprintf (text, SZ_LINE, "  reject = ccdclip, mclip = %b, nkeep = %d")
	    call pargb (MCLIP)
	    call pargi (NKEEP)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	    call sprintf (text,	SZ_LINE, "  lsigma = %g, hsigma = %g")
	    call pargr (LSIGMA)
	    call pargr (HSIGMA)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	case CCDCRREJ:
	    call sprintf (text, SZ_LINE, "  reject = ccdcrrej, mclip = %b, nkeep = %d")
	    call pargb (MCLIP)
	    call pargi (NKEEP)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	    call sprintf (text, SZ_LINE, "  hsigma = %g")
	    call pargr (HSIGMA)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	case RSIGCLIP:
	    call sprintf (text, SZ_LINE, "  reject = rsigclip, mclip = %b, nkeep = %d, lsigma = %g, hsigma = %g")
	    call pargb (MCLIP)
	    call pargi (NKEEP)
	    call pargr (LSIGMA)
	    call pargr (HSIGMA)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	case RSIGCRREJ:
	    call sprintf (text, SZ_LINE, "  reject = rsigcrrej, mclip = %b, nkeep = %d")
	    call pargb (MCLIP)
	    call pargi (NKEEP)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	    call sprintf (text, SZ_LINE, "  hsigma = %g")
	    call pargr (HSIGMA)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	case AVSIGCLIP:
	    call sprintf (text, SZ_LINE, "  reject = avsigclip, mclip = %b, nkeep = %d, lsigma = %g, hsigma = %g")
	    call pargb (MCLIP)
	    call pargi (NKEEP)
	    call pargr (LSIGMA)
	    call pargr (HSIGMA)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	case AVSIGCRREJ:
	    call sprintf (text, SZ_LINE, "  reject = avsigcrrej, mclip = %b, nkeep = %d")
	    call pargb (MCLIP)
	    call pargi (NKEEP)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	    call sprintf (text, SZ_LINE, "  hsigma = %g")
	    call pargr (HSIGMA)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	case ERRCLIP:
	    call sprintf (text, SZ_LINE, "  reject = errclip, mclip = %b, nkeep = %d, lsigma = %g, hsigma = %g")
	    call pargb (MCLIP)
	    call pargi (NKEEP)
	    call pargr (LSIGMA)
	    call pargr (HSIGMA)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	case ERRCRREJ:
	    call sprintf (text, SZ_LINE, "  reject = errcrrej, mclip = %b, nkeep = %d")
	    call pargb (MCLIP)
	    call pargi (NKEEP)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	    call sprintf (text, SZ_LINE, "  hsigma = %g")
	    call pargr (HSIGMA)
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	}
	if (DOTHRESH) {
	    if (LTHRESH > -MAX_REAL && HTHRESH < MAX_REAL) {
		call sprintf (text, SZ_LINE, "  lthreshold = %g, hthreshold = %g")
		call pargr (LTHRESH)
		call pargr (HTHRESH)
		call putline (LOGFD, text)
		call putline (LOGFD, nline)
		call imputh (out[1], histitle, text)
	    } else if (LTHRESH > -MAX_REAL) {
		call sprintf (text, SZ_LINE, "  lthreshold = %g")
		call pargr (LTHRESH)
		call putline (LOGFD, text)
		call putline (LOGFD, nline)
		call imputh (out[1], histitle, text)
	    } else {
		call sprintf (text, SZ_LINE, " hthreshold = %g")
		call pargr (HTHRESH)
		call putline (LOGFD, text)
		call putline (LOGFD, nline)
		call imputh (out[1], histitle, text)
	    }
	}
	call sprintf (text, SZ_LINE, "  blank = %g")
	call pargr (BLANK)
	call putline (LOGFD, text)
	call putline (LOGFD, nline)
	call imputh (out[1], histitle, text)
	call clgstr ("statsec", Memc[fname], SZ_LINE)
	if (!isblank(Memc[fname])) {
	    call sprintf (text, SZ_LINE, "  statsec = %s\n")
	    call pargstr (Memc[fname])
	    call putline (LOGFD, text)
	    call putline (LOGFD, nline)
	    call imputh (out[1], histitle, text)
	}
	call sprintf (text, SZ_LINE, "  Mean Sky Level = %g")
	call pargr (BSKY)
	call putline (LOGFD, text)
	call putline (LOGFD, nline)
	call imputh (out[1], "HISTORY", text)
	
	call sfree (sp)
end

# G_LOG2 -- Output log information
#
# CYZhang Apr 4, 94

procedure g_log2 (in, msk, err, out, exptime, darktime,
		 mode, median, mean, nimages, nout, expname, exposure,
		 darkname, darksecs, szuw, nm)

# Calling arguments
pointer	in[nimages]		# Input images
pointer	msk[nimages]		# Input mask DQF images
pointer	err[nimages]		# Input Error maps
pointer	out[3]			# Output images
real	exptime[nimages]	# Exposure time
real	darktime[nimages]	# Dark time
real	mode[nimages], median[nimages], mean[nimages]
int	nimages
int	nout			# Number of images combined in output
char	expname[ARB]		# Exposure name
char	darkname[ARB]		# Darktime name
real	exposure		# Output exposure
real	darksecs		# Output darksecs
pointer	nm, szuw

# Local variables and functions used
real	asumr(), b, imgetr()
int	i, j, ctor()
real	rval
bool	prncombine, prexptime, prdarktime, prmode, prmedian, prmean
bool	prrdn, prgain, prsn, prmask
pointer	sp, fname, c
char	nline[SZ_FNAME], histitle[SZ_FNAME], s1[SZ_FNAME], s2[SZ_FNAME]
bool	isblank()

include	"gcombine.com"

begin

	call smark (sp)
	call salloc (fname, SZ_LINE, TY_CHAR)

	call strcpy ("HISTORY", histitle, SZ_FNAME)
	call strcpy ("\n", nline, SZ_FNAME)
	call strcpy (" %6s", s1, SZ_FNAME)
	call strcpy (" %6g", s2, SZ_FNAME)

	# Print information pertaining to individual images as a set
	# of columns

	prncombine = false
	prexptime = false
	prdarktime = false
	prmode = false
	prmedian = false
	prmean = false
	prmask = false
	prrdn = false
	prgain = false
	prsn = false
	do i = 1, nimages {
	    if (Memi[NCOMB(szuw)+i-1] != Memi[NCOMB(szuw)])
		prncombine = true
	    if (exptime[i] != exptime[1])
		prexptime = true
	    if (darktime[i] != darktime[1])
		prdarktime = true
	    if (mode[i] != mode[1])
		prmode = true
	    if (median[i] != median[1])
		prmedian = true
	    if (mean[i] != mean[1])
		prmean = true
	    if (DOMASK)
		prmask = true
	    if (NSMOD_W || NSMOD_E || REJ_NSMOD) {
		# if these params not from cl, they must be from header
		# with different values for different images
		j = 1
		if (ctor (Memc[rdns], j, rval) == 0)
		    prrdn = true
		j = 1
		if (ctor (Memc[gns], j, rval) == 0)
		    prgain = true
		j = 1
		if (ctor (Memc[sns], j, rval) == 0)
		    prsn = true
	    }
	}
	call fprintf (LOGFD, nline)
	call fprintf (LOGFD, " %20s ")
	call pargstr ("Images")

	if (prncombine) {
	    call fprintf (LOGFD, s1)
	    call pargstr ("N")
	}
	if (prexptime) {
	    call fprintf (LOGFD, s1)
	    call pargstr ("Exp")
	}
	if (prdarktime) {
	    call fprintf (LOGFD, s1)
	    call pargstr ("Dark")
	}
	if (prmode) {
	    call fprintf (LOGFD, s1)
	    call pargstr ("Mode")
	}
	if (prmedian) {
	    call fprintf (LOGFD, s1)
	    call pargstr ("Median")
	}
	if (prmean) {
	    call fprintf (LOGFD, s1)
	    call pargstr ("Mean")
	}
	if (prrdn) {
	    call fprintf (LOGFD, " %7s")
	    call pargstr ("Rdnoise")
	}
	if (prgain) {
	    call fprintf (LOGFD, s1)
	    call pargstr ("Gain")
	}
	if (prsn) {
	    call fprintf (LOGFD, s1)
	    call pargstr ("Snoise")
	}
	if (DOSCALE) {
	    call fprintf (LOGFD, s1)
	    call pargstr ("Scale")
	    call fprintf (LOGFD, s1)
	    call pargstr ("Zero")
	}
	if (DOWTS && G_WEIGHT == W_UNIFORM) {
	    call fprintf (LOGFD, s1)
	    call pargstr ("Weight")
	}
	if (DOWTS && G_WEIGHT == W_PIXWISE) {
	    call fprintf (LOGFD, " %9s")
	    call pargstr ("Errmap")
	}
	if (prmask) {
	    call fprintf (LOGFD, " %8s")
	    call pargstr ("Masks")
	}
	call fprintf (LOGFD, "\n")
	do i = 1, nimages {
	    call gc_filnam (in[i], Memc[fname])
	    call fprintf (LOGFD, " %21s")
	    call pargstr (Memc[fname])
	    if (prncombine) {
		call fprintf (LOGFD, " %6d")
		call pargi (Memi[NCOMB(szuw)+i-1])
	    }
	    if (prexptime) {
		call fprintf (LOGFD, " %6.1f")
		call pargr (exptime[i])
	    }
	    if (prdarktime) {
		call fprintf (LOGFD, " %6.1f")
		call pargr (darktime[i])
	    }
	    if (prmode) {
		call fprintf (LOGFD, s2)
		call pargr (mode[i])
	    }
	    if (prmedian) {
		call fprintf (LOGFD, s2)
		call pargr (median[i])
	    }
	    if (prmean) {
		call fprintf (LOGFD, s2)
		call pargr (mean[i])
	    }
	    if (prrdn) {
		rval = imgetr (in[i], Memc[rdns])
		call fprintf (LOGFD, " %7g")
		call pargr (rval)
	    }
	    if (prgain) {
		rval = imgetr (in[i], Memc[gns])
		call fprintf (LOGFD, " %7g")
		call pargr (rval)
	    }
	    if (prsn) {
		rval = imgetr (in[i], Memc[sns])
		call fprintf (LOGFD, " %7g")
		call pargr (rval)
	    }
	    if (DOSCALE) {
		# Print in (Input_image - ZEROS) / SCALES
		call fprintf (LOGFD, " %6.3f")
		rval = Memr[SCALES(szuw)+i-1]
		call pargr (rval)
		call fprintf (LOGFD, s2)
		rval = Memr[ZEROS(szuw)+i-1] * rval
		call pargr (rval)
	    }
	    if (DOWTS && G_WEIGHT == W_UNIFORM) {
		call salloc (c, nimages, TY_REAL)
		b = asumr (Memr[UWTS(szuw)], nimages)
		call adivkr (Memr[UWTS(szuw)], b, Memr[c], nimages)
		call fprintf (LOGFD, s2)
		call pargr (Memr[c+i-1])
	    }
	    if (DOWTS && G_WEIGHT == W_PIXWISE) {
		call gc_filnam (err[i], Memc[fname])
		call fprintf (LOGFD, " %9s")
		call pargstr (Memc[fname])
	    }
	    if (prmask) {
		call gc_filnam (msk[i], Memc[fname])
		call fprintf (LOGFD, " %9s")
		call pargstr (Memc[fname])
	    }
	    call fprintf (LOGFD, "\n")
	}
	call fprintf (LOGFD, "\n   -------------------------------------------------------------------\n")
	# Log information about output images
	call gc_filnam (out[1], Memc[fname])
	call fprintf (LOGFD, "  Output image = %s, ncombine = %d")
	call pargstr (Memc[fname])
	call pargi (nout)
	if (!isblank(expname)) {
	    call fprintf (LOGFD, ", %s = %g")
	    call pargstr (expname)
	    call pargr (exposure)
	}
	if (!isblank(darkname)) {
	    call fprintf (LOGFD, ", %s = %g")
	    call pargstr (darkname)
	    call pargr (darksecs)
	}
	if (out[2] != NULL) {
	    call gc_filnam (out[2], Memc[fname])
	    call fprintf (LOGFD, "\n  Output mask image = %s")
	    call pargstr (Memc[fname])
	}
	if (out[3] != NULL) {
	    call gc_filnam (out[3], Memc[fname])
	    call fprintf (LOGFD, "\n  Output sigma image = %s")
	    call pargstr (Memc[fname])
	}
	call fprintf (LOGFD, "\n")
#	call fprintf (LOGFD,
#	    "\n  ====================================================================\n")
	call flush (LOGFD)
	call sfree (sp)

end	

# G_SECTION -- Parse an image section into its elements.
# 1. The default values must be set by the caller.
# 2. A null image section is OK.
# 3. The first nonwhitespace character must be '['.
# 4. The last interpreted character must be ']'.
#
# This procedure should be replaced with an IMIO procedure at some
# point.
# Copied from images.imcombine

procedure g_section (section, x1, x2, xs, ndim)

char	section[ARB]		# Image section
int	x1[ndim]		# Starting pixel
int	x2[ndim]		# Ending pixel
int	xs[ndim]		# Step
int	ndim			# Number of dimensions

int	i, ip, a, b, c, temp, ctoi()
define	error_	99

begin
	# Decode the section string.
	ip = 1
	while (IS_WHITE(section[ip]))
	    ip = ip + 1
	if (section[ip] == '[')
	    ip = ip + 1
	else if (section[ip] == EOS)
	    return
	else
	    goto error_

	do i = 1, ndim {
	    while (IS_WHITE(section[ip]))
	        ip = ip + 1
	    if (section[ip] == ']')
		break

	    # Default values
	    a = x1[i]
	    b = x2[i]
	    c = xs[i]

	    # Get a:b:c.  Allow notation such as "-*:c"
	    # (or even "-:c") where the step is obviously negative.

	    if (ctoi (section, ip, temp) > 0) {			# a
		a = temp
	        if (section[ip] == ':') {	
		    ip = ip + 1
		    if (ctoi (section, ip, b) == 0)		# a:b
		        goto error_
	        } else
		    b = a
	    } else if (section[ip] == '-') {			# -*
		temp = a
		a = b
		b = temp
	        ip = ip + 1
	        if (section[ip] == '*')
		    ip = ip + 1
	    } else if (section[ip] == '*')			# *
	        ip = ip + 1
	    if (section[ip] == ':') {				# ..:step
	        ip = ip + 1
	        if (ctoi (section, ip, c) == 0)
		    goto error_
	        else if (c == 0)
		    goto error_
	    }
	    if (a > b && c > 0)
	        c = -c

	    x1[i] = a
	    x2[i] = b
	    xs[i] = c

	    while (IS_WHITE(section[ip]))
	        ip = ip + 1
	    if (section[ip] == ',')
		ip = ip + 1
	}

	if (section[ip] != ']')
	    goto error_

	return
error_
	call error (0, "Error in image section specification")
end

# rtparse -- Parse an image name into the cluster name, cluster index,
# cluster size
#
#	Syntax:		cluster[cl_index/cl_size]
#
# where all fields are optional except the cluster name.  In the limiting case
# (cl_size = 1) the cluster name and image name are the same.  CL_INDEX and
# CL_SIZE must be simple positive decimal integer constants, if given.  The
# [ character must be escaped to be included in the filename of the cluster.
#

procedure rtparse (root, cluster, sz_cluster, cl_index, cl_size)

char	root[ARB]		# full image specification
char	cluster[ARB]		# receives cluster name
int	sz_cluster		# max chars in cluster name
int	cl_index		# receives cluster index (default 0)
int	cl_size			# receives cluster size (default 0)

int	ip, op, lbrack, ch, n
errchk	syserrs

begin
	ip = 1
	op = 1

	# Extract cluster name.  The first (unescaped) [ marks the start of
	# either the cl_index subscript or a section field.

	for (ch=root[ip];  ch != EOS && ch != '[';  ch=root[ip]) {
	    if (ch == '\\' && root[ip+1] == '[') {
		cluster[op] = '\\'
		op = op + 1
		cluster[op] = '['
		ip = ip + 1
	    } else
		cluster[op] = ch

	    op = min (sz_cluster, op + 1)
	    ip = ip + 1
	}

	cluster[op] = EOS
	lbrack      = ip
	cl_index    = 0
	cl_size     = 0

	if (ch == EOS) {
	    return
	}

	# If we have a [...] field, it is a cl_index subscript.  
 	# A cl_index subscript is anything with the syntax [ddd] 
	# or [ddd/ddd]

	ip = ip + 1
	n = 0

	for (ch=root[ip];  ch != EOS;  ch=root[ip]) {
	    if (IS_DIGIT(ch)) {
		n = (n * 10) + TO_INTEG(ch)
	    } else if (ch == '/') {
		cl_index = max (n, 1)
		n = 0
	    } else if (ch == ']') {
		ip = ip + 1
		break
	    } else {
		# Not a cl_index subscript; must be a section.
		ip = lbrack
		n  = 0
		break
	    }
	    ip = ip + 1
	}

	if (cl_index == 0)
	    cl_index = n
	else
	    cl_size = n
end

# MYPARSE -- Parse an image specification into the cluster name, cluster index,
# cluster size, kernel section, and image section fields.
#
#	Syntax:		cluster[cl_index/cl_size][ksection][section]
#
# where all fields are optional except the cluster name.  In the limiting case
# (cl_size = 1) the cluster name and image name are the same.  CL_INDEX and
# CL_SIZE must be simple positive decimal integer constants, if given.  The
# [ character must be escaped to be included in the filename of the cluster.

procedure myparse (imspec, cluster, sz_cluster, ksection, sz_ksection,
	section, sz_section, cl_index, cl_size)

char	imspec[ARB]		# full image specification
char	cluster[ARB]		# receives cluster name
int	sz_cluster		# max chars in cluster name
char	ksection[ARB]		# receives kernel section
int	sz_ksection		# max chars in kernel section name
char	section[ARB]		# receives image section
int	sz_section		# max chars in image section name
int	cl_index		# receives cluster index (default 0)
int	cl_size			# receives cluster size (default 0)

pointer	sp, cp, secbuf
int	ip, op, lbrack, level, ch, n
bool	is_ksection, sect_out, ksect_out
int	stridx()
errchk	syserrs

begin
	call smark (sp)
	call salloc (secbuf, SZ_LINE, TY_CHAR)

	ip = 1
	op = 1

	# Extract cluster name.  The first (unescaped) [ marks the start of
	# either the cl_index subscript or a section field.

	for (ch=imspec[ip];  ch != EOS && ch != '[';  ch=imspec[ip]) {
	    if (ch == '\\' && imspec[ip+1] == '[') {
		cluster[op] = '\\'
		op = op + 1
		cluster[op] = '['
		ip = ip + 1
	    } else
		cluster[op] = ch

	    op = min (sz_cluster, op + 1)
	    ip = ip + 1
	}

	cluster[op] = EOS
	ksection[1] = EOS
	section[1]  = EOS
	lbrack      = ip
	cl_index    = 0
	cl_size     = 0

	if (ch == EOS) {
	    call sfree (sp)
	    return
	}

	# If we have a [...] field, determine whether it is a cl_index
	# subscript or a kernel or image section.  A cl_index subscript is
	# anything with the syntax [ddd] or [ddd/ddd]; anything else is a
	# kernel or image section.

	ip = ip + 1
	n = 0

	for (ch=imspec[ip];  ch != EOS;  ch=imspec[ip]) {
	    if (IS_DIGIT(ch)) {
		n = (n * 10) + TO_INTEG(ch)
	    } else if (ch == '/') {
		cl_index = max (n, 1)
		n = 0
	    } else if (ch == ']') {
		ip = ip + 1
		break
	    } else {
		# Not a cl_index subscript; must be a section.
		ip = lbrack
		n  = 0
		break
	    }
	    ip = ip + 1
	}

	if (cl_index == 0)
	    cl_index = n
	else
	    cl_size = n

	# The rest of the input string consists of the kernel and image
	# sections, if any.

	sect_out = false
	ksect_out = false

	while (imspec[ip] == '[') {
	    is_ksection = false
	    cp = secbuf
	    level = 0

	    for (ch=imspec[ip];  ch != EOS;  ch=imspec[ip]) {
		if (ch == '[')
		    level = level + 1
		else if (ch == ']')
		    level = level - 1
		else if (!is_ksection)
		    if (stridx (imspec[ip], " 0123456789+-:*,") == 0)
			is_ksection = true

		Memc[cp] = ch
		cp = cp + 1
		ip = ip + 1

		if (level == 0)
		    break
	    }
	    Memc[cp] = EOS

	    if (level != 0)
		call syserrs (SYS_IMSYNSEC, imspec)
	    if (is_ksection) {
		if (ksect_out)
		    call syserrs (SYS_IMSYNSEC, imspec)
		call strcpy (Memc[secbuf], ksection, sz_ksection)
		ksect_out = true
	    } else {
		if (sect_out)
		    call syserrs (SYS_IMSYNSEC, imspec)
		call strcpy (Memc[secbuf], section, sz_section)
		sect_out = true
	    }

	    while (imspec[ip] != EOS && imspec[ip] != '[')
		ip = ip + 1
	}

	call sfree (sp)
end

# GC_FILNAM -- Get a file name with the group specification
#
# CYZhang 1 May, 1994

procedure gc_filnam (im, filename)

# Calling arguments
pointer	im
char	filename[SZ_LINE]

# Local variables
char	clstname[SZ_LINE]
char	text[SZ_FNAME]
int	gn, ngroups

int	gf_gstfval()

begin
	call imstats (im, IM_IMAGENAME, clstname, SZ_LINE)
	call imgcluster (clstname, filename, SZ_LINE)
	gn = gf_gstfval (im, "GROUP")
	ngroups = gf_gstfval (im, "GCOUNT")
	call sprintf (text, SZ_FNAME, "[%d/%d]")
	call pargi (gn)
	call pargi (ngroups)
	call strcat (text, filename, SZ_LINE)
end


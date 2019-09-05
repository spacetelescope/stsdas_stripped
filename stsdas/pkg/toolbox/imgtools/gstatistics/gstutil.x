# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "gstat.h"

#-------------------------------------------------------------------------------
.help gstutil.x Dec95 imgtools$gstatistics
.ih
NAME
   gst_fields - Decode the fields string to determine the quantities to compute
  gst_gsfield - Determine whether a specified field was selected.
  gst_pheader - Print the banner fields
    gst_print - Print the fields
    gst_ppset - Save the statistics for the last group of the last image
gst_setswitch - Set computation switches for statistical quantitites.
.endhelp
#-------------------------------------------------------------------------------
# GST_FIELDS -- Procedure to decode the fields string into a list of the
# 		fields to be computed and printed.
#
#  Revision history:
#	Aug 93  CYZhang		Initial implementation

int procedure gst_fields (fieldstr, fields, max_nfields)

char	fieldstr[ARB]		# string containing the list of fields
int	fields[ARB]		# fields array
int	max_nfields		# maximum number of fields

int	n_fields, flist, field
pointer	sp, fname
int	fntopnb(), fntgfnb(), strdic()

define	NO_MATCH	0

begin
	n_fields = 0

	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	flist = fntopnb (fieldstr, NO)

	# fntopnb (template, sort), now sort = NO

	while (fntgfnb (flist, Memc[fname], SZ_FNAME) != EOF && 
	    (n_fields < max_nfields)) {
	    field = strdic (Memc[fname], Memc[fname], SZ_FNAME, GS_FIELDS)
	    if (field == NO_MATCH)
		next

	    n_fields = n_fields + 1
	    fields[n_fields] = field
	}

	call fntclsb (flist)
	call sfree (sp)

	return (n_fields)
end


#-------------------------------------------------------------------------------
# GST_GSFIELD -- Determine whether a specified field was selected. 

int procedure gst_gsfield (field, fields, n_fields)

int	field		# field to be tested
int	fields[ARB]	# array of selected fields
int	n_fields	# number of fields

int	i, isfield

begin
	isfield = NO
	do i = 1, n_fields {
	    if (field != fields[i])
		next
	    isfield = YES
	    break
	}

	return (isfield)
end

#-------------------------------------------------------------------------------
# GST_PHEADER -- Print the banner fields.
#
#  Revision history:
#	  -Aug-93 C.-Y. Zhang	Initial implementation
#	 4-Dec-95 RAShaw	Simplify logic for printing header. 

procedure gst_pheader (image, mskflg, mask, g_accum, n_groups, group_list, 
			fields, n_fields)

# Calling arguments
char	image[SZ_FNAME]		# Image name
bool	mskflg			# Mask operation?
char	mask[SZ_FNAME]		# Mask name
int	n_groups		# Number of groups
bool	g_accum			# Accumulate over groups?
char	group_list[SZ_LINE]	# List of group ranges	
int	fields[ARB]		# Fields to be printed
int 	n_fields		# Number of fields

# Local declarations:
int	i			# generic

begin
 	call printf ("\n# Image Statistics for: %s \n") 
	call pargstr (image)

	if ( n_groups > 1 && g_accum ) {
	    call printf ("# Accumulated over groups: %s \n")
	    call pargstr (group_list)
	}

	if ( mskflg ) {
	    call printf ("# Bad pixels rejected by mask: %s \n")
	    call pargstr (mask)
	}

	if ( n_groups > 1 && !g_accum ) 
	    call printf ("# GROUP ")
	else
	    call printf ("#")

	do i = 1, n_fields {
	    switch (fields[i]) {
	    case GS_FDOALL:
	        call printf ("      NPIX       MIN       MAX       SUM")
		call printf ("      MEAN     MIDPT      MODE")
		call printf ("    STDDEV  SKEWNESS  KURTOSIS")
	    case GS_FNPIX:
	        call printf ("      NPIX")
	    case GS_FMIN:
		call printf ("       MIN")
	    case GS_FMAX:
		call printf ("       MAX")
	    case GS_FSUM:
		call printf ("       SUM")
	    case GS_FMEAN:
		call printf ("      MEAN")
	    case GS_FMIDPT:
		call printf ("     MIDPT")
	    case GS_FMODE:
		call printf ("      MODE")
	    case GS_FSTDDEV:
		call printf ("    STDDEV")
	    case GS_FSKEW:
		call printf ("  SKEWNESS")
	    case GS_FKURT:
		call printf ("  KURTOSIS")
	    }
	}

	call printf ("\n")
	call flush (STDOUT)
end

#-------------------------------------------------------------------------------
# GST_PRINT -- Print the fields
#
#  Modification history:
#	  -Aug-93 by C.-Y. Zhang	Initial implementation
#	 4-Mar-95 by RAShaw		Remove brackets from printed group number

procedure gst_print (g_accum, gn, gst, fields, n_fields)

bool	g_accum			# Accumulate over groups?
int	gn			# Group No.
pointer	gst			# Pointer to the statistics structure
int	fields[ARB]		# Fields to be printed
int	n_fields			# Number of fields

int	i

begin

# Set gn = 0 for a non-group image, when this procedure is called
	if (gn >= 1) {
	    if ( g_accum ) {
		call printf ("")
	    } else {
		call printf (" %5d ")
		call pargi (gn)
	    }
	}
	call printf (" ")
	do i = 1, n_fields {
	    switch (fields[i]) {
	    case GS_FDOALL:
	        call printf (" %9d %9.6g %9.6g %9.6g %9.6g %9.6g %9.6g")
		call pargi (GS_NPIX(gst))
		call pargr (GS_MIN(gst))
		call pargr (GS_MAX(gst))
		call pargd (GS_SUMX(gst))
		call pargr (GS_MEAN(gst))
		call pargr (GS_MIDPT(gst))
		call pargr (GS_MODE(gst))
		call printf (" %9.6g %9.6g %9.6g")
		call pargr (GS_STDDEV(gst))
		call pargr (GS_SKEW(gst))
		call pargr (GS_KURT(gst))
	    case GS_FNPIX:
	        call printf (" %9d")
		    call pargi (GS_NPIX(gst))
	    case GS_FMIN:
		call printf (" %9.6g")
		    call pargr (GS_MIN(gst))
	    case GS_FMAX:
		call printf (" %9.6g")
		    call pargr (GS_MAX(gst))
	    case GS_FSUM:
		call printf (" %9.6g")
		    call pargd (GS_SUMX(gst))
	    case GS_FMEAN:
		call printf (" %9.6g")
		    call pargr (GS_MEAN(gst))
	    case GS_FMIDPT:
		call printf (" %9.6g")
		    call pargr (GS_MIDPT(gst))
	    case GS_FMODE:
		call printf (" %9.6g")
		    call pargr (GS_MODE(gst))
	    case GS_FSTDDEV:
		call printf (" %9.6g")
		    call pargr (GS_STDDEV(gst))
	    case GS_FSKEW:
		call printf (" %9.6g")
		    call pargr (GS_SKEW(gst))
	    case GS_FKURT:
		call printf (" %9.6g")
		    call pargr (GS_KURT(gst))
	    }
	}

	call printf ("\n")
	call flush (STDOUT)
end

#-------------------------------------------------------------------------------
#  GST_PPSET - Save the statistics for the last group of the last image
#
#	 4-Dec-95 by RAShaw	Remove field selection logic & write all 
#				computed values to the pset.  

procedure gst_ppset (image, pp, gst)

char	image[SZ_FNAME]			# Image name
pointer	pp				# Pointer to the output pset
pointer	gst				# Pointer to the statistics structure

errchk	clppset, clppseti, clppsetr, clppsetd

begin
	call clppset (pp, "image", image)

   	call clppseti (pp, "npix",   GS_NPIX(gst))
   	call clppsetr (pp, "min",    GS_MIN(gst))
   	call clppsetr (pp, "max",    GS_MAX(gst))
   	call clppsetd (pp, "sum",    GS_SUMX(gst))
   	call clppsetr (pp, "mean",   GS_MEAN(gst))
	call clppsetr (pp, "stddev", GS_STDDEV(gst))
	call clppsetr (pp, "midpt",  GS_MIDPT(gst))
	call clppsetr (pp, "mode",   GS_MODE(gst))
	call clppsetr (pp, "skew",   GS_SKEW(gst))
	call clppsetr (pp, "kurt",   GS_KURT(gst))

end


#-------------------------------------------------------------------------------
#  GST_SETSWITCH -- Set computation switches for statistical quantitites.
#
#  Revision History:
#	 1 Dec 95 by RAShaw	Initial implementation.

procedure gst_setswitch (gsw, fields, n_fields)

# Calling arguments:
pointer	gsw			#I: Calculation switch structure
int	fields			#I: Selected statistical quantities to calculate
int	n_fields		#I: Number of quantities selected

# Function used:
int	gst_gsfield() 		# Is selected field right?

begin
	SW_DOALL(gsw) = gst_gsfield (GS_FDOALL, fields, n_fields)

	if (SW_DOALL(gsw) == YES) {
	    SW_NPIX(gsw)   = YES
	    SW_SUM(gsw)    = YES
	    SW_MEAN(gsw)   = YES
	    SW_MIDPT(gsw)  = YES
	    SW_MODE(gsw)   = YES
	    SW_STDDEV(gsw) = YES
	    SW_SKEW(gsw)   = YES
	    SW_KURT(gsw)   = YES
	    SW_MNMX(gsw)   = YES

	} else {
	    SW_NPIX(gsw)   = gst_gsfield (GS_FNPIX,   fields, n_fields)
	    SW_SUM(gsw)    = gst_gsfield (GS_FSUM,    fields, n_fields)
	    SW_MEAN(gsw)   = gst_gsfield (GS_FMEAN,   fields, n_fields)
	    SW_MIDPT(gsw)  = gst_gsfield (GS_FMIDPT,  fields, n_fields)
	    SW_MODE(gsw)   = gst_gsfield (GS_FMODE,   fields, n_fields)
	    SW_STDDEV(gsw) = gst_gsfield (GS_FSTDDEV, fields, n_fields)
	    SW_SKEW(gsw)   = gst_gsfield (GS_FSKEW,   fields, n_fields)
	    SW_KURT(gsw)   = gst_gsfield (GS_FKURT,   fields, n_fields)

	    if (gst_gsfield (GS_FMIN, fields, n_fields) == YES ||
	    	gst_gsfield (GS_FMAX, fields, n_fields) == YES ||
	    	SW_MIDPT(gsw) == YES || SW_MODE(gsw) == YES)

	    	SW_MNMX(gsw) = YES
	    else
	    	SW_MNMX(gsw) = NO
	}

end



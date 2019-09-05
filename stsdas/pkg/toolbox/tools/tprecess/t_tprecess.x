include <imhdr.h>
include <math.h>
include <error.h>
include <tbset.h>
include "precess.h"

# file types
define	TYPE_UNKNOWN	0
define	TYPE_IMAGE	1
define	TYPE_TEXT	2
define	TYPE_TABLE	3

# t_precess -- precess celestial coordinates
# The input may be either a list of images, text files, or tables,
# or any combination.  For each file, the task first attempts to open
# the file as an image.  If that fails, the access function is used to
# see if the file is a text file.  If not, the file is opened as a table.
# (Note that the text-table option of tables is therefore bypassed.)
#
# Precessing an ascii file or table means that right ascension and
# declination values in a pair of columns are precessed.
#
# Precessing an image does not change the image data values.  The
# values of keywords CRVAL1 and CRVAL2 are precessed, and the CD matrix
# is rotated by the appropriate amount.  Although an image may be higher
# than 2-D, the right ascension and declination must be the first two
# axes, but not necessarily in that order.
#
# Andrew Cseko, Jr.   May-1990    Original
# Phil Hodge, 26-Jul-1990  Add ra_units & hms_format parameters.
# Phil Hodge, 11-Jun-1992  Call tpr_what_kind to check file type.
# Phil Hodge, 14-Jan-1992  Call tpr_imcopy instead of imcopy to copy an image;
#			use imt instead of fnt; don't get newcopy parameter.

procedure t_tprecess()

pointer input		# name(s) of image/table to precess
pointer output		# name of new table
pointer indate		# date of input data  | date to precess from
pointer outdate		# date or output data | date to precess to
int	ra_units	# hours or degrees
int	units[2]	# units that will be used for ra & dec
bool	hms_format	# ascii output in HMS, DMS format?
char	i_colname[SZ_COLNAME, 4]	# (input) of source in tables
bool	verbose		# print file names?
#--
pointer i_flist, o_flist # input/output file name list pointers
int	outlen		# number of names in output file list
bool	newcopy		# make new table/image vs modify old
int	dummy, mode
int	file_type	# image, text, or table
int	ip
int	t_ctype1	# passed from test_image() to pimag_precess_image()
pointer sp, fp
char	temp[SZ_LINE]
char	o_colname[SZ_COLNAME, 4]  # output names (when newcopy false)

int	i_sptr,  o_sptr  # textfile precess input/output stream ptrs
bool	i_j_set, o_j_set # are the start/end julian date parameters set

double	i_j_day, o_j_day # input/output julian day

pointer imtopen()
int	imtgetim(), imtlen()
int     clgwrd()
bool    clgetb()
int     test_table(), test_image(), interpret_date()
int     strlen()
int     open()
int     errget()
bool	strne()
pointer tbtopn()
pointer immap()

begin
	# Allocate stack memory for strings

	call smark (sp)
	call salloc (input,  SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	call salloc (indate,  SZ_LINE,  TY_CHAR)
	call salloc (outdate, SZ_LINE,  TY_CHAR)

	# Read the task parameters

	call clgstr ("input",    Memc[input],   SZ_FNAME)
	call xt_stripwhite (Memc[input])
	if (Memc[input] == EOS)
	    call error (1, "input undefined")

	call clgstr ("output",   Memc[output],  SZ_FNAME)
	call xt_stripwhite (Memc[output])

	call clgstr ("fromdate", Memc[indate],  SZ_LINE)
	call clgstr ("todate",   Memc[outdate], SZ_LINE)
	ra_units = clgwrd ("ra_units", temp, SZ_LINE, "|hours|degrees|radians|")
	hms_format = clgetb ("hms_format")
	call clgstr ("namcol1",  i_colname[1, RA],     SZ_COLNAME)
	call clgstr ("namcol2",  i_colname[1, DEC],    SZ_COLNAME)
	call clgstr ("namcol3",  i_colname[1, FROMJD], SZ_COLNAME)
	call clgstr ("namcol4",  i_colname[1, TOJD],   SZ_COLNAME)
	verbose = clgetb ("verbose")

	# Open image template lists for input and output.
	i_flist = imtopen (Memc[input])
	o_flist = imtopen (Memc[output])

	outlen = imtlen (o_flist)
	if (outlen > 0 && outlen != imtlen (i_flist))
	    call error (1, "input and output lists not the same length")

	call xt_stripwhite (Memc[indate])
	if (Memc[indate] == EOS)
	    i_j_set = false
	else
	    i_j_set = true

	call xt_stripwhite (Memc[outdate])
	if (Memc[outdate] == EOS)
	    o_j_set = false
	else
	    o_j_set = true;

	if (i_j_set) {
	    ip = 1
	    if (strlen (Memc[indate]) != interpret_date (Memc[indate],
			ip, i_j_day))
		call error (1, "can't interpret fromdate")
	}

	if (o_j_set) {
	    ip = 1
	    if (strlen (Memc[outdate]) != interpret_date (Memc[outdate],
			ip, o_j_day))
		call error (1, "can't interpret todate")
	}

	# Check the units for ra, and set units for dec accordingly.
	if (ra_units == PREC_HOURS) {
	    units[RA] = PREC_HOURS
	    units[DEC] = PREC_DEGREES
	} else if (ra_units == PREC_DEGREES) {
	    units[RA] = PREC_DEGREES
	    units[DEC] = PREC_DEGREES
	} else if (ra_units == PREC_RADIANS) {
	    units[RA] = PREC_RADIANS
	    units[DEC] = PREC_RADIANS
	}

	# Loop over all the files in the file list

	while (imtgetim (i_flist, Memc[input], SZ_FNAME) != EOF) {
	    if (outlen > 0) {
		dummy = imtgetim (o_flist, Memc[output], SZ_FNAME)
		newcopy = strne (Memc[input], Memc[output])
	    } else {
		# output list is null, so input files will be modified in-place
		newcopy = false
	    }

	    if (verbose) {
		call printf ("%s")
		    call pargstr (Memc[input])
		if (newcopy) {
		    call printf (" --> %s")
			call pargstr (Memc[output])
		} else {
		    call printf (" (in-place)")
		}
		call printf ("\n")
		call flush (STDOUT)
	    }

	    # Check what kind of input file we've got.  If it's an
	    # image or table, then open it.
	    if (newcopy)
		mode = READ_ONLY
	    else
		mode = READ_WRITE
	    call tpr_what_kind (Memc[input], mode, fp, file_type)

	    # Do the precession.
	    if (file_type == TYPE_IMAGE) {

		#-------------------------------------------------------------
		# precess the file as an image

		if (ERR == test_image (fp, i_j_set, o_j_set,
			    i_j_day, o_j_day, t_ctype1, temp, SZ_LINE)) {
		    call eprintf ("ERROR> image '%s':\n       %s - skipping\n")
			call pargstr (Memc[input])
			call pargstr (temp)
		    call imunmap (fp)
		    next
		}

		if (newcopy) {
		    call imunmap (fp)

		    iferr (call tpr_imcopy (Memc[input], Memc[output])) {
			dummy = errget (temp, SZ_LINE)
			call eprintf (
		    "error> could not copy image '%s' to '%s'\n  because %s")
			    call pargstr (Memc[input])
			    call pargstr (Memc[output]) 
			    call pargstr (temp)
			next
		    }
		    fp = immap (Memc[output], READ_WRITE, NULL)  
		}

		call pimag_precess_image (fp, i_j_day, o_j_day, t_ctype1)
		call imunmap (fp)

	    } else if (file_type == TYPE_TEXT) {

		#-------------------------------------------------------------
		# precess the file as a text file

		iferr (i_sptr = open (Memc[input], READ_ONLY, TEXT_FILE)) {
		    call erract (EA_WARN)
		    next
		}
		iferr (o_sptr = open (Memc[output], NEW_FILE, TEXT_FILE)) {
		    call erract (EA_WARN)
		    next
		}

		call ptext_precess_textfile (i_j_set, o_j_set,
			i_j_day, o_j_day, i_sptr, o_sptr, units, hms_format)

		if (i_sptr != STDIN)
		    call close (i_sptr)
		if (o_sptr != STDOUT)
		    call close (o_sptr)

	    } else if (file_type == TYPE_TABLE) {

		#------------------------------------------------------------
		# precess the file as a table

		if (ERR == test_table (fp, i_colname, o_colname,
		    "p_", newcopy, i_j_set, o_j_set, temp, SZ_LINE)) {
		    call eprintf ("ERROR> table '%s' : %s - skipping \n")
			call pargstr (Memc[input])
			call pargstr (temp)
		    call tbtclo (fp) # close table and free memory
		    next
		}

		if (newcopy) {
		    call tbtclo (fp)

		    iferr (call tbtcpy (Memc[input], Memc[output])) {
			dummy = errget (temp, SZ_LINE)
			call eprintf (
		"error> unable to copy table '%s' to '%s'\n  because %s\n")
			    call pargstr (Memc[input])
			    call pargstr (Memc[output]) 
			    call pargstr (temp)
			next
		    }
		    fp = tbtopn (Memc[output], READ_WRITE, NULL)
		}

		call ptabl_precess_table (fp, i_colname, o_colname,
		    newcopy, i_j_set, o_j_set, i_j_day, o_j_day, ra_units)

		call tbtclo (fp)	# close table and free memory

	    } else {
		call eprintf ("couldn't open %s\n")
		    call pargstr (Memc[input])
	    }
	}

	call imtclose (i_flist)
	call imtclose (o_flist)
	call sfree (sp)
end


# what_kind -- what kind of input file
# This routine checks whether the input file is an image, a text file,
# or a table.  If the file is either an image or a table, it will be
# opened with the I/O mode specified.  For a text file, however, the
# file will not be opened.

procedure tpr_what_kind (input, iomode, fp, file_type)

char	input[ARB]		# i: name of input file
int	iomode			# i: I/O mode for opening image or table
pointer fp			# o: pointer, or NULL for text file
int	file_type		# o: specifies image, text, or table
#--
pointer im, immap()
pointer tp, tbtopn()
int	access()
bool	streq()

begin
	if (streq (input, "STDIN")) {
	    fp = NULL
	    file_type = TYPE_TEXT
	    return
	}

	# Check for an image.
	ifnoerr (im = immap (input, iomode, NULL)) {
	    fp = im
	    file_type = TYPE_IMAGE
	    return
	}

	# Check for a text file.
	if (access (input, 0, TEXT_FILE) == YES) {
	    fp = NULL				# note:  not open
	    file_type = TYPE_TEXT
	    return
	}

	# Check for a table.
	ifnoerr (tp = tbtopn (input, iomode, NULL)) {
	    fp = tp
	    file_type = TYPE_TABLE
	    return
	}

	# None of the above.
	fp = NULL
	file_type = TYPE_UNKNOWN
end

# tpr_imcopy -- copy an image

procedure tpr_imcopy (input, output)

char	input[ARB]	# i: name of input image
char	output[ARB]	# i: name of output image
#--
pointer iim, oim	# pointers to imhdr struct for input & output images
pointer ival, oval	# pointers to data values for an image line
int	naxis1		# length of a line
long	ic[IM_MAXDIM]	# loop counter for input image
long	oc[IM_MAXDIM]	# loop counter for output image
long	one
pointer immap()
int	imgnlr(), impnlr()
errchk	immap

begin
	iim = immap (input, READ_ONLY, NULL)
	oim = immap (output, NEW_COPY, iim)

	naxis1 = IM_LEN(iim, 1)

	# Initialize the counters prior to calling imgnlr, impnlr.
	one = 1
	call amovkl (one, ic, IM_MAXDIM)
	call amovkl (one, oc, IM_MAXDIM)

	# Copy the data.
	while (imgnlr (iim, ival, ic) != EOF &&
	       impnlr (oim, oval, oc) != EOF)
	    call amovr (Memr[ival], Memr[oval], naxis1)

	call imunmap (oim)
	call imunmap (iim)
end

#=============================================================================

# ptext_precess_textfile -- precess the user input RA and DEC. Sexagesimal 
# notation is excepted, although RA must be in degrees, not in time.
# If the precession dates were not specified in the task parameters the user
# must input them.  An EOF on input is interpreted a quit.

procedure ptext_precess_textfile (f_d_set, t_d_set, f_day, t_day,
		i_file, o_file, units, hms_format)

bool    f_d_set, t_d_set	# have the input/output julian dates been set
double  f_day, t_day		# precession dates
int     i_file, o_file		# input/output file pointers
int	units[ARB]		# i: hours or degrees
bool	hms_format		# i: output in HMS, DMS format?
#--
char    buf[SZ_LINE]		# buffer for line from input file
char	p_fmt[SZ_FNAME]		# format for ra, then for both ra & dec
char	dec_fmt[SZ_FNAME]	# format for dec
double  ra, dec
double  zetaO, Z, theta
int     index
int	ra_index, dec_index	# starting locations in buf for reading ra, dec

int     interpret_date()
int     ctod()
int     getline()

begin
	if (i_file == STDIN) {

	    call eprintf ("%-15s%-15s")
		call pargstr ("--RA--")
		call pargstr ("--DEC--")

	    if (!f_d_set)
		call eprintf ("%-15s")
		    call pargstr ("--FROM EPOCH--")

	    if (!t_d_set)
		call eprintf ("%-15s")
		    call pargstr ("--TO EPOCH--")

	    call eprintf ("\n")
	}

	while (true) {

	    call flush (o_file)
	    if (EOF == getline (i_file, buf))
		return

	    call xt_stripwhite (buf)
	    if ((buf[1] == '#') || (buf[1] == '\n') || (buf[1] == EOS))
		next

	    index = 1
	    ra_index = 1			# this doesn't change
	    if (0 == ctod (buf, index, ra)) {
		call eprintf ("error > ra value uninterpretable\n")
		next
	    }

	    dec_index = index			# used by pr_get_fmt
	    if (0 == ctod (buf, index, dec)) {
		call eprintf ("error > dec value uninterpretable\n")
		next
	    }

	    if (!f_d_set)
		if (0 == interpret_date (buf, index, f_day)) {
		    call eprintf ("error > from epoch value uninterpretable\n")
		    next
		}

	    if (!t_d_set)
		if (0 == interpret_date (buf, index, t_day)) {
		    call eprintf ("error > to epoch value uninterpretable\n")
		    next
		}

	    # Calculate the rotation angles.
	    call prec_angles (f_day, t_day, zetaO, Z, theta)

	    # Convert ra and dec to radians.
	    call ad_to_rad (ra, units[RA], ra)
	    call ad_to_rad (dec, units[DEC], dec)

	    # Precess ra & dec, returning the result in-place.
	    call precess (ra, dec, zetaO, Z, theta)

	    # Convert ra and dec from radians to hours, degrees, or whatever.
	    call ad_from_rad (ra, units[RA], ra)
	    call ad_from_rad (dec, units[DEC], dec)

	    # Get the format for printing the results, depending on the
	    # precision of the input (in buf).  pr_get_fmt returns the ra
	    # print format in p_fmt, then we append the dec format to it.
	    call pr_get_fmt (buf, ra_index, dec_index, units[RA], hms_format,
			p_fmt, dec_fmt, SZ_FNAME)
	    call strcat ("  ", p_fmt, SZ_FNAME)
	    call strcat (dec_fmt, p_fmt, SZ_FNAME)
	    call strcat ("\n", p_fmt, SZ_FNAME)

	    # output to the user
	    call fprintf (o_file, p_fmt)
		call pargd (ra)
		call pargd (dec)
	}
end

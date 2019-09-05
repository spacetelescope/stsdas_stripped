include	<imhdr.h>
include	"pixpos.h"

define	STR_ONE_IN  "Star name: %s %30tStar number: %d\nCatalog position: \
(%0.1f,%0.1f)\nFitted position (%0.1f,%0.1f)\nMarked position: \
(%0.1f,%0.1f)\n\n"

define	STR_ONE_OUT "Star name: %s %30tStar number: %d\nCatalog position: \
(%0.1f,%0.1f)\nFitted position (%0.1f,%0.1f)\n\n"

define	STR_TITLE   "        CATALOG POSITION  FITTED POSITION   \
MARKED POSITION STAR NAME\n\n"

# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# The procedures in this file manipulate the position structure. The
# position structure contains the names and coordinates of the catalog 
# stars in both the sky and plate reference frames.
#
# B.Simon	22-Jun-90	Original

# FREE_POS -- Free the position structure

procedure free_pos (pos)

pointer	pos		# i: Position structure
#--

begin

	call mfree (POS_NAMEVEC(pos), TY_CHAR)
	call mfree (POS_FLAGVEC(pos), TY_INT)
	call mfree (POS_RAVEC(pos), TY_DOUBLE)
	call mfree (POS_DECVEC(pos), TY_DOUBLE)
	call mfree (POS_XOLDVEC(pos), TY_DOUBLE)
	call mfree (POS_YOLDVEC(pos), TY_DOUBLE)
	call mfree (POS_XNEWVEC(pos), TY_DOUBLE)
	call mfree (POS_YNEWVEC(pos), TY_DOUBLE)

	call mfree (pos, TY_STRUCT)
end

# GCOLNUM_POS -- Get the column index from the column name and type

int procedure gcolnum_pos (name, type)

char	name[ARB]	# i: Position column name
int	type		# i: Position column type
#--
int	index
pointer	sp, typestr

string	param_list  "nstar,xlen,ylen"
string	text_list   "name"
string	int_list    "flag"
string	dbl_list    "ra,dec,xold,yold,xnew,ynew"

string	badtype     "Bad position column type (%s)"
string	badname     "Bad position column name (%s)"

int	word_match(), itoc()

begin

	switch (type) {
	case POS_PARAM:
	    index = word_match (name, param_list)

	case POS_TEXT:
	    index = word_match (name, text_list)
	    if (index > 0)
		index = index + 3

	case POS_INT:
	    index = word_match (name, int_list)
	    if (index > 0) 
		index = index + 4

	case POS_DBL:
	    index = word_match (name, dbl_list)
	    if (index > 0)
		index = index + 5

	default:
	    call smark (sp)
	    call salloc (typestr, SZ_FNAME, TY_CHAR)

	    index = itoc (type, Memc[typestr], SZ_FNAME)
	    call pixerror (badtype, Memc[typestr])

	    call sfree (sp)
	}

	if (index == 0)
	   call pixerror (badname, name)

	return (index-1)
end

# INI_POS --  Initialize the position structure

procedure ini_pos (par, pos)

pointer	par		# i: Parameter structure
pointer	pos		# o: Position structure
#--
int	nstar
pointer	sp, fi, catalog, name, ra, dec

string	badcatnam  "Catalog file not found (%s)"
string	badnamecol "Name column not found in catalog (%s)"
string	badracol   "RA column not found in catalog (%s)"
string	baddeccol  "Dec column not found in catalog (%s)"

int	getflag_param(), opn_file(), nrow_file(), rdcol_file()

begin
	call putflag_param (par, "cat", NO)

	call smark (sp)
	call salloc (catalog, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (ra, SZ_FNAME, TY_CHAR)
	call salloc (dec, SZ_FNAME, TY_CHAR)

	# Free old position structure if already open

	if (getflag_param (par, "cat") == YES)
	    call free_pos (pos)

	# See if input file is a list file or an SDAS table 
	# and open it accordingly

	call rdstr_param (par, "catalog", Memc[catalog], SZ_FNAME)
	fi = opn_file (Memc[catalog])
	if (fi == ERR) {
	    call pixmessage (badcatnam, Memc[catalog])
	    call rest_param (par, "catalog")
	    return
	}

	# Allocate the data structure after determining
	# how many stars are in the input file

	call malloc (pos, POS_LENGTH, TY_STRUCT)
	nstar = nrow_file (fi)

	POS_NSTAR(pos) = nstar
	POS_XLEN(pos) = 0
	POS_YLEN(pos) = 0

	call malloc (POS_NAMEVEC(pos), nstar*(SZ_STARNAME+1), TY_CHAR)
	call malloc (POS_FLAGVEC(pos), nstar, TY_INT)
	call malloc (POS_RAVEC(pos), nstar, TY_DOUBLE)
	call malloc (POS_DECVEC(pos), nstar, TY_DOUBLE)
	call malloc (POS_XOLDVEC(pos), nstar, TY_DOUBLE)
	call malloc (POS_YOLDVEC(pos), nstar, TY_DOUBLE)
	call malloc (POS_XNEWVEC(pos), nstar, TY_DOUBLE)
	call malloc (POS_YNEWVEC(pos), nstar, TY_DOUBLE)

	# Read the star name, right ascension, and declination from
	# the input file. Mark the pixel positions as invalid

	call rdstr_param (par, "name", Memc[name], SZ_FNAME)
	if (rdcol_file (fi, Memc[name], -SZ_STARNAME, 
			POS_NAMEVEC(pos), nstar) == ERR) {
	    call pixmessage (badnamecol, Memc[name])
	    call rest_param (par, "name")
	    call cls_file (fi)
	    return
	}

	call rdstr_param (par, "ra", Memc[ra], SZ_FNAME)
	if (rdcol_file (fi, Memc[ra], TY_DOUBLE, 
			POS_RAVEC(pos), nstar) == ERR) {
	    call pixmessage (badracol, Memc[ra])
	    call rest_param (par, "ra")	
	    call cls_file (fi)
	    return
	}


	call rdstr_param (par, "dec", Memc[dec], SZ_FNAME)
	if (rdcol_file (fi, Memc[dec], TY_DOUBLE, 
			POS_DECVEC(pos), nstar) == ERR) {
	    call pixmessage (baddeccol, Memc[dec])
	    call rest_param (par, "dec")
	    call cls_file (fi)
	    return
	}

	call amovki (FLAG_OFF, POS_FLAG(pos,1), nstar)

	# Convert the right ascension and declination to standard units

	call star_units (fi, Memc[ra], "ra_unit", POS_RA(pos,1), nstar)
	call star_units (fi, Memc[dec], "dec_unit", POS_DEC(pos,1), nstar)

	# Close the input file and free memory

	call cls_file (fi)
	call sfree (sp)

end

# LIST_POS -- List positions of stars in catalog on STDOUT

procedure list_pos (pos, frm, maxsol, solution, istar)

pointer	pos			# i: Position descriptor
pointer	frm			# i: Coordinate frame descriptor
int	maxsol			# i: Length of solution vector
double	solution[maxsol,2]	# i: Solution vector
int	istar			# i: Star number
#--
double	xfit, yfit
int	jstar, nstar

string	fmt_one_in   STR_ONE_IN
string	fmt_one_out  STR_ONE_OUT
string	fmt_title    STR_TITLE
string	fmt_all_in   "%5d %8.1f %8.1f %8.1f %8.1f %8.1f %8.1f %-s\n"
string	fmt_all_out  "%5d %8.1f %8.1f %8.1f %8.1f %17w %-s\n"

begin

	if (istar > 0) {
	    call apply_fit (frm, maxsol, solution, POS_XOLD(pos,istar),
			    POS_YOLD(pos,istar), xfit, yfit, 1)

	    if (POS_FLAG(pos,istar) == FLAG_IN) {
		call fprintf (STDERR, fmt_one_in)
		call pargstr (POS_NAME(pos,istar))
		call pargi (istar)
		call pargd (POS_XOLD(pos,istar))
		call pargd (POS_YOLD(pos,istar))
		call pargd (xfit)
		call pargd (yfit)
		call pargd (POS_XNEW(pos,istar))
		call pargd (POS_YNEW(pos,istar))
	    } else {
		call fprintf (STDERR, fmt_one_out)
		call pargstr (POS_NAME(pos,istar))
		call pargi (istar)
		call pargd (POS_XOLD(pos,istar))
		call pargd (POS_YOLD(pos,istar))
		call pargd (xfit)
		call pargd (yfit)
	    }
	    call flush (STDERR)

	} else {
	    nstar = 0

	    call fprintf (STDERR, fmt_title)

	    do jstar = 1, POS_NSTAR(pos) {
		if (POS_FLAG(pos,jstar) != FLAG_OFF) {
		    nstar = nstar + 1
		    call apply_fit (frm, maxsol, solution, 
				    POS_XOLD(pos,jstar), POS_YOLD(pos,jstar), 
				    xfit, yfit, 1)

		    if (POS_FLAG(pos,jstar) == FLAG_IN) {
			call fprintf (STDERR, fmt_all_in)
			call pargi (jstar)
			call pargd (POS_XOLD(pos,jstar))
			call pargd (POS_YOLD(pos,jstar))
			call pargd (xfit)
			call pargd (yfit)
			call pargd (POS_XNEW(pos,jstar))
			call pargd (POS_YNEW(pos,jstar))
			call pargstr (POS_NAME(pos,jstar))
		    } else {
			call fprintf (STDERR, fmt_all_out)
			call pargi (jstar)
			call pargd (POS_XOLD(pos,jstar))
			call pargd (POS_YOLD(pos,jstar))
			call pargd (xfit)
			call pargd (yfit)
			call pargstr (POS_NAME(pos,jstar))
		    }
		}
	   }
	   if (nstar == 0)
		call fprintf (STDERR, "\nNo catalog stars found in image.\n")

	   call flush (STDERR)
	}

end

# NAME_POS -- Get the index of the star from its name

int procedure name_pos (pos, name)

pointer pos		# i: Position descriptor
char	name[ARB]	# i: Star name
#--
int	ic, junk, istar, jstar, status

bool	strsame()
int	ctoi()

begin
	# First, see if name is actually an index

	ic = 1
	status = OK
	junk = ctoi (name, ic, jstar)

	# If not, search list of stars for name

	if (name[ic] != EOS || jstar < 1 || jstar > POS_NSTAR(pos)) {
	    jstar = ERR
	    do istar = 1, POS_NSTAR(pos) {
		if (POS_FLAG(pos,istar) == FLAG_OFF)
		    next

		if (strsame (name, POS_NAME(pos,istar))) {
		    jstar = istar
		    break
		}
	    }
	}

	return (jstar)
end

# NEAR_POS -- Get the index of the star nearest to a point

int procedure near_pos (pos, xlog, ylog)

pointer	pos		# i: Position descriptor
real	xlog		# i: X coordinate in logical frame
real	ylog		# i: Y coordinate in logical frame
#--
int	istar, jstar
real	xdist, ydist, idist, jdist

begin
	# Find the first star which is on the image

	jstar = 0
	do istar = 1, POS_NSTAR(pos) {
	    if (POS_FLAG(pos,istar) != FLAG_OFF) {
		jstar = istar
		break
	    }
	}

	if (jstar == 0)
	    return (ERR)

	# Set the initial minimum distance to that of the first star

	xdist = (xlog - POS_XOLD(pos,jstar)) 
	ydist = (ylog - POS_YOLD(pos,jstar))
	jdist = sqrt (xdist * xdist + ydist * ydist)

	# Find the minimum distance, saving the star number

	do istar = 2, POS_NSTAR(pos) {
	    if (POS_FLAG(pos,istar) != FLAG_OFF) {
		xdist = abs (xlog - POS_XOLD(pos,istar))
		if (xdist < jdist) {
		    ydist = abs (ylog - POS_YOLD(pos,istar))
		    if (ydist < jdist) {
			idist = sqrt (xdist * xdist + ydist * ydist)
			if (idist < jdist) {
			    jstar = istar
			    jdist = idist
			}
		    }
		}
	    }
	}

	# Return the star number with the minimum distance

	return (jstar)

end

# RDDCOL_POS -- Read a double precision column from the position structure

int procedure rddcol_pos (pos, name, flag, numcol, column, maxcol)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Flag indicating which values to include
int	numcol		# o: Number of values read
double	column[ARB]	# o: Array of values
int	maxcol		# i: Size of column array
#--
int	status, istar, jstar, index
pointer	vector

int	gcolnum_pos()

begin
	numcol = 0
	do istar = 1, POS_NSTAR(pos) {
	   if (POS_FLAG(pos,istar) <= flag)
		numcol = numcol + 1
	}

	if (numcol > maxcol) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_DBL)

	    jstar = 0
	    vector = Memi[pos+index]
	    do istar = 1, POS_NSTAR(pos) {
		if (POS_FLAG(pos,istar) <= flag) {
		    jstar = jstar + 1
		    column[jstar] = Memd[vector]
		}
		vector = vector + 1
	    }
	}

	return (status)
end

# RDICOL_POS -- Read an integer column from the position structure

int procedure rdicol_pos (pos, name, flag, numcol, column, maxcol)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Flag indicating which values to include
int	numcol		# o: Number of values read
int	column[ARB]	# o: Array of values
int	maxcol		# i: Size of column array
#--
int	status, istar, jstar, index
pointer	vector

int	gcolnum_pos()

begin
	numcol = 0
	do istar = 1, POS_NSTAR(pos) {
	   if (POS_FLAG(pos,istar) <= flag)
		numcol = numcol + 1
	}

	if (numcol > maxcol) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_INT)

	    jstar = 0
	    vector = Memi[pos+index]
	    do istar = 1, POS_NSTAR(pos) {
		if (POS_FLAG(pos,istar) <= flag) {
		    jstar = jstar + 1
		    column[jstar] = Memi[vector]
		}
		vector = vector + 1
	    }
	}

	return (status)
end

# RDRCOL_POS -- Read a real column from the position structure

int procedure rdrcol_pos (pos, name, flag, numcol, column, maxcol)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Flag indicating which values to include
int	numcol		# o: Number of values read
real	column[ARB]	# o: Array of values
int	maxcol		# i: Size of column array
#--
int	status, istar, jstar, index
pointer	vector

int	gcolnum_pos()

begin
	numcol = 0
	do istar = 1, POS_NSTAR(pos) {
	   if (POS_FLAG(pos,istar) <= flag)
		numcol = numcol + 1
	}

	if (numcol > maxcol) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_DBL)

	    jstar = 0
	    vector = Memi[pos+index]
	    do istar = 1, POS_NSTAR(pos) {
		if (POS_FLAG(pos,istar) <= flag) {
		    jstar = jstar + 1
		    column[jstar] = Memd[vector]
		}
		vector = vector + 1
	    }
	}

	return (status)
end

# RDTCOL_POS -- Read a text column from the position structure

int procedure rdtcol_pos (pos, name, flag, numcol, column, maxcol)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Flag indicating which values to include
int	numcol		# o: Number of values read
pointer	column[ARB]	# o: Array of values
int	maxcol		# i: Size of column array
#--
int	status, istar, jstar, index
pointer	vector

int	gcolnum_pos()

begin
	numcol = 0
	do istar = 1, POS_NSTAR(pos) {
	   if (POS_FLAG(pos,istar) <= flag)
		numcol = numcol + 1
	}

	if (numcol > maxcol) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_TEXT)

	    jstar = 0
	    vector = Memi[pos+index]
	    do istar = 1, POS_NSTAR(pos) {
		if (POS_FLAG(pos,istar) <= flag) {
		    jstar = jstar + 1
		    column[jstar] = vector
		}
		vector = vector + SZ_STARNAME + 1
	    }
	}

	return (status)
end

# RDINDEX_POS -- Read an array of star indices from the position structure

int procedure rdindex_pos (pos, flag, numcol, column, maxcol)

pointer	pos		# i: Star position structure
int	flag		# i: Flag indicating which values to include
int	numcol		# o: Number of values read
int	column[ARB]	# o: Array of values
int	maxcol		# i: Size of column array
#--
int	status, istar

begin
	numcol = 0
	status = OK

	do istar = 1, POS_NSTAR(pos) {
	   if (POS_FLAG(pos,istar) <= flag) {
		numcol = numcol + 1
		if (numcol > maxcol) {
		    status = ERR
		    break
		} else {
		    column[numcol] = istar
		}
	    }
	}

	return (status)
end

# RDIPAR_POS -- Read an integer parameter from the position structure

procedure rdipar_pos (pos, name, value)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	value		# o: Value
#--
int	index

int	gcolnum_pos()

begin
	index = gcolnum_pos (name, POS_PARAM)
	value = Memi[pos+index]

end

# RDDVAL_POS -- Read a double precision value from the position structure

int procedure rddval_pos (pos, name, flag, istar, value)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Star's flag
int	istar		# i: Index to star 
double	value		# o: Value
#--
int	status, index
pointer	vector

int	gcolnum_pos()

begin

	if (POS_FLAG(pos,istar) > flag) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_DBL)

	    vector = Memi[pos+index]
	    value = Memd[vector+istar-1]
	}

	return (status)
end

# RDIVAL_POS -- Read an integer value from the position structure

int procedure rdival_pos (pos, name, flag, istar, value)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Star's flag
int	istar		# i: Index to star 
int	value		# o: Value
#--
int	status, index
pointer	vector

int	gcolnum_pos()

begin

	if (POS_FLAG(pos,istar) > flag) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_INT)

	    vector = Memi[pos+index]
	    value = Memi[vector+istar-1]
	}

	return (status)
end

# RDRVAL_POS -- Read a real value from the position structure

int procedure rdrval_pos (pos, name, flag, istar, value)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Star's flag
int	istar		# i: Index to star 
real	value		# o: Value
#--
int	status, index
pointer	vector

int	gcolnum_pos()

begin

	if (POS_FLAG(pos,istar) > flag) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_DBL)

	    vector = Memi[pos+index]
	    value = Memd[vector+istar-1]
	}

	return (status)
end

# RDTVAL_POS -- Read a text value from the position structure

int procedure rdtval_pos (pos, name, flag, istar, value, maxch)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Star's flag
int	istar		# i: Index to star 
char	value[ARB]	# o: Text string
int	maxch		# i: String length
#--
int	status, index
pointer	vector

int	gcolnum_pos()

begin

	if (POS_FLAG(pos,istar) > flag) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_TEXT)

	    vector = Memi[pos+index]
	    call strcpy (Memc[vector+(SZ_STARNAME+1)*(istar-1)], value, maxch)
	}

	return (status)
end

# UPDATE_POS -- Update the old xy positions from the image wcs

procedure update_pos (par, pos, im, frm)

pointer	par		#  i: Parameter structure
pointer	pos		# io: Star position descriptor
pointer	im		#  o: Image descriptor
pointer	frm		#  o: Coordinate frame structure
#--
double	xold, yold
int	idim, ndim, istar
pointer	sp, image, mw, ct

string	badimage "Cannot read image (%s)"
string	baddimen "Image is not two dimensional (%s)"

int	getflag_param()
pointer	gf_map(), mw_openim(), mw_sctran()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Close image and frame structures if already open

	if (getflag_param (par, "im") == YES) {
	    call cls_frame (frm, mw)
	    call mw_close (mw)
	    call gf_unmap (im)
	}

	# Turn off image flag

	call putflag_param (par, "im", NO)

	# Get image name and open image

	call rdstr_param (par, "image", Memc[image], SZ_FNAME)
	iferr (im = gf_map (Memc[image], READ_ONLY, 0)) {
	    call pixmessage (badimage, Memc[image])
	    call rest_param (par, "image")
	    return
	}

	# Make sure the image is two dimensional

	ndim = min (IM_NDIM(im), 2)
	do idim = 3, IM_NDIM(im) {
	    if (IM_NDIM(im) != 1)
		ndim = idim
	}

	if (ndim != 2) {
	    call pixmessage (baddimen, Memc[image])
	    call rest_param (par, "image")
	    return
	}

	# Put the image dimensions in the position descriptor

	POS_XLEN(pos) = IM_LEN(im,1)
	POS_YLEN(pos) = IM_LEN(im,2)

	# Get world coordinate system from image

	mw = mw_openim (im)

	# Convert ra and dec to xy pixel position

	ct = mw_sctran (mw, "world", "logical", 3)

	call mw_v2trand (ct, POS_RA(pos,1), POS_DEC(pos,1), 
			 POS_XOLD(pos,1), POS_YOLD(pos,1), POS_NSTAR(pos))

	call mw_ctfree (ct)

	# Initialize coordinate frame transformation matrices

	call init_frame (mw, frm)

	# Set flags according to whether star is on or off the image

	do istar = 1, POS_NSTAR (pos) {
	    xold = POS_XOLD(pos,istar)
	    yold = POS_YOLD(pos,istar)

	    if (xold < 1.0 || xold > POS_XLEN(pos)) {
		POS_FLAG(pos,istar) = FLAG_OFF
	    } else if (yold < 1.0 || yold > POS_YLEN(pos)) {
		POS_FLAG(pos,istar) = FLAG_OFF
	    } else {
		POS_FLAG(pos,istar) = FLAG_OUT
	    }
	}

	call sfree (sp)
end

# WRTDVAL_POS -- Write a double precision value to the position structure

int procedure wrtdval_pos (pos, name, flag, istar, value)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Star's flag
int	istar		# i: Index to star 
double	value		# i: Value
#--
int	status, index
pointer	vector

int	gcolnum_pos()

begin

	if (POS_FLAG(pos,istar) > flag) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_DBL)

	    vector = Memi[pos+index]
	    Memd[vector+istar-1] = value
	}

	return (status)
end

# WRTIVAL_POS -- Write an integer value to the position structure

int procedure wrtival_pos (pos, name, flag, istar, value)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Star's flag
int	istar		# i: Index to star 
int	value		# i: Value
#--
int	status, index
pointer	vector

int	gcolnum_pos()

begin

	if (POS_FLAG(pos,istar) > flag) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_INT)

	    vector = Memi[pos+index]
	    Memi[vector+istar-1] = value
	}

	return (status)
end

# WRTRVAL_POS -- Write a real value to the position structure

int procedure wrtrval_pos (pos, name, flag, istar, value)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Star's flag
int	istar		# i: Index to star 
real	value		# i: Value
#--
int	status, index
pointer	vector

int	gcolnum_pos()

begin

	if (POS_FLAG(pos,istar) > flag) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_DBL)

	    vector = Memi[pos+index]
	    Memd[vector+istar-1] = value
	}

	return (status)
end

# WRTTVAL_POS -- Read a text value from the position structure

int procedure wrttval_pos (pos, name, flag, istar, value)

pointer	pos		# i: Star position structure
char	name[ARB]	# i: Name of column to read
int	flag		# i: Star's flag
int	istar		# i: Index to star 
char	value[ARB]	# i: Text string
#--
int	status, index
pointer	vector, valptr

int	gcolnum_pos()

begin

	if (POS_FLAG(pos,istar) > flag) {
	    status = ERR

	} else {
	    status = OK
	    index = gcolnum_pos (name, POS_TEXT)

	    vector = Memi[pos+index]
	    valptr = Memi[vector+istar-1]
	    call strcpy (value, Memc[valptr], SZ_STARNAME)
	}

	return (status)
end


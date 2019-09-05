include <imhdr.h>
include <ctotok.h>
include <error.h>

define	MAX_NAXIS	2	# max number of axes we can handle
define	SZ_PNAME	8	# size of a header parameter name

# t_shift -- shift an image
# This routine shifts the contents of an image or list of images by
# a specified integral number of pixels.
#
# Phil Hodge, 19-Aug-1988  Task SHIFT created.
# Phil Hodge,  7-Feb-1990  Remove "tmp$" from names of temporary images.

procedure t_shift ()

#--
pointer sp
pointer in_str, out_str		# scr for lists of file names
pointer shift_str		# scr for amount of shift in each axis
pointer infile, outfile		# scr for file names
pointer im1, im2		# imhdr pointers
pointer in, out			# pointers to input, output data
char	pname[SZ_PNAME]		# string for "crpix1", etc
real	crpix			# for updating the value of crpix
int	list1, list2		# imt pointers for input, output
int	naxis			# dimension of image
int	npts[MAX_NAXIS]		# length of each axis
int	shift[MAX_NAXIS]	# pixel shift (may be pos or neg)
int	inline, outline		# line numbers in input, output images
int	k			# loop index
int	dummy			# ignored
bool	blank_output		# output name blank?  (then modify in-place)
bool	in_place		# image to be modified in-place?
bool	verbose
pointer immap(), imgl1r(), impl1r(), imgl2r(), impl2r()
real	imgetr()
int	imaccf()
int	imtopen(), imtlen(), imtgetim()
bool	clgetb(), streq()

begin
	call smark (sp)
	call salloc (in_str, SZ_FNAME, TY_CHAR)
	call salloc (out_str, SZ_FNAME, TY_CHAR)
	call salloc (shift_str, SZ_FNAME, TY_CHAR)

	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)

	call clgstr ("input", Memc[in_str], SZ_FNAME)
	call clgstr ("output", Memc[out_str], SZ_FNAME)
	call clgstr ("shift", Memc[shift_str], SZ_FNAME)
	verbose = clgetb ("verbose")

	list1 = imtopen (Memc[in_str])		# open list of input files

	# If the output string is null then all input files will be
	# modified in-place.
	if (Memc[out_str] == EOS) {
	    blank_output = true
	    in_place = true
	} else {
	    blank_output = false
	    list2 = imtopen (Memc[out_str])	# open list of output files
	    if (imtlen (list1) != imtlen (list2))
		call error (1, "input and output lists not the same length")
	}

	while (imtgetim (list1, Memc[infile], SZ_FNAME) != EOF) {

	    im1 = immap (Memc[infile], READ_ONLY, 0)

	    if (blank_output) {
		in_place = true
	    } else {
		dummy = imtgetim (list2, Memc[outfile], SZ_FNAME)
		in_place = streq (Memc[infile], Memc[outfile])
	    }
	    if (in_place)			# replace output file name
		call mktemp ("shift", Memc[outfile], SZ_FNAME)

	    naxis = IM_NDIM(im1)
	    npts[1] = IM_LEN(im1,1)
	    if (naxis > 1)
		npts[2] = IM_LEN(im1,2)
	    if (naxis > MAX_NAXIS) {
		call imunmap (im1)
		call eprintf ("can only shift 1-D or 2-D images:\n")
		call eprintf ("    %s ignored\n")
		    call pargstr (Memc[infile])
		next				# go on to the next input file
	    }

	    # Read the amount of shift from the string supplied by the user.
	    iferr {
		call ft_g_shift (naxis, npts, MAX_NAXIS, Memc[shift_str], shift)
	    } then {
		call imunmap (im1)
		call imtclose (list1)
		if ( ! blank_output )
		    call imtclose (list2)
		call sfree (sp)
		call erract (EA_ERROR)
	    }

	    im2 = immap (Memc[outfile], NEW_COPY, im1)

	    if (verbose) {
		if (in_place) {
		    call printf ("(in-place) %s\n")
			call pargstr (Memc[infile])
		} else {
		    call printf ("%s --> %s\n")
			call pargstr (Memc[infile])
			call pargstr (Memc[outfile])
		}
		call flush (STDOUT)
	    }

	    if (naxis == 1) {
		in = imgl1r (im1)
		out = impl1r (im2)
		call ft_shift (npts[1], shift[1], Memr[in], Memr[out])

	    } else {
		outline = 1 + shift[2]		# target for line 1 of input
		outline = mod (outline, npts[2])

		do inline = 1, npts[2] {
		    if (outline < 1)
			outline = outline + npts[2]
		    if (outline > npts[2])
			outline = outline - npts[2]

		    in = imgl2r (im1, inline)
		    out = impl2r (im2, outline)

		    call ft_shift (npts[1], shift[1], Memr[in], Memr[out])

		    outline = outline + 1
		}
	    }

	    # Adjust crpix.
	    do k = 1, naxis {
		call sprintf (pname, SZ_PNAME, "crpix%d")
		    call pargi (k)
		if (imaccf (im1, pname) == YES) {
		    crpix = imgetr (im1, pname)		# get from input
		    crpix = crpix + shift[k]
		    call imputr (im2, pname, crpix)	# replace in output
		}
	    }

	    call imunmap (im1)
	    call imunmap (im2)

	    if (in_place) {
		iferr (call imdelete (Memc[infile])) {
		    call eprintf ("No privilege to clobber input file;\n")
		    call eprintf ("shifted file is called %s\n")
			call pargstr (Memc[outfile])
		} else {
		    # Memc[outfile] is the name of the scratch file.
		    call imrename (Memc[outfile], Memc[infile])
		}
	    }
	}

	call imtclose (list1)

	if ( ! blank_output )
	    call imtclose (list2)

	call sfree (sp)
end

define	SZ_SH_STR	15	# length of a short string
define	NUMBER		1
define	CENTER		2
define	DECENTER	3

procedure ft_g_shift (naxis, npts, max_naxis, instr, shift)

int	naxis			# i: dimension of image
int	npts[naxis]		# i: length of each axis
int	max_naxis		# i: max value of naxis
char	instr[ARB]		# i: string containing shift(s)
int	shift[max_naxis]	# o: amount of shift
#--
char	word[SZ_SH_STR]
int	ip, ipn, token, nchar
int	k
int	do_what			# number, center, or decenter
int	num_words		# number of words or numbers read
bool	done
int	ctotok(), ctoi()

begin
	do_what = NULL
	num_words = 0
	do k = 1, max_naxis
	    shift[k] = 0		# initial values

	k = 1
	ip = 1

	done = false
	while ( ! done ) {

	    token = ctotok (instr, ip, word, SZ_SH_STR)
	    if (k > naxis || token == TOK_EOS || token == TOK_NEWLINE) {

		done = true

	    } else {

		if (token == TOK_NUMBER) {
		    ipn = 1
		    nchar = ctoi (word, ipn, shift[k])
		    shift[k] = mod (shift[k], npts[k])
		    do_what = NUMBER
		    num_words = num_words + 1
		    k = k + 1
		} else if (word[1] == 'c' || word[1] == 'C') {
		    shift[k] = npts[k] / 2	# shift right to center
		    do_what = CENTER
		    num_words = num_words + 1
		    k = k + 1
		} else if (word[1] == 'd' || word[1] == 'D') {
		    shift[k] = - npts[k] / 2	# shift left to decenter
		    do_what = DECENTER
		    num_words = num_words + 1
		    k = k + 1
		} else if (token == TOK_PUNCTUATION) {
		    ;
		} else {
		    call error (1, "don't understand shift")
		}
	    }
	}

	# Take care of the case that e.g. shift = "center" rather than
	# shift = "center center" for a 2-D image.
	if (num_words < naxis) {
	    if (do_what == CENTER) {
		do k = num_words+1, naxis
		    shift[k] = npts[k] / 2
	    } else if (do_what == DECENTER) {
		do k = num_words+1, naxis
		    shift[k] = - npts[k] / 2
	    } else if (do_what == NUMBER) {
		call error (1, "must specify a shift for each axis")
	    } else {
		call error (1, "no shift specified")
	    }
	}
end

# ft_shift -- shift a 1-D array
# This routine copies an array from input to output, shifting by an
# integral number of pixels.  The actual arguments corresponding to
# the input and output arrays must be distinct.

procedure ft_shift (npts, shift, in, out)

int	npts		# i: size of arrays
int	shift		# i: amount of shift
real	in[npts]	# i: input array
real	out[npts]	# o: output array
#--
int	sh		# abs (shift)
int	n		# npts - abs(shift)

begin
	if (shift > 0) {

	    n = npts - shift

	    call amovr (in, out[shift+1], n)
	    call amovr (in[n+1], out, shift)

	} else if (shift < 0) {

	    sh = abs (shift)
	    n = npts - sh

	    call amovr (in, out[n+1], sh)
	    call amovr (in[sh+1], out, n)

	} else {

	    call amovr (in, out, npts)
	}
end

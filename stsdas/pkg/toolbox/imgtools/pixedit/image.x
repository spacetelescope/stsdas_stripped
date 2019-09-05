include	<mach.h>
include <imio.h>
include <imhdr.h>
include <ctype.h>
include	<curses.h>
include "pixedit.h"
include "object.h"
include "message.h"
include "image.h"
include "lex.h"

# IMG_BUFFER -- Allocate buffer to store image pixels

procedure img_buffer (img)

pointer	img		# i: image descriptor
#--
int	win, height, width
pointer	id, im

string	notopen  "img_buffer: image is not open"

begin
	# Get image and window descriptors

	id = OBJ_DESCRIP(img)
	im = IMG_DESCRIP(id)
	win = IMG_WINDOW(id)

	# If image is not open exit with error

	if (IMG_DESCRIP(id) == NULL) {
	    call img_error (img, notopen)
	    return
	}

	# Deallocate old buffer that store pixels

	if (IMG_BUFPTR(id) != NULL) {
	    if (IMG_UPDBUF(id) == YES)
		call img_wbuf (img)
		call mfree (IMG_BUFPTR(id), IMG_TYPE(id))
	}

	# Allocate buffer to store image pixels

	call wdimen (win, height, width)

	if (IM_NDIM(im) == 1) {
	    IMG_NROW(id) = height - TITLE_HEIGHT
	    IMG_NCOL(id) = (width - 1) / IMG_CWIDTH(id)
	} else {
	    IMG_NROW(id) = (width - 1) / IMG_CWIDTH(id)
	    IMG_NCOL(id) = height - TITLE_HEIGHT
	}

	IMG_NROW(id) = min (IMG_NROW(id), IM_LEN(im,1))
	IMG_NCOL(id) = min (IMG_NCOL(id), IM_LEN(im,2))
	
	call malloc (IMG_BUFPTR(id), IMG_NROW(id)*IMG_NCOL(id), IMG_TYPE(id))

end

# IMG_CREATE -- Create a new image

procedure img_create (img, image, rdonly, inplace)

pointer	img		# i: image descriptor
char	image[ARB]	# i: image name
int	rdonly		# i: is image read only?
int	inplace		# i: edit image in place?
#--
pointer	id

begin
	# Allocate the descriptor

	call malloc (id, LEN_IMGSTRUCT, TY_STRUCT)
	OBJ_DESCRIP(img) = id

	# Fill in fields with default values

	IMG_WINDOW(id) = 0
	IMG_HIDDEN(id) = NO
	IMG_DESCRIP(id) = NULL
	IMG_RDONLY(id) = rdonly
	IMG_INPLACE(id) = inplace
	IMG_UPDFILE(id) = NO
	IMG_UPDBUF(id) = NO
	IMG_UPDPIX(id) = NO
	IMG_TYPE(id) = TY_DOUBLE
	IMG_NROW(id) = 0
	IMG_NCOL(id) = 0
	IMG_CWIDTH(id) = 10
	IMG_LOROW(id) = 1
	IMG_HIROW(id) = 0
	IMG_LOCOL(id) = 1
	IMG_HICOL(id) = 0
	IMG_CURROW(id) = 1
	IMG_CURCOL(id) = 1
	IMG_CURPLANE(id) = 1
	IMG_STRIDX(id) = 0
	IMG_STRLEN(id) = 0
	IMG_PREV(id) = img
	IMG_NEXT(id) = img
	IMG_BUFPTR(id) = NULL

	# Allocate and initialize strings in structure

	call malloc (IMG_NAMPTR(id), SZ_FNAME, TY_CHAR)
	call malloc (IMG_STRPTR(id), IMG_MAXSTR, TY_CHAR)
	call malloc (IMG_DELPTR(id), IMG_MAXSTR, TY_CHAR)
	call malloc (IMG_FMTPTR(id), IMG_MAXSTR, TY_CHAR)

	IMG_STR(id,0) = EOS
	IMG_DELBUF(id,0) = EOS
	## This code does not handle image sections now
	call img_namcpy (image, IMG_NAME(id), SZ_FNAME)

	# Open the image and set fields in structure

	call img_map (img)

end

# IMG_DESTROY -- Destroy an image

procedure img_destroy (img)

pointer	img		# i: image descriptor
#--
pointer	id, im, prev, nxt, pd, nd

begin
	id = OBJ_DESCRIP(img)

	if (id != NULL) {
	    # Free image window

	    if (IMG_WINDOW(id) != 0)
		call delwin (IMG_WINDOW(id))

	    # Close the image if still open

	    if (IMG_DESCRIP(id) != NULL) {
		im = IMG_DESCRIP(id)
		IMG_DESCRIP(id) = NULL

		call imgcluster (IM_NAME(im), IMG_NAME(id), SZ_FNAME)
		call imunmap (im)
	    }

	    # Delete the temporary image

	    if (IMG_INPLACE(id) == NO)
		call imdelete (IMG_NAME(id))

	    # Update pointers to ring of images

	    prev = IMG_PREV(id)
	    nxt = IMG_NEXT(id)

	    pd = OBJ_DESCRIP(prev)
	    nd = OBJ_DESCRIP(nxt)

	    IMG_NEXT(pd) = IMG_NEXT(id)
	    IMG_PREV(nd) = IMG_PREV(id)

	    # Deallocate structure

	    call mfree (IMG_NAMPTR(id), TY_CHAR)
	    call mfree (IMG_STRPTR(id), TY_CHAR)
	    call mfree (IMG_DELPTR(id), TY_CHAR)
	    call mfree (IMG_FMTPTR(id), TY_CHAR)
	    call mfree (IMG_BUFPTR(id), IMG_TYPE(id))
	    call mfree (id, TY_STRUCT)

	    OBJ_DESCRIP(img) = NULL
	}

	# Free object structure

	call obj_destroy (img)
end

# IMG_DIRTY -- Find the next image which has been modified

pointer procedure img_dirty ()

#--
pointer	dirty, term, osearch, obj, id

pointer	trm_info(), obj_search(), obj_next()

begin
	dirty = NULL

	# Look for the first image which has been modified

	term = trm_info (T_TERM)
	osearch = obj_search (term)

	repeat{ 
	    obj = obj_next (osearch)
	    if (obj == NULL)
		break

	    # Check to see if object is an image which has been modified

	    if (OBJ_KIND(obj) == O_IMAGE) {
		id = OBJ_DESCRIP(obj)
		if (IMG_UPDFILE(id) == YES) {
		    dirty = obj
		    break
		}
	    }
	}

	call obj_done (osearch)
	return (dirty)

end

# IMG_DPIXEL -- Redraw a single pixel

procedure img_dpixel (img, row, col)

pointer	img		# i: image window descriptor
int	row		# i: pixel row
int	col		# i: pixel column
#--
int	win, opixrow, opixcol, pixrow, pixcol
pointer	sp, id, value, focus

int	trm_info(), winstat()

begin
	call smark (sp)
	call salloc (value, IMG_MAXSTR, TY_CHAR)

	id = OBJ_DESCRIP(img)
	win = IMG_WINDOW(id)

	if (row < IMG_LOROW(id) || row > IMG_HIROW(id) ||
	    col < IMG_LOCOL(id) || col > IMG_HICOL(id)   )
	    return

	call img_rpixel (img, row, col, Memc[value], IMG_MAXSTR)

	opixrow = winstat (win, W_CURROW)
	opixcol = winstat (win, W_CURCOL)

	call img_pixpos (img, row, col, pixrow, pixcol)
	call wmove (win, pixrow, pixcol)
	call waddstr (win, Memc[value])

	focus = trm_info (T_FOCUS)
	if (focus == img)
	    call wmove (win, opixrow, opixcol)

	call sfree (sp)
end

# IMG_DRAW -- Redraw the image window

procedure img_draw (img)

pointer	img		# i: Image window descriptor
#--
int	win, maxrow, maxcol, nrows, ncols
int	irow, icol, ic
pointer	sp, id, im, term, line, item

string	notopen  "img_draw: image is not open"
string	badsize  "img_draw: bad sceen dimensions"

int	gstrcpy()
pointer	trm_info()

begin

	id = OBJ_DESCRIP(img)
	win = IMG_WINDOW(id)

	# If image is not open exit with error

	if (IMG_DESCRIP(id) == NULL) {
	    call img_error (img, notopen)
	    return
	}

	# If window is hidden, send message to bring it to front

	if (IMG_HIDDEN(id) == YES) {
	    term = trm_info (T_TERM)
	    call msg_send (T_FRONT, term, img, NULL)
	    return
	}

	# Get size of window and allocate strings based on size

	call wdimen (IMG_WINDOW(id), nrows, ncols)

	call smark (sp)
	call salloc (line, ncols+1, TY_CHAR)
	call salloc (item, IMG_CWIDTH(id), TY_CHAR)

	# Write data back to image if it has been changed

	if (IMG_UPDBUF(id) == YES)
	    call img_wbuf (img)

	# Calculate number of rows and columns in image

	im = IMG_DESCRIP(id)

	maxrow = IM_LEN(im,1)
	maxcol = IM_LEN(im,2)

	# Set the limits of the rows and columns to display

	if (IMG_CURROW(id) <= IMG_NROW(id) / 2) {
	    IMG_LOROW(id) = 1
	    IMG_HIROW(id) = min (maxrow, IMG_NROW(id))

	} else if (IMG_CURROW(id) >= (maxrow - IMG_NROW(id) / 2)) {
	    IMG_HIROW(id) = maxrow
	    IMG_LOROW(id) = max (1, maxrow - (IMG_NROW(id) - 1))

	} else {
	    IMG_LOROW(id) = IMG_CURROW(id) - IMG_NROW(id) / 2
	    IMG_HIROW(id) = IMG_NROW(id) +  (IMG_LOROW(id) - 1)
	}

	if (IMG_CURCOL(id) <= IMG_NCOL(id) / 2) {
	    IMG_LOCOL(id) = 1
	    IMG_HICOL(id) = min (maxcol, IMG_NCOL(id))

	} else if (IMG_CURCOL(id) >= (maxcol - IMG_NCOL(id) / 2)) {
	    IMG_HICOL(id) = maxcol
	    IMG_LOCOL(id) = max (1, maxcol - (IMG_NCOL(id) - 1))

	} else {
	    IMG_LOCOL(id) = IMG_CURCOL(id) - IMG_NCOL(id) / 2
	    IMG_HICOL(id) = IMG_NCOL(id) +  (IMG_LOCOL(id) - 1)
	}

	if (IMG_LOROW(id) > IMG_HIROW(id) ||
	    IMG_LOCOL(id) > IMG_HICOL(id)   ) {
	    call img_error (img, badsize)
	    call sfree (sp)
	    return
	}

	# Read new data in from the image

	call img_rbuf (img)

	# Erase the current contents of the window

	call werase (win)
	call wmove (win, 1, 1)

	# Redraw the window a line at a time

	if (IM_NDIM(im) == 1) {
	    do irow = IMG_HIROW(id), IMG_LOROW(id), -1 {
		call img_rpixel (img, irow, 1, Memc[line], ncols)
		call strcat ("\n", Memc[line], ncols)
		call waddstr (win, Memc[line])
	    }

	} else {
	    do icol = IMG_HICOL(id), IMG_LOCOL(id), -1 {
		ic = 0
		do irow = IMG_LOROW(id), IMG_HIROW(id) {
		    call img_rpixel (img, irow, icol, Memc[item], IMG_MAXSTR)
		    ic = ic + gstrcpy (Memc[item], Memc[line+ic], ncols-ic)
		}

		Memc[line+ic] = '\n'
		Memc[line+ic+1] = EOS
		call waddstr (win, Memc[line])
	    }
	}

	# Set the cursor at the proper pixel
	
	call img_move (img, 1, IMG_CURROW(id), IMG_CURCOL(id), 
		       IMG_CURPLANE(id))

	# Flush screen to terminal

	call wrefresh (win)	
	call sfree (sp)
end

# IMG_ERROR -- Send a fatal error message from the image routines

procedure img_error (img, msg)

pointer	img		    # i: image descriptor
char	msg[ARB]    # c: error message
#--
pointer	id, logger, errmsg1, errmsg2

pointer	trm_info()

begin
	id = OBJ_DESCRIP(img)
	logger = trm_info (T_LOG)

	call msg_alloc (errmsg1, SZ_FNAME, TY_CHAR)
	call msg_alloc (errmsg2, SZ_FNAME, TY_CHAR)

	call strcpy (msg, Memc[errmsg1], SZ_FNAME)
	call strcpy (IMG_NAME(id), Memc[errmsg2], SZ_FNAME)

	call msg_send (L_ERROR, logger, errmsg1, errmsg2)
end

# IMG_FMT -- Change image format

procedure img_fmt (img, format)

pointer	img		# i: image descriptor
char	format[ARB]	# i: new format string
#--
int	pad, width, dec, type
pointer	id

string	badformat  "img_fmt: invalid display format"

int	lex_format()

begin
	id = OBJ_DESCRIP(img)

	# One last check of the format

	if (lex_format (format, pad, width, dec, type) != LEX_OKAY) {
	    call img_error (img, badformat)
	    return
	}

	# Set column width and format

	IMG_CWIDTH(id) = width
	call strcpy (format, IMG_FORMAT(id), IMG_MAXSTR)

	# Allocate new pixel buffer

	call img_buffer (img)

	# Redraw image

	call img_draw (img)
end

# IMG_INFO -- Get information from an image window

int procedure img_info (img, what)

pointer	img		# i: image window descriptor
int	what		# i: requested item of information
#--
int	info
pointer	id, im, errmsg, logger

string	badwhat    "img_info: unknown item of info requested"

pointer	trm_info()

begin
	id = OBJ_DESCRIP(img)

	switch (what) {
	case I_NAME:
	    info = IMG_NAMPTR(id)

	case I_RDONLY:
	    info = IMG_RDONLY(id)

	case I_INPLACE:
	    info = IMG_INPLACE(id)

	case I_UPFILE:
	    info = IMG_UPDFILE(id)

	case I_NAXIS:
	    im = IMG_DESCRIP(id)
	    info = IM_NDIM(im)

	case I_NAXIS1:
	    im = IMG_DESCRIP(id)
	    info = IM_LEN(im,1)

	case I_NAXIS2:
	    im = IMG_DESCRIP(id)
	    info = IM_LEN(im,2)

	case I_NAXIS3:
	    im = IMG_DESCRIP(id)
	    info = IM_LEN(im,3)

	case I_AXIS1:
	    info = IMG_CURROW(id)

	case I_AXIS2:
	    info = IMG_CURCOL(id)

	case I_AXIS3:
	    info = IMG_CURPLANE(id)

	default:
	    info = 0

	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badwhat, Memc[errmsg], SZ_LINE)

	    logger = trm_info (T_LOG)
	    call msg_send (L_ERROR, logger, errmsg, NULL)
	}

	return (info)
end

# IMG_LINK -- Search to find the same image on the screen

pointer procedure img_link (img)

pointer	img		# i: image descriptor
#--
pointer	term, osearch, obj, id, od

bool	streq()
pointer	trm_info(), obj_search(), obj_next()

begin
	# Start search at root object (terminal)

	term = trm_info (T_TERM)
	osearch = obj_search (term)
	id = OBJ_DESCRIP(img)

	# Look for first match to image name

	repeat{ 
	    obj = obj_next (osearch)
	    if (obj == NULL)
		break

	    if (obj != img) {
		if (OBJ_KIND(obj) == O_IMAGE) {
		    od = OBJ_DESCRIP(obj)
		    if (streq (IMG_NAME(id), IMG_NAME(od)))
			break
		}
	    }
	}

	# Return pointer to object, or NULL if not found

	call obj_done (osearch)
	return (obj)

end

# IMG_MAP -- Map an image into its structure

procedure img_map (img)

pointer	img		# i: image descriptor
#--
int	mode, junk, try
pointer	sp, dir, ext, fname, link, id, ld

string	noread   "Image does not exist"
string	nowrite  "No write access to image"
string	nocopy   "Cannot create temporary image"

int	imaccess(), fnldir(), fnextn()
pointer	img_link(), immap()

begin
	call smark (sp)
	call salloc (dir, SZ_FNAME, TY_CHAR)
	call salloc (ext, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	id = OBJ_DESCRIP(img)

	# Check to see if image is already read in another window

	link = img_link (img)
	if (link != NULL) {
	    # If so, set image descriptor to values in other window

	    ld = OBJ_DESCRIP(link)

	    IMG_DESCRIP(id) = IMG_DESCRIP(ld)
	    IMG_UPDFILE(id) = IMG_UPDFILE(ld)
	    IMG_UPDBUF(id) = NO
	    IMG_UPDPIX(id) = NO

	    IMG_LOROW(id) = IMG_LOROW(ld)
	    IMG_LOCOL(id) = IMG_LOCOL(ld)

	    IMG_CURROW(id) = IMG_CURROW(ld)
	    IMG_CURCOL(id) = IMG_CURCOL(ld)
	    IMG_CURPLANE(id) = IMG_CURPLANE(ld)

	    IMG_PREV(id) = link
	    IMG_NEXT(id) = IMG_NEXT(ld)
	    IMG_NEXT(ld) = img

	    IMG_CWIDTH(id) = IMG_CWIDTH(ld)
	    call strcpy (IMG_FORMAT(ld), IMG_FORMAT(id), IMG_MAXSTR)

	} else {
	    # Otherwise, read image for first time
	    # Check image access first

	    if (imaccess (IMG_NAME(id), READ_ONLY) == NO) {
		# Don't handle case of creating new image yet
		call img_error (img, noread)
		call sfree (sp)
		return

	    } else if (imaccess (IMG_NAME(id), READ_WRITE) == NO && 
		       IMG_RDONLY(id) == NO) {
		# No write access to image
		call img_error (img, nowrite)
		call sfree (sp)
		return

	    } else {
		# Set access mode

		if (IMG_RDONLY(id) == YES) {
		    mode = READ_ONLY
		    IMG_INPLACE(id) = YES
		} else {
		    mode = READ_WRITE
		}

		# Create temporary image name if not editing in place

		if (IMG_INPLACE(id) == YES) {
		    IMG_DESCRIP(id) = immap (IMG_NAME(id), mode, 0)

		} else {
		    call imgcluster (IMG_NAME(id), Memc[fname], SZ_FNAME)
		    junk = fnldir (Memc[fname], Memc[dir], SZ_FNAME)
		    junk = fnextn (Memc[fname], Memc[ext], SZ_FNAME)

		    # First try to put temporary image in same directory
		    # with image, then try to put in tmp$

		    for (try = 1; try <= 2; try = try + 1) {
			call strcat ("pix", Memc[dir], SZ_FNAME)
			call mktemp (Memc[dir], Memc[fname], SZ_FNAME)

			call strcat (".", Memc[fname], SZ_FNAME)
			call strcat (Memc[ext], Memc[fname], SZ_FNAME)

			ifnoerr (call copyimg (IMG_NAME(id), Memc[fname]))
			    break
			call strcpy ("tmp$", Memc[dir], SZ_FNAME)
		    }

		    if (try > 2) {
			call img_error (img, nocopy)
			call sfree (sp)
			return
		    } else {
			IMG_DESCRIP(id) = immap (Memc[fname], mode, 0)
		    }
		}

		# Set default format for screen display

		IMG_TYPE(id) = IM_PIXTYPE(IMG_DESCRIP(id))
		switch (IMG_TYPE(id)) {
		case TY_SHORT:
		    call strcpy ("%-7d", IMG_FORMAT(id), IMG_MAXSTR)
		    IMG_CWIDTH(id) = 7
		case TY_INT, TY_LONG:
		    call strcpy ("%-10d", IMG_FORMAT(id), IMG_MAXSTR)
		    IMG_CWIDTH(id) = 10
		case TY_REAL, TY_DOUBLE:
		    call strcpy ("%-10.3g", IMG_FORMAT(id), IMG_MAXSTR)
		    IMG_CWIDTH(id) = 10
		case TY_USHORT:
		    call strcpy ("%-7d", IMG_FORMAT(id), IMG_MAXSTR)
		    IMG_TYPE(id) = TY_LONG
		    IMG_CWIDTH(id) = 7
		default:
		    call strcpy ("%-10.3g", IMG_FORMAT(id), IMG_MAXSTR)
		    IMG_TYPE(id) = TY_DOUBLE
		    IMG_CWIDTH(id) = 10
		}
	    }
	}

	call sfree (sp)
end

# IMG_MOVE -- Move the cursor to a new pixel

procedure img_move (img, index, row, col, plane)

pointer	img		# i: Image window descriptor
int	index		# i: index to character within pixel
int	row		# i: new pixel row
int	col		# i: new pixel column
int	plane		# i: new pixel plane
#--
int	win, nrow, ncol, pixrow, pixcol, idx
pointer	id, im, sp, logger, title

int	trm_info(), strlen()

begin
	id = OBJ_DESCRIP(img)
	win = IMG_WINDOW(id)

	IMG_CURROW(id) = row
	IMG_CURCOL(id) = col
	IMG_CURPLANE(id) = plane

	if (row < IMG_LOROW(id) || row > IMG_HIROW(id) ||
	    col < IMG_LOCOL(id) || col > IMG_HICOL(id) ||
	    plane != IMG_CURPLANE(id)) {

	    call msg_send (W_REDRAW, img, NULL, NULL)

	    logger = trm_info (T_LOG)
	    call msg_send (L_CLEAR, logger, NULL, NULL)

	    return
	}

	# Write title line at bottom of screen

	call wdimen (win, nrow, ncol)

	call smark (sp)
	call salloc (title, ncol, TY_CHAR)

	im = IMG_DESCRIP(id)
	switch (IM_NDIM(im)) {
	case 1:
	    call sprintf (Memc[title], ncol, " %s[%d]%*t")
	    call pargstr (IMG_NAME(id))
	    call pargi (row)
	    call pargi (ncol)
	case 2:
	    call sprintf (Memc[title], ncol, " %s[%d,%d]%*t")
	    call pargstr (IMG_NAME(id))
	    call pargi (row)
	    call pargi (col)
	    call pargi (ncol)
	default:
	    call sprintf (Memc[title], ncol, " %s[%d,%d,%d]%*t")
	    call pargstr (IMG_NAME(id))
	    call pargi (row)
	    call pargi (col)
	    call pargi (plane)
	    call pargi (ncol)
	}

	call wstandout (win)
	call wmove (win, nrow, 1)
	call waddstr (win, Memc[title])
	call wstandend (win)

	# Update fields describing pixel string

	call img_rpixel (img, row, col, IMG_STR(id,0), IMG_MAXSTR)
	call img_trim (IMG_STR(id,0))

	IMG_DELBUF(id,0) = EOS
	IMG_STRLEN(id) = strlen (IMG_STR(id,0))

	idx = max (1, index)
	idx = min (idx, IMG_STRLEN(id)+1)
	IMG_STRIDX(id) = idx - 1

	# Calculate cursor position and move cursor

	call img_pixpos (img, row, col, pixrow, pixcol)
	call wmove (win, pixrow, pixcol+idx-1)

	call sfree (sp)
end

# IMG_NAMCPY -- Copy the name of an image to a string

procedure img_namcpy (old, new, maxch)

char	old[ARB]	# i: old image name
char	new[ARB]	# o: new image name
int	maxch		# i: length of new name
#--
bool	brak, slash
int	ic, jc

begin
	brak = false
	slash = false

	jc = 1
	for (ic = 1; old[ic] != EOS && jc <= maxch; ic = ic + 1) {
	    if (! brak) {
		if (old[ic] == '[')
		    brak = true

		new[jc] = old[ic]
		jc = jc + 1

	    } else {
		if (old[ic] == ']') {
		    brak = false
		    slash = false
		} else if (old[ic] == '/') {
		    slash = true
		}

		if (! slash) {
		    new[jc] = old[ic]
		    jc = jc + 1
		}
	    }
	}

	new[jc] = EOS
end

# IMG_PIXPOS -- Compute the location of a pixel in a window

procedure img_pixpos (img, row, col, pixrow, pixcol)

pointer	img		# i: image window descriptor
int	row		# i: pixel row in image
int	col		# i: pixel column in image
int	pixrow		# o: pixel row in window
int	pixcol		# o: pixel column in window
#--
pointer	id, im

begin
	id = OBJ_DESCRIP(img)
	im = IMG_DESCRIP(id)

	if (IM_NDIM(im) == 1) {
	    pixrow = 1 + IMG_HIROW(id) - row
	    pixcol = IMG_CWIDTH(id) * (col - IMG_LOCOL(id)) + 1
	} else {
	    pixrow = 1 + IMG_HICOL(id) - col
	    pixcol = IMG_CWIDTH(id) * (row - IMG_LOROW(id)) + 1
	}

end

# IMG_RBUF -- Read data buffer from the image

procedure img_rbuf (img)

pointer	img		# i: Image window descriptor
#--
int	nbuf
pointer	id, im, vec, buf

string	noflush  "img_rbuf: pixel buffer was not flushed"

pointer	imgs3s(), imgs3i(), imgs3l(), imgs3r(), imgs3d()

begin
	id = OBJ_DESCRIP(img)
	im = IMG_DESCRIP(id)

	if (IMG_UPDBUF(id) == YES) {
	    call img_error (img, noflush)
	    return
	}

	# Read the image according to the data type

	buf = IMG_BUFPTR(id)
	nbuf = IMG_NROW(id) * IMG_NCOL(id)

	switch (IMG_TYPE(id)) {
	case TY_SHORT:
	    vec = imgs3s (im, IMG_LOROW(id), IMG_HIROW(id), 
			  IMG_LOCOL(id), IMG_HICOL(id),
			  IMG_CURPLANE(id), IMG_CURPLANE(id))
	    call amovs (Mems[vec], Mems[buf], nbuf)
	case TY_INT:
	    vec = imgs3i (im, IMG_LOROW(id), IMG_HIROW(id), 
			  IMG_LOCOL(id), IMG_HICOL(id),
			  IMG_CURPLANE(id), IMG_CURPLANE(id))
	    call amovi (Memi[vec], Memi[buf], nbuf)
	case TY_LONG:
	    vec = imgs3l (im, IMG_LOROW(id), IMG_HIROW(id), 
			  IMG_LOCOL(id), IMG_HICOL(id),
			  IMG_CURPLANE(id), IMG_CURPLANE(id))
	    call amovl (Meml[vec], Meml[buf], nbuf)
	case TY_REAL:
	    vec = imgs3r (im, IMG_LOROW(id), IMG_HIROW(id), 
			  IMG_LOCOL(id), IMG_HICOL(id),
			  IMG_CURPLANE(id), IMG_CURPLANE(id))
	    call amovr (Memr[vec], Memr[buf], nbuf)
	case TY_DOUBLE:
	    vec = imgs3d (im, IMG_LOROW(id), IMG_HIROW(id), 
			  IMG_LOCOL(id), IMG_HICOL(id),
			  IMG_CURPLANE(id), IMG_CURPLANE(id))
	    call amovd (Memd[vec], Memd[buf], nbuf)
	}

end

# IMG_RDCUR -- Read the image cursor

procedure img_rdcur (img, key, row, col, command)

pointer	img		# i: image window descriptor
int	key		# i: key press which ended cursor read
int	row		# u: cursor row
int	col		# u: cursor column
char	command[ARB]	# i: cursor command
#--
int	cmdlen
pointer	sp, errmsg, term, logger, info, cmd, id, im

string	badkey  "Illegal cursor key. Press ? for help."

int	trm_info(), strlen()

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	id = OBJ_DESCRIP(img)
	im = IMG_DESCRIP(id)
	term = trm_info (T_TERM)

	# Make sure cursor position is within image bounds

	if (row < 1) {
	    row = 1
	} else if (row > IM_LEN(im,1)) {
	    row = IM_LEN(im,1)
	}

	if (col < 1) {
	    col = 1
	} else if (col > IM_LEN(im,2)) {
	    col = IM_LEN(im,2)
	}

	switch (key) {
	case ' ','g':
	    call msg_send (T_USEKEY, term, NULL, NULL)

	    call msg_alloc (info, LEN_IPOSSTRUCT, TY_STRUCT)

	    IPOS_INDEX(info) = 1
	    IPOS_ROW(info) = row
	    IPOS_COL(info) = col
	    IPOS_PLANE(info) = IMG_CURPLANE(id)

	    call msg_send (I_MOVE, img, info, NULL)

	case '?':
	    call msg_send (T_USEKEY, term, NULL, NULL)
	    call msg_send (T_ADDWIN, term, O_HELP, NULL)

	case ':':
	    call msg_send (T_USEKEY, term, NULL, NULL)

	    cmdlen = strlen (command)
	    call msg_alloc (info, LEN_CNEWSTRUCT, TY_STRUCT)
	    call msg_alloc (cmd, cmdlen, TY_CHAR)

	    call strcpy (command, Memc[cmd], cmdlen)
	    CNEW_CODE(info) = CMD_ANY
	    CNEW_STRPTR(info) = cmd

	    call msg_send (T_ADDWIN, term, O_CMD, info)

	default:
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badkey, Memc[errmsg], SZ_LINE)

	    logger = trm_info (T_LOG)
	    call msg_send (L_WARN, logger, errmsg, NULL)
	}

	call sfree (sp)
end

# IMG_RDKEY -- Read a keystroke from the keyboard

procedure img_rdkey (img, ch, done)

pointer	img		# i: image window descriptor
int	ch		# i: character read
int	done		# o: finished reading string?
#--
char	blank
int	win, nrows, ncols, ic, mc, nc
int	pixrow, pixcol, pgsize
pointer	id, im, term, logger, info, errmsg

data	blank	 / ' ' /

string	fullfld  "Maximum field length  exceeded"
string	nowrite  "Cannot change field: image is read only"

int	winstat(), strlen()
pointer	trm_info()

begin
	done = NO

	id = OBJ_DESCRIP(img)
	im = IMG_DESCRIP(id)
	win = IMG_WINDOW(id)

	pixrow = winstat (win, W_CURROW)
	pixcol = winstat (win, W_CURCOL) - IMG_STRIDX(id)

	if (IS_PRINT(ch)) {
	    if (IMG_RDONLY(id) == YES) {
		goto 90

	    } else {
		# Truncate fields longer than column width

		if (IMG_STRLEN(id) >= IMG_CWIDTH(id)) {
		    nc = IMG_CWIDTH(id) - 1
		    IMG_STRLEN(id) = nc
		    IMG_STR(id,nc) = EOS

		    if (IMG_STRIDX(id) > nc)
			IMG_STRIDX(id) = nc
		}

		if (IMG_STRIDX(id) == IMG_STRLEN(id)) {
		    IMG_CURCHAR(id) = ch
		    IMG_NXTCHAR(id) = EOS

		} else {
		    nc = IMG_STRLEN(id) - IMG_STRIDX(id)
		    call amovc (IMG_CURCHAR(id), IMG_NXTCHAR(id), nc+1)

		    IMG_CURCHAR(id) = ch
		}

		call waddstr (win, IMG_CURCHAR(id))

		IMG_UPDPIX(id) = YES
		IMG_STRIDX(id) = IMG_STRIDX(id) + 1
		IMG_STRLEN(id) = IMG_STRLEN(id) + 1
		call wmove (win, pixrow, pixcol + IMG_STRIDX(id)) 
	    }

	} else if (ch == '\r') {
	    call img_relmov (img, POS_LEFT, MV_DOWN, 1, done)

	} else if (ch == '\t') {
	    call img_relmov (img, POS_LEFT, MV_RIGHT, 1, done)

	} else {
	    switch (ch) {
	    case K_UP:  # Move up one field
		call img_relmov (img, POS_CENTER, MV_UP, 1, done)

	    case K_DOWN:  # Move down one field
		call img_relmov (img, POS_CENTER, MV_DOWN, 1, done)

	    case K_RIGHT:  # Move right one column
		call img_relmov (img, POS_CENTER, MV_RIGHT, 0, done)

	    case K_LEFT:  # Move left one column
		call img_relmov (img, POS_CENTER, MV_LEFT, 0, done)

	    case K_NEXTW:  # Move forwards one word
		call img_relmov (img, POS_LEFT, MV_RIGHT, 1, done)

	    case K_PREVW:  # Move backwards one word
		call img_relmov (img, POS_RIGHT, MV_LEFT, 1, done)

	    case K_NEXTP:  # Move forwards one screen
		call wdimen (win, nrows, ncols)
		pgsize = nrows - TITLE_HEIGHT
		call img_relmov (img, POS_LEFT, MV_DOWN, pgsize, done)

	    case K_PREVP:  # Move backwards one screen
		call wdimen (win, nrows, ncols)
		pgsize = nrows - TITLE_HEIGHT
		call img_relmov (img, POS_LEFT, MV_UP, pgsize, done)

	    case K_HOME:  # Move to first field
		call img_relmov (img, POS_LEFT, MV_UP, ARB, done)

	    case K_END:  # Move to last field
		call img_relmov (img, POS_LEFT, MV_DOWN, ARB, done)

	    case K_BOL:  # Move to first field in line
		call img_relmov (img, POS_LEFT, MV_LEFT, ARB, done)

	    case K_EOL:  # Move to last field in line
		call img_relmov (img, POS_RIGHT, MV_RIGHT, ARB, done)

	    case K_DEL:  # Delete character underneath cursor
		if (IMG_RDONLY(id) == YES) {
		    goto 90

		} else if (IMG_STRIDX(id) < IMG_STRLEN(id)) {
		    IMG_UPDPIX(id) = YES

		    ic = IMG_STRIDX(id)
		    mc = strlen (IMG_DELBUF(id,0))
		    nc = IMG_STRLEN(id) - ic

		    IMG_DELBUF(id,mc) = IMG_STR(id,ic)
		    IMG_DELBUF(id,mc+1) = EOS

		    call amovc (IMG_STR(id,ic+1), IMG_STR(id,ic), nc)

		    nc = IMG_STRLEN(id)
		    IMG_STR(id,nc-1) = blank
		    IMG_STR(id,nc) = EOS

		    call waddstr (win, IMG_CURCHAR(id))

		    IMG_STR(id,nc-1) = EOS
		    IMG_STRLEN(id) = IMG_STRLEN(id) - 1
		    call wmove (win, pixrow, pixcol + IMG_STRIDX(id)) 
		}

	    case K_BS:  # Delete character to left of cursor
 		if (IMG_RDONLY(id) == YES) {
		    goto 90

		} else if (IMG_STRIDX(id) > 0) {
		    IMG_UPDPIX(id) = YES

		    ic = IMG_STRIDX(id)
		    mc = strlen (IMG_DELBUF(id,0))
		    nc = IMG_STRLEN(id) - ic

		    call amovc (IMG_DELBUF(id,0), IMG_DELBUF(id,1), mc+1)
		    IMG_DELBUF(id,0) = IMG_STR(id,ic-1)

		    call amovc (IMG_STR(id,ic), IMG_STR(id,ic-1), nc)

		    nc = IMG_STRLEN(id)
		    IMG_STR(id,nc-1) = blank
		    IMG_STR(id,nc) = EOS

		    IMG_STRIDX(id) = IMG_STRIDX(id) - 1
		    IMG_STRLEN(id) = IMG_STRLEN(id) - 1

		    call wmove (win, pixrow, pixcol + IMG_STRIDX(id))
		    call waddstr (win, IMG_CURCHAR(id))

		    IMG_STR(id,nc-1) = EOS
		    call wmove (win, pixrow, pixcol + IMG_STRIDX(id)) 
		}

	    case K_DWORD:  # Delete next word
 		if (IMG_RDONLY(id) == YES) {
		    goto 90

		} else if (IMG_STRLEN(id) > 0) {
		    IMG_UPDPIX(id) = YES

		    ic = IMG_STRIDX(id)
		    nc = IMG_STRLEN(id)

		    call strcpy (IMG_STR(id,ic), IMG_DELBUF(id,0), nc-ic)
		    call amovkc (blank, IMG_STR(id,ic), nc-ic)
		    IMG_STR(id,nc) = EOS

		    call wmove (win, pixrow, pixcol + IMG_STRIDX(id))
		    call waddstr (win, IMG_CURCHAR(id))

		    IMG_STR(id,ic) = EOS
		    IMG_STRLEN(id) = IMG_STRIDX(id)
		    call wmove (win, pixrow, pixcol + IMG_STRIDX(id)) 
		}

	    case K_DLINE:  # Delete rest of line
		;

	    case K_UNDCHR: # Undelete a character
		mc = strlen (IMG_DELBUF(id,0))

		if (mc > 0) {
		    ic = IMG_STRIDX(id)
		    nc = IMG_STRLEN(id) - ic

		    call amovc (IMG_STR(id,ic), IMG_STR(id,ic+1), nc+1)

		    IMG_STR(id,ic) = IMG_DELBUF(id,mc-1)
		    IMG_DELBUF(id,mc-1) = EOS

		    call waddstr (win, IMG_STR(id,ic))

		    IMG_STRIDX(id) = IMG_STRIDX(id) + 1
		    IMG_STRLEN(id) = IMG_STRLEN(id) + 1
		    call wmove (win, pixrow, pixcol + IMG_STRIDX(id)) 

		}

	    case K_UNDWRD: # Undelete a word
		mc = strlen (IMG_DELBUF(id,0))
		if ((IMG_STRLEN(id)+mc) > IMG_CWIDTH(id)) {
		    logger = trm_info (T_LOG)
		    call msg_alloc (errmsg, SZ_FNAME, TY_CHAR)

		    call strcpy (fullfld, Memc[errmsg], SZ_FNAME)
		    call msg_send (L_WARN, logger, errmsg, NULL)

		} else if (mc > 0) {
		    ic = IMG_STRIDX(id)
		    nc = IMG_STRLEN(id) - ic

		    call amovc (IMG_STR(id,ic), IMG_STR(id,ic+mc), nc+1)
		    call amovc (IMG_DELBUF(id,0), IMG_STR(id,ic), mc)
		    IMG_DELBUF(id,0) = EOS

		    IMG_STRIDX(id) = IMG_STRIDX(id) + mc
		    IMG_STRLEN(id) = IMG_STRLEN(id) + mc

		    call waddstr (win, IMG_STR(id,ic))
		    call wmove (win, pixrow, pixcol + IMG_STRIDX(id)) 
		}

	    case K_UNDLIN: # Undelete a line
		;

	    case K_HELP:  # Display help screen
		term = trm_info (T_TERM)
		call msg_send (T_ADDWIN, term, O_HELP, NULL)

	    case K_PAINT:  # Redraw the screen
		call clearok (STDSCR, true)
		call wrefresh (STDSCR)
		call wmove (win, pixrow, pixcol + IMG_STRIDX(id)) 

	    case K_EXIT:  # Exit procedure
		done = YES

		call msg_alloc (info, LEN_CNEWSTRUCT, TY_STRUCT)
		CNEW_CODE(info) = CMD_ANY
		CNEW_STRPTR(info) = NULL

		term = trm_info (T_TERM)
		call msg_send (T_ADDWIN, term, O_CMD, info)
	    }
	}

	return

	# Read only error exit

   90	logger = trm_info (T_LOG)
	call msg_alloc (errmsg, SZ_FNAME, TY_CHAR)

	call strcpy (nowrite, Memc[errmsg], SZ_FNAME)
	call msg_send (L_WARN, logger, errmsg, NULL)

	return
end

# IMG_RECEIVE -- Process messages sent to an image window

procedure img_receive (msg)

pointer	msg		# i: message descriptor
#--
bool	change
int	done
pointer	img, logger, errmsg, info, id

string	badobject "img_receive: object is not image window"
string	badkind   "img_receive: illegal message type"

pointer	trm_info()

begin
	img = MSG_OBJ(msg)

	if (OBJ_KIND(img) != O_IMAGE) {
	    logger = trm_info (T_LOG)
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badobject, Memc[errmsg], SZ_LINE)

	    call msg_send (L_ERROR, logger, errmsg, NULL)
	    return
	}

	switch (MSG_KIND(msg)) {
	case M_CREATE:
	    info = MSG_ARG1(msg)
	    call img_create (img, Memc[INEW_FNAME(info)], 
			     INEW_RDONLY(info), INEW_INPLACE(info))

	case M_DESTROY:
	    call img_destroy (img)

	case W_RDKEY:
	    id = OBJ_DESCRIP(img)
	    call img_rdkey (img, MSG_ARG1(msg), done)

	    if (done == YES && IMG_UPDPIX(id) == YES) {
		call img_wpixel (img, IMG_CURROW(id), IMG_CURCOL(id),
				 IMG_STR(id,0), change)
		if (change)
		    call img_dpixel (img, IMG_CURROW(id), IMG_CURCOL(id))
	    }

	case W_RDCUR:
	    info = MSG_ARG1(msg)
	    if (MSG_ARG2(msg) == NULL) {
		call img_rdcur (img, CURS_KEY(info), CURS_ROW(info), 
				CURS_COL(info), "")
	    } else {
		call img_rdcur (img, CURS_KEY(info), CURS_ROW(info), 
				CURS_COL(info), Memc[MSG_ARG2(msg)])
	    }

	case W_RESIZE:
	    call img_size (img, MSG_ARG1(msg), MSG_ARG2(msg))

	case W_REDRAW:
	    call img_draw (img)

	case W_HIDDEN:
	    id = OBJ_DESCRIP(img)
	    IMG_HIDDEN(id) = MSG_ARG1(msg)

	case W_FOCUS:
	    call win_focus (img)

	case I_READ:
	    ## later
	    ;
	case I_WRITE:
	    if (MSG_ARG2(msg) == NULL) {
	        call img_write (img, MSG_ARG1(msg), "")
	    } else {
		call img_write (img, MSG_ARG1(msg), Memc[MSG_ARG2(msg)])
	    }

	case I_MOVE:
	    info = MSG_ARG1(msg)
	    call img_move (img, IPOS_INDEX(info), IPOS_ROW(info),
			   IPOS_COL(info), IPOS_PLANE(info))

	case I_UPDATE:
	    info = MSG_ARG1(msg)
	    call img_upixel (img, IPOS_ROW(info), IPOS_COL(info),
			     IPOS_PLANE(info), Memc[MSG_ARG2(msg)])

	case I_SET:
	    call img_set (img, MSG_ARG1(msg), MSG_ARG2(msg))

	case I_SETFMT:
	    call img_fmt (img, Memc[MSG_ARG1(msg)])

	default:
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badkind, Memc[errmsg], SZ_LINE)

	    logger = trm_info (T_LOG)
	    call msg_send (L_ERROR, logger, errmsg, NULL)
	}

end

# IMG_RELMOV -- Convert a relative movemnt to an absolute movement

procedure img_relmov (img, mvpos, mvdir, mvlen, done)

pointer	img		# i: image window descriptor
int	mvpos		# i: position of cursor in pixel
int	mvdir		# i: direction of movement
int	mvlen		# i: length of movement in pixels
int	done		# o: flag indicating movement to new pixel
#--
int	pos, len, win, pixrow, pixcol, ndim
int	lastrow, lastcol, nxtrow, nxtcol, nxtidx
pointer	id, im, info

string	badlen  "img_relmov: zero length move"

int	winstat()

begin
	# Make copies of input parameters which may change

	pos = mvpos
	len = mvlen

	# Get current locatopn of start of pixel

	id = OBJ_DESCRIP(img)
	win = IMG_WINDOW(id)
	pixrow = winstat (win, W_CURROW)
	pixcol = winstat (win, W_CURCOL) - IMG_STRIDX(id)

	# Handle inter-pixel movement first

	if (len == 0) {
	    if (pos == POS_CENTER) {
		if (mvdir == MV_LEFT) {
		    if (IMG_STRIDX(id) == 0) {
			len = 1
			pos = POS_RIGHT
		    } else {
			IMG_STRIDX(id) = IMG_STRIDX(id) - 1
		    }

		} else if (mvdir == MV_RIGHT) {
		    if (IMG_STRIDX(id) >= IMG_STRLEN(id)) {
			len = 1
			pos = POS_LEFT
		    } else {
			IMG_STRIDX(id) = IMG_STRIDX(id) + 1
		    }

		} else {
		    call img_error (img, badlen)
		}
	    }
	}

	# Movement between pixels. Distinguish between one-dimensional 
	# and greater images, as they are oriented differently on the screen.

	if (len > 0) {
	    im = IMG_DESCRIP(id)

	    ndim = IM_NDIM(im)
	    lastrow = IM_LEN(im,1)
	    lastcol = IM_LEN(im,2)

	    nxtrow = IMG_CURROW(id)
	    nxtcol = IMG_CURCOL(id)

	    if (ndim == 1) {
		switch (mvdir) {
		case MV_UP:
		    if (IMG_CURROW(id) >= lastrow) {
			len = 0
			pos = POS_CENTER
		    } else {
			nxtrow = min (lastrow, IMG_CURROW(id) + len)
		    }
		case MV_DOWN:
		    if (IMG_CURROW(id) <= 1) {
			len = 0
			pos = POS_CENTER
		    } else {
			nxtrow = max (1, IMG_CURROW(id) - len)
		    }
		case MV_RIGHT:
		    len = 0
		    pos = POS_CENTER
		case MV_LEFT:
		    len = 0
		    pos = POS_CENTER
		}

	    } else {
		switch (mvdir) {
		case MV_UP:
		    if (IMG_CURCOL(id) >= lastcol) {
			len = 0
			pos = POS_CENTER
		    } else {
			nxtcol = min (lastcol, IMG_CURCOL(id) + len)
		    }
		case MV_DOWN:
		    if (IMG_CURCOL(id) <= 1) {
			len = 0
			pos = POS_CENTER
		    } else {
			nxtcol = max (1, IMG_CURCOL(id) - len)
		    }
		case MV_RIGHT:
		    if (IMG_CURROW(id) >= lastrow) {
			len = 0
			pos = POS_CENTER
		    } else {
			nxtrow = min (lastrow, IMG_CURROW(id) + len)
		    }
		case MV_LEFT:
		    if (IMG_CURROW(id) <= 1) {
			len = 0
			pos = POS_CENTER
		    } else {
			nxtrow = max (1, IMG_CURROW(id) - len)
		    }
		}
	    }
	}

	# Movement within a pixel is the simpler case and is handled here as
	# an offset from the current location within the pixel. Movement
	# to a new pixel may require redrawing the screen, and so is
	# packaged as a message to move to a new location on the screen

	if (len == 0) {
	    done = NO

	    if (pos == POS_LEFT) {
		IMG_STRIDX(id) = 0

	    } else if (pos == POS_RIGHT) {
		IMG_STRIDX(id) = IMG_STRLEN(id)
	    }

	    call wmove (win, pixrow, pixcol + IMG_STRIDX(id))

	} else {
	    if (pos == POS_CENTER) {
		nxtidx = IMG_STRIDX(id) + 1
	    } else if (pos == POS_LEFT) {
		nxtidx = 1
	    } else {
		nxtidx = IMG_MAXSTR + 1
	    }

	    done = YES
	    call msg_alloc (info, LEN_IPOSSTRUCT, TY_STRUCT)

	    IPOS_INDEX(info) = nxtidx
	    IPOS_ROW(info) = nxtrow
	    IPOS_COL(info) = nxtcol
	    IPOS_PLANE(info) = IMG_CURPLANE(id)

	    call msg_send (I_MOVE, img, info, NULL)
	}

end

# IMG_RPIXEL -- Read a pixel from the data buffer

procedure img_rpixel (img, row, col, value, maxch)

pointer	img		# i: image window descriptor
int	row		# i: pixel row
int	col		# i: pixel column
char	value[ARB]	# o: pixel value
int	maxch		# i: maximum length of value string
#--
pointer	id, pix

string	notopen  "Cannot read pixel from closed image"

begin
	id = OBJ_DESCRIP(img)
	if (IMG_DESCRIP(id) == NULL) {
	    call img_error (img, notopen)
	    return
	}

	# Convert pixel value into a string

	IMG_UPDPIX(id) = NO
	pix = IMG_BUFPTR(id) + (col - IMG_LOCOL(id)) * IMG_NROW(id) + 
	      (row - IMG_LOROW(id))

	switch (IMG_TYPE(id)) {
	case TY_SHORT:
	    call sprintf (value, maxch, IMG_FORMAT(id))
	    call pargs (Mems[pix])
	case TY_INT:
	    call sprintf (value, maxch, IMG_FORMAT(id))
	    call pargi (Memi[pix])
	case TY_LONG:
	    call sprintf (value, maxch, IMG_FORMAT(id))
	    call pargl (Meml[pix])
	case TY_REAL:
	    call sprintf (value, maxch, IMG_FORMAT(id))
	    call pargr (Memr[pix])
	case TY_DOUBLE:
	    call sprintf (value, maxch, IMG_FORMAT(id))
	    call pargd (Memd[pix])
	}

end

# IMG_SET -- Change the flag values in the image descriptor

procedure img_set (img, flag, value)

pointer	img		# i: image window descriptor
int	flag		# i: flag to be changed
int	value		# i: new value
#--
pointer	id

begin
	id = OBJ_DESCRIP(img)

	switch (flag) {
	case I_NAME:
	    ;	## don't handle renaming images for now
	case I_RDONLY:
	    IMG_RDONLY(id) = value
	case I_INPLACE:
	    ;	## don't handle changing inplace for now
	case I_UPFILE:
	    IMG_UPDFILE(id) = value
	}

end

# IMG_SIZE -- Resize and redraw the image window

procedure img_size (img, row, height)

pointer	img		# i: image window descriptor
int	row		# i: topmost row of log window
int	height		# i: height of log window
#--
int	oheight, width
pointer	id, im

int	newwin()

begin
	# Delete current window, if any

	id = OBJ_DESCRIP(img)
	im = IMG_DESCRIP(id)

	if (IMG_WINDOW(id) != 0)
	   call delwin (IMG_WINDOW(id))

	# Create new window of proper size

	call wdimen (STDSCR, oheight, width)
	IMG_WINDOW(id) = newwin (height, width, row, 1)

	# Allocate buffer to hold pixels

	call img_buffer (img)

	# Draw contents of new window

	call img_draw (img)

end

# IMG_TRIM -- Trim a string containing a pixel value of trailing blanks

procedure img_trim (value)

char	value[ARB]	# u: Pixel value string
#--
int	ic, jc

begin
	jc = 0

	for (ic = 1; value[ic] != EOS; ic = ic + 1) {
	    if (value[ic] != ' ')
		jc = ic
	}

	value[jc+1] = EOS

end

# IMG_UPIXEL -- Update  a pixel  in an image

procedure img_upixel (img, row, col, plane, value)

pointer	img		# i: image window descriptor
int	row		# i: pixel row
int	col		# i: pixel column
int	plane		# i: pixel plane
char	value[ARB]	# i: pixel value
#--
bool	change
pointer	id

begin
	# Exit if updated pixel is not in window

	id = OBJ_DESCRIP(img)

	if (plane != IMG_CURPLANE(id))
	    return

	# Write updated pixel to buffer

	call img_wpixel (img, row, col, value, change)

	# Display updated pixel

	if (change)
	    call img_dpixel (img, row, col)

end

# IMG_WBUF -- Write the image data buffer

procedure img_wbuf (img)

pointer	img		# i: image window descriptor
#--
int	nbuf
pointer	id, im, vec, buf

string	nodata  "img_wbuf: Data buffer not initialized"

pointer	imps3s(), imps3i(), imps3l(), imps3r(), imps3d()

begin
	id = OBJ_DESCRIP(img)
	im = IMG_DESCRIP(id)
	
	if (IMG_BUFPTR(id) == NULL) {
	    call img_error (img, nodata)
	    return
	}

	IMG_UPDPIX(id) = NO
	IMG_UPDBUF(id) = NO

	# Get output buffer 

	buf = IMG_BUFPTR(id)
	nbuf = IMG_NROW(id) * IMG_NCOL(id)

	switch (IMG_TYPE(id)) {
	case TY_SHORT:
	    vec = imps3s (im, IMG_LOROW(id), IMG_HIROW(id), 
			       IMG_LOCOL(id), IMG_HICOL(id),
			       IMG_CURPLANE(id), IMG_CURPLANE(id))
	    call amovs (Mems[buf], Mems[vec], nbuf)
	case TY_INT:
	    vec = imps3i (im, IMG_LOROW(id), IMG_HIROW(id), 
			       IMG_LOCOL(id), IMG_HICOL(id),
			       IMG_CURPLANE(id), IMG_CURPLANE(id))
	    call amovi (Memi[buf], Memi[vec], nbuf)
	case TY_LONG:
	    vec = imps3l (im, IMG_LOROW(id), IMG_HIROW(id), 
			       IMG_LOCOL(id), IMG_HICOL(id),
			       IMG_CURPLANE(id), IMG_CURPLANE(id))
	    call amovl (Meml[buf], Meml[vec], nbuf)
	case TY_REAL:
	    vec = imps3r (im, IMG_LOROW(id), IMG_HIROW(id), 
			       IMG_LOCOL(id), IMG_HICOL(id),
			       IMG_CURPLANE(id), IMG_CURPLANE(id))
	    call amovr (Memr[buf], Memr[vec], nbuf)
	case TY_DOUBLE:
	    vec = imps3d (im, IMG_LOROW(id), IMG_HIROW(id), 
			       IMG_LOCOL(id), IMG_HICOL(id),
			       IMG_CURPLANE(id), IMG_CURPLANE(id))
	    call amovd (Memd[buf], Memd[vec], nbuf)
	}

	# Flush output buffer

	call imflush (im)

end

# IMG_WPIXEL -- Write a single pixel to the data buffer

procedure img_wpixel (img, row, col, value, change)

pointer	img		# i: image descriptor
int	row		# i: pixel row
int	col		# i: pixel column
char	value[ARB]	# i: pixel value
bool	change		# o: has pixel changed?
#--
double	dval
int	ic, junk, temp, ival
long	lval
pointer	id, pix
real	rval

string	notopen  "Cannot write pixel to closed image"

int	ctoi(), ctol(), ctor(), ctod()

begin

	id = OBJ_DESCRIP(img)
	if (IMG_DESCRIP(id) == NULL) {
	    call img_error (img, notopen)
	    return
	}

	if (row < IMG_LOROW(id) || row > IMG_HIROW(id) ||
	    col < IMG_LOCOL(id) || col > IMG_HICOL(id)   ) {

	    change = false
	    return
	}

	# Calculate location of pixel in data buffer

	pix = IMG_BUFPTR(id) + (col - IMG_LOCOL(id)) * IMG_NROW(id) + 
	      (row - IMG_LOROW(id))

	# Get old pixel value, convert string to value
	# and compare old to new value to see if it changed

	ic = 1
	switch (IMG_TYPE(id)) {
	case TY_SHORT:
	    ival = Mems[pix]
	    junk = ctoi (value, ic, temp)
	    if (abs(temp) < MAX_SHORT) {
		Mems[pix] = temp
	    } else {
		Mems[pix] = 0
	    }
	    change = ival != Mems[pix]
	case TY_INT:
	    ival = Memi[pix]
	    junk = ctoi (value, ic, Memi[pix])
	    change = ival != Memi[pix]
	case TY_LONG:
	    lval = Meml[pix]
	    junk = ctol (value, ic, Meml[pix])
	    change = lval != Meml[pix]
	case TY_REAL:
	    rval = Memr[pix]
	    junk = ctor (value, ic, Memr[pix])
	    change = rval != Memr[pix]
	case TY_DOUBLE:
	    dval = Memd[pix]
	    junk = ctod (value, ic, Memd[pix])
	    change = dval != Memd[pix]
	}

	if (change) {
	    IMG_UPDFILE(id) = YES
	    IMG_UPDBUF(id) = YES
	}

	IMG_UPDPIX(id) = NO
end

# IMG_WRITE -- Write an image to disk

procedure img_write (img, reopen, name)

pointer	img		# i: image window descriptor
int	reopen		# i: reopen image after writing?
char	name[ARB]	# i: image name or null string
#--
pointer	sp, id, im, output

bool	strne()
pointer	immap()

begin
	call smark (sp)
	call salloc (output, SZ_FNAME, TY_CHAR)

	id = OBJ_DESCRIP(img)

	# Flush the output buffer 

	if (IMG_UPDBUF(id) == YES)
	    call img_wbuf (img)

	# Copy the image name to a output string

	if (name[1] == EOS) {
	    call strcpy (IMG_NAME(id), Memc[output], SZ_FNAME)
	} else {
	    call strcpy (name, Memc[output], SZ_FNAME)
	}

	# Close the image to flush all changes

	if (IMG_DESCRIP(id) != NULL) {
	    im = IMG_DESCRIP(id)

	    if (IM_CLINDEX(im) <= 1) {
		call imgcluster (IM_NAME(im), IMG_NAME(id), SZ_FNAME)
	    } else {
		call img_namcpy (IM_NAME(im), IMG_NAME(id), SZ_FNAME)
	    }

	    IMG_DESCRIP(id) = NULL
	    call imunmap (im)
	}

	# Copy from temporary image to permanent image

	if (strne (IMG_NAME(id), Memc[output]))
	    call copyimg (IMG_NAME(id), Memc[output])

	# Reopen the image if requested

	if (reopen == YES) {
	    if (IMG_RDONLY(id) == NO) {	    
		IMG_DESCRIP(id) = immap (IMG_NAME(id), READ_WRITE, 0)
	    } else {
		IMG_DESCRIP(id) = immap (IMG_NAME(id), READ_ONLY, 0)
	    }
	    call strcpy (Memc[output], IMG_NAME(id), SZ_FNAME)
	}

	# Set update file flag to NO

	IMG_UPDFILE(id) = NO

	call sfree (sp)
end

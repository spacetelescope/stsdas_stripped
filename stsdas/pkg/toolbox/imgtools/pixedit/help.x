include <curses.h>
include "object.h"
include "pixedit.h"
include "message.h"
include "help.h"

define	K_HALF_UP	254
define	K_HALF_DOWN	255

# HLP_ADDLINE -- Add a line of text to the help structure

procedure hlp_addline (hlp, line)

pointer	hlp		# i: help descriptor
char	line[ARB]	# i: text to add to help structure
#--
bool	done
char	nl
int	ic, nc
pointer	hd

data	nl  / '\n' /

int	stridx(), strlen()

begin
	ic = 1
	done = false
	hd = OBJ_DESCRIP(hlp)

	# Process each newline delimeted portion of the line separately

	repeat {

	    # Get length of next part of string

	    nc = stridx (nl, line[ic])
	    if (nc == 0) {
		nc = strlen (line[ic])
		done = true
	    }

	    # Copy string to text buffer

	    if (HLP_NXTBUF(hd) + nc + 1 >= HLP_MAXBUF(hd)) {
		HLP_MAXBUF(hd) = HLP_MAXBUF(hd) + HLP_SIZEBUF
		call realloc (HLP_BUFPTR(hd), HLP_MAXBUF(hd), TY_CHAR)
	    }
		
	    call strcpy (line[ic], HLP_BUFFER(hd), nc)
	    HLP_NXTBUF(hd) = HLP_NXTBUF(hd) + nc

	    # Increment next row pointer if string ends in newline

	    if (line[nc] == nl) {
		if (HLP_NXTROW(hd) + 1 >= HLP_MAXROW(hd)) {
		    HLP_MAXROW(hd) = HLP_MAXROW(hd) + HLP_SIZEARY
		    call realloc (HLP_ROWARY(hd), HLP_MAXROW(hd), TY_INT)
		}

		HLP_BUFFER(hd) = EOS
		HLP_NXTBUF(hd) = HLP_NXTBUF(hd) + 1
		
		HLP_NXTROW(hd) = HLP_NXTROW(hd) + 1
		HLP_ROWPTR(hd,HLP_NXTROW(hd)) = HLP_NXTBUF(hd)
	    }

	    ic = ic + nc
	} until (done)

end

# HLP_CREATE -- Create a help window

procedure hlp_create (hlp)

pointer	hlp		# i: help descriptor
#--
int	fd, ic, col
pointer	sp, hd, eseq, line, seqstr

string	title1  "The following commands are available after typing %s\n\n"
string	title2  "The following editing commands are available\n\n"

int	open(), getline(), hlp_format()

begin
	call smark (sp)
	call salloc (eseq, SZ_FNAME, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Create the help descriptor

	call calloc (hd, LEN_HLPSTRUCT, TY_STRUCT)
	OBJ_DESCRIP(hlp) = hd

	# Fill in the fields of the descriptor

	call malloc (HLP_BUFPTR(hd), HLP_SIZEBUF, TY_CHAR)
	call malloc (HLP_ROWARY(hd), HLP_SIZEARY, TY_INT)

	HLP_MAXBUF(hd) = HLP_SIZEBUF
	HLP_MAXROW(hd) = HLP_SIZEARY

	HLP_BUFFER(hd) = EOS
	HLP_ROWPTR(hd, 0) = 0

	# Read the list of commands

	ifnoerr {
	    fd = open (HLP_FILE, READ_ONLY, TEXT_FILE)

	} then {
	    call k_eseq ("EXIT_UPDATE", Memc[eseq], SZ_FNAME)
	    call sprintf (Memc[line], SZ_LINE, title1)
	    call pargstr (Memc[eseq])

	    call hlp_addline (hlp, Memc[line])
	    while (getline (fd, Memc[line]) != EOF)
		call hlp_addline (hlp, Memc[line])

	    call close (fd)
	}

	# Read the list of editing key sequences

	call hlp_addline (hlp, title2)

	ic = 1
	col = 1
	call k_help (seqstr)

	while (hlp_format (Memc[seqstr], ic, Memc[line], SZ_LINE) > 0) {
	    if (col == 2) {
		col = 0
		call strcat ("\n", Memc[line], SZ_LINE)
	    }

	    call hlp_addline (hlp, Memc[line])
	    col = col + 1
	}

	call hlp_addline (hlp, "\n")
	call sfree (sp)
end

# HLP_DESTROY -- Eliminate the help object

procedure hlp_destroy (hlp)

pointer	hlp		# i: help descriptor
#--
pointer	hd

begin
	hd = OBJ_DESCRIP(hlp)
	if (hd != NULL) {
	    if (HLP_WINDOW(hd) != 0)
		call delwin (HLP_WINDOW(hd))

	    call mfree (HLP_BUFPTR(hd), TY_CHAR)
	    call mfree (HLP_ROWARY(hd), TY_INT)
	    call mfree (hd, TY_STRUCT)

	    OBJ_DESCRIP(hlp) = NULL
	}

	call obj_destroy (hlp)
end

# HLP_DRAW -- Redraw the contents of the help window

procedure hlp_draw (hlp)

pointer	hlp		# i: help descriptor
#--
int	win, irow, height, width
pointer	sp, hd, term, line, title

int	trm_info()

string	fmt  "q=quit d=down u=up f|sp=fwd_page b=back_page .=bof%*t"

begin
	# If help window is hidden, bring it to the front

	hd = OBJ_DESCRIP(hlp)
	if (HLP_HIDDEN(hd) == YES) {
	    term = trm_info (T_TERM)
	    call msg_send (T_FRONT, term, hlp, NULL)
	    return
	}

	# Calculate lowest and highest rows on the screen

	win = HLP_WINDOW(hd)
	call wdimen (win, height, width)

	HLP_LOROW(hd) = max (0, HLP_CURROW(hd) - height / 2)
	HLP_HIROW(hd) = min (HLP_NXTROW(hd) - 1, height + (HLP_LOROW(hd) - 2))

	# Write the help lines

	call werase (win)
	call wmove (win, 1, 1)

	do irow = HLP_LOROW(hd), HLP_HIROW(hd) {
	    line = HLP_BUFPTR(hd) + HLP_ROWPTR(hd,irow)
	    call waddstr (win, Memc[line])
	}

	# Write an explanatory message at the bottom of the screen

	call smark (sp)
	call salloc (title, width, TY_CHAR)

	call sprintf (Memc[title], width, fmt)
	call pargi (width)

	call wmove (win, height, 1)
	call wstandout (win)
	call waddstr (win, Memc[title])
	call wstandend (win)

	# Move the cursor to its proper position and flush the screen

	call hlp_move (hlp, HLP_CURROW(hd), HLP_CURCOL(hd))

	call wrefresh (win)	
	call sfree (sp)

end

# HLP_FORMAT -- Reformat the editing key sequence string

int procedure hlp_format (seqstr, ic, str, maxch)

char	seqstr[ARB]	# i: string containing editing key sequences
int	ic		# u: index into editing key sequence string
char	str[ARB]	# o: formatted editing key sequence string
int	maxch		# i: declared length of formatted string
#--
bool	flag
int	jc, kc
pointer	sp, label, name

string	strfmt "%4w%-12.12s = %-12.12s"

begin
	call smark (sp)
	call salloc (label, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_FNAME, TY_CHAR)

	jc = 0
	kc = 0
	flag = true
	str[1] = EOS

	# Extract the name and label fields from escape sequence string

	for ( ; seqstr[ic] != EOS; ic = ic + 1) {
	    if (flag) {	
		if (seqstr[ic] != '=') {
		    Memc[label+jc] = seqstr[ic]
		    jc = jc + 1
		} else {
		    Memc[label+jc] = EOS
		    flag = false
		    kc = jc
		    jc = 0
		}
	    } else {
		if (seqstr[ic] != '\n') {
		    Memc[name+jc] = seqstr[ic]
		    jc = jc + 1
		} else {
		    Memc[name+jc] = EOS
		    flag = true

		    ic = ic + 1
		    kc = kc + jc

		    # When both fields have been extracted, 
		    # print them as a formatted string

		    call sprintf (str, maxch, strfmt)
		    call pargstr (Memc[label])
		    call pargstr (Memc[name])

		    break
		}
	    }
	}

	# Return the number of escape sequence characters read
	# None indicates we are at the end of the string

	call sfree (sp)
	return (kc)

end

# HLP_MOVE -- Move the cursor in the help window

procedure hlp_move (hlp, row, col)

pointer	hlp		# i: help descriptor
int	row		# i: new row
int	col		# i: new column
#--
int	win, height, width, pixrow, pixcol
pointer	hd

begin
	# Update current row and column position in help descriptor

	hd = OBJ_DESCRIP(hlp)

	win = HLP_WINDOW(hd)
	call wdimen (win, height, width)

	HLP_CURROW(hd) = row
	HLP_CURCOL(hd) = min (col, width-1)

	# Redraw help window if current row is off the screen
	# otherwise calculate and move to new location on screen

	if (row < HLP_LOROW(hd) || row > HLP_HIROW(hd)) {
	    call msg_send (W_REDRAW, hlp, NULL, NULL)

	} else {
	    pixrow = (row - HLP_LOROW(hd)) + 1
	    pixcol = col + 1

	    call wmove (win, pixrow, pixcol)
	}

end

# HLP_RDKEY -- Read a keystroke while in the help window

procedure hlp_rdkey (hlp, ch, done)

pointer	hlp		# i: help window descriptor
int	ch		# i: character read
int	done		# o: finished using help screen?
#--
char	code
int	keyvals[7]
int	index, win, nxtrow, nxtcol, nrows, ncols, pgsize
pointer	hd

string	keycodes "qduf b."
data	keyvals  /K_EXIT, K_HALF_DOWN, K_HALF_UP, K_NEXTP, K_NEXTP,
		  K_PREVP, K_HOME /

int	stridx()

begin
	done = NO
	code = ch

	# Convert character to corresponding command
	# return if there is no corresponding command

	if (code < K_BASE) {
	    index = stridx (code, keycodes)
	    if (index > 0) {
		code = keyvals[index]
	    } else {
		return
	    }
	}

	# Calculate size of help screen

	hd = OBJ_DESCRIP(hlp)
	win = HLP_WINDOW(hd)

        call wdimen (win, nrows, ncols)
	pgsize = nrows - 1

	# Use command to compute new location in help screen

	nxtrow = HLP_CURROW(hd)
	nxtcol = HLP_CURCOL(hd)

	switch (code) {
	case K_HALF_UP:  # Move up half a screen
	    if (HLP_CURROW(hd) > 0) {
		nxtrow = max (0, HLP_CURROW(hd) - pgsize / 2)
		nxtcol = 0
	    }

	case K_HALF_DOWN:  # Move down half a screen
	    if (HLP_CURROW(hd) < HLP_NXTROW(hd) - 1) {
		nxtrow = min (HLP_NXTROW(hd) - 1, HLP_CURROW(hd) + pgsize / 2)
		nxtcol = 0
	    }

	case K_UP:  # Move up one row
	    if (HLP_CURROW(hd) > 0)
		nxtrow = HLP_CURROW(hd) - 1

	case K_DOWN:  # Move down one row
	    if (HLP_CURROW(hd) < HLP_NXTROW(hd) - 1)
		nxtrow = HLP_CURROW(hd) + 1

	case K_RIGHT:  # Move right one column
	    if (HLP_CURCOL(hd)  < ncols - 1)
		nxtcol = HLP_CURCOL(hd) + 1

	case K_LEFT:  # Move left one column
	    if (HLP_CURCOL(hd) > 0)
		nxtcol = HLP_CURCOL(hd) - 1

	case K_NEXTW:  # Move forwards one word
	    ;

	case K_PREVW:  # Move backwards one word
	    ;

	case K_NEXTP:  # Move forwards one screen
	    if (HLP_CURROW(hd) < HLP_NXTROW(hd) - 1) {
		nxtrow = min (HLP_NXTROW(hd) - 1, HLP_CURROW(hd) + pgsize)
		nxtcol = 0
	    }

	case K_PREVP:  # Move backwards one screen
	    if (HLP_CURROW(hd) > 0) {
		nxtrow = max (0, HLP_CURROW(hd) - pgsize)
		nxtcol = 0
	    }

	case K_HOME:  # Move to first field
	    if (HLP_CURROW(hd) > 0) {
		nxtrow = 0
		nxtcol = 0
	    } 

	case K_END:  # Move to last field
	    if (HLP_CURROW(hd) < HLP_NXTROW(hd) - 1) {
		nxtrow = HLP_NXTROW(hd) - 1
		nxtcol = 0
	    } 

	case K_BOL:  # Move to first column in line
	    if (HLP_CURCOL(hd) > 0)
		nxtcol = 0

	case K_EOL:  # Move to last column in line
	    if (HLP_CURCOL(hd) < ncols - 1)
		nxtcol = ncols - 1

	case K_DEL:  # Delete character underneath cursor
	    ;

	case K_BS:  # Delete character to left of cursor
	    ;

	case K_DWORD:  # Delete next word
	    ;

	case K_DLINE:  # Delete rest of line
	    ;

	case K_UNDCHR: # Undelete a character
	    ;

	case K_UNDWRD: # Undelete a word
	    ;

	case K_UNDLIN: # Undelete a line
	    ;

	case K_HELP:  # Display help screen
	    ;

	case K_PAINT:  # Redraw the screen
	    call clearok (STDSCR, true)
	    call wrefresh (STDSCR)

	case K_EXIT:  # Exit procedure
	    done = YES

	}

	if (HLP_CURROW(hd) != nxtrow || HLP_CURCOL(hd) != nxtcol)
	    call hlp_move (hlp, nxtrow, nxtcol)

end

# HLP_RECEIVE -- Process messages sent to the help window

procedure hlp_receive (msg)

pointer	msg		# i: message descriptor
#--
int	done
pointer	term, logger, errmsg, hlp, hd

string	badobject "hlp_receive: object is not help window"
string	badkind   "hlp_receive: illegal message type"

pointer	trm_info()

begin
	hlp = MSG_OBJ(msg)

	if (OBJ_KIND(hlp) != O_HELP) {
	    logger = trm_info (T_LOG)
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badobject, Memc[errmsg], SZ_LINE)

	    call msg_send (L_ERROR, logger, errmsg, NULL)
	    return
	}

	switch (MSG_KIND(msg)) {
	case M_CREATE:
	    call hlp_create (hlp)

	case M_DESTROY:
	    call hlp_destroy (hlp)

	case W_RDKEY:
	    call hlp_rdkey (hlp, MSG_ARG1(msg), done)
	    if (done == YES) {
		term = trm_info (T_TERM)
		call msg_send (T_DELWIN, term, hlp, NULL)
	    }

	case W_RESIZE:
	    call hlp_size (hlp, MSG_ARG1(msg), MSG_ARG2(msg))

	case W_REDRAW:
	    call hlp_draw (hlp)

	case W_HIDDEN:
	    hd = OBJ_DESCRIP(hlp)
	    HLP_HIDDEN(hd) = MSG_ARG1(msg)

	case W_FOCUS:
	    call win_focus (hlp)

	default:
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badkind, Memc[errmsg], SZ_LINE)

	    logger = trm_info (T_LOG)
	    call msg_send (L_ERROR, logger, errmsg, NULL)
	}

end

# HLP_SIZE -- Resize and redraw the help window

procedure hlp_size (hlp, row, height)

pointer	hlp		# i: help window descriptor
int	row		# i: topmost row of help window
int	height		# i: height of help window
#--
int	oheight, width
pointer	hd

int	newwin()

begin
	hd = OBJ_DESCRIP(hlp)

	if (HLP_WINDOW(hd) != 0)
	    call delwin (HLP_WINDOW(hlp))

	call wdimen (STDSCR, oheight, width)
	HLP_WINDOW(hd) = newwin (height, width, row, 1)

	call hlp_draw (hlp)
end

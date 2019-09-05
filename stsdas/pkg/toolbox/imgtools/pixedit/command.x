include <ctype.h>
include	<curses.h>
include "object.h"
include "pixedit.h"
include "message.h"
include "command.h"
include "lex.h"

# CMD_ANY -- Handle a command which has yet to be determined what it is

procedure cmd_any (cmd)

pointer	cmd		# i: command descriptor
#--
int	code
pointer	cd, term

string	clist	    CMD_LIST
string	newcommand  "Please enter command."
string	badcommand  "Unrecognized command. Please reenter."

int	strdic(), lst_get()
pointer	trm_info()

begin
	cd = OBJ_DESCRIP(cmd)

	switch (CMD_FIELD(cd)) {
	case 0:
	    CMD_FIELD(cd) = 1
	    call strcpy (newcommand, CMD_TITLE(cd), CMD_MAXSTR)

	case 1:
	    call lst_parse (CMD_STR(cd,0))

	    if (lst_get (CMD_STR(cd,0), 0, CMD_COMMAND(cd), CMD_MAXSTR) == 0) {
		code = -1
	    } else {
		code = strdic (CMD_COMMAND(cd), CMD_COMMAND(cd), 
			       CMD_MAXSTR, clist)
	    }

	    if (code == -1) {
		term = trm_info (T_TERM)
		call msg_send (T_DELWIN, term, cmd, NULL)

	    } else if (code == 0) {
		call strcpy (badcommand, CMD_TITLE(cd), CMD_MAXSTR)
		CMD_COMMAND(cd) = EOS
		call cmd_draw (cmd)

	    } else {
		term = trm_info (T_TERM)
		call msg_send (T_DELWIN, term, cmd, NULL)

		call cmd_start (code, CMD_STR(cd,0))
	     }
	}

end

# CMD_ARGS -- Number of arguments that a command takes

int procedure cmd_args (code)

int	code		# i: command code
#--
int	narg
pointer	logger, errmsg

string	badcmd "cmd_args: unrecognized command"

pointer	trm_info()

begin
	switch (code) {
	case CMD_CURSOR:
	    narg = 0
	case CMD_FINISH:
	    narg = 0
	case CMD_QWRITE:
	    narg = 1
	case CMD_ANY:
	    narg = 1
	case CMD_EXIT:
	    narg = 0
	case CMD_FORMAT:
	    narg = 1
	case CMD_GOTO:
	    narg = 3
	case CMD_HELP:
	    narg = 0
	case CMD_QUIT:
	    narg = 0
	default:
	    narg = 0

	    logger = trm_info (T_LOG)
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badcmd, Memc[errmsg], SZ_LINE)

	    call msg_send (L_ERROR, logger, errmsg, NULL)
	}

	return (narg)
end

# CMD_CREATE -- Create a new command window

procedure cmd_create (cmd, code, str)

pointer	cmd		# i: command descriptor
int	code		# i: command code
char	str[ARB]	# i: command string
#--
pointer	cd

extern	cmd_finish, cmd_qwrite, cmd_any, cmd_cursor, cmd_exit
extern	cmd_format, cmd_goto, cmd_help, cmd_quit
int	cmd_args()
pointer	locpr()

begin
	# Create the command descriptor

	call malloc (cd, LEN_CMDSTRUCT, TY_STRUCT)
	OBJ_DESCRIP(cmd) = cd

	# Fill in the fields of the descriptor

	CMD_WINDOW(cd) = 0
	CMD_HIDDEN(cd) = NO
	CMD_FIELD(cd) = 0
	CMD_STRIDX(cd) = 0
	CMD_STRLEN(cd) = 0

	if (cmd_args (code) == 0) {
	    call obj_remove (cmd)

	    CMD_STRPTR(cd) = NULL
	    CMD_BUFPTR(cd) = NULL
	    CMD_TTLPTR(cd) = NULL
	    CMD_CMDPTR(cd) = NULL

	} else {
	    call malloc (CMD_STRPTR(cd), CMD_MAXSTR, TY_CHAR)
	    call malloc (CMD_BUFPTR(cd), CMD_MAXSTR, TY_CHAR)
	    call malloc (CMD_TTLPTR(cd), CMD_MAXSTR, TY_CHAR)
	    call malloc (CMD_CMDPTR(cd), CMD_MAXSTR, TY_CHAR)

	    CMD_STR(cd,0) = EOS
	    CMD_BUF(cd,0) = EOS
	    CMD_TITLE(cd) = EOS

	    if (code != CMD_ANY)
		call strcpy (str, CMD_COMMAND(cd), CMD_MAXSTR)
	}

	switch (code) {
	case CMD_CURSOR:
	    CMD_FUNC(cd) = locpr (cmd_cursor)
	case CMD_FINISH:
	    CMD_FUNC(cd) = locpr (cmd_finish)
	case CMD_QWRITE:
	    CMD_FUNC(cd) = locpr (cmd_qwrite)
	case CMD_ANY:
	    CMD_FUNC(cd) = locpr (cmd_any)
	case CMD_EXIT:
	    CMD_FUNC(cd) = locpr (cmd_exit)
	case CMD_FORMAT:
	    CMD_FUNC(cd) = locpr (cmd_format)
	case CMD_GOTO:
	    CMD_FUNC(cd) = locpr (cmd_goto)
	case CMD_HELP:
	    CMD_FUNC(cd) = locpr (cmd_help)
	case CMD_QUIT:
	    CMD_FUNC(cd) = locpr (cmd_quit)
	}

	call zcall1 (CMD_FUNC(cd), cmd)

end

# CMD_CURSOR -- Put editor in cursor mode

procedure cmd_cursor (cmd)

pointer cmd		# i: command descriptor
#--
pointer	term
pointer	trm_info()

begin
	call msg_send (M_DESTROY, cmd, NULL, NULL)

	term = trm_info (T_TERM)
	call msg_send (T_USECUR, term, NULL, NULL)
end

# CMD_DESTROY -- Eliminate the command object

procedure cmd_destroy (cmd)

pointer	cmd		# i: command descriptor
#--
pointer	cd

begin
	cd = OBJ_DESCRIP(cmd)
	if (cd != NULL) {
	    if (CMD_WINDOW(cd) != 0) {
		call werase (CMD_WINDOW(cd))
		call delwin (CMD_WINDOW(cd))
	    }

	    if (CMD_STRPTR(cd) != NULL) {
		call mfree (CMD_STRPTR(cd), TY_CHAR)
		call mfree (CMD_BUFPTR(cd), TY_CHAR)
		call mfree (CMD_TTLPTR(cd), TY_CHAR)
		call mfree (CMD_CMDPTR(cd), TY_CHAR)
	    }
	    call mfree (cd, TY_STRUCT)
	    OBJ_DESCRIP(cmd) = NULL
	}

	call obj_destroy (cmd)
end

# CMD_DRAW -- Redraw the command window

procedure cmd_draw (cmd)

pointer	cmd		# i: Command descriptor
#--
int	win
pointer	cd, term, logger

pointer	trm_info()

begin
	cd = OBJ_DESCRIP(cmd)
	win = CMD_WINDOW(cd)

	if (CMD_HIDDEN(cd) == YES) {
	    term = trm_info (T_TERM)
	    call msg_send (T_FRONT, term, cmd, NULL)

	    return
	}

	# Send message to clear log window

	logger = trm_info (T_LOG)
	call msg_send (L_CLEAR, logger, NULL, NULL)

	# Write command title and edit string

	call werase (win)

	call wmove (win, CMD_TTLROW, CMD_TTLCOL)
	call waddstr (win, CMD_TITLE(cd))

	call wmove (win, CMD_STRROW, CMD_STRCOL)
	call waddstr (win, CMD_STR(cd,0))

	call wmove (win, CMD_STRROW, CMD_CHCOL(cd))

end

# CMD_EXIT -- Process the exit command

procedure cmd_exit (cmd)

pointer	cmd		# i: command descriptor
#--
pointer	img

int	img_info()
pointer	trm_info()

begin
	img = trm_info (T_SELECT)
	if (img_info (img, I_UPFILE) == YES)
	    call msg_send (I_WRITE, img, NO, NULL)

	call cmd_start (CMD_FINISH, "")
	call msg_send (M_DESTROY, cmd, NULL, NULL)

end

# CMD_FINISH -- Send the appropriate command for finishing the editor

procedure cmd_finish (cmd)

pointer	cmd		# i: command descriptor
#--
pointer	img, term, select

int	img_info()
pointer	img_dirty(), trm_info()

begin
	img = img_dirty ()

	# Send the quit command if no more images need updating

	if (img == NULL) {
	    term = trm_info (T_TERM)
	    call msg_send (T_QUIT, term, NULL, NULL)

	# Write the modified image if the image was edited in place

	} else if (img_info (img, I_INPLACE) == YES) {
	    call msg_send (I_WRITE, img, NO, NULL)
	    call cmd_start (CMD_FINISH, "")

	# Otherwise, ask the user before writing

	} else {

	    # Set focus to window to be queried
	    select = trm_info (T_SELECT)
	    if (img != select)
		call msg_send (T_REFOCUS, term, img, NULL)

	    # Start query write command
	    call cmd_start (CMD_QWRITE, "")
	}

	call msg_send (M_DESTROY, cmd, NULL, NULL)
end

# CMD_FORMAT -- Process the format command

procedure cmd_format (cmd)

pointer	cmd		# i: command descriptor
#--
int	junk, pad, width, dec, type
pointer	cd, sp, ftnfmt, sppfmt, img, format, term

string	noformat   "Please enter display format."
string	badformat  "Illegal display format. Please reenter."

int	lst_get(), lex_format(), nowhite(), gstrcpy()
pointer	trm_info()

begin
	cd = OBJ_DESCRIP(cmd)

	call smark (sp)
	call salloc (ftnfmt, CMD_MAXSTR, TY_CHAR)
	call salloc (sppfmt, CMD_MAXSTR, TY_CHAR)

	# Copy format from command string

	if (lst_get (CMD_COMMAND(cd), 1, Memc[ftnfmt], CMD_MAXSTR) == 0)
	    junk = nowhite (CMD_STR(cd,0), Memc[ftnfmt], CMD_MAXSTR)

	# Convert format into correct SPP format

	Memc[sppfmt] = EOS
	if (Memc[ftnfmt] != EOS) {
	    call tbbftp (Memc[ftnfmt], Memc[sppfmt])
	    if (Memc[sppfmt] != EOS) {
		if (lex_format (Memc[sppfmt], pad, width, dec, type) 
		    != LEX_OKAY) {
		    Memc[sppfmt] = EOS

		} else if (width > PIXWIDTH) {
		    Memc[sppfmt] = EOS

		} else if (type != 'd' && type != 'x') {
		    call sprintf (Memc[sppfmt], CMD_MAXSTR, "%%-%d.%d%c")
		    call pargi (width)
		    call pargi (dec)
		    call pargi (type)

		} else {
		    call sprintf (Memc[sppfmt], CMD_MAXSTR, "%%-%d%c")
		    call pargi (width)
		    call pargi (type)
		}
	    }
	}

	# If no format or illegal format, query for format

	if (Memc[sppfmt] == EOS) {
	    CMD_FIELD(cd) = 1
	    if (Memc[ftnfmt] == EOS) {
		CMD_STR(cd,0) = EOS
		CMD_STRLEN(cd) = 0
		CMD_STRIDX(cd) = 0

		call strcpy (noformat, CMD_TITLE(cd), CMD_MAXSTR)

	    } else {
		CMD_STRLEN(cd) = gstrcpy (Memc[ftnfmt], 
					  CMD_STR(cd,0), CMD_MAXSTR)
		CMD_STRIDX(cd) = CMD_STRLEN(cd)

		call strcpy (badformat, CMD_TITLE(cd), CMD_MAXSTR)
	    }

	    if (CMD_WINDOW(cd) != 0)
		call cmd_draw (cmd)

	# Send command to change display format

	} else {
	    call msg_alloc (format, CMD_MAXSTR, TY_CHAR)
	    call strcpy (Memc[sppfmt], Memc[format], CMD_MAXSTR)

	    img = trm_info (T_SELECT)
	    call msg_send(I_SETFMT, img, format, NULL)

	    term = trm_info (T_TERM)
	    call msg_send (T_DELWIN, term, cmd, NULL)
	}

	call sfree (sp)
end

# CMD_GOTO -- Process the goto command

procedure cmd_goto (cmd)

pointer	cmd		# i: command descriptor
#--
int	lencode[3], poscode[3], pos[3]
int	axis, naxis, value, junk, ic, maxval
pointer	cd, img, sp, word, titlefmt, info, term

data	lencode	/ I_NAXIS1, I_NAXIS2, I_NAXIS3 /
data	poscode	/ I_AXIS1, I_AXIS2, I_AXIS3 /

string	fieldname  "row\tcolumn\tplane"
string	nofield    "Enter %s number."
string	nonumber   "Invalid %s number. Please reenter."
string	maxnumber  "Maximum %%s number is %d. Please reenter."

int	img_info(), lst_get(), lex_number(), nowhite(), itoc(), gstrcpy()
pointer	trm_info()

begin
	cd = OBJ_DESCRIP(cmd)
	img = trm_info (T_SELECT)

	call smark (sp)
	call salloc (word, CMD_MAXSTR, TY_CHAR)
	call salloc (titlefmt, CMD_MAXSTR, TY_CHAR)

	# Check command arguments until illegal argument found
	# or all arguments are exhausted

	naxis = img_info (img, I_NAXIS)
	for (axis = max (1, CMD_FIELD(cd)); axis <= naxis; axis = axis + 1) {

	    # Retrieve argument. Look first in the command string
	    # and then in the editing string. If not found, break.

	    if (lst_get (CMD_COMMAND(cd), axis, Memc[word], CMD_MAXSTR) == 0) {
		if (nowhite (CMD_STR(cd,0), Memc[word], CMD_MAXSTR) == 0) {
		    value = img_info (img, poscode[axis])
		    junk = itoc (value, Memc[word], CMD_MAXSTR)
		    call strcpy (nofield, Memc[titlefmt], CMD_MAXSTR)
		    break

		} else {
		    CMD_STR(cd,0) = EOS
		    call lst_put (CMD_COMMAND(cd), axis, 
				  Memc[word], CMD_MAXSTR)
		}
	    }

	    # Check to see if argument is a number

	    ic = 1
	    if (lex_number (Memc[word], ic, value) != LEX_OKAY) {
		call strcpy (nonumber, Memc[titlefmt], CMD_MAXSTR)
		break
	    }

	    # Check argument against size of image

	    maxval = img_info (img, lencode[axis])
	    if (value < 1 || value > maxval) {
		call sprintf (Memc[titlefmt], CMD_MAXSTR, maxnumber)
		call pargi (maxval)
		break
	    }
	}

	# If all arguments exist and are correct, send move command

	if (axis > naxis) {
	    do axis = 1, 3  {
		if (axis > naxis) {
		    pos[axis] = 1
		} else {
		    ic = 1
		    junk = lst_get (CMD_COMMAND(cd), axis, 
				    Memc[word], CMD_MAXSTR)
		    junk = lex_number (Memc[word], ic, pos[axis])
		}
	    }

	    call msg_alloc (info, LEN_IPOSSTRUCT, TY_STRUCT)
	    IPOS_INDEX(info) = 1
	    IPOS_ROW(info) = pos[1]
	    IPOS_COL(info) = pos[2]
	    IPOS_PLANE(info) = pos[3]

	    call msg_send (I_MOVE, img, info, NULL)

	    term = trm_info (T_TERM)
	    call msg_send (T_DELWIN, term, cmd, NULL)

	# Query for next argument to goto command

	} else {
	    call lst_put (CMD_COMMAND(cd), axis, "", CMD_MAXSTR)

	    CMD_FIELD(cd) = axis
	    CMD_STRLEN(cd) = gstrcpy (Memc[word], CMD_STR(cd,0), CMD_MAXSTR)
	    CMD_STRIDX(cd) = CMD_STRLEN(cd)

	    junk = lst_get (fieldname, axis-1, Memc[word], CMD_MAXSTR)
	    call sprintf (CMD_TITLE(cd), CMD_MAXSTR, Memc[titlefmt])
	    call pargstr (Memc[word])

	    if (CMD_WINDOW(cd) != 0)
		call cmd_draw (cmd)
	}

	call sfree (sp)
end

# CMD_HELP -- Process the help command

procedure cmd_help (cmd)

pointer cmd		# i: command descriptor
#--
pointer	term
pointer	trm_info()

begin
	term = trm_info (T_TERM)
	call msg_send (T_ADDWIN, term, O_HELP, NULL)
end

# CMD_QUIT -- Process the quit command

procedure cmd_quit (cmd)

pointer	cmd		# i: command descriptor
#--

begin
	call cmd_start (CMD_FINISH, "")
	call msg_send (M_DESTROY, cmd, NULL, NULL)
end

# CMD_QWRITE -- Query user about writing image

procedure cmd_qwrite (cmd)

pointer	cmd		# i: command descriptor
#--
int	ic, value
pointer	cd, name, img, term

string	query1  "Write %s?"
string	query2  "Please answer yes or no. Write %s?"

int	lex_yorn()
pointer	trm_info(), img_info()

begin
	cd = OBJ_DESCRIP(cmd)
	img = trm_info (T_SELECT)

	switch (CMD_FIELD(cd)) {
	case 0:
	    name = img_info (img, I_NAME)

	    call sprintf (CMD_TITLE(cd), CMD_MAXSTR, query1)
	    call pargstr (Memc[name])

	    CMD_FIELD(cd) = 1

	case 1:
	    ic = 1
	    if (lex_yorn (CMD_STR(cd,0), ic, value) == LEX_ERROR) {
		name = img_info (img, I_NAME)

		call sprintf (CMD_TITLE(cd), CMD_MAXSTR, query2)
		call pargstr (Memc[name])

		CMD_STR(cd,0) = EOS
		call cmd_draw (cmd)

	    } else {
		if (value == NO) {
		    call msg_send (I_SET, img, I_UPFILE, NO)
		} else {
		    call msg_send (I_WRITE, img, NO, NULL)
		}
		call cmd_start (CMD_FINISH, "")

		term = trm_info (T_TERM)
		call msg_send (T_DELWIN, term, cmd, NULL)
	    }
	}

end

# CMD_RDKEY -- Read a keystroke from the keyboard

procedure cmd_rdkey (cmd, ch, done)

pointer	cmd		# i: command descriptor
int	ch		# i: character read
int	done		# o: finished reading string?
#--
bool	moved
int	win, ic, jc, mc, nc
pointer	cd

string	maxline   "Maximum command length reached"

int	strlen()

begin
	done = NO
	cd = OBJ_DESCRIP(cmd)

	if (CMD_STRLEN(cd) >= CMD_MAXSTR) {
	    call strcpy (maxline, CMD_TITLE(cd), CMD_MAXSTR)
	    call cmd_draw (cmd)
	    return
	}

	# Check for carriage return

	win = CMD_WINDOW(cd)

	if (ch == '\r') {
	    CMD_STRIDX(cd) = CMD_STRLEN(cd)
	    CMD_NXTCHAR(cd) = EOS

	    done = YES
	    return

	} else if (IS_PRINT(ch)) {
	    if (CMD_STRIDX(cd) == CMD_STRLEN(cd)) {
		CMD_CURCHAR(cd) = ch
		CMD_NXTCHAR(cd) = EOS
		call waddstr (win, CMD_CURCHAR(cd))

	    } else {
		nc = CMD_STRLEN(cd) - CMD_STRIDX(cd)
		call amovc (CMD_CURCHAR(cd), CMD_NXTCHAR(cd), nc+1)

		CMD_CURCHAR(cd) = ch
		call winsch (win, CMD_CURCHAR(cd))
	    }

	    CMD_STRIDX(cd) = CMD_STRIDX(cd) + 1
	    CMD_STRLEN(cd) = CMD_STRLEN(cd) + 1

	} else {
	    moved = false
	    switch (ch) {
	    case K_UP:  # Move up one field
		;

	    case K_DOWN:  # Move down one field
		;

	    case K_RIGHT:  # Move right one column
		if (CMD_STRIDX(cd) < CMD_STRLEN(cd)) {
		    CMD_STRIDX(cd) = CMD_STRIDX(cd) + 1
		    moved = true
		}

	    case K_LEFT:  # Move left one column
		if (CMD_STRIDX(cd) > 0) {
		    CMD_STRIDX(cd) = CMD_STRIDX(cd) - 1
		    moved = true
		}

	    case K_NEXTW:  # Move forwards one word
		call mvword_next (CMD_STR(cd,0), CMD_STRIDX(cd), jc)

		if (jc > CMD_STRIDX(cd)) {
		    CMD_STRIDX(cd) = jc
		    moved = true
		}

	    case K_PREVW:  # Move backwards one word
		call mvword_prev (CMD_STR(cd,0), CMD_STRIDX(cd), jc)

		if (jc < CMD_STRIDX(cd)) {
		    CMD_STRIDX(cd) = jc
		    moved = true
		}

	    case K_NEXTP:  # Move forwards one screen
		;

	    case K_PREVP:  # Move backwards one screen
		;

	    case K_HOME:  # Move to first field
		;

	    case K_END:  # Move to last field
		;

	    case K_BOL:  # Move to first column in line
		if (CMD_STRIDX(cd) > 0) {
		    CMD_STRIDX(cd) = 0
		    moved = true
		}

	    case K_EOL:  # Move to last column in line
		if (CMD_STRIDX(cd) < CMD_STRLEN(cd)) {
		    CMD_STRIDX(cd) = CMD_STRLEN(cd)
		    moved = true
		}

	    case K_DEL:  # Delete character underneath cursor
		if (CMD_STRIDX(cd) < CMD_STRLEN(cd)) {
		    ic = CMD_STRIDX(cd)
		    mc = strlen (CMD_BUF(cd,0))
		    nc = CMD_STRLEN(cd) - ic

		    CMD_BUF(cd,mc) = CMD_STR(cd,ic)
		    CMD_BUF(cd,mc+1) = EOS

		    call amovc (CMD_STR(cd,ic+1), CMD_STR(cd,ic), nc+1)
		    CMD_STRLEN(cd) = CMD_STRLEN(cd) - 1

		    call wdelch (win)
		}

	    case K_BS:  # Delete character to left of cursor
		if (CMD_STRIDX(cd) > 0) {
		    ic = CMD_STRIDX(cd)
		    mc = strlen (CMD_BUF(cd,0))
		    nc = CMD_STRLEN(cd) - ic

		    call amovc (CMD_BUF(cd,0), CMD_BUF(cd,1), mc+1)
		    CMD_BUF(cd,0) = CMD_STR(cd,ic-1)

		    call amovc (CMD_STR(cd,ic), CMD_STR(cd,ic-1), nc+1)
		    CMD_STRIDX(cd) = CMD_STRIDX(cd) - 1
		    CMD_STRLEN(cd) = CMD_STRLEN(cd) - 1

		    call wmove (win, CMD_STRROW, CMD_CHCOL(cd))
		    call wdelch (win)
		}

	    case K_DWORD:  # Delete next word
		call mvword_next (CMD_STR(cd,0), CMD_STRIDX(cd), jc)

		if (jc > CMD_STRIDX(cd)) {
		    ic = CMD_STRIDX(cd)
		    mc = strlen (CMD_BUF(cd,0))
		    nc = CMD_STRLEN(cd) - jc + 1

		    call strcpy (CMD_STR(cd,ic), CMD_BUF(cd,mc), jc-ic)
		    call amovc (CMD_STR(cd,jc), CMD_STR(cd,ic), nc)

		    call wclrtoeol (win)
		    call waddstr (win, CMD_STR(cd,ic))

		    CMD_STRLEN(cd) = CMD_STRLEN(cd) - (jc - ic)
		    moved = true
		}

	    case K_DLINE:  # Delete rest of line
		if (CMD_STRLEN(cd) > 0) {
		    nc = CMD_STRLEN(cd) - CMD_STRIDX(cd)
		    call strcpy (CMD_CURCHAR(cd), CMD_BUF(cd,0), nc+1)

		    CMD_CURCHAR(cd) = EOS
		    CMD_STRLEN(cd) = CMD_STRIDX(cd)

		    call wclrtoeol (win)
		}

	    case K_UNDCHR: # Undelete a character
		mc = strlen (CMD_BUF(cd,0))

		if (mc > 0) {
		    ic = CMD_STRIDX(cd)
		    nc = CMD_STRLEN(cd) - ic

		    call amovc (CMD_STR(cd,ic), CMD_STR(cd,ic+1), nc+1)

		    CMD_STR(cd,ic) = CMD_BUF(cd,mc-1)
		    CMD_BUF(cd,mc-1) = EOS

		    CMD_STRIDX(cd) = CMD_STRIDX(cd) + 1
		    CMD_STRLEN(cd) = CMD_STRLEN(cd) + 1

		    call winsch (win, CMD_STR(cd,ic))
		}

	    case K_UNDWRD: # Undelete a word
		mc = strlen (CMD_BUF(cd,0))
		call mvword_prev (CMD_BUF(cd,0), mc, jc)

		mc = mc - jc
		if (mc > 0) {
		    ic = CMD_STRIDX(cd)
		    nc = CMD_STRLEN(cd) - ic

		    call amovc (CMD_STR(cd,ic), CMD_STR(cd,ic+mc), nc+1)
		    call amovc (CMD_BUF(cd,jc), CMD_STR(cd,ic), mc)
		    CMD_BUF(cd,jc) = EOS

		    CMD_STRIDX(cd) = CMD_STRIDX(cd) + mc
		    CMD_STRLEN(cd) = CMD_STRLEN(cd) + mc

		    call wclrtoeol (win)
		    call waddstr (win, CMD_STR(cd,ic))
		    moved = true
		}

	    case K_UNDLIN: # Undelete a line
		mc = strlen (CMD_BUF(cd,0))
		if (mc > 0) {
		    ic = CMD_STRIDX(cd)
		    nc = CMD_STRLEN(cd) - ic

		    call amovc (CMD_STR(cd,ic), CMD_STR(cd,ic+mc), nc+1)
		    call amovc (CMD_BUF(cd,0), CMD_STR(cd,ic), mc)
		    CMD_BUF(cd,0) = EOS

		    CMD_STRIDX(cd) = CMD_STRIDX(cd) + mc
		    CMD_STRLEN(cd) = CMD_STRLEN(cd) + mc

		    call wclrtoeol (win)
		    call waddstr (win, CMD_STR(cd,ic))
		    moved = true
		}

	    case K_HELP:  # Display help screen
		;

	    case K_PAINT:  # Redraw the screen
		call clearok (STDSCR, true)
		call wrefresh (STDSCR)
		moved = true

	    case K_EXIT:  # Exit procedure
		;
	    }
	}

	if (moved)
	    call wmove (win, CMD_STRROW, CMD_CHCOL(cd))

end

# CMD_RECEIVE -- Process messages sent to the command window

procedure cmd_receive (msg)

pointer	msg		# i: message descriptor
#--
int	done
pointer	cmd, cd, logger, info, errmsg

string	badobject "cmd_receive: object is not command window"
string	badkind   "cmd_receive: illegal message type"

pointer	trm_info()

begin
	cmd = MSG_OBJ(msg)

	if (OBJ_KIND(cmd) != O_CMD) {
	    logger = trm_info (T_LOG)
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badobject, Memc[errmsg], SZ_LINE)

	    call msg_send (L_ERROR, logger, errmsg, NULL)
	    return
	}

	switch (MSG_KIND(msg)) {
	case M_CREATE:
	    info = MSG_ARG1(msg)
	    if (CNEW_STRPTR(info) == NULL) {
		call cmd_create (cmd, CNEW_CODE(info), "")
	    } else {
		call cmd_create (cmd, CNEW_CODE(info), Memc[CNEW_STRPTR(info)])
	    }

	case M_DESTROY:
	    call cmd_destroy (cmd)

	case W_RDKEY:
	    call cmd_rdkey (cmd, MSG_ARG1(msg), done)
	    if (done == YES) {
		cd = OBJ_DESCRIP(cmd)
		call zcall1 (CMD_FUNC(cd), cmd)
	    }

	case W_RESIZE:
	    call cmd_size (cmd, MSG_ARG1(msg), MSG_ARG2(msg))

	case W_REDRAW:
	    call cmd_draw (cmd)

	case W_HIDDEN:
	    cd = OBJ_DESCRIP(cmd)
	    CMD_HIDDEN(cd) = MSG_ARG1(msg)

	case W_FOCUS:
	    call win_focus (cmd)

	default:
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badkind, Memc[errmsg], SZ_LINE)

	    logger = trm_info (T_LOG)
	    call msg_send (L_ERROR, logger, errmsg, NULL)
	}
end

# CMD_SIZE -- Resize and redraw the command window

procedure cmd_size (cmd, row, height)

pointer	cmd		# i: log window descriptor
int	row		# i: topmost row of log window
int	height		# i: height of log window
#--
int	oheight, width
pointer	ld

int	newwin()

begin
	ld = OBJ_DESCRIP(cmd)

	if (CMD_WINDOW(ld) != 0)
	    call delwin (CMD_WINDOW(ld))

	call wdimen (STDSCR, oheight, width)
	CMD_WINDOW(ld) = newwin (height, width, row, 1)

	call cmd_draw (cmd)
end

# CMD_START -- Start execution of a new command

procedure cmd_start (code, str)

int	code		# i: Command code
char	str[ARB]	# i: Command string
#--
pointer	term, info, ptr, logger, cmd

extern	cmd_receive
int	cmd_args()
pointer	trm_info(), obj_create()

begin
	# Create information structure for new command

	term = trm_info (T_TERM)

	call msg_alloc (info, LEN_CNEWSTRUCT, TY_STRUCT)
	call msg_alloc (ptr, CMD_MAXSTR, TY_CHAR)

	CNEW_CODE(info) = code
	CNEW_STRPTR(info) = ptr
	call strcpy (str, Memc[ptr], CMD_MAXSTR)

	# Create naked structure for commands with no arguments

	if (cmd_args (code) == 0) {
	    logger = trm_info (T_LOG)
	    cmd = obj_create (logger, O_CMD, cmd_receive)
	    call msg_send (M_CREATE, cmd, info, NULL)

	# Create command window for commands with arguments

	}else {
	    call msg_send (T_ADDWIN, term, O_CMD, info)
	}
end

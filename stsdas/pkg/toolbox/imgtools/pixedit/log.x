include	<curses.h>
include "object.h"
include "pixedit.h"
include "message.h"
include	"log.h"

# LOG_CLEAR -- Clear the log window

procedure log_clear (logger)

pointer logger		# i: message log descriptor
#--
pointer	ld

begin
	ld = OBJ_DESCRIP(logger)
	LOG_MESSAGE(ld) = EOS

	if (LOG_HIDDEN(ld) == NO)
	    call log_draw (logger)

end

# LOG_CREATE -- Create a new message log window

procedure log_create (logger, silent)

pointer	logger		# i: message log descriptor
int	silent		# i: don't ring bell when error occurs
#--
pointer	sp, ld, exit

string	helpfmt "Type %s quit to leave editor, %s help for help."

begin
	call smark (sp)
	call salloc (exit, SZ_FNAME, TY_CHAR)

	# Create the logger descriptor

	call malloc (ld, LEN_LOGSTRUCT, TY_STRUCT)
	OBJ_DESCRIP(logger) = ld

	# Fill in the fields of the descriptor

	LOG_WINDOW(ld) = 0
	LOG_HIDDEN(ld) = NO

	call malloc (LOG_MSGPTR(ld), SZ_LOGMSG, TY_CHAR)

	if (silent == NO) {
	    LOG_RING(ld) = YES
	} else {
	    LOG_RING(ld) = NO
	}

	# Write the initial help message  in the message string

	call k_eseq ("EXIT_UPDATE", Memc[exit], SZ_FNAME)

	call sprintf (LOG_MESSAGE(ld), SZ_LINE, helpfmt)
	call pargstr (Memc[exit])
	call pargstr (Memc[exit])

	call sfree (sp)
end

# LOG_DESTROY -- Eliminate the message log object

procedure log_destroy (logger)

pointer	logger		# i: logger descriptor
#--
pointer	ld

begin
	ld = OBJ_DESCRIP(logger)
	if (ld != NULL) {
	    if (LOG_WINDOW(ld) != 0)
		call delwin (LOG_WINDOW(ld))

	    call mfree (LOG_MSGPTR(ld), TY_CHAR)
	    call mfree (ld, TY_STRUCT)

	    OBJ_DESCRIP(logger) = NULL
	}

	call obj_destroy (logger)
end

# LOG_DRAW -- Redraw the contents of the log window

procedure log_draw (logger)

pointer	logger		# i: Message log descriptor
#--
int	win, row, col
pointer	ld, term, focus

pointer	trm_info()

begin
	ld = OBJ_DESCRIP(logger)

	if (LOG_HIDDEN(ld) == YES) {
	    term = trm_info (T_TERM)
	    call msg_send (T_FRONT, term, logger, NULL)
	    return
	}

	win = LOG_WINDOW(ld)
	if (win != 0) {
	    call wmove (win, 1, 1)
	} else {
	    win = STDSCR
	    call wdimen (win, row, col)
	    call wmove (win, row, 1)
	}

	call wclrtoeol (win)
	if (LOG_MESSAGE(ld) != EOS)
	    call waddstr (win, LOG_MESSAGE(ld))

	call wrefresh (win)

	# Move cursor to window with focus

	focus = trm_info (T_FOCUS)
	call msg_send (W_FOCUS, focus, NULL, NULL)
end

# LOG_FATAL -- Process fatal errors in the message passing code

procedure log_fatal (logger, text)

pointer	logger		# i: log message descriptor
char	text[ARB]	# i: text of error message
#--
pointer	sp, errmsg

begin
	call smark (sp)
	call salloc (errmsg, SZ_LOGMSG, TY_CHAR)

	call strcpy (text, Memc[errmsg], SZ_LOGMSG)
	call log_write (logger, L_ERROR, errmsg, NULL)

	call sfree (sp)
end

# LOG_RECEIVE -- Process messages sent to the log window

procedure log_receive (msg)

pointer	msg		# i: message descriptor
#--
pointer	sp, errmsg, logger, info, ld

string	badobject "log_receive: object is not message logger"
string	badkind   "log_receive: illegal message type"

pointer	trm_info()

begin
	call smark (sp)
	call salloc (errmsg, SZ_LOGMSG, TY_CHAR)

	logger = MSG_OBJ(msg)

	if (OBJ_KIND(logger) != O_LOG) {
	    logger = trm_info (T_LOG)
	    call strcpy (badobject, Memc[errmsg], SZ_LOGMSG)
	    call log_write (logger, L_ERROR, errmsg, NULL)

	    return
	}

	switch (MSG_KIND(msg)) {
	case M_CREATE:
	    info = MSG_ARG1(msg)
	    call log_create (logger, LNEW_SILENT(info))

	case M_DESTROY:
	    call log_destroy (logger)

	case W_RESIZE:
	    call log_size (logger, MSG_ARG1(msg), MSG_ARG2(msg))

	case W_REDRAW:
	    call log_draw (logger)

	case W_HIDDEN:
	    ld = OBJ_DESCRIP(logger)
	    LOG_HIDDEN(ld) = MSG_ARG1(msg)

	case W_FOCUS:
	    ; # ignore focus messages

	case L_ERROR,L_WARN,L_INFO:
	    call log_write (logger, MSG_KIND(msg), 
			    MSG_ARG1(msg), MSG_ARG2(msg))

	case L_CLEAR:
	    call log_clear (logger)

	default:
	    call strcpy (badkind, Memc[errmsg], SZ_LOGMSG)
	    call log_write (logger, L_ERROR, errmsg, NULL)
	}

	call sfree (sp)
end

# LOG_SIZE -- Resize and redraw the log window

procedure log_size (logger, row, height)

pointer	logger		# i: log window descriptor
int	row		# i: topmost row of log window
int	height		# i: height of log window
#--
int	oheight, width
pointer	ld

int	newwin()

begin
	ld = OBJ_DESCRIP(logger)

	if (LOG_WINDOW(ld) != 0)
	    call delwin (LOG_WINDOW(logger))

	call wdimen (STDSCR, oheight, width)
	LOG_WINDOW(ld) = newwin (height, width, row, 1)

	call log_draw (logger)
end

# LOG_WRITE -- Write a line to the message log window

procedure log_write (logger, kind, arg1, arg2)

pointer	logger		# i: log window descriptor
int	kind		# i: kind of error message
pointer	arg1		# i: first string to print
pointer	arg2		# i: second string to print
#--
pointer	sp, ld, msg

string	nologger  "log_write: no window for log messages"

begin
	call smark (sp)

	if (logger == NULL)
	    call error (1, nologger)

	ld = OBJ_DESCRIP(logger)

	# If this is an error message, save the text for the call to error()
	# otherwise write the text in the message buffer of the descriptor

	if (kind == L_ERROR) {
	    LOG_MESSAGE(ld) = EOS
	    call salloc (msg, SZ_LOGMSG, TY_CHAR)

	} else {
	    msg = LOG_MSGPTR(ld)
	}

	if (LOG_RING(ld) == YES && kind != L_INFO)
	    call ps_beep

	if (arg2 == NULL) {
	    call strcpy (Memc[arg1], Memc[msg], SZ_LOGMSG)
	} else {
	    call sprintf (Memc[msg], SZ_LOGMSG, "%s (%s)")
	    call pargstr (Memc[arg1])
	    call pargstr (Memc[arg2])
	}

	# This procedure clears the log window and writes any 
	# text in the message buffer. It is called unconditionally
	# because we want to clear the window even if error() 
	# prints the message

	call log_draw (logger)

	if (kind == L_ERROR)
	    call error (1, Memc[msg])

	call sfree (sp)
end

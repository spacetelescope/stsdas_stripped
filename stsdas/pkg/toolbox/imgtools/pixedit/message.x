include <config.h>
include "pixedit.h"
include "message.h"
include "object.h"

define	DBGLOG		NO
define	LOGFILE		"pixedit.log"

# MSG_ALLOC -- Allocate memory for the arguments of a message

procedure msg_alloc (ptr, size, type)

pointer	ptr		# o: pointer to memory block
int	size		# i: size of block in units of element's type
int	type		# i: data type requested
#--
include	"message.com"

int	totsize, delta
pointer	logger

string	nomemory  "msg_alloc: cannot allocate memory"

int	sizeof(), trm_info()
pointer	coerce()

begin
	# Calculate amount of memory to allocate

	totsize = sizeof (type) * size
	if (type == TY_CHAR)
	    totsize = totsize + 1

	delta = mod (totsize, SZ_MEMALIGN)
	if (delta != 0)
	    totsize = totsize + SZ_MEMALIGN - delta

	if ((endbuf + size) >= LEN_MSGBUF) {
	    logger = trm_info (T_LOG)
	    call log_fatal (logger, nomemory)
	}

	# Get pointer to memory block 
	# Compute new end of buffer and total message size

	ptr = coerce (msgbuf+endbuf, TY_CHAR, type)
	endbuf = endbuf + totsize
	
end

# MSG_CREATE -- Initialize the global variables used for passing messages

procedure msg_create ()

#--
include "message.com"

begin
	call malloc (queue, LEN_MSGQUEUE, TY_INT)
	call malloc (msgbuf, LEN_MSGBUF, TY_CHAR)

	frontq = 0
	endq = 0
	endbuf = 0

	msgkind = 0
end

# MSG_DBGLOG -- Print messages to log file

procedure msg_dbglog (msg)

pointer	msg		# i: message descriptor
char	str		# i: diagnostic string
#--
include "message.com"

long	aeon
pointer	sp, dbg, obj, date

data	aeon	/ 0 /

int	open()
long	clktime()

begin

	if (dbg != 0) {
	    switch (MSG_KIND(msg)) {
	    case M_CREATE:
		call fprintf (dbg, "Create object ")
	    case M_DESTROY:
		call fprintf (dbg, "Destroy object ")
	    case T_QUIT:
		call fprintf (dbg, "Quit editor ")
	    case T_REFOCUS:
		call fprintf (dbg, "Change window with focus ")
	    case T_ADDWIN:
		call fprintf (dbg, "Add window ")
	    case T_DELWIN:
		call fprintf (dbg, "Delete window ")
	    case T_FRONT:
		call fprintf (dbg, "Move window to front ")
	    case T_USEKEY:
		call fprintf (dbg, "Exit cursor mode ")
	    case T_USECUR:
		call fprintf (dbg, "Enter cursor mode ")
	    case W_RDKEY:
		call fprintf (dbg, "Read keyboard ")
	    case W_RDCUR:
		call fprintf (dbg, "Read cursor ")
	    case W_RESIZE:
		call fprintf (dbg, "Resize window ")
	    case W_REDRAW:
		call fprintf (dbg, "Redraw window ")
	    case W_HIDDEN:
		call fprintf (dbg, "Mark window as hidden or shown ")
	    case W_FOCUS:
		call fprintf (dbg, "Move cursor into window ")
	    case I_READ:
		call fprintf (dbg, "Read an image file ")
	    case I_WRITE:
		call fprintf (dbg, "Write an image file ")
	    case I_MOVE:
		call fprintf (dbg, "Move cursor in image window ")
	    case I_UPDATE:
		call fprintf (dbg, "Update pixel value ")
	    case I_SET:
		call fprintf (dbg, "Set image flag ")
	    case I_SETFMT:
		call fprintf (dbg, "Set image format ")
	    case L_ERROR:
		call fprintf (dbg, "Error message ")
	    case L_WARN:
		call fprintf (dbg, "Warning message ")
	    case L_INFO:
		call fprintf (dbg, "Informational message ")
	    case L_CLEAR:
		call fprintf (dbg, "Clear logger ")
	    default:
		call fprintf (dbg, "UNKNOWN ")
	    }

	    obj = MSG_OBJ(msg)
	    switch (OBJ_KIND(obj)) {
	    case O_TERMINAL:
		call fprintf (dbg, "<TERMINAL %d>\n")
	    case O_WINDOW:
		call fprintf (dbg, "<WINDOW %d>\n")
	    case O_IMAGE:
		call fprintf (dbg, "<IMAGE %d>\n")
	    case O_LOG:
		call fprintf (dbg, "<LOG WINDOW %d>\n")
	    case O_CMD:
		call fprintf (dbg, "<COMMAND WINDOW %d>\n")
	    case O_HELP:
		call fprintf (dbg, "<HELP WINDOW %d>\n")
	    default:
		call fprintf (dbg, "<UNKNOWN %d>\n")
	    }
	    call pargi (obj)
	    call flush (dbg)
	}
	return

entry msg_dbgwrite (str)

	if (dbg != 0) {
	    call fprintf (dbg, "%s\n")
	    call pargstr (str)

	    call flush (dbg)
	}
	return

entry msg_dbginit

	call smark (sp)
	call salloc (date, SZ_LINE, TY_CHAR)

	if (DBGLOG == YES) {
	    dbg = open (LOGFILE, APPEND, TEXT_FILE)

	    call cnvdate (clktime (aeon), Memc[date], SZ_LINE)
	    call fprintf (dbg, "*** %s ***\n")
	    call pargstr (Memc[date])

	} else {
	    dbg = 0
	}

	call sfree (sp)
	return
end

# MSG_DESTROY -- Remove memory buffer used for messages

procedure msg_destroy ()

#--
include "message.com"

begin
	call mfree (queue, TY_INT)
	call mfree (msgbuf, TY_CHAR)

end

# MSG_FREE -- Free memory allocated for message

procedure msg_free ()

#--
include "message.com"

begin
	endbuf = 0
end

# MSG_LOOP -- Main message processing loop

procedure msg_loop ()

#--
include	"message.com"

bool	done
int	input
pointer	sp, rdmsg, msg, obj

int	msg_receive()

begin
	call smark (sp)
	call salloc (rdmsg, LEN_MSG, TY_INT)

	done = false
	input = IO_KEYBOARD

	call msg_dbginit

	repeat {
	    # No pending message
	    if (msg_receive (msg) == 0) {
		if (done) {
		    break
		} else {
		    call msg_free
		    msg = rdmsg

		    if (input == IO_KEYBOARD) {
			call msg_rdkey (msg)
		    } else {
			call msg_rdcursor (msg)
		    }
		}
	    }

	    # Execute function bound to object

	    obj = MSG_OBJ(msg)
	    call msg_dbglog (msg)
	    call zcall1 (OBJ_FUNC(obj), msg)

	    # Set flags which change with message type

	    switch (MSG_KIND(msg)) {
	    case T_QUIT:
		 done = true
	    case T_USEKEY:
		input = IO_KEYBOARD
	    case T_USECUR:
		input = IO_CURSOR
	    }

	}

	call sfree (sp)
end

# MSG_RDCURSOR -- Create a message by reading the image cursor

procedure msg_rdcursor (msg)

pointer	msg		# u: message descriptor
#--
int	wcs, key, cmdlen
pointer	sp, command, info, cmd
real	row, col

int	clgcur(), strlen()
pointer	trm_info()

begin
	call smark (sp)
	call salloc (command, SZ_LINE, TY_CHAR)

	if (clgcur ("cur", row, col, wcs, key, 
		    Memc[command], SZ_LINE) == EOF) {

	    MSG_KIND(msg) = T_USEKEY
	    MSG_OBJ(msg) = trm_info (T_TERM)
	    MSG_ARG1(msg) = NULL
	    MSG_ARG2(msg) = NULL
	    
	} else {
	    MSG_KIND(msg) = W_RDCUR
	    MSG_OBJ(msg) = trm_info (T_SELECT)

	    call msg_alloc (info, LEN_CURSSTRUCT, TY_INT)
	    CURS_KEY(info) = key
	    if (IS_INDEFR (row) || IS_INDEFR (col)) {
		CURS_ROW(info) = 0
		CURS_COL(info) = 0
	    } else {
		CURS_ROW(info) = row + 0.5
		CURS_COL(info) = col + 0.5
	    }

	    MSG_ARG1(msg) = info
	    if (key != ':') {
		MSG_ARG2(msg) = NULL

	    } else {
		cmdlen = strlen (Memc[command])
		call msg_alloc (cmd, cmdlen, TY_CHAR)
		call strcpy (Memc[command], Memc[cmd], cmdlen)
		MSG_ARG2(msg) = cmd
	    }
	}

	call sfree (sp)
end

# MSG_RDKEY -- Create a message by reading the keyboard

procedure msg_rdkey (msg)

pointer	msg		# u: message descriptor
#--
int	ch

int	k_get()
pointer	trm_info()

begin
	call ps_synch
	ch = k_get ()

	MSG_KIND(msg) = W_RDKEY
	MSG_OBJ(msg) = trm_info (T_FOCUS)
	MSG_ARG1(msg) = ch
	MSG_ARG2(msg) = NULL

end

# MSG_RECEIVE -- Remove a message from the queue

int procedure msg_receive (msg)

pointer	msg		# o: message descriptor
#--
include "message.com"

begin
	if (frontq == endq) {
	    msg = NULL
	    return (0)
	}

	msg = queue + frontq
	frontq = mod (frontq + LEN_MSG, LEN_MSGQUEUE)

	return (MSG_KIND(msg))
end

# MSG_SEND -- Add a message to the queue

procedure msg_send (kind, obj, arg1, arg2)

int	kind		# i: kind of message
pointer	obj		# i: object receiving message
int	arg1		# i: first message argument
int	arg2		# i: second message argument
#--
include "message.com"

pointer	logger, msg

string	badmsg  "msg_send: unitialized message sent"
string	noroom  "msg_send: message queue is full"

int	trm_info()

begin
	# Make sure mandatory arguments are present

	if (kind == 0 || obj == NULL) {
	    logger = trm_info (T_LOG)
	    call log_fatal (logger, badmsg)
	}

	# Increment pointer to message buffer

	msg = queue + endq
	endq = mod (endq + LEN_MSG, LEN_MSGQUEUE)

	if (frontq == endq) {
	    logger = trm_info (T_LOG)
	    call log_fatal (logger, noroom)
	}

	# Set elements of stack structure

	MSG_KIND(msg) = kind
	MSG_OBJ(msg) = obj
	MSG_ARG1(msg) = arg1
	MSG_ARG2(msg) = arg2

end

include	<curses.h>
include	"object.h"
include "pixedit.h"
include "message.h"

# TRM_ADDBOT -- Add a new window to the bottom of the terminal screen

procedure trm_addbot (term, win)

pointer	term		# i: terminal descriptor
pointer	win		# i: window descriptor
#--
int	nrow, ncol
pointer	child

begin
	# Remove the window from the terminal list so it can be
	# reinserted in its proper location later

	call obj_remove (win)

	# Send message to change focus

	if (OBJ_KIND(win) != O_LOG)
	    call msg_send (T_REFOCUS, term, win, NULL)

	# Send message to resize window

	call wdimen (STDSCR, nrow, ncol)
	call msg_send (W_RESIZE, win, nrow-1, 2)

	# Find the bottom window on the terminal

	if (OBJ_CHILD(term) == NULL) {
	    child = NULL
	} else {
	    child = OBJ_CHILD(term)
	    while (OBJ_NEXT(child) != NULL)
		child = OBJ_NEXT(child)

	    if (IS_TOPWIN(child))
		child = NULL
	}

	# Update list of children of terminal
	# (which is list of the visible windows)

	if (child != NULL) {
	    call msg_send (W_HIDDEN, child, YES, NULL)
	    call obj_remove (child)
	}

	call obj_insert (term, win, NULL)
	call obj_insert (win, child, NULL)

end

# TRM_ADDTOP -- Add a new window to the top of the terminal screen

procedure trm_addtop (term, win, split)

pointer	term		# i: terminal descriptor
pointer	win		# i: window descriptor
int	split		# i: split the current screen
#--
include	"terminal.com"

bool	cover
int	nrow, ncol, row1, row2, height1, height2
pointer	child, obj

int	win_info()

begin
	# Remove the window from the terminal list so it can be
	# reinserted in its proper location later

	call obj_remove (win)

	# Send message to change focus

	call msg_send (T_REFOCUS, term, win, NULL)

	# Cover the entire top area if split == NO
	# or there is no image currently on the terminal

	if (OBJ_CHILD(term) == NULL) {
	    cover = true
	} else {
	    child = OBJ_CHILD(term)
	    cover = split == NO || IS_BOTWIN(child)
	}

	if (cover) {
	    # Send message to resize top window

	    call wdimen (STDSCR, nrow, ncol)
	    call msg_send (W_RESIZE, win, 1, nrow-2)

	    # Update list of children of terminal
	    # (which is list of the visible windows)

	    child = OBJ_CHILD(term)
	    call obj_insert (term, win, child)

	    while (child != NULL) {
		obj = child
		child = OBJ_NEXT(child)

		if (IS_TOPWIN(obj)) {
		    call msg_send (W_HIDDEN, obj, YES, NULL)

		    call obj_remove (obj)
		    call obj_insert (win, obj, NULL)
		}
	    }

	    if (focus == select)
		focus = win
	    if (OBJ_KIND(win) == O_IMAGE)
		select = win

	} else {

	    # Get dimensions of old and new windows

	    height1 = win_info (select, W_HEIGHT)
	    height2 = height1 / 2
	    height1 = height1 - height2

	    row1 = win_info (select, W_TOPROW)
	    row2 = row1 + height2

	    # Split the current window if there is enough room

	    if (height2 >= 4) {
		obj = select
		call msg_send (W_RESIZE, obj, row1, height1)
		call msg_send (W_RESIZE, win, row2, height2)

		# Update list of visible windows

		call obj_insert (term, win, OBJ_NEXT(obj))

	    # Otherwise, cover the current window

	    } else {
		obj = select
		call msg_send (W_RESIZE, win, row1, height1+height2)
		call msg_send (W_HIDDEN, obj, YES, NULL)

		# Update list of visible windows

		call obj_insert (term, win, OBJ_NEXT(obj))

		call obj_remove (obj)
		call obj_insert (win, obj, NULL)

		if (focus == select)
		    focus = win
		if (OBJ_KIND(win) == O_IMAGE)
		    select = win
	    }
	}
end

# TRM_DELBOT -- Delete a window from the bottom of terminal screen

procedure trm_delbot (term, win)

pointer	term		# i: terminal descriptor
pointer	win		# i: window descriptor
#--
include	"terminal.com"

pointer	child, errmsg

string	nodelete  "Cannot remove last window"

begin
	child = OBJ_CHILD(win)

	if (child == NULL) {
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (nodelete, Memc[errmsg], SZ_LINE)

	    call msg_send (L_WARN, logger, errmsg, NULL)

	} else {
	    if (focus == win)
		call msg_send (T_REFOCUS, term, select, NULL)

	    call msg_send (W_HIDDEN, child, NO, NULL)
	    call msg_send (W_REDRAW, child, NULL, NULL)

	    call obj_remove (win)
	    OBJ_CHILD(win) = NULL
	    call obj_insert (term, child, NULL)
	}

end

# TRM_DELTOP -- Delete a window from the top of terminal screen

procedure trm_deltop (term, win)

pointer	term		# i: terminal descriptor
pointer	win		# i: window descriptor
#--
include	"terminal.com"

int	row, height
pointer	child, nwin, obj, errmsg

int	win_info()

string	notfound  "trm_deltop: could not find window"
string	nodelete  "Cannot remove last window"

begin
	if (OBJ_CHILD(win) != NULL) {
	    nwin = OBJ_NEXT(win)
	    child = OBJ_CHILD(win)
	    call obj_remove (win)

	    if (focus == win)
		call msg_send (T_REFOCUS, term, child, NULL)

	    while (child != NULL) {
		obj = child
		child = OBJ_NEXT(child)
		call msg_send (W_HIDDEN, obj, NO, NULL)
		call msg_send (W_REDRAW, obj, NULL, NULL)

		call obj_insert (term, obj, nwin)
	    }

	} else {
	    if (OBJ_CHILD(term) == win) {
		nwin = OBJ_NEXT(win)
		row = win_info (win, W_TOPROW)

	    } else {
		nwin = OBJ_CHILD(term)

		while (OBJ_NEXT(nwin) != NULL) {
		    if (OBJ_NEXT(nwin) == win)
			break
		    nwin = OBJ_NEXT(nwin)
		}

		if (nwin == NULL) {
		    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
		    call strcpy (notfound, Memc[errmsg], SZ_LINE)

		    call msg_send (L_ERROR, logger, errmsg, NULL)
		    return
		}

		row = win_info (nwin, W_TOPROW)
	    }

	    if (IS_BOTWIN(nwin)) {
		call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
		call strcpy (nodelete, Memc[errmsg], SZ_LINE)

		call msg_send (L_WARN, logger, errmsg, NULL)
		return
	    }

	    # Change focus if current window has it

	    if (focus == win)
		call msg_send (T_REFOCUS, term, nwin, NULL)

	    # Resize adjacent window

	    height = win_info (win, W_HEIGHT) + win_info (nwin, W_HEIGHT)
	    call msg_send (W_RESIZE, nwin, row, height)

	    # Delete window from list of windows

	    call obj_remove (win)
	}

	OBJ_CHILD(win) = NULL
	OBJ_NEXT(win) = NULL
end

# TRM_CREATE -- Create the terminal object

procedure trm_create (term)

pointer	term		# o: terminal descriptor
#--
include	"terminal.com"

extern	trm_receive
pointer	obj_create()

begin
	# Allocate the object

	term = obj_create (NULL, O_TERMINAL, trm_receive)

	# Set the remaining global variables to null

	terminal = term
	focus = NULL
	select = NULL
	logger = NULL
	help = NULL
end

# TRM_DESTROY -- Destroy the terminal object and all its subobjects

procedure trm_destroy (term)

pointer	term		# i: terminal descriptor
#--

begin
	# Free the terminal object

	call obj_destroy (term)
end

# TRM_FRONT -- Move window to front of terminal

procedure trm_front (term, win)

pointer	term		# i: terminal descriptor
pointer	win		# i: window descriptor
#--
include	"terminal.com"

pointer	parent, front, errmsg

string	nofront  "trm_front: cannot bring window with siblings to front"

begin
	# Make sure window has no siblings

	parent = OBJ_PARENT(win)
	if (OBJ_NEXT(win) != NULL || OBJ_CHILD(parent) != win) {
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (nofront, Memc[errmsg], SZ_LINE)

	    call msg_send (L_ERROR, logger, errmsg, NULL)
	    return
	}

	# Find object which currently covers window

	front = win
	while (OBJ_PARENT(front) != term)
	    front = OBJ_PARENT(front)

	# Remove window from list

	call obj_remove (win)
	call obj_insert (parent, OBJ_CHILD(win), NULL)

	# Add window to list of visible windows

	call obj_insert (term, win, front)
	call msg_send (W_HIDDEN, win, NO, NULL)
	call msg_send (W_REDRAW, win, NULL, NULL)

	# Remove front window from list of visible windows

	call obj_remove (front)
	call msg_send (W_HIDDEN, front, YES, NULL)

	OBJ_CHILD(win) = front

	# Change focus

	if (focus == front && OBJ_KIND(win) != O_LOG)
	    focus = win

end

# TRM_INFO -- Get information from terminal structure

pointer	procedure trm_info (what)

int	what		# i: requested item of information
#--
include	"terminal.com"

pointer	obj, child, errmsg

string	badwhat    "trm_info: unknown item of info requested"

begin

	switch (what) {
	case T_TERM:
	    obj = terminal
	case T_FOCUS:
	    obj = focus
	case T_SELECT:
	    obj = select
	case T_LOG:
	    obj = logger
	case T_HELP:
	    obj = help
	case T_NEXT:
	    obj = NULL
	    child = OBJ_CHILD(terminal)
	    while (child != NULL) {
		if (child == select) {
		    obj = OBJ_NEXT(child)
		    break
		}
		child = OBJ_NEXT(child)
	    }

	    if (obj == NULL)
		obj = OBJ_CHILD(terminal)
	    if (IS_BOTWIN(obj))
		obj = OBJ_CHILD(terminal)
	    if (IS_BOTWIN(obj))
		obj = select

	case T_BOTTOM:
	    obj = OBJ_CHILD(terminal)
	    if (obj != NULL) {
		while (OBJ_NEXT(obj) != NULL)
		    obj = OBJ_NEXT(obj)

		if (IS_TOPWIN(obj))
		    obj = NULL
	    }
	default:
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badwhat, Memc[errmsg], SZ_LINE)

	    call msg_send (L_ERROR, logger, errmsg, NULL)
	    obj = NULL
	}

	return (obj)
end

# TRM_INIT -- Initialize the terminal by creating its default children

procedure trm_init (image, silent, rdonly, inplace)

char	image[ARB]	# image name
bool	silent		# don't ring bell when error occurs
bool	rdonly		# edit image read only
bool	inplace		# edit image in place
#--
pointer	term, info

int	btoi()

begin
	# Create the terminal object

	call trm_create (term)

	# Create the logging window

	call msg_alloc (info, LEN_LNEWSTRUCT, TY_STRUCT)
	LNEW_SILENT(info) = btoi (silent)

	call msg_send (T_ADDWIN, term, O_LOG, info)

	# Create the first image window

	call msg_alloc (info, LEN_INEWSTRUCT, TY_STRUCT)
	call msg_alloc (INEW_FNAME(info), SZ_FNAME, TY_CHAR)

	call strcpy (image, Memc[INEW_FNAME(info)], SZ_FNAME)
	INEW_RDONLY(info) = btoi (rdonly)
	INEW_INPLACE(info) = btoi (inplace)
	
	call msg_send (T_ADDWIN, term, O_IMAGE, info)

end

# TRM_RECEIVE -- Process messages sent to terminal

procedure trm_receive (msg)

pointer	msg		# i: message descriptor
#--
include	"terminal.com"


extern	img_receive, log_receive, cmd_receive, hlp_receive
pointer	obj, win, errmsg

string	badobject "trm_receive: object is not a terminal"
string	badkind   "trm_receive: illegal message type"
string	cursmode  "Cursor mode. Press ? for help."

int	trm_info()
pointer	obj_create()

begin
	obj = MSG_OBJ(msg)
	if (OBJ_KIND(obj) != O_TERMINAL) {
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badobject, Memc[errmsg], SZ_LINE)

	    call msg_send (L_ERROR, logger, errmsg, NULL)
	    return
	}

	switch (MSG_KIND(msg)) {
	case M_DESTROY, T_QUIT:
	    call trm_destroy (obj)

	case T_REFOCUS:
	    focus = MSG_ARG1(msg)
	    call msg_send (W_FOCUS, focus, NULL, NULL)

	    if (OBJ_KIND(focus) == O_IMAGE)
		select = focus

	case T_ADDWIN:
	    switch (MSG_ARG1(msg)) {
	    case O_IMAGE:
		win = obj_create (obj, O_IMAGE, img_receive)
		call msg_send (M_CREATE, win, MSG_ARG2(msg), NULL)
		call trm_addtop (obj, win, YES)
	    case O_LOG:
		win = obj_create (obj, O_LOG, log_receive)
		call msg_send (M_CREATE, win, MSG_ARG2(msg), NULL)
		call trm_addbot (obj, win)
		logger = win
	    case O_CMD:
		win = obj_create (obj, O_CMD, cmd_receive)
		call msg_send (M_CREATE, win, MSG_ARG2(msg), NULL)
		call trm_addbot (obj, win)
	    case O_HELP:
		win = obj_create (obj, O_HELP, hlp_receive)
		call msg_send (M_CREATE, win, MSG_ARG2(msg), NULL)
		call trm_addtop (obj, win, NO)
		help = win
	    }

	case T_DELWIN:
	    win = MSG_ARG1(msg)
	    switch (OBJ_KIND(win)) {
	    case O_IMAGE:
		call trm_deltop (obj, win)
	    case O_LOG:
		call trm_delbot (obj, win)
		logger = NULL
	    case O_CMD:
		call trm_delbot (obj, win)
	    case O_HELP:
		call trm_deltop (obj, win)
		help = NULL
	    }
	    call msg_send (M_DESTROY, win, NULL, NULL)

	case T_FRONT:
	    call trm_front (obj, MSG_ARG1(msg))

	case T_USEKEY:
	    logger = trm_info (T_LOG)
	    call msg_send (L_CLEAR, logger, NULL, NULL)

	case T_USECUR:
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (cursmode, Memc[errmsg], SZ_LINE)

	    logger = trm_info (T_LOG)
	    call msg_send (L_INFO, logger, errmsg, NULL)

	default:
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badkind, Memc[errmsg], SZ_LINE)

	    logger = trm_info (T_LOG)
	    call msg_send (L_ERROR, logger, errmsg, NULL)
	}

end



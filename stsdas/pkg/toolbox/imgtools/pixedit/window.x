include	<curses.h>
include "object.h"
include "pixedit.h"
include	"window.h"

# WIN_FOCUS -- Move cursor into window

procedure win_focus (win)

pointer	win		# i: window descriptor
#--
int	pixrow, pixcol
pointer	wd

int	winstat()
begin
	wd = OBJ_DESCRIP(win)
	if (wd == NULL)
	    return

	pixrow = winstat (WIN_WINDOW(wd), W_CURROW)
	pixcol = winstat (WIN_WINDOW(wd), W_CURCOL)

	call wmove (WIN_WINDOW(wd), pixrow, pixcol)
end

# WIN_INFO -- Retreive generic information about a window

int procedure win_info (win, what)

pointer	win		# i: window descriptor
int	what		# i: requested item of information
#--
int	width, info
pointer	wd, errmsg, logger

string	badwhat    "win_info: unknown item of info requested"

int	winstat(), trm_info()

begin
	wd = OBJ_DESCRIP(win)

	switch (what) {
	case W_TOPROW:
	    info = winstat (WIN_WINDOW(wd), W_TOP)

	case W_HEIGHT:
	    call wdimen (WIN_WINDOW(wd), info, width)

	default:
	    call msg_alloc (errmsg, SZ_LINE, TY_CHAR)
	    call strcpy (badwhat, Memc[errmsg], SZ_LINE)

	    logger = trm_info (T_LOG)
	    call msg_send (L_ERROR, logger, errmsg, NULL)
	}

	return (info)
end

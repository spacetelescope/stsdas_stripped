include "pixpos.h"

define	done_		90
define	MAX_TRY		 2
define	MAX_WORD	 3

# Copyright restrictions apply - see stsdas$copyright.stsdas 

# PIXCURSOR -- Process cursor commands
#
# This procedure is the main interactive loop for this task. Commands are 
# read from the image cursor until a "quit" command is received. The main
# command is the least squares fit, other commands add or delete catalog
# stars to the fit, write the fitted positions to the output file, or
# change the task parameters.
#
# B.Simon	03-Jul-90	Original

procedure pixcursor (par, pos, im, frm)

pointer par		# io: Parameter descriptor
pointer	pos		# io: Star position descriptor
pointer	im		# io: Image descriptor
pointer	frm		# io: Coordinate frame descriptor
#--
bool	done
double	ra, dec, solution[MAX_SOLUTION,2]
int	wcs, key, istar, junk, nterm, boxsize, ntry
pointer sp, temp, command
real	xlog, ylog, xnew, ynew

string	nullstr    ""

string	enterpos   "Enter new position of %s with cursor"
string	addedpos   "%s added to fit"
string	deletepos  "%s deleted from fit"
string	nodelete   "Cannot delete %s, not in fit"
string	fmtcoord   "The cursor position is RA:%12.1h Dec:%12.1h"

string	nocatstars "No stars in catalog were found on this image"
string	noname     "Star name not given"
string	badname    "Unrecognized star name: %s"
string	offimage   "Star %s unxpectedly off image"

int	clgcur(), near_pos(), getflag_param(), interact_cmd()
int	rdtval_pos(), wrtival_pos(), wrtrval_pos()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)
	call salloc (command, SZ_LINE, TY_CHAR)

	# Set solution array to zeroes

	call aclrd (solution, 2*MAX_SOLUTION)

	# Loop while cursor commands are received

	while (clgcur ("cur", xlog, ylog, wcs, key, 
		        Memc[command], SZ_LINE) != EOF) {
	    switch (key) {
	    # Add star to solution
	    case 'a':
		istar = near_pos (pos, xlog, ylog)
		if (istar < 1) {
		    call pixmessage (nocatstars, nullstr)
		} else {
		    call pixreply (pos, istar, enterpos)
		    if (clgcur ("cur", xlog, ylog, wcs, key, 
		        	Memc[command], SZ_LINE) == EOF)
			goto done_

		    call rdint_param (par, "boxsize", boxsize)
		    call starcenter (im, xlog, ylog, boxsize, MAX_TRY, 
				     ntry, xnew, ynew)

		    if (wrtival_pos (pos, "flag", FLAG_OUT, 
				     istar, FLAG_IN) == ERR) {
			junk = rdtval_pos (pos, "name", FLAG_NULL, istar,
					   Memc[temp], SZ_LINE)
			call pixerror (offimage, Memc[temp])
		    }

		    junk = wrtrval_pos (pos, "xnew", FLAG_IN, istar, xnew)
		    junk = wrtrval_pos (pos, "ynew", FLAG_IN, istar, ynew)

		    call pixreply (pos, istar, addedpos)
		}


	    # Delete star from solution
	    case 'd':
		istar = near_pos (pos, xlog, ylog)
		if (istar < 1) {
		    call pixmessage (nocatstars, nullstr)
		} else if (wrtival_pos (pos, "flag", FLAG_IN, 
			   istar, FLAG_OUT) == ERR) {
		    call pixreply (pos, istar, nodelete)
		} else {
		    call pixreply (pos, istar, deletepos)
		}

	    # Fit least squares solution
	    case 'f':
		call rdint_param (par, "nterm", nterm)
		call solve_fit (pos, frm, nterm, MAX_SOLUTION, solution, done)
		if (done) {
		    call stats_fit (pos, frm, MAX_SOLUTION, solution)
		    call putflag_param (par, "coord", YES)
		}

	    # List catalog position
	    case 'l':
		istar = near_pos (pos, xlog, ylog)
		if (istar < 1) {
		    call pixmessage (nocatstars, nullstr)
		} else {
		     call list_pos (pos, frm, MAX_SOLUTION, solution, istar)
		}

	    # RA and Dec of cursor position
 	    case 'p':
		call coord_fit (pos, frm, MAX_SOLUTION, solution, xlog, ylog,
				ra, dec)

		call sprintf (Memc[temp], SZ_LINE, fmtcoord)
		call pargd (ra)
		call pargd (dec)
		call pixmessage (Memc[temp], nullstr)

	    # Quit task
	    case 'q':
		goto done_

	    # Refresh display
	    case 'r':
		call putflag_param (par, "tv", YES)

	    # Show help file
	    case '?':
		call pixhelp

	    # Colon command
	    case ':':
		if (interact_cmd (command, par, pos, im, frm, solution) == EOF)
		   goto done_

	    # Illegal cursor command
	    default:
		call pixbeep

	    }

	    if (getflag_param (par, "im") == YES)
		call update_pos (par, pos, im, frm)

	    if (getflag_param (par, "tv") == YES)
		call pixdisplay (par, pos)

	    if (getflag_param (par, "cat") == YES)
		call ini_pos (par, pos)
	}

done_	if (getflag_param (par, "coord") == YES)
	    call wrt_coord (par, pos, frm, MAX_SOLUTION, solution)

	call sfree (sp)

end

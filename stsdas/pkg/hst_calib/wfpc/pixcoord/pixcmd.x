include	"pixpos.h"

define	MAX_WORD	7
define  CMD_STRING	"|add|delete|fit|list|next|pos|quit|set|write|"

# SCRIPT_CMD -- Execute a script of commands

procedure script_cmd (script, par, pos, im, frm)

char	script[ARB]		 #  i: Script file name
pointer par			 # io: Parameter descriptor
pointer	pos			 # io: Star position descriptor
pointer	im			 # io: Image descriptor
pointer	frm			 # io: Coordinate frame descriptor
#--
double	solution[MAX_SOLUTION,2]
int	fd, nc
pointer	sp, command

int	open(), getline(), nonint_cmd(), getflag_param()

begin
	# Allocate memory to hold strings

	call smark (sp)
	call salloc (command, SZ_LINE, TY_CHAR)

	# Set solution array to zeroes

	call aclrd (solution, 2*MAX_SOLUTION)

	# Open script

	fd = open (script, READ_ONLY, TEXT_FILE)

	# Loop until end of file or quit command

	repeat {
	    nc = getline (fd, Memc[command])

	    # Execute any non-empty line

	    if (nc > 1) {
		Memc[command+nc-1] = EOS
		nc = nonint_cmd (command, par, pos, im, frm, solution)

		if (getflag_param (par, "im") == YES)
		    call update_pos (par, pos, im, frm)

		if (getflag_param (par, "tv") == YES)
		    call pixdisplay (par, pos)

		if (getflag_param (par, "cat") == YES)
		    call ini_pos (par, pos)
	    }
	} until (nc == EOF)

	# Write output file if not yet written

	if (getflag_param (par, "coord") == YES)
	    call wrt_coord (par, pos, frm, MAX_SOLUTION, solution)

	# Close script amd free dynamic memory

	call close (fd)
	call sfree (sp)

end

# INTERACT_CMD -- Execute a command string interactively

int procedure interact_cmd (command, par, pos, im, frm, solution)

pointer command			 # io: Command buffer
pointer par			 # io: Parameter descriptor
pointer	pos			 # io: Star position descriptor
pointer	im			 # io: Image descriptor
pointer	frm			 # io: Coordinate frame descriptor
double	solution[MAX_SOLUTION,2] # io: Least squares solution
#--
bool	done, found
double	ra, dec
int	ic, wcs, key, istar, junk, nterm, boxsize, nword, ntry, max_try
pointer sp, temp, word[MAX_WORD]
real	xlog, ylog, xnew, ynew

string	nullstr    ""

string	enterpos   "Enter new position of %s with cursor"
string	addedpos   "%s added to fit"
string	deletepos  "%s deleted from fit"
string	nodelete   "Cannot delete %s, not in fit"
string	fmtcoord   "The position is RA:%12.1h Dec:%12.1h"

string	no_args    "Arguments not given for command: %s"
string	nocatstars "No stars in catalog were found on this image"
string	noname     "Star name not given"
string	badname    "Unrecognized star name: %s"
string	offimage   "Star %s unxpectedly off image"
string	nogroup    "No more groups in image"
string	novalue    "No value given for %s"
string	noparam    "Unknown parameter: %s"
string	newimage   "The new image is %s"
string	newparam   "Task parameter %s has been changed"
string	newcoord   "Coordinate file %s has been written"

string	cmdlist    CMD_STRING

int	strdic(), clgcur(), name_pos(), ctor(), pixnxtgrp()
int	getflag_param() rdtval_pos(), wrtival_pos(), wrtrval_pos()
int	wrt_param()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)

	# Parse command into words

	call pixparse (command, word, nword, MAX_WORD)

	# Choose command based on first word

	switch (strdic (Memc[word[1]], Memc[temp], SZ_LINE, cmdlist)) {
	# Add named star to fit
	case 1:
	    istar = name_pos (pos, Memc[word[2]])
	    if (istar < 1) {
		if (nword == 1) {
		    call pixmessage (noname, nullstr)
		} else {
		    call pixmessage (badname, Memc[word[2]])
		}

	    } else {
		call pixreply (pos, istar, enterpos)
		if (clgcur ("cur", xlog, ylog, wcs, key, 
			    Memc[command], SZ_LINE) == EOF)
			    return (EOF)

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

	# Delete named star from fit
	case 2:
	    istar = name_pos (pos, Memc[word[2]])
	    if (istar < 1) {
		if (nword == 1) {
		    call pixmessage (noname, nullstr)
		} else {
		    call pixmessage (badname, Memc[word[2]])
		}

	    } else if (wrtival_pos (pos, "flag", FLAG_IN, 
		       istar, FLAG_OUT) == ERR) {
		call pixreply (pos, istar, nodelete)
	    } else {
		call pixreply (pos, istar, deletepos)
	    }

	# Compute least squares fit
	case 3:
	    call rdint_param (par, "nterm", nterm)
	    call solve_fit (pos, frm, nterm, MAX_SOLUTION, solution, done)
	    if (done) {
		call stats_fit (pos, frm, MAX_SOLUTION, solution)
		call putflag_param (par, "coord", YES)
	    }

	# List star positions
	case 4:
	    if (nword == 1) {
		call list_pos (pos, frm, MAX_SOLUTION, solution, 0)
	    } else {
		istar = name_pos (pos, Memc[word[2]])
		if (istar < 1)
		    call pixmessage (badname, Memc[word[2]])

		call list_pos (pos, frm, MAX_SOLUTION, solution, istar)
	    }

	# Next group in image
	case 5:
	    if (pixnxtgrp (par, im) == OK) {
		if (getflag_param (par, "coord") == YES)
		    call wrt_coord (par, pos, frm, MAX_SOLUTION,
				    solution)

		call rdstr_param (par, "image", Memc[temp], SZ_LINE)
		call pixmessage (newimage, Memc[temp])

	    } else {
		call pixmessage (nogroup, nullstr)
	    }

	# Print star position 
	case 6:
	    found = true 	# changed pixerror to pixmessage (BPS 08.01.91)
	    ic = 1
	    if (ctor (Memc[word[2]], ic, xlog) == 0) {
		call pixmessage (no_args, Memc[word[1]])
		found = false
	    }
	    ic = 1
	    if (ctor (Memc[word[3]], ic, ylog) == 0) {
		call pixmessage (no_args, Memc[word[1]])
		found = false
	    }

	    if (found) {
		call coord_fit (pos, frm, MAX_SOLUTION, solution, xlog, ylog,
				ra, dec)

		call sprintf (Memc[temp], SZ_LINE, fmtcoord)
		call pargd (ra)
		call pargd (dec)
		call pixmessage (Memc[temp], nullstr)
	    }

	# Quit command loop
	case 7:
	    return (EOF)

	# Set task parameter
	case 8:
	    if (nword == 1) {
		call list_param (par)

	    } else if (nword == 2) {
		call pixmessage (novalue, Memc[word[2]])

	    } else if (wrt_param (par, Memc[word[2]], Memc[word[3]]) == ERR) {
		call pixmessage (noparam, Memc[word[2]])

	    } else {
		call pixmessage (newparam, Memc[word[2]])
	    }

	# Write coordinate file
	case 9:
	    if (nword == 2)	# Changed from 3 (BPS 08.01.91)
		junk = wrt_param (par, "coordfile", Memc[word[2]])
	    call wrt_coord (par, pos, frm, MAX_SOLUTION, solution)

	    call rdstr_param (par, "coordfile", Memc[temp], SZ_LINE)
	    call pixmessage (newcoord, Memc[temp])

	# Illegal colon command
	default:
	    call pixbeep
	    Memc[command] = EOS
	}

	call sfree (sp)
	return (nword)
end

# NONINT_CMD -- Execute a command string noninteractively

int procedure nonint_cmd (command, par, pos, im, frm, solution)

pointer command			 # io: Command buffer
pointer par			 # io: Parameter descriptor
pointer	pos			 # io: Star position descriptor
pointer	im			 # io: Image descriptor
pointer	frm			 # io: Coordinate frame descriptor
double	solution[MAX_SOLUTION,2] # io: Least squares solution
#--
bool	done
double	ra, dec
int	istar, ic, junk, nterm, boxsize, nword, ntry, max_try
pointer sp, temp, word[MAX_WORD]
real	xlog, ylog, xnew, ynew

string	nullstr    ""

string	fmtcoord   "The position is RA:%12.1h Dec:%12.1h"

string	no_args    "Arguments not given for command: %s"
string	nocatstars "No stars in catalog were found on this image"
string	noname     "Star name not given"
string	offimage   "Star %s unxpectedly off image"
string	badname    "Unrecognized star name: %s"
string	nodelete   "Cannot delete star, not in fit"
string	nogroup    "No more groups in image"
string	novalue    "No value given for %s"
string	noparam    "Unknown parameter: %s"
string	badcommand "Unrecogized command: %s"

string	cmdlist    CMD_STRING

int	strdic(), ctor(), name_pos(), pixnxtgrp()
int	getflag_param() rdtval_pos(), wrtival_pos(), wrtrval_pos()
int	wrt_param()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)

	# Break command into words. Return if no words on line

	call pixparse (command, word, nword, MAX_WORD)
	if (nword == 0)
	    return (0)

	# Choose command based on first word

	switch (strdic (Memc[word[1]], Memc[temp], SZ_LINE, cmdlist)) {
	# Add named star to fit
	case 1:
	    istar = name_pos (pos, Memc[word[2]])
	    if (istar < 1) {
		if (nword == 1) {
		    call pixerror (noname, nullstr)
		} else {
		    call pixerror (badname, Memc[word[2]])
		}

	    } else {
		ic = 1
		if (ctor (Memc[word[3]], ic, xlog) == 0)
		    call pixerror (no_args, Memc[word[1]])
		ic = 1
		if (ctor (Memc[word[4]], ic, ylog) == 0)
		    call pixerror (no_args, Memc[word[1]])

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
	    }

	# Delete named star from fit
	case 2:
	    istar = name_pos (pos, Memc[word[2]])
	    if (istar < 1) {
		if (nword == 1) {
		    call pixerror (noname, nullstr)
		} else {
		    call pixerror (badname, Memc[word[2]])
		}

	    } else if (wrtival_pos (pos, "flag", FLAG_IN, 
		       istar, FLAG_OUT) == ERR) {
		call pixerror (pos, nodelete)
	    }

	# Compute least squares fit
	case 3:
	    call rdint_param (par, "nterm", nterm)
	    call solve_fit (pos, frm, nterm, MAX_SOLUTION, solution, done)
	    if (done) {
		call stats_fit (pos, frm, MAX_SOLUTION, solution)
		call putflag_param (par, "coord", YES)
	    }

	# List star positions
	case 4:
	    if (nword == 1) {
		call list_pos (pos, frm, MAX_SOLUTION, solution, 0)
	    } else {
		istar = name_pos (pos, Memc[word[2]])
		if (istar < 1)
		    call pixmessage (badname, Memc[word[2]])

		call list_pos (pos, frm, MAX_SOLUTION, solution, istar)
	    }

	# Next group in image
	case 5:
	    if (pixnxtgrp (par, im) == OK) {
		if (getflag_param (par, "coord") == YES)
		    call wrt_coord (par, pos, frm, MAX_SOLUTION,
				    solution)

		call rdstr_param (par, "image", Memc[temp], SZ_LINE)

	    } else {
		call pixerror (nogroup, nullstr)
	    }

	# Print star position
	case 6:
	    ic = 1
	    if (ctor (Memc[word[2]], ic, xlog) == 0)
		call pixerror (no_args, Memc[word[1]])
	    ic = 1
	    if (ctor (Memc[word[3]], ic, ylog) == 0)
		call pixerror (no_args, Memc[word[1]])

	    call coord_fit (pos, frm, MAX_SOLUTION, solution, xlog, ylog,
			    ra, dec)

	    call sprintf (Memc[temp], SZ_LINE, fmtcoord)
	    call pargd (ra)
	    call pargd (dec)
	    call pixmessage (Memc[temp], nullstr)

	# Quit command loop
	case 7:
	    return (EOF)

	# Set task parameter
	case 8:
	    if (nword == 1) {
		call list_param (par)

	    } else if (nword == 2) {
		call pixerror (novalue, Memc[word[2]])

	    } else if (wrt_param (par, Memc[word[2]], Memc[word[3]]) == ERR) {
		call pixerror (noparam, Memc[word[2]])
	    }

	# Write coordinate file
	case 9:
	    if (nword == 2)
		junk = wrt_param (par, "coordfile", Memc[word[2]])
	    call wrt_coord (par, pos, frm, MAX_SOLUTION, solution)

	# Illegal command
	default:
	    call pixerror (badcommand, Memc[word[1]])
	}

	call sfree (sp)
	return (nword)
end

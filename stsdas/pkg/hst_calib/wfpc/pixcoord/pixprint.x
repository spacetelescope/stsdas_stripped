include	"pixpos.h"

# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# This file contains the procedures used to print messages read by the
# interactive user. Messages are all printed on STDERR. In addition to
# the routines in this file, pixpos.x and pixparam.x also have routines
# that print the contents of their data structures
#
# B.Simon	05-Jul-90	Original

# PIXBEEP -- Sound the bell on the terminal

procedure pixbeep ()

#--

begin
	call putci (STDERR, '\007')
	call flush (STDERR)
end

# PIXERROR -- Fatal error, print message and exit

procedure pixerror (format, value)

char	format[ARB]	# i: Error message format
char	value[ARB]	# i: Value which caused error
#--
pointer	sp, errmsg

begin
	# Just build error message and quit

	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	call sprintf (Memc[errmsg], SZ_LINE, format)
	call pargstr (value)

	call error (1, Memc[errmsg])
	call sfree (sp)

end

# PIXHELP -- Print the help message on the screen

procedure pixhelp ()

#--
char	hline[80,23]
int	ihelp

equivalence  (hline[1,1], help1)
equivalence  (hline[1,2], help2)
equivalence  (hline[1,3], help3)
equivalence  (hline[1,4], help4)
equivalence  (hline[1,5], help5)
equivalence  (hline[1,6], help6)
equivalence  (hline[1,7], help7)
equivalence  (hline[1,8], help8)
equivalence  (hline[1,9], help9)
equivalence  (hline[1,10], help10)
equivalence  (hline[1,11], help11)
equivalence  (hline[1,12], help12)
equivalence  (hline[1,13], help13)
equivalence  (hline[1,14], help14)
equivalence  (hline[1,15], help15)
equivalence  (hline[1,16], help16)
equivalence  (hline[1,17], help17)
equivalence  (hline[1,18], help18)
equivalence  (hline[1,19], help19)
equivalence  (hline[1,20], help20)
equivalence  (hline[1,21], help21)
equivalence  (hline[1,22], help22)
equivalence  (hline[1,23], help23)

string	help1  "\tHelp file for the pixcoord task\n"
string	help2  "\n"
string	help3  "\ta\t\t Add a star to the least squares fit\n"
string	help4  "\td\t\t Delete a star from the least squares fit\n"
string	help5  "\tf\t\t Fit the least squares solution\n"
string	help6  "\tl\t\t List the position of the nearest catalog star\n"
string	help7  "\tp\t\t Print the ra and dec of the cursor position\n"
string	help8  "\tq\t\t Quit the task\n"
string	help9  "\tr\t\t Redisplay the image\n"
string	help10 "\t?\t\t Show this help file\n"
string	help11 "\t:\t\t Commands with arguments, see below\n"
string	help12 "\n"
string	help13 "\t:add name\t Add named star to the least squares fit\n"
string	help14 "\t:delete name\t Delete named star from least squares fit\n"
string	help15 "\t:fit\t\t Fit the least squares solution\n"
string	help16 "\t:list [name]\t List the catalog stars [Name is optional]\n"
string	help17 "\t:next\t\t Next group in this image\n"
string	help18 "\t:pos x y\t Print the ra and dec of pixel (x,y)\n"
string	help19 "\t:quit\t\t Quit this task\n"
string	help20 "\t:set param = val Set task parameter to new value\n"
string	help21 "\t:write [name]\t Write coordinate file. [Name is optional]\n"
string	help22 "\n"
string	help23 "\tColon commands can be abbreviated to their first letter\n"

begin
	do ihelp = 1, 23
	    call putline (STDERR, hline[1,ihelp])
	call flush (STDERR)
end

# PIXMESSAGE -- Send a message to standard error

procedure pixmessage (format, value)

char	format[ARB]	# i: Error message format
char	value[ARB]	# i: Value which caused warning
#--

begin

	call fprintf (STDERR, format)
	if (value[1] != EOS)
	    call pargstr (value)

	call putci (STDERR, '\n')
	call flush (STDERR)

end

# PIXREPLY -- Print a reply message to a cursor command

procedure pixreply (pos, istar, format)

pointer	pos		# i: Position descriptor
int	istar		# i: Star number
char	format[ARB]	# i: Format string
#--
char	newline
int	stat
double	xold, yold, xnew, ynew
pointer	sp, tag, name

data	newline  / '\n' /

int	rdtval_pos(), rddval_pos()

begin
	call smark (sp)
	call salloc (tag, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)

	stat = rdtval_pos (pos, "name", FLAG_NULL, istar, Memc[name], SZ_LINE)

	stat = rddval_pos (pos, "xold", FLAG_OUT, istar, xold)
	stat = rddval_pos (pos, "yold", FLAG_OUT, istar, yold)
	if (stat == ERR) {
	    call strcpy (Memc[name], Memc[tag], SZ_LINE)
	} else {
	    stat = rddval_pos (pos, "xnew", FLAG_IN, istar, xnew)
	    stat = rddval_pos (pos, "ynew", FLAG_IN, istar, ynew)
	    if (stat == ERR) {
		call sprintf (Memc[tag], SZ_LINE, "%s (%0.1f, %0.1f)")
		call pargstr (Memc[name])
		call pargd (xold)
		call pargd (yold)
	    } else {
		call sprintf (Memc[tag], SZ_LINE, 
			      "%s (%0.1f, %0.1f) => (%0.1f, %0.1f)")
		call pargstr (Memc[name])
		call pargd (xold)
		call pargd (yold)
		call pargd (xnew)
		call pargd (ynew)
	    }
	}

	call fprintf (STDERR, format)
	call pargstr (Memc[tag])
	call putc (STDERR, newline)
	call flush (STDERR)

	call sfree (sp)
end


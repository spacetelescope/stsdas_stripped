include	"pixparam.h"

# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# This file contains the procedures which manipulate the parameter 
# structure. The parameter structure conains the values of the task
# parameters which can be modified by the user interactively.
#
# B.Simon	22-Jun-90	Original

# FREE_PARAM -- Free parameter structure

procedure free_param (par)

pointer	par		# i: Parameter structure
#--
include "pixparam.com"

begin
	oldname[1] = EOS

	call mfree (PAR_BUFFER(par), TY_CHAR)
	call mfree (PAR_STRVEC(par), TY_STRUCT)
	call mfree (PAR_INTVEC(par), TY_STRUCT)

	call mfree (par, TY_STRUCT)
end

# GETFLAG_PARAM -- Retrieve the value of the parameter flags

int procedure getflag_param (par, flag)

pointer	par		# i: Parameter structure
char	flag[ARB]	# i: Flag name
#--
int on

string	flag_list  PAR_FLAGNAMES
string	badflag    "Illegal flag name (%s)"

int	word_match()

begin

	switch (word_match (flag, flag_list)) {
	case 1:
	    on = PAR_IMFLAG(par)
	case 2:
	    on = PAR_TVFLAG(par)
	case 3:
	    on = PAR_CATFLAG(par)
	case 4:
	    on = PAR_COORDFLAG(par)
	default:
	    call pixerror (badflag, flag)
	}

	return (on)
end

# INIT_PARAM -- Initialize the parameter structure by reading the parameters

procedure init_param (par)

pointer	par		# o: Parameter structure
#--
include "pixparam.com"

int	ic, ivec
pointer	sp, parname, parval

string	strlist	 PAR_STRNAMES
string	intlist  PAR_INTNAMES

int	word_fetch(), clgeti()

begin
	call smark (sp)
	call salloc (parname, PAR_MAXSTR, TY_CHAR)

	# Allocate memory for parameter structure

	call malloc (par, PAR_LENGTH, TY_STRUCT)
	call malloc (PAR_BUFFER(par), PAR_NUMSTR*(PAR_MAXSTR+1), TY_CHAR)
	call malloc (PAR_STRVEC(par), PAR_NUMSTR, TY_STRUCT)
	call malloc (PAR_INTVEC(par), PAR_NUMINT, TY_STRUCT)

	# Read string parameters into structure

	ic = 1
	ivec = 1
	parval = PAR_BUFFER(par)
	while (word_fetch (strlist, ic, Memc[parname], PAR_MAXSTR) > 0) {
	    call clgstr (Memc[parname], Memc[parval], PAR_MAXSTR)
	    PAR_STRPTR(par,ivec) = parval
	    ivec = ivec + 1
	    parval = parval + PAR_MAXSTR + 1
	}

	# Read integer parameters into structure

	ic = 1
	parval = PAR_INTVEC(par)
	while (word_fetch (intlist, ic, Memc[parname], PAR_MAXSTR) > 0) {
	    Memi[parval] = clgeti (Memc[parname])
	    parval = parval + 1
	}

	# Set flags

	PAR_IMFLAG(par) = NO
	PAR_TVFLAG(par) = NO
	PAR_CATFLAG(par) = NO
	PAR_COORDFLAG(par) = NO

	oldname[1] = EOS
	call sfree (sp)
end

# LIST_PARAM -- Display parameter values on standard output

procedure list_param (par)

pointer	par		# o: Parameter structure
#--
int	ic, ivec
pointer	sp, parname, parval

string	strlist	 PAR_STRNAMES
string	intlist  PAR_INTNAMES

int	word_fetch()

begin
	call smark (sp)
	call salloc (parname, PAR_MAXSTR, TY_CHAR)

	ic = 1
	ivec = 1
	while (word_fetch (strlist, ic, Memc[parname], PAR_MAXSTR) > 0) {
	    parval = PAR_STRPTR(par,ivec)
	    call fprintf (STDERR, "%12s = %s\n")
	    call pargstr (Memc[parname])
	    call pargstr (Memc[parval])

	    ivec = ivec + 1
	}

	ic = 1
	parval = PAR_INTVEC(par)
	while (word_fetch (intlist, ic, Memc[parname], PAR_MAXSTR) > 0) {
	    call fprintf (STDERR, "%12s = %d\n")
	    call pargstr (Memc[parname])
	    call pargi (Memi[parval])

	    parval = parval + 1
	}

	call flush (STDERR)
	call sfree (sp)
end

# PUTFLAG_PARAM -- Change the value of the parameter flags

procedure putflag_param (par, flag, on)

pointer	par		# i: Parameter structure
char	flag[ARB]	# i: Flag name
int	on		# i: Flag value (yes or no)
#--
string	flag_list  PAR_FLAGNAMES
string	badflag    "Illegal flag name (%s)"

int	word_match()

begin

	switch (word_match (flag, flag_list)) {
	case 1:
	    PAR_IMFLAG(par) = on
	case 2:
	    PAR_TVFLAG(par) = on
	case 3:
	    PAR_CATFLAG(par) = on
	case 4:
	    PAR_COORDFLAG(par) = on
	default:
	    call pixerror (badflag, flag)
	}

end

# RDINT_PARAM -- Read an integer parameter

procedure rdint_param (par, name, value)

pointer	par		# i: Parameter structure
char	name[ARB]	# i: Parameter name
int	value		# o: Parameter value
#--
int	index

string	intlist   PAR_INTNAMES
string	badname   "Task parameter name not recognized (%s)"

int	word_match()

begin

	index = word_match (name, intlist)
	if (index == 0) {
	    value = 0
	    call pixerror (badname, name)
	} else {
	    value = PAR_INTVAL(par,index)
	}

end

# RDSTR_PARAM -- Read a string parameter

procedure rdstr_param (par, name, value, maxch)

pointer	par		# i: Parameter structure
char	name[ARB]	# i: Parameter name
char	value[ARB]	# o: Parameter value
int	maxch		# i: Declared length of value
#--
int	index

string	strlist   PAR_STRNAMES
string	badname   "Task parameter name not recognized (%s)"

int	word_match()

begin

	index = word_match (name, strlist)
	if (index == 0) {
	    value[1] = EOS
	    call pixerror (badname, name)
	} else {
	    call strcpy (PAR_STRVAL(par,index), value, maxch)
	}

end

procedure query_param (par, name, prompt)

pointer	par		# i: Parameter structure
char	name[ARB]	# i: Parameter name
char	prompt[ARB]	# i: Query prompt
#--
int	type, index, nc, wrtpar
pointer	sp, value

string	intlist   PAR_INTNAMES
string	strlist   PAR_STRNAMES
string	badname   "Task parameter name not recognized (%s)"

int	word_match(), itoc(), getline(), wrt_param()

begin

	call smark (sp)
	call salloc (value, PAR_MAXSTR, TY_CHAR)

	# Use the parameter name to determine the type
	# If the name is not found, return an error 

	index = word_match (name, intlist)
	if (index > 0) {
	    type = TY_INT
	} else {
	    type = TY_CHAR
	    index = word_match (name, strlist)
	}

	if (index == 0)
	    call pixerror (badname, name)

	if (type == TY_CHAR) {
	    call strcpy (PAR_STRVAL(par,index), Memc[value], PAR_MAXSTR)
	} else {
	    nc = itoc (PAR_INTVAL(par,index), Memc[value], PAR_MAXSTR)
	}

	call pixmessage (prompt, Memc[value])
	call fprintf (STDERR, "%s? ")
	call pargstr (name)
	call flush (STDERR)

	nc = getline (STDIN, Memc[value])
	if (nc <= 0) {
	    call pixerror (prompt, name)
	} else {
	    Memc[value+nc-1] = EOS	# remove newline from string
	}

	wrtpar = wrt_param (par, name, Memc[value])
	call sfree (sp)

end

# REST_PARAM -- Restore old value of parameter

procedure rest_param (par, name)

pointer	par		# i: Parameter structure
char	name[ARB]	# i: Parameter name
#--
include	"pixparam.com"

string	norestore  "Could not restore value of parameter (%s)"
string	badname    "Illegal parmeter name (%s)"

bool	strne()
int	wrt_param ()

begin
	if (strne (name, oldname)) {
	    if (oldname[1] == EOS) {
		call pixerror (norestore, name)
	    } else {
		call pixmessage (norestore, name)
		return
	    }
	}

	if (wrt_param (par, name, oldvalue) == ERR)
	    call pixerror (badname, name)

end

# SAVE_PARAM -- Save the value of a task parameter

procedure save_param (par, name)

pointer	par		# i: Parameter structure
char	name[ARB]	# i: Parameter name
#--
include	"pixparam.com"

int	index, intval, nc

string	intlist   PAR_INTNAMES
string	strlist   PAR_STRNAMES
string	badname   "Task parameter name not recognized (%s)"

int	word_match(), itoc ()

begin
	# Use the parameter name to determine the type
	# If the name is not found, exit with an error

	index = word_match (name, intlist)
	if (index > 0) {
	    intval = PAR_INTVAL(par,index)
	    nc = itoc (intval, oldvalue, PAR_MAXSTR)

	} else {
	    index = word_match (name, strlist)
	    if (index == 0)
		call pixerror (badname, name)

	    call strcpy (PAR_STRVAL(par,index), oldvalue, PAR_MAXSTR)
	}

	# Save parameter name

	call strcpy (name, oldname, PAR_MAXSTR)
end

# UPDFLG_PARAM -- Update the flag parameters

procedure updflg_param (par, name)

pointer	par		# i: Parameter structure
char	name[ARB]	# i: Parameter name
#--
string	im_list	 "image,catalog,name,ra,dec"
string	tv_list	 "image,tvcmd,tvframe,label,mkcolor,mksize"
string	cat_list "catalog,name,ra,dec"

int	word_match()

begin

	if (word_match (name, im_list) > 0)
	    call putflag_param (par, "im", YES)

	if (word_match (name, tv_list) > 0)
	    call putflag_param (par, "tv", YES)

	if (word_match (name, cat_list) > 0)
	    call putflag_param (par, "cat", YES)

end

# WRT_PARAM -- Write a task parameter

int procedure wrt_param (par, name, strval)

pointer	par		# i: Parameter structure
char	name[ARB]	# i: Parameter name
char	strval[ARB]	# i: Parameter value
#--
int	type, index, intval, ic, nc

string	intlist   PAR_INTNAMES
string	strlist   PAR_STRNAMES

int	word_match(), ctoi ()

begin
	# Use the parameter name to determine the type
	# If the name is not found, return an error 

	index = word_match (name, intlist)
	if (index > 0) {
	    type = TY_INT
	} else {
	    type = TY_CHAR
	    index = word_match (name, strlist)
	}

	if (index == 0)
	    return (ERR)

	# Save old parameter value

	call save_param (par, name)

	# Copy the new value into the parameter structure
	# No checking is done to see if the parameter is valid,
	# this must be done by the calling procedure.

	if (type == TY_CHAR) {
	    call strcpy (strval, PAR_STRVAL(par,index), PAR_MAXSTR)

	} else {
	    ic = 1
	    nc = ctoi (strval, ic, intval)
	    if (nc == 0)
		return (ERR)

	    PAR_INTVAL(par,index) = intval
	}

	# Update the flag parameters

	call updflg_param (par, name)
	return (OK)
end


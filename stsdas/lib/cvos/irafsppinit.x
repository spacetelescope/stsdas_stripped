# modification of iraf_main.x for standalone initialization
#
# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
#
# M.D. De La Pena  01 September 1998: Removed unused function declarations for
# streq, oscmd, and sys_getcommand from irinit routine.
# M.D. De La Pena  23 April 1999: Call cvos_ideh() instead of ma_ideh() to
# install the default exception handlers for C programs compiled as host 
# level tasks.

include	<config.h>
include	<error.h>
include	<syserr.h>
include	<clset.h>
include	<fset.h>
include	<ctype.h>
include	<printf.h>
include	<xwhen.h>
include	<knet.h>

define	SZ_VALSTR		SZ_COMMAND
define	SZ_CMDBUF		(SZ_COMMAND+1024)
define	SZ_TASKNAME		32
define	TIMEIT_CHAR		'$'
define	MAXFD			5		# max redirectable fd's
define	STARTUP			0		# stages of execution
define	SHUTDOWN		1
define	IDLE			2
define	EXECUTING		3
define	DUMMY			finit		# any procedure will do


# IRAF_MAIN -- Execute commands read from the standard input until the special
# command "bye" is received, initiating process shutdown.  The arguments tell
# the process type (connected, detached, or host) and identify the process
# standard i/o channels and device driver to be used.

procedure irinit (a_cmd, a_inchan, a_outchan, a_errchan,
	a_driver, a_devtype, prtype, bkgfile, jobcode, onentry)

char	a_cmd[ARB]		# command to be executed or null string
int	a_inchan		# process standard input
int	a_outchan		# process standard output
int	a_errchan		# process standard error output
int	a_driver		# ZLOCPR address of device driver
int	a_devtype		# device type (text or binary)
int	prtype			# process type (connected, detached, host)
char	bkgfile[ARB]		# packed filename of bkg file if detached
int	jobcode			# jobcode if detached process
extern	onentry()		# client onentry procedure

bool	networking
int	inchan, outchan, errchan, driver, devtype
char	cmd[SZ_CMDBUF], taskname[SZ_TASKNAME], bkgfname[SZ_FNAME]
int	arglist_offset, timeit, junk, interactive, builtin_task, cmdin
int	jumpbuf[LEN_JUMPBUF], status, state, interpret, i
long	save_time[2]
pointer	sp
int	ruk_eawarn

extern	DUMMY()
int	access(), envscan(), onentry(), stropen()
errchk	xonerror, fio_cleanup
common	/JUMPCOM/ jumpbuf
string	nullfile "dev$null"
data	networking /KNET/
data	ruk_eawarn /3/
define	shutdown_ 91

# The following common is required on VMS systems to defeat the Fortran
# optimizer, which would otherwise produce optimizations that would cause
# a future return from ZSVJMP to fail.  Beware that this trick may fail on
# other systems with clever optimizers.

common	/zzfakecom/ state

begin
	# The following initialization code is executed upon process
	# startup only.

	state = STARTUP
	call zsvjmp (jumpbuf, status)
	if (status != OK)
	    call sys_panic (0, "fatal error during process startup")

	# Install the standard exception handlers, but if we are a connected
	# subprocess do not enable interrupts until process startup has
	# completed.

	call cvos_ideh()
	if (prtype == PR_CONNECTED)
	    call intr_disable()

	inchan  = a_inchan
	outchan = a_outchan
	errchan = a_errchan
	driver  = a_driver
	devtype = a_devtype

	# If the system is configured with networking initialize the network
	# interface and convert the input channel codes and device driver
	# code to their network equivalents.

	if (networking)
	    call ki_init (inchan, outchan, errchan, driver, devtype)

	# Other initializations.
	call env_init()
	call fmt_init (FMT_INITIALIZE)			# init printf
	call xer_reset()				# init error checking
	call erract (OK)				# init error handling
	call onerror (DUMMY)				# init onerror
	call onexit  (DUMMY)				# init onexit 
	call finit()					# initialize FIO
	call clopen (inchan, outchan, errchan, driver, devtype)
	call clseti (CL_PRTYPE, prtype)
	call clc_init()					# init param cache
	call strupk (bkgfile, bkgfname, SZ_FNAME)

	# If we are running as a host process (no IRAF parent process) look
	# for the file "zzsetenv.def" in the current directory and then in
	# the system library, and initialize the environment from this file
	# if found.  This works because the variable "iraf$" is defined at
	# the ZGTENV level.

	interactive = NO
	if (prtype == PR_HOST) {
	    interactive = YES
	    if (access ("zzsetenv.def",0,0) == YES) {
		iferr (junk = envscan ("set @zzsetenv.def"))
		    ;
	    } else if (access ("host$hlib/zzsetenv.def",0,0) == YES) {
		iferr (junk = envscan ("set @host$hlib/zzsetenv.def"))
		    ;
	    }
	}

	# Save context for error restart.  If an error occurs execution
	# resumes just past the ZSVJMP statement with a nonzero status.

	call smark (sp)
	call zsvjmp (jumpbuf, status)

	if (status != OK) {
	    # Give up if error occurs during shutdown.
	    if (state == SHUTDOWN)
		call sys_panic (0, "fatal error during process shutdown")

	    # Tell error handling package that an error restart is in
	    # progress (necessary to avoid recursion).

	    call erract (EA_RESTART)

	    iferr {
		# Call user cleanup routines and then clean up files system.
		# Make sure that user cleanup routines are called FIRST.

		call xonerror (status)
		call cvos_ideh()
		call flush (STDERR)
		do i = CLIN, STDPLOT
		    call fseti (i, F_CANCEL, OK)
		call fio_cleanup (status)
		call fmt_init (FMT_INITIALIZE)
		call sfree (sp)
	    } then
		call erract (EA_FATAL)			# panic abort

	    # Send ERROR statement to the CL, telling the CL that the task
	    # has terminated abnormally.  The CL will either kill us, resulting
	    # in error restart with status=SYS_XINT, or send us another command
	    # to execute.  If we are connected but idle, do not send the ERROR
	    # statement because the CL will not read it until it executes the
	    # next task (which it will then mistakenly think has aborted).

	    if (!(prtype == PR_CONNECTED && state == IDLE))
		call xer_send_error_statement_to_cl (status)

	    # Inform error handling code that error restart has completed,
	    # or next error call will result in a panic shutdown.

	    call erract (OK)
	    call xer_reset ()
	    status = OK
	}

	# During process startup and shutdown the parent is not listening to
	# us, hence we dump STDOUT and STDERR into the null file.  If this is
	# not done and we write to CLOUT, deadlock may occur.  During startup
	# we also call the ONENTRY procedure.  This is a no-op for connected
	# and host subprocesses unless a special procedure is linked by the
	# user (for detached processes the standard ONENTRY procedure opens
	# the bkgfile as CLIN).  The return value of ONENTRY determines whether
	# the interpreter loop is entered.  Note that ONENTRY permits complete
	# bypass of the standard interpreter loop by an application (e.g. the
	# IRAF CL).

	if (state == STARTUP) {
	    # Redirect stderr and stdout to the null file.
	    if (prtype == PR_CONNECTED) {
		call fredir (STDOUT, nullfile, WRITE_ONLY, TEXT_FILE)
		call fredir (STDERR, nullfile, WRITE_ONLY, TEXT_FILE)
	    }

	    # Call the custom or default ONENTRY procedure.
	    if (onentry (prtype, bkgfname, a_cmd) == PR_EXIT) {
		interpret = NO
		goto shutdown_
	    } else
		interpret = YES

	    # Open the command input stream.  If a command string was given on
	    # the command line then we read commands from that, otherwise we
	    # take commands from CLIN.

	    for (i=1;  IS_WHITE(a_cmd[i]) || a_cmd[i] == '\n';  i=i+1)
		;
	    if (a_cmd[i] != EOS) {
		cmdin = stropen (a_cmd, ARB, READ_ONLY)
		call fseti (cmdin, F_KEEP, YES)
		interpret = NO
		interactive = NO
	    } else
		cmdin = CLIN
	}

#	The following modification assumes that this task is being 
#	run as a stand-alone task

	builtin_task = NO
	state = EXECUTING

	timeit = NO
	if (timeit == YES)
	    call sys_mtime (save_time)

	# Clear the parameter cache.
	call clc_init()

	# Set the name of the root pset.
	call clc_newtask (taskname)

	# Process the argument list, consisting of any mixture of
	# parameter=value directives and i/o redirection directives.

	arglist_offset = 1
	call sys_scanarglist (cmdin, cmd[arglist_offset])

shutdown_
	call flush(CLOUT)
	
end


# SYS_GETCOMMAND -- Get the next command from the input file.  Ignore blank
# lines and comment lines.  Parse the command and return the components as
# output arguments.  EOF is returned as the function value when eof file is
# reached on the input file.

int procedure sys_getcommand (fd, cmd, taskname, arglist_offset, timeit, prtype)

int	fd			#I command input file
char	cmd[SZ_CMDBUF]		#O command line
char	taskname[SZ_TASKNAME]	#O extracted taskname, lower case
int	arglist_offset		#O offset into CMD of first argument
int	timeit			#O if YES, time the command
int	prtype			#I process type code

int	ip, op
int	getlline(), stridx()

begin
	repeat {
	    # Get command line.  Issue prompt first if process is being run
	    # interactively.

	    if (prtype == PR_HOST && fd == CLIN) {
		call putline (CLOUT, "> ")
		call flush (CLOUT)
	    }					
	    if (getlline (fd, cmd, SZ_CMDBUF) == EOF)
		return (EOF)

	    # Check for timeit character and advance to first character of
	    # the task name.

	    timeit = NO
	    for (ip=1;  cmd[ip] != EOS;  ip=ip+1) {
		if (cmd[ip] == TIMEIT_CHAR && timeit == NO)
		    timeit = YES
		else if (!IS_WHITE (cmd[ip]))
		    break
	    }

	    # Skip blank lines and comment lines.
	    switch (cmd[ip]) {
	    case '#', '\n', EOS:
		next
	    case '?', '!':
		taskname[1] = cmd[ip]
		taskname[2] = EOS
		arglist_offset = ip + 1
		return (OK)
	    }

	    # Extract task name.
	    op = 1
	    while (IS_ALNUM (cmd[ip]) || stridx (cmd[ip], "_.$") > 0) {
		taskname[op] = cmd[ip]
		ip = ip + 1
		op = min (SZ_TASKNAME + 1, op + 1)
	    }
	    taskname[op] = EOS

	    # Determine index of argument list.
	    while (IS_WHITE (cmd[ip]))
		ip = ip + 1
	    arglist_offset = ip

	    # Get rid of the newline.
	    for (;  cmd[ip] != EOS;  ip=ip+1)
		if (cmd[ip] == '\n') {
		    cmd[ip] = EOS
		    break
		}

	    return (OK)
	}
end


# SYS_SCANARGLIST -- Parse the argument list of a task.  At the level of the
# iraf main the command syntax is very simple.  There are two types of
# arguments, parameter assignments (including switches) and i/o redirection
# directives.  All param assignments are of the form  "param=value",  where
# PARAM must start with a lower case alpha and where VALUE is either quoted or
# is delimited by one of the metacharacters [ \t\n<>\\].  A redirection argument
# is anything which is not a parameter set argument, i.e., any argument which
# does not start with a lower case alpha.

procedure sys_scanarglist (cmdin, i_args)

int	cmdin			# command input stream
char	i_args[ARB]		# (first part of) argument list

int	fd
char	ch
pointer	sp, fname, args, ip, op
int	getlline()

begin
	call smark (sp)
	call salloc (args, SZ_CMDBUF, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	call strcpy (i_args, Memc[args], SZ_CMDBUF)

	# Inform FIO that all standard i/o streams are unredirected (overridden
	# below if redirected by an argument).

	for (fd=1;  fd < FIRST_FD;  fd=fd+1)
	    call fseti (fd, F_REDIR, NO)

	# Process each argument in the argument list.  If the command line ends
	# with an escaped newline then continuation is assumed.  Arguments are
	# delimited by whitespace.

	for (ip=args;  Memc[ip] != '\n' && Memc[ip] != EOS;  ) {
	    # Advance to the next argument.
	    while (IS_WHITE (Memc[ip]))
		ip = ip + 1

	    # Check for continuation.
	    ch = Memc[ip]
	    if (ch == '\\' && (Memc[ip+1] == '\n' || Memc[ip+1] == EOS)) {
		if (getlline (cmdin, Memc[args], SZ_CMDBUF) == EOF)
		    return
		ip = args
		next
	    } else if (ch == '\n' || ch == EOS)
		break

	    # If the argument begins with an alpha, _, or $ (e.g., $nargs)
	    # then it is a param=value argument, otherwise it must be a redir.
	    # The form @filename causes param=value pairs to be read from
	    # the named file.

	    if (ch == '@') {
		op = fname
		for (ip=ip+1;  Memc[ip] != EOS;  ip=ip+1)
		    if (IS_WHITE (Memc[ip]) || Memc[ip] == '\n')
			break
		    else if (Memc[ip] == '\\' && Memc[ip+1] == '\n')
			break
		    else {
			Memc[op] = Memc[ip]
			op = op + 1
		    }
		Memc[op] = EOS
		call sys_getpars (Memc[fname])

	    } else if (IS_ALPHA(ch) || ch == '_' || ch == '$') {
		call sys_paramset (Memc, ip)
	    } else
		call sys_redirect (Memc, ip)
	}

	call sfree (sp)
end


# SYS_GETPARS -- Read a sequence of param=value parameter assignments from
# the named file and enter them into the CLIO cache for the task.

procedure sys_getpars (fname)

char	fname			# pset file

int	lineno, fd
pointer	sp, lbuf, ip
int	open(), getlline()
errchk	open, getlline

begin
	call smark (sp)
	call salloc (lbuf, SZ_CMDBUF, TY_CHAR)

	fd = open (fname, READ_ONLY, TEXT_FILE)

	lineno = 0
	while (getlline (fd, Memc[lbuf], SZ_CMDBUF) != EOF) {
	    lineno = lineno + 1
	    for (ip=lbuf;  IS_WHITE (Memc[ip]);  ip=ip+1)
		;
	    if (Memc[ip] == '#' || Memc[ip] == '\n')
		next
	    iferr (call sys_paramset (Memc, ip)) {
		for (;  Memc[ip] != EOS && Memc[ip] != '\n';  ip=ip+1)
		    ;
		Memc[ip] = EOS
		call eprintf ("Bad param assignment, line %d: `%s'\n")
		    call pargi (lineno)
		    call pargstr (Memc[lbuf])
	    }
	}

	call close (fd)
	call sfree (sp)
end


# SYS_PARAMSET -- Extract the param and value substrings from a param=value
# or switch argument and enter them into the CL parameter cache.  (see also
# clio.clcache).

procedure sys_paramset (args, ip)

char	args[ARB]		# argument list
int	ip			# pointer to first char of argument

pointer	sp, param, value, op
int	stridx()

begin
	call smark (sp)
	call salloc (param, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_VALSTR, TY_CHAR)

	# Extract the param field.
	op = param
	while (IS_ALNUM (args[ip]) || stridx (args[ip], "_.$") > 0) {
	    Memc[op] = args[ip]
	    op = op + 1
	    ip = ip + 1
	}
	Memc[op] = EOS

	# Advance to the switch character or assignment operator.
	while (IS_WHITE (args[ip]))
	    ip = ip + 1

	switch (args[ip]) {
	case '+':
	    # Boolean switch "yes".
	    ip = ip + 1
	    call strcpy ("yes", Memc[value], SZ_VALSTR)
	
	case '-':
	    # Boolean switch "no".
	    ip = ip + 1
	    call strcpy ("no", Memc[value], SZ_VALSTR)

	case '=':
	    # Extract the value field.  This is either a quoted string or a
	    # string delimited by any of the metacharacters listed below.

	    ip = ip + 1
	    call sys_gstrarg (args, ip, Memc[value], SZ_VALSTR)

	default:
	    call error (1, "IRAF Main: command syntax error")
	}

	# Enter the param=value pair into the CL parameter cache.
	call clc_enter (Memc[param], Memc[value])

	call sfree (sp)
end


# SYS_REDIRECT -- Process a single redirection argument.  The syntax of an
# argument to redirect the standard input is
#
#	< [fname]
#
# If the filename is omitted it is understood that STDIN has been redirected
# in the CL.  The syntax to redirect a standard output stream is
#
#	[45678][TB](>|>>)[fname]
#
# where [4567] is the FD number of a standard output stream (STDOUT, STDERR,
# STDGRAPH, STDIMAGE, or STDPLOT), and [TB] indicates if the file is text or
# binary.  If the stream is redirected at the CL level the output filename
# will be given as `$', serving only to indicate that the stream is redirected.

procedure sys_redirect (args, ip)

char	args[ARB]		# argument list
int	ip			# pointer to first char of redir arg

pointer	sp, fname
int	fd, mode, type
int	ctoi()
define	badredir_ 91
errchk	fredir, fseti

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Get number of stream (0 if not given).
	if (ctoi (args, ip, fd) <= 0)
	    fd = 0

	# Get file type (optional).
	while (IS_WHITE (args[ip]))
	    ip = ip + 1

	switch (args[ip]) {
	case 'T', 't':
	    type = TEXT_FILE
	    ip = ip + 1
	case 'B', 'b':
	    type = BINARY_FILE
	    ip = ip + 1
	default:
	    type = 0
	}

	# Check for "<", ">", or ">>".
	while (IS_WHITE (args[ip]))
	    ip = ip + 1

	switch (args[ip]) {
	case '<':
	    ip = ip + 1
	    mode = READ_ONLY
	    if (fd == 0)
		fd = STDIN
	    else if (fd != STDIN || fd != CLIN)
		goto badredir_

	case '>':
	    ip = ip + 1
	    if (args[ip] == '>') {
		ip = ip + 1
		mode = APPEND
	    } else
		mode = NEW_FILE

	    if (fd == 0)
		fd = STDOUT
	    else {
		switch (fd) {
		case CLOUT, STDOUT, STDERR, STDGRAPH, STDIMAGE, STDPLOT:
		    ;
		default:
		    goto badredir_
		}
	    }

	default:
	    # Not a redirection argument.
	    call error (1, "IRAF Main: command syntax error")
	}

	# Set default file type for given stream if no type specified.
	if (type == 0)
	    switch (fd) {
	    case CLIN, CLOUT, STDIN, STDOUT, STDERR:
		type = TEXT_FILE
	    default:
		type = BINARY_FILE
	    }

	# Extract the filename, if any.  If the CL has redirected the output
	# and is merely using the redirection syntax to inform us of this,
	# the metafilename "$" is given.

	while (IS_WHITE (args[ip]))
	    ip = ip + 1
	
	if (args[ip] == '$') {
	    Memc[fname] = EOS
	    ip = ip + 1
	} else
	    call sys_gstrarg (args, ip, Memc[fname], SZ_FNAME)

	# At this point we have FD, FNAME, MODE and TYPE.  If no file is
	# named the stream has already been redirected by the parent and
	# all we need to is inform FIO that the stream has been redirected.
	# Otherwise we redirect the stream in the local process.  A locally
	# redirected stream will be closed and the normal direction restored
	# during FIO cleanup, at program termination or during error
	# recovery.

	if (Memc[fname] != EOS)
	    call fredir (fd, Memc[fname], mode, type)
	else
	    call fseti (fd, F_REDIR, YES)

	call sfree (sp)
	return

badredir_
	call error (2, "IRAF Main: illegal redirection")
end


# SYS_GSTRARG -- Extract a string field.  This is either a quoted string or a
# string delimited by any of the metacharacters " \t\n<>\\".

procedure sys_gstrarg (args, ip, outstr, maxch)

char	args[ARB]		# input string
int	ip			# pointer into input string
char	outstr[maxch]		# receives string field
int	maxch

char	delim, ch
int	op
int	stridx()

begin
	op = 1
	while (IS_WHITE (args[ip]))
	    ip = ip + 1

	if (args[ip] == '"' || args[ip] == '\'') {
	    # Quoted value string.

	    delim = args[ip]
	    for (ip=ip+1;  args[ip] != delim && args[ip] != EOS;  ip=ip+1) {
		if (args[ip] == '\n') {
		    break
		} else if (args[ip] == '\\' && args[ip+1] == delim) {
		    outstr[op] = delim
		    op = op + 1
		    ip = ip + 1
		} else {
		    outstr[op] = args[ip]
		    op = op + 1
		}
	    }

	} else {
	    # Nonquoted value string.

	    for (delim=-1;  args[ip] != EOS;  ip=ip+1) {
		ch = args[ip]
		if (ch == '\\' && (args[ip+1] == '\n' || args[ip+1] == EOS))
		    break
		else if (stridx (ch, " \t\n<>\\") > 0)
		    break
		else {
		    outstr[op] = ch
		    op = op + 1
		}
	    }
	}

	outstr[op] = EOS
	if (args[ip] == delim)
	    ip = ip + 1
end

 
include	<config.h>
include	<error.h>
include	<syserr.h>
include	<clset.h>
include	<fset.h>
include	<ctype.h>
include	<printf.h>
include	<xwhen.h>
include	<knet.h>


define	SZ_VALSTR		512
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

procedure iraf_main (a_inchan, a_outchan, a_errchan, a_driver, a_devtype,
	prtype, bkgfile, jobcode)

int	a_inchan		# process standard input
int	a_outchan		# process standard output
int	a_errchan		# process standard error output
int	a_driver		# ZLOCPR address of device driver
int	a_devtype		# device type (text or binary)
int	prtype			# process type (connected, detached, host)
char	bkgfile[ARB]		# packed filename of bkg file if detached
int	jobcode			# jobcode if detached process

bool	networking
int	inchan, outchan, errchan, driver, devtype
char	cmd[SZ_LINE], taskname[SZ_TASKNAME], bkgfname[SZ_FNAME]
int	arglist_offset, timeit, junk, interactive, builtin_task
int	jumpbuf[LEN_JUMPBUF], status, state, interpret, i
long	save_time[2]
pointer	sp, clc_marker

bool	streq()
extern	DUMMY()
int	sys_getcommand(), sys_runtask(), oscmd()
int	access(), envscan(), onentry()
errchk	xonerror, fio_cleanup
common	/JUMPCOM/ jumpbuf
string	nullfile "dev$null"
data	networking /KNET/
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

	call ma_ideh()

	inchan  = a_inchan
	outchan = a_outchan
	errchan = a_errchan
	driver  = a_driver
	devtype = a_devtype


	# Other initializations.
	call env_init()
	call fmt_init (FMT_INITIALIZE)			# init printf
	call xer_reset()				# init error checking
	call erract (OK)				# init error handling
	call onerror (DUMMY)				# init onerror
	call onexit  (DUMMY)				# init onexit 
	call finit()					# initialize FIO
#	call clopen (inchan, outchan, errchan, driver, devtype)
	call clseti (CL_PRTYPE, prtype)
	call clc_init()					# init param cache
	call clc_mark (clc_marker)
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
		call ma_ideh()
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
	    if (onentry (prtype, bkgfname) == PR_EXIT) {
		interpret = NO
		goto shutdown_
	    } else
		interpret = YES
	}

#	status = sys_getcommand (CLIN, cmd, taskname, arglist_offset,
#	    timeit, prtype)
#
#	    builtin_task = NO
#
#		# Clear the parameter cache.
#		call clc_free (clc_marker)
#		call clc_mark (clc_marker)
#
#		# Set the name of the root pset.
#		call clc_newtask (taskname)
#
	    # Call sys_runtask (the code for which was generated automatically
	    # by the preprocessor in place of the TASK statement) to search
	    # the dictionary and run the named task.

#	    call strlwr (taskname)
#	    if (sys_runtask (taskname,cmd,arglist_offset,interactive) == ERR) {
#		call flush (STDOUT)
#		call putline (CLOUT,
#		    "ERROR (0, \"Iraf Main: Unknown task name\")\n")
#		call flush (CLOUT)
#		state = IDLE
#	    }


#		call xer_verify()
#		call xonerror (OK)
##		call fio_cleanup (OK)

	
	# The interpreter has exited after receipt of "bye" or EOF.  Redirect
	# stdout and stderr to the null file (since the parent is no longer
	# listening to us), call the user exit procedures if any, and exit.
shutdown_
	state = SHUTDOWN
#	if (prtype == PR_HOST && cmd[1] == EOS && interpret == YES) {
#	    call putci (CLOUT, '\n')
#	    call flush (CLOUT)
#	}

#	call xonexit (OK)
#	call fio_cleanup (OK)

end

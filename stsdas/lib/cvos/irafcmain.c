/*
** Reference: IRAF program $iraf/unix/os/zmain.c
**
** This program is a modification of the IRAF program zmain.c, which
** is the C main program for all SPP programs.  The "main" procedure
** name has been changed to "irafcmain".  It has been altered to compile
** under both ANSI C and C++.  The preprocessor variable CPLUSPLUS must
** be defined in order to compile under C++.
**
** MDD - 09 February 1998: modified (as suggested by Mike F.) the call to
** setpgrp.  Original version had no parameters; new version depends upon 
** SYSV being defined.
**
** M.D. De La Pena - 09 March 1998: use NO_UNDERSCORE to set cirafm properly.
**
** M.D. De La Pena - 19 June 1998: Mod to ifdef statement for setpgrp function.
** 
** M.D. De La Pena - 29 July 1999: Cast to void function return values being 
** ignored. Ipc variables declared static *int*.  Did not modify ZZ routines.
*/
# include <stdlib.h>
# include <unistd.h>
# include <fcntl.h>
# include <string.h>
# include <c_iraf.h>

# include <stdio.h>
# include <signal.h>
# include <sys/time.h>
# include <sys/resource.h>

# define import_spp
# define import_kernel
# define import_prtype
# define import_knames
# define import_xnames

# include <iraf.h>

# if defined (NO_UNDERSCORE)
# define cirafm_ cirafm
# endif

typedef int VFuncRI();
# ifdef CPLUSPLUS
extern "C" {
# endif
	void ZZSTRT();
	void ZLOCPR(VFuncRI, XINT *);
	void ZZSETK(char *, char *, int, int, int, int);
	void cirafm_ (XCHAR *, XINT *, XINT *, XINT *, XINT *, XINT *, 
		int *, char *, XINT *, VFuncRI, VFuncRI);
	int ZGETTX();
	int ZGETTY();
	int ZARDPR();
	int SYSRUK();
	int ONENTRY();
# ifdef CPLUSPLUS
};
# endif

/*
 * ZMAIN.C -- C main for IRAF processes.
 */

extern	unsigned USHLIB[];
extern	int sh_debug;

#define	LOGIPC	"LOGIPC"		/* define to enable IPC logging. */

static	char os_process_name[SZ_FNAME];
static	char osfn_bkgfile[SZ_PATHNAME];
static	int ipc_in = 0, ipc_out = 0;
static	int ipc_isatty = NO;
static	int prtype;


/* MAIN -- UNIX Main routine for IRAF processes.  The process is a C process
 * to UNIX, even though nearly all the code is Fortran.  The process main
 * determines whether the process is a connected subprocess, a detached
 * process, or a process spawned by the host system.  We must set up the
 * process standard i/o channels then call the IRAF Main to complete process
 * initialization.  Control returns when the IRAF Main shuts down the process
 * in response to a BYE request.  The only other way a process can exit is
 * if a panic abort occurs.
 *
 * The following switches are recognized:
 *	-C			debug -c (IPC) protocol from a terminal
 *	-c			connected subprocess
 *	-d bkgfile		detached subprocess
 *	-h			host process (default)
 *	-w			permit writing into shared image (debugging)
 */
void irafcmain (int argc, char **argv) {
	XINT	inchan=0, outchan=1;	/* process stdin, stdout	*/
	XINT	errchan=2;		/* process std error output	*/
	XINT	driver;			/* EPA i/o chan device driver	*/
	XINT	devtype;		/* device type (text or binary)	*/
	XINT	jobcode;		/* bkg jobcode, if detached pr	*/
	int	len_irafcmd, nchars;
	XCHAR	*irafcmd;
	char	*ip;

	int	arg = 1;

	/* The following flag must be set before calling ZZSTRT. */
	if (argc > 1 && strcmp (argv[arg], "-w") == 0) {
	    sh_debug++;
	    arg++;
	}

	ZZSTRT();

	(void) strcpy (os_process_name, argv[0]);
	(void) strcpy ((char *)osfn_bkgfile, "");

	/* Determine process type.  If we were spawned by the host the TTY
	 * driver is used regardless of whether the standard i/o device is
	 * a tty or a file.  Otherwise the IPC driver is used.  If we are a
	 * detached process the standard input is connected to /dev/null,
	 * which will cause the IPC driver to return EOF if a task tries to
	 * read from stdin.
	 */

        /* Default if no arguments (same as -h, or host process). */
	prtype = PR_HOST;
	ZLOCPR (ZGETTY, &driver);
	devtype = TEXT_FILE;

	if (arg < argc) {
	    if (strcmp (argv[arg], "-C") == 0) {
		ipc_isatty = 1;
		arg++;
		goto ipc_;

	    } else if (strcmp (argv[arg], "-c") == 0) {
		/* Disable SIGINT so that child process does not die when the
		 * parent process is interrupted.  Parent sends SIGTERM to
		 * interrupt a child process.
		 */
		(void) signal (SIGINT, SIG_IGN);
		arg++;

		/* Check if we want IPC debug logging. */
		if (getenv (LOGIPC)) {
		    char   fname[SZ_FNAME];

		    (void) sprintf (fname, "%d.in", getpid());
		    ipc_in = creat (fname, 0644);
		    (void) sprintf (fname, "%d.out", getpid());
		    ipc_out = creat (fname, 0644);
		}

ipc_:
		os_host_task = 0;
		prtype = PR_CONNECTED;
		ZLOCPR (ZARDPR, &driver);
		devtype = BINARY_FILE;

	    } else if (strcmp (argv[arg], "-d") == 0) {
		(void) signal (SIGINT,  SIG_IGN);
		(void) signal (SIGTSTP, SIG_IGN);
		arg++;

		/* Put this background process in its own process group,
		 * so that it will be unaffected by signals sent to the
		 * parent's process group, and to prevent the detached process
		 * from trying to read from the parent's terminal.
		 * [Sun/IRAF Note - this is necessary to prevent SunView from
		 * axeing bkg jobs when "Exit Suntools" is selected from the
		 * root menu].
		 */
		jobcode = getpid();

		/* 
		* this implements setpgrp(), but differently for different machines.
		* see setpgrp_hack.c 
		*/
		{
		extern void setpgrp_hack(int);
		setpgrp_hack(jobcode);
		}

		(void) freopen ("/dev/null", "r", stdin);
		prtype = PR_DETACHED;
		ZLOCPR (ZGETTX, &driver);
		devtype = TEXT_FILE;

		/* Copy the bkgfile to PKCHAR buffer to avoid the possibility
		 * that argv[2] is not PKCHAR aligned.
		 */
		(void) strcpy ((char *)osfn_bkgfile, argv[arg]);
		arg++;

	    } else if (strcmp (argv[arg], "-h") == 0) {
		/* Default case. */
		arg++;
	    }
	}

	len_irafcmd = SZ_LINE;
	irafcmd = (XCHAR *) malloc (len_irafcmd * sizeof(XCHAR));

	/* If there are any additional arguments on the command line pass
	 * these on to the IRAF main as the IRAF command to be executed.
	 */
	if (arg < argc) {
	    for (nchars=0;  arg < argc;  arg++) {
		while (nchars + strlen(argv[arg]) > len_irafcmd) {
		    len_irafcmd += 1024;
		    irafcmd = (XCHAR *) realloc ((char *)irafcmd,
			len_irafcmd * sizeof(XCHAR));
		}
		for (ip=argv[arg];  (irafcmd[nchars] = *ip++);  nchars++)
		    ;
		irafcmd[nchars++] = ' ';
	    }

	    irafcmd[nchars?nchars-1:0] = XEOS;
	} else
	    irafcmd[0] = XEOS;

	/* Pass some parameters into the kernel; avoid a global reference to
	 * the actual external parmeters (which may be in a shared library
	 * and hence inaccessible).
	 */
	ZZSETK (os_process_name, osfn_bkgfile, prtype,
	    ipc_isatty, ipc_in, ipc_out);

	/* Call the IRAF Main, which does all the real work.  Return status
	 * OK when the main returns.  The process can return an error status
	 * code only in the event of a panic.
	 */
	cirafm_ (irafcmd, &inchan, &outchan, &errchan,
	    &driver, &devtype, &prtype, osfn_bkgfile, &jobcode, 
	    SYSRUK,ONENTRY);

	/* Normal process shutdown.  Our only action is to delete the bkgfile
	 * if run as a detached process (see also zpanic).
	 */
	if (prtype == PR_DETACHED)
	    (void) unlink ((char *)osfn_bkgfile);

	exit (XOK);
}

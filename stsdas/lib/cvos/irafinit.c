/*
** Reference: IRAF program $iraf/unix/os/zmain.c
**
** This program is a modification of the IRAF program zmain.c, which
** is the C main program for all SPP programs.  The "main" procedure
** name has been changed to "c_irafcinit".  It has been altered to compile
** under both ANSI C and C++.  The preprocessor variable __cplusplus must
** be defined in order to compile under C++.
**
** A. Farris - original implementation.
** M.D. De La Pena - 09 March 1998: use NO_UNDERSCORE to set irinit properly.
** M.D. De La Pena - 07 January 1999: Added use of CVOS VERSION macro.  This
**  routine is called by all C tasks (native IRAF or host level).
** M.D. De La Pena - 27 May 1999: Added NOLIBC macro before the include of
** vms.h file in order to avoid conflicts with string functions defined in
** this header file.
** M.D. De La Pena - 16 July 1999: Modified NOLIBC macro to NOSTRDEFS on
** advice of D.Tody. The IRAF group will modify the vms.h file to correspond.
** Moved CVOS VERSION indicator to c_iraf_priv.c.  Cast to void function 
** return values being ignored. Ipc variables declared static *int*.  Did 
** not modify ZZ routines.
**
** This version incorporates both UNIX and VMS versions.
*/

# include <stdlib.h>
# include <string.h>
# include <c_iraf.h>

# if defined (NO_UNDERSCORE)
# define irinit_ irinit
# endif

# if !defined(__VMS)

/* This is the UNIX version. */
# include <unistd.h>
# include <fcntl.h>
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

typedef int VFuncRI();
# if defined (__cplusplus)
extern "C" {
# endif
	void ZZSTRT();
	void ZLOCPR(VFuncRI, XINT *);
	void ZZSETK(char *, char *, int, int, int, int);
	void irinit_(XCHAR *, XINT *, XINT *, XINT *, XINT *, XINT *, 
		int *, char *, XINT *, VFuncRI);
	int ZGETTX();
	int ZGETTY();
	int ZARDPR();
	int ONENTRY();
# if  defined (__cplusplus)
};
# endif

extern	unsigned USHLIB[];
extern	int sh_debug;

#define	LOGIPC	"LOGIPC"		/* define to enable IPC logging. */

static	char os_process_name[SZ_FNAME];
static	char osfn_bkgfile[SZ_PATHNAME];
static	int ipc_in = 0, ipc_out = 0;
static	int ipc_isatty = NO;
static	int prtype;

# else

/* This is the VMS version. */
# undef VMS
#include <descrip.h>
#include <jpidef.h>
#include <ssdef.h>

#define  import_finfo
#define  import_prtype
#define  import_xwhen
#define  import_xnames
#define  NOSTRDEFS
#include <vms.h>

#ifdef __ALPHA
#pragma extern_model common_block noshr
#endif
int	vms_batch = 0;			/* exported */

static	int argc;			/* C like argc, argv (see _get_args) */
static	char **argv;
static	int file_mode = NEW_FILE;	/* for I/O redirection */
static	XCHAR irafcmd[SZ_COMMAND];	/* initial command for iraf main */

typedef int VFuncRI();
# if defined (__cplusplus)
extern "C" {
# endif
	int printf(const char *, ...);
	int _tranlog(char *, char *);
	int ZFINFO(char *, struct _finfo *, XINT *);
	int ZOPNTY(char *, int *, XINT *);
	int ZOPNTX(char *, int *, XINT *);
	int _exit_init(XINT, XINT, XINT, char *);
	int ZPANIC(XINT *, char *);
	void irinit(XCHAR *, XINT *, XINT *, XINT *, XINT *, XINT *, 
		int *, char *, XINT *, VFuncRI);
	int ZGETTX();
	int ZGETTY();
	int ZARDPR();
	int ONENTRY();
# if defined (__cplusplus)
};
# endif

# endif

void c_irafinit (int argc, char **argv) {
# if !defined(__VMS)
	XINT	inchan=0, outchan=1;	/* process stdin, stdout	*/
	XINT	errchan=2;		/* process std error output	*/
	XINT	driver;			/* EPA i/o chan device driver	*/
	XINT	devtype;		/* device type (text or binary)	*/
	XINT	jobcode;		/* bkg jobcode, if detached pr	*/
	int	len_irafcmd, nchars;
	XCHAR	*irafcmd;
	char	*ip;
# else
	struct  _finfo  finfo_struct;
	int	interactive, status;
	XINT	prtype, jobcode, driver, devtype, inchan, outchan, errchan;
	char  	*p, inbuf[SZ_PATHNAME], outbuf[SZ_PATHNAME], *bkgfile = "";
	char    *sys_input  = "SYS$INPUT";
	char	*sys_output = "SYS$OUTPUT";
	char	*sys_error  = "SYS$ERROR";

# endif
	extern 	int cvos_irafinit;

	cvos_irafinit = 1; /* let the world know irafinit has been called */

	if (!iraf_native_task && !os_host_task) {
		(void) printf("Program must be compiled either as IRAF native task\
 or a host operating system task.\n");
		exit(-1);
	}
	if (!iraf_native_task) {
# if !defined(__VMS)
		ZZSTRT();

		/*strcpy (os_process_name, argv[0]);*/
		(void) strcpy (os_process_name, " ");
		(void) strcpy ((char *)osfn_bkgfile, "");


		prtype = PR_HOST;
		ZLOCPR (ZGETTY, &driver);
		devtype = TEXT_FILE;

		len_irafcmd = SZ_LINE;
		irafcmd = (XCHAR *) malloc (len_irafcmd * sizeof(XCHAR));
		irafcmd[0] = XEOS;

		ZZSETK (os_process_name, osfn_bkgfile, prtype,
		    ipc_isatty, ipc_in, ipc_out);

		irinit_ (irafcmd, &inchan, &outchan, &errchan,
		    &driver, &devtype, &prtype, osfn_bkgfile, &jobcode, 
		    ONENTRY);
# else
	prtype = PR_HOST;

	/* Translate input and output logicals:
	 *  
	 *		SYS$INPUT	    	SYS$OUTPUT 
	 *     
	 *  Host:	terminal	    	terminal  (interactive)
	 *	    	file	    	    	file	  (I/O to files)
	 *
	 *  Connected:  input shared memory section	output s.m. section
	 *
 	 *  Detached:   NLA0:	    	    	parent's terminal   (subprocess)
	 *              diskname		diskname	    (VMS batch)
	 */
	_tranlog (sys_input,  inbuf);
	_tranlog (sys_output, outbuf);

	ZFINFO (inbuf, &finfo_struct, &status);
	if (finfo_struct.fi_type == FI_SPECIAL)
                interactive = 1;
	else 	    		    /* I/O redirected to files */
	    	interactive = 0;  

	if (interactive) {
	    	ZOPNTY (inbuf, &READ_ONLY, &inchan);
	    	ZOPNTY (outbuf, &APPEND, &outchan);
	    	errchan = outchan;
	    	driver = (XINT) ZGETTY;
	} else {
	    	ZOPNTX (inbuf, &READ_ONLY, &inchan);
	    	ZOPNTX (outbuf, &file_mode, &outchan);
	    	errchan = outchan;
	    	driver = (XINT) ZGETTX;
	}
	devtype = TEXT_FILE;


	/* Set up an exit handler to take care of some loose ends (and for
	 * ZXWHEN).  Also check the validity of the I/O channels.  Then, 
	 * finally, call the IRAF_MAIN to get things going...
	 */
	_exit_init (prtype, inchan, outchan, bkgfile);

	if (inchan == XERR || outchan == XERR ||
            (prtype != PR_CONNECTED && errchan == XERR)) 
	    ZPANIC (&(SS$_IVCHAN), "Error setting up I/O channels.");


	irinit (irafcmd, &inchan, &outchan, &errchan,
	    &driver, &devtype, &prtype, bkgfile, &jobcode, ONENTRY);
# endif
	}
}

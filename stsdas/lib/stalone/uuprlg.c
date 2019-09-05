#include <descrip.h>
#include <jpidef.h>
#include <ssdef.h>

#define  import_finfo
#define  import_prtype
#define  import_xwhen
#include "pvms.h"


static	int  file_mode = NEW_FILE;	/* for I/O redirection */
	int  vms_batch = 0;


/* ZMAIN -- The IRAF/VMS process main routine.
 * This uuprlg version of zmain was created by Nelson Zarate.
 * Phil Hodge included the section for batch mode on 9 Apr 1991.
 */
uuprlg()
{
	XINT	prtype, jobcode, driver, devtype, inchan, outchan, errchan;
	int	status, fake_cl_mode = 0;
	int	ZARDPR(), ZGETTY(), ZGETTX();
	char  	*p, inbuf[SZ_PATHNAME], outbuf[SZ_PATHNAME], *bkgfile = "", 
	        *sys_input  = "SYS$INPUT",  
	    	*sys_output = "SYS$OUTPUT",
	    	*sys_error  = "SYS$ERROR";

	
	prtype = _getprtype();		/* may also change vms_batch */
	
	_tranlog (sys_input,  inbuf);
	_tranlog (sys_output, outbuf);

	/*  Set up I/O channels and get appropriate driver routine. 
	 */

	if (prtype == PR_HOST) {
	    ZOPNTY (inbuf, &READ_ONLY, &inchan);
	    ZOPNTY (outbuf, &APPEND, &outchan);
	    errchan = outchan;
	    driver = (XINT) ZGETTY;
	} else if (prtype == PR_DETACHED)  {
	    /* STDIN is set to the null device for detached processes.  When
	     * using parallel subprocesses, STDOUT and STDERR go to the parent's
	     * terminal.  When a VMS batch job, it'll be the batch log file
	     * (set up in ZOPDPR).
	     *   note:  assumed to be vms_batch
	     */
	    ZOPNTX ("NLA0:", &READ_ONLY, &inchan);
	    ZOPNTX ("SYS$OUTPUT:", &READ_WRITE, &outchan); 	/* need ':'!! */
	    errchan = outchan;
	    driver = (XINT) ZGETTX;
	}
	devtype = TEXT_FILE;

	prtype = PR_HOST;		/* Now reset prtype. */

	/* Set up an exit handler to take care of some loose ends (and for
	 * ZXWHEN).  Also check the validity of the I/O channels.  Then, 
	 * finally, call the IRAF_MAIN to get things going...
	 */
	_exit_init (prtype, inchan, outchan, bkgfile);

	if (inchan == XERR || outchan == XERR ||
            (prtype != PR_CONNECTED && errchan == XERR)) 
	    ZPANIC (&(SS$_IVCHAN), "Error setting up I/O channels.");

	IRAF_MAIN (&inchan, &outchan, &errchan, &driver, &devtype, &prtype,
                           bkgfile, &jobcode);

        return (SS$_NORMAL);
}


#define PCB$M_BATCH 16384	/* From SYS$LIBRARY:LIB.MLB */

/* _GETPRTYPE -- Find out the process name and determine the process type.
 * 
 *	Copied from _get_prtype(); determine whether it's a batch job or not.
 */
static
_getprtype()
{
	int	status;

	_getjpi (0, 4, JPI$_STS, &status, 0);
	if (status & PCB$M_BATCH) {
	    vms_batch = 1;
	    return (PR_DETACHED);
	}

	return (PR_HOST);
}

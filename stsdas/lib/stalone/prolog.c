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
 */
prolog()
{
	XINT	prtype, jobcode, driver, devtype, inchan, outchan, errchan;
	int	interactive, status, fake_cl_mode = 0;
	int	ZARDPR(), ZGETTY(), ZGETTX();
	struct  _finfo  finfo_struct;
	char  	*p, inbuf[SZ_PATHNAME], outbuf[SZ_PATHNAME], *bkgfile = "", 
	        *sys_input  = "SYS$INPUT",  
	    	*sys_output = "SYS$OUTPUT",
	    	*sys_error  = "SYS$ERROR";

	
	prtype = PR_HOST;
	
	_tranlog (sys_input,  inbuf);
	_tranlog (sys_output, outbuf);

	    ZFINFO (inbuf, &finfo_struct, &status);
	    if (finfo_struct.fi_type == FI_SPECIAL)
                interactive = 1;
	    else 	    		    /* I/O redirected to files */
	    	interactive = 0;  

	/*  Set up I/O channels and get appropriate driver routine. 
	 */

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


	IRAF_MAIN (&inchan, &outchan, &errchan, &driver, &devtype, &prtype,
                           bkgfile, &jobcode);

        return (SS$_NORMAL);
}

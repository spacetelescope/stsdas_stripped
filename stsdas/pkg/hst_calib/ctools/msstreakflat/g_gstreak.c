# include <stdio.h>
# include "estreak.h"


/*  G_GSTREAK  -  Compute flatfield image based on Earth streaked images.
 *
 *  This is the actual main routine, it is called from another module
 *  just to avoid exit() calls.
 *
 *
 *
 *                                                         Author: I. Busko
 *
 *  Revision history:
 *  ----------------
 *  03 May 96  -  Version 0 (support NICMOS only) is mostly a C translation
 *                of original SPP WFPC-only task written by J.C.Hsu, however
 *                with hooks for other instruments.
 *  08 May 96  -  IRAF native task - released (IB).
 *  23 May 96  -  WFPC support - version 1.0 (IB).
 *  31 May 96  -  Virtual files - version 1.1 (IB).
 *  07 Oct 96  -  Revised after code review - version 1.2 (IB).
 *  16 Oct 96  -  Added protection against immap failure (IB)
 *  28 Oct 96  -  Fixed g_select call (IB)
 *  29 Oct 96  -  Window size matches SPP version (IB)
 *  30 May 97  -  Task renamed "msstreakflat" (IB)
 *  03 Jun 97  -  Added new <stdio.h> (IB)
 *
 */

int g_gstreak () {

	char                 output[SZ_NAME];
	int                     niter, ngood;
	int                  width[MAX_ITER];
	Bool                         verbose;
	IRAFPointer                      fin;
	IOControl                        ioc;
	short                        badmask;
	char        time_stamp[SZ_TIMESTAMP];

	int g_inputPar (IOControl *, IRAFPointer *, char *, 
                        int *, int [], int *, short *, Bool *);
	int g_doIt (IOControl *, IRAFPointer *, char *, int,  int [], 
                    int, short, char *, Bool);
	void g_timeStamp (char *);

	/* Input parameters and initialize. */
	if (g_inputPar (&ioc, &fin, output, &niter, width, &ngood, &badmask,
                        &verbose))
	    return (1);

	g_timeStamp (time_stamp);
	if (verbose) {
	    sprintf (ErrText,
             "***  MSSTREAKFLAT version %s starting at %s\n", VERSION, 
             time_stamp);
	    g_message (ErrText);
	}

	/* Do it ! */
	if (g_doIt (&ioc, &fin, output, niter, width, ngood, badmask, 
                    time_stamp, verbose))
	    return (1);

	if (verbose) {
	    g_timeStamp (time_stamp);
	    sprintf (ErrText,
             "***  MSSTREAKFLAT finished successfully at %s\n", time_stamp);
	    g_message (ErrText);
	}
	return (0);
}

# include <stdio.h>
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "estreak.h"

/*  G_GETPIXELMASK  --   Reads pset with bit selections and builds mask.
 *
 *  As by-product, read also the DQF extension for WFPC.
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   08 May 96  -  Implementation (IB)
 *   22 May 98  -  Add zero-read DQ bit (IB)
 *   10 Sep 99  -  Changed bad background DQ bit to grot (H.Bushouse)
 *
 */


void g_getPixelMask (IOControl *ioc) {

	IRAFPointer ps;  /* pset pointer */

	/* Flags common to WFPC and NICMOS */
	Bool rs, sat, cray;

	/* WFPC-only flags */
	Bool generic, calib, camdef, hot, fixhot, mis, trap, overlap;

	/* NICMOS-only flags */
	Bool lin, dark, flat, grot, defect, telem, badpix, source, zread;

# if defined(NATIVE_IRAF)

	ioc->mask = 0x0;
	strcpy (ioc->dqfExten, "");

	switch (ioc->instrument) {

	case NICMOS:
	    ps = c_clopset ("nicdqpar");
	    rs     = c_clgpsetb (ps, "rs");
	    sat    = c_clgpsetb (ps, "sat");
	    cray   = c_clgpsetb (ps, "cray");
	    lin    = c_clgpsetb (ps, "lin");
	    dark   = c_clgpsetb (ps, "dark");
	    flat   = c_clgpsetb (ps, "flat");
	    grot   = c_clgpsetb (ps, "grot");
	    defect = c_clgpsetb (ps, "defect");
	    telem  = c_clgpsetb (ps, "telemetry");
	    badpix = c_clgpsetb (ps, "badpix");
	    source = c_clgpsetb (ps, "source");
	    zread  = c_clgpsetb (ps, "zread");
	    c_clcpset (ps);

	    if (rs)     ioc->mask |= N_RS; 
	    if (sat)    ioc->mask |= N_SATUR; 
	    if (cray)   ioc->mask |= N_CRAY; 
	    if (lin)    ioc->mask |= N_PLIN; 
	    if (dark)   ioc->mask |= N_PDARK; 
	    if (flat)   ioc->mask |= N_PFF; 
	    if (grot)   ioc->mask |= N_GROT; 
	    if (defect) ioc->mask |= N_DEFECT; 
	    if (telem)  ioc->mask |= N_TELEM; 
	    if (badpix) ioc->mask |= N_BADPIX; 
	    if (source) ioc->mask |= N_SOURCE; 
	    if (zread)  ioc->mask |= N_ZREAD; 

	    break;

	case WFPC:
	case WFPC2:
	    ps = c_clopset ("wfdqpar");
	    rs      = c_clgpsetb (ps, "rs");
	    sat     = c_clgpsetb (ps, "sat");
	    cray    = c_clgpsetb (ps, "cray");
	    generic = c_clgpsetb (ps, "generic");
	    calib   = c_clgpsetb (ps, "calib");
	    camdef  = c_clgpsetb (ps, "camdef");
	    hot     = c_clgpsetb (ps, "hot");
	    fixhot  = c_clgpsetb (ps, "fixhot");
	    mis     = c_clgpsetb (ps, "mis");
	    c_clgpseta (ps, "dqfextn", ioc->dqfExten, 6);
	    c_clcpset (ps);

	    if (rs)      ioc->mask |= W_RS; 
	    if (sat)     ioc->mask |= W_SATUR; 
	    if (cray)    ioc->mask |= W_CRAY; 
	    if (generic) ioc->mask |= W_BADPIX; 
	    if (calib)   ioc->mask |= W_CAL; 
	    if (camdef)  ioc->mask |= W_CAMERA; 
	    if (hot)     ioc->mask |= W_UHOT; 
	    if (fixhot)  ioc->mask |= W_FHOT; 
	    if (mis)     ioc->mask |= W_MISSING; 

	    break;

	}
# else
	ioc->mask = 0xFFFF;
	strcpy (ioc->dqfExten, "");
# endif
}

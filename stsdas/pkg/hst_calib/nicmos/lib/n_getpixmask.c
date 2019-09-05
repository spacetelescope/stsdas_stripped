# include <stdio.h>
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "nicmos.h"

/*  N_GETPIXELMASK  --   Reads pset with bit selections and builds mask.
**
**
**   Revision history:
**   ---------------
**   H. Bushouse	26-Mar-1999	Implementation.
**
*/

short n_getPixelMask(void) {

	/* Local variables */
	IRAFPointer ps;		/* pset pointer */
	short mask;		/* combined mask value */
	Bool rs, sat, cray, lin, dark, flat, grot, defect, telem, badpix,
	source, zread;		/* bit mask switches */

# if defined(NATIVE_IRAF)

	/* Open the pset */
	ps     = c_clopset ("nicdqpar");

	/* Read each of the bit mask settings */
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

	/* Close the pset */
	c_clcpset (ps);

	/* Combine the bit settings into a mask */
	mask = 0;
	if (rs)     mask |= N_RS; 
	if (sat)    mask |= N_SATUR; 
	if (cray)   mask |= N_CRAY; 
	if (lin)    mask |= N_PLIN; 
	if (dark)   mask |= N_PDARK; 
	if (flat)   mask |= N_PFF; 
	if (grot)   mask |= N_GROT; 
	if (defect) mask |= N_DEFECT; 
	if (telem)  mask |= N_TELEM; 
	if (badpix) mask |= N_BADPIX; 
	if (source) mask |= N_SOURCE; 
	if (zread)  mask |= N_ZREAD; 

	/* Return the combined bit mask */
	return (mask);

# else

	/* Return a null mask for non-native IRAF implementations */
	return (0);

# endif

}


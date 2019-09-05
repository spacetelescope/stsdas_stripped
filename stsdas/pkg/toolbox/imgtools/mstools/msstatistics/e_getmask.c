# include <stdio.h>
# include <c_iraf.h>
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "msstat.h"

/*  E_GETMASK  --   Reads psets with bit selections and builds masks.
 *
 *  As by-product, read also the DQF extension for GEIS files.
 *
 *
 *
 *   Revision history:
 *   ---------------
 *   06 Jun 98  -  Implementation (IB)
 *   10 Feb 98  -  Add STIS support (IB)
 *   22 May 98  -  Add zero-read DQ bit (IB)
 *    2 May 07  -  Add COS,WFC3 support (WJH)
 *    2 Jun 10  -  Updated WFC3 & ACS flag assignments (HAB)
 *
 */


void e_getMask (Control *con, Bool dqon) {

	IRAFPointer ps;
	int          i;

	Bool rs, sat, cray;

	Bool generic, calib, camdef, hot, fixhot, mis, trap, overlap;
	Bool lin, dark, flat, back, defect, telem, badpix, source, zread;
	Bool brush, grid, nearedge, dead, chot, burst, outbounds, datafill;
    Bool phlow, phhigh, badtime, badwave;

    Bool softerr, datalost, datamasked, hotpix, largeblem, badzero;  
    Bool overscan, badbias, satpix, badflat, smallblem, atodsat;
    Bool zerosig, warmpix, unstable, ctetail, datarej1, datarej2, crosstalk; 
        
	Bool fill, baddet, masked, lfblem, sfblem, badref;
	Bool extback, extbad, rej;

	Bool bit1, bit2,  bit3,  bit4,  bit5,  bit6,  bit7,  bit8;
	Bool bit9, bit10, bit11, bit12, bit13, bit14, bit15, bit16;

# if defined(NATIVE_IRAF)

	for (i = 0; i < MAX_FTYPE; con->gmask[i++] = 0x0000);
	strcpy (con->dqfExten, "");

	/* Turn DQ masking off if user asked to. */
	if (!dqon) return;

	/* NICMOS */
	ps = c_clopset ("nicdqpar");
	rs     = c_clgpsetb (ps, "rs");
	sat    = c_clgpsetb (ps, "sat");
	cray   = c_clgpsetb (ps, "cray");
	lin    = c_clgpsetb (ps, "lin");
	dark   = c_clgpsetb (ps, "dark");
	flat   = c_clgpsetb (ps, "flat");
	back   = c_clgpsetb (ps, "grot");
	defect = c_clgpsetb (ps, "defect");
	telem  = c_clgpsetb (ps, "telemetry");
	badpix = c_clgpsetb (ps, "badpix");
	source = c_clgpsetb (ps, "source");
	zread  = c_clgpsetb (ps, "zread");
	c_clcpset (ps);

	if (rs)     con->gmask[NICMOS] |= N_RS; 
	if (sat)    con->gmask[NICMOS] |= N_SATUR; 
	if (cray)   con->gmask[NICMOS] |= N_CRAY; 
	if (lin)    con->gmask[NICMOS] |= N_PLIN; 
	if (dark)   con->gmask[NICMOS] |= N_PDARK; 
	if (flat)   con->gmask[NICMOS] |= N_PFF; 
	if (back)   con->gmask[NICMOS] |= N_PBCK; 
	if (defect) con->gmask[NICMOS] |= N_DEFECT; 
	if (telem)  con->gmask[NICMOS] |= N_TELEM; 
	if (badpix) con->gmask[NICMOS] |= N_BADPIX; 
	if (source) con->gmask[NICMOS] |= N_SOURCE; 
	if (zread)  con->gmask[NICMOS] |= N_ZREAD; 

	/* STIS */
	ps = c_clopset ("stisdqpar");
	rs      = c_clgpsetb (ps, "rs");
	fill    = c_clgpsetb (ps, "fill");
	baddet  = c_clgpsetb (ps, "baddet");
	masked  = c_clgpsetb (ps, "masked");
	dark    = c_clgpsetb (ps, "dark");
	lfblem  = c_clgpsetb (ps, "lfblem");
	sat     = c_clgpsetb (ps, "sat");
	badref  = c_clgpsetb (ps, "badref");
	sfblem  = c_clgpsetb (ps, "sfblem");
	extback = c_clgpsetb (ps, "extback");
	extbad  = c_clgpsetb (ps, "extbad");
	rej     = c_clgpsetb (ps, "rej");
	c_clcpset (ps);

	if (rs)      con->gmask[STIS] |= S_RS;
	if (fill)    con->gmask[STIS] |= S_FILL;
	if (baddet)  con->gmask[STIS] |= S_BADDET;
	if (masked)  con->gmask[STIS] |= S_MASK;
	if (dark)    con->gmask[STIS] |= S_LDARK;
	if (lfblem)  con->gmask[STIS] |= S_LFBLEM;
	if (sfblem)  con->gmask[STIS] |= S_SFBLEM;
	if (sat)     con->gmask[STIS] |= S_SAT; 
	if (badref)  con->gmask[STIS] |= S_BADREF; 
	if (extback) con->gmask[STIS] |= S_EXTBACK; 
	if (extbad)  con->gmask[STIS] |= S_EXTBAD; 
	if (rej)     con->gmask[STIS] |= S_COMBREJ; 

	/* ACS  */
	ps = c_clopset ("acsdqpar");
	rs       = c_clgpsetb (ps, "rs");
	fill     = c_clgpsetb (ps, "fill");
	baddet   = c_clgpsetb (ps, "baddet");
	masked   = c_clgpsetb (ps, "masked");
	hotpix   = c_clgpsetb (ps, "hotpix");
	ctetail  = c_clgpsetb (ps, "ctetail");
	warmpix  = c_clgpsetb (ps, "warmpix");
	badbias  = c_clgpsetb (ps, "badbias");
	satpix   = c_clgpsetb (ps, "satpix");
	badflat  = c_clgpsetb (ps, "badflat");
	trap     = c_clgpsetb (ps, "trap");
	atodsat  = c_clgpsetb (ps, "atodsat");
	datarej2 = c_clgpsetb (ps, "mdcr");
	datarej1 = c_clgpsetb (ps, "calcr");
	c_clcpset (ps);

	if (rs)       con->gmask[ACS] |= A_RS;
	if (fill)     con->gmask[ACS] |= A_FILL;
	if (baddet)   con->gmask[ACS] |= A_BADDET;
	if (masked)   con->gmask[ACS] |= A_MASK;
	if (hotpix)   con->gmask[ACS] |= A_HOTPIX;
	if (ctetail)  con->gmask[ACS] |= A_CTETAIL;
	if (warmpix)  con->gmask[ACS] |= A_WARMPIX; 
	if (badbias)  con->gmask[ACS] |= A_BADBIAS;
	if (satpix)   con->gmask[ACS] |= A_SATPIX; 
	if (badflat)  con->gmask[ACS] |= A_BADFLAT; 
	if (trap)     con->gmask[ACS] |= A_TRAP; 
	if (atodsat)  con->gmask[ACS] |= A_ATODSAT; 
	if (datarej2) con->gmask[ACS] |= A_MDCR; 
	if (datarej1) con->gmask[ACS] |= A_CALCR; 

	/* WFC3  */
	ps = c_clopset ("wfc3dqpar");
	rs        = c_clgpsetb (ps, "rs");
	fill      = c_clgpsetb (ps, "fill");
	baddet    = c_clgpsetb (ps, "baddet");
	badzero   = c_clgpsetb (ps, "badzero");
	hotpix    = c_clgpsetb (ps, "hotpix");
	ctetail   = c_clgpsetb (ps, "ctetail");
	unstable  = c_clgpsetb (ps, "unstable");
	warmpix   = c_clgpsetb (ps, "warmpix");
	badbias   = c_clgpsetb (ps, "badbias");
	satpix    = c_clgpsetb (ps, "satpix");
	badflat   = c_clgpsetb (ps, "badflat");
	trap      = c_clgpsetb (ps, "trap");
	atodsat   = c_clgpsetb (ps, "atodsat");
	zerosig   = c_clgpsetb (ps, "zerosig");
	datarej2  = c_clgpsetb (ps, "mdcr");
	datarej1  = c_clgpsetb (ps, "calcr");
	crosstalk = c_clgpsetb (ps, "crosstalk");
	c_clcpset (ps);

	if (rs)        con->gmask[WFC3UV] |= W3_RS;
	if (fill)      con->gmask[WFC3UV] |= W3_FILL;
	if (baddet)    con->gmask[WFC3UV] |= W3_BADDET;
	if (hotpix)    con->gmask[WFC3UV] |= W3_HOTPIX;
	if (ctetail)   con->gmask[WFC3UV] |= W3_CTETAIL;
	if (warmpix)   con->gmask[WFC3UV] |= W3_WARMPIX; 
	if (badbias)   con->gmask[WFC3UV] |= W3_BADBIAS;
	if (satpix)    con->gmask[WFC3UV] |= W3_SATPIX; 
	if (badflat)   con->gmask[WFC3UV] |= W3_BADFLAT; 
	if (trap)      con->gmask[WFC3UV] |= W3_TRAP; 
	if (atodsat)   con->gmask[WFC3UV] |= W3_ATODSAT; 
	if (datarej2)  con->gmask[WFC3UV] |= W3_MDCR; 
	if (datarej1)  con->gmask[WFC3UV] |= W3_CALCR; 
	if (crosstalk) con->gmask[WFC3UV] |= W3_CROSSTALK; 

	if (rs)        con->gmask[WFC3IR] |= W3_RS;
	if (fill)      con->gmask[WFC3IR] |= W3_FILL;
	if (baddet)    con->gmask[WFC3IR] |= W3_BADDET;
	if (badzero)   con->gmask[WFC3IR] |= W3_BADZERO;
	if (hotpix)    con->gmask[WFC3IR] |= W3_HOTPIX;
	if (unstable)  con->gmask[WFC3IR] |= W3_UNSTABLE;
	if (warmpix)   con->gmask[WFC3IR] |= W3_WARMPIX; 
	if (badbias)   con->gmask[WFC3IR] |= W3_BADBIAS;
	if (satpix)    con->gmask[WFC3IR] |= W3_SATPIX; 
	if (badflat)   con->gmask[WFC3IR] |= W3_BADFLAT; 
	if (zerosig)   con->gmask[WFC3IR] |= W3_ZEROSIG; 
	if (datarej2)  con->gmask[WFC3IR] |= W3_MDCR; 
	if (datarej1)  con->gmask[WFC3IR] |= W3_CALCR; 
	if (crosstalk) con->gmask[WFC3IR] |= W3_CROSSTALK; 

	/* COS */
	ps = c_clopset ("cosdqpar");
	rs      = c_clgpsetb (ps, "rs");
	brush   = c_clgpsetb (ps, "brush");
	grid    = c_clgpsetb (ps, "grid");
	nearedge= c_clgpsetb (ps, "nearedge");
	dead    = c_clgpsetb (ps, "dead");
	chot    = c_clgpsetb (ps, "hot");
	burst   = c_clgpsetb (ps, "burst");
	outbounds= c_clgpsetb (ps, "outbounds");
	datafill= c_clgpsetb (ps, "datafill");
	phlow   = c_clgpsetb (ps, "phlow");
	phhigh  = c_clgpsetb (ps, "phhigh");
	badtime = c_clgpsetb (ps, "badtime");
	badwave = c_clgpsetb (ps, "badwave");
	c_clcpset (ps);

	if (rs)        con->gmask[COS] |= C_RS;
	if (brush)     con->gmask[COS] |= C_BRUSH;
	if (grid)      con->gmask[COS] |= C_GRID;
	if (nearedge)  con->gmask[COS] |= C_NEAREDGE;
	if (dead)      con->gmask[COS] |= C_DEAD;
	if (chot)      con->gmask[COS] |= C_HOT;
	if (burst)     con->gmask[COS] |= C_BURST;
	if (outbounds) con->gmask[COS] |= C_OUTBOUNDS; 
	if (datafill)  con->gmask[COS] |= C_DATAFILL; 
	if (phlow)     con->gmask[COS] |= C_PHLOW; 
	if (phhigh)    con->gmask[COS] |= C_PHHIGH; 
	if (badtime)   con->gmask[COS] |= C_BADTIME; 
	if (badwave)   con->gmask[COS] |= C_BADWAVE; 

	/* WFPC */
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
	c_clgpseta (ps, "dqfextn", con->dqfExten, 6);
	c_clcpset (ps);

	if (rs)      con->gmask[GEIS] |= W_RS; 
	if (sat)     con->gmask[GEIS] |= W_SATUR; 
	if (cray)    con->gmask[GEIS] |= W_CRAY; 
	if (generic) con->gmask[GEIS] |= W_BADPIX; 
	if (calib)   con->gmask[GEIS] |= W_CAL; 
	if (camdef)  con->gmask[GEIS] |= W_CAMERA; 
	if (hot)     con->gmask[GEIS] |= W_UHOT; 
	if (fixhot)  con->gmask[GEIS] |= W_FHOT; 
	if (mis)     con->gmask[GEIS] |= W_MISSING;

	/* OIF is all masked for now. */
 	con->gmask[OIF]  = 0xFFFF;

	/* The generic bits are ORed with the instrument-specific ones. */
	ps = c_clopset ("dqbits");
	bit1  = c_clgpsetb (ps, "bit1");
	bit2  = c_clgpsetb (ps, "bit2");
	bit3  = c_clgpsetb (ps, "bit3");
	bit4  = c_clgpsetb (ps, "bit4");
	bit5  = c_clgpsetb (ps, "bit5");
	bit6  = c_clgpsetb (ps, "bit6");
	bit7  = c_clgpsetb (ps, "bit7");
	bit8  = c_clgpsetb (ps, "bit8");
	bit9  = c_clgpsetb (ps, "bit9");
	bit10 = c_clgpsetb (ps, "bit10");
	bit11 = c_clgpsetb (ps, "bit11");
	bit12 = c_clgpsetb (ps, "bit12");
	bit13 = c_clgpsetb (ps, "bit13");
	bit14 = c_clgpsetb (ps, "bit14");
	bit15 = c_clgpsetb (ps, "bit15");
	bit16 = c_clgpsetb (ps, "bit16");
	c_clcpset (ps);

	for (i = 0; i < MAX_FTYPE; i++) {
	    if (bit1)  con->gmask[i] |= 0x0001; 
	    if (bit2)  con->gmask[i] |= 0x0002; 
	    if (bit3)  con->gmask[i] |= 0x0004; 
	    if (bit4)  con->gmask[i] |= 0x0008; 
	    if (bit5)  con->gmask[i] |= 0x0010; 
	    if (bit6)  con->gmask[i] |= 0x0020; 
	    if (bit7)  con->gmask[i] |= 0x0040; 
	    if (bit8)  con->gmask[i] |= 0x0080; 
	    if (bit9)  con->gmask[i] |= 0x0100; 
	    if (bit10) con->gmask[i] |= 0x0200; 
	    if (bit11) con->gmask[i] |= 0x0400; 
	    if (bit12) con->gmask[i] |= 0x0800; 
	    if (bit13) con->gmask[i] |= 0x1000; 
	    if (bit14) con->gmask[i] |= 0x2000; 
	    if (bit15) con->gmask[i] |= 0x4000; 
	    if (bit16) con->gmask[i] |= 0x8000; 
	}


# else
	for (i = 0; i < MAX_FTYPE; con->gmask[i++] = 0xFFFF);
# endif

}

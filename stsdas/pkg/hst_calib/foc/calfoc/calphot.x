include <tbset.h>

# This file contains the following four routines:
# cal_g_phot	get mode info from image header, construct an HST mode string,
#			and get photometry info (pivot wavelength, filter
#			bandwidth, and inverse sensitivity)
# cal_filt	append filter names to mode string
# cal_g_uni	get UNI file name from pivot wavelength
# cal_p_phot	put photometry info into an image header

define	LEN_PHOT	4	# number of elements in phot array
define	SZ_FILT		11	# max length of filter name
define	NUM_FILT48	8	# number of filters per wheel for f/48
define	NUM_FILT96	12	# number of filters per wheel for f/96
define	NUM_WH48	2	# number of filter wheels for f/48
define	NUM_WH96	4	# number of filter wheels for f/96

# Sampoff and lineoff values used by cal_aperture.
# f/96 relay:
define	X04		0	# sampoff for 0.4 arcsec finger
define	Y04		512	# lineoff for   "
define	X08		421	# sampoff for 0.8 arcsec finger
define	Y08		0	# lineoff for   "
define	XUF		256	# sampoff for FUVOP undispersed format
define	YUF		0	# lineoff for   "
define	XDF		256	# sampoff for FUVOP dispersed format
define	YDF		512	# lineoff for   "
# f/96 relay with cammode:
define	X04_512_288	152	# sampoff for 0.4 arcsec finger, 512 x 512
define	Y04_512_288	460	# lineoff for   "
define	X04_256_288	280	# sampoff for 0.4 arcsec finger, 256 x 256
define	Y04_256_288	588	# lineoff for   "
# f/48 relay:
define	XNOSLIT		512	# sampoff for slitless spectrograph format
define	YNOSLIT		0	# lineoff for   "

# cal_g_phot -- get photometry information
# This routine gets the relay and filter information from an image header,
# constructs a string containing that info, and calls GETPHOTX to compute
# the photometry parameters.  GETPHOTX is based on the 'evalband' task in
# the 'synphot' package.
#
# Phil Hodge  2-Jun-1989  subroutine created
# Phil Hodge 25-Aug-1989  call getphotx instead of evlbnd; replace cal_filt
# Phil Hodge 25-Jan-1990  cal_g_uni:  convert UNI file name to lower case
#				and prepend "xref$"
# Phil Hodge  4-Oct-1990  cal_g_phot & cal_filt:  get filtnam instead of optelt
# Phil Hodge 11-Apr-1994  cal_g_phot:  append COSTAR to mode string if kxdeploy
# Phil Hodge 31-May-1994  cal_aperture:  append aperture name to mode string
# Phil Hodge 12-Jul-1995  cal_g_phot:  append an explicit NOCOSTAR to mode
#			string if pre-COSTAR or if COSTAR is not deployed;
#			cal_g_uni:  also compare CAMMODE values.

procedure cal_g_phot (im, phot, modestr, phot_path, maxpath)

pointer im			# i: pointer to imhdr struct
real	phot[LEN_PHOT]		# o: flam, zero pt, pivot wl, rms bandwidth
char	modestr[SZ_FNAME]	# o: mode string
char	phot_path[ARB]		# o: history info for phot
int	maxpath			# i: max size of phot_path
#--
pointer sp
pointer optcrly, smmmode, cammode	# scr for values of header keywords
pointer aperture		# scr for aperture name (e.g. X96N512)
pointer graphtab, comptab	# scr for names of graph & component tables
char	keyword[9]		# keyword for filtnam1, etc
char	filtnam[SZ_FILT,NUM_FILT96]	# filter names
int	relay			# 48, 96 or 288 (part of aperture name)
int	numwheel		# number (2 or 4) of filter wheels
int	k			# loop index
int	imaccf()
bool	streq(), imgetb()
errchk	imgstr, imgetb, getphotx

begin
	# Allocate scratch for character strings.
	call smark (sp)
	call salloc (optcrly, SZ_FNAME, TY_CHAR)
	call salloc (aperture, SZ_FNAME, TY_CHAR)
	call salloc (graphtab, SZ_FNAME, TY_CHAR)
	call salloc (comptab, SZ_FNAME, TY_CHAR)

	# Get the names of the graph table and components table.
	call imgstr (im, "graphtab", Memc[graphtab], SZ_FNAME)
	call imgstr (im, "comptab", Memc[comptab], SZ_FNAME)

	# Assign the first part of the mode string, containing
	# the instrument, focal ratio, and spectrograph flag.

	call imgstr (im, "optcrly", Memc[optcrly], SZ_FNAME)

	if (streq (Memc[optcrly], "F48")) {

	    relay = 48
	    numwheel = NUM_WH48

	    call strcpy ("FOC F/48", modestr, SZ_FNAME)

	    # Long-slit spectrograph mode?
	    call salloc (smmmode, SZ_FNAME, TY_CHAR)
	    call imgstr (im, "smmmode", Memc[smmmode], SZ_FNAME)

	    if (streq (Memc[smmmode], "INBEAM"))
	        call strcat (" SPEC", modestr, SZ_FNAME)

	} else if (streq (Memc[optcrly], "F96")) {

	    numwheel = NUM_WH96

	    # HRA used?
	    call salloc (cammode, SZ_FNAME, TY_CHAR)
	    call imgstr (im, "cammode", Memc[cammode], SZ_FNAME)

	    if (streq (Memc[cammode], "INBEAM")) {
	        call strcpy ("FOC F/288", modestr, SZ_FNAME)
		relay = 288
	    } else {
		relay = 96
	        call strcpy ("FOC F/96", modestr, SZ_FNAME)
	    }

	} else {
	    call error (1, "optcrly is invalid")
	}

	if (imaccf (im, "kxdeploy") == YES) {		# 1994 May 31, PEH
	    if (imgetb (im, "kxdeploy"))		# 1994 Apr 11, PEH
		call strcat (" COSTAR", modestr, SZ_FNAME)
	    else
		call strcat (" NOCOSTAR", modestr, SZ_FNAME)
	} else {
	    call strcat (" NOCOSTAR", modestr, SZ_FNAME)
	}
                 
	# Get the filter name for each filter wheel.
	do k = 1, numwheel {
	    call sprintf (keyword, 9, "filtnam%d")
		call pargi (k)
	    call imgstr (im, keyword, filtnam[1,k], SZ_FILT)
	}

	# Append the filter names to the mode string.
	call cal_filt (filtnam, numwheel, modestr, SZ_FNAME)

	# Assign the aperture name (e.g. "X96N512"), and append it to modestr.
	call cal_aperture (im, relay, Memc[aperture], SZ_FNAME)
	call strcat (" ", modestr, SZ_FNAME)
	call strcat (Memc[aperture], modestr, SZ_FNAME)

	# Get photometry info.
	call getphotx (modestr, Memc[graphtab], Memc[comptab],
		phot_path, maxpath, phot)

	call sfree (sp)
end

# cal_aperture -- assign aperture name
# This routine uses the relay (48, 96 or 288), pixel format (ZOOM or NORMAL)
# and image size (SAMPPLN, LINEPFM) to determine the aperture name.  The
# spectrograph mode (SMMMODE) is checked if the relay is 48.

procedure cal_aperture (im, relay, aperture, maxch)

pointer im			# i: pointer to imhdr struct
int	relay			# i: 48, 96 or 288 (part of aperture name)
char	aperture[maxch]		# o: aperture name, e.g. X96N512
int	maxch			# i: size of aperture
#--
pointer sp
pointer pxformt, smmmode	# scr for values of header keywords
int	samppln, linepfm	# size of image
int	sampoff, lineoff	# offset from beginning of photocathode
int	imaccf(), imgeti()
real	imgetr()
bool	streq()
errchk	imgstr, imgetr, imgeti

begin
	call smark (sp)
	call salloc (pxformt, SZ_FNAME, TY_CHAR)

	# Assign initial part of aperture name.
	if (relay == 48)
	    call strcpy ("X48", aperture, SZ_FNAME)
	else if (relay == 96)
	    call strcpy ("X96", aperture, SZ_FNAME)
	else if (relay == 288)
	    call strcpy ("X28", aperture, SZ_FNAME)

	# Get header keyword values.

	if (imaccf (im, "pxformt") == YES)
	    call imgstr (im, "pxformt", Memc[pxformt], SZ_FNAME)
	else
	    Memc[pxformt] = EOS

	if (imaccf (im, "samppln") == YES)
	    samppln = imgeti (im, "samppln")
	else
	    samppln = INDEFI

	if (imaccf (im, "linepfm") == YES)
	    linepfm = imgeti (im, "linepfm")
	else
	    linepfm = INDEFI

	if (imaccf (im, "sampoff") == YES)
	    sampoff = nint (imgetr (im, "sampoff"))
	else
	    sampoff = INDEFI

	if (imaccf (im, "lineoff") == YES)
	    lineoff = nint (imgetr (im, "lineoff"))
	else
	    lineoff = INDEFI

	# Concatenate aperture size info to aperture name.

	if (streq (Memc[pxformt], "ZOOM"))
	    call strcat ("Z", aperture, SZ_FNAME)
	else
	    call strcat ("N", aperture, SZ_FNAME)

	if (samppln == 512 && linepfm == 1024) {
	    call strcat ("LRG", aperture, SZ_FNAME)

	} else if (samppln == 256 && linepfm == 1024) {
	    call strcat ("REC", aperture, SZ_FNAME)

	} else if (samppln == 512 && linepfm == 512) {
	    call strcat ("512", aperture, SZ_FNAME)

	} else if (samppln == 256 && linepfm == 256) {
	    call strcat ("256", aperture, SZ_FNAME)

	} else if (samppln == 256 && linepfm == 128) {
	    call strcat ("256D", aperture, SZ_FNAME)

	} else if (samppln == 128 && linepfm == 128) {
	    call strcat ("128", aperture, SZ_FNAME)

	} else {
	    call strcat ("???", aperture, SZ_FNAME)
	    call logmsg ("warning:  can't determine aperture name")
	    call sfree (sp)
	    return
	}

	# Special format?

	if (relay == 48) {
	    # spectrographic mode
	    call salloc (smmmode, SZ_FNAME, TY_CHAR)
	    if (imaccf (im, "smmmode") == YES)
		call imgstr (im, "smmmode", Memc[smmmode], SZ_FNAME)
	    else
		Memc[smmmode] = EOS
	    if (streq (Memc[smmmode], "INBEAM")) {
		# slitless spectrograph format
		if (sampoff == XNOSLIT && lineoff == YNOSLIT)
		    call strcat ("_CD", aperture, SZ_FNAME)
		else
		    call strcat ("S", aperture, SZ_FNAME)
	    }

	} else if (relay == 96) {

	    if (samppln == 512 && linepfm == 512) {

		# 0.4 arcsec coronographic finger
		if (sampoff == X04 && lineoff == Y04)
		    call strcat ("_4", aperture, SZ_FNAME)

		# 0.8 arcsec coronographic finger
		else if (sampoff == X08 && lineoff == Y08)
		    call strcat ("_8", aperture, SZ_FNAME)

		# undispersed format for FUVOP
		else if (sampoff == XUF && lineoff == YUF)
		    call strcat ("_UF", aperture, SZ_FNAME)

		# dispersed format for FUVOP
		else if (sampoff == XDF && lineoff == YDF)
		    call strcat ("_DF", aperture, SZ_FNAME)
	    }

	} else if (relay == 288) {

	    # 0.4 arcsec coronographic finger, 512 x 512 format
	    if (sampoff == X04_512_288 && lineoff == Y04_512_288)
		call strcat ("_4", aperture, SZ_FNAME)

	    # 0.4 arcsec coronographic finger, 256 x 256 format
	    else if (sampoff == X04_256_288 && lineoff == Y04_256_288)
		call strcat ("_4", aperture, SZ_FNAME)
	}

	call sfree (sp)
end

# cal_filt -- append filter names to mode string
# This routine takes an array of filter names (one for each filter wheel that
# exists for the specified relay) and appends the names to the mode string.
#
# Phil Hodge,  4-Oct-1990  Take filter names instead of numbers.

procedure cal_filt (filtnam, numwheel, modestr, maxch)

char	filtnam[SZ_FILT,numwheel]	# i: filter name for each relay
int	numwheel		# i: number of filter wheels in use
char	modestr[ARB]		# io: mode string; filt names will be appended
int	maxch			# i: max char in modestr
#--
int	k			# filter wheel number (loop index)
int	strncmp()

begin
	# For each filter wheel, append the filter name.

	do k = 1, numwheel {
	    if (strncmp (filtnam[1,k], "CLEAR", 5) != 0) {
		call strcat (" ", modestr, maxch)
		call strcat (filtnam[1,k], modestr, maxch)
	    }
	}
end

# cal_g_uni -- get uni file name
# This routine gets the name of the uni file (flat field) from a table.
# Each row of the table contains relay number, "INBEAM" or "NOTUSED" for
# the CAMMODE flag, uni file name, and the mean wavelength for that file.
# The table name is gotten from the image header.  Every row of the table
# is read to find a row such that the wavelength in the table is closest
# (in the sense of smallest ratio) to the input pivot wavelength.  The
# name of the uni file is gotten from that row.

procedure cal_g_uni (im, pivot_wl, relay, c_cammode, uni_name, maxch)

pointer im			# i: pointer to imhdr struct
real	pivot_wl		# i: pivot wavelength
int	relay			# i: relay number (48 or 96)
char	c_cammode[ARB]		# i: "INBEAM" or "NOTUSED"
char	uni_name[ARB]		# o: name of uni image
int	maxch			# i: max size of uni_name
#--
pointer sp
pointer tname			# scratch for table name
pointer emess			# scratch for possible error message
pointer tp			# pointer to table descriptor
pointer cp[4]			# column pointers
char	uni[SZ_FNAME]		# UNI file name without leading "xref$"
char	colname[SZ_COLNAME,4]	# column names
char	c_relay[SZ_FILT]	# "F48" or "F96" according to 'relay'
char	t_relay[SZ_FILT]	# relay id from table
char	t_cammode[SZ_FILT]	# cammode from table
real	t_wl			# wavelength from table
real	ratio			# ratio of wavelengths
real	best_ratio		# ratio closest to one
int	n_row			# row with nearest wavelength
int	row			# loop index for row number
int	k
pointer tbtopn()
int	tbpsta()
bool	streq()
string	uni_prefix	"xref$"	# environment variable for directory
errchk	imgstr, tbtopn

begin
	call smark (sp)
	call salloc (tname, SZ_FNAME, TY_CHAR)
	call imgstr (im, "unitab", Memc[tname], SZ_FNAME)

	tp = tbtopn (Memc[tname], READ_ONLY, NULL)
	call strcpy ("OPTCRLY", colname[1,1], SZ_COLNAME)
	call strcpy ("CAMMODE", colname[1,2], SZ_COLNAME)
	call strcpy ("WAVELENGTH", colname[1,3], SZ_COLNAME)
	call strcpy ("HEADER_FILE", colname[1,4], SZ_COLNAME)
	do k = 1, 4
	    cp[k] = NULL
	call tbcfnd (tp, colname, cp, 4)
	do k = 1, 4 {
	    if (cp[k] == NULL) {
		call salloc (emess, SZ_LINE, TY_CHAR)
		call sprintf (Memc[emess], SZ_LINE,
			"column %s not found in uni table %s")
		    call pargstr (colname[1,k])
		    call pargstr (Memc[tname])
		call error (1, Memc[emess])
	    }
	}

	if (relay == 48)
	    call strcpy ("F48", c_relay, SZ_FILT)
	else
	    call strcpy ("F96", c_relay, SZ_FILT)

	n_row = 0
	do row = 1, tbpsta (tp, TBL_NROWS) {

	    call tbegtt (tp, cp[1], row, t_relay, SZ_FILT)	# get OPTCRLY
	    call strupr (t_relay)
	    call tbegtt (tp, cp[2], row, t_cammode, SZ_FILT)	# get CAMMODE
	    call strupr (t_cammode)

	    if (streq (t_relay, c_relay) && streq (t_cammode, c_cammode)) {

		call tbegtr (tp, cp[3], row, t_wl)	# get wavelength
		if (t_wl >= pivot_wl)		# divide smaller by larger
		    ratio = pivot_wl / t_wl
		else
		    ratio = t_wl / pivot_wl

		if (n_row == 0) {
		    best_ratio = ratio		# initial value for best_ratio
		    n_row = row
		} else if (ratio >= best_ratio) {
		    n_row = row
		    best_ratio = ratio
		}
	    }
	}
	if (n_row == 0) {
	    call salloc (emess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[emess], SZ_LINE,
			"relay number %d not found in uni table %s")
		call pargi (relay)
		call pargstr (Memc[tname])
	    call error (1, Memc[emess])
	}
	# Get file name, convert to lower case, add prefix.
	call tbegtt (tp, cp[4], n_row, uni, maxch)
	call strlwr (uni)
	call strcpy (uni_prefix, uni_name, maxch)
	call strcat (uni, uni_name, maxch)

	call tbtclo (tp)

	call sfree (sp)
end


# cal_p_phot -- put photometry info
# This routine puts the photometry info into an image header.

procedure cal_p_phot (im, phot, modestr, phot_path)

pointer im			# i: pointer to imhdr struct
real	phot[LEN_PHOT]	  	# i: flam, zero pt, pivot wl, rms bandwidth
char	modestr[ARB]		# i: mode string
char	phot_path[ARB]		# i: history info for phot
#--

begin
	call imaddr (im, "photflam", phot[1])
	call imaddr (im, "photzpt", phot[2])
	call imaddr (im, "photplam", phot[3])
	call imaddr (im, "photbw", phot[4])
	call imastr (im, "photmode", modestr)

	call imputh (im, "history", phot_path)
end

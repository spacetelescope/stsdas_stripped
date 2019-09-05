include	"pplist.h"

include	<tbset.h>

# determine the Observation quality flag for STIS

int procedure oobs_qual (jih)

pointer	jih			# input (jitter) file descripter

int	i
int	tmp[10]

int	imaccf()
int	imgeti()
real	imgetr()
bool	imgetb()

begin
	# if the file does not exist, return UNKNOWN
	if (jih == NULL) {
	    return (UNKNOWN)
	}
	
	# if any of the following keywords is true, return BAD
	# updated 5/1/97 by JC Hsu, re OPR 33671
	if (imaccf (jih, "T_ACQ2FL") == NO ||
	    imaccf (jih, "T_GSFAIL") == NO ||
	    imaccf (jih, "T_TAPDRP") == NO ||
	    imaccf (jih, "T_SLEWNG") == NO ||
	    imaccf (jih, "T_TDFDWN") == NO ||
	    imaccf (jih, "T_SISAFE") == NO ||
	    imaccf (jih, "T_FGSFAL") == NO ||

	    # skip this keyword, according to Ron Downes, 2/24/98
	    #imaccf (jih, "T_NO_EPH") == NO ||
	    imaccf (jih, "T_NO_SAM") == NO ||
	    imaccf (jih, "T_NOSLEW") == NO ||
	    imaccf (jih, "T_SDLOST") == NO ||
	    imaccf (jih, "T_SISPND") == NO) {
	    tmp[5] = UNKNOWN
	} else if (imgetb (jih, "T_ACQ2FL") ||
	    imgetb (jih, "T_GSFAIL") ||
	    imgetb (jih, "T_TAPDRP") ||
	    imgetb (jih, "T_SLEWNG") ||
	    imgetb (jih, "T_TDFDWN") ||
	    imgetb (jih, "T_SISAFE") ||
	    imgetb (jih, "T_FGSFAL") ||
	   #imgetb (jih, "T_NO_EPH") ||
	    imgetb (jih, "T_NO_SAM") ||
	    imgetb (jih, "T_NOSLEW") ||
	    imgetb (jih, "T_SDLOST") ||
	    imgetb (jih, "T_SISPND") ) {
	    return (BAD)
	}
	
	# other criteria
	if (imaccf (jih, "NRECENT") == YES) {
	    if (imgeti (jih, "NRECENT") > 0) 
		return (BAD)
	    else 
		tmp[1] = GOOD
	} else
	    tmp[1] = UNKNOWN 

	if (imaccf (jih, "NLOSSES") == YES) {
	    if (imgeti (jih, "NLOSSES") > 0)
		return (BAD)
	    else 
		tmp[2] = GOOD
	} else
	    tmp[2] = UNKNOWN 

	if (imaccf (jih, "V2_P2P") == YES) {
	    if (imgetr (jih, "V2_P2P") > 60.)
		return (BAD)
	    else 
		tmp[3] = GOOD
	} else
	    tmp[3] = UNKNOWN 

	if (imaccf (jih, "V3_P2P") == YES) {
	    if (imgetr (jih, "V3_P2P") > 60.)
		return (BAD)
	    else 
		tmp[4] = GOOD
	} else
	    tmp[4] = UNKNOWN 

	# if any of the above keywords is missing, return UNKNOWN
	do i = 1, 5 {
	    if (tmp[i] == UNKNOWN)
		return (UNKNOWN)
	}

	return (GOOD)
end

# determine the Observation quality flag for STIS ACQ and ACQ/PEAK modes

int procedure oacq_qual (jih)

pointer	jih			# input (jitter) file descripter

int	i
int	tmp[10]

int	imaccf()
int	imgeti()
bool	imgetb()

begin
	# if the file does not exist, return UNKNOWN
	if (jih == NULL) {
	    return (UNKNOWN)
	}
	
	# if any of the following keywords is true, return BAD
	# updated 5/1/97 by JC Hsu, re OPR 33671
	if (imaccf (jih, "T_ACQ2FL") == NO ||
	    imaccf (jih, "T_GSFAIL") == NO ||
	    imaccf (jih, "T_TAPDRP") == NO ||
	    imaccf (jih, "T_SISAFE") == NO ||
	    imaccf (jih, "T_FGSFAL") == NO ||
	   #imaccf (jih, "T_NO_EPH") == NO ||
	    imaccf (jih, "T_NO_SAM") == NO ||
	    imaccf (jih, "T_NOSLEW") == NO ||
	    imaccf (jih, "T_SDLOST") == NO ||
	    imaccf (jih, "T_SISPND") == NO) {
	    tmp[3] = UNKNOWN
	} else if (imgetb (jih, "T_ACQ2FL") ||
	    imgetb (jih, "T_GSFAIL") ||
	    imgetb (jih, "T_TAPDRP") ||
	    imgetb (jih, "T_SISAFE") ||
	    imgetb (jih, "T_FGSFAL") ||
	   #imgetb (jih, "T_NO_EPH") ||
	    imgetb (jih, "T_NO_SAM") ||
	    imgetb (jih, "T_NOSLEW") ||
	    imgetb (jih, "T_SDLOST") ||
	    imgetb (jih, "T_SISPND") ) {
	    return (BAD)
	}
	
	# other criteria
	if (imaccf (jih, "NRECENT") == YES) {
	    if (imgeti (jih, "NRECENT") > 0) 
		return (BAD)
	    else 
		tmp[1] = GOOD
	} else
	    tmp[1] = UNKNOWN 

	if (imaccf (jih, "NLOSSES") == YES) {
	    if (imgeti (jih, "NLOSSES") > 0)
		return (BAD)
	    else 
		tmp[2] = GOOD
	} else
	    tmp[2] = UNKNOWN 

	# if any of the above keywords is missing, return UNKNOWN
	do i = 1, 3 {
	    if (tmp[i] == UNKNOWN)
		return (UNKNOWN)
	}

	return (GOOD)
end

# determine the pipeline processing quality flag for STIS

int procedure oproc_qual (trl)

pointer	trl			# input (trailer) file descripter

pointer	colptr
int	nrows, i, indx
char	buf[SZ_LINE]

int   	strmatch()
int	tbpsta()

begin

	# if the file does not exist, return UNKNOWN
	if (trl == NULL)
	    return (UNKNOWN)
	
	# find the (only) column in the trailer file
	call tbcfnd1 (trl, "TEXT_FILE", colptr)

	# find how many rows are there
	nrows = tbpsta (trl, TBL_NROWS)

	# go through each row
	do i = 1, nrows {
	    call tbegtt (trl, colptr, i, buf, SZ_LINE)
	
            if (strmatch(buf, "ERROR") != 0)
		return (BAD)
	    else {
		indx = strmatch(buf, "-E")
		if (indx > 3) {
                   if (buf[indx-3] >= '0' && buf[indx-3] <= '9')
			return (BAD)
		}
	    }
	}

	return (GOOD)
end

# determine the calibration quality flag for STIS

int procedure ocal_qual (raw, trl)

pointer	raw			# input (data) file descripter
pointer	trl			# input (trailer) file descripter

pointer	colptr
int	nrows, i
char	buf[SZ_LINE]
char	corr[SZ_LINE, 30]

int   	strmatch()
int	tbpsta()
bool	strne()

begin

	# if all calibration switches are set to "OMIT", return NA.
	iferr (call imgstr (raw, "DQICORR",  corr[1, 1], SZ_LINE)) 
	    call strcpy ("OMIT", corr[1, 1], SZ_LINE)
	iferr (call imgstr (raw, "GLINCORR", corr[1, 2], SZ_LINE)) 
	    call strcpy ("OMIT", corr[1, 2], SZ_LINE)
	iferr (call imgstr (raw, "ATODCORR", corr[1, 3], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 3], SZ_LINE)
	iferr (call imgstr (raw, "BIASCORR", corr[1, 4], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 4], SZ_LINE)
	iferr (call imgstr (raw, "CRCORR",   corr[1, 5], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 5], SZ_LINE)

	iferr (call imgstr (raw, "DARKCORR", corr[1, 6], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 6], SZ_LINE)
	iferr (call imgstr (raw, "FLATCORR", corr[1, 7], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 7], SZ_LINE)
	iferr (call imgstr (raw, "PHOTCORR", corr[1, 8], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 8], SZ_LINE)
	iferr (call imgstr (raw, "FLUXCORR", corr[1, 9], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 9], SZ_LINE)
	iferr (call imgstr (raw, "X2DCORR",  corr[1, 10], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 10], SZ_LINE)

	iferr (call imgstr (raw, "GEOCORR",  corr[1, 11], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 11], SZ_LINE)
	iferr (call imgstr (raw, "WAVECORR", corr[1, 12], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 12], SZ_LINE)
	iferr (call imgstr (raw, "DISPCORR", corr[1, 13], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 13], SZ_LINE)
	iferr (call imgstr (raw, "SGEOCORR", corr[1, 14], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 14], SZ_LINE)
	iferr (call imgstr (raw, "SHADCORR", corr[1, 15], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 15], SZ_LINE)

	iferr (call imgstr (raw, "X1DCORR",  corr[1, 16], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 16], SZ_LINE)

	do i = 1, 16 {
	    if (strne (corr[1, i], "OMIT")) go to 10
	}
	return (NA)

	# if the file does not exist, return UNKNOWN
10
	if (trl == NULL)
	    return (UNKNOWN)
	
	# find the (only) column in the trailer file
	call tbcfnd1 (trl, "TEXT_FILE", colptr)

	# find how many rows are there
	nrows = tbpsta (trl, TBL_NROWS)

	# go through each row
	do i = 1, nrows {
	    call tbegtt (trl, colptr, i, buf, SZ_LINE)
	
            if (strmatch(buf, "SKIPPED") != 0 || strmatch(buf, "DUMMY") != 0)
		return (BAD)
	}

	return (GOOD)
end

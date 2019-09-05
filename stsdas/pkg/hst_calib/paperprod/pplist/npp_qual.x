include	"pplist.h"

include	<tbset.h>

# determine the Observation quality flag for NICMOS

int procedure nobs_qual (pdq)

pointer	pdq			# input (PDQ) file descripter

pointer	colptr
int	nrows, i
char	buf[SZ_LINE]
char    qual[SZ_LINE]
char    dummy[8]

int   	strmatch()
int	tbpsta()
bool	qual_check()

begin

	# if the file does not exist, return UNKNOWN
	if (pdq == NULL)
	    return (UNKNOWN)
	
	# find the (only) column in the trailer file
	call tbcfnd1 (pdq, "TEXT_FILE", colptr)
	if (colptr == NULL) {
	    call printf ("Column TEXT_FILE not found in the PDQ file.\n")
	    return (UNKNOWN)
	}

	# find how many rows are there
	nrows = tbpsta (pdq, TBL_NROWS)

	# go through each row
	do i = 1, nrows {
	    call tbegtt (pdq, colptr, i, buf, SZ_LINE)

	    # if QUALITY = OK, return good, otherwise bad.
            call strcpy (buf, dummy, 9)
            if (strmatch (dummy, "QUALITY") != 0) {
                call qual_val (buf, qual)
		if (qual_check(qual, "OK, NO-EVAL, NO-TLM, TM_GAP"))
		    return (GOOD)
		else 
		    return (BAD)
	    }
	}

	return (GOOD)
end

int procedure xnobs_qual (jih)

pointer	jih			# input (jitter) file descripter

int	i
int	tmp[10]

int	imaccf()
int	imgeti()
real	imgetr()

begin
	# if the file does not exist, return UNKNOWN
	if (jih == NULL)
	    return (UNKNOWN)
	
	# if any of the following keywords is present, return BAD
	if (imaccf (jih, "T_ACQ2FL") == YES ||
	    imaccf (jih, "T_GSFAIL") == YES ||
	    imaccf (jih, "T_TAPDRP") == YES ||
	    imaccf (jih, "T_SLEWNG") == YES ||
	    imaccf (jih, "T_TDFDWN") == YES ||
	    imaccf (jih, "T_SISAFE") == YES ||
	    imaccf (jih, "NO_SAM")   == YES ||
	    imaccf (jih, "NO_SLEW")  == YES ||
	    imaccf (jih, "SD_LOST")  == YES ||
	    imaccf (jih, "FW_ERROR") == YES ||
	    imaccf (jih, "SISUSPND") == YES) 
	    return (BAD)
	
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
	do i = 1, 4 {
	    if (tmp[i] == UNKNOWN)
		return (UNKNOWN)
	}

	return (GOOD)
end

# determine the pipeline processing quality flag for NICMOS

int procedure nproc_qual (trl)

pointer	trl			# input (trailer) file descripter

pointer	colptr
int	nrows, i
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
	}

	return (GOOD)
end

# determine the calibration quality flag for NICMOS

int procedure ncal_qual (raw, trl)

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
	iferr (call imgstr (raw, "BIASCORR",  corr[1, 1], SZ_LINE)) 
	    call strcpy ("OMIT", corr[1, 1], SZ_LINE)
	iferr (call imgstr (raw, "ZOFFCORR", corr[1, 2], SZ_LINE)) 
	    call strcpy ("OMIT", corr[1, 2], SZ_LINE)
	iferr (call imgstr (raw, "MASKCORR", corr[1, 3], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 3], SZ_LINE)
	iferr (call imgstr (raw, "NOISCALC", corr[1, 4], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 4], SZ_LINE)
	iferr (call imgstr (raw, "NLINCORR",   corr[1, 5], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 5], SZ_LINE)

	iferr (call imgstr (raw, "DARKCORR", corr[1, 6], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 6], SZ_LINE)
	iferr (call imgstr (raw, "FLATCORR", corr[1, 7], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 7], SZ_LINE)
	iferr (call imgstr (raw, "UNITCORR", corr[1, 8], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 8], SZ_LINE)
	iferr (call imgstr (raw, "PHOTCALC", corr[1, 9], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 9], SZ_LINE)
	iferr (call imgstr (raw, "CRIDCALC", corr[1, 10], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 10], SZ_LINE)

	iferr (call imgstr (raw, "BACKCALC", corr[1, 11], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 11], SZ_LINE)
	iferr (call imgstr (raw, "WARNCALC", corr[1, 12], SZ_LINE))
	    call strcpy ("OMIT", corr[1, 12], SZ_LINE)

	do i = 1, 12 {
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

	# go through each row, if the word DUMMY appears, return BAD
	do i = 1, nrows {
	    call tbegtt (trl, colptr, i, buf, SZ_LINE)
	
            if (strmatch(buf, "DUMMY") != 0)
		return (BAD)
	}

	return (GOOD)
end

# get the pipeline processing and calibration data quality summaries for STIS
# from the trailer file

include	<tbset.h>
include	"opp.h"

procedure opp_trlsum (trl, pipeline, caldq)

char	trl[SZ_FNAME]			# input (trailer) file name
char	pipeline[SZ_LINE, MAX_SUMMARY]	# pipeline processing comments
char	caldq[SZ_LINE, MAX_SUMMARY]	# calibartion processing comments

pointer	tb			
pointer	colptr
int	nrows, i, j
int	npipe, ncal
int	indx
char	buf[SZ_LINE]
bool    same

pointer	tbtopn()
int   	strmatch()
int	tbpsta()
bool	streq()

begin
	# initialize the output strings
	do i = 1, MAX_SUMMARY {
	    pipeline[1,i] = EOS
	    caldq[1,i] = EOS
	}

	# open the trailer file (a table)
        iferr (tb = tbtopn (trl, READ_ONLY, 0)) {
            call strcpy ("(Cannot access the trailer file.)", pipeline[1,1], 
                         SZ_LINE)
            npipe = 1
            call strcpy ("(Cannot access the trailer file.)", caldq[1,1], 
                         SZ_LINE)
            ncal = 1
            return
        }
	
	# find the (only) column in the trailer file
	call tbcfnd1 (tb, "TEXT_FILE", colptr)

	# find how many rows are there
	nrows = tbpsta (tb, TBL_NROWS)

	npipe = 0
	ncal = 0

	# go through each row
	do i = 1, nrows {
	    call tbegtt (tb, colptr, i, buf, SZ_LINE)
	
	    # look for the word "ERROR"
            if (strmatch(buf, "ERROR") != 0) {
 
                # skip redundant messages
                same = false
                do j = 1, min(npipe, MAX_SUMMARY) {
                    same = (streq (buf, pipeline[1,j]))
                    if (same) break
                }
                if (same) next
		npipe = npipe + 1
		if (npipe >= MAX_SUMMARY) {
		    call strcpy ("(There are more errors...)", 
				 pipeline[1,MAX_SUMMARY], SZ_LINE)
		} else
		    call strcpy (buf, pipeline[1,npipe], SZ_LINE)

	    # look for the string "-E"
	    } else if (strmatch(buf, "-E") != 0) {
		indx = strmatch (buf, "-E")
		if (indx > 3) {
		    if (buf[indx-3] >= '0' && buf[indx-3] <= '9') {
			npipe = npipe + 1
			if (npipe >= MAX_SUMMARY) {
		            call strcpy ("(There are more errors...)", 
				         pipeline[1,MAX_SUMMARY], SZ_LINE)
			} else {
		            call strcpy("ERROR: ", pipeline[1,npipe], SZ_LINE)
		            call strcat(buf[indx+1], pipeline[1,npipe], SZ_LINE)
			}
		    }
		}

	    # look for the work "DUMMY"
	    } else if (strmatch(buf, "DUMMY") != 0) {
 
                # skip redundant messages
                same = false
                do j = 1, min(ncal, MAX_SUMMARY) {
                    same = (streq (buf, caldq[1,j]))
                    if (same) break
                }
                if (same) next
		ncal = ncal + 1
		if (ncal >= MAX_SUMMARY) {
		    call strcpy ("(There are more errors...)", 
				 caldq[1,MAX_SUMMARY], SZ_LINE)
		} else 
		    call strcpy (buf, caldq[1,ncal], SZ_LINE)

	    # look for the work "SKIPPED"
	    } else if (strmatch(buf, "SKIPPED") != 0) {
 
                # skip redundant messages
                same = false
                do j = 1, min(ncal, MAX_SUMMARY) {
                    same = (streq (buf, caldq[1,j]))
                    if (same) break
                }
                if (same) next
		ncal = ncal + 1
		if (ncal >= MAX_SUMMARY) {
		    call strcpy ("(There are more errors...)", 
				 caldq[1,MAX_SUMMARY], SZ_LINE)
		} else 
		    call strcpy (buf, caldq[1,ncal], SZ_LINE)
	    }
	}

	if (npipe == 0) {
	    call strcpy ("No anomalies.", pipeline[1,1], SZ_LINE)
	    npipe = 1
	}

	if (ncal == 0) {
	    call strcpy ("No anomalies.", caldq[1,1], SZ_LINE)
	    ncal = 1
	}

	# close the table
	if (tb != NULL) call tbtclo (tb)
end

include <imhdr.h>
include	<tbset.h>
include <imio.h>
include <mach.h>
include <error.h>
include "wrdata.h"

#################################################################################
# WR_CALC --	Main processing routine. This processes all groups in the 	#
#		image and handles the file management. The actual processing 	#
#		of each group is done by wr_dochip.  				#

procedure wr_calc ( )

include "wrincl.h"

int	grpnum		# current group element number
int	groupnumber	# group number in reference file for this CHIPNUM
bool	c0flag		# pass to WR_UPDATEKW to limit phot error messages
char	text[SZ_LINE]	# buffer for message construction
pointer	wr_input ()
pointer	wr_reffile ()
pointer wr_reftab ()
pointer wr_output ()
int	wr_outfile ()
int	imgeti ()

errchk	sprintf, pargi, wr_message, wr_input, imgeti, wr_kwerr,
	    wr_error, wr_reffile, wr_reftab, wr_output, wr_outfile,
	    wr_dochip, wr_updatekw, wr_groups, wr_finishup

begin
	#  Do each group
	do grpnum = 1, NGROUP {	

	    call sprintf ( text, SZ_LINE, "Start Process Element %d" )
	    call pargi ( grpnum )
	    call wr_message ( text )

	    #  Open input image element and DQF (already open for element 1)
	    if ( grpnum > 1 ) {
		II_IM = wr_input ( grpnum, II_NAME, II_OPEN )
		ID_IM = wr_input ( grpnum, ID_NAME, ID_OPEN )
	    }

	    #  Determine detector number to get ref file group number
	    iferr ( CHIPNUM = imgeti ( II_IM, "DETECTOR" ) )
		call wr_kwerr ( "DETECTOR" )
	    if ( WFC )
		groupnumber = CHIPNUM		# WFC group numbers
	    else
		groupnumber = CHIPNUM - 4	# PC group numbers

	    if ( groupnumber < 1 || groupnumber > 4 ) {
		call sprintf ( text, SZ_LINE,
		" DETECTOR parameter in element %d out of bounds")
		call pargi ( grpnum )
		call wr_error ( text )
	    }

	    #  Open necessary reference files (to group groupnumber)
	    #  (we assume all ref files have four groups)

	    if ( DOMASK )
		MD_IM  = wr_reffile ( groupnumber, MD_NAME, MD_OPEN )

	    if ( DOATOD )
		A_IM   =  wr_reffile ( groupnumber, A_NAME, A_OPEN )

	    if ( DOBLEV ) {
		BL_IM  = wr_input ( grpnum, BL_NAME, BL_OPEN )
		BLD_IM = wr_input ( grpnum, BLD_NAME, BLD_OPEN )
	    }
	    if ( DOBIAS ) {
		BI_IM  = wr_reffile ( groupnumber, BI_NAME, BI_OPEN )
		BD_IM  = wr_reffile ( groupnumber, BD_NAME, BD_OPEN )
	    }
	    if ( DOPREF ) {
		PI_IM  = wr_reffile ( groupnumber, PI_NAME, PI_OPEN )
		PD_IM  = wr_reffile ( groupnumber, PD_NAME, PD_OPEN )
	    }
	    if ( DOPURG ) {
		RI_IM  = wr_reffile ( groupnumber, RI_NAME, RI_OPEN )
		RD_IM  = wr_reffile ( groupnumber, RD_NAME, RD_OPEN )
	    }
	    if ( DODARK ) {
		DI_IM  = wr_reffile ( groupnumber, DI_NAME, DI_OPEN )
		DD_IM  = wr_reffile ( groupnumber, DD_NAME, DD_OPEN )
	    }
	    if ( DOFLAT ) {
		FI_IM  = wr_reffile ( groupnumber, FI_NAME, FI_OPEN )
		FD_IM  = wr_reffile ( groupnumber, FD_NAME, FD_OPEN )
	    }
	    if ( DOPHOT )
		PH_TAB  = wr_reftab ( PH_NAME, PH_OPEN )

#  Open output image (.c0h) file
	    OI_IM  = wr_output ( grpnum, OI_OUT, OI_OPEN, II_IM )

#  Set datatype of the output image (from input_image keyword)
	    IM_PIXTYPE ( OI_IM ) = OUTDTYPE

#  Open output DQF (.c1h) file
	    OD_IM = wr_output ( grpnum, OD_OUT, OD_OPEN, ID_IM )

#  Open output histogram (.c2h) file as TY_LONG image with three lines 
#  each having HISTLENGTH pixels
	    if ( DOHIST ) {
		H_IM  = wr_output ( grpnum, H_OUT, H_OPEN, II_IM )
		IM_LEN ( H_IM, 1) = HISTLENGTH
		IM_LEN ( H_IM, 2) = 3
		IM_PIXTYPE ( H_IM ) = TY_LONG
	    }

#  Open output saturated pixels (.c3h) file
	    if ( DOSAT )
		SD_IM = wr_output ( grpnum, SD_OUT, SD_OPEN, ID_IM )

#  Open group parameters text file
	    if ( DOGRP )
		G_FILE = wr_outfile ( grpnum, G_OUT, G_OPEN )


#  Zero statistics
	    MINVAL         =  MAX_REAL		# min value of all pixels
	    MAXVAL         = -MAX_REAL		# max value of all pixels
	    GOODMIN        =  MAX_REAL		# min good pixel value
	    GOODMAX        = -MAX_REAL		# max good pixel value
	    SUM            = 0.0		# sum of all good pixel values
	    N_GOODPIXEL    = 0			# number of GOODPIXEL pixels
	    N_SOFTERROR    = 0			# number of SOFTERROR pixels
	    N_CALIBDEFECT  = 0			# number of CALIBDEFECT pixels
	    N_STATICDEFECT = 0			# num of STATICDDEFECT pixels
	    N_ATODSAT      = 0			# number of ATODSAT pixels
	    N_DATALOST     = 0			# number of DATALOST pixels
	    N_BADPIXEL     = 0			# number of BADPIXEL pixels

#  Do the actual processing for this element
	    call wr_dochip ()

#  Update action and statistics keywords
	    c0flag = true			# print missing phot params
						# only for 1st case
	    call wr_updatekw ( OI_IM, c0flag, grpnum )

	    c0flag = false			# omit redundent messages
	    call wr_updatekw ( OD_IM, c0flag, grpnum )
	    if ( DOHIST )
		call wr_updatekw ( H_IM, c0flag, grpnum )
	    if ( DOSAT )
		call wr_updatekw ( SD_IM, c0flag, grpnum )

	    if ( DOGRP ) {			# write group params textfile
		call wr_group1 ( OI_IM, grpnum, G_FILE )
		call wr_group2 ( OI_IM, grpnum, G_FILE )
	    }
#  Close all open files
	    call wr_finishup ()
        }
end

#################################################################################
# WR_DOCHIP --	This performs the calculations for one CCD detector.  		#

procedure wr_dochip ( )

include "wrincl.h"

real	row[FULL_NX]		# Working image row
short	rowd[FULL_NX]		# Working dqf row
short	satrow[FULL_NX]		# Working saturated row
short	stemp[FULL_NX]		# temporary short row
long	ltemp[FULL_NX]		# temporary long row
real	rtemp[FULL_NX]		# temporary real row
real	atod[MAXATODLENGTH]	# AtoD conversion array
real	ctefix[FULL_NX]		# Serial Register CTE fixup array
long	hist1[MAXHISTLENGTH]	# histogram 1 (input data)
long	hist2[MAXHISTLENGTH]	# histogram 2 (post AtoD fix)
long	hist3[MAXHISTLENGTH]	# histogram 3 (output data)

int	i,j			# loop index (row number)
pointer	buf
long	v_ini[IM_MAXDIM]
long	v_ind[IM_MAXDIM]
long	v_outi[IM_MAXDIM]
long	v_outd[IM_MAXDIM]
int	status

int	impl2s(), imgl2s(), imgl2r(), impl2l()
int	imgnls(), impnls(), impnlr()
int	andi(), ori()

errchk	wr_getatod, wr_blev, wr_hist, wr_sat, wr_atod, wr_stat,
	    wr_error, imgl2s, imgl2r, impl2s, impl2l, 
	    amovr, amovs, amovl, abors, achtsr, achtrl, achtrs, aclrl, 
	    asubkr, asubr, amulkr, amulr, aaddkr

begin
	#  Load AtoD conversion array
	if ( DOATOD )
	    call wr_getatod ( atod )

	#  Load Serial Register CTE fix array if PTIME = 0 and DOPREF true
	if ( DOPREF && PTIME <= 0 )
	    call amovr ( Memr[imgl2r(PI_IM,1)], ctefix, KNX )

#  Get bias level from extracted engineering data file
#	Average bias level is placed in the global variable DEZERO; 
#	Bias level for even columns is placed in BIASEVEN
#	Bias level for odd columns is placed in BIASODD
	if ( DOBLEV ) 			
	    call wr_blev ( atod )

#  set the keywords to zero if BLEVCORR = no.  JC Hsu 7/9/92
	else {
	    DEZERO = 0.
	    BIASEVEN = 0.
	    BIASODD = 0.
	}

#  Zero histograms
	if ( DOHIST ) {
	    call aclrl ( hist1, HISTLENGTH )
	    call aclrl ( hist2, HISTLENGTH )
	    call aclrl ( hist3, HISTLENGTH )
	}

	call amovkl (long(1), v_ini, IM_MAXDIM)
	call amovkl (long(1), v_ind, IM_MAXDIM)
	call amovkl (long(1), v_outi, IM_MAXDIM)
	call amovkl (long(1), v_outd, IM_MAXDIM)

#  Process image line by line
	do j = 1, KNY {	
	    status = imgnls (II_IM, buf, v_ini)
	    call amovs (Mems[buf], stemp, KNX ) 
	    status = imgnls (ID_IM, buf, v_ind)
	    call amovs (Mems[buf], rowd, KNX ) 
	    #call amovs ( Mems[imgl2s(II_IM, j)], stemp, KNX ) # input image
	    #call amovs ( Mems[imgl2s(ID_IM, j)], rowd, KNX )  # input DQF

	    if ( DOMASK )
		call abors ( rowd, Mems[imgl2s(MD_IM,j)], rowd, KNX )

#  Clip input data to within reasonable limits and flag any pixel not already 
#  identified as fill values
	    do i = 1, KNX {
		if ( stemp[i] < MINALLOWED || stemp[i] > MAXALLOWED ) {
		    stemp[i] = 0
		    if (andi(int(rowd[i]), DATALOST) == 0)
			rowd[i] = short(ori(int(rowd[i]), BADPIXEL))
		}
	    }
	    if ( DOHIST ) {				# Histogram 1
		call achtsr ( stemp, row, KNX )
		call wr_hist ( row, rowd, hist1 )
	    }
	    call wr_sat ( stemp, rowd, satrow )	# Flag Saturated pixels
	    if  ( DOSAT ) 				# update sat map
		call amovs ( satrow, Mems[impl2s(SD_IM,j)], KNX )

	    if ( DOATOD )				# do AtoD conversion
		call wr_atod ( stemp, row, atod ) # image is now REAL
	    else
		call achtsr ( stemp, row, KNX )	# or copy buf to real

	    if ( DOHIST )				# Histogram #2
		call wr_hist ( row, rowd, hist2 )

#  Subtract bias level
#  Note: algorithm changed 10/91 to accomodate different bias levels in odd/even
#  columns -- RAShaw
#	    if ( DOBLEV )
#		call asubkr ( row, DEZERO, row, KNX )
	    if ( DOBLEV ) {
		do i = 1, KNX {
		    if ((i/2)*2 == i)
			row[i] = row[i] - BIASEVEN
		    else
			row[i] = row[i] - BIASODD
		}
	    }

	    if ( DOBIAS ) {				# subtract Bias
		call asubr ( row, Memr[imgl2r(BI_IM,j)], row, KNX )
		call abors ( rowd, Mems[imgl2s(BD_IM,j)], rowd, KNX )
	    }
	    if ( DOPREF ) {				# scale & sub Preflash
		if ( PTIME > 0 ) {			# subtract image
		    call amovr ( Memr[imgl2r(PI_IM, j)], rtemp, KNX )
		    call amulkr( rtemp, PTIME, rtemp, KNX )
		    call asubr ( row, rtemp, row, KNX )
		    call abors ( rowd, Mems[imgl2s(PD_IM,j)], rowd, KNX )
		} else				# Serial Reg CTE fix
		    call asubr ( row, ctefix, row, KNX )
	    }
	    if ( DOPURG ) {				# scale and sub Purge
		call amovr ( Memr[imgl2r(RI_IM, j)], rtemp, KNX )
		call amulkr( rtemp, PURGESCALE, rtemp, KNX )
		call asubr ( row, rtemp, row, KNX )
		call abors ( rowd, Mems[imgl2s(RD_IM,j)], rowd, KNX )
	    }
	    if ( DODARK ) {				# scale & subtract Dark
		call amovr ( Memr[imgl2r(DI_IM, j)], rtemp, KNX )
		call amulkr( rtemp, DTIME, rtemp, KNX )
		call asubr ( row, rtemp, row, KNX )
		call abors ( rowd, Mems[imgl2s(DD_IM,j)], rowd, KNX )
	    }
	    if ( DOFLAT ) {				# Multiply by Flat
		call amulr ( row, Memr[imgl2r(FI_IM,j)], row, KNX )
		call abors ( rowd, Mems[imgl2s(FD_IM,j)], rowd, KNX )
	    }
	    if ( DOHIST )				# Histogram #3
		call wr_hist ( row, rowd, hist3)

	    do i = 1, KNX {				# clean output image
		if ( andi ( int(rowd[i]), BADPIXEL ) != 0 || 
		     andi ( int(rowd[i]), DATALOST ) != 0 )
			row[i] = RSDPFILL
	    }
	    call wr_stat ( row, rowd )		# Accumulate Statistics

	    switch ( OUTDTYPE ) {
		case TY_REAL :	
	    	    status = impnlr (OI_IM, buf, v_outi)
	    	    call amovr (row, Memr[buf], KNX ) 
		    #call amovr ( row, Memr[impl2r(OI_IM,j)], KNX )
		case TY_LONG : {
		    call amulkr ( row, WSCALE, row, KNX )
		    call aaddkr ( row, WZERO, row, KNX )
		    call achtrl ( row, ltemp, KNX )
		    call amovl ( ltemp, Meml[impl2l(OI_IM,j)], KNX )
		}
		case TY_SHORT : {
		    call amulkr ( row, WSCALE, row, KNX )
		    call aaddkr ( row, WZERO, row, KNX )
		    call achtrs ( row, stemp, KNX )
		    call amovs ( stemp, Mems[impl2s(OI_IM,j)], KNX )
		}
		default :
		    call wr_error ( "output datatype unknown" )
	    }
	    status = impnls (OD_IM, buf, v_outd)
	    call amovs (rowd, Mems[buf], KNX ) 
	    #call amovs ( rowd, Mems[impl2s(OD_IM,j)], KNX )	# write DQF
	}
	if ( DOHIST ) {
	    call amovl ( hist1, Meml[impl2l(H_IM, 1)], HISTLENGTH )
	    call amovl ( hist2, Meml[impl2l(H_IM, 2)], HISTLENGTH )
	    call amovl ( hist3, Meml[impl2l(H_IM, 3)], HISTLENGTH )
	}
end

#################################################################################
# WR_HIST --	Adds the data in a line into a histogram.  			#

procedure wr_hist ( row, rowd, hist )

include "wrincl.h"

real	row[ARB]		# Input data
short	rowd[ARB]		# Input dqf
long	hist[ARB]		# Histogram
long	i, ival

begin
	do i = 1, KNX {
	    if ( rowd[i] == GOODPIXEL ) {
		ival = int(row[i]) + 1

		if ( ival < 1 )
		    ival = 1
		else if ( ival > HISTLENGTH )
		    ival = HISTLENGTH

		hist[ival] = hist[ival] + 1
	    }
	}
end

#################################################################################
# WR_GETATOD -- Get appropriate A-to-D lookup table from the reference file. 
#		The reference files is structured as a real image in which the 
#		first line is a sequence of temperatures in Kelvins whose 
#		element number corresponds to a line within the image.  The 
#		temperature closest to the BAY3TEMP extracted from the image 
#		header is used to select the desired AtoD lookup table.

procedure wr_getatod ( atod )

include "wrincl.h"

real	atod[ARB]		# AtoD conversion array
real	deltatemp		# temp difference between element and bay3temp
int	tline			# element closest to bay3temp
int	i			# loop index
int	imgl2r()

errchk	amovr, imgl2r, wr_error

begin
	#  Get first line of AtoD image -- this contains temperatures
	call amovr ( Memr[imgl2r(A_IM,1)], atod, ATODLENGTH )

	#  Search this line to find element closest to "bay3temp"
	deltatemp = MAX_REAL
	tline = 1
	do i = 1, ATODLENGTH {
	    if ( atod[i] > 0.0 && abs(atod[i] - BAY3TEMP)< deltatemp ) {
		deltatemp = abs ( atod[i] - BAY3TEMP)
		tline = i
	    }
	}
	if ( tline > IM_LEN ( A_IM, 2 ) )
	    call wr_error ("AtoD Table Line Selection Failed")

#  Get the actual AtoD conversion table from line "tline" in ref file
	call amovr ( Memr[imgl2r(A_IM,tline)], atod, ATODLENGTH )

end

#################################################################################
# WR_ATOD --	Correct data for the A-to-D converter problem.  This 		#
#		correction require short integer input and returns real 	#
#		output obtained from the supplied lookup table.  		#

procedure wr_atod ( srow, row, atod )

include "wrincl.h"

short	srow[ARB]		# input row
real	row[ARB]		# output row
real	atod[ARB]		# AtoD conversion array
int	i			# loop index

begin
	do i = 1, KNX {
	    if ( srow[i] <= 0 )
		row[i] = atod[1]
	    else if ( srow[i] >= SATLIM )
		row[i] = atod[ATODLENGTH]
	    else
		row[i] = atod[srow[i]+1]
	}
end

#################################################################################
# WR_SAT --	Find saturated values in a data row. Make a dqf line of these 	#
#		and merge with an existing dqf line. ( Saturated means > the 	#
#		saturation limit.)						#

procedure wr_sat ( srow, rowd, satrow )

include "wrincl.h"

short	srow[ARB]		# Input data
short	rowd[ARB]		# Input dqf
short	satrow[ARB]		# Saturated dqf
int	i

errchk	abors

begin
	do i = 1, KNX {
	    if ( srow[i] >= SATLIM )
		satrow[i] = ATODSAT
	    else
		satrow[i] = GOODPIXEL
	}
	call abors ( rowd, satrow, rowd, KNX )
end

#################################################################################
# WR_BLEV --	Determine the global chip bias level (DEZERO), as well as the 	#
#		even- and odd-column bias level, from the extracted 		#
#		engineering data file.  All pixels flagged in the EED DQF are 	#
#		excluded from the determination. If DOATOD is true, apply the 	#
#		AtoD fixup to each pixel in full mode only. 			#
#										#
#		For MODE = FULL: dezero = mean of all pixels in columns 	#
#                               3 to 14 inclusive.  				#
#		For MODE = AREA: dezero = 2 times average of packed bytes in 	#
#				word 1 of odd numbered rows (excluding row 1).	#
#										#
#	10/91	RAShaw	Substituted code to compute bias for even and odd 	#
#			columns separately.  N.B.: BIASEVEN comes from odd 	#
#			columns in EED file, and vice versa.			#

procedure wr_blev ( atod )

include "wrincl.h"

#  Calling argument:
real	atod[ARB]		# AtoD lookup table

#  Local variables:
short	row[EEDSIZE]		# EED row buffer
short	rowd[EEDSIZE]		# DQF row buffer
short	areabuf[AREA_NY]	# buffer to store AREA mode packed bytes
int	i,j			# loop index
long	count			# no. good pixels in average
long	n_even, n_odd		# no. good pixels in even, odd columns

#  Functions used:
pointer	imgl2s()

errchk	amovs, imgl2s, wr_error, achtbs

begin
	DEZERO   = 0.0
	BIASEVEN = 0.0
	BIASODD  = 0.0
	n_even   = 0
	n_odd    = 0
	count    = 0

#  MODE = FULL case
	if ( DOFULL ) {	
#	    call amovs (Mems[imgs2s(BL_IM,FULLEEDSTART,FULLEEDSIZE,1,KNY)], 
#			row, FULLEEDSIZE)
	    do j = 1, KNY {
		call amovs (Mems[imgl2s(BL_IM,j)], row, FULLEEDSIZE)
		call amovs (Mems[imgl2s(BLD_IM,j)], rowd, FULLEEDSIZE)
		if ( DOATOD ) {
		    do i = FULLEEDSTART, FULLEEDSIZE {
#			if ( rowd[i] == GOODPIXEL ) {
#			    if ( row[i] <= 0 )
#				DEZERO = DEZERO + atod[1]
#			    else if ( row[i] >= SATLIM )
#				DEZERO = DEZERO + atod[ATODLENGTH]
#			    else
#				DEZERO = DEZERO + atod[row[i] + 1]
#			    count = count + 1
#			}
			if ((i/2)*2 == i) {
			    if ( rowd[i] == GOODPIXEL ) {
				if ( row[i] <= 0 )
				    BIASODD = BIASODD + atod[1]
				else if ( row[i] >= SATLIM )
				    BIASODD = BIASODD + atod[ATODLENGTH]
				else
				    BIASODD = BIASODD + atod[row[i] + 1]
				n_odd = n_odd + 1
			    }
			} else {
			    if ( rowd[i] == GOODPIXEL ) {
				if ( row[i] <= 0 )
				    BIASEVEN = BIASEVEN + atod[1]
				else if ( row[i] >= SATLIM )
				    BIASEVEN = BIASEVEN + atod[ATODLENGTH]
				else
				    BIASEVEN = BIASEVEN + atod[row[i] + 1]
				n_even = n_even + 1
			    }
			}
		    }
		} else {

#  AtoD correction case
		    do i = FULLEEDSTART, FULLEEDSIZE {
			if ( rowd[i] == GOODPIXEL ) {
#			    DEZERO = DEZERO + row[i]
#			    count = count + 1
			    if ((i/2)*2 == i) {
				BIASODD = BIASODD + row[i]
				n_odd = n_odd + 1
			    } else {
				BIASEVEN = BIASEVEN + row[i]
				n_even = n_even + 1
			    }
			}
		    }
		}
	    }

#  Compute mean value of good pixels
	    if ( n_even > 0 && n_odd > 0 ) {
		DEZERO   = (BIASEVEN + BIASODD) / (n_even + n_odd)
		BIASEVEN = BIASEVEN / n_even
		BIASODD  = BIASODD  / n_odd
#	    if ( count > 0 ) {
#		DEZERO   = DEZERO / count	
	    } else
#		call wr_error ("No good pixels in Extracted Eng File")
		call wr_error ("Insufficient good pixels in Extracted Eng File")
	} else {

#  MODE = AREA case
	    do j = 3, KNY, 2 {
		call amovs (Mems[imgl2s(BL_IM,j)], row, AREAEEDSIZE)
		call amovs (Mems[imgl2s(BLD_IM,j)], rowd, AREAEEDSIZE)

		if ( rowd[1] == GOODPIXEL ) {
		    count = count + 1
		    areabuf[count] = row[1]
		}
	    }
	    call achtbs ( areabuf, areabuf, count )	# unpack bytes to short

	    if ( count > 0 ) {
		do i = 1, count
		    DEZERO = DEZERO + areabuf[i]

		DEZERO   = (DEZERO / count) * 2.0	
		BIASEVEN = DEZERO
		BIASODD  = DEZERO
	    } else
		call wr_error ("No good pixels in Extracted Eng File")
	}
end

#################################################################################
# WR_STAT --	Updates running totals of all statistics.  			#

procedure wr_stat ( row, rowd )

include	"wrincl.h"

real	row[ARB]		# row of image data
short	rowd[ARB]		# dqf
int	i			# loop index

int	andi()

begin
	do i = 1, KNX {
	    if ( rowd[i] == GOODPIXEL ) {
		GOODMIN = min ( GOODMIN, row[i] ) # min good pixel
		GOODMAX = max ( GOODMAX, row[i] ) # max good pixel
		SUM = SUM + row[i]		# mean (good pixels)
		N_GOODPIXEL = N_GOODPIXEL + 1	# number good pixels
	    } else {					# count all bad pixels
		if ( andi (int(rowd[i]), SOFTERROR) != 0 )
		    N_SOFTERROR = N_SOFTERROR + 1
		if ( andi (int(rowd[i]), CALIBDEFECT) != 0 )
		    N_CALIBDEFECT = N_CALIBDEFECT + 1
		if ( andi (int(rowd[i]), STATICDEFECT) != 0 )
		    N_STATICDEFECT = N_STATICDEFECT + 1
		if ( andi (int(rowd[i]), ATODSAT) != 0 )
		    N_ATODSAT = N_ATODSAT + 1
		if ( andi (int(rowd[i]), DATALOST) != 0 )
		    N_DATALOST = N_DATALOST + 1
		if ( andi (int(rowd[i]), BADPIXEL) != 0 )
		    N_BADPIXEL = N_BADPIXEL + 1
	    }
	    MINVAL = min ( MINVAL, row[i] )		# min of all pixels
	    MAXVAL = max ( MAXVAL, row[i] )		# max of all pixels
	}
end


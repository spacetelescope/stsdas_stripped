include <imhdr.h>
include <imio.h>
include <error.h>
include "wrdata.h"

#################################################################################
#										#
# T_CALWFP -- Run the stage 1 Enhanced WFPC pipeline				#
#										#
# STScI WF/PC Stage I Pipeline for STSDAS and PODPS/RSDP			#
#										#
# John W. MacKenty STScI 10.12.88						#
# Version 1:	12.30.88							#
# Version 1a:	03.28.89							#
# Last Revised: 10.23.89 Conformance with STSDAS-PODPS				#
#	1.00	11.27.89 Protect against zero divide when wrong PURGTIME	#
#	1.01	10.05.90 Fix Datamin/Datamax Keywords; PHOTMODE keyword		#
#	1.10	06.02.91 Get photometry keywords from PHOTTAB table		#
#	1.11	06.14.91 Add output file rootname.grp for group parameters	#
#	1.12	07.19.91 Change .grp to .cgr; _1 to _A, etc.			#
#										#
# Last Revised:		by RAShaw, STSDAS Group					#
#	1.13	08.28.91 Set FILTNAM1 and FILTNAM1 strings="OPEN" when blank	#
#	1.2.0	10.31.91 Calculate, store, and apply separate BIAS corrections	#
#			 for even and odd-numbered rows.			#
#	1.2.0.1	01.07.92 Report missing BIASEVEN/ODD keywords & values to 	#
#			 STDOUT	& continue					#
#	1.2.0.2 01.30.92 Report MIR_REVR as "T|F" rather than "yes|no" in 	#
#				.CGR file.					#
#	1.2.0.3	07.09.92 Set DEZERO, BIASEVEN/ODD to 0 when BLEVCORR = no.
#	1.2.1  	10.28.92 Add "cal" to the end of photmode when flatfield is 
#			 applied, expand photmode to 48 characters.

procedure t_calwfp ()

char	inname[SZ_FNAME]				# name of input data
char	outname[SZ_FNAME]				# name of output data

begin
#	call ccwrpp ( )					# Set up processing
# 10/23/89 commented out -- no longer need by SOGS

        call clgstr ( "inname", inname, SZ_FNAME ) 	# Input rootname
        call clgstr ( "outname", outname, SZ_FNAME ) 	# Input rootname

	call calwfp ( inname, outname )		# Do the processing
end

#################################################################################
#										#
# CCWRPP --	Set up the stage 1 Enhanced WFPC pipeline.  			#
#		(Does nothing but used to be required by SOGS)			#

procedure ccwrpp ()

include "wrincl.h"

errchk	strcpy, wr_message

begin
	call strcpy ( "", ROOT, SZ_FNAME )
	call wr_message ( "WF/PC Calibration Initialization Starting" )
	call wr_message ( "WF/PC Calibration Initialization Ending" )
end


#################################################################################
#										#
#  CALWFP -- Processes a WFPC image in the RSDP pipeline.  			#
#										#
#  This program performs the AtoD correction, Bias Level determination and 	#
#  subtraction, Bias, Preflash (or CTE), SuperPurge Residual, and Dark image 	#
#  subtractions, and flat field correction. The input data quality file (DQF) 	#
#  is updated by the DQFs of each reference file together with a static mask 	#
#  DQF.  An additional output DQF flagging all saturated pixels is created.  	#
#  Histograms of the input data, the data after AtoD correction, and the 	#
#  output data are generated.  							#
#										#
#  This procedure requires the root names of the input and output datasets. 	#
#  A path name may be included but extensions are not allowed.  If the output 	#
#  name is null, it defaults to the input name.  The extensions ".d0h" and 	#
#  ".q0h" are assumed for the input image and its DQF.  Control of the program 	#
#  is accomplished by keywords in the header file of the input image.  All 	#
#  reference file names are provided in these keywords.  The output files 	#
#  names are derived from the output root name as follows:			#
#										#
#         root.c0h    -- output image						#
#         root.c1h    -- output DQF						#
#         root.c2h    -- output histograms (3 image rows per group)		#
#         root.c3h    -- output saturated pixel DQF				#
#         root.cgr    -- output text file of group params for DMF/DADS		#
#										#
#  The control keywords are: 							#
#	MASKCORR, ATODCORR, BLEVCORR, BIASCORR, PREFCORR, PURGCORR, 		#
#	DARKCORR, FLATCORR, DOSATMAP, DOHISTOS, DOPHOTOM, OUTDTYPE		#
#										#
#  The reference file names are specified in the keywords:			#
#	MASKFILE, ATODFILE, BLEVFILE, BLEVDFIL, BIASFILE, BIASDFIL, 		#
#	PREFFILE, PREFDFIL, PURGFILE, PURGDFIL, DARKFILE, DARKDFIL, 		#
#	FLATFILE, FLATDFIL, PHOTTAB						#
#										#
#  n.b. All constants and global variables placed in common blocks are 		#
#	expressed in upper case in the procedures.				#
#										#

procedure calwfp ( inname, outname )

include "wrincl.h"

char	inname[SZ_FNAME]	# Input rootname of the WF/PC dataset
char	outname[SZ_FNAME]	# Output rootname of the WF/PC dataset
int	strlen()

errchk  strcpy, strlen, wr_setup, wr_calc, wr_finishup, wr_message

begin

	# clear rootname global used in messages
	call strcpy ( "", ROOT, SZ_FNAME )

	# copy input rootname into global common
	call strcpy ( inname, IN_ROOT, SZ_FNAME )

	# print startup message
	call wr_message ("WF/PC Calibration Starting: CALWFP Version 1.2.1")

	# place output rootname into global common
	if ( strlen ( outname ) > 0 )
	    call strcpy ( outname, OUT_ROOT, SZ_FNAME )
	else
	    call strcpy ( inname, OUT_ROOT, SZ_FNAME )

#  Set up some starting values, get the input image, load some hardwired 
#  values, get "action" keywords, get ref and output file names.
	call wr_setup ( )

	call wr_calc  ( )		# Do the calculations

	call wr_finishup ( )		# Close any open files

	call wr_message ("WF/PC Calibration Ending for observation")

	call strcpy ( "", ROOT, SZ_FNAME )
end


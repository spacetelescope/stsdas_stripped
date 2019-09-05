#			File:	t_calwp2.x

include <imhdr.h>
include <imio.h>
include <fio.h>
#include "u_context.h"
include <error.h>
include "u_incl.h"
include "u_data.h"


###############################################################################
#  T_CALWP2 -- Processes a WFPC-2 image in the PODPS/RSDP pipeline.  		
#									
#  This program performs the AtoD correction, the bias level determination     
#  and subtraction, bias and dark image subtractions, flat field correction   
#  and shutter shading correction.  The input data quality file (DQF) is     
#  updated by the DQFs of each reference file together with a static mask DQF. 
#  See the STSDAS Calibration Guide for details.                              
#									
#  This procedure requires the root names of the input and output datasets. 
#  A path name may be included but extensions are not allowed.  If the output 
#  name is null, it defaults to the input name.  The extensions ".d0h" and 
#  ".q0h" are assumed for the input image and its DQF.  Control of the program 
#  is accomplished by keywords in the header file of the input image.  All 
#  reference file names are provided in these keywords.  The output files 
#  names are derived from the output root name as follows:		
#								
#         root.c0h    -- output image					
#         root.c1h    -- output DQF					
#         root.c2h    -- output histogram				
#         root.c3t    -- throughput table 				
#         root.cgr    -- output text file of group params for DMF/DADS
#									
#  The control keywords are: 						
#	MASKCORR, ATODCORR, BLEVCORR, BIASCORR, DARKCORR, FLATCORR, SHADCORR,  
#	DOPHOTOM, DOHISTOS, OUTDTYPE                       		
#									
#  The reference file names are specified in the keywords:		
#	MASKFILE, ATODFILE, BLEVFILE, BLEVDFIL, BIASFILE, BIASDFIL, DARKFILE,  
#	DARKDFIL, FLATFILE, FLATDFIL, SHADFILE, GRAPHTAB, COMPTAB	
#									
#  Note that all constants, macros, and structure variables are expressed in 
#  upper case in the procedures.  See the README file for additional infor-
#  mation.  								
#
#  History:
#	   Jul 92 RA Shaw 	initial coding based on "calwfp" by J. MacKenty
#	24 Jul 92 RA Shaw	Version 1.2.1.0: Initial implementation,	
#				V1.0 under STSDAS V1.2				
#	 6 Aug 92 JC Hsu	Initial execution-time debugging		
#	13 Jul 93 JC Hsu	Version 1.2.1.1: Change WFII to WFPC2		
#	10 Sep 93 CYZhang	Version 1.3.0.0:				
#	  Add AtoD correction (OPR25362); Shutter shading correction (OPR25110);
#	  Generate histograms (OPR25364); Throughput table and the four		
#	  photometric quantities calculated from GRAPHTAB and COMPTAB 		
#	  (OPR 24645); Compute statistics for various sections (OPR25162);	
#	  and Fix up a few bugs including the wrongly populated DATAMIN		
#	  and DATAMAX for the DQF file and the histogram (.c1h and .c2h files)	
#	  (OPR 25256)								
#
#	15 Oct 93 CYZhang	Version 1.3.0.1:				
#	  Add more error checkings on the reference images; Separate image	
#	  statistics from major calibration processing;				
#	  Update the .DGR file; Add LRFWAVE to LRF FILTNAMs			
#
#	14 Dec 93 CYZhang	Version 1.3.0.1.1:				
#	  Fix "file attributes" problem of dgr files on VMS (OPR 25902);	
#	  Fix wrong FILETYPE of C1H file (OPR 25908);				
#	  Fix memory corruption problem in u_sec_stat (OPR 25909)		
#
#	01 Feb 94 CYZhang	Version 1.3.0.2:				
#	  Fine-tuning the code to improve the performance (OPR 26007);		
#	  Add function ABS to expression of mismatching of ATODGAINs (OPR 26120)
#	  Put history records for thruput files used in photometry (OPR 26121)	
#	  Fix the problem of missing the second line of error messages on VMS	
#	  machines (OPR 26124);						
#
#	18 Feb 94 CYZhang	Version 1.3.0.3:				
#		Remove the keyword, "DN", from the PHOTMODE string (OPR 26208);
#		Open c3t file with u_outtab (OPR 26234)			
#
#	22 Feb 94 CYZhang	Version 1.3.0.4:				
#		The global bias level is redefined by section [9:14,11:790] of	
#		the EED file (OPR 26230)					
#
#	 1 Mar 94 CYZhang	Version 1.3.0.5:
#		Trim down the memory used by u_sec_stat				
#
#	24 Apr 94 CYZhang	Version 1.3.0.6:
#		Accomodate even/odd bias levels (OPR 26622)
#
#	06 Dec 94 JC Hsu	Version 1.3.0.7 :
#		Check the PEDIGREE and DESCRIP keywords (OPR 26123)
#
#	31 Oct 95 JC Hsu	Version 1.3.0.8 :
#		Add reference file names in the trailer file (OPR 30118)
#
#	29 Jul 96 JC Hsu	Version 1.3.5.1 :
#		Add bias jump checking (OPR 31880)
#
#       15 Nov 96 JC Hsu     	Version 1.3.5.2:
#               Fix a bug that single WF chip observations are using PC1's
#               reference files, OPR 32629
#
#       13 Oct 2000 H Shukla    Version 2.1:
#		Modify to run with FITS extension files               
#
#       22 Nov 2000 JC Hsu    	Version 2.1.2:
#		Initialize all c3t table columns at the beginning
#               Generate proper photmode with LRF filters
#               Generate proper FILENAME for FITS output
#
#       14 May 2002 JT Miller   Version 2.1.3
#               Changed u_phot.x nwave & version
#
#       04 Mar 2002 JC Hsu    	Version 2.2.0:
#		Modify u_fio.x (u_reffile) so it can have (extension) FITS
#		reference files
#        
#       24 May 2007 WJ Hack     Version 2.3.0:
#       Added WF4 correction step.
#
#       13 Nov 2007 WJ Hack     Version 2.4.0:
#       Added computation of contamination-correction zero-point
#
#       29 Nov 2007 WJ Hack     Version 2.4.1:
#       Corrected implementation of WF4 correction
#       
#       19 Dec 2007 WJ Hack     Version 2.4.2:
#       Turned on BLEV reference file usage when WF4TCORR is PERFORM
#       even if BLEVCORR is OMIT. Also allows for blank WF4TFILE as well as N/A
#
#       4 Feb 2008 WJ Hack     Version 2.4.3:
#       Corrected a problem with photmodes which are represented as all zeroes
#       in the synphot throughput files when computing the delta magnitude for 
#       time-dependent sensitivity keywords.
#
#       24 Aug 2009 WJ Hack    Version 2.5.5:
#       Initializates of zp_corr keyword value.
#
procedure t_calwp2 ()

#  Local variables:
pointer	act				# ACTion data structure
pointer	cam				# CAMera data structure
pointer	nam				# reference file NAMes structure
pointer	kw				# KeyWord data structure
pointer	ptr				# image descriptor data structure
pointer	stat		 		# Section STATistics structure -- CYZ
pointer gpix            # good pixel structure for all chips -- WJH
char	tmpname[SZ_FNAME]		# name of input data
char    wp2id[SZ_LINE]         # full version ID for CALWP2

#  Functions used:
int	strlen()
errchk  u_setup, u_calc, u_message

begin

    
	# Print startup message
    
    call sprintf(wp2id, SZ_LINE, "WFPC2 Calibration Starting: CALWP2 Version %s")
    call pargstr(VERSION)
    call u_message(wp2id)
    
    
	# Initialize data structures 
	call malloc (act, LEN_ACT, TY_STRUCT)	      # ACTion keywords
	call malloc (cam, LEN_CAM, TY_STRUCT)	      # CAMera parameters
	call malloc (kw,  LEN_KW,  TY_STRUCT)	      # statistics KeyWords
	call malloc (nam, LEN_NAM,   TY_CHAR)	      # reference file NAMes
	call malloc (ptr, LEN_PTR, TY_STRUCT)	      # reference file PoinTeRs
	call malloc (stat, LEN_STAT, TY_STRUCT)	      # Section statistics -CYZ
    call malloc (gpix, LEN_GPIX, TY_STRUCT)       # Goodpix for chips - WJH

	call u_isFits(nam)
	
	# Fetch output rootname; if blank, default to input rootname 
	call clgstr ("outname", tmpname, SZ_FNAME)

	if (strlen (tmpname) > 0)
	    call strcpy (tmpname, OUT_ROOT(nam), SZ_FNAME)
	else
	    call strcpy (IN_ROOT(nam), OUT_ROOT(nam), SZ_FNAME)

	# Set up some starting values, get the input image, load some hardwired
	# values, get "action" keywords, get ref and output file names.
	call u_setup (act, cam, nam, ptr)

	# Do the calculations
	call u_calc  (act, cam, kw, nam, ptr, stat, gpix)

	# Release memory
	call mfree (act,  TY_STRUCT)
	call mfree (cam,  TY_STRUCT)
	call mfree (kw,   TY_STRUCT)
	call mfree (nam,    TY_CHAR)
	call mfree (ptr,  TY_STRUCT)
	call mfree (stat, TY_STRUCT)
	call mfree (gpix, TY_STRUCT)

	# Print ending message
	call u_message ("WFPC2 Calibration Completed")
end

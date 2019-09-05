#			File:	u_proc.x

include <imhdr.h>
include	<tbset.h>
include <imio.h>
include <mach.h>
include <error.h>
include "u_incl.h"
include "u_data.h"

define	SZ_PHOT_PATH	1000

################################################################################
#										
#  U_CALC --	Main processing routine.  This processes all groups in the 	
#		image and handles the file management. The actual processing 	
#		of each group is done by u_dochip.  				
#										
#  History:								
#	20 Aug 92 by RAShaw	Initial implementation				
#
#	27 Aug 93 by CYZhang	Add AtoDCORR, SHADCORR, DOHISTOS, 		
#	 	New statistics required for various sections; New photmode	
#		stuff; Fixing up a few bugs (e.g., updating DATAMIN, DADAMAX)	
#
#	7  Oct 93 by CYZhang	Add updating .DGR file to accomodate		
#		some of keywords only computed for calibrated images;		
#		Separate major calibration steps from the image statistics	
#		and header keywords updating part.				
#
#	6  Dec 93 by CYZhang	Use IN_DQF as template for opening OUT_DQF	
#
#	20 Jan 94 by CYZhang   	Move u_stat back to u_dochip; Write 		
#		thruput table filenames into HISTORY; Add ABS for the		
#		ATODGAIN mismatching checking					
#
#	18 Feb 94 by CYZhang	Version 1.3.0.3:
#		Remove the keyword, "DN", from the PHOTMODE string (OPR 26208);	
#		Open c3t file with u_outtab (OPR pending)			
#
#	22 Feb 94 by CYZhang	Version 1.3.0.4:
#		The global bias level is redefined by section [9:14,11:790] of	
#		the EED file (OPR 26230)					
#
#	25 Apr 94 by CYZhang	Version 1.3.0.6:
#		Accomodate different bias levels in odd/even columns
# 		(OPR 26622)							
#
#	06 Dec 94 by JC Hsu	Version 1.3.0.7:
#		Add keyword PEDIGREE checking 
#
#	29 Jul 96 by JC Hsu	Version 1.3.5.1:
#		Add checking bias jump, OPR 31880
#
#	15 Nov 96 by JC Hsu	Version 1.3.5.2:
#		Fix a bug that single WF chip observations are using PC1's 
#		reference files, OPR 32629
#
#	22 Nov 2000 by JC Hsu	
#		populate FILENAME properly for both _c0f.fits and _c1f.fits
#
#   3 April 2007 by WJ Hack
#       add WF4 correction processing step

procedure u_calc (act, cam, kw, nam, ptr, stat, gpix)

#  Calling arguments:
pointer	act			# pointer to ACTion structure
pointer	cam			# pointer to CAMera structure
pointer	kw			# pointer to KeyWord structure
pointer	nam			# pointer to file NAMe structure
pointer	ptr			# PoinTeR to image descriptors
pointer	stat			# Pointer to section STATistics 
pointer	gpix			# Pointer to GoodPIXel values for each chip 

#  Local variables:
int	grpnum			# Current group element number
bool	c0flag			# Pass to U_UPDATEKW to limit phot error 
				# messages
char	text[SZ_LINE]		# Buffer for message construction
char	obuf[SZ_LINE]		# Buffer for message construction
real	dmin1, dmax1, dmin2, dmax2, dmin3, dmax3
char	phot_path[SZ_PHOT_PATH]
char 	fitsName2[SZ_FNAME]#, fitsName3[SZ_FNAME]
pointer	im
int	dummy

#  Functions used:
int	imgeti ()		# get header keyword value
real imgetr ()      # get header keyword value
pointer	u_outtab() 		# map a new table file
pointer	u_input ()		# map input image for specified group
pointer	u_reffile ()		# map reference image for specified group
pointer u_output ()		# map output image for specified group
int	envgets()		# Get environment variable
pointer gf_map()
int	gf_gcount()

errchk	imgeti, u_dochip, u_error, u_finishup, u_input, envgets, u_outtab,
    	u_in_gskip, u_kwerr, u_message, u_output, u_out_gskip, u_photcalc,
        u_contcalc, u_photmode2,
    	u_photmode, u_reffile, u_ref_gskip, imgeti, tbtopn, tbtcre, u_kwstat, 
	strcpy(), imastr

begin

	# Initialize image descriptors
	MASK_P(ptr)     = NULL
	ATOD_IMG_P(ptr) = NULL
	WF4T_IMG_P(ptr) = NULL
	SHAD_IMG_P(ptr) = NULL
	BLVL_IMG_P(ptr) = NULL
	BLVL_DQF_P(ptr) = NULL
	BIAS_IMG_P(ptr) = NULL
	BIAS_DQF_P(ptr) = NULL
	DARK_IMG_P(ptr) = NULL
	DARK_DQF_P(ptr) = NULL
	FLAT_IMG_P(ptr) = NULL
	FLAT_DQF_P(ptr) = NULL
	THRU_TBL_P(ptr) = NULL
	HIST_IMG_P(ptr) = NULL
	OUT_IMG_P(ptr)  = NULL
	OUT_DQF_P(ptr)  = NULL	
	IN_DGR_P(ptr)   = NULL
	OUT_GRP_P(ptr)  = NULL
	TEMP_P(ptr)     = NULL

	# Open table file with u_outtab -- 7/2/94 CYZhang
	if ( DOPHOT(act) ) {
	    THRU_TBL_P(ptr) = u_outtab (THRU_TBL(nam))
	    call tbtcre (THRU_TBL_P(ptr))
	}

	# Open output image (root.c0h) file, output DQF (.c1h) file, and 
	# set datatype of the output image
	grpnum = 1
	OUT_IMG_P(ptr) = u_output (grpnum, OUT_IMG(nam), IN_IMG_P(ptr))
	IM_PIXTYPE (OUT_IMG_P(ptr)) = OUTDTYPE(act)

	# Using IN_DQF as template for OUT_DQF -- CYZhang 1/12/93
	OUT_DQF_P(ptr) = u_output (grpnum, OUT_DQF(nam), IN_DQF_P(ptr))
	IM_PIXTYPE (OUT_DQF_P(ptr)) = TY_SHORT

	# Open output histogram (.c2h) file as TY_LONG image with three lines
	# each having HISTLENGTH pixels, copied from JCHsu by CYZ 2/9/93
	if ( DOHIST(act) ) {

	    HIST_IMG_P(ptr) = u_output (grpnum, HIST_IMG(nam), IN_IMG_P(ptr))
	    IM_PIXTYPE (HIST_IMG_P(ptr)) = TY_LONG
	    IM_LEN (HIST_IMG_P(ptr), 1) = HISTLENGTH
	    IM_LEN (HIST_IMG_P(ptr), 2) = 3
	}

    # Update the header with the CALWP2 Version ID
	iferr ( call gf_iastr ( OUT_IMG_P(ptr), "CAL_VER", VERSION ) ) 
	call u_error ( "Error in updating CAL_VER keyword" )

	# Open reference files and check the PEDIGREE keyword. 12/6/94 JCH
	if (DOMASK(act)) {
	    MASK_P(ptr) = u_reffile (cam, MASK(nam), REF_MASK)
	    call u_pedigree (MASK_P(ptr), OUT_IMG_P(ptr), MASK(nam), 
				DOMASK(act), "MASKFILE", "MASKCORR")
	}
	if (DOATOD(act)) {
	    ATOD_IMG_P(ptr) = u_reffile (cam, ATOD_IMG(nam), REF_ATOD)
	    call u_pedigree (ATOD_IMG_P(ptr), OUT_IMG_P(ptr), ATOD_IMG(nam), 
				DOATOD(act), "ATODFILE", "ATODCORR")
	}
	if (DOWF4T(act)) {
	    WF4T_IMG_P(ptr) = u_reffile (cam, WF4T_IMG(nam), REF_WF4T)
	    call u_pedigree (WF4T_IMG_P(ptr), OUT_IMG_P(ptr), WF4T_IMG(nam), 
				DOWF4T(act), "WF4TFILE", "WF4TCORR")
	    iferr ( BIASLVL(kw) = imgetr (WF4T_IMG_P(ptr), "WF4TBDQ") ) {
		call u_kwerr ("WF4TBDQ") 
        }
        
	}
	if (DOBIAS(act)) {
	    BIAS_IMG_P(ptr) = u_reffile (cam, BIAS_IMG(nam), REF_BIAS)
	    call u_pedigree (BIAS_IMG_P(ptr), OUT_IMG_P(ptr), BIAS_IMG(nam), 
				DOBIAS(act), "BIASFILE", "BIASCORR")
	}
	if (DODARK(act)) {
	    DARK_IMG_P(ptr) = u_reffile (cam, DARK_IMG(nam), REF_DARK)
	    call u_pedigree (DARK_IMG_P(ptr), OUT_IMG_P(ptr), DARK_IMG(nam), 
				DODARK(act), "DARKFILE", "DARKCORR")
	}
	if (DOFLAT(act)) {
	    FLAT_IMG_P(ptr) = u_reffile (cam, FLAT_IMG(nam), REF_FLAT)
	    call u_pedigree (FLAT_IMG_P(ptr), OUT_IMG_P(ptr), FLAT_IMG(nam), 
				DOFLAT(act), "FLATFILE", "FLATCORR")
	}
	if (DOSHAD(act)) {
	    SHAD_IMG_P(ptr) = u_reffile (cam, SHAD_IMG(nam), REF_SHAD)
	    call u_pedigree (SHAD_IMG_P(ptr), OUT_IMG_P(ptr), SHAD_IMG(nam), 
				DOSHAD(act), "SHADFILE", "SHADCORR")
	}

	###############Hemant(8/25/00)

	call strcpy (BLVL_IMG(nam), fitsName2, SZ_FNAME)
	call strcpy (IN_ROOT(nam), BLVL_IMG(nam), SZ_FNAME)
	call strcat ("_x0f.fits", BLVL_IMG(nam), SZ_FNAME)
	
  	# Check if the FITS file is there.
	iferr (im = gf_map (BLVL_IMG(nam), READ_ONLY, 0)) {
	    call sprintf (text, SZ_LINE, "Error opening input -- %s" )
	    	call pargstr (BLVL_IMG(nam))
	    #call u_error (text) 	   

  	    # If FITS file is missing search for GEIS counterpart
  	    #call strcpy (fitsName2, BLVL_IMG(nam), SZ_FNAME)
	    call strcpy (IN_ROOT(nam), BLVL_IMG(nam), SZ_FNAME)
	    call strcat (".x0h", BLVL_IMG(nam), SZ_FNAME)
	    iferr (im = gf_map (BLVL_IMG(nam), READ_ONLY, 0)) {
		call sprintf (text, SZ_LINE, "Error opening input -- %s" )
	    	    call pargstr (BLVL_IMG(nam))
	    	call u_error (text)
	    }
	}
	   
	#call strcpy (BLVL_DQF(nam), fitsName3, SZ_FNAME)
	call strcpy (IN_ROOT(nam), BLVL_DQF(nam), SZ_FNAME)
	call strcat ("_q1f.fits", BLVL_DQF(nam), SZ_FNAME)

        # Check if the FITS file is there.
	iferr (im = gf_map (BLVL_DQF(nam), READ_ONLY, 0)) {	  

  	    # If FITS file is missing search for GEIS counterpart
  	    #call strcpy (fitsName3, BLVL_DQF(nam), SZ_FNAME)
	    call strcpy (IN_ROOT(nam), BLVL_DQF(nam), SZ_FNAME)
	    call strcat (".q1h", BLVL_DQF(nam), SZ_FNAME) 
	    iferr (im = gf_map (BLVL_DQF(nam), READ_ONLY, 0)) {
		call sprintf (text, SZ_LINE, "Error opening input %s" )
	    	call pargstr (BLVL_DQF(nam))
	    	call u_error (text)
	    }
	}	   

	# Loop over each group
	if (ISFITS(nam) == 'T') {
            iferr ( NGROUP(cam) = gf_gcount (im))
                call u_kwerr ( "NEXTEND" )
        }
	
	do grpnum = 1, NGROUP(cam) {	
	    call sprintf (text, SZ_LINE, "Starting processing of element %d")
		call pargi (grpnum)
	 	call u_message (text)

	    # Open input image element and DQF (already open for element 1)
	    if (grpnum > 1) {
		call u_in_gskip (grpnum, IN_IMG_P(ptr))
		call u_in_gskip (grpnum, IN_DQF_P(ptr))
	    }

	    # Fetch detector number for correct reference file group 
	    # number, then check for valid value.  
	    iferr ( DETECTOR(cam) = imgeti (IN_IMG_P(ptr), "DETECTOR") )
		call u_kwerr ("DETECTOR")

	    if (DETECTOR(cam) < 1 || DETECTOR(cam) > 4) {
		call sprintf (text, SZ_LINE,
		"DETECTOR parameter in element %d out of bounds")
		    call pargi (grpnum)
		    call u_error (text)
	    }

	    # Open necessary reference files (to group DETECTOR)
	    # NOTE: assume all reference files have four groups
	    if (DOMASK(act)) {
		call u_ref_gskip (cam, MASK_P(ptr))
	    }

	    #  Open ATODFILE -- CYZhang  28/8/93
  	    if (DOATOD(act)) {
		call u_ref_gskip (cam, ATOD_IMG_P(ptr))
	    } 

	    #  Open WF4TFILE -- WJHack  3 April 2007
  	    if (DOWF4T(act)) {
		call u_ref_gskip (cam, WF4T_IMG_P(ptr))
	    } 

	    if (DOBLEV(act) || DOWF4T(act)) {
		if (grpnum == 1) {
		    BLVL_IMG_P(ptr) = u_input (grpnum, BLVL_IMG(nam))
		    BLVL_DQF_P(ptr) = u_input (grpnum, BLVL_DQF(nam))
		} else {
		    call u_in_gskip (grpnum, BLVL_IMG_P(ptr))
		    call u_in_gskip (grpnum, BLVL_DQF_P(ptr))
		}
	    } 

	    if (DOBIAS(act)) {
		if (grpnum == 1) {
		    BIAS_DQF_P(ptr) = u_reffile (cam, BIAS_DQF(nam), REF_BIAS)
		} 
		call u_ref_gskip (cam, BIAS_IMG_P(ptr))
		call u_ref_gskip (cam, BIAS_DQF_P(ptr))
	    } 

	    if (DODARK(act)) {
		if (grpnum == 1) {
		    DARK_DQF_P(ptr) = u_reffile (cam, DARK_DQF(nam), REF_DARK)
		}
		call u_ref_gskip (cam, DARK_IMG_P(ptr))
		call u_ref_gskip (cam, DARK_DQF_P(ptr))
	    } 

	    if (DOFLAT(act)) {
		if (grpnum == 1) {
		    FLAT_DQF_P(ptr) = u_reffile (cam, FLAT_DQF(nam), REF_FLAT)
		}
		call u_ref_gskip (cam, FLAT_IMG_P(ptr))
		call u_ref_gskip (cam, FLAT_DQF_P(ptr))
	    } 

	    #  Open SHADFILE  --  CYZhang  28/8/93
	    if (DOSHAD(act)) {
		if (grpnum == 1) {
		    iferr (SHADTIME(cam) = imgeti (SHAD_IMG_P(ptr), "EXPTIME"))
		        call u_error ("Shutter shading exposure time missing")
		}
		call u_ref_gskip (cam, SHAD_IMG_P(ptr))
	    } 

	    # Skip groups for output images -- CYZhang 18/1/94
	    call u_out_gskip (OUT_IMG_P(ptr), grpnum, dmin1, dmax1,
	    	      		IN_IMG_P(ptr))
	    call u_out_gskip (OUT_DQF_P(ptr), grpnum, dmin2, dmax2,
	    		      	IN_DQF_P(ptr))

	    if (DOHIST(act)) {

		    call u_out_gskip (HIST_IMG_P(ptr),
				  grpnum, dmin3, dmax3, IN_IMG_P(ptr))
            # Reset datasize for output histograms -- WJHack 27Aug09
    	    IM_PIXTYPE (HIST_IMG_P(ptr)) = TY_LONG
            IM_LEN (HIST_IMG_P(ptr), 1) = HISTLENGTH
	        IM_LEN (HIST_IMG_P(ptr), 2) = 3

	    }
	    
        # Set the GOODPIX register for the new chip/group
        call u_get_gpix(gpix, grpnum)
        
	    #  Initialize KW statistics
	    MINVAL(kw)     =  MAX_REAL		# min value of all pixels
	    MAXVAL(kw)     = -MAX_REAL		# max value of all pixels
	    DQFMIN(kw)     =  MAX_SHORT		# min value of DQF
	    DQFMAX(kw)     = -MAX_SHORT		# max value of DQF
	    HISTMIN(kw)    =  MAX_LONG		# min value of DQF
	    HISTMAX(kw)    = -MAX_LONG		# max value of DQF
	    GOODMIN(kw)    =  MAX_REAL		# min good pixel value
	    GOODMAX(kw)    = -MAX_REAL		# max good pixel value
	    SUM(kw)         = 0.D0		# sum of all good pixel values
	    DATAMEAN(kw)    = -MAX_REAL
	    N_GOODPIXEL(kw) = 0			# number of GOODPIXEL pixels
	    N_GOODPIX(gpix) = 0			# number of GOODPIXEL pixels
	    N_SOFTERROR(kw) = 0			# number of SOFTERROR pixels
	    N_CALDEFECT(kw) = 0			# number of CALIBDEFECT pixels
	    N_STATICDEF(kw) = 0			# num of STATICDDEFECT pixels
	    N_SATURATE(kw)  = 0			# number of ATODSAT pixels
	    N_DATALOST(kw)  = 0			# number of DATALOST pixels
	    N_BADPIXEL(kw)  = 0			# number of BADPIXEL pixels
	    N_OVERLAP(kw)   = 0			# no. image OVERLAP pixels
	    DEZERO(kw)      = 0.0		# global bias level
	    BIASLVL(kw)     = 10.0		# low bias level threshold
	    BIASEVEN(kw)    = 0.0		# bias level: even columns
	    BIASODD(kw)     = 0.0		# bias level: odd columns
	    PHOTFLAM(kw)    = 0.0		# flux for unit DN
	    PHOTPLAM(kw)    = 0.0		# pivot wavelength
	    PHOTBW(kw)      = 0.0		# filter bandwidth
	    PHOTZPT(kw)     = 0.0		# magnitude zero point
        DELTAMAG(kw)    = 0.0       # zero-point correction 
	    call strcpy ("                                               ", 
                     PHOTMODE(kw), SZ_PHOT)	# synphot "mode" string
        #PHOTMODE(kw) = EOS
        
	    # check bias jump, OPR 31880, JC Hsu 7/29/96.
	    # only do this for full mode.
	    if (DOBLEV(act) && IS_FULL(cam)) {
	        call bjdet_do (BLVL_IMG_P(ptr), OUT_IMG_P(ptr), 
				0.09, 8, 7, 5, 14, IM_LEN(BLVL_IMG_P(ptr), 2), 
				"", dummy, NGROUP(cam), "calwp2")
	    }

	    #  Do the actual processing for this element
	    call u_dochip (act, cam, kw, ptr, gpix)

	    #  Construct the  PHOTMODE string
	    if (DOPHOT(act)) {
	    	iferr ( call u_photmode (cam, kw) )
		    call u_error ("Failed to construct PHOTMODE string")
	    	iferr ( call u_photmode2 (cam, kw) )
		    call u_error ("Failed to construct PHOTMODE2 string")

		#  Calculate photometry keywords and update in 
		#  output image headers
    	    	call u_photcalc (kw, cam, grpnum, THRU_TBL_P(ptr), 
				GRAPH_TBL(nam), COMP_TBL(nam), phot_path, 
				SZ_PHOT_PATH)
 		call gf_iputh (OUT_IMG_P(ptr), "HISTORY", phot_path)
                
                call u_contcalc (kw, cam, grpnum, THRU_TBL_P(ptr), 
				GRAPH_TBL(nam), COMP_TBL(nam), phot_path, 
				SZ_PHOT_PATH)
		#call gf_iputh (OUT_IMG_P(ptr), "HISTORY", phot_path)
	    }
	    c0flag = true

	    # Populate PHOTTAB with thruput filename only in PODPS environment
	    # How PHOTTAB should be used outside pipeline is open question
	    # -- CYZhang  16/11/93
	    
	    dmin1 = MINVAL (kw)
	    dmax1 = MAXVAL (kw)
	    IM_MIN (OUT_IMG_P(ptr)) = dmin1
	    IM_MAX (OUT_IMG_P(ptr)) = dmax1
	    call u_updtkw1 (OUT_IMG_P(ptr), act, kw, cam, nam, gpix, c0flag)

	    # Get DATAMIN and DATAMAX for DQF file-- CYZ 2/9/93 (see OPR 25256)
	    MINVAL(kw) = real (DQFMIN(kw))
	    MAXVAL(kw) = real (DQFMAX(kw))
	    dmin2 = MINVAL (kw)
	    dmax2 = MAXVAL (kw)
	    IM_MIN (OUT_DQF_P(ptr)) = dmin2
	    IM_MAX (OUT_DQF_P(ptr)) = dmax2
	    c0flag = false
	    call u_updtkw1 (OUT_DQF_P(ptr), act, kw, cam, nam, gpix, c0flag)

	    if (DOHIST(act)) {
		    # Get DATAMIN and DATAMAX for HIST file-- CYZ 2/9/93 
		    # (see OPR 25256)
		    MINVAL(kw) = real (HISTMIN(kw))
		    MAXVAL(kw) = real (HISTMAX(kw))
		    dmin3 = MINVAL (kw)
		    dmax3 = MAXVAL (kw)
		    IM_MIN (HIST_IMG_P(ptr)) = dmin3
		    IM_MAX (HIST_IMG_P(ptr)) = dmax3
		    call u_updtkw1 (HIST_IMG_P(ptr), act, kw, cam, nam, gpix, c0flag)
	    }

        # Record the GOODPIX value for this chip
        call u_set_gpix(gpix, grpnum)

	}

	# To force IM_MIN and IM_MAX updated, one must set up IM_LIMTIME
	# so that IM_LIMTIME is greater than IM_MTIME!
	IM_LIMTIME (OUT_IMG_P(ptr)) = IM_MTIME(OUT_IMG_P(ptr)) + 1
	IM_LIMTIME (OUT_DQF_P(ptr)) = IM_MTIME(OUT_DQF_P(ptr)) + 1
	if (DOHIST(act)) {
	    IM_LIMTIME (HIST_IMG_P(ptr))= IM_MTIME(HIST_IMG_P(ptr)) + 1
	}
	call u_finishup (ptr)
    

	# Update ACT, KW and STAT keywords in header, write to .CGR text file
	call u_kwstat (nam, ptr, cam, act, kw, stat, gpix)

	# Update .DGR file
	if (envgets (PODPS_ENVIRON, obuf, SZ_FNAME) >= 1) {
	    call u_updt_dgr (nam, ptr, cam)

	}
end

################################################################################
#										
#  U_DOCHIP --	Perform the calculations on one CCD detector.  			
#										
#  History:								
#	24 Jul 92 by RAShaw	Initial implementation				
#
#	 6 Aug 92 by JCHsu	Initial debugging			        
#
#	28 Aug 93 by CYZhang	Add AtoD, shutter shading correction, 		
#		histograms, section statistics;	Group No. must be in calling 	
#		arguments when the "pyramid shadow area" varies from chip to	
#		chip, and hard-wired in code!					
#
# 	18 Jan 94 by CYZhang	Move u_stat back here; call new version of	
#		u_sec_stat to see if it is faster; get PHOTMODE string and 	
#		throughput table file names into the history of the header; 	
#		Remove the keyword, "DN", from the PHOTMODE string (OPR 26208);	
#		Open c3t file with u_outtab (OPR 26234)				
#
#	22 Feb 94 by CYZhang	Version 1.3.0.4:				
#		The global bias level is determined from section [9:14,11:790]	
#		of the EED file (OPR 26230)					
#
#	20 Apr 94 by CYZhang	Version 1.3.0.6:				
#		The even/odd bias levela are subtracted separately (OPR 26622)	


procedure u_dochip (act, cam, kw, ptr, gpix)

#  Calling arguments:
pointer	act			# pointer to action structure
pointer	cam			# pointer to camera structure
pointer	kw			# pointer to keyword structure
pointer	ptr			# pointer to image descriptors
pointer	gpix	    # pointer to good pixel values

#  Local variables:
pointer	sbuf, rbuf, lbuf	# temp buffers, separated for diff types -- CYZ
pointer	dqf			# dqf row
int	i, j			# loop indexes 
real	atod[MAXATODLENGTH]     # AtoD conversion array
real	wf4t[MAXATODLENGTH]     # WF4T conversion array
pointer	row, rrow		# image row
pointer	rtemp			# temp work array
pointer	sp			# top of stack memory
int	status			# imgnlT EOF status
short	satrow[NX_FULL]		# Working saturated row
long	hist1[HISTLENGTH]	# histogram 1 (input image)
long	hist2[HISTLENGTH]	# histogram 2 (post AtoD fixup)
long	hist3[HISTLENGTH]	# histogram 3 (output image)
long	v_mask[IM_MAXDIM]	# offset to next image line: MASK image
long	v_ini[IM_MAXDIM]	# 			INPUT image
long	v_ind[IM_MAXDIM]	# 			INPUT DQF
long	v_outi[IM_MAXDIM]	# 			OUTPUT image
long	v_outd[IM_MAXDIM]	# 			OUTPUT DQF
long	v_biasi[IM_MAXDIM]	# 			BIAS image
long	v_darki[IM_MAXDIM]	# 			DARK image
long	v_flati[IM_MAXDIM]	# 			FLAT image
long	v_biasd[IM_MAXDIM]	# 			BIAS DQF
long	v_darkd[IM_MAXDIM]	# 			DARK DQF
long	v_flatd[IM_MAXDIM]	# 			FLAT DQF
long	v_shadi[IM_MAXDIM]	#			SHAD image
short   v_wf4td[NX_FULL]  #  array for low wf4 DQ values
char	text[SZ_LINE]
short	smin, smax
long	lmin, lmax
short   dqs

bool    wf4tflag
bool    wf4low

#  Functions used:
int	impnls(), impnlr(), impnll(), imgnls(), imgnlr(), impl2l()
short	alovs(), ahivs()
long	alovl(), ahivl()

errchk	u_getatod, u_blev, u_error, u_find_sat, u_atod, u_stat,
	u_wf4t, u_initbias, asubkr, asubr, amulkr, amulr, aaddkr,
	impnls, impnll, impnlr, imgnls

begin

	#  Allocate temp working array
	call smark (sp)
	call salloc (rtemp, NPTS(cam), TY_REAL)
	call salloc (rrow,  NPTS(cam), TY_REAL)

	#  Load AtoD conversion array --  CYZhang  28/8/93
	if ( DOATOD(act) )
	    call u_getatod ( ptr, cam, atod )

	# Get bias level from extracted engineering data file
	# Average bias level is placed in the global variable DEZERO; 
	# Bias level for even columns is placed in BIASEVEN
	# Bias level for odd columns is placed in BIASODD
    # Turn off WF4T correction for initial computation
    # However, we need to restore original setting for subsequent
    # processing steps.
    wf4low = false
	if ( DOWF4T(act)) {            
        wf4tflag = DOWF4T(act)
        DOWF4T(act) = false

        call u_initbias(kw)  
	    call u_blev (cam, act, kw, ptr, atod, wf4t)

        # Record the un-corrected bias level values in the image header
        call u_updtkw3 (OUT_IMG_P(ptr), kw)

        if (DETECTOR(cam) == 4){
            # Now, check to see whether these values trigger 
            #    the low bias level flags for the DQ array 
            if (BIASEVEN(kw) < BIASLVL(kw) || BIASODD(kw) < BIASLVL(kw)) {
                wf4low = true
	            dqs = LOWBLEV
	            call amovks(dqs, v_wf4td, NX_FULL)
            } 
        }
        DOWF4T(act) = wf4tflag
    }

	#  Load WF4T conversion array --  WJHack  3 April 2007
	if ( DOWF4T(act) && DETECTOR(cam) == 4 ) {
	    call u_getwf4t ( ptr, kw, wf4t )
    }

    # Recompute based on proper WF4T correction.
	# Get bias level from extracted engineering data file
	# Average bias level is placed in the global variable DEZERO; 
	# Bias level for even columns is placed in BIASEVEN
	# Bias level for odd columns is placed in BIASODD
	if ( DOBLEV(act) ) {	
        call u_initbias(kw)
	    call u_blev (cam, act, kw, ptr, atod, wf4t)
	# Set the keywords to zero if BLEVCORR = "OMIT".  CYZhang 1/9/93
	} else {
        call u_initbias(kw)
	}

	#  Zero histograms
	if (DOHIST(act)) {
	    call aclrl ( hist1, HISTLENGTH )
	    call aclrl ( hist2, HISTLENGTH )
	    call aclrl ( hist3, HISTLENGTH )
	}

	# Initialize image line offsets
	call amovkl (long(1), v_mask, IM_MAXDIM)
	call amovkl (long(1), v_ini, IM_MAXDIM)
	call amovkl (long(1), v_ind, IM_MAXDIM)
	call amovkl (long(1), v_outi, IM_MAXDIM)
	call amovkl (long(1), v_outd, IM_MAXDIM)
	call amovkl (long(1), v_biasi, IM_MAXDIM)
	call amovkl (long(1), v_biasd, IM_MAXDIM)
	call amovkl (long(1), v_darki, IM_MAXDIM)
	call amovkl (long(1), v_darkd, IM_MAXDIM)
	call amovkl (long(1), v_flati, IM_MAXDIM)
	call amovkl (long(1), v_flatd, IM_MAXDIM)
	call amovkl (long(1), v_shadi, IM_MAXDIM)

	# Process image line by line
	do j = 1, NPTS(cam) {	
	    status = imgnls (IN_IMG_P(ptr), row, v_ini)
	    status = imgnls (IN_DQF_P(ptr), dqf, v_ind)

	    if ( DOMASK(act) ) {
		status = imgnls (MASK_P(ptr), sbuf, v_mask)
		call abors (Mems[dqf], Mems[sbuf], Mems[dqf], NPTS(cam))
	    }
        
	    # Clip input data to within reasonable limits & flag any pixel 
	    # not already identified as fill values, and flag saturated pixels
	    # call u_find_sat (Mems[row], Mems[dqf], NPTS(cam))
	    # First clipping the out-of-bounds pixels, then do histogarm
	    # Flagging saturated pixels is now separated and put after first
	    # histogram generated  -- CYZ 12 Oct 93
	    call u_clip (Mems[row], Mems[dqf], NPTS(cam))

	    # Coerce row to TY_REAL
	    # call achtsr (Mems[row], Memr[rrow], NPTS(cam))
	    do i = 0, NPTS(cam)-1
		Memr[rrow+i] = Mems[row+i]

	    # Do the 1st histogram  -- CYZ 1/9/93
	    if ( DOHIST(act) )
		call u_hist (cam, Memr[rrow], Mems[dqf], hist1)

	    # Flag saturated pixels -- CYZ 12 Oct 93
	    call u_sat (cam, Mems[row], Mems[dqf], satrow)

	    # Do map of saturated pixels here??  -- CYZhang

	    # Do AtoD conversion correction & datatype becomes real 
	    # -- CYZhang  28/8/93
	    if ( DOATOD(act) )
		call u_atod ( cam, Mems[row], Memr[rrow], atod )

	    # Do WF4T correction 
	    # -- WJHack  3 April 2007
	    if ( DOWF4T(act) && DETECTOR(cam) == 4)
		call u_wf4t ( cam, Memr[rrow], wf4t )

	    # Do the 2nd histogram
	    if ( DOHIST(act) )
		call u_hist (cam, Memr[rrow], Mems[dqf], hist2)

	    # Subtract bias level.  Note that algorithm could accomodate 
	    # different bias levels in odd/even columns
#	    if ( DOBLEV(act) )  
#		call asubkr (Memr[rrow], DEZERO(kw), Memr[rrow], NPTS(cam))

	    # Accomodate different bias levels in odd/even columns
	    # CYZhang 18/4/94 -- OPR 26622
            if ( DOBLEV(act) ) {
                do i = 0, NPTS(cam)-1 {
                    if ((i/2)*2 == i)
                        Memr[rrow+i] = Memr[rrow+i] - BIASODD(kw)
                    else
                        Memr[rrow+i] = Memr[rrow+i] - BIASEVEN(kw)

                }
            }
	    
	    # Subtract Bias image
	    if ( DOBIAS(act) ) {
		status = imgnlr (BIAS_IMG_P(ptr), rbuf, v_biasi)
		call asubr (Memr[rrow], Memr[rbuf], Memr[rrow], NPTS(cam))
		status = imgnls (BIAS_DQF_P(ptr), sbuf, v_biasd)
		call abors (Mems[dqf], Mems[sbuf], Mems[dqf], NPTS(cam))
	    }

	    # Scale & subtract Dark
	    if ( DODARK(act) ) {
		status = imgnlr (DARK_IMG_P(ptr), rbuf, v_darki)
		call amulkr (Memr[rbuf], DARKTIME(cam), Memr[rtemp], NPTS(cam))
		call asubr (Memr[rrow], Memr[rtemp], Memr[rrow], NPTS(cam))
		status = imgnls (DARK_DQF_P(ptr), sbuf, v_darkd)
		call abors (Mems[dqf], Mems[sbuf], Mems[dqf], NPTS(cam))
	    }

	    # Multiply by Flat
	    if ( DOFLAT(act) ) {
		status = imgnlr (FLAT_IMG_P(ptr), rbuf, v_flati)
		call amulr (Memr[rrow], Memr[rbuf], Memr[rrow], NPTS(cam))
		status = imgnls (FLAT_DQF_P(ptr), sbuf, v_flatd)
		call abors (Mems[dqf], Mems[sbuf], Mems[dqf], NPTS(cam))
	    }

	    # Correcting for shutter shading  --  CYZhang  28/8/93
	    if ( DOSHAD(act) ) {
		status = imgnlr (SHAD_IMG_P(ptr), rbuf, v_shadi)
		call amulkr (Memr[rbuf], SHADTIME(cam), Memr[rtemp], NPTS(cam))
		if ( EXPTIME(cam) <= EPSILONR ) {
		    call sprintf (text, SZ_LINE,
		        "EXPTIME must greater than 0.0 sec")
		    call u_error (text)
		}
		call adivkr (Memr[rtemp], EXPTIME(cam), Memr[rtemp], NPTS(cam))
		call aaddkr (Memr[rtemp], 1.0, Memr[rtemp], NPTS(cam))
		call amulr (Memr[rrow], Memr[rtemp], Memr[rrow], NPTS(cam))
	    }
		
	    # Do the 3rd histogram  -- CYZhang
	    if ( DOHIST(act) )
		call u_hist (cam, Memr[rrow], Mems[dqf], hist3)

	    # Replace BADPIXELs and LOST data in output image with RSDPFILL 
	    # --CYZ 14/10/93
	    call u_fill (Memr[rrow], mems[dqf], NPTS(cam), RSDPFILL(cam))

        # Update Output DQF file with flags for low bias level 
        if ( DETECTOR(cam) == 4 && wf4low) {
	        call abors (Mems[dqf], v_wf4td, Mems[dqf], NPTS(cam))
        } 

	    # accumulate statistics
	    call u_stat (kw, Memr[rrow], Mems[dqf], NPTS(cam), gpix)

	    switch (OUTDTYPE(act)) {
		case TY_REAL : {
		    status = impnlr (OUT_IMG_P(ptr), rbuf, v_outi)
		    call amovr (Memr[rrow], Memr[rbuf], NPTS(cam))
		}

		case TY_LONG : {
		    call amulkr (Memr[rrow], USCALE(cam), Memr[rrow], NPTS(cam))
		    call aaddkr (Memr[rrow], UZERO(cam), Memr[rrow], NPTS(cam))
		    status = impnll (OUT_IMG_P(ptr), lbuf, v_outi)
		    call achtrl (Memr[rrow], Meml[lbuf], NPTS(cam))
		}

		case TY_SHORT : {
		    call amulkr (Memr[rrow], USCALE(cam), Memr[rrow], NPTS(cam))
		    call aaddkr (Memr[rrow], UZERO(cam), Memr[rrow], NPTS(cam))
		    status = impnls (OUT_IMG_P(ptr), sbuf, v_outi)
		    call achtrs (Memr[rrow], Mems[sbuf], NPTS(cam))
		}

		default :
		    call u_error ("Invalid output datatype")
	    }

	    # Get min and max from DQF line -- CYZ 2/9/93 (see OPR 25256)
	    smin = alovs (Mems[dqf], NPTS(cam))
	    smax = ahivs (Mems[dqf], NPTS(cam))
	    if (smin < DQFMIN(kw))
		DQFMIN(kw) = smin
	    if (smax > DQFMAX(kw))
		DQFMAX(kw) = smax
	    
	    # Write DQF line
	    status = impnls (OUT_DQF_P(ptr), sbuf, v_outd)
	    call amovs (Mems[dqf], Mems[sbuf], NPTS(cam))
	}

	#  Get the DATAMEAN -- CYZhang  30/9/93
	# Test to see if no good pixels at all -- CYZhang  16/11/93
	#if (N_GOODPIXEL(kw) <= 0) {
	if (N_GOODPIX(gpix) <= 0) {
	    DATAMEAN(kw) = -MAX_REAL
	    MINVAL(kw) = -MAX_REAL
	} else {
	    DATAMEAN(kw) = real (SUM(kw) / N_GOODPIX(gpix))		
	    #DATAMEAN(kw) = real (SUM(kw) / N_GOODPIXEL(kw))		
	}

	if ( DOHIST(act) ) {
	    lmin = alovl (hist1, HISTLENGTH)
	    lmax = ahivl (hist1, HISTLENGTH)
	    do i = 1, HISTLENGTH {
		if (lmin > hist2[i])
		    lmin = hist2[i]
		if (lmax < hist2[i])
		    lmax = hist2[i]
	    }
	    do i = 1, HISTLENGTH {
		if (lmin > hist3[i])
		    lmin = hist3[i]
		if (lmax < hist3[i])
		    lmax = hist3[i]
	    }
	    HISTMIN(kw) = lmin
	    HISTMAX(kw) = lmax
	    call amovl ( hist1, Meml[impl2l(HIST_IMG_P(ptr), 1)], HISTLENGTH )
	    call amovl ( hist2, Meml[impl2l(HIST_IMG_P(ptr), 2)], HISTLENGTH )
	    call amovl ( hist3, Meml[impl2l(HIST_IMG_P(ptr), 3)], HISTLENGTH )
	}

	# Release memory and flush output buffers
	call sfree (sp)
#	call imflush (OUT_IMG_P(ptr))
#	call imflush (OUT_DQF_P(ptr))
#	call imflush (HIST_IMG_P(ptr))

end

################################################################################
# U_HIST --	Adds the data in a line into a histogram.  			
#
#  History:
#	28 Aug 93	by CYZhang	Initial implementation
#					Based on J. Mackenty's code for WFPC

procedure u_hist ( cam, row, rowd, hist )

# Calling arguments
pointer	cam			# Camera specifics structure
real	row[ARB]		# Input data
short	rowd[ARB]		# Input dqf
long	hist[ARB]		# Histogram

# Local variables
long	i, ival

begin
	do i = 1, NPTS(cam) {
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

################################################################################
# U_INITBIAS -- Initialize the BIAS level keyword values.
#		
#
#  History:
# 	03/4/07	  by WJHack		Initial implementation
#					

procedure u_initbias (kw) 

pointer	kw			# pointer to keyword structure

begin

	DEZERO(kw) = 0.
	BIASEVEN(kw) = 0.
	BIASODD(kw) =0.

end

################################################################################
# U_GETWF4T -- Get appropriate WF4 correction table from the reference file.
#		The reference files is structured as a real image in which  
#		each line corresponds to an integer BIASEVEN value for the image.   
#
#  History:
# 	03/4/07	  by WJHack		Initial implementation
#					

procedure u_getwf4t ( ptr, kw, wf4t )

# Calling arguments
pointer	ptr			# Pointer to file pointer structure
pointer	kw			# Pointer to KeyWord structure
real	wf4t[ARB]		# WF4T conversion array

# Local variables
real    diff[MAXATODLENGTH] # difference array
real    fract       # fractional portion of BIASEVEN value
int     rownum
int     maxbias

# Function used
int	imgl2r()

errchk	amovr, imgl2r, u_error, u_warn

begin
    
    rownum = int(BIASEVEN(kw)+1)
    maxbias = IM_LEN ( WF4T_IMG_P(ptr), 2 )
    
	if ( rownum >= maxbias ) {
	    call u_warn ("BIASEVEN too large for WF4TFILE.")
        call u_warn ("Using maximum bias value for WF4TCORR correction.")
        rownum = maxbias

	    #  Get line of WF4T image which corresponds to BIASEVEN value
	    call amovr ( Memr[imgl2r(WF4T_IMG_P(ptr), rownum)], wf4t, MAXATODLENGTH)
        
    } else {

	    #  Get line of WF4T image which corresponds to BIASEVEN value
	    call amovr ( Memr[imgl2r(WF4T_IMG_P(ptr), rownum)], wf4t, MAXATODLENGTH)
    
        # Now get the difference with the next line to use for interpolation
        call asubr (Memr[imgl2r(WF4T_IMG_P(ptr), rownum+1)], wf4t, diff, MAXATODLENGTH)

        # Now, compute fractional difference based on real value of BIASEVEN
        fract = BIASEVEN(kw) - int(BIASEVEN(kw))
        call amulkr(diff, fract, diff, MAXATODLENGTH)

        # Update output values by adding in fractional difference
        call aaddr(wf4t, diff, wf4t, MAXATODLENGTH)
    }
end

################################################################################
# U_WF4T --	Correct data for the WF4 readout problem.  This 		
#		correction returns real output.  The input value is 
#       used to interpolate between the integer values represented 
#       by the columns in the reference table row.  That input
#       value will then get multiplied by the interpolated reference 
#       value.  A dummy reference file will contain all ones so that
#       the input value does not get modified at all.
#
#  History:
#	3April07   by WJHack		Initial implementation
#					

procedure u_wf4t ( cam, row, wf4t )

# Calling arguments
pointer	cam			# Pointer to Camera specifics
real	row[ARB]		# output row
real	wf4t[ARB]		# WF4T conversion array

# Local variables
int	i			# loop index
real fract      # fractional value of pixel
real diff       # difference wf4t value and next higher value
real wf4corr

begin
	do i = 1, NPTS(cam) {
        fract = row[i] - int(row[i])

	    if ( row[i] <= 0 )
		    row[i] = wf4t[1] * row[i]
	    else if ( row[i] >= SATLIM(cam))
		    row[i] = wf4t[MAXATODLENGTH] * row[i]

	    else {
            diff = wf4t[int(row[i])+1] - wf4t[int(row[i])]
            wf4corr = (wf4t[int(row[i])] + fract*diff)

		    row[i] = wf4corr * row[i]
        }
	}
end


################################################################################
# U_GETATOD -- Get appropriate A-to-D lookup table from the reference file.
#		The reference files is structured as a real image in which the 
#		first line is a sequence of temperatures in Kelvins whose 
#		element number corresponds to a line within the image.  The 
#		temperature closest to the UBAY3TMP extracted from the image 
#		header is used to select the desired AtoD lookup table.
#
#  History:
# 	28/8/93	  by CYZhang		Initial implementation
#					Baed on J. Mackenty's code for WFPC

procedure u_getatod ( ptr, cam, atod )

# Calling arguments
pointer	ptr			# Pointer to file pointer structure
pointer	cam			# Pointer to Camera specifics structure
real	atod[ARB]		# AtoD conversion array

# Local variables
real	deltatemp		# temp difference between element and ubay3tmp
int	tline			# element closest to ubay3tmp
int	i			# loop index

# Function used
int	imgl2r()

errchk	amovr, imgl2r, u_error

begin

	#  Get first line of AtoD image -- this contains temperatures
	call amovr ( Memr[imgl2r(ATOD_IMG_P(ptr), 1)], atod, ATODLENGTH(cam) )

	#  Search this line to find element closest to "ubay3tmp"
	deltatemp = MAX_REAL
	tline = 1
	do i = 1, ATODLENGTH(cam) {
	    if ( atod[i] > 0.0 && abs(atod[i] - UBAY3TMP(cam))< deltatemp ) {
		deltatemp = abs ( atod[i] - UBAY3TMP(cam) )
		tline = i
	    }
	}
	if ( tline > IM_LEN ( ATOD_IMG_P(ptr), 2 ) )
	    call u_error ("AtoD Table Line Selection Failed")

	#  Get the actual AtoD conversion table from line "tline" in ref file
	call amovr ( Memr[imgl2r(ATOD_IMG_P(ptr),tline)],atod, ATODLENGTH(cam))
end

################################################################################
# U_ATOD --	Correct data for the A-to-D converter problem.  This 		
#		correction require short integer input and returns real 	
#		output obtained from the supplied lookup table.  		
#
#  History:
#	28/8/93   by CYZhang		Initial implementation
#					Based on J. Mackenty's code for WFPC1

procedure u_atod ( cam, srow, row, atod )

# Calling arguments
pointer	cam			# Pointer to Camera specifics
short	srow[ARB]		# input row
real	row[ARB]		# output row
real	atod[ARB]		# AtoD conversion array

# Local variables
int	i			# loop index

begin
	do i = 1, NPTS(cam) {
	    if ( srow[i] <= 0 )
		row[i] = atod[1]
	    else if ( srow[i] >= SATLIM(cam) )
		row[i] = atod[ATODLENGTH(cam)]
	    else
		row[i] = atod[srow[i]+1]
	}
end

################################################################################
#										
# U_FIND_SAT --	Find saturated values in a data row and update DQF line and 	
#		reset out-of-bounds values.  NB: Saturated means > saturation 	
#		limit.								
#										
#  History:								#
#	18 Aug 92 by RAShaw	Initial implementation				
#  	15 Sep 93 by CYZhang 	Modified

procedure u_find_sat (row, dqf, jpts)

#  Calling arguments:
short	row[ARB]		# input data
short	dqf[ARB]		# input DQF
int	jpts			# array size

#  Local variable:
int	i			# loop index

int	andi(), ori()

errchk andi, ori

begin

	# Reset out-of-bounds values
	do i = 1, jpts {
	    if (row[i] < short(MINALLOWED) || row[i] > short(MAXALLOWED)) {
		row[i] = 0
		if (andi (int(dqf[i]), short(DATALOST)) == 0)
		    dqf[i] = short(ori(int(dqf[i]), short(BADPIXEL)))

	    } else if (row[i] == short(MAXALLOWED)) 
		dqf[i] = dqf[i] + short(ATODSAT)
	}
end

################################################################################
#										
#  U_CLIP --	Clip out-of-bounds pixels in a data raw line and reset them 	
#		to zero. Update the DQF line					
#										
#  History:								
#	12 Oct 93 by CYZhang	Initial implementation				

procedure u_clip (row, dqf, jpts)

#  Calling arguments:
short	row[ARB]		# input data
short	dqf[ARB]		# input DQF
int	jpts			# array size

#  Local variable:
int	i			# loop index

int	andi(), ori()

errchk andi, ori

begin

	# Reset out-of-bounds values
	do i = 1, jpts {
	    if (row[i] < short(MINALLOWED) || row[i] > short(MAXALLOWED)) {
		row[i] = 0
		if (andi (int(dqf[i]), short(DATALOST)) == 0)
		    dqf[i] = short(ori(int(dqf[i]), short(BADPIXEL)))
	    }
	}
end

################################################################################
#
#  U_SAT --     Find saturated values in a data row. Make a dqf line of these  	
#               and merge with an existing dqf line. ( Saturated means > the	
#               saturation limit.)                                              
#
#  History:								#
#	12 Oct 93 by CYZhang	Initial implementation				
#	16 Nov 93 by CYZhang	Use SATLIM(cam) which is taken from header	
#				keyword, SATURATE.				
 
procedure u_sat ( cam, row, dqf, satrow )

# Calling arguments
pointer	cam			# Camera specifics
short   row[ARB]               # Input data
short   dqf[ARB]               # Input dqf
short   satrow[ARB]             # Saturated dqf

# Local variables
int     i

errchk  abors

begin
        do i = 1, NPTS(cam) {
#           if ( row[i] >= short(MAXALLOWED) )
            if ( row[i] >= short(SATLIM(cam)) )		
                satrow[i] = ATODSAT
            else
		satrow[i] = GOODPIXEL
	}
        call abors ( dqf, satrow, dqf, NPTS(cam))
end


################################################################################
#										
#  U_BLEV --	Determine the global chip bias level (DEZERO), as well as the 	
#		even- and odd-column bias level, from the extracted 		
#		engineering data file.  All pixels flagged in the EED DQF are 	
#		excluded from the determination. 				
#										
#		For MODE = FULL: dezero = mean of all pixels in columns 	
#                               3 to 14 inclusive.  				
#		For MODE = AREA: dezero = 2 times average of packed bytes in 	
#				word 1 of odd numbered rows (excluding row 1).	
#										
#		NOTE: BIASEVEN comes from odd columns in EED file, and vice 	
#			versa.							
#										
#  History:								#
#	24 Jul 92 by RAShaw	Initial implementation				
#	 6 Aug 92 by JCHsu	Initial debugging				
#	 1 Sep 93 by CYZhang	Add AtoD correction                             
#	22 Feb 94 by CYZhang	Change [3:14,*] to [9:14,11:790] (OPR 26230)	

procedure u_blev (cam, act, kw, ptr, atod, wf4t)

#  Calling Arguments
pointer	kw			# pointer to KeyWord structure
pointer	ptr			# pointer to image descriptors

# Including cam in the calling argumets so that jpts and full are not needed
# They are components included in the structure "cam"
#int	jpts			# no. columns in image
pointer	cam			# Pointer to CAMera structure
pointer	act			# Pointer to ACTion structure
real	atod[ARB]		# AtoD lookup table
real	wf4t[MAXATODLENGTH]     # WF4T conversion array


#  Local variables:
pointer	buf			# temp work array 
pointer	col			# EED overscan buffer
pointer	rcol			# AtoD corrected (real)
pointer	dqf			# DQF buffer
int	i, j			# loop indexes
long	n_even, n_odd		# no. good pixels in even, odd columns
pointer	sp			# top of stack memory
int	nfull			# buffer size
long n_area
real biasarea
int  ny1_area, ny2_area

#  Functions used:
real	asumr()			# sum of vector elements
real	asums()			# sum of vector elements
pointer	imgl2s()		# fetch image line
pointer	imgs2s()		# General image section

errchk	u_error, imgs2s, imgl2s, asums, asumr, achtbs, amulr, amuls

begin

	n_even = 0
	n_odd  = 0

	# Add NY1_FULL, NY2_FULL to u_data.h and
	# Add local variable nfull -- CYZ 22/2/94
	nfull = NY2_FULL - NY1_FULL + 1
	
	call smark (sp)

	# Buffer size is now nfull instead of NPTS(cam) -- CYZ 22/2/94
	#	call salloc (buf, NPTS(cam), TY_SHORT)
	call salloc (buf, nfull, TY_SHORT)

	# Allocate memory for the real array to accomodate AtoD correction 
	# -- CYZ 10 Sep 93
#	call salloc (rcol, NPTS(cam), TY_REAL)
	call salloc (rcol, nfull, TY_REAL)
	
	#  MODE = FULL case
	if ( IS_FULL(cam) ) {
        
	    do j = FULL_START, FULL_END {
		col = imgs2s (BLVL_IMG_P(ptr), j, j, NY1_FULL, NY2_FULL)
		dqf = imgs2s (BLVL_DQF_P(ptr), j, j, NY1_FULL, NY2_FULL)

		# Construct a mask with values of 1 for GOODPIXELs, and 0 
		# otherwise.  Total of mask is no. of GOODPIXELs; multiply 
		# data by mask before accumulating sum.  
		# Note: abeqki doesn't work here because returned vector 
		# is always TY_INT

		# Change loop index range from 0 to NY_FULL-1 by CYZhang 1/9/93
		do i = 0, nfull-1 {
		    if (Mems[dqf+i] == short (GOODPIXEL)) 
			Mems[buf+i] = 1
		    else
			Mems[buf+i] = 0
		}

		# AtoD correction applied?  -- CYZhang  1/9/93
		if ( DOATOD(act) ) {
		    do i = 0, nfull-1 {
			if ( Mems[col+i] <= 0 )
			    Memr[rcol+i] = atod[1]
			else if ( Mems[col+i] >= SATLIM(cam) )
			    Memr[rcol+i] = atod[ATODLENGTH(cam)]
			else
			    Memr[rcol+i] = atod[Mems[col+i]+1]
		    }
                        
		    do i = 0, nfull-1
			Memr[rcol+i] = Memr[rcol+i] * Mems[buf+i]
		} else {
		    call amuls (Mems[col], Mems[buf], Mems[col], nfull)
            call achtsr (Mems[col],Memr[rcol], nfull)
        }
        
        # Perform WF4T Correction if needed
        if (DOWF4T(act) && DETECTOR(cam) == 4) {
		    call u_wf4t ( cam, Memr[rcol], wf4t )
        }

		# Even-numbered columns:
		if ((j/2)*2 == j) {
		    n_odd = n_odd + long (asums (Mems[buf], nfull))
			BIASODD(kw) = BIASODD(kw) + asumr (Memr[rcol], nfull)
		    #if ( DOATOD(act) )
			#   BIASODD(kw) = BIASODD(kw) + asumr (Memr[rcol], nfull)
		    #else
			#   BIASODD(kw) = BIASODD(kw) + asums (Mems[col], nfull)

		# Odd-numbered columns:
		} else {
		    n_even = n_even + long (asums (Mems[buf], nfull))
			BIASEVEN(kw) = BIASEVEN(kw) + asumr (Memr[rcol], nfull)
		    #if ( DOATOD(act) )
			#   BIASEVEN(kw) = BIASEVEN(kw) + asumr (Memr[rcol], nfull)
		    #else
			#   BIASEVEN(kw) = BIASEVEN(kw) + asums (Mems[col], nfull)
		}
	    }
        
	    # Compute mean value of good pixels
	    if (n_even > 0 && n_odd > 0) {
		DEZERO(kw)   = (BIASEVEN(kw) + BIASODD(kw)) / (n_even + n_odd)
		BIASEVEN(kw) = BIASEVEN(kw) / n_even
		BIASODD(kw)  = BIASODD(kw)  / n_odd
	    } else
		call u_error ("Insufficient good pixels in Extracted Eng file")

	# MODE = AREA case
	} else {
        ny1_area = 3
        ny2_area = NPTS(cam)
        nfull = ny2_area - ny1_area + 1
        n_area = 0
        biasarea = 0.0
        
	    do j = 3, FULL_END {
		col = imgs2s (BLVL_IMG_P(ptr), j, j, ny1_area, ny2_area)
		dqf = imgs2s (BLVL_DQF_P(ptr), j, j, ny1_area, ny2_area)
		# Change loop index range from 0 to NY_FULL-1 by CYZhang 1/9/93
		do i = 0, nfull-1 {
		    if (Mems[dqf+i] == short (GOODPIXEL)) { 
			   Mems[buf+i] = 1
		    } else {
			   Mems[buf+i] = 0
            }
		}

		# AtoD correction applied?  -- CYZhang  1/9/93
		if ( DOATOD(act) ) {
		    do i = 0, nfull-1 {
			if ( Mems[col+i] <= 0 )
			    Memr[rcol+i] = atod[1]
			else if ( Mems[col+i] >= SATLIM(cam) )
			    Memr[rcol+i] = atod[ATODLENGTH(cam)]
			else
			    Memr[rcol+i] = atod[Mems[col+i]+1]
		    }
                        
		    do i = 0, nfull-1
			Memr[rcol+i] = Memr[rcol+i] * Mems[buf+i]
		} else {
		    call amuls (Mems[col], Mems[buf], Mems[col], nfull)
            call achtsr (Mems[col],Memr[rcol], nfull)
        }
        
        # Perform WF4T Correction if needed
        if (DOWF4T(act) && DETECTOR(cam) == 4) {
		    call u_wf4t ( cam, Memr[rcol], wf4t )
        }

		n_area = n_area + long (asums (Mems[buf], nfull))
		biasarea = biasarea + asumr (Memr[rcol], nfull)

	    } # End loop over lines (j)
        

	    if (n_area > 0) {
		DEZERO(kw)   = (biasarea / n_area)
		BIASEVEN(kw) = DEZERO(kw)
		BIASODD(kw)  = DEZERO(kw)
	    } else
		call u_error ("No good pixels in Extracted Engineering file")
	}
	call sfree (sp)
end

################################################################################
#										
#  U_MNMXS --	Updates DATAMIN and DATAMAX of output DQF			
#										
#  History:								
#	14 Oct 93 by CYZhang	Initial implementation				

procedure u_mnmxs (kw, dqf, jpts)

#  Calling arguments:
pointer	kw			# pointer to KeyWord structure
short	dqf[jpts]		# row of image data
int	jpts			# size of arrays

#  Local variables:
int	i			# loop index
short	vmin			# min of all pixels
short	vmax			# max of all pixels

begin

	# Assign structure variables to local variables
	vmin = DQFMIN(kw)
	vmax = DQFMAX(kw)

	# Determine min/max of ALL pixels
	do i = 1, jpts {
	    vmin = min (vmin, dqf[i])
	    vmax = max (vmax, dqf[i])
	}

	# Reset structure variables to local values
	DQFMIN(kw)      = vmin
	DQFMAX(kw)      = vmax
end

################################################################################
#										
#  U_MNMXR --	Updates DATAMIN and DATAMAX of all pixels in output image	
#										
#  History:								
#	14 Oct 93 by CYZhang	Initial implementation				

procedure u_mnmxr (kw, row, jpts)

#  Calling arguments:
pointer	kw			# pointer to KeyWord structure
real	row[jpts]		# row of image data
int	jpts			# size of arrays

#  Local variables:
int	i			# loop index
real	vmin			# min of all pixels
real	vmax			# max of all pixels

begin

	# Assign structure variables to local variables
	vmin = MINVAL(kw)
	vmax = MAXVAL(kw)

	# Determine min/max of ALL pixels
	do i = 1, jpts {
	    vmin = min (vmin, row[i])
	    vmax = max (vmax, row[i])
	}

	# Reset structure variables to local values
	MINVAL(kw)      = vmin
	MAXVAL(kw)      = vmax
end

################################################################################
#										
#  U_FILL --	Insert fill values for cases DATALOST and BADPIXEL.  		
#										
#  History:								
#	14 Oct 93 by CYZHang	Initial implementation				

procedure u_fill (row, dqf, jpts, fill)

# Calling arguments:
real	row[jpts]		# row of image data
short	dqf[jpts]		# dqf
int	jpts			# size of arrays
int	fill			# RSDP fill value for DATALOST and BADPIXELs

# Local variables:
int	i			# loop index

int	andi()

begin
	do i = 1, jpts {
	    if (andi (int(dqf[i]), short(DATALOST)) != 0) {
		row[i] = fill
	    }
	    if (andi (int(dqf[i]), short(BADPIXEL)) != 0) {
		row[i] = fill
	    }
	}
end

################################################################################
#										
#  U_GET_GPIX -- Sets N_GOODPIX to the value for the current chip/group
#  History:								
#	24 May 07 by WHack   	Initial implementation				
#	

procedure u_get_gpix (gpix, grpnum)

# Calling arguments
pointer	gpix		    # Pointer to good pixel values
int	grpnum				# Current group number

# Local variables
int npix

begin 
    if (grpnum == 1)
        npix = GOODPIX1(gpix)
    else if (grpnum == 2)
        npix = GOODPIX2(gpix)
    else if (grpnum == 3)
        npix = GOODPIX3(gpix)
    else
        npix = GOODPIX3(gpix)
        
    N_GOODPIX(gpix) = npix

end

################################################################################
#										
#  U_SET_GPIX -- Records the current N_GOODPIX value in the GPIX entry 
#                for the current chip/group
#  History:								
#	24 May 07 by WHack   	Initial implementation				
#	

procedure u_set_gpix (gpix, grpnum)

# Calling arguments
pointer	gpix		    # Pointer to good pixel values
int	grpnum				# Current group number

begin 
    if (grpnum == 1)
       GOODPIX1(gpix) = N_GOODPIX(gpix)
    else if (grpnum == 2)
       GOODPIX2(gpix) = N_GOODPIX(gpix)
    else if (grpnum == 3)
       GOODPIX3(gpix) = N_GOODPIX(gpix)
    else
       GOODPIX4(gpix) = N_GOODPIX(gpix)
end


################################################################################
#										
#  U_KWSTAT --	Computing image statistics and updating image header keywords  	
#		Updating the .DGR file so as to include some statistics		
#		for calibrated image.						
#  History:								
#	 7 Oct 93 by CYZhang	Initial implementation				
#	16 Dec 93 by CYZhang 	Test if there is no good pixel at all		

procedure u_kwstat (nam, ptr, cam, act, kw, stat, gpix)

# Calling arguments
pointer	nam				# Pointer to file NAMes
pointer	ptr				# Pointer to images and files
pointer cam				# Pointer to Camera specifics
pointer act				# Pointer to ACTtions 
pointer	kw				# Pointer to kw structure
pointer	stat				# Pointer to image section statistics
pointer	gpix		    # Pointer to good pixel values

# Local variables
int	grpnum				# Current group number
char	text[SZ_LINE]			# Text of error message
bool	c0flag				# Flag passed to u_updatekw
#char	obuf[SZ_LINE]			# Buffer for message construction
real	dmin1, dmax1, dmin2, dmax2, dmin3, dmax3
char 	tempFileName[SZ_FNAME]
char    dir[SZ_FNAME]            # O:  Directory part of pathname.
char    root[SZ_FNAME]         	 # O:  Root part of pathname.
char    ext[SZ_FNAME]            # O:  Extension part of pathname.
int     cl_index                 # O:  The cluster index.
int     cl_size                  # O:  The cluster size.
char    section[SZ_FNAME]   	 # O:  The section part of pathname.
char    ksection[SZ_FNAME] 	 # O:  The remainder of the pathname.

####
pointer inIm, im, immap()  	# This is used at the end of this function.
char	errText[SZ_LINE], tempString[SZ_LINE], tempInFileName[SZ_FNAME]
####

# Functions used
pointer	gf_map()	
int	imgeti()
int	u_grpfile()

errchk strcpy, strcat, fparse, imgstr, u_kwerr, immap, u_error

begin

	# Construct output .CGR text filename
	call strcpy (OUT_ROOT(nam), OUT_GRP(nam), SZ_FNAME)
	call strcat (".cgr", OUT_GRP(nam), SZ_FNAME)

	HIST_IMG_P(ptr) = NULL
	OUT_IMG_P(ptr)  = NULL
	OUT_DQF_P(ptr)  = NULL	
	OUT_GRP_P(ptr)  = NULL

	OUT_GRP_P(ptr) = u_grpfile (1, OUT_GRP(nam))
	
	iferr (OUT_IMG_P(ptr) = gf_map (OUT_IMG(nam), READ_WRITE, 0)) {
	    call sprintf (text, SZ_LINE, "Error opening calibrated image: %s")
	    call pargstr (OUT_IMG(nam))
	    call u_error (text)
	}
	iferr (OUT_DQF_P(ptr) = gf_map (OUT_DQF(nam), READ_WRITE, 0)) {
	    call sprintf (text, SZ_LINE, "Error opening calibrated DQF: %s")
	    call pargstr (OUT_DQF(nam))
	    call u_error (text)
	}


	if (DOHIST(act)) {
	    iferr (HIST_IMG_P(ptr) = gf_map (HIST_IMG(nam), READ_WRITE, 0)) {
		call sprintf (text, SZ_LINE, "Error opening histogram file:%s")
		call pargstr (HIST_IMG(nam))
		call u_error (text)
	    }
	}

	do grpnum = 1, NGROUP(cam) {

	    call sprintf (text, SZ_LINE,
			  "Computing image statistics of element %d")
	    call pargi (grpnum)
	    call u_message (text)

	    dmin1 = IM_MIN(OUT_IMG_P(ptr))
	    dmax1 = IM_MAX(OUT_IMG_P(ptr))

	    dmin2 = IM_MIN(OUT_DQF_P(ptr))
	    dmax2 = IM_MAX(OUT_DQF_P(ptr))

	    if (DOHIST(act)) {
		dmin3 = IM_MIN(HIST_IMG_P(ptr))
		dmax3 = IM_MAX(HIST_IMG_P(ptr))
	    }

#	    if (grpnum > 1) {
		iferr (call gf_opengr (OUT_IMG_P(ptr), grpnum, dmin1, dmax1,
				   NULL)) {
		    call sprintf (text, SZ_LINE,
			      "Error skipping output image group: %1d")
		    call pargi (grpnum)
		    call u_error (text)
		}
		iferr (call gf_opengr (OUT_DQF_P(ptr), grpnum, dmin2, dmax2,
				   NULL)) {
		    call sprintf (text, SZ_LINE,
			      "Error skipping output DQF group: %1d")
		    call pargi (grpnum)
		    call u_error (text)
		}
		if (DOHIST(act)) {
		    iferr (call gf_opengr (HIST_IMG_P(ptr),
				       grpnum, dmin3, dmax3, NULL)) {
			call sprintf (text, SZ_LINE,
				  "Error skipping output histogram group: %1d")
			call pargi (grpnum)
			call u_error (text)
		    }
		}
#	    }
        
	    #  Fetch detector number from OUT_IMG, and check for valid value.  
	    iferr ( DETECTOR(cam) = imgeti (OUT_IMG_P(ptr), "DETECTOR") )
		call u_kwerr ("DETECTOR")

	    if (DETECTOR(cam) < 1 || DETECTOR(cam) > 4) {
		call sprintf (text, SZ_LINE,
		"DETECTOR parameter in element %d out of bounds")
		    call pargi (grpnum)
		    call u_error (text)
	    }

	    # Get Image section statistics - CYZ 5/10/93
	    # Test to see if no good pixels at all -- CYZhang 16/11/93
        call u_get_gpix(gpix, grpnum)
        
	    if (N_GOODPIX(gpix) <= 0) {
#	    if (N_GOODPIXEL(kw) <= 0) {
#		MINVAL(kw) = -MAX_REAL
		S_MEANC10(stat) = -MAX_REAL
		S_MEANC25(stat) = -MAX_REAL
		S_MEANC50(stat) = -MAX_REAL
		S_MEANC100(stat) = -MAX_REAL
		S_MEANC200(stat) = -MAX_REAL
		S_MEANC300(stat) = -MAX_REAL
		S_GMEDIAN(stat) = -MAX_REAL
		S_HISTWIDE(stat)= -MAX_REAL
		S_SKEW(stat) = -MAX_REAL
		S_BCKGRD(stat) = -MAX_REAL
		S_SMEDIAN(stat)= -MAX_REAL  
	    } else {
		call u_sec_stat (OUT_IMG_P(ptr), OUT_DQF_P(ptr), stat,
			     DETECTOR(cam), IS_FULL(cam))
	    }
        
	    #  Update image section statistics keywords...
	    c0flag = true
    	    call u_updtkw2 (OUT_IMG_P(ptr), kw, stat, c0flag)

	    #	...but omit redundant messages when updating DQF header
	    c0flag = false
	    call u_updtkw2 (OUT_DQF_P(ptr), kw, stat, c0flag)

	    if (DOHIST(act)) {
		call u_updtkw2 (HIST_IMG_P(ptr), kw, stat, c0flag)
	    }

	    # Write .cgr text files even when running outside pipeline!
#	    if (envgets (PODPS_ENVIRON, obuf, SZ_FNAME) >= 1) {
		call u_wrt_grp1 (OUT_IMG_P(ptr), grpnum, OUT_GRP_P(ptr))
		call u_wrt_grp2 (OUT_IMG_P(ptr), grpnum, OUT_GRP_P(ptr))
		call u_wrt_grp3 (OUT_IMG_P(ptr), grpnum, OUT_GRP_P(ptr))
#	    }
	}

	# Update/add FILENAME in the output file Hemant (Oct 11, 2000)	    
	if (ISFITS(nam) == 'T') {
	    call strcpy (OUT_IMG(nam), tempFileName, SZ_FNAME)
	    call fparse (tempFileName, dir, SZ_FNAME, root, SZ_FNAME, ext, 
				SZ_FNAME, cl_index, cl_size, section, 
				SZ_FNAME, ksection, SZ_FNAME)
	
	    call strcat (".fits", root, SZ_FNAME)
	    call gf_iastr (OUT_IMG_P(ptr), "FILENAME", root)

	    # do the same to the data quality file
	    call strcpy (OUT_DQF(nam), tempFileName, SZ_FNAME)
	    call fparse (tempFileName, dir, SZ_FNAME, root, SZ_FNAME, ext, 
				SZ_FNAME, cl_index, cl_size, section, 
				SZ_FNAME, ksection, SZ_FNAME)
	
	    call strcat (".fits", root, SZ_FNAME)
	    call gf_iastr (OUT_DQF_P(ptr), "FILENAME", root)
	}

	if (OUT_IMG_P(ptr) != NULL)  			# Close output image
	    call u_gf_unmap (OUT_IMG_P(ptr))
	if (OUT_DQF_P(ptr) != NULL)			# Close output DQF
	    call u_gf_unmap (OUT_DQF_P(ptr))	
	if (HIST_IMG_P(ptr) != NULL && DOHIST(act))	# Close output HIST
	    call u_gf_unmap (HIST_IMG_P(ptr))	
	if (OUT_GRP_P(ptr) != NULL)                 	# Close .CGR file
	    call u_close   (OUT_GRP_P(ptr))

	########
	# Added in order to update/add the keyword ROOTNAME in primary header of
	# the output file - Hemant (Oct. 13, 2000)	
	
	if (ISFITS(nam) == 'T') {
	
	    # open the input file and extract the ROOTNAME keyword in a string
	    call strcpy (IN_IMG(nam), tempInFileName, SZ_FNAME)
	    call strcat ("[0]", tempInFileName, SZ_FNAME)
		
	    iferr (inIm = immap (tempInFileName, READ_ONLY, NULL)) {
            	call sprintf (errText, SZ_LINE, "Error opening input %s" )
            	    call pargstr (tempInFileName)
            	call u_error (errText)
       	    }
	    iferr (call imgstr (inIm, "ROOTNAME", tempString, SZ_LINE))
		call u_kwerr ("ROOTNAME")
	
	    call strcpy (OUT_IMG(nam), tempFileName, SZ_FNAME)

	    # Add [0] to the filename
	    call strcat ("[0]", tempFileName, SZ_FNAME)

	    # Open the file with immap and add the keyword ROOTNAME
	    im = immap (tempFileName, READ_WRITE, NULL)
            call imastr (im, "ROOTNAME", tempString)

	    # Close the file
	    call imunmap (im)
		
	    # Do the same for .c1f file as well
	    call strcpy (OUT_DQF(nam), tempFileName, SZ_FNAME)

	    # Add [0] to the filename
	    call strcat ("[0]", tempFileName, SZ_FNAME)

	    # Open the file with immap and add the keyword ROOTNAME
	    im = immap (tempFileName, READ_WRITE, NULL)
            call imastr (im, "ROOTNAME", tempString)

	    # Close the file
	    call imunmap (im)
	}

end

################################################################################
#										
#  U_STAT --	Updates running totals of some image statistics 	 	
#										
#  Revision History:								
#	20 Aug 92 by RAShaw	Initial implementation				
#	14 Oct 93 by CYZhang	Move fill values to u_fill.			

procedure u_stat (kw, row, dqf, jpts, gpix)

#  Calling arguments:
pointer	kw			# pointer to KeyWord structure
pointer	gpix			# pointer to GoodPIXel structure
real	row[jpts]		# row of image data
short	dqf[jpts]		# dqf
int	jpts			# size of arrays
#int	fill			# RSDP fill value for DATALOST and BADPIXELs

#  Local variables:
int	i			# loop index
double	accum			# running sum
int	a2dp			# no. of ATODSATurations
int	badp			# no. of generic BADPIXELs
int	cald			# no. of CALIBDEFECTs
real	gmin			# min of good pixels
real	gmax			# max of good pixels
int	good			# no. of GOODPIXELs
int	lost			# no. of DATALOST
int	over			# no. of OVERLAP
int	soft			# no. of SOFTERRORs
int	stat			# no. of STATICDEFECTs
real	vmin			# min of all pixels
real	vmax			# max of all pixels

int	andi()

begin

	#  Assign structure variables to local accumulation variables
	a2dp = N_SATURATE(kw)
	badp = N_BADPIXEL(kw)
	cald = N_CALDEFECT(kw)
	lost = N_DATALOST(kw)
	over = N_OVERLAP(kw)
	soft = N_SOFTERROR(kw)
	stat = N_STATICDEF(kw)
	accum = SUM(kw)
	gmin = GOODMIN(kw)
	gmax = GOODMAX(kw)
	vmin = MINVAL(kw)
	vmax = MAXVAL(kw)
	good = N_GOODPIX(gpix)
	#good = N_GOODPIXEL(kw)

	# Accumulate statistics on non-flagged pixels
	do i = 1, jpts {
	    if (dqf[i] == short(GOODPIXEL)) {
		gmin  = min (gmin, row[i])
		gmax  = max (gmax, row[i])
		accum = accum + row[i]
		good  = good + 1
	    } else {

		# Accumulate statistics on flagged pixels
		if (andi (int(dqf[i]), short(SOFTERROR)) != 0)
		    soft = soft + 1

		if (andi (int(dqf[i]), short(CALIBDEFECT)) != 0)
		    cald = cald + 1

		if (andi (int(dqf[i]), short(STATICDEFECT)) != 0)
		    stat = stat + 1

		if (andi (int(dqf[i]), short(ATODSAT)) != 0)
		    a2dp = a2dp + 1

		if (andi (int(dqf[i]), short(DATALOST)) != 0) {
		    lost = lost + 1
		}

		if (andi (int(dqf[i]), short(BADPIXEL)) != 0) {
		    badp = badp + 1
		}

		if (andi (int(dqf[i]), short(OVERLAP)) != 0)
		    over = over + 1
	    }

	    # Determine min/max of ALL pixels
	    vmin = min (vmin, row[i])
	    vmax = max (vmax, row[i])
	}

	# Reset structure variables to local accumulation values
	#N_GOODPIXEL(kw) = good
	N_GOODPIX(gpix) = good
	N_SATURATE(kw)  = a2dp
	N_BADPIXEL(kw)  = badp
	N_CALDEFECT(kw) = cald
	N_DATALOST(kw)  = lost
	N_OVERLAP(kw)   = over
	N_SOFTERROR(kw) = soft
	N_STATICDEF(kw) = stat
	SUM(kw)         = accum
	GOODMIN(kw)     = gmin
	GOODMAX(kw)     = gmax
	MINVAL(kw)      = vmin
	MAXVAL(kw)      = vmax
end

#  u_incl.h     CALWP2 Common Include file

#################################################################################
#										#
#  Structure variables for various CALWP2 subroutines				#
#										#
#  Modification History: 							#
#	19 Aug 1992 by RAShaw	Initial implementation				#
#       27 Aug 1993 by CYZhang: Add things for ATODCORR, SHADCORR, and
#                               more image statistics calculations
#   3 April 2007 by WJHack: Added WF4TCORR elements
#

#  Version ID for inclusion in CALWP2
define  VERSION    "2.5.5 (Aug 27, 2009)"

#  Keyword values derived by CALWP2
define	SZ_PHOT		48			# Max_len PHOTMODE char string
define	LEN_KW		31+(2*SZ_PHOT)		# 

define	SUM		Memd[P2D($1)]		# sum of good data
define  HISTMIN         Meml[P2L($1+2)]		# Min of the histograms
define  HISTMAX         Meml[P2L($1+4)]		# Max of the histograms
define	MINVAL		Memr[($1+6)]		# value of min data point
define	MAXVAL		Memr[($1+7)]		# value of max data point
define	GOODMIN		Memr[($1+8)]		# value of min good data point
define	GOODMAX		Memr[($1+9)]		# value of max good data point
define	DATAMEAN	Memr[($1+10)]		# Mean of good pixels
define	N_GOODPIXEL	Memi[($1+11)]		# number of good pixels
define	N_SOFTERROR	Memi[($1+12)]		# Number of SOFTERROR pixels
define	N_CALDEFECT	Memi[($1+13)]		# Number of CALDEFECT pixels
define	N_STATICDEF	Memi[($1+14)]		# Number of STATICDEFECT pixels
define	N_SATURATE	Memi[($1+15)]		# Number of ATODSAT pixels
define	N_DATALOST	Memi[($1+16)]		# Number of DATALOST pixels
define	N_BADPIXEL	Memi[($1+17)]		# Number of BADPIXEL pixels
define	N_OVERLAP	Memi[($1+18)]		# Number of OVERLAP pixels
define	DEZERO		Memr[($1+19)]		# avg bias level for current chip
define	BIASEVEN	Memr[($1+20)]		# bias level for even columns 
define	BIASODD		Memr[($1+21)]		# bias level for odd columns
define	PHOTFLAM	Memr[($1+22)]		# flux for unit DN
define	PHOTPLAM	Memr[($1+23)]		# pivot wavelength
define	PHOTBW		Memr[($1+24)]		# filter bandwidth
define	PHOTZPT		Memr[($1+25)]		# zero point
define  DQFMIN          Mems[P2S($1+26)]	# Min of the DQF
define  DQFMAX          Mems[P2S($1+27)]	# Max of the DQF
define  BIASLVL     Memr[($1+28)]       # low bias value for flagging DQF
define  DELTAMAG    Memr[($1+29)]       # zero-point diff for contamination
define	PHOTMODE	Memc[P2C($1+30)]	# synphot string
define	PHOTMODE2	Memc[P2C($1+30+SZ_PHOT+1)]	# synphot string 

#  Keywords defining actions to be performed by CALWP2
define	LEN_ACT		11

define	DOMASK		Memb[($1)]		# Apply static bad pixels mask?
define	DOATOD		Memb[($1+1)]		# Apply AtoD correction? -CYZ
define  DOSHAD          Memb[($1+2)]        # Correct for shutter shading? -CYZ
define	DOBLEV		Memb[($1+3)]		# Remove bias level?
define	DOBIAS		Memb[($1+4)]		# Subtract bias image?
define	DODARK		Memb[($1+5)]		# Subtract scaled dark image?
define	DOFLAT		Memb[($1+6)]		# Divide by flat?
define	DOPHOT		Memb[($1+7)]	# Get photometry keywords from PHOTTAB?
define	OUTDTYPE	Memi[($1+8)]		# DataType of output image
define  DOHIST          Memb[($1+9)]            # Histograms (.C2H)? - CYZ
define  DOWF4T      Memb[($1+10)]       # Apply WF4T correction? - WJH

#  Camera-specific constants
define	LEN_CAM		18+3*(SZ_FNAME+1)

define	USCALE		Memr[($1)]		# Output Scaling Factors:
define	UZERO		Memr[($1+1)]		# out= (USCALE * value) + UZERO
define	A2DGAIN		Memr[($1+2)]		# Gain setting for A/D converter
define	DARKTIME	Memr[($1+3)]		# Dark accumulation time
define	DETECTOR	Memi[($1+4)]		# Detector no. being processed
define	NPTS		Memi[($1+5)]		# Image size in X and Y
define	NGROUP		Memi[($1+6)]		# No. groups in dataset
define	SATLIM		Memi[($1+7)]		# highest DN that isn't saturated
define	IS_FULL		Memb[($1+8)]		# Unbinned (T) or 2x2 binned (F)?
define	IS_CLKON	Memb[($1+9)]		# Is the clock ON or OFF?
define	RSDPFILL	Memi[($1+10)]		# fill vals for BADPIXEL|DATALOST
define  EXPTIME         Memr[($1+11)]         # Exposure of science IMAGE -CYZ
define  SHADTIME        Memr[($1+12)]        # Exposure time for SHADFILE -CYZ
define  UBAY3TMP        Memr[($1+13)]           # UBAY3 temperature (C) -CYZ
define  ATODLENGTH      Memi[($1+14)]         # Length of AtoD table lines -CYZ
define	LRFWAVE		Memr[($1+15)]		# LRF wavelength
define  EXPDATE     Memr[($1+16)]       # MJD of EXPSTART
define	FILTER1		Memc[P2C($1+17)]		# name of filter 1 
define	FILTER2		Memc[P2C($1+17+SZ_FNAME+1)]	# name of filter 2
define	SHTBLD		Memc[P2C($1+17+(SZ_FNAME+1)*2)]	# name of the blade

#  Names of input, reference, and output files and their DQFs
define	N_NAMES		26			# Max no. I/O and ref. files
define	LEN_NAM	       	(SZ_FNAME+1)*N_NAMES	#

define	IN_ROOT		Memc[$1]			# input image rootname 
define	OUT_ROOT	Memc[$1+(SZ_FNAME+1)]		# output image rootname
define	IN_IMG		Memc[$1+(SZ_FNAME+1)*2]		# name of input image
define	IN_DQF		Memc[$1+(SZ_FNAME+1)*3]		# name of static DQF
define	OUT_IMG		Memc[$1+(SZ_FNAME+1)*4] 	# output image
define	OUT_DQF		Memc[$1+(SZ_FNAME+1)*5]		# output DQF
define	MASK		Memc[$1+(SZ_FNAME+1)*6]		# static mask
define	BLVL_IMG	Memc[$1+(SZ_FNAME+1)*7]		# Bias level 
define	BLVL_DQF	Memc[$1+(SZ_FNAME+1)*8]		# Bias level DQF
define	BIAS_IMG	Memc[$1+(SZ_FNAME+1)*9]		# Bias image
define	BIAS_DQF	Memc[$1+(SZ_FNAME+1)*10]	# Bias image DQF
define	DARK_IMG	Memc[$1+(SZ_FNAME+1)*11]	# Dark image
define	DARK_DQF	Memc[$1+(SZ_FNAME+1)*12]	# Dark image DQF
define	FLAT_IMG	Memc[$1+(SZ_FNAME+1)*13]	# Flat image
define	FLAT_DQF	Memc[$1+(SZ_FNAME+1)*14]	# Flat image DQF
define	GRAPH_TBL	Memc[$1+(SZ_FNAME+1)*15]	# Graph table
define	COMP_TBL	Memc[$1+(SZ_FNAME+1)*16]	# Component table
define	THRU_TBL	Memc[$1+(SZ_FNAME+1)*17]	# Output thruput table
define	OUT_GRP		Memc[$1+(SZ_FNAME+1)*18]	# Grp params ASCII file
define  SHAD_IMG	Memc[$1+(SZ_FNAME+1)*19]	# Shutter shading image
define  ATOD_IMG	Memc[$1+(SZ_FNAME+1)*20]	# AtoD image
define  HIST_IMG	Memc[$1+(SZ_FNAME+1)*21]	# Histogram image
define  IN_DGR		Memc[$1+(SZ_FNAME+1)*22]	# input .dgr file
define  TEMP		Memc[$1+(SZ_FNAME+1)*23]	# temporary file
define	ISFITS		Memc[$1+(SZ_FNAME+1)*24]	# FITS flag (T/F)
define  WF4T_IMG	Memc[$1+(SZ_FNAME+1)*25]	# WF4T image

#  Descriptors of input, reference, and output files and their DQFs
define	LEN_PTR		21			# Max no. I/O and ref. files

define	IN_IMG_P	Memi[($1)]		# input image descriptor
define	IN_DQF_P	Memi[($1+1)]		# static DQF
define	OUT_IMG_P	Memi[($1+2)]		# output image
define	OUT_DQF_P	Memi[($1+3)]		# output DQF
define	MASK_P		Memi[($1+4)]		# static mask
define	BLVL_IMG_P	Memi[($1+5)]		# Bias level 
define	BLVL_DQF_P	Memi[($1+6)]		# Bias level DQF
define	BIAS_IMG_P	Memi[($1+7)]		# Bias image
define	BIAS_DQF_P	Memi[($1+8)]		# Bias image DQF
define	DARK_IMG_P	Memi[($1+9)]		# Dark image
define	DARK_DQF_P	Memi[($1+10)]		# Dark image DQF
define	FLAT_IMG_P	Memi[($1+11)]		# Flat image
define	FLAT_DQF_P	Memi[($1+12)]		# Flat image DQF
define	THRU_TBL_P	Memi[($1+13)]		# Thruput table
define	OUT_GRP_P	Memi[($1+14)]		# Group params ASCII file
define  ATOD_IMG_P      Memi[($1+15)]           # AtoD correction image
define  SHAD_IMG_P      Memi[($1+16)]           # Shutter shading image
define  HIST_IMG_P      Memi[($1+17)]           # Histogram image
define	IN_DGR_P	Memi[($1+18)]		# Input .dgr file
define	TEMP_P		Memi[($1+19)]		# temporary file
define  WF4T_IMG_P  Memi[($1+20)]       # WF4T correction image

# Statistics of calibrated WFPC2 images -- CYZhang  22/8/93

define	LEN_STAT  	25

define	S_SUMX		Memd[P2D($1)]     # Sum of good pixels
define	S_LO		Memr[$1+2]	  # Lower limit
define	S_HI		Memr[$1+3]        # Upper limit
define	S_AVG		Memr[$1+4]        # Mean
define	S_GMEDIAN	Memr[$1+5]        # Median of good pixels
define	S_SMEDIAN 	Memr[$1+6]        # Median in shadow area
define  S_DN10          Memr[$1+7]        # DN value at 10% of good pixels
define  S_DN25          Memr[$1+8]        # DN value at 25% of good pixels
define  S_DN75          Memr[$1+9]        # DN value at 75% of good pixels
define	S_HISTWIDE	Memr[$1+10]       # DN range 25% to 75% of good pixels
define	S_SKEW		Memr[$1+11]       # Skewness
define	S_MEANC10   	Memr[$1+12]       # Mean of central 10 x 10 area
define	S_MEANC25	Memr[$1+13]       # Mean of central 25 x 25 area
define	S_MEANC50	Memr[$1+14]       # Mean of central 50 x 50 area
define	S_MEANC100	Memr[$1+15]       # Mean of central 100 x 100 area
define	S_MEANC200	Memr[$1+16]       # Mean of central 200 x 200 area
define	S_MEANC300	Memr[$1+17]       # Mean of central 300 x 300 area
define  S_BCKGRD        Memr[$1+18]       # Peak of histogram
define  S_HMIN          Memr[$1+19]       # Floor of histogram
define  S_HMAX          Memr[$1+20]       # Ceiling of histogram
define  S_HWIDTH        Memr[$1+21]       # Bin width of histogram
define  S_NBIN10        Memi[$1+22]        # Bin No at 10%
define  S_HGM           Memi[$1+23]       # Array holding histogram
define  S_NBINS         Memi[$1+24]       # number of bins in histogram

#  Descriptors of input, reference, and output files and their DQFs
define	LEN_GPIX		5		    # No. of chips plus NGOODPIX entry

define	N_GOODPIX	Memi[($1)]		    # No. of good pixels for current group 
define	GOODPIX1	Memi[($1+1)]		# No. of good pixels for group 1
define	GOODPIX2	Memi[($1+2)]		# No. of good pixels for group 2
define	GOODPIX3	Memi[($1+3)]		# No. of good pixels for group 3
define	GOODPIX4	Memi[($1+4)]		# No. of good pixels for group 4

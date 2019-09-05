#  u_data.h     CALWP2 Data Include file
#
#  Last Modified: 
#	11 Aug 1992 by RAShaw	Initial implementation
#       28 Aug 1993 by CYZhang  Revised
#        1 Oct 1993 by CYZhang  Revised
#
#

#  Data Quality File condition values (listed by default severity)

define	GOODPIXEL	  0	# DQF value of unflagged pixel
define	SOFTERROR	  1	# Reed-Solomon Error
define	CALIBDEFECT	  2	# Defect in calibration file applied to image
define	STATICDEFECT  4	# Static defect
define	ATODSAT		  8	# AtoD pinned at full scale
define	DATALOST	 16	# Data missing
define	BADPIXEL	 32	# Generic "bad" pixel
define	OVERLAP		 64	# "Overlap Region" pixel
define  LOWBLEV    2048 # "Low Bias level readout" pixel

#  Defined constants

define  NX_FULL		800	# Standard input image FULL X size
define  NY_FULL		800	# Standard input image FULL Y size
define	NX_AREA	 	400	# X size for AREA mode
define	NY_AREA	 	400	# Y size for AREA mode
#define	FULL_START	  3	# FULL bias level actual data start column
define	FULL_START	  9	# FULL bias start column --CYZ 22/2/94
define	FULL_END	 14	# FULL bias level actual data end column
define	NY1_FULL	 11	# Staring useful lines of .x0h --CYZ 22/2/94
define	NY2_FULL	790	# Ending useful lines of .x0h --CYZ 22/2/94
define	EEDSIZE	   	 14	# max length of EED line
define	AREAEEDSIZE	  2	# 
define	SZ_PHOT		 48	# max length of PHOTMODE string
define	HI_A2D		 15.	# High A-to-D gain setting 
define	LO_A2D		  7.	# Low A-to-D gain setting 

#  Range of the WF/PC-2 12 bit AtoD after screening for fill values
define	MINALLOWED 	  0	# min value allowed in input image
define	MAXALLOWED     4095	# max value allowed in input image
define  MAXATODLENGTH  4096     # max length of AtoD

#  Define histogram length  -- CYZhang  28/8/93
define  HISTLENGTH     4096     # Number of histogram bins

#  Define PODPS environment test:
define	PODPS_ENVIRON	"PODPS_CLD"

#  Define reference file types -- CYZhang 29/9/93
#   Added WF4T -- WJHack 3 April 07
define	REF_MASK	1
define	REF_ATOD	2
define	REF_BIAS	3
define	REF_DARK	4
define	REF_FLAT	5
define	REF_SHAD	6
define  REF_WF4T    7

#  Define thigs needed in .dgr  -- CYZhang 1/10/93
define	NUM_GKW		49		# Number of DGR keywords in each chip
define	NUM_NKW		11		# Number of "new" statistics keywords
define	SZ_LONG_LINE	(80+SZ_LINE)	# Input line length > 80
define	SZ_GKW		10		# Size of the kw 
define	MIN_BW		0.01		# Minimum bin width

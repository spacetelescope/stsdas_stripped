# WRDATA.H     CALWFP Data Include file
# Last Modified 23 Oct 1991 by RAShaw

#######################################################################
#  Data Quality File Condition Values (listed by default severity)

define	GOODPIXEL	0	# DQF value of unflagged pixel

define	SOFTERROR	1	# Reed-Sol Error
define	CALIBDEFECT	2	# Defect in calibration file applied to image
define	STATICDEFECT	4	# Static defect
define	ATODSAT		8	# AtoD pinned at full scale
define	DATALOST	16	# Data missing
define	BADPIXEL	32	# Generic "bad" pixel

#######################################################################
#  Miscellaneous constants

define  FULL_NX  800		# Standard input image FULL X size
define  FULL_NY  800		# Standard input image FULL Y size

define	FULLEEDSTART 3		# FULL bias level actual data start column
define	FULLEEDSIZE 14		# FULL bias level actual data end column
define	EEDSIZE	    14		# max length of EED line
define	AREA_NY	    800
define	AREAEEDSIZE 2

define	MAXATODLENGTH 4096	# Max length of AtoD conversion array
define	MAXHISTLENGTH 10000	# max length of histogram arrays

define	MINALLOWED 0		# min/max value allowed in input image
define	MAXALLOWED 4095		# after screening for fill values;
				# this is the range of the WF/PC 12 bit AtoD

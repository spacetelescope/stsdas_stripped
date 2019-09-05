# calfoc.h  1994 March 24.

# extension for input image
define	INPUTEXT	".d0h"

# extensions for output images
define	GEO_EXT		".c0h"		# geometrically corrected image
define	UNI_EXT		".c1h"		# fully corrected image
define	TEMP_EXT	".cth"		# temporary image name
define	BAC_EXT		".cbh"		# BAC, ITF, PXL, or ABS only
define	GEO_ONLY_EXT	".cgh"		# GEO only
define	UNI_ONLY_EXT	".cuh"		# UNI only

# This is the name of an environment variable that's supposed to always
# be defined in the PODPS system.
define	PODPS_ENVIRON	"PODPS_CLD"

# calibration data and files
define	SZ_CALDATA	45		# 42 used at present

define	EXP_TIME	Memr[$1]	# exposure time
define	GEO_DEFV	Memr[$1+1]	# default value for output from geo
define	FOC_IMG_MODE	Memi[$1+2]	# normal or spectrographic mode

# These are pointers to the imhdr structure for calibration images.
define	BAC_FILE	Memi[$1+3]
define	ITF_FILE	Memi[$1+4]
define	GEO_FILE	Memi[$1+5]
define	UNI_FILE	Memi[$1+6]
define	SDE_FILE	Memi[$1+7]

# Is there any calibration to be done?  True if any of the following seven
# flags is true.
define	DO_SOMETHING	Memb[$1+8]

# The flag will be set to true if we should do the correction.
define	DO_BAC		Memb[$1+9]
define	DO_ITF		Memb[$1+10]
define	DO_PXL		Memb[$1+11]
define	DO_ABS		Memb[$1+12]
define	DO_GEO		Memb[$1+13]
define	DO_UNI		Memb[$1+14]
define	DO_SDE		Memb[$1+15]
define	DO_FLAT		Memb[$1+16]	# true if either DO_UNI or DO_SDE

# Flag values are PERFORM, OMIT, COMPLETE (see status codes below).
define	BAC_FLAG	Memi[$1+17]
define	ITF_FLAG	Memi[$1+18]
define	PXL_FLAG	Memi[$1+19]
define	ABS_FLAG	Memi[$1+20]
define	GEO_FLAG	Memi[$1+21]
define	UNI_FLAG	Memi[$1+22]
define	SDE_FLAG	Memi[$1+23]

# Allowed values are NO_LOG, LOG_TRAILER, LOG_HEADER, or LOG_TRAILER+LOG_HEADER.
define	BAC_LOG		Memi[$1+24]
define	ITF_LOG		Memi[$1+25]
define	PXL_LOG		Memi[$1+26]
define	ABS_LOG		Memi[$1+27]
define	GEO_LOG		Memi[$1+28]
define	UNI_LOG		Memi[$1+29]
define	SDE_LOG		Memi[$1+30]

# UNI file name given in header?
define	EXPLICIT_UNI	Memb[$1+31]

# These are pointers to the values (character strings) of these keywords.
define	BAC_PEDIGREE	Memi[$1+32]
define	BAC_DESCRIP	Memi[$1+33]
define	ITF_PEDIGREE	Memi[$1+34]
define	ITF_DESCRIP	Memi[$1+35]
define	UNI_PEDIGREE	Memi[$1+36]
define	UNI_DESCRIP	Memi[$1+37]
define	GEO_PEDIGREE	Memi[$1+38]
define	GEO_DESCRIP	Memi[$1+39]
define	SDE_PEDIGREE	Memi[$1+40]
define	SDE_DESCRIP	Memi[$1+41]

# Status codes.  These are used for the ..._FLAG values above
# and also for the cal_op_stat function.
define	PERFORM		1
define	OMIT		2
define	COMPLETE	3
define	SKIPPED		4
define	NOT_APPLICABLE	5

# These are the values for the ..._LOG flags above.  If a calibration flag
# is PERFORM (or will be set to SKIPPED), the corresponding _LOG flag will
# initially be set to LOG_TRAILER+LOG_HEADER (except for PXL_LOG and ABS_LOG)
# to indicate that info should be logged to both the trailer file and as
# history to the output header.  After logging, the _LOG flag will be left
# as NO_LOG.
define	NO_LOG		0	# nothing to be logged
define	LOG_TRAILER	1	# must log to trailer file
define	LOG_HEADER	2	# must log as history in output image header

# Normal image mode or spectrographic mode for FOC_IMG_MODE.
define	FOC_NORMAL	1
define	FOC_SPECTRO	2

# size of full photocathode
define	MAX_ZSAMPS	 512
define	MAX_ZLINES	1024
define	MAX_NSAMPS	1024	#Changed Y.F.
define	MAX_NLINES	1024

# The values of sampoff and lineoff saved in this struct are as gotten
# directly from the image header.  The value of sampbeg, however, is
# "flipped" so it can be used (with linebeg and the image size) for
# taking image sections of calibration reference files.

define	SZ_FOC_COORD		20		# only eight needed

define	FOC_IMPT		Memi[$1]
define	FOC_NPIX1		Memi[$1+1]
define	FOC_NPIX2		Memi[$1+2]
define	FOC_SAMPOFF		Memr[$1+3]
define	FOC_LINEOFF		Memr[$1+4]
define	FOC_SAMPBEG		Memi[$1+5]
define	FOC_LINEBEG		Memi[$1+6]
define	FOC_ZOOM_MODE		Memb[$1+7]

#  WRINCL.H     CALWFP Common Include file
#  Last Modified 28 Jan 1992 by RAShaw

##########################################################################
# Variables stored in COMMON blocks in all CALWFP subroutines

real	ETIME			# Exposure time (shutter open)
real	DTIME			# Dark accumulation time
real	PTIME			# Preflash time
real	RTIME			# time since last superpurge
real	PURGESCALE		# scaling factor for purge residual removal
real	BAY3TEMP		# Temperature of WF/PC Bay 3 (for AtoD)

real	MAXVAL			# Running value of maximum data point
real	MINVAL			# Running value of minimum data point
real	GOODMIN			# Running value of minimun good data point
real	GOODMAX			# Running value of maximum good data point
real	SUM			# Running sum of good data

long	N_GOODPIXEL		# Running sum of number of good data points
long	N_SOFTERROR		# Running sum of number of SOFTERROR pixels
long	N_CALIBDEFECT		# Running sum of number of CALIBDEFECT pixels
long	N_STATICDEFECT		# Running sum of number of STATICDEFECT pixels
long	N_ATODSAT		# Running sum of number of ATODSAT pixels
long	N_DATALOST		# Running sum of number of DATALOST pixels
long	N_BADPIXEL		# Running sum of number of BADPIXEL pixels

real	DEZERO			# Average bias level for current chip
real	BIASEVEN		# Bias level for even rows of current chip
real	BIASODD			# Bias level for odd rows of current chip
bool	BOEFLAG			# Are BIASEVEN/ODD keywords present in header?

int	SATLIM			# highest data number which is not saturated
int	CHIPNUM			# Number of chip being processed
int	KNX			# Image X size
int	KNY			# Image Y size
int	ATODLENGTH		# No of elements in AtoD conversion array
int	NGROUP			# No of groups in the dataset
int	HISTLENGTH		# Length of the histograms

bool	DOMASK			# Apply the static bad pixels mask?
bool	DOATOD			# Do the AtoD correction?
bool	DOBLEV			# Do the bias level removal?
bool	DOBIAS			# Do the bias removal?
bool	DOPREF			# Do the preflash removal?
bool	DOPURG			# Do the superpurge removal?
bool	DODARK			# Do the dark removal?
bool	DOFLAT			# Do the flat removal?
bool	DOSAT			# Make an output saturation DQF?
bool	DOHIST			# Make the output histograms?
bool	DOPHOT			# Get photometry keywords from PHOTTAB?
bool	DOGRP			# Write group parameters to text file?

int	OUTDTYPE		# Iraf DataType of output image
real	WSCALE			# Output Scaling Factors:
real	WZERO			# out = (WSCALE * value) + WZERO

bool	WFC			# WF Camera (true) or PC (false)
bool	DOFULL			# Unbinned (true) or 2x2 binned (false) ?

bool	PFILL			# PODPS fill values present
bool	SFILL			# ST-DCF fill values present
int	PODPSFILL		# PODPS fill value
int	STDCFFILL		# ST-DCF fill value

int	RSDPFILL		# fill values inserted by CALWFP for all
				# pixels flagged BADPIXEL or DATALOST

char	FILTER1[SZ_LINE]	# name of filter 1 (from input_image header)
char	FILTER2[SZ_LINE]	# name of filter 2

# Image pointers
pointer	II_IM, ID_IM		# Input image pointer and DQF
pointer	MD_IM			# Static DQF image pointer
pointer	A_IM			# AtoD conversion image pointer
pointer	BL_IM, BLD_IM		# Bias level image pointer and DQF
pointer	BI_IM, BD_IM		# Bias image pointer and DQF
pointer	PI_IM, PD_IM		# Preflash image pointer and DQF
pointer RI_IM, RD_IM		# Superpurge (Residual) image pointer and DQF
pointer	DI_IM, DD_IM		# Dark image pointer and DQF
pointer	FI_IM, FD_IM		# Flat image pointer and DQF
pointer	PH_TAB			# Photometry calibration table pointer
pointer OI_IM, OD_IM		# Output image pointer and DQF
pointer	H_IM			# Histogram image pointer
pointer	SD_IM			# Saturation DQF image pointer
int	G_FILE			# Group parameters text file

# Image open (true)/ closed (false) flags
bool	II_OPEN, ID_OPEN	# Input image and its DQF
bool	MD_OPEN			# Static DQF image
bool    A_OPEN 			# AtoD conversion file
bool    BL_OPEN, BLD_OPEN	# Bias level image and DQF
bool    BI_OPEN, BD_OPEN	# Bias image and DQF
bool    PI_OPEN, PD_OPEN	# Preflash image and DQF
bool	RI_OPEN, RD_OPEN	# Superpurge (Residual) image and DQF
bool    DI_OPEN, DD_OPEN	# Dark image and DQF
bool    FI_OPEN, FD_OPEN	# Flat Field image DQF
bool	PH_OPEN			# Photometry calibration table
bool    OI_OPEN, OD_OPEN	# Output image DQF
bool    H_OPEN			# Output Histograms file
bool    SD_OPEN			# Output Saturation DQF
bool	G_OPEN			# Group parameters text file

#  Actual names of input, reference, and output files and their DQFs
char	IN_ROOT[SZ_FNAME]	# rootname of input data set
char	OUT_ROOT[SZ_FNAME]	# rootname for output data set
char	ROOT[SZ_FNAME]		# Rootname of data set (from image header)
char	II_NAME[SZ_FNAME]	# Input image
char	ID_NAME[SZ_FNAME]	# Input image DQF
char	MD_NAME[SZ_FNAME]	# Static DQF
char	A_NAME[SZ_FNAME]	# AtoD conversion 
char	BL_NAME[SZ_FNAME]	# Bias level image (engineering file)
char	BLD_NAME[SZ_FNAME]	# Bias level image (engineering file) dqf
char	BI_NAME[SZ_FNAME]	# Bias image 
char	BD_NAME[SZ_FNAME]	# Bias image dqf 
char	PI_NAME[SZ_FNAME]	# Preflash image 
char	PD_NAME[SZ_FNAME]	# Preflash image dqf 
char	RI_NAME[SZ_FNAME]	# Superpurge (residual) image 
char	RD_NAME[SZ_FNAME]	# Superpurge (residual) image dqf 
char	DI_NAME[SZ_FNAME]	# Dark image 
char	DD_NAME[SZ_FNAME]	# Dark image dqf 
char	FI_NAME[SZ_FNAME]	# Flat image 
char	FD_NAME[SZ_FNAME]	# Flat image dqf 
char	PH_NAME[SZ_FNAME]	# Photometry calibration table
char	OI_OUT[SZ_FNAME]	# Output image 
char	OD_OUT[SZ_FNAME]	# Output image dqf 
char	H_OUT[SZ_FNAME]		# Output histogram 
char	SD_OUT[SZ_FNAME]	# Output saturated pixels dqf
char	G_OUT[SZ_FNAME]		# Output group parameters text file

#  Define PODPS environment test:
# define	PODPS_ENVIRON	"PODPS_CLD"

#####################################################################

common /wrcoma/ ETIME, DTIME, PTIME, RTIME, PURGESCALE, BAY3TEMP,
		MAXVAL, MINVAL, GOODMIN, GOODMAX, SUM,
		N_GOODPIXEL, N_SOFTERROR, N_CALIBDEFECT, N_STATICDEFECT,
		N_ATODSAT, N_DATALOST, N_BADPIXEL, DEZERO, BIASEVEN, BIASODD,
		SATLIM, CHIPNUM, KNX, KNY, ATODLENGTH, NGROUP, HISTLENGTH

common /wrcomb/	WSCALE, WZERO, PODPSFILL, STDCFFILL, RSDPFILL

common /wrcomc/ DOMASK, DOATOD, DOBLEV, DOBIAS, DOPREF, DOPURG,
		DODARK, DOFLAT, DOSAT, DOHIST, OUTDTYPE, DOPHOT, DOGRP,
		WFC, DOFULL, PFILL, SFILL, BOEFLAG

common /wrcomd/ FILTER1, FILTER2

common /wrcome/ II_IM, ID_IM, MD_IM, A_IM, BL_IM, BLD_IM, BI_IM, BD_IM, 
		PI_IM, PD_IM, RI_IM, RD_IM, DI_IM, DD_IM, FI_IM, FD_IM,
		OI_IM, OD_IM, H_IM, SD_IM, PH_TAB, G_FILE

common /wrcomf/ II_OPEN, ID_OPEN, MD_OPEN, A_OPEN, BL_OPEN, BLD_OPEN, 
		BI_OPEN, BD_OPEN, PI_OPEN, PD_OPEN, RI_OPEN, RD_OPEN, 
		DI_OPEN, DD_OPEN, FI_OPEN, FD_OPEN, PH_OPEN,
		OI_OPEN, OD_OPEN, H_OPEN, SD_OPEN, G_OPEN

common /wrcomg/ IN_ROOT, OUT_ROOT, ROOT, II_NAME, ID_NAME, MD_NAME, A_NAME, 
		BL_NAME, BLD_NAME, BI_NAME, BD_NAME, DI_NAME, DD_NAME, 
		RI_NAME, RD_NAME, PI_NAME, PD_NAME,FI_NAME, FD_NAME, PH_NAME,
		OI_OUT, OD_OUT, H_OUT, SD_OUT, G_OUT

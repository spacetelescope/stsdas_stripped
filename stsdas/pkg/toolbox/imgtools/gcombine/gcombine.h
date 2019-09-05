# GCOMBINE Definitions

define	VERSION		"1.3.2"		# Version number

# Memory management parameters;
define	DEFBUFSIZE		65536		# default IMIO buffer size
define	FUDGE			0.8		# fudge factor

# Rejection options:
define	REJECT	"|none|ccdclip|ccdcrrej|minmax|rsigclip|rsigcrrej|avsigclip|avsigcrrej|errclip|errcrrej|"
define	NONE		1	# No rejection algorithm
define	CCDCLIP		2	# CCD noise function clipping
define	CCDCRREJ	3	# CCD noise function rejecting CR
define	MINMAX		4	# Minmax rejection
define	RSIGCLIP	5	# Robust sigma clip
define	RSIGCRREJ	6	# Robust sigma rejecting CR
define	AVSIGCLIP	7	# Sigma clip with average poisson sigma
define	AVSIGCRREJ	8	# CR rejection with average poisson sigma
define	ERRCLIP		9	# Clipping using error maps
define	ERRCRREJ	10	# Using error maps to reject CR

# Combine options:
define	COMBINE	"|average|median|"
define	C_AVERAGE	1
define	C_MEDIAN	2

# Weighting schemes:
#define	WTYPES		"|none|uniform|pixelwise|" # not used -- CYZ Jul 8, 94
define	W_NONE		1 	# No weighting applied
define	W_UNIFORM	2	# Use uniform weighting
define	W_PIXWISE	3	# Use pixelwise weighting

# Scaling options:
define	STYPES		"|none|mode|median|mean|exposure|"
define	ZTYPES		"|none|mode|median|mean|"
define	UWTYPES		"|none|mode|median|mean|exposure|"
define	S_NONE		1
define	S_MODE		2
define	S_MEDIAN	3
define	S_MEAN		4
define	S_EXPOSURE	5
define	S_FILE		6
define	S_KEYWORD	7

# Clipping schemes: -- not yet implemented
define	CLIPSCHEME	"|iclip|lclip|hclip|"
define	C_ICLIP		1	# Use info across images to be combined
define	C_LCLIP		2	# Use info along image lines
define	C_HCLIP		3	# Use info across images and along image lines

# 
define	TOL		0.001	# Tolerance for equal residuals
define	MVALUE		1000	# DQF value for rejected pixels

# Scale factors:
define	LEN_SZUW	4  			# Pointers

define	SCALES		Memi[($1)]		# Divisive Scaling Factors
define	ZEROS		Memi[($1+1)]		# Subtractive zero levels
define	UWTS		Memi[($1+2)]		# Uniform weights
define	NCOMB		Memi[($1+3)] 	       # N of combines done to an image

# Noise model parameters:
define	LEN_NM		3			# Pointers

define	RDNOISE		Memi[($1)]		# Readnoise in electrons
define	GAIN		Memi[($1+1)]		# Gain in e-/DN
define	SNOISE		Memi[($1+2)]	    # Sensitivity noise as a fraction

# Maximum range list
define	MAX_RANGES	100
define	GPIX		10	 	# Critical number of good pixels

# Data flags
define	D_ALL		0		# All pixels are good
define	D_NONE		1		# All pixels are bad
define	D_MIX		2		# Mixture of good and bad

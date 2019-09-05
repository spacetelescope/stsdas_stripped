# ICOMBINE Common

int	G_COMBINE			# Combine algorithm
int	G_REJECT			# Rejection algorithm
int	G_WEIGHT			# Weighting schemes
bool	MCLIP				# Use median in sigma clipping?
bool	NSMOD_W				# Use noise model for weights?
bool	NSMOD_E				# Use noise model for output error map?
bool	REJ_NSMOD			# Use noise model for rejections?
bool	DOMASK				# Use DQF for masking?
bool	DOREJ				# Output rejection maps?
pointer	rdns, gns, sns			# rdnoise, gain and snoise
real	FLOW				# Fraction of low pixels to reject
real	FHIGH				# Fraction of high pixels to reject
real	LTHRESH				# Low threshold
real	HTHRESH				# High threshold
int	NKEEP				# Minimum to keep
real	LSIGMA				# Low sigma cutoff
real	HSIGMA				# High sigma cutoff
int	LOGFD				# Log file descriptor
real	BLANK				# Balnk value
real	BSKY				# Mean Sky Level

# These flags allow special conditions to be optimized.

bool	DOMODE, DOMEDIAN, DOMEAN# For scales computations
int	DFLAG			# First group?
bool	DOSCALE			# Do the images have to be scaled?
#bool	doscale1		# Do the sigma calculations have to be scaled?
bool	DOTHRESH		# Check pixels outside specified thresholds?
bool	DOWTS			# Does the final average have to be weighted?
#bool	keepids			# Keep track of the image indices?
bool	DOCOMBINE		# Call the combine procedure?
bool	DOSORT			# Sort data?

common	/gccom/ G_COMBINE, G_REJECT, G_WEIGHT, MCLIP, REJ_NSMOD,
		DOMASK, DOREJ, rdns, gns, sns, FLOW, FHIGH, NSMOD_W, NSMOD_E,
		LTHRESH, HTHRESH, NKEEP, LSIGMA, HSIGMA, LOGFD, BLANK, BSKY,
		DOSCALE, DOTHRESH, DOWTS, DOCOMBINE, DOSORT, 
		DOMODE, DOMEDIAN, DOMEAN, DFLAG


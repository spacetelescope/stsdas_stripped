# Data quality flags for STIS.
# This task computes the sum of the data quality flag values (bit mask)
# for the specified conditions.  The sum is assigned to the parameter
# sdqflags.value.  If verbose=yes, the sum is also printed.

procedure sdqflags()

bool	softerr = no {prompt = "Reed-Solomon decoding error"}
bool	datalost = no {prompt = "data replaced by fill value"}
bool	detectorprob = no {prompt = "beyond aperture or bad detector pixel"}
bool	datamasked = no {prompt = "masked by occulting bar or repeller wire"}
bool	hotpix = no {prompt = "hot pixel"}
bool	largeblem = no {prompt = "large blemish"}
bool	overscan = no {prompt = "can be used for finding bias level"}
bool	satpixel = no {prompt = "saturated pixel"}
bool	calibdefect = no {prompt = "bad pixel in reference file"}
bool	smallblem = no {prompt = "small blemish"}
bool	x1d_bad_background = no {prompt = "bad pixels in background region"}
bool	x1d_discarded = no {prompt = "pixel discarded from extraction region"}
bool	datareject = no {prompt = "rejected during image combination"}
bool	not_cti_corr = no {prompt = "pixel not CTI corrected"}
bool	b64 = no {prompt = "not assigned"}
bool	verbose = yes {prompt = "print the sum?"}
int	value = 0 {prompt = "sum of flag values (output)"}
string	Version = "4Apr2007" {prompt = "date of installation"}
string	mode = "al"

begin
	int sum

	sum = 0

	if (softerr)
	    sum = sum + 1

	if (datalost)
	    sum = sum + 2

	if (detectorprob)
	    sum = sum + 4

	if (datamasked)
	    sum = sum + 8

	if (hotpix)
	    sum = sum + 16

	if (largeblem)
	    sum = sum + 32

	if (overscan)
	    sum = sum + 128

	if (satpixel)
	    sum = sum + 256

	if (calibdefect)
	    sum = sum + 512

	if (smallblem)
	    sum = sum + 1024

	if (x1d_bad_background)
	    sum = sum + 2048

	if (x1d_discarded)
	    sum = sum + 4096

	if (datareject)
	    sum = sum + 8192

	if (not_cti_corr)
	    sum = sum + 16384

	# not currently assigned
	if (b64)
	    sum = sum + 64

	value = sum

	if (verbose)
	    print (sum)
end

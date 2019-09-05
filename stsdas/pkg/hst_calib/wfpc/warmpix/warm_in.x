#  warm_in -- Read CL parameters for the task warmpix.
#
#  Description:
#  ------------
#  Reads CL parameters and does necessary checkings
#  
#  Input CL parameters:
#  -----------------
#
#  "input"		Input science data file template name
#  "masks"		Input data quality file template name
#  "tables"		Input warm pixel table
#  "rej_thresh"		rejection threshold
#  "fix_thresh"		fix threshold
#  "var_thresh"		the "variation" threshold
#  "fix_dqval"		Data quality value for fixed pixels
#  "rej_val"		Pixel value for high dark count hot pixels
#  "verbose"		print out verbose messages?
#
#  Date		Author			Description
#  ----		------			-----------
#  21-Jul-1995  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------
procedure warm_in (tpin, tpinmask, intable, thresh, fix_dqval, rej_val, 
			verbose)

# outputs:
pointer	tpin 			# input image template pointer
pointer	tpinmask 		# input mask template pointer
char	intable[SZ_FNAME]
real	thresh[ARB]
short	fix_dqval		# data quality value of fixed pixels
real	rej_val			# pixel value of rejected pixels
bool	verbose

real	clgetr()
short	clgets()
int	imtlen()
bool	clgetb()
pointer	imtopenp()
#==============================================================================
begin

	# open input/outut image and input/output mask templates and 
	# find out how many files are in there 
	tpin = imtopenp ("input")
	tpinmask = imtopenp ("masks")

	# read input table name
	call clgstr ("tables", intable, SZ_FNAME)

	# check that (1) the input template can not be empty, (2) input mask
	# template must match the input image template
        if (imtlen(tpin) == 0)
            call error (1, "empty input template")
        if (imtlen(tpinmask) < imtlen(tpin))
            call error (1, "Not enough input masks in the template")

	# read threshold levels
	thresh[1] = clgetr ("rej_thresh")
	thresh[2] = clgetr ("fix_thresh")
	thresh[3] = clgetr ("var_thresh")

	fix_dqval = clgets ("fix_dqval")
	rej_val = clgetr ("rej_val")
	verbose = clgetb ("verbose")
end

include	"streakflat.h"

#  flat_in -- Read CL parameters for the task streakflat.
#
#  Description:
#  ------------
#  Read CL parameters and do necessary checking and conversions.
#  
#  Input CL parameters:
#  -----------------
#
#  "infile"		Input science data file template name
#  "outfile"		Output science data file template name
#  "iterations"		Number of iterations in the calculation
#  "widths"		Half widths used in boxcar filtering
#  "good_points"	Number of good points needed to calculate the flat field
#			value for each pixel
#  "verbose"		print out verbose messages?
#
#  Date		Author			Description
#  ----		------			-----------
#  26-Mar-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure flat_in (fin, fout, iter, width, ngood, verbose)

pointer	fin
char	fout[SZ_FNAME]
int	iter
int	width[MAX_ITER]
int	ngood
bool	verbose

char	widthstr[SZ_LINE]
int	i, ip

int	clgeti()
int	next_num()
bool	clgetb()
pointer	imtopenp()
#==============================================================================
begin

	# open input file template and find out how many files are in there 
	fin = imtopenp ("infile")
	call clgstr ("outfile", fout, SZ_FNAME)

	# check that the output file can not be empty
	if (fout[1] == EOS)
	    call error (1, "blank output file name")

	# read other CL parameters
	iter = clgeti ("iterations")

	# limit iterations to a maximum
	if (iter > MAX_ITER) {
	    call printf (
		"max number of iterations is %d, iteration will stop at %d\n")
		call pargi (MAX_ITER)
		call pargi (MAX_ITER)
	    call flush (STDOUT)
	    iter = MAX_ITER
	}

	call clgstr ("widths", widthstr, SZ_LINE)
	ngood = clgeti ("good_points")
	verbose = clgetb ("verbose")
	
	# parse the widths string into numbers
	i = 1
	ip = 1
	while (next_num (widthstr, ip, width[i]) != 0 && i < iter)
	    i = i + 1
end

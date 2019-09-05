include "delaytime.h"
define	SZ_DUNIT	6

#  ODELAY_GET -- Read parameters for the task odelaytime.
#
#  Description:
#  ------------
#  Read CL parameters and do necessary checking and conversions.
#  
#  Input CL parameters:
#  -----------------
#  "input"		Input/output science data table
#  "distance"		distance of the target
#  "dist_unit"		unit used in the distance parameter
#  "earth_ephem"	name of the table which contains earth's J2000 state 
#			vectors
#  "obs_ephem"		name of the table which contains observer's J2000 state 
#			vectors
#  "in_col"		input time column name
#
#  Date		Author		Description
#  ----		------		-----------
#  09-Nov-1984  C. B. Biemesderfer	original
#  11-Apr-1990  J.-C. Hsu	rewrite in SPP
#  19-Aug-1997  J.-C. Hsu	modify for STIS data
#  14-Aug-2000  Phil Hodge	remove out_col parameter, and get
#				verbose parameter
#  10-Jun-2003  Phil Hodge	return the names of the ephemeris tables,
#				instead of opening them here
#------------------------------------------------------------------------------

procedure odelay_get (fin, nfin, parallax, earth_ephem, obs_ephem, maxch,
		in_col, verbose)

pointer	fin			# o: file template pointer
int	nfin			# o: number of input files 
double	parallax		# o: parallax of the target (in arc sec)
char	earth_ephem[maxch]	# o: name of the earth_ephem table
char	obs_ephem[maxch]	# o: names of the obs_ephem tables
int	maxch			# i: size of file name strings
char	in_col[ARB]		# o: time column name
bool	verbose			# o: print timing info?
#--
char	dist_unit[SZ_DUNIT]	# unit of the distance

pointer	imtopenp() 
int	imtlen()
double	clgetd()
bool	clgetb()
bool	streq(), strne()
#==============================================================================
begin

	# open file templates and find out how many files are in the templates
	fin = imtopenp ("input")

	nfin = imtlen (fin)

	# the list of input files must not be empty
	if (nfin < 1)
	    call error (1, "blank input file template")

	# read the names of the ephemeris tables
	call clgstr ("earth_ephem", earth_ephem, maxch)
	call clgstr ("obs_ephem", obs_ephem, maxch)

	# read other CL parameters
        parallax = clgetd ("distance")
	call clgstr ("dist_unit", dist_unit, SZ_DUNIT)
	call clgstr ("in_col", in_col, SZ_LINE)
	verbose = clgetb ("verbose")

	# convert the distance to parallax in arcseconds
	if (strne (dist_unit, "arcsec")) {

	    # convert to parsec first
	    if (streq (dist_unit, "au"))
	    	parallax = parallax / AUPERPC
	    else if (streq (dist_unit, "ly"))
	    	parallax = parallax / LYPERPC
	    else if (streq (dist_unit, "km"))
	   	parallax = parallax / KMPERPC
	    else if (streq (dist_unit, "pc"))
	    	;
	    else
	    	call error (1, "illegal distance unit")
	    
	    # make sure the distance is non-zero (pc)
	    if (parallax <= 0.)
		call error (1, "non-positive distance")
	    parallax = 1. / parallax
	}	
end

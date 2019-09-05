# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"engextr.h"

#  engextr_in -- Read parameters for the task engextr.
#
#  Description:
#  ------------
#  
#  Input CL parameters:
#  -----------------
#
#  "infile"		Input file template name
#  "outtable"		Output table template name
#  "errval"		fill value in the output HEX column
#  "loc_execute"	pixel location of the execute command
#  "loc_infrequent"	pixel location of the infrequent command
#  "loc_prepare"	pixel location of the prepare/readout command
#  "loc_riua"		pixel location of the RIU A command
#  "loc_riub"		pixel location of the RIU B command
#  "verbose"		verbose messages on STDOUT?
#
#  Date		Author			Description
#  ----		------			-----------
#  23-May-1991  J.-C. Hsu		Coding
#------------------------------------------------------------------------------

procedure engextr_in (fin, outtbl, errval, loc, verbose)

				## outputs:
pointer	fin			# input file template pointer
char	outtbl[SZ_FNAME]	# output file template
int	errval			# fill value for error 
int	loc[NCMD]		# pixel location of the "flexible-address" 
				# commands
bool	verbose			# verbose messages on STDOUT?

int	i

pointer	imtopenp() 
int	clgeti()
bool	clgetb()
#==============================================================================
begin

	# open file templates 
	fin = imtopenp ("infile")

	# read other CL parameters
	call clgstr ("outtable", outtbl, SZ_FNAME)
        errval = clgeti ("errval")
        loc[EXEC] = clgeti ("loc_execute")
        loc[INFREQ] = clgeti ("loc_infrequent")
        loc[PREP] = clgeti ("loc_prepare")
        loc[RIUA] = clgeti ("loc_riua")
        loc[RIUB] = clgeti ("loc_riub")
        verbose = clgetb ("verbose")

	do i = 1, NCMD {
	   loc[i] = loc[i] + 1
	}
end

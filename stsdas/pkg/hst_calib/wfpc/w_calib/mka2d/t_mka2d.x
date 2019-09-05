include	"mka2d.h"

#  t_mka2d -- Generate a WFPC A-to-D conversion mapping file
#
#  "Though analogy is often misleading, it is the least misleading thing 
#    we have"
#						-Samuel Butler
#
#  Description:
#  ------------
#  Create or Modify correction table for WF/PC AtoD converter problem.
#
#  This task creates a new WF/PC AtoD Reference File (ADRF) or modifies an
#  an existing one. The lookup tables in the ADRF are used by the WF/PC 
#  Stage 1 pipeline (calwfp) to compensate for the pattern noise introduced
#  by the WF/PC's Analog-to-Digital (AtoD) converter.
#
#  Inputs:	Input File (existing ADRF or generic WF/PC image template)
#		Bay 3 Temperature (K)
#		An SDAS table containing the voltage bits errors 
#
#  Output:	New or updated AtoD Reference File (SDAS image format)
#		Optional summary of Temperatures currently in the file
#
#  AtoD Reference File Format:
#   	To facilitate the use of the WF/PC Stage 1 pipeline within PODPS, the
#  format of the ADRF is somewhat complicated. The ADRF is stored as a REAL 
#  image. Each row is 4096 elements in length (the range of possible values 
#  from the WF/PC 12 bit AtoD converter).
#  	Starting with the second row, each row contains the REAL values which
#  will be used to replace the WF/PC Data Numbers (DN). The row is used
#  as a "lookup table" where the value observed in a pixel is replaced
#  by the value in the lookup table with the following algorithm:
#
#		New Value = LOOKUP_TABLE ( DN + 1 )
#
#  	The first row is used to indirectly reference the lookup table
#  stored in rows 2 and greater. Each element in the first row contains
#  the Temperature (in degrees K) associated with the lookup table in 
#  the row at the index of that temperature. For example, if element 6 in
#  the first row contains 201.3, then row 6 in the image contains the
#  AtoD lookup table corresponding to a temperature of 201.3 K.
#  This task assumes that the ADRF is in STSDAS group format and that the
#  image contains four (4) groups. All groups of the ADRF are processed 
#  during a single execution of this task.
#
#  Date		Author			Description
#  ----		------			-----------
#  04-Oct-1991  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure t_mka2d ()

char	infile[SZ_FNAME], outfile[SZ_FNAME]
char	errtable[SZ_FNAME]
char	option[SZ_LINE]
real	b3temp
#==============================================================================
begin

	# get CL parameters
	call mka2d_in (infile, outfile, errtable, option, b3temp)

	# generate the A-to-D file
	call mka2d_do (infile, outfile, errtable, option, b3temp)

end

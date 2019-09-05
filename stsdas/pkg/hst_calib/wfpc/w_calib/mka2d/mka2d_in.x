include	"mka2d.h"

#  mka2d_in -- Get CL parameters for the task mka2d
#
#  Description:
#  ------------
#  
#  Input CL parameters:
#  -----------------
#  "infile"		Input A-to-D file name
#  "outfile"		output file containing the A-to-D corrections
#  "option"		Operation options
#  "errtable"		Name of the table containing bits errors
#  "bay3temp"		Bay 3 temperature
#
#  Date		Author			Description
#  ----		------			-----------
#  04-Oct-1991  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure mka2d_in (infile, outfile, errtable, option, b3temp)

char	infile[SZ_FNAME]	# input A-to-D file
char	outfile[SZ_FNAME]	# output A-to-D file
char	option[SZ_LINE]		# operation option
char	errtable[SZ_FNAME]	# input error table
real	b3temp			# Bay 3 temperature

int	k

int	nowhite()
int	strdic()
real	clgetr()
bool	streq()
bool	strne()
#==============================================================================
begin

	# get CL parameters
	call clgstr ("infile", infile, SZ_FNAME)
	call clgstr ("outfile", outfile, SZ_FNAME)
	call clgstr ("option", option, SZ_LINE)
	if (streq (option, "new") || streq (option, "add"))
	    call clgstr ("errtable", errtable, SZ_FNAME)
	if (strne (option, "list")) 
	    b3temp = clgetr ("bay3temp")

	call strlwr (option)
	k = nowhite (outfile, outfile, SZ_FNAME)

	# accept abbreviated options
	    k = strdic (option, option, SZ_LINE, "|add|new|list|delete|")
	    if (k == 0)
		call error (1, "illegal option")

	# if the option is delete or add and if the output file name is blank,
	# then the output is the same as input.
	if (outfile[1] == EOS || streq (outfile, ".")) {
	    if (streq (option, "add") || streq (option, "delete"))
		call strcpy (infile, outfile, SZ_FNAME)
	    else if (streq (option, "new"))
		call error (1, "empty output file name")
	}

	# if the option is new, output file can not be the same as input file
	if (streq (option, "new") && streq (infile, outfile))
	    call error (1, 
		"output file must be different from input file for option NEW")

end

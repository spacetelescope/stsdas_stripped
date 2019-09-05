#			File:	u_updtdgr.x
include "u_data.h"	# defines SZ_GKW, NUM_NKW, SZ_LONG_LINE
include "u_incl.h"	# defines files names and pointers, NGROUPS(cam)

################################################################################
#										
#  U_UPDT_DGR -- Update .DGR file by constructing a temporary file,		
#		 and then copying it into the original .DGR file. 		
#										
#  Revision History:								
#	11 Oct 93 by CYZhang	Initial implementation				
#	 9 Dec 93 by CYZhang	Reserve the file attributes of DGR file		
#===============================================================================
procedure u_updt_dgr (nam, ptr, cam)

# Input arguments
pointer	nam				# Pointer to NAMe structure
pointer	ptr				# Pointer to file pointer structure
pointer	cam				# Pointer to CAMera specifics

# Local variables
int	grpnum

# Functions called
int	u_cgrfile(), u_dgrfile()

begin

	#  Construct input  .DGR text filename
	call strcpy (IN_ROOT(nam), IN_DGR(nam), SZ_FNAME)
	call strcat (".dgr", IN_DGR(nam), SZ_FNAME)

	#  Create temporary text file
	call mktemp ("tmp$tmpdgr", TEMP(nam), SZ_FNAME)

	# Initialize the text file pointers
	IN_DGR_P(ptr)   = NULL
	OUT_GRP_P(ptr)  = NULL
	TEMP_P(ptr)     = NULL

	# Open the text files, cgr file open as READ_ONLY
	# N.B., u_dgrfile sets file mode to READ_WRITE, while u_grpfile
	# sets it to NEW_FILE
	OUT_GRP_P(ptr) = u_cgrfile (OUT_GRP(nam))
	IN_DGR_P(ptr)  = u_dgrfile (IN_DGR(nam))

	# Preserve the file attributes of dgr file using fmkcopy
	# -- 8/12/93 CYZhang
	call fmkcopy (IN_DGR(nam),TEMP(nam))
	TEMP_P(ptr)    = u_dgrfile (TEMP(nam))

	# Update .DGR file
	do grpnum = 1, NGROUP(cam) {
	    call u_getdgr (IN_DGR_P(ptr), OUT_GRP_P(ptr),
			   TEMP_P(ptr), grpnum)
	}
	if (OUT_GRP_P(ptr) != NULL)                	# Close .CGR file
	    call u_close   (OUT_GRP_P(ptr))
	if (IN_DGR_P(ptr) != NULL)                 	# Close .DGR file
	    call u_close   (IN_DGR_P(ptr))
	if (TEMP_P(ptr) != NULL)                   	# Close TEMP file
	    call u_close   (TEMP_P(ptr))
	call rename (TEMP(nam), IN_DGR(nam))
	
end

#  U_GETDGR --	Get a line from .DGR file, and compare the first word in the	
#		line with a set of keyword names (image section statistics of	
#		raw image computed by PODPS). If the first word does match 	
#		a keyword name in the string "list", that line from .DGR will	
#		be copied to a temporary file. Get a line from .CGR file, and	
#		if the first word does not match any of the names in the string	
#		"list", that line will be copied to the temporary file		
#										
#  Revision History:								
#      11 Oct 93 by CYZhang	Initial implementation				
#-------------------------------------------------------------------------------
procedure u_getdgr (fpdgr, fpcgr, fptmp, element)

# Calling arguments	
pointer fpdgr			# i: pointer to .DGR file
pointer fpcgr			# i: pointer to .CGR file
pointer fptmp			# i: pointer to a temp file
int	element			# i: chip number

# Local variables	
int	i			# Loop index
int	lstart, lend		# Srart and end of all kw for one chip
pointer	lbufd		 	# Buffer of the input line from .DGR
char	textd[SZ_GKW]		# String of Group Keyword from .DGR
pointer	lbufc		 	# Buffer of the input line from .CGR
char	textc[SZ_GKW]		# String of Group Keyword from .CGR
int	status
int	ip			# Index of the words
pointer	sp			# Pointer to stack

string 	list "MEDIAN_1,MEDSHADO_1,HISTWIDE_1,SKEWNESS_1,MEANC10_1,\
	      MEANC25_1,MEANC50_1,MEANC100_1,MEANC200_1,MEANC300_1,\
	      BACKGRND_1,MEDIAN_2,MEDSHADO_2,HISTWIDE_2,SKEWNESS_2,\
	      MEANC10_2,MEANC25_2,MEANC50_2,MEANC100_2,MEANC200_2,\
	      MEANC300_2,BACKGRND_2,MEDIAN_3,MEDSHADO_3,HISTWIDE_3,\
 	      SKEWNESS_3,MEANC10_3,MEANC25_3,MEANC50_3,MEANC100_3,\
 	      MEANC200_3,MEANC300_3,BACKGRND_3,MEDIAN_4,MEDSHADO_4,\
 	      HISTWIDE_4,SKEWNESS_4,MEANC10_4,MEANC25_4,MEANC50_4,\
  	      MEANC100_4,MEANC200_4,MEANC300_4,BACKGRND_4"

# Functions called	
int	getlline(), ctowrd(), word_match()

begin
	
	call smark (sp)
	call salloc (lbufd, SZ_LONG_LINE, TY_CHAR)
	call salloc (lbufc, SZ_LONG_LINE, TY_CHAR)

	# Get lines for the element
	lstart = 1 + (element - 1) * NUM_GKW
	lend   = element * NUM_GKW
	do i = lstart, lend {

	    # Match a line from .CGR
	    status = getlline (fpcgr, Memc[lbufc], SZ_LONG_LINE)
	    ip = 1
	    if (ctowrd (Memc[lbufc], ip, textc, SZ_GKW) < 1)

		# textc has a size of SZ_GKW + 1 with EOS, ip = 11
		call u_error ("Could not read keyword from .CGR file")
	    if (word_match (textc, list) <= 0)
		call putline (fptmp, Memc[lbufc])

	    # Match a line from .DGR
	    status = getlline (fpdgr, Memc[lbufd], SZ_LONG_LINE)
	    ip = 1
	    if (ctowrd (Memc[lbufd], ip, textd, SZ_GKW) < 1)

		# textd has a size of SZ_GKW + 1 with EOS, ip = 11
		call u_error ("Could not read keyword from .DGR file")
	    if (word_match (textd, list) >= 1)
		call putline (fptmp, Memc[lbufd])
	}

	call sfree (sp)
end

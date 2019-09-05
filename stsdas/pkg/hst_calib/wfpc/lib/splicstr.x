###############################################################################
#									
#  SPLICSTR --	This routine is used to generate the DQF diskfile name from 
#		the datafile name.					
#									
#	7/91 RAShaw	Initial code					
#	10/91 RAShaw	Error trap for substring not found
#	11/95 JCHsu	modify the error trap condition
#	03/2003 JCHsu	generalize the substitution to be applicable to FITS 
#			files.

procedure splicstr (inimage, outimage, filextn1, filextn2)

# Calling arguments:
char	inimage[SZ_FNAME]		# Data filename
char	outimage[SZ_FNAME]		# DQF filename
char	filextn1[SZ_FNAME]		# Data file extension (often "c0h")
char	filextn2[SZ_FNAME]		# DQF file extension (often "c1h")

# Local variables:
int	first				# Begining index of datafile extn string
int	i, junk				# Dummies
int	last				# End index of datafile extn string
int	nchar				# Size of datafile string
char	text[SZ_LINE]

# Functions called:
int	gstrmatch ()			# Substring matching
int	strlen ()			# Length of string

begin
	nchar = strlen (inimage)
	junk  = gstrmatch (inimage, filextn1, first, last)

	# Protect against "filextn1" string not found in "inimage" string
	# modified by JC Hsu, 11/95
	if (junk == 0) {
	#if (first >= last)
	    call sprintf (text, SZ_LINE, 
			  "datextn '%s' not found in input image name '%s'")
		call pargstr (filextn1)
		call pargstr (inimage)
	    call error (1, text)
	} else {
	    call strcpy (inimage, outimage, first-1)

	    # modified by JC Hsu 3/2003 to make the substitution more general 
	    # to be appicable to FITS files.
	    #do i = 0, 2
	    do i = 0, strlen(filextn1)
		outimage[first+i] = filextn2[i+1]

	    do i = last+1, nchar+1
		outimage[i] = inimage[i]
	}
end

include	<ctotok.h>
include	"mkphottb.h"

#  MKPH_DISSECT -- Dissect the input photmode string into components
#
#  Description:
#  ------------
#  Dissect the photmode string into its components: camera, detector, filter
#  names.
#
#  Date		Author			Description
#  ----		------			-----------
#  08-Aug-1991  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

procedure mkph_dissect (photmode, camera, detector, filtnam1, filtnam2, flat)

char	photmode[SZ_PHOTMODE]		# input: time string
char	camera[SZ_CAMERA]
int	detector
char	filtnam1[SZ_FILTNAM1]
char	filtnam2[SZ_FILTNAM2]
char	flat[SZ_FLAT]

int	i, j, k, ip
char	tmpstr[SZ_PHOTMODE]

int	ctotok()
int	ctoi()
#==============================================================================
begin
	ip = 1

	# the first component should be a camera name
	if (ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) != TOK_IDENTIFIER)
	    call error (1, "") 
	call strcpy (tmpstr, camera, SZ_CAMERA)
	    
	# should be followed by a comma
	if (ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) != TOK_PUNCTUATION)
	    call error (1, "") 
	    
	# the second component should be a detector number
	if (ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) != TOK_NUMBER)
	    call error (1, "") 
	j = 1
	i = ctoi (tmpstr, j, detector)

	# should be followed by a comma
	if (ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) != TOK_PUNCTUATION)
	    call error (1, "") 
	    
	# the third component should be the data mode 
	# this component is not in the final output
	if (ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) != TOK_IDENTIFIER)
	    call error (1, "") 
	    
	# should be followed by a comma
	if (ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) != TOK_PUNCTUATION)
	    call error (1, "") 
	    
	# the fourth component should be the unit field (e.g. DN) 
	# this component is not in the final output
	if (ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) != TOK_IDENTIFIER)
	    call error (1, "") 
	    
	# should be followed by a comma
	if (ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) != TOK_PUNCTUATION)
	    call error (1, "") 
	    
	# the fifth component should be the first filter name
	if (ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) != TOK_IDENTIFIER)
	    call error (1, "") 
	call strcpy (tmpstr, filtnam1, SZ_FILTNAM1)
	    
	# should be followed by a comma
	if (ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) != TOK_PUNCTUATION)
	    call error (1, "") 
	    
	# the sixth component should be the second filter name
	if (ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) != TOK_IDENTIFIER)
	    call error (1, "") 
	call strcpy (tmpstr, filtnam2, SZ_FILTNAM2)

	# if there is nothing after the second filter name, set flat to NULL
	# otherwise, copy the string to be the flatfield setting
	k = ctotok (photmode, ip, tmpstr, SZ_PHOTMODE) 
	if (k == TOK_EOS)
	    flat[1] = EOS
	else if (k == TOK_PUNCTUATION) {
	    k = ctotok (photmode, ip, tmpstr, SZ_PHOTMODE)
	    call strcpy (tmpstr, flat, SZ_FLAT)
	} else
	    call error (1, "") 
end

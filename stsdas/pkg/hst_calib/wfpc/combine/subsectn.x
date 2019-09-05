include	<ctype.h>

#################################################################################
# SECTION --	Parse an image section into its elements.  The default values 	#
#		must be set by the caller, but a null image section is OK.  	#
#		The first nonwhitespace character must be '[', and the last 	#
#		interpreted character must be ']'.  This procedure should be 	#
#		replaced with an IMIO procedure at some point.  This routine	#
#		is taken from the `images.imcombine' package.			#
#										#
#		Development version:	11/90	RAShaw				#

procedure subsectn (section, x1, x2, xs, ndim)

# Calling arguments:
char		section[ARB]		# Image section
int		x1[ndim]		# Starting pixel
int		x2[ndim]		# Ending pixel
int		xs[ndim]		# Step
int		ndim			# Number of dimensions

# Local variables:
int		a, b, c			# 
int		i, ip			# 
int		temp			# 

# Function used:
int		ctoi()			# Convert character number to integer
define	error_	99

begin

# Decode the section string.
	ip = 1
	while (IS_WHITE(section[ip]))
	    ip = ip + 1
	if (section[ip] == '[')
	    ip = ip + 1
	else if (section[ip] == EOS)
	    return
	else
	    goto error_

	do i = 1, ndim {
	    while (IS_WHITE(section[ip]))
	        ip = ip + 1
	    if (section[ip] == ']')
		break

# Set default values
	    a = x1[i]
	    b = x2[i]
	    c = xs[i]

# Get a:b:c.  Allow notation such as "-*:c" (or even "-:c") where the step 
# is obviously negative. 

	    if (ctoi (section, ip, temp) > 0) {			# a
		a = temp
	        if (section[ip] == ':') {	
		    ip = ip + 1
		    if (ctoi (section, ip, b) == 0)		# a:b
		        goto error_
	        } else
		    b = a
	    } else if (section[ip] == '-') {			# -*
		temp = a
		a = b
		b = temp
	        ip = ip + 1
	        if (section[ip] == '*')
		    ip = ip + 1
	    } else if (section[ip] == '*')			# *
	        ip = ip + 1
	    if (section[ip] == ':') {				# ..:step
	        ip = ip + 1
	        if (ctoi (section, ip, c) == 0)
		    goto error_
	        else if (c == 0)
		    goto error_
	    }
	    if (a > b && c > 0)
	        c = -c

	    x1[i] = a
	    x2[i] = b
	    xs[i] = c

	    while (IS_WHITE(section[ip]))
	        ip = ip + 1
	    if (section[ip] == ',')
		ip = ip + 1
	}

	if (section[ip] != ']')
	    goto error_

	return
error_
	call error (0, "Error in image section specification")
end

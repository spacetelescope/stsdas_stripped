include	<math.h>

#  OBJECT_POS -- Calculate the unit state vector of the target
#
#  Description:
#  ------------
#  Convert right ascension and declination to a unit vector in rectangular 
#  coordinates. 
#
#  Date		Author			Description
#  ----		------			-----------
#  12-Nov-1984	C. D. Biemesderfer	Original module
#  30-Mar-1990	J.-C. Hsu		rewrite in SPP
#  19-Aug-1996	J.-C. Hsu		get rid of the proper motion
#------------------------------------------------------------------------------

procedure object_pos (ra, dec, objposn)

double	ra		# input: right ascension of target (degs)
double	dec		# input: declination of target (degs)
double	objposn[3]	# output: Obj coords corrected for prop mtn

double	cosdec		# Cosine of declination
#==============================================================================
begin
	# Convert true coordinates to unit Cartesian
	cosdec = cos(dec/RADIAN)
 
	objposn[1] = cosdec * cos(ra/RADIAN)
	objposn[2] = cosdec * sin(ra/RADIAN)
	objposn[3] = sin(dec/RADIAN)
end

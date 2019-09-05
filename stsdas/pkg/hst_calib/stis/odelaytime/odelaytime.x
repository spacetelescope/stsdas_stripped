#  ODELAYTIME -- Correct for light delay time for STIS data
#
#  Description:
#  ------------
#  The orbital motions of the earth and the telescope on the observed
#  times may be compensated for by referring the times to the solar-system
#  barycenter.  This routine creates a file of times of observation cor-
#  rected for the light delay from three sources :  (1) general relativistic
#  effects (up to 2 millisec),  (2) the displacement of the telescope from the 
#  center of the earth (up to 20 millisec),  (3) and the displacement of the 
#  earth from the solar-system barycenter (up to 500 seconds).  
#
#  This task is capable of processing multiple files.  
#
#  The maximum time delay due to proper motions is 2.4 milliseconds per arc 
#  second of position displacement and 1.2 milliseconds per arc second of 
#  parallax.
#
#  It is assumed that the object under observation is well beyond the
#  solar system.  For a nearby object it would perhaps make more sense to
#  refer the times of observation to the object itself.  
#
#  Date		Author		Description
#  ----		------		-----------
#  05-Nov-1984	C. D. Biemesderfer	Original module
#  18-Apr-1990  J.-C. Hsu	rewrite in SPP
#  19-Aug-1997  J.-C. Hsu	modify for STIS data
#  14-Aug-2000  Phil Hodge	remove out_col parameter, and
#				add verbose parameter; also update
#				GTI extension and header keywords
#  10-Jun-2003  Phil Hodge	change calling sequence (ephem table names
#				instead of table pointers)
#------------------------------------------------------------------------------

procedure odelaytime ()

pointer	fin			# file template pointer
int	nfin 			# number of files in the input template
double	parallax		# parallax of the target (in arc sec)
char	earth_ephem[SZ_FNAME]	# earth ephemeris table name
char	obs_ephem[SZ_FNAME]	# list of observer ephemeris table names
char	in_col[SZ_LINE]		# time column name
bool	verbose			# print timing info?
#==============================================================================
begin
	# read input parameters
	call odelay_get (fin, nfin, parallax, earth_ephem, obs_ephem, SZ_FNAME,
		in_col, verbose)

	# do the delay time calculations
	call odelay_do (fin, nfin, parallax, earth_ephem, obs_ephem,
		in_col, verbose)

	# close file templates
	call imtclose (fin)
end

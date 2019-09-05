include "streakflat.h"

#  t_streakflat -- construct a WFPC flat field from streak exposures
#
#  "The stars, like measles, fade at last."
#						-Samuel Hoffenstein
#
#  Description:
#  ------------
#
#  Date		Author		Description
#  ----		------		-----------
#  05-19-92	J.-C. Hsu	V 1.0: Adapt from Jeff Hester's C code.
#  12-29-92	J.-C. Hsu	V 1.1: Add good_points as a new parameter. 
#				Modify the routines flat_do.x and flat_check.x 
#				to open a tmp file as NEW_COPY or READ_WRITE 
#				when writing to it then close and reopen it as 
#				READ_ONLY when needed in the routine 
#				flat_median.x.  This reduces the memory needed 
#				to keep all tmp files opened as NEW_COPY.
#  01-02-93	J.-C. Hsu	V 1.2: Use a faster sort routine and other speed
#				improvements.
#  04-22-94	J.-C. Hsu	V 1.3: Fix a bug in flat_smooth.x to make sure
#				the boxcar smoothing covers the entire frame.  
#  01-16-95	J.-C. Hsu	V 1.4: Modify the code to be able to run WFPC2
#				images.
# ------------------------------------------------------------------------------

procedure t_streakflat ()

pointer	fin 			# file template pointer
char	fout[SZ_FNAME]
int	iter
int	width[MAX_ITER]
int	ngood
bool	verbose
#==============================================================================
begin

	# get CL parameters and related quantities
	call flat_in (fin, fout, iter, width, ngood, verbose)
 
	# announce start of the task
	call printf ("*** STREAKFLAT - Version %s ***\n")
	    call pargstr (VERSION)

	# perform the calculation
	call flat_do (fin, fout, iter, width, ngood, verbose)

	# close file template
	call imtclose (fin)

	# announce completion of task
	call printf ("streak flat task completed with no error\n")
end

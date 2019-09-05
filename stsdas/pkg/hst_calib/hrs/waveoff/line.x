include <imhdr.h>
include	<tbset.h>

# The Line List descriptor.
define	LL_nlines		Memi[$1]
define	LL_waves_ptr		Memi[$1+1]
define	LL_waves_null_ptr 	Memi[$1+2]
define	LL_int_ptr		Memi[$1+3]
define	LL_sort_ptr		Memi[$1+4]
define	SZ_LL		5

define	LL_waves		Memd[LL_waves_ptr($1)+$2-1]
define	LL_waves_null		Memb[LL_waves_null_ptr($1)+$2-1]
define	LL_int			Memd[LL_int_ptr($1)+$2-1]
define	LL_sort			Memi[LL_sort_ptr($1)+$2-1]
define	LL_sort_waves		LL_waves($1,LL_sort($1,$2))
define	LL_sort_int		LL_int($1,LL_sort($1,$2))

# Memory management.
define	Null		Memb[null]
define	Sx		Memc[sx]

#---------------------------------------------------------------------------
.help line Nov93 source
.ih
NAME
line -- Manage a line list.
.endhelp
#---------------------------------------------------------------------------
pointer	procedure wo_alloc_line (file)

char	file[ARB]		# I:  Name of the line list table.

pointer	col			# Column descriptor.
int	i			# Generic.
pointer	ll			# Line list descriptor.
pointer	null			# Null column holder.
pointer	sp			# Stack pointer.
pointer	sx			# Generic string.
pointer	table			# Line list table.
pointer	tbtopn()		# Open a table.
int	tbpsta()		# Get table information.
int	wo_comp_line()		# How to compare lines.
extern	wo_comp_line

common	/wo_common/	ll

begin
	call smark (sp)
	call salloc (sx, SZ_LINE, TY_CHAR)
	
	# Allocate the descriptor.
	call malloc (ll, SZ_LL, TY_STRUCT)

	# Open the table.
	table = tbtopn (file, READ_ONLY, NULL)

	# Get the number of lines.
	LL_nlines(ll) = tbpsta (table, TBL_NROWS)

	# Get the wavelengths.
	call clgstr ("wavelength_col", Sx, SZ_LINE)
	call tbcfnd (table, Sx, col, 1)
	if (col == NULL) {
	    call eprintf ("ERROR: wo_alloc_line: Table %s does not contain column %s.\n")
	    call pargstr (file)
	    call pargstr (Sx)
	    call error (1, "Could not read in PtNe line list")
	}
	call malloc (LL_waves_ptr(ll), LL_nlines(ll), TY_DOUBLE)
	call malloc (LL_waves_null_ptr(ll), LL_nlines(ll), TY_BOOL)
	call tbcgtd (table, col, LL_waves(ll,1), LL_waves_null(ll,1),
		     1, LL_nlines(ll))

	# Get the intensities.
	call malloc (LL_int_ptr(ll), LL_nlines(ll), TY_DOUBLE)
	call salloc (null, LL_nlines(ll), TY_BOOL)
	call clgstr ("intensity_col", Sx, SZ_LINE)
	call tbcfnd (table, Sx, col, 1)
	if (col == NULL) {
	    call eprintf ("WARNING: wo_alloc_line: Table %s\n   does not contain column %s.\n")
	    call pargstr (file)
	    call pargstr (Sx)
	    call eprintf ("    Intensities will be uniformly set to 1.")

	    call amovkd (1.0d0, LL_int(ll,1), LL_nlines(ll))
	}  else
	    call tbcgtd (table, col, LL_int(ll,1), Null, 1, LL_nlines(ll))

	# Create the sorted list of wavelengths.
	call malloc (LL_sort_ptr(ll), LL_nlines(ll), TY_INT)
	do i = 1, LL_nlines(ll)
	    LL_sort(ll,i) = i
	call qsort (LL_sort(ll,1), LL_nlines(ll), wo_comp_line)

	# That's all folks
	call tbtclo (table)
	call sfree (sp)
	return (ll)
end
#---------------------------------------------------------------------------
# End of wo_alloc_line
#---------------------------------------------------------------------------
procedure wo_free_line (ll)

pointer	ll			# IO: Line list descriptor.

begin
	call mfree (LL_sort_ptr(ll), TY_INT)
	call mfree (LL_int_ptr(ll), TY_DOUBLE)
	call mfree (LL_waves_null_ptr(ll), TY_BOOL)
	call mfree (LL_waves_ptr(ll), TY_DOUBLE)

	call mfree (ll, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of wo_free_line
#---------------------------------------------------------------------------
int procedure wo_comp_line (r1, r2)

int	r1, r2			# I:  Row 1,2 to compare.

pointer ll			# Line list descriptor.

common	/wo_common/	ll

begin
	if (LL_waves(ll,r1) < LL_waves(ll,r2))
	    return (-1)
	else if (LL_waves(ll,r1) == LL_waves(ll,r2))
	    return (0)
	else
	    return (1)
end
#---------------------------------------------------------------------------
# End of wo_comp_line
#---------------------------------------------------------------------------
procedure wo_pred_obs (wave, len, ll, pred_obs)

double	wave[len]		# I:  Wavelength vector.
int	len			# I:  Length of wavelength/pred_obs vector.
pointer	ll			# I:  Line table descripter.
double	pred_obs[len]		# O:  The predicted spectrum.

int	cur_x			# Current position.
int	first			# Index of smallest wavelength.
int	i			# Generic.
int	incr			# Increment along the wavelength vector.
int	last			# Index of largest wavelength.
pointer	sp			# Stack pointer.

begin
	call smark (sp)

	# Confirm that the wavelength vector is monotonic.  It can be
	# increasing or decreasing.
	if (wave(1) <= wave(len)) {
	    incr = 1
	    first = 1
	    last = len
	} else {
	    incr = -1
	    first = len
	    last = 1
	}
	do i = first, last-incr, incr
	    if (wave(i) > wave(i+incr))
		call error (1, "waveoff: wavelengths are not monotonic")
	       
	# For each line in the line table, find its position in the wavelength
	# array.
	call aclrd (pred_obs, len)
	if (incr > 0)
	    cur_x = 2
	else
	    cur_x = len - 1
	do i = 1, LL_nlines(ll) {
	    if (LL_sort_waves(ll,i) > wave[last])
		break
	    if (LL_sort_waves(ll,i) < wave[first])
		next
	    while (LL_sort_waves(ll,i) > wave[cur_x] && cur_x < len && cur_x > 1)
	    cur_x = cur_x + incr
	    if (cur_x == len || cur_x == 1)
		break
	    
	    if (LL_sort_waves(ll,i) < (wave[cur_x-incr]+wave[cur_x])/2.0d0)
		pred_obs[cur_x-incr] = LL_sort_int(ll,i)
	    else
		pred_obs[cur_x] = LL_sort_int(ll,i)
	}

	# That's all folks.
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of wo_pred_obs
#---------------------------------------------------------------------------

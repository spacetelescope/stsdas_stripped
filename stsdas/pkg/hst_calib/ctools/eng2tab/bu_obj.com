#---------------------------------------------------------------------------
.help bu_obj.com May94 source
.ih
NAME
bu_obj.com -- Common block for expression evaluation.
.endhelp
#---------------------------------------------------------------------------

pointer	data_ptr		# Pointer to raw data array.
int	n_pts			# Number of points in array.
pointer	f_val			# Unformated value.

common	/bu_obj/	data_ptr, n_pts, f_val
#---------------------------------------------------------------------------
# End of bu_obj.com
#---------------------------------------------------------------------------

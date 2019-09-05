# PIXFIT.COM -- Common block used for inverting fitted coordinates

double	sv_soln[MAX_SOLUTION,2]		# The least squares solution vector
double	sv_point[2]			# The point being inverted
pointer	sv_frm				# The saved frame descriptor
int	sv_maxsol			# The length of the solution vector
int	sv_axis				# The axis being solved for

common /pixfit/ sv_soln, sv_point, sv_frm, sv_maxsol, sv_axis
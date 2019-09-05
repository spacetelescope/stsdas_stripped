include <gset.h>

procedure axmarg (gd)

pointer	gd				# Graphics descriptor

real	margin				# Margin between curve and axis
					# (fraction of window)
real	left, right, bottom, top	# Window
real	dd

begin
	margin = 0.05

	call ggwind (gd, left, right, bottom, top)

	#dd = margin * (right - left)
	#left = left - dd
	#right = right + dd

	dd = margin * (top - bottom)
	bottom = bottom - dd
	top = top + dd

	call gswind (gd, left, right, bottom, top)

end

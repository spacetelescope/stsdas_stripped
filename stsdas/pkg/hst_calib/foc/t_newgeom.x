include <imhdr.h>

define	NEWGEOM_SET_VALUES	1
define	NEWGEOM_APPLY_DIST	2
define	NEWGEOM_NONE		3

# t_newgeom -- task to perform geometric correction
#
# Robert Jedrzejewski		Original.
# Phil Hodge,  9-Aug-1991	Get defval and pass to newgeom; update geocorr.
# Phil Hodge, 29-May-1992	Include chg_coords and pass to newgeom.
# Phil Hodge, 25-Jan-1993	Include common block xcroot.
# Phil Hodge,  7-Apr-1994	Change parameter chg_coords to coord_trans.

procedure t_newgeom()

pointer input		# scratch for name of input image
pointer output		# scratch for name of output (corrected) image
pointer geo		# scratch for name of geometric correction file
pointer s_coord_trans	# scratch for value of coord_trans (ingored)
real	defval		# default value for output
bool	box_check	# check for errors in computing pixel overlap?
bool	verbose		# print line numbers as we go?
#--
pointer sp
pointer geopt, p_im, g_im	# imhdr pointers for images
int	coord_trans		# how shall we modify coordinate parameters?
bool	set_values		# set coord param based on GEO header?
bool	apply_dist		# modify coord param based on geo distortion?
pointer immap()
real	clgetr()
int	clgwrd()
bool	clgetb()

# This common block is used by logmsg, which is called by several routines
# used by newgeom.  In the pipeline (see calfoc.x) logmsg puts the rootname
# of the observation in front of every message.  We don't need to do that
# in newgeom, but we do need to assign something (a null string) to this
# common block variable.
char	rootname[SZ_FNAME]	# root name of the observation
common	/xcroot/ rootname	# root name, to be included in message

begin
	rootname[1] = EOS		# for logmsg; see explanation above

	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (geo, SZ_FNAME, TY_CHAR)
	call salloc (s_coord_trans, SZ_FNAME, TY_CHAR)

	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("geo_file", Memc[geo], SZ_FNAME)
	defval = clgetr ("defval")
	coord_trans = clgwrd ("coord_trans", Memc[s_coord_trans], SZ_FNAME,
			"|set_values|apply_distortion|none")
	box_check = clgetb ("box_check")
	verbose = clgetb ("verbose")

	p_im = immap (Memc[input], READ_ONLY, NULL)
	g_im = immap (Memc[output], NEW_COPY, p_im)
	geopt = immap (Memc[geo], READ_ONLY, NULL)

	IM_PIXTYPE(g_im) = TY_REAL

	set_values = (coord_trans == NEWGEOM_SET_VALUES)
	apply_dist = (coord_trans == NEWGEOM_APPLY_DIST)
	call newgeom (p_im, g_im, geopt, defval,
			set_values, apply_dist, box_check, verbose)

	call imastr (g_im, "geocorr", "COMPLETE")

	call imunmap (geopt)
	call imunmap (g_im)
	call imunmap (p_im)

	call sfree (sp)
end

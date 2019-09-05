# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
#  T_PIXCOORD -- Compute star coordinates in pixels from ra and dec
#
# B.Simon	22-Jun-90	Original

procedure t_pixcoord ()

#--
pointer	script		# Name of script to run
pointer	par		# Task parameter structure
pointer	pos		# Catalog position structure
pointer	im		# Image descriptor
pointer	frm		# Coordinate frame structure
pointer	mw		# World coordinate structure

pointer	sp

int	word_count()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (script, SZ_FNAME, TY_CHAR)

	# Read task parameters

	call init_param (par)
	call clgstr ("script", Memc[script], SZ_FNAME)

	# Open the catalog file and read into position structure

	call ini_pos (par, pos)
	call update_pos (par, pos, im, frm)

	# Enter command processing loop

	if (word_count (Memc[script]) == 0) {
	    call pixdisplay (par, pos)
	    call pixcursor (par, pos, im, frm)
	} else {
	    call script_cmd (Memc[script], par, pos, im, frm)
	}

	# Release data structures

	call cls_frame (frm, mw)
	call mw_close (mw)
	call gf_unmap (im)

	call free_param (par)
	call free_pos (pos)
	call sfree (sp)

end


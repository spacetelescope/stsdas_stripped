include	<gset.h>
include "pixpos.h"

define	ROW_LABEL	1
define	NAME_LABEL	2

# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# PIXDISPLAY -- Display image and catalog star positions
#
# B.Simon	20-Jun-90	Original
# B.Simon	09-Aug-91	Updated with more options

procedure pixdisplay (par, pos)

pointer	par		# i: Parameter descriptor
pointer	pos		# i: Position descriptor
#--
int	tvframe, mksize, labtype, junk, ic
int	index, nstar, xlen, ylen, xrow, yrow, istar, jstar
pointer	sp, tag, image, tvcmd, label, mkcolor, device, sizestr
pointer	stdimage, params, command
pointer	im, tty, gp, xold, yold, itag
real	mkdimen, xmax, ymax

string	clist    "white,red,green,blue,yellow"
string	devlist  "imdw,imdr,imdg,imdb,imdy"
string	lablist  "|rownumber|name|"

string	nullstr    ""
string	ljustify   "h=l,v=c"
string	rjustify   "h=r,v=c"
string	badimage   "Cannot read image (%s)"
string  badsize    "Marker size < 0 (%s)"
string  badcolor   "Bad marker color (%s)"
string  badlabel   "Bad label type (%s). Legal types are rownumber, name."
string	badposcol  "Cannot read position column (%s)"
string	baddisplay "Warning: display size (%d,%d) != image size (%d,%d)"

int	word_match(), strdic(), gstrcpy(), envgets(), ttygeti(), itoc()
int	rdindex_pos(), rdrcol_pos(), rdtcol_pos()
pointer	gf_map(), gopen(), ttygdes()

begin
	call putflag_param (par, "tv", NO)

	call smark (sp)
	call salloc (tag, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (tvcmd, SZ_FNAME, TY_CHAR)
	call salloc (label, SZ_FNAME, TY_CHAR)
	call salloc (mkcolor, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (sizestr, SZ_FNAME, TY_CHAR)
	call salloc (stdimage, SZ_FNAME, TY_CHAR)
	call salloc (params, SZ_LINE, TY_CHAR)
	call salloc (command, SZ_LINE, TY_CHAR)

	# Read the image name and check if it can be opened

	call rdstr_param (par, "image", Memc[image], SZ_FNAME)

	iferr (im = gf_map (Memc[image], READ_ONLY, 0)) {
	    call pixmessage (badimage, Memc[image])
	    call rest_param (par, "image")
	    return
	}
	call gf_unmap (im)

	# Read label type and marker size

	call rdstr_param (par, "label", Memc[label], SZ_FNAME)

	labtype = strdic (Memc[label], Memc[label], SZ_FNAME, lablist)
	if (labtype == 0) {
	    call pixmessage (badlabel, Memc[label])
	    call rest_param (par, "label")
	    return
	}

	call rdint_param (par, "mksize", mksize)
	if (mksize <= 0) {
	    call sprintf (Memx[sizestr], SZ_FNAME, "%d")
	    call pixmessage (badsize, Memc[sizestr])
	    call rest_param (par, "mksize")
	    return
	}

	# Get the device corresponding to the cursor color

	call rdstr_param (par, "mkcolor", Memc[mkcolor], SZ_FNAME)

	index = word_match (Memc[mkcolor], clist)
	if (index == 0) {
	    call pixmessage (badcolor, Memc[mkcolor])
	    call rest_param (par, "mkcolor")
	    return
	}

	call word_find (index, devlist, Memc[device], SZ_FNAME)

	# Build the command to display the image

	call rdstr_param (par, "tvcmd", Memc[tvcmd], SZ_FNAME)
	call rdint_param (par, "tvframe", tvframe)

	call aclrc (Memc[params], SZ_LINE)
	
	ic = gstrcpy (Memc[image], Memc[params], SZ_FNAME) + 1
	junk = itoc (tvframe, Memc[params+ic], SZ_FNAME)

	call strmac (Memc[tvcmd], Memc[params], Memc[command], SZ_LINE)
	call clcmdw (Memc[command])

	# Compare the display size to the image size 
	# and print a warning message if they are different

	call rdipar_pos (pos, "xlen", xlen)
	call rdipar_pos (pos, "ylen", ylen)
	if (envgets ("stdimage", Memc[stdimage], SZ_FNAME) > 0) {
	    tty = ttygdes (Memc[stdimage])
	    xrow = ttygeti (tty, "xr")
	    yrow = ttygeti (tty, "yr")
	    call ttycdes (tty)
	    if (xrow != xlen || yrow != ylen) {
		call sprintf (Memc[command], SZ_LINE, baddisplay)
		call pargi (xlen)
		call pargi (ylen)
		call pargi (xrow)
		call pargi (yrow)
		call pixmessage (Memc[command], nullstr)
	    }
	}

	# Plot boxes at each star position on the image

	call rdipar_pos (pos, "nstar", nstar)
	call salloc (xold, nstar, TY_REAL)
	call salloc (yold, nstar, TY_REAL)
	call salloc (itag, nstar, TY_INT)

	if (rdrcol_pos (pos, "xold", FLAG_OFF, jstar, 
			Memr[xold], nstar) == ERR)
	    call pixerror (badposcol, "xold")

	if (rdrcol_pos (pos, "yold", FLAG_OFF, jstar, 
			Memr[yold], nstar) == ERR)
	    call pixerror (badposcol, "yold")

	if (labtype == ROW_LABEL) {
	    if (rdindex_pos (pos, FLAG_OFF, jstar, Memi[itag], nstar) == ERR)
	        call pixerror (badposcol, "index")
	} else {
	    if (rdtcol_pos (pos, "name", FLAG_OFF, jstar, 
			    Memi[itag], nstar) == ERR)
		call pixerror (badposcol, "name")
	}

	xmax = xlen
	ymax = ylen
	mkdimen = - mksize

	gp = gopen (Memc[device], NEW_FILE, STDGRAPH)

	call gsetr (gp, G_TXSIZE, 0.75)
	call gswind (gp, 0.0, xmax, 0.0, ymax)

	do istar = 1, jstar {
	    call gmark (gp, Memr[xold], Memr[yold], GM_BOX, mkdimen, mkdimen)

	    if (labtype == ROW_LABEL) {
		junk = itoc (Memi[itag], Memc[tag], SZ_FNAME)
	    } else if (labtype == NAME_LABEL) {
		call strcpy (Memc[Memi[itag]], Memc[tag], SZ_FNAME)
	    } else {
		Memc[tag] = EOS
	    }

	    if (Memr[xold] < xmax / 2.) {
		call gtext (gp, Memr[xold]+((mksize+1)/2), Memr[yold], 
			    Memc[tag], ljustify)
	    } else {
		call gtext (gp, Memr[xold]-((mksize+1)/2), Memr[yold], 
			    Memc[tag], rjustify)
	    }

	    xold = xold + 1
	    yold = yold + 1
	    itag = itag + 1
	}

	call gclose (gp)
	call sfree (sp)

end

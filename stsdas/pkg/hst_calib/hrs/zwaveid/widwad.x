include	<gset.h>
include	<pkg/gtools.h>
include	"wid.h"
include	"line.h"
include	"../mkwave/mkw.h"

# Define color
define	GTCOLOR		23

# Colon commands.
define	WID_COLON_CMDS	",no_dups,sigma,subsample,continuum,slope,temperature,rshift,fnu,z,nsigma,width,radius,threshold,offset,units,xcor"
define	CMD_NO_DUPS	1
define	CMD_SIGMA	2
define	CMD_SUBSAMP	3
define	CMD_CONT	4
define	CMD_SLOPE	5
define	CMD_TEMP	6
define	CMD_RSHIFT	7
define	CMD_FNU		8
define	CMD_Z		9
define	CMD_NSIGMA	10
define	CMD_WIDTH	11
define	CMD_RADIUS	12
define	CMD_THRESH	13
define	CMD_OFFSET	14
define	CMD_UNITS	15
define	CMD_XCOR	16

# Show types.
define	SHOW_ALL	1
define	SHOW_BOTH	2
define	SHOW_LOC	3
define	SHOW_PRED	4

# Identification steps
define	ID_START	1
define	ID_FEATURE	2
define	ID_LINE		3

# Memory management.
define	Lineobs		Memr[lineobs+$1-1]
define	Linepred	Memr[linepred+$1-1]
define	Rwave		Memr[rwave+$1-1]
define	Strval		Memc[strval]
define	Sx		Memc[sx]
define	Sy		Memc[sy]

# Gotos
define	change_z_	10

#---------------------------------------------------------------------------
.help wid_waveid 10Apr95 source
.ih
NAME
wid_waveid -- Identify lines in a wavelength observation.
.endhelp
#---------------------------------------------------------------------------
procedure wid_waveid (wid, ll, ot, mkw)

pointer	wid			# I:  WID object.
pointer	ll			# I:  Line list object.
pointer	ot			# I:  Output table object.
pointer	mkw			# I:  MKW object.

# Delcarations
real	ahivr()			# Get maximum value of vector.
int	ctowrd()		# Convert string to word.
bool	done			# TRUE if done with interactions.
double	dx, dy			# Generic.
int	ggeti()			# Get integer-valued GRAPHCAP parameter.
pointer	gopen()			# Open the graphics device.
pointer	gp			# Graphics descriptor.
pointer	gt			# GTOOLS descripttor.
int	gt_gcur()		# Get graphics cursor value.
pointer	gt_init()		# Initialize the gtools descriptor.
int	i, ip, j		# Generic.
int	id_step			# Current step in identification process.
real	id_fw			# Predicted wavelength of observed feature.
int	key			# Cursor key.
pointer	lineobs			# Spectrum of where the lines are observed.
pointer	linepred		# Spectrum of where the lines are predicted.
real	lnorm			# Normalization for the predicted intensities.
pointer	loc			# Found lines.
pointer	wid_locate()		# Find lines in observation.
real	max_obs			# Maximum value in observation.
int	recal			# YES to recalculate line positions.
int	redraw			# YES to redraw the graphics window.
int	respec			# YES to reproduce the spectrum.
int	rexcor			# YES to find offset by cross correlation.
double	rshift			# Redshift.
pointer	rwave			# Wavelength in reals.
real	rx			# Generic.
int	show			# What to show.
pointer	sp			# Stack pointer.
int	strdic()		# Retrieve dictionary index.
pointer	strval			# Cursor command.
pointer	sx, sy			# Generic string.
bool	use_color		# True to use color.
real	w2p()			# Wavelength to pixel.
int	wcs			# WCS of cursor position.
real	wx, wy			# Cursor position.
real	xc_shift()		# Find shift in cross correlation.
int	z			# Type of redshift.

errchk	wid_ll_free, wid_ll_no_dups, wid_locate

begin
	call smark (sp)
	call salloc (strval, SZ_LINE, TY_CHAR)
	call salloc (sx, SZ_LINE, TY_CHAR)
	call salloc (sy, SZ_LINE, TY_CHAR)
	z = YES
	rshift = 0.d0
	
	# Initialize the MKW object.
	MKW_WAVE_PTR(mkw) = WID_WAVE_PTR(wid)
	MKW_NPIX(mkw) = WID_NPIX(wid)
	call realloc (lineobs, WID_NPIX(wid), TY_REAL)
	call realloc (linepred, WID_NPIX(wid), TY_REAL)

	# Get normalization for line intensities.
	max_obs = ahivr (WID_OBS(wid,1), WID_NPIX(wid))

	# If interactive, initialize the graphics system.
	gp = NULL
	id_step = ID_START
        done = true
	if (WID_INTER(wid) == YES) {
            done = false
            
	    # Open the graphics device.
	    call clgstr ("device", Sx, SZ_LINE)
	    gp = gopen (Sx, NEW_FILE, STDGRAPH)
	    gt = gt_init()
	    call gsview (gp, 0.1, 0.85, 0.1, 0.95)
	    call gt_sets (gt, GTTYPE, "line")

	    # See whether colors or different line types should be used.
	    use_color = (ggeti (gp, "zr") > 0)

	    # Setup real version of the wavelengths.
	    call malloc (rwave, WID_NPIX(wid), TY_REAL)
	    call achtdr (WID_WAVE(wid,1), Rwave(1), WID_NPIX(wid))

	}
	
	# Start interactive loop.
	loc = NULL
	recal = YES
	show = SHOW_LOC
	if (WID_XCOR(wid) == YES)
	    key = 'x'
	else
	    key = 'r'
	repeat {

	    # Execute appropriate command.
	    switch (key) {
            case ' ':
                # Show wavelength/pixel coordinates of cursor.
		dx = wx
                rx = w2p (dx, WID_WAVE(wid,1), WID_NPIX(wid))
		call printf ("wave=%g pixel=%g")
		call pargr (wx)
		call pargr (rx)

	    case '?':
		# Show help pages.
		call gpagefile (gp, "hrs$doc/zwaveid.key", "")

	    case 'a':
		# Show all plots
		show = SHOW_ALL
		redraw = YES

	    case 'b':
		# Show predicted and located plots
		show = SHOW_BOTH
		redraw = YES
		
	    case 'g':
		# Report on nearest feature.
		dx = WID_WAVE(wid,WID_NPIX(wid))
		do i = 1, LL_N(loc) {
		    if (show == SHOW_PRED)
			dy = abs (LL_WAVE(loc,i) - wx)
		    else
			dy = abs (LL_WAVEO(loc,i) - wx)
		    if (dy < dx) {
			dx = dy
			j = i
		    }
		}
		if (show == SHOW_PRED) {
		    call printf ("line %g predicated at pixel %g")
		    call pargd (LL_WAVE(loc,j))
		    call pargr (LL_POSP(loc,j))
		} else {
		    call printf ("line %g located at pixel %g wavelength %g")
		    call pargd (LL_WAVE(loc,j))
		    call pargr (LL_POSO(loc,j))
		    call pargd (LL_WAVEO(loc,j))
		}

	    case 'i':
		# Identify a line with a feature.
		switch (id_step) {
		case ID_START:
		    call printf ("Select observed feature to identify")
		    id_step = ID_FEATURE

		case ID_FEATURE:
		    id_fw = wx
		    call printf ("Select line assocaited with feature")
		    id_step = ID_LINE

		case ID_LINE:
		    WID_UNITS(wid) = WID_UNITS_WAVE
		    WID_OFF(wid) = wx - id_fw
		    id_step = ID_START
		    recal = YES
		}	

	    case 'l':
		# Show observation and located plots
		show = SHOW_LOC
		redraw = YES
		
	    case 'n':
		# Finished with this one, go to the next.
		break

	    case 'p':
		# Show observation and predicted plots
		show = SHOW_PRED
		redraw = YES
		
	    case 'r':
		# Redraw everything.
		redraw = YES

            case 's':
                # No more interactions.
                WID_INTER(wid) = NO
                done = true
		
	    case 'w':
		# Execute the gtools windowing options
		call gt_window (gt, gp, "coords", redraw)

	    case 'x':
		# Redo the cross correlation.
		respec = YES
		rexcor = YES

	    case 'I':
		# Abort task.
		call fatal (1, "waveid: task aborted")

	    case ':':
		# Execute colon commands
		ip = 1
		i = ctowrd (Strval, ip, Sx, SZ_LINE)
		i = strdic (Sx, Sy, SZ_LINE, WID_COLON_CMDS)
		switch (i) {
		case CMD_NO_DUPS:

		    # Report or change duplication
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("no_dups = %b")
			call pargi (WID_NO_DUPS(wid))
		    } else {
			call sscan (Sx)
			call gargi (WID_NO_DUPS(wid))
			recal = YES
		    }

		case CMD_SIGMA:
		    # Report or change sigma.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("sigma = %g")
			call pargd (MKW_SIGMA(mkw))
		    } else {
			call sscan (Sx)
			call gargd (MKW_SIGMA(mkw))
			respec = YES
		    }

		case CMD_SUBSAMP:
		    # Report/change subsampling.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("subsample = %g")
			call pargd (1.d0/MKW_SUBSAMPLE(mkw))
		    } else {
			call sscan (Sx)
			call gargd (MKW_SUBSAMPLE(mkw))
			MKW_SUBSAMPLE(mkw) = 1.d0/MKW_SUBSAMPLE(mkw)
			respec = YES
		    }
		    
		case CMD_CONT:
		    # Report/change continuum.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("continuum = %g")
			call pargd (MKW_CONT(mkw))
		    } else {
			call sscan (Sx)
			call gargd (MKW_CONT(mkw))
			respec = YES
		    }
		    
		case CMD_SLOPE:
		    # Report/change slope.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("slope = %g")
			call pargd (MKW_SLOPE(mkw))
		    } else {
			call sscan (Sx)
			call gargd (MKW_SLOPE(mkw))
			respec = YES
		    }
		    
		case CMD_TEMP:
		    # Report/change temperature.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("temperature = %g")
			call pargd (MKW_TEMP(mkw))
		    } else {
			call sscan (Sx)
			call gargd (MKW_TEMP(mkw))
			respec = YES
		    }
		    
		case CMD_RSHIFT:
		    # Report/change red-shift.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("rshift = %g")
			call pargd (rshift)
		    } else {
			call sscan (Sx)
			call gargd (rshift)
change_z_
			if (z == YES)
			    MKW_Z(mkw) = 1 + rshift
			else {
			    MKW_Z(mkw) = rshift / 299792.5
			    MKW_Z(mkw) = sqrt ((1+MKW_Z(mkw))/(1-MKW_Z(mkw))) 
			}
			respec = YES
		    }
		    
		case CMD_FNU:
		    # Report or change fnu
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("fnu = %b")
			call pargi (MKW_FNU(mkw))
		    } else {
			call sscan (Sx)
			call gargi (MKW_FNU(mkw))
			recal = YES
		    }

		case CMD_Z:
		    # Report or change z.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("z = %b")
			call pargi (z)
		    } else {
			call sscan (Sx)
			call gargi (z)
			goto change_z_
		    }

		case CMD_NSIGMA:
		    # Report/change nsigma.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("nsigma = %g")
			call pargd (MKW_NSIGMA(mkw))
		    } else {
			call sscan (Sx)
			call gargd (MKW_NSIGMA(mkw))
			respec = YES
		    }
		    
		case CMD_WIDTH:
		    # Report/change feature width.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("width = %g")
			call pargr (WID_WIDTH(wid))
		    } else {
			call sscan (Sx)
			call gargr (WID_WIDTH(wid))
			recal = YES
		    }
		    
		case CMD_RADIUS:
		    # Report/change search radius.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("radius = %g")
			call pargr (WID_RADIUS(wid))
		    } else {
			call sscan (Sx)
			call gargr (WID_RADIUS(wid))
			recal = YES
		    }
		    
		case CMD_THRESH:
		    # Report/change line threshold.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("threshold = %g")
			call pargr (WID_THRESH(wid))
		    } else {
			call sscan (Sx)
			call gargr (WID_THRESH(wid))
			recal = YES
		    }
		    
		case CMD_OFFSET:
		    # Report/change offset.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("offset = %g (%s)")
			call pargr (WID_OFF(wid))
			switch (WID_UNITS(wid)) {
			case WID_UNITS_PIXEL:
			    call pargstr ("pixel")
			case WID_UNITS_WAVE:
			    call pargstr ("wavelength")
			case WID_UNITS_SAMPLE:
			    call pargstr ("sample")
			}	
		    } else {
			call sscan (Sx)
			call gargr (WID_OFF(wid))
			recal = YES
		    }
		    
		case CMD_UNITS:
		    # Report change how wavelengths are shown.
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("units = %s")
			switch (WID_UNITS(wid)) {
			case WID_UNITS_PIXEL:
			    call pargstr ("pixel")
			case WID_UNITS_WAVE:
			    call pargstr ("wavelength")
			case WID_UNITS_SAMPLE:
			    call pargstr ("sample")
			}	
		    } else {
			i = strdic (Sx, Sy, SZ_LINE, WID_UNITS_DICT)
			if (i > 0) {
			    WID_UNITS(wid) = i
			    call printf ("Make sure 'offset' is in the specified units")
			} else {
			    call printf ("Units '%s' unknown, try 'pixel', 'wavelength', or 'sample'")
			    call pargstr (Sx)
			}
		    }
		    
		case CMD_XCOR:

		    # Report or change initial cross correlation
		    if (ctowrd (Strval, ip, Sx, SZ_LINE) <= 0) {
			call printf ("xcor = %b")
			call pargi (WID_XCOR(wid))
		    } else {
			call sscan (Sx)
			call gargi (WID_XCOR(wid))
			recal = YES
		    }

		default:
		    call gt_colon (Strval, gp, gt, redraw)
		    
		}	
	    }

	    # Reproduce the predicted line spectrum.
	    if (respec == YES) {
		MKW_LINES_PTR(mkw) = LL_WAVE_PTR(ll)
		MKW_NLINES(mkw) = LL_N(ll)
		MKW_INT_PTR(mkw) = LL_INTP_PTR(ll)
		MKW_SPEC_PTR(mkw) = linepred
		
		# Recalculate spectrum.
		call amovkr (0., MKW_SPEC(mkw,1), MKW_NPIX(mkw))
		call mkw_mkwave (mkw, 0)

		# Normalize the spectrum.
		lnorm = max_obs / ahivr (MKW_SPEC(mkw,1), MKW_NPIX(mkw))
		call amulkr (MKW_SPEC(mkw,1), lnorm, MKW_SPEC(mkw,1),
			     MKW_NPIX(mkw))
	    }

	    # Use cross correlation to find the gross offset.
	    if (rexcor == YES) {
		WID_UNITS(wid) = WID_UNITS_PIXEL
		WID_OFF(wid) = xc_shift (WID_OBS(wid,1), Linepred(1),
					 WID_NPIX(wid), WID_MSHIFT(wid))
		recal = YES
		rexcor = NO
	    }

	    # Locate the lines.
	    if (recal == YES) {
		
		# Find the locations.
		if (loc != NULL)
		    call wid_ll_free (loc)
		loc = wid_locate (wid, ll)

		# Remove duplicate finds.
		if (WID_NO_DUPS(wid) == YES)
		    call wid_ll_no_dups (loc)

		# Done recalculating.
		recal = NO
		respec = YES
		redraw = YES
	    }

	    # Recompute the wavelength spectrum.
	    if (respec == YES) {
		MKW_LINES_PTR(mkw) = LL_WAVEO_PTR(loc)
		MKW_NLINES(mkw) = LL_N(loc)
		MKW_INT_PTR(mkw) = LL_INTP_PTR(loc)
		MKW_SPEC_PTR(mkw) = lineobs
		
		# Recalculate the wavelength spectrum.
		call amovkr (0., MKW_SPEC(mkw,1), MKW_NPIX(mkw))
		call mkw_mkwave (mkw, 0)

		# Normalize the spectrum.
		call amulkr (MKW_SPEC(mkw,1), lnorm, MKW_SPEC(mkw,1),
			     MKW_NPIX(mkw))

		respec = NO
		redraw = YES
	    }
	    
	    # Redraw the graphics.
	    if (WID_INTER(wid) == YES && redraw == YES) {

		# Set the graphics range.
		call gframe (gp)
		call gt_ascale (gp, gt, Rwave(1), WID_OBS(wid,1), WID_NPIX(wid))

		# Draw the axis.
		call gt_labax (gp, gt)

		# Draw the data.
		if (show != SHOW_BOTH) {
		    if (use_color)
			call gt_seti (gt, GTCOLOR, 1)
		    else
			call gt_seti (gt, GTLINE, GL_SOLID)
		    call gt_plot (gp, gt, Rwave(1), WID_OBS(wid,1),
				  WID_NPIX(wid))
		}

		# Draw the predicted line spectrum.
		if (show != SHOW_LOC) {
		    if (use_color)
			call gt_seti (gt, GTCOLOR, 2)
		    else
			call gt_seti (gt, GTLINE, GL_DASHED)
		    call gt_plot (gp, gt, Rwave(1), Linepred(1), MKW_NPIX(mkw))
		}

		# Draw the located line spectrum.
		if (show != SHOW_PRED) {
		    if (use_color)
			call gt_seti (gt, GTCOLOR, 3)
		    else
			call gt_seti (gt, GTLINE, GL_DOTTED)
		    call gt_plot (gp, gt, Rwave(1), Lineobs(1), MKW_NPIX(mkw))
		}

		# Draw labels
		call gseti (gp, G_WCS, 0)
		if (show != SHOW_BOTH) {
		    call gtext (gp, 0.92, 0.8, "Observed", "hj=c,vj=t")
		    if (use_color)
			call gseti (gp, G_PLCOLOR, 1)
		    else
			call gseti (gp, G_PLTYPE, GL_SOLID)
		    call gline (gp, 0.87, 0.8, 0.97, 0.8)
		}
		if (show != SHOW_LOC) {
		    call gtext (gp, 0.92, 0.7, "Predicted", "hj=c,vj=t")
		    if (use_color)
			call gseti (gp, G_PLCOLOR, 2)
		    else
			call gseti (gp, G_PLTYPE, GL_DASHED)
		    call gline (gp, 0.87, 0.7, 0.97, 0.7)
		}
		if (show != SHOW_PRED) {
		    call gtext (gp, 0.92, 0.6, "Located", "hj=c,vj=t")
		    if (use_color)
			call gseti (gp, G_PLCOLOR, 3)
		    else
			call gseti (gp, G_PLTYPE, GL_DOTTED)
		    call gline (gp, 0.87, 0.6, 0.97, 0.6)
		}
		call gseti (gp, G_WCS, 1)

		# Done with drawing.
		redraw = NO
	    }

	    # Reset line identification if key sequence hit out of order.
	    if (id_step != ID_START && key != 'i' & key != ' ')
		id_step = ID_START

	    # Get next cursor event.
	    if (WID_INTER(wid) == YES && !done) {
		if (gt_gcur ("coords", wx, wy, wcs, key, Strval, SZ_LINE) ==
		    EOF )
                    done = true
	    }
	} until (done)
	
	# Write the found lines out to the table.
	call wid_o_wtab (wid, loc, ot)
	call wid_ll_free (loc)

	# That's all folks.
	if (gp != NULL) {
	    call mfree (rwave, TY_REAL)
	    call mfree (lineobs, TY_REAL)
	    call mfree (linepred, TY_REAL)
	    call gt_free (gt)
	    call gclose (gp)
	}
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of wid_waveid
#---------------------------------------------------------------------------

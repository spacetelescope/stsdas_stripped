# Graphics parameters.
define  DISTANCE                .05
define	OFFSET_SIZE		.05
define  SZ_JUST                 10

# Memory.
define	C		Memr[c+$1-1]
define	Sx    		Memc[sx]

#---------------------------------------------------------------------------
.help wo_preview Nov93 source
.ih
NAME
wo_preview -- Display the correlation function
.endhelp
#---------------------------------------------------------------------------
bool procedure wo_preview(file, corr, hlen, offset, value, gp)

char	file[ARB]		# I:  The image the correlation belongs to.
double	corr[hlen*2+1]		# I:  The correlation function.
int	hlen			# I:  Half-length of the correlation function.
double	offset			# I:  Where the offset is found.
double	value			# I:  Value of correlation at offset.
pointer	gp			# I:  Graphics descriptor.

pointer	c			# Real version of the correlation.
int	clgcur()		# Read graphics cursor.
real	cx, cy			# Cursor position.
int	i, j			# Generic.
int	key			# Key hit.
int	len			# Length of the correlation function.
bool	preview			# True to preview the next correlation.
real	rx, ry			# Generic.
pointer	sp			# Stack pointer.
pointer	sx			# Generic string.
int	wcs			# Current graphics wcs.

begin
	call smark(sp)

	# Convert to real.
	len = hlen * 2 + 1
	call salloc (c, len, TY_REAL)
	call achtdr (corr, C(1), len)
	do i = 1, hlen {
	    j = len-i+1
	    rx = C(i)
	    C(i) = C(j)
	    C(j) = rx
	}

	# Create the title.
	call salloc (sx, SZ_LINE, TY_CHAR)
	call sprintf (Sx, SZ_LINE, "Correlation function for image %s")
	call pargstr (file)

	# Plot the data.
	rx =  hlen
	call gclear (gp)
	call gploto (gp, C(1), len, -rx, rx, Sx)

	# Draw where the shift is found and its value.
	call sprintf (Sx, SZ_LINE, "offset = %7.3f")
	call pargd (offset)
	rx = offset
	ry = value
	call wo_poffset (gp, rx, ry, Sx, OFFSET_SIZE)

	# Now loop.
	preview = true
	while (clgcur ("cursor", cx, cy, wcs, key, Sx, SZ_LINE) != EOF) {
	    switch (key) {
	    case '?':
		call gpagefile (gp, "hrs$doc/waveoff.key", NULL)

	    case 'n':
		preview = false
		call printf ("waveoff: Preview off")

	    case 'p':
		preview = true
		call printf ("waveoff: Preview on")

	    case 'q':
		break

	    default:
		call printf ("cursor: %g %g")
		call pargr (cx)
		call pargr (cy)
	    }	
	}

	# That's all folks.
	call sfree(sp)
	return (preview)
end
#---------------------------------------------------------------------------
# End of wo_preview
#---------------------------------------------------------------------------
procedure wo_poffset (gp, x, y, label, size)

pointer gp                      # I:  Graphics descriptor.
real    x, y                    # I:  Position of point.
char    label[ARB]              # I:  Label of the point.
real    size                    # I:  Size of the line to draw.

char    just[SZ_JUST]           # Text justification.
real    ty                      # Y-pos of the label.
real    wx1, wx2, wy1, wy2      # Edges of the graph.
real    y_sz                    # Y-extent of the graph.
real    y1, y2                  # End y-points of the marking line.

begin
        # Calculate extent of graph.
        call ggwind (gp, wx1, wx2, wy1, wy2)
        y_sz = wy2 - wy1

        # Draw line according to which half of the graph we are on.
        if (y > (wy1+wy2)/2.) {
            y2 = y - (DISTANCE * y_sz)
            y1 = y2 - (size * y_sz)
            ty = y1 
            call strcpy ("v=t", just, SZ_JUST)
        } else {
            y1 = y + (DISTANCE * y_sz)
            y2 = y1 + (size * y_sz)
            ty = y2
            call strcpy ("v=b", just, SZ_JUST)
       }

        # Draw the line
        call gline (gp, x, y1, x, y2)
        
        # Determine text justification depending on what half of the graph.
        if (x > (wx1+wx2)/2.)
            call strcat (";h=l", just, SZ_JUST)
        else
            call strcat (";h=r", just, SZ_JUST)

        # Write the text.
        call gtext (gp, x, ty, label, just)
end
#---------------------------------------------------------------------------
# End of wo_poffset
#---------------------------------------------------------------------------

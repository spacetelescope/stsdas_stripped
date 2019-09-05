include	<pkg/center1d.h>
include	"wid.h"
include	"line.h"

#---------------------------------------------------------------------------
.help wid_locate 13Mar95 source
.ih
NAME
wid_locate -- Locate lines in the observation.
.in
DESCRIPTION
.ls OFFSETS
The general idea of an 'offset' is defined by

.nf
	observed = predicted + offset
.fi

For the wavelengths, the 'predicted' wavelength is the provided
wavelength solution.  The 'observed' wavelength is the wavelength
defined for a feature from the line list.  This is then expressed by

.nf
	w(obs) = w(pred) + woff
.fi

Similarly, the 'predicted' pixel position is the position of a
wavelength as specified by the provided wavelength solution.  The
'observed' pixel is the centroid of the feature itself.  The pixel
offset is then

.nf
	p(obs) = p(pred) + poff
.fi

Sample offsets are the same as pixels offsets, just converted from
units of sample to pixel.
.le
.endhelp
#---------------------------------------------------------------------------
pointer procedure wid_locate (wid, ll)

pointer	wid			# I:  WID object.
pointer	ll			# I:  Line list object.

# Declarations
real	center1d()		# Find center of line.
int	l			# Current line.
pointer	wid_ll_alloc()		# Allocate a line list.
pointer	loc			# Find lines.
real	op			# Observed pixel of line.
double	ow			# Observed wavelength of line.
double	p2w()			# Convert pixel to wavelength.
real	poff			# Pixel offset.
real	pp			# Predicted pixel of line
double	pw			# Predicted wavelength of line.
real	w2p()			# Convert wavelength to pixel.
double	woff			# Wavelength offset.

errchk	wid_ll_alloc

begin
	loc = wid_ll_alloc (LL_N(ll))

	# Determine the offset to apply
	switch (WID_UNITS(wid)) {
	case WID_UNITS_PIXEL:
	    poff = WID_OFF(wid)
	    woff = 0.d0

	case WID_UNITS_SAMPLE:
	    poff = WID_OFF(wid) / WID_SD(wid)
	    woff = 0.d0

	case WID_UNITS_WAVE:
	    poff = 0.0
	    woff = WID_OFF(wid)
	}	

	# Loop through the lines.
	do l = 1, LL_N(ll) {

	    # Get observed wavelength with offset.
	    ow = LL_WAVE(ll,l) - woff
	    
	    # If line is not in observation, skip it.
	    if (ow < min (WID_WAVE(wid,1), WID_WAVE(wid,WID_NPIX(wid))))
		next
	    if (ow > max (WID_WAVE(wid,1), WID_WAVE(wid,WID_NPIX(wid))))
		break

	    # Get pixel position of line.
	    pp = w2p (ow, WID_WAVE(wid,1), WID_NPIX(wid))
	    op = pp + poff
	    if (op < WID_WIDTH(wid))
		next
	    if (op > WID_NPIX(wid)-WID_WIDTH(wid))
		break

	    # Find true position.
	    op = center1d (op, WID_OBS(wid,1), WID_NPIX(wid),
			   WID_WIDTH(wid), EMISSION, WID_RADIUS(wid),
			   WID_THRESH(wid))
	    if (IS_INDEFR(op))
		next

	    # Get predicted wavelength of line.
	    pw = p2w (op, WID_WAVE(wid,1), WID_NPIX(wid))
	    
	    # Found a line, add to the found positions.
	    call wid_ll_add (loc, LL_WAVE(ll,l), LL_INTP(ll,l), pp, op, pw)
	}

	# That's all folks.
	return (loc)
end
#---------------------------------------------------------------------------
# End of wid_locate
#---------------------------------------------------------------------------

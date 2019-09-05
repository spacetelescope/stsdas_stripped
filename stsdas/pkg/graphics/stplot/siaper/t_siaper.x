include <gset.h>
include <imhdr.h>
include "siaper.h"

#---------------------------------------------------------------------------
.help siaper Mar91 source
.ih
NAME
siaper - Draw apertures of the HST SI's using the MWCS info from an image.
.ih
DESCRIPTION
siaper draws selected apertures of the HST's SI's onto the specified 
graphics device.  Information about the scale is gleaned from the MWCS of 
an image and the HST's roll is specified by the user.  This is meant to 
overlay onto an image to get an idea of what the field of view will be
for a given part of the sky.
.ih
BUGS
- Not sure whether the roll is implemented correctly for situations
where the center aperture is not the V1 axis.
.endhelp
#---------------------------------------------------------------------------
procedure t_siaper

# Declarations
pointer a_aper		# An aperture to display.
pointer apertures       # The apertures to draw.
pointer center_ap       # The aperture which should be centered at the
                        # specified RA,DEC.
pointer colptr          # The pointers to the columns in the SDAS table.
pointer dec_string      # String representing DEC in DMS format.
pointer device          # The graphics device to draw on.
pointer display_mw      # Display MWCS to display the apertures.
pointer gp              # The graphics device descriptor.
pointer im              # The image descriptor.
pointer image_mw        # The MWCS descriptor for the image.
pointer image_name      # Name of the image.
pointer ra_string       # String representing RA in HMS format.
pointer siaf_name       # Name of the SIAF file.
pointer siaf_tp         # The SIAF SDAS table descriptor.
pointer sp              # The stack pointer.
pointer tmp_str_1, tmp_str_2
                        # Temporary strings.

real c1, c2, l1, l2     # The extent of the image.
real ra, dec            # The RA,DEC to center the OTA on.
real roll               # The HST's roll angle (in radians).
real	rx, ry		# Generic.
real left, right, bottom, top  # The graphics viewport.

int fd                  # File descriptor for list of inputs.
int ip                  # Character pointer for the aperture list.

bool append             # YES if this will be appended to a graph.
bool fill               # True if the viewport should be filled.
bool flip               # True if axes are transposed.
bool sky_project	# True to invert V3 for sky projection.

string world   "world"   # The World coordinate system.
string logical "logical" # The Logical coordinate system.

# Function declarations.
pointer gopen(), immap(), mw_openim(), mw_sctran()
real clgetr(), si_string_to_degrees()
int ctowrd(), fscan(), open(), strlen(), strmatch(), stropen()
bool clgetb()

begin
	call smark (sp)
	call salloc (siaf_name, SZ_FNAME, TY_CHAR)
	call salloc (image_name, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (apertures, SZ_LINE, TY_CHAR)
	call salloc (a_aper, SZ_LINE, TY_CHAR)
	call salloc (tmp_str_1, SZ_LINE, TY_CHAR)
	call salloc (tmp_str_2, SZ_LINE, TY_CHAR)
	call salloc (center_ap, SZ_LINE, TY_CHAR)
	call salloc (ra_string, SZ_LINE, TY_CHAR)
	call salloc (dec_string, SZ_LINE, TY_CHAR)
	call salloc (colptr, N_COLS, TY_POINTER)

	# Retrieve all the necessary parameters.
	call clgstr ("apertures", Memc[apertures], SZ_LINE)
	call strlwr (Memc[apertures])
	call clgstr ("image", Memc[image_name], SZ_FNAME)

	call clgstr ("ra", Memc[ra_string], SZ_FNAME)
	call clgstr ("dec", Memc[dec_string], SZ_FNAME)
	call clgstr ("center_ap", Memc[center_ap], SZ_LINE)
	call strlwr (Memc[center_ap])
	roll = clgetr ("roll")
	call clgstr ("siaf", Memc[siaf_name], SZ_FNAME)
	call clgstr ("device", Memc[device], SZ_FNAME)
	left = clgetr ("left")
	right = clgetr ("right")
	bottom = clgetr ("bottom")
	top = clgetr ("top")
	append = clgetb ("append")
	fill = clgetb ("fill")
	sky_project = clgetb ("sky_project")

	# Convert the RA/DEC strings.
	ra = si_string_to_degrees (Memc[ra_string], LONGITUDE)
	dec = si_string_to_degrees (Memc[dec_string], LATITUDE)

	# Open the SDAS table containing the Science Instrument Aperture File
	# (siaf) which lists the aperture locations, sizes, shapes, etc. of all
	# the Science Instruments.  This file can be retrieve from the CDBS
	# database as cosiaf.
	call si_open_siaf (Memc[siaf_name], siaf_tp, Memi[colptr])

	# Retrieve the MWCS information from the image.
	if (strlen (Memc[image_name]) == 0) {
	    im = NULL
	    call sp_wcsparams (image_mw, c1, c2, l1, l2)
	} else {
	    im = immap (Memc[image_name], READ_ONLY, NULL)
	    image_mw = mw_openim (im)
	    c1 = 1.
	    c2 = real (IM_LEN(im, X_DIM))
	    l1 = 1.
	    l2 = real (IM_LEN(im, Y_DIM))
	}

	# Determine the transformations necessary to place the apertures
	# correctly on the image.
	call si_get_display_transformation (image_mw, Memc[center_ap],
					    siaf_tp, Memi[colptr], roll,
					    sky_project,
					    ra, dec, display_mw, flip)

	# Open the graphics device.
	if (append)
	    gp = gopen (Memc[device], APPEND, STDGRAPH)
	else {
	    gp = gopen (Memc[device], NEW_FILE, STDGRAPH)
	    rx = abs (c2 - c1)
	    ry = abs (l2 - l1)
	    call sp_map_viewport (gp, rx, ry, left, right, bottom, top,
				  !fill, false)
	}
	call gswind (gp, c1, c2, l1, l2)
	
	# Now plot each of the apertures.  There are two ways of specifying the
	# aperture list.  They may be listed in the parameter, or if the
	# aperture parameter is of the form '@filename', the list is read
	# from the specified file.  Plot each aperture until done.

	# Open the aperture list either as a file or a string.
	ip = strmatch (ONEC (apertures, 1), "^#@")
	if  (ip == 0)
	    fd = stropen (Memc[apertures], strlen (Memc[apertures]),
			  READ_ONLY)
	else
	    fd = open (ONEC (apertures, ip), READ_ONLY, TEXT_FILE)

	# Now read in each word.  Each word is an aperture specification.  This
	# list may be whitespace or comma-separated.  Read in the line, replace all
	# commas with whitespace, then extract each aperture.
	while (fscan (fd) != EOF) {
	    call gargstr (Memc[tmp_str_1], SZ_LINE)
	    call sp_change_string (Memc[tmp_str_1], ",", " ", Memc[tmp_str_2], 
				   SZ_LINE)
	    ip = 1
	    while  (ctowrd (Memc[tmp_str_2], ip, Memc[a_aper], SZ_LINE) != 0) {
		call strlwr (Memc[a_aper])
		call si_plot_aperture(Memc[a_aper], siaf_tp, Memi[colptr], gp, 
				      mw_sctran(display_mw, logical, world, 3b),
				      flip)
	    }
	}

	# Close up, we're done.
	call close (fd)
	call tbtclo (siaf_tp)
	call gclose (gp)
	call mw_close (image_mw)
	call mw_close (display_mw)
	if (im != NULL)
	    call imunmap (im)
	call sfree (sp)

end
#---------------------------------------------------------------------------
# End of t_siaper
#---------------------------------------------------------------------------

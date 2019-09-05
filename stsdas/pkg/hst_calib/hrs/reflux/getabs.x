define  SZ_KEYWORD      18              # max chars in POPDS keyword value

include <imhdr.h>

# GET_ABS -- Read absolute sensitivity and corresponding wavelength net
#	     for HRS and check consistency
#
# Task reads absolute sensitivity image and wavelength net image.
# Checks that both conform to ICD-47 description of referenced files
# CZABSR and CZNETR, respectively.
#
# S. Hulbert, Jul 91    Original

procedure get_abs (im_abs, im_net, abs, net, npix1, grat1)

pointer im_abs			#I: image pointer for absolute sensitivity
pointer im_net			#I: image pointer for wavelength grid
pointer abs			#I: buffer pointer for abs. sens. 
pointer net			#I: buffer pointer for wave net,
int     npix1			#I: numbert of pixels in abs. sens./wave net
char    grat1[SZ_KEYWORD]	#I: grating to which abs. sens. data applies

int     gcount1, gcount2
int     npix2
int     offset1, offset2
char    grat2[SZ_KEYWORD]
char    aper1[SZ_KEYWORD], aper2[SZ_KEYWORD]
char    aper3[SZ_KEYWORD], aper4[SZ_KEYWORD]
real	datamax, datamin

int     gf_gstfval()
bool    streq()
pointer imgl1r()

begin

	# verify that reference files conform to ICD-47
	# check number of pixels
        npix1 = IM_LEN(im_abs, 1)
        npix2 = IM_LEN(im_net, 1)
        if (npix1 != npix2)
            call error (0, "Wavelength net and flux images have different lengths")

	# check number of groups
        gcount1 = gf_gstfval (im_abs, "GCOUNT")
        gcount2 = gf_gstfval (im_net, "GCOUNT")
        if (gcount1 != 2 || gcount2 != 2)
            call error (0, "Wavelength net and flux images have incorrect number of groups")

	# check grating
        call imgstr (im_abs, "GRATING", grat1, SZ_KEYWORD)
        call imgstr (im_net, "GRATING", grat2, SZ_KEYWORD)
        if (!streq (grat1, grat2))
            call error (0, "Wavelength net and flux images are for different gratings")

        # check aperture for first group
        call imgstr (im_abs, "APERTURE", aper1, SZ_KEYWORD)
        call imgstr (im_net, "APERTURE", aper2, SZ_KEYWORD)
        if (!streq (aper1, aper2))
            call error (0, "Wavelength net and flux images have mismatched apertures in first group")

	# check for old aperture values and reset to new values         
        if (streq(aper1, "Z1")) 
	    call strcpy ("SSA", aper1, SZ_KEYWORD)
        else if (streq(aper1, "Z2")) 
	    call strcpy ("LSA", aper1, SZ_KEYWORD)

        # allocate and buffers for data
        call realloc (abs, gcount1*npix1, TY_REAL)
        call realloc (net, gcount1*npix1, TY_REAL)

        # store SSA data first and LSA last
        if (streq(aper1, "SSA")) {
            offset1 = 0
            offset2 = npix1
        } else if (streq(aper1, "LSA")) {
            offset1 = npix1
            offset2 = 0
        } else {
            call error (0, "Invalid APERTURE keyword value")
        }

        # read data for first group
        call amovr (Memr[imgl1r(im_abs)], Memr[abs+offset1], npix1)
        call amovr (Memr[imgl1r(im_net)], Memr[net+offset1], npix1)

        # point to second group
        call gf_opengr (im_abs, 2, datamin, datamax, 0)
        call gf_opengr (im_net, 2, datamin, datamax, 0)

        # check aperture keyword for second group
        call imgstr (im_abs, "APERTURE", aper3, SZ_KEYWORD)
        call imgstr (im_net, "APERTURE", aper4, SZ_KEYWORD)
        if (!streq (aper3, aper4) || streq(aper1, aper3))
            call error (0, "Wavelength net and flux images have mismatched apertures in second group")

        # read data in second group
        call amovr (Memr[imgl1r(im_abs)], Memr[abs+offset2], npix1)
        call amovr (Memr[imgl1r(im_net)], Memr[net+offset2], npix1)


end


include <imhdr.h>
include <math.h>
include	<od.h>

# Orbital period for the doppler algorithm.
define  PERIOD                  97.d0

# Diode facts.
define  N_DIODES                500
define  DIODE2PIXEL             8

# GOTO destinations.
define  skip_                   10

#---------------------------------------------------------------------------
.help do_dopoff May93 source
.ih
NAME
do_dopoff -- Return doppler shift of input data.
.ih
USAGE
double = do_dopoff (od)
.ih
RETURNS
The magnitude of the doppler compensation for the image specified by
the od descriptor.
.ih
ARGUMENTS
.ls od (I: pointer)
The data file from which to determine the doppler compensation.  This
must be an image and it must have a ULH file counterpart.
.le
.ih
DESCRIPTION
The doppler compensation magnitude is determined by the same algorithm
as that used in the Flight Software (up to version 4.0).  This is so
the task reports what was actually done to the data, regardless of
whether the compensation is "correct".  The ULH file is required to
get an accurate time of when the observation occured.
.endhelp
#---------------------------------------------------------------------------
double procedure do_dopoff (od)

pointer od                      # I:  OD descriptor.

# IMIO.
int     gf_gstfval()            # Get STF kernel parameter.
pointer im                      # IMIO descriptor of image.
double  imgetd()                # Get double value header parameter.
pointer immap()                 # Open an image.
bool    is_ulh                  # TRUE if the ulh file is open.
pointer ulh                     # IMIO descr. of the corresponding ULH file.

# Doppler, times.
double  dopoff                  # Doppler compensation in deflection units.
double  etime, stime            # End/start time of observations.
double  mag                     # Maximum doppler magnitude.
double  time                    # Time to calculate doppler compensation.
double  zero                    # Time (MJD) of zero doppler shift.

# Misc.
double  dx                      # Generic.
int     ix                      # Generic.
real    rx                      # Generic.
pointer sp                      # Stack pointer.
bool    strne()                 # TRUE is strings are not equal.
pointer sx                      # Generic string.

begin
        call smark (sp)
        call salloc (sx, max (SZ_LINE, SZ_PATHNAME), TY_CHAR)
        
        # Assume that the granularity offset cannot be calculated.
        dopoff = 0.d0

        # Check that this is an image.
        if (OD_TYPE(od) != OD_IMAGE) {
            call eprintf ("dopoff: %s[%d] is not an image, cannot find doppler compensation, skipping...\n")
            call pargstr (OD_NAME(od))
            call pargi (OD_GRP(od))
            goto skip_
        }
        im = OD_FD(od)
        
        # First check for instrument.  So far, it needs to be the GHRS.
        call imgstr (im, "instrume", Memc[sx], SZ_LINE)
        call strlwr (Memc[sx])
        if (strne (Memc[sx], "hrs")) {
            call eprintf ("dopoff: %s[%d]: INSTRUME = %s is not GHRS\n        cannot calculate doppler, skipping..\n")
            call pargstr (OD_NAME(od))
            call pargi (OD_GRP(od))
            call pargstr (Memc[sx])
            goto skip_
        }

        # If doppler is not on, don't bother.
        call imgstr (im, "doppler", Memc[sx], SZ_LINE)
        call strlwr (Memc[sx])
        if (strne (Memc[sx], "on")) {
            call eprintf ("dopoff: %s[%d]: DOPPLER compensation not on, no need to calculate, skipping...\n")
            call pargstr (OD_NAME(od))
            call pargi (OD_GRP(od))
            goto skip_
        }

        # Retrieve the doppler information.
        zero = imgetd (im, "dopzer")
        if (zero <= 0.d0) {
            call eprintf ("dopoff: %s[%d]: invalid value for keyword DOPZER, skipping..\n")
            call pargstr (OD_NAME(od))
            call pargi (OD_GRP(od))
            goto skip_
        }
        mag = imgetd (im, "dopmag")
        if (mag <= 0.d0) {
            call eprintf ("dopoff: %s[%d]: invalid value for keyword DOPMAG, skipping..\n")
            call pargstr (OD_NAME(od))
            call pargi (OD_GRP(od))
            goto skip_
        }

        # The start/end of the observation are had from the packet times
        # of the two corresponding UDL groups that bracket the observation.
        # See if we can get the this information.
        call change_ext (IM_HDRFILE(im), "ulh", Memc[sx], SZ_PATHNAME)
        is_ulh = false
        iferr (ulh = immap (Memc[sx], READ_ONLY, NULL)) {
            call eprintf ("dopoff: %s[%d]: cannot find %s ULH file, skipping..\n")
            call pargstr (OD_NAME(od))
            call pargi (OD_GRP(od))
            call pargstr (Memc[sx])
            goto skip_
        }
        is_ulh = true
        
        # Get times from the appropriate groups.
        ix = gf_gstfval (im, "GROUP")
        call gf_opengr (ulh, 2*ix-1, rx, rx, 0)
        stime = imgetd (ulh, "PKTTIME")
        call gf_opengr (ulh, 2*ix, rx, rx, 0)
        etime = imgetd (ulh, "PKTTIME")
        
        # The doppler will be calculated at the midpoint of the observation.
        dx = (stime + etime) / 2.d0
        time = dx - zero

        # Convert to minutes.
        time = time * 24 * 60
        if (time <= 0.d0) {
            call eprintf ("dopoff: %s[%d]: Invalid exposure time calculated, skipping..\n")
            call pargstr (OD_NAME(od))
            call pargi (OD_GRP(od))
            goto skip_
        }

        # Calculate offset in deflection units.
        dopoff = mag * sin (time * TWOPI / PERIOD)

        # Calculate offset in pixels.
        dx = double (IM_LEN(im,1)) / N_DIODES
        dopoff = dopoff / DIODE2PIXEL * dx

        # That's all folks.
skip_
        if (is_ulh)
            call imunmap (ulh)
        call sfree (sp)
        return (dopoff)
        
end
#---------------------------------------------------------------------------
# End of do_dopoff
#---------------------------------------------------------------------------

include "obsum.h"

# Define when the maximum is too big.
define  MAX_DOP         23

# Convert from double days to integer seconds.
define  DAY2SEC         (int(($1) * 86400 + 0.5))

# Convert from integer minutes to integer seconds.
define  MIN2SEC         (($1) * 60)

# Convert from integer seconds to rounded integer minutes.
define  SEC2MIN         (int(($1) / 60.0 + 0.5))

#---------------------------------------------------------------------------
.help obs_doppler Feb93 source
.ih
NAME
obs_doppler -- Find out whether a maximum doppler compensation occured.
.ih
USAGE
call obs_doppler (images, stime, etime, period, group, o)
.ih
ARGUMENTS
.fs images (pointer :input)
Array of image descripters that point to at least the science data.
.fe
.fs stime (double :input)
The start time, in MJD, of the observation.
.fe
.fs etime (double :input)
The end time, in MJD, of the observation.
.fe
.fs period (int :input)
The period of the orbit.  See DESCRIPTION about what this should be.
.fe
.fs group (int :input)
The current "group" of data.  This is not "group" in the GEIS sense, but
group in the sense of a single "pattern" of an observation.  For GHRS, each
pattern is delimited by an SHP group, 2 UDL groups, and 0 or more science
groups.  Effectively, this is which SHP group the current data represents.
.fe
.fs o (pointer :input)
The is the local output descriptor, used to "store" output until an
appropriate time to actual display is determined.
.fe
.ih
DESCRIPTION
This routine represents the algorithm that the Flight SoftWare (FSW),
versions 4.0 and earlier, used to calculate the an offset into a
table of sine values.  the sine values are then used to determine the Doppler
shift the spacecraft was adding to any observered spectrum.

However, around 1Feb93, someone (finally), discovered that this algorithm did
not work.  Specifically, the sine table consited of 24 entries,
offsets 0 through 23.  The intent was that the table would represent
sine values, 1 for each minute, of a quarter of an orbit.  This should
work for orbits up to 96 minutes in duration.  However, it doesn't.
For maximum Doppler, the algorithm calculates and index 1 larger than
the size of the table.  In the FSW, the resultant sine value is 0, or
no Doppler correction.  This routine is meant to mimic the "bad"
algorithm, so that observers can determine whether their data has been
corrupted, and for relatively how long the data spent at "no
correction".

Unfortunately, with just the generic conversion output, there isn't
enough information to determine what the telescope was doing when.  In
particular, when the telescope idled the obervation, due to
occultation or any number of other things.  This affects mostly the
blue shifted time, since reaquisition after an occultation, usually,
but not always, last longer than when the blue shift maximum would
occur.  There is also the question of SAA passage, which again, cannot
be determined.

Nonetheless, this is meant to just provide a "flag" to the observer to
look at the data a bit more closely.

v1.1  25 June 1998   M.D. De La Pena  Fixed a syntax error in call to
                                      obs_print_out.

.endhelp
#---------------------------------------------------------------------------
procedure obs_doppler (images, stime, etime, period, group, o)

pointer images[ARB]             # I:  The image descriptors.
double  stime                   # I:  Start time of the current observation.
double  etime                   # I:  The end time of the current observation.
int     period                  # I:  The period of the orbit.
int     group                   # I:  Current SHP group number.
pointer o                       # I:  The output descriptor.

# Important times in the orbit.
real    dopmag                  # Value of the DAPMAG keyword.
double  dopzer                  # Time of zero doppler (MJD).
int     p, p2, p4               # Times in orbit (seconds)
int     orbmin,sintptr,sgn      # Doppler magnitude.
int     rshift, bshift          # How long doppler magnitude is at max.

# Important times of the observation.
real    exp                     # Calculated length of exposure.
int     mst, met                # Start/stop relative to doppler zero time (SEC).

# Misc.
int     i                       # Generic.
real    imgetr()                # Get header keyword real.
pointer sp                      # Stack pointer.
int     strdic()                # Match string.
pointer xstr                    # Generic.

begin
        call smark (sp)
        call salloc (xstr, SZ_LINE, TY_CHAR)
        
        # Retrieve time of zero doppler shift.
        if (images[ZSCI] == NULL)
            return
        iferr (call imgstr (images[ZSCI], "doppler", Memc[xstr], SZ_LINE))
            return
        call strlwr (Memc[xstr])
        if (strdic (Memc[xstr], Memc[xstr], SZ_LINE, "|off|on") != 2)
            return
        iferr (dopzer = imgetr (images[ZSCI], "dopzer"))
            return
        if (dopzer <= 0.d0)
            return
        iferr (dopmag = imgetr (images[ZSCI], "dopmag"))
            return
        
        # Calculate the actual exposure.
        iferr (exp = imgetr (images[ZSCI], "infob") *
               imgetr (images[ZSCI], "maxgss") *
               imgetr (images[ZSCI], "steptime"))
            return
        if (exp <= 0.0) {
            call obs_print_out (o, "   negative exposure time\n")
            return
        }
        
        # Subtract the zero point from the times.
        mst = DAY2SEC(stime - dopzer)
        met = DAY2SEC(etime - dopzer)

        # For each second, determine whether a maximum was calculated.
        rshift = 0
        bshift = 0
        p = MIN2SEC(period)
        p2 = MIN2SEC(period / 2)
        p4 = MIN2SEC(period / 4)
	for (i = mst; i<met; i = i+1){

            # Mod to the period.
            orbmin = mod(i,p)
            sgn = 1
            sintptr = orbmin

            # Indicate if in the "blue" half of the orbit.
            if (orbmin >= p2) {
                sgn = -1
                sintptr = orbmin-p2
            }

            # Check if in the second quadrant of the orbit
            if (sintptr >= p4)
                sintptr = p2-sintptr

            # Check if the maximum was reached.
            if (SEC2MIN(sintptr) > MAX_DOP) {
                if (sgn == 1)
                    rshift = rshift + 1
                else
                    bshift = bshift + 1
            }
        }

        # If any time was spent at maximums, say so.
        if (rshift > 0 || bshift > 0) {
            call sprintf (Memc[xstr], SZ_LINE, "  cal group %-3d exp %-9.2f over %-9.2f rshift %-3d bshift %-3d dopmag %-4d\n")
            call pargi (group)
            call pargr (exp)
            call pargr (met-mst-exp)
            call pargi (rshift)
            call pargi (bshift)
            call pargr (dopmag)
            call obs_print_out (o, Memc[xstr])
        }
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of obs_doppler
#---------------------------------------------------------------------------

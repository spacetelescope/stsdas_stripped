include	<imhdr.h>

# FOSBOT -- Print header specific to the Faint Object Spectrograph

procedure fosbot (im)

int     imgeti(), imaccf()
int     gcount,   pcount,   naxis1,   npat,     nchnls,   nread,    overscan
int     nxsteps,  livetime, deadtime, slices,   ysteps,   fchnl,    nmclears
int     ints

real    imgetr()
real    pa_aper, live, dead, expop, totalexp, xbase, ybase, yspace

char    fpkttime[SZ_FNAME],  aper_id[SZ_FNAME],   polar_id[SZ_FNAME]
char    grndmode[SZ_FNAME],  detector[SZ_FNAME],  fgwa_id[SZ_FNAME]
char    time[SZ_FNAME]

string  unavail  "unavailable"
              
pointer im

begin

        if (imaccf (im, "TIME-OBS") == YES)
           call imgstr (im, "TIME-OBS", time, SZ_FNAME)
        else if (imaccf (im, "FPKTTIME") == YES) {
                call imgstr (im, "FPKTTIME",  fpkttime,  SZ_FNAME)
                call strcpy (fpkttime[13], time, 8)
        } else call strcpy (unavail, time, 11)

        iferr (call imgstr (im, "APER_ID",   aper_id,  SZ_FNAME))
           aper_id[1] = EOS
        iferr (call imgstr (im, "POLAR_ID",  polar_id,  SZ_FNAME))
           polar_id[1] = EOS
        iferr (call imgstr (im, "GRNDMODE",  grndmode,  SZ_FNAME))
           grndmode[1] = EOS
        iferr (call imgstr (im, "DETECTOR",  detector,  SZ_FNAME))
           detector[1] = EOS
        iferr (call imgstr (im, "FGWA_ID",   fgwa_id,  SZ_FNAME))
           fgwa_id[1] = EOS
 
        iferr (gcount = imgeti (im, "GCOUNT"))
           gcount = 0
        iferr (pcount = imgeti (im, "PCOUNT"))
           pcount = 0
        iferr (npat = imgeti (im, "NPAT"))
           npat = 0
        iferr (nchnls = imgeti (im, "NCHNLS"))
           nchnls = 0
        iferr (nread = imgeti (im, "NREAD"))
           nread = 0
        iferr (overscan = imgeti (im, "OVERSCAN"))
           overscan = 0
        iferr (nmclears = imgeti (im, "NMCLEARS"))
           nmclears = 0
        iferr (nxsteps = imgeti (im, "NXSTEPS"))
           nxsteps = 0
        iferr (livetime = imgeti (im, "LIVETIME"))
           livetime = 0
        iferr (deadtime = imgeti (im, "DEADTIME"))
           deadtime = 0
        iferr (xbase = imgetr (im, "XBASE"))
           xbase = 0
        iferr (ybase = imgetr (im, "YBASE"))
           ybase = 0
        iferr (slices = imgeti (im, "SLICES"))
           slices = 0
        iferr (ysteps = imgeti (im, "YSTEPS"))
           ysteps = 0
        iferr (ints = imgeti (im, "INTS"))
           ints = 0
        iferr (fchnl = imgeti (im, "FCHNL"))
           fchnl = 0
        iferr (yspace = imgetr (im, "YSPACE"))
           yspace = 0

        if (imaccf (im, "PA_APER") == YES)
           pa_aper = imgetr (im, "PA_APER")
        else iferr (pa_aper = imgetr (im, "PANGAPER"))
           pa_aper = 0.0

        naxis1 = IM_LEN(im,1)

        live = livetime * 7.8125E-6
        dead = deadtime * 7.8125E-6
        expop = live * ints * npat * overscan * nread
        totalexp = expop * nxsteps * ysteps * slices
        if (ybase > 32768.)
           ybase = ybase - 65536.
        
        call printf (
        "Aperture = %s%29tDetector  = %s%54tNumber of groups = %d\n")
           call pargstr (aper_id)
           call pargstr (detector)
           call pargi (gcount)

        call printf (
        "Polarizer = %s%29tDisperser = %s%54tParameters/group = %d\n")
           call pargstr (polar_id)
           call pargstr (fgwa_id)
           call pargi (pcount)

        call printf (
        "Obs mode  = %s%54tNaxis1%71t= %d\n\n")
           call pargstr (grndmode)
           call pargi (naxis1)

        call printf (
        "Position angle = %g%29tPatterns%43t= %d%54tNo channels%69t= %d\n")
           call pargr (pa_aper)
           call pargi (npat)
           call pargi (nchnls)

        call printf (
        "Obs time%16t= %s%29tReadouts%43t= %d%54tComb%69t= %d\n")
           call pargstr (time)
           call pargi (nread)
           call pargi (overscan)

        call printf (
        "Exposure/pixel = %g%29tMemory clears = %d%54tSub step%69t= %d\n")
           call pargr (expop)
           call pargi (nmclears)
           call pargi (nxsteps)

        call printf ("Live time%16t= %g%29tX base%43t= %g%54tBins%69t= %d\n")
           call pargr (live)
           call pargr (xbase)
           call pargi (slices)

        call printf ("Dead time%16t= %g%29tY base%43t= %g%54tY size%69t= %d\n")
           call pargr (dead)
           call pargr (ybase)
           call pargi (ysteps)

        call printf ( 
        "Integrations   = %d%29tFirst channel = %d%54tY space%69t= %g\n")
           call pargi (ints)
           call pargi (fchnl)
           call pargr (yspace)

        call printf ("Total exposure = %g\n")
           call pargr(totalexp)

end


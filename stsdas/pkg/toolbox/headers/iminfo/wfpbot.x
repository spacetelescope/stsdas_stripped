include	<imhdr.h>

# WFPBOT -- Print header specific to the Wide Field Planetary Camera

procedure wfpbot (im)

int     imgeti()
int     gcount 

real    imgetr()
real    exptime, darktime, preftime, purgtime

bool    streq()

char    filtnam1[SZ_FNAME],  filtnam2[SZ_FNAME]
char    imagetyp[SZ_FNAME],  mode[SZ_FNAME],      camera[SZ_FNAME]
char    serials[SZ_FNAME],   shutter[SZ_FNAME],   kspots[SZ_FNAME]
char    maskcorr[SZ_FNAME],  atodcorr[SZ_FNAME],  blevcorr[SZ_FNAME]
char    biascorr[SZ_FNAME],  prefcorr[SZ_FNAME],  purgcorr[SZ_FNAME]
char    darkcorr[SZ_FNAME],  flatcorr[SZ_FNAME],  outcal[40]

string  done      "DONE"             
string  mask      " MASK"
string  atod      " ATOD"
string  blev      " BLEV"
string  bias      " BIAS"
string  pref      " PREF"
string  purg      " PURG"
string  dark      " DARK"
string  flat      " FLAT"
string  none      " none" 

pointer im

begin

        iferr (call imgstr (im, "FILTNAM1", filtnam1, SZ_FNAME))
            filtnam1[1] = EOS
        iferr (call imgstr (im, "FILTNAM2", filtnam2, SZ_FNAME))
            filtnam2[1] = EOS
        iferr (call imgstr (im, "IMAGETYP", imagetyp, SZ_FNAME))
            imagetyp[1] = EOS
        iferr (call imgstr (im, "MODE",     mode,     SZ_FNAME))
            mode[1] = EOS
        iferr (call imgstr (im, "CAMERA",   camera,   SZ_FNAME))
            camera[1] = EOS
        iferr (call imgstr (im, "SERIALS",  serials,  SZ_FNAME))
            serials[1] = EOS
        iferr (call imgstr (im, "SHUTTER",  shutter,  SZ_FNAME))
            shutter[1] = EOS
        iferr (call imgstr (im, "KSPOTS",   kspots,   SZ_FNAME))
            kspots[1] = EOS

        iferr (gcount = imgeti (im, "GCOUNT"))
           gcount = 0
        
        iferr (exptime = imgetr (im, "EXPTIME"))
           exptime = 0.0
        iferr (darktime = imgetr (im, "DARKTIME"))
           darktime = 0.0
        iferr (preftime = imgetr (im, "PREFTIME"))
           preftime = 0.0
        iferr (purgtime = imgetr (im, "PURGTIME"))
           purgtime = 0.0

        iferr (call imgstr (im, "MASKCORR", maskcorr, SZ_FNAME))
           maskcorr[1]=EOS
        iferr (call imgstr (im, "ATODCORR", atodcorr, SZ_FNAME))
           atodcorr[1]=EOS
        iferr (call imgstr (im, "BLEVCORR", blevcorr, SZ_FNAME))
           blevcorr[1]=EOS
        iferr (call imgstr (im, "BIASCORR", biascorr, SZ_FNAME))
           biascorr[1]=EOS
        iferr (call imgstr (im, "PREFCORR", prefcorr, SZ_FNAME))
           prefcorr[1]=EOS
        iferr (call imgstr (im, "PURGCORR", purgcorr, SZ_FNAME))
           purgcorr[1]=EOF
        iferr (call imgstr (im, "DARKCORR", darkcorr, SZ_FNAME))
           darkcorr[1]=EOS
        iferr (call imgstr (im, "FLATCORR", flatcorr, SZ_FNAME))
           flatcorr[1]=EOS

        call printf ("First filtername  = %s%50tNumber of groups = %d\n")
           call pargstr (filtnam1)
           call pargi (gcount)

        call printf ("Second filtername = %s%50tData type%67t= %s\n\n")
           call pargstr (filtnam2)
           call pargtype (IM_PIXTYPE(im))

        call printf ("Image type%19t= %s%40tExposure time (sec)%64t= %g\n")
           call pargstr (imagetyp)
           call pargr (exptime)
 
        call printf ("Mode%19t= %s%40tDark time (sec)%64t= %g\n")
           call pargstr (mode)
           call pargr (darktime)

        call printf ("Camera%19t= %s%40tPreflash time (sec)%64t= %g\n")
           call pargstr (camera)
           call pargr (preftime)

        call printf ("Serials%19t= %s%40tTime since purge (sec)%64t= %g\n")
           call pargstr (serials)
           call pargr (purgtime)

        # Build string of calibration switches done

        outcal[1]=EOS
        if (streq (maskcorr, done)) call strcat (mask, outcal, 40)
        if (streq (atodcorr, done)) call strcat (atod, outcal, 40)
        if (streq (blevcorr, done)) call strcat (blev, outcal, 40)
        if (streq (biascorr, done)) call strcat (bias, outcal, 40)
        if (streq (prefcorr, done)) call strcat (pref, outcal, 40)
        if (streq (purgcorr, done)) call strcat (purg, outcal, 40)
        if (streq (darkcorr, done)) call strcat (dark, outcal, 40)
        if (streq (flatcorr, done)) call strcat (flat, outcal, 40)
        if (outcal[1] == EOS) call strcat (none, outcal, 40)
 
        call printf ("Shutter%19t= %s%40tCalibration steps done:\n")
           call pargstr (shutter)

        call printf ("Kelsall spot lamp = %s%41t%s\n")
           call pargstr (kspots)
           call pargstr (outcal)

end



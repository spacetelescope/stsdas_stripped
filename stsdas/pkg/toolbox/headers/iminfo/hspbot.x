# HSPBOT -- Print the header specific to the High Speed Photometer

procedure hspbot (im)

int     imgeti(), imaccf()
int     gcount, pcount, words, detectob, detectsk

real    imgetr()
real    datamin, datamax, samptim

char    fpkttime[SZ_FNAME],  time[SZ_FNAME],  apertobj[SZ_FNAME]
char    apertsky[SZ_FNAME],  bunit[SZ_FNAME], data_typ[SZ_FNAME]
char    data_src[SZ_FNAME],  mode[SZ_FNAME]

string  unavail "unavailable"              
pointer im

begin

        if (imaccf (im, "TIME-OBS") == YES)
           call imgstr (im, "TIME-OBS", time, SZ_FNAME)
        else if (imaccf (im, "FPKTTIME") == YES) {
                call imgstr (im, "FPKTTIME",  fpkttime,  SZ_FNAME)
                call strcpy (fpkttime[13], time, 8)
             }
        else call strcpy (unavail, time, 11)
        iferr (call imgstr (im, "APERTOBJ",  apertobj,  SZ_FNAME))
           apertobj[1] = EOS
        iferr (call imgstr (im, "APERTSKY",  apertsky,  SZ_FNAME))
           apertsky[1] = EOS
        iferr (call imgstr (im, "BUNIT", bunit,  SZ_FNAME))
           bunit[1] = EOS
        iferr (call imgstr (im, "DATA_TYP",  data_typ,  SZ_FNAME))
           data_typ[1] = EOS
        iferr (call imgstr (im, "DATA_SRC",  data_src, SZ_FNAME))
           data_src[1] = EOS
        iferr (call imgstr (im, "MODE", mode, SZ_FNAME))
           mode[1] = EOS

        iferr (gcount = imgeti (im, "GCOUNT"))
           gcount = 0
        iferr (pcount = imgeti (im, "PCOUNT"))
           pcount = 0
        iferr (words = imgeti (im, "WORDS"))
           words = 0
        iferr (detectob = imgeti (im, "DETECTOB"))
           detectob = 0
        iferr (detectsk = imgeti (im, "DETECTSK"))
           detectsk = 0
        
        iferr (datamin = imgetr (im, "DATAMIN"))
           datamin = 0.0
        iferr (datamax = imgetr (im, "DATAMAX"))
           datamax = 0.0
        iferr (samptim = imgetr (im, "CD1_1"))
           samptim = 0.0

        call printf ("%45tNumber of groups = %d\n")
           call pargi (gcount)
        call printf ("Obs start time  = %s%45tParameters/group = %d\n")
           call pargstr (time)
           call pargi (pcount)
        call printf ("Number of samples = %d%45tDatamin%62t= %g\n")
           call pargi(words)
           call pargr(datamin)
        call printf ("Time between samples = %g%45tDatamax%62t= %g\n")
           call pargr (samptim)
           call pargr (datamax)
        
        call printf ("Aperture (object)   = %s%45tBunit       = %s\n")
           call pargstr (apertobj)
           call pargstr (bunit)
  
       call printf ("Aperture (sky)      = %s%45tData type   = %s\n")
          call pargstr (apertsky)
          call pargstr (data_typ)

       call printf ("Detector (object)   = %d%45tData source = %s\n")
          call pargi (detectob)
          call pargstr (data_src)
       call printf ("Detector (sky)      = %d%45tMode        = %s\n")
          call pargi (detectsk)
          call pargstr (mode)

end


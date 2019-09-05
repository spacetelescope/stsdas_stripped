include	<imhdr.h>

# FOCBOT -- Print header specific to the Faint Object Camera

procedure focbot (im)

int     imgeti()
int     naxis1, naxis2, gcount, pcount, sampbeg, linebeg, dnformt

real    imgetr()
real    datamin, datamax, orientat, exposure, sampoff, lineoff

bool    streq()

char    optcrly[SZ_FNAME],  pxformt[SZ_FNAME],  shtmode[SZ_FNAME]
char    ledmode[SZ_FNAME],  ledcolor[SZ_FNAME], cammode[SZ_FNAME]
char    smmmode[SZ_FNAME],  filtnam1[SZ_FNAME], filtnam2[SZ_FNAME]
char    filtnam3[SZ_FNAME], filtnam4[SZ_FNAME]
char    configuration[60],  outcal[30]
char    baccorr[SZ_FNAME],  itfcorr[SZ_FNAME],  pxlcorr[SZ_FNAME]
char    unicorr[SZ_FNAME],  wavcorr[SZ_FNAME],  geocorr[SZ_FNAME]
char    sdecorr[SZ_FNAME]

string  f48          "f/48"
string  f96          "f/96"
string  f288         "f/288"
string  spectrograph " spectrograph"
string  occult4      " 0.4 occultation"
string  occult8      " 0.8 occultation"
string  flat         " internal flat"
string  dark         " dark"
string  complete     "COMPLETE"
string  bac          " BAC "
string  itf          " ITF "
string  pxl          " PXL "
string  uni          " UNI "
string  wav          " WAV "
string  geo          " GEO "
string  sde          " SDE "
string  none         " none"
              
pointer im

begin

        iferr (call imgstr (im, "OPTCRLY",  optcrly,  SZ_FNAME))
           optcrly[1] = EOS
        iferr (call imgstr (im, "PXFORMT",  pxformt,  SZ_FNAME))
           pxformt[1] = EOS
        iferr (call imgstr (im, "SHTMODE",  shtmode,  SZ_FNAME))
           shtmode[1] = EOS
        iferr (call imgstr (im, "LEDMODE",  ledmode,  SZ_FNAME))
           ledmode[1] = EOS
        iferr (call imgstr (im, "CAMMODE",  cammode,  SZ_FNAME))
           cammode[1] = EOS
        iferr (call imgstr (im, "SMMMODE",  smmmode,  SZ_FNAME))
           smmmode[1] = EOS
        iferr (call imgstr (im, "FILTNAM1", filtnam1, SZ_FNAME))
           filtnam1[1] = EOS
        iferr (call imgstr (im, "FILTNAM2", filtnam2, SZ_FNAME))
           filtnam2[1] = EOS
        iferr (call imgstr (im, "FILTNAM3", filtnam3, SZ_FNAME))
           filtnam3[1] = EOS
        iferr (call imgstr (im, "FILTNAM4", filtnam4, SZ_FNAME))
           filtnam4[1] = EOS  

        naxis1 = IM_LEN(im,1)
        naxis2 = IM_LEN(im,2)
        iferr (gcount = imgeti (im, "GCOUNT"))
           gcount = 0
        iferr (pcount = imgeti (im, "PCOUNT"))
           pcount = 0
        iferr (sampoff = imgetr (im, "SAMPOFF"))
           sampoff = 0.0
        iferr (lineoff = imgetr (im, "LINEOFF"))
           lineoff = 0.0
        sampbeg = int (sampoff + 0.5) + 1
        linebeg = int (lineoff + 0.5) + 1
        iferr (dnformt = imgeti (im, "DNFORMT"))
           dnformt = INDEFI 
        
        iferr (datamin = imgetr (im, "DATAMIN"))
           datamin = INDEFR 
        iferr (datamax = imgetr (im, "DATAMAX"))
           datamax = INDEFR 
        iferr (orientat = imgetr (im, "ORIENTAT"))
           orientat = INDEFR 
        iferr (exposure = imgetr (im, "EXPTIME"))
           exposure = INDEFR 

        iferr (call imgstr (im, "BACCORR", baccorr, SZ_FNAME))
           baccorr[1]=EOS
        iferr (call imgstr (im, "ITFCORR", itfcorr, SZ_FNAME))
           itfcorr[1]=EOS
        iferr (call imgstr (im, "PXLCORR", pxlcorr, SZ_FNAME))
           pxlcorr[1]=EOS
        iferr (call imgstr (im, "UNICORR", unicorr, SZ_FNAME))
           unicorr[1]=EOS
        iferr (call imgstr (im, "WAVCORR", wavcorr, SZ_FNAME))
           wavcorr[1]=EOS
        iferr (call imgstr (im, "GEOCORR", geocorr, SZ_FNAME))
           geocorr[1]=EOS
        iferr (call imgstr (im, "SDECORR", sdecorr, SZ_FNAME))
           sdecorr[1]=EOS

        call printf (
        "Naxis1 = %d%25tDatamin = %g%56tNumber of groups = %d\n")
           call pargi (naxis1)
           call pargr (datamin)
           call pargi (gcount)

        call printf (
        "Naxis2 = %d%25tDatamax = %g%56tParameters/group = %d\n\n")
           call pargi (naxis2)
           call pargr (datamax)
           call pargi (pcount)

        # Build the configuration string to print

        configuration[1] = EOS
        if (streq (optcrly, "F48")) {
           call strcpy (f48, configuration, 4)
           if (streq (smmmode, "INBEAM")) 
              call strcat (spectrograph, configuration, 60)
           
        } else if (streq (optcrly, "F96")) {
           if (streq (cammode, "INBEAM")) {
              call strcpy (f288, configuration, 5)
              if (sampbeg == 153 && linebeg == 461) 
                 call strcat (occult4, configuration, 60)
              
           } else {
              call strcpy (f96, configuration, 4)
              if (sampbeg == 1 && linebeg == 153) 
                 call strcat (occult4, configuration, 60)
              else if (sampbeg == 422 && linebeg == 1) 
                 call strcat (occult8, configuration, 60)
              else if (sampbeg == 129 && linebeg == 641) 
                 call strcat (occult4, configuration, 60)
              else if (sampbeg == 550 && linebeg == 13) 
                 call strcat (occult8, configuration, 60)
           }
        }

        if (streq (shtmode, "INBEAM")) {
           if (streq (ledmode, "ACTIVE")) 
              call strcat (flat, configuration, 60)
           else 
             call strcat (dark, configuration, 60)
        }

        call printf ("Configuration   = %s\n")
           call pargstr (configuration)
        call printf ("Image format    = %s%48tImage orientation = %5.1f\n")
           call pargstr (pxformt)
           call pargr(orientat)
        call printf ("Shutter mode    = %s%48tExposure time     = %6.1f\n")
           call pargstr (shtmode)
           call pargr (exposure)
        call printf ("LED calibration = %s%48tSample begin      = %d\n")
           call pargstr (ledmode)
           call pargi (sampbeg)
        call printf ("LED color       = %s%48tLine begin        = %d\n")
           call pargstr (ledcolor)
           call pargi (linebeg)
        call printf ("Coronographic apodizer = %s%48tBits/data number  = %d\n")
           call pargstr (cammode)
           call pargi (dnformt)
        call printf (
           "Spectrographic mirror  = %s%48tCalibration steps completed:\n")
             call pargstr (smmmode)

        # Build string of completed calibration switches

        outcal[1]=EOS
        if (streq (baccorr, complete)) call strcat (bac, outcal, 30)
        if (streq (itfcorr, complete)) call strcat (itf, outcal, 30)
        if (streq (pxlcorr, complete)) call strcat (pxl, outcal, 30)
        if (streq (unicorr, complete)) call strcat (uni, outcal, 30)
        if (streq (wavcorr, complete)) call strcat (wav, outcal, 30)
        if (streq (geocorr, complete)) call strcat (geo, outcal, 30)
        if (streq (sdecorr, complete)) call strcat (sde, outcal, 30)
        if (outcal[1] == EOS) call strcpy (none, outcal, 5)

        call printf ("Filter names: %s, %s, %s, %s%50t%s\n")
             call pargstr (filtnam1)
             call pargstr (filtnam2)
             call pargstr (filtnam3)
             call pargstr (filtnam4)
             call pargstr (outcal)

end


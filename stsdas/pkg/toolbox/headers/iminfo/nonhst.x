include	<imhdr.h>

# NONHST -- Print general header (for images not identified as HST instruments)

procedure nonhst(im, image)

int      imgeti()
int      otime, ttime, i
double   imgetd()
double   airmass

pointer  im

char     ra[SZ_FNAME], dec[SZ_FNAME], zd[SZ_FNAME]
char     ut[SZ_FNAME], st[SZ_FNAME],  image[ARB]

string   none  "[NONE]"

begin

         call printf ("Imagename%45tObject\n")
         call printf ("%s%45t%s\n\n")
            call pargstr (image)
            call pargstr (IM_TITLE(im))

         call printf ("Naxis1 = %d%22tDatatype%50tDatamin = %g\n")
            call pargi (IM_LEN(im,1))
            call pargr (IM_MIN(im))

         if (IM_NDIM(im) > 1) {
            call printf ("Naxis2 = %d%24t%s%50tDatamax = %g\n")
               call pargi (IM_LEN(im,2))
               call pargtype (IM_PIXTYPE(im))
               call pargr (IM_MAX(im))

         } else {
            call printf ("%24t%s%50tDatamax = %g\n")
               call pargtype (IM_PIXTYPE(im))
               call pargr (IM_MAX(im))
         }

         if (IM_NDIM(im) > 2) {
            do i = 3, IM_NDIM(im) { 
               call printf ("Naxis%d = %d\n")
                  call pargi (i)
                  call pargi (IM_LEN(im,i))
            }
         }

         iferr (otime = imgeti (im, "OTIME"))
            otime = 0
         iferr (ttime = imgeti (im, "TTIME"))
            ttime = 0
         iferr (airmass = imgetd (im, "AIRMASS"))
            airmass = 0.

         iferr (call imgstr (im, "RA", ra, SZ_FNAME))
            call strcpy (none, ra, SZ_FNAME)
         iferr (call imgstr (im, "DEC", dec, SZ_FNAME))
            call strcpy (none, dec, SZ_FNAME)
         iferr (call imgstr (im, "ZD", zd, SZ_FNAME))
            call strcpy (none, zd, SZ_FNAME)
         iferr (call imgstr (im, "UT", ut, SZ_FNAME))
            call strcpy (none, ut, SZ_FNAME)
         iferr (call imgstr (im, "ST", st, SZ_FNAME))
            call strcpy (none, st, SZ_FNAME)

         call printf ("\nIntegration time = %d%45tRight ascension = %s\n")
            call pargi (otime)
            call pargstr (ra)

         call printf ("Elapsed time%18t= %d%45tDeclination%61t= %s\n")
            call pargi (ttime)
            call pargstr (dec)

         call printf ("Universal time%18t= %d%45tZenith distance = %s\n")
            call pargstr (ut)
            call pargstr (zd)

         call printf ("Sidereal time%18t= %d%45tAirmass%61t= %8g\n")
            call pargstr (st)
            call pargd (airmass)

end


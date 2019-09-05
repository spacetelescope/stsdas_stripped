include	<imhdr.h>

# HRSBOT -- Print header specific to the High Resolution Spectrograph

procedure hrsbot (im)

int     imgeti(), imaccf()
int     gcount, pcount, naxis1, sclamp, order, pktfmt
int     zfluxm, rptobs, carpos, detector
int     evcnt, ev2cnt, ev4cnt, ev8cnt, stsacnt, stlacnt, upcnt, lowcnt
int     sksacnt, sklacnt, darkcnt, sumcnt, nn, i
int     srchsize, deltax, deltay

int     binid[7], starsa[7], starla[7], upper[7], lower[7], skysa[7]
int     skyla[7], dark[7]
int     rptcd[6], every[7], every2[6], every4[6], every8[6]

real    imgetr()
real    exposure, lambda, dispersion, steptime, bright, faint

double  a, b, c 

bool    streq()

char    grating[SZ_FNAME],   obsmode[SZ_FNAME],   obstime[SZ_FNAME]
char    time[SZ_FNAME], locate[SZ_FNAME], map[SZ_FNAME], precision[SZ_FNAME]

string  unavail  "unavailable"
              
pointer im

begin

        # Initialize counters and arrays for substep patterns and repeat codes

        stsacnt = 0
        stlacnt = 0
        upcnt = 0
        lowcnt = 0
        sksacnt = 0
        sklacnt = 0
        darkcnt = 0
        ev2cnt = 0
        ev4cnt = 0
        ev8cnt = 0

        do i = 1,7 {
           starsa[i] = 0
           starla[i] = 0
           upper[i] = 0
           lower[i] = 0
           skysa[i] = 0
           skyla[i] = 0
           dark[i] = 0
        }

        do i = 1, 6 {
           every[i+1] = 0
           every2[i] = 0
           every4[i] = 0
           every8[i] = 0
        }

        # The first bin is repeated every time
        every[1] = 1
        evcnt = 1

        b = 10430.378

        iferr (call imgstr (im, "GRATING", grating, SZ_FNAME))
           grating[1] = EOS
        if (imaccf (im, "TIME-OBS") == YES)
           call imgstr (im, "TIME-OBS", time, SZ_FNAME)
        else if (imaccf (im, "PKTTIME") == YES) {
                call imgstr (im, "PKTTIME",  obstime,  SZ_FNAME)
                call strcpy (obstime[13], time, 8)
             }
        else call strcpy (unavail, time, 11)
        iferr (call imgstr (im, "OBSMODE", obsmode, SZ_FNAME))
           obsmode[1] = EOS

        naxis1 = IM_LEN(im,1)

        iferr (gcount = imgeti (im, "GCOUNT"))
           gcount = 0
        iferr (pcount = imgeti (im, "PCOUNT"))
           pcount = 0
        iferr (zfluxm = imgeti (im, "ZFLUXM"))
           zfluxm = 0
        iferr (rptobs = imgeti (im, "RPTOBS"))
           rptobs = 0
        iferr (carpos = imgeti (im, "CARPOS"))
           carpos = 0
        iferr (detector = imgeti (im, "DETECTOR"))
           detector = 0
        iferr (sclamp = imgeti (im, "SCLAMP"))
           sclamp = 0
        if (imaccf (im, "SPORDER") == YES)
           order = imgeti (im, "SPORDER")
        else iferr (order = imgeti (im, "ORDER"))
           order = 0
        
        iferr (exposure = imgetr (im, "EXPTIME"))
           exposure = 0.0
 
        call hrsbin (im, binid, rptcd) 

        iferr (pktfmt = imgeti (im, "PKTFMT"))
           pktfmt = 0

        # Set substep patterns and repeat codes

        do i = 1,7 {

           switch (binid[i]) {

           case 1:
              stsacnt = stsacnt + 1
              starsa[stsacnt] = i

           case 2:
              stlacnt = stlacnt + 1
              starla[stlacnt] = i

           case 3:
              upcnt = upcnt + 1
              upper[upcnt] = i

           case 4:
              lowcnt = lowcnt + 1
              lower[lowcnt] = i

           case 5:
              sksacnt = sksacnt + 1
              skysa[sksacnt] = i

           case 6:
              sklacnt = sklacnt + 1
              skyla[sklacnt] = i

           case 7:
              darkcnt = darkcnt + 1
              dark[darkcnt] = i

           default:
           }
        }

        do i = 1,6 {

           switch (rptcd[i]) {

           case 1:
              evcnt = evcnt + 1
              every[evcnt] = i + 1

           case 2:
              ev2cnt = ev2cnt + 1
              every2[ev2cnt] = i + 1

           case 4:
              ev4cnt = ev4cnt + 1
              every4[ev4cnt] = i + 1

           case 8:
              ev8cnt = ev8cnt + 1
              every8[ev8cnt] = i + 1
   
           default:
           }
        }

        # Calculate central wavelength and mean dispersion

        if (streq (grating, "G140M") || streq (grating, "G-1")) {
           a = 3301.8
           c = 11106.5
           dispersion = 1.588983e-6 * carpos + 0.0442
       
        } else if (streq (grating, "G160M") || streq (grating, "G-2")) {
           a = 4020.5
           c = 54807.9
           dispersion = 2.32342e-6 * carpos - 0.0475

        } else if (streq (grating, "G200M") || streq (grating, "G-3")) {
           a = 4615.7
           c = 30600.9
           dispersion = 3.63475e-6 * carpos - 0.0151

        } else if (streq (grating, "G270M") || streq (grating, "G-4")) {
           a = 5539.5
           c = 14887.3
           dispersion = 4.41666e-6 * carpos + 0.0500

        } else if (streq (grating, "G140L") || streq (grating, "G-5")) {
           a = 33013.4
           c = 49183.2
           dispersion = -6.451612e-6 * carpos + 0.8860

        } else if ((streq (grating, "ECH-A") && order > 0) || 
                   (streq (grating, "E-A") && order > 0 )) {
           a = 62886.3
           c = 39021.2
           dispersion = (9.570313e-5 * carpos - 2.0512) / order

        } else if ((streq (grating, "ECH-B") && order > 0) ||
                   (streq (grating, "E-B") && order > 0 )) {
           a = 63194.1
           c = 50575.0
           dispersion = (8.48148e-5 * carpos - 2.7303) / order 

        } else dispersion = 0.0

        if (order > 0 && carpos > 0) 
           lambda = (a/order) * sin((c-carpos)/b)

        call printf (
           "Grating   = %s%32tDetector = %d%53tNumber of groups = %d\n")
           call pargstr (grating)
           call pargi (detector)
           call pargi (gcount)

        call printf (
           "Obs mode  = %s%32tNaxis1   = %d%53tParameters/group = %d\n")
           call pargstr (obsmode)
           call pargi (naxis1)
           call pargi (pcount)

        call pktprnt (pktfmt)

     if (IM_NDIM(im) == 2) {
        iferr (srchsize = imgeti (im, "SRCHSIZE")) srchsize=0
        iferr (deltax = imgeti (im, "DELTA_X")) deltax=0
        iferr (deltay = imgeti (im, "DELTA_Y")) deltay=0
        iferr (steptime = imgetr (im, "STEPTIME")) steptime=0.
        iferr (bright = imgetr (im, "BRIGHT")) bright=0.
        iferr (faint = imgetr (im, "FAINT")) faint=0.
        iferr (call imgstr (im, "LOCATE", locate, SZ_FNAME))
           locate[1]=EOS
        iferr (call imgstr (im, "MAP", map, SZ_FNAME))
           map[1]=EOS
        iferr (call imgstr (im, "PRECISN", precision, SZ_FNAME))
           precision[1]=EOS

        call printf ("Search size = %d%24tLocate %40t= %s%58tBright   = %g\n")
           call pargi (srchsize)
           call pargstr (locate)
           call pargr (bright)
        call printf ("X increment = %d%24tMap %40t= %s%58tFaint    = %g\n")
           call pargi (deltax)
           call pargstr (map)
           call pargr (faint)
        call printf (
           "Y increment = %d%24tExposure/dwell point = %g%58tPrecision = %s\n")
           call pargi (deltay)
           call pargr (steptime)
           call pargstr (precision)
     } else
        call printf ("Substep Pattern         Bin%44tRepeat Code        Bin\n")

     if (stsacnt > 0) {
        sumcnt = evcnt + ev2cnt + ev4cnt + ev8cnt
        call printf ("star small aperture    ")
        do i = 1, stsacnt {
           if (i != stsacnt) {
              call printf (" %d,")
              call pargi (starsa[i])
           } else {
              if (sumcnt == 0) {
                 call printf (" %d\n")
                    call pargi (starsa[i])
              } else {
                  nn = 20 - ((stsacnt-1)*3 + 2)
                  call printf (" %d%*wevery")
                     call pargi (starsa[i])
                     call pargi (nn)
                  call rpt (every, evcnt, every2, ev2cnt, every4, ev4cnt,
                            every8, ev8cnt)
              }
           }
        }
     }
    
     if (stlacnt > 0) {
        sumcnt = evcnt + ev2cnt + ev4cnt + ev8cnt
        call printf ("star large aperture    ")
        do i = 1, stlacnt {
           if (i != stlacnt) {
              call printf (" %d,")
                 call pargi (starla[i])
           } else {
              if (sumcnt == 0) {
                 call printf (" %d\n")
                 call pargi (starla[i])
              } else {
                 nn = 20 - ((stlacnt-1)*3 + 2)
                 call printf (" %d%*wevery")
                    call pargi (starla[i])
                    call pargi (nn)
                 call rpt (every, evcnt, every2, ev2cnt, every4, ev4cnt,
                           every8, ev8cnt)
              }
           }
        }
     }
   
     if (upcnt > 0) {
        sumcnt = evcnt + ev2cnt + ev4cnt + ev8cnt
        call printf ("upper order            ")
        do i = 1, upcnt {
           if (i != upcnt) {
              call printf (" %d,")
                 call pargi (upper[i])
           } else {
              if (sumcnt == 0) {
                 call printf (" %d\n")
                 call pargi (upper[i])
              } else {
                 nn = 20 - ((upcnt-1)*3 + 2)
                 call printf (" %d%*wevery")
                    call pargi (upper[i])
                    call pargi (nn)
                 call rpt (every, evcnt, every2, ev2cnt, every4, ev4cnt,
                           every8, ev8cnt)
              }
           }
        }
     }
 
     if (lowcnt > 0) {
        sumcnt = evcnt + ev2cnt + ev4cnt + ev8cnt
        call printf ("lower order            ")
        do i = 1, lowcnt {
           if (i != lowcnt) {
              call printf (" %d,")
                 call pargi (lower[i])
           } else {
              if (sumcnt == 0) {
                 call printf (" %d\n")
                 call pargi (lower[i])
              } else {
                 nn = 20 - ((lowcnt-1)*3 + 2)
                 call printf (" %d%*wevery")
                    call pargi (lower[i])
                    call pargi (nn)
                 call rpt (every, evcnt, every2, ev2cnt, every4, ev4cnt,
                           every8, ev8cnt)
              }
           }
        }
     }

     if (sksacnt > 0) {
        sumcnt = evcnt + ev2cnt + ev4cnt + ev8cnt
        call printf ("sky small aperture     ")
        do i = 1, sksacnt {
           if (i != sksacnt) {
              call printf (" %d,")
                 call pargi (skysa[i])
           } else {
              if (sumcnt == 0) {
                 call printf (" %d\n")
                 call pargi (skysa[i])
              } else {
                 nn = 20 - ((sksacnt-1)*3 + 2)
                 call printf (" %d%*wevery")
                    call pargi (skysa[i])
                    call pargi (nn)
                 call rpt (every, evcnt, every2, ev2cnt, every4, ev4cnt,
                           every8, ev8cnt)
              }
           }
        }
     }

     if (sklacnt > 0) {
        sumcnt = evcnt + ev2cnt + ev4cnt + ev8cnt
        call printf ("sky large aperture     ")
        do i = 1, sklacnt {
           if (i != sklacnt) {
              call printf (" %d,")
                 call pargi (skyla[i])
           } else {
              if (sumcnt == 0) {
                 call printf (" %d\n")
                 call pargi (skyla[i])
              } else {
                 nn = 20 - ((sklacnt-1)*3 + 2)
                 call printf (" %d%*wevery")
                    call pargi (skyla[i])
                    call pargi (nn)
                 call rpt (every, evcnt, every2, ev2cnt, every4, ev4cnt,
                           every8, ev8cnt)
              }
           }
        }
     }

     if (darkcnt > 0) {
        sumcnt = evcnt + ev2cnt + ev4cnt + ev8cnt
        call printf ("dark                   ")
        do i = 1, darkcnt {
           if (i != darkcnt) {
              call printf (" %d,")
                 call pargi (dark[i])
           } else {
              if (sumcnt == 0) {
                 call printf (" %d\n")
                 call pargi (dark[i])
              } else {
                 nn = 20 - ((darkcnt-1)*3 + 2)
                 call printf (" %d%*wevery")
                    call pargi (dark[i])
                    call pargi (nn)
                 call rpt (every, evcnt, every2, ev2cnt, every4, ev4cnt,
                           every8, ev8cnt)
              }
           }
        }
     }

      if (carpos > 0. ) {
         call printf (
         "\nCarrousel position = %d%34tObs time    = %s%61tCal lamp = %d\n")
            call pargi (carpos)
            call pargstr (time)
            call pargi (sclamp)
      } else {
         call printf (
         "\nCarrousel position =%34tObs time    = %s%61tCal lamp = %d\n")
            call pargstr (time)
            call pargi (sclamp)
      }

      if (carpos > 0 && order > 0 ) {
         call printf (
         "Central wavelength = %7g%34tObs repeats = %d%61tOrder%70t= %d\n")
            call pargr (lambda)
            call pargi (rptobs)
            call pargi (order)
         if (exposure > 0.) {
            call printf (
            "Mean dispersion    = %7g%34tExposure    = %g%61tFlux%70t= %d\n")
               call pargr (dispersion)
               call pargr (exposure)
               call pargi (zfluxm)
         } else {
             call printf (
             "Mean dispersion    = %7g%34tExposure    =%61tFlux%70t= %d\n")
                call pargr (dispersion)
                call pargi (zfluxm)
         }
      } else {
         call printf (
         "Central wavelength =%34tObs repeats = %d%61tOrder%70t= %d\n")
            call pargi (rptobs)
            call pargi (order)
         if (exposure > 0.) {
            call printf (
            "Mean dispersion    =%34tExposure    = %g%61tFlux%70t= %d\n")
               call pargr (exposure)
               call pargi (zfluxm)
         } else {
            call printf (
            "Mean dispersion    =%34tExposure    =%61tFlux%70t= %d\n")
               call pargi(zfluxm)
         }
      }          
 
 end

